import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import AbsGramatyka
import System.IO (print)
import Control.Monad

type Identifier = String
type Location = Int

data VarType = IntType | StringType | BoolType | FunctionType
  deriving (Show, Eq)

type Env = Map.Map Identifier (Location, VarType, Bool)

data Store = Store {
    intStore :: Map.Map Location Int,
    stringStore :: Map.Map Location String,
    boolStore :: Map.Map Location Bool,
    functionStore :: Map.Map Location ([Statement], Env)
  } deriving (Show)

data Value
  = IntVal Int
  | StringVal String
  | BoolVal Bool
  | FunctionVal [Statement] Env
  deriving (Show, Eq)

data VarError = TypeMismatch String | ConstViolation String | VarNotFound String | InvalidOperation String
  deriving (Show, Eq)

type Eval a = ExceptT VarError (StateT (Env, Store) IO) a

-- Lookup information about an identifier
lookupInfo :: Identifier -> State Env (Maybe (Location, VarType, Bool))
lookupInfo id = gets (Map.lookup id)

-- Insert a new binding in the environment
insertBinding :: Identifier -> Location -> VarType -> Bool -> State Env ()
insertBinding id loc typ isConst = modify (Map.insert id (loc, typ, isConst))

-- Declare a new variable
declareVariable :: VarType -> Identifier -> Value -> Bool -> Eval ()
declareVariable typ id val isConst = do
  env <- lift $ gets fst
  case Map.lookup id env of
    Just (_, _, True) -> throwError $ ConstViolation ("Cannot redeclare constant variable " ++ id)
    _ -> return ()

  storeSize <- lift $ gets (case typ of
    IntType -> Map.size . intStore . snd
    StringType -> Map.size . stringStore . snd
    BoolType -> Map.size . boolStore . snd
    FunctionType -> Map.size . functionStore . snd)
  let loc = storeSize

  lift $ modify (\(env, store) -> case typ of
    IntType -> case val of
      IntVal i -> (Map.insert id (loc, IntType, isConst) env, store { intStore = Map.insert loc i (intStore store) })
      _ -> error "Expected Int value"
    StringType -> case val of
      StringVal s -> (Map.insert id (loc, StringType, isConst) env, store { stringStore = Map.insert loc s (stringStore store) })
      _ -> error "Expected String value"
    BoolType -> case val of
      BoolVal b -> (Map.insert id (loc, BoolType, isConst) env, store { boolStore = Map.insert loc b (boolStore store) })
      _ -> error "Expected Bool value"
    FunctionType -> case val of
      FunctionVal stmts capturedEnv -> (Map.insert id (loc, FunctionType, isConst) env, store { functionStore = Map.insert loc (stmts, capturedEnv) (functionStore store) })
      _ -> error "Expected Function value")

-- Function to look up the value of an identifier in the current state
getValue :: Identifier -> Eval (Maybe Value)
getValue id = do
  env <- lift $ gets fst
  store <- lift $ gets snd
  case Map.lookup id env of
    Nothing -> return Nothing -- Identifier not found in environment
    Just (loc, typ, _) -> case typ of
      IntType -> return $ fmap IntVal (Map.lookup loc (intStore store))
      StringType -> return $ fmap StringVal (Map.lookup loc (stringStore store))
      BoolType -> return $ fmap BoolVal (Map.lookup loc (boolStore store))
      FunctionType -> return $ fmap (\(stmts, capturedEnv) -> FunctionVal stmts capturedEnv) (Map.lookup loc (functionStore store))

-- Function to assign a value to an existing variable
assignVariable :: Identifier -> Value -> Eval ()
assignVariable id val = do
  env <- lift $ gets fst
  case Map.lookup id env of
    Nothing -> throwError $ VarNotFound ("Variable " ++ id ++ " not found")
    Just (loc, typ, isConst) -> do
      when isConst $ throwError $ ConstViolation ("Cannot assign to constant variable " ++ id)
      lift $ modify (\(env, store) -> case (typ, val) of
        (IntType, IntVal i) -> (env, store { intStore = Map.insert loc i (intStore store) })
        (StringType, StringVal s) -> (env, store { stringStore = Map.insert loc s (stringStore store) })
        (BoolType, BoolVal b) -> (env, store { boolStore = Map.insert loc b (boolStore store) })
        (FunctionType, FunctionVal stmts capturedEnv) -> (env, store { functionStore = Map.insert loc (stmts, capturedEnv) (functionStore store) })
        _ -> error "Type mismatch during assignment")

-- Ensure a value is of type IntVal
ensureInt :: Value -> Eval Int
ensureInt (IntVal i) = return i
ensureInt _ = throwError $ TypeMismatch "Expected Int"

-- Ensure a value is of type BoolVal
ensureBool :: Value -> Eval Bool
ensureBool (BoolVal b) = return b
ensureBool _ = throwError $ TypeMismatch "Expected Bool"

-- Evaluate an expression
evaluateExpr :: Expr -> Eval Value
evaluateExpr (EVar (Ident id)) = do
  val <- getValue id
  case val of
    Nothing -> throwError $ VarNotFound ("Variable " ++ id ++ " not found")
    Just v -> return v
evaluateExpr (ELitInt i) = return $ IntVal (fromIntegral i)
evaluateExpr ELitTrue = return $ BoolVal True
evaluateExpr ELitFalse = return $ BoolVal False
evaluateExpr (EApp (Ident id)) = do
  val <- getValue id
  case val of
    Just (FunctionVal stmts capturedEnv) -> do
      -- Save the current environment
      currentEnv <- lift $ gets fst
      -- Execute the function with the captured environment
      lift $ modify (\(_, store) -> (capturedEnv, store))
      mapM_ evaluateStatement stmts
      -- Restore the original environment
      lift $ modify (\(_, store) -> (currentEnv, store))
      return $ FunctionVal stmts capturedEnv
    _ -> throwError $ VarNotFound ("Function " ++ id ++ " not found or not a function")
evaluateExpr (EString s) = return $ StringVal s
evaluateExpr (Neg expr) = do
  val <- evaluateExpr expr
  case val of
    IntVal i -> return $ IntVal (-i)
    _ -> throwError $ TypeMismatch "Expected Int for negation"
evaluateExpr (Not expr) = do
  val <- evaluateExpr expr
  case val of
    BoolVal b -> return $ BoolVal (not b)
    _ -> throwError $ TypeMismatch "Expected Bool for logical not"
evaluateExpr (EMul expr1 op expr2) = do
  val1 <- evaluateExpr expr1 >>= ensureInt
  val2 <- evaluateExpr expr2 >>= ensureInt
  let result = case op of
        Times -> val1 * val2
        Div -> val1 `div` val2
        Mod -> val1 `mod` val2
  return $ IntVal result
evaluateExpr (EAdd expr1 op expr2) = do
  val1 <- evaluateExpr expr1 >>= ensureInt
  val2 <- evaluateExpr expr2 >>= ensureInt
  let result = case op of
        Plus -> val1 + val2
        Minus -> val1 - val2
  return $ IntVal result
evaluateExpr (ERel expr1 op expr2) = do
  val1 <- evaluateExpr expr1 >>= ensureInt
  val2 <- evaluateExpr expr2 >>= ensureInt
  let result = case op of
        LTH -> val1 < val2
        LE -> val1 <= val2
        GTH -> val1 > val2
        GE -> val1 >= val2
        EQU -> val1 == val2
        NE -> val1 /= val2
  return $ BoolVal result
evaluateExpr (EAnd expr1 expr2) = do
  val1 <- evaluateExpr expr1 >>= ensureBool
  val2 <- evaluateExpr expr2 >>= ensureBool
  return $ BoolVal (val1 && val2)
evaluateExpr (EOr expr1 expr2) = do
  val1 <- evaluateExpr expr1 >>= ensureBool
  val2 <- evaluateExpr expr2 >>= ensureBool
  return $ BoolVal (val1 || val2)

-- Evaluate a statement
evaluateStatement :: Statement -> Eval ()
evaluateStatement (EVarDecl typ (Ident id) expr) = do
  val <- evaluateExpr expr
  let varType = case typ of
        AbsGramatyka.Int -> IntType
        AbsGramatyka.Str -> StringType
        AbsGramatyka.Bool -> BoolType
  declareVariable varType id val False
evaluateStatement (ERovarDecl typ (Ident id) expr) = do
  val <- evaluateExpr expr
  let varType = case typ of
        AbsGramatyka.Int -> IntType
        AbsGramatyka.Str -> StringType
        AbsGramatyka.Bool -> BoolType
  declareVariable varType id val True
evaluateStatement (EAssign (Ident id) expr) = do
  val <- evaluateExpr expr
  assignVariable id val
evaluateStatement (EPrint expr) = do
  val <- evaluateExpr expr
  liftIO $ print val
evaluateStatement (EWhile expr block) = do
  cond <- evaluateExpr expr >>= ensureBool
  when cond $ do
    evaluateBlock block
    evaluateStatement (EWhile expr block)
evaluateStatement (EIf expr block1 block2) = do
  cond <- evaluateExpr expr >>= ensureBool
  if cond
    then evaluateBlock block1
    else evaluateBlock block2
evaluateStatement (EFor typ (Ident id) expr1 expr2 block) = do
  let varType = case typ of
        AbsGramatyka.Int -> IntType
        AbsGramatyka.Str -> StringType
        AbsGramatyka.Bool -> BoolType
  startVal <- evaluateExpr expr1
  endVal <- evaluateExpr expr2 >>= ensureInt
  declareVariable varType id startVal False
  let loop = do
        currentVal <- evaluateExpr (EVar (Ident id)) >>= ensureInt
        when (currentVal < endVal) $ do
          evaluateBlock block
          assignVariable id (IntVal (currentVal + 1))
          loop
  loop
evaluateStatement (EBlockStatement (EBlock stmts)) = do
  savedEnv <- lift $ gets fst
  mapM_ evaluateStatement stmts
  lift $ modify (\(_, store) -> (savedEnv, store))
evaluateStatement (EExpressionStatement expr) = do
  _ <- evaluateExpr expr
  return ()

-- Evaluate a block
evaluateBlock :: Block -> Eval ()
evaluateBlock (EBlock stmts) = mapM_ evaluateStatement stmts

-- Main function for testing
main :: IO ()
main = do
  let initialState = (Map.empty, Store Map.empty Map.empty Map.empty Map.empty)
      actions = do
        -- Declare variables
        result1 <- runExceptT $ declareVariable IntType "x" (IntVal 10) False
        result2 <- runExceptT $ declareVariable StringType "y" (StringVal "hello") True
        result3 <- runExceptT $ declareVariable BoolType "z" (BoolVal True) False
        result4 <- runExceptT $ declareVariable FunctionType "f" (FunctionVal [] Map.empty) False
        -- Assign to variables
        result5 <- runExceptT $ assignVariable "x" (IntVal 20)
        result6 <- runExceptT $ assignVariable "y" (StringVal "world")
        -- Evaluate expressions
        result7 <- runExceptT $ evaluateExpr (EVar (Ident "x"))
        result8 <- runExceptT $ evaluateExpr (ELitInt 42)
        result9 <- runExceptT $ evaluateExpr ELitTrue
        -- Evaluate statements
        let stmts = [ EVarDecl AbsGramatyka.Int (Ident "a") (ELitInt 1)
                    , EWhile (EVar (Ident "a")) (EBlock [EPrint (EVar (Ident "a")), EAssign (Ident "a") (EAdd (EVar (Ident "a")) Plus (ELitInt 1))])
                    , EIf ELitFalse (EBlock [EPrint (EString "True branch")]) (EBlock [EPrint (EString "False branch")])
                    , EFor AbsGramatyka.Int (Ident "i") (ELitInt 0) (ELitInt 3) (EBlock [EPrint (EVar (Ident "i"))])
                    ]
        result10 <- runExceptT $ evaluateBlock (EBlock stmts)
        return (result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)
      finalState = execStateT actions initialState
  finalResult <- finalState
  print finalResult
