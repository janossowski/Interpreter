module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import AbsSmol
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

data VarError = TypeMismatch String | ConstViolation String | VarNotFound String | InvalidOperation String | RuntimeError String
  deriving (Show, Eq)

type Eval a = ExceptT VarError (StateT (Env, Store) IO) a

-- Get information about identifier in the environment 
lookupInfo :: Identifier -> State Env (Maybe (Location, VarType, Bool))
lookupInfo id = gets (Map.lookup id)

-- Insert a new binding in the environment
insertBinding :: Identifier -> Location -> VarType -> Bool -> State Env ()
insertBinding id loc typ isConst = modify (Map.insert id (loc, typ, isConst))

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
      _ -> (env, store)
    StringType -> case val of
      StringVal s -> (Map.insert id (loc, StringType, isConst) env, store { stringStore = Map.insert loc s (stringStore store) })
      _ -> (env, store)
    BoolType -> case val of
      BoolVal b -> (Map.insert id (loc, BoolType, isConst) env, store { boolStore = Map.insert loc b (boolStore store) })
      _ -> (env, store)
    FunctionType -> case val of
      FunctionVal stmts capturedEnv -> (Map.insert id (loc, FunctionType, isConst) env, store { functionStore = Map.insert loc (stmts, capturedEnv) (functionStore store) })
      _ -> (env, store))

  case typ of
    IntType -> case val of
      IntVal _ -> return ()
      _ -> throwError $ TypeMismatch "Expected Int value"
    StringType -> case val of
      StringVal _ -> return ()
      _ -> throwError $ TypeMismatch "Expected String value"
    BoolType -> case val of
      BoolVal _ -> return ()
      _ -> throwError $ TypeMismatch "Expected Bool value"
    FunctionType -> case val of
      FunctionVal _ _ -> return ()
      _ -> throwError $ TypeMismatch "Expected Function value"

-- Look up the value of an identifier in the current state
getValue :: Identifier -> Eval (Maybe Value)
getValue id = do
  env <- lift $ gets fst
  store <- lift $ gets snd
  case Map.lookup id env of
    Nothing -> return Nothing
    Just (loc, typ, _) -> case typ of
      IntType -> return $ fmap IntVal (Map.lookup loc (intStore store))
      StringType -> return $ fmap StringVal (Map.lookup loc (stringStore store))
      BoolType -> return $ fmap BoolVal (Map.lookup loc (boolStore store))
      FunctionType -> return $ fmap (uncurry FunctionVal) (Map.lookup loc (functionStore store))

-- Assign a value to an existing variable
assignVariable :: Identifier -> Value -> Eval ()
assignVariable id val = do
  env <- lift $ gets fst
  case Map.lookup id env of
    Nothing -> throwError $ VarNotFound ("Variable not found: " ++ id)
    Just (loc, typ, isConst) -> do
      when isConst $ throwError $ ConstViolation ("Cannot assign to constant variable " ++ id)

      case typ of
        IntType -> case val of
          IntVal i -> lift $ modify (\(env, store) -> (env, store { intStore = Map.insert loc i (intStore store) }))
          _ -> throwError $ TypeMismatch "Expected Int value"
        StringType -> case val of
          StringVal s -> lift $ modify (\(env, store) -> (env, store { stringStore = Map.insert loc s (stringStore store) }))
          _ -> throwError $ TypeMismatch "Expected String value"
        BoolType -> case val of
          BoolVal b -> lift $ modify (\(env, store) -> (env, store { boolStore = Map.insert loc b (boolStore store) }))
          _ -> throwError $ TypeMismatch "Expected Bool value"
        FunctionType -> throwError $ TypeMismatch "Cannot assign to a function"

ensureInt :: Value -> Eval Int
ensureInt (IntVal i) = return i
ensureInt _ = throwError $ TypeMismatch "Expected Int"

ensureBool :: Value -> Eval Bool
ensureBool (BoolVal b) = return b
ensureBool _ = throwError $ TypeMismatch "Expected Bool"

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
      -- Save current environment
      currentEnv <- lift $ gets fst
      -- Execute function with the captured environment
      lift $ modify (\(_, store) -> (capturedEnv, store))
      mapM_ evaluateStatement stmts
      -- Restore original environment
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
  val1 <- ensureInt =<< evaluateExpr expr1
  val2 <- ensureInt =<< evaluateExpr expr2
  case op of
    Times -> return $ IntVal (val1 * val2)
    Div -> if val2 == 0
             then throwError $ RuntimeError "Division by zero"
             else return $ IntVal (val1 `div` val2)
    Mod -> if val2 == 0
             then throwError $ RuntimeError "Modulus by zero"
             else return $ IntVal (val1 `mod` val2)
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

evaluateStatement :: Statement -> Eval ()
evaluateStatement (EVarDecl typ (Ident id) expr) = do
  val <- evaluateExpr expr
  let varType = case typ of
        AbsSmol.Int -> IntType
        AbsSmol.Str -> StringType
        AbsSmol.Bool -> BoolType
  declareVariable varType id val False
evaluateStatement (ERovarDecl typ (Ident id) expr) = do
  val <- evaluateExpr expr
  let varType = case typ of
        AbsSmol.Int -> IntType
        AbsSmol.Str -> StringType
        AbsSmol.Bool -> BoolType
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
        AbsSmol.Int -> IntType
        AbsSmol.Str -> StringType
        AbsSmol.Bool -> BoolType
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

-- Block evaluation for convenience
evaluateBlock :: Block -> Eval ()
evaluateBlock (EBlock stmts) = do
  savedEnv <- lift $ gets fst
  mapM_ evaluateStatement stmts
  lift $ modify (\(_, store) -> (savedEnv, store))

-- Add function definition to the environment
addFunctionDef :: FunctionDef -> Eval ()
addFunctionDef (EFunDef (Ident id) (EBlock stmts)) = do
  (env, store) <- get
  -- Get new location for the function
  let loc = Map.size (functionStore store)
  -- Insert the function into the environment with a placeholder
  let updatedEnv = Map.insert id (loc, FunctionType, False) env
  -- Capture the environment including the new function binding
  let capturedEnv = updatedEnv
  -- Insert the function with its statements and captured environment into the store
  let updatedStore = store { functionStore = Map.insert loc (stmts, capturedEnv) (functionStore store) }
  -- Update the state with the new environment and store
  put (updatedEnv, updatedStore)

executeProgram :: Program -> Eval ()
executeProgram (EProg funcDefs) = do
  mapM_ addFunctionDef funcDefs
  val <- getValue "main"
  case val of
    Just (FunctionVal stmts capturedEnv) -> do
      lift $ modify (\(_, store) -> (capturedEnv, store))
      mapM_ evaluateStatement stmts
    _ -> throwError $ VarNotFound "main function not found"

-- Main for debug purposes
main :: IO ()
main = do
  let initialState = (Map.empty, Store Map.empty Map.empty Map.empty Map.empty)
      program = EProg
        [ EFunDef (Ident "main") (EBlock
            [ EVarDecl AbsSmol.Int (Ident "x") (ELitInt 10)
            , EPrint (EVar (Ident "x"))
            , EFor AbsSmol.Int (Ident "i") (ELitInt 0) (ELitInt 3) (EBlock [EPrint (EVar (Ident "i"))])
            ])
        ]
  result <- runStateT (runExceptT $ executeProgram program) initialState
  print result