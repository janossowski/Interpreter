## File generated by the BNF Converter (bnfc 2.9.5).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestGramatyka

# Rules for building the parser.

AbsGramatyka.hs LexGramatyka.x ParGramatyka.y PrintGramatyka.hs TestGramatyka.hs : gramatyka.cf
	bnfc --haskell gramatyka.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestGramatyka : AbsGramatyka.hs LexGramatyka.hs ParGramatyka.hs PrintGramatyka.hs TestGramatyka.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsGramatyka.hs AbsGramatyka.hs.bak ComposOp.hs ComposOp.hs.bak DocGramatyka.txt DocGramatyka.txt.bak ErrM.hs ErrM.hs.bak LayoutGramatyka.hs LayoutGramatyka.hs.bak LexGramatyka.x LexGramatyka.x.bak ParGramatyka.y ParGramatyka.y.bak PrintGramatyka.hs PrintGramatyka.hs.bak SkelGramatyka.hs SkelGramatyka.hs.bak TestGramatyka.hs TestGramatyka.hs.bak XMLGramatyka.hs XMLGramatyka.hs.bak ASTGramatyka.agda ASTGramatyka.agda.bak ParserGramatyka.agda ParserGramatyka.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak gramatyka.dtd gramatyka.dtd.bak TestGramatyka LexGramatyka.hs ParGramatyka.hs ParGramatyka.info ParDataGramatyka.hs Makefile


# EOF
