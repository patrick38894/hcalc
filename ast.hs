data Func = Unop UOP Func | Binop BOP Func Func | Constant Double | Variable VAR
	deriving Show

data UOP = NEG | SIN | COS | TAN | EXP | LN | ASIN | ACOS | ATAN | SQRT
	deriving (Show, Eq)

data BOP = ADD | SUB | MUL | DIV | POW | LOG
	deriving (Show, Eq)

type VAR = String

differentiate :: Func -> VAR -> Func

differentiate (Constant f) x = Constant 0.0

differentiate (Variable f) x = if x == f
					  then Constant 1.0
					  else Constant 0.0


differentiate (Unop u f) x
	| u == NEG  = Unop NEG  $ differentiate f x
	| u == SIN  = Binop MUL (differentiate f x) $ Unop COS f
	| u == COS  = Binop MUL (differentiate f x) $ Unop NEG $ Unop SIN f
	| u == TAN  = Binop DIV (differentiate f x) $ Binop POW (Unop COS f) $ Constant 2.0
	| u == EXP  = Binop MUL (differentiate f x) $ Unop EXP f
	| u == LN   = Binop DIV (differentiate f x) f
	| u == SQRT = Binop DIV (differentiate f x) $ Binop MUL (Constant 2.0) $ Unop SQRT f
	| u == ASIN = Binop DIV (differentiate f x) $ Unop SQRT $ Binop SUB (Constant 1.0) $ Binop POW f $ Constant 2.0
	| u == ACOS = Binop DIV (Unop NEG $ differentiate f x) $ Unop SQRT $ Binop SUB (Constant 1.0) $ Binop POW f $ Constant 2.0
	| u == ATAN = Binop DIV (differentiate f x) $ Binop ADD (Constant 1.0) $ Binop POW f $ Constant 2.0
	| otherwise = Constant 0.0

differentiate (Binop b f g) x
	| b == ADD = Binop ADD (differentiate f x) $ differentiate g x
	| b == SUB = Binop SUB (differentiate f x) $ differentiate g x
	| b == MUL = Binop ADD (Binop MUL g  $ differentiate f x) $ Binop MUL f $ differentiate g x
	| b == DIV = Binop DIV (Binop SUB (Binop MUL g $ differentiate f x) $ Binop MUL f $ differentiate g x) $ Binop POW g $ Constant 2.0
	| b == POW = Binop MUL (Binop POW f $ Binop SUB g $ Constant 1.0) $ Binop ADD (Binop MUL g $ differentiate f x)  $ Binop MUL f (Binop MUL (Unop LN f) $ differentiate g x)
	| b == LOG = Binop DIV (Binop SUB (Binop DIV (Binop MUL (Unop LN f) $ differentiate g x) g) $ Binop DIV (Binop MUL (Unop LN g) $ differentiate f x) f) $ Binop POW (Unop LN f) $ Constant 2.0
	| otherwise = Constant 0.0
	
