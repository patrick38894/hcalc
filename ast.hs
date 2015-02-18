data Func = Unop UOP Func | Binop BOP Func Func | Constant Double | Variable VAR | Real RE
	deriving Show

data UOP = NEG | SIN | COS | TAN | EXP | LN | ASIN | ACOS | ATAN | SQRT | INV
	deriving (Show, Eq)

data BOP = ADD | SUB | MUL | DIV | POW | LOG
	deriving (Show, Eq)

type VAR = String

data RE = PI | E

differentiate :: Func -> VAR -> Func
--differentiate a function with respect to a specific variable

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
	| u == INV  = Unop NEG $ Binop DIV (differentiate f x) $ Binop POW f $ Constant 2.0
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

diff_n :: Func -> Int -> VAR -> Func

diff_n f 0 _ = f

diff_n f n x = differentiate (diff_n f (n-1) x) x


eval :: Func -> Func

eval Unop u (Constant c)
	| u == NEG = Constant (-c)
	| u == SIN = Constant $ sin c
	| u == COS = Constant $ cos c
	| u == TAN = Constant $ tan c
	| u == EXP = Constant $ exp c
	| u == LN  = Constant $ log c
	| u == SQRT= Constant $ sqrt c
	| u == INV = Constant $ 1.0 / c
	| u == ASIN= Constant $ asin c
	| u == ACOS= Constant $ acos c
	| u == ATAN= Constant $ atan c
	| otherwise = Constant 0.0

eval Binop b (Constant c1) (Constant c2)
	| b == ADD = Constant $ c1 + c2
	| b == SUB = Constant $ c1 - c2
	| b == MUL = Constant $ c1 * c2
	| b == DIV = Constant $ c1 / c2
	| b == POW = Constant $ c1 ^ c2
	| b == LOG = Constant $ logBase c1 c2

eval Unop u f = let v = eval f
		in case v of Constant c -> eval $ Unop u $ Constant c
			     otherwise -> Unop u v

eval Binop b (Constant c1) g = let v = eval g
			      in case v of Constant c2 -> eval $ Binop b (Constant c1) (Constant c2)
					   otherwise -> Binop b (Constant c1) v
					   
	
eval Binop b f (Constant c2) = let v = eval f
			      in case v of Constant c1 -> eval $ Binop b (Constant c1) (Constant c2)
					   otherwise -> Binop b v (Constant c1)

eval Binop b f g = let v1 = eval f
		   in case v1 of (Constant c1) -> eval $ Binop b (Constant c1) g
				 otherwise -> let v2 = eval g
					      in case v2 of (Constant c2) -> eval $ Binop b v1 (Constant c2)
							    otherwise -> Binop b v1 v2
eval Variable v = Variable v


--identities


eval Binop ADD (Constant 0.0) g = g
eval Binop ADD f (Constant 0.0) = f

eval Binop SUB (Constant 0.0) g = eval $ Unop NEG g
eval Binop SUB f (Constant 0.0) = f

eval Binop MUL (Constant 1.0) g = g
eval Binop MUL f (Constant 1.0) = f
eval Binop MUL (Constant 0.0) g = Constant 0.0
eval Binop MUL f (Constant 0.0) = Constant 0.0

eval Binop DIV f (Constant 1.0) = f

eval Binop POW f (Constant 1.0) = f
eval Binop POW f (Constant 0.0) = Constant 0.0

eval Binop LOG f (Constant 1.0) = Constant 0.0

eval 







	
main :: IO()

main = putStrLn (show $ differentiate (Unop EXP $ Variable "x") "x")







