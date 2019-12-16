module ArithParser where


import MyParser



--- grammar ---
-- 
--     expression = term + expression
--                | term - expression
--                | term
--     
--     term = factor * term
--          | factor / term
--          | factor
--
--     factor = base ^ exponent
--            | base
-- 
--     base = '(' expression ')'
--          | -expression
--          | number
--
--     exponent = '(' expression ')'
--              | number
--
--     number = integer


data AExp = Add AExp AExp
          | Sub AExp AExp
          | Mul AExp AExp
          | Div AExp AExp
          | Pow AExp AExp
          | Neg AExp
          | Num Int
          deriving Show

expr :: Parser AExp
--expr = (do t <- term; symbol "+"; e <- expr; return (Add t e))
--     +++ (do t <- term; symbol "-"; e <- expr; return (Sub t e))
expr = (term =>> \t -> symbol "+" =>> \_ -> expr =>> \e -> ret (Add t e))
     +++ (term =>> \t -> symbol "-" =>> \_ -> expr =>> \e -> ret (Sub t e))
     +++ term

term :: Parser AExp
--term = (do f <- factor; symbol "*"; t <- term; return (Mul f t))
--     +++ (do f <- factor; symbol "/"; t <- term; return (Div f t))
term = (factor =>> \f -> symbol "*" =>> \_ -> term =>> \t -> ret (Mul f t))
     +++ (factor =>> \f -> symbol "/" =>> \_ -> term =>> \t -> ret (Div f t))
     +++ factor

factor :: Parser AExp
--factor = (do b <- base; symbol "^"; e <- expnt; return (Pow b e))
factor = (base =>> \b -> symbol "^" =>> \_ -> expnt =>> \e -> ret (Pow b e))
       +++ base

base :: Parser AExp
--base = (do symbol "("; e <- expr; symbol ")"; return e)
--     +++ (do symbol "-"; e <- expr; return (Neg e))
base = (symbol "(" =>> \_ -> expr =>> \e -> symbol ")" =>> \_ -> ret e)
     +++ (symbol "-" =>> \_ -> expr =>> \e -> ret (Neg e))
     +++ number

expnt :: Parser AExp
--expnt = (do symbol "("; e <- expr; symbol ")"; return e)
expnt = (symbol "(" =>> \_ -> expr =>> \e -> symbol ")" =>> \_ -> ret e)
      +++ number

number :: Parser AExp
--number = do n <- integer; return (Num n)
number = integer =>> \n ->
         ret (Num n)

arithExpr = expr +++ term +++ factor

arithParser :: String -> AExp
arithParser inp = case parse arithExpr inp of
                        [(e, [])] -> e
                        [(e, out)] -> error "Unused input!"
                        [] -> error "Invalid input!"

eval :: AExp -> Int
eval (Num n)   = n
eval (Add t e) = eval t + eval e
eval (Sub t e) = eval t - eval e
eval (Mul f t) = eval f * eval t
eval (Div f t) = eval f `div` eval t
eval (Pow b e) = eval b ^ eval e
eval (Neg e)   = - eval e






-- Machine Instructions and State Transitions --
--
-- C |n| = VAL n
-- C |t + u| = C |t|; C |u|; ADD
-- C |t - u| = C |t|; C |u|; SUB
-- C |t * u| = C |t|; C |u|; MUL
-- C |t / u| = C |t|; C |u|; DIV

data Ins = VAL Int | ADD | SUB | MUL | DIV | POW | NEG
           deriving Show
type Code = [Ins]

toCode :: AExp -> Code
toCode (Num n)   = [VAL n]
toCode (Add t u) = toCode t ++ toCode u ++ [ADD]
toCode (Sub t u) = toCode t ++ toCode u ++ [SUB]
toCode (Mul t u) = toCode t ++ toCode u ++ [MUL]
toCode (Div t u) = toCode t ++ toCode u ++ [DIV]
toCode (Pow t u) = toCode t ++ toCode u ++ [POW]
toCode (Neg t)   = toCode t ++ [NEG]

compile :: String -> Code
compile = toCode . arithParser




-- Machine's Operational Semantics --
--
-- Components:
--      1. Code Segment (list of instructions)
--      2. Stack (list of values)
--
-- represented by a pair : (code, stack)
--
-- Initial State:
--      code    = C |a|
--      stack   = empty
--
-- Final State:
--      code    = empty
--      stack   = result is on the top of the stack
--
-- State Transistion Rules:
--
--  Before                  After
--  ((VAL n; c), s)         = (c, (n, s))
--  ((ADD; c), (m, n, s))   = (c, (r, s)) where r = n + m
--  ((SUB; c), (m, n, s))   = (c, (r, s)) where r = n - m
--  ((MUL; c), (m, n, s))   = (c, (r, s)) where r = n * m
--  ((DIV; c), (m, n, s))   = (c, (r, s)) where r = n / m
--  ((POW; c), (m, n, s))   = (c, (r, s)) where r = n ^ m
--  ((NEG; c), (n, s))      = (c, (r, s)) where r = -n
--

type Stack = [Int]
type State = (Code, Stack)

initState :: Code -> State
initState c = (c, [])

mEval :: State -> State
mEval ((VAL n:c), s)          = mEval (c, (n:s))
mEval ((ADD: c), (m:n:s))     = mEval (c, (r:s)) where r = n + m
mEval ((SUB: c), (m:n:s))     = mEval (c, (r:s)) where r = n - m
mEval ((MUL: c), (m:n:s))     = mEval (c, (r:s)) where r = n * m
mEval ((DIV: c), (m:n:s))     = mEval (c, (r:s)) where r = n `div` m
mEval ((POW: c), (m:n:s))     = mEval (c, (r:s)) where r = n ^ m
mEval ((NEG: c), (n:s))       = mEval (c, (r:s)) where r = -n
mEval ([], s)                 = ([], s)

exec :: String -> Int
exec e = res
            where ([], [res]) = mEval $ initState $ compile e






-- inc x = x + 1
--
-- f : N -> N
-- inc : N -> N
--
-- inc = fun x -> x + 1
--
-- inc x = (fun x -> x + 1) 2               // (fun x -> T) U = T[x <- U]
--       = x + 1[x <- 2]
--       = 2 + 1
--       = 3
--
-- add = (fun x -> (fun y -> (x + y)))
--
-- add 2 3 = (((fun x -> (fun y -> (x + y))) 2) 3)
--         = 2 + 3
--         = 5
