import DeBruijn
import ParseExpression



--- instruction set ---
data Ins = Access Int
         | Grab
         | Push Code
         deriving Show

         
--- structure ---
type Code = [Ins]
type Env = [Closure]
type Stk = [Closure]

data Closure = Clo (Code, Env)
                deriving Show



--- compiler from de Bruijn term to abstract machine language ---
compile :: DBExp -> [Ins]
compile (DBVar n) = [Access n]
compile (DBAbs e) = Grab : (compile e)
compile (DBApp e1 e2) = (Push (compile e1)) : (compile e2)

decompile :: [Ins] -> DBExp
decompile [Access n] = DBVar n
decompile (Grab : c) = DBAbs (decompile c)
decompile ((Push c1) : c2) = DBApp (decompile c1) (decompile c2)




--- state ---
type State = (Code, Env, Stk)

initState :: Code -> State
initState c = (c, [], [])

--- state utility ---
getStateCode :: State -> Code
getStateCode (c, e, s) = c

--- transition rules ---
evalm :: State -> State
evalm ((Access i) : c, e, s) = evalm (c', e', s)
                                    where Clo (c', e') = e !! i
evalm (Grab : c, e, (Clo (c', e')) : s) = evalm (c, (Clo (c', e')) : e, s)
evalm (Push c' : c, e, s) = evalm (c, e, (Clo (c', e)) : s)
evalm (Grab : c, e, []) = (Grab : c, e, [])
