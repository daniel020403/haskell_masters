module DeBruijn where



import ParseExpression





data DBExp = DBVar Int
           | DBAbs DBExp
           | DBApp DBExp DBExp
           deriving Show
           





getIndex :: [String] -> String -> Int
getIndex lst v
    | lst == [] = error "Undefined Variable!"
    | head lst == v = 0
    | otherwise = 1 + (getIndex (tail lst) v)

lc2dbwithenv :: LCExp -> [String] -> DBExp
lc2dbwithenv exp env = case exp of
                            (LCVar v) -> DBVar (getIndex env v)
                            (LCAbs v e) -> DBAbs (lc2dbwithenv e (v : env))
                            (LCApp e1 e2) -> DBApp (lc2dbwithenv e1 env) (lc2dbwithenv e2 env)

-- convert lambda expression using variable names into de Bruijn term --
lc2db :: LCExp -> DBExp
lc2db exp = lc2dbwithenv exp []






getVar :: Int -> Int -> [String] -> String
getVar idx dpth env 
    | env == [] || idx >= (length env) || idx > dpth = error "Undefined Variable!"
    | otherwise = env !! idx

newVar :: Int -> String
newVar i = "x" ++ (show i)

db2lcwithenv :: DBExp -> [String] -> Int -> LCExp
db2lcwithenv exp env depth = case exp of
                                (DBVar i) -> LCVar (getVar i depth env)
                                (DBAbs e) -> LCAbs (newVar depth) (db2lcwithenv e ((newVar depth) : env) (depth + 1))
                                (DBApp e1 e2) -> LCApp (db2lcwithenv e1 env depth) (db2lcwithenv e2 env depth)

-- convert de Bruijn term into lambda expression using variable names --
db2lc :: DBExp -> LCExp
db2lc exp = db2lcwithenv exp [] 0






-- lifting operation --
lift :: Int -> Int -> DBExp -> DBExp
lift n k exp@(DBVar i)
    | i < k = exp
    | otherwise = DBVar (i + n)
lift n k exp = case exp of
                    (DBAbs e) -> DBAbs (lift n (k + 1) e)
                    (DBApp e1 e2) -> DBApp (lift n k e1) (lift n k e2)






-- substitution --
subst :: DBExp -> (Int, DBExp) -> DBExp
subst exp@(DBVar i) (k, m)
    | i < k = DBVar i
    | i == k = lift k 0 m
    | i > k = DBVar (i - 1)
subst exp (k, m) = case exp of
                        (DBAbs e) -> DBAbs (subst e ((k + 1), m))
                        (DBApp e1 e2) -> DBApp (subst e1 (k, m)) (subst e2 (k, m))





--- beta reduction (normal order) ---
noReduce :: DBExp -> [DBExp]
noReduce (DBVar _) = []
noReduce (DBApp (DBAbs e1) e2) = [subst e1 (0, e2)]
noReduce (DBApp e1 e2) = case noReduce e1 of
                            [e1'] -> [DBApp e1' e2]
                            [] -> case noReduce e2 of
                                    [e2'] -> [DBApp e1 e2']
                                    [] -> []
noReduce (DBAbs e) = case noReduce e of
                        [e'] -> [DBAbs e']
                        [] -> []

--- de Bruijn evaluation---
evaldb :: DBExp -> DBExp
evaldb e = case noReduce e of
                [e'] -> evaldb e'
                [] -> e
