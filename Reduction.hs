-- Daniel B. Garcia
-- ICT-203
-- Reduction

module Reduction where

import ParseExpression
import Data.List

type Ide = String

-- free variable --
freeVar :: LCExp -> [Ide]
freeVar (LCVar x) = [x]
freeVar (LCApp e1 e2) = union (freeVar e1) (freeVar e2)
freeVar (LCAbs x e) = delete x (freeVar e)

-- substitution --
subst :: LCExp -> (Ide, LCExp) -> LCExp
subst (LCVar y) (x, exp)
      | x == y = exp
      | otherwise = (LCVar y)
subst (LCApp e1 e2) (x, exp) = LCApp (subst e1 (x, exp)) (subst e2 (x, exp))
subst (LCAbs y e) (x, exp)
      | x == y = LCAbs y e
      | y `notElem` (freeVar exp) = LCAbs y (subst e (x, exp))
      | otherwise = LCAbs z (subst (subst e (y, LCVar z)) (x, exp))
          where z = newIde (y:(freeVar exp))

newIde :: [Ide] -> Ide
newIde xs = concat xs

-- beta reduction --
-- returns [e] if reducible, [] otherwise
betaReduce :: LCExp -> [LCExp]
betaReduce (LCApp (LCAbs x e1) e2) = [subst e1 (x, e2)]
betaReduce _ = []

-- eta reduction --
-- returns [e] if reducible, [] otherwise
etaReduce :: LCExp -> [LCExp]
etaReduce (LCAbs x (LCApp e (LCVar y)))
          | (x == y) && (x `notElem` freeVar(e)) = [e]
          | otherwise = []
etaReduce _ = []

-- leftmost-outermost reduction --
loReduce :: LCExp -> [LCExp]
loReduce (LCVar _) = []
loReduce (LCApp (LCAbs x e1) e2) = [subst e1 (x, e2)]
loReduce (LCApp e1 e2) = case loReduce e1 of
                         [e1'] -> [LCApp e1' e2]
                         [] -> case loReduce e2 of
                               [e2'] -> [LCApp e1 e2']
                               [] -> []
loReduce (LCAbs x e'@(LCApp e (LCVar y)))
         | (x == y) && (x `notElem` freeVar(e)) = [e]
         | otherwise = case loReduce e' of
                       [e''] -> [LCAbs x e'']
                       [] -> []
loReduce (LCAbs x e) = case loReduce e of
                       [e'] -> [LCAbs x e']
                       [] -> []

-- leftmost-innermost reduction --
liReduce :: LCExp -> [LCExp]
liReduce (LCVar _) = []
liReduce (LCApp e@(LCAbs x e1) e2) = case liReduce e2 of
                                     [e2'] -> [LCApp e e2']
                                     [] -> [subst e1 (x, e2)]
liReduce (LCApp e1 e2) = case liReduce e1 of
                         [e1'] -> [LCApp e1' e2]
                         [] -> case liReduce e2 of
                               [e2'] -> [LCApp e1 e2']
                               [] -> []
liReduce (LCAbs x e'@(LCApp e (LCVar y)))
         | (x == y) && (x `notElem` freeVar(e)) = [e]
         | otherwise = case liReduce e of
                       [e'] -> [LCAbs x e']
                       [] -> []
liReduce (LCAbs x e) = case liReduce e of
                       [e'] -> [LCAbs x e']
                       [] -> []

-- dbg leftmost-outermost reduction --
myloReduce :: LCExp -> LCExp
myloReduce e = case loReduce e of
               [e'] -> myloReduce e'
               [] -> e

-- dbg leftmost-innermost reduction --
myliReduce :: LCExp -> LCExp
myliReduce e = case liReduce e of
               [e'] -> myliReduce e'
               [] -> e
