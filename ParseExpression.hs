-- Daniel B. Garcia
-- ICT-203
-- Feb 10, 2014
--

module ParseExpression where

-- import Parsing
import ParsingDaniel
-- some modification were made on the original implementation of Parsing.lhs (from the Book "Programming in Haskell" by Graham Huton) to fit the changes on my version of ghci


-- ******************************
-- Concrete Syntax
-- ******************************
-- Variable: x
-- Abstraction: (\\x.x)
-- Application: (x y)
-- 
-- ******************************
-- Abstract Syntax
-- ******************************
-- Variable: LCVar String
-- Abstraction: LCAbs String LCExp
-- Application: LCApp LCExp LCExp
--
-- *****************************
-- Sample Output
-- *****************************
-- *Main> con2Abs "(\\x.(\\y.(x y)))"
-- LCAbs "x" (LCAbs "y" (LCApp (LCVar "x") (LCVar "y")))
--
-- *Main> abs2Con (LCAbs "x" (LCAbs "y" (LCApp (LCVar "x") (LCVar "y"))))
-- "(\\x.(\\y.(x y)))"


data LCExp = LCVar String
           | LCAbs String LCExp
           | LCApp LCExp LCExp
           deriving Show

lcVariable :: Parser LCExp
lcVariable = do x <- identifier
                return (LCVar x)

lcAbstraction :: Parser LCExp
lcAbstraction = do symbol "("
                   symbol "\\"
                   x <- identifier
                   symbol "."
                   y <- lcTerm
                   symbol ")"
                   return (LCAbs x y)

lcApplication :: Parser LCExp
lcApplication = do symbol "("
                   x <- lcTerm
                   y <- lcTerm
                   symbol ")"
                   return (LCApp x y)

lcTerm :: Parser LCExp
lcTerm = lcVariable +++ lcAbstraction +++ lcApplication

-- concrete syntax to abstract syntax
con2Abs :: String -> LCExp
con2Abs inp = case parse lcTerm inp of
                   [(x, [])] -> x
                   [(_, out)] -> error ("Unused input: " ++ out)
                   [] -> error "Invalid input!"

-- abtract syntax to concrete syntax
abs2Con :: LCExp -> String
abs2Con exp = case exp of
                   (LCVar v) -> v
                   (LCAbs v e) -> "(\\" ++ v ++ "." ++ abs2Con(e) ++")"
                   (LCApp e1 e2) -> "(" ++ abs2Con(e1) ++ " " ++ abs2Con(e2) ++ ")"
