module MyParser where



import Data.Char




newtype Parser a = Parser (String -> [(a, String)])



class MyMonad m where
    ret   :: a -> m a
    (=>>) :: m a -> (a -> m b) -> m b

instance MyMonad Parser where
    ret a    = Parser (\cs -> [(a, cs)])
    p =>> f  = Parser (\cs -> concat [
                                parse (f a) cs' |
                                (a, cs') <- parse p cs
                                ])
                                
parse :: Parser a -> String -> [(a, String)]
parse (Parser a) inp = a inp



class MyMonad m => MyMonadZero m where
    zero :: m a
    
class MyMonadZero m => MyMonadPlus m where
    (+++) :: m a -> m a -> m a
    
instance MyMonadZero Parser where
    zero = Parser (\cs -> [])

instance MyMonadPlus Parser where
    p +++ q = Parser (\cs -> case parse p cs of
                                []  -> parse q cs
                                [x] -> [x])


    
item :: Parser Char
item = Parser (\cs -> case cs of
                        ""      -> []
                        (c:cs)  -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = item =>> \c ->
        if p c
            then ret c
            else zero

char :: Char -> Parser Char
char c = sat (c ==)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

string :: String -> Parser String
string ""     = ret ""
string (c:cs) = char c    =>> \_ ->
                string cs =>> \_ ->
                ret (c:cs)

many :: Parser a -> Parser [a]
many p = many1 p +++ ret []

many1 :: Parser a -> Parser [a]
many1 p = p =>>      \x ->
          many p =>> \xs ->
          ret (x:xs)

ident :: Parser String
ident = lower =>>         \x ->
        many alphanum =>> \xs ->
        ret (x:xs)

nat :: Parser Int
nat = many1 digit =>> \xs ->
      ret (read xs)

int :: Parser Int
int = (char '-' =>> \_ ->
       nat      =>> \n ->
       ret (-n))
      +++ nat

space :: Parser ()
space = many (sat isSpace) =>> \_ ->
        ret ()

token :: Parser a -> Parser a
token p = space =>> \_ ->
          p     =>> \v ->
          space =>> \_ ->
          ret v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)