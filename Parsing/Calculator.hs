import NanoParsec
import Control.Applicative
import Control.Monad

data Expr
    = Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Lit Int
    deriving Show

eval :: Expr -> Int
eval expr = case expr of
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Sub a b -> eval a - eval b
    Lit n   -> n

int :: Parser Expr
int = number >>= return . Lit

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp s f = reserved s >> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $ do
    putStr "> "
    a <- getLine
    print $ eval $ run a
