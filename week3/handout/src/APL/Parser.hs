module APL.Parser (parseAPL,lInteger, parseTest,pExp,lVName) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String


-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x


-- parse one or more decimal digits
lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lVName :: Parser VName 
lVName = lexeme $ try $ do 
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlpha)

lBool :: Parser Bool 
lBool = lexeme $
  choice [
    const True <$> (lKeyword "true"),
    const False <$> (lKeyword "false")
  ]

keywords :: [String]
keywords = 
  [ "if",
    "then",
    "else"
  ]

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s


-- parseTest lInteger "123"

lexeme :: Parser a -> Parser a 
lexeme p = p <* space

pAtom :: Parser Exp
pAtom = choice 
    [
        CstInt <$> lInteger,
        CstBool <$> lBool,
        Var <$> lVName,
        lString "(" *> pExp <* lString ")"
    ]

pExp0 :: Parser Exp
pExp0 = pAtom >>= chain
    where
        chain x = 
            choice 
              [ do
                    lString "+"
                    y <- pAtom
                    chain $ Add x y,
                do
                    lString "-"
                    y <- pAtom
                    chain $ Sub x y,
                do
                    lString "*"
                    y <- pAtom
                    chain $ Mul x y,
                do
                    lString "/"
                    y <- pAtom
                    chain $ Div x y,
                pure x
                ]

pExp :: Parser Exp
pExp = pExp0


plExp :: Parser Exp
pLExp = 
    choice
      [ If
        <$> (lKeyword "if" *> pExp0)
        <*> (lKeyword "then" * pExp0)
        <*> (lKeyword "else" *>pExp0),
        pAtom
      ]


pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where 
    chain x = 
      choice 
        [ do 
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do 
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          pure x
        ]

