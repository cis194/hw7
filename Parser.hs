{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

{- the Parser monad -}

newtype Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser k) s = k s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
  case s of
    [] -> Nothing
    (c:cs)
      | p c -> Just (c, cs)
      | otherwise -> Nothing

{-| Parse any character regardless of what it is -}
anyChar :: Parser Char
anyChar = satisfy $ \_ -> True

{-| Parse the specific character that is passed in -}
char :: Char -> Parser Char
char c = satisfy $ \c' -> c == c'

{-| Parse an alphabetic character (see Data.Char.isAlpha for more info) -}
alpha :: Parser Char
alpha = satisfy isAlpha

{-| Parse an alphanumeric character (see Data.Char.isAlphaNum for more info) -}
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

{-| Parse a space character (see Data.Char.isSpace for more info) -}
space :: Parser Char
space = satisfy isSpace

{-| Parse a digit character (see Data.Char.isDigit for more info) -}
digit :: Parser Char
digit = satisfy isDigit

{-| Parse a character that is in the passed in list of characters -}
anyOf :: [Char] -> Parser Char
anyOf cs = satisfy $ \c -> c `elem` cs

{-| Parse any character that is not in the passed in list of characters -}
noneOf :: [Char] -> Parser Char
noneOf cs = satisfy $ \c -> not (c `elem` cs)

instance Monad Parser where
  return :: a -> Parser a
  return x = Parser $ \s -> Just (x, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  m >>= k = Parser $ \s ->
    case runParser m s of
      Nothing -> Nothing
      Just (x, s') -> runParser (k x) s'

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

{-| Parse the specific string that is passed in -}
string :: String -> Parser String
string "" = return ""
string (c:cs) =
  char c >>= (\first ->
    string cs >>= (\rest ->
      return (first:rest)
    )
  )


eitherOr :: Parser a -> Parser a -> Parser a
eitherOr parser1 parser2 = Parser $ \s ->
  case runParser parser1 s of
    Nothing -> runParser parser2 s
    justSomething -> justSomething

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = eitherOr

choice :: [Parser a] -> Parser a
choice [] = empty
choice (p:ps) = p <|> choice ps

{-| Parse an integer (with support for negative values) -}
integer :: Parser Int
integer =
  signParser >>= (\sign ->
    oneOrMore digit >>= (\digits ->
      return (read (sign ++ digits))
    )
  )
  where
    signParser = choice [string "+" >> string "", string "-", string ""]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p =
  p >>= (\first ->
    zeroOrMore p >>= (\rest ->
      return (first:rest)
    )
  )


spaces :: Parser ()
spaces = (\_ -> ()) <$> zeroOrMore space


eol :: Parser String
eol = choice
  [ string "\n\r"
  , string "\r\n"
  , string "\n"
  , string "\r"
  ] >> return "\n"


eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((), s) else Nothing
