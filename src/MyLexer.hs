
module MyLexer where 

import Types 
import Data.Char

-- tokens for lexer. Token is either a variable identifer, a number, 
-- or a symbol (keyword or +,:=, etc.) 
data Token = Ident String
           | Number Int
           | Symbol String
           deriving (Eq, Show)

-- detect whitespace characters 
layout :: Char -> Bool
layout = flip elem " \n\t\r"

-- test whether a string is a keyword 
keyword :: String -> Bool
keyword = flip elem ["IF", "THEN", "ELSE", "ENDIF", "WHILE", "DO", "END", "DEF", "BEGIN", "RET"]

keycheck :: String -> Token
keycheck (c:s)  -- Symbols may or may not start with a letter
  | keyword (c:s) = Symbol (c:s)
  | isLetter c    = Ident  (c:s)
  | otherwise     = Symbol (c:s)
keycheck _ = error "Empty string token"

-- detect letter/digit/underscore/apostrophe 
letDigEtc :: Char -> Bool
letDigEtc c = isDigit c || isLetter c || elem c "'_"

-- detect symbols that can appear in programs 
symbolchar :: Char -> Bool
symbolchar = flip elem "*->:=;," 

-- get integer value of character 
intOfDigit :: Char -> Int
intOfDigit = subtract (ord '0') . ord 

-- helper function to lex a string as a particular word
getword :: String -> String -> [Token]
getword l [] = [keycheck (reverse l)]
getword l (a:x) = if letDigEtc a
                    then getword (a:l) x
                    else (keycheck (reverse l)) : (lexer (a:x))

-- helper function to lex a string as a particular symbol 
getsymbol :: String -> String -> [Token]
getsymbol l [] = [keycheck (reverse l)]
getsymbol l (a:x) = if symbolchar a
                      then getsymbol (a:l) x
                      else (keycheck (reverse l)) : (lexer (a:x))

-- helper function to lex a string as an integer
getnum :: Int -> String -> [Token]
getnum n [] = [Number n]
getnum n (a:x) = if isDigit a
                  then getnum (10 * n + intOfDigit a) x
                  else (Number n) : (lexer (a:x))


-- read a string and produce a list of tokens
-- exit with error if string cannot be lexed 
lexer :: String -> [Token]
lexer [] = []
lexer (a:x) 
 -- ignore whitespace 
 | layout a = lexer x  
 -- handle parentheses
 | a == '(' = Symbol "(" : (lexer x) 
 | a == ')' = Symbol ")" : (lexer x)
 -- handle words 
 | isLetter a = getword [a] x 
 -- handle numbers 
 | isDigit a = getnum (intOfDigit a) x 
 -- handle other symbols 
 | symbolchar a = getsymbol [a] x 
 | otherwise = error ("Lexical error : unrecognized token '" ++ (a:x) ++ "'")

