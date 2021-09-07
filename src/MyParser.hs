module MyParser where 

import MyLexer
import Types

-- Parser of type 'a' takes a list of tokens and either returns 
-- an element of 'a' and a list of remaining tokens, or reports failure. 
type Parser a = [Token] -> Maybe (a, [Token])

-- parser combinator for alternative (use parser1 or parser2)
(<|>) :: Parser a -> Parser a -> Parser a
(parser1 <|> parser2) s =
   let parser2IfNothing Nothing = parser2 s
       parser2IfNothing x       = x
   in
     parser2IfNothing (parser1 s)

-- utility function to modify the result of a parser
modify :: Parser a -> (a -> b) -> Parser b
(parser `modify` f) s =
   let modResult Nothing      = Nothing
       modResult (Just (x,y)) = Just (f x,y)
   in
     modResult (parser s)

-- parser combinator for sequential parsing (use parser1 then parser2)
(<&>) :: Parser a -> Parser b -> Parser (a, b)
(parser1 <&> parser2) s =
   let parser2After Nothing      = Nothing
       parser2After (Just (x,s)) = (parser2 `modify` (\y -> (x,y))) s
   in
     parser2After (parser1 s)

emptyseq s = Just ([],s)

-- parse an element of 'a' or an empty list of 'a'
optional :: Parser a -> Parser [a] 
optional pr = (pr `modify` (consonto [])) <|> emptyseq
               where consonto [] x = [x]

-- parse a number
number :: Parser Val
number (Number n : s) = Just (n, s)
number _              = Nothing

-- parse a variable
variable :: Parser Variable
variable (Ident v : s) = Just (v, s)
variable _                = Nothing

-- parse a particular literal 
literal :: String -> Parser String
literal str (Symbol str' : s) 
  | str == str' = Just (str, s)
  | otherwise   = Nothing
literal _ _ = Nothing

-- parse an expression 
expr :: Parser Expr
expr = aexpr <&> optional (literal ">" <&> aexpr) `modify` optGreater
      where optGreater (e, [])         = e 
            optGreater (e1, [(_, e2)]) = Greater e1 e2
            optGreater _               = error "impossible"

aexpr :: Parser Expr
aexpr = bexpr <&> optional (literal "-" <&> aexpr) `modify` optSub
      where optSub (e, [])         = e
            optSub (e1, [(_, e2)]) = Minus e1 e2
            optSub _               = error "impossible"

bexpr :: Parser Expr
bexpr = cexpr <&> optional (literal "*" <&> bexpr) `modify` optMult
      where optMult (e, [])         = e
            optMult (e1, [(_, e2)]) = Times e1 e2
            optMult _               = error "impossible"

cexpr :: Parser Expr
cexpr = 
    (literal "(" <&> expr <&> literal ")") `modify` unparenth
    <|> funccall
    <|> (number `modify` Const)
    <|> (variable `modify` Var)
    where unparenth ((_lp, ex), _rp) = ex
    
funccall :: Parser Expr
funccall = variable <&> literal "(" <&> optergs <&> literal ")" `modify` mkFuncNode
           where mkFuncNode (((v, _lp), es), _rp) = Call v es

whilecom :: Parser BaseCommand
whilecom = literal "WHILE" <&> expr <&> literal "DO" 
           <&> command <&> literal "END" `modify` mkWhileNode
             where mkWhileNode ((((_w, e), _d), cmd), _end) = While e cmd

ifcom :: Parser BaseCommand
ifcom = literal "IF" <&> expr <&> literal "THEN" 
        <&> command <&> literal "ELSE" <&> command <&> literal "ENDIF" `modify` mkIfNode
          where mkIfNode ((((((_i, e), _t), cmd1), _el), cmd2), _end) = Cond e cmd1 cmd2

assign :: Parser BaseCommand
assign = variable <&> literal ":=" <&> expr `modify` mkAssignNode
           where mkAssignNode ((v, _), e) = Assign v e

ergs :: Parser Ergs
ergs = expr <&> optional (literal "," <&> ergs) `modify` mkErgsNode
       where mkErgsNode (v, []) = [v]
             mkErgsNode (v, [(_c, es)]) = (v : es)

optergs :: Parser Ergs
optergs = optional ergs `modify` optErgs
        where optErgs [] = []
              optErgs [es] = es

unitcom :: Parser BaseCommand
unitcom = whilecom <|> (ifcom <|> assign) 


command :: Parser BaseCommand
command = unitcom <&> optional (literal ";" <&> command) `modify` optSeq
            where optSeq (c, []) = c
                  optSeq (c1, [(_, c2)]) = Seq c1 c2
                  optSeq _               = error "impossible"

funcdef :: Parser Command
funcdef = literal "DEF" <&> variable <&> literal "(" <&> optargs <&> literal ")" <&> literal "BEGIN"
        <&> command <&> literal "RET" <&> expr <&> literal "END" `modify` mkDefNode
          where mkDefNode (((((((((_d, fname), _lp), aNames), _rp), _b), com), _r), ret), _end) = Def fname aNames com ret

args :: Parser Args
args = variable <&> optional (literal "," <&> args) `modify` mkArgsNode
       where mkArgsNode (v, []) = [v]
             mkArgsNode (v, [(_c, as)]) = (v : as)

optargs = optional args `modify` optArgs
        where optArgs [] = []
              optArgs [as] = as

-- function to lift BaseCommand to Command
commandWrapper :: Parser Command 
commandWrapper toks = wrapResult res
                      where wrapResult Nothing = Nothing
                            wrapResult (Just (com, toks)) = Just (Normal com, toks)
                            res = unitcom toks

-- combine 'normal' command parser with function definition parser
unitMainCommand :: Parser Command
unitMainCommand = commandWrapper <|> funcdef

-- mainCommand is used to parse programs that 
-- are sequences of BaseCommands and function definitions 
mainCommand :: Parser Command
mainCommand = unitMainCommand <&> optional (literal ";" <&> mainCommand) `modify` optSeq
            where optSeq (c, []) = c
                  optSeq (c1, [(_, c2)]) = Seq' c1 c2

-- turn a token into a string for error reporting 
lit :: Token -> String
lit (Ident s)  = s ++ " "
lit (Symbol s) = s ++ " "
lit (Number n) = show n ++ " "

-- Take output from parser and return output 
-- if there are no remaining characters. 
-- Otherwise exit with error message
report :: Maybe (a, [Token]) -> a
report Nothing = error "Parse error"
report (Just (c, [])) = c
report (Just (c, xs)) = 
  error (stringwith ("Syntax error\n Unparsed:-\n", " ", "\n") (map lit xs))
    where stringwith (front, sep, back) ls = 
              let sepback [] = back
                  sepback [a] = a ++ back
                  sepback (a:xs) = a ++ sep ++ sepback xs
              in 
                front ++ sepback ls

-- for testing purposes only. 
-- baseParser is used to parser BaseCommands, 
-- which are not allowed to include function definitions  
baseParser :: [Token] -> BaseCommand
baseParser = report . command


-- mainParser is used to parse top-level programs, 
-- which consist of sequences of function definitions
-- and BaseCommands 
mainParser :: [Token] -> Command
mainParser = report . mainCommand
