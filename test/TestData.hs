module TestData where

import Types
import MyLexer (Token(..))

-- This file contains some data for testing
-- the lexer, parser and interpreter modules. 
-- The data takes one of three forms: 
--   - strings
--   - lists of tokens
--   - commands 
--
-- lexing strX should produce tokensX 
-- and parsing tokensX should produce cmdX

-------------------------------------------------------
-- STRINGS 
-------------------------------------------------------

-- code text #1 
-- arithmetic, loops, and conditionals 
str1 = "A := 0 - 1;\
        \X := 10;\n\
        \X := (4 * (2 - 3 - 1) - 1) - (A * (X - A)); \n\
        \Y := 3;\n\
        \IF X > (Y * Y) - (4 * (0 - Y)) THEN \n\
        \  WHILE X > 0 DO \n\
        \    Y := Y * Y\n\
        \  END\n\
        \ELSE\n\
        \  IF X > 100 THEN  \n\
        \    Y := 0\n\
        \  ELSE\n\
        \    Y := 0 - 1   \n\
        \  ENDIF          \n\
        \ENDIF; \n\
        \Y := 4 * Y"



-- test 2 -- while loop with multiplication
str2 = "WHILE X > Y DO\n\
        \IF X > Y * Y\n\
        \  THEN \n\
        \    X := X * 2 \n\
        \  ELSE\n\
        \    X := X * (0-2) \n\
        \ENDIF\n\
        \ END"


-- test 3 
str3 = "Z := 9;\n\
        \IF X > Y THEN\n\
        \  IF X > Z THEN MAX := X ELSE MAX := Z ENDIF \n\
        \ELSE\n\
        \  IF Y > Z THEN MAX := Y ELSE MAX := Z ENDIF \n\
        \ENDIF"

-- test 4 -- division
str4 = "IF A > B\n\
        \  THEN MAX := A\n\
        \  ELSE MAX := B\n\
        \ENDIF;\n\
        \MIN := (A - (0 - B)) - MAX;\n\
        \Q := 0;\n\
        \R := 0;\n\
        \WHILE MAX > MIN - 1 DO\n\
        \  Q := Q - (0 - 1);\n\
        \  MAX := MAX - MIN\n\
        \END;\n\
        \REM := MAX"

-- test 5 -- function definition using test 4 as body
str5 = "DEF MOD(A, B) BEGIN " ++ str4 ++ " RET REM END; \n\
      \ DEF DIV(A, B) BEGIN " ++ str4 ++ " RET Q END; \n\
      \ T21mod6 := MOD(21, 6); T100div3 := DIV(100, 3)"

fibCode = "DEF FIB(N) BEGIN\n\
          \   IF 2 > N THEN\n\
          \     R := N\n\
          \   ELSE\n\
          \     R := FIB(N-1) - (0 - FIB(N-2))\n\
          \   ENDIF\n\
          \   RET R\n\
          \ END; \n\
          \ test_a := FIB(7); \n\
          \ test_b := FIB(8); \n\
          \ test_c := FIB(9)"

-- compute length of collatz sequence beginning with N
-- initial store is store3 which has MOD/DIV definitions
collatzCode = "DEF COLLATZ(N) BEGIN\n\
             \   IF (N - 1) > 0 THEN\n\
             \     IF MOD(N, 2) > 0 THEN\n\
             \       RES := COLLATZ((3 * N) - (0-1)) - (0-1)\n\
             \     ELSE\n\
             \       RES := COLLATZ(DIV(N, 2)) - (0-1)\n\
             \     ENDIF\n\
             \   ELSE\n\
             \     RES := 1\n\
             \   ENDIF \n\
             \   RET RES \n\
             \ END; \n\
             \ test_a := COLLATZ(100);\n\
             \ test_b := COLLATZ(1024);\n\
             \ test_c := COLLATZ(1)" 


-------------------------------------------------------
-- TOKENS 
-------------------------------------------------------

tokens1 :: [Token]
tokens1 = [ Ident "A", Symbol ":=", Number 0, Symbol "-", Number 1, Symbol ";", 
            Ident "X", Symbol ":=", Number 10, Symbol ";", 
            Ident "X", Symbol ":=", Symbol "(", Number 4, Symbol "*", 
              Symbol "(", Number 2, Symbol "-", Number 3, Symbol "-", Number 1, Symbol ")", 
                Symbol "-", Number 1, Symbol ")", Symbol "-", Symbol "(", Ident "A", Symbol "*", 
                Symbol "(", Ident "X", Symbol "-", Ident "A", Symbol ")", Symbol ")", Symbol ";", 
                Ident "Y", Symbol ":=", Number 3, Symbol ";", 
            Symbol "IF", Ident "X", Symbol ">", Symbol "(", Ident "Y", Symbol "*", Ident "Y", Symbol ")",
                Symbol "-", Symbol "(", Number 4, Symbol "*", 
                  Symbol "(", Number 0, Symbol "-", Ident "Y", Symbol ")", Symbol ")",
              Symbol "THEN", Symbol "WHILE", Ident "X", Symbol ">", Number 0, Symbol "DO", 
                  Ident "Y", Symbol ":=", Ident "Y", Symbol "*", Ident "Y", Symbol "END", 
              Symbol "ELSE", Symbol "IF", Ident "X", Symbol ">", Number 100, 
                Symbol "THEN", Ident "Y", Symbol ":=", Number 0, 
                Symbol "ELSE", Ident "Y", Symbol ":=", Number 0, Symbol "-", Number 1, Symbol "ENDIF", 
                Symbol "ENDIF", Symbol ";", 
            Ident "Y", Symbol ":=", Number 4, Symbol "*", Ident "Y"]

tokens2 = 
  [ Symbol "WHILE", Ident "X", Symbol ">", Ident "Y", Symbol "DO",
    Symbol "IF", Ident "X", Symbol ">", Ident "Y", Symbol "*", Ident "Y", Symbol "THEN",
    Ident "X", Symbol ":=", Ident "X", Symbol "*", Number 2,  Symbol "ELSE",
    Ident "X", Symbol ":=", Ident "X", Symbol "*", 
      Symbol "(", Number 0, Symbol "-", Number 2, Symbol ")", 
    Symbol "ENDIF", Symbol "END" ]


tokens3 = 
 [ Ident "Z", Symbol ":=", Number 9, Symbol ";",
   Symbol "IF", Ident "X", Symbol ">", Ident "Y", Symbol "THEN",
     Symbol "IF", Ident "X", Symbol ">", Ident "Z", Symbol "THEN",
       Ident "MAX", Symbol ":=", Ident "X", Symbol "ELSE",
       Ident "MAX", Symbol ":=", Ident "Z", 
     Symbol "ENDIF", 
   Symbol "ELSE",
     Symbol "IF", Ident "Y", Symbol ">", Ident "Z", Symbol "THEN",
       Ident "MAX", Symbol ":=", Ident "Y", Symbol "ELSE",
       Ident "MAX", Symbol ":=", Ident "Z", 
     Symbol "ENDIF", 
   Symbol "ENDIF" ]

tokens4 = 
  [ Symbol "IF", Ident "A", Symbol ">", Ident "B", Symbol "THEN", 
    Ident "MAX", Symbol ":=", Ident "A", 
    Symbol "ELSE", Ident "MAX", Symbol ":=", Ident "B", Symbol "ENDIF", Symbol ";", 
    Ident "MIN", Symbol ":=", Symbol "(", Ident "A", Symbol "-", 
      Symbol "(", Number 0, Symbol "-", Ident "B", Symbol ")", Symbol ")", Symbol "-", Ident "MAX", Symbol ";", 
    Ident "Q", Symbol ":=", Number 0, Symbol ";", Ident "R", Symbol ":=", Number 0, Symbol ";", 
    Symbol "WHILE", Ident "MAX", Symbol ">", Ident "MIN", Symbol "-", Number 1, Symbol "DO", 
      Ident "Q", Symbol ":=", Ident "Q", Symbol "-", Symbol "(", Number 0, Symbol "-", Number 1, Symbol ")", Symbol ";", 
      Ident "MAX", Symbol ":=", Ident "MAX", Symbol "-", Ident "MIN", Symbol "END", Symbol ";", 
    Ident "REM", Symbol ":=", Ident "MAX" ]

tokens5 = 
  [ Symbol "DEF",Ident "MOD",Symbol "(",Ident "A",Symbol ",",Ident "B",Symbol ")",Symbol "BEGIN" ] 
  ++ tokens4 ++ 
  [ Symbol "RET", Ident "REM", Symbol "END", Symbol ";", 
    Symbol "DEF",Ident "DIV",Symbol "(",Ident "A",Symbol ",",Ident "B",Symbol ")",Symbol "BEGIN" ] 
  ++ tokens4 ++ 
  [ Symbol "RET", Ident "Q", Symbol "END", Symbol ";", 
    Ident "T21mod6", Symbol ":=", Ident "MOD", Symbol "(", Number 21, Symbol ",", Number 6, Symbol ")", Symbol ";",
    Ident "T100div3", Symbol ":=", Ident "DIV", Symbol "(", Number 100, Symbol ",", Number 3, Symbol ")" ] 

-------------------------------------------------------
-- COMMANDS
-------------------------------------------------------
bcmd1 = Seq (Assign "A" (Minus (Const 0) (Const 1)))
    (Seq (Assign "X" (Const 10))
    (Seq (Assign "X" (Minus (Minus (Times (Const 4) 
          (Minus (Const 2) (Minus (Const 3) (Const 1)))) (Const 1))
     (Times (Var "A") (Minus (Var "X") (Var "A")))))
   (Seq (Assign "Y" (Const 3))
   (Seq (Cond (Greater (Var "X") (Minus (Times (Var "Y") (Var "Y")) 
                (Times (Const 4) (Minus (Const 0) (Var "Y")))))
           (While (Greater (Var "X") (Const 0))
             (Assign "Y" (Times (Var "Y") (Var "Y"))))
           (Cond (Greater (Var "X") (Const 100))
                (Assign "Y" (Const 0))
                (Assign "Y" (Minus (Const 0) (Const 1)))))
        (Assign "Y" (Times (Const 4) (Var "Y")))))))

cmd1 = Normal bcmd1

bcmd2 = While (Greater (Var "X") (Var "Y"))
        (Cond (Greater (Var "X") (Times (Var "Y") (Var "Y")))
            (Assign "X" (Times (Var "X") (Const 2)))
            (Assign "X" (Times (Var "X") (Minus (Const 0) (Const 2)))))
cmd2 = Normal bcmd2

bcmd3 = Seq (Assign "Z" (Const 9))
       (Cond (Greater (Var "X") (Var "Y"))
         (Cond (Greater (Var "X") (Var "Z"))
           (Assign "MAX" (Var "X"))
           (Assign "MAX" (Var "Z")))
         (Cond (Greater (Var "Y") (Var "Z"))
           (Assign "MAX" (Var "Y"))
           (Assign "MAX" (Var "Z"))))
cmd3 = Normal bcmd3
       
bcmd4 = Seq (Cond (Greater (Var "A") (Var "B")) 
              (Assign "MAX" (Var "A")) (Assign "MAX" (Var "B")))
        (Seq (Assign "MIN" (Minus (Minus (Var "A") 
              (Minus (Const 0) (Var "B"))) (Var "MAX")))
        (Seq (Assign "Q" (Const 0))
        (Seq (Assign "R" (Const 0))
        (Seq 
        (While (Greater (Var "MAX") (Minus (Var "MIN" ) (Const 1)))
          (Seq (Assign "Q" (Minus (Var "Q") (Minus (Const 0) (Const 1))))
               (Assign "MAX" (Minus (Var "MAX") (Var "MIN")))))
        (Assign "REM" (Var "MAX"))))))
cmd4 = Normal bcmd4

cmd5 = Seq' (Def "MOD" ["A", "B"] bcmd4 (Var "REM"))
       (Seq' (Def "DIV" ["A", "B"] bcmd4 (Var "Q"))
        (Seq' (Normal (Assign "T21mod6" (Call "MOD" [Const 21, Const 6])))
              (Normal (Assign "T100div3" (Call "DIV" [Const 100, Const 3])))))

