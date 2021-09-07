module TestLexer where 

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import TestData 
import MyLexer (lexer, Token)

-- Lexer tests

lexerTests :: [Test]
lexerTests = 
  [ lexTest1
  , lexTest2
  , lexTest3
  , lexTest4
  , lexTest5
  ]

-- take a string and the expected output lexemes
-- and test whether the lexer output matches the expected output
testString :: String -> [Token] -> Assertion
testString str expTokens = 
  let actualTokens = lexer str
   in assertEqual "test lexer output" expTokens actualTokens

-- take a test description and input/output and produce a test 
makeLexTest :: String -> String -> [Token] -> Test
makeLexTest name inputStr expTokens = 
    testCase ("Lexer: " ++ name) $ testString inputStr expTokens

-- test cases 
lexTest1 :: Test
lexTest1 = makeLexTest 
            "arithmetic, loops, conditionals" 
            str1 tokens1 

lexTest2 :: Test
lexTest2 = makeLexTest 
            "While loop"
            str2 tokens2

lexTest3 :: Test
lexTest3 = makeLexTest
           "Max of two ints"
           str3 tokens3

lexTest4 :: Test
lexTest4 = makeLexTest
           "Division program"
           str4 tokens4

lexTest5 :: Test
lexTest5 = makeLexTest
           "Mod/div function definitions"
           str5 tokens5

