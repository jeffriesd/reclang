module TestParser where 

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import TestData 
import MyLexer (Token)
import MyParser
import Types 

parserTests :: [Test]
parserTests = 
  [ parserTest1
  , parserTest2
  , parserTest3
  , parserTest4
  , parserTest5
  ]

-- take some input tokens and an expected output command, 
-- then assert that actual parser output matches expected 
testTokens :: (Show c, Eq c) => ([Token] -> c) -> [Token] -> c -> Assertion
testTokens parser tokens expCommand = 
  let actualCommand = parser tokens
   in assertEqual "test parser output" expCommand actualCommand

-- test parsing of programs that do not 
-- involve function definitions 
makeBaseParserTest :: String -> [Token] -> BaseCommand -> Test
makeBaseParserTest name inputTokens expCommand = 
    testCase ("Parser: " ++ name) $ testTokens baseParser inputTokens expCommand

-- test parsing of programs that may involve function definitions
makeParserTest :: String -> [Token] -> Command -> Test
makeParserTest name inputTokens expCommand = 
    testCase ("Parser: " ++ name) $ testTokens mainParser inputTokens expCommand

parserTest1 :: Test
parserTest1 = makeBaseParserTest
              "arithmetic, loops, conditionals"
              tokens1 bcmd1

parserTest2 :: Test
parserTest2 = makeBaseParserTest
              "While loop"
              tokens2 bcmd2

parserTest3 :: Test
parserTest3 = makeBaseParserTest
              "Max of two ints"
              tokens3 bcmd3

parserTest4 :: Test
parserTest4 = makeBaseParserTest
              "Division program"
              tokens4 bcmd4

parserTest5 :: Test
parserTest5 = makeParserTest
              "Mod/div function definitions"
              tokens5 cmd5
