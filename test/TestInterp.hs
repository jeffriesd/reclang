module TestInterp where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import TestData 
import Interp
import Types 

-- In this file, construct test cases 
-- for the interpreter. Given a command, 
-- an input store and an expected store, 
-- check whether interpreting the command 
-- in the input store produces the expected 
-- output store. 
--
-- also do some tests of
--
-- string -> lexemes -> command -> output store 
--
-- that test all components of the interpreter 

interpTests :: [Test]
interpTests =  
  [ interpTest1
  , interpTest2
  , interpTest3
  , interpTest4
  , interpTest5
  , interpTest6
  , interpTest7
  ] 


-- take a function (command interpreter or full interpreter) 
-- that processes input into a function of type Store -> Store
-- and an input for that function. 
-- Test whether the function applied to its input and an input store
-- produces the expected output store. 
--
-- Since stores are represented as functions, we can't compare
-- them for equality directly. we have to provide a list of 
-- variables to check. 
testCommandGen :: (a -> Store -> Store) -> a -> Store -> Store -> [Variable] -> Assertion
testCommandGen f input inputStore expStore vs = 
  let actualStore   = f input inputStore
      -- fetch values of variables from each store
      -- for comparison 
      actualOutputs = fetch actualStore <$> vs
      expOutputs    = fetch expStore <$> vs
   in assertEqual "test interpreter output" expOutputs actualOutputs


-- take a command, an input store and the expected output store
-- and test whether the interpreter output matches the expected store
-- on a provided list of variables. 
--
testCommand :: Command -> Store -> Store -> [Variable] -> Assertion
testCommand cmd = testCommandGen interpret' cmd

-- take a test description, command, input/output stores, 
-- a list of variables to check, and produce a test 
makeInterpTest :: String -> Command -> Store -> Store -> [Variable] -> Test
makeInterpTest name cmd inputStore expStore vs =
    testCase ("Interpreter: " ++ name) $ testCommand cmd inputStore expStore vs

-- same as makeInterpTest but take string instead of command 
makeInterpStrTest :: String -> String -> Store -> Store -> [Variable] -> Test
makeInterpStrTest name str inputStore expStore vs =
    testCase ("Interpreter: " ++ name) $ testCommandStr str inputStore expStore vs

-- same as testCommand but take string as input and 
-- use run :: String -> Store -> Store
-- to test full lexer, parser, interpreter pipeline 
testCommandStr :: String -> Store -> Store -> [Variable] -> Assertion
testCommandStr str = testCommandGen run str 

-- reminder: 
-- update :: Store -> Variable -> Either Val FuncDef -> Store
-- update s v x produces store with v -> x 
--
-- produce a store from a table of values 
makeStore :: Store -> [(Variable, Val)] -> Store
-- foldr (t :: (Variable, Either Val FuncDef) -> Store -> Store) 
makeStore = foldr (\ (v, x) s -> update s v (Left x))

interpTest1 :: Test
interpTest1 = 
  let expStore = makeStore initial [("A", -1), ("X", 10), ("Y", -4)] 
  in makeInterpTest
     "arithmetic, loops, conditionals" 
     cmd1 initial expStore ["A", "X", "Y"]

-- use output store from command 1 in 
-- next test 
st1 = interpret' cmd1 initial

-- test result of interpreting cmd2 after cmd1
interpTest2 :: Test
interpTest2 = 
  let expStore = makeStore initial [("A", -1), ("X", -20), ("Y", -4)] 
  in makeInterpTest
     "While loop"
     cmd2 st1 expStore ["A", "X", "Y"]
st2 = interpret' cmd2 st1

-- test result of interpreting cmd3 after cmd2
interpTest3 :: Test
interpTest3 = 
  let expStore = makeStore initial 
        [("A", -1), ("X", -20), ("Y", -4), ("Z", 9), ("MAX", 9)] 
  in makeInterpTest
     "Max of two ints"
     cmd3 st2 expStore ["A", "X", "Y", "Z", "MAX"]

st3 = interpret' cmd3 st2
-- also assign A = 20; B = 4 
st3' = makeStore st3 [("A", 20), ("B", 4)] 

-- test result of interpreting cmd4 after cmd3 and after 
-- assigning A = 20 ; B = 4
-- 
-- (program divides 20 by 4) 
interpTest4 :: Test
interpTest4 = 
  let expStore = 
        makeStore initial 
          [("A", 20), ("B", 4), 
           ("X", -20), ("Y", -4),
           ("Z", 9), ("MAX", 0),
           ("MIN", 4), ("Q", 5), ("REM", 0)] 
  in makeInterpTest
     "Division program"
     cmd4 st3' expStore ["A", "B", "X", "Y", "Z", "MAX", "MIN", "Q", "REM"]
st4 = interpret' cmd4 st3'

interpTest5 :: Test
interpTest5 = 
  let expStore = 
        makeStore initial 
          [("A", 20), ("B", 4), 
           ("X", -20), ("Y", -4),
           ("Z", 9), ("MAX", 0),
           ("MIN", 4), ("Q", 5), ("REM", 0),
           ("T21mod6", 3), ("T100div3", 33)
           ] 
  in makeInterpTest
     "Mod/div function definitions"
     cmd5 st4 expStore 
     ["A", "B", "X", "Y", "Z", 
     "MAX", "MIN", "Q", "REM", "T21mod6", "T100div3"]

st5 = interpret' cmd5 st4


-- run some tests on full 
-- lexer -> parser -> interpreter 
-- pipeline

-- test fibonacci function, recursion 
interpTest6 :: Test 
interpTest6 = 
  let expStore = makeStore initial [("test_a", 13), ("test_b", 21), ("test_c", 34)]
  in makeInterpStrTest 
     "Fibonacci function"
     fibCode initial expStore ["test_a", "test_b", "test_c"]

-- compute length of collatz sequence beginning with N.
-- input store is st5 which has MOD/DIV definitions
interpTest7 :: Test 
interpTest7 = 
  let expStore = makeStore initial 
        [("test_a", 26), ("test_b", 11), ("test_c", 1),
         ("T21mod6", 3), ("T100div3", 33)]
  in makeInterpStrTest 
     "Collatz sequence length function"
     collatzCode st5 expStore 
     ["test_a", "test_b", "test_c", "T21mod6", "T100div3"]



