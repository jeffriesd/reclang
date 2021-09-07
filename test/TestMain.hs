import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid (mempty)
import TestLexer (lexerTests)
import TestParser (parserTests)
import TestInterp (interpTests)

opts :: RunnerOptions
opts = mempty { ropt_color_mode = Just ColorAlways , ropt_hide_successes = Just False }

tests :: [Test]
tests = [ testGroup "Lexer tests" lexerTests
        , testGroup "Parser tests" parserTests
        , testGroup "Interpreter tests" interpTests ]

-- run all tests 
main :: IO ()
main = defaultMainWithOpts tests opts 
