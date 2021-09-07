module Types where 

-- variables are represented as strings 
type Variable = String

-- base values of expressions are integers 
type Val = Int

-- Type synonym for variable stores (also referred to as contexts). 
-- Variable stores are represented as functions, where each variable 
-- represents either an integer or a user-defined function. 
type Store = Variable -> Either Val FuncDef

-- Additional type synonyms for function 
-- definitions:
-- Args - list of variables, used in parsing of function definition 
-- Ergs - list of expressions, used in parsing of function call 
type Args = [Variable]
type Ergs = [Expr]

-- A user-defined function takes a list of argument values and produces
-- a return value. 
--
-- Functions are call-by-value so arguments 
-- (which are expressions) get evaluated in the
-- calling context before calling the function. 
type FuncDef = [Val] -> Val 

-- top level commands are function definitions, base commands, 
-- or sequences of top level commands 
-- (function definitions may only appear at top-level, 
-- since the body of a func. def. is a BaseCommand and 
-- BaseCommand doesn't include function definitions)
data Command = Def Variable Args BaseCommand Expr
             | Normal BaseCommand
             | Seq' Command Command
             deriving (Eq, Show)

-- base commands are assignments, conditionals, while loops, 
-- or sequences of base commands
data BaseCommand = Assign Variable Expr
             | Cond Expr BaseCommand BaseCommand
             | While Expr BaseCommand
             | Seq BaseCommand BaseCommand
             deriving (Eq, Show)


-- the separation of commands into BaseCommand and Command 
-- ensures that function definitions are not nested. 

-- expression grammar allows constants, variables, 
-- arithmetic expressions, or function calls  
data Expr = Const Val
          | Var Variable
          | Minus Expr Expr
          | Times Expr Expr
          | Greater Expr Expr
          | Call Variable Ergs
          deriving (Eq, Show)

