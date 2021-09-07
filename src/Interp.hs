-- Author: Daniel Jeffries
--
-- In this project we implement a lexer, parser and interpreter
-- for an imperative language supporting loops, conditionals, 
-- and recursive function definitions. 
-- This is an extension of a previous parser project 
-- for a similar language that did not support function definitions. 
--
-- The interpreter, combined with the lexer and parser, 
-- takes a string and produces a program embedded in Haskell. 
-- The interpreted programs are functions of type
--
--   Store -> Store 
--
-- where a Store is a function from variables to values 
-- (variables can be mapped to ints or function definitions)
-- representing a program state. 



-- Function definition command takes 4 arguments:
--  - Function name :: Variable
--    - function name is store with leading '$' to prevent
--      users of L from directly accessing the function definition(s)
--      in the store 
--  - Function argument names :: Args (i.e. [Variable])
--  - Function body :: BaseCommand
--  - Return expression :: Expr 
--    - To simplify the logic of return expressions, they
--      must appear at the end of every function definition
--      (using the RET keyword)
--
--  Added keywords: DEF, BEGIN, RET
--  Added symbols: ,
--  Added Expressions: Call
--  Added parsers: funccall, funcdef, args, ergs, optargs, optergs, 
--              commandWrapper, unitMainCommand, mainCommand
--  Other changes: 
--  - Separate Commands into two ADTs,
--    BaseCommand (original commands) and Command
--    so that function definitions can only exist at top-level. 
--    This prevents function definitions from being nested, as desired. 
--
--  - modify fetch and add fetchfunc to incorporate new
--    type for Store (Variable -> Either Val FuncDef)

module Interp where

import Data.Char
import Types
import MyParser
import MyLexer

-- evaluate an expression with respect to a store 
eval :: Expr -> Store -> Val
eval (Const c) = const c
eval (Var v) = flip fetch v
eval (Minus e1 e2)   = \s -> eval e1 s - eval e2 s
eval (Times e1 e2)   = \s -> eval e1 s * eval e2 s
eval (Greater e1 e2) = \s -> eval e1 s >* eval e2 s
                        where x >* y | x > y     = 1
                                     | otherwise = 0 

-- Function arguments must be evaluated in calling context. 
-- (map (flip eval s) exprs) evaluates each expression in exprs 
-- with respect to store s. (fetchfunc s fname) returns a function 
-- of type [Val] -> Val, which is then applied to the list of values 
-- given by evaluating the expressions in exprs. 
eval (Call fname exprs) = \s -> (fetchfunc s fname) (map (flip eval s) exprs)
 

-- interpret base commands -- 
interpret :: BaseCommand -> Store -> Store
interpret (Assign v e) = \s -> update s v (Left (eval e s))
interpret (Seq c1 c2) = interpret c2 . interpret c1
interpret (Cond e c1 c2) = 
  \s -> switch (eval e s) (interpret c1) (interpret c2) s
interpret (While e c) = 
  \s -> switch (eval e s) (interpret (Seq c (While e c))) id s

-- interpret' top level Command -- 
interpret' :: Command -> Store -> Store
interpret' (Seq' c1 c2) = interpret' c2 . interpret' c1
interpret' (Normal com) = interpret com 

-- update the store with a new function definition
-- fdef :: FuncDef -- (i.e. [Val] -> Val )
--   evaluates return expression in the context of the interpreted function body
-- updatedStore :: [Val] -> Store
--   takes a list of argument values and executes the function body 
--   after assigning the argument values (in the context updated with the function definition)
-- subLocal :: [Val] -> Args -> Store -> Store
--   takes a list of argument values and argument names and 
--   assigns each argument its value in the given context
interpret' (Def fn args com ret) =
  \s -> let fdefStore = update s ('$':fn) (Right fdef)
            fdef = eval ret . updatedStore
            updatedStore vals = interpret com (subLocal vals args fdefStore)
            subLocal [] [] = id
            subLocal (v:vs) (vr:vrs) = interpret (Assign vr (Const v)) . subLocal vs vrs
            subLocal _      _        = error "Number of arguments must match definition"
          in fdefStore


-- utility function used to interpret conditional statements
switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
switch 1 f _ = f
switch 0 _ g = g
switch _ _ _ = error "Switch applies only to 0 or 1"

-- initial store sends every variable to the value 0 
initial :: Store
initial = const (Left 0)

-- update a single variable within a store 
update :: Store -> Variable -> Either Val FuncDef -> Store
update s vr vl v
  | v == vr = vl
  | otherwise = s v

-- fetch value of variable from a store. 
-- fetch should only be used on variables that are assigned *values*, 
-- not on variablers that are assigned to function definitions. 
fetch :: Store -> Variable -> Val
fetch st vr = unwrap (st vr)
  where unwrap (Left vl) = vl
        unwrap _         = error ("Attempting to fetch value from function " ++ vr)

-- fetch function definition from a variable 
fetchfunc :: Store -> Variable -> FuncDef
fetchfunc st vr = unwrap (st ('$':vr))
  where unwrap (Right fd) = fd
        unwrap _         = error ("Function " ++ vr ++ " is undefined")

-- an interpreted program is a function 
-- that takes an input store and returns an output store 
run :: String -> Store -> Store
run = interpret' . mainParser . lexer

