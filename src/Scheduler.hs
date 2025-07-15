module Scheduler where

import qualified NakeAST as N
import qualified Data.Map.Strict as Map

-------------------------------------------------------------------------------
-- | Environment --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Mapping from function name to its definition so that we can resolve
--   calls later when walking the AST.
type FunctionEnv = Map.Map N.Name N.Function

-------------------------------------------------------------------------------
-- | Entry point --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Process a parsed Nake 'Program'.  Currently we only:
--
--   1. Collect all top-level function definitions into an environment.
--   2. Walk through the remaining top-level declarations and, for rules, print
--      a summary while showing where functions are referenced.
--
--   This is a placeholder implementation – the important part is that we keep
--   track of functions so that later evaluations can resolve calls.
processNakeFile :: N.Program -> IO ()
processNakeFile (N.Program tops) = do
  let funEnv = collectFunctions tops
  putStrLn "Collected functions:"
  mapM_ (putStrLn . ("  " ++)) (Map.keys funEnv)
  putStrLn "\nProcessing top-level declarations:\n"
  mapM_ (processTop funEnv) tops

-------------------------------------------------------------------------------
-- | Collect definitions ------------------------------------------------------
-------------------------------------------------------------------------------

collectFunctions :: [N.TopLevel] -> FunctionEnv
collectFunctions = foldr collect Map.empty
  where
    collect (N.InstanceDecl (N.InstanceSingle _name funs)) acc =
      foldr (\f -> Map.insert (N.functionName f) f) acc funs
    collect _ acc = acc

-------------------------------------------------------------------------------
-- | Processing ---------------------------------------------------------------
-------------------------------------------------------------------------------

processTop :: FunctionEnv -> N.TopLevel -> IO ()
processTop env tl = case tl of
  N.RuleDecl name rule -> processRule env name rule
  _                    -> pure ()  -- ignore other declarations for now

processRule :: FunctionEnv -> N.Name -> N.Rule -> IO ()
processRule env name rule = do
  putStrLn $ "Rule: " ++ name
  maybe (pure ()) (\e -> putStrLn $ "  src  : " ++ prettyExpr env e) (N.ruleSrc rule)
  maybe (pure ()) (\e -> putStrLn $ "  cmd  : " ++ prettyExpr env e) (N.ruleCmd rule)
  maybe (pure ()) (\e -> putStrLn $ "  cmds : " ++ prettyExpr env e) (N.ruleCmds rule)
  putStrLn ""

-------------------------------------------------------------------------------
-- | Expression pretty printing ----------------------------------------------
-------------------------------------------------------------------------------

prettyExpr :: FunctionEnv -> N.Expr -> String
prettyExpr env expr = case expr of
  N.StringLit s        -> show s
  N.IdentExpr i        -> i
  N.GlobExpr g         -> "glob " ++ show g
  N.ListExpr xs        -> "[" ++ commaSep (map (prettyExpr env) xs) ++ "]"
  N.RecordExpr fs      -> "{" ++ commaSep (map field fs) ++ "}"
    where field (k,v) = k ++ " = " ++ prettyExpr env v
  N.FuncCall f args    ->
    let note = if Map.member f env then " (resolved)" else " (UNRESOLVED)"
    in f ++ "(" ++ commaSep (map (prettyExpr env) args) ++ ")" ++ note
  N.MetaVar v          -> '$':v
  N.MetaFunc f args    -> "$$" ++ f ++ "(" ++ commaSep (map (prettyExpr env) args) ++ ")"
  N.QualifiedIdent a b -> a ++ "::" ++ b

commaSep :: [String] -> String
commaSep [] = ""
commaSep (x:xs) = foldl (\a b -> a ++ ", " ++ b) x xs
