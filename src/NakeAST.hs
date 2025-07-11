module NakeAST where

import Data.List (intercalate)

type Ident = String
type Name = Ident 
type FlagName = Ident

data NakeEnum = NakeEnum Ident [Ident]
  deriving (Show, Eq)

data Builtins = FMT
  deriving (Show, Eq)

data Deriving = Deriving [Builtins] Ident
  deriving (Show, Eq)

data Expr 
  = StringLit String
  | IdentExpr Ident
  | GlobExpr String
  | FuncCall Ident [Expr]
  | ListExpr [Expr]
  | RecordExpr [(Ident, Expr)]
  | MetaVar Ident
  | MetaFunc Ident [Expr]
  | QualifiedIdent Ident Ident
  deriving (Show, Eq)

data Src
  = Glob String
  | SrcFile String
  | Directory String
  deriving (Show, Eq)

data Dependencies = WithFlake [(Ident, Expr)]
  deriving (Show, Eq)

data File = File
  { fileName :: String
  , filePath :: String
  }
  deriving (Show, Eq)

data Output = OutputFile File
  deriving (Show, Eq)

data Rule = Rule
  { ruleName :: Name
  , ruleFlags :: [FlagName]
  , ruleSrc :: Maybe Expr
  , ruleCmd :: Maybe Expr
  , ruleCmds :: Maybe Expr
  , ruleInputs :: Maybe Expr
  , ruleDependencies :: Maybe Dependencies
  , ruleOutputs :: [Output]
  }
  deriving (Show, Eq)

data Instance
  = InstanceFor Ident Ident [Assignment]
  | InstanceSingle Ident [Function]
  deriving (Show, Eq)

data Assignment = Assignment Ident Expr
  deriving (Show, Eq)

data Function = Function
  { functionName :: Name
  , functionParams :: [(Ident, Ident)]
  , functionBody :: [Stmt]
  }
  deriving (Show, Eq)

data Stmt 
  = MatchStmt Ident [MatchArm]
  | ExprStmt Expr
  deriving (Show, Eq)

data MatchArm = MatchArm Expr Expr
  deriving (Show, Eq)

data TopLevel
  = EnumDecl NakeEnum
  | DerivingDecl Deriving
  | InstanceDecl Instance
  | RuleDecl Name Rule
  deriving (Show, Eq)

data Program = Program [TopLevel]
  deriving (Show, Eq)
  
