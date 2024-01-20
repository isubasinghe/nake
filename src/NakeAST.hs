module NakeAST where


type Ident = String
type Name = Ident 
type FlagName = Ident

data Enum
  = Enum Ident

data Builtins
  = FMT

data Deriving
  = Deriving [Builtins] Ident

data Src
  = Glob String
  | File String
  | Directory String


data Cmd
  = Cmd
  | Cmds 

data Dependencies
  = WithFlake


data File = File
  { name :: String
  , path :: String
  }

data Output
  = OutputFile

data Rule = Rule
  { name :: Name
  , flags :: [FlagName]
  , src :: Src
  , cmd :: Cmd
  , dependencies :: Dependencies
  , outputs = [Output]
  }

data Instance
  = InstanceFor Ident Ident
  | InstanceInternal Ident [Function]

data Function = Function
  { name :: Name
  }
  
