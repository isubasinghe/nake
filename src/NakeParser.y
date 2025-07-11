{
module NakeParser (parseProgram) where
import NakeLexer
import NakeAST
}

%name parseProgram Program
%tokentype { Token }
%error { parseError }
%monad { Either String } { >>= } { return }

%token
  enum         { T_Enum _ }
  instance     { T_Instance _ }
  deriving     { T_Deriving _ }
  fn           { T_Fn _ }
  self         { T_Self _ }
  match        { T_Match _ }
  flags        { T_Flags _ }
  src          { T_Src _ }
  cmd          { T_Cmd _ }
  cmds         { T_Cmds _ }
  dependencies { T_Dependencies _ }
  inputs       { T_Inputs _ }
  outputs      { T_Outputs _ }
  file         { T_File _ }
  name         { T_Name _ }
  path         { T_Path _ }
  fmt          { T_Fmt _ }
  for          { T_For _ }
  glob         { T_Glob _ }
  withFlake    { T_WithFlake _ }
  ident        { T_Ident _ $$ }
  string       { T_String _ $$ }
  '{'          { T_LBrace _ }
  '}'          { T_RBrace _ }
  '['          { T_LBracket _ }
  ']'          { T_RBracket _ }
  '('          { T_LParen _ }
  ')'          { T_RParen _ }
  '::'         { T_DoubleColon _ }
  ':'          { T_Colon _ }
  ','          { T_Comma _ }
  ';'          { T_Semi _ }
  '='          { T_Eq _ }
  '=>'         { T_Arrow _ }
  '.'          { T_Dot _ }
  '$$'         { T_MetaMetaFunc _ }
  '$'          { T_MetaVar _ }

%%

Program :: { Program }
Program : TopLevelList { Program $1 }

TopLevelList :: { [TopLevel] }
TopLevelList : TopLevel TopLevelList { $1 : $2 }
             | TopLevel              { [$1] }

TopLevel :: { TopLevel }
TopLevel : EnumDecl     { EnumDecl $1 }
         | DerivingDecl { DerivingDecl $1 }
         | InstanceDecl { InstanceDecl $1 }
         | RuleDecl     { uncurry RuleDecl $1 }

EnumDecl :: { NakeEnum }
EnumDecl : enum ident '{' IdentList '}' ';' { NakeEnum $2 $4 }

IdentList :: { [Ident] }
IdentList : ident ';' IdentList { $1 : $3 }
          | ident ';'           { [$1] }

DerivingDecl :: { Deriving }
DerivingDecl : deriving '[' BuiltinList ']' for ident ';' { Deriving $3 $6 }

BuiltinList :: { [Builtins] }
BuiltinList : fmt ',' BuiltinList { FMT : $3 }
            | fmt                 { [FMT] }

InstanceDecl :: { Instance }
InstanceDecl : instance ident for ident '{' AssignmentList '}' ';' { InstanceFor $2 $4 $6 }
             | instance BuiltinIdent for ident '{' AssignmentList '}' ';' { InstanceFor $2 $4 $6 }
             | instance ident '{' FunctionList '}' ';'                { InstanceSingle $2 $4 }

BuiltinIdent :: { Ident }
BuiltinIdent : fmt { "fmt" }

AssignmentList :: { [Assignment] }
AssignmentList : Assignment AssignmentList { $1 : $2 }
               | Assignment                { [$1] }

Assignment :: { Assignment }
Assignment : ident '=' Expr ';' { Assignment $1 $3 }

FunctionList :: { [Function] }
FunctionList : Function FunctionList { $1 : $2 }
             | Function              { [$1] }

Function :: { Function }
Function : fn ident '(' ParamList ')' '{' StmtList '}' ';' { Function $2 $4 $7 }

ParamList :: { [(Ident, Ident)] }
ParamList : ident ':' ident ',' ParamList { ($1, $3) : $5 }
          | ident ':' ident               { [($1, $3)] }
          | self ':' ident ',' ParamList  { ("self", $3) : $5 }
          | self ':' ident                { [("self", $3)] }
          | {- empty -}                   { [] }

StmtList :: { [Stmt] }
StmtList : Stmt StmtList { $1 : $2 }
         | Stmt          { [$1] }

Stmt :: { Stmt }
Stmt : match ident '{' MatchArmList '}' { MatchStmt $2 $4 }
     | Expr                             { ExprStmt $1 }

MatchArmList :: { [MatchArm] }
MatchArmList : MatchArm ',' MatchArmList { $1 : $3 }
             | MatchArm                  { [$1] }

MatchArm :: { MatchArm }
MatchArm : Expr '=>' Expr { MatchArm $1 $3 }

RuleDecl :: { (Name, Rule) }
RuleDecl : ident '=' '{' RuleFieldList '}' ';' { ($1, makeRule $1 $4) }

RuleFieldList :: { [(Ident, Expr)] }
RuleFieldList : RuleField RuleFieldList { $1 : $2 }
              | RuleField                { [$1] }

RuleField :: { (Ident, Expr) }
RuleField : flags '=' Expr ';'        { ("flags", $3) }
          | src '=' Expr ';'          { ("src", $3) }
          | cmd '=' Expr ';'          { ("cmd", $3) }
          | cmds '=' Expr ';'         { ("cmds", $3) }
          | inputs '=' Expr ';'       { ("inputs", $3) }
          | dependencies '=' Expr ';' { ("dependencies", $3) }
          | outputs '=' Expr ';'      { ("outputs", $3) }

Expr :: { Expr }
Expr : string                           { StringLit $1 }
     | ident                            { IdentExpr $1 }
     | glob string                      { GlobExpr $2 }
     | ident '(' ExprList ')'           { FuncCall $1 $3 }
     | '[' ExprList ']'                 { ListExpr $2 }
     | '{' RecordFieldList '}'          { RecordExpr $2 }
     | file '{' RecordFieldList '}'     { RecordExpr $3 }
     | withFlake '{' RecordFieldList '}' { RecordExpr $3 }
     | '$' ident                        { MetaVar $2 }
     | '$$' ident                       { MetaFunc $2 [] }
     | ident '::' ident                 { QualifiedIdent $1 $3 }
     | ident '.' ident                  { FuncCall "field_access" [IdentExpr $1, IdentExpr $3] }
     | ident '.' FieldName              { FuncCall "field_access" [IdentExpr $1, IdentExpr $3] }

FieldName :: { Ident }
FieldName : outputs { "outputs" }
          | inputs  { "inputs" }
          | flags   { "flags" }
          | src     { "src" }
          | cmd     { "cmd" }
          | cmds    { "cmds" }

ExprList :: { [Expr] }
ExprList : Expr ',' ExprList { $1 : $3 }
         | Expr              { [$1] }
         | {- empty -}       { [] }

RecordFieldList :: { [(Ident, Expr)] }
RecordFieldList : RecordField RecordFieldList { $1 : $2 }
                | RecordField                  { [$1] }
                | {- empty -}                  { [] }

RecordField :: { (Ident, Expr) }
RecordField : ident '=' Expr ';' { ($1, $3) }
            | name '=' Expr ';'  { ("name", $3) }
            | path '=' Expr ';'  { ("path", $3) }

{
parseError :: [Token] -> Either String a
parseError tokens = Left $ "Parse error at: " ++ show (take 5 tokens)

makeRule :: Name -> [(Ident, Expr)] -> Rule
makeRule name fields = Rule
  { ruleName = name
  , ruleFlags = getFlags fields
  , ruleSrc = lookup "src" fields
  , ruleCmd = lookup "cmd" fields
  , ruleCmds = lookup "cmds" fields
  , ruleInputs = lookup "inputs" fields
  , ruleDependencies = getDependencies fields
  , ruleOutputs = getOutputs fields
  }

getFlags :: [(Ident, Expr)] -> [FlagName]
getFlags fields = case lookup "flags" fields of
  Just (ListExpr exprs) -> map exprToIdent exprs
  _ -> []

exprToIdent :: Expr -> Ident
exprToIdent (IdentExpr i) = i
exprToIdent _ = ""

getDependencies :: [(Ident, Expr)] -> Maybe Dependencies
getDependencies fields = case lookup "dependencies" fields of
  Just (RecordExpr rs) -> Just (WithFlake rs)
  _ -> Nothing

getOutputs :: [(Ident, Expr)] -> [Output]
getOutputs fields = case lookup "outputs" fields of
  Just (ListExpr exprs) -> map exprToOutput exprs
  _ -> []

exprToOutput :: Expr -> Output
exprToOutput (RecordExpr fields) = 
  let nameVal = case lookup "name" fields of
        Just (StringLit s) -> s
        _ -> ""
      pathVal = case lookup "path" fields of
        Just (StringLit s) -> s
        _ -> ""
  in OutputFile (File nameVal pathVal)
exprToOutput _ = OutputFile (File "" "")
}
