{
module NakeLexer
  ( runLexer
  , Token(..)
  , AlexPosn(..)
  ) where
}

%wrapper "posn"

$digit   = 0-9 
$alpha   = [a-zA-Z] 
$alnum   = [ $alpha $digit ]
@ident   = $alpha [ $alnum \_ ]*
@string  = \" ([^ \" \\ ] | \\ . | \n | \t | \r)* \"
@comment = \# .*
@number  = $digit+

rules :-
  $white+      ;
  fn           { mkTok T_Fn }
  instance     { mkTok T_Instance }
  self         { mkTok T_Self }
  match        { mkTok T_Match }
  data         { mkTok T_Data }
  flags        { mkTok T_Flags }
  src          { mkTok T_Src }
  cmd          { mkTok T_Cmd }
  cmds         { mkTok T_Cmds }
  dependencies { mkTok T_Dependencies }
  inputs       { mkTok T_Inputs }
  outputs      { mkTok T_Outputs } 
  file         { mkTok T_File }
  name         { mkTok T_Name }
  path         { mkTok T_Path }
  enum         { mkTok T_Enum }
  deriving     { mkTok T_Deriving }
  fmt          { mkTok T_Fmt }
  for          { mkTok T_For }
  glob         { mkTok T_Glob }
  withFlake    { mkTok T_WithFlake }
  \{           { mkTok T_LBrace }
  \}           { mkTok T_RBrace }
  \[           { mkTok T_LBracket }
  \]           { mkTok T_RBracket }
  \(           { mkTok T_LParen }
  \)           { mkTok T_RParen }
  \:\:         { mkTok T_DoubleColon }
  \:           { mkTok T_Colon }
  \,           { mkTok T_Comma }
  \;           { mkTok T_Semi }
  \=           { mkTok T_Eq }
  \=\>         { mkTok T_Arrow }
  \.           { mkTok T_Dot }
  \$\$         { mkTok T_MetaMetaFunc }
  \$           { mkTok T_MetaVar }
  @comment     ;
  @string      { (\p s -> T_String p ((tail . init ) s)) }
  @ident       { T_Ident }

  {



data Token
    = T_Fn AlexPosn
    | T_Instance AlexPosn
    | T_Self AlexPosn
    | T_Match AlexPosn
    | T_Data AlexPosn
    | T_Flags AlexPosn
    | T_Src AlexPosn
    | T_Cmd AlexPosn
    | T_Cmds AlexPosn
    | T_Dependencies AlexPosn
    | T_Inputs AlexPosn
    | T_Outputs AlexPosn
    | T_File AlexPosn
    | T_Name AlexPosn
    | T_Path AlexPosn
    | T_Enum AlexPosn
    | T_Deriving AlexPosn
    | T_Fmt AlexPosn
    | T_For AlexPosn
    | T_Glob AlexPosn
    | T_WithFlake AlexPosn
    | T_LBrace AlexPosn
    | T_RBrace AlexPosn
    | T_LBracket AlexPosn
    | T_RBracket AlexPosn
    | T_LParen AlexPosn
    | T_RParen AlexPosn
    | T_DoubleColon AlexPosn
    | T_Colon AlexPosn
    | T_Comma AlexPosn
    | T_Semi AlexPosn
    | T_Eq AlexPosn
    | T_Arrow AlexPosn
    | T_Dot AlexPosn
    | T_MetaMetaFunc AlexPosn
    | T_MetaVar AlexPosn
    | T_String AlexPosn String
    | T_Ident AlexPosn String
    deriving (Show, Eq)

mkTok :: (AlexPosn -> Token) -> AlexPosn -> String -> Token 
mkTok f p s = f p


runLexer :: String -> Either String [Token]
runLexer str = go (alexStartPos,'\n',[],str)
  where 
    go inp@(pos,_,_,s) = 
      case alexScan inp 0 of
        AlexEOF 
          -> Right []
        AlexError ((AlexPn _ l c),_,_,_) 
          -> let ls = lines str in
             let err = if c > 0 && l > 0 && l <= length ls
                       then ":\n" ++ (ls !! (l - 1)) ++ "\n" 
                            ++ replicate (c - 1) ' ' ++ "^ here"
                       else ""
             in Left $ "Lexical error at line " ++ show l 
                       ++ ", column " ++ (show c) ++ err
        AlexSkip inp' len     
          -> go inp' 
        AlexToken inp' len act 
          -> go inp' >>= Right . (:) (act pos $ take len s)

}
