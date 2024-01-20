{
  module NakeParser (
    runParser,
    parse,
    parseFile
  ) where
  import NakeLexer
}

%name runParser
%monad { Either String } { >>= } { return }
%tokentype Token
%error parseError

%token
  enum         { T_Enum $$ }
  instance     {
  dependencies {}
  ident        { T_Ident $$ $$ }
  '{'          { T_LBrace _ }
  '}'          { T_RBrace _ }
  ':'          { T_Semi _ }
%%

Enum: 
