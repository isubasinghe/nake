import NakeParser
import NakeAST
import NakeLexer

main :: IO ()
main = do
  content <- readFile "testdata/simple.nake"
  case runLexer content of
    Left err -> putStrLn $ "Lexer error: " ++ err
    Right tokens -> do
      case parseProgram tokens of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right program -> do
          putStrLn "Parse successful!"
          print program
