module Main where
         
import Text.Parsec
import Text.Parsec.String (Parser)

whitespace :: Parser ()
whitespace = do
  _ <- many $ (try comment_parser <|> try (void $ satisfy (`elem` " \n\t\r")))
  return ()

void :: Parser a -> Parser ()
void p = do
    _ <- p 
    return ()

comment_parser :: Parser ()
comment_parser = string "--" >> (many $ satisfy (`elem` " \t")) >> (many $ satisfy (/= '\n')) >> (void $ char '\n')

prepend_whitespace :: Parser a -> Parser a
prepend_whitespace p = do
    whitespace 
    x <- p
    return x

array_of :: Parser a -> Parser [a]
array_of p = do
    ps <- between (char '{') (whitespace *> char '}') 
       $ (whitespace *> p <* whitespace) `sepEndBy` (char ',' <* whitespace)
    return $ ps

data Val = Val_Str String 
         | Val_Int Int 
         | Val_Bool Bool
         | Val_Float Float 
         | Val_Array [Val] 
         | Val_Obj [KV] 
  deriving(Eq, Show)

val_str :: Parser Val
val_str = do
    str <- between (char '"') (char '"') (many $ satisfy (/= '"'))
    return $ Val_Str str

val_num :: Parser Val
val_num = do
    f <- many1 (digit <|> char '.')
    case ('.' `elem` f) of
        True  -> return $ Val_Float (read f)
        False -> return $ Val_Int (read f)

val_bool :: Parser Val
val_bool = do
    str <- (string "false") <|> (string "true")
    case str of 
        "true" -> return $ Val_Bool True
        _ -> return $ Val_Bool False

val_object :: Parser Val
val_object = do
    void $ lookAhead $ (char '{' >> whitespace >> (parsecMap snd kv_parser))
    kvs <- array_of kv_parser
    return $ Val_Obj kvs

val_array :: Parser Val
val_array = do
    vals <- array_of val_parser
    return $ Val_Array vals

val_parser :: Parser Val
val_parser = (try val_object) <|> (try val_array) <|> (try val_str) <|> (try val_num) <|> (try val_bool)

kv_parser :: Parser KV
kv_parser = do
    key <- many1 $ satisfy (`notElem` "#- ={\t\n\"};)")
    void $ (whitespace *> char '=' <* whitespace)
    val <- val_parser
    return (key, val)

type KV = (String, Val)

doc_parser :: Parser Val 
doc_parser = 
    string "data:extend" *> whitespace *> char '(' *> whitespace *> 
      val_parser 
    <* char ')' <* whitespace <* eof

main :: IO ()
main = do
    f <- readFile "data/recipes.txt"
    case (runParser doc_parser () "" f) of
        (Right res) -> putStrLn $ show res
        (Left err) -> putStrLn $ show err
