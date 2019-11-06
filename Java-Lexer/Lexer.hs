{-
Name: Eunsoo Jang
File: Lexer.hs
Desc: A Java lexer
-}
module Main where

import Data.Char

import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main = do
 args     <- getArgs
 filename <- checkArgs args
 input    <- readFile filename
 let result = lexJava input
 writeFile (takeBaseName filename <.> "lex") (unlines result)

--Check the command-line arguments. Returns the filename
--to lex upon success.
checkArgs :: [String] -> IO FilePath
checkArgs [path] = pure path

checkArgs _other = do
 putStrLn "Usage: ./Lexer <filename>.java"
 putStrLn "Writes to <filename>.lex"
 exitFailure

-- Takes Java code as input and returns a list of Strings.
-- Each String in the output list is one Java token.
-- Comments and whitespace are discarded.
lexJava :: String -> [String]
lexJava [] = []
lexJava str = lexNoPrefix (findToken str)  -- You will edit this line

-- lex1 :  takes a non-empty string as its first two parameters (the Char is the first letter and the String is the possibly-empty remainder). It returns two strings: the first token in the input and the rest of the input.
lex1 :: Char -> String -> (String, String)
lex1 '\0' [] = error "Error"
lex1 ch str
 |isAlpha ch || ch=='_' || ch == '$' = isIdentifier (ch:str)
 |operatorToken /= "" = isOperators (ch:str) ""
 |separatorToken /= "" = isSeparators (ch:str) ""
 |isDigit ch = isIntegerLiteral ch str
 |ch == '\"' && strToken /= "" = ([ch]++strToken, tRestOfString)
 |ch == '\'' && charToken /= "" = ([ch]++charToken, cRestOfString)
 |otherwise = lex1 '\0' []
  where
   (operatorToken, oRestOfString) = isOperators (ch:str) ""
   (separatorToken, sRestOfString) = isSeparators (ch:str) ""
   (charToken, cRestOfString) = isCharLiteral (str) ""
   (strToken, tRestOfString) = isStringLiteral (str) ""

   -- isIdentifier : takes in string and returns two strings (first string contains identifier token and second string contains the rest of the string)
   -- note: did not make keywords and boolean literal function because isIdentifier takes care of keywords as well.
isIdentifier :: String ->(String,String)
isIdentifier [] = ([],[])
isIdentifier (x:xs)
 |isAlpha x || isDigit x || x == '_' || x == '$' = ([x] ++ first, second)
 |otherwise = ("", [x] ++ xs)
 where
    (first, second) = isIdentifier xs

--isSeparators : takes in two strings (first string is the input and the second string is empty) and returns two strings (first string contains separator token and second string contains the rest of the string)
isSeparators :: String -> String -> (String, String)
isSeparators [] restStr = ([],restStr)
isSeparators str emptyStr
 |str `elem` separatorStrings =  (str,emptyStr)
 |otherwise = isSeparators (init str) ([last str] ++ emptyStr)
  where
    separatorStrings = ["(", ")", "{", "}", "[", "]", ";", ",", ".", "...", "@", "::"]

--isOperators : takes in two strings (first string is the input and the second string is empty) and returns two strings (first string contains operator token and second string contains the rest of the string)
isOperators :: String -> String -> (String, String)
isOperators [] restStr = ([],restStr)
isOperators str emptyStr
 |str `elem` operatorsStrings = (str , emptyStr)
 |otherwise = isOperators (init str) ([last str] ++ emptyStr)
   where
     operatorsStrings = ["=", "==", ">", ">=", ">>", ">>=", ">>>", ">>>=", "<","<=", "<<", "<<=", "!", "!=", "~", "?", ":", "-", "--", "-=", "->", "&","&=", "&&", "|", "||", "|=", "^", "^=", "*", "*=", "+", "++", "+=", "/", "/=", "%", "%="]

--isIntegerLiteral : takes in char and string as input and returns to strings. First string contains the integer literal token and the second string contains the rest of the string inputted.
isIntegerLiteral :: Char -> String -> (String, String)
isIntegerLiteral '\0' _ = error "Error"
isIntegerLiteral ch [] = ([ch], [])
isIntegerLiteral ch (x:xs)
 |((length (x:xs))>=2) && ch == '0' && (x == 'b' || x == 'B') && isBinaryDigit (head xs) = ([ch]++(take 2 (x:xs))++binaryToken, bRestOfString)
 |((length (x:xs))>=2)&& ch == '0' && (x == 'x' || x == 'X') && isHexDigit (head xs) = ([ch]++(take 2 (x:xs))++hexToken, hRestOfString)
 |ch == '0' && (isOctDigit x || x == '_') = ([ch]++[x]++octalToken,oRestOfString)
 |isDigit ch && ch /= '0' = ([ch]++decimalToken, dRestOfString)
 |otherwise = ([], [ch]++(x:xs))
  where
    (hexToken, hRestOfString) = isHexInt (tail xs)
    (binaryToken, bRestOfString) = isBinaryInt (tail xs)
    (octalToken, oRestOfString) = isOctalInt xs
    (decimalToken, dRestOfString) = isDecimalInt (x:xs)

--isDecimalInt : takes in a string and returns two strings. First string contains decimal integer token and second string contains the rest of the string.
isDecimalInt :: String -> (String, String)
isDecimalInt [] = ([],[])
isDecimalInt (x:xs)
 |(length (filter (=='_') (x:xs))) == length (x:xs) = ("", (x:xs))
 |((length xs)>=1) && ((x == 'l' || x =='L') && (head xs =='l' || head xs =='L')) = ([x] , xs)
 |((length xs)>=1) && x=='_' && (head xs == 'l' || head xs =='L') = ("", (x:xs))
 |isDigit x || x == '_' || x == 'l' || x =='L' = ([x] ++ first, second)
 |otherwise = ("", [x] ++ xs)
  where
    (first, second) = isDecimalInt xs

--isHexInt : takes in a string and returns two strings. First string contains hex integer token and second string contains the rest of the string.
isHexInt :: String -> (String, String)
isHexInt [] = ([],[])
isHexInt (x:xs)
  |(length (filter (=='_') (x:xs))) == length (x:xs) = ("", (x:xs))
  |((length xs)>=1) && ((x == 'l' || x =='L') && (head xs =='l' || head xs =='L')) = ([x],xs)
  |((length xs)>=1) && x=='_' && (head xs == 'l' || head xs =='L') = ("", (x:xs))
  |isHexDigit x || x == '_' ||x == 'l' || x =='L' = ([x] ++ first, second)
  |otherwise = ("", [x] ++ xs)
   where
     (first, second) = isHexInt xs

--isOctalInt : takes in a string and returns two strings. First string contains octal integer token and second string contains the rest of the string.
isOctalInt :: String -> (String, String)
isOctalInt [] = ([], [])
isOctalInt (x:xs)
  |(length (filter (=='_') (x:xs))) == length (x:xs) = ("", (x:xs))
  |((length xs)>=1) && ((x == 'l' || x =='L') && (head xs =='l' || head xs =='L')) = ([x], xs)
  |((length xs)>=1) && x=='_' && (head xs == 'l' || head xs =='L') = ("", (x:xs))
  |isOctDigit x || x == '_' || x == 'l' || x =='L'  = ([x] ++ first, second)
  |otherwise = ("", [x] ++ xs)
   where
     (first, second) = isOctalInt xs

--isBinaryDigit : takes in a char and returns a boolean. It returns true when char is a binary digit and false otherwise.
isBinaryDigit :: Char -> Bool
isBinaryDigit ch
 |ch == '0' || ch == '1' = True
 |otherwise = False

--isBinaryInt : takes in a string and returns two strings. First string contains binary integer token and second string contains the rest of the string.
isBinaryInt :: String -> (String, String)
isBinaryInt [] = ([], [])
isBinaryInt (x:xs)
 |(length (filter (=='_') (x:xs))) == length (x:xs) = ("", (x:xs))
 |((length xs)>=1) && ((x == 'l' || x =='L') && (head xs =='l' || head xs =='L')) = ([x] , xs)
 |((length xs)>=1) && x=='_' && (head xs == 'l' || head xs =='L') = ("", (x:xs))
 |isBinaryDigit x || x == '_' || x == 'l' || x =='L' = ([x] ++ first, second)
 |otherwise = ("", [x] ++ xs)
  where
    (first, second) = isBinaryInt xs

--isEscapeSeq : takes in a string and returns a boolean. It returns true when string is an escape sequence and false otherwise.
isEscapeSeq :: String -> Bool
isEscapeSeq str
 |str `elem` esStrings = True
 |otherwise = False
  where
   esStrings = ["\\b", "\\t", "\\n", "\\f", "\\r", "\\'", "\\\"","\\\\"]

--isCharLiteral : takes in two strings (first string is the input and the second string is empty) and returns two strings (first string contains char literal token and second string contains the rest of the string)
isCharLiteral :: String -> String -> (String, String)
isCharLiteral [] [] = ([],[])
isCharLiteral (x:xs) emptyStr
  |(length (x:xs)) >= 1 && isAlphaNum x && x/='\'' && x/='\\' && head xs== '\'' = ([x]++[head xs], tail xs)
  |(length (x:xs)) >= 1 && isEscapeSeq ([x]++[head xs]) && ((x:xs)!!1) == '\'' = ([x]++[head xs], tail xs)
  |otherwise = ([], (x:xs))

--isStringLiteral : takes in two strings (first string is the input and the second string is empty) and returns two strings (first string contains string literal token and second string contains the rest of the string)
isStringLiteral :: String -> String -> (String, String)
isStringLiteral [] [] = ([],[])
isStringLiteral str emptyStr
 |last str == '\"' = (str,emptyStr)
 |otherwise = isStringLiteral (init str) ([last str] ++ emptyStr)

-- findToken : discard a prefix of the input string containing only whitespace, returning  a suffix of the input string that starts with a non-discardable character.
findToken :: String -> String
findToken [] = []
findToken (x:xs)
 |isSpace x = findToken xs
 |length (x:xs) >= 2 && x == '/' && head xs == '/' = findToken (isComment(tail xs))
 |length (x:xs) >= 2 && x == '/' && head xs == '*' = findToken (isMComment (tail xs))
 |otherwise = (x:xs)

-- isComment : takes in a string and returns rest of string after the comment.
isComment :: String -> String
isComment [] = []
isComment (x:xs)
 |x == '\n'  = (xs)
 |otherwise = isComment (xs)

-- isMComment : same as isComment, but used for multi-line commments
isMComment :: String -> String
isMComment [] = []
isMComment (x:xs)
 |length (x:xs) >= 2 &&  x == '*' && head xs == '/' = tail xs
 |otherwise = isMComment xs

lexNoPrefix :: String -> [String]
lexNoPrefix [] = []
lexNoPrefix str = [token] ++ lexJava restOfString
  where
   (token, restOfString)=(lex1 (head str) (tail str))
