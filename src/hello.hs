-- main = do
--   putStrLn "reverse a line:"
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main

-- reverseWords :: String -> String
-- reverseWords = unwords . reverse . words

-- main = do
--   a <- return "hello"
--   b <- return "world"
--   putStrLn $ a ++ " " ++ b
-- import           Data.Char

-- main = do
--   contents <- getContents
--   putStr $ map toUpper contents

main = interact respondPalindromes

respondPalindromes :: String -> String

respondPalindromes =
  let isPalindrome xs = xs == reverse xs
      toString xs = if isPalindrome xs then "palindrome" else "not palindrom"
  in  (unlines . map toString . lines)

