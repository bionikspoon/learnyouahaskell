import           System.Environment
import           System.Directory
import           System.IO
import           Data.List

main = do
  (command : args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("remove", remove), ("bump", bump)]


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks =
        zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] =
  updateTodos fileName (\todos -> delete (todos !! read numberString) todos)



bump :: [String] -> IO ()
bump [fileName, numberString] = updateTodos
  fileName
  (\todos -> let task = todos !! read numberString in task : delete task todos)


updateTodos :: String -> ([String] -> [String]) -> IO ()
updateTodos fileName fn = withFile
  fileName
  ReadMode
  (\handle -> do
    (tempName, tempHandle) <- openTempFile "." fileName
    contents               <- hGetContents handle

    hPutStr tempHandle $ unlines (fn (lines contents))
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
  )


