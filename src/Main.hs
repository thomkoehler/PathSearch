
-----------------------------------------------------------------------------------------------------------------------

module Main(main) where


import Data.List.Split(splitOn)
import Control.Monad(forM_, when, filterM)
import System.Directory(doesDirectoryExist, getDirectoryContents)
import System.FilePath((</>))
import Text.Printf(printf)
import System.Environment(getArgs, getEnv)
import System.FilePath.Glob(compile, match)
import System.Info(os)

-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
   envs <- getEnv "PATH"
   let delimiter = if isWindows then ";" else ":"
   let entries = splitOn delimiter envs
   args <- getArgs
   case args of
      [fileSpec] -> findFile entries fileSpec
      _ -> printEnv entries
  

printEnv :: [FilePath] -> IO ()
printEnv entries = forM_ entries $ \dir -> do
   exists <- doesDirectoryExist dir
   printf "%s %s\n" dir $ boolToStr exists
   where
      boolToStr True = ""
      boolToStr False = " => not exists !"
   
   
findFile :: [FilePath] -> FilePath -> IO ()
findFile entries fileSpec = do
      let pattern = compile fileSpec
      properDirs <- filterM doesDirectoryExist entries
      forM_ properDirs $ \dir -> do
         names <- getDirectoryContents dir
         let properNames = filter (`notElem` [".", ".."]) names
         forM_ properNames $ \name -> do
            let 
               m = match pattern name
               fullName = dir </> name
            isDir <- doesDirectoryExist fullName 
            when (m && not isDir) (putStrLn fullName)


isWindows :: Bool
isWindows = os == "mingw32"
   

-----------------------------------------------------------------------------------------------------------------------
   
