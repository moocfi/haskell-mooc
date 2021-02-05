module Examples.ReadTypes where

import Control.Monad (forM)
import Data.List (isInfixOf, isSuffixOf)
import System.Directory (listDirectory, doesDirectoryExist)

-- a line is a type signature if it contains :: but does not contain =
isTypeSignature :: String -> Bool
isTypeSignature s = not (isInfixOf "=" s) && isInfixOf "::" s

-- return list of types for a .hs file
readTypesFile :: FilePath -> IO [String]
readTypesFile file
  | isSuffixOf ".hs" file = do content <- readFile file
                               let ls = lines content
                               return (filter isTypeSignature ls)
  | otherwise             = return []

-- list children of directory, prepend directory name
qualifiedChildren :: String -> IO [String]
qualifiedChildren path = do childs <- listDirectory path
                            return (map (\name -> path++"/"++name) childs)

-- get type signatures for all entries in given directory
-- note mutual recursion with readTypes
readTypesDir :: String -> IO [String]
readTypesDir path = do childs <- qualifiedChildren path
                       typess <- forM childs readTypes
                       return (concat typess)

-- recursively read types contained in a file or directory
-- note mutual recursion with readTypesDir
readTypes :: String -> IO [String]
readTypes path = do isDir <- doesDirectoryExist path
                    if isDir then readTypesDir path else readTypesFile path

-- main is the IO action that gets run when you run the program
main :: IO ()
main = do ts <- readTypes "."
          mapM_ putStrLn ts
