{-# LANGUAGE ScopedTypeVariables #-}
module Ch09.ControlledVisit(
  Info(..),
  getInfo,
  getUsefulContents,
  isDirectory
) where

import System.Directory (Permissions(..),getDirectoryContents, getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)
import Control.Monad (forM, liftM)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO ( withFile path ReadMode hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse1 :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse1 order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse1 order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_::SomeException) -> return Nothing) (liftM Just act)


traverseVerbose:: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = maybe False searchable (infoPerms info)
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]