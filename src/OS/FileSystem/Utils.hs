module OS.FileSystem.Utils
    ( Info(..)
    , getInfo
    , getUsefulContents
    , isDirectory
    , traverseDir
    , foldTree
    , atMostThreePictures
    , countDirectories
    )
where

import           Control.Exception
import           Control.Monad
import           Data.Time.Clock                ( UTCTime(..) )
import           Data.Char                      ( toLower )
import           Foreign
import           Foreign.C
import           System.Directory
import           System.FilePath
import           Text.Regex.Posix               ( (=~) )
import           System.Posix.Files             ( fileExist )
import           System.IO

import           Utils.Preclude

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms    <- maybeIO (getPermissions path)
    size     <- maybeIO (withFile path ReadMode hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

traverseDir :: FilePath -> ([Info] -> [Info]) -> IO [Info]
traverseDir path order = do
    names           <- getUsefulContents path
    contents        <- forM (path : map (path </>) names) getInfo
    recurseContents <- forM (order contents) recurse
    return (concat recurseContents)
  where
    recurse info = do
        let childPath = infoPath info
        if isDirectory info && childPath /= path
            then traverseDir childPath order
            else return [info]


data Iterate seed =
    Done {unwrap :: seed}
    | Skip {unwrap :: seed}
    | Continue {unwrap :: seed}
    deriving (Show)


type Iterator seed = seed -> Info -> Iterate seed


foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return $ unwrap endSeed
  where
    -- 遍历下级目录
    fold seed subPath = getUsefulContents subPath >>= walk subPath seed
    -- 遍历当前目录
    walk curPath seed (name : names) = do
        let path' = curPath </> name
        info <- getInfo path'
        case iter seed info of
            done@(Done _) -> return done
            Skip seed'    -> walk curPath seed' names
            Continue seed'
                | isDirectory info -> do
                    next <- fold seed' path' -- 进入并搜索此子目录, 搜索完毕后回到当前目录
                    case next of
                        done@(Done _) -> return done -- 搜索此子目录的过程中, 刚好完成搜索
                        seed''        -> walk curPath (unwrap seed'') names
                | otherwise -> walk curPath seed' names -- 继续遍历此子目录的兄弟文件or兄弟文件夹
    walk _ seed _ = return (Continue seed)


atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3 = Done paths
    | isDirectory info && (fileName == ".svn" || fileName == ".git") = Skip
        paths
    | extension `elem` [".hs", ".png"] = Continue (path : paths)
    | otherwise = Continue paths
  where
    path      = infoPath info
    extension = map toLower (takeExtension path)
    fileName  = takeFileName path


countDirectories :: Iterator Integer
countDirectories count info =
    Continue (if isDirectory info then count + 1 else count)
