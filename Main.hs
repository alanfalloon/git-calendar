{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.List
import Data.Set (Set)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Version (showVersion)
import Paths_git_calendar (version)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Locale (defaultTimeLocale)
import System.Posix.Files
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- getArgs
  projects <- programOpts args
  projects' <- mapM getProject projects
  prjdata <- mapM readProject $ concat projects'
  let evtlist = Set.toAscList $ Set.unions prjdata
      days = groupBy sameDay evtlist
      daysprjs = map (groupBy sameProject) days
  mapM_ printDayProjects daysprjs

printDayProjects :: [[Event]] -> IO ()
printDayProjects dayprjs = do
  putStrLn day
  mapM_ printProject dayprjs
    where
      day = showGregorian $ utctDay $ time $ head $ head dayprjs

printProject :: [Event] -> IO ()
printProject [Event {time, project}] =
    putStrLn $ formatTime defaultTimeLocale "%t%T%t%t" time ++ project
printProject evts = putStrLn $ "\t" ++ start ++ "-" ++ end ++ "\t" ++ project (head evts)
    where
      start = formatTime defaultTimeLocale "%T" $ time $ head evts
      end = formatTime defaultTimeLocale "%T" $ time $ last evts

readProject :: (String, FilePath) -> IO (Set Event)
readProject (proj,logdir) = do
  cnt <- walkDirectory logdir
  logs <- mapM (readLog proj logdir) cnt
  return $ Set.unions logs

readLog :: String -> FilePath -> FilePath -> IO (Set Event)
readLog proj logdir logfile = do
  cnt <- readFile logfile
  return $ parseLog proj (makeRelative logdir logfile) cnt

parseLog :: String -> String -> String -> Set Event
parseLog proj branch log = Set.fromList events
    where
      events = map (parseEvent proj branch) $ lines log

parseEvent :: String -> String -> String -> Event
parseEvent proj branch line = Event time proj branch desc'
    where
      (Just line') = stripPrefix "> " $ dropWhile (/= '>') line
      (timestamp, desc) = span (/= '\t') line'
      time = readTime defaultTimeLocale "%s %z" timestamp
      desc' = case desc of
                '\t':d -> d
                d -> d

data Event = Event { time :: UTCTime
                   , project :: String
                   , branch :: String
                   , description :: String
                   }
             deriving (Show, Ord, Eq)

sameDay,sameProject :: Event -> Event -> Bool
sameDay (Event {time=t1}) (Event {time=t2}) = utctDay t1 == utctDay t2
sameProject (Event {project=p1}) (Event {project=p2}) = p1 == p2

getProject :: Monad m => FilePath -> IO (m (String,FilePath))
getProject path = do
  isD <- isDir logdir
  if isD then return (return (proj,logdir)) else return failure
    where
      path' = dropTrailingPathSeparator path
      (path'', gitdir) = splitFileName path'
      proj = if gitdir == ".git" then dropTrailingPathSeparator path'' else path'
      logdir = joinPath [proj, ".git", "logs"]
      failure = fail (logdir ++ " doesn't exist")

isDir :: FilePath -> IO Bool
isDir d = flip catch (\_ -> return False) $ do
            stat <- getFileStatus d
            return $ isDirectory stat

walkDirectory :: FilePath -> IO [FilePath]
walkDirectory dir = do
  cnt <- getDirectoryContents dir
  cnt' <- mapM walk' cnt
  return $ concat cnt'
    where
      walk' "." = return []
      walk' ".." = return []
      walk' f = do
        let f' = dir </> f
        isD <- isDir f'
        if isD then walkDirectory f' else return [f']


data Flag = Help | Version deriving (Show, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['h']     ["help"]    (NoArg Help)          "Show this help message"
    , Option ['V']     ["version"] (NoArg Version)       "show version number"
    ]

programOpts :: [String] -> IO [String]
programOpts argv =
    case getOpt Permute options argv of
      (o,_,[]  ) | Help `elem` o -> do
        putStr $ usageInfo header options
        exitWith ExitSuccess
      (o,_,[]  ) | Version `elem` o -> do
        putStrLn $ "git-calendar " ++ showVersion version
        exitWith ExitSuccess
      (_,n,[]  ) -> return n
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where
      header = "Usage: git-calendar [OPTION...] git-directories..."
