{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}

module QuickBench (
  defaultMain
)
where

import Control.Exception
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
-- import Debug.Trace
import Safe
import System.Console.Docopt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Show.Pretty
import Text.Printf
import Text.Tabular
import qualified Text.Tabular.AsciiArt as TA

-- Command line help, parsed by docopt to generate the CLI.
-- docopt's parsing is fragile and easy to break; always test after changing this.
-- https://github.com/docopt/docopt.hs#readme
-- https://github.com/docopt/docopt.hs?tab=readme-ov-file#help-text-format
-- https://hackage.haskell.org/package/docopt-0.7.0.8/docs/System-Console-Docopt.html
-- Remember to keep versions etc synced between here, quickbench.cabal, quickbench.1.md, README.md.
docoptpatterns :: Docopt
docoptpatterns = [docopt|
------------------------------------------------------------------------------
quickbench 1.1 - run programs and show how long they took.

Each command must begin with the name of an executable (not a shell command).
Multi-word commands should be enclosed in quotes.
Commands can also be read from a file, specified with -f.
If neither of these are provided, it looks for commands in `./bench.sh`.
Any quickbench options must be written first, before the commands.

Usage:
  quickbench [options] [-f CMDFILE] [CMD...]

Options:
  -f, --file CMDFILE    file containing commands, one per line (- for stdin)
  -w, --with EXE[,...]  run commands with first word replaced by each EXE
  -n, --iterations=N    run each command this many times [default: 1]
  -N, --cycles=N        run the whole test suite this many times [default: 1]
  -p, --precision=N     show times with this many decimal places [default: 2]
  -v, --verbose         show the commands being run
  -V, --more-verbose    show the commands' output
      --debug           show this program's debug output
  -h, --help            show this help

|]

defaultFile :: FilePath
defaultFile = "bench.sh"

data Opts = Opts {
   file        :: Maybe FilePath
  ,executables :: [String]
  ,iterations  :: Int
  ,cycles      :: Int
  ,precision   :: Int
  ,verbose     :: Bool
  ,moreVerbose :: Bool
  ,debug       :: Bool
  ,help        :: Bool
  ,clicmds     :: [String]
} deriving (Show)

getOpts :: IO Opts
getOpts = do
  cliargs <- getArgs
  case parseArgs docoptpatterns cliargs of
    Left e -> putStrLn "Could not parse command line:" >> print e >> exitFailure
    Right dopts -> do
      let
        flag f   = dopts `isPresent` longOption f
        option f = dopts `getArg`    longOption f
        readint s =
          case readMay s of
            Just a  -> return a
            Nothing -> fail $ "could not read " ++ show s ++ " as an integer"
        (lateflags, args) = partition ("-" `isPrefixOf`) $ dopts `getAllArgs` (argument "CMD")
      -- These will have a value because of their [default: ...] above.
      -- Use fromJust to show an error if the default is accidentally removed.
      iterations' <- readint $ fromJust $ option "iterations"
      cycles'     <- readint $ fromJust $ option "cycles"
      precision'  <- readint $ fromJust $ option "precision"
      let
        opts = Opts {
          file        = option "file"
          ,executables = maybe [] (splitOn ",") $ option "with"
          ,iterations  = iterations'
          ,cycles      = cycles'
          ,precision   = precision'
          ,verbose     = flag "verbose"
          ,moreVerbose = flag "more-verbose"
          ,debug       = flag "debug"
          ,help        = flag "help"
          ,clicmds     = args
          }

      when (debug opts || "--debug" `elem` lateflags) $ do
        err $ "docopts: " ++ ppShow dopts ++ "\n" ++ ppShow opts ++ "\n"

      when (help opts) $ putStrLn (usage docoptpatterns) >> exitSuccess

      -- try to report some errors docopts misses
      case (lateflags, args) of
        -- unknown option
        -- quickbench a -fk       user error (unknown option: "fk")
        -- (f:_,[]) | not $ elem f [
        --    "file","f"
        --   ,"with","w"
        --   ,"iterations","n"
        --   ,"cycles","N"
        --   ,"precision","p"
        --   ,"verbose","v"
        --   ,"more-verbose","V"
        --   ] -> fail $ printf "unknown option: %s" (show f)
        -- option value missing
        (f:_,[]) -> fail $ printf "option %s needs a value or is unknown" (show f)
        -- option following arguments
        (f:_,a:_) -> fail $ printf "option %s should appear before argument %s or is unknown" (show f) (show a)
        _ -> return ()
      return opts

-- | Run the quickbench program, returning an error message if there was a problem.
defaultMain :: IO (Maybe String)
defaultMain =
  (runSuite >> return Nothing)
    `catch` \(e :: SomeException) -> return $
      if fromException e == Just ExitSuccess
      then Nothing
      else Just $ show e
  where
    runSuite = do
      opts <- getOpts
      filecmds <-
        (filter istest . lines) <$>
        (case (file opts, clicmds opts) of
          (Just "-", _) -> getContents
          (Just f, _)   -> readFile f
          (Nothing, []) -> doesFileExist defaultFile >>=
            \yes -> if yes then readFile defaultFile else return ""
          (Nothing, _)  -> return [])
      let cmds = filecmds ++ clicmds opts
      when (null cmds) $ do
        out opts "No test commands found; provide some as arguments, with -f, or in ./bench.sh\n"
        exitSuccess
      now <- getCurrentZonedTime
      out opts $ printf "Running %d tests %d times%s at %s:\n"
        (length cmds)
        (iterations opts)
        (case executables opts of
          [] -> ""
          es -> printf " with %d executables" (length es))
        (formatTime defaultTimeLocale "%Y-%m-%d %T %Z" now)
      let
        exes = case executables opts of
          [] -> [""]
          es -> es
      hSetBuffering stdout NoBuffering
      forM_ [1..cycles opts] $ \cyc -> do
        results <- mapM (runTestWithExes opts exes) cmds
        printSummary opts cmds exes cyc results

getCurrentZonedTime :: IO ZonedTime
getCurrentZonedTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToZonedTime tz t

runTestWithExes :: Opts -> [String] -> String -> IO [[Float]]
runTestWithExes opts exes cmd = mapM (runTestWithExe opts cmd) exes

runTestWithExe :: Opts -> String -> String -> IO [Float]
runTestWithExe opts cmd exe = mapM (runTestOnce opts cmd exe) [1..iterations opts]

runTestOnce :: Opts -> String -> String -> Int -> IO Float
runTestOnce opts cmd exe iteration = do
  let (cmd',exe',args) = replaceExecutable exe cmd
  dbg opts $ printf "replaceExecutable: %s -> %s\n" (show (cmd,exe)) (show (cmd',exe',args))
  outv opts (show iteration ++ ": " ++ cmd' ++ "\n")
  t <- time opts exe' args
  outv opts $ printf "\t[%ss]\n" (showtime opts t)
  return t

-- | Replace a command line's command (first word) with the given executable.
-- Returns the new command line, its command, and its arguments.
-- If the command line was empty, the executable is used as the new command line.
-- If the executable was empty, the command line remains unchanged.
-- (And if both were empty, return empty strings.)
replaceExecutable :: String -> String -> (String,String,[String])
replaceExecutable exe cmdline =
  case (exe, words $ clean cmdline) of
    ("", [])       -> ("",  "",  [])
    (_,  [])       -> (exe, exe, [])
    ("", cmd:args) -> (unwords $ cmd:args, cmd, args)
    (_,  _:args)   -> (unwords $ exe:args, exe, args)
  -- XXX might display wrong quoting here

time :: Opts -> String -> [String] -> IO Float
time opts exe args = do
  t1 <- getCurrentTime
  (c, o, e) <- readProcessWithExitCode' exe args ""
  t2 <- getCurrentTime
  when (not $ null o) $ outvv opts $ (if verbose opts then "\n" else "") ++ o
  unless (c == ExitSuccess) $ out opts $ " (error: " ++ clean e ++ ") "
  return $ realToFrac $ diffUTCTime t2 t1

-- ^ This variant also returns a failure when the executable is missing.
readProcessWithExitCode' :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode' exe args inp =
  readProcessWithExitCode exe args inp
    `catch` \(e :: IOException) -> return (ExitFailure 1, "", show e)

printSummary :: Opts -> [String] -> [String] -> Int -> [[[Float]]] -> IO ()
printSummary opts cmds exes cyc results = do
  out opts $ printf "\nBest times%s:\n" (if cycles opts > 1 then " "++show cyc else "")
  let t = maketable opts cmds' exes results
  out opts $ TA.render id id id t
  -- let outname = "benchresults"
  -- writeFile (outname <.> "txt") $ TA.render id id id t
  -- writeFile (outname <.> "html") $ renderHtml $ TH.css TH.defaultCss +++ TH.render stringToHtml stringToHtml stringToHtml t
  where
    cmds' =
      case executables opts of
        []  -> cmds
        [e] -> [c | (c,_,_) <- map (replaceExecutable e) cmds]
        _   -> map (unwords . drop 1 . words) cmds

maketable :: Opts -> [String] -> [String] -> [[[Float]]] -> Table String String String
maketable opts rownames colnames results = Table rowhdrs colhdrs rows
 where
  rowhdrs = Group NoLine $ map Header $ padright rownames
  colhdrs = Group SingleLine $ map Header colnames
  rows = map (map (showtime opts . minimum)) results
  padright ss = map (printf (printf "%%-%ds" w)) ss
      where w = maximum $ map length ss

showtime :: Opts -> (Float -> String)
showtime opts = printf $ "%." ++ show (precision opts) ++ "f"

istest :: String -> Bool
istest s = not (null s' || ("#" `isPrefixOf` s')) where s' = clean s

clean :: String -> String
clean = unwords . words

out :: Opts -> String -> IO ()
out _ = putStr

outv :: Opts -> String -> IO ()
outv opts s = when (verbose opts || moreVerbose opts) $ putStr s

outvv :: Opts -> String -> IO ()
outvv opts s = when (moreVerbose opts) $ putStr s

err :: String -> IO ()
err = hPutStr stderr

dbg :: Opts -> String -> IO ()
dbg opts s = when (debug opts) $ err s

