{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module QuickBench
-- (
--   defaultMain
-- )
where

-- import Debug.Trace
import Control.Exception
import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Safe
import System.Console.Docopt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Megaparsec (ParsecT, Stream (Token), between, many, noneOf, runParser, satisfy, sepBy, takeWhile1P, (<|>))
import Text.Megaparsec.Char (char)
import Text.Show.Pretty
import Text.Printf
import Text.Tabular
import qualified Text.Tabular.AsciiArt as TA


-- Command line help, parsed by docopt to generate the CLI.
-- This is fragile and easy to break; always test after any changes.
-- See also: https://github.com/docopt/docopt.hs?tab=readme-ov-file#help-text-format
-- https://hackage.haskell.org/package/docopt-0.7.0.8/docs/System-Console-Docopt.html
-- Keep version & doc synced: here, quickbench.cabal, quickbench.1.md, README.md
docoptpatterns :: Docopt
docoptpatterns = [docopt|
------------------------------------------------------------------------------
quickbench 1.1 - run commands and show how long they took.

Usage:
  quickbench [options] [CMD...]

Each command's first word should be an executable (not a shell builtin).
Multi-word commands must be enclosed in quotes.
Commands can also be read from a file specified with -f.
If no commands are provided, it looks for commands in ./bench.sh.
Any quickbench options should be written first, before the commands.

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
        flag f = dopts `isPresent` longOption f
        opt f  = dopts `getArg`    longOption f
        readint s =
          case readMay s of
            Just a  -> return a
            Nothing -> fail $ "could not read " ++ show s ++ " as an integer"
        (lateflags, args) = partition ("-" `isPrefixOf`) $ dopts `getAllArgs` (argument "CMD")
      -- These will have a value because of their [default: ...] above.
      -- Use fromJust to show an error if the default is accidentally removed.
      iterations' <- readint $ fromJust $ opt "iterations"
      cycles'     <- readint $ fromJust $ opt "cycles"
      precision'  <- readint $ fromJust $ opt "precision"
      let
        opts = Opts {
          file         = opt "file"
          ,executables = maybe [] (splitOn ",") $ opt "with"
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
      let istest s = not (null s' || ("#" `isPrefixOf` s')) where s' = strip s
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
  when (not $ null exe) $ dbg opts $ "replaced executable with " <> show exe
  outv opts (show iteration ++ ": " ++ cmd' ++ "\n")
  t <- time opts exe' args
  outv opts $ printf "\t[%ss]\n" (showtime opts t)
  return t

-- XXX confusing
-- | Replace a command line's command (first word) with the given executable.
-- Returns the new command line, its command, and its arguments.
-- If the command line was empty, the executable is used as the new command line.
-- If the executable was empty, the command line remains unchanged.
-- (And if both were empty, return empty strings.)
replaceExecutable :: String -> String -> (String,String,[String])
replaceExecutable exe cmdline =
  case (exe, words' cmdline) of
    ("", [])       -> ("",  "",  [])
    (_,  [])       -> (exe, exe, [])
    ("", cmd:args) -> (unwords $ cmd:args, cmd, args)
    (_,  _:args)   -> (unwords $ exe:args, exe, args)

time :: Opts -> String -> [String] -> IO Float
time opts exe args = do
  dbg opts $ printf "running: %s\n" (show (exe,args))
  t1 <- getCurrentTime
  (c, o, e) <- readProcessWithExitCode' exe args ""
  t2 <- getCurrentTime
  when (not $ null o) $ outvv opts $ (if verbose opts then "\n" else "") ++ o
  unless (c == ExitSuccess) $ out opts $ " (error: " ++ strip e ++ ") "
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

---------------------------------------
-- utils

-- IO

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

showtime :: Opts -> (Float -> String)
showtime opts = printf $ "%." ++ show (precision opts) ++ "f"

-- Strings

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = lstrip . rstrip

-- | Remove leading whitespace.
lstrip :: String -> String
lstrip = dropWhile isSpace

-- | Remove trailing whitespace.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- XXX megaparsec dependency was added just for this
-- Quote-aware version of words - don't split on spaces which are inside quotes.
-- NB correctly handles "a'b" but not "''a''".
-- Partial, can raise an error if parsing fails.
words' :: String -> [String]
words' ""  = []
words' str = map stripquotes $ either errexit id $ runParser p "" str
 where
  errexit = errorWithoutStackTrace . ("parse error at " ++) . show
  p = (singleQuotedPattern <|> doubleQuotedPattern <|> patterns) `sepBy` skipNonNewlineSpaces1
  singleQuotedPattern = between (char '\'') (char '\'') (many $ noneOf "'")
  doubleQuotedPattern = between (char '"') (char '"') (many $ noneOf "\"")
  patterns = many (satisfy $ not . isSpace)
  skipNonNewlineSpaces1 :: (Stream s, Token s ~ Char) => ParsecT String s m ()
  skipNonNewlineSpaces1 = void $ takeWhile1P Nothing $ \c -> not (c == '\n') && isSpace c

  -- | Strip one matching pair of single or double quotes on the ends of a string.
  stripquotes :: String -> String
  stripquotes s = if isSingleQuoted s || isDoubleQuoted s then init $ tailErr s else s -- PARTIAL tailErr won't fail because isDoubleQuoted

  isSingleQuoted :: String -> Bool
  isSingleQuoted s@(_ : _ : _) = headErr s == '\'' && last s == '\'' -- PARTIAL headErr, last will succeed because of pattern
  isSingleQuoted _ = False

  isDoubleQuoted :: String -> Bool
  isDoubleQuoted s@(_ : _ : _) = headErr s == '"' && last s == '"' -- PARTIAL headErr, last will succeed because of pattern
  isDoubleQuoted _ = False
