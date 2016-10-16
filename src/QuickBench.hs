{-# LANGUAGE ScopedTypeVariables #-}

module QuickBench (
  defaultMain
)
where

import Control.Exception
import Control.Monad
import Data.List
import Data.Time.Clock
import Data.Time.Format ()
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf
import Text.Tabular
import qualified Text.Tabular.AsciiArt as TA

usage :: String
usage = usageInfo usagehdr options ++ usageftr

usagehdr :: String
usagehdr = "quickbench [-f TESTSFILE] [-nITERATIONS] [-NCYCLES] [-pPRECISION] [-v|-V] [EXES]\n" ++
           "\n" ++
           "Combine zero or more executables with each of the lines in TESTFILE,\n" ++
           "run the resulting commands once or more, and show the best execution times.\n"

options :: [OptDescr Opt]
options = [
  Option "f" ["file"] (ReqArg File "TESTSFILE") "file containing tests, one per line, default: bench.tests"
 ,Option "n" ["iterations"] (ReqArg Iterations "N") "run each test this many times, default: 1"
 ,Option "N" ["cycles"] (ReqArg Cycles "N") "run the whole suite this many times, default: 1"
 ,Option "p" ["precision"] (ReqArg Prec "N") "show times with this many decimal places, default: 2"
 ,Option "v" ["verbose"] (NoArg Verbose) "show commands being run"
 ,Option "V" ["more verbose"] (NoArg MoreVerbose) "show command output"
 ,Option "h" ["help"] (NoArg Help) "show this help"
 ]             

usageftr :: String
usageftr = "\n" ++
           "Tips:\n" ++
           "- tests can be commented out with #\n" ++
           "- executables and additional arguments can be combined in quotes\n"
           -- "- results are saved in benchresults.{html,txt}\n"

data Opt = File {value::String}
         | Iterations  {value::String}
         | Cycles  {value::String}
         | Prec {value::String}
         | Verbose
         | MoreVerbose
         | Help
           deriving (Eq,Show)

-- options helpers

fileopt :: [Opt] -> String
fileopt = optValueWithDefault File "bench.tests"

-- TODO read errors should throw a userError
precisionopt :: [Opt] -> Int
precisionopt = read . optValueWithDefault Prec "2"

iterationsopt :: [Opt] -> Int
iterationsopt = read . optValueWithDefault Iterations "1"

cyclesopt :: [Opt] -> Int
cyclesopt = read . optValueWithDefault Cycles "1"

verboseopt :: [Opt] -> Bool
verboseopt opts = Verbose `elem` opts || moreverboseopt opts

moreverboseopt :: [Opt] -> Bool
moreverboseopt = (MoreVerbose `elem`)

optValueWithDefault :: (String -> Opt) -> String -> [Opt] -> String
optValueWithDefault optcons def opts =
    last $ def : optValuesForConstructor optcons opts

optValuesForConstructor :: (String -> Opt) -> [Opt] -> [String]
optValuesForConstructor optcons opts = concatMap get opts
    where get o = [v | optcons v == o] where v = value o

parseArgs :: [String] -> IO ([Opt],[String])
parseArgs as =
  case (getOpt Permute options as) of
    (_,_,errs@(_:_)) -> fail $ concat errs
    (opts,[],_)      -> return (opts, [""])
    (opts,args,[])   -> return (opts,args)

-- | Run the quickbench program, returning an error message if there was a problem.
defaultMain :: IO (Maybe String)
defaultMain =
  (runSuite >> return Nothing)
    `catch` \(e :: IOException) -> return $ Just $ show e
  where
    runSuite = do
      args <- getArgs
      if ("-h" `elem` args || "--help" `elem` args)
      then
        putStr usage
      else do
        (opts,exes) <- parseArgs args
        let (file, num) = (fileopt opts, iterationsopt opts)
        tests <- liftM (filter istest . lines) (readFile file)
        now <- getCurrentTime
        putStrLn $ printf "Using %s" file
        putStrLn $ printf "Running %d tests %d times with %d executables at %s:"
                     (length tests) num (length exes) (show now)
        hSetBuffering stdout NoBuffering
        let
          runTestWithExes t = mapM (runTestWithExe t) exes
          runTestWithExe t e = mapM (runTestOnce opts t e) [1..num]
        forM [1..cyclesopt opts] $ \cyc -> do
          results <- mapM runTestWithExes tests
          summarise opts tests exes cyc results
        return ()

istest :: String -> Bool
istest s = not (null s' || ("#" `isPrefixOf` s')) where s' = clean s

clean :: String -> String
clean = unwords . words

runTestOnce :: [Opt] -> String -> String -> Int -> IO Float
runTestOnce opts argsstr exestr iteration = do
  let
    cmd = clean $ exestr ++ " " ++ argsstr
    ws = words cmd
  case ws of
    [] -> fail "some command should be provided"
    exe:args -> do
      when (verboseopt opts) $ putStr (show iteration ++ ": " ++ cmd) >> hFlush stdout
      t <- time opts exe args
      when (verboseopt opts) $ printf "\t[%ss]\n" (showtime opts t)
      return t

time :: [Opt] -> String -> [String] -> IO Float
time opts exe args = do
  t1 <- getCurrentTime
  (code, out, err) <- readProcessWithExitCode' exe args ""
  t2 <- getCurrentTime
  when (moreverboseopt opts && not (null out)) $
    putStr $ (if verboseopt opts then "\n" else "")++out
  unless (code == ExitSuccess) $
    putStr $ " (error: " ++ clean err ++ ") "
  return $ realToFrac $ diffUTCTime t2 t1

-- ^ Return a failure when the executable is missing, also.
readProcessWithExitCode' :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode' exe args inp =
  readProcessWithExitCode exe args inp
    `catch` \(e :: IOException) -> return (ExitFailure 1, "", show e)

summarise :: [Opt] -> [String] -> [String] -> Int -> [[[Float]]] -> IO ()
summarise opts tests exes cyc results = do
  putStrLn $ printf "\nBest times%s:" (if cyclesopt opts > 1 then " "++show cyc else "")
  let t = maketable opts tests exes results
  putStrLn $ TA.render id id id t
  -- let outname = "benchresults"
  -- writeFile (outname <.> "txt") $ TA.render id id id t
  -- writeFile (outname <.> "html") $ renderHtml $ TH.css TH.defaultCss +++ TH.render stringToHtml stringToHtml stringToHtml t

maketable :: [Opt] -> [String] -> [String] -> [[[Float]]] -> Table String String String
maketable opts rownames colnames results = Table rowhdrs colhdrs rows
 where
  rowhdrs = Group NoLine $ map Header $ padright rownames
  colhdrs = Group SingleLine $ map Header colnames
  rows = map (map (showtime opts . minimum)) results
  padright ss = map (printf (printf "%%-%ds" w)) ss
      where w = maximum $ map length ss

showtime :: [Opt] -> (Float -> String)
showtime opts = printf $ "%." ++ show (precisionopt opts) ++ "f"
