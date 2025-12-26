{-
opencog-cogutil.hs

OpenCog Cogutil - Haskell Utility Library
A collection of utility functions showcasing Haskell's functional paradigm

This single-file implementation demonstrates Haskell's strengths:
- Pure functions and immutability
- Strong static typing with type inference
- Higher-order functions and function composition
- Algebraic data types and pattern matching
- Monadic operations (IO, Maybe, Either)
-}

module OpenCogCogutil where

import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List (intercalate, isPrefixOf)
import Data.Char (toLower, toUpper)
import Control.Monad (when)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

-- | Log levels using algebraic data type
data LogLevel = DEBUG | INFO | WARN | ERROR
    deriving (Show, Eq, Ord)

-- | Logger configuration with current log level
data Logger = Logger {
    loggerName :: String,
    loggerLevel :: LogLevel
} deriving (Show)

-- | Create a new logger
newLogger :: String -> LogLevel -> Logger
newLogger = Logger

-- | Log a message if level is sufficient
-- Demonstrates: Pure function, pattern matching, monadic IO
logMessage :: Logger -> LogLevel -> String -> IO ()
logMessage logger level msg = 
    when (level >= loggerLevel logger) $ do
        timestamp <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
        let levelStr = show level
        putStrLn $ "[" ++ timeStr ++ "] " ++ levelStr ++ ": " ++ msg

-- | Convenience logging functions (partially applied)
logDebug, logInfo, logWarn, logError :: Logger -> String -> IO ()
logDebug = flip logMessage DEBUG
logInfo = flip logMessage INFO
logWarn = flip logMessage WARN
logError = flip logMessage ERROR

-- | Configuration as immutable Map
-- Demonstrates: Immutable data structures, type aliases
type Config = Map.Map String String

-- | Create empty config
newConfig :: Config
newConfig = Map.empty

-- | Set a config value (returns new config)
-- Demonstrates: Immutability - old config unchanged
setConfig :: String -> String -> Config -> Config
setConfig = Map.insert

-- | Get a config value with default
-- Demonstrates: Maybe monad for optional values
getConfig :: String -> String -> Config -> String
getConfig key defaultVal = Map.findWithDefault defaultVal key

-- | Load config from file (simplified)
-- Demonstrates: IO monad, pure/impure separation
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
    contents <- readFile path
    let linesOfFile = lines contents
    let validLines = filter (not . null) $ 
                     filter (not . isPrefixOf "#") linesOfFile
    let pairs = map parseLine validLines
    return $ Right $ Map.fromList pairs
  where
    parseLine line = case break (== '=') line of
        (k, '=':v) -> (trim k, trim v)
        _ -> ("", "")
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Timer result
-- Demonstrates: Algebraic data type
data TimerResult = TimerResult {
    timerName :: String,
    timerElapsed :: Double
} deriving (Show)

-- | Time an IO action
-- Demonstrates: Higher-order functions, IO monad
timeAction :: String -> IO a -> IO (a, TimerResult)
timeAction name action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let elapsed = realToFrac $ diffUTCTime end start
    return (result, TimerResult name elapsed)

-- | String utilities using pure functions
-- Demonstrates: Pure functions, list operations, function composition

-- | Split string by delimiter
splitString :: Char -> String -> [String]
splitString delim = filter (not . null) . split
  where
    split [] = [""]
    split (c:cs)
        | c == delim = "" : split cs
        | otherwise = case split cs of
            (x:xs) -> (c:x):xs
            [] -> [[c]]

-- | Join strings with delimiter
-- Demonstrates: Higher-order function (intercalate)
joinStrings :: String -> [String] -> String
joinStrings = intercalate

-- | String transformations using function composition
-- Demonstrates: Function composition (.), map, filter

-- | Convert string to lowercase (pure function)
toLowerCase :: String -> String
toLowerCase = map toLower

-- | Convert string to uppercase (pure function)
toUpperCase :: String -> String
toUpperCase = map toUpper

-- | Trim whitespace from both ends
-- Demonstrates: Function composition, point-free style
trimString :: String -> String
trimString = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Camel case to snake case conversion
-- Demonstrates: Pattern matching, recursion
camelToSnake :: String -> String
camelToSnake = map toLower . go
  where
    go [] = []
    go (x:xs)
        | x `elem` ['A'..'Z'] = '_' : x : go xs
        | otherwise = x : go xs

-- | Snake case to camel case
snakeToCamel :: String -> String
snakeToCamel s = case splitString '_' s of
    [] -> ""
    (first:rest) -> first ++ concatMap capitalize rest
  where
    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs

-- | Functional programming utilities
-- Demonstrates: Higher-order functions, currying, partial application

-- | Apply function n times
-- Demonstrates: Recursion, higher-order function
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

-- | Compose a list of functions
-- Demonstrates: Fold, function composition
composeFunctions :: [a -> a] -> (a -> a)
composeFunctions = foldr (.) id

-- | Filter and map in one pass (more efficient)
-- Demonstrates: List comprehension
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap pred f = map f . filter pred

-- | Main demonstration
main :: IO ()
main = do
    putStrLn "======================================================================"
    putStrLn "OpenCog Cogutil - Haskell Utility Library Demo"
    putStrLn "Showcasing Haskell's strengths: Pure functions, strong typing"
    putStrLn "======================================================================"
    putStrLn ""
    
    -- Logger demonstration
    putStrLn "1. Logger with Algebraic Data Types"
    putStrLn "----------------------------------------------------------------------"
    let logger = newLogger "CogUtil" INFO
    logInfo logger "Cogutil library initialized"
    logDebug logger "This debug message won't show (level too low)"
    logWarn logger "This is a warning message"
    logError logger "This is an error message"
    putStrLn ""
    
    let debugLogger = logger { loggerLevel = DEBUG }
    logDebug debugLogger "Now debug messages are visible"
    putStrLn ""
    
    -- Config demonstration (immutability)
    putStrLn "2. Immutable Configuration"
    putStrLn "----------------------------------------------------------------------"
    let config1 = newConfig
    let config2 = setConfig "opencog.version" "1.0.0" config1
    let config3 = setConfig "atomspace.enabled" "true" config2
    let config4 = setConfig "cogserver.port" "17001" config3
    
    logInfo logger "Configuration (immutable updates):"
    putStrLn $ "  opencog.version = " ++ getConfig "opencog.version" "" config4
    putStrLn $ "  atomspace.enabled = " ++ getConfig "atomspace.enabled" "" config4
    putStrLn $ "  cogserver.port = " ++ getConfig "cogserver.port" "" config4
    putStrLn ""
    
    -- Timer demonstration
    putStrLn "3. Timing Pure Computation"
    putStrLn "----------------------------------------------------------------------"
    (result, timerResult) <- timeAction "Sum calculation" $ do
        return $ sum [1..1000000]
    logInfo logger $ "Calculated sum: " ++ show result
    logInfo logger $ timerName timerResult ++ " took " ++ show (timerElapsed timerResult) ++ " seconds"
    putStrLn ""
    
    -- String utilities (pure functions)
    putStrLn "4. Pure String Functions"
    putStrLn "----------------------------------------------------------------------"
    let text = "OpenCog,AtomSpace,CogServer,Cogutil"
    let parts = splitString ',' text
    logInfo logger "Split result:"
    mapM_ (putStrLn . ("  - " ++)) parts
    
    let joined = joinStrings " + " parts
    logInfo logger $ "Joined: " ++ joined
    
    logInfo logger $ "Uppercase: " ++ toUpperCase "opencog rocks"
    logInfo logger $ "Lowercase: " ++ toLowerCase "OPENCOG ROCKS"
    logInfo logger $ "Trimmed: '" ++ trimString "  spaced out  " ++ "'"
    putStrLn ""
    
    -- Function composition
    putStrLn "5. Function Composition"
    putStrLn "----------------------------------------------------------------------"
    let transformString = toUpperCase . trimString
    logInfo logger $ "Composed transform: '" ++ transformString "  hello  " ++ "'"
    
    let increment = (+1)
    let double = (*2)
    let transform = double . increment
    logInfo logger $ "Composed math (double . increment) 5 = " ++ show (transform 5)
    putStrLn ""
    
    -- Higher-order functions
    putStrLn "6. Higher-Order Functions"
    putStrLn "----------------------------------------------------------------------"
    let numbers = [1..10]
    logInfo logger $ "Numbers: " ++ show numbers
    logInfo logger $ "Doubled: " ++ show (map (*2) numbers)
    logInfo logger $ "Evens: " ++ show (filter even numbers)
    logInfo logger $ "Sum: " ++ show (foldl (+) 0 numbers)
    
    let squared = map (^2) numbers
    logInfo logger $ "Squares: " ++ show squared
    putStrLn ""
    
    -- Advanced composition
    putStrLn "7. Advanced Composition"
    putStrLn "----------------------------------------------------------------------"
    let triple = applyN 3 (*2)
    logInfo logger $ "Apply (*2) 3 times to 1: " ++ show (triple 1)
    
    let pipeline = composeFunctions [(*2), (+10), (^2)]
    logInfo logger $ "Function pipeline (^2, +10, *2) on 3: " ++ show (pipeline 3)
    putStrLn ""
    
    -- Case conversion
    putStrLn "8. Case Conversion"
    putStrLn "----------------------------------------------------------------------"
    logInfo logger $ "camelCase → snake_case: " ++ camelToSnake "myVariableName"
    logInfo logger $ "snake_case → camelCase: " ++ snakeToCamel "my_variable_name"
    putStrLn ""
    
    logInfo logger "Cogutil demonstration complete!"
    putStrLn ""
    putStrLn "======================================================================"
    putStrLn "Haskell strengths demonstrated:"
    putStrLn "  ✓ Pure functions (no side effects)"
    putStrLn "  ✓ Immutable data structures"
    putStrLn "  ✓ Strong static typing with inference"
    putStrLn "  ✓ Algebraic data types (LogLevel, TimerResult)"
    putStrLn "  ✓ Function composition and point-free style"
    putStrLn "  ✓ Higher-order functions (map, filter, fold)"
    putStrLn "  ✓ Pattern matching and recursion"
    putStrLn "  ✓ Monadic operations (IO, Maybe, Either)"
    putStrLn "======================================================================"
