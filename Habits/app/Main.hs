{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Calendar
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.Environment
import System.IO
import System.IO.Error

data Habit = Habit { name :: String
                   , occurences :: [Day]
                   } deriving (Generic, Show)

instance Eq Habit where
    x == y = map toLower (name x) == map toLower (name y)

instance Ord Habit where
    compare x y = compare (name x) (name y)

instance FromJSON Habit
instance ToJSON Habit

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", view)
            , ("add", add)
            , ("remove", remove)
            , ("mark", mark)
            ]

view :: [String] -> IO ()
view [fileName] = do
    d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
    case d of
        Left err -> putStrLn err
        Right habits -> do
            today <- getToday
            let scores = map (getHabitScore today) habits
            print $ map habitToString (zip habits scores)

add :: [String] -> IO ()
add [fileName, habitName] = do
    fileExists <- doesFileExist fileName
    let newHabit = Habit { name = habitName, occurences = [] }
    if fileExists
        then do
            d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
            case d of
                Left err -> putStrLn err
                Right habits -> if newHabit `elem` habits
                    then putStrLn "Habit already exists"
                    else B.writeFile fileName (encode (newHabit:habits))
                            -- print "Habit " ++ habitName ++ " has been added"
        else do
            B.writeFile fileName (encode [newHabit])
            putStrLn "The file doesn't exist, created a new one with the habit."

remove :: [String] -> IO ()
remove [fileName, habitName] = do
    fileExists <- doesFileExist fileName
    let newHabit = Habit { name = habitName, occurences = [] }
    if fileExists
        then do
            d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
            case d of
                Left err -> putStrLn err
                Right habits -> if newHabit `elem` habits
                    then B.writeFile fileName (encode (delete newHabit habits))
                    else putStrLn "Habit doesn't exist"
        else putStrLn "The file doesn't exist."

mark :: [String] -> IO ()
mark [fileName, habitName, dateStr] = do
    d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
    case d of
        Left err -> putStrLn err
        Right habits -> do
            let newHabit = Habit { name = habitName, occurences = [] }
            if newHabit `elem` habits
                then do
                    let i = fromJust (elemIndex newHabit habits)
                    let date = read dateStr::Day
                    let o = date : delete date (occurences (habits !! i))
                    let h = Habit { name = habitName, occurences = o }
                    let j = h : delete h habits
                    B.writeFile fileName (encode j)
                else putStrLn "Please, add the Habit first"

getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getHabitScore :: Day -> Habit -> Int
getHabitScore today h = score where
    ds = Prelude.map (getDayScore today) (occurences h)
    score = safeScore (round (100 * sum ds))

safeScore :: Int -> Int
safeScore s
    | s >= 100 = 100
    | otherwise = s

getDayScore :: Day -> Day -> Float
getDayScore d1 d2 = 1 / (3 * fromInteger(1 + diffDays d1 d2))

habitToString :: (Habit, Int) -> String
habitToString (h, s) = name h ++ ": " ++ show s ++ "%"
