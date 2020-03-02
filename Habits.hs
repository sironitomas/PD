{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Calendar
import GHC.Generics
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as B
import qualified Data.Sequence as S
import System.Directory
import System.Environment

data Habit = Habit { name :: String
                   , occurences :: [Day]
                   } deriving (Generic, Show)

instance Eq Habit where
    x == y = name x == name y

instance Ord Habit where
    compare x y = compare (name x) (name y)

instance FromJSON Habit
instance ToJSON Habit

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", view)
            , ("add", add)
            , ("mark", mark)
            ]

view :: [String] -> IO ()
view [fileName] = do
    d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
    case d of
        Left err -> putStrLn err
        Right hs -> print hs

add :: [String] -> IO ()
add [fileName, habitName] = do
    d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
    case d of
        Left err -> putStrLn err
        Right hs -> do
            let newHabit = Habit { name = habitName, occurences = [] }
            if newHabit `elem` hs
                then putStrLn "Habit already exists"
                else B.writeFile fileName (encode (newHabit:hs))
                    -- print "Habit " ++ habitName ++ " has been added"

mark :: [String] -> IO ()
mark [fileName, habitName, dateStr] = do
    d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
    case d of
        Left err -> putStrLn err
        Right hs -> do
            let newHabit = Habit { name = habitName, occurences = [] }
            if newHabit `elem` hs
                then do
                    let i = fromJust (elemIndex newHabit hs)
                    let date = read dateStr::Day
                    let o = date : delete date (occurences (hs !! i))
                    let h = Habit { name = habitName, occurences = o }
                    let j = h : delete h hs
                    B.writeFile fileName (encode j)
                else putStrLn "Create Habit first"


getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getHabitScore :: Day -> Habit -> Int
getHabitScore today h = score where
    ds = Prelude.map (getDayScore today) (occurences h)
    score = round (100 * sum ds)

getDayScore :: Day -> Day -> Float
getDayScore d1 d2 = 1 / (5 * fromInteger(1 + diffDays d1 d2))
