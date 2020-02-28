{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text
import Data.Time
import Data.Time.Calendar
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.Environment
import System.IO

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile

data Habit = Habit { name :: Text
                   , occurences :: [Day]
                   } deriving (Generic, Show)

instance FromJSON Habit
instance ToJSON Habit

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", view) ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

view :: [String] -> IO ()
view [fileName] = do
    -- Get JSON data and decode it
    d <- (eitherDecode <$> getJSON fileName) :: IO (Either String [Habit])
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    case d of
        Left err -> putStrLn err
        Right ps -> do
            today <- getToday
            print (Prelude.map (getHabitScore today) ps)

getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getHabitScore :: Day -> Habit -> Int
getHabitScore today h = score where
    ds = Prelude.map (getDayScore today) (occurences h)
    score = round (100 * sum ds)

getDayScore :: Day -> Day -> Float
getDayScore d1 d2 = ds where
    ds = 1 / (5 * fromInteger(1 + diffDays d1 d2))
