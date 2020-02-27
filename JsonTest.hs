{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Time.Calendar
import Data.Maybe

data Habit = Habit { name :: Text
                   , occurences :: [Day]
                   } deriving (Generic, Show)

instance FromJSON Habit
instance ToJSON Habit

main :: IO ()
main = do
    putStrLn $ "Encode: " ++ (show (encode (Habit { name = "Run", occurences = [fromJust (fromGregorianValid 2020 02 24)]})))
    putStrLn $ "Decode: " ++ (show (decode "{ \"name\": \"Run\", \"occurences\": [\"2020-02-24\"] }" :: Maybe Habit))
