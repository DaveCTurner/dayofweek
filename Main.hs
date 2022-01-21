module Main where

import Control.Monad
import System.Random
import Data.Time.Calendar
import Data.Time
import Data.Time.Format
import System.IO
import Data.Char
import Text.Printf
import System.Process
import qualified Text.Numeral.Language.ENG as EN
import Text.Numeral.Grammar
import qualified Data.Text as T
import Data.Maybe

main :: IO ()
main = do
    rng <- getStdGen
    hSetBuffering stdin NoBuffering
    loop rng 0

firstDay :: Day
firstDay = fromGregorian 1753 01 01

lastDay :: Day
lastDay = fromGregorian 2499 12 31

isHardCase :: Day -> Bool
isHardCase d = (yy `mod` 4) == 0 && mm <= 2 where (yy, mm, _) = toGregorian d

randomDay :: RandomGen g => Bool -> g -> (Day, g)
randomDay wantHardCase g = let
    (dayNum, g') = randomR
                    ((toModifiedJulianDay firstDay)
                    ,(toModifiedJulianDay lastDay))
                    g
    day = ModifiedJulianDay dayNum
    in if wantHardCase && not (isHardCase day)
       then randomDay wantHardCase g'
       else (day, g')

ordinalWords :: Integral a => a -> Maybe String
ordinalWords x = T.unpack <$> EN.gb_ordinal defaultInflection x

cardinalWords :: Integral a => a -> Maybe String
cardinalWords x = T.unpack <$> EN.gb_cardinal defaultInflection x

loop :: StdGen -> Int -> IO ()
loop rng0 correctAnswerCount = let
    (hardCaseInt, rng1) = randomR (0,10::Int) rng0
    wantHardCase = hardCaseInt < 2
    (day, rng2) = randomDay wantHardCase rng1
  in do
    putStrLn "Press any key when ready"
    _ <- getChar
    let text = fromMaybe (error $ show day) $ do
            let (year, month, dayOfMonth) = toGregorian day
                (century, yearOfCentury) = year `divMod` 100
            dayOfMonthWords       <- ordinalWords dayOfMonth
            monthWord             <- lookup month $ zip [1..] $ words "January February March April May June July August September October November December"
            centuryWords          <- cardinalWords century
            yearOfCenturyNumWords <- cardinalWords yearOfCentury
            let yearOfCenturyWords = if yearOfCentury == 0 then "hundred"
                                     else if yearOfCentury < 9 then "oh " ++ yearOfCenturyNumWords
                                     else yearOfCenturyNumWords
            let yearWords = if year == 2000 then "two thousand"
                            else if 2001 <= year && year <= 2009 then "two thousand and " ++ yearOfCenturyNumWords
                            else centuryWords ++ " " ++ yearOfCenturyWords
            return $ concat
                [ "the "
                , dayOfMonthWords
                , " of "
                , monthWord
                , ", "
                , yearWords
                ]
    callProcess "say" [text]
    startTime <- getCurrentTime
    answer <- getAnswer
    endTime <- getCurrentTime
    let isCorrect = fromIntegral answer == (toModifiedJulianDay day + 3) `mod` 7
        elapsedTime = floor (1000 * diffUTCTime endTime startTime)
    putStrLn $ printf "\r%s answer in %dms: %s"
        (if isCorrect then "Correct" else ("WRONG (" ++ show answer ++ ")"))
        (floor (1000 * diffUTCTime endTime startTime) :: Int)
        (formatTime defaultTimeLocale "%e %B %0Y == %A" day)
    putStrLn ""
    let correctAnswerCount' = correctAnswerCount + if isCorrect then 1 else 0
    when (correctAnswerCount' < 5) $ loop rng2 correctAnswerCount'

getAnswer :: IO Int
getAnswer = do
    c <- getChar
    if '0' <= c && c <= '6' then return (ord c - ord '0') else do
        putStrLn "\rexpecting '0' through '6' (for Sunday through Saturday), try again"
        getAnswer
