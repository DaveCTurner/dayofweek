module Main where

import Control.Monad
import System.Random
import Data.Time.Calendar
import Data.Time
import Data.Time.Format
import System.IO
import Data.Char
import Text.Printf

main :: IO ()
main = do
    rng <- getStdGen
    hSetBuffering stdin NoBuffering
    loop rng 0

firstDay :: Day
firstDay = fromGregorian 1753 01 01

lastDay :: Day
lastDay = fromGregorian 2499 12 31

randomDay :: RandomGen g => g -> (Day, g)
randomDay g = let 
    (dayNum, g') = randomR 
                    ((toModifiedJulianDay firstDay)
                    ,(toModifiedJulianDay lastDay))
                    g
    in (ModifiedJulianDay dayNum, g')

loop :: StdGen -> Int -> IO ()
loop rng correctAnswers = let (day, rng') = randomDay rng in do
    putStrLn $ formatTime defaultTimeLocale "%e %B %0Y" day
    startTime <- getCurrentTime
    answer <- getAnswer
    endTime <- getCurrentTime
    let isCorrect = fromIntegral answer == (toModifiedJulianDay day + 3) `mod` 7
        elapsedTime = floor (1000 * diffUTCTime endTime startTime)
    putStrLn $ printf "\r%s answer in %dms: %s"
        (if isCorrect then "Correct" else "WRONG")
        (floor (1000 * diffUTCTime endTime startTime) :: Int)
        (formatTime defaultTimeLocale "%e %B %0Y == %A" day)
    putStrLn ""
    let correctAnswers' = correctAnswers + if isCorrect then 1 else 0
    when (correctAnswers' < 5) $ loop rng' correctAnswers'

getAnswer :: IO Int
getAnswer = do
    c <- getChar
    if '0' <= c && c <= '6' then return (ord c - ord '0') else do
        putStrLn "\rexpecting '0' through '6' (for Sunday through Saturday), try again"
        getAnswer
