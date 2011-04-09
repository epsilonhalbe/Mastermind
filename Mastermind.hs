--module Mastermind (reds, whites, parseInts) where

{- my (=ε/2) haskellous version of the game called mastermind -}

import Data.List.Split (splitOneOf)
{- has to be fetched with
    foo@bar~> cabal update
    foo@bar~> cabal install split
-}
import Random
import Data.Maybe
import Control.Applicative
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    seed <- newStdGen
    putStrLn "\nplease do give me a length or accept that it will be 4"
    laength <- fmap (head . (dvalue 4) . parseInts) getLine
    putStrLn "\nuuhm and the number of colours would be cool too or accept a 5"
    colours <- fmap (head . (dvalue 5) . parseInts) getLine
--    putStrLn ("laength="++show(laength)++" and colours="++show(colours))
    list_of_randoms <- return (take laength (randomRs (1,colours) seed::[Int])){-todo mkStdGen with system time-}
--    putStrLn(show(list_of_randoms))
    game_loop laength colours list_of_randoms
    putStrLn "\nWant to play again??"
    hSetBuffering stdin NoBuffering
    c <- getChar
    regame c
        where regame c
                | elem c "yY" = do
                     putStrLn "\ngame on mate"
                     main
                | elem c "nN" = putStrLn "\nGame Over"
                | otherwise = do
                     putStrLn "\nyou must type one of Yy to confirm or nN to abort"
                     c'<- getChar
                     regame c'

game_loop :: Int -> Int -> [Int] -> IO ()
game_loop laength colours list_of_randoms = do
    list_of_guesses <- fetch_guesslist laength colours
    if (list_of_guesses == list_of_randoms)
        then putStrLn "\ncorrect guess"
        else do
            putStrLn (show ((reds list_of_randoms list_of_guesses) ++ (whites list_of_randoms list_of_guesses)))
            game_loop laength colours list_of_randoms

reds :: (Num a) => [a]->[a]->[a]{-denotes the right colours & right positions -}
reds randoms guesses = filter (==0) (zipWith (-) randoms guesses)
{- reds [1,2,3,4,5] [5,4,3,2,1] ~~> filter (==0) [-4,-2,0,2,5] ~~> [0] -}

whites :: (Num a) => [a]->[a]->[a] {- denotes the right colours but on wrong positions -}
whites randoms guesses = map (const 1) (
    filter (\x -> elem x guesses) (zipWith (*) randoms helper))
        where helper = (map (signum.abs) (zipWith (-) randoms guesses))
{-
whites [1,2,3,4,5] [5,4,3,2,1]
~~>        filter (\x -> elem x [5,4,3,2,1]) (zipWith (*) [1,2,3,4,5] [1,1,0,1,1]
~~>        filter (\x -> elem x [5,4,3,2,1]) [1,2,0,4,5] {- essentially deletes the "red" guesses -}
~~>        [1,2,4,5]
~~>        map (const 1) [1,2,4,5] ~~> [1,1,1,1]

helper [1,2,3,4,5] [5,4,3,2,1]
~~>        (map (signum.abs) [-4,-2,0,2,5])
~~>        [1,1,0,1,1]

{- for those who forgot their math:
(signum.abs) x =
        | x == 0 = 0
        | otherwise = 1 -}
-}

--parse input from getLine
maybeReads :: Read a => String -> Maybe a
maybeReads = fmap fst . listToMaybe . reads

{- todo make default values-}
dvalue :: (Show a) => a -> [a] -> [a]
dvalue dfault x
    | null x = [dfault]
    | otherwise = x

parseInts :: String -> [Int]
parseInts = catMaybes . map maybeReads . splitOneOf ",.;: "

fetch_guesslist ::Int -> Int -> IO [Int]
fetch_guesslist l c = do
    putStrLn ("\nplease do give me a sequence of "++show(l)++" numbers between 1 and "++show(c)++" separated by \",\"") {- 4 should be replaced by 'length' and 5 by 'colours' - fixeme -}
    parseInts <$> getLine

{- $ is evaluate; it returns the result of the input function - has to be used since line is an action "<-" and the action has to be evaluated; still a little mystery to me (= ε/2) -}
