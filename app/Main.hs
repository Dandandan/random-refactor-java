{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Language.Java.Parser
import           Language.Java.Pretty

import qualified Control.Exception    as Exception
import           Control.Monad        (forM_, when)
import           Control.Monad.State  (evalState)
import           Data.Char (isAlphaNum, isDigit)
import qualified Data.Map             as Map
import qualified Format
import qualified Refactor
import           System.IO
import qualified System.Random        as Random

-- | Generate infinite number of infinite lists
rands :: Random.Random a => Random.StdGen -> [[a]]
rands g =
    let rs = Random.randoms g
        (_, n) = Random.next g
    in
        rs: rands n

-- | Generate random strings
randomStr :: Random.StdGen -> String
randomStr g =
    let
        (s, n) = Random.randomR ('$','z') g
    in
        s: randomStr n

randomIdentifier :: Random.StdGen -> String
randomIdentifier g = filter (\x -> isAlphaNum x || x == '_' || x == '$') (randomStr g)

removeFirstNumerics :: String -> String
removeFirstNumerics (c: cs) = if isDigit c then removeFirstNumerics cs else (c: cs)

-- | Generate random strings
randomStrs :: Random.StdGen -> String -> [String]
randomStrs g stream =
    let
        (len, n) = Random.randomR (1, 8) g
        (s, ss) = splitAt len (removeFirstNumerics stream)
    in
        s: randomStrs n ss

example :: IO ()
example = do
    text <- readFile "./testFile.java"
    let Right ast = parser compilationUnit text
    gen <- Random.newStdGen
    let rStream = randomIdentifier gen
    let refactored = evalState (Refactor.compilationUnit ast) (Refactor.Refactor (rands gen) (randomStrs gen rStream) (rands gen) Map.empty 1.0)
    let output = pretty refactored
    putStr $ show output

removeFormatting :: String -> String
removeFormatting = filter (\x -> x /= ' ' && x /= '\n' && x /= '\t')


main :: IO ()
main = do
    filesDoc <- readFile "../javafiles.txt"
    let files = lines filesDoc
    -- example
    -- return ()

    withFile "./output/pairs.txt" WriteMode $ \hp ->
     withFile "./output/unique.txt" WriteMode $ \hu ->
        forM_ (zip files [0..]) $ \(fileName, i) -> Exception.catch (do
            text <- readFile ("../" ++ fileName)
            when (length text <= 6000 && length text >= 256) $ do
                putStrLn fileName

                case parser compilationUnit text of
                    Right ast -> do
                        let
                            create origin = do
                                gen <- Random.newStdGen
                                let rStream = randomIdentifier gen
                                let refactored = evalState (Refactor.compilationUnit origin) (Refactor.Refactor (rands gen) (randomStrs gen rStream) (rands gen) Map.empty 1.0)
                                return refactored
                        a <- create ast
                        b <- create a
                        let rA = removeFormatting $ show (pretty a)
                        let rB = removeFormatting $ show (pretty b)
                        c <- create ast
                        let rC = removeFormatting $ show (pretty c)

                        when (length rA <= 2048 && length rA >= 256 &&
                              length rB <= 2048 && length rB >= 256 &&
                              length rC <= 2048 && length rC >= 256) $ do
                            writeFile ("./output/pairs/" ++ show i ++ "_a.java") rA
                            writeFile ("./output/pairs/" ++ show i ++ "_b.java") rB
                            hPutStrLn hp (show i ++ "_a.java," ++ show i ++ "_b.java")
                            
                            writeFile ("./output/unique/" ++ show i ++ ".java") rC
                            hPutStrLn hu (show i ++ ".java")

                        return ()
                    Left _ ->
                        return ()
            )
            (\ (ex :: Exception.SomeException) -> do print ex; return ())

