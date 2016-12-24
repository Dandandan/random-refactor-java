{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Language.Java.Parser
import           Language.Java.Pretty

import qualified Control.Exception    as Exception
import           Control.Monad        (forM_, when)
import           Control.Monad.State  (evalState)
import qualified Data.Map             as Map
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
        (s, n) = Random.randomR ('a','z') g
    in
        s: randomStr n

-- | Generate random strings
randomStrs :: Random.StdGen -> String -> [String]
randomStrs g stream =
    let
        (len, n) = Random.randomR (1, 8) g
        (s, ss) = splitAt len stream
    in
        s: randomStrs n ss

example :: IO ()
example = do
    text <- readFile "./testFile.java"
    let Right ast = parser compilationUnit text
    gen <- Random.newStdGen
    let rStream = randomStr gen
    let refactored = evalState (Refactor.compilationUnit ast) (Refactor.Refactor (rands gen) (randomStrs gen rStream) (rands gen) Map.empty 1.0)
    let output = pretty refactored
    putStr $ show output

main :: IO ()
main = do
    filesDoc <- readFile "../javafiles.txt"
    let files = lines filesDoc
    --example
    --return ()

    withFile "./output/pairs.txt" WriteMode $ \hp ->
     withFile "./output/unique.txt" WriteMode $ \hu ->
        forM_ (zip files [0..]) $ \(fileName, i) -> Exception.catch (do
            text <- readFile ("../" ++ fileName)
            when (length text <= 6000 && length text >= 256) $ do
                print fileName

                case parser compilationUnit text of
                    Right ast -> do
                        let
                            create origin = do
                                gen <- Random.newStdGen
                                let rStream = randomStr gen
                                let refactored = evalState (Refactor.compilationUnit origin) (Refactor.Refactor (rands gen) (randomStrs gen rStream) (rands gen) Map.empty 1.0)
                                let output = pretty refactored
                                return refactored
                        a <- create ast
                        b <- create a
                        let rA = show (pretty a)
                        let rB = show (pretty b)
                        when (length rA <= 4096 && length rA >= 256 && length rB <= 4096 && length rB >= 256) $ do
                            writeFile ("./output/pairs/" ++ show i ++ "_a.java") rA
                            writeFile ("./output/pairs/" ++ show i ++ "_b.java") rB
                        hPutStrLn hp (show i ++ "_a.java," ++ show i ++ "_b.java")
                        c <- create ast
                        let rC = show (pretty c)
                        when (length rC <= 4096 && length rC >= 256 && length rC <= 4096 && length rC >= 256) $
                            writeFile ("./output/unique/" ++ show i ++ ".java") rC
                        hPutStrLn hu (show i ++ ".java")

                        return ()
                    Left e ->
                        return ()
            )
            (\ (ex :: Exception.SomeException) -> return ())

