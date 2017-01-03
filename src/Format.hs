module Format
    ( java
    , Format (..)
    ) where

import           Control.Monad.State
import Data.List (isPrefixOf, stripPrefix)

data Format = Format 
    { remaining :: String
    , stack :: String
    , rands :: [Double]
    }

getRandom :: State Format Double
getRandom = do
    f@Format { rands = (r:rs) } <- get
    put (f { rands = rs })
    return r

consume :: State Format (Maybe Char)
consume = do
    f@Format { remaining = r } <- get
    let 
        (remainingTail, res) = 
            case r of
                "" -> ("", Nothing)
                r:rs -> (rs, Just r)
    put (f { remaining = remainingTail })
    return res

tryReplace :: String -> String -> String -> Double -> Double -> (String, String)
tryReplace prefix replacement remaining prob r =
    if isPrefixOf prefix remaining && prob >= r then
        let Just xs = stripPrefix prefix remaining
        in (replacement, xs)
    else
        ("", remaining)
        


reformat :: State Format String
reformat = do
    f@Format { remaining = rm } <- get
    let 
        rules = 
            [ ("    ", "\t", 0.1)
            , ("\n", "\n\n", 0.1)
            , (" ", "", 0.1)
            , (" ", "  ", 0.1)
            , ("\t", "    ", 0.1)
            , ("", "", 1.0)
            ]
    repls <-
            forM rules $ \(prefix, replacement, prob) -> do
                r <- getRandom
                f@Format { remaining = rm } <- get
                let (repl, rm') = tryReplace prefix replacement rm prob r
                put (f {remaining = if replacement /= "" then rm' else if rm == "" then "" else tail rm})
                return (if replacement /= "" then repl else if rm /= "" then head rm:"" else "")

    rest <- reformat
    if rm == [] then return "" else return (concat repls ++ rest)

addNewLine :: State Format ()
addNewLine = do
    r <- getRandom
    let add = if 0.5 >= r then "\n" else ""
    f@Format { remaining = rm } <- get
    put (f { remaining = add ++ rm })
    return ()

-- | Randomly format Java code
java :: State Format String
java = do
    addNewLine
    formatted <- reformat
    return formatted
