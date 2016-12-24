module Format
    ( formatString
    ) where

import           Control.Monad.State

data Format = Format


formatString :: String -> State Format String
formatString =
    return
