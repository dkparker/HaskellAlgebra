module XOR where

import Algebra
import Data.Bits

instance Group Integer where
    gbinop = xor
    gid = 0
    ginv w = w
