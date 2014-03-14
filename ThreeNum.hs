module ThreeNum where

import Algebra

data ThreeNum = Zero | One | Two
    deriving Show

instance Field ThreeNum where
    fplus Zero x = x
    fplus x Zero = x
    fplus One Two = Zero
    fplus Two One = Zero
    fplus One One = Two
    fplus Two Two = One
    fplusid = Zero
    fplusinv Zero = Zero
    fplusinv One = Two
    fplusinv Two = One
    ftimes Zero x = Zero
    ftimes x Zero = Zero
    ftimes One Two = Two
    ftimes Two One = Two
    ftimes Two Two = One
    ftimesid = One
    ftimesinv One = One
    ftimesinv Two = Two
