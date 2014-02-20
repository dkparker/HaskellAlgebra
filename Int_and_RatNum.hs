import Algebra

instance Group Integer where
    gbinop = (+)
    gid = 0
    ginv i = (-i)

instance Ring Integer where
    rplus = (+)
    rplusid = 0
    rplusinv i = (-i)
    rtimes = (*)
    rtimesid = 1

data RatNum = RatNum Int Int
    deriving(Show)

instance Eq RatNum where
    RatNum a b == RatNum c d = a*d == b*c

instance Group RatNum where
    (RatNum a b) `gbinop` (RatNum c d) = RatNum (a*d + b*c) (b*d)
    gid = RatNum 0 1
    ginv (RatNum a b) = RatNum (-a) b

instance Ring RatNum where
    rplus (RatNum a b) (RatNum c d) = RatNum (a*d + b*c) (b*d)
    rplusid = RatNum 0 1
    rplusinv (RatNum a b) = RatNum (-a) b
    rtimes (RatNum a b) (RatNum c d) = RatNum (a*c) (b*d)
    rtimesid = RatNum 1 1

instance Field RatNum where
    fplus (RatNum a b) (RatNum c d) = RatNum (a*d + b*c) (b*d)
    fplusid = RatNum 0 1
    fplusinv (RatNum a b) = RatNum (-a) b
    ftimes (RatNum a b) (RatNum c d) = RatNum (a*c) (b*d)
    ftimesid = RatNum 1 1
    ftimesinv (RatNum a b) = RatNum b a
