module Algebra (Group,Ring,Field,gbinop,gid,ginv,rplus,rplusid,rplusinv,rtimes,rtimesid,fplus,fplusid,fplusinv,ftimes,ftimesid,ftimesinv) where

class Group g where
    gbinop :: g -> g -> g
    gid :: g
    ginv :: g -> g

class Ring r where
    rplus :: r -> r -> r
    rplusid:: r
    rplusinv :: r -> r
    rtimes :: r -> r -> r
    rtimesid :: r

class Field f where
    fplus :: f -> f -> f
    fplusid :: f
    fplusinv :: f -> f
    ftimes :: f -> f -> f
    ftimesid :: f
    ftimesinv :: f -> f

