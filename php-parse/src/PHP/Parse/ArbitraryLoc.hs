module PHP.Parse.ArbitraryLoc where

import Test.QuickCheck



class ArbitraryLoc t where
  arbitraryLoc :: Loc -> Gen (t, Loc)
