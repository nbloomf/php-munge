module PHP.Parse.Loc where

import Test.QuickCheck
import Data.Char (ord, chr)

-- map to the range a-z
shrinkLower :: Char -> Char
shrinkLower c = chr $ ((ord c) `rem` 26) + 97


-- | Denotes a character position in a file
data Loc = Loc
  { _line :: Int
  , _col  :: Int
  , _char :: Int
  } deriving (Eq, Show)



data Extent
  = IsNowhere
  | IsLocated Loc
  | IsBetween Loc Loc
  deriving (Eq, Show)

class HasExtent t where
  extentOf :: t -> Extent

  shiftTo :: Loc -> t -> (Loc, t)

instance Arbitrary Extent where
  arbitrary = return IsNowhere

instance Semigroup Extent where
  x <> y = case x of
    IsNowhere -> y
    IsLocated lx -> case y of
      IsNowhere -> x
      IsLocated ly -> if lx == ly
        then IsLocated lx
        else IsBetween lx ly
      IsBetween _ ly -> IsBetween lx ly
    IsBetween lx _ -> case y of
      IsNowhere -> x
      IsLocated ly -> IsBetween lx ly
      IsBetween _ ly -> IsBetween lx ly

instance Monoid Extent where
  mempty = IsNowhere

shiftToWrap
  :: ( HasExtent a )
  => (a -> w) -> Loc -> a -> (Loc, w)
shiftToWrap f loc a =
  let (loc', a') = shiftTo loc a
  in (loc', f a')

instance
  ( HasExtent t
  ) => HasExtent (Maybe t)
  where
    extentOf x = case x of
      Nothing -> IsNowhere
      Just z  -> extentOf z

    shiftTo loc0 x = case x of
      Nothing -> (loc0, Nothing)
      Just z ->
        let (loc1, z') = shiftTo loc0 z
        in (loc1, Just z')

instance
  ( HasExtent t1, HasExtent t2
  ) => HasExtent (t1, t2)
  where
    extentOf (x1,x2) =
      mconcat [ extentOf x1, extentOf x2 ]

    shiftTo loc0 (x1,x2) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
      in (loc2, (x1',x2'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3
  ) => HasExtent (t1, t2, t3)
  where
    extentOf (x1,x2,x3) =
      mconcat [ extentOf x1, extentOf x2, extentOf x3 ]

    shiftTo loc0 (x1,x2,x3) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
      in (loc3, (x1',x2',x3'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3, HasExtent t4
  ) => HasExtent (t1, t2, t3, t4)
  where
    extentOf (x1,x2,x3,x4) =
      mconcat [ extentOf x1, extentOf x2, extentOf x3, extentOf x4 ]

    shiftTo loc0 (x1,x2,x3,x4) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
        (loc4, x4') = shiftTo loc3 x4
      in (loc4, (x1',x2',x3',x4'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3, HasExtent t4
  , HasExtent t5
  ) => HasExtent (t1, t2, t3, t4, t5)
  where
    extentOf (x1,x2,x3,x4,x5) = mconcat
      [ extentOf x1, extentOf x2, extentOf x3, extentOf x4
      , extentOf x5 ]

    shiftTo loc0 (x1,x2,x3,x4,x5) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
        (loc4, x4') = shiftTo loc3 x4
        (loc5, x5') = shiftTo loc4 x5
      in (loc5, (x1',x2',x3',x4',x5'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3, HasExtent t4
  , HasExtent t5, HasExtent t6
  ) => HasExtent (t1, t2, t3, t4, t5, t6)
  where
    extentOf (x1,x2,x3,x4,x5,x6) = mconcat
      [ extentOf x1, extentOf x2, extentOf x3, extentOf x4
      , extentOf x5, extentOf x6 ]

    shiftTo loc0 (x1,x2,x3,x4,x5,x6) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
        (loc4, x4') = shiftTo loc3 x4
        (loc5, x5') = shiftTo loc4 x5
        (loc6, x6') = shiftTo loc5 x6
      in (loc6, (x1',x2',x3',x4',x5',x6'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3, HasExtent t4
  , HasExtent t5, HasExtent t6, HasExtent t7
  ) => HasExtent (t1, t2, t3, t4, t5, t6, t7)
  where
    extentOf (x1,x2,x3,x4,x5,x6,x7) = mconcat
      [ extentOf x1, extentOf x2, extentOf x3, extentOf x4
      , extentOf x5, extentOf x6, extentOf x7 ]

    shiftTo loc0 (x1,x2,x3,x4,x5,x6,x7) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
        (loc4, x4') = shiftTo loc3 x4
        (loc5, x5') = shiftTo loc4 x5
        (loc6, x6') = shiftTo loc5 x6
        (loc7, x7') = shiftTo loc6 x7
      in (loc7, (x1',x2',x3',x4',x5',x6',x7'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3, HasExtent t4
  , HasExtent t5, HasExtent t6, HasExtent t7, HasExtent t8
  ) => HasExtent (t1, t2, t3, t4, t5, t6, t7, t8)
  where
    extentOf (x1,x2,x3,x4,x5,x6,x7,x8) = mconcat
      [ extentOf x1, extentOf x2, extentOf x3, extentOf x4
      , extentOf x5, extentOf x6, extentOf x7, extentOf x8 ]

    shiftTo loc0 (x1,x2,x3,x4,x5,x6,x7,x8) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
        (loc4, x4') = shiftTo loc3 x4
        (loc5, x5') = shiftTo loc4 x5
        (loc6, x6') = shiftTo loc5 x6
        (loc7, x7') = shiftTo loc6 x7
        (loc8, x8') = shiftTo loc7 x8
      in (loc8, (x1',x2',x3',x4',x5',x6',x7',x8'))

instance
  ( HasExtent t1, HasExtent t2, HasExtent t3, HasExtent t4
  , HasExtent t5, HasExtent t6, HasExtent t7, HasExtent t8
  , HasExtent t9
  ) => HasExtent (t1, t2, t3, t4, t5, t6, t7, t8, t9)
  where
    extentOf (x1,x2,x3,x4,x5,x6,x7,x8,x9) = mconcat
      [ extentOf x1, extentOf x2, extentOf x3, extentOf x4
      , extentOf x5, extentOf x6, extentOf x7, extentOf x8
      , extentOf x9 ]

    shiftTo loc0 (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
      let
        (loc1, x1') = shiftTo loc0 x1
        (loc2, x2') = shiftTo loc1 x2
        (loc3, x3') = shiftTo loc2 x3
        (loc4, x4') = shiftTo loc3 x4
        (loc5, x5') = shiftTo loc4 x5
        (loc6, x6') = shiftTo loc5 x6
        (loc7, x7') = shiftTo loc6 x7
        (loc8, x8') = shiftTo loc7 x8
        (loc9, x9') = shiftTo loc8 x9
      in (loc9, (x1',x2',x3',x4',x5',x6',x7',x8',x9'))

instance
  ( HasExtent t1, HasExtent t2,  HasExtent t3, HasExtent t4
  , HasExtent t5, HasExtent t6,  HasExtent t7, HasExtent t8
  , HasExtent t9, HasExtent t10
  ) => HasExtent (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
  where
    extentOf (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) = mconcat
      [ extentOf x1, extentOf x2,  extentOf x3, extentOf x4
      , extentOf x5, extentOf x6,  extentOf x7, extentOf x8
      , extentOf x9, extentOf x10 ]

    shiftTo loc0 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) =
      let
        (loc1,  x1')  = shiftTo loc0 x1
        (loc2,  x2')  = shiftTo loc1 x2
        (loc3,  x3')  = shiftTo loc2 x3
        (loc4,  x4')  = shiftTo loc3 x4
        (loc5,  x5')  = shiftTo loc4 x5
        (loc6,  x6')  = shiftTo loc5 x6
        (loc7,  x7')  = shiftTo loc6 x7
        (loc8,  x8')  = shiftTo loc7 x8
        (loc9,  x9')  = shiftTo loc8 x9
        (loc10, x10') = shiftTo loc9 x10
      in (loc10, (x1',x2',x3',x4',x5',x6',x7',x8',x9',x10'))

  

instance Arbitrary Loc where
  arbitrary = return origin

origin :: Loc
origin = Loc
  { _line = 1
  , _col  = 1
  , _char = 0
  }

bumpLine :: Loc -> Loc
bumpLine loc = loc
  { _line = 1 + _line loc
  , _col  = 1
  , _char = 1 + _char loc
  }

bumpCol :: Loc -> Loc
bumpCol loc = loc
  { _col  = 1 + _col loc
  , _char = 1 + _char loc
  }

bumpTab :: Loc -> Loc
bumpTab loc = loc
  { _col  = (((_col loc + 7) `div` 8) * 8) + 1
  , _char = 1 + _char loc
  }

bumpChar :: Char -> Loc -> Loc
bumpChar c loc = case c of
  '\t' -> bumpTab loc
  '\n' -> bumpLine loc
  _    -> bumpCol loc

bumpChars :: [Char] -> Loc -> Loc
bumpChars xs loc = case xs of
  [] -> loc
  c:cs -> bumpChars cs $ bumpChar c loc
