{-# LANGUAGE ScopedTypeVariables #-}

module PHP.Parse.ConcreteSyntax.Seq where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Gen (Gen, suchThat, scale, getSize, oneof)

import PHP.Parse.Render
import PHP.Parse.Loc



-- | Many production rules represent sequences of
-- elements which need to satisfy some additional
-- structural constraints. The @Seq@ type is designed
-- to handle these in a uniform way.
data Seq a
  = Seq_Head a
  | Seq_Snoc (Seq a, a)
  deriving (Eq, Show)

instance (Render a) => Render (Seq a) where
  render x = case x of
    Seq_Head z -> render z
    Seq_Snoc z -> render z

instance (HasExtent a) => HasExtent (Seq a) where
  extentOf x = case x of
    Seq_Head z -> extentOf z
    Seq_Snoc z -> extentOf z

  shiftTo loc x = case x of
    Seq_Head a -> shiftToWrap Seq_Head loc a
    Seq_Snoc z -> shiftToWrap Seq_Snoc loc z

instance (Arbitrary a) => Arbitrary (Seq a) where
  arbitrary = scale (`div` 2) $ do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then Seq_Head <$> arbitrary
      else oneof
        [ Seq_Head <$> arbitrary
        , Seq_Snoc <$> arbitrary
        ]

  shrink x = case x of
    Seq_Head z ->
      map Seq_Head $ shrink z
    Seq_Snoc z@(w,v) ->
      (Seq_Head v) : w :
        (map Seq_Snoc $ shrink z)





arbitrarySeqAvoiding
  :: forall a. ( Arbitrary a )
  => (a -> Bool, a -> Bool) -> Gen (Seq a)
arbitrarySeqAvoiding (p,q) = do
  let
    next :: Seq a -> Gen (Maybe a)
    next x = do
      k <- getSize
      case mod k 4 of
        0 -> return Nothing
        _ -> scale (`div` 2) $ case x of
          Seq_Head a ->
            if p a
              then Just <$> arbitrary `suchThat` (not . q)
              else Just <$> arbitrary
          Seq_Snoc (_,a) -> do
            if p a
              then Just <$> arbitrary `suchThat` (not . q)
              else Just <$> arbitrary
  a <- arbitrary
  extendM (curry Seq_Snoc) (Seq_Head a) next





-- | Specialized predicates on @Seq@. Often data defined
-- in terms of @Seq@ is only valid if it satisfies some
-- extra constraints, which we need to take into account
-- when defining @Arbitrary@ instances.
data SeqPredicate a = SeqPredicate
  { seqInitial  :: [a -> Bool]
  , seqAdjacent :: [(a -> Bool, a -> Bool)]
  , seqBailout  :: [a -> Bool]
  , seqTerminal :: [a -> Bool]
  }





seqSatisfies :: forall a. SeqPredicate a -> Seq a -> Bool
seqSatisfies p x = start x && middle x && bail x && end
  where
    start :: Seq a -> Bool
    start x = case x of
      Seq_Head a -> and $ map ($ a) $ seqInitial p
      Seq_Snoc (as,_) -> start as

    middle :: Seq a -> Bool
    middle x = case x of
      Seq_Head _ -> True
      Seq_Snoc (Seq_Head a2, a1) ->
        let qs = map snd $ filter (\c -> fst c a2) $ seqAdjacent p
        in and $ map ($ a1) qs
      Seq_Snoc (Seq_Snoc (y, a2), a1) ->
        let qs = map snd $ filter (\c -> fst c a2) $ seqAdjacent p
        in (and $ map ($ a1) qs) && (middle (Seq_Snoc (y,a2)))

    bail :: Seq a -> Bool
    bail x = case x of
      Seq_Head _ -> True
      Seq_Snoc (Seq_Head a2, a1) ->
        not $ or $ map ($ a2) $ seqBailout p
      Seq_Snoc (Seq_Snoc (y, a2), a1) ->
        (not $ or $ map ($ a2) $ seqBailout p)
          && (bail (Seq_Snoc (y,a2)))

    end :: Bool
    end = case x of
      Seq_Head a -> and $ map ($ a) $ seqTerminal p
      Seq_Snoc (_,a) -> and $ map ($ a) $ seqTerminal p





-- | Generate @Seq@ values which satisfy the given predicate.
arbitrarySeqWith
  :: forall a. ( Arbitrary a )
  => (Int -> Int)
  -> SeqPredicate a
  -> Gen (Seq a)
arbitrarySeqWith factor p = do
  let
    some :: [a -> Bool] -> a -> Bool
    some ps a = or $ map ($ a) ps

    each :: [a -> Bool] -> a -> Bool
    each ps a = and $ map ($ a) ps

    next :: Seq a -> Gen (Maybe a)
    next x = do
      k <- arbitrary :: Gen Int
      case x of
        Seq_Head a -> scale factor $
          if ( (0 == k `mod` 3) && (each (seqTerminal p) a) )
              || ( some (seqBailout p) a )
            then return Nothing
            else do
              let qs = map snd $ filter (\c -> fst c a) $ seqAdjacent p
              Just <$> arbitrary `suchThat` (each qs)
        Seq_Snoc (_,a) -> scale factor $ scale (frac 1 2) $
          if ( (0 == k `mod` 3) && (each (seqTerminal p) a) )
              || ( some (seqBailout p) a )
            then return Nothing
            else do
              let qs = map snd $ filter (\c -> fst c a) $ seqAdjacent p
              Just <$> arbitrary `suchThat` (each qs)
  a <- arbitrary `suchThat` (each (seqInitial p))
  extendM (curry Seq_Snoc) (Seq_Head a) next





extendM
  :: (b -> a -> b) -> b -> (b -> Gen (Maybe a)) -> Gen b
extendM cons init next = do
  k <- getSize
  if (k <= 0)
    then return init
    else scale (`div` 2) $ do 
      x <- next init
      case x of
        Nothing   -> return init
        Just succ -> extendM cons (cons init succ) next







-- | Many production rules represent sequences of
-- elements which need to satisfy some additional
-- structural constraints. The @Seq@ type is designed
-- to handle these in a uniform way.
data SeqSep sep a
  = SeqSep_Head a
  | SeqSep_Snoc (SeqSep sep a, sep, a)
  deriving (Eq, Show)

instance (Render sep, Render a) => Render (SeqSep sep a) where
  render x = case x of
    SeqSep_Head z -> render z
    SeqSep_Snoc z -> render z

instance (HasExtent sep, HasExtent a) => HasExtent (SeqSep sep a) where
  extentOf x = case x of
    SeqSep_Head z -> extentOf z
    SeqSep_Snoc z -> extentOf z

  shiftTo loc x = case x of
    SeqSep_Head a -> shiftToWrap SeqSep_Head loc a
    SeqSep_Snoc z -> shiftToWrap SeqSep_Snoc loc z

instance (Arbitrary sep, Arbitrary a) => Arbitrary (SeqSep sep a) where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 5) $ oneof
        [ SeqSep_Head <$> arbitrary
        ]
      else scale (frac 1 6) $ oneof
        [ SeqSep_Head <$> arbitrary
        , SeqSep_Snoc <$> arbitrary
        ]

  shrink x = case x of
    SeqSep_Head z ->
      map SeqSep_Head $ shrink z
    SeqSep_Snoc z@(w,_,v) ->
      (SeqSep_Head v) : w :
        (map SeqSep_Snoc $ shrink z)

mapSep :: (sep1 -> sep2) -> SeqSep sep1 a -> SeqSep sep2 a
mapSep f x = case x of
  SeqSep_Head a       -> SeqSep_Head a
  SeqSep_Snoc (y,s,a) -> SeqSep_Snoc (mapSep f y, f s, a)



frac :: Int -> Int -> Int -> Int
frac a b n = (a * n) `div` b
