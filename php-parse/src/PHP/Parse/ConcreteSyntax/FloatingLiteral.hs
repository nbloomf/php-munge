module PHP.Parse.ConcreteSyntax.FloatingLiteral where

import qualified Data.Text.Lazy as T
import           Data.List (tails)
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Gen (Gen, oneof, listOf, listOf1, elements)

import Data.Tuple.Extra

import PHP.Parse.Render
import PHP.Parse.Loc
import PHP.Parse.ConcreteSyntax.Symbol



newtype FloatingLiteral = FloatingLiteral
  { unFloatingLiteral ::
      ( FloatingDigitSequence
      , Maybe ExponentPart
      )
  } deriving (Eq, Show)

instance Render FloatingLiteral where
  render = render . unFloatingLiteral

instance HasExtent FloatingLiteral where
  extentOf = extentOf . unFloatingLiteral

  shiftTo loc (FloatingLiteral x) =
    shiftToWrap FloatingLiteral loc x

instance Arbitrary FloatingLiteral where
  arbitrary = do
    f@(FloatingDigitSequence (_,ds)) <- arbitrary
    curry FloatingLiteral f <$> if elem '.' ds
      then arbitrary
      else Just <$> arbitrary





newtype FloatingDigitSequence = FloatingDigitSequence
  { unFloatingDigitSequence :: (Loc, String)
  } deriving (Eq, Show)

instance Render FloatingDigitSequence where
  render (FloatingDigitSequence (_,x)) = T.pack x

instance HasExtent FloatingDigitSequence where
  extentOf (FloatingDigitSequence (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (FloatingDigitSequence (_,x)) =
    (bumpChars x loc, FloatingDigitSequence (loc,x))

instance Arbitrary FloatingDigitSequence where
  arbitrary = do
    let
      digits = (:)
            <$> (elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ])
            <*> (listOf $ elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ])

    as <- digits
    bs <- digits
    elements
      [ curry FloatingDigitSequence origin as
      , curry FloatingDigitSequence origin (as ++ ".")
      , curry FloatingDigitSequence origin ("." ++ bs)
      , curry FloatingDigitSequence origin (as ++ "." ++ bs)
      ]

  shrink (FloatingDigitSequence x) =
    map FloatingDigitSequence $
      filter (\(_,s) -> (s /= "") && (s /= ".") && (elem '.' s)) $
      shrink x





data Sign
  = Sign_Positive Plus_
  | Sign_Negative Minus_
  deriving (Eq, Show)

instance Render Sign where
  render x = case x of
    Sign_Positive z -> render z
    Sign_Negative z -> render z

instance HasExtent Sign where
  extentOf x = case x of
    Sign_Positive z -> extentOf z
    Sign_Negative z -> extentOf z

  shiftTo loc x = case x of
    Sign_Positive z ->
      shiftToWrap Sign_Positive loc z
    Sign_Negative z ->
      shiftToWrap Sign_Negative loc z

instance Arbitrary Sign where
  arbitrary = oneof
    [ Sign_Positive <$> arbitrary
    , Sign_Negative <$> arbitrary
    ]

  shrink x = case x of
    Sign_Positive z ->
      map Sign_Positive $ shrink z
    Sign_Negative z ->
      map Sign_Negative $ shrink z





data ExponentPart
  = ExponentPart_Large
      ( LargeE_
      , Maybe Sign
      , DigitSequence
      )
  | ExponentPart_Small
      ( SmallE_
      , Maybe Sign
      , DigitSequence
      )
  deriving (Eq, Show)

instance Render ExponentPart where
  render x = case x of
    ExponentPart_Large z -> render z
    ExponentPart_Small z -> render z

instance HasExtent ExponentPart where
  extentOf x = case x of
    ExponentPart_Large z -> extentOf z
    ExponentPart_Small z -> extentOf z

  shiftTo loc x = case x of
    ExponentPart_Large z ->
      shiftToWrap ExponentPart_Large loc z
    ExponentPart_Small z ->
      shiftToWrap ExponentPart_Small loc z

instance Arbitrary ExponentPart where
  arbitrary = oneof
    [ ExponentPart_Large <$> arbitrary
    , ExponentPart_Small <$> arbitrary
    ]

  shrink x = case x of
    ExponentPart_Large z ->
      map ExponentPart_Large $ shrink z
    ExponentPart_Small z ->
      map ExponentPart_Small $ shrink z





newtype DigitSequence = DigitSequence
  { unDigitSequence :: (Loc, String)
  } deriving (Eq, Show)

instance Render DigitSequence where
  render (DigitSequence (_,x)) = T.pack x

instance HasExtent DigitSequence where
  extentOf (DigitSequence (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (DigitSequence (_,x)) =
    (bumpChars x loc, DigitSequence (loc, x))

instance Arbitrary DigitSequence where
  arbitrary = curry DigitSequence origin
    <$> ((:)
          <$> (elements
                [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ])
          <*> (listOf $ elements
                [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]))

  shrink (DigitSequence (loc,x)) =
    map (curry DigitSequence loc) $
      filter (\s -> s /= "") $
      tails x
