module PHP.Parse.ConcreteSyntax.IntegerLiteral where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Gen (Gen, oneof, listOf, listOf1, elements)

import PHP.Parse.Render
import PHP.Parse.Loc





data IntegerLiteral
  = IntegerLiteral_Decimal DecimalLiteral
  | IntegerLiteral_Hex HexadecimalLiteral
  | IntegerLiteral_Binary BinaryLiteral
  | IntegerLiteral_Octal OctalLiteral
  deriving (Eq, Show)

instance Render IntegerLiteral where
  render x = case x of
    IntegerLiteral_Decimal z -> render z
    IntegerLiteral_Hex z     -> render z
    IntegerLiteral_Binary z  -> render z
    IntegerLiteral_Octal z   -> render z

instance HasExtent IntegerLiteral where
  extentOf x = case x of
    IntegerLiteral_Decimal z -> extentOf z
    IntegerLiteral_Hex z     -> extentOf z
    IntegerLiteral_Binary z  -> extentOf z
    IntegerLiteral_Octal z   -> extentOf z

  shiftTo loc x = case x of
    IntegerLiteral_Decimal z ->
      shiftToWrap IntegerLiteral_Decimal loc z
    IntegerLiteral_Hex z ->
      shiftToWrap IntegerLiteral_Hex loc z
    IntegerLiteral_Binary z ->
      shiftToWrap IntegerLiteral_Binary loc z
    IntegerLiteral_Octal z ->
      shiftToWrap IntegerLiteral_Octal loc z

instance Arbitrary IntegerLiteral where
  arbitrary = oneof
    [ IntegerLiteral_Decimal <$> arbitrary
    , IntegerLiteral_Hex <$> arbitrary
    , IntegerLiteral_Binary <$> arbitrary
    , IntegerLiteral_Octal <$> arbitrary
    ]

  shrink x = case x of
    IntegerLiteral_Decimal z ->
      map IntegerLiteral_Decimal $ shrink z
    IntegerLiteral_Hex z ->
      map IntegerLiteral_Hex $ shrink z
    IntegerLiteral_Binary z ->
      map IntegerLiteral_Binary $ shrink z
    IntegerLiteral_Octal z ->
      map IntegerLiteral_Octal $ shrink z





data DecimalLiteral = DecimalLiteral
  { unDecimalLiteral ::
      ( Loc
      , String
      )
  } deriving (Eq, Show)

instance Render DecimalLiteral where
  render (DecimalLiteral (_,x)) = T.pack x

instance HasExtent DecimalLiteral where
  extentOf (DecimalLiteral (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (DecimalLiteral (_,x)) =
    (bumpChars x loc, DecimalLiteral (loc,x))

instance Arbitrary DecimalLiteral where
  arbitrary = curry DecimalLiteral origin
    <$> ((:)
          <$> (elements
                [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ])
          <*> (listOf $ elements
                [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]))

  shrink (DecimalLiteral (l,x)) =
    map (curry DecimalLiteral l) $
      filter (/= "") $
      shrink x





newtype HexadecimalLiteral = HexadecimalLiteral
  { unHexadecimalLiteral ::
      ( HexadecimalPrefix
      , HexadecimalDigits
      )
  } deriving (Eq, Show)

instance Render HexadecimalLiteral where
  render = render . unHexadecimalLiteral

instance HasExtent HexadecimalLiteral where
  extentOf = extentOf . unHexadecimalLiteral

  shiftTo loc (HexadecimalLiteral x) =
    shiftToWrap HexadecimalLiteral loc x

instance Arbitrary HexadecimalLiteral where
  arbitrary = HexadecimalLiteral <$> arbitrary

  shrink (HexadecimalLiteral x) =
    map HexadecimalLiteral $ shrink x





data HexadecimalPrefix
  = HexadecimalPrefix_Large Loc
  | HexadecimalPrefix_Small Loc
  deriving (Eq, Show)

instance Render HexadecimalPrefix where
  render x = case x of
    HexadecimalPrefix_Large _ -> T.pack "0X"
    HexadecimalPrefix_Small _ -> T.pack "0x"

instance HasExtent HexadecimalPrefix where
  extentOf x = case x of
    HexadecimalPrefix_Large loc -> IsLocated loc
    HexadecimalPrefix_Small loc -> IsLocated loc

  shiftTo loc x = case x of
    HexadecimalPrefix_Large _ ->
      ( bumpCol $ bumpCol loc, HexadecimalPrefix_Large loc )
    HexadecimalPrefix_Small _ ->
      ( bumpCol $ bumpCol loc, HexadecimalPrefix_Small loc )

instance Arbitrary HexadecimalPrefix where
  arbitrary = elements
    [ HexadecimalPrefix_Large origin
    , HexadecimalPrefix_Small origin
    ]





data HexadecimalDigits = HexadecimalDigits
  { unHexadecimalDigits ::
      ( Loc
      , String
      )
  } deriving (Eq, Show)

instance Render HexadecimalDigits where
  render (HexadecimalDigits (_,x)) = T.pack x

instance HasExtent HexadecimalDigits where
  extentOf (HexadecimalDigits (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (HexadecimalDigits (_,x)) =
    (bumpChars x loc, HexadecimalDigits (loc,x))

instance Arbitrary HexadecimalDigits where
  arbitrary = curry HexadecimalDigits origin
    <$> (listOf1 $ elements
          [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
          , 'a', 'b', 'c', 'd', 'e', 'f'
          , 'A', 'B', 'C', 'D', 'E', 'F'
          ])

  shrink (HexadecimalDigits (l,x)) =
    map (curry HexadecimalDigits l) $
      filter (/= "") $
      shrink x





newtype BinaryLiteral = BinaryLiteral
  { unBinaryLiteral ::
      ( BinaryPrefix
      , BinaryDigits
      )
  } deriving (Eq, Show)

instance Render BinaryLiteral where
  render = render . unBinaryLiteral

instance HasExtent BinaryLiteral where
  extentOf = extentOf . unBinaryLiteral

  shiftTo loc (BinaryLiteral x) =
    shiftToWrap BinaryLiteral loc x

instance Arbitrary BinaryLiteral where
  arbitrary = BinaryLiteral <$> arbitrary

  shrink (BinaryLiteral x) =
    map BinaryLiteral $ shrink x





data BinaryPrefix
  = BinaryPrefix_Large Loc
  | BinaryPrefix_Small Loc
  deriving (Eq, Show)

instance Render BinaryPrefix where
  render x = case x of
    BinaryPrefix_Large _ -> T.pack "0B"
    BinaryPrefix_Small _ -> T.pack "0b"

instance HasExtent BinaryPrefix where
  extentOf x = case x of
    BinaryPrefix_Large loc -> IsBetween loc (bumpCol loc)
    BinaryPrefix_Small loc -> IsBetween loc (bumpCol loc)

  shiftTo loc x = case x of
    BinaryPrefix_Large _ ->
      ( bumpCol $ bumpCol loc, BinaryPrefix_Large loc )
    BinaryPrefix_Small _ ->
      ( bumpCol $ bumpCol loc, BinaryPrefix_Small loc )

instance Arbitrary BinaryPrefix where
  arbitrary = elements
    [ BinaryPrefix_Large origin
    , BinaryPrefix_Small origin
    ]





data BinaryDigits = BinaryDigits
  { unBinaryDigits ::
      ( Loc
      , String
      )
  } deriving (Eq, Show)

instance Render BinaryDigits where
  render (BinaryDigits (_,x)) = T.pack x

instance HasExtent BinaryDigits where
  extentOf (BinaryDigits (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (BinaryDigits (_,x)) =
    (bumpChars x loc, BinaryDigits (loc,x))

instance Arbitrary BinaryDigits where
  arbitrary = curry BinaryDigits origin
    <$> (listOf1 $ elements
          [ '0', '1' ])

  shrink (BinaryDigits (l,x)) =
    map (curry BinaryDigits l) $
      filter (/= "") $
      shrink x





newtype OctalLiteral = OctalLiteral
  { unOctalLiteral ::
      ( OctalPrefix
      , OctalDigits
      )
  } deriving (Eq, Show)

instance Render OctalLiteral where
  render = render . unOctalLiteral

instance HasExtent OctalLiteral where
  extentOf = extentOf . unOctalLiteral

  shiftTo loc (OctalLiteral x) =
    shiftToWrap OctalLiteral loc x

instance Arbitrary OctalLiteral where
  arbitrary = OctalLiteral <$> arbitrary

  shrink (OctalLiteral x) =
    map OctalLiteral $ shrink x





newtype OctalPrefix = OctalPrefix
  { unOctalPrefix :: Loc
  } deriving (Eq, Show)

instance Render OctalPrefix where
  render _ = T.singleton '0'

instance HasExtent OctalPrefix where
  extentOf (OctalPrefix loc) = IsLocated loc

  shiftTo loc (OctalPrefix _) =
    ( bumpCol loc, OctalPrefix loc )

instance Arbitrary OctalPrefix where
  arbitrary = pure $ OctalPrefix origin





data OctalDigits = OctalDigits
  { unOctalDigits ::
      ( Loc
      , String
      )
  } deriving (Eq, Show)

instance Render OctalDigits where
  render (OctalDigits (_,x)) = T.pack x

instance HasExtent OctalDigits where
  extentOf (OctalDigits (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (OctalDigits (_,x)) =
    (bumpChars x loc, OctalDigits (loc,x))

instance Arbitrary OctalDigits where
  arbitrary = curry OctalDigits origin
    <$> (listOf1 $ elements
          [ '0', '1', '2', '3', '4', '5', '6', '7' ])

  shrink (OctalDigits (l,x)) =
    map (curry OctalDigits l) $
      filter (/= "") $
      shrink x
