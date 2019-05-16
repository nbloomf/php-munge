module PHP.Parse.ConcreteSyntax.WhiteSpaces where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Gen (Gen, elements, scale, getSize, suchThat, oneof)
import           Debug.Trace

import PHP.Parse.Render
import PHP.Parse.Loc
import PHP.Parse.ConcreteSyntax.Seq
import PHP.Parse.ConcreteSyntax.Keyword
import PHP.Parse.ConcreteSyntax.Symbol





newtype WhiteSpaces = WhiteSpaces
  { unWhiteSpaces :: Seq WhiteSpace
  } deriving (Eq, Show)

instance Render WhiteSpaces where
  render = render . unWhiteSpaces

instance HasExtent WhiteSpaces where
  extentOf = extentOf . unWhiteSpaces

  shiftTo loc (WhiteSpaces x) =
    shiftToWrap WhiteSpaces loc x

instance Arbitrary WhiteSpaces where
  -- Need to avoid generating \r followed by \n,
  -- as well as a single line comment followed by
  -- something other than a newline, to avoid
  -- syntax which does not parse back to itself.
  arbitrary = WhiteSpaces
    <$> arbitrarySeqWith (frac 3 4) seqPredicate_WhiteSpaces

  shrink (WhiteSpaces x) =
    (WhiteSpaces (Seq_Head (WhiteSpace_Space origin))) :
      (map WhiteSpaces $
        filter (seqSatisfies seqPredicate_WhiteSpaces) $
        shrink x)

seqPredicate_WhiteSpaces :: SeqPredicate WhiteSpace
seqPredicate_WhiteSpaces =
  let
    isCR :: WhiteSpace -> Bool
    isCR x = case x of
      WhiteSpace_NewLine (NewLine_CR _) -> True
      _ -> False

    isLF :: WhiteSpace -> Bool
    isLF x = case x of
      WhiteSpace_NewLine (NewLine_LF _) -> True
      _ -> False

    isSLC :: WhiteSpace -> Bool
    isSLC x = case x of
      WhiteSpace_Comment (Comment_SingleLine _) -> True
      _ -> False

    isNL :: WhiteSpace -> Bool
    isNL x = case x of
      WhiteSpace_NewLine _ -> True
      _ -> False
  in
    SeqPredicate
      { seqInitial  = []
      , seqAdjacent = [(isCR, not . isLF), (isSLC, isNL)]
      , seqBailout  = []
      , seqTerminal = [not . isSLC]
      }





data WhiteSpace
  = WhiteSpace_NewLine NewLine
  | WhiteSpace_Space Loc
  | WhiteSpace_HorizontalTab Loc
  | WhiteSpace_Comment Comment
  deriving (Eq, Show)

instance Render WhiteSpace where
  render x = case x of
    WhiteSpace_NewLine z       -> render z
    WhiteSpace_Space _         -> T.singleton ' '
    WhiteSpace_HorizontalTab _ -> T.singleton '\t'
    WhiteSpace_Comment z       -> render z

instance HasExtent WhiteSpace where
  extentOf x = case x of
    WhiteSpace_NewLine z       -> extentOf z
    WhiteSpace_Space p         -> IsLocated p
    WhiteSpace_HorizontalTab p -> IsLocated p
    WhiteSpace_Comment z       -> extentOf z

  shiftTo loc x = case x of
    WhiteSpace_NewLine z ->
      shiftToWrap WhiteSpace_NewLine loc z
    WhiteSpace_Space _ ->
      ( bumpCol loc, WhiteSpace_Space loc )
    WhiteSpace_HorizontalTab z ->
      ( bumpTab loc, WhiteSpace_HorizontalTab loc )
    WhiteSpace_Comment z ->
      shiftToWrap WhiteSpace_Comment loc z

instance Arbitrary WhiteSpace where
  arbitrary = do
    k <- getSize
    if k <= 0
      then elements
        [ WhiteSpace_Space origin
        , WhiteSpace_HorizontalTab origin
        ]
      else scale (`div` 2) $ oneof
        [ WhiteSpace_NewLine <$> arbitrary
        , return $ WhiteSpace_Space origin
        , return $ WhiteSpace_HorizontalTab origin
        , WhiteSpace_Comment <$> arbitrary
        ]

  shrink x = case x of
    WhiteSpace_Comment s ->
      WhiteSpace_Space origin :
        (map WhiteSpace_Comment $ shrink s)
    WhiteSpace_HorizontalTab l ->
      [ WhiteSpace_Space l ]
    _ -> []





data NewLine
  = NewLine_CR   Loc
  | NewLine_LF   Loc
  | NewLine_CRLF Loc
  deriving (Eq, Show)

instance Render NewLine where
  render x = case x of
    NewLine_CR _   -> T.singleton '\r'
    NewLine_LF _   -> T.singleton '\n'
    NewLine_CRLF _ -> T.pack "\r\n"

instance HasExtent NewLine where
  extentOf x = case x of
    NewLine_CR   z -> IsLocated z
    NewLine_LF   z -> IsLocated z
    NewLine_CRLF z -> IsLocated z

  shiftTo loc x = case x of
    NewLine_CR _ ->
      ( bumpCol loc, NewLine_CR loc )
    NewLine_LF _ ->
      ( bumpLine loc, NewLine_LF loc )
    NewLine_CRLF _ ->
      ( bumpLine $ bumpCol loc, NewLine_CRLF loc )

instance Arbitrary NewLine where
  arbitrary = elements
    [ NewLine_CR origin
    , NewLine_LF origin
    , NewLine_CRLF origin
    ]

  shrink x = case x of
    NewLine_CRLF z ->
      [ NewLine_LF z
      , NewLine_CR z
      ]
    _ -> []





data Comment
  = Comment_SingleLine SingleLineComment
  | Comment_MultiLine  MultiLineComment
  deriving (Eq, Show)

instance Render Comment where
  render x = case x of
    Comment_SingleLine z -> render z
    Comment_MultiLine z  -> render z

instance HasExtent Comment where
  extentOf x = case x of
    Comment_SingleLine z -> extentOf z
    Comment_MultiLine  z -> extentOf z

  shiftTo loc x = case x of
    Comment_SingleLine y -> shiftToWrap Comment_SingleLine loc y
    Comment_MultiLine y  -> shiftToWrap Comment_MultiLine loc y

instance Arbitrary Comment where
  arbitrary = scale (`div` 2) $ oneof
    [ Comment_SingleLine <$> arbitrary
    , Comment_MultiLine  <$> arbitrary
    ]

  shrink x = case x of
    Comment_SingleLine z ->
      map Comment_SingleLine $ shrink z
    Comment_MultiLine z ->
      map Comment_MultiLine $ shrink z





data SingleLineComment
  = SingleLineComment_Backslash
      ( Loc
      , Maybe SingleLineCommentChars
      )
  | SingleLineComment_Octothorpe
      ( Loc
      , Maybe SingleLineCommentChars
      )
  deriving (Eq, Show)

instance Render SingleLineComment where
  render x = case x of
    SingleLineComment_Backslash (_,z) -> T.concat
      [ T.pack "//", render z ]
    SingleLineComment_Octothorpe (_,z) -> T.concat
      [ T.singleton '#', render z ]

instance HasExtent SingleLineComment where
  extentOf x = case x of
    SingleLineComment_Backslash (loc, z) ->
      (IsLocated loc) <> (extentOf z)
    SingleLineComment_Octothorpe (loc, z) ->
      (IsLocated loc) <> (extentOf z)

  shiftTo loc x = case x of
    SingleLineComment_Backslash (_, cs) ->
      let (loc', z) = shiftTo (bumpCol $ bumpCol loc) cs
      in (loc', SingleLineComment_Backslash (loc, z))
    SingleLineComment_Octothorpe (_, cs) ->
      let (loc', z) = shiftTo (bumpCol loc) cs
      in (loc', SingleLineComment_Octothorpe (loc, z))

instance Arbitrary SingleLineComment where
  arbitrary = trace "SingleLineComment" $ scale (frac 1 2) $ oneof
    [ SingleLineComment_Backslash  <$> arbitrary
    , SingleLineComment_Octothorpe <$> arbitrary
    ]

  shrink x = case x of
    SingleLineComment_Backslash z ->
      map SingleLineComment_Backslash $ shrink z
    SingleLineComment_Octothorpe z ->
      map SingleLineComment_Octothorpe $ shrink z





newtype SingleLineCommentChars = SingleLineCommentChars
  { unSingleLineCommentChars :: Seq SingleLineCommentChar
  } deriving (Eq, Show)

instance Render SingleLineCommentChars where
  render = render . unSingleLineCommentChars

instance HasExtent SingleLineCommentChars where
  extentOf = extentOf . unSingleLineCommentChars

  shiftTo loc (SingleLineCommentChars x) =
    shiftToWrap SingleLineCommentChars loc x

instance Arbitrary SingleLineCommentChars where
  arbitrary = SingleLineCommentChars
    <$> arbitrarySeqWith (frac 3 4) seqPredicate_SingleLineCommentChars

  shrink (SingleLineCommentChars x) =
    map SingleLineCommentChars $
      filter (seqSatisfies seqPredicate_SingleLineCommentChars) $
      shrink x

seqPredicate_SingleLineCommentChars
  :: SeqPredicate SingleLineCommentChar
seqPredicate_SingleLineCommentChars =
  let
    isQuestion :: SingleLineCommentChar -> Bool
    isQuestion (SingleLineCommentChar (_,c)) = c == '?'

    isRightAngle :: SingleLineCommentChar -> Bool
    isRightAngle (SingleLineCommentChar (_,c)) = c == '>'
  in
    SeqPredicate
      { seqInitial  = []
      , seqAdjacent = [(isQuestion, not . isRightAngle)]
      , seqBailout  = []
      , seqTerminal = []
      }





newtype SingleLineCommentChar = SingleLineCommentChar
  { unSingleLineCommentChar :: (Loc, Char)
  } deriving (Eq, Show)

instance Render SingleLineCommentChar where
  render (SingleLineCommentChar (_,c)) = T.singleton c

instance HasExtent SingleLineCommentChar where
  extentOf (SingleLineCommentChar (loc,_)) = IsLocated loc

  shiftTo loc (SingleLineCommentChar (_,c)) =
    case c of
      '\t' -> (bumpTab loc, SingleLineCommentChar (loc, '\t'))
      _    -> (bumpCol loc, SingleLineCommentChar (loc, c))

instance Arbitrary SingleLineCommentChar where
  arbitrary = do
    c <- arbitrary `suchThat` (\c -> not $ elem c ['\n', '\r'])
    return $ SingleLineCommentChar (origin, c)





newtype MultiLineComment = MultiLineComment
  { unMultiLineComment ::
      ( Loc
      , Maybe MultiLineCommentChars
      )
  } deriving (Eq, Show)

instance Render MultiLineComment where
  render (MultiLineComment (_,x)) = T.concat
    [ T.pack "/*", render x, T.pack "*/" ]

instance HasExtent MultiLineComment where
  extentOf (MultiLineComment (loc,x)) =
    (IsBetween loc (bumpCol $ bumpCol loc)) <> (extentOf x)

  shiftTo loc (MultiLineComment (_,x)) =
    let (loc', z) = shiftTo (bumpCol $ bumpCol loc) x
    in (bumpCol $ bumpCol loc', MultiLineComment (loc,z))

instance Arbitrary MultiLineComment where
  arbitrary = MultiLineComment <$> arbitrary

  shrink (MultiLineComment x) =
    map MultiLineComment $ shrink x





newtype MultiLineCommentChars = MultiLineCommentChars
  { unMultiLineCommentChars :: Seq MultiLineCommentChar
  } deriving (Eq, Show)

instance Render MultiLineCommentChars where
  render = render . unMultiLineCommentChars

instance HasExtent MultiLineCommentChars where
  extentOf = extentOf . unMultiLineCommentChars

  shiftTo loc (MultiLineCommentChars x) =
    let (loc1,z) = shiftTo loc x
    in (loc1, MultiLineCommentChars z)

instance Arbitrary MultiLineCommentChars where
  arbitrary = do
    let
      isAst :: MultiLineCommentChar -> Bool
      isAst (MultiLineCommentChar (_,c)) = c == '*'

      isSlash :: MultiLineCommentChar -> Bool
      isSlash (MultiLineCommentChar (_,c)) = c == '/'
    MultiLineCommentChars
      <$> arbitrarySeqAvoiding (isAst, isSlash)

  shrink (MultiLineCommentChars x) =
    map MultiLineCommentChars $ shrink x





newtype MultiLineCommentChar = MultiLineCommentChar
  { unMultiLineCommentChar :: (Loc, Char)
  } deriving (Eq, Show)

instance Render MultiLineCommentChar where
  render (MultiLineCommentChar (_,c)) = T.singleton c

instance HasExtent MultiLineCommentChar where
  extentOf (MultiLineCommentChar (loc,_)) = IsLocated loc

  shiftTo loc (MultiLineCommentChar (_,c)) =
    case c of
      '\t' -> (bumpTab loc,  MultiLineCommentChar (loc, '\t'))
      '\n' -> (bumpLine loc, MultiLineCommentChar (loc, '\n'))
      _    -> (bumpCol loc,  MultiLineCommentChar (loc, c))

instance Arbitrary MultiLineCommentChar where
  arbitrary = do
    c <- arbitrary
    return $ MultiLineCommentChar (origin, c)





newtype Symbol a = Symbol
  { unSymbol :: (a, Maybe WhiteSpaces)
  } deriving (Eq, Show)

instance (Render a) => Render (Symbol a) where
  render = render . unSymbol

instance (HasExtent a) => HasExtent (Symbol a) where
  extentOf (Symbol x) = extentOf x

  shiftTo loc (Symbol x) =
    shiftToWrap Symbol loc x

instance (Arbitrary a) => Arbitrary (Symbol a) where
  arbitrary = scale (frac 1 3) $
    Symbol <$> arbitrary

  shrink (Symbol x) =
    map Symbol $ shrink x

instance Functor Symbol where
  fmap f (Symbol (a, x)) = Symbol (f a, x)





newtype Keyword a = Keyword
  { unKeyword :: (a, Maybe WhiteSpaces)
  } deriving (Eq, Show)

instance (Render a) => Render (Keyword a) where
  render = render . unKeyword

instance (HasExtent a) => HasExtent (Keyword a) where
  extentOf (Keyword x) = extentOf x

  shiftTo loc (Keyword x) =
    shiftToWrap Keyword loc x

instance (Arbitrary a) => Arbitrary (Keyword a) where
  arbitrary = scale (frac 1 3) $
    Keyword <$> arbitrary

  shrink (Keyword x) =
    map Keyword $ shrink x

instance Functor Keyword where
  fmap f (Keyword (a,b)) = Keyword (f a, b)



arbitraryKeyword2
  :: ( Arbitrary k, Arbitrary a, Render a )
  => Gen (Keyword k, a)
arbitraryKeyword2 = do
  z@(Keyword (k,w), a) <- arbitrary
  return $ if unambiguousKeyword2 z
    then z
    else (Keyword (k, Just (WhiteSpaces (Seq_Head (WhiteSpace_Space origin)))), a)

arbitraryKeyword3
  :: ( Arbitrary k, Arbitrary a, Arbitrary b, Render a, Render b )
  => Gen (Keyword k, a, b)
arbitraryKeyword3 = arbitrary `suchThat` unambiguousKeyword3

unambiguousKeyword2
  :: ( Render a )
  => (Keyword k, a) -> Bool
unambiguousKeyword2 (Keyword (_, w), a) =
  case T.uncons $ render a of
    Nothing -> True
    Just (c,_) -> case w of
      Just _ -> True
      Nothing -> not $ elem c $ concat
        [ ['_'], ['a'..'z'], ['A'..'Z'], ['0'..'9'], ['\x80'..'\xff'] ]

unambiguousKeyword3
  :: ( Render a, Render b )
  => (Keyword k, a, b) -> Bool
unambiguousKeyword3 (k, a, b) =
  unambiguousKeyword2 (k, (a,b))



unambiguousKeywordPost2
  :: ( Render a )
  => (a, Keyword k) -> Bool
unambiguousKeywordPost2 (a, _) =
  case T.unsnoc $ render a of
    Nothing -> True
    Just (_,c) -> not $ elem c $ concat
      [ ['_'], ['a'..'z'], ['A'..'Z'], ['0'..'9'], ['\x80'..'\xff'] ]



arbitraryKeywordOp
  :: ( Arbitrary op, Arbitrary a, Arbitrary b, Render a, Render b )
  => Gen (a, Keyword op, b)
arbitraryKeywordOp = scale (frac 1 2) $
  arbitrary `suchThat` unambiguousKeywordOp

unambiguousKeywordOp
  :: ( Render a, Render b )
  => (a, Keyword op, b) -> Bool
unambiguousKeywordOp (a, op, b) =
  (unambiguousKeyword2 (op, b)) && (unambiguousKeywordPost2 (a, op))
