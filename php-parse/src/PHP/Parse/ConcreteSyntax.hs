module PHP.Parse.ConcreteSyntax where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck
                   (Arbitrary(..))
import           Test.QuickCheck.Gen
                   (Gen, elements, suchThat, oneof, scale, getSize, listOf, frequency)
import           Data.Char (ord)
import           Data.List (isPrefixOf, isSuffixOf)

import Data.Tuple.Extra

import PHP.Parse.Render
import PHP.Parse.Loc
import PHP.Parse.ConcreteSyntax.Seq
import PHP.Parse.ConcreteSyntax.Keyword
import PHP.Parse.ConcreteSyntax.Symbol
import PHP.Parse.ConcreteSyntax.WhiteSpaces
import PHP.Parse.ConcreteSyntax.IntegerLiteral
import PHP.Parse.ConcreteSyntax.FloatingLiteral
import PHP.Parse.ConcreteSyntax.Name





-- ====== --
-- Script --
-- ====== --

newtype Script = Script
  { unScript ::
      ( Maybe Text
      , Seq ScriptSection
      )
  } deriving (Eq, Show)

instance Render Script where
  render = render . unScript

instance HasExtent Script where
  extentOf = extentOf . unScript

  shiftTo loc (Script x) =
    shiftToWrap Script loc x

instance Arbitrary Script where
  arbitrary = curry Script
    <$> arbitrary
    <*> arbitrarySeqWith (frac 3 4) seqPredicate_Script

  shrink (Script xs) =
    map Script $
      filter (seqSatisfies seqPredicate_Script . snd) $
      shrink xs

seqPredicate_Script :: SeqPredicate ScriptSection
seqPredicate_Script =
  let
    isNotClosed :: ScriptSection -> Bool
    isNotClosed (ScriptSection (_,_,x,_)) = case x of
      Nothing -> True
      Just _  -> False
  in
    SeqPredicate
      { seqInitial  = []
      , seqAdjacent = []
      , seqBailout  = [isNotClosed]
      , seqTerminal = []
      }





newtype ScriptSection = ScriptSection
 { unScriptSection ::
    ( StartTag
    , Maybe StatementList
    , Maybe EndTag
    , Maybe Text
    )
  } deriving (Eq, Show)

instance Render ScriptSection where
  render = render . unScriptSection

instance HasExtent ScriptSection where
  extentOf (ScriptSection x) = extentOf x

  shiftTo loc (ScriptSection x) =
    shiftToWrap ScriptSection loc x

instance Arbitrary ScriptSection where
  arbitrary = scale (frac 1 2) $ oneof
    [ curry4 ScriptSection
        <$> arbitrary
        <*> arbitrary
        <*> pure Nothing
        <*> pure Nothing
    , curry4 ScriptSection
        <$> arbitrary
        <*> arbitrary
        <*> (Just <$> arbitrary)
        <*> arbitrary
    ]

  shrink x = case x of
    ScriptSection (a,b,c,d) ->
      case c of
        Nothing ->
          map ScriptSection $ shrink (a,b,Nothing,Nothing)
        Just c' ->
          map ScriptSection
            [ (a',b',Just c'',d') | a' <- shrink a, b' <- shrink b, c'' <- shrink c', d' <- shrink d ]





data StartTag
  = StartTag_Long  (Loc, WhiteSpaces)
  | StartTag_Short (Loc, WhiteSpaces)
  deriving (Eq, Show)

instance Render StartTag where
  render x = case x of
    StartTag_Long (_,z) -> T.concat
      [ T.pack "<?php", render z ]
    StartTag_Short (_,z) -> T.concat
      [ T.pack "<?=", render z ]

instance HasExtent StartTag where
  extentOf x = case x of
    StartTag_Long (loc,w) ->
      (IsLocated loc) <> (extentOf w)

    StartTag_Short (loc,w) ->
      (IsLocated loc) <> (extentOf w)

  shiftTo loc x = case x of
    StartTag_Long (_,z) ->
      let
        (loc', z') = shiftTo
          (bumpCol $ bumpCol $ bumpCol $ bumpCol $ bumpCol loc) z
      in (loc', StartTag_Long (loc, z'))
    StartTag_Short (_,z) ->
      let
        (loc', z') = shiftTo
          (bumpCol $ bumpCol $ bumpCol loc) z
      in (loc', StartTag_Short (loc, z'))

instance Arbitrary StartTag where
  arbitrary = scale (frac 1 2) $ oneof
    [ StartTag_Long  <$> arbitrary
    , StartTag_Short <$> arbitrary
    ]





data EndTag
  = EndTag Loc
  deriving (Eq, Show)

instance Render EndTag where
  render (EndTag _) = T.pack "?>"

instance HasExtent EndTag where
  extentOf (EndTag loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (EndTag _) =
    (bumpCol $ bumpCol loc, EndTag loc)

instance Arbitrary EndTag where
  arbitrary = pure $ EndTag origin





newtype Text = Text
  { unText :: Seq TextChar
  } deriving (Eq, Show)

instance Render Text where
  render = render . unText

instance HasExtent Text where
  extentOf = extentOf . unText

  shiftTo loc (Text cs) =
    shiftToWrap Text loc cs

instance Arbitrary Text where
  arbitrary = Text <$> arbitrarySeqWith (frac 3 4) seqPredicate_Text

  shrink (Text cs) =
    map Text $
      filter (seqSatisfies seqPredicate_Text) $
      shrink cs

seqPredicate_Text :: SeqPredicate TextChar
seqPredicate_Text =
  let
    isLeftAngle :: TextChar -> Bool
    isLeftAngle x = case x of
      TextChar (_,'<') -> True
      _ -> False

    isQuestion :: TextChar -> Bool
    isQuestion x = case x of
      TextChar (_,'?') -> True
      _ -> False
  in
    SeqPredicate
      { seqInitial  = []
      , seqAdjacent = [(isLeftAngle, not . isQuestion)]
      , seqBailout  = []
      , seqTerminal = []
      }





newtype TextChar = TextChar
  { unTextChar :: (Loc, Char)
  } deriving (Eq, Show)

instance Render TextChar where
  render (TextChar (_,c)) = T.singleton c

instance HasExtent TextChar where
  extentOf (TextChar (loc,_)) = IsLocated loc

  shiftTo loc (TextChar (_,c)) =
    case c of
      '\t' -> (bumpTab loc,  TextChar (loc, c))
      '\n' -> (bumpLine loc, TextChar (loc, c))
      _    -> (bumpCol loc,  TextChar (loc, c))

instance Arbitrary TextChar where
  arbitrary = TextChar <$> arbitrary

  shrink (TextChar (loc,c)) =
    let k = ord c in
    if (97 <= k) && (k <= 122)
      then []
      else [TextChar (loc, shrinkLower c)]





newtype StatementList = StatementList
  { unStatementList :: Seq Statement
  } deriving (Eq, Show)

instance Render StatementList where
  render = render . unStatementList

instance HasExtent StatementList where
  extentOf = extentOf . unStatementList

  shiftTo loc (StatementList xs) =
    shiftToWrap StatementList loc xs

instance Arbitrary StatementList where
  arbitrary = StatementList <$> arbitrary

  shrink (StatementList xs) =
    map StatementList $ shrink xs







-- ========== --
-- Statements --
-- ========== --

data Statement
  = Statement_CompoundStatement   CompoundStatement
  | Statement_NamedLabelStatement NamedLabelStatement
  | Statement_JumpStatement       JumpStatement
  | Statement_Selection           SelectionStatement
  | Statement_TryStatement        TryStatement
  | Statement_Expression          ExpressionStatement
  | Statement_Iteration           IterationStatement
  | Statement_Declare             DeclareStatement
  | Statement_Echo                EchoStatement
  | Statement_Unset               UnsetStatement
  | Statement_Const               ConstDeclaration
  | Statement_Function            FunctionDefinition
  | Statement_Class               ClassDeclaration
  | Statement_Interface           InterfaceDeclaration
  | Statement_Trait               TraitDeclaration
  | Statement_Namespace           NamespaceDefinition
  | Statement_NamespaceUse        NamespaceUseDeclaration
  | Statement_Global              GlobalDeclaration
  | Statement_FunctionStatic      FunctionStaticDeclaration
  deriving (Eq, Show)

instance Render Statement where
  render x = case x of
    Statement_CompoundStatement   z -> render z
    Statement_NamedLabelStatement z -> render z
    Statement_JumpStatement       z -> render z
    Statement_Selection           z -> render z
    Statement_TryStatement        z -> render z
    Statement_Expression          z -> render z
    Statement_Iteration           z -> render z
    Statement_Declare             z -> render z
    Statement_Echo                z -> render z
    Statement_Unset               z -> render z
    Statement_Const               z -> render z
    Statement_Function            z -> render z
    Statement_Class               z -> render z
    Statement_Interface           z -> render z
    Statement_Trait               z -> render z
    Statement_Namespace           z -> render z
    Statement_NamespaceUse        z -> render z
    Statement_Global              z -> render z
    Statement_FunctionStatic      z -> render z

instance HasExtent Statement where
  extentOf x = case x of
    Statement_CompoundStatement   z -> extentOf z
    Statement_NamedLabelStatement z -> extentOf z
    Statement_JumpStatement       z -> extentOf z
    Statement_Selection           z -> extentOf z
    Statement_TryStatement        z -> extentOf z
    Statement_Expression          z -> extentOf z
    Statement_Iteration           z -> extentOf z
    Statement_Declare             z -> extentOf z
    Statement_Echo                z -> extentOf z
    Statement_Unset               z -> extentOf z
    Statement_Const               z -> extentOf z
    Statement_Function            z -> extentOf z
    Statement_Class               z -> extentOf z
    Statement_Interface           z -> extentOf z
    Statement_Trait               z -> extentOf z
    Statement_Namespace           z -> extentOf z
    Statement_NamespaceUse        z -> extentOf z
    Statement_Global              z -> extentOf z
    Statement_FunctionStatic      z -> extentOf z

  shiftTo loc x = case x of
    Statement_CompoundStatement z ->
      shiftToWrap Statement_CompoundStatement loc z
    Statement_NamedLabelStatement z ->
      shiftToWrap Statement_NamedLabelStatement loc z
    Statement_JumpStatement z ->
      shiftToWrap Statement_JumpStatement loc z
    Statement_Selection z ->
      shiftToWrap Statement_Selection loc z
    Statement_TryStatement z ->
      shiftToWrap Statement_TryStatement loc z
    Statement_Expression z ->
      shiftToWrap Statement_Expression loc z
    Statement_Iteration z ->
      shiftToWrap Statement_Iteration loc z
    Statement_Declare z ->
      shiftToWrap Statement_Declare loc z
    Statement_Echo z ->
      shiftToWrap Statement_Echo loc z
    Statement_Unset z ->
      shiftToWrap Statement_Unset loc z
    Statement_Const z ->
      shiftToWrap Statement_Const loc z
    Statement_Function z ->
      shiftToWrap Statement_Function loc z
    Statement_Class z ->
      shiftToWrap Statement_Class loc z
    Statement_Interface z ->
      shiftToWrap Statement_Interface loc z
    Statement_Trait z ->
      shiftToWrap Statement_Trait loc z
    Statement_Namespace z ->
      shiftToWrap Statement_Namespace loc z
    Statement_NamespaceUse z ->
      shiftToWrap Statement_NamespaceUse loc z
    Statement_Global z ->
      shiftToWrap Statement_Global loc z
    Statement_FunctionStatic z ->
      shiftToWrap Statement_FunctionStatic loc z

instance Arbitrary Statement where
  arbitrary = do
    k <- getSize
    if (k <= 0)
      then scale (frac 3 4) $ oneof
        [ Statement_NamedLabelStatement <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ Statement_CompoundStatement   <$> arbitrary
        , Statement_NamedLabelStatement <$> arbitrary
        , Statement_JumpStatement       <$> arbitrary
        , Statement_Selection           <$> arbitrary
        , Statement_TryStatement        <$> arbitrary
        , Statement_Expression          <$> arbitrary
        , Statement_Iteration           <$> arbitrary
        , Statement_Declare             <$> arbitrary
        , Statement_Echo                <$> arbitrary
        , Statement_Unset               <$> arbitrary
        , Statement_Const               <$> arbitrary
        , Statement_Function            <$> arbitrary
        , Statement_Class               <$> arbitrary
        , Statement_Interface           <$> arbitrary
        , Statement_Trait               <$> arbitrary
        , Statement_Namespace           <$> arbitrary
        , Statement_NamespaceUse        <$> arbitrary
        , Statement_Global              <$> arbitrary
        , Statement_FunctionStatic      <$> arbitrary
        ]

  shrink x = case x of
    Statement_CompoundStatement z ->
      map Statement_CompoundStatement $ shrink z
    Statement_NamedLabelStatement z ->
      map Statement_NamedLabelStatement $ shrink z
    Statement_JumpStatement z ->
      map Statement_JumpStatement $ shrink z
    Statement_Selection z ->
      map Statement_Selection $ shrink z
    Statement_TryStatement z ->
      map Statement_TryStatement $ shrink z
    Statement_Expression z ->
      map Statement_Expression $ shrink z
    Statement_Iteration z ->
      map Statement_Iteration $ shrink z
    Statement_Declare z ->
      map Statement_Declare $ shrink z
    Statement_Echo z ->
      map Statement_Echo $ shrink z
    Statement_Unset z ->
      map Statement_Unset $ shrink z
    Statement_Const z ->
      map Statement_Const $ shrink z
    Statement_Function z ->
      map Statement_Function $ shrink z
    Statement_Class z ->
      map Statement_Class $ shrink z
    Statement_Interface z ->
      map Statement_Interface $ shrink z
    Statement_Trait z ->
      map Statement_Trait $ shrink z
    Statement_Namespace z ->
      map Statement_Namespace $ shrink z
    Statement_NamespaceUse z ->
      map Statement_NamespaceUse $ shrink z
    Statement_Global z ->
      map Statement_Global $ shrink z
    Statement_FunctionStatic z ->
      map Statement_FunctionStatic $ shrink z





newtype CompoundStatement = CompoundStatement
  { unCompoundStatement ::
      ( Symbol OpenBrace_
      , Maybe StatementList
      , Symbol ClosedBrace_
      )
  } deriving (Eq, Show)

instance Render CompoundStatement where
  render (CompoundStatement x) = render x

instance HasExtent CompoundStatement where
  extentOf = extentOf . unCompoundStatement

  shiftTo loc (CompoundStatement x) =
    shiftToWrap CompoundStatement loc x

instance Arbitrary CompoundStatement where
  arbitrary = scale (frac 1 2) $
    CompoundStatement <$> arbitrary

  shrink (CompoundStatement x) =
    map CompoundStatement $ shrink x





newtype NamedLabelStatement = NamedLabelStatement
  { unNamedLabelStatement ::
      ( Name
      , Symbol Colon_
      )
  } deriving (Eq, Show)

instance Render NamedLabelStatement where
  render = render . unNamedLabelStatement

instance HasExtent NamedLabelStatement where
  extentOf = extentOf . unNamedLabelStatement

  shiftTo loc (NamedLabelStatement x) =
    shiftToWrap NamedLabelStatement loc x

instance Arbitrary NamedLabelStatement where
  arbitrary = scale (frac 1 2) $
    NamedLabelStatement <$> arbitrary

  shrink (NamedLabelStatement x) =
    map NamedLabelStatement $ shrink x







-- =========== --
-- Expressions --
-- =========== --

data Expression
  = Expression_IncOr       LogicalIncOrExpression2
  | Expression_Include     IncludeExpression
  | Expression_IncludeOnce IncludeOnceExpression
  | Expression_Require     RequireExpression
  | Expression_RequireOnce RequireOnceExpression
  deriving (Eq, Show)

instance Render Expression where
  render x = case x of
    Expression_IncOr       z -> render z
    Expression_Include     z -> render z
    Expression_IncludeOnce z -> render z
    Expression_Require     z -> render z
    Expression_RequireOnce z -> render z

instance HasExtent Expression where
  extentOf x = case x of
    Expression_IncOr       z -> extentOf z
    Expression_Include     z -> extentOf z
    Expression_IncludeOnce z -> extentOf z
    Expression_Require     z -> extentOf z
    Expression_RequireOnce z -> extentOf z

  shiftTo loc x = case x of
    Expression_IncOr       z -> shiftToWrap Expression_IncOr loc z
    Expression_Include     z -> shiftToWrap Expression_Include loc z
    Expression_IncludeOnce z -> shiftToWrap Expression_IncludeOnce loc z
    Expression_Require     z -> shiftToWrap Expression_Require loc z
    Expression_RequireOnce z -> shiftToWrap Expression_RequireOnce loc z

instance Arbitrary Expression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 2) $ oneof
        [ Expression_IncOr       <$> arbitrary
        ]
      else scale (frac 1 3) $ frequency
        [ ( 16
          , Expression_IncOr       <$> arbitrary
          )
        , ( 1
          , Expression_Include     <$> arbitrary
          )
        , ( 1
          , Expression_IncludeOnce <$> arbitrary
          )
        , ( 1
          , Expression_Require     <$> arbitrary
          )
        , ( 1
          , Expression_RequireOnce <$> arbitrary
          )
        ]

  {- shrink x = case x of
    Expression_IncOr z ->
      map Expression_IncOr $ shrink z
    Expression_Include z ->
      map Expression_Include $ shrink z
    Expression_IncludeOnce z ->
      map Expression_IncludeOnce $ shrink z
    Expression_Require z ->
      map Expression_Require $ shrink z
    Expression_RequireOnce z ->
      map Expression_RequireOnce $ shrink z -}





newtype IncludeExpression = IncludeExpression
  { unIncludeExpression ::
      ( Keyword Include_
      , Expression
      )
  } deriving (Eq, Show)

instance Render IncludeExpression where
  render = render . unIncludeExpression

instance HasExtent IncludeExpression where
  extentOf = extentOf . unIncludeExpression

  shiftTo loc (IncludeExpression x) =
    shiftToWrap IncludeExpression loc x

instance Arbitrary IncludeExpression where
  arbitrary = scale (frac 1 2) $
    IncludeExpression <$> arbitraryKeyword2

  {- shrink (IncludeExpression x) =
    map IncludeExpression $
      filter unambiguousKeyword2 $
      shrink x -}





newtype IncludeOnceExpression = IncludeOnceExpression
  { unIncludeOnceExpression ::
      ( Keyword IncludeOnce_
      , Expression
      )
  } deriving (Eq, Show)

instance Render IncludeOnceExpression where
  render = render . unIncludeOnceExpression

instance HasExtent IncludeOnceExpression where
  extentOf = extentOf . unIncludeOnceExpression

  shiftTo loc (IncludeOnceExpression x) =
    shiftToWrap IncludeOnceExpression loc x

instance Arbitrary IncludeOnceExpression where
  arbitrary = scale (frac 1 3) $
    IncludeOnceExpression <$> arbitraryKeyword2

  {- shrink (IncludeOnceExpression x) =
    map IncludeOnceExpression $
      filter unambiguousKeyword2 $
      shrink x -}





newtype RequireExpression = RequireExpression
  { unRequireExpression ::
      ( Keyword Require_
      , Expression
      )
  } deriving (Eq, Show)

instance Render RequireExpression where
  render = render . unRequireExpression

instance HasExtent RequireExpression where
  extentOf = extentOf . unRequireExpression

  shiftTo loc (RequireExpression x) =
    shiftToWrap RequireExpression loc x

instance Arbitrary RequireExpression where
  arbitrary = scale (frac 1 3) $
    RequireExpression <$> arbitraryKeyword2

  {- shrink (RequireExpression x) =
    map RequireExpression $
      filter unambiguousKeyword2 $
      shrink x -}





newtype RequireOnceExpression = RequireOnceExpression
  { unRequireOnceExpression ::
      ( Keyword RequireOnce_
      , Expression
      )
  } deriving (Eq, Show)

instance Render RequireOnceExpression where
  render = render . unRequireOnceExpression

instance HasExtent RequireOnceExpression where
  extentOf = extentOf . unRequireOnceExpression

  shiftTo loc (RequireOnceExpression x) =
    shiftToWrap RequireOnceExpression loc x

instance Arbitrary RequireOnceExpression where
  arbitrary = scale (frac 1 2) $
    RequireOnceExpression <$> arbitraryKeyword2

  {- shrink (RequireOnceExpression x) =
    map RequireOnceExpression $
      filter unambiguousKeyword2 $
      shrink x -}





data LogicalIncOrExpression2
  = LogicalIncOrExpression2_Xor
        LogicalExcOrExpression
  | LogicalIncOrExpression2_Or
      ( LogicalIncOrExpression2
      , Keyword Or_
      , LogicalExcOrExpression
      )
  deriving (Eq, Show)

instance Render LogicalIncOrExpression2 where
  render x = case x of
    LogicalIncOrExpression2_Xor z -> render z
    LogicalIncOrExpression2_Or  z -> render z

instance HasExtent LogicalIncOrExpression2 where
  extentOf x = case x of
    LogicalIncOrExpression2_Xor z -> extentOf z
    LogicalIncOrExpression2_Or  z -> extentOf z

  shiftTo loc x = case x of
    LogicalIncOrExpression2_Xor z ->
      shiftToWrap LogicalIncOrExpression2_Xor loc z
    LogicalIncOrExpression2_Or z ->
      shiftToWrap LogicalIncOrExpression2_Or loc z

instance Arbitrary LogicalIncOrExpression2 where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ LogicalIncOrExpression2_Xor <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ LogicalIncOrExpression2_Xor <$> arbitrary
        , LogicalIncOrExpression2_Or  <$> arbitraryKeywordOp
        ]

  shrink x = case x of
    LogicalIncOrExpression2_Xor z ->
      map LogicalIncOrExpression2_Xor $ shrink z
    LogicalIncOrExpression2_Or z@(u,_,w) ->
      (LogicalIncOrExpression2_Xor w) : u :
        (map LogicalIncOrExpression2_Or $ shrink z)





data LogicalExcOrExpression
  = LogicalExcOrExpression_And
        LogicalAndExpression2
  | LogicalExcOrExpression_Xor
      ( LogicalExcOrExpression
      , Keyword Xor_
      , LogicalAndExpression2
      )
  deriving (Eq, Show)

instance Render LogicalExcOrExpression where
  render x = case x of
    LogicalExcOrExpression_And z -> render z
    LogicalExcOrExpression_Xor z -> render z

instance HasExtent LogicalExcOrExpression where
  extentOf x = case x of
    LogicalExcOrExpression_And z -> extentOf z
    LogicalExcOrExpression_Xor z -> extentOf z

  shiftTo loc x = case x of
    LogicalExcOrExpression_And z ->
      shiftToWrap LogicalExcOrExpression_And loc z
    LogicalExcOrExpression_Xor z ->
      shiftToWrap LogicalExcOrExpression_Xor loc z

instance Arbitrary LogicalExcOrExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ LogicalExcOrExpression_And <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ LogicalExcOrExpression_And <$> arbitrary
        , LogicalExcOrExpression_Xor <$> arbitraryKeywordOp
        ]

  shrink x = case x of
    LogicalExcOrExpression_And z ->
      map LogicalExcOrExpression_And $ shrink z
    LogicalExcOrExpression_Xor z@(u,_,w) ->
      (LogicalExcOrExpression_And w) : u :
        (map LogicalExcOrExpression_Xor $ shrink z)





data LogicalAndExpression2
  = LogicalAndExpression2_Print
        PrintExpression
  | LogicalAndExpression2_And
      ( LogicalAndExpression2
      , Keyword And_
      , YieldExpression
      )
  deriving (Eq, Show)

instance Render LogicalAndExpression2 where
  render x = case x of
    LogicalAndExpression2_Print z -> render z
    LogicalAndExpression2_And   z -> render z

instance HasExtent LogicalAndExpression2 where
  extentOf x = case x of
    LogicalAndExpression2_Print z -> extentOf z
    LogicalAndExpression2_And   z -> extentOf z

  shiftTo loc x = case x of
    LogicalAndExpression2_Print z ->
      shiftToWrap LogicalAndExpression2_Print loc z
    LogicalAndExpression2_And z ->
      shiftToWrap LogicalAndExpression2_And loc z

instance Arbitrary LogicalAndExpression2 where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ LogicalAndExpression2_Print <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ LogicalAndExpression2_Print <$> arbitrary
        , LogicalAndExpression2_And   <$> arbitraryKeywordOp
        ]

  shrink x = case x of
    LogicalAndExpression2_Print z ->
      map LogicalAndExpression2_Print $ shrink z
    LogicalAndExpression2_And z@(w,_,_) ->
      w : (map LogicalAndExpression2_And $ shrink z)





data PrintExpression
  = PrintExpression_Print
      ( Keyword Print_
      , YieldExpression
      )
  | PrintExpression_Yield
        YieldExpression
  deriving (Eq, Show)

instance Render PrintExpression where
  render x = case x of
    PrintExpression_Print z -> render z
    PrintExpression_Yield z -> render z

instance HasExtent PrintExpression where
  extentOf x = case x of
    PrintExpression_Print z -> extentOf z
    PrintExpression_Yield z -> extentOf z

  shiftTo loc x = case x of
    PrintExpression_Print z ->
      shiftToWrap PrintExpression_Print loc z
    PrintExpression_Yield z ->
      shiftToWrap PrintExpression_Yield loc z

instance Arbitrary PrintExpression where
  arbitrary = scale (frac 8 9) $ oneof
    [ PrintExpression_Print <$> arbitraryKeyword2
    , PrintExpression_Yield <$> arbitrary
    ]

  shrink x = case x of
    PrintExpression_Print z@(_,w) ->
      (PrintExpression_Yield w) :
        (map PrintExpression_Print $ shrink z)
    PrintExpression_Yield z ->
      map PrintExpression_Yield $ shrink z
































data SingleQuotedStringChar
  = SingleQuotedStringChar_Escape
        SingleQuotedStringEscape
  | SingleQuotedStringChar_Char
      ( Loc, Char )
  deriving (Eq, Show)

instance Render SingleQuotedStringChar where
  render x = case x of
    SingleQuotedStringChar_Escape z -> render z
    SingleQuotedStringChar_Char (_,c) -> T.singleton c

instance HasExtent SingleQuotedStringChar where
  extentOf x = case x of
    SingleQuotedStringChar_Escape z    -> extentOf z
    SingleQuotedStringChar_Char (lc,_) -> IsLocated lc

  shiftTo loc x = case x of
    SingleQuotedStringChar_Escape z ->
      shiftToWrap SingleQuotedStringChar_Escape loc z
    SingleQuotedStringChar_Char (_,c) ->
      ( bumpChar c loc
      , SingleQuotedStringChar_Char (loc, c)
      )

instance Arbitrary SingleQuotedStringChar where
  arbitrary = oneof
    [ SingleQuotedStringChar_Escape <$> arbitrary
    , curry SingleQuotedStringChar_Char origin
        <$> arbitrary `suchThat` (\c -> not $ elem c ['\'', '\\'])
    ]





data SingleQuotedStringEscape
  = SingleQuotedStringEscape_SingleQuote Loc
  | SingleQuotedStringEscape_Backslash   Loc
  deriving (Eq, Show)

instance Render SingleQuotedStringEscape where
  render x = case x of
    SingleQuotedStringEscape_SingleQuote _ -> T.pack "\\\'"
    SingleQuotedStringEscape_Backslash   _ -> T.pack "\\\\"

instance HasExtent SingleQuotedStringEscape where
  extentOf x = case x of
    SingleQuotedStringEscape_SingleQuote l -> IsLocated l
    SingleQuotedStringEscape_Backslash l -> IsLocated l

  shiftTo loc x = case x of
    SingleQuotedStringEscape_SingleQuote _ ->
      ( bumpCol $ bumpCol loc
      , SingleQuotedStringEscape_SingleQuote loc
      )
    SingleQuotedStringEscape_Backslash _ ->
      ( bumpCol $ bumpCol loc
      , SingleQuotedStringEscape_Backslash loc
      )

instance Arbitrary SingleQuotedStringEscape where
  arbitrary = elements
    [ SingleQuotedStringEscape_SingleQuote origin
    , SingleQuotedStringEscape_Backslash   origin
    ]





newtype SingleQuotedStringChars = SingleQuotedStringChars
  { unSingleQuotedStringChars :: Seq SingleQuotedStringChar
  } deriving (Eq, Show)

instance Render SingleQuotedStringChars where
  render = render . unSingleQuotedStringChars

instance HasExtent SingleQuotedStringChars where
  extentOf = extentOf . unSingleQuotedStringChars

  shiftTo loc (SingleQuotedStringChars x) =
    shiftToWrap SingleQuotedStringChars loc x

instance Arbitrary SingleQuotedStringChars where
  arbitrary = SingleQuotedStringChars
    <$> arbitrarySeqWith (frac 3 4) seqPredicate_SingleQuotedStringChars

  shrink (SingleQuotedStringChars x) =
    map SingleQuotedStringChars $
      filter (seqSatisfies seqPredicate_SingleQuotedStringChars) $
      shrink x

seqPredicate_SingleQuotedStringChars
  :: SeqPredicate SingleQuotedStringChar
seqPredicate_SingleQuotedStringChars =
  let
    isBackslash :: SingleQuotedStringChar -> Bool
    isBackslash x = case x of
      (SingleQuotedStringChar_Char (_,'\\')) -> True
      _ -> False

    isSingleQuote :: SingleQuotedStringChar -> Bool
    isSingleQuote x = case x of
      (SingleQuotedStringChar_Char (_,'\'')) -> True
      _ -> False

    isNormal :: SingleQuotedStringChar -> Bool
    isNormal x = case x of
      (SingleQuotedStringChar_Char (_,c)) ->
        not $ elem c ['\'', '\\']
      (SingleQuotedStringChar_Escape _) -> False

  in
    SeqPredicate
      { seqInitial  = []
      , seqAdjacent = [(isBackslash, isNormal)]
      , seqBailout  = [isSingleQuote]
      , seqTerminal = [not . isBackslash]
      }




newtype SingleQuotedString = SingleQuotedString
  { unSingleQuotedString ::
      ( Maybe BPrefix
      , SingleQuote_
      , Maybe SingleQuotedStringChars
      , SingleQuote_
      )
  } deriving (Eq, Show)

instance Render SingleQuotedString where
  render (SingleQuotedString x) = render x

instance HasExtent SingleQuotedString where
  extentOf = extentOf . unSingleQuotedString

  shiftTo loc (SingleQuotedString x) =
    shiftToWrap SingleQuotedString loc x

instance Arbitrary SingleQuotedString where
  arbitrary = scale (frac 1 2) $
    SingleQuotedString <$> arbitrary

  shrink (SingleQuotedString x) =
    map SingleQuotedString $ shrink x





data BPrefix
  = BPrefix_Large Loc
  | BPrefix_Small Loc
  deriving (Eq, Show)

instance Render BPrefix where
  render x = case x of
    BPrefix_Large _ -> T.singleton 'B'
    BPrefix_Small _ -> T.singleton 'b'

instance HasExtent BPrefix where
  extentOf x = case x of
    BPrefix_Large l -> IsLocated l
    BPrefix_Small l -> IsLocated l

  shiftTo loc x = case x of
    BPrefix_Large _ ->
      ( bumpCol loc, BPrefix_Large loc )
    BPrefix_Small _ ->
      ( bumpCol loc, BPrefix_Small loc )

instance Arbitrary BPrefix where
  arbitrary = elements
    [ BPrefix_Large origin
    , BPrefix_Small origin
    ]








































newtype GotoStatement = GotoStatement
  { unGotoStatement ::
      ( Keyword Goto_
      , Name
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render GotoStatement where
  render = render . unGotoStatement

instance HasExtent GotoStatement where
  extentOf = extentOf . unGotoStatement

  shiftTo loc (GotoStatement x) =
    shiftToWrap GotoStatement loc x

instance Arbitrary GotoStatement where
  arbitrary = scale (frac 1 2) $
    GotoStatement <$> arbitraryKeyword3

  {- shrink (GotoStatement x) =
    map GotoStatement $
      filter unambiguousKeyword3 $
      shrink x -}























data YieldExpression
  = YieldExpression_Assignment
        AssignmentExpression
  | YieldExpression_From
        YieldFromExpression
  | YieldExpression_Plain
        (Keyword Yield_)
  | YieldExpression_Yield
      ( Keyword Yield_
      , AssignmentExpression
      )
  | YieldExpression_Arrow
      ( Keyword Yield_
      , YieldExpression
      , Symbol EqualGreater_
      , YieldExpression
      )
  deriving (Eq, Show)

instance Render YieldExpression where
  render x = case x of
    YieldExpression_Assignment z -> render z
    YieldExpression_From       z -> render z
    YieldExpression_Plain      z -> render z
    YieldExpression_Yield      z -> render z
    YieldExpression_Arrow      z -> render z

instance HasExtent YieldExpression where
  extentOf x = case x of
    YieldExpression_Assignment z -> extentOf z
    YieldExpression_From       z -> extentOf z
    YieldExpression_Plain      z -> extentOf z
    YieldExpression_Yield      z -> extentOf z
    YieldExpression_Arrow      z -> extentOf z

  shiftTo loc x = case x of
    YieldExpression_Assignment z ->
      shiftToWrap YieldExpression_Assignment loc z
    YieldExpression_From z ->
      shiftToWrap YieldExpression_From loc z
    YieldExpression_Plain z ->
      shiftToWrap YieldExpression_Plain loc z
    YieldExpression_Yield z ->
      shiftToWrap YieldExpression_Yield loc z
    YieldExpression_Arrow z ->
      shiftToWrap YieldExpression_Arrow loc z

instance Arbitrary YieldExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ YieldExpression_Plain      <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ YieldExpression_Assignment <$> arbitrary
        , YieldExpression_From       <$> arbitrary
        , YieldExpression_Plain      <$> arbitrary
        , YieldExpression_Yield      <$> arbitrary
        , YieldExpression_Arrow      <$> arbitrary
        ]

  shrink x = case x of
    YieldExpression_Assignment z ->
      map YieldExpression_Assignment $ shrink z
    YieldExpression_From z ->
      map YieldExpression_From $ shrink z
    YieldExpression_Plain z ->
      map YieldExpression_Plain $ shrink z
    YieldExpression_Yield z ->
      map YieldExpression_Yield $ shrink z
    YieldExpression_Arrow z ->
      map YieldExpression_Arrow $ shrink z





data AssignmentExpression
  = AssignmentExpression_Conditional ConditionalExpression
  | AssignmentExpression_Simple      SimpleAssignmentExpression
  | AssignmentExpression_Compound    CompoundAssignmentExpression
  deriving (Eq, Show)

instance Render AssignmentExpression where
  render x = case x of
    AssignmentExpression_Conditional z -> render z
    AssignmentExpression_Simple      z -> render z
    AssignmentExpression_Compound    z -> render z

instance HasExtent AssignmentExpression where
  extentOf x = case x of
    AssignmentExpression_Conditional z -> extentOf z
    AssignmentExpression_Simple      z -> extentOf z
    AssignmentExpression_Compound    z -> extentOf z

  shiftTo loc x = case x of
    AssignmentExpression_Conditional z ->
      shiftToWrap AssignmentExpression_Conditional loc z
    AssignmentExpression_Simple z ->
      shiftToWrap AssignmentExpression_Simple loc z
    AssignmentExpression_Compound z ->
      shiftToWrap AssignmentExpression_Compound loc z

instance Arbitrary AssignmentExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ AssignmentExpression_Conditional <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ AssignmentExpression_Conditional <$> arbitrary
        , AssignmentExpression_Simple      <$> arbitrary
        , AssignmentExpression_Compound    <$> arbitrary
        ]

  shrink x = case x of
    AssignmentExpression_Conditional z ->
      map AssignmentExpression_Conditional $ shrink z
    AssignmentExpression_Simple z ->
      map AssignmentExpression_Simple $ shrink z
    AssignmentExpression_Compound z ->
      map AssignmentExpression_Compound $ shrink z





data ConditionalExpression
  = ConditionalExpression_Coalesce
        CoalesceExpression
  | ConditionalExpression_Conditional
      ( ConditionalExpression
      , Symbol Question_
      , Maybe Expression
      , Symbol Colon_
      , CoalesceExpression
      )
  deriving (Eq, Show)

instance Render ConditionalExpression where
  render x = case x of
    ConditionalExpression_Coalesce z -> render z
    ConditionalExpression_Conditional z -> render z

instance HasExtent ConditionalExpression where
  extentOf x = case x of
    ConditionalExpression_Coalesce    z -> extentOf z
    ConditionalExpression_Conditional z -> extentOf z

  shiftTo loc x = case x of
    ConditionalExpression_Coalesce z ->
      shiftToWrap ConditionalExpression_Coalesce loc z
    ConditionalExpression_Conditional z ->
      shiftToWrap ConditionalExpression_Conditional loc z

instance Arbitrary ConditionalExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ConditionalExpression_Coalesce    <$> arbitrary
        ]
      else scale (frac 1 5) $ oneof
        [ ConditionalExpression_Coalesce    <$> arbitrary
        , ConditionalExpression_Conditional <$> arbitrary
        ]

  shrink x = case x of
    ConditionalExpression_Coalesce z ->
      map ConditionalExpression_Coalesce $ shrink z
    ConditionalExpression_Conditional z@(u,_,_,_,v) ->
      u : (ConditionalExpression_Coalesce v) :
        (map ConditionalExpression_Conditional $ shrink z)




data CoalesceExpression
  = CoalesceExpression_Or
        LogicalIncOrExpression1
  | CoalesceExpression_Coalesce
      ( LogicalIncOrExpression1
      , Symbol DoubleQuestion_
      , CoalesceExpression
      )
  deriving (Eq, Show)

instance Render CoalesceExpression where
  render x = case x of
    CoalesceExpression_Or z -> render z
    CoalesceExpression_Coalesce z -> render z

instance HasExtent CoalesceExpression where
  extentOf x = case x of
    CoalesceExpression_Or       z -> extentOf z
    CoalesceExpression_Coalesce z -> extentOf z

  shiftTo loc x = case x of
    CoalesceExpression_Or z ->
      shiftToWrap CoalesceExpression_Or loc z
    CoalesceExpression_Coalesce z ->
      shiftToWrap CoalesceExpression_Coalesce loc z

instance Arbitrary CoalesceExpression where
  arbitrary = scale (`div` 3) $ do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ CoalesceExpression_Or       <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ CoalesceExpression_Or       <$> arbitrary
        , CoalesceExpression_Coalesce <$> arbitrary
        ]

  shrink x = case x of
    CoalesceExpression_Or z ->
      map CoalesceExpression_Or $ shrink z
    CoalesceExpression_Coalesce z@(u,_,v) ->
      v : (CoalesceExpression_Or u) :
        (map CoalesceExpression_Coalesce $ shrink z)





data LogicalIncOrExpression1
  = LogicalIncOrExpression1_And
        LogicalAndExpression1
  | LogicalIncOrExpression1_Or
      ( LogicalIncOrExpression1
      , Symbol DoublePipe_
      , LogicalAndExpression1
      )
  deriving (Eq, Show)

instance Render LogicalIncOrExpression1 where
  render x = case x of
    LogicalIncOrExpression1_And z -> render z
    LogicalIncOrExpression1_Or z -> render z

instance HasExtent LogicalIncOrExpression1 where
  extentOf x = case x of
    LogicalIncOrExpression1_And z -> extentOf z
    LogicalIncOrExpression1_Or  z -> extentOf z

  shiftTo loc x = case x of
    LogicalIncOrExpression1_And z ->
      shiftToWrap LogicalIncOrExpression1_And loc z
    LogicalIncOrExpression1_Or z ->
      shiftToWrap LogicalIncOrExpression1_Or loc z

instance Arbitrary LogicalIncOrExpression1 where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ LogicalIncOrExpression1_And <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ LogicalIncOrExpression1_And <$> arbitrary
        , LogicalIncOrExpression1_Or  <$> arbitrary
        ]

  shrink x = case x of
    LogicalIncOrExpression1_And z ->
      map LogicalIncOrExpression1_And $ shrink z
    LogicalIncOrExpression1_Or z@(u,_,v) ->
      u : (LogicalIncOrExpression1_And v) :
        (map LogicalIncOrExpression1_Or $ shrink z)





data LogicalAndExpression1
  = LogicalAndExpression1_BitwiseOr
        BitwiseIncOrExpression
  | LogicalAndExpression1_And
      ( LogicalAndExpression1
      , Symbol AmpAmp_
      , BitwiseIncOrExpression
      )
  deriving (Eq, Show)

instance Render LogicalAndExpression1 where
  render x = case x of
    LogicalAndExpression1_BitwiseOr z -> render z
    LogicalAndExpression1_And z -> render z

instance HasExtent LogicalAndExpression1 where
  extentOf x = case x of
    LogicalAndExpression1_BitwiseOr z -> extentOf z
    LogicalAndExpression1_And       z -> extentOf z

  shiftTo loc x = case x of
    LogicalAndExpression1_BitwiseOr z ->
      shiftToWrap LogicalAndExpression1_BitwiseOr loc z
    LogicalAndExpression1_And z ->
      shiftToWrap LogicalAndExpression1_And loc z

instance Arbitrary LogicalAndExpression1 where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ LogicalAndExpression1_BitwiseOr <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ LogicalAndExpression1_BitwiseOr <$> arbitrary
        , LogicalAndExpression1_And       <$> arbitrary
        ]

  shrink x = case x of
    LogicalAndExpression1_BitwiseOr z ->
      map LogicalAndExpression1_BitwiseOr $ shrink z
    LogicalAndExpression1_And z@(u,_,v) ->
      u : (LogicalAndExpression1_BitwiseOr v) :
        (map LogicalAndExpression1_And $ shrink z)





data BitwiseIncOrExpression
  = BitwiseIncOrExpression_ExcOr
        BitwiseExcOrExpression
  | BitwiseIncOrExpression_IncOr
      ( BitwiseIncOrExpression
      , Symbol Pipe_
      , BitwiseExcOrExpression
      )
  deriving (Eq, Show)

instance Render BitwiseIncOrExpression where
  render x = case x of
    BitwiseIncOrExpression_ExcOr z -> render z
    BitwiseIncOrExpression_IncOr z -> render z

instance HasExtent BitwiseIncOrExpression where
  extentOf x = case x of
    BitwiseIncOrExpression_ExcOr z -> extentOf z
    BitwiseIncOrExpression_IncOr z -> extentOf z

  shiftTo loc x = case x of
    BitwiseIncOrExpression_ExcOr z ->
      shiftToWrap BitwiseIncOrExpression_ExcOr loc z
    BitwiseIncOrExpression_IncOr z ->
      shiftToWrap BitwiseIncOrExpression_IncOr loc z

instance Arbitrary BitwiseIncOrExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ BitwiseIncOrExpression_ExcOr <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ BitwiseIncOrExpression_ExcOr <$> arbitrary
        , BitwiseIncOrExpression_IncOr <$> arbitrary
        ]

  shrink x = case x of
    BitwiseIncOrExpression_ExcOr z ->
      map BitwiseIncOrExpression_ExcOr $ shrink z
    BitwiseIncOrExpression_IncOr z@(u,_,v) ->
      u : (BitwiseIncOrExpression_ExcOr v) :
        (map BitwiseIncOrExpression_IncOr $ shrink z)





data BitwiseExcOrExpression
  = BitwiseExcOrExpression_And
        BitwiseAndExpression
  | BitwiseExcOrExpression_ExcOr
      ( BitwiseExcOrExpression
      , Symbol Caret_
      , BitwiseAndExpression
      )
  deriving (Eq, Show)

instance Render BitwiseExcOrExpression where
  render x = case x of
    BitwiseExcOrExpression_And   z -> render z
    BitwiseExcOrExpression_ExcOr z -> render z

instance HasExtent BitwiseExcOrExpression where
  extentOf x = case x of
    BitwiseExcOrExpression_And   z -> extentOf z
    BitwiseExcOrExpression_ExcOr z -> extentOf z

  shiftTo loc x = case x of
    BitwiseExcOrExpression_And z ->
      shiftToWrap BitwiseExcOrExpression_And loc z
    BitwiseExcOrExpression_ExcOr z ->
      shiftToWrap BitwiseExcOrExpression_ExcOr loc z

instance Arbitrary BitwiseExcOrExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ BitwiseExcOrExpression_And   <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ BitwiseExcOrExpression_And   <$> arbitrary
        , BitwiseExcOrExpression_ExcOr <$> arbitrary
        ]

  shrink x = case x of
    BitwiseExcOrExpression_And z ->
      map BitwiseExcOrExpression_And $ shrink z
    BitwiseExcOrExpression_ExcOr z@(u,_,v) ->
      u : (BitwiseExcOrExpression_And v) :
        (map BitwiseExcOrExpression_ExcOr $ shrink z)





data BitwiseAndExpression
  = BitwiseAndExpression_Equality
        EqualityExpression
  | BitwiseAndExpression_And
      ( BitwiseAndExpression
      , Symbol Amp_
      , EqualityExpression
      )
  deriving (Eq, Show)

instance Render BitwiseAndExpression where
  render x = case x of
    BitwiseAndExpression_Equality z -> render z
    BitwiseAndExpression_And      z -> render z

instance HasExtent BitwiseAndExpression where
  extentOf x = case x of
    BitwiseAndExpression_Equality z -> extentOf z
    BitwiseAndExpression_And      z -> extentOf z

  shiftTo loc x = case x of
    BitwiseAndExpression_Equality z ->
      shiftToWrap BitwiseAndExpression_Equality loc z
    BitwiseAndExpression_And z ->
      shiftToWrap BitwiseAndExpression_And loc z

instance Arbitrary BitwiseAndExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ BitwiseAndExpression_Equality <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ BitwiseAndExpression_Equality <$> arbitrary
        , BitwiseAndExpression_And      <$> arbitrary
        ]

  {- shrink x = case x of
    BitwiseAndExpression_Equality z ->
      map BitwiseAndExpression_Equality $ shrink z
    BitwiseAndExpression_And z@(u,_,w) ->
      (BitwiseAndExpression_Equality w) : u :
        (map BitwiseAndExpression_And $ shrink z) -}





data EqualityExpression
  = EqualityExpression_Relational
        RelationalExpression
  | EqualityExpression_DoubleEqual
      ( EqualityExpression
      , Symbol DoubleEqual_
      , RelationalExpression
      )
  | EqualityExpression_BangEqual
      ( EqualityExpression
      , Symbol BangEqual_
      , RelationalExpression
      )
  | EqualityExpression_LessGreater
      ( EqualityExpression
      , Symbol LessGreater_
      , RelationalExpression
      )
  | EqualityExpression_TripleEqual
      ( EqualityExpression
      , Symbol TripleEqual_
      , RelationalExpression
      )
  | EqualityExpression_BangEqualEqual
      ( EqualityExpression
      , Symbol BangEqualEqual_
      , RelationalExpression
      )
  deriving (Eq, Show)

instance Render EqualityExpression where
  render x = case x of
    EqualityExpression_Relational     z -> render z
    EqualityExpression_DoubleEqual    z -> render z
    EqualityExpression_BangEqual      z -> render z
    EqualityExpression_LessGreater    z -> render z
    EqualityExpression_TripleEqual    z -> render z
    EqualityExpression_BangEqualEqual z -> render z

instance HasExtent EqualityExpression where
  extentOf x = case x of
    EqualityExpression_Relational     z -> extentOf z
    EqualityExpression_DoubleEqual    z -> extentOf z
    EqualityExpression_BangEqual      z -> extentOf z
    EqualityExpression_LessGreater    z -> extentOf z
    EqualityExpression_TripleEqual    z -> extentOf z
    EqualityExpression_BangEqualEqual z -> extentOf z

  shiftTo loc x = case x of
    EqualityExpression_Relational z ->
      shiftToWrap EqualityExpression_Relational loc z
    EqualityExpression_DoubleEqual z ->
      shiftToWrap EqualityExpression_DoubleEqual loc z
    EqualityExpression_BangEqual z ->
      shiftToWrap EqualityExpression_BangEqual loc z
    EqualityExpression_LessGreater z ->
      shiftToWrap EqualityExpression_LessGreater loc z
    EqualityExpression_TripleEqual z ->
      shiftToWrap EqualityExpression_TripleEqual loc z
    EqualityExpression_BangEqualEqual z ->
      shiftToWrap EqualityExpression_BangEqualEqual loc z

instance Arbitrary EqualityExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ EqualityExpression_Relational     <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ EqualityExpression_Relational     <$> arbitrary
        , EqualityExpression_DoubleEqual    <$> arbitrary
        , EqualityExpression_BangEqual      <$> arbitrary
        , EqualityExpression_LessGreater    <$> arbitrary
        , EqualityExpression_TripleEqual    <$> arbitrary
        , EqualityExpression_BangEqualEqual <$> arbitrary
        ]

  {- shrink x = case x of
    EqualityExpression_Relational z ->
      map EqualityExpression_Relational $ shrink z
    EqualityExpression_DoubleEqual z@(u,_,v) ->
      (EqualityExpression_Relational v) : u :
        (map EqualityExpression_DoubleEqual $ shrink z)
    EqualityExpression_BangEqual z@(u,_,v) ->
      (EqualityExpression_Relational v) : u :
        (map EqualityExpression_BangEqual $ shrink z)
    EqualityExpression_LessGreater z@(u,_,v) ->
      (EqualityExpression_Relational v) : u :
        (map EqualityExpression_LessGreater $ shrink z)
    EqualityExpression_TripleEqual z@(u,_,v) ->
      (EqualityExpression_Relational v) : u :
        (map EqualityExpression_TripleEqual $ shrink z)
    EqualityExpression_BangEqualEqual z@(u,_,v) ->
      (EqualityExpression_Relational v) : u :
        (map EqualityExpression_BangEqualEqual $ shrink z) -}





data RelationalExpression
  = RelationalExpression_Shift
        ShiftExpression
  | RelationalExpression_Less
      ( RelationalExpression
      , Symbol Less_
      , ShiftExpression
      )
  | RelationalExpression_Greater
      ( RelationalExpression
      , Symbol Greater_
      , ShiftExpression
      )
  | RelationalExpression_LessEqual
      ( RelationalExpression
      , Symbol LessEqual_
      , ShiftExpression
      )
  | RelationalExpression_GreaterEqual
      ( RelationalExpression
      , Symbol GreaterEqual_
      , ShiftExpression
      )
  | RelationalExpression_Spaceship
      ( RelationalExpression
      , Symbol Spaceship_
      , ShiftExpression
      )
  deriving (Eq, Show)

instance Render RelationalExpression where
  render x = case x of
    RelationalExpression_Shift        z -> render z
    RelationalExpression_Less         z -> render z
    RelationalExpression_Greater      z -> render z
    RelationalExpression_LessEqual    z -> render z
    RelationalExpression_GreaterEqual z -> render z
    RelationalExpression_Spaceship    z -> render z

instance HasExtent RelationalExpression where
  extentOf x = case x of
    RelationalExpression_Shift        z -> extentOf z
    RelationalExpression_Less         z -> extentOf z
    RelationalExpression_Greater      z -> extentOf z
    RelationalExpression_LessEqual    z -> extentOf z
    RelationalExpression_GreaterEqual z -> extentOf z
    RelationalExpression_Spaceship    z -> extentOf z

  shiftTo loc x = case x of
    RelationalExpression_Shift z ->
      shiftToWrap RelationalExpression_Shift loc z
    RelationalExpression_Less z ->
      shiftToWrap RelationalExpression_Less loc z
    RelationalExpression_Greater z ->
      shiftToWrap RelationalExpression_Greater loc z
    RelationalExpression_LessEqual z ->
      shiftToWrap RelationalExpression_LessEqual loc z
    RelationalExpression_GreaterEqual z ->
      shiftToWrap RelationalExpression_GreaterEqual loc z
    RelationalExpression_Spaceship z ->
      shiftToWrap RelationalExpression_Spaceship loc z

instance Arbitrary RelationalExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ RelationalExpression_Shift        <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ RelationalExpression_Shift        <$> arbitrary
        , RelationalExpression_Less         <$> arbitrary
        , RelationalExpression_Greater      <$> arbitrary
        , RelationalExpression_LessEqual    <$> arbitrary
        , RelationalExpression_GreaterEqual <$> arbitrary
        , RelationalExpression_Spaceship    <$> arbitrary
        ]

  {- shrink x = case x of
    RelationalExpression_Shift z ->
      map RelationalExpression_Shift $ shrink z
    RelationalExpression_Less z@(u,_,v) ->
      (RelationalExpression_Shift v) : u :
        (map RelationalExpression_Less $ shrink z)
    RelationalExpression_Greater z@(u,_,v) ->
      (RelationalExpression_Shift v) : u :
        (map RelationalExpression_Greater $ shrink z)
    RelationalExpression_LessEqual z@(u,_,v) ->
      (RelationalExpression_Shift v) : u :
        (map RelationalExpression_LessEqual $ shrink z)
    RelationalExpression_GreaterEqual z@(u,_,v) ->
      (RelationalExpression_Shift v) : u :
        (map RelationalExpression_GreaterEqual $ shrink z)
    RelationalExpression_Spaceship z@(u,_,v) ->
      (RelationalExpression_Shift v) : u :
        (map RelationalExpression_Spaceship $ shrink z) -}





data ShiftExpression
  = ShiftExpression_Additive
        AdditiveExpression
  | ShiftExpression_Left
      ( ShiftExpression
      , Symbol DoubleLess_
      , AdditiveExpression
      )
  | ShiftExpression_Right
      ( ShiftExpression
      , Symbol DoubleGreater_
      , AdditiveExpression
      )
  deriving (Eq, Show)

instance Render ShiftExpression where
  render x = case x of
    ShiftExpression_Additive z -> render z
    ShiftExpression_Left     z -> render z
    ShiftExpression_Right    z -> render z

instance HasExtent ShiftExpression where
  extentOf x = case x of
    ShiftExpression_Additive z -> extentOf z
    ShiftExpression_Left     z -> extentOf z
    ShiftExpression_Right    z -> extentOf z

  shiftTo loc x = case x of
    ShiftExpression_Additive z ->
      shiftToWrap ShiftExpression_Additive loc z
    ShiftExpression_Left z ->
      shiftToWrap ShiftExpression_Left loc z
    ShiftExpression_Right z ->
      shiftToWrap ShiftExpression_Right loc z

instance Arbitrary ShiftExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ShiftExpression_Additive <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ ShiftExpression_Additive <$> arbitrary
        , ShiftExpression_Left     <$> arbitrary
        , ShiftExpression_Right    <$> arbitrary
        ]

  shrink x = case x of
    ShiftExpression_Additive z ->
      map ShiftExpression_Additive $ shrink z
    ShiftExpression_Left z@(u,_,v) ->
      (ShiftExpression_Additive v) : u :
        (map ShiftExpression_Left $ shrink z)
    ShiftExpression_Right z@(u,_,v) ->
      (ShiftExpression_Additive v) : u :
        (map ShiftExpression_Right $ shrink z)





data AdditiveExpression
  = AdditiveExpression_Multiplicative
        MultiplicativeExpression
  | AdditiveExpression_Plus
      ( AdditiveExpression
      , Symbol Plus_
      , MultiplicativeExpression
      )
  | AdditiveExpression_Minus
      ( AdditiveExpression
      , Symbol Minus_
      , MultiplicativeExpression
      )
  | AdditiveExpression_Dot
      ( AdditiveExpression
      , Symbol Dot_
      , MultiplicativeExpression
      )
  deriving (Eq, Show)

instance Render AdditiveExpression where
  render x = case x of
    AdditiveExpression_Multiplicative z -> render z
    AdditiveExpression_Plus           z -> render z
    AdditiveExpression_Minus          z -> render z
    AdditiveExpression_Dot            z -> render z

instance HasExtent AdditiveExpression where
  extentOf x = case x of
    AdditiveExpression_Multiplicative z -> extentOf z
    AdditiveExpression_Plus           z -> extentOf z
    AdditiveExpression_Minus          z -> extentOf z
    AdditiveExpression_Dot            z -> extentOf z

  shiftTo loc x = case x of
    AdditiveExpression_Multiplicative z ->
      shiftToWrap AdditiveExpression_Multiplicative loc z
    AdditiveExpression_Plus z ->
      shiftToWrap AdditiveExpression_Plus loc z
    AdditiveExpression_Minus z ->
      shiftToWrap AdditiveExpression_Minus loc z
    AdditiveExpression_Dot z ->
      shiftToWrap AdditiveExpression_Dot loc z

instance Arbitrary AdditiveExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ AdditiveExpression_Multiplicative <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ AdditiveExpression_Multiplicative <$> arbitrary
        , AdditiveExpression_Plus           <$> arbitrary
        , AdditiveExpression_Minus          <$> arbitrary
        , AdditiveExpression_Dot            <$> arbitrary
        ]

  shrink x = case x of
    AdditiveExpression_Multiplicative z ->
      map AdditiveExpression_Multiplicative $ shrink z
    AdditiveExpression_Plus z@(u,_,v) ->
      (AdditiveExpression_Multiplicative v) : u :
        (map AdditiveExpression_Plus $ shrink z)
    AdditiveExpression_Minus z@(u,_,v) ->
      (AdditiveExpression_Multiplicative v) : u :
        (map AdditiveExpression_Minus $ shrink z)
    AdditiveExpression_Dot z@(u,_,v) ->
      (AdditiveExpression_Multiplicative v) : u :
        (map AdditiveExpression_Dot $ shrink z)






data MultiplicativeExpression
  = MultiplicativeExpression_Not
        LogicalNotExpression
  | MultiplicativeExpression_Ast
      ( MultiplicativeExpression
      , Symbol Ast_
      , LogicalNotExpression
      )
  | MultiplicativeExpression_Slash
      ( MultiplicativeExpression
      , Symbol Slash_
      , LogicalNotExpression
      )
  | MultiplicativeExpression_Percent
      ( MultiplicativeExpression
      , Symbol Percent_
      , LogicalNotExpression
      )
  deriving (Eq, Show)

instance Render MultiplicativeExpression where
  render x = case x of
    MultiplicativeExpression_Not     z -> render z
    MultiplicativeExpression_Ast     z -> render z
    MultiplicativeExpression_Slash   z -> render z
    MultiplicativeExpression_Percent z -> render z

instance HasExtent MultiplicativeExpression where
  extentOf x = case x of
    MultiplicativeExpression_Not     z -> extentOf z
    MultiplicativeExpression_Ast     z -> extentOf z
    MultiplicativeExpression_Slash   z -> extentOf z
    MultiplicativeExpression_Percent z -> extentOf z

  shiftTo loc x = case x of
    MultiplicativeExpression_Not z ->
      shiftToWrap MultiplicativeExpression_Not loc z
    MultiplicativeExpression_Ast z ->
      shiftToWrap MultiplicativeExpression_Ast loc z
    MultiplicativeExpression_Slash z ->
      shiftToWrap MultiplicativeExpression_Slash loc z
    MultiplicativeExpression_Percent z ->
      shiftToWrap MultiplicativeExpression_Percent loc z

instance Arbitrary MultiplicativeExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ MultiplicativeExpression_Not     <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ MultiplicativeExpression_Not     <$> arbitrary
        , MultiplicativeExpression_Ast     <$> arbitrary
        , MultiplicativeExpression_Slash   <$> arbitrary
        , MultiplicativeExpression_Percent <$> arbitrary
        ]

  shrink x = case x of
    MultiplicativeExpression_Not z ->
      map MultiplicativeExpression_Not $ shrink z
    MultiplicativeExpression_Ast z ->
      map MultiplicativeExpression_Ast $ shrink z
    MultiplicativeExpression_Slash z ->
      map MultiplicativeExpression_Slash $ shrink z
    MultiplicativeExpression_Percent z ->
      map MultiplicativeExpression_Percent $ shrink z





data LogicalNotExpression
  = LogicalNotExpression_Instanceof
        InstanceofExpression
  | LogicalNotExpression_Not
      ( Symbol Bang_
      , InstanceofExpression
      )
  deriving (Eq, Show)

instance Render LogicalNotExpression where
  render x = case x of
    LogicalNotExpression_Instanceof z -> render z
    LogicalNotExpression_Not        z -> render z

instance HasExtent LogicalNotExpression where
  extentOf x = case x of
    LogicalNotExpression_Instanceof z -> extentOf z
    LogicalNotExpression_Not        z -> extentOf z

  shiftTo loc x = case x of
    LogicalNotExpression_Instanceof z ->
      shiftToWrap LogicalNotExpression_Instanceof loc z
    LogicalNotExpression_Not z ->
      shiftToWrap LogicalNotExpression_Not loc z

instance Arbitrary LogicalNotExpression where
  arbitrary = scale (frac 1 2) $ oneof
    [ LogicalNotExpression_Instanceof <$> arbitrary
    , LogicalNotExpression_Not        <$> arbitrary
    ]

  shrink x = case x of
    LogicalNotExpression_Instanceof z ->
      map LogicalNotExpression_Instanceof $ shrink z
    LogicalNotExpression_Not z@(_,v) ->
      (LogicalNotExpression_Instanceof v) :
        (map LogicalNotExpression_Not $ shrink z)





data InstanceofExpression
  = InstanceofExpression_Unary
        UnaryExpression
  | InstanceofExpression_Instanceof
      ( InstanceofSubject
      , Keyword Instanceof_
      , ClassTypeDesignator
      )
  deriving (Eq, Show)

instance Render InstanceofExpression where
  render x = case x of
    InstanceofExpression_Unary      z -> render z
    InstanceofExpression_Instanceof z -> render z

instance HasExtent InstanceofExpression where
  extentOf x = case x of
    InstanceofExpression_Unary      z -> extentOf z
    InstanceofExpression_Instanceof z -> extentOf z

  shiftTo loc x = case x of
    InstanceofExpression_Unary z ->
      shiftToWrap InstanceofExpression_Unary loc z
    InstanceofExpression_Instanceof z ->
      shiftToWrap InstanceofExpression_Instanceof loc z

instance Arbitrary InstanceofExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ InstanceofExpression_Unary      <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ InstanceofExpression_Unary      <$> arbitrary
        , InstanceofExpression_Instanceof <$> arbitraryKeywordOp
        ]

  {- shrink x = case x of
    InstanceofExpression_Unary z ->
      map InstanceofExpression_Unary $ shrink z
    InstanceofExpression_Instanceof z ->
      map InstanceofExpression_Instanceof $
        filter unambiguousKeywordOp $
        shrink z -}





data ClassTypeDesignator
  = ClassTypeDesignator_Qualified QualifiedName
  | ClassTypeDesignator_New       NewVariable
  deriving (Eq, Show)

instance Render ClassTypeDesignator where
  render x = case x of
    ClassTypeDesignator_Qualified z -> render z
    ClassTypeDesignator_New       z -> render z

instance HasExtent ClassTypeDesignator where
  extentOf x = case x of
    ClassTypeDesignator_Qualified z -> extentOf z
    ClassTypeDesignator_New       z -> extentOf z

  shiftTo loc x = case x of
    ClassTypeDesignator_Qualified z ->
      shiftToWrap ClassTypeDesignator_Qualified loc z
    ClassTypeDesignator_New z ->
      shiftToWrap ClassTypeDesignator_New loc z

instance Arbitrary ClassTypeDesignator where
  arbitrary = scale (frac 1 2) $ oneof
    [ ClassTypeDesignator_Qualified <$> arbitrary
    , ClassTypeDesignator_New       <$> arbitrary
    ]

  shrink x = case x of
    ClassTypeDesignator_Qualified z ->
      map ClassTypeDesignator_Qualified $ shrink z
    ClassTypeDesignator_New z ->
      map ClassTypeDesignator_New $ shrink z





data NewVariable
  = NewVariable_Simple
        SimpleVariable
  | NewVariable_Brack
      ( NewVariable
      , Symbol OpenBrack_
      , Maybe Expression
      , Symbol ClosedBrack_
      )
  | NewVariable_Brace
      ( NewVariable
      , Symbol OpenBrace_
      , Expression
      , Symbol ClosedBrace_
      )
  | NewVariable_Member
      ( NewVariable
      , Symbol MinusGreater_
      , MemberName
      )
  | NewVariable_Qualified
      ( QualifiedName
      , Symbol DoubleColon_
      , SimpleVariable
      )
  | NewVariable_Relative
      ( RelativeScope
      , Symbol DoubleColon_
      , SimpleVariable
      )
  | NewVariable_Colon
      ( NewVariable
      , Symbol DoubleColon_
      , SimpleVariable
      )
  deriving (Eq, Show)

instance Render NewVariable where
  render x = case x of
    NewVariable_Simple    z -> render z
    NewVariable_Brack     z -> render z
    NewVariable_Brace     z -> render z
    NewVariable_Member    z -> render z
    NewVariable_Qualified z -> render z
    NewVariable_Relative  z -> render z
    NewVariable_Colon     z -> render z

instance HasExtent NewVariable where
  extentOf x = case x of
    NewVariable_Simple    z -> extentOf z
    NewVariable_Brack     z -> extentOf z
    NewVariable_Brace     z -> extentOf z
    NewVariable_Member    z -> extentOf z
    NewVariable_Qualified z -> extentOf z
    NewVariable_Relative  z -> extentOf z
    NewVariable_Colon     z -> extentOf z

  shiftTo loc x = case x of
    NewVariable_Simple z ->
      shiftToWrap NewVariable_Simple loc z
    NewVariable_Brack z ->
      shiftToWrap NewVariable_Brack loc z
    NewVariable_Brace z ->
      shiftToWrap NewVariable_Brace loc z
    NewVariable_Member z ->
      shiftToWrap NewVariable_Member loc z
    NewVariable_Qualified z ->
      shiftToWrap NewVariable_Qualified loc z
    NewVariable_Relative z ->
      shiftToWrap NewVariable_Relative loc z
    NewVariable_Colon z ->
      shiftToWrap NewVariable_Colon loc z

instance Arbitrary NewVariable where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ NewVariable_Simple    <$> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ NewVariable_Simple    <$> arbitrary
        , NewVariable_Brack     <$> arbitrary
        , NewVariable_Brace     <$> arbitrary
        , NewVariable_Member    <$> arbitrary
        , NewVariable_Qualified <$> arbitrary
        , NewVariable_Relative  <$> arbitrary
        , NewVariable_Colon     <$> arbitrary
        ]

  shrink x = case x of
    NewVariable_Simple z ->
      map NewVariable_Simple $ shrink z
    NewVariable_Brack z ->
      map NewVariable_Brack $ shrink z
    NewVariable_Brace z ->
      map NewVariable_Brace $ shrink z
    NewVariable_Member z ->
      map NewVariable_Member $ shrink z
    NewVariable_Qualified z ->
      map NewVariable_Qualified $ shrink z
    NewVariable_Relative z ->
      map NewVariable_Relative $ shrink z
    NewVariable_Colon z ->
      map NewVariable_Colon $ shrink z





data UnaryExpression
  = UnaryExpression_Exp   ExponentiationExpression
  | UnaryExpression_Op    UnaryOpExpression
  | UnaryExpression_Error ErrorControlExpression
  | UnaryExpression_Cast  CastExpression
  deriving (Eq, Show)

instance Render UnaryExpression where
  render x = case x of
    UnaryExpression_Exp   z -> render z
    UnaryExpression_Op    z -> render z
    UnaryExpression_Error z -> render z
    UnaryExpression_Cast  z -> render z

instance HasExtent UnaryExpression where
  extentOf x = case x of
    UnaryExpression_Exp   z -> extentOf z
    UnaryExpression_Op    z -> extentOf z
    UnaryExpression_Error z -> extentOf z
    UnaryExpression_Cast  z -> extentOf z

  shiftTo loc x = case x of
    UnaryExpression_Exp z ->
      shiftToWrap UnaryExpression_Exp loc z
    UnaryExpression_Op z ->
      shiftToWrap UnaryExpression_Op loc z
    UnaryExpression_Error z ->
      shiftToWrap UnaryExpression_Error loc z
    UnaryExpression_Cast z ->
      shiftToWrap UnaryExpression_Cast loc z

instance Arbitrary UnaryExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ UnaryExpression_Exp   <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ UnaryExpression_Exp   <$> arbitrary
        , UnaryExpression_Op    <$> arbitrary
        , UnaryExpression_Error <$> arbitrary
        , UnaryExpression_Cast  <$> arbitrary
        ]

  shrink x = case x of
    UnaryExpression_Exp z ->
      map UnaryExpression_Exp $ shrink z
    UnaryExpression_Op z ->
      map UnaryExpression_Op $ shrink z
    UnaryExpression_Error z ->
      map UnaryExpression_Error $ shrink z
    UnaryExpression_Cast z ->
      map UnaryExpression_Cast $ shrink z





data ExponentiationExpression
  = ExponentiationExpression_Clone
        CloneExpression
  | ExponentiationExpression_Exp
      ( CloneExpression
      , Symbol AstAst_
      , ExponentiationExpression
      )
  deriving (Eq, Show)

instance Render ExponentiationExpression where
  render x = case x of
    ExponentiationExpression_Clone z -> render z
    ExponentiationExpression_Exp   z -> render z

instance HasExtent ExponentiationExpression where
  extentOf x = case x of
    ExponentiationExpression_Clone z -> extentOf z
    ExponentiationExpression_Exp   z -> extentOf z

  shiftTo loc x = case x of
    ExponentiationExpression_Clone z ->
      shiftToWrap ExponentiationExpression_Clone loc z
    ExponentiationExpression_Exp z ->
      shiftToWrap ExponentiationExpression_Exp loc z

instance Arbitrary ExponentiationExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ExponentiationExpression_Clone <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ ExponentiationExpression_Clone <$> arbitrary
        , ExponentiationExpression_Exp   <$> arbitrary
        ]

  shrink x = case x of
    ExponentiationExpression_Clone z ->
      map ExponentiationExpression_Clone $ shrink z
    ExponentiationExpression_Exp z ->
      map ExponentiationExpression_Exp $ shrink z







data CloneExpression
  = CloneExpression_Primary
        PrimaryExpression
  | CloneExpression_Clone
      ( Keyword Clone_
      , PrimaryExpression
      )
  deriving (Eq, Show)

instance Render CloneExpression where
  render x = case x of
    CloneExpression_Primary z -> render z
    CloneExpression_Clone   z -> render z

instance HasExtent CloneExpression where
  extentOf x = case x of
    CloneExpression_Primary z -> extentOf z
    CloneExpression_Clone   z -> extentOf z

  shiftTo loc x = case x of
    CloneExpression_Primary z -> shiftToWrap CloneExpression_Primary loc z
    CloneExpression_Clone   z -> shiftToWrap CloneExpression_Clone loc z

instance Arbitrary CloneExpression where
  arbitrary = oneof
    [ CloneExpression_Primary <$> arbitrary
    , CloneExpression_Clone   <$> arbitrary
    ]

  shrink x = case x of
    CloneExpression_Primary z ->
      map CloneExpression_Primary $ shrink z
    CloneExpression_Clone z ->
      map CloneExpression_Clone $ shrink z





data PrimaryExpression
  = PrimaryExpression_Variable   Variable
  | PrimaryExpression_Class      ClassConstantAccessExpression
  | PrimaryExpression_Constant   ConstantAccessExpression
  | PrimaryExpression_Literal    Literal
  | PrimaryExpression_Array      ArrayCreationExpression
  | PrimaryExpression_Intrinsic  Intrinsic
  | PrimaryExpression_Anonymous  AnonymousFunctionCreationExpression
  | PrimaryExpression_Object     ObjectCreationExpression
  | PrimaryExpression_PostfixInc PostfixIncrementExpression
  | PrimaryExpression_PostfixDec PostfixDecrementExpression
  | PrimaryExpression_PrefixInc  PrefixIncrementExpression
  | PrimaryExpression_PrefixDec  PrefixDecrementExpression
  | PrimaryExpression_Byref      ByrefAssignmentExpression
  | PrimaryExpression_Shell      ShellCommandExpression
  | PrimaryExpression_Expression
      ( Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      )
  deriving (Eq, Show)

instance Render PrimaryExpression where
  render x = case x of
    PrimaryExpression_Variable   z -> render z
    PrimaryExpression_Class      z -> render z
    PrimaryExpression_Constant   z -> render z
    PrimaryExpression_Literal    z -> render z
    PrimaryExpression_Array      z -> render z
    PrimaryExpression_Intrinsic  z -> render z
    PrimaryExpression_Anonymous  z -> render z
    PrimaryExpression_Object     z -> render z
    PrimaryExpression_PostfixInc z -> render z
    PrimaryExpression_PostfixDec z -> render z
    PrimaryExpression_PrefixInc  z -> render z
    PrimaryExpression_PrefixDec  z -> render z
    PrimaryExpression_Byref      z -> render z
    PrimaryExpression_Shell      z -> render z
    PrimaryExpression_Expression z -> render z

instance HasExtent PrimaryExpression where
  extentOf x = case x of
    PrimaryExpression_Variable   z -> extentOf z
    PrimaryExpression_Class      z -> extentOf z
    PrimaryExpression_Constant   z -> extentOf z
    PrimaryExpression_Literal    z -> extentOf z
    PrimaryExpression_Array      z -> extentOf z
    PrimaryExpression_Intrinsic  z -> extentOf z
    PrimaryExpression_Anonymous  z -> extentOf z
    PrimaryExpression_Object     z -> extentOf z
    PrimaryExpression_PostfixInc z -> extentOf z
    PrimaryExpression_PostfixDec z -> extentOf z
    PrimaryExpression_PrefixInc  z -> extentOf z
    PrimaryExpression_PrefixDec  z -> extentOf z
    PrimaryExpression_Byref      z -> extentOf z
    PrimaryExpression_Shell      z -> extentOf z
    PrimaryExpression_Expression z -> extentOf z

  shiftTo loc x = case x of
    PrimaryExpression_Variable   z -> shiftToWrap PrimaryExpression_Variable loc z
    PrimaryExpression_Class      z -> shiftToWrap PrimaryExpression_Class loc z
    PrimaryExpression_Constant   z -> shiftToWrap PrimaryExpression_Constant loc z
    PrimaryExpression_Literal    z -> shiftToWrap PrimaryExpression_Literal loc z
    PrimaryExpression_Array      z -> shiftToWrap PrimaryExpression_Array loc z
    PrimaryExpression_Intrinsic  z -> shiftToWrap PrimaryExpression_Intrinsic loc z
    PrimaryExpression_Anonymous  z -> shiftToWrap PrimaryExpression_Anonymous loc z
    PrimaryExpression_Object     z -> shiftToWrap PrimaryExpression_Object loc z
    PrimaryExpression_PostfixInc z -> shiftToWrap PrimaryExpression_PostfixInc loc z
    PrimaryExpression_PostfixDec z -> shiftToWrap PrimaryExpression_PostfixDec loc z
    PrimaryExpression_PrefixInc  z -> shiftToWrap PrimaryExpression_PrefixInc loc z
    PrimaryExpression_PrefixDec  z -> shiftToWrap PrimaryExpression_PrefixDec loc z
    PrimaryExpression_Byref      z -> shiftToWrap PrimaryExpression_Byref loc z
    PrimaryExpression_Shell      z -> shiftToWrap PrimaryExpression_Shell loc z
    PrimaryExpression_Expression z -> shiftToWrap PrimaryExpression_Expression loc z

instance Arbitrary PrimaryExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ PrimaryExpression_Variable   <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ PrimaryExpression_Variable   <$> arbitrary
        , PrimaryExpression_Class      <$> arbitrary
        , PrimaryExpression_Constant   <$> arbitrary
        , PrimaryExpression_Literal    <$> arbitrary
        , PrimaryExpression_Array      <$> arbitrary
        , PrimaryExpression_Intrinsic  <$> arbitrary
        , PrimaryExpression_Anonymous  <$> arbitrary
        , PrimaryExpression_Object     <$> arbitrary
        , PrimaryExpression_PostfixInc <$> arbitrary
        , PrimaryExpression_PostfixDec <$> arbitrary
        , PrimaryExpression_PrefixInc  <$> arbitrary
        , PrimaryExpression_PrefixDec  <$> arbitrary
        , PrimaryExpression_Byref      <$> arbitrary
        , PrimaryExpression_Shell      <$> arbitrary
        , PrimaryExpression_Expression <$> arbitrary
        ]

  shrink x = case x of
    PrimaryExpression_Variable z ->
      map PrimaryExpression_Variable $ shrink z
    PrimaryExpression_Class z ->
      map PrimaryExpression_Class $ shrink z
    PrimaryExpression_Constant z ->
      map PrimaryExpression_Constant $ shrink z
    PrimaryExpression_Literal z ->
      map PrimaryExpression_Literal $ shrink z
    PrimaryExpression_Array z ->
      map PrimaryExpression_Array $ shrink z
    PrimaryExpression_Intrinsic z ->
      map PrimaryExpression_Intrinsic $ shrink z
    PrimaryExpression_Anonymous z ->
      map PrimaryExpression_Anonymous $ shrink z
    PrimaryExpression_Object z ->
      map PrimaryExpression_Object $ shrink z
    PrimaryExpression_PostfixInc z ->
      map PrimaryExpression_PostfixInc $ shrink z
    PrimaryExpression_PostfixDec z ->
      map PrimaryExpression_PostfixDec $ shrink z
    PrimaryExpression_PrefixInc z ->
      map PrimaryExpression_PrefixInc $ shrink z
    PrimaryExpression_PrefixDec z ->
      map PrimaryExpression_PrefixDec $ shrink z
    PrimaryExpression_Byref z ->
      map PrimaryExpression_Byref $ shrink z
    PrimaryExpression_Shell z ->
      map PrimaryExpression_Shell $ shrink z
    PrimaryExpression_Expression z ->
      map PrimaryExpression_Expression $ shrink z





data Variable
  = Variable_Callable CallableVariable
  | Variable_Scoped   ScopedPropertyAccessExpression
  | Variable_Member   MemberAccessExpression
  deriving (Eq, Show)

instance Render Variable where
  render x = case x of
    Variable_Callable z -> render z
    Variable_Scoped   z -> render z
    Variable_Member   z -> render z

instance HasExtent Variable where
  extentOf x = case x of
    Variable_Callable z -> extentOf z
    Variable_Scoped   z -> extentOf z
    Variable_Member   z -> extentOf z

  shiftTo loc x = case x of
    Variable_Callable z ->
      shiftToWrap Variable_Callable loc z
    Variable_Scoped z ->
      shiftToWrap Variable_Scoped loc z
    Variable_Member z ->
      shiftToWrap Variable_Member loc z

instance Arbitrary Variable where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ Variable_Callable <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ Variable_Callable <$> arbitrary
        , Variable_Scoped   <$> arbitrary
        , Variable_Member   <$> arbitrary
        ]

  shrink x = case x of
    Variable_Callable z ->
      map Variable_Callable $ shrink z
    Variable_Scoped z ->
      map Variable_Scoped $ shrink z
    Variable_Member z ->
      map Variable_Member $ shrink z





data CallableVariable
  = CallableVariable_Simple    SimpleVariable
  | CallableVariable_Subscript SubscriptExpression 
  | CallableVariable_Member    MemberCallExpression
  | CallableVariable_Scoped    ScopedCallExpression
  | CallableVariable_Function  FunctionCallExpression
  deriving (Eq, Show)

instance Render CallableVariable where
  render x = case x of
    CallableVariable_Simple    z -> render z
    CallableVariable_Subscript z -> render z
    CallableVariable_Member    z -> render z
    CallableVariable_Scoped    z -> render z
    CallableVariable_Function  z -> render z

instance HasExtent CallableVariable where
  extentOf x = case x of
    CallableVariable_Simple    z -> extentOf z
    CallableVariable_Subscript z -> extentOf z
    CallableVariable_Member    z -> extentOf z
    CallableVariable_Scoped    z -> extentOf z
    CallableVariable_Function  z -> extentOf z

  shiftTo loc x = case x of
    CallableVariable_Simple z ->
      shiftToWrap CallableVariable_Simple loc z
    CallableVariable_Subscript z ->
      shiftToWrap CallableVariable_Subscript loc z
    CallableVariable_Member z ->
      shiftToWrap CallableVariable_Member loc z
    CallableVariable_Scoped z ->
      shiftToWrap CallableVariable_Scoped loc z
    CallableVariable_Function z ->
      shiftToWrap CallableVariable_Function loc z

instance Arbitrary CallableVariable where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ CallableVariable_Simple    <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ CallableVariable_Simple    <$> arbitrary
        , CallableVariable_Subscript <$> arbitrary
        , CallableVariable_Member    <$> arbitrary
        , CallableVariable_Scoped    <$> arbitrary
        , CallableVariable_Function  <$> arbitrary
        ]

  shrink x = case x of
    CallableVariable_Simple z ->
      map CallableVariable_Simple $ shrink z
    CallableVariable_Subscript z ->
      map CallableVariable_Subscript $ shrink z
    CallableVariable_Member z ->
      map CallableVariable_Member $ shrink z
    CallableVariable_Scoped z ->
      map CallableVariable_Scoped $ shrink z
    CallableVariable_Function z ->
      map CallableVariable_Function $ shrink z





data SimpleVariable
  = SimpleVariable_Name
        VariableName
  | SimpleVariable_Simple
      ( Dollar_
      , SimpleVariable
      )
  | SimpleVariable_Expression
      ( Dollar_
      , Symbol OpenBrace_
      , Expression
      , Symbol ClosedBrace_
      )
  deriving (Eq, Show)

instance Render SimpleVariable where
  render x = case x of
    SimpleVariable_Name       z -> render z
    SimpleVariable_Simple     z -> render z
    SimpleVariable_Expression z -> render z

instance HasExtent SimpleVariable where
  extentOf x = case x of
    SimpleVariable_Name       z -> extentOf z
    SimpleVariable_Simple     z -> extentOf z
    SimpleVariable_Expression z -> extentOf z

  shiftTo loc x = case x of
    SimpleVariable_Name z ->
      shiftToWrap SimpleVariable_Name loc z
    SimpleVariable_Simple z ->
      shiftToWrap SimpleVariable_Simple loc z
    SimpleVariable_Expression z ->
      shiftToWrap SimpleVariable_Expression loc z

instance Arbitrary SimpleVariable where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ SimpleVariable_Name       <$> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ SimpleVariable_Name       <$> arbitrary
        , SimpleVariable_Simple     <$> arbitrary
        , SimpleVariable_Expression <$> arbitrary
        ]

  shrink x = case x of
    SimpleVariable_Name z ->
      map SimpleVariable_Name $ shrink z
    SimpleVariable_Simple z@(_,w) ->
      w : (map SimpleVariable_Simple $ shrink z)
    SimpleVariable_Expression z ->
      map SimpleVariable_Expression $ shrink z





newtype ConstantExpression = ConstantExpression
  { unConstantExpression :: Expression
  } deriving (Eq, Show)

instance Render ConstantExpression where
  render = render . unConstantExpression

instance HasExtent ConstantExpression where
  extentOf = extentOf . unConstantExpression

  shiftTo loc (ConstantExpression x) =
    shiftToWrap ConstantExpression loc x

instance Arbitrary ConstantExpression where
  arbitrary = ConstantExpression
    <$> (scale (frac 1 2) arbitrary)

  shrink (ConstantExpression x) =
    map ConstantExpression $ shrink x





newtype ElseClause1 = ElseClause1
  { unElseClause1 ::
      ( Keyword Else_
      , Statement
      )
  } deriving (Eq, Show)

instance Render ElseClause1 where
  render = render . unElseClause1

instance HasExtent ElseClause1 where
  extentOf = extentOf . unElseClause1

  shiftTo loc (ElseClause1 x) =
    shiftToWrap ElseClause1 loc x

instance Arbitrary ElseClause1 where
  arbitrary = scale (frac 1 2) $
    ElseClause1 <$> arbitraryKeyword2

  {- shrink (ElseClause1 x) =
    map ElseClause1 $
      filter unambiguousKeyword2 $
      shrink x -}





newtype ElseClause2 = ElseClause2
  { unElseClause2 ::
      ( Keyword Else_
      , Symbol Colon_
      , StatementList
      )
  } deriving (Eq, Show)

instance Render ElseClause2 where
  render = render . unElseClause2

instance HasExtent ElseClause2 where
  extentOf = extentOf . unElseClause2

  shiftTo loc (ElseClause2 x) =
    shiftToWrap ElseClause2 loc x

instance Arbitrary ElseClause2 where
  arbitrary = scale (frac 1 4) $
    ElseClause2 <$> arbitraryKeyword3

  {- shrink (ElseClause2 x) =
    map ElseClause2 $
      filter unambiguousKeyword3 $
      shrink x -}





newtype FunctionStaticInitializer = FunctionStaticInitializer
  { unFunctionStaticInitializer ::
      ( Symbol Equal_
      , ConstantExpression
      )
  } deriving (Eq, Show)

instance Render FunctionStaticInitializer where
  render = render . unFunctionStaticInitializer

instance HasExtent FunctionStaticInitializer where
  extentOf = extentOf . unFunctionStaticInitializer

  shiftTo loc (FunctionStaticInitializer z) =
    shiftToWrap FunctionStaticInitializer loc z

instance Arbitrary FunctionStaticInitializer where
  arbitrary = scale (frac 1 2) $
    FunctionStaticInitializer <$> arbitrary

  shrink (FunctionStaticInitializer x) =
    map FunctionStaticInitializer $ shrink x





newtype DefaultArgumentSpecifier = DefaultArgumentSpecifier
  { unDefaultArgumentSpecifier ::
      ( Symbol Equal_
      , ConstantExpression
      )
  } deriving (Eq, Show)

instance Render DefaultArgumentSpecifier where
  render = render . unDefaultArgumentSpecifier

instance HasExtent DefaultArgumentSpecifier where
  extentOf = extentOf . unDefaultArgumentSpecifier

  shiftTo loc (DefaultArgumentSpecifier z) =
    shiftToWrap DefaultArgumentSpecifier loc z

instance Arbitrary DefaultArgumentSpecifier where
  arbitrary = scale (frac 1 2) $
    DefaultArgumentSpecifier <$> arbitrary

  shrink (DefaultArgumentSpecifier x) =
    map DefaultArgumentSpecifier $ shrink x





data ScalarType
  = ScalarType_Bool   (Keyword Bool_)
  | ScalarType_Float  (Keyword Float_)
  | ScalarType_Int    (Keyword Int_)
  | ScalarType_String (Keyword String_)
  deriving (Eq, Show)

instance Render ScalarType where
  render x = case x of
    ScalarType_Bool   z -> render z
    ScalarType_Float  z -> render z
    ScalarType_Int    z -> render z
    ScalarType_String z -> render z

instance HasExtent ScalarType where
  extentOf x = case x of
    ScalarType_Bool   z -> extentOf z
    ScalarType_Float  z -> extentOf z
    ScalarType_Int    z -> extentOf z
    ScalarType_String z -> extentOf z

  shiftTo loc x = case x of
    ScalarType_Bool   z -> shiftToWrap ScalarType_Bool loc z
    ScalarType_Float  z -> shiftToWrap ScalarType_Float loc z
    ScalarType_Int    z -> shiftToWrap ScalarType_Int loc z
    ScalarType_String z -> shiftToWrap ScalarType_String loc z

instance Arbitrary ScalarType where
  arbitrary = scale (frac 1 3) $ oneof
    [ ScalarType_Bool   <$> arbitrary
    , ScalarType_Float  <$> arbitrary
    , ScalarType_Int    <$> arbitrary
    , ScalarType_String <$> arbitrary
    ]

  shrink x = case x of
    ScalarType_Bool z ->
      map ScalarType_Bool $ shrink z
    ScalarType_Float z ->
      map ScalarType_Float $ shrink z
    ScalarType_Int z ->
      map ScalarType_Int $ shrink z
    ScalarType_String z ->
      map ScalarType_String $ shrink z





data ClassModifier
  = ClassModifier_Abstract (Keyword Abstract_)
  | ClassModifier_Final    (Keyword Final_)
  deriving (Eq, Show)

instance Render ClassModifier where
  render x = case x of
    ClassModifier_Abstract z -> render z
    ClassModifier_Final    z -> render z

instance HasExtent ClassModifier where
  extentOf x = case x of
    ClassModifier_Abstract z -> extentOf z
    ClassModifier_Final    z -> extentOf z

  shiftTo loc x = case x of
    ClassModifier_Abstract z ->
      shiftToWrap ClassModifier_Abstract loc z
    ClassModifier_Final z ->
      shiftToWrap ClassModifier_Final loc z

instance Arbitrary ClassModifier where
  arbitrary = oneof
    [ ClassModifier_Abstract <$> arbitrary
    , ClassModifier_Final    <$> arbitrary
    ]

  shrink x = case x of
    ClassModifier_Abstract z ->
      map ClassModifier_Abstract $ shrink z
    ClassModifier_Final z ->
      map ClassModifier_Final $ shrink z





newtype FinallyClause = FinallyClause
  { unFinallyClause ::
      ( Keyword Finally_
      , CompoundStatement
      )
  } deriving (Eq, Show)

instance Render FinallyClause where
  render = render . unFinallyClause

instance HasExtent FinallyClause where
  extentOf = extentOf . unFinallyClause

  shiftTo loc (FinallyClause x) =
    shiftToWrap FinallyClause loc x

instance Arbitrary FinallyClause where
  arbitrary = scale (frac 1 2) $
    FinallyClause <$> arbitrary

  shrink (FinallyClause x) =
    map FinallyClause $ shrink x





newtype ThrowStatement = ThrowStatement
  { unThrowStatement ::
      ( Keyword Throw_
      , Expression
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render ThrowStatement where
  render = render . unThrowStatement

instance HasExtent ThrowStatement where
  extentOf = extentOf . unThrowStatement

  shiftTo loc (ThrowStatement x) =
    shiftToWrap ThrowStatement loc x

instance Arbitrary ThrowStatement where
  arbitrary = scale (frac 1 3) $
    ThrowStatement <$> arbitraryKeyword3

  {- shrink (ThrowStatement x) =
    map ThrowStatement $
      filter unambiguousKeyword3 $
      shrink x -}





newtype ReturnStatement = ReturnStatement
  { unReturnStatement ::
      ( Keyword Return_
      , Maybe Expression
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render ReturnStatement where
  render = render . unReturnStatement

instance HasExtent ReturnStatement where
  extentOf = extentOf . unReturnStatement

  shiftTo loc (ReturnStatement x) =
    shiftToWrap ReturnStatement loc x

instance Arbitrary ReturnStatement where
  arbitrary = scale (frac 1 3) $
    ReturnStatement <$> arbitraryKeyword3

  {- shrink (ReturnStatement x) =
    map ReturnStatement $
      filter unambiguousKeyword3 $
      shrink x -}





data BreakoutLevel
  = BreakoutLevel_Int
        IntegerLiteral
  | BreakoutLevel_Nest
      ( Symbol OpenParen_
      , BreakoutLevel
      , Symbol ClosedParen_
      )
  deriving (Eq, Show)

instance Render BreakoutLevel where
  render x = case x of
    BreakoutLevel_Int  z -> render z
    BreakoutLevel_Nest z -> render z

instance HasExtent BreakoutLevel where
  extentOf x = case x of
    BreakoutLevel_Int  z -> extentOf z
    BreakoutLevel_Nest z -> extentOf z

  shiftTo loc x = case x of
    BreakoutLevel_Int z ->
      shiftToWrap BreakoutLevel_Int loc z
    BreakoutLevel_Nest z ->
      shiftToWrap BreakoutLevel_Nest loc z

instance Arbitrary BreakoutLevel where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ BreakoutLevel_Int  <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ BreakoutLevel_Int  <$> arbitrary
        , BreakoutLevel_Nest <$> arbitrary
        ]

  shrink x = case x of
    BreakoutLevel_Int z ->
      map BreakoutLevel_Int $ shrink z
    BreakoutLevel_Nest z ->
      map BreakoutLevel_Nest $ shrink z





newtype ContinueStatement = ContinueStatement
  { unContinueStatement ::
      ( Keyword Continue_
      , Maybe BreakoutLevel
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render ContinueStatement where
  render = render . unContinueStatement

instance HasExtent ContinueStatement where
  extentOf = extentOf . unContinueStatement

  shiftTo loc (ContinueStatement x) =
    shiftToWrap ContinueStatement loc x

instance Arbitrary ContinueStatement where
  arbitrary = scale (frac 1 3) $
    ContinueStatement <$> arbitraryKeyword3

  {- shrink (ContinueStatement x) =
    map ContinueStatement $
      filter unambiguousKeyword3 $
      shrink x -}





newtype BreakStatement = BreakStatement
  { unBreakStatement ::
      ( Keyword Break_
      , Maybe BreakoutLevel
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render BreakStatement where
  render = render . unBreakStatement

instance HasExtent BreakStatement where
  extentOf = extentOf . unBreakStatement

  shiftTo loc (BreakStatement x) =
    shiftToWrap BreakStatement loc x

instance Arbitrary BreakStatement where
  arbitrary = scale (frac 1 3) $
    BreakStatement <$> arbitraryKeyword3

  {- shrink (BreakStatement x) =
    map BreakStatement $
      filter unambiguousKeyword3 $
      shrink x -}





data JumpStatement
  = JumpStatement_Goto     GotoStatement
  | JumpStatement_Continue ContinueStatement
  | JumpStatement_Break    BreakStatement
  | JumpStatement_Return   ReturnStatement
  | JumpStatement_Throw    ThrowStatement
  deriving (Eq, Show)

instance Render JumpStatement where
  render x = case x of
    JumpStatement_Goto     z -> render z
    JumpStatement_Continue z -> render z
    JumpStatement_Break    z -> render z
    JumpStatement_Return   z -> render z
    JumpStatement_Throw    z -> render z

instance HasExtent JumpStatement where
  extentOf x = case x of
    JumpStatement_Goto     z -> extentOf z
    JumpStatement_Continue z -> extentOf z
    JumpStatement_Break    z -> extentOf z
    JumpStatement_Return   z -> extentOf z
    JumpStatement_Throw    z -> extentOf z

  shiftTo loc x = case x of
    JumpStatement_Goto z ->
      shiftToWrap JumpStatement_Goto loc z
    JumpStatement_Continue z ->
      shiftToWrap JumpStatement_Continue loc z
    JumpStatement_Break z ->
      shiftToWrap JumpStatement_Break loc z
    JumpStatement_Return z ->
      shiftToWrap JumpStatement_Return loc z
    JumpStatement_Throw z ->
      shiftToWrap JumpStatement_Throw loc z

instance Arbitrary JumpStatement where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 2) $ oneof
        [ JumpStatement_Goto     <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ JumpStatement_Goto     <$> arbitrary
        , JumpStatement_Continue <$> arbitrary
        , JumpStatement_Break    <$> arbitrary
        , JumpStatement_Return   <$> arbitrary
        , JumpStatement_Throw    <$> arbitrary
        ]

  shrink x = case x of
    JumpStatement_Goto z ->
      map JumpStatement_Goto $ shrink z
    JumpStatement_Continue z ->
      map JumpStatement_Continue $ shrink z
    JumpStatement_Break z ->
      map JumpStatement_Break $ shrink z
    JumpStatement_Return z ->
      map JumpStatement_Return $ shrink z
    JumpStatement_Throw z ->
      map JumpStatement_Throw $ shrink z





newtype ElseifClause1 = ElseifClause1
  { unElseifClause1 ::
      ( Keyword Elseif_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      , Statement
      )
  } deriving (Eq, Show)

instance Render ElseifClause1 where
  render = render . unElseifClause1

instance HasExtent ElseifClause1 where
  extentOf = extentOf . unElseifClause1

  shiftTo loc (ElseifClause1 x) =
    shiftToWrap ElseifClause1 loc x

instance Arbitrary ElseifClause1 where
  arbitrary = scale (frac 1 6) $
    ElseifClause1 <$> arbitrary

  shrink (ElseifClause1 x) =
    map ElseifClause1 $ shrink x





newtype ElseifClauses1 = ElseifClauses1
  { unElseifClauses1 :: Seq ElseifClause1
  } deriving (Eq, Show)

instance Render ElseifClauses1 where
  render = render . unElseifClauses1

instance HasExtent ElseifClauses1 where
  extentOf = extentOf . unElseifClauses1

  shiftTo loc (ElseifClauses1 x) =
    shiftToWrap ElseifClauses1 loc x

instance Arbitrary ElseifClauses1 where
  arbitrary = ElseifClauses1
    <$> (scale (`div` 2) arbitrary)

  shrink (ElseifClauses1 x) =
    map ElseifClauses1 $ shrink x





newtype ElseifClause2 = ElseifClause2
  { unElseifClause2 ::
      ( Keyword Elseif_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      , Symbol Colon_
      , StatementList
      )
  } deriving (Eq, Show)

instance Render ElseifClause2 where
  render = render . unElseifClause2

instance HasExtent ElseifClause2 where
  extentOf = extentOf . unElseifClause2

  shiftTo loc (ElseifClause2 x) =
    shiftToWrap ElseifClause2 loc x

instance Arbitrary ElseifClause2 where
  arbitrary = scale (frac 1 7) $
    ElseifClause2 <$> arbitrary

  shrink (ElseifClause2 x) =
    fmap ElseifClause2 $ shrink x





newtype ElseifClauses2 = ElseifClauses2
  { unElseifClauses2 :: Seq ElseifClause2
  } deriving (Eq, Show)

instance Render ElseifClauses2 where
  render = render . unElseifClauses2

instance HasExtent ElseifClauses2 where
  extentOf = extentOf . unElseifClauses2

  shiftTo loc (ElseifClauses2 x) =
    shiftToWrap ElseifClauses2 loc x

instance Arbitrary ElseifClauses2 where
  arbitrary = scale (frac 1 2) $
    ElseifClauses2 <$> arbitrary

  shrink (ElseifClauses2 x) =
    map ElseifClauses2 $ shrink x





newtype IfStatement = IfStatement
  { unIfStatement ::
      ( Keyword If_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      , IfStatementBody
      )
  } deriving (Eq, Show)

instance Render IfStatement where
  render = render . unIfStatement

instance HasExtent IfStatement where
  extentOf = extentOf . unIfStatement

  shiftTo loc (IfStatement x) =
    shiftToWrap IfStatement loc x

instance Arbitrary IfStatement where
  arbitrary = scale (frac 1 6) $
    IfStatement <$> arbitrary

  -- shrink (IfStatement x) =
  --   map IfStatement $ shrink x





data IfStatementBody
  = IfStatementBody_Brace
      ( Statement
      , Maybe ElseifClauses1
      , Maybe ElseClause1
      )
  | IfStatementBody_Colon
      ( Symbol Colon_
      , StatementList
      , Maybe ElseifClauses2
      , Maybe ElseClause2
      , Keyword Endif_
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render IfStatementBody where
  render x = case x of
    IfStatementBody_Brace z -> render z
    IfStatementBody_Colon z -> render z

instance HasExtent IfStatementBody where
  extentOf x = case x of
    IfStatementBody_Brace z -> extentOf z
    IfStatementBody_Colon z -> extentOf z

  shiftTo loc x = case x of
    IfStatementBody_Brace z ->
      shiftToWrap IfStatementBody_Brace loc z
    IfStatementBody_Colon z ->
      shiftToWrap IfStatementBody_Colon loc z

instance Arbitrary IfStatementBody where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 2) $ oneof
        [ curry3 IfStatementBody_Brace
            <$> arbitrary
            <*> pure Nothing
            <*> pure Nothing
        ]
      else scale (frac 1 12) $ oneof
        [ IfStatementBody_Brace <$> arbitrary
        , IfStatementBody_Colon <$> arbitrary
        ]

  shrink x = case x of
    IfStatementBody_Brace z ->
      map IfStatementBody_Brace $ shrink z
    IfStatementBody_Colon z ->
      map IfStatementBody_Colon $ shrink z





newtype PropertyInitializer = PropertyInitializer
  { unPropertyInitializer ::
      ( Symbol Equal_
      , ConstantExpression
      )
  } deriving (Eq, Show)

instance Render PropertyInitializer where
  render = render . unPropertyInitializer

instance HasExtent PropertyInitializer where
  extentOf = extentOf . unPropertyInitializer

  shiftTo loc (PropertyInitializer z) =
    shiftToWrap PropertyInitializer loc z

instance Arbitrary PropertyInitializer where
  arbitrary = scale (frac 1 3) $
    PropertyInitializer <$> arbitrary

  shrink (PropertyInitializer x) =
    map PropertyInitializer $ shrink x





newtype NamespaceAliasingClause = NamespaceAliasingClause
  { unNamespaceAliasingClause ::
      ( Keyword As_
      , Name
      )
  } deriving (Eq, Show)

instance Render NamespaceAliasingClause where
  render = render . unNamespaceAliasingClause

instance HasExtent NamespaceAliasingClause where
  extentOf = extentOf . unNamespaceAliasingClause

  shiftTo loc (NamespaceAliasingClause x) =
    shiftToWrap NamespaceAliasingClause loc x

instance Arbitrary NamespaceAliasingClause where
  arbitrary = scale (frac 1 2) $
    NamespaceAliasingClause <$> arbitrary

  shrink (NamespaceAliasingClause x) =
    map NamespaceAliasingClause $ shrink x





newtype StaticVariableDeclaration = StaticVariableDeclaration
  { unStaticVariableDeclaration ::
      ( VariableName
      , Maybe FunctionStaticInitializer
      )
  } deriving (Eq, Show)

instance Render StaticVariableDeclaration where
  render = render . unStaticVariableDeclaration

instance HasExtent StaticVariableDeclaration where
  extentOf = extentOf . unStaticVariableDeclaration

  shiftTo loc (StaticVariableDeclaration x) =
    shiftToWrap StaticVariableDeclaration loc x

instance Arbitrary StaticVariableDeclaration where
  arbitrary = scale (frac 1 2) $
    StaticVariableDeclaration <$> arbitrary

  shrink (StaticVariableDeclaration x) =
    map StaticVariableDeclaration $ shrink x





newtype StaticVariableNameList = StaticVariableNameList
  { unStaticVariableNameList ::
      SeqSep (Symbol Comma_) StaticVariableDeclaration
  } deriving (Eq, Show)

instance Render StaticVariableNameList where
  render = render . unStaticVariableNameList

instance HasExtent StaticVariableNameList where
  extentOf = extentOf . unStaticVariableNameList

  shiftTo loc (StaticVariableNameList x) =
    shiftToWrap StaticVariableNameList loc x

instance Arbitrary StaticVariableNameList where
  arbitrary = StaticVariableNameList <$> arbitrary

  shrink (StaticVariableNameList x) =
    map StaticVariableNameList $ shrink x





newtype FunctionStaticDeclaration = FunctionStaticDeclaration
  { unFunctionStaticDeclaration ::
      ( Keyword Static_
      , StaticVariableNameList
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render FunctionStaticDeclaration where
  render = render . unFunctionStaticDeclaration

instance HasExtent FunctionStaticDeclaration where
  extentOf = extentOf . unFunctionStaticDeclaration

  shiftTo loc (FunctionStaticDeclaration x) =
    shiftToWrap FunctionStaticDeclaration loc x

instance Arbitrary FunctionStaticDeclaration where
  arbitrary = scale (frac 1 4) $
    FunctionStaticDeclaration <$> arbitrary

  shrink (FunctionStaticDeclaration x) =
    map FunctionStaticDeclaration $ shrink x





newtype VariableNameList = VariableNameList
  { unVariableNameList ::
      SeqSep (Symbol Comma_) SimpleVariable
  } deriving (Eq, Show)

instance Render VariableNameList where
  render = render . unVariableNameList

instance HasExtent VariableNameList where
  extentOf = extentOf . unVariableNameList

  shiftTo loc (VariableNameList x) =
    shiftToWrap VariableNameList loc x

instance Arbitrary VariableNameList where
  arbitrary = VariableNameList <$> arbitrary

  shrink (VariableNameList x) =
    map VariableNameList $ shrink x





newtype VariableList = VariableList
  { unVariableList ::
      SeqSep (Symbol Comma_) Variable
  } deriving (Eq, Show)

instance Render VariableList where
  render = render . unVariableList

instance HasExtent VariableList where
  extentOf = extentOf . unVariableList

  shiftTo loc (VariableList x) =
    shiftToWrap VariableList loc x

instance Arbitrary VariableList where
  arbitrary = scale (frac 1 4) $
    VariableList <$> arbitrary

  shrink (VariableList x) =
    map VariableList $ shrink x





newtype GlobalDeclaration = GlobalDeclaration
  { unGlobalDeclaration ::
      ( Keyword Global_
      , VariableNameList
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render GlobalDeclaration where
  render = render . unGlobalDeclaration

instance HasExtent GlobalDeclaration where
  extentOf = extentOf . unGlobalDeclaration

  shiftTo loc (GlobalDeclaration x) =
    shiftToWrap GlobalDeclaration loc x

instance Arbitrary GlobalDeclaration where
  arbitrary = scale (frac 1 4) $
    GlobalDeclaration <$> arbitrary

  shrink (GlobalDeclaration x) =
    map GlobalDeclaration $ shrink x





newtype EmptyIntrinsic = EmptyIntrinsic
  { unEmptyIntrinsic ::
      ( Keyword Empty_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      )
  } deriving (Eq, Show)

instance Render EmptyIntrinsic where
  render = render . unEmptyIntrinsic

instance HasExtent EmptyIntrinsic where
  extentOf = extentOf . unEmptyIntrinsic

  shiftTo loc (EmptyIntrinsic x) =
    shiftToWrap EmptyIntrinsic loc x

instance Arbitrary EmptyIntrinsic where
  arbitrary = scale (frac 1 5) $
    EmptyIntrinsic <$> arbitrary

  shrink (EmptyIntrinsic x) =
    map EmptyIntrinsic $ shrink x





newtype EvalIntrinsic = EvalIntrinsic
  { unEvalIntrinsic ::
      ( Keyword Eval_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      )
  } deriving (Eq, Show)

instance Render EvalIntrinsic where
  render = render . unEvalIntrinsic

instance HasExtent EvalIntrinsic where
  extentOf = extentOf . unEvalIntrinsic

  shiftTo loc (EvalIntrinsic x) =
    shiftToWrap EvalIntrinsic loc x

instance Arbitrary EvalIntrinsic where
  arbitrary = scale (frac 1 4) $
    EvalIntrinsic <$> arbitrary

  shrink (EvalIntrinsic x) =
    map EvalIntrinsic $ shrink x





newtype IssetIntrinsic = IssetIntrinsic
  { unIssetIntrinsic ::
      ( Keyword Isset_
      , Symbol OpenParen_
      , VariableList
      , Maybe (Symbol Comma_)
      , Symbol ClosedParen_
      )
  } deriving (Eq, Show)

instance Render IssetIntrinsic where
  render = render . unIssetIntrinsic

instance HasExtent IssetIntrinsic where
  extentOf = extentOf . unIssetIntrinsic

  shiftTo loc (IssetIntrinsic x) =
    shiftToWrap IssetIntrinsic loc x

instance Arbitrary IssetIntrinsic where
  arbitrary = scale (frac 1 5) $
    IssetIntrinsic <$> arbitrary

  shrink (IssetIntrinsic x) =
    map IssetIntrinsic $ shrink x





newtype ElementKey = ElementKey
  { unElementKey :: Expression
  } deriving (Eq, Show)

instance Render ElementKey where
  render = render . unElementKey

instance HasExtent ElementKey where
  extentOf = extentOf . unElementKey

  shiftTo loc (ElementKey x) =
    shiftToWrap ElementKey loc x

instance Arbitrary ElementKey where
  arbitrary = scale (frac 1 5) $
    ElementKey <$> arbitrary

  shrink (ElementKey x) =
    map ElementKey $ shrink x





newtype ElementValue = ElementValue
  { unElementValue :: Expression
  } deriving (Eq, Show)

instance Render ElementValue where
  render = render . unElementValue

instance HasExtent ElementValue where
  extentOf = extentOf . unElementValue

  shiftTo loc (ElementValue x) =
    shiftToWrap ElementValue loc x

instance Arbitrary ElementValue where
  arbitrary = ElementValue
    <$> (scale (`div` 2) arbitrary)

  shrink (ElementValue x) =
    map ElementValue $ shrink x





newtype VariadicUnpacking = VariadicUnpacking
  { unVariadicUnpacking ::
      ( Symbol Ellipsis_
      , Expression
      )
  } deriving (Eq, Show)

instance Render VariadicUnpacking where
  render = render . unVariadicUnpacking

instance HasExtent VariadicUnpacking where
  extentOf = extentOf . unVariadicUnpacking

  shiftTo loc (VariadicUnpacking x) =
    shiftToWrap VariadicUnpacking loc x

instance Arbitrary VariadicUnpacking where
  arbitrary = scale (frac 1 3) $
    VariadicUnpacking <$> arbitrary

  shrink (VariadicUnpacking x) =
    map VariadicUnpacking $ shrink x





newtype MemberAccessExpression = MemberAccessExpression
  { unMemberAccessExpression ::
      ( DereferencableExpression
      , Symbol MinusGreater_
      , MemberName
      )
  } deriving (Eq, Show)

instance Render MemberAccessExpression where
  render = render . unMemberAccessExpression

instance HasExtent MemberAccessExpression where
  extentOf = extentOf . unMemberAccessExpression

  shiftTo loc (MemberAccessExpression x) =
    shiftToWrap MemberAccessExpression loc x

instance Arbitrary MemberAccessExpression where
  arbitrary = scale (frac 1 3) $
    MemberAccessExpression <$> arbitrary

  shrink (MemberAccessExpression x) =
    map MemberAccessExpression $ shrink x





data MemberName
  = MemberName_Name
        Name
  | MemberName_Variable
        SimpleVariable
  | MemberName_Nest
      ( Symbol OpenBrace_
      , Expression
      , Symbol ClosedBrace_
      )
  deriving (Eq, Show)

instance Render MemberName where
  render x = case x of
    MemberName_Name     z -> render z
    MemberName_Variable z -> render z
    MemberName_Nest     z -> render z

instance HasExtent MemberName where
  extentOf x = case x of
    MemberName_Name     z -> extentOf z
    MemberName_Variable z -> extentOf z
    MemberName_Nest     z -> extentOf z

  shiftTo loc x = case x of
    MemberName_Name z ->
      shiftToWrap MemberName_Name loc z
    MemberName_Variable z ->
      shiftToWrap MemberName_Variable loc z
    MemberName_Nest z ->
      shiftToWrap MemberName_Nest loc z

instance Arbitrary MemberName where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ MemberName_Name     <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ MemberName_Name     <$> arbitrary
        , MemberName_Variable <$> arbitrary
        , MemberName_Nest     <$> arbitrary
        ]

  shrink x = case x of
    MemberName_Name z ->
      map MemberName_Name $ shrink z
    MemberName_Variable z ->
      map MemberName_Variable $ shrink z
    MemberName_Nest z ->
      map MemberName_Nest $ shrink z





data DereferencableExpression
  = DereferencableExpression_Variable
        Variable
  | DereferencableExpression_Expression
      ( Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      )
  | DereferencableExpression_Array
        ArrayCreationExpression
  | DereferencableExpression_String
        StringLiteral
  deriving (Eq, Show)

instance Render DereferencableExpression where
  render x = case x of
    DereferencableExpression_Variable   z -> render z
    DereferencableExpression_Expression z -> render z
    DereferencableExpression_Array      z -> render z
    DereferencableExpression_String     z -> render z

instance HasExtent DereferencableExpression where
  extentOf x = case x of
    DereferencableExpression_Variable   z -> extentOf z
    DereferencableExpression_Expression z -> extentOf z
    DereferencableExpression_Array      z -> extentOf z
    DereferencableExpression_String     z -> extentOf z

  shiftTo loc x = case x of
    DereferencableExpression_Variable z ->
      shiftToWrap DereferencableExpression_Variable loc z
    DereferencableExpression_Expression z ->
      shiftToWrap DereferencableExpression_Expression loc z
    DereferencableExpression_Array z ->
      shiftToWrap DereferencableExpression_Array loc z
    DereferencableExpression_String z ->
      shiftToWrap DereferencableExpression_String loc z

instance Arbitrary DereferencableExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 5) $ oneof
        [ DereferencableExpression_Variable   <$> arbitrary
        ]
      else scale (frac 1 6) $ oneof
        [ DereferencableExpression_Variable   <$> arbitrary
        , DereferencableExpression_Expression <$> arbitrary
        , DereferencableExpression_Array      <$> arbitrary
        , DereferencableExpression_String     <$> arbitrary
        ]

  shrink x = case x of
    DereferencableExpression_Variable z ->
      map DereferencableExpression_Variable $ shrink z
    DereferencableExpression_Expression z ->
      map DereferencableExpression_Expression $ shrink z
    DereferencableExpression_Array z ->
      map DereferencableExpression_Array $ shrink z
    DereferencableExpression_String z ->
      map DereferencableExpression_String $ shrink z





newtype PrefixDecrementExpression = PrefixDecrementExpression
  { unPrefixDecrementExpression ::
      ( Symbol DoubleMinus_
      , Variable
      )
  } deriving (Eq, Show)

instance Render PrefixDecrementExpression where
  render = render . unPrefixDecrementExpression

instance HasExtent PrefixDecrementExpression where
  extentOf = extentOf . unPrefixDecrementExpression

  shiftTo loc (PrefixDecrementExpression x) =
    shiftToWrap PrefixDecrementExpression loc x

instance Arbitrary PrefixDecrementExpression where
  arbitrary = scale (frac 1 4) $
    PrefixDecrementExpression <$> arbitrary

  shrink (PrefixDecrementExpression x) =
    map PrefixDecrementExpression $ shrink x





newtype PrefixIncrementExpression = PrefixIncrementExpression
  { unPrefixIncrementExpression ::
      ( Symbol DoublePlus_
      , Variable
      )
  } deriving (Eq, Show)

instance Render PrefixIncrementExpression where
  render = render . unPrefixIncrementExpression

instance HasExtent PrefixIncrementExpression where
  extentOf = extentOf . unPrefixIncrementExpression

  shiftTo loc (PrefixIncrementExpression x) =
    shiftToWrap PrefixIncrementExpression loc x

instance Arbitrary PrefixIncrementExpression where
  arbitrary = scale (frac 1 4) $
    PrefixIncrementExpression <$> arbitrary

  shrink (PrefixIncrementExpression x) =
    map PrefixIncrementExpression $ shrink x





newtype PostfixDecrementExpression = PostfixDecrementExpression
  { unPostfixDecrementExpression ::
      ( Variable
      , Symbol DoubleMinus_
      )
  } deriving (Eq, Show)

instance Render PostfixDecrementExpression where
  render = render . unPostfixDecrementExpression

instance HasExtent PostfixDecrementExpression where
  extentOf = extentOf . unPostfixDecrementExpression

  shiftTo loc (PostfixDecrementExpression x) =
    shiftToWrap PostfixDecrementExpression loc x

instance Arbitrary PostfixDecrementExpression where
  arbitrary = scale (frac 1 2) $
    PostfixDecrementExpression <$> arbitrary

  shrink (PostfixDecrementExpression x) =
    map PostfixDecrementExpression $ shrink x





newtype PostfixIncrementExpression = PostfixIncrementExpression
  { unPostfixIncrementExpression ::
      ( Variable
      , Symbol DoublePlus_
      )
  } deriving (Eq, Show)

instance Render PostfixIncrementExpression where
  render = render . unPostfixIncrementExpression

instance HasExtent PostfixIncrementExpression where
  extentOf = extentOf . unPostfixIncrementExpression

  shiftTo loc (PostfixIncrementExpression x) =
    shiftToWrap PostfixIncrementExpression loc x

instance Arbitrary PostfixIncrementExpression where
  arbitrary = scale (frac 1 2) $
    PostfixIncrementExpression <$> arbitrary

  shrink (PostfixIncrementExpression x) =
    map PostfixIncrementExpression $ shrink x





data ArgumentExpression
  = ArgumentExpression_Variadic   VariadicUnpacking
  | ArgumentExpression_Expression Expression
  deriving (Eq, Show)

instance Render ArgumentExpression where
  render x = case x of
    ArgumentExpression_Variadic   z -> render z
    ArgumentExpression_Expression z -> render z

instance HasExtent ArgumentExpression where
  extentOf x = case x of
    ArgumentExpression_Variadic   z -> extentOf z
    ArgumentExpression_Expression z -> extentOf z

  shiftTo loc x = case x of
    ArgumentExpression_Variadic z ->
      shiftToWrap ArgumentExpression_Variadic loc z
    ArgumentExpression_Expression z ->
      shiftToWrap ArgumentExpression_Expression loc z

instance Arbitrary ArgumentExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 6) $ oneof
        [ ArgumentExpression_Variadic   <$> arbitrary
        ]
      else scale (frac 1 5) $ oneof
        [ ArgumentExpression_Variadic   <$> arbitrary
        , ArgumentExpression_Expression <$> arbitrary
        ]

  shrink x = case x of
    ArgumentExpression_Variadic z ->
      map ArgumentExpression_Variadic $ shrink z
    ArgumentExpression_Expression z ->
      map ArgumentExpression_Expression $ shrink z





newtype ArgumentExpressionList = ArgumentExpressionList
  { unArgumentExpressionList ::
      SeqSep (Symbol Comma_) ArgumentExpression
  } deriving (Eq, Show)

instance Render ArgumentExpressionList where
  render = render . unArgumentExpressionList

instance HasExtent ArgumentExpressionList where
  extentOf = extentOf . unArgumentExpressionList

  shiftTo loc (ArgumentExpressionList x) =
    shiftToWrap ArgumentExpressionList loc x

instance Arbitrary ArgumentExpressionList where
  arbitrary = ArgumentExpressionList <$> arbitrary

  shrink (ArgumentExpressionList x) =
    map ArgumentExpressionList $ shrink x





newtype ErrorControlExpression = ErrorControlExpression
  { unErrorControlExpression ::
      ( Symbol At_
      , UnaryExpression
      )
  } deriving (Eq, Show)

instance Render ErrorControlExpression where
  render = render . unErrorControlExpression

instance HasExtent ErrorControlExpression where
  extentOf = extentOf . unErrorControlExpression

  shiftTo loc (ErrorControlExpression x) =
    shiftToWrap ErrorControlExpression loc x

instance Arbitrary ErrorControlExpression where
  arbitrary = scale (frac 1 2) $
    ErrorControlExpression <$> arbitrary

  shrink (ErrorControlExpression x) =
    map ErrorControlExpression $ shrink x





data CaseDefaultLabelTerminator
  = CaseDefaultLabelTerminator_Colon     (Symbol Colon_)
  | CaseDefaultLabelTerminator_Semicolon (Symbol Semicolon_)
  deriving (Eq, Show)

instance Render CaseDefaultLabelTerminator where
  render x = case x of
    CaseDefaultLabelTerminator_Colon     z -> render z
    CaseDefaultLabelTerminator_Semicolon z -> render z

instance HasExtent CaseDefaultLabelTerminator where
  extentOf x = case x of
    CaseDefaultLabelTerminator_Colon     z -> extentOf z
    CaseDefaultLabelTerminator_Semicolon z -> extentOf z

  shiftTo loc x = case x of
    CaseDefaultLabelTerminator_Colon z ->
      shiftToWrap CaseDefaultLabelTerminator_Colon loc z
    CaseDefaultLabelTerminator_Semicolon z ->
      shiftToWrap CaseDefaultLabelTerminator_Semicolon loc z

instance Arbitrary CaseDefaultLabelTerminator where
  arbitrary = oneof
    [ CaseDefaultLabelTerminator_Colon     <$> arbitrary
    , CaseDefaultLabelTerminator_Semicolon <$> arbitrary
    ]

  shrink x = case x of
    CaseDefaultLabelTerminator_Colon z ->
      map CaseDefaultLabelTerminator_Colon $ shrink z
    CaseDefaultLabelTerminator_Semicolon z ->
      map CaseDefaultLabelTerminator_Semicolon $ shrink z





newtype DefaultStatement = DefaultStatement
  { unDefaultStatement ::
      ( Keyword Default_
      , CaseDefaultLabelTerminator
      , Maybe StatementList
      )
  } deriving (Eq, Show)

instance Render DefaultStatement where
  render = render . unDefaultStatement

instance HasExtent DefaultStatement where
  extentOf = extentOf . unDefaultStatement

  shiftTo loc (DefaultStatement x) =
    shiftToWrap DefaultStatement loc x

instance Arbitrary DefaultStatement where
  arbitrary = scale (frac 1 4) $
    DefaultStatement <$> arbitrary

  shrink (DefaultStatement x) =
    map DefaultStatement $ shrink x





newtype CaseStatement = CaseStatement
  { unCaseStatement ::
      ( Keyword Case_
      , Expression
      , CaseDefaultLabelTerminator
      , Maybe StatementList
      )
  } deriving (Eq, Show)

instance Render CaseStatement where
  render = render . unCaseStatement

instance HasExtent CaseStatement where
  extentOf = extentOf . unCaseStatement

  shiftTo loc (CaseStatement x) =
    shiftToWrap CaseStatement loc x

instance Arbitrary CaseStatement where
  arbitrary = scale (frac 1 4) $
    CaseStatement <$> arbitrary

  shrink (CaseStatement x) =
    map CaseStatement $ shrink x





newtype DoStatement = DoStatement
  { unDoStatement ::
      ( Keyword Do_
      , Statement
      , Keyword While_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render DoStatement where
  render = render . unDoStatement

instance HasExtent DoStatement where
  extentOf = extentOf . unDoStatement

  shiftTo loc (DoStatement x) =
    shiftToWrap DoStatement loc x

instance Arbitrary DoStatement where
  arbitrary = scale (frac 1 8) $
    DoStatement <$> arbitrary

  shrink (DoStatement x) =
    map DoStatement $ shrink x





newtype ForExpressionGroup = ForExpressionGroup
  { unForExpressionGroup ::
      SeqSep (Symbol Comma_) Expression
  } deriving (Eq, Show)

instance Render ForExpressionGroup where
  render = render . unForExpressionGroup

instance HasExtent ForExpressionGroup where
  extentOf = extentOf . unForExpressionGroup

  shiftTo loc (ForExpressionGroup x) =
    shiftToWrap ForExpressionGroup loc x

instance Arbitrary ForExpressionGroup where
  arbitrary = ForExpressionGroup <$> arbitrary

  shrink (ForExpressionGroup x) =
    map ForExpressionGroup $ shrink x





newtype ForInitializer = ForInitializer
  { unForInitializer :: ForExpressionGroup
  } deriving (Eq, Show)

instance Render ForInitializer where
  render = render . unForInitializer

instance HasExtent ForInitializer where
  extentOf = extentOf . unForInitializer

  shiftTo loc (ForInitializer x) =
    shiftToWrap ForInitializer loc x

instance Arbitrary ForInitializer where
  arbitrary = scale (frac 1 2) $
    ForInitializer <$> arbitrary

  shrink (ForInitializer x) =
    map ForInitializer $ shrink x





newtype ForControl = ForControl
  { unForControl :: ForExpressionGroup
  } deriving (Eq, Show)

instance Render ForControl where
  render = render . unForControl

instance HasExtent ForControl where
  extentOf = extentOf . unForControl

  shiftTo loc (ForControl x) =
    shiftToWrap ForControl loc x

instance Arbitrary ForControl where
  arbitrary = scale (frac 1 2) $
    ForControl <$> arbitrary

  shrink (ForControl x) =
    map ForControl $ shrink x





newtype ForEndOfLoop = ForEndOfLoop
  { unForEndOfLoop :: ForExpressionGroup
  } deriving (Eq, Show)

instance Render ForEndOfLoop where
  render = render . unForEndOfLoop

instance HasExtent ForEndOfLoop where
  extentOf = extentOf . unForEndOfLoop

  shiftTo loc (ForEndOfLoop x) =
    shiftToWrap ForEndOfLoop loc x

instance Arbitrary ForEndOfLoop where
  arbitrary = scale (frac 1 2) $
    ForEndOfLoop <$> arbitrary

  shrink (ForEndOfLoop x) =
    map ForEndOfLoop $ shrink x





newtype ForeachCollectionName = ForeachCollectionName
  { unForeachCollectionName :: Expression
  } deriving (Eq, Show)

instance Render ForeachCollectionName where
  render = render . unForeachCollectionName

instance HasExtent ForeachCollectionName where
  extentOf = extentOf . unForeachCollectionName

  shiftTo loc (ForeachCollectionName x) =
    shiftToWrap ForeachCollectionName loc x

instance Arbitrary ForeachCollectionName where
  arbitrary = scale (frac 1 2) $
    ForeachCollectionName <$> arbitrary

  shrink (ForeachCollectionName x) =
    map ForeachCollectionName $ shrink x





newtype EchoStatement = EchoStatement
  { unEchoStatement ::
      ( Keyword Echo_
      , ExpressionList
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render EchoStatement where
  render = render . unEchoStatement

instance HasExtent EchoStatement where
  extentOf = extentOf . unEchoStatement

  shiftTo loc (EchoStatement x) =
    shiftToWrap EchoStatement loc x

instance Arbitrary EchoStatement where
  arbitrary = scale (frac 1 3) $
    EchoStatement <$> arbitrary

  shrink (EchoStatement x) =
    map EchoStatement $ shrink x





newtype ExpressionList = ExpressionList
  { unExpressionList ::
      SeqSep (Symbol Comma_) Expression
  } deriving (Eq, Show)

instance Render ExpressionList where
  render = render . unExpressionList

instance HasExtent ExpressionList where
  extentOf = extentOf . unExpressionList

  shiftTo loc (ExpressionList x) =
    shiftToWrap ExpressionList loc x

instance Arbitrary ExpressionList where
  arbitrary = ExpressionList <$> arbitrary

  shrink (ExpressionList x) =
    map ExpressionList $ shrink x





newtype NamespaceUseClause = NamespaceUseClause
  { unNamespaceUseClause ::
      ( QualifiedName
      , Maybe NamespaceAliasingClause
      )
  } deriving (Eq, Show)

instance Render NamespaceUseClause where
  render = render . unNamespaceUseClause

instance HasExtent NamespaceUseClause where
  extentOf = extentOf . unNamespaceUseClause

  shiftTo loc (NamespaceUseClause x) =
    shiftToWrap NamespaceUseClause loc x

instance Arbitrary NamespaceUseClause where
  arbitrary = NamespaceUseClause
    <$> (scale (`div` 2) arbitrary)

  shrink (NamespaceUseClause x) =
    map NamespaceUseClause $ shrink x





newtype NamespaceUseClauses = NamespaceUseClauses
  { unNamespaceUseClauses ::
      SeqSep (Symbol Comma_) NamespaceUseClause
  } deriving (Eq, Show)

instance Render NamespaceUseClauses where
  render = render . unNamespaceUseClauses

instance HasExtent NamespaceUseClauses where
  extentOf = extentOf . unNamespaceUseClauses

  shiftTo loc (NamespaceUseClauses x) =
    shiftToWrap NamespaceUseClauses loc x

instance Arbitrary NamespaceUseClauses where
  arbitrary = NamespaceUseClauses <$> arbitrary

  shrink (NamespaceUseClauses x) =
    map NamespaceUseClauses $ shrink x





newtype NamespaceUseGroupClause1 = NamespaceUseGroupClause1
  { unNamespaceUseGroupClause1 ::
      ( NamespaceName
      , Maybe NamespaceAliasingClause
      )
  } deriving (Eq, Show)

instance Render NamespaceUseGroupClause1 where
  render = render . unNamespaceUseGroupClause1

instance HasExtent NamespaceUseGroupClause1 where
  extentOf = extentOf . unNamespaceUseGroupClause1

  shiftTo loc (NamespaceUseGroupClause1 x) =
    shiftToWrap NamespaceUseGroupClause1 loc x

instance Arbitrary NamespaceUseGroupClause1 where
  arbitrary = NamespaceUseGroupClause1
    <$> (scale (`div` 2) arbitrary)

  shrink (NamespaceUseGroupClause1 x) =
    map NamespaceUseGroupClause1 $ shrink x





newtype NamespaceUseGroupClauses1 = NamespaceUseGroupClauses1
  { unNamespaceUseGroupClauses1 ::
      SeqSep (Symbol Comma_) NamespaceUseGroupClause1
  } deriving (Eq, Show)

instance Render NamespaceUseGroupClauses1 where
  render = render . unNamespaceUseGroupClauses1

instance HasExtent NamespaceUseGroupClauses1 where
  extentOf = extentOf . unNamespaceUseGroupClauses1

  shiftTo loc (NamespaceUseGroupClauses1 x) =
    shiftToWrap NamespaceUseGroupClauses1 loc x

instance Arbitrary NamespaceUseGroupClauses1 where
  arbitrary = NamespaceUseGroupClauses1 <$> arbitrary

  shrink (NamespaceUseGroupClauses1 x) =
    map NamespaceUseGroupClauses1 $ shrink x





newtype ConstElement = ConstElement
  { unConstElement ::
      ( Name
      , Symbol Equal_
      , ConstantExpression
      )
  } deriving (Eq, Show)

instance Render ConstElement where
  render = render . unConstElement

instance HasExtent ConstElement where
  extentOf = extentOf . unConstElement

  shiftTo loc (ConstElement x) =
    shiftToWrap ConstElement loc x

instance Arbitrary ConstElement where
  arbitrary = scale (frac 1 2) $
    ConstElement <$> arbitrary

  shrink (ConstElement x) =
    map ConstElement $ shrink x





newtype ConstElements = ConstElements
  { unConstElements ::
      SeqSep (Symbol Comma_) ConstElement
  } deriving (Eq, Show)

instance Render ConstElements where
  render = render . unConstElements

instance HasExtent ConstElements where
  extentOf = extentOf . unConstElements

  shiftTo loc (ConstElements x) =
    shiftToWrap ConstElements loc x

instance Arbitrary ConstElements where
  arbitrary = ConstElements <$> arbitrary

  shrink (ConstElements x) =
    map ConstElements $ shrink x





newtype ConstDeclaration = ConstDeclaration
  { unConstDeclaration ::
      ( Keyword Const_
      , ConstElements
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render ConstDeclaration where
  render = render . unConstDeclaration

instance HasExtent ConstDeclaration where
  extentOf = extentOf . unConstDeclaration

  shiftTo loc (ConstDeclaration x) =
    shiftToWrap ConstDeclaration loc x

instance Arbitrary ConstDeclaration where
  arbitrary = scale (frac 1 3) $
    ConstDeclaration <$> arbitrary

  shrink (ConstDeclaration x) =
    map ConstDeclaration $ shrink x





newtype PropertyElement = PropertyElement
  { unPropertyElement ::
      ( VariableName
      , Maybe PropertyInitializer
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render PropertyElement where
  render = render . unPropertyElement

instance HasExtent PropertyElement where
  extentOf = extentOf . unPropertyElement

  shiftTo loc (PropertyElement x) =
    shiftToWrap PropertyElement loc x

instance Arbitrary PropertyElement where
  arbitrary = scale (frac 1 3) $
    PropertyElement <$> arbitrary

  shrink (PropertyElement x) =
    map PropertyElement $ shrink x





newtype PropertyElements = PropertyElements
  { unPropertyElements :: Seq PropertyElement
  } deriving (Eq, Show)

instance Render PropertyElements where
  render = render . unPropertyElements

instance HasExtent PropertyElements where
  extentOf = extentOf . unPropertyElements

  shiftTo loc (PropertyElements x) =
    shiftToWrap PropertyElements loc x

instance Arbitrary PropertyElements where
  arbitrary = scale (frac 1 2) $
    PropertyElements <$> arbitrary

  shrink (PropertyElements x) =
    map PropertyElements $ shrink x





newtype ClassBaseClause = ClassBaseClause
  { unClassBaseClause ::
      ( Keyword Extends_
      , QualifiedName
      )
  } deriving (Eq, Show)

instance Render ClassBaseClause where
  render = render . unClassBaseClause

instance HasExtent ClassBaseClause where
  extentOf = extentOf . unClassBaseClause

  shiftTo loc (ClassBaseClause x) =
    shiftToWrap ClassBaseClause loc x

instance Arbitrary ClassBaseClause where
  arbitrary = scale (frac 1 2) $
    ClassBaseClause <$> arbitrary

  shrink (ClassBaseClause x) =
    map ClassBaseClause $ shrink x





newtype CatchNameList = CatchNameList
  { unCatchNameList ::
      SeqSep (Symbol Pipe_) QualifiedName
  } deriving (Eq, Show)

instance Render CatchNameList where
  render = render . unCatchNameList

instance HasExtent CatchNameList where
  extentOf = extentOf . unCatchNameList

  shiftTo loc (CatchNameList x) =
    shiftToWrap CatchNameList loc x

instance Arbitrary CatchNameList where
  arbitrary = CatchNameList <$> arbitrary

  shrink (CatchNameList x) =
    map CatchNameList $ shrink x





newtype CatchClause = CatchClause
  { unCatchClause ::
      ( Keyword Catch_
      , Symbol OpenParen_
      , CatchNameList
      , VariableName
      , Symbol ClosedParen_
      , CompoundStatement
      )
  } deriving (Eq, Show)

instance Render CatchClause where
  render = render . unCatchClause

instance HasExtent CatchClause where
  extentOf = extentOf . unCatchClause

  shiftTo loc (CatchClause x) =
    shiftToWrap CatchClause loc x

instance Arbitrary CatchClause where
  arbitrary = scale (frac 1 6) $
    CatchClause <$> arbitrary

  shrink (CatchClause x) =
    map CatchClause $ shrink x





newtype CatchClauses = CatchClauses
  { unCatchClauses :: Seq CatchClause
  } deriving (Eq, Show)

instance Render CatchClauses where
  render = render . unCatchClauses

instance HasExtent CatchClauses where
  extentOf = extentOf . unCatchClauses

  shiftTo loc (CatchClauses x) =
    shiftToWrap CatchClauses loc x

instance Arbitrary CatchClauses where
  arbitrary = CatchClauses <$> arbitrary

  shrink (CatchClauses x) =
    map CatchClauses $ shrink x





newtype TraitNameList = TraitNameList
  { unTraitNameList ::
      SeqSep (Symbol Comma_) QualifiedName
  } deriving (Eq, Show)

instance Render TraitNameList where
  render = render . unTraitNameList

instance HasExtent TraitNameList where
  extentOf = extentOf . unTraitNameList

  shiftTo loc (TraitNameList x) =
    shiftToWrap TraitNameList loc x

instance Arbitrary TraitNameList where
  arbitrary = scale (frac 1 3) $
    TraitNameList <$> arbitrary

  shrink (TraitNameList x) =
    map TraitNameList $ shrink x





newtype TraitSelectInsteadofClause = TraitSelectInsteadofClause
  { unTraitSelectInsteadofClause ::
      ( QualifiedName
      , Symbol DoubleColon_
      , Name
      , Keyword Insteadof_
      , TraitNameList
      )
  } deriving (Eq, Show)

instance Render TraitSelectInsteadofClause where
  render = render . unTraitSelectInsteadofClause

instance HasExtent TraitSelectInsteadofClause where
  extentOf = extentOf . unTraitSelectInsteadofClause

  shiftTo loc (TraitSelectInsteadofClause x) =
    shiftToWrap TraitSelectInsteadofClause loc x

instance Arbitrary TraitSelectInsteadofClause where
  arbitrary = scale (frac 1 5) $
    TraitSelectInsteadofClause <$> arbitrary

  shrink (TraitSelectInsteadofClause x) =
    map TraitSelectInsteadofClause $ shrink x





data VisibilityModifier
  = VisibilityModifier_Public    (Keyword Public_)
  | VisibilityModifier_Protected (Keyword Protected_)
  | VisibilityModifier_Private   (Keyword Private_)
  deriving (Eq, Show)

instance Render VisibilityModifier where
  render x = case x of
    VisibilityModifier_Public    z -> render z
    VisibilityModifier_Protected z -> render z
    VisibilityModifier_Private   z -> render z

instance HasExtent VisibilityModifier where
  extentOf x = case x of
    VisibilityModifier_Public    z -> extentOf z
    VisibilityModifier_Protected z -> extentOf z
    VisibilityModifier_Private   z -> extentOf z

  shiftTo loc x = case x of
    VisibilityModifier_Public z ->
      shiftToWrap VisibilityModifier_Public loc z
    VisibilityModifier_Protected z ->
      shiftToWrap VisibilityModifier_Protected loc z
    VisibilityModifier_Private z ->
      shiftToWrap VisibilityModifier_Private loc z

instance Arbitrary VisibilityModifier where
  arbitrary = oneof
    [ VisibilityModifier_Public    <$> arbitrary
    , VisibilityModifier_Protected <$> arbitrary
    , VisibilityModifier_Private   <$> arbitrary
    ]

  shrink x = case x of
    VisibilityModifier_Public z ->
      map VisibilityModifier_Public $ shrink z
    VisibilityModifier_Protected z ->
      map VisibilityModifier_Protected $ shrink z
    VisibilityModifier_Private z ->
      map VisibilityModifier_Private $ shrink z





newtype StaticModifier = StaticModifier
  { unStaticModifier :: Keyword Static_
  } deriving (Eq, Show)

instance Render StaticModifier where
  render = render . unStaticModifier

instance HasExtent StaticModifier where
  extentOf = extentOf . unStaticModifier

  shiftTo loc (StaticModifier x) =
    shiftToWrap StaticModifier loc x

instance Arbitrary StaticModifier where
  arbitrary = StaticModifier <$> arbitrary

  shrink (StaticModifier x) =
    map StaticModifier $ shrink x





data PropertyModifier
  = PropertyModifier_Var
        (Keyword Var_)
  | PropertyModifier_Visibility
      ( VisibilityModifier
      , Maybe StaticModifier
      )
  | PropertyModifier_Static
      ( StaticModifier
      , Maybe VisibilityModifier
      )
  deriving (Eq, Show)

instance Render PropertyModifier where
  render x = case x of
    PropertyModifier_Var        z -> render z
    PropertyModifier_Visibility z -> render z
    PropertyModifier_Static     z -> render z

instance HasExtent PropertyModifier where
  extentOf x = case x of
    PropertyModifier_Var        z -> extentOf z
    PropertyModifier_Visibility z -> extentOf z
    PropertyModifier_Static     z -> extentOf z

  shiftTo loc x = case x of
    PropertyModifier_Var z ->
      shiftToWrap PropertyModifier_Var loc z
    PropertyModifier_Visibility z ->
      shiftToWrap PropertyModifier_Visibility loc z
    PropertyModifier_Static z ->
      shiftToWrap PropertyModifier_Static loc z

instance Arbitrary PropertyModifier where
  arbitrary = oneof
    [ PropertyModifier_Var        <$> arbitrary
    , PropertyModifier_Visibility <$> arbitrary
    , PropertyModifier_Static     <$> arbitrary
    ]

  shrink x = case x of
    PropertyModifier_Var z ->
      map PropertyModifier_Var $ shrink z
    PropertyModifier_Visibility z ->
      map PropertyModifier_Visibility $ shrink z
    PropertyModifier_Static z ->
      map PropertyModifier_Static $ shrink z





newtype PropertyDeclaration = PropertyDeclaration
  { unPropertyDeclaration ::
      ( PropertyModifier
      , PropertyElements
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render PropertyDeclaration where
  render = render . unPropertyDeclaration

instance HasExtent PropertyDeclaration where
  extentOf = extentOf . unPropertyDeclaration

  shiftTo loc (PropertyDeclaration x) =
    shiftToWrap PropertyDeclaration loc x

instance Arbitrary PropertyDeclaration where
  arbitrary = scale (frac 1 3) $
    PropertyDeclaration <$> arbitrary

  shrink (PropertyDeclaration x) =
    map PropertyDeclaration $ shrink x





newtype ClassConstDeclaration = ClassConstDeclaration
  { unClassConstDeclaration ::
      ( Maybe VisibilityModifier
      , Keyword Const_
      , ConstElements
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render ClassConstDeclaration where
  render = render . unClassConstDeclaration

instance HasExtent ClassConstDeclaration where
  extentOf = extentOf . unClassConstDeclaration

  shiftTo loc (ClassConstDeclaration x) =
    shiftToWrap ClassConstDeclaration loc x

instance Arbitrary ClassConstDeclaration where
  arbitrary = scale (frac 1 4) $
    ClassConstDeclaration <$> arbitrary

  shrink (ClassConstDeclaration x) =
    map ClassConstDeclaration $ shrink x





data MethodModifier
  = MethodModifier_Visibility VisibilityModifier
  | MethodModifier_Static     StaticModifier
  | MethodModifier_Class      ClassModifier
  deriving (Eq, Show)

instance Render MethodModifier where
  render x = case x of
    MethodModifier_Visibility z -> render z
    MethodModifier_Static     z -> render z
    MethodModifier_Class      z -> render z

instance HasExtent MethodModifier where
  extentOf x = case x of
    MethodModifier_Visibility z -> extentOf z
    MethodModifier_Static     z -> extentOf z
    MethodModifier_Class      z -> extentOf z

  shiftTo loc x = case x of
    MethodModifier_Visibility z ->
      shiftToWrap MethodModifier_Visibility loc z
    MethodModifier_Static z ->
      shiftToWrap MethodModifier_Static loc z
    MethodModifier_Class z ->
      shiftToWrap MethodModifier_Class loc z

instance Arbitrary MethodModifier where
  arbitrary = oneof
    [ MethodModifier_Visibility <$> arbitrary
    , MethodModifier_Static     <$> arbitrary
    , MethodModifier_Class      <$> arbitrary
    ]

  shrink x = case x of
    MethodModifier_Visibility z ->
      map MethodModifier_Visibility $ shrink z
    MethodModifier_Static z ->
      map MethodModifier_Static $ shrink z
    MethodModifier_Class z ->
      map MethodModifier_Class $ shrink z





newtype MethodModifiers = MethodModifiers
  { unMethodModifiers :: Seq MethodModifier
  } deriving (Eq, Show)

instance Render MethodModifiers where
  render = render . unMethodModifiers

instance HasExtent MethodModifiers where
  extentOf = extentOf . unMethodModifiers

  shiftTo loc (MethodModifiers x) =
    shiftToWrap MethodModifiers loc x

instance Arbitrary MethodModifiers where
  arbitrary = scale (frac 1 9) $
    MethodModifiers <$> arbitrary

  shrink (MethodModifiers x) =
    map MethodModifiers $ shrink x





data TraitAliasAsClause
  = TraitAliasAsClause_Visibility
      ( Name
      , Keyword As_
      , Maybe VisibilityModifier
      , Name
      )
  | TraitAliasAsClause_Name
      ( Name
      , Keyword As_
      , VisibilityModifier
      , Maybe Name
      )
  deriving (Eq, Show)

instance Render TraitAliasAsClause where
  render x = case x of
    TraitAliasAsClause_Visibility z -> render z
    TraitAliasAsClause_Name       z -> render z

instance HasExtent TraitAliasAsClause where
  extentOf x = case x of
    TraitAliasAsClause_Visibility z -> extentOf z
    TraitAliasAsClause_Name       z -> extentOf z

  shiftTo loc x = case x of
    TraitAliasAsClause_Visibility z ->
      shiftToWrap TraitAliasAsClause_Visibility loc z
    TraitAliasAsClause_Name z ->
      shiftToWrap TraitAliasAsClause_Name loc z

instance Arbitrary TraitAliasAsClause where
  arbitrary = scale (frac 1 4) $ oneof
    [ TraitAliasAsClause_Visibility <$> arbitrary
    , TraitAliasAsClause_Name       <$> arbitrary
    ]

  shrink x = case x of
    TraitAliasAsClause_Visibility z ->
      map TraitAliasAsClause_Visibility $ shrink z
    TraitAliasAsClause_Name z ->
      map TraitAliasAsClause_Name $ shrink z





data NamespaceFunctionOrConst
  = NamespaceFunctionOrConst_Function (Keyword Function_)
  | NamespaceFunctionOrConst_Const    (Keyword Const_)
  deriving (Eq, Show)

instance Render NamespaceFunctionOrConst where
  render x = case x of
    NamespaceFunctionOrConst_Function z -> render z
    NamespaceFunctionOrConst_Const    z -> render z

instance HasExtent NamespaceFunctionOrConst where
  extentOf x = case x of
    NamespaceFunctionOrConst_Function z -> extentOf z
    NamespaceFunctionOrConst_Const    z -> extentOf z

  shiftTo loc x = case x of
    NamespaceFunctionOrConst_Function z ->
      shiftToWrap NamespaceFunctionOrConst_Function loc z
    NamespaceFunctionOrConst_Const z ->
      shiftToWrap NamespaceFunctionOrConst_Const loc z

instance Arbitrary NamespaceFunctionOrConst where
  arbitrary = oneof
    [ NamespaceFunctionOrConst_Function <$> arbitrary
    , NamespaceFunctionOrConst_Const    <$> arbitrary
    ]

  shrink x = case x of
    NamespaceFunctionOrConst_Function z ->
      map NamespaceFunctionOrConst_Function $ shrink z
    NamespaceFunctionOrConst_Const z ->
      map NamespaceFunctionOrConst_Const $ shrink z





newtype NamespaceUseGroupClause2 = NamespaceUseGroupClause2
  { unNamespaceUseGroupClause2 ::
      ( Maybe NamespaceFunctionOrConst
      , NamespaceName
      , Maybe NamespaceAliasingClause
      )
  } deriving (Eq, Show)

instance Render NamespaceUseGroupClause2 where
  render = render . unNamespaceUseGroupClause2

instance HasExtent NamespaceUseGroupClause2 where
  extentOf = extentOf . unNamespaceUseGroupClause2

  shiftTo loc (NamespaceUseGroupClause2 x) =
    shiftToWrap NamespaceUseGroupClause2 loc x

instance Arbitrary NamespaceUseGroupClause2 where
  arbitrary = scale (frac 1 3) $
    NamespaceUseGroupClause2 <$> arbitrary

  shrink (NamespaceUseGroupClause2 x) =
    map NamespaceUseGroupClause2 $ shrink x





newtype NamespaceUseGroupClauses2 = NamespaceUseGroupClauses2
  { unNamespaceUseGroupClauses2 ::
      SeqSep (Symbol Comma_) NamespaceUseGroupClause2
  } deriving (Eq, Show)

instance Render NamespaceUseGroupClauses2 where
  render = render . unNamespaceUseGroupClauses2

instance HasExtent NamespaceUseGroupClauses2 where
  extentOf = extentOf . unNamespaceUseGroupClauses2

  shiftTo loc (NamespaceUseGroupClauses2 x) =
    shiftToWrap NamespaceUseGroupClauses2 loc x

instance Arbitrary NamespaceUseGroupClauses2 where
  arbitrary = NamespaceUseGroupClauses2 <$> arbitrary

  shrink (NamespaceUseGroupClauses2 x) =
    map NamespaceUseGroupClauses2 $ shrink x





data TraitSelectAndAliasClause
  = TraitSelectAndAliasClause_Select
      ( TraitSelectInsteadofClause
      , Symbol Semicolon_
      )
  | TraitSelectAndAliasClause_Alias
      ( TraitAliasAsClause
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render TraitSelectAndAliasClause where
  render x = case x of
    TraitSelectAndAliasClause_Select z -> render z
    TraitSelectAndAliasClause_Alias  z -> render z

instance HasExtent TraitSelectAndAliasClause where
  extentOf x = case x of
    TraitSelectAndAliasClause_Select z -> extentOf z
    TraitSelectAndAliasClause_Alias  z -> extentOf z

  shiftTo loc x = case x of
    TraitSelectAndAliasClause_Select z ->
      shiftToWrap TraitSelectAndAliasClause_Select loc z
    TraitSelectAndAliasClause_Alias z ->
      shiftToWrap TraitSelectAndAliasClause_Alias loc z

instance Arbitrary TraitSelectAndAliasClause where
  arbitrary = scale (frac 1 2) $ oneof
    [ TraitSelectAndAliasClause_Select <$> arbitrary
    , TraitSelectAndAliasClause_Alias  <$> arbitrary
    ]

  shrink x = case x of
    TraitSelectAndAliasClause_Select z ->
      map TraitSelectAndAliasClause_Select $ shrink z
    TraitSelectAndAliasClause_Alias z ->
      map TraitSelectAndAliasClause_Alias $ shrink z





newtype TraitSelectAndAliasClauses = TraitSelectAndAliasClauses
  { unTraitSelectAndAliasClauses :: Seq TraitSelectAndAliasClause
  } deriving (Eq, Show)

instance Render TraitSelectAndAliasClauses where
  render = render . unTraitSelectAndAliasClauses

instance HasExtent TraitSelectAndAliasClauses where
  extentOf = extentOf . unTraitSelectAndAliasClauses

  shiftTo loc (TraitSelectAndAliasClauses x) =
    shiftToWrap TraitSelectAndAliasClauses loc x

instance Arbitrary TraitSelectAndAliasClauses where
  arbitrary = TraitSelectAndAliasClauses <$> arbitrary

  shrink (TraitSelectAndAliasClauses x) =
    map TraitSelectAndAliasClauses $ shrink x





data BaseTypeDeclaration
  = BaseTypeDeclaration_Array
      ( Keyword Array_ )
  | BaseTypeDeclaration_Callable
      ( Keyword Callable_ )
  | BaseTypeDeclaration_Iterable
      ( Keyword Iterable_ )
  | BaseTypeDeclaration_Scalar
        ScalarType
  | BaseTypeDeclaration_Name
        QualifiedName
  deriving (Eq, Show)

instance Render BaseTypeDeclaration where
  render x = case x of
    BaseTypeDeclaration_Array    z -> render z
    BaseTypeDeclaration_Callable z -> render z
    BaseTypeDeclaration_Iterable z -> render z
    BaseTypeDeclaration_Scalar   z -> render z
    BaseTypeDeclaration_Name     z -> render z

instance HasExtent BaseTypeDeclaration where
  extentOf x = case x of
    BaseTypeDeclaration_Array    z -> extentOf z
    BaseTypeDeclaration_Callable z -> extentOf z
    BaseTypeDeclaration_Iterable z -> extentOf z
    BaseTypeDeclaration_Scalar   z -> extentOf z
    BaseTypeDeclaration_Name     z -> extentOf z

  shiftTo loc x = case x of
    BaseTypeDeclaration_Array z ->
      shiftToWrap BaseTypeDeclaration_Array loc z
    BaseTypeDeclaration_Callable z ->
      shiftToWrap BaseTypeDeclaration_Callable loc z
    BaseTypeDeclaration_Iterable z ->
      shiftToWrap BaseTypeDeclaration_Iterable loc z
    BaseTypeDeclaration_Scalar z ->
      shiftToWrap BaseTypeDeclaration_Scalar loc z
    BaseTypeDeclaration_Name z ->
      shiftToWrap BaseTypeDeclaration_Name loc z

instance Arbitrary BaseTypeDeclaration where
  arbitrary = scale (frac 1 2) $ oneof
    [ BaseTypeDeclaration_Array    <$> arbitrary
    , BaseTypeDeclaration_Callable <$> arbitrary
    , BaseTypeDeclaration_Iterable <$> arbitrary
    , BaseTypeDeclaration_Scalar   <$> arbitrary
    , BaseTypeDeclaration_Name     <$> arbitrary
    ]

  shrink x = case x of
    BaseTypeDeclaration_Array z ->
      map BaseTypeDeclaration_Array $ shrink z
    BaseTypeDeclaration_Callable z ->
      map BaseTypeDeclaration_Callable $ shrink z
    BaseTypeDeclaration_Iterable z ->
      map BaseTypeDeclaration_Iterable $ shrink z
    BaseTypeDeclaration_Scalar z ->
      map BaseTypeDeclaration_Scalar $ shrink z
    BaseTypeDeclaration_Name z ->
      map BaseTypeDeclaration_Name $ shrink z





newtype TypeDeclaration = TypeDeclaration
  { unTypeDeclaration ::
      ( Maybe (Symbol Question_)
      , BaseTypeDeclaration
      )
  } deriving (Eq, Show)

instance Render TypeDeclaration where
  render = render . unTypeDeclaration

instance HasExtent TypeDeclaration where
  extentOf = extentOf . unTypeDeclaration

  shiftTo loc (TypeDeclaration x) =
    shiftToWrap TypeDeclaration loc x

instance Arbitrary TypeDeclaration where
  arbitrary = TypeDeclaration <$> arbitrary

  shrink (TypeDeclaration x) =
    map TypeDeclaration $ shrink x





newtype VariadicParameter = VariadicParameter
  { unVariadicParameter ::
      ( Maybe TypeDeclaration
      , Maybe (Symbol Amp_)
      , Symbol Ellipsis_
      , VariableName
      )
  } deriving (Eq, Show)

instance Render VariadicParameter where
  render = render . unVariadicParameter

instance HasExtent VariadicParameter where
  extentOf = extentOf . unVariadicParameter

  shiftTo loc (VariadicParameter x) =
    shiftToWrap VariadicParameter loc x

instance Arbitrary VariadicParameter where
  arbitrary = scale (frac 1 4) $
    VariadicParameter <$> arbitrary

  shrink (VariadicParameter x) =
    map VariadicParameter $ shrink x





data ReturnType
  = ReturnType_Type
      ( Symbol Colon_
      , TypeDeclaration
      )
  | ReturnType_Void
      ( Symbol Colon_
      , Keyword Void_
      )
  deriving (Eq, Show)

instance Render ReturnType where
  render x = case x of
    ReturnType_Type z -> render z
    ReturnType_Void z -> render z

instance HasExtent ReturnType where
  extentOf x = case x of
    ReturnType_Type z -> extentOf z
    ReturnType_Void z -> extentOf z

  shiftTo loc x = case x of
    ReturnType_Type z ->
      shiftToWrap ReturnType_Type loc z
    ReturnType_Void z ->
      shiftToWrap ReturnType_Void loc z

instance Arbitrary ReturnType where
  arbitrary = oneof
    [ ReturnType_Type <$> arbitrary
    , ReturnType_Void <$> arbitrary
    ]

  shrink x = case x of
    ReturnType_Type z ->
      map ReturnType_Type $ shrink z
    ReturnType_Void z ->
      map ReturnType_Void $ shrink z





newtype ParameterDeclaration = ParameterDeclaration
  { unParameterDeclaration ::
      ( Maybe TypeDeclaration
      , Maybe (Symbol Amp_)
      , VariableName
      , Maybe DefaultArgumentSpecifier
      )
  } deriving (Eq, Show)

instance Render ParameterDeclaration where
  render = render . unParameterDeclaration

instance HasExtent ParameterDeclaration where
  extentOf = extentOf . unParameterDeclaration

  shiftTo loc (ParameterDeclaration x) =
    shiftToWrap ParameterDeclaration loc x

instance Arbitrary ParameterDeclaration where
  arbitrary = scale (frac 1 5) $
    ParameterDeclaration <$> arbitrary

  shrink (ParameterDeclaration x) =
    map ParameterDeclaration $ shrink x





data ExitIntrinsic
  = ExitIntrinsic_Exit
        (Keyword Exit_)
  | ExitIntrinsic_ExitArg
      ( Keyword Exit_
      , Symbol OpenParen_
      , Maybe Expression
      , Symbol ClosedParen_
      )
  | ExitIntrinsic_Die
        (Keyword Die_)
  | ExitIntrinsic_DieArg
      ( Keyword Die_
      , Symbol OpenParen_
      , Maybe Expression
      , Symbol ClosedParen_
      )
  deriving (Eq, Show)

instance Render ExitIntrinsic where
  render x = case x of
    ExitIntrinsic_Exit    z -> render z
    ExitIntrinsic_ExitArg z -> render z
    ExitIntrinsic_Die     z -> render z
    ExitIntrinsic_DieArg  z -> render z

instance HasExtent ExitIntrinsic where
  extentOf x = case x of
    ExitIntrinsic_Exit    z -> extentOf z
    ExitIntrinsic_ExitArg z -> extentOf z
    ExitIntrinsic_Die     z -> extentOf z
    ExitIntrinsic_DieArg  z -> extentOf z

  shiftTo loc x = case x of
    ExitIntrinsic_Exit z ->
      shiftToWrap ExitIntrinsic_Exit loc z
    ExitIntrinsic_ExitArg z ->
      shiftToWrap ExitIntrinsic_ExitArg loc z
    ExitIntrinsic_Die z ->
      shiftToWrap ExitIntrinsic_Die loc z
    ExitIntrinsic_DieArg z ->
      shiftToWrap ExitIntrinsic_DieArg loc z

instance Arbitrary ExitIntrinsic where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ExitIntrinsic_Exit    <$> arbitrary
        , ExitIntrinsic_Die     <$> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ ExitIntrinsic_Exit    <$> arbitrary
        , ExitIntrinsic_ExitArg <$> arbitrary
        , ExitIntrinsic_Die     <$> arbitrary
        , ExitIntrinsic_DieArg  <$> arbitrary
        ]

  shrink x = case x of
    ExitIntrinsic_Exit z ->
      map ExitIntrinsic_Exit $ shrink z
    ExitIntrinsic_ExitArg z ->
      map ExitIntrinsic_ExitArg $ shrink z
    ExitIntrinsic_Die z ->
      map ExitIntrinsic_Die $ shrink z
    ExitIntrinsic_DieArg z ->
      map ExitIntrinsic_DieArg $ shrink z





data Intrinsic
  = Intrinsic_Empty EmptyIntrinsic
  | Intrinsic_Eval  EvalIntrinsic
  | Intrinsic_Exit  ExitIntrinsic
  | Intrinsic_Isset IssetIntrinsic
  deriving (Eq, Show)

instance Render Intrinsic where
  render x = case x of
    Intrinsic_Empty z -> render z
    Intrinsic_Eval  z -> render z
    Intrinsic_Exit  z -> render z
    Intrinsic_Isset z -> render z

instance HasExtent Intrinsic where
  extentOf x = case x of
    Intrinsic_Empty z -> extentOf z
    Intrinsic_Eval  z -> extentOf z
    Intrinsic_Exit  z -> extentOf z
    Intrinsic_Isset z -> extentOf z

  shiftTo loc x = case x of
    Intrinsic_Empty z ->
      shiftToWrap Intrinsic_Empty loc z
    Intrinsic_Eval z ->
      shiftToWrap Intrinsic_Eval loc z
    Intrinsic_Exit z ->
      shiftToWrap Intrinsic_Exit loc z
    Intrinsic_Isset z ->
      shiftToWrap Intrinsic_Isset loc z

instance Arbitrary Intrinsic where
  arbitrary = scale (frac 1 2) $ oneof
    [ Intrinsic_Empty <$> arbitrary
    , Intrinsic_Eval  <$> arbitrary
    , Intrinsic_Exit  <$> arbitrary
    , Intrinsic_Isset <$> arbitrary
    ]

  shrink x = case x of
    Intrinsic_Empty z ->
      map Intrinsic_Empty $ shrink z
    Intrinsic_Eval z ->
      map Intrinsic_Eval $ shrink z
    Intrinsic_Exit z ->
      map Intrinsic_Exit $ shrink z
    Intrinsic_Isset z ->
      map Intrinsic_Isset $ shrink z





data RelativeScope
  = RelativeScope_Self   (Keyword Self_)
  | RelativeScope_Parent (Keyword Parent_)
  | RelativeScope_Static (Keyword Static_)
  deriving (Eq, Show)

instance Render RelativeScope where
  render x = case x of
    RelativeScope_Self   z -> render z
    RelativeScope_Parent z -> render z
    RelativeScope_Static z -> render z

instance HasExtent RelativeScope where
  extentOf x = case x of
    RelativeScope_Self   z -> extentOf z
    RelativeScope_Parent z -> extentOf z
    RelativeScope_Static z -> extentOf z

  shiftTo loc x = case x of
    RelativeScope_Self z ->
      shiftToWrap RelativeScope_Self loc z
    RelativeScope_Parent z ->
      shiftToWrap RelativeScope_Parent loc z
    RelativeScope_Static z ->
      shiftToWrap RelativeScope_Static loc z

instance Arbitrary RelativeScope where
  arbitrary = oneof
    [ RelativeScope_Self   <$> arbitrary
    , RelativeScope_Parent <$> arbitrary
    , RelativeScope_Static <$> arbitrary
    ]

  shrink x = case x of
    RelativeScope_Self z ->
      map RelativeScope_Self $ shrink z
    RelativeScope_Parent z ->
      map RelativeScope_Parent $ shrink z
    RelativeScope_Static z ->
      map RelativeScope_Static $ shrink z





data ScopeResolutionQualifier
  = ScopeResolutionQualifier_Relative  RelativeScope
  | ScopeResolutionQualifier_Qualified QualifiedName
  | ScopeResolutionQualifier_Deref     DereferencableExpression
  deriving (Eq, Show)

instance Render ScopeResolutionQualifier where
  render x = case x of
    ScopeResolutionQualifier_Relative  z -> render z
    ScopeResolutionQualifier_Qualified z -> render z
    ScopeResolutionQualifier_Deref     z -> render z

instance HasExtent ScopeResolutionQualifier where
  extentOf x = case x of
    ScopeResolutionQualifier_Relative  z -> extentOf z
    ScopeResolutionQualifier_Qualified z -> extentOf z
    ScopeResolutionQualifier_Deref     z -> extentOf z

  shiftTo loc x = case x of
    ScopeResolutionQualifier_Relative z ->
      shiftToWrap ScopeResolutionQualifier_Relative loc z
    ScopeResolutionQualifier_Qualified z ->
      shiftToWrap ScopeResolutionQualifier_Qualified loc z
    ScopeResolutionQualifier_Deref z ->
      shiftToWrap ScopeResolutionQualifier_Deref loc z

instance Arbitrary ScopeResolutionQualifier where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ScopeResolutionQualifier_Relative  <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ ScopeResolutionQualifier_Relative  <$> arbitrary
        , ScopeResolutionQualifier_Qualified <$> arbitrary
        , ScopeResolutionQualifier_Deref     <$> arbitrary
        ]

  shrink x = case x of
    ScopeResolutionQualifier_Relative z ->
      map ScopeResolutionQualifier_Relative $ shrink z
    ScopeResolutionQualifier_Qualified z ->
      map ScopeResolutionQualifier_Qualified $ shrink z
    ScopeResolutionQualifier_Deref z ->
      map ScopeResolutionQualifier_Deref $ shrink z





newtype ClassConstantAccessExpression = ClassConstantAccessExpression
  { unClassConstantAccessExpression ::
      ( ScopeResolutionQualifier
      , Symbol DoubleColon_
      , Name
      )
  } deriving (Eq, Show)

instance Render ClassConstantAccessExpression where
  render = render . unClassConstantAccessExpression

instance HasExtent ClassConstantAccessExpression where
  extentOf = extentOf . unClassConstantAccessExpression

  shiftTo loc (ClassConstantAccessExpression x) =
    shiftToWrap ClassConstantAccessExpression loc x

instance Arbitrary ClassConstantAccessExpression where
  arbitrary = scale (frac 1 2) $
    ClassConstantAccessExpression <$> arbitrary

  shrink (ClassConstantAccessExpression x) =
    map ClassConstantAccessExpression $ shrink x





data TraitUseSpecification
  = TraitUseSpecification_Empty
        (Symbol Semicolon_)
  | TraitUseSpecification_Alias
      ( Symbol OpenBrace_
      , Maybe TraitSelectAndAliasClauses
      , Symbol ClosedBrace_
      )
  deriving (Eq, Show)

instance Render TraitUseSpecification where
  render x = case x of
    TraitUseSpecification_Empty z -> render z
    TraitUseSpecification_Alias z -> render z

instance HasExtent TraitUseSpecification where
  extentOf x = case x of
    TraitUseSpecification_Empty z -> extentOf z
    TraitUseSpecification_Alias z -> extentOf z

  shiftTo loc x = case x of
    TraitUseSpecification_Empty z ->
      shiftToWrap TraitUseSpecification_Empty loc z
    TraitUseSpecification_Alias z ->
      shiftToWrap TraitUseSpecification_Alias loc z

instance Arbitrary TraitUseSpecification where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ TraitUseSpecification_Empty <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ TraitUseSpecification_Empty <$> arbitrary
        , TraitUseSpecification_Alias <$> arbitrary
        ]

  shrink x = case x of
    TraitUseSpecification_Empty z ->
      map TraitUseSpecification_Empty $ shrink z
    TraitUseSpecification_Alias z ->
      map TraitUseSpecification_Alias $ shrink z





newtype TraitUseClause = TraitUseClause
  { unTraitUseClause ::
      ( Keyword Use_
      , TraitNameList
      , TraitUseSpecification
      )
  } deriving (Eq, Show)

instance Render TraitUseClause where
  render = render . unTraitUseClause

instance HasExtent TraitUseClause where
  extentOf = extentOf . unTraitUseClause

  shiftTo loc (TraitUseClause x) =
    shiftToWrap TraitUseClause loc x

instance Arbitrary TraitUseClause where
  arbitrary = scale (frac 3 4) $
    TraitUseClause <$> arbitrary

  shrink (TraitUseClause x) =
    map TraitUseClause $ shrink x





newtype TraitUseClauses = TraitUseClauses
  { unTraitUseClauses :: Seq TraitUseClause
  } deriving (Eq, Show)

instance Render TraitUseClauses where
  render = render . unTraitUseClauses

instance HasExtent TraitUseClauses where
  extentOf = extentOf . unTraitUseClauses

  shiftTo loc (TraitUseClauses x) =
    shiftToWrap TraitUseClauses loc x

instance Arbitrary TraitUseClauses where
  arbitrary = TraitUseClauses <$> arbitrary

  shrink (TraitUseClauses x) =
    map TraitUseClauses $ shrink x





newtype UnsetStatement = UnsetStatement
  { unUnsetStatement ::
      ( Keyword Unset_
      , Symbol OpenParen_
      , VariableList
      , Maybe (Symbol Comma_)
      , Symbol ClosedParen_
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render UnsetStatement where
  render = render . unUnsetStatement

instance HasExtent UnsetStatement where
  extentOf = extentOf . unUnsetStatement

  shiftTo loc (UnsetStatement x) =
    shiftToWrap UnsetStatement loc x

instance Arbitrary UnsetStatement where
  arbitrary = scale (frac 1 9) $
    UnsetStatement <$> arbitrary

  shrink (UnsetStatement x) =
    map UnsetStatement $ shrink x





newtype TryStatement = TryStatement
  { unTryStatement ::
      ( Keyword Try_
      , CompoundStatement
      , TryStatementBody
      )
  } deriving (Eq, Show)

instance Render TryStatement where
  render = render . unTryStatement

instance HasExtent TryStatement where
  extentOf = extentOf . unTryStatement

  shiftTo loc (TryStatement x) =
    shiftToWrap TryStatement loc x

instance Arbitrary TryStatement where
  arbitrary = scale (frac 1 3) $
    TryStatement <$> arbitrary

  shrink (TryStatement x) =
    map TryStatement $ shrink x





data TryStatementBody
  = TryStatementBody_Catch
      ( CatchClauses
      , Maybe FinallyClause
      )
  | TryStatementBody_Finally
      ( FinallyClause
      )
  deriving (Eq, Show)

instance Render TryStatementBody where
  render x = case x of
    TryStatementBody_Catch   z -> render z
    TryStatementBody_Finally z -> render z

instance HasExtent TryStatementBody where
  extentOf x = case x of
    TryStatementBody_Catch   z -> extentOf z
    TryStatementBody_Finally z -> extentOf z

  shiftTo loc x = case x of
    TryStatementBody_Catch z ->
      shiftToWrap TryStatementBody_Catch loc z
    TryStatementBody_Finally z ->
      shiftToWrap TryStatementBody_Finally loc z

instance Arbitrary TryStatementBody where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ TryStatementBody_Finally <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ TryStatementBody_Catch   <$> arbitrary
        , TryStatementBody_Finally <$> arbitrary
        ]

  shrink x = case x of
    TryStatementBody_Catch z ->
      map TryStatementBody_Catch $ shrink z
    TryStatementBody_Finally z ->
      map TryStatementBody_Finally $ shrink z



data CaseStatements
  = CaseStatements_Case
      ( CaseStatement
      , Maybe CaseStatements
      )
  | CaseStatements_Default
      ( DefaultStatement
      , Maybe CaseStatements
      )
  deriving (Eq, Show)

instance Render CaseStatements where
  render x = case x of
    CaseStatements_Case    z -> render z
    CaseStatements_Default z -> render z

instance HasExtent CaseStatements where
  extentOf x = case x of
    CaseStatements_Case    z -> extentOf z
    CaseStatements_Default z -> extentOf z

  shiftTo loc x = case x of
    CaseStatements_Case z ->
      shiftToWrap CaseStatements_Case loc z
    CaseStatements_Default z ->
      shiftToWrap CaseStatements_Default loc z

instance Arbitrary CaseStatements where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ curry CaseStatements_Case
            <$> arbitrary
            <*> pure Nothing
        , curry CaseStatements_Default
            <$> arbitrary
            <*> pure Nothing
        ]
      else scale (frac 1 2) $ oneof
        [ CaseStatements_Case    <$> arbitrary
        , CaseStatements_Default <$> arbitrary
        ]

  shrink x = case x of
    CaseStatements_Case z ->
      map CaseStatements_Case $ shrink z
    CaseStatements_Default z ->
      map CaseStatements_Default $ shrink z





newtype SwitchStatement = SwitchStatement
  { unSwitchStatement ::
      ( Keyword Switch_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      , SwitchStatementBody
      )
  } deriving (Eq, Show)

instance Render SwitchStatement where
  render = render . unSwitchStatement

instance HasExtent SwitchStatement where
  extentOf = extentOf . unSwitchStatement

  shiftTo loc (SwitchStatement x) =
    shiftToWrap SwitchStatement loc x

instance Arbitrary SwitchStatement where
  arbitrary = scale (frac 1 5) $
    SwitchStatement <$> arbitrary

  shrink (SwitchStatement x) =
    map SwitchStatement $ shrink x





data SwitchStatementBody
  = SwitchStatementBody_Brace
      ( Symbol OpenBrace_
      , Maybe CaseStatements
      , Symbol ClosedBrace_
      )
  | SwitchStatementBody_Colon
      ( Symbol Colon_
      , Maybe CaseStatements
      , Keyword Endswitch_
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render SwitchStatementBody where
  render x = case x of
    SwitchStatementBody_Brace z -> render z
    SwitchStatementBody_Colon z -> render z

instance HasExtent SwitchStatementBody where
  extentOf x = case x of
    SwitchStatementBody_Brace z -> extentOf z
    SwitchStatementBody_Colon z -> extentOf z

  shiftTo loc x = case x of
    SwitchStatementBody_Brace z ->
      shiftToWrap SwitchStatementBody_Brace loc z
    SwitchStatementBody_Colon z ->
      shiftToWrap SwitchStatementBody_Colon loc z

instance Arbitrary SwitchStatementBody where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ curry3 SwitchStatementBody_Brace
            <$> arbitrary
            <*> pure Nothing
            <*> arbitrary
        , curry4 SwitchStatementBody_Colon
            <$> arbitrary
            <*> pure Nothing
            <*> arbitrary
            <*> arbitrary
        ]
      else scale (frac 1 5) $ oneof
        [ SwitchStatementBody_Brace <$> arbitrary
        , SwitchStatementBody_Colon <$> arbitrary
        ]

  shrink x = case x of
    SwitchStatementBody_Brace z ->
      map SwitchStatementBody_Brace $ shrink z
    SwitchStatementBody_Colon z ->
      map SwitchStatementBody_Colon $ shrink z





data SelectionStatement
  = SelectionStatement_If     IfStatement
  | SelectionStatement_Switch SwitchStatement
  deriving (Eq, Show)

instance Render SelectionStatement where
  render x = case x of
    SelectionStatement_If     z -> render z
    SelectionStatement_Switch z -> render z

instance HasExtent SelectionStatement where
  extentOf x = case x of
    SelectionStatement_If     z -> extentOf z
    SelectionStatement_Switch z -> extentOf z

  shiftTo loc x = case x of
    SelectionStatement_If z ->
      shiftToWrap SelectionStatement_If loc z
    SelectionStatement_Switch z ->
      shiftToWrap SelectionStatement_Switch loc z

instance Arbitrary SelectionStatement where
  arbitrary = oneof
    [ SelectionStatement_If     <$> arbitrary
    , SelectionStatement_Switch <$> arbitrary
    ]

  shrink x = case x of
    SelectionStatement_If z ->
      map SelectionStatement_If $ shrink z
    SelectionStatement_Switch z ->
      map SelectionStatement_Switch $ shrink z





newtype WhileStatement = WhileStatement
  { unWhileStatement ::
      ( Keyword While_
      , Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      , WhileStatementBody
      )
  } deriving (Eq, Show)

instance Render WhileStatement where
  render = render . unWhileStatement

instance HasExtent WhileStatement where
  extentOf = extentOf . unWhileStatement

  shiftTo loc (WhileStatement x) =
    shiftToWrap WhileStatement loc x

instance Arbitrary WhileStatement where
  arbitrary = scale (frac 1 5) $
    WhileStatement <$> arbitrary

  shrink (WhileStatement x) =
    map WhileStatement $ shrink x





data WhileStatementBody
  = WhileStatementBody_Brace
        Statement
  | WhileStatementBody_Colon
      ( Symbol Colon_
      , StatementList
      , Keyword Endwhile_
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render WhileStatementBody where
  render x = case x of
    WhileStatementBody_Brace z -> render z
    WhileStatementBody_Colon z -> render z

instance HasExtent WhileStatementBody where
  extentOf x = case x of
    WhileStatementBody_Brace z -> extentOf z
    WhileStatementBody_Colon z -> extentOf z

  shiftTo loc x = case x of
    WhileStatementBody_Brace z ->
      shiftToWrap WhileStatementBody_Brace loc z
    WhileStatementBody_Colon z ->
      shiftToWrap WhileStatementBody_Colon loc z

instance Arbitrary WhileStatementBody where
  arbitrary = scale (frac 1 4) $ oneof
    [ WhileStatementBody_Brace <$> arbitrary
    , WhileStatementBody_Colon <$> arbitrary
    ]

  shrink x = case x of
    WhileStatementBody_Brace z ->
      map WhileStatementBody_Brace $ shrink z
    WhileStatementBody_Colon z ->
      map WhileStatementBody_Colon $ shrink z





data NamespaceUseDeclaration
  = NamespaceUseDeclaration_Use
      ( Keyword Use_
      , Maybe NamespaceFunctionOrConst
      , NamespaceUseClauses
      , Symbol Semicolon_
      )
  | NamespaceUseDeclaration_Function
      ( Keyword Use_
      , NamespaceFunctionOrConst
      , Maybe (Symbol Backslash_)
      , NamespaceName
      , Symbol Backslash_
      , Symbol OpenBrace_
      , NamespaceUseGroupClauses1
      , Symbol ClosedBrace_
      , Symbol Semicolon_
      )
  | NamespaceUseDeclaration_Name
      ( Keyword Use_
      , Maybe (Symbol Backslash_)
      , NamespaceName
      , Symbol Backslash_
      , Symbol OpenBrace_
      , NamespaceUseGroupClauses2
      , Symbol ClosedBrace_
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render NamespaceUseDeclaration where
  render x = case x of
    NamespaceUseDeclaration_Use      z -> render z
    NamespaceUseDeclaration_Function z -> render z
    NamespaceUseDeclaration_Name     z -> render z

instance HasExtent NamespaceUseDeclaration where
  extentOf x = case x of
    NamespaceUseDeclaration_Use      z -> extentOf z
    NamespaceUseDeclaration_Function z -> extentOf z
    NamespaceUseDeclaration_Name     z -> extentOf z

  shiftTo loc x = case x of
    NamespaceUseDeclaration_Use z ->
      shiftToWrap NamespaceUseDeclaration_Use loc z
    NamespaceUseDeclaration_Function z ->
      shiftToWrap NamespaceUseDeclaration_Function loc z
    NamespaceUseDeclaration_Name z ->
      shiftToWrap NamespaceUseDeclaration_Name loc z

instance Arbitrary NamespaceUseDeclaration where
  arbitrary = scale (frac 1 9) $ oneof
    [ NamespaceUseDeclaration_Use      <$> arbitrary
    , NamespaceUseDeclaration_Function <$> arbitrary
    , NamespaceUseDeclaration_Name     <$> arbitrary
    ]

  shrink x = case x of
    NamespaceUseDeclaration_Use z ->
      map NamespaceUseDeclaration_Use $ shrink z
    NamespaceUseDeclaration_Function z ->
      map NamespaceUseDeclaration_Function $ shrink z
    NamespaceUseDeclaration_Name z ->
      map NamespaceUseDeclaration_Name $ shrink z



data NamespaceDefinition
  = NamespaceDefinition_Name
      ( Keyword Namespace_
      , Name
      , Symbol Semicolon_
      )
  | NamespaceDefinition_Compound
      ( Keyword Namespace_
      , Maybe Name
      , CompoundStatement
      )
  deriving (Eq, Show)

instance Render NamespaceDefinition where
  render x = case x of
    NamespaceDefinition_Name     z -> render z
    NamespaceDefinition_Compound z -> render z

instance HasExtent NamespaceDefinition where
  extentOf x = case x of
    NamespaceDefinition_Name     z -> extentOf z
    NamespaceDefinition_Compound z -> extentOf z

  shiftTo loc x = case x of
    NamespaceDefinition_Name z ->
      shiftToWrap NamespaceDefinition_Name loc z
    NamespaceDefinition_Compound z ->
      shiftToWrap NamespaceDefinition_Compound loc z

instance Arbitrary NamespaceDefinition where
  arbitrary = scale (frac 1 2) $ oneof
    [ NamespaceDefinition_Name     <$> arbitrary
    , NamespaceDefinition_Compound <$> arbitrary
    ]

  shrink x = case x of
    NamespaceDefinition_Name z ->
      map NamespaceDefinition_Name $ shrink z
    NamespaceDefinition_Compound z ->
      map NamespaceDefinition_Compound $ shrink z





newtype FunctionDefinition = FunctionDefinition
  { unFunctionDefinition ::
      ( FunctionDefinitionHeader
      , CompoundStatement
      )
  } deriving (Eq, Show)

instance Render FunctionDefinition where
  render = render . unFunctionDefinition

instance HasExtent FunctionDefinition where
  extentOf = extentOf . unFunctionDefinition

  shiftTo loc (FunctionDefinition x) =
    shiftToWrap FunctionDefinition loc x

instance Arbitrary FunctionDefinition where
  arbitrary = scale (frac 3 4) $
    FunctionDefinition <$> arbitrary

  shrink (FunctionDefinition x) =
    map FunctionDefinition $ shrink x





data VariadicDeclarationList
  = VariadicDeclarationList_Simple
      ( SimpleParameterDeclarationList
      , Symbol Comma_
      , VariadicParameter
      )
  | VariadicDeclarationList_Variadic
        VariadicParameter
  deriving (Eq, Show)

instance Render VariadicDeclarationList where
  render x = case x of
    VariadicDeclarationList_Simple   z -> render z
    VariadicDeclarationList_Variadic z -> render z

instance HasExtent VariadicDeclarationList where
  extentOf x = case x of
    VariadicDeclarationList_Simple   z -> extentOf z
    VariadicDeclarationList_Variadic z -> extentOf z

  shiftTo loc x = case x of
    VariadicDeclarationList_Simple z ->
      shiftToWrap VariadicDeclarationList_Simple loc z
    VariadicDeclarationList_Variadic z ->
      shiftToWrap VariadicDeclarationList_Variadic loc z

instance Arbitrary VariadicDeclarationList where
  arbitrary = scale (frac 3 4) $ oneof
    [ VariadicDeclarationList_Simple   <$> arbitrary
    , VariadicDeclarationList_Variadic <$> arbitrary
    ]

  shrink x = case x of
    VariadicDeclarationList_Simple z ->
      map VariadicDeclarationList_Simple $ shrink z
    VariadicDeclarationList_Variadic z ->
      map VariadicDeclarationList_Variadic $ shrink z





newtype SimpleParameterDeclarationList = SimpleParameterDeclarationList
  { unSimpleParameterDeclarationList ::
      SeqSep (Symbol Comma_) ParameterDeclaration
  } deriving (Eq, Show)

instance Render SimpleParameterDeclarationList where
  render = render . unSimpleParameterDeclarationList

instance HasExtent SimpleParameterDeclarationList where
  extentOf = extentOf . unSimpleParameterDeclarationList

  shiftTo loc (SimpleParameterDeclarationList x) =
    shiftToWrap SimpleParameterDeclarationList loc x

instance Arbitrary SimpleParameterDeclarationList where
  arbitrary = SimpleParameterDeclarationList <$> arbitrary

  shrink (SimpleParameterDeclarationList x) =
    map SimpleParameterDeclarationList $ shrink x





data ParameterDeclarationList
  = ParameterDeclarationList_Simple   SimpleParameterDeclarationList
  | ParameterDeclarationList_Variadic VariadicDeclarationList
  deriving (Eq, Show)

instance Render ParameterDeclarationList where
  render x = case x of
    ParameterDeclarationList_Simple   z -> render z
    ParameterDeclarationList_Variadic z -> render z

instance HasExtent ParameterDeclarationList where
  extentOf x = case x of
    ParameterDeclarationList_Simple   z -> extentOf z
    ParameterDeclarationList_Variadic z -> extentOf z

  shiftTo loc x = case x of
    ParameterDeclarationList_Simple z ->
      shiftToWrap ParameterDeclarationList_Simple loc z
    ParameterDeclarationList_Variadic z ->
      shiftToWrap ParameterDeclarationList_Variadic loc z

instance Arbitrary ParameterDeclarationList where
  arbitrary = oneof
    [ ParameterDeclarationList_Simple   <$> arbitrary
    , ParameterDeclarationList_Variadic <$> arbitrary
    ]

  shrink x = case x of
    ParameterDeclarationList_Simple z ->
      map ParameterDeclarationList_Simple $ shrink z
    ParameterDeclarationList_Variadic z ->
      map ParameterDeclarationList_Variadic $ shrink z





newtype FunctionDefinitionHeader = FunctionDefinitionHeader
  { unFunctionDefinitionHeader ::
      ( Keyword Function_
      , Maybe (Symbol Amp_)
      , Name
      , Symbol OpenParen_
      , Maybe ParameterDeclarationList
      , Symbol ClosedParen_
      , Maybe ReturnType
      )
  } deriving (Eq, Show)

instance Render FunctionDefinitionHeader where
  render = render . unFunctionDefinitionHeader

instance HasExtent FunctionDefinitionHeader where
  extentOf = extentOf . unFunctionDefinitionHeader

  shiftTo loc (FunctionDefinitionHeader x) =
    shiftToWrap FunctionDefinitionHeader loc x

instance Arbitrary FunctionDefinitionHeader where
  arbitrary = scale (frac 1 7) $
    FunctionDefinitionHeader <$> arbitrary

  shrink (FunctionDefinitionHeader x) =
    map FunctionDefinitionHeader $ shrink x





newtype ConstructorDeclaration = ConstructorDeclaration
  { unConstructorDeclaration ::
      ( MethodModifiers
      , Keyword Function_
      , Maybe (Symbol Amp_)
      , Keyword Construct_
      , Symbol OpenParen_
      , Maybe ParameterDeclarationList
      , Symbol ClosedParen_
      , CompoundStatement
      )
  } deriving (Eq, Show)

instance Render ConstructorDeclaration where
  render = render . unConstructorDeclaration

instance HasExtent ConstructorDeclaration where
  extentOf = extentOf . unConstructorDeclaration

  shiftTo loc (ConstructorDeclaration x) =
    shiftToWrap ConstructorDeclaration loc x

instance Arbitrary ConstructorDeclaration where
  arbitrary = scale (frac 1 8) $
    ConstructorDeclaration <$> arbitrary

  shrink (ConstructorDeclaration x) =
    map ConstructorDeclaration $ shrink x





newtype DestructorDeclaration = DestructorDeclaration
  { unDestructorDeclaration ::
      ( MethodModifiers
      , Keyword Function_
      , Maybe (Symbol Amp_)
      , Keyword Destruct_
      , Symbol OpenParen_
      , Symbol ClosedParen_
      , CompoundStatement
      )
  } deriving (Eq, Show)

instance Render DestructorDeclaration where
  render = render . unDestructorDeclaration

instance HasExtent DestructorDeclaration where
  extentOf = extentOf . unDestructorDeclaration

  shiftTo loc (DestructorDeclaration x) =
    shiftToWrap DestructorDeclaration loc x

instance Arbitrary DestructorDeclaration where
  arbitrary = scale (frac 1 7) $
    DestructorDeclaration <$> arbitrary

  shrink (DestructorDeclaration x) =
    map DestructorDeclaration $ shrink x





data MethodDeclaration
  = MethodDeclaration_Function
      ( Maybe MethodModifiers
      , FunctionDefinition
      )
  | MethodDeclaration_Header
      ( MethodModifiers
      , FunctionDefinitionHeader
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render MethodDeclaration where
  render x = case x of
    MethodDeclaration_Function z -> render z
    MethodDeclaration_Header   z -> render z

instance HasExtent MethodDeclaration where
  extentOf x = case x of
    MethodDeclaration_Function z -> extentOf z
    MethodDeclaration_Header   z -> extentOf z

  shiftTo loc x = case x of
    MethodDeclaration_Function z ->
      shiftToWrap MethodDeclaration_Function loc z
    MethodDeclaration_Header z ->
      shiftToWrap MethodDeclaration_Header loc z

instance Arbitrary MethodDeclaration where
  arbitrary = scale (frac 1 3) $ oneof
    [ MethodDeclaration_Function <$> arbitrary
    , MethodDeclaration_Header   <$> arbitrary
    ]

  shrink x = case x of
    MethodDeclaration_Function z ->
      map MethodDeclaration_Function $ shrink z
    MethodDeclaration_Header z ->
      map MethodDeclaration_Header $ shrink z





data TraitMemberDeclaration
  = TraitMemberDeclaration_Property  PropertyDeclaration
  | TraitMemberDeclaration_Method    MethodDeclaration
  | TraitMemberDeclaration_Construct ConstructorDeclaration
  | TraitMemberDeclaration_Destruct  DestructorDeclaration
  | TraitMemberDeclaration_Trait     TraitUseClauses
  deriving (Eq, Show)

instance Render TraitMemberDeclaration where
  render x = case x of
    TraitMemberDeclaration_Property  z -> render z
    TraitMemberDeclaration_Method    z -> render z
    TraitMemberDeclaration_Construct z -> render z
    TraitMemberDeclaration_Destruct  z -> render z
    TraitMemberDeclaration_Trait     z -> render z

instance HasExtent TraitMemberDeclaration where
  extentOf x = case x of
    TraitMemberDeclaration_Property  z -> extentOf z
    TraitMemberDeclaration_Method    z -> extentOf z
    TraitMemberDeclaration_Construct z -> extentOf z
    TraitMemberDeclaration_Destruct  z -> extentOf z
    TraitMemberDeclaration_Trait     z -> extentOf z

  shiftTo loc x = case x of
    TraitMemberDeclaration_Property z ->
      shiftToWrap TraitMemberDeclaration_Property loc z
    TraitMemberDeclaration_Method z ->
      shiftToWrap TraitMemberDeclaration_Method loc z
    TraitMemberDeclaration_Construct z ->
      shiftToWrap TraitMemberDeclaration_Construct loc z
    TraitMemberDeclaration_Destruct z ->
      shiftToWrap TraitMemberDeclaration_Destruct loc z
    TraitMemberDeclaration_Trait z ->
      shiftToWrap TraitMemberDeclaration_Trait loc z

instance Arbitrary TraitMemberDeclaration where
  arbitrary = oneof
    [ TraitMemberDeclaration_Property  <$> arbitrary
    , TraitMemberDeclaration_Method    <$> arbitrary
    , TraitMemberDeclaration_Construct <$> arbitrary
    , TraitMemberDeclaration_Destruct  <$> arbitrary
    , TraitMemberDeclaration_Trait     <$> arbitrary
    ]

  shrink x = case x of
    TraitMemberDeclaration_Property z ->
      map TraitMemberDeclaration_Property $ shrink z
    TraitMemberDeclaration_Method z ->
      map TraitMemberDeclaration_Method $ shrink z
    TraitMemberDeclaration_Construct z ->
      map TraitMemberDeclaration_Construct $ shrink z
    TraitMemberDeclaration_Destruct z ->
      map TraitMemberDeclaration_Destruct $ shrink z
    TraitMemberDeclaration_Trait z ->
      map TraitMemberDeclaration_Trait $ shrink z





newtype TraitMemberDeclarations = TraitMemberDeclarations
  { unTraitMemberDeclarations ::
      Seq TraitMemberDeclaration
  } deriving (Eq, Show)

instance Render TraitMemberDeclarations where
  render = render . unTraitMemberDeclarations

instance HasExtent TraitMemberDeclarations where
  extentOf = extentOf . unTraitMemberDeclarations

  shiftTo loc (TraitMemberDeclarations x) =
    shiftToWrap TraitMemberDeclarations loc x

instance Arbitrary TraitMemberDeclarations where
  arbitrary = TraitMemberDeclarations <$> arbitrary

  shrink (TraitMemberDeclarations x) =
    map TraitMemberDeclarations $ shrink x





newtype TraitDeclaration = TraitDeclaration
  { unTraitDeclaration ::
      ( Keyword Trait_
      , Name
      , Symbol OpenBrace_
      , Maybe TraitMemberDeclarations
      , Symbol ClosedBrace_
      )
  } deriving (Eq, Show)

instance Render TraitDeclaration where
  render = render . unTraitDeclaration

instance HasExtent TraitDeclaration where
  extentOf = extentOf . unTraitDeclaration

  shiftTo loc (TraitDeclaration x) =
    shiftToWrap TraitDeclaration loc x

instance Arbitrary TraitDeclaration where
  arbitrary = scale (frac 1 5) $
    TraitDeclaration <$> arbitrary

  shrink (TraitDeclaration x) =
    map TraitDeclaration $ shrink x





data InterfaceMemberDeclaration
  = InterfaceMemberDeclaration_Const  ClassConstDeclaration
  | InterfaceMemberDeclaration_Method MethodDeclaration
  deriving (Eq, Show)

instance Render InterfaceMemberDeclaration where
  render x = case x of
    InterfaceMemberDeclaration_Const  z -> render z
    InterfaceMemberDeclaration_Method z -> render z

instance HasExtent InterfaceMemberDeclaration where
  extentOf x = case x of
    InterfaceMemberDeclaration_Const  z -> extentOf z
    InterfaceMemberDeclaration_Method z -> extentOf z

  shiftTo loc x = case x of
    InterfaceMemberDeclaration_Const z ->
      shiftToWrap InterfaceMemberDeclaration_Const loc z
    InterfaceMemberDeclaration_Method z ->
      shiftToWrap InterfaceMemberDeclaration_Method loc z

instance Arbitrary InterfaceMemberDeclaration where
  arbitrary = oneof
    [ InterfaceMemberDeclaration_Const  <$> arbitrary
    , InterfaceMemberDeclaration_Method <$> arbitrary
    ]

  shrink x = case x of
    InterfaceMemberDeclaration_Const z ->
      map InterfaceMemberDeclaration_Const $ shrink z
    InterfaceMemberDeclaration_Method z ->
      map InterfaceMemberDeclaration_Method $ shrink z





newtype InterfaceMemberDeclarations = InterfaceMemberDeclarations
  { unInterfaceMemberDeclarations ::
      Seq InterfaceMemberDeclaration
  } deriving (Eq, Show)

instance Render InterfaceMemberDeclarations where
  render = render . unInterfaceMemberDeclarations

instance HasExtent InterfaceMemberDeclarations where
  extentOf = extentOf . unInterfaceMemberDeclarations

  shiftTo loc (InterfaceMemberDeclarations x) =
    shiftToWrap InterfaceMemberDeclarations loc x

instance Arbitrary InterfaceMemberDeclarations where
  arbitrary = InterfaceMemberDeclarations <$> arbitrary

  shrink (InterfaceMemberDeclarations x) =
    map InterfaceMemberDeclarations $ shrink x





data InterfaceBaseClause
  = InterfaceBaseClause_Extends
      ( Keyword Extends_
      , QualifiedName
      )
  | InterfaceBaseClause_List
      ( InterfaceBaseClause
      , Symbol Comma_
      , QualifiedName
      )
  deriving (Eq, Show)

instance Render InterfaceBaseClause where
  render x = case x of
    InterfaceBaseClause_Extends z -> render z
    InterfaceBaseClause_List    z -> render z

instance HasExtent InterfaceBaseClause where
  extentOf x = case x of
    InterfaceBaseClause_Extends z -> extentOf z
    InterfaceBaseClause_List    z -> extentOf z

  shiftTo loc x = case x of
    InterfaceBaseClause_Extends z ->
      shiftToWrap InterfaceBaseClause_Extends loc z
    InterfaceBaseClause_List z ->
      shiftToWrap InterfaceBaseClause_List loc z

instance Arbitrary InterfaceBaseClause where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ InterfaceBaseClause_Extends <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ InterfaceBaseClause_Extends <$> arbitrary
        , InterfaceBaseClause_List    <$> arbitrary
        ]

  shrink x = case x of
    InterfaceBaseClause_Extends z ->
      map InterfaceBaseClause_Extends $ shrink z
    InterfaceBaseClause_List z ->
      map InterfaceBaseClause_List $ shrink z





newtype InterfaceDeclaration = InterfaceDeclaration
  { unInterfaceDeclaration ::
      ( Keyword Interface_
      , Name
      , Maybe InterfaceBaseClause
      , Symbol OpenBrace_
      , Maybe InterfaceMemberDeclarations
      , Symbol ClosedBrace_
      )
  } deriving (Eq, Show)

instance Render InterfaceDeclaration where
  render = render . unInterfaceDeclaration

instance HasExtent InterfaceDeclaration where
  extentOf = extentOf . unInterfaceDeclaration

  shiftTo loc (InterfaceDeclaration x) =
    shiftToWrap InterfaceDeclaration loc x

instance Arbitrary InterfaceDeclaration where
  arbitrary = scale (frac 1 5) $
    InterfaceDeclaration <$> arbitrary

  shrink (InterfaceDeclaration x) =
    map InterfaceDeclaration $ shrink x





data ClassMemberDeclaration
  = ClassMemberDeclaration_Const     ClassConstDeclaration
  | ClassMemberDeclaration_Property  PropertyDeclaration
  | ClassMemberDeclaration_Method    MethodDeclaration
  | ClassMemberDeclaration_Construct ConstructorDeclaration
  | ClassMemberDeclaration_Destruct  DestructorDeclaration
  | ClassMemberDeclaration_Trait     TraitUseClause
  deriving (Eq, Show)

instance Render ClassMemberDeclaration where
  render x = case x of
    ClassMemberDeclaration_Const     z -> render z
    ClassMemberDeclaration_Property  z -> render z
    ClassMemberDeclaration_Method    z -> render z
    ClassMemberDeclaration_Construct z -> render z
    ClassMemberDeclaration_Destruct  z -> render z
    ClassMemberDeclaration_Trait     z -> render z

instance HasExtent ClassMemberDeclaration where
  extentOf x = case x of
    ClassMemberDeclaration_Const     z -> extentOf z
    ClassMemberDeclaration_Property  z -> extentOf z
    ClassMemberDeclaration_Method    z -> extentOf z
    ClassMemberDeclaration_Construct z -> extentOf z
    ClassMemberDeclaration_Destruct  z -> extentOf z
    ClassMemberDeclaration_Trait     z -> extentOf z

  shiftTo loc x = case x of
    ClassMemberDeclaration_Const z ->
      shiftToWrap ClassMemberDeclaration_Const loc z
    ClassMemberDeclaration_Property z ->
      shiftToWrap ClassMemberDeclaration_Property loc z
    ClassMemberDeclaration_Method z ->
      shiftToWrap ClassMemberDeclaration_Method loc z
    ClassMemberDeclaration_Construct z ->
      shiftToWrap ClassMemberDeclaration_Construct loc z
    ClassMemberDeclaration_Destruct z ->
      shiftToWrap ClassMemberDeclaration_Destruct loc z
    ClassMemberDeclaration_Trait z ->
      shiftToWrap ClassMemberDeclaration_Trait loc z

instance Arbitrary ClassMemberDeclaration where
  arbitrary = oneof
    [ ClassMemberDeclaration_Const     <$> arbitrary
    , ClassMemberDeclaration_Property  <$> arbitrary
    , ClassMemberDeclaration_Method    <$> arbitrary
    , ClassMemberDeclaration_Construct <$> arbitrary
    , ClassMemberDeclaration_Destruct  <$> arbitrary
    , ClassMemberDeclaration_Trait     <$> arbitrary
    ]

  shrink x = case x of
    ClassMemberDeclaration_Const z ->
      map ClassMemberDeclaration_Const $ shrink z
    ClassMemberDeclaration_Property z ->
      map ClassMemberDeclaration_Property $ shrink z
    ClassMemberDeclaration_Method z ->
      map ClassMemberDeclaration_Method $ shrink z
    ClassMemberDeclaration_Construct z ->
      map ClassMemberDeclaration_Construct $ shrink z
    ClassMemberDeclaration_Destruct z ->
      map ClassMemberDeclaration_Destruct $ shrink z
    ClassMemberDeclaration_Trait z ->
      map ClassMemberDeclaration_Trait $ shrink z





newtype ClassMemberDeclarations = ClassMemberDeclarations
  { unClassMemberDeclarations ::
      Seq ClassMemberDeclaration
  } deriving (Eq, Show)

instance Render ClassMemberDeclarations where
  render = render . unClassMemberDeclarations

instance HasExtent ClassMemberDeclarations where
  extentOf = extentOf . unClassMemberDeclarations

  shiftTo loc (ClassMemberDeclarations x) =
    shiftToWrap ClassMemberDeclarations loc x

instance Arbitrary ClassMemberDeclarations where
  arbitrary = ClassMemberDeclarations <$> arbitrary

  shrink (ClassMemberDeclarations x) =
    map ClassMemberDeclarations $ shrink x





data ClassInterfaceClause
  = ClassInterfaceClause_Implements
      ( Keyword Implements_
      , QualifiedName
      )
  | ClassInterfaceClause_List
      ( ClassInterfaceClause
      , Symbol Comma_
      , QualifiedName
      )
  deriving (Eq, Show)

instance Render ClassInterfaceClause where
  render x = case x of
    ClassInterfaceClause_Implements z -> render z
    ClassInterfaceClause_List       z -> render z

instance HasExtent ClassInterfaceClause where
  extentOf x = case x of
    ClassInterfaceClause_Implements z -> extentOf z
    ClassInterfaceClause_List       z -> extentOf z

  shiftTo loc x = case x of
    ClassInterfaceClause_Implements z ->
      shiftToWrap ClassInterfaceClause_Implements loc z
    ClassInterfaceClause_List z ->
      shiftToWrap ClassInterfaceClause_List loc z

instance Arbitrary ClassInterfaceClause where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ClassInterfaceClause_Implements <$> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ ClassInterfaceClause_Implements <$> arbitrary
        , ClassInterfaceClause_List       <$> arbitrary
        ]

  shrink x = case x of
    ClassInterfaceClause_Implements z ->
      map ClassInterfaceClause_Implements $ shrink z
    ClassInterfaceClause_List z ->
      map ClassInterfaceClause_List $ shrink z





newtype ClassDeclaration = ClassDeclaration
  { unClassDeclaration ::
      ( Maybe ClassModifier
      , Keyword Class_
      , Name
      , Maybe ClassBaseClause
      , Maybe ClassInterfaceClause
      , Symbol OpenBrace_
      , Maybe ClassMemberDeclarations
      , Symbol ClosedBrace_
      )
  } deriving (Eq, Show)

instance Render ClassDeclaration where
  render = render . unClassDeclaration

instance HasExtent ClassDeclaration where
  extentOf = extentOf . unClassDeclaration

  shiftTo loc (ClassDeclaration x) =
    shiftToWrap ClassDeclaration loc x

instance Arbitrary ClassDeclaration where
  arbitrary = scale (frac 1 8) $
    ClassDeclaration <$> arbitrary

  shrink (ClassDeclaration x) =
    map ClassDeclaration $ shrink x





newtype ForStatement = ForStatement
  { unForStatement ::
      ( Keyword For_
      , Symbol OpenParen_
      , Maybe ForInitializer
      , Symbol Semicolon_
      , Maybe ForControl
      , Symbol Semicolon_
      , Maybe ForEndOfLoop
      , Symbol ClosedParen_
      , ForStatementBody
      )
  } deriving (Eq, Show)

instance Render ForStatement where
  render = render . unForStatement

instance HasExtent ForStatement where
  extentOf = extentOf . unForStatement

  shiftTo loc (ForStatement x) =
    shiftToWrap ForStatement loc x

instance Arbitrary ForStatement where
  arbitrary = scale (frac 1 10) $
    ForStatement <$> arbitrary

  shrink (ForStatement x) =
    map ForStatement $ shrink x





data ForStatementBody
  = ForStatementBody_Brace
        Statement
  | ForStatementBody_Colon
      ( Symbol Colon_
      , StatementList
      , Keyword Endfor_
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render ForStatementBody where
  render x = case x of
    ForStatementBody_Brace z -> render z
    ForStatementBody_Colon z -> render z

instance HasExtent ForStatementBody where
  extentOf x = case x of
    ForStatementBody_Brace z -> extentOf z
    ForStatementBody_Colon z -> extentOf z

  shiftTo loc x = case x of
    ForStatementBody_Brace z ->
      shiftToWrap ForStatementBody_Brace loc z
    ForStatementBody_Colon z ->
      shiftToWrap ForStatementBody_Colon loc z

instance Arbitrary ForStatementBody where
  arbitrary = scale (frac 1 3) $ oneof
    [ ForStatementBody_Brace <$> arbitrary
    , ForStatementBody_Colon <$> arbitrary
    ]

  shrink x = case x of
    ForStatementBody_Brace z ->
      map ForStatementBody_Brace $ shrink z
    ForStatementBody_Colon z ->
      map ForStatementBody_Colon $ shrink z





newtype PropertyInString = PropertyInString
  { unPropertyInString ::
      ( MinusGreater_
      , NameInString
      )
  } deriving (Eq, Show)

instance Render PropertyInString where
  render = render . unPropertyInString

instance HasExtent PropertyInString where
  extentOf = extentOf . unPropertyInString

  shiftTo loc (PropertyInString x) =
    shiftToWrap PropertyInString loc x

instance Arbitrary PropertyInString where
  arbitrary = scale (frac 1 2) $
    PropertyInString <$> arbitrary

  shrink (PropertyInString x) =
    map PropertyInString $ shrink x





data OffsetInString
  = OffsetInString_Name
      ( Symbol OpenBrack_
      , Name
      , ClosedBrack_
      )
  | OffsetInString_Variable
      ( Symbol OpenBrack_
      , VariableName
      , ClosedBrack_
      )
  | OffsetInString_Integer
      ( Symbol OpenBrack_
      , IntegerLiteral
      , Maybe WhiteSpaces
      , ClosedBrack_
      )
  deriving (Eq, Show)

instance Render OffsetInString where
  render x = case x of
    OffsetInString_Name     z -> render z
    OffsetInString_Variable z -> render z
    OffsetInString_Integer  z -> render z

instance HasExtent OffsetInString where
  extentOf x = case x of
    OffsetInString_Name     z -> extentOf z
    OffsetInString_Variable z -> extentOf z
    OffsetInString_Integer  z -> extentOf z

  shiftTo loc x = case x of
    OffsetInString_Name z ->
      shiftToWrap OffsetInString_Name loc z
    OffsetInString_Variable z ->
      shiftToWrap OffsetInString_Variable loc z
    OffsetInString_Integer z ->
      shiftToWrap OffsetInString_Integer loc z

instance Arbitrary OffsetInString where
  arbitrary = scale (frac 1 5) $ oneof
    [ OffsetInString_Name     <$> arbitrary
    , OffsetInString_Variable <$> arbitrary
    , OffsetInString_Integer  <$> arbitrary
    ]

  shrink x = case x of
    OffsetInString_Name z ->
      map OffsetInString_Name $ shrink z
    OffsetInString_Variable z ->
      map OffsetInString_Variable $ shrink z
    OffsetInString_Integer z ->
      map OffsetInString_Integer $ shrink z





data OffsetOrProperty
  = OffsetOrProperty_Offset   OffsetInString
  | OffsetOrProperty_Property PropertyInString
  deriving (Eq, Show)

instance Render OffsetOrProperty where
  render x = case x of
    OffsetOrProperty_Offset   z -> render z
    OffsetOrProperty_Property z -> render z

instance HasExtent OffsetOrProperty where
  extentOf x = case x of
    OffsetOrProperty_Offset   z -> extentOf z
    OffsetOrProperty_Property z -> extentOf z

  shiftTo loc x = case x of
    OffsetOrProperty_Offset z ->
      shiftToWrap OffsetOrProperty_Offset loc z
    OffsetOrProperty_Property z ->
      shiftToWrap OffsetOrProperty_Property loc z

instance Arbitrary OffsetOrProperty where
  arbitrary = scale (frac 1 3) $ oneof
    [ OffsetOrProperty_Offset   <$> arbitrary
    , OffsetOrProperty_Property <$> arbitrary
    ]

  shrink x = case x of
    OffsetOrProperty_Offset z ->
      map OffsetOrProperty_Offset $ shrink z
    OffsetOrProperty_Property z ->
      map OffsetOrProperty_Property $ shrink z





data DoubleQuotedStringVariable
  = DoubleQuotedStringVariable_Simple
      ( VariableNameInString
      , Maybe OffsetOrProperty
      )
  | DoubleQuotedStringVariable_Complex
      ( Symbol DollarOpenBrace_
      , Expression
      , ClosedBrace_
      )
  | DoubleQuotedStringVariable_Simplex
      ( OpenBrace_
      , VariableNameInString
      , ClosedBrace_
      )
  deriving (Eq, Show)

instance Render DoubleQuotedStringVariable where
  render x = case x of
    DoubleQuotedStringVariable_Simple  z -> render z
    DoubleQuotedStringVariable_Complex z -> render z
    DoubleQuotedStringVariable_Simplex z -> render z

instance HasExtent DoubleQuotedStringVariable where
  extentOf x = case x of
    DoubleQuotedStringVariable_Simple  z -> extentOf z
    DoubleQuotedStringVariable_Complex z -> extentOf z
    DoubleQuotedStringVariable_Simplex z -> extentOf z

  shiftTo loc x = case x of
    DoubleQuotedStringVariable_Simple z ->
      shiftToWrap DoubleQuotedStringVariable_Simple loc z
    DoubleQuotedStringVariable_Complex z ->
      shiftToWrap DoubleQuotedStringVariable_Complex loc z
    DoubleQuotedStringVariable_Simplex z ->
      shiftToWrap DoubleQuotedStringVariable_Simplex loc z

instance Arbitrary DoubleQuotedStringVariable where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 2) $ oneof
        [ DoubleQuotedStringVariable_Simple  <$> arbitrary
        ]
      else scale (\_ -> 0) $ frequency
        [ ( 8
          , DoubleQuotedStringVariable_Simple  <$> arbitrary
          )
        , ( 1
          , DoubleQuotedStringVariable_Complex <$> arbitrary
          )
        , ( 1
          , DoubleQuotedStringVariable_Simplex <$> arbitrary
          )
        ]

  {- shrink x = case x of
    DoubleQuotedStringVariable_Simple z ->
      map DoubleQuotedStringVariable_Simple $ shrink z
    DoubleQuotedStringVariable_Complex z ->
      map DoubleQuotedStringVariable_Complex $ shrink z
    DoubleQuotedStringVariable_Simplex z ->
      map DoubleQuotedStringVariable_Simplex $ shrink z -}





newtype DoubleQuotedUnicodeEscape = DoubleQuotedUnicodeEscape
  { unDoubleQuotedUnicodeEscape :: (Loc, String)
  } deriving (Eq, Show)

instance Render DoubleQuotedUnicodeEscape where
  render (DoubleQuotedUnicodeEscape (_,x)) = T.pack x

instance HasExtent DoubleQuotedUnicodeEscape where
  extentOf (DoubleQuotedUnicodeEscape (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (DoubleQuotedUnicodeEscape (_,x)) =
    (bumpChars x loc, DoubleQuotedUnicodeEscape (loc,x))

instance Arbitrary DoubleQuotedUnicodeEscape where
  arbitrary = scale (frac 1 8) $ do
    ds <- ((:)
            <$> (elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
                  , 'a', 'b', 'c', 'd', 'e', 'f'
                  , 'A', 'B', 'C', 'D', 'E', 'F' ])
            <*> (fmap (take 6) $ listOf $ elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
                  , 'a', 'b', 'c', 'd', 'e', 'f'
                  , 'A', 'B', 'C', 'D', 'E', 'F' ]))
    return $ DoubleQuotedUnicodeEscape (origin, "\\u{" ++ ds ++ "}")

  shrink (DoubleQuotedUnicodeEscape (l,x)) =
    map (curry DoubleQuotedUnicodeEscape l) $
      filter (\s -> (isPrefixOf "\\u{" s) && (s /= "\\u{}") && (isSuffixOf "}" s)) $
      shrink x





newtype DoubleQuotedHexEscape = DoubleQuotedHexEscape
  { unDoubleQuotedHexEscape :: (Loc, String)
  } deriving (Eq, Show)

instance Render DoubleQuotedHexEscape where
  render (DoubleQuotedHexEscape (_,x)) = T.pack x

instance HasExtent DoubleQuotedHexEscape where
  extentOf (DoubleQuotedHexEscape (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (DoubleQuotedHexEscape (_,x)) =
    (bumpChars x loc, DoubleQuotedHexEscape (loc,x))

instance Arbitrary DoubleQuotedHexEscape where
  arbitrary = do
    d <- elements [ 'x', 'X' ]
    ds <- ((:)
            <$> (elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
                  , 'a', 'b', 'c', 'd', 'e', 'f'
                  , 'A', 'B', 'C', 'D', 'E', 'F' ])
            <*> (fmap (take 4) $ listOf $ elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
                  , 'a', 'b', 'c', 'd', 'e', 'f'
                  , 'A', 'B', 'C', 'D', 'E', 'F' ]))
    return $ DoubleQuotedHexEscape (origin, "\\" ++ [d] ++ ds)

  shrink (DoubleQuotedHexEscape (l,x)) =
    map (curry DoubleQuotedHexEscape l) $
      filter (\s -> ((isPrefixOf "\\x" s) && (s /= "\\x")) || ((isPrefixOf "\\X" s) && (s /= "\\X"))) $
      shrink x





newtype DoubleQuotedOctalEscape = DoubleQuotedOctalEscape
  { unDoubleQuotedOctalEscape :: (Loc, String)
  } deriving (Eq, Show)

instance Render DoubleQuotedOctalEscape where
  render (DoubleQuotedOctalEscape (_,x)) = T.pack x

instance HasExtent DoubleQuotedOctalEscape where
  extentOf (DoubleQuotedOctalEscape (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (DoubleQuotedOctalEscape (_,x)) =
    (bumpChars x loc, DoubleQuotedOctalEscape (loc,x))

instance Arbitrary DoubleQuotedOctalEscape where
  arbitrary = do
    k <- elements [ 1, 2, 3 ]
    ds <- ((:)
            <$> (elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7' ])
            <*> (listOf $ elements
                  [ '0', '1', '2', '3', '4', '5', '6', '7' ]))
    return $ DoubleQuotedOctalEscape (origin, "\\" ++ take k ds)

  shrink (DoubleQuotedOctalEscape (l,x)) =
    map (curry DoubleQuotedOctalEscape l) $
      filter (\s -> (isPrefixOf "\\" s) && (s /= "\\")) $
      shrink x





newtype DoubleQuotedStringVariableName = DoubleQuotedStringVariableName
  { unDoubleQuotedStringVariableName :: (Loc, String)
  } deriving (Eq, Show)

instance Render DoubleQuotedStringVariableName where
  render (DoubleQuotedStringVariableName (_,x)) = T.pack x

instance HasExtent DoubleQuotedStringVariableName where
  extentOf (DoubleQuotedStringVariableName (loc,x)) =
    IsBetween loc (bumpChars (tail x) loc)

  shiftTo loc (DoubleQuotedStringVariableName (_,x)) =
    (bumpChars x loc, DoubleQuotedStringVariableName (loc,x))

instance Arbitrary DoubleQuotedStringVariableName where
  arbitrary = scale (frac 1 8) $ do
    ds <- ((:)
            <$> (elements $ concat
                  [ ['a'..'z'], ['A'..'Z'] ])
            <*> (fmap (take 8) $ listOf $ elements $ concat
                  [ ['a'..'z'], ['A'..'Z'], ['0'..'9'] ]))
    return $ DoubleQuotedStringVariableName (origin, "$" ++ ds)

  shrink (DoubleQuotedStringVariableName (l,x)) =
    map (curry DoubleQuotedStringVariableName l) $
      filter (\s -> (isPrefixOf "$" s) && (s /= "$")) $
      shrink x





data DoubleQuotedSimpleEscape
  = DoubleQuotedSimpleEscape_DoubleQuote    Loc
  | DoubleQuotedSimpleEscape_Backslash      Loc
  | DoubleQuotedSimpleEscape_Dollar         Loc
  | DoubleQuotedSimpleEscape_Escape         Loc
  | DoubleQuotedSimpleEscape_FormFeed       Loc
  | DoubleQuotedSimpleEscape_LineFeed       Loc
  | DoubleQuotedSimpleEscape_CarriageReturn Loc
  | DoubleQuotedSimpleEscape_HorizontalTab  Loc
  | DoubleQuotedSimpleEscape_VerticalTab    Loc
  deriving (Eq, Show)

instance Render DoubleQuotedSimpleEscape where
  render x = case x of
    DoubleQuotedSimpleEscape_DoubleQuote    _ -> T.pack "\\\""
    DoubleQuotedSimpleEscape_Backslash      _ -> T.pack "\\\\"
    DoubleQuotedSimpleEscape_Dollar         _ -> T.pack "\\$"
    DoubleQuotedSimpleEscape_Escape         _ -> T.pack "\\e"
    DoubleQuotedSimpleEscape_FormFeed       _ -> T.pack "\\f"
    DoubleQuotedSimpleEscape_LineFeed       _ -> T.pack "\\n"
    DoubleQuotedSimpleEscape_CarriageReturn _ -> T.pack "\\r"
    DoubleQuotedSimpleEscape_HorizontalTab  _ -> T.pack "\\t"
    DoubleQuotedSimpleEscape_VerticalTab    _ -> T.pack "\\v"

instance HasExtent DoubleQuotedSimpleEscape where
  extentOf x = case x of
    DoubleQuotedSimpleEscape_DoubleQuote    l -> IsLocated l
    DoubleQuotedSimpleEscape_Backslash      l -> IsLocated l
    DoubleQuotedSimpleEscape_Dollar         l -> IsLocated l
    DoubleQuotedSimpleEscape_Escape         l -> IsLocated l
    DoubleQuotedSimpleEscape_FormFeed       l -> IsLocated l
    DoubleQuotedSimpleEscape_LineFeed       l -> IsLocated l
    DoubleQuotedSimpleEscape_CarriageReturn l -> IsLocated l
    DoubleQuotedSimpleEscape_HorizontalTab  l -> IsLocated l
    DoubleQuotedSimpleEscape_VerticalTab    l -> IsLocated l

  shiftTo loc x = case x of
    DoubleQuotedSimpleEscape_DoubleQuote _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_DoubleQuote loc
      )
    DoubleQuotedSimpleEscape_Backslash _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_Backslash loc
      )
    DoubleQuotedSimpleEscape_Dollar _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_Dollar loc
      )
    DoubleQuotedSimpleEscape_Escape _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_Escape loc
      )
    DoubleQuotedSimpleEscape_FormFeed _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_FormFeed loc
      )
    DoubleQuotedSimpleEscape_LineFeed _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_LineFeed loc
      )
    DoubleQuotedSimpleEscape_CarriageReturn _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_CarriageReturn loc
      )
    DoubleQuotedSimpleEscape_HorizontalTab _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_HorizontalTab loc
      )
    DoubleQuotedSimpleEscape_VerticalTab _ ->
      ( bumpCol $ bumpCol loc
      , DoubleQuotedSimpleEscape_VerticalTab loc
      )

instance Arbitrary DoubleQuotedSimpleEscape where
  arbitrary = elements
    [ DoubleQuotedSimpleEscape_DoubleQuote    origin
    , DoubleQuotedSimpleEscape_Backslash      origin
    , DoubleQuotedSimpleEscape_Dollar         origin
    , DoubleQuotedSimpleEscape_Escape         origin
    , DoubleQuotedSimpleEscape_FormFeed       origin
    , DoubleQuotedSimpleEscape_LineFeed       origin
    , DoubleQuotedSimpleEscape_CarriageReturn origin
    , DoubleQuotedSimpleEscape_HorizontalTab  origin
    , DoubleQuotedSimpleEscape_VerticalTab    origin
    ]





data DoubleQuotedEscapeSequence
  = DoubleQuotedEscapeSequence_Simple  DoubleQuotedSimpleEscape
  | DoubleQuotedEscapeSequence_Octal   DoubleQuotedOctalEscape
  | DoubleQuotedEscapeSequence_Hex     DoubleQuotedHexEscape
  | DoubleQuotedEscapeSequence_Unicode DoubleQuotedUnicodeEscape
  deriving (Eq, Show)

instance Render DoubleQuotedEscapeSequence where
  render x = case x of
    DoubleQuotedEscapeSequence_Simple  z -> render z
    DoubleQuotedEscapeSequence_Octal   z -> render z
    DoubleQuotedEscapeSequence_Hex     z -> render z
    DoubleQuotedEscapeSequence_Unicode z -> render z

instance HasExtent DoubleQuotedEscapeSequence where
  extentOf x = case x of
    DoubleQuotedEscapeSequence_Simple  z -> extentOf z
    DoubleQuotedEscapeSequence_Octal   z -> extentOf z
    DoubleQuotedEscapeSequence_Hex     z -> extentOf z
    DoubleQuotedEscapeSequence_Unicode z -> extentOf z

  shiftTo loc x = case x of
    DoubleQuotedEscapeSequence_Simple z ->
      shiftToWrap DoubleQuotedEscapeSequence_Simple loc z
    DoubleQuotedEscapeSequence_Octal z ->
      shiftToWrap DoubleQuotedEscapeSequence_Octal loc z
    DoubleQuotedEscapeSequence_Hex z ->
      shiftToWrap DoubleQuotedEscapeSequence_Hex loc z
    DoubleQuotedEscapeSequence_Unicode z ->
      shiftToWrap DoubleQuotedEscapeSequence_Unicode loc z

instance Arbitrary DoubleQuotedEscapeSequence where
  arbitrary = scale (frac 1 3) $ oneof
    [ DoubleQuotedEscapeSequence_Simple  <$> arbitrary
    , DoubleQuotedEscapeSequence_Octal   <$> arbitrary
    , DoubleQuotedEscapeSequence_Hex     <$> arbitrary
    , DoubleQuotedEscapeSequence_Unicode <$> arbitrary
    ]

  shrink x = case x of
    DoubleQuotedEscapeSequence_Simple z ->
      map DoubleQuotedEscapeSequence_Simple $ shrink z
    DoubleQuotedEscapeSequence_Octal z ->
      map DoubleQuotedEscapeSequence_Octal $ shrink z
    DoubleQuotedEscapeSequence_Hex z ->
      map DoubleQuotedEscapeSequence_Hex $ shrink z
    DoubleQuotedEscapeSequence_Unicode z ->
      map DoubleQuotedEscapeSequence_Unicode $ shrink z





newtype DoubleQuotedString = DoubleQuotedString
  { unDoubleQuotedString ::
      ( Maybe BPrefix
      , DoubleQuote_
      , Maybe DoubleQuotedStringChars
      , DoubleQuote_
      )
  } deriving (Eq, Show)

instance Render DoubleQuotedString where
  render = render . unDoubleQuotedString

instance HasExtent DoubleQuotedString where
  extentOf = extentOf . unDoubleQuotedString

  shiftTo loc (DoubleQuotedString x) =
    shiftToWrap DoubleQuotedString loc x

instance Arbitrary DoubleQuotedString where
  arbitrary = scale (frac 1 2) $
    DoubleQuotedString <$> arbitrary

  -- shrink (DoubleQuotedString x) =
  --  map DoubleQuotedString $ shrink x





newtype DoubleQuotedStringChars = DoubleQuotedStringChars
  { unDoubleQuotedStringChars ::
      Seq DoubleQuotedStringChar
  } deriving (Eq, Show)

instance Render DoubleQuotedStringChars where
  render = render . unDoubleQuotedStringChars

instance HasExtent DoubleQuotedStringChars where
  extentOf = extentOf . unDoubleQuotedStringChars

  shiftTo loc (DoubleQuotedStringChars x) =
    shiftToWrap DoubleQuotedStringChars loc x

instance Arbitrary DoubleQuotedStringChars where
  arbitrary = scale (frac 1 2) $
    DoubleQuotedStringChars
      <$> arbitrarySeqWith (frac 1 8) seqPredicate_DoubleQuotedStringChars

  {- shrink (DoubleQuotedStringChars x) =
    map DoubleQuotedStringChars $
      filter (seqSatisfies seqPredicate_DoubleQuotedStringChars) $
      shrink x -}

seqPredicate_DoubleQuotedStringChars :: SeqPredicate DoubleQuotedStringChar
seqPredicate_DoubleQuotedStringChars =
  let
    isBackslash :: DoubleQuotedStringChar -> Bool
    isBackslash x = case x of
      DoubleQuotedStringChar_Char (_,'\\') -> True
      _ -> False

    isOpenBrace :: DoubleQuotedStringChar -> Bool
    isOpenBrace x = case x of
      DoubleQuotedStringChar_Char (_,'{') -> True
      _ -> False

    isEscape :: DoubleQuotedStringChar -> Bool
    isEscape x = case x of
      DoubleQuotedStringChar_Char (_,c) ->
        elem c
          [ '"', '$', 'e', 'f', 'n', 'r', 't', 'v', 'x', 'X', '\\'
          , '0', '1', '2', '3', '4', '5', '6', '7' ]

    isCodepointEscape :: DoubleQuotedStringChar -> Bool
    isCodepointEscape x = case x of
      DoubleQuotedStringChar_Escape z -> case z of
        DoubleQuotedEscapeSequence_Octal _ -> True
        DoubleQuotedEscapeSequence_Hex _ -> True
        _ -> False
      _ -> False

    isCodepointChar :: DoubleQuotedStringChar -> Bool
    isCodepointChar x = case x of
      DoubleQuotedStringChar_Char (_,c) -> elem c
        [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
        , 'a', 'b', 'c', 'd', 'e', 'f'
        , 'A', 'B', 'C', 'D', 'E', 'F'
        ]
      _ -> False

    isVariable :: DoubleQuotedStringChar -> Bool
    isVariable x = case x of
      DoubleQuotedStringChar_Variable _ -> True
      _ -> False

    isNameChar :: DoubleQuotedStringChar -> Bool
    isNameChar x = case x of
      DoubleQuotedStringChar_Char (_,c) -> elem c $ concat
        [ ['_'], ['0'..'9'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'] ]
      _ -> False
  in
    SeqPredicate
      { seqInitial  = []
      , seqAdjacent = [(isBackslash, not . isEscape)
                      ,(isCodepointEscape, not . isCodepointChar)
                      ,(isVariable, not . isNameChar)
                      ,(isOpenBrace, not . isVariable)]
      , seqBailout  = [isBackslash]
      , seqTerminal = []
      }





data DoubleQuotedStringChar
  = DoubleQuotedStringChar_Char     (Loc, Char)
  | DoubleQuotedStringChar_Escape   DoubleQuotedEscapeSequence
  | DoubleQuotedStringChar_Variable DoubleQuotedStringVariable
  deriving (Eq, Show)

instance Render DoubleQuotedStringChar where
  render x = case x of
    DoubleQuotedStringChar_Char     (_,c) -> T.singleton c
    DoubleQuotedStringChar_Escape   z     -> render z
    DoubleQuotedStringChar_Variable z     -> render z

instance HasExtent DoubleQuotedStringChar where
  extentOf x = case x of
    DoubleQuotedStringChar_Char     (l,_) -> IsLocated l
    DoubleQuotedStringChar_Escape   z     -> extentOf z
    DoubleQuotedStringChar_Variable z     -> extentOf z

  shiftTo loc x = case x of
    DoubleQuotedStringChar_Char (_,c) ->
      (bumpChar c loc, DoubleQuotedStringChar_Char (loc, c))
    DoubleQuotedStringChar_Escape z ->
      shiftToWrap DoubleQuotedStringChar_Escape loc z
    DoubleQuotedStringChar_Variable z ->
      shiftToWrap DoubleQuotedStringChar_Variable loc z

instance Arbitrary DoubleQuotedStringChar where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then do
        c <- arbitrary `suchThat` (\c -> not $ elem c [ '"', '\\', '$' ])
        return $ DoubleQuotedStringChar_Char (origin, c)
      else scale (frac 1 10) $ frequency
        [ ( max k 30
          , do
              c <- arbitrary `suchThat` (\c -> not $ elem c [ '"', '\\', '$' ])
              return $ DoubleQuotedStringChar_Char (origin, c)
          )
        , ( 1
          , DoubleQuotedStringChar_Escape   <$> arbitrary
          )
        , ( 1
          , DoubleQuotedStringChar_Variable <$> arbitrary
          )
        ]

  shrink x = case x of
    DoubleQuotedStringChar_Char z -> []
    DoubleQuotedStringChar_Escape z ->
      map DoubleQuotedStringChar_Escape $ shrink z
    DoubleQuotedStringChar_Variable z ->
      map DoubleQuotedStringChar_Variable $ shrink z





data CastType
  = CastType_Array   (Keyword Array_)
  | CastType_Binary  (Keyword Binary_)
  | CastType_Bool    (Keyword Bool_)
  | CastType_Boolean (Keyword Boolean_)
  | CastType_Double  (Keyword Double_)
  | CastType_Int     (Keyword Int_)
  | CastType_Integer (Keyword Integer_)
  | CastType_Float   (Keyword Float_)
  | CastType_Object  (Keyword Object_)
  | CastType_Real    (Keyword Real_)
  | CastType_String  (Keyword String_)
  | CastType_Unset   (Keyword Unset_)
  deriving (Eq, Show)

instance Render CastType where
  render x = case x of
    CastType_Array   z -> render z
    CastType_Binary  z -> render z
    CastType_Bool    z -> render z
    CastType_Boolean z -> render z
    CastType_Double  z -> render z
    CastType_Int     z -> render z
    CastType_Integer z -> render z
    CastType_Float   z -> render z
    CastType_Object  z -> render z
    CastType_Real    z -> render z
    CastType_String  z -> render z
    CastType_Unset   z -> render z

instance HasExtent CastType where
  extentOf x = case x of
    CastType_Array   z -> extentOf z
    CastType_Binary  z -> extentOf z
    CastType_Bool    z -> extentOf z
    CastType_Boolean z -> extentOf z
    CastType_Double  z -> extentOf z
    CastType_Int     z -> extentOf z
    CastType_Integer z -> extentOf z
    CastType_Float   z -> extentOf z
    CastType_Object  z -> extentOf z
    CastType_Real    z -> extentOf z
    CastType_String  z -> extentOf z
    CastType_Unset   z -> extentOf z

  shiftTo loc x = case x of
    CastType_Array z ->
      shiftToWrap CastType_Array loc z
    CastType_Binary z ->
      shiftToWrap CastType_Binary loc z
    CastType_Bool z ->
      shiftToWrap CastType_Bool loc z
    CastType_Boolean z ->
      shiftToWrap CastType_Boolean loc z
    CastType_Double z ->
      shiftToWrap CastType_Double loc z
    CastType_Int z ->
      shiftToWrap CastType_Int loc z
    CastType_Integer z ->
      shiftToWrap CastType_Integer loc z
    CastType_Float z ->
      shiftToWrap CastType_Float loc z
    CastType_Object z ->
      shiftToWrap CastType_Object loc z
    CastType_Real z ->
      shiftToWrap CastType_Real loc z
    CastType_String z ->
      shiftToWrap CastType_String loc z
    CastType_Unset z ->
      shiftToWrap CastType_Unset loc z

instance Arbitrary CastType where
  arbitrary = scale (frac 1 2) $ oneof
    [ CastType_Array   <$> arbitrary
    , CastType_Binary  <$> arbitrary
    , CastType_Bool    <$> arbitrary
    , CastType_Boolean <$> arbitrary
    , CastType_Double  <$> arbitrary
    , CastType_Int     <$> arbitrary
    , CastType_Integer <$> arbitrary
    , CastType_Float   <$> arbitrary
    , CastType_Object  <$> arbitrary
    , CastType_Real    <$> arbitrary
    , CastType_String  <$> arbitrary
    , CastType_Unset   <$> arbitrary
    ]

  shrink x = case x of
    CastType_Array z ->
      map CastType_Array $ shrink z
    CastType_Binary z ->
      map CastType_Binary $ shrink z
    CastType_Bool z ->
      map CastType_Bool $ shrink z
    CastType_Boolean z ->
      map CastType_Boolean $ shrink z
    CastType_Double z ->
      map CastType_Double $ shrink z
    CastType_Int z ->
      map CastType_Int $ shrink z
    CastType_Integer z ->
      map CastType_Integer $ shrink z
    CastType_Float z ->
      map CastType_Float $ shrink z
    CastType_Object z ->
      map CastType_Object $ shrink z
    CastType_Real z ->
      map CastType_Real $ shrink z
    CastType_String z ->
      map CastType_String $ shrink z
    CastType_Unset z ->
      map CastType_Unset $ shrink z





newtype CastExpression = CastExpression
  { unCastExpression ::
      ( Symbol OpenParen_
      , CastType
      , Symbol ClosedParen_
      , UnaryExpression
      )
  } deriving (Eq, Show)

instance Render CastExpression where
  render = render . unCastExpression

instance HasExtent CastExpression where
  extentOf = extentOf . unCastExpression

  shiftTo loc (CastExpression x) =
    shiftToWrap CastExpression loc x

instance Arbitrary CastExpression where
  arbitrary = scale (frac 1 4) $
    CastExpression <$> arbitrary

  shrink (CastExpression x) =
    map CastExpression $ shrink x





newtype InstanceofSubject = InstanceofSubject
  { unInstanceofSubject :: InstanceofExpression
  } deriving (Eq, Show)

instance Render InstanceofSubject where
  render = render . unInstanceofSubject

instance HasExtent InstanceofSubject where
  extentOf = extentOf . unInstanceofSubject

  shiftTo loc (InstanceofSubject x) =
    shiftToWrap InstanceofSubject loc x

instance Arbitrary InstanceofSubject where
  arbitrary = InstanceofSubject <$> arbitrary

  shrink (InstanceofSubject x) =
    map InstanceofSubject $ shrink x





newtype ScopedPropertyAccessExpression = ScopedPropertyAccessExpression
  { unScopedPropertyAccessExpression ::
      ( ScopeResolutionQualifier
      , Symbol DoubleColon_
      , SimpleVariable
      )
  } deriving (Eq, Show)

instance Render ScopedPropertyAccessExpression where
  render = render . unScopedPropertyAccessExpression

instance HasExtent ScopedPropertyAccessExpression where
  extentOf = extentOf . unScopedPropertyAccessExpression

  shiftTo loc (ScopedPropertyAccessExpression x) =
    shiftToWrap ScopedPropertyAccessExpression loc x

instance Arbitrary ScopedPropertyAccessExpression where
  arbitrary = scale (frac 1 3) $
    ScopedPropertyAccessExpression <$> arbitrary

  shrink (ScopedPropertyAccessExpression x) =
    map ScopedPropertyAccessExpression $ shrink x





data StringLiteral
  = StringLiteral_SingleQuoted SingleQuotedString
  | StringLiteral_DoubleQuoted DoubleQuotedString
  | StringLiteral_Heredoc      HeredocString
  | StringLiteral_Nowdoc       NowdocString
  deriving (Eq, Show)

instance Render StringLiteral where
  render x = case x of
    StringLiteral_SingleQuoted z -> render z
    StringLiteral_DoubleQuoted z -> render z
    StringLiteral_Heredoc      z -> render z
    StringLiteral_Nowdoc       z -> render z

instance HasExtent StringLiteral where
  extentOf x = case x of
    StringLiteral_SingleQuoted z -> extentOf z
    StringLiteral_DoubleQuoted z -> extentOf z
    StringLiteral_Heredoc      z -> extentOf z
    StringLiteral_Nowdoc       z -> extentOf z

  shiftTo loc x = case x of
    StringLiteral_SingleQuoted z ->
      shiftToWrap StringLiteral_SingleQuoted loc z
    StringLiteral_DoubleQuoted z ->
      shiftToWrap StringLiteral_DoubleQuoted loc z
    StringLiteral_Heredoc z ->
      shiftToWrap StringLiteral_Heredoc loc z
    StringLiteral_Nowdoc z ->
      shiftToWrap StringLiteral_Nowdoc loc z

instance Arbitrary StringLiteral where
  arbitrary = scale (frac 1 4) $ oneof
    [ StringLiteral_SingleQuoted <$> arbitrary
    , StringLiteral_DoubleQuoted <$> arbitrary
    , StringLiteral_Heredoc      <$> arbitrary
    , StringLiteral_Nowdoc       <$> arbitrary
    ]

  shrink x = case x of
    StringLiteral_SingleQuoted z ->
      map StringLiteral_SingleQuoted $ shrink z
    StringLiteral_DoubleQuoted z ->
      map StringLiteral_DoubleQuoted $ shrink z
    StringLiteral_Heredoc z ->
      map StringLiteral_Heredoc $ shrink z
    StringLiteral_Nowdoc z ->
      map StringLiteral_Nowdoc $ shrink z





data Literal
  = Literal_Int    IntegerLiteral
  | Literal_Float  FloatingLiteral
  | Literal_String StringLiteral
  deriving (Eq, Show)

instance Render Literal where
  render x = case x of
    Literal_Int    z -> render z
    Literal_Float  z -> render z
    Literal_String z -> render z

instance HasExtent Literal where
  extentOf x = case x of
    Literal_Int    z -> extentOf z
    Literal_Float  z -> extentOf z
    Literal_String z -> extentOf z

  shiftTo loc x = case x of
    Literal_Int z ->
      shiftToWrap Literal_Int loc z
    Literal_Float z ->
      shiftToWrap Literal_Float loc z
    Literal_String z ->
      shiftToWrap Literal_String loc z

instance Arbitrary Literal where
  arbitrary = scale (frac 1 2) $ oneof
    [ Literal_Int    <$> arbitrary
    , Literal_Float  <$> arbitrary
    , Literal_String <$> arbitrary
    ]

  shrink x = case x of
    Literal_Int z ->
      map Literal_Int $ shrink z
    Literal_Float z ->
      map Literal_Float $ shrink z
    Literal_String z ->
      map Literal_String $ shrink z





data DeclareDirective
  = DeclareDirective_Ticks
      ( Keyword Ticks_
      , Symbol Equal_
      , Literal
      )
  | DeclareDirective_Encoding
      ( Keyword Encoding_
      , Symbol Equal_
      , Literal
      )
  | DeclareDirective_StrictTypes
      ( Keyword StrictTypes_
      , Symbol Equal_
      , Literal
      )
  deriving (Eq, Show)

instance Render DeclareDirective where
  render x = case x of
    DeclareDirective_Ticks       z -> render z
    DeclareDirective_Encoding    z -> render z
    DeclareDirective_StrictTypes z -> render z

instance HasExtent DeclareDirective where
  extentOf x = case x of
    DeclareDirective_Ticks       z -> extentOf z
    DeclareDirective_Encoding    z -> extentOf z
    DeclareDirective_StrictTypes z -> extentOf z

  shiftTo loc x = case x of
    DeclareDirective_Ticks z ->
      shiftToWrap DeclareDirective_Ticks loc z
    DeclareDirective_Encoding z ->
      shiftToWrap DeclareDirective_Encoding loc z
    DeclareDirective_StrictTypes z ->
      shiftToWrap DeclareDirective_StrictTypes loc z

instance Arbitrary DeclareDirective where
  arbitrary = scale (frac 1 3) $ oneof
    [ DeclareDirective_Ticks       <$> arbitrary
    , DeclareDirective_Encoding    <$> arbitrary
    , DeclareDirective_StrictTypes <$> arbitrary
    ]

  shrink x = case x of
    DeclareDirective_Ticks z ->
      map DeclareDirective_Ticks $ shrink z
    DeclareDirective_Encoding z ->
      map DeclareDirective_Encoding $ shrink z
    DeclareDirective_StrictTypes z ->
      map DeclareDirective_StrictTypes $ shrink z





data HeredocStartIdentifier
  = HeredocStartIdentifier_Plain
        LocString
  | HeredocStartIdentifier_Quoted
      ( DoubleQuote_
      , LocString
      , DoubleQuote_
      )
  deriving (Eq, Show)

instance Render HeredocStartIdentifier where
  render x = case x of
    HeredocStartIdentifier_Plain  z -> render z
    HeredocStartIdentifier_Quoted z -> render z

instance HasExtent HeredocStartIdentifier where
  extentOf x = case x of
    HeredocStartIdentifier_Plain  z -> extentOf z
    HeredocStartIdentifier_Quoted z -> extentOf z

  shiftTo loc x = case x of
    HeredocStartIdentifier_Plain z ->
      shiftToWrap HeredocStartIdentifier_Plain loc z
    HeredocStartIdentifier_Quoted z ->
      shiftToWrap HeredocStartIdentifier_Quoted loc z

instance Arbitrary HeredocStartIdentifier where
  arbitrary = oneof
    [ HeredocStartIdentifier_Plain
        <$> arbitraryLocString
              (concat [ ['a'..'z'], ['A'..'Z'] ])
              (concat [ ['a'..'z'], ['A'..'Z'], ['0'..'9'] ])
    , curry3 HeredocStartIdentifier_Quoted
        <$> arbitrary
        <*> arbitraryLocString
              (concat [ ['a'..'z'], ['A'..'Z'] ])
              (concat [ ['a'..'z'], ['A'..'Z'], ['0'..'9'] ])
        <*> arbitrary
    ]

  shrink x = case x of
    HeredocStartIdentifier_Plain z ->
      map HeredocStartIdentifier_Plain $ shrink z
    HeredocStartIdentifier_Quoted z ->
      map HeredocStartIdentifier_Quoted $ shrink z





newtype HeredocEndIdentifier = HeredocEndIdentifier
  { unHeredocEndIdentifier :: LocString
  } deriving (Eq, Show)

instance Render HeredocEndIdentifier where
  render = render . unHeredocEndIdentifier

instance HasExtent HeredocEndIdentifier where
  extentOf = extentOf . unHeredocEndIdentifier

  shiftTo loc (HeredocEndIdentifier z) =
    shiftToWrap HeredocEndIdentifier loc z

instance Arbitrary HeredocEndIdentifier where
  arbitrary = HeredocEndIdentifier
    <$> arbitraryLocString
          (concat [ ['a'..'z'], ['A'..'Z'] ])
          (concat [ ['a'..'z'], ['A'..'Z'], ['0'..'9'] ])

  shrink (HeredocEndIdentifier x) =
    map HeredocEndIdentifier $ shrink x





data HeredocSimpleEscape
  = HeredocSimpleEscape_Backslash      Loc
  | HeredocSimpleEscape_Dollar         Loc
  | HeredocSimpleEscape_Escape         Loc
  | HeredocSimpleEscape_FormFeed       Loc
  | HeredocSimpleEscape_LineFeed       Loc
  | HeredocSimpleEscape_CarriageReturn Loc
  | HeredocSimpleEscape_HorizontalTab  Loc
  | HeredocSimpleEscape_VerticalTab    Loc
  deriving (Eq, Show)

instance Render HeredocSimpleEscape where
  render x = case x of
    HeredocSimpleEscape_Backslash      _ -> T.pack "\\\\"
    HeredocSimpleEscape_Dollar         _ -> T.pack "\\$"
    HeredocSimpleEscape_Escape         _ -> T.pack "\\e"
    HeredocSimpleEscape_FormFeed       _ -> T.pack "\\f"
    HeredocSimpleEscape_LineFeed       _ -> T.pack "\\n"
    HeredocSimpleEscape_CarriageReturn _ -> T.pack "\\r"
    HeredocSimpleEscape_HorizontalTab  _ -> T.pack "\\t"
    HeredocSimpleEscape_VerticalTab    _ -> T.pack "\\v"

instance HasExtent HeredocSimpleEscape where
  extentOf x = case x of
    HeredocSimpleEscape_Backslash      l -> IsLocated l
    HeredocSimpleEscape_Dollar         l -> IsLocated l
    HeredocSimpleEscape_Escape         l -> IsLocated l
    HeredocSimpleEscape_FormFeed       l -> IsLocated l
    HeredocSimpleEscape_LineFeed       l -> IsLocated l
    HeredocSimpleEscape_CarriageReturn l -> IsLocated l
    HeredocSimpleEscape_HorizontalTab  l -> IsLocated l
    HeredocSimpleEscape_VerticalTab    l -> IsLocated l

  shiftTo loc x = case x of
    HeredocSimpleEscape_Backslash _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_Backslash loc
      )
    HeredocSimpleEscape_Dollar _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_Dollar loc
      )
    HeredocSimpleEscape_Escape _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_Escape loc
      )
    HeredocSimpleEscape_FormFeed _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_FormFeed loc
      )
    HeredocSimpleEscape_LineFeed _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_LineFeed loc
      )
    HeredocSimpleEscape_CarriageReturn _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_CarriageReturn loc
      )
    HeredocSimpleEscape_HorizontalTab _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_HorizontalTab loc
      )
    HeredocSimpleEscape_VerticalTab _ ->
      ( bumpCol $ bumpCol loc
      , HeredocSimpleEscape_VerticalTab loc
      )

instance Arbitrary HeredocSimpleEscape where
  arbitrary = elements
    [ HeredocSimpleEscape_Backslash      origin
    , HeredocSimpleEscape_Dollar         origin
    , HeredocSimpleEscape_Escape         origin
    , HeredocSimpleEscape_FormFeed       origin
    , HeredocSimpleEscape_LineFeed       origin
    , HeredocSimpleEscape_CarriageReturn origin
    , HeredocSimpleEscape_HorizontalTab  origin
    , HeredocSimpleEscape_VerticalTab    origin
    ]





data HeredocEscapeSequence
  = HeredocEscapeSequence_Simple  HeredocSimpleEscape
  | HeredocEscapeSequence_Octal   DoubleQuotedOctalEscape
  | HeredocEscapeSequence_Hex     DoubleQuotedHexEscape
  | HeredocEscapeSequence_Unicode DoubleQuotedUnicodeEscape
  deriving (Eq, Show)

instance Render HeredocEscapeSequence where
  render x = case x of
    HeredocEscapeSequence_Simple  z -> render z
    HeredocEscapeSequence_Octal   z -> render z
    HeredocEscapeSequence_Hex     z -> render z
    HeredocEscapeSequence_Unicode z -> render z

instance HasExtent HeredocEscapeSequence where
  extentOf x = case x of
    HeredocEscapeSequence_Simple  z -> extentOf z
    HeredocEscapeSequence_Octal   z -> extentOf z
    HeredocEscapeSequence_Hex     z -> extentOf z
    HeredocEscapeSequence_Unicode z -> extentOf z

  shiftTo loc x = case x of
    HeredocEscapeSequence_Simple z ->
      shiftToWrap HeredocEscapeSequence_Simple loc z
    HeredocEscapeSequence_Octal z ->
      shiftToWrap HeredocEscapeSequence_Octal loc z
    HeredocEscapeSequence_Hex z ->
      shiftToWrap HeredocEscapeSequence_Hex loc z
    HeredocEscapeSequence_Unicode z ->
      shiftToWrap HeredocEscapeSequence_Unicode loc z

instance Arbitrary HeredocEscapeSequence where
  arbitrary = scale (frac 1 3) $ oneof
    [ HeredocEscapeSequence_Simple  <$> arbitrary
    , HeredocEscapeSequence_Octal   <$> arbitrary
    , HeredocEscapeSequence_Hex     <$> arbitrary
    , HeredocEscapeSequence_Unicode <$> arbitrary
    ]

  shrink x = case x of
    HeredocEscapeSequence_Simple z ->
      map HeredocEscapeSequence_Simple $ shrink z
    HeredocEscapeSequence_Octal z ->
      map HeredocEscapeSequence_Octal $ shrink z
    HeredocEscapeSequence_Hex z ->
      map HeredocEscapeSequence_Hex $ shrink z
    HeredocEscapeSequence_Unicode z ->
      map HeredocEscapeSequence_Unicode $ shrink z





data HeredocChar
  = HeredocChar_Char     (Loc, Char)
  | HeredocChar_Chars    (Loc, String)
  | HeredocChar_Escape   HeredocEscapeSequence
  | HeredocChar_Variable DoubleQuotedStringVariable
  deriving (Eq, Show)

instance Render HeredocChar where
  render x = case x of
    HeredocChar_Char     (_,c) -> T.singleton c
    HeredocChar_Chars    (_,s) -> T.pack s
    HeredocChar_Escape   z     -> render z
    HeredocChar_Variable z     -> render z

instance HasExtent HeredocChar where
  extentOf x = case x of
    HeredocChar_Char     (l,_) -> IsLocated l
    HeredocChar_Chars    (l,s) -> IsBetween l (bumpChars (tail s) l)
    HeredocChar_Escape   z     -> extentOf z
    HeredocChar_Variable z     -> extentOf z

  shiftTo loc x = case x of
    HeredocChar_Char (_,c) ->
      (bumpChar c loc, HeredocChar_Char (loc, c))
    HeredocChar_Chars (_,s) ->
      (bumpChars s loc, HeredocChar_Chars (loc, s))
    HeredocChar_Escape z ->
      shiftToWrap HeredocChar_Escape loc z
    HeredocChar_Variable z ->
      shiftToWrap HeredocChar_Variable loc z

instance Arbitrary HeredocChar where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then do
        c <- arbitrary `suchThat` (\c -> not $ elem c [ '"', '\\' ])
        return $ HeredocChar_Char (origin, c)
      else scale (frac 1 10) $ oneof
        [ do
            c <- arbitrary `suchThat` (\c -> not $ elem c [ '"', '\\' ])
            return $ HeredocChar_Char (origin, c)

        , do
            cs <- ((:)
              <$> (elements $ concat
                  [ ['a'..'z'], ['A'..'Z'] ])
              <*> (fmap (take 8) $ listOf $ elements $ concat
                  [ ['a'..'z'], ['A'..'Z'], ['0'..'9'] ]))
            return $ HeredocChar_Chars (origin, cs)

        , HeredocChar_Escape <$> arbitrary
        , HeredocChar_Variable <$> arbitrary
        ]

  shrink x = case x of
    HeredocChar_Char z ->
      map HeredocChar_Char $ shrink z
    HeredocChar_Chars z ->
      map HeredocChar_Chars $ shrink z
    HeredocChar_Escape z ->
      map HeredocChar_Escape $ shrink z
    HeredocChar_Variable z ->
      map HeredocChar_Variable $ shrink z





newtype HeredocChars = HeredocChars
  { unHeredocChars ::
      Seq HeredocChar
  } deriving (Eq, Show)

instance Render HeredocChars where
  render = render . unHeredocChars

instance HasExtent HeredocChars where
  extentOf = extentOf . unHeredocChars

  shiftTo loc (HeredocChars x) =
    shiftToWrap HeredocChars loc x

instance Arbitrary HeredocChars where
  arbitrary = HeredocChars
    <$> arbitrarySeqWith (frac 1 8) seqPredicate_HeredocChars

  shrink (HeredocChars x) =
    map HeredocChars $
      filter (seqSatisfies seqPredicate_HeredocChars) $
      shrink x

seqPredicate_HeredocChars :: SeqPredicate HeredocChar
seqPredicate_HeredocChars =
  let
    isBackslash :: HeredocChar -> Bool
    isBackslash x = case x of
      HeredocChar_Char (_,'\\') -> True
      _ -> False

    isEscape :: HeredocChar -> Bool
    isEscape x = case x of
      HeredocChar_Char (_,c) ->
        elem c
          [ '$', 'e', 'f', 'n', 'r', 't', 'v', 'x', 'X', '\\'
          , '0', '1', '2', '3', '4', '5', '6', '7' ]

    isNewLine :: HeredocChar -> Bool
    isNewLine x = case x of
      HeredocChar_Char (_,c) -> elem c [ '\n', '\r' ]
      _ -> False

    isNameChar :: HeredocChar -> Bool
    isNameChar x = case x of
      HeredocChar_Char (_,c) -> elem c $ concat
        [ ['a'..'z'], ['A'..'Z'] ]
      _ -> False
  in
    SeqPredicate
      { seqInitial  = [not . isNameChar]
      , seqAdjacent = [ (isBackslash, isEscape)
                      , (isNewLine,   not . isNameChar) ]
      , seqBailout  = [isBackslash]
      , seqTerminal = [not . isNewLine]
      }





newtype HeredocBody = HeredocBody
  { unHeredocBody ::
      ( Maybe HeredocChars
      , NewLine
      )
  } deriving (Eq, Show)

instance Render HeredocBody where
  render = render . unHeredocBody

instance HasExtent HeredocBody where
  extentOf = extentOf . unHeredocBody

  shiftTo loc (HeredocBody x) =
    shiftToWrap HeredocBody loc x

instance Arbitrary HeredocBody where
  arbitrary = HeredocBody <$> arbitrary

  shrink (HeredocBody x) =
    map HeredocBody $ shrink x





newtype HeredocString = HeredocString
  { unHeredocString ::
      ( Maybe BPrefix
      , TripleLess_
      , HeredocStartIdentifier
      , NewLine
      , Maybe HeredocBody
      , HeredocEndIdentifier
      , Maybe Semicolon_
      , NewLine
      )
  } deriving (Eq, Show)

instance Render HeredocString where
  render = render . unHeredocString

instance HasExtent HeredocString where
  extentOf = extentOf . unHeredocString

  shiftTo loc (HeredocString x) =
    shiftToWrap HeredocString loc x

instance Arbitrary HeredocString where
  arbitrary = scale (frac 1 8) $ do
    start <- arbitrary
    let
      end = case start of
        HeredocStartIdentifier_Plain x ->
          HeredocEndIdentifier x
        HeredocStartIdentifier_Quoted (_,x,_) ->
          HeredocEndIdentifier x
    curry8 HeredocString
      <$> arbitrary
      <*> arbitrary
      <*> pure start
      <*> (arbitrary `suchThat` (\x -> T.pack "\r" /= render x))
      <*> (arbitrary `suchThat` (\x -> not $ T.isInfixOf (render end) (render x)))
      <*> pure end
      <*> arbitrary
      <*> arbitrary

  {- shrink (HeredocString x) =
    let
      fixEnd (a0,a1,u,a3,a4,_,a6,a7) =
        case u of
          HeredocStartIdentifier_Plain x ->
            (a0,a1,u,a3,a4, HeredocEndIdentifier x,a6,a7)
          HeredocStartIdentifier_Quoted (_,x,_) ->
            (a0,a1,u,a3,a4, HeredocEndIdentifier x,a6,a7)

      checkBody (_,_,_,_,a4,a5,_,_) =
        not $ T.isInfixOf (render a5) (render a4)

      initNewline (_,_,_,a3,a4,_,_,_) =
        if T.pack "\r" == render a3
          then '\n' /= T.head (render a4)
          else True
    in
      map HeredocString $
        filter checkBody $
        filter initNewline $
        map fixEnd $
        shrink x -}





newtype NowdocString = NowdocString
  { unNowdocString ::
      ( Maybe BPrefix
      , TripleLess_
      , SingleQuote_
      , Name
      , SingleQuote_
      , NewLine
      , Maybe HeredocBody
      , Name
      , Maybe Semicolon_
      , NewLine
      )
  } deriving (Eq, Show)

instance Render NowdocString where
  render = render . unNowdocString

instance HasExtent NowdocString where
  extentOf = extentOf . unNowdocString

  shiftTo loc (NowdocString x) =
    shiftToWrap NowdocString loc x

instance Arbitrary NowdocString where
  arbitrary = scale (frac 1 11) $ do
    name <- arbitrary
    curry10 NowdocString
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure name
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure name
      <*> arbitrary
      <*> arbitrary

  shrink (NowdocString x) =
    let
      fixEnd (a0,a1,a2,u,a4,a5,a6,_,a8,a9) =
        (a0,a1,a2,u,a4,a5,a6,u,a8,a9)
    in
      map NowdocString $ map fixEnd $ shrink x





newtype DeclareStatement = DeclareStatement
  { unDeclareStatement ::
      ( Keyword Declare_
      , Symbol OpenParen_
      , DeclareDirective
      , Symbol ClosedParen_
      , DeclareStatementBody
      )
  } deriving (Eq, Show)

instance Render DeclareStatement where
  render = render . unDeclareStatement

instance HasExtent DeclareStatement where
  extentOf = extentOf . unDeclareStatement

  shiftTo loc (DeclareStatement x) =
    shiftToWrap DeclareStatement loc x

instance Arbitrary DeclareStatement where
  arbitrary = scale (frac 1 5) $
    DeclareStatement <$> arbitrary

  shrink (DeclareStatement x) =
    map DeclareStatement $ shrink x





data DeclareStatementBody
  = DeclareStatementBody_Plain
        Statement
  | DeclareStatementBody_Colon
      ( Symbol Colon_
      , StatementList
      , Keyword Enddeclare_
      , Symbol Semicolon_
      )
  | DeclareStatementBody_Empty
        (Symbol Semicolon_)
  deriving (Eq, Show)

instance Render DeclareStatementBody where
  render x = case x of
    DeclareStatementBody_Plain z -> render z
    DeclareStatementBody_Colon z -> render z
    DeclareStatementBody_Empty z -> render z

instance HasExtent DeclareStatementBody where
  extentOf x = case x of
    DeclareStatementBody_Plain z -> extentOf z
    DeclareStatementBody_Colon z -> extentOf z
    DeclareStatementBody_Empty z -> extentOf z

  shiftTo loc x = case x of
    DeclareStatementBody_Plain z ->
      shiftToWrap DeclareStatementBody_Plain loc z
    DeclareStatementBody_Colon z ->
      shiftToWrap DeclareStatementBody_Colon loc z
    DeclareStatementBody_Empty z ->
      shiftToWrap DeclareStatementBody_Empty loc z

instance Arbitrary DeclareStatementBody where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ DeclareStatementBody_Empty <$> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ DeclareStatementBody_Plain <$> arbitrary
        , DeclareStatementBody_Colon <$> arbitrary
        , DeclareStatementBody_Empty <$> arbitrary
        ]

  shrink x = case x of
    DeclareStatementBody_Plain z ->
      map DeclareStatementBody_Plain $ shrink z
    DeclareStatementBody_Colon z ->
      map DeclareStatementBody_Colon $ shrink z
    DeclareStatementBody_Empty z ->
      map DeclareStatementBody_Empty $ shrink z





data ForeachValue
  = ForeachValue_Expr
      ( Maybe (Symbol Amp_)
      , Expression
      )
  | ForeachValue_List
        ListIntrinsic
  deriving (Eq, Show)

instance Render ForeachValue where
  render x = case x of
    ForeachValue_Expr z -> render z
    ForeachValue_List z -> render z

instance HasExtent ForeachValue where
  extentOf x = case x of
    ForeachValue_Expr z -> extentOf z
    ForeachValue_List z -> extentOf z

  shiftTo loc x = case x of
    ForeachValue_Expr z ->
      shiftToWrap ForeachValue_Expr loc z
    ForeachValue_List z ->
      shiftToWrap ForeachValue_List loc z

instance Arbitrary ForeachValue where
  arbitrary = scale (frac 1 2) $ oneof
    [ ForeachValue_Expr <$> arbitrary
    , ForeachValue_List <$> arbitrary
    ]

  shrink x = case x of
    ForeachValue_Expr z ->
      map ForeachValue_Expr $ shrink z
    ForeachValue_List z ->
      map ForeachValue_List $ shrink z







newtype ForeachKey = ForeachKey
  { unForeachKey ::
      ( Expression
      , Symbol EqualGreater_
      )
  } deriving (Eq, Show)

instance Render ForeachKey where
  render = render . unForeachKey

instance HasExtent ForeachKey where
  extentOf = extentOf . unForeachKey

  shiftTo loc (ForeachKey x) =
    shiftToWrap ForeachKey loc x

instance Arbitrary ForeachKey where
  arbitrary = scale (frac 1 3) $
    ForeachKey <$> arbitrary

  shrink (ForeachKey x) =
    map ForeachKey $ shrink x





newtype ForeachStatement = ForeachStatement
  { unForeachStatement ::
      ( Keyword Foreach_
      , Symbol OpenParen_
      , ForeachCollectionName
      , Keyword As_
      , Maybe ForeachKey
      , ForeachValue
      , Symbol ClosedParen_
      , ForeachStatementBody
      )
  } deriving (Eq, Show)

instance Render ForeachStatement where
  render = render . unForeachStatement

instance HasExtent ForeachStatement where
  extentOf = extentOf . unForeachStatement

  shiftTo loc (ForeachStatement x) =
    shiftToWrap ForeachStatement loc x

instance Arbitrary ForeachStatement where
  arbitrary = scale (frac 1 9) $
    ForeachStatement <$> arbitrary

  shrink (ForeachStatement x) =
    map ForeachStatement $ shrink x





data ForeachStatementBody
  = ForeachStatementBody_Brace
        Statement
  | ForeachStatementBody_Colon
      ( Symbol Colon_
      , StatementList
      , Keyword Endforeach_
      , Symbol Semicolon_
      )
  deriving (Eq, Show)

instance Render ForeachStatementBody where
  render x = case x of
    ForeachStatementBody_Brace z -> render z
    ForeachStatementBody_Colon z -> render z

instance HasExtent ForeachStatementBody where
  extentOf x = case x of
    ForeachStatementBody_Brace z -> extentOf z
    ForeachStatementBody_Colon z -> extentOf z

  shiftTo loc x = case x of
    ForeachStatementBody_Brace z ->
      shiftToWrap ForeachStatementBody_Brace loc z
    ForeachStatementBody_Colon z ->
      shiftToWrap ForeachStatementBody_Colon loc z

instance Arbitrary ForeachStatementBody where
  arbitrary = scale (frac 1 4) $ oneof
    [ ForeachStatementBody_Brace <$> arbitrary
    , ForeachStatementBody_Colon <$> arbitrary
    ]

  shrink x = case x of
    ForeachStatementBody_Brace z ->
      map ForeachStatementBody_Brace $ shrink z
    ForeachStatementBody_Colon z ->
      map ForeachStatementBody_Colon $ shrink z





data IterationStatement
  = IterationStatement_While   WhileStatement
  | IterationStatement_Do      DoStatement
  | IterationStatement_For     ForStatement
  | IterationStatement_Foreach ForeachStatement
  deriving (Eq, Show)

instance Render IterationStatement where
  render x = case x of
    IterationStatement_While   z -> render z
    IterationStatement_Do      z -> render z
    IterationStatement_For     z -> render z
    IterationStatement_Foreach z -> render z

instance HasExtent IterationStatement where
  extentOf x = case x of
    IterationStatement_While   z -> extentOf z
    IterationStatement_Do      z -> extentOf z
    IterationStatement_For     z -> extentOf z
    IterationStatement_Foreach z -> extentOf z

  shiftTo loc x = case x of
    IterationStatement_While z ->
      shiftToWrap IterationStatement_While loc z
    IterationStatement_Do z ->
      shiftToWrap IterationStatement_Do loc z
    IterationStatement_For z ->
      shiftToWrap IterationStatement_For loc z
    IterationStatement_Foreach z ->
      shiftToWrap IterationStatement_Foreach loc z

instance Arbitrary IterationStatement where
  arbitrary = oneof
    [ IterationStatement_While   <$> arbitrary
    , IterationStatement_Do      <$> arbitrary
    , IterationStatement_For     <$> arbitrary
    , IterationStatement_Foreach <$> arbitrary
    ]

  shrink x = case x of
    IterationStatement_While z ->
      map IterationStatement_While $ shrink z
    IterationStatement_Do z ->
      map IterationStatement_Do $ shrink z
    IterationStatement_For z ->
      map IterationStatement_For $ shrink z
    IterationStatement_Foreach z ->
      map IterationStatement_Foreach $ shrink z





data ListOrVariable
  = ListOrVariable_List
        ListIntrinsic
  | ListOrVariable_Var
      ( Maybe (Symbol Amp_)
      , Variable
      )
  deriving (Eq, Show)

instance Render ListOrVariable where
  render x = case x of
    ListOrVariable_List z -> render z
    ListOrVariable_Var  z -> render z

instance HasExtent ListOrVariable where
  extentOf x = case x of
    ListOrVariable_List z -> extentOf z
    ListOrVariable_Var  z -> extentOf z

  shiftTo loc x = case x of
    ListOrVariable_List z ->
      shiftToWrap ListOrVariable_List loc z
    ListOrVariable_Var z ->
      shiftToWrap ListOrVariable_Var loc z

instance Arbitrary ListOrVariable where
  arbitrary = scale (frac 1 2) $ oneof
    [ ListOrVariable_List <$> arbitrary
    , ListOrVariable_Var  <$> arbitrary
    ]

  shrink x = case x of
    ListOrVariable_List z ->
      map ListOrVariable_List $ shrink z
    ListOrVariable_Var z ->
      map ListOrVariable_Var $ shrink z





data KeyedListExpressionList
  = KeyedListExpressionList_Head
      ( Expression
      , Symbol EqualGreater_
      , ListOrVariable
      )
  | KeyedListExpressionList_Snoc
      ( KeyedListExpressionList
      , Symbol Comma_
      , Expression
      , Symbol EqualGreater_
      , ListOrVariable
      )
  deriving (Eq, Show)

instance Render KeyedListExpressionList where
  render x = case x of
    KeyedListExpressionList_Head z -> render z
    KeyedListExpressionList_Snoc z -> render z

instance HasExtent KeyedListExpressionList where
  extentOf x = case x of
    KeyedListExpressionList_Head z -> extentOf z
    KeyedListExpressionList_Snoc z -> extentOf z

  shiftTo loc x = case x of
    KeyedListExpressionList_Head z ->
      shiftToWrap KeyedListExpressionList_Head loc z
    KeyedListExpressionList_Snoc z ->
      shiftToWrap KeyedListExpressionList_Snoc loc z

instance Arbitrary KeyedListExpressionList where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ KeyedListExpressionList_Head <$> arbitrary
        ]
      else scale (frac 1 5) $ oneof
        [ KeyedListExpressionList_Head <$> arbitrary
        , KeyedListExpressionList_Snoc <$> arbitrary
        ]

  shrink x = case x of
    KeyedListExpressionList_Head z ->
      map KeyedListExpressionList_Head $ shrink z
    KeyedListExpressionList_Snoc z ->
      map KeyedListExpressionList_Snoc $ shrink z





data UnkeyedListExpressionList
  = UnkeyedListExpressionList_Head
        ListOrVariable
  | UnkeyedListExpressionList_Comma
        (Symbol Comma_)
  | UnkeyedListExpressionList_Snoc
      ( UnkeyedListExpressionList
      , Symbol Comma_
      , Maybe ListOrVariable
      )
  deriving (Eq, Show)

instance Render UnkeyedListExpressionList where
  render x = case x of
    UnkeyedListExpressionList_Head  z -> render z
    UnkeyedListExpressionList_Comma z -> render z
    UnkeyedListExpressionList_Snoc  z -> render z

instance HasExtent UnkeyedListExpressionList where
  extentOf x = case x of
    UnkeyedListExpressionList_Head  z -> extentOf z
    UnkeyedListExpressionList_Comma z -> extentOf z
    UnkeyedListExpressionList_Snoc  z -> extentOf z

  shiftTo loc x = case x of
    UnkeyedListExpressionList_Head z ->
      shiftToWrap UnkeyedListExpressionList_Head loc z
    UnkeyedListExpressionList_Comma z ->
      shiftToWrap UnkeyedListExpressionList_Comma loc z
    UnkeyedListExpressionList_Snoc z ->
      shiftToWrap UnkeyedListExpressionList_Snoc loc z

instance Arbitrary UnkeyedListExpressionList where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ UnkeyedListExpressionList_Head  <$> arbitrary
        , UnkeyedListExpressionList_Comma <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ UnkeyedListExpressionList_Head  <$> arbitrary
        , UnkeyedListExpressionList_Comma <$> arbitrary
        , UnkeyedListExpressionList_Snoc  <$> arbitrary
        ]

  shrink x = case x of
    UnkeyedListExpressionList_Head z ->
      map UnkeyedListExpressionList_Head $ shrink z
    UnkeyedListExpressionList_Comma z ->
      map UnkeyedListExpressionList_Comma $ shrink z
    UnkeyedListExpressionList_Snoc z ->
      map UnkeyedListExpressionList_Snoc $ shrink z





data ListExpressionList
  = ListExpressionList_Unkeyed
        UnkeyedListExpressionList
  | ListExpressionList_Keyed
      ( KeyedListExpressionList
      , Maybe (Symbol Comma_)
      )
  deriving (Eq, Show)

instance Render ListExpressionList where
  render x = case x of
    ListExpressionList_Unkeyed z -> render z
    ListExpressionList_Keyed   z -> render z

instance HasExtent ListExpressionList where
  extentOf x = case x of
    ListExpressionList_Unkeyed z -> extentOf z
    ListExpressionList_Keyed   z -> extentOf z

  shiftTo loc x = case x of
    ListExpressionList_Unkeyed z ->
      shiftToWrap ListExpressionList_Unkeyed loc z
    ListExpressionList_Keyed z ->
      shiftToWrap ListExpressionList_Keyed loc z

instance Arbitrary ListExpressionList where
  arbitrary = scale (frac 1 2) $ oneof
    [ ListExpressionList_Unkeyed <$> arbitrary
    , ListExpressionList_Keyed   <$> arbitrary
    ]

  shrink x = case x of
    ListExpressionList_Unkeyed z ->
      map ListExpressionList_Unkeyed $ shrink z
    ListExpressionList_Keyed z ->
      map ListExpressionList_Keyed $ shrink z





newtype ListIntrinsic = ListIntrinsic
  { unListIntrinsic ::
      ( Keyword List_
      , Symbol OpenParen_
      , ListExpressionList
      , Symbol ClosedParen_
      )
  } deriving (Eq, Show)

instance Render ListIntrinsic where
  render = render . unListIntrinsic

instance HasExtent ListIntrinsic where
  extentOf = extentOf . unListIntrinsic

  shiftTo loc (ListIntrinsic x) =
    shiftToWrap ListIntrinsic loc x

instance Arbitrary ListIntrinsic where
  arbitrary = scale (frac 1 4) $
    ListIntrinsic <$> arbitrary

  shrink (ListIntrinsic x) =
    map ListIntrinsic $ shrink x





data SimpleAssignmentExpression
  = SimpleAssignmentExpression_Var
      ( Variable
      , Symbol Equal_
      , AssignmentExpression
      )
  | SimpleAssignmentExpression_List
      ( ListIntrinsic
      , Symbol Equal_
      , AssignmentExpression
      )
  deriving (Eq, Show)

instance Render SimpleAssignmentExpression where
  render x = case x of
    SimpleAssignmentExpression_Var  z -> render z
    SimpleAssignmentExpression_List z -> render z

instance HasExtent SimpleAssignmentExpression where
  extentOf x = case x of
    SimpleAssignmentExpression_Var  z -> extentOf z
    SimpleAssignmentExpression_List z -> extentOf z

  shiftTo loc x = case x of
    SimpleAssignmentExpression_Var z ->
      shiftToWrap SimpleAssignmentExpression_Var loc z
    SimpleAssignmentExpression_List z ->
      shiftToWrap SimpleAssignmentExpression_List loc z

instance Arbitrary SimpleAssignmentExpression where
  arbitrary = scale (frac 1 3) $ oneof
    [ SimpleAssignmentExpression_Var  <$> arbitrary
    , SimpleAssignmentExpression_List <$> arbitrary
    ]

  shrink x = case x of
    SimpleAssignmentExpression_Var z ->
      map SimpleAssignmentExpression_Var $ shrink z
    SimpleAssignmentExpression_List z ->
      map SimpleAssignmentExpression_List $ shrink z





newtype CompoundAssignmentExpression = CompoundAssignmentExpression
  { unCompoundAssignmentExpression ::
      ( Variable
      , CompoundAssignmentOperator
      , AssignmentExpression
      )
  } deriving (Eq, Show)

instance Render CompoundAssignmentExpression where
  render = render . unCompoundAssignmentExpression

instance HasExtent CompoundAssignmentExpression where
  extentOf = extentOf . unCompoundAssignmentExpression

  shiftTo loc (CompoundAssignmentExpression x) =
    shiftToWrap CompoundAssignmentExpression loc x

instance Arbitrary CompoundAssignmentExpression where
  arbitrary = scale (frac 1 3) $
    CompoundAssignmentExpression <$> arbitrary

  shrink (CompoundAssignmentExpression x) =
    map CompoundAssignmentExpression $ shrink x





data CompoundAssignmentOperator
  = CompoundAssignmentOperator_AstAst         (Symbol AstAstEqual_)
  | CompoundAssignmentOperator_Ast            (Symbol AstEqual_)
  | CompoundAssignmentOperator_Slash          (Symbol SlashEqual_)
  | CompoundAssignmentOperator_Percent        (Symbol PercentEqual_)
  | CompoundAssignmentOperator_Plus           (Symbol PlusEqual_)
  | CompoundAssignmentOperator_Minus          (Symbol MinusEqual_)
  | CompoundAssignmentOperator_Dot            (Symbol DotEqual_)
  | CompoundAssignmentOperator_LessLess       (Symbol DoubleLessEqual_)
  | CompoundAssignmentOperator_GreaterGreater (Symbol DoubleGreaterEqual_)
  | CompoundAssignmentOperator_Amp            (Symbol AmpEqual_)
  | CompoundAssignmentOperator_Caret          (Symbol CaretEqual_)
  | CompoundAssignmentOperator_Pipe           (Symbol PipeEqual_)
  deriving (Eq, Show)

instance Render CompoundAssignmentOperator where
  render x = case x of
    CompoundAssignmentOperator_AstAst         z -> render z
    CompoundAssignmentOperator_Ast            z -> render z
    CompoundAssignmentOperator_Slash          z -> render z
    CompoundAssignmentOperator_Percent        z -> render z
    CompoundAssignmentOperator_Plus           z -> render z
    CompoundAssignmentOperator_Minus          z -> render z
    CompoundAssignmentOperator_Dot            z -> render z
    CompoundAssignmentOperator_LessLess       z -> render z
    CompoundAssignmentOperator_GreaterGreater z -> render z
    CompoundAssignmentOperator_Amp            z -> render z
    CompoundAssignmentOperator_Caret          z -> render z
    CompoundAssignmentOperator_Pipe           z -> render z

instance HasExtent CompoundAssignmentOperator where
  extentOf x = case x of
    CompoundAssignmentOperator_AstAst         z -> extentOf z
    CompoundAssignmentOperator_Ast            z -> extentOf z
    CompoundAssignmentOperator_Slash          z -> extentOf z
    CompoundAssignmentOperator_Percent        z -> extentOf z
    CompoundAssignmentOperator_Plus           z -> extentOf z
    CompoundAssignmentOperator_Minus          z -> extentOf z
    CompoundAssignmentOperator_Dot            z -> extentOf z
    CompoundAssignmentOperator_LessLess       z -> extentOf z
    CompoundAssignmentOperator_GreaterGreater z -> extentOf z
    CompoundAssignmentOperator_Amp            z -> extentOf z
    CompoundAssignmentOperator_Caret          z -> extentOf z
    CompoundAssignmentOperator_Pipe           z -> extentOf z

  shiftTo loc x = case x of
    CompoundAssignmentOperator_AstAst z ->
      shiftToWrap CompoundAssignmentOperator_AstAst loc z
    CompoundAssignmentOperator_Ast z ->
      shiftToWrap CompoundAssignmentOperator_Ast loc z
    CompoundAssignmentOperator_Slash z ->
      shiftToWrap CompoundAssignmentOperator_Slash loc z
    CompoundAssignmentOperator_Percent z ->
      shiftToWrap CompoundAssignmentOperator_Percent loc z
    CompoundAssignmentOperator_Plus z ->
      shiftToWrap CompoundAssignmentOperator_Plus loc z
    CompoundAssignmentOperator_Minus z ->
      shiftToWrap CompoundAssignmentOperator_Minus loc z
    CompoundAssignmentOperator_Dot z ->
      shiftToWrap CompoundAssignmentOperator_Dot loc z
    CompoundAssignmentOperator_LessLess z ->
      shiftToWrap CompoundAssignmentOperator_LessLess loc z
    CompoundAssignmentOperator_GreaterGreater z ->
      shiftToWrap CompoundAssignmentOperator_GreaterGreater loc z
    CompoundAssignmentOperator_Amp z ->
      shiftToWrap CompoundAssignmentOperator_Amp loc z
    CompoundAssignmentOperator_Caret z ->
      shiftToWrap CompoundAssignmentOperator_Caret loc z
    CompoundAssignmentOperator_Pipe z ->
      shiftToWrap CompoundAssignmentOperator_Pipe loc z

instance Arbitrary CompoundAssignmentOperator where
  arbitrary = scale (frac 1 6) $ oneof
    [ CompoundAssignmentOperator_AstAst         <$> arbitrary
    , CompoundAssignmentOperator_Ast            <$> arbitrary
    , CompoundAssignmentOperator_Slash          <$> arbitrary
    , CompoundAssignmentOperator_Percent        <$> arbitrary
    , CompoundAssignmentOperator_Plus           <$> arbitrary
    , CompoundAssignmentOperator_Minus          <$> arbitrary
    , CompoundAssignmentOperator_Dot            <$> arbitrary
    , CompoundAssignmentOperator_LessLess       <$> arbitrary
    , CompoundAssignmentOperator_GreaterGreater <$> arbitrary
    , CompoundAssignmentOperator_Amp            <$> arbitrary
    , CompoundAssignmentOperator_Caret          <$> arbitrary
    , CompoundAssignmentOperator_Pipe           <$> arbitrary
    ]





data ScopedCallExpression
  = ScopedCallExpression_List
      ( ScopeResolutionQualifier
      , Symbol DoubleColon_
      , MemberName
      , Symbol OpenParen_
      , Maybe ArgumentExpressionList
      , Symbol ClosedParen_
      )
  | ScopedCallExpression_Comma
      ( ScopeResolutionQualifier
      , Symbol DoubleColon_
      , MemberName
      , Symbol OpenParen_
      , ArgumentExpressionList
      , Symbol Comma_
      , Symbol ClosedParen_
      )
  deriving (Eq, Show)

instance Render ScopedCallExpression where
  render x = case x of
    ScopedCallExpression_List z -> render z
    ScopedCallExpression_Comma z -> render z

instance HasExtent ScopedCallExpression where
  extentOf x = case x of
    ScopedCallExpression_List z -> extentOf z
    ScopedCallExpression_Comma z -> extentOf z

  shiftTo loc x = case x of
    ScopedCallExpression_List z ->
      shiftToWrap ScopedCallExpression_List loc z
    ScopedCallExpression_Comma z ->
      shiftToWrap ScopedCallExpression_Comma loc z

instance Arbitrary ScopedCallExpression where
  arbitrary = scale (frac 1 7) $ oneof
    [ ScopedCallExpression_List <$> arbitrary
    , ScopedCallExpression_Comma <$> arbitrary
    ]

  shrink x = case x of
    ScopedCallExpression_List z ->
      map ScopedCallExpression_List $ shrink z
    ScopedCallExpression_Comma z ->
      map ScopedCallExpression_Comma $ shrink z





newtype MemberCallExpression = MemberCallExpression
  { unMemberCallExpression ::
      ( DereferencableExpression
      , Symbol MinusGreater_
      , MemberName
      , Symbol OpenParen_
      , MemberCallExpressionArgs
      )
  } deriving (Eq, Show)

instance Render MemberCallExpression where
  render = render . unMemberCallExpression

instance HasExtent MemberCallExpression where
  extentOf = extentOf . unMemberCallExpression

  shiftTo loc (MemberCallExpression x) =
    shiftToWrap MemberCallExpression loc x

instance Arbitrary MemberCallExpression where
  arbitrary = scale (frac 1 5) $
    MemberCallExpression <$> arbitrary

  shrink (MemberCallExpression x) =
    map MemberCallExpression $ shrink x





data MemberCallExpressionArgs
  = MemberCallExpressionArgs_Empty
      ( Symbol ClosedParen_
      )
  | MemberCallExpressionArgs_List
      ( ArgumentExpressionList
      , Maybe (Symbol Comma_)
      , Symbol ClosedParen_
      )
  deriving (Eq, Show)

instance Render MemberCallExpressionArgs where
  render x = case x of
    MemberCallExpressionArgs_Empty z -> render z
    MemberCallExpressionArgs_List  z -> render z

instance HasExtent MemberCallExpressionArgs where
  extentOf x = case x of
    MemberCallExpressionArgs_Empty z -> extentOf z
    MemberCallExpressionArgs_List  z -> extentOf z

  shiftTo loc x = case x of
    MemberCallExpressionArgs_Empty z ->
      shiftToWrap MemberCallExpressionArgs_Empty loc z
    MemberCallExpressionArgs_List z ->
      shiftToWrap MemberCallExpressionArgs_List loc z

instance Arbitrary MemberCallExpressionArgs where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 1 8) $ oneof
        [ MemberCallExpressionArgs_Empty <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ MemberCallExpressionArgs_Empty <$> arbitrary
        , MemberCallExpressionArgs_List  <$> arbitrary
        ]

  shrink x = case x of
    MemberCallExpressionArgs_Empty z ->
      map MemberCallExpressionArgs_Empty $ shrink z
    MemberCallExpressionArgs_List z ->
      map MemberCallExpressionArgs_List $ shrink z





data UnaryOperator
  = UnaryOperator_Plus  (Symbol Plus_)
  | UnaryOperator_Minus (Symbol Minus_)
  | UnaryOperator_Tilde (Symbol Tilde_)
  deriving (Eq, Show)

instance Render UnaryOperator where
  render x = case x of
    UnaryOperator_Plus  z -> render z
    UnaryOperator_Minus z -> render z
    UnaryOperator_Tilde z -> render z

instance HasExtent UnaryOperator where
  extentOf x = case x of
    UnaryOperator_Plus  z -> extentOf z
    UnaryOperator_Minus z -> extentOf z
    UnaryOperator_Tilde z -> extentOf z

  shiftTo loc x = case x of
    UnaryOperator_Plus z ->
      shiftToWrap UnaryOperator_Plus loc z
    UnaryOperator_Minus z ->
      shiftToWrap UnaryOperator_Minus loc z
    UnaryOperator_Tilde z ->
      shiftToWrap UnaryOperator_Tilde loc z

instance Arbitrary UnaryOperator where
  arbitrary = scale (frac 1 4) $ oneof
    [ UnaryOperator_Plus  <$> arbitrary
    , UnaryOperator_Minus <$> arbitrary
    , UnaryOperator_Tilde <$> arbitrary
    ]

  shrink x = case x of
    UnaryOperator_Plus z ->
      map UnaryOperator_Plus $ shrink z
    UnaryOperator_Minus z ->
      map UnaryOperator_Minus $ shrink z
    UnaryOperator_Tilde z ->
      map UnaryOperator_Tilde $ shrink z





newtype UnaryOpExpression = UnaryOpExpression
  { unUnaryOpExpression ::
      ( UnaryOperator
      , UnaryExpression
      )
  } deriving (Eq, Show)

instance Render UnaryOpExpression where
  render = render . unUnaryOpExpression

instance HasExtent UnaryOpExpression where
  extentOf = extentOf . unUnaryOpExpression

  shiftTo loc (UnaryOpExpression x) =
    shiftToWrap UnaryOpExpression loc x

instance Arbitrary UnaryOpExpression where
  arbitrary = scale (frac 1 5) $
    UnaryOpExpression <$> arbitrary

  shrink (UnaryOpExpression x) =
    map UnaryOpExpression $ shrink x





data ArrayElementInitializer
  = ArrayElementInitializer_Plain
      ( Maybe (Symbol Amp_)
      , ElementValue
      )
  | ArrayElementInitializer_Arrow
      ( ElementKey
      , Symbol EqualGreater_
      , Maybe (Symbol Amp_)
      , ElementValue
      )
  deriving (Eq, Show)

instance Render ArrayElementInitializer where
  render x = case x of
    ArrayElementInitializer_Plain z -> render z
    ArrayElementInitializer_Arrow z -> render z

instance HasExtent ArrayElementInitializer where
  extentOf x = case x of
    ArrayElementInitializer_Plain z -> extentOf z
    ArrayElementInitializer_Arrow z -> extentOf z

  shiftTo loc x = case x of
    ArrayElementInitializer_Plain z ->
      shiftToWrap ArrayElementInitializer_Plain loc z
    ArrayElementInitializer_Arrow z ->
      shiftToWrap ArrayElementInitializer_Arrow loc z

instance Arbitrary ArrayElementInitializer where
  arbitrary = scale (frac 1 4) $ oneof
    [ ArrayElementInitializer_Plain <$> arbitrary
    , ArrayElementInitializer_Arrow <$> arbitrary
    ]

  shrink x = case x of
    ArrayElementInitializer_Plain z ->
      map ArrayElementInitializer_Plain $ shrink z
    ArrayElementInitializer_Arrow z ->
      map ArrayElementInitializer_Arrow $ shrink z





data ArrayInitializerList
  = ArrayInitializerList_Head
        ArrayElementInitializer
  | ArrayInitializerList_Cons
      ( ArrayElementInitializer
      , Symbol Comma_
      , ArrayInitializerList
      )
  deriving (Eq, Show)

instance Render ArrayInitializerList where
  render x = case x of
    ArrayInitializerList_Head z -> render z
    ArrayInitializerList_Cons z -> render z

instance HasExtent ArrayInitializerList where
  extentOf x = case x of
    ArrayInitializerList_Head z -> extentOf z
    ArrayInitializerList_Cons z -> extentOf z

  shiftTo loc x = case x of
    ArrayInitializerList_Head z ->
      shiftToWrap ArrayInitializerList_Head loc z
    ArrayInitializerList_Cons z ->
      shiftToWrap ArrayInitializerList_Cons loc z

instance Arbitrary ArrayInitializerList where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ArrayInitializerList_Head <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ ArrayInitializerList_Head <$> arbitrary
        , ArrayInitializerList_Cons <$> arbitrary
        ]

  shrink x = case x of
    ArrayInitializerList_Head z ->
      map ArrayInitializerList_Head $ shrink z
    ArrayInitializerList_Cons z ->
      map ArrayInitializerList_Cons $ shrink z





newtype ArrayInitializer = ArrayInitializer
  { unArrayInitializer ::
      ( ArrayInitializerList
      , Maybe (Symbol Comma_)
      )
  } deriving (Eq, Show)

instance Render ArrayInitializer where
  render = render . unArrayInitializer

instance HasExtent ArrayInitializer where
  extentOf = extentOf . unArrayInitializer

  shiftTo loc (ArrayInitializer x) =
    shiftToWrap ArrayInitializer loc x

instance Arbitrary ArrayInitializer where
  arbitrary = ArrayInitializer <$> arbitrary

  shrink (ArrayInitializer x) =
    map ArrayInitializer $ shrink x





data SubscriptExpression
  = SubscriptExpression_Brack
      ( DereferencableExpression
      , Symbol OpenBrack_
      , Maybe Expression
      , Symbol ClosedBrack_
      )
  | SubscriptExpression_Brace
      ( DereferencableExpression
      , Symbol OpenBrace_
      , Expression
      , Symbol ClosedBrace_
      )
  deriving (Eq, Show)

instance Render SubscriptExpression where
  render x = case x of
    SubscriptExpression_Brack z -> render z
    SubscriptExpression_Brace z -> render z

instance HasExtent SubscriptExpression where
  extentOf x = case x of
    SubscriptExpression_Brack z -> extentOf z
    SubscriptExpression_Brace z -> extentOf z

  shiftTo loc x = case x of
    SubscriptExpression_Brack z ->
      shiftToWrap SubscriptExpression_Brack loc z
    SubscriptExpression_Brace z ->
      shiftToWrap SubscriptExpression_Brace loc z

instance Arbitrary SubscriptExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ curry4 SubscriptExpression_Brack
            <$> arbitrary
            <*> arbitrary
            <*> pure Nothing
            <*> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ SubscriptExpression_Brack <$> arbitrary
        , SubscriptExpression_Brace <$> arbitrary
        ]

  shrink x = case x of
    SubscriptExpression_Brack z ->
      map SubscriptExpression_Brack $ shrink z
    SubscriptExpression_Brace z ->
      map SubscriptExpression_Brace $ shrink z





newtype ByrefAssignmentExpression = ByrefAssignmentExpression
  { unByrefAssignmentExpression ::
      ( Variable
      , Symbol Equal_
      , Symbol Amp_
      , Variable
      )
  } deriving (Eq, Show)

instance Render ByrefAssignmentExpression where
  render = render . unByrefAssignmentExpression

instance HasExtent ByrefAssignmentExpression where
  extentOf = extentOf . unByrefAssignmentExpression

  shiftTo loc (ByrefAssignmentExpression x) =
    shiftToWrap ByrefAssignmentExpression loc x

instance Arbitrary ByrefAssignmentExpression where
  arbitrary = scale (frac 1 5) $
    ByrefAssignmentExpression <$> arbitrary

  shrink (ByrefAssignmentExpression x) =
    map ByrefAssignmentExpression $ shrink x





data ArrayCreationExpression
  = ArrayCreationExpression_Array
      ( Keyword Array_
      , Symbol OpenParen_
      , Maybe ArrayInitializer
      , Symbol ClosedParen_
      )
  | ArrayCreationExpression_Brack
      ( Symbol OpenBrack_
      , Maybe ArrayInitializer
      , Symbol ClosedBrack_
      )
  deriving (Eq, Show)

instance Render ArrayCreationExpression where
  render x = case x of
    ArrayCreationExpression_Array z -> render z
    ArrayCreationExpression_Brack z -> render z

instance HasExtent ArrayCreationExpression where
  extentOf x = case x of
    ArrayCreationExpression_Array z -> extentOf z
    ArrayCreationExpression_Brack z -> extentOf z

  shiftTo loc x = case x of
    ArrayCreationExpression_Array z ->
      shiftToWrap ArrayCreationExpression_Array loc z
    ArrayCreationExpression_Brack z ->
      shiftToWrap ArrayCreationExpression_Brack loc z

instance Arbitrary ArrayCreationExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ curry3 ArrayCreationExpression_Brack
            <$> arbitrary
            <*> pure Nothing
            <*> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ ArrayCreationExpression_Array <$> arbitrary
        , ArrayCreationExpression_Brack <$> arbitrary
        ]

  shrink x = case x of
    ArrayCreationExpression_Array z ->
      map ArrayCreationExpression_Array $ shrink z
    ArrayCreationExpression_Brack z ->
      map ArrayCreationExpression_Brack $ shrink z





data FunctionCallExpression
  = FunctionCallExpression_NameList
      ( QualifiedName
      , Symbol OpenParen_
      , Maybe ArgumentExpressionList
      , Symbol ClosedParen_
      )
  | FunctionCallExpression_NameComma
      ( QualifiedName
      , Symbol OpenParen_
      , ArgumentExpressionList
      , Symbol Comma_
      , Symbol ClosedParen_
      )
  | FunctionCallExpression_CallList
      ( CallableExpression
      , Symbol OpenParen_
      , Maybe ArgumentExpressionList
      , Symbol ClosedParen_
      )
  | FunctionCallExpression_CallComma
      ( CallableExpression
      , Symbol OpenParen_
      , ArgumentExpressionList
      , Symbol Comma_
      , Symbol ClosedParen_
      )
  deriving (Eq, Show)

instance Render FunctionCallExpression where
  render x = case x of
    FunctionCallExpression_NameList z  -> render z
    FunctionCallExpression_NameComma z -> render z
    FunctionCallExpression_CallList z  -> render z
    FunctionCallExpression_CallComma z -> render z

instance HasExtent FunctionCallExpression where
  extentOf x = case x of
    FunctionCallExpression_NameList z  -> extentOf z
    FunctionCallExpression_NameComma z -> extentOf z
    FunctionCallExpression_CallList z  -> extentOf z
    FunctionCallExpression_CallComma z -> extentOf z

  shiftTo loc x = case x of
    FunctionCallExpression_NameList z ->
      shiftToWrap FunctionCallExpression_NameList loc z
    FunctionCallExpression_NameComma z ->
      shiftToWrap FunctionCallExpression_NameComma loc z
    FunctionCallExpression_CallList z ->
      shiftToWrap FunctionCallExpression_CallList loc z
    FunctionCallExpression_CallComma z ->
      shiftToWrap FunctionCallExpression_CallComma loc z

instance Arbitrary FunctionCallExpression where
  arbitrary = scale (frac 1 6) $ oneof
    [ FunctionCallExpression_NameList  <$> arbitrary
    , FunctionCallExpression_NameComma <$> arbitrary
    , FunctionCallExpression_CallList  <$> arbitrary
    , FunctionCallExpression_CallComma <$> arbitrary
    ]

  shrink x = case x of
    FunctionCallExpression_NameList z ->
      map FunctionCallExpression_NameList $ shrink z
    FunctionCallExpression_NameComma z ->
      map FunctionCallExpression_NameComma $ shrink z
    FunctionCallExpression_CallList z ->
      map FunctionCallExpression_CallList $ shrink z
    FunctionCallExpression_CallComma z ->
      map FunctionCallExpression_CallComma $ shrink z





data ObjectCreationExpression
  = ObjectCreationExpression_List
      ( Keyword New_
      , ClassTypeDesignator
      , Symbol OpenParen_
      , Maybe ArgumentExpressionList
      , Symbol ClosedParen_
      )
  | ObjectCreationExpression_Comma
      ( Keyword New_
      , ClassTypeDesignator
      , Symbol OpenParen_
      , ArgumentExpressionList
      , Maybe (Symbol Comma_)
      , Symbol ClosedParen_
      )
  | ObjectCreationExpression_Empty
      ( Keyword New_
      , ClassTypeDesignator
      )
  | ObjectCreationExpression_Args
      ( Keyword New_
      , Keyword Class_
      , Symbol OpenParen_
      , Maybe ArgumentExpressionList
      , Symbol ClosedParen_
      , Maybe ClassBaseClause
      , Maybe ClassInterfaceClause
      , Symbol OpenBrace_
      , Maybe ClassMemberDeclarations
      , Symbol ClosedBrace_
      )
  | ObjectCreationExpression_Plain
      ( Keyword New_
      , Keyword Class_
      , Maybe ClassBaseClause
      , Maybe ClassInterfaceClause
      , Symbol OpenBrace_
      , Maybe ClassMemberDeclarations
      , Symbol ClosedBrace_
      )
  deriving (Eq, Show)

instance Render ObjectCreationExpression where
  render x = case x of
    ObjectCreationExpression_List  z -> render z
    ObjectCreationExpression_Comma z -> render z
    ObjectCreationExpression_Empty z -> render z
    ObjectCreationExpression_Args  z -> render z
    ObjectCreationExpression_Plain z -> render z

instance HasExtent ObjectCreationExpression where
  extentOf x = case x of
    ObjectCreationExpression_List  z -> extentOf z
    ObjectCreationExpression_Comma z -> extentOf z
    ObjectCreationExpression_Empty z -> extentOf z
    ObjectCreationExpression_Args  z -> extentOf z
    ObjectCreationExpression_Plain z -> extentOf z

  shiftTo loc x = case x of
    ObjectCreationExpression_List z ->
      shiftToWrap ObjectCreationExpression_List loc z
    ObjectCreationExpression_Comma z ->
      shiftToWrap ObjectCreationExpression_Comma loc z
    ObjectCreationExpression_Empty z ->
      shiftToWrap ObjectCreationExpression_Empty loc z
    ObjectCreationExpression_Args z ->
      shiftToWrap ObjectCreationExpression_Args loc z
    ObjectCreationExpression_Plain z ->
      shiftToWrap ObjectCreationExpression_Plain loc z

instance Arbitrary ObjectCreationExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ ObjectCreationExpression_Empty <$> arbitrary
        ]
      else scale (frac 1 10) $ oneof
        [ ObjectCreationExpression_List  <$> arbitrary
        , ObjectCreationExpression_Comma <$> arbitrary
        , ObjectCreationExpression_Empty <$> arbitrary
        , ObjectCreationExpression_Args  <$> arbitrary
        , ObjectCreationExpression_Plain <$> arbitrary
        ]

  shrink x = case x of
    ObjectCreationExpression_List z ->
      map ObjectCreationExpression_List $ shrink z
    ObjectCreationExpression_Comma z ->
      map ObjectCreationExpression_Comma $ shrink z
    ObjectCreationExpression_Empty z ->
      map ObjectCreationExpression_Empty $ shrink z
    ObjectCreationExpression_Args z ->
      map ObjectCreationExpression_Args $ shrink z
    ObjectCreationExpression_Plain z ->
      map ObjectCreationExpression_Plain $ shrink z





data UseVariableNameList
  = UseVariableNameList_Head
      ( Maybe (Symbol Amp_)
      , VariableName
      )
  | UseVariableNameList_Snoc
      ( UseVariableNameList
      , Symbol Comma_
      , Maybe (Symbol Amp_)
      , VariableName
      )
  deriving (Eq, Show)

instance Render UseVariableNameList where
  render x = case x of
    UseVariableNameList_Head z -> render z
    UseVariableNameList_Snoc z -> render z

instance HasExtent UseVariableNameList where
  extentOf x = case x of
    UseVariableNameList_Head z -> extentOf z
    UseVariableNameList_Snoc z -> extentOf z

  shiftTo loc x = case x of
    UseVariableNameList_Head z ->
      shiftToWrap UseVariableNameList_Head loc z
    UseVariableNameList_Snoc z ->
      shiftToWrap UseVariableNameList_Snoc loc z

instance Arbitrary UseVariableNameList where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ UseVariableNameList_Head <$> arbitrary
        ]
      else scale (frac 1 4) $ oneof
        [ UseVariableNameList_Head <$> arbitrary
        , UseVariableNameList_Snoc <$> arbitrary
        ]

  shrink x = case x of
    UseVariableNameList_Head z ->
      map UseVariableNameList_Head $ shrink z
    UseVariableNameList_Snoc z ->
      map UseVariableNameList_Snoc $ shrink z





newtype AnonymousFunctionUseClause = AnonymousFunctionUseClause
  { unAnonymousFunctionUseClause ::
      ( Keyword Use_
      , Symbol OpenParen_
      , UseVariableNameList
      , Symbol ClosedParen_
      )
  } deriving (Eq, Show)

instance Render AnonymousFunctionUseClause where
  render = render . unAnonymousFunctionUseClause

instance HasExtent AnonymousFunctionUseClause where
  extentOf = extentOf . unAnonymousFunctionUseClause

  shiftTo loc (AnonymousFunctionUseClause x) =
    shiftToWrap AnonymousFunctionUseClause loc x

instance Arbitrary AnonymousFunctionUseClause where
  arbitrary = scale (frac 1 5) $
    AnonymousFunctionUseClause <$> arbitrary

  shrink (AnonymousFunctionUseClause x) =
    map AnonymousFunctionUseClause $ shrink x





newtype AnonymousFunctionCreationExpression = AnonymousFunctionCreationExpression
  { unAnonymousFunctionCreationExpression ::
      ( Maybe (Keyword Static_)
      , Keyword Function_
      , Maybe (Symbol Amp_)
      , Symbol OpenParen_
      , Maybe ParameterDeclarationList
      , Symbol ClosedParen_
      , Maybe AnonymousFunctionUseClause
      , Maybe ReturnType
      , CompoundStatement
      )
  } deriving (Eq, Show)

instance Render AnonymousFunctionCreationExpression where
  render = render . unAnonymousFunctionCreationExpression

instance HasExtent AnonymousFunctionCreationExpression where
  extentOf = extentOf . unAnonymousFunctionCreationExpression

  shiftTo loc (AnonymousFunctionCreationExpression x) =
    shiftToWrap AnonymousFunctionCreationExpression loc x

instance Arbitrary AnonymousFunctionCreationExpression where
  arbitrary = scale (frac 1 10) $
    AnonymousFunctionCreationExpression <$> arbitrary

  shrink (AnonymousFunctionCreationExpression x) =
    map AnonymousFunctionCreationExpression $ shrink x





data CallableExpression
  = CallableExpression_Var
        CallableVariable
  | CallableExpression_Nest
      ( Symbol OpenParen_
      , Expression
      , Symbol ClosedParen_
      )
  | CallableExpression_Array
        ArrayCreationExpression
  | CallableExpression_String
        StringLiteral
  deriving (Eq, Show)

instance Render CallableExpression where
  render x = case x of
    CallableExpression_Var    z -> render z
    CallableExpression_Nest   z -> render z
    CallableExpression_Array  z -> render z
    CallableExpression_String z -> render z

instance HasExtent CallableExpression where
  extentOf x = case x of
    CallableExpression_Var    z -> extentOf z
    CallableExpression_Nest   z -> extentOf z
    CallableExpression_Array  z -> extentOf z
    CallableExpression_String z -> extentOf z

  shiftTo loc x = case x of
    CallableExpression_Var z ->
      shiftToWrap CallableExpression_Var loc z
    CallableExpression_Nest z ->
      shiftToWrap CallableExpression_Nest loc z
    CallableExpression_Array z ->
      shiftToWrap CallableExpression_Array loc z
    CallableExpression_String z ->
      shiftToWrap CallableExpression_String loc z

instance Arbitrary CallableExpression where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ CallableExpression_Var    <$> arbitrary
        ]
      else scale (frac 1 3) $ oneof
        [ CallableExpression_Var    <$> arbitrary
        , CallableExpression_Nest   <$> arbitrary
        , CallableExpression_Array  <$> arbitrary
        , CallableExpression_String <$> arbitrary
        ]

  shrink x = case x of
    CallableExpression_Var z ->
      map CallableExpression_Var $ shrink z
    CallableExpression_Nest z ->
      map CallableExpression_Nest $ shrink z
    CallableExpression_Array z ->
      map CallableExpression_Array $ shrink z
    CallableExpression_String z ->
      map CallableExpression_String $ shrink z





newtype ShellCommandExpression = ShellCommandExpression
  { unShellCommandExpression ::
      ( Backtick_
      , Maybe DoubleQuotedStringChars
      , Backtick_
      )
  } deriving (Eq, Show)

instance Render ShellCommandExpression where
  render = render . unShellCommandExpression

instance HasExtent ShellCommandExpression where
  extentOf = extentOf . unShellCommandExpression

  shiftTo loc (ShellCommandExpression x) =
    shiftToWrap ShellCommandExpression loc x

instance Arbitrary ShellCommandExpression where
  arbitrary = scale (frac 1 8) $
    ShellCommandExpression <$> arbitrary

  shrink (ShellCommandExpression x) =
    map ShellCommandExpression $ shrink x





newtype YieldFromExpression = YieldFromExpression
  { unYieldFromExpression ::
      ( Keyword YieldFrom_
      , AssignmentExpression
      )
  } deriving (Eq, Show)

instance Render YieldFromExpression where
  render = render . unYieldFromExpression

instance HasExtent YieldFromExpression where
  extentOf = extentOf . unYieldFromExpression

  shiftTo loc (YieldFromExpression x) =
    shiftToWrap YieldFromExpression loc x

instance Arbitrary YieldFromExpression where
  arbitrary = scale (frac 1 2) $
    YieldFromExpression <$> arbitrary

  shrink (YieldFromExpression x) =
    map YieldFromExpression $ shrink x





newtype ExpressionStatement = ExpressionStatement
  { unExpressionStatement ::
      ( Maybe Expression
      , Symbol Semicolon_
      )
  } deriving (Eq, Show)

instance Render ExpressionStatement where
  render = render . unExpressionStatement

instance HasExtent ExpressionStatement where
  extentOf = extentOf . unExpressionStatement

  shiftTo loc (ExpressionStatement x) =
    shiftToWrap ExpressionStatement loc x

instance Arbitrary ExpressionStatement where
  arbitrary = do
    k <- getSize
    p <- arbitrary
    if (k <= 0) || p
      then scale (frac 3 4) $ oneof
        [ curry ExpressionStatement
            <$> pure Nothing
            <*> arbitrary
        ]
      else scale (frac 1 2) $ oneof
        [ ExpressionStatement <$> arbitrary
        ]

  shrink (ExpressionStatement x) =
    map ExpressionStatement $ shrink x
