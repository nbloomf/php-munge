module PHP.Parse.ConcreteSyntax.Name where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Gen (Gen, oneof, listOf, elements, suchThat)
import           Data.Char (toLower)

import PHP.Parse.Render
import PHP.Parse.Loc
import PHP.Parse.ConcreteSyntax.Seq
import PHP.Parse.ConcreteSyntax.Symbol
import PHP.Parse.ConcreteSyntax.Keyword
import PHP.Parse.ConcreteSyntax.WhiteSpaces
import Data.Tuple.Extra





data LocString = LocString
  { unLocString :: (Loc, String)
  } deriving (Eq, Show)

instance Render LocString where
  render (LocString (_,s)) = T.pack s

instance HasExtent LocString where
  extentOf (LocString (l,s)) =
    IsBetween l (bumpChars (tail s) l)

  shiftTo loc (LocString (_,s)) =
    (bumpChars s loc, LocString (loc,s))

instance Arbitrary LocString where
  arbitrary = LocString <$> arbitrary

  shrink (LocString z) =
    map LocString $ filter (\(_,s) -> s /= "") $ shrink z

arbitraryLocString :: [Char] -> [Char] -> Gen LocString
arbitraryLocString as bs = do
  c  <- elements as
  cs <- listOf $ elements bs
  return $ LocString (origin, c:cs)

shrinkLocString :: (String -> Bool) -> LocString -> [LocString]
shrinkLocString p (LocString z) =
  map LocString $ filter (\(_,x) -> (x /= "") && (p x)) $ shrink z





data Name = Name
  { unName ::
      ( (Loc, String)
      , Maybe WhiteSpaces
      )
  } deriving (Eq, Show)

instance Render Name where
  render (Name ((_,x),w)) = T.concat
    [ T.pack x, render w ]

instance HasExtent Name where
  extentOf (Name ((loc,_),w)) =
    (IsLocated loc) <> (extentOf w)

  shiftTo loc (Name ((_,x),w)) =
    let (loc', w') = shiftTo (bumpChars x loc) w
    in (loc', Name ((loc, x), w'))

instance Arbitrary Name where
  -- Need to avoid generating a keyword
  arbitrary = do
    x <- suchThat ((:)
          <$> (elements $ concat
                [ ['_'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'] ])
          <*> (listOf $ elements $ concat
                [ ['_'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'], ['0'..'9'] ])) $
          not . isKeyword
    w <- arbitrary
    return $ Name ((origin, x), w)

  shrink (Name ((l,x), w)) =
    map (\(z,v) -> Name ((l,z),v)) $
      filter (\(z,_) -> isName z) $
        shrink (x,w)

isName :: String -> Bool
isName str = case str of
  [] -> False
  c:cs -> start c && rest cs
    where
      start x = elem x $ concat [ ['_'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'] ]
      rest xs = all (\x -> elem x $ concat [ ['_'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'], ['0'..'9'] ]) xs

isKeyword :: String -> Bool
isKeyword str = elem (map toLower str)
  [ "abstract", "and", "array", "as", "break", "callable", "case"
  , "catch", "class", "clone", "const", "continue", "declare", "default"
  , "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor"
  , "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends"
  , "final", "finally", "for", "foreach", "function", "global", "goto"
  , "if", "implements", "include", "include_once", "instanceof", "insteadof"
  , "interface", "isset", "list", "namespace", "new", "or", "print", "private"
  , "protected", "public", "require", "require_once", "return", "static"
  , "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor"
  , "yield", "yield from"
  ]





data NameInString = NameInString
  { unNameInString :: (Loc, String)
  } deriving (Eq, Show)

instance Render NameInString where
  render (NameInString (_,x)) = T.pack x

instance HasExtent NameInString where
  extentOf (NameInString (loc,_)) = IsLocated loc

  shiftTo loc (NameInString (_,x)) =
    (bumpChars x loc, NameInString (loc, x))

instance Arbitrary NameInString where
  -- Need to avoid generating a keyword
  arbitrary = do
    x <- suchThat ((:)
          <$> (elements $ concat
                [ ['_'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'] ])
          <*> (listOf $ elements $ concat
                [ ['_'], ['a'..'z'], ['A'..'Z'], ['\x80'..'\xff'], ['0'..'9'] ])) $
          not . isKeyword
    return $ NameInString (origin, x)

  shrink (NameInString (l,x)) =
    map (\z -> NameInString (l,z)) $
      filter (\z -> isName z) $
        shrink x





newtype VariableName = VariableName
  { unVariableName ::
      ( Loc
      , Name
      )
  } deriving (Eq, Show)

instance Render VariableName where
  render (VariableName (_,x)) = T.concat
    [ T.singleton '$', render x ]

instance HasExtent VariableName where
  extentOf (VariableName (loc,x)) =
    (IsLocated loc) <> (extentOf x)

  shiftTo loc (VariableName (_,x)) =
    let (loc', x') = shiftTo (bumpCol loc) x
    in (loc', VariableName (loc, x'))

instance Arbitrary VariableName where
  arbitrary = curry VariableName origin
    <$> arbitrary

  shrink (VariableName (l,x)) =
    map (curry VariableName l) $ shrink x



newtype VariableNameInString = VariableNameInString
  { unVariableNameInString ::
      ( Loc
      , NameInString
      )
  } deriving (Eq, Show)

instance Render VariableNameInString where
  render (VariableNameInString (_,x)) = T.concat
    [ T.singleton '$', render x ]

instance HasExtent VariableNameInString where
  extentOf (VariableNameInString (loc,x)) =
    (IsLocated loc) <> (extentOf x)

  shiftTo loc (VariableNameInString (_,x)) =
    let (loc', x') = shiftTo (bumpCol loc) x
    in (loc', VariableNameInString (loc, x'))

instance Arbitrary VariableNameInString where
  arbitrary = curry VariableNameInString origin
    <$> arbitrary

  shrink (VariableNameInString (l,x)) =
    map (curry VariableNameInString l) $ shrink x





newtype NamespaceName = NamespaceName
  { unNamespaceName ::
      SeqSep (Symbol Slash_) Name
  } deriving (Eq, Show)

instance Render NamespaceName where
  render = render . unNamespaceName

instance HasExtent NamespaceName where
  extentOf = extentOf . unNamespaceName

  shiftTo loc (NamespaceName x) =
    shiftToWrap NamespaceName loc x

instance Arbitrary NamespaceName where
  arbitrary = NamespaceName <$> arbitrary

  shrink (NamespaceName x) =
    map NamespaceName $ shrink x





newtype NamespaceNameWithBackslash = NamespaceNameWithBackslash
  { unNamespaceNameWithBackslash ::
      ( NamespaceName
      , Symbol Backslash_
      )
  } deriving (Eq, Show)

instance Render NamespaceNameWithBackslash where
  render = render . unNamespaceNameWithBackslash

instance HasExtent NamespaceNameWithBackslash where
  extentOf = extentOf . unNamespaceNameWithBackslash

  shiftTo loc (NamespaceNameWithBackslash x) =
    shiftToWrap NamespaceNameWithBackslash loc x

instance Arbitrary NamespaceNameWithBackslash where
  arbitrary = NamespaceNameWithBackslash <$> arbitrary

  shrink (NamespaceNameWithBackslash x) =
    map NamespaceNameWithBackslash $ shrink x





data NamespaceNameAsAPrefix
  = NamespaceNameAsAPrefix_Root
      ( Symbol Backslash_
      , Maybe NamespaceNameWithBackslash
      )
  | NamespaceNameAsAPrefix_Relative
        NamespaceNameWithBackslash
  | NamespaceNameAsAPrefix_Namespace
      ( Keyword Namespace_
      , Symbol Backslash_
      , Maybe NamespaceNameWithBackslash
      )
  deriving (Eq, Show)

instance Render NamespaceNameAsAPrefix where
  render x = case x of
    NamespaceNameAsAPrefix_Root      z -> render z
    NamespaceNameAsAPrefix_Relative  z -> render z
    NamespaceNameAsAPrefix_Namespace z -> render z

instance HasExtent NamespaceNameAsAPrefix where
  extentOf x = case x of
    NamespaceNameAsAPrefix_Root      z -> extentOf z
    NamespaceNameAsAPrefix_Relative  z -> extentOf z
    NamespaceNameAsAPrefix_Namespace z -> extentOf z

  shiftTo loc x = case x of
    NamespaceNameAsAPrefix_Root z ->
      shiftToWrap NamespaceNameAsAPrefix_Root loc z
    NamespaceNameAsAPrefix_Relative z ->
      shiftToWrap NamespaceNameAsAPrefix_Relative loc z
    NamespaceNameAsAPrefix_Namespace z ->
      shiftToWrap NamespaceNameAsAPrefix_Namespace loc z

instance Arbitrary NamespaceNameAsAPrefix where
  arbitrary = oneof
    [ NamespaceNameAsAPrefix_Root      <$> arbitrary
    , NamespaceNameAsAPrefix_Relative  <$> arbitrary
    , NamespaceNameAsAPrefix_Namespace <$> arbitrary
    ]

  shrink x = case x of
    NamespaceNameAsAPrefix_Root z ->
      map NamespaceNameAsAPrefix_Root $ shrink z
    NamespaceNameAsAPrefix_Relative z ->
      map NamespaceNameAsAPrefix_Relative $ shrink z
    NamespaceNameAsAPrefix_Namespace z ->
      map NamespaceNameAsAPrefix_Namespace $ shrink z





newtype QualifiedName = QualifiedName
  { unQualifiedName ::
      ( Maybe NamespaceNameAsAPrefix
      , Name
      )
  } deriving (Eq, Show)

instance Render QualifiedName where
  render = render . unQualifiedName

instance HasExtent QualifiedName where
  extentOf = extentOf . unQualifiedName

  shiftTo loc (QualifiedName x) =
    shiftToWrap QualifiedName loc x

instance Arbitrary QualifiedName where
  arbitrary = QualifiedName <$> arbitrary

  shrink (QualifiedName x) =
    map QualifiedName $ shrink x





newtype ConstantAccessExpression = ConstantAccessExpression
  { unConstantAccessExpression :: QualifiedName
  } deriving (Eq, Show)

instance Render ConstantAccessExpression where
  render = render . unConstantAccessExpression

instance HasExtent ConstantAccessExpression where
  extentOf = extentOf . unConstantAccessExpression

  shiftTo loc (ConstantAccessExpression x) =
    shiftToWrap ConstantAccessExpression loc x

instance Arbitrary ConstantAccessExpression where
  arbitrary = ConstantAccessExpression <$> arbitrary

  shrink (ConstantAccessExpression x) =
    map ConstantAccessExpression $ shrink x
