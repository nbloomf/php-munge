module PHP.Parse.ConcreteSyntax.Symbol where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck (Arbitrary(..))

import PHP.Parse.Render
import PHP.Parse.Loc





newtype AmpAmp_ = AmpAmp_
  { unAmpAmp_ :: Loc
  } deriving (Eq, Show)

instance Render AmpAmp_ where
  render _ = T.pack "&&"

instance HasExtent AmpAmp_ where
  extentOf (AmpAmp_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (AmpAmp_ _) =
    ( bumpCol $ bumpCol loc, AmpAmp_ loc )

instance Arbitrary AmpAmp_ where
  arbitrary = AmpAmp_ <$> arbitrary





newtype AmpEqual_ = AmpEqual_
  { unAmpEqual_ :: Loc
  } deriving (Eq, Show)

instance Render AmpEqual_ where
  render _ = T.pack "&="

instance HasExtent AmpEqual_ where
  extentOf (AmpEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (AmpEqual_ _) =
    ( bumpCol $ bumpCol loc, AmpEqual_ loc )

instance Arbitrary AmpEqual_ where
  arbitrary = AmpEqual_ <$> arbitrary





newtype Amp_ = Amp_
  { unAmp_ :: Loc
  } deriving (Eq, Show)

instance Render Amp_ where
  render _ = T.singleton '&'

instance HasExtent Amp_ where
  extentOf (Amp_ loc) = IsLocated loc

  shiftTo loc (Amp_ _) =
    (bumpCol loc, Amp_ loc)

instance Arbitrary Amp_ where
  arbitrary = Amp_ <$> arbitrary





newtype Ast_ = Ast_
  { unAst_ :: Loc
  } deriving (Eq, Show)

instance Render Ast_ where
  render _ = T.singleton '*'

instance HasExtent Ast_ where
  extentOf (Ast_ loc) = IsLocated loc

  shiftTo loc (Ast_ _) =
    (bumpCol loc, Ast_ loc)

instance Arbitrary Ast_ where
  arbitrary = Ast_ <$> arbitrary





newtype AstAst_ = AstAst_
  { unAstAst_ :: Loc
  } deriving (Eq, Show)

instance Render AstAst_ where
  render _ = T.pack "**"

instance HasExtent AstAst_ where
  extentOf (AstAst_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (AstAst_ _) =
    ( bumpCol $ bumpCol loc, AstAst_ loc )

instance Arbitrary AstAst_ where
  arbitrary = AstAst_ <$> arbitrary





newtype AstAstEqual_ = AstAstEqual_
  { unAstAstEqual_ :: Loc
  } deriving (Eq, Show)

instance Render AstAstEqual_ where
  render _ = T.pack "**="

instance HasExtent AstAstEqual_ where
  extentOf (AstAstEqual_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (AstAstEqual_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, AstAstEqual_ loc )

instance Arbitrary AstAstEqual_ where
  arbitrary = AstAstEqual_ <$> arbitrary





newtype AstEqual_ = AstEqual_
  { unAstEqual_ :: Loc
  } deriving (Eq, Show)

instance Render AstEqual_ where
  render _ = T.pack "*="

instance HasExtent AstEqual_ where
  extentOf (AstEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (AstEqual_ _) =
    ( bumpCol $ bumpCol loc, AstEqual_ loc )

instance Arbitrary AstEqual_ where
  arbitrary = AstEqual_ <$> arbitrary





newtype AstSlash_ = AstSlash_
  { unAstSlash_ :: Loc
  } deriving (Eq, Show)

instance Render AstSlash_ where
  render _ = T.pack "*/"

instance HasExtent AstSlash_ where
  extentOf (AstSlash_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (AstSlash_ _) =
    ( bumpCol loc, AstSlash_ loc )

instance Arbitrary AstSlash_ where
  arbitrary = AstSlash_ <$> arbitrary





newtype At_ = At_
  { unAt_ :: Loc
  } deriving (Eq, Show)

instance Render At_ where
  render _ = T.singleton '@'

instance HasExtent At_ where
  extentOf (At_ loc) = IsLocated loc

  shiftTo loc (At_ _) =
    (bumpCol loc, At_ loc)

instance Arbitrary At_ where
  arbitrary = At_ <$> arbitrary





newtype Backslash_ = Backslash_
  { unBackslash_ :: Loc
  } deriving (Eq, Show)

instance Render Backslash_ where
  render _ = T.singleton '\\'

instance HasExtent Backslash_ where
  extentOf (Backslash_ loc) = IsLocated loc

  shiftTo loc (Backslash_ _) =
    ( bumpCol loc, Backslash_ loc )

instance Arbitrary Backslash_ where
  arbitrary = Backslash_ <$> arbitrary





newtype Backtick_ = Backtick_
  { unBacktick_ :: Loc
  } deriving (Eq, Show)

instance Render Backtick_ where
  render _ = T.singleton '`'

instance HasExtent Backtick_ where
  extentOf (Backtick_ loc) = IsLocated loc

  shiftTo loc (Backtick_ _) =
    (bumpCol loc, Backtick_ loc)

instance Arbitrary Backtick_ where
  arbitrary = Backtick_ <$> arbitrary





newtype Bang_ = Bang_
  { unBang_ :: Loc
  } deriving (Eq, Show)

instance Render Bang_ where
  render _ = T.singleton '!'

instance HasExtent Bang_ where
  extentOf (Bang_ loc) = IsLocated loc

  shiftTo loc (Bang_ _) =
    ( bumpCol loc, Bang_ loc )

instance Arbitrary Bang_ where
  arbitrary = Bang_ <$> arbitrary





newtype BangEqual_ = BangEqual_
  { unBangEqual_ :: Loc
  } deriving (Eq, Show)

instance Render BangEqual_ where
  render _ = T.pack "!="

instance HasExtent BangEqual_ where
  extentOf (BangEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (BangEqual_ _) =
    ( bumpCol $ bumpCol loc, BangEqual_ loc )

instance Arbitrary BangEqual_ where
  arbitrary = BangEqual_ <$> arbitrary





newtype BangEqualEqual_ = BangEqualEqual_
  { unBangEqualEqual_ :: Loc
  } deriving (Eq, Show)

instance Render BangEqualEqual_ where
  render _ = T.pack "!=="

instance HasExtent BangEqualEqual_ where
  extentOf (BangEqualEqual_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (BangEqualEqual_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, BangEqualEqual_ loc )

instance Arbitrary BangEqualEqual_ where
  arbitrary = BangEqualEqual_ <$> arbitrary





newtype Caret_ = Caret_
  { unCaret_ :: Loc
  } deriving (Eq, Show)

instance Render Caret_ where
  render _ = T.singleton '^'

instance HasExtent Caret_ where
  extentOf (Caret_ loc) = IsLocated loc

  shiftTo loc (Caret_ _) =
    ( bumpCol loc, Caret_ loc )

instance Arbitrary Caret_ where
  arbitrary = Caret_ <$> arbitrary





newtype CaretEqual_ = CaretEqual_
  { unCaretEqual_ :: Loc
  } deriving (Eq, Show)

instance Render CaretEqual_ where
  render _ = T.pack "^="

instance HasExtent CaretEqual_ where
  extentOf (CaretEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (CaretEqual_ _) =
    ( bumpCol $ bumpCol loc, CaretEqual_ loc )

instance Arbitrary CaretEqual_ where
  arbitrary = CaretEqual_ <$> arbitrary





newtype ClosedBrace_ = ClosedBrace_
  { unClosedBrace_ :: Loc
  } deriving (Eq, Show)

instance Render ClosedBrace_ where
  render _ = T.singleton '}'

instance HasExtent ClosedBrace_ where
  extentOf (ClosedBrace_ loc) = IsLocated loc

  shiftTo loc (ClosedBrace_ _) =
    ( bumpCol loc, ClosedBrace_ loc )

instance Arbitrary ClosedBrace_ where
  arbitrary = ClosedBrace_ <$> arbitrary





newtype ClosedBrack_ = ClosedBrack_
  { unClosedBrack_ :: Loc
  } deriving (Eq, Show)

instance Render ClosedBrack_ where
  render _ = T.singleton ']'

instance HasExtent ClosedBrack_ where
  extentOf (ClosedBrack_ loc) = IsLocated loc

  shiftTo loc (ClosedBrack_ _) =
    ( bumpCol loc, ClosedBrack_ loc )

instance Arbitrary ClosedBrack_ where
  arbitrary = ClosedBrack_ <$> arbitrary





newtype ClosedParen_ = ClosedParen_
  { unClosedParen_ :: Loc
  } deriving (Eq, Show)

instance Render ClosedParen_ where
  render _ = T.singleton ')'

instance HasExtent ClosedParen_ where
  extentOf (ClosedParen_ loc) = IsLocated loc

  shiftTo loc (ClosedParen_ _) =
    ( bumpCol loc, ClosedParen_ loc )

instance Arbitrary ClosedParen_ where
  arbitrary = ClosedParen_ <$> arbitrary





newtype Colon_ = Colon_
  { unColon_ :: Loc
  } deriving (Eq, Show)

instance Render Colon_ where
  render _ = T.singleton ':'

instance HasExtent Colon_ where
  extentOf (Colon_ loc) = IsLocated loc

  shiftTo loc (Colon_ _) =
    ( bumpCol loc, Colon_ loc )

instance Arbitrary Colon_ where
  arbitrary = Colon_ <$> arbitrary





newtype Comma_ = Comma_
  { unComma_ :: Loc
  } deriving (Eq, Show)

instance Render Comma_ where
  render _ = T.singleton ','

instance HasExtent Comma_ where
  extentOf (Comma_ loc) = IsLocated loc

  shiftTo loc (Comma_ _) =
    ( bumpCol loc, Comma_ loc )

instance Arbitrary Comma_ where
  arbitrary = Comma_ <$> arbitrary





newtype Dollar_ = Dollar_
  { unDollar_ :: Loc
  } deriving (Eq, Show)

instance Render Dollar_ where
  render _ = T.singleton '$'

instance HasExtent Dollar_ where
  extentOf (Dollar_ loc) = IsLocated loc

  shiftTo loc (Dollar_ _) =
    ( bumpCol loc, Dollar_ loc )

instance Arbitrary Dollar_ where
  arbitrary = Dollar_ <$> arbitrary





newtype DollarOpenBrace_ = DollarOpenBrace_
  { unDollarOpenBrace_ :: Loc
  } deriving (Eq, Show)

instance Render DollarOpenBrace_ where
  render _ = T.pack "${"

instance HasExtent DollarOpenBrace_ where
  extentOf (DollarOpenBrace_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DollarOpenBrace_ _) =
    ( bumpCol $ bumpCol loc, DollarOpenBrace_ loc )

instance Arbitrary DollarOpenBrace_ where
  arbitrary = DollarOpenBrace_ <$> arbitrary





newtype Dot_ = Dot_
  { unDot_ :: Loc
  } deriving (Eq, Show)

instance Render Dot_ where
  render _ = T.singleton '.'

instance HasExtent Dot_ where
  extentOf (Dot_ loc) = IsLocated loc

  shiftTo loc (Dot_ _) =
    ( bumpCol loc, Dot_ loc )

instance Arbitrary Dot_ where
  arbitrary = Dot_ <$> arbitrary





newtype DotEqual_ = DotEqual_
  { unDotEqual_ :: Loc
  } deriving (Eq, Show)

instance Render DotEqual_ where
  render _ = T.pack ".="

instance HasExtent DotEqual_ where
  extentOf (DotEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DotEqual_ _) =
    ( bumpCol $ bumpCol loc, DotEqual_ loc )

instance Arbitrary DotEqual_ where
  arbitrary = DotEqual_ <$> arbitrary





newtype DoubleColon_ = DoubleColon_
  { unDoubleColon_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleColon_ where
  render _ = T.pack "::"

instance HasExtent DoubleColon_ where
  extentOf (DoubleColon_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleColon_ _) =
    ( bumpCol $ bumpCol loc, DoubleColon_ loc )

instance Arbitrary DoubleColon_ where
  arbitrary = DoubleColon_ <$> arbitrary





newtype DoubleEqual_ = DoubleEqual_
  { unDoubleEqual_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleEqual_ where
  render _ = T.pack "=="

instance HasExtent DoubleEqual_ where
  extentOf (DoubleEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleEqual_ _) =
    ( bumpCol $ bumpCol loc, DoubleEqual_ loc )

instance Arbitrary DoubleEqual_ where
  arbitrary = DoubleEqual_ <$> arbitrary





newtype DoubleGreater_ = DoubleGreater_
  { unDoubleGreater_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleGreater_ where
  render _ = T.pack ">>"

instance HasExtent DoubleGreater_ where
  extentOf (DoubleGreater_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleGreater_ _) =
    ( bumpCol $ bumpCol loc, DoubleGreater_ loc )

instance Arbitrary DoubleGreater_ where
  arbitrary = DoubleGreater_ <$> arbitrary





newtype DoubleGreaterEqual_ = DoubleGreaterEqual_
  { unDoubleGreaterEqual_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleGreaterEqual_ where
  render _ = T.pack ">>="

instance HasExtent DoubleGreaterEqual_ where
  extentOf (DoubleGreaterEqual_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (DoubleGreaterEqual_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, DoubleGreaterEqual_ loc )

instance Arbitrary DoubleGreaterEqual_ where
  arbitrary = DoubleGreaterEqual_ <$> arbitrary





newtype DoubleLess_ = DoubleLess_
  { unDoubleLess_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleLess_ where
  render _ = T.pack "<<"

instance HasExtent DoubleLess_ where
  extentOf (DoubleLess_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleLess_ _) =
    ( bumpCol $ bumpCol loc, DoubleLess_ loc )

instance Arbitrary DoubleLess_ where
  arbitrary = DoubleLess_ <$> arbitrary





newtype DoubleLessEqual_ = DoubleLessEqual_
  { unDoubleLessEqual_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleLessEqual_ where
  render _ = T.pack "<<="

instance HasExtent DoubleLessEqual_ where
  extentOf (DoubleLessEqual_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (DoubleLessEqual_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, DoubleLessEqual_ loc )

instance Arbitrary DoubleLessEqual_ where
  arbitrary = DoubleLessEqual_ <$> arbitrary





newtype DoubleMinus_ = DoubleMinus_
  { unDoubleMinus_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleMinus_ where
  render _ = T.pack "--"

instance HasExtent DoubleMinus_ where
  extentOf (DoubleMinus_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleMinus_ _) =
    ( bumpCol $ bumpCol loc, DoubleMinus_ loc )

instance Arbitrary DoubleMinus_ where
  arbitrary = DoubleMinus_ <$> arbitrary





newtype DoublePipe_ = DoublePipe_
  { unDoublePipe_ :: Loc
  } deriving (Eq, Show)

instance Render DoublePipe_ where
  render _ = T.pack "||"

instance HasExtent DoublePipe_ where
  extentOf (DoublePipe_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoublePipe_ _) =
    ( bumpCol $ bumpCol loc, DoublePipe_ loc )

instance Arbitrary DoublePipe_ where
  arbitrary = DoublePipe_ <$> arbitrary





newtype DoublePlus_ = DoublePlus_
  { unDoublePlus_ :: Loc
  } deriving (Eq, Show)

instance Render DoublePlus_ where
  render _ = T.pack "++"

instance HasExtent DoublePlus_ where
  extentOf (DoublePlus_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoublePlus_ _) =
    ( bumpCol $ bumpCol loc, DoublePlus_ loc )

instance Arbitrary DoublePlus_ where
  arbitrary = DoublePlus_ <$> arbitrary





newtype DoubleQuestion_ = DoubleQuestion_
  { unDoubleQuestion_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleQuestion_ where
  render _ = T.pack "??"

instance HasExtent DoubleQuestion_ where
  extentOf (DoubleQuestion_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleQuestion_ _) =
    ( bumpCol $ bumpCol loc, DoubleQuestion_ loc )

instance Arbitrary DoubleQuestion_ where
  arbitrary = DoubleQuestion_ <$> arbitrary





newtype DoubleQuote_ = DoubleQuote_
  { unDoubleQuote_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleQuote_ where
  render _ = T.singleton '"'

instance HasExtent DoubleQuote_ where
  extentOf (DoubleQuote_ loc) = IsLocated loc

  shiftTo loc (DoubleQuote_ _) =
    ( bumpCol loc, DoubleQuote_ loc )

instance Arbitrary DoubleQuote_ where
  arbitrary = DoubleQuote_ <$> arbitrary





newtype DoubleSlash_ = DoubleSlash_
  { unDoubleSlash_ :: Loc
  } deriving (Eq, Show)

instance Render DoubleSlash_ where
  render _ = T.pack "//"

instance HasExtent DoubleSlash_ where
  extentOf (DoubleSlash_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (DoubleSlash_ _) =
    ( bumpCol $ bumpCol loc, DoubleSlash_ loc )

instance Arbitrary DoubleSlash_ where
  arbitrary = DoubleSlash_ <$> arbitrary





newtype Ellipsis_= Ellipsis_
  { unEllipsis_ :: Loc
  } deriving (Eq, Show)

instance Render Ellipsis_ where
  render _ = T.pack "..."

instance HasExtent Ellipsis_ where
  extentOf (Ellipsis_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (Ellipsis_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, Ellipsis_ loc )

instance Arbitrary Ellipsis_ where
  arbitrary = Ellipsis_ <$> arbitrary





newtype Equal_ = Equal_
  { unEqual_ :: Loc
  } deriving (Eq, Show)

instance Render Equal_ where
  render _ = T.singleton '='

instance HasExtent Equal_ where
  extentOf (Equal_ loc) = IsLocated loc

  shiftTo loc (Equal_ _) =
    ( bumpCol loc, Equal_ loc )

instance Arbitrary Equal_ where
  arbitrary = Equal_ <$> arbitrary





newtype EqualGreater_ = EqualGreater_
  { unEqualGreater_ :: Loc
  } deriving (Eq, Show)

instance Render EqualGreater_ where
  render _ = T.pack "=>"

instance HasExtent EqualGreater_ where
  extentOf (EqualGreater_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (EqualGreater_ _) =
    ( bumpCol $ bumpCol loc, EqualGreater_ loc )

instance Arbitrary EqualGreater_ where
  arbitrary = EqualGreater_ <$> arbitrary





newtype Greater_ = Greater_
  { unGreater_ :: Loc
  } deriving (Eq, Show)

instance Render Greater_ where
  render _ = T.singleton '>'

instance HasExtent Greater_ where
  extentOf (Greater_ loc) = IsLocated loc

  shiftTo loc (Greater_ _) =
    ( bumpCol loc, Greater_ loc )

instance Arbitrary Greater_ where
  arbitrary = Greater_ <$> arbitrary





newtype GreaterEqual_ = GreaterEqual_
  { unGreaterEqual_ :: Loc
  } deriving (Eq, Show)

instance Render GreaterEqual_ where
  render _ = T.pack ">="

instance HasExtent GreaterEqual_ where
  extentOf (GreaterEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (GreaterEqual_ _) =
    ( bumpCol $ bumpCol loc, GreaterEqual_ loc )

instance Arbitrary GreaterEqual_ where
  arbitrary = GreaterEqual_ <$> arbitrary





newtype LargeE_ = LargeE_
  { unLargeE_ :: Loc
  } deriving (Eq, Show)

instance Render LargeE_ where
  render _ = T.singleton 'E'

instance HasExtent LargeE_ where
  extentOf (LargeE_ loc) = IsLocated loc

  shiftTo loc (LargeE_ _) =
    ( bumpCol loc, LargeE_ loc )

instance Arbitrary LargeE_ where
  arbitrary = LargeE_ <$> arbitrary





newtype Less_ = Less_
  { unLess_ :: Loc
  } deriving (Eq, Show)

instance Render Less_ where
  render _ = T.singleton '<'

instance HasExtent Less_ where
  extentOf (Less_ loc) = IsLocated loc

  shiftTo loc (Less_ _) =
    ( bumpCol loc, Less_ loc )

instance Arbitrary Less_ where
  arbitrary = Less_ <$> arbitrary





newtype LessEqual_ = LessEqual_
  { unLessEqual_ :: Loc
  } deriving (Eq, Show)

instance Render LessEqual_ where
  render _ = T.pack "<="

instance HasExtent LessEqual_ where
  extentOf (LessEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (LessEqual_ _) =
    ( bumpCol $ bumpCol loc, LessEqual_ loc )

instance Arbitrary LessEqual_ where
  arbitrary = LessEqual_ <$> arbitrary





newtype LessGreater_ = LessGreater_
  { unLessGreater_ :: Loc
  } deriving (Eq, Show)

instance Render LessGreater_ where
  render _ = T.pack "<>"

instance HasExtent LessGreater_ where
  extentOf (LessGreater_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (LessGreater_ _) =
    ( bumpCol $ bumpCol loc, LessGreater_ loc )

instance Arbitrary LessGreater_ where
  arbitrary = LessGreater_ <$> arbitrary





newtype Minus_ = Minus_
  { unMinus_ :: Loc
  } deriving (Eq, Show)

instance Render Minus_ where
  render _ = T.singleton '-'

instance HasExtent Minus_ where
  extentOf (Minus_ loc) = IsLocated loc

  shiftTo loc (Minus_ _) =
    ( bumpCol loc, Minus_ loc )

instance Arbitrary Minus_ where
  arbitrary = Minus_ <$> arbitrary





newtype MinusEqual_ = MinusEqual_
  { unMinusEqual_ :: Loc
  } deriving (Eq, Show)

instance Render MinusEqual_ where
  render _ = T.pack "-="

instance HasExtent MinusEqual_ where
  extentOf (MinusEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (MinusEqual_ _) =
    ( bumpCol $ bumpCol loc, MinusEqual_ loc )

instance Arbitrary MinusEqual_ where
  arbitrary = MinusEqual_ <$> arbitrary





newtype MinusGreater_ = MinusGreater_
  { unMinusGreater_ :: Loc
  } deriving (Eq, Show)

instance Render MinusGreater_ where
  render _ = T.pack "->"

instance HasExtent MinusGreater_ where
  extentOf (MinusGreater_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (MinusGreater_ _) =
    ( bumpCol $ bumpCol loc, MinusGreater_ loc )

instance Arbitrary MinusGreater_ where
  arbitrary = MinusGreater_ <$> arbitrary





newtype Octothorpe_ = Octothorpe_
  { unOctothorpe_ :: Loc
  } deriving (Eq, Show)

instance Render Octothorpe_ where
  render _ = T.singleton '#'

instance HasExtent Octothorpe_ where
  extentOf (Octothorpe_ loc) = IsLocated loc

  shiftTo loc (Octothorpe_ _) =
    ( bumpCol loc, Octothorpe_ loc )

instance Arbitrary Octothorpe_ where
  arbitrary = Octothorpe_ <$> arbitrary





newtype OpenBrace_ = OpenBrace_
  { unOpenBrace_ :: Loc
  } deriving (Eq, Show)

instance Render OpenBrace_ where
  render _ = T.singleton '{'

instance HasExtent OpenBrace_ where
  extentOf (OpenBrace_ loc) = IsLocated loc

  shiftTo loc (OpenBrace_ _) =
    ( bumpCol loc, OpenBrace_ loc )

instance Arbitrary OpenBrace_ where
  arbitrary = OpenBrace_ <$> arbitrary





newtype OpenBrack_ = OpenBrack_
  { unOpenBrack_ :: Loc
  } deriving (Eq, Show)

instance Render OpenBrack_ where
  render _ = T.singleton '['

instance HasExtent OpenBrack_ where
  extentOf (OpenBrack_ loc) = IsLocated loc

  shiftTo loc (OpenBrack_ _) =
    ( bumpCol loc, OpenBrack_ loc )

instance Arbitrary OpenBrack_ where
  arbitrary = OpenBrack_ <$> arbitrary





newtype OpenParen_ = OpenParen_
  { unOpenParen_ :: Loc
  } deriving (Eq, Show)

instance Render OpenParen_ where
  render _ = T.singleton '('

instance HasExtent OpenParen_ where
  extentOf (OpenParen_ loc) = IsLocated loc

  shiftTo loc (OpenParen_ _) =
    ( bumpCol loc, OpenParen_ loc )

instance Arbitrary OpenParen_ where
  arbitrary = OpenParen_ <$> arbitrary





newtype Percent_ = Percent_
  { unPercent_ :: Loc
  } deriving (Eq, Show)

instance Render Percent_ where
  render _ = T.singleton '%'

instance HasExtent Percent_ where
  extentOf (Percent_ loc) = IsLocated loc

  shiftTo loc (Percent_ _) =
    ( bumpCol loc, Percent_ loc )

instance Arbitrary Percent_ where
  arbitrary = Percent_ <$> arbitrary





newtype PercentEqual_ = PercentEqual_
  { unPercentEqual_ :: Loc
  } deriving (Eq, Show)

instance Render PercentEqual_ where
  render _ = T.pack "%="

instance HasExtent PercentEqual_ where
  extentOf (PercentEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (PercentEqual_ _) =
    ( bumpCol $ bumpCol loc, PercentEqual_ loc )

instance Arbitrary PercentEqual_ where
  arbitrary = PercentEqual_ <$> arbitrary





newtype Pipe_ = Pipe_
  { unPipe_ :: Loc
  } deriving (Eq, Show)

instance Render Pipe_ where
  render _ = T.singleton '|'

instance HasExtent Pipe_ where
  extentOf (Pipe_ loc) = IsLocated loc

  shiftTo loc (Pipe_ _) =
    ( bumpCol loc, Pipe_ loc )

instance Arbitrary Pipe_ where
  arbitrary = Pipe_ <$> arbitrary





newtype PipeEqual_ = PipeEqual_
  { unPipeEqual_ :: Loc
  } deriving (Eq, Show)

instance Render PipeEqual_ where
  render _ = T.pack "|="

instance HasExtent PipeEqual_ where
  extentOf (PipeEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (PipeEqual_ _) =
    ( bumpCol $ bumpCol loc, PipeEqual_ loc )

instance Arbitrary PipeEqual_ where
  arbitrary = PipeEqual_ <$> arbitrary





newtype Plus_ = Plus_
  { unPlus_ :: Loc
  } deriving (Eq, Show)

instance Render Plus_ where
  render _ = T.singleton '+'

instance HasExtent Plus_ where
  extentOf (Plus_ loc) = IsLocated loc

  shiftTo loc (Plus_ _) =
    ( bumpCol loc, Plus_ loc )

instance Arbitrary Plus_ where
  arbitrary = Plus_ <$> arbitrary





newtype PlusEqual_ = PlusEqual_
  { unPlusEqual_ :: Loc
  } deriving (Eq, Show)

instance Render PlusEqual_ where
  render _ = T.pack "+="

instance HasExtent PlusEqual_ where
  extentOf (PlusEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (PlusEqual_ _) =
    ( bumpCol $ bumpCol loc, PlusEqual_ loc )

instance Arbitrary PlusEqual_ where
  arbitrary = PlusEqual_ <$> arbitrary





newtype Question_ = Question_
  { unQuestion_ :: Loc
  } deriving (Eq, Show)

instance Render Question_ where
  render _ = T.singleton '?'

instance HasExtent Question_ where
  extentOf (Question_ loc) = IsLocated loc

  shiftTo loc (Question_ _) =
    ( bumpCol loc, Question_ loc )

instance Arbitrary Question_ where
  arbitrary = Question_ <$> arbitrary





newtype SmallE_ = SmallE_
  { unSmallE_ :: Loc
  } deriving (Eq, Show)

instance Render SmallE_ where
  render _ = T.singleton 'e'

instance HasExtent SmallE_ where
  extentOf (SmallE_ loc) = IsLocated loc

  shiftTo loc (SmallE_ _) =
    ( bumpCol loc, SmallE_ loc )

instance Arbitrary SmallE_ where
  arbitrary = SmallE_ <$> arbitrary





newtype Semicolon_ = Semicolon_
  { unSemicolon_ :: Loc
  } deriving (Eq, Show)

instance Render Semicolon_ where
  render _ = T.singleton ';'

instance HasExtent Semicolon_ where
  extentOf (Semicolon_ loc) = IsLocated loc

  shiftTo loc (Semicolon_ _) =
    ( bumpCol loc, Semicolon_ loc )

instance Arbitrary Semicolon_ where
  arbitrary = Semicolon_ <$> arbitrary





newtype SingleQuote_ = SingleQuote_
  { unSingleQuote_ :: Loc
  } deriving (Eq, Show)

instance Render SingleQuote_ where
  render _ = T.singleton '\''

instance HasExtent SingleQuote_ where
  extentOf (SingleQuote_ loc) = IsLocated loc

  shiftTo loc (SingleQuote_ _) =
    ( bumpCol loc, SingleQuote_ loc )

instance Arbitrary SingleQuote_ where
  arbitrary = SingleQuote_ <$> arbitrary





newtype Slash_ = Slash_
  { unSlash_ :: Loc
  } deriving (Eq, Show)

instance Render Slash_ where
  render _ = T.singleton '/'

instance HasExtent Slash_ where
  extentOf (Slash_ loc) = IsLocated loc

  shiftTo loc (Slash_ _) =
    ( bumpCol loc, Slash_ loc )

instance Arbitrary Slash_ where
  arbitrary = Slash_ <$> arbitrary





newtype SlashAst_ = SlashAst_
  { unSlashAst_ :: Loc
  } deriving (Eq, Show)

instance Render SlashAst_ where
  render _ = T.pack "/*"

instance HasExtent SlashAst_ where
  extentOf (SlashAst_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (SlashAst_ _) =
    ( bumpCol $ bumpCol loc, SlashAst_ loc )

instance Arbitrary SlashAst_ where
  arbitrary = SlashAst_ <$> arbitrary





newtype SlashEqual_ = SlashEqual_
  { unSlashEqual_ :: Loc
  } deriving (Eq, Show)

instance Render SlashEqual_ where
  render _ = T.pack "/="

instance HasExtent SlashEqual_ where
  extentOf (SlashEqual_ loc) =
    IsBetween loc (bumpCol loc)

  shiftTo loc (SlashEqual_ _) =
    ( bumpCol $ bumpCol loc, SlashEqual_ loc )

instance Arbitrary SlashEqual_ where
  arbitrary = SlashEqual_ <$> arbitrary





newtype Spaceship_ = Spaceship_
  { unSpaceship_ :: Loc
  } deriving (Eq, Show)

instance Render Spaceship_ where
  render _ = T.pack "<=>"

instance HasExtent Spaceship_ where
  extentOf (Spaceship_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (Spaceship_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, Spaceship_ loc )

instance Arbitrary Spaceship_ where
  arbitrary = Spaceship_ <$> arbitrary





newtype Tilde_ = Tilde_
  { unTilde_ :: Loc
  } deriving (Eq, Show)

instance Render Tilde_ where
  render _ = T.singleton '~'

instance HasExtent Tilde_ where
  extentOf (Tilde_ loc) = IsLocated loc

  shiftTo loc (Tilde_ _) =
    ( bumpCol loc, Tilde_ loc )

instance Arbitrary Tilde_ where
  arbitrary = Tilde_ <$> arbitrary





newtype TripleEqual_ = TripleEqual_
  { unTripleEqual_ :: Loc
  } deriving (Eq, Show)

instance Render TripleEqual_ where
  render _ = T.pack "==="

instance HasExtent TripleEqual_ where
  extentOf (TripleEqual_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (TripleEqual_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, TripleEqual_ loc )

instance Arbitrary TripleEqual_ where
  arbitrary = TripleEqual_ <$> arbitrary





newtype TripleLess_ = TripleLess_
  { unTripleLess_ :: Loc
  } deriving (Eq, Show)

instance Render TripleLess_ where
  render _ = T.pack "<<<"

instance HasExtent TripleLess_ where
  extentOf (TripleLess_ loc) =
    IsBetween loc (bumpCol $ bumpCol loc)

  shiftTo loc (TripleLess_ _) =
    ( bumpCol $ bumpCol $ bumpCol loc, TripleLess_ loc )

instance Arbitrary TripleLess_ where
  arbitrary = TripleLess_ <$> arbitrary
