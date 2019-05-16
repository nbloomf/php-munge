module PHP.Parse.ConcreteSyntax.Keyword where

import qualified Data.Text.Lazy as T
import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Gen (Gen, elements)

import PHP.Parse.Render
import PHP.Parse.Loc





newtype Abstract_ = Abstract_
  { unAbstract_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Abstract_ where
  render (Abstract_ (_,w)) = T.pack w

instance HasExtent Abstract_ where
  extentOf (Abstract_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Abstract_ (_,w)) =
    (bumpChars w loc, Abstract_ (loc,w))

instance Arbitrary Abstract_ where
  arbitrary = curry Abstract_
    <$> pure origin
    <*> arbitraryCase "abstract"





newtype And_ = And_
  { unAnd_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render And_ where
  render (And_ (_,w)) = T.pack w

instance HasExtent And_ where
  extentOf (And_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (And_ (_,w)) =
    (bumpChars w loc, And_ (loc,w))

instance Arbitrary And_ where
  arbitrary = curry And_
    <$> pure origin
    <*> arbitraryCase "and"





newtype Array_ = Array_
  { unArray_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Array_ where
  render (Array_ (_,w)) = T.pack w

instance HasExtent Array_ where
  extentOf (Array_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Array_ (_,w)) =
    (bumpChars w loc, Array_ (loc,w))

instance Arbitrary Array_ where
  arbitrary = curry Array_
    <$> pure origin
    <*> arbitraryCase "array"





newtype As_ = As_
  { unAs_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render As_ where
  render (As_ (_,w)) = T.pack w

instance HasExtent As_ where
  extentOf (As_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (As_ (_,w)) =
    (bumpChars w loc, As_ (loc,w))

instance Arbitrary As_ where
  arbitrary = curry As_
    <$> pure origin
    <*> arbitraryCase "as"





newtype Binary_ = Binary_
  { unBinary_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Binary_ where
  render (Binary_ (_,w)) = T.pack w

instance HasExtent Binary_ where
  extentOf (Binary_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Binary_ (_,w)) =
    (bumpChars w loc, Binary_ (loc,w))

instance Arbitrary Binary_ where
  arbitrary = curry Binary_
    <$> pure origin
    <*> arbitraryCase "binary"





newtype Bool_ = Bool_
  { unBool_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Bool_ where
  render (Bool_ (_,w)) = T.pack w

instance HasExtent Bool_ where
  extentOf (Bool_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Bool_ (_,w)) =
    (bumpChars w loc, Bool_ (loc,w))

instance Arbitrary Bool_ where
  arbitrary = curry Bool_
    <$> pure origin
    <*> arbitraryCase "bool"





newtype Boolean_ = Boolean_
  { unBoolean_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Boolean_ where
  render (Boolean_ (_,w)) = T.pack w

instance HasExtent Boolean_ where
  extentOf (Boolean_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Boolean_ (_,w)) =
    (bumpChars w loc, Boolean_ (loc,w))

instance Arbitrary Boolean_ where
  arbitrary = curry Boolean_
    <$> pure origin
    <*> arbitraryCase "boolean"





newtype Break_ = Break_
  { unBreak_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Break_ where
  render (Break_ (_,w)) = T.pack w

instance HasExtent Break_ where
  extentOf (Break_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Break_ (_,w)) =
    (bumpChars w loc, Break_ (loc,w))

instance Arbitrary Break_ where
  arbitrary = curry Break_
    <$> pure origin
    <*> arbitraryCase "break"





newtype Callable_ = Callable_
  { unCallable_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Callable_ where
  render (Callable_ (_,w)) = T.pack w

instance HasExtent Callable_ where
  extentOf (Callable_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Callable_ (_,w)) =
    (bumpChars w loc, Callable_ (loc,w))

instance Arbitrary Callable_ where
  arbitrary = curry Callable_
    <$> pure origin
    <*> arbitraryCase "callable"





newtype Case_ = Case_
  { unCase_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Case_ where
  render (Case_ (_,w)) = T.pack w

instance HasExtent Case_ where
  extentOf (Case_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Case_ (_,w)) =
    (bumpChars w loc, Case_ (loc,w))

instance Arbitrary Case_ where
  arbitrary = curry Case_
    <$> pure origin
    <*> arbitraryCase "case"





newtype Catch_ = Catch_
  { unCatch_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Catch_ where
  render (Catch_ (_,w)) = T.pack w

instance HasExtent Catch_ where
  extentOf (Catch_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Catch_ (_,w)) =
    (bumpChars w loc, Catch_ (loc,w))

instance Arbitrary Catch_ where
  arbitrary = curry Catch_
    <$> pure origin
    <*> arbitraryCase "catch"





newtype Class_ = Class_
  { unClass_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Class_ where
  render (Class_ (_,w)) = T.pack w

instance HasExtent Class_ where
  extentOf (Class_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Class_ (_,w)) =
    (bumpChars w loc, Class_ (loc,w))

instance Arbitrary Class_ where
  arbitrary = curry Class_
    <$> pure origin
    <*> arbitraryCase "class"





newtype Clone_ = Clone_
  { unClone_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Clone_ where
  render (Clone_ (_,w)) = T.pack w

instance HasExtent Clone_ where
  extentOf (Clone_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Clone_ (_,w)) =
    (bumpChars w loc, Clone_ (loc,w))

instance Arbitrary Clone_ where
  arbitrary = curry Clone_
    <$> pure origin
    <*> arbitraryCase "clone"





newtype Const_ = Const_
  { unConst_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Const_ where
  render (Const_ (_,w)) = T.pack w

instance HasExtent Const_ where
  extentOf (Const_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Const_ (_,w)) =
    (bumpChars w loc, Const_ (loc,w))

instance Arbitrary Const_ where
  arbitrary = curry Const_
    <$> pure origin
    <*> arbitraryCase "const"





newtype Construct_ = Construct_
  { unConstruct_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Construct_ where
  render (Construct_ (_,w)) = T.pack w

instance HasExtent Construct_ where
  extentOf (Construct_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Construct_ (_,w)) =
    (bumpChars w loc, Construct_ (loc,w))

instance Arbitrary Construct_ where
  arbitrary = curry Construct_
    <$> pure origin
    <*> arbitraryCase "__construct"





newtype Continue_ = Continue_
  { unContinue_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Continue_ where
  render (Continue_ (_,w)) = T.pack w

instance HasExtent Continue_ where
  extentOf (Continue_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Continue_ (_,w)) =
    (bumpChars w loc, Continue_ (loc,w))

instance Arbitrary Continue_ where
  arbitrary = curry Continue_
    <$> pure origin
    <*> arbitraryCase "continue"





newtype Declare_ = Declare_
  { unDeclare_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Declare_ where
  render (Declare_ (_,w)) = T.pack w

instance HasExtent Declare_ where
  extentOf (Declare_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Declare_ (_,w)) =
    (bumpChars w loc, Declare_ (loc,w))

instance Arbitrary Declare_ where
  arbitrary = curry Declare_
    <$> pure origin
    <*> arbitraryCase "declare"





newtype Default_ = Default_
  { unDefault_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Default_ where
  render (Default_ (_,w)) = T.pack w

instance HasExtent Default_ where
  extentOf (Default_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Default_ (_,w)) =
    (bumpChars w loc, Default_ (loc,w))

instance Arbitrary Default_ where
  arbitrary = curry Default_
    <$> pure origin
    <*> arbitraryCase "default"





newtype Destruct_ = Destruct_
  { unDestruct_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Destruct_ where
  render (Destruct_ (_,w)) = T.pack w

instance HasExtent Destruct_ where
  extentOf (Destruct_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Destruct_ (_,w)) =
    (bumpChars w loc, Destruct_ (loc,w))

instance Arbitrary Destruct_ where
  arbitrary = curry Destruct_
    <$> pure origin
    <*> arbitraryCase "__destruct"





newtype Die_ = Die_
  { unDie_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Die_ where
  render (Die_ (_,w)) = T.pack w

instance HasExtent Die_ where
  extentOf (Die_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Die_ (_,w)) =
    (bumpChars w loc, Die_ (loc,w))

instance Arbitrary Die_ where
  arbitrary = curry Die_
    <$> pure origin
    <*> arbitraryCase "die"





newtype Do_ = Do_
  { unDo_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Do_ where
  render (Do_ (_,w)) = T.pack w

instance HasExtent Do_ where
  extentOf (Do_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Do_ (_,w)) =
    (bumpChars w loc, Do_ (loc,w))

instance Arbitrary Do_ where
  arbitrary = curry Do_
    <$> pure origin
    <*> arbitraryCase "do"





newtype Double_ = Double_
  { unDouble_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Double_ where
  render (Double_ (_,w)) = T.pack w

instance HasExtent Double_ where
  extentOf (Double_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Double_ (_,w)) =
    (bumpChars w loc, Double_ (loc,w))

instance Arbitrary Double_ where
  arbitrary = curry Double_
    <$> pure origin
    <*> arbitraryCase "double"





newtype Echo_ = Echo_
  { unEcho_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Echo_ where
  render (Echo_ (_,w)) = T.pack w

instance HasExtent Echo_ where
  extentOf (Echo_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Echo_ (_,w)) =
    (bumpChars w loc, Echo_ (loc,w))

instance Arbitrary Echo_ where
  arbitrary = curry Echo_
    <$> pure origin
    <*> arbitraryCase "echo"





newtype Else_ = Else_
  { unElse_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Else_ where
  render (Else_ (_,w)) = T.pack w

instance HasExtent Else_ where
  extentOf (Else_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Else_ (_,w)) =
    (bumpChars w loc, Else_ (loc,w))

instance Arbitrary Else_ where
  arbitrary = curry Else_
    <$> pure origin
    <*> arbitraryCase "else"





newtype Elseif_ = Elseif_
  { unElseif_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Elseif_ where
  render (Elseif_ (_,w)) = T.pack w

instance HasExtent Elseif_ where
  extentOf (Elseif_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Elseif_ (_,w)) =
    (bumpChars w loc, Elseif_ (loc,w))

instance Arbitrary Elseif_ where
  arbitrary = curry Elseif_
    <$> pure origin
    <*> arbitraryCase "elseif"





newtype Empty_ = Empty_
  { unEmpty_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Empty_ where
  render (Empty_ (_,w)) = T.pack w

instance HasExtent Empty_ where
  extentOf (Empty_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Empty_ (_,w)) =
    (bumpChars w loc, Empty_ (loc,w))

instance Arbitrary Empty_ where
  arbitrary = curry Empty_
    <$> pure origin
    <*> arbitraryCase "empty"





newtype Encoding_ = Encoding_
  { unEncoding_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Encoding_ where
  render (Encoding_ (_,w)) = T.pack w

instance HasExtent Encoding_ where
  extentOf (Encoding_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Encoding_ (_,w)) =
    (bumpChars w loc, Encoding_ (loc,w))

instance Arbitrary Encoding_ where
  arbitrary = curry Encoding_
    <$> pure origin
    <*> arbitraryCase "encoding"





newtype Enddeclare_ = Enddeclare_
  { unEnddeclare_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Enddeclare_ where
  render (Enddeclare_ (_,w)) = T.pack w

instance HasExtent Enddeclare_ where
  extentOf (Enddeclare_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Enddeclare_ (_,w)) =
    (bumpChars w loc, Enddeclare_ (loc,w))

instance Arbitrary Enddeclare_ where
  arbitrary = curry Enddeclare_
    <$> pure origin
    <*> arbitraryCase "enddeclare"





newtype Endforeach_ = Endforeach_
  { unEndforeach_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Endforeach_ where
  render (Endforeach_ (_,w)) = T.pack w

instance HasExtent Endforeach_ where
  extentOf (Endforeach_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Endforeach_ (_,w)) =
    (bumpChars w loc, Endforeach_ (loc,w))

instance Arbitrary Endforeach_ where
  arbitrary = curry Endforeach_
    <$> pure origin
    <*> arbitraryCase "endforeach"





newtype Endfor_ = Endfor_
  { unEndfor_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Endfor_ where
  render (Endfor_ (_,w)) = T.pack w

instance HasExtent Endfor_ where
  extentOf (Endfor_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Endfor_ (_,w)) =
    (bumpChars w loc, Endfor_ (loc,w))

instance Arbitrary Endfor_ where
  arbitrary = curry Endfor_
    <$> pure origin
    <*> arbitraryCase "endfor"





newtype Endif_ = Endif_
  { unEndif_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Endif_ where
  render (Endif_ (_,w)) = T.pack w

instance HasExtent Endif_ where
  extentOf (Endif_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Endif_ (_,w)) =
    (bumpChars w loc, Endif_ (loc,w))

instance Arbitrary Endif_ where
  arbitrary = curry Endif_
    <$> pure origin
    <*> arbitraryCase "endif"





newtype Endswitch_ = Endswitch_
  { unEndswitch_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Endswitch_ where
  render (Endswitch_ (_,w)) = T.pack w

instance HasExtent Endswitch_ where
  extentOf (Endswitch_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Endswitch_ (_,w)) =
    (bumpChars w loc, Endswitch_ (loc,w))

instance Arbitrary Endswitch_ where
  arbitrary = curry Endswitch_
    <$> pure origin
    <*> arbitraryCase "endswitch"





newtype Endwhile_ = Endwhile_
  { unEndwhile_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Endwhile_ where
  render (Endwhile_ (_,w)) = T.pack w

instance HasExtent Endwhile_ where
  extentOf (Endwhile_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Endwhile_ (_,w)) =
    (bumpChars w loc, Endwhile_ (loc,w))

instance Arbitrary Endwhile_ where
  arbitrary = curry Endwhile_
    <$> pure origin
    <*> arbitraryCase "endwhile"





newtype Eval_ = Eval_
  { unEval_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Eval_ where
  render (Eval_ (_,w)) = T.pack w

instance HasExtent Eval_ where
  extentOf (Eval_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Eval_ (_,w)) =
    (bumpChars w loc, Eval_ (loc,w))

instance Arbitrary Eval_ where
  arbitrary = curry Eval_
    <$> pure origin
    <*> arbitraryCase "eval"





newtype Exit_ = Exit_
  { unExit_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Exit_ where
  render (Exit_ (_,w)) = T.pack w

instance HasExtent Exit_ where
  extentOf (Exit_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Exit_ (_,w)) =
    (bumpChars w loc, Exit_ (loc,w))

instance Arbitrary Exit_ where
  arbitrary = curry Exit_
    <$> pure origin
    <*> arbitraryCase "exit"





newtype Extends_ = Extends_
  { unExtends_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Extends_ where
  render (Extends_ (_,w)) = T.pack w

instance HasExtent Extends_ where
  extentOf (Extends_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Extends_ (_,w)) =
    (bumpChars w loc, Extends_ (loc,w))

instance Arbitrary Extends_ where
  arbitrary = curry Extends_
    <$> pure origin
    <*> arbitraryCase "extends"





newtype Final_ = Final_
  { unFinal_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Final_ where
  render (Final_ (_,w)) = T.pack w

instance HasExtent Final_ where
  extentOf (Final_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Final_ (_,w)) =
    (bumpChars w loc, Final_ (loc,w))

instance Arbitrary Final_ where
  arbitrary = curry Final_
    <$> pure origin
    <*> arbitraryCase "final"





newtype Finally_ = Finally_
  { unFinally_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Finally_ where
  render (Finally_ (_,w)) = T.pack w

instance HasExtent Finally_ where
  extentOf (Finally_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Finally_ (_,w)) =
    (bumpChars w loc, Finally_ (loc,w))

instance Arbitrary Finally_ where
  arbitrary = curry Finally_
    <$> pure origin
    <*> arbitraryCase "finally"





newtype Float_ = Float_
  { unFloat_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Float_ where
  render (Float_ (_,w)) = T.pack w

instance HasExtent Float_ where
  extentOf (Float_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Float_ (_,w)) =
    (bumpChars w loc, Float_ (loc,w))

instance Arbitrary Float_ where
  arbitrary = curry Float_
    <$> pure origin
    <*> arbitraryCase "float"





newtype Foreach_ = Foreach_
  { unForeach_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Foreach_ where
  render (Foreach_ (_,w)) = T.pack w

instance HasExtent Foreach_ where
  extentOf (Foreach_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Foreach_ (_,w)) =
    (bumpChars w loc, Foreach_ (loc,w))

instance Arbitrary Foreach_ where
  arbitrary = curry Foreach_
    <$> pure origin
    <*> arbitraryCase "foreach"





newtype For_ = For_
  { unFor_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render For_ where
  render (For_ (_,w)) = T.pack w

instance HasExtent For_ where
  extentOf (For_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (For_ (_,w)) =
    (bumpChars w loc, For_ (loc,w))

instance Arbitrary For_ where
  arbitrary = curry For_
    <$> pure origin
    <*> arbitraryCase "for"





newtype Function_ = Function_
  { unFunction_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Function_ where
  render (Function_ (_,w)) = T.pack w

instance HasExtent Function_ where
  extentOf (Function_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Function_ (_,w)) =
    (bumpChars w loc, Function_ (loc,w))

instance Arbitrary Function_ where
  arbitrary = curry Function_
    <$> pure origin
    <*> arbitraryCase "function"





newtype Global_ = Global_
  { unGlobal_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Global_ where
  render (Global_ (_,w)) = T.pack w

instance HasExtent Global_ where
  extentOf (Global_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Global_ (_,w)) =
    (bumpChars w loc, Global_ (loc,w))

instance Arbitrary Global_ where
  arbitrary = curry Global_
    <$> pure origin
    <*> arbitraryCase "global"





newtype Goto_ = Goto_
  { unGoto_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Goto_ where
  render (Goto_ (_,w)) = T.pack w

instance HasExtent Goto_ where
  extentOf (Goto_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Goto_ (_,w)) =
    (bumpChars w loc, Goto_ (loc,w))

instance Arbitrary Goto_ where
  arbitrary = curry Goto_
    <$> pure origin
    <*> arbitraryCase "goto"





newtype If_ = If_
  { unIf_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render If_ where
  render (If_ (_,w)) = T.pack w

instance HasExtent If_ where
  extentOf (If_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (If_ (_,w)) =
    (bumpChars w loc, If_ (loc,w))

instance Arbitrary If_ where
  arbitrary = curry If_
    <$> pure origin
    <*> arbitraryCase "if"





newtype Implements_ = Implements_
  { unImplements_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Implements_ where
  render (Implements_ (_,w)) = T.pack w

instance HasExtent Implements_ where
  extentOf (Implements_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Implements_ (_,w)) =
    (bumpChars w loc, Implements_ (loc,w))

instance Arbitrary Implements_ where
  arbitrary = curry Implements_
    <$> pure origin
    <*> arbitraryCase "implements"





newtype Include_ = Include_
  { unInclude_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Include_ where
  render (Include_ (_,w)) = T.pack w

instance HasExtent Include_ where
  extentOf (Include_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Include_ (_,w)) =
    (bumpChars w loc, Include_ (loc,w))

instance Arbitrary Include_ where
  arbitrary = curry Include_
    <$> pure origin
    <*> arbitraryCase "include"





newtype IncludeOnce_ = IncludeOnce_
  { unIncludeOnce_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render IncludeOnce_ where
  render (IncludeOnce_ (_,w)) = T.pack w

instance HasExtent IncludeOnce_ where
  extentOf (IncludeOnce_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (IncludeOnce_ (_,w)) =
    (bumpChars w loc, IncludeOnce_ (loc,w))

instance Arbitrary IncludeOnce_ where
  arbitrary = curry IncludeOnce_
    <$> pure origin
    <*> arbitraryCase "include_once"





newtype Instanceof_ = Instanceof_
  { unInstanceof_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Instanceof_ where
  render (Instanceof_ (_,w)) = T.pack w

instance HasExtent Instanceof_ where
  extentOf (Instanceof_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Instanceof_ (_,w)) =
    (bumpChars w loc, Instanceof_ (loc,w))

instance Arbitrary Instanceof_ where
  arbitrary = curry Instanceof_
    <$> pure origin
    <*> arbitraryCase "instanceof"





newtype Insteadof_ = Insteadof_
  { unInsteadof_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Insteadof_ where
  render (Insteadof_ (_,w)) = T.pack w

instance HasExtent Insteadof_ where
  extentOf (Insteadof_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Insteadof_ (_,w)) =
    (bumpChars w loc, Insteadof_ (loc,w))

instance Arbitrary Insteadof_ where
  arbitrary = curry Insteadof_
    <$> pure origin
    <*> arbitraryCase "insteadof"





newtype Int_ = Int_
  { unInt_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Int_ where
  render (Int_ (_,w)) = T.pack w

instance HasExtent Int_ where
  extentOf (Int_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Int_ (_,w)) =
    (bumpChars w loc, Int_ (loc,w))

instance Arbitrary Int_ where
  arbitrary = curry Int_
    <$> pure origin
    <*> arbitraryCase "int"





newtype Integer_ = Integer_
  { unInteger_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Integer_ where
  render (Integer_ (_,w)) = T.pack w

instance HasExtent Integer_ where
  extentOf (Integer_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Integer_ (_,w)) =
    (bumpChars w loc, Integer_ (loc,w))

instance Arbitrary Integer_ where
  arbitrary = curry Integer_
    <$> pure origin
    <*> arbitraryCase "integer"





newtype Interface_ = Interface_
  { unInterface_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Interface_ where
  render (Interface_ (_,w)) = T.pack w

instance HasExtent Interface_ where
  extentOf (Interface_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Interface_ (_,w)) =
    (bumpChars w loc, Interface_ (loc,w))

instance Arbitrary Interface_ where
  arbitrary = curry Interface_
    <$> pure origin
    <*> arbitraryCase "interface"





newtype Isset_ = Isset_
  { unIsset_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Isset_ where
  render (Isset_ (_,w)) = T.pack w

instance HasExtent Isset_ where
  extentOf (Isset_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Isset_ (_,w)) =
    (bumpChars w loc, Isset_ (loc,w))

instance Arbitrary Isset_ where
  arbitrary = curry Isset_
    <$> pure origin
    <*> arbitraryCase "isset"





newtype Iterable_ = Iterable_
  { unIterable_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Iterable_ where
  render (Iterable_ (_,w)) = T.pack w

instance HasExtent Iterable_ where
  extentOf (Iterable_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Iterable_ (_,w)) =
    (bumpChars w loc, Iterable_ (loc,w))

instance Arbitrary Iterable_ where
  arbitrary = curry Iterable_
    <$> pure origin
    <*> arbitraryCase "iterable"





newtype List_ = List_
  { unList_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render List_ where
  render (List_ (_,w)) = T.pack w

instance HasExtent List_ where
  extentOf (List_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (List_ (_,w)) =
    (bumpChars w loc, List_ (loc,w))

instance Arbitrary List_ where
  arbitrary = curry List_
    <$> pure origin
    <*> arbitraryCase "list"





newtype Namespace_ = Namespace_
  { unNamespace_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Namespace_ where
  render (Namespace_ (_,w)) = T.pack w

instance HasExtent Namespace_ where
  extentOf (Namespace_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Namespace_ (_,w)) =
    (bumpChars w loc, Namespace_ (loc,w))

instance Arbitrary Namespace_ where
  arbitrary = curry Namespace_
    <$> pure origin
    <*> arbitraryCase "namespace"





newtype New_ = New_
  { unNew_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render New_ where
  render (New_ (_,w)) = T.pack w

instance HasExtent New_ where
  extentOf (New_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (New_ (_,w)) =
    (bumpChars w loc, New_ (loc,w))

instance Arbitrary New_ where
  arbitrary = curry New_
    <$> pure origin
    <*> arbitraryCase "new"





newtype Object_ = Object_
  { unObject_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Object_ where
  render (Object_ (_,w)) = T.pack w

instance HasExtent Object_ where
  extentOf (Object_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Object_ (_,w)) =
    (bumpChars w loc, Object_ (loc,w))

instance Arbitrary Object_ where
  arbitrary = curry Object_
    <$> pure origin
    <*> arbitraryCase "object"





newtype Or_ = Or_
  { unOr_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Or_ where
  render (Or_ (_,w)) = T.pack w

instance HasExtent Or_ where
  extentOf (Or_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Or_ (_,w)) =
    (bumpChars w loc, Or_ (loc,w))

instance Arbitrary Or_ where
  arbitrary = curry Or_
    <$> pure origin
    <*> arbitraryCase "or"





newtype Parent_ = Parent_
  { unParent_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Parent_ where
  render (Parent_ (_,w)) = T.pack w

instance HasExtent Parent_ where
  extentOf (Parent_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Parent_ (_,w)) =
    (bumpChars w loc, Parent_ (loc,w))

instance Arbitrary Parent_ where
  arbitrary = curry Parent_
    <$> pure origin
    <*> arbitraryCase "parent"





newtype Print_ = Print_
  { unPrint_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Print_ where
  render (Print_ (_,w)) = T.pack w

instance HasExtent Print_ where
  extentOf (Print_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Print_ (_,w)) =
    (bumpChars w loc, Print_ (loc,w))

instance Arbitrary Print_ where
  arbitrary = curry Print_
    <$> pure origin
    <*> arbitraryCase "print"





newtype Private_ = Private_
  { unPrivate_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Private_ where
  render (Private_ (_,w)) = T.pack w

instance HasExtent Private_ where
  extentOf (Private_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Private_ (_,w)) =
    (bumpChars w loc, Private_ (loc,w))

instance Arbitrary Private_ where
  arbitrary = curry Private_
    <$> pure origin
    <*> arbitraryCase "private"





newtype Protected_ = Protected_
  { unProtected_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Protected_ where
  render (Protected_ (_,w)) = T.pack w

instance HasExtent Protected_ where
  extentOf (Protected_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Protected_ (_,w)) =
    (bumpChars w loc, Protected_ (loc,w))

instance Arbitrary Protected_ where
  arbitrary = curry Protected_
    <$> pure origin
    <*> arbitraryCase "protected"





newtype Public_ = Public_
  { unPublic_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Public_ where
  render (Public_ (_,w)) = T.pack w

instance HasExtent Public_ where
  extentOf (Public_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Public_ (_,w)) =
    (bumpChars w loc, Public_ (loc,w))

instance Arbitrary Public_ where
  arbitrary = curry Public_
    <$> pure origin
    <*> arbitraryCase "public"





newtype Real_ = Real_
  { unReal_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Real_ where
  render (Real_ (_,w)) = T.pack w

instance HasExtent Real_ where
  extentOf (Real_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Real_ (_,w)) =
    (bumpChars w loc, Real_ (loc,w))

instance Arbitrary Real_ where
  arbitrary = curry Real_
    <$> pure origin
    <*> arbitraryCase "real"





newtype Require_ = Require_
  { unRequire_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Require_ where
  render (Require_ (_,w)) = T.pack w

instance HasExtent Require_ where
  extentOf (Require_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Require_ (_,w)) =
    (bumpChars w loc, Require_ (loc,w))

instance Arbitrary Require_ where
  arbitrary = curry Require_
    <$> pure origin
    <*> arbitraryCase "require"





newtype RequireOnce_ = RequireOnce_
  { unRequireOnce_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render RequireOnce_ where
  render (RequireOnce_ (_,w)) = T.pack w

instance HasExtent RequireOnce_ where
  extentOf (RequireOnce_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (RequireOnce_ (_,w)) =
    (bumpChars w loc, RequireOnce_ (loc,w))

instance Arbitrary RequireOnce_ where
  arbitrary = curry RequireOnce_
    <$> pure origin
    <*> arbitraryCase "require_once"





newtype Return_ = Return_
  { unReturn_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Return_ where
  render (Return_ (_,w)) = T.pack w

instance HasExtent Return_ where
  extentOf (Return_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Return_ (_,w)) =
    (bumpChars w loc, Return_ (loc,w))

instance Arbitrary Return_ where
  arbitrary = curry Return_
    <$> pure origin
    <*> arbitraryCase "return"





newtype Self_ = Self_
  { unSelf_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Self_ where
  render (Self_ (_,w)) = T.pack w

instance HasExtent Self_ where
  extentOf (Self_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Self_ (_,w)) =
    (bumpChars w loc, Self_ (loc,w))

instance Arbitrary Self_ where
  arbitrary = curry Self_
    <$> pure origin
    <*> arbitraryCase "self"





newtype Static_ = Static_
  { unStatic_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Static_ where
  render (Static_ (_,w)) = T.pack w

instance HasExtent Static_ where
  extentOf (Static_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Static_ (_,w)) =
    (bumpChars w loc, Static_ (loc,w))

instance Arbitrary Static_ where
  arbitrary = curry Static_
    <$> pure origin
    <*> arbitraryCase "static"





newtype StrictTypes_ = StrictTypes_
  { unStrictTypes_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render StrictTypes_ where
  render (StrictTypes_ (_,w)) = T.pack w

instance HasExtent StrictTypes_ where
  extentOf (StrictTypes_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (StrictTypes_ (_,w)) =
    (bumpChars w loc, StrictTypes_ (loc,w))

instance Arbitrary StrictTypes_ where
  arbitrary = curry StrictTypes_
    <$> pure origin
    <*> arbitraryCase "strict_types"





newtype String_ = String_
  { unString_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render String_ where
  render (String_ (_,w)) = T.pack w

instance HasExtent String_ where
  extentOf (String_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (String_ (_,w)) =
    (bumpChars w loc, String_ (loc,w))

instance Arbitrary String_ where
  arbitrary = curry String_
    <$> pure origin
    <*> arbitraryCase "string"





newtype Switch_ = Switch_
  { unSwitch_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Switch_ where
  render (Switch_ (_,w)) = T.pack w

instance HasExtent Switch_ where
  extentOf (Switch_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Switch_ (_,w)) =
    (bumpChars w loc, Switch_ (loc,w))

instance Arbitrary Switch_ where
  arbitrary = curry Switch_
    <$> pure origin
    <*> arbitraryCase "switch"





newtype Throw_ = Throw_
  { unThrow_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Throw_ where
  render (Throw_ (_,w)) = T.pack w

instance HasExtent Throw_ where
  extentOf (Throw_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Throw_ (_,w)) =
    (bumpChars w loc, Throw_ (loc,w))

instance Arbitrary Throw_ where
  arbitrary = curry Throw_
    <$> pure origin
    <*> arbitraryCase "throw"





newtype Ticks_ = Ticks_
  { unTicks_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Ticks_ where
  render (Ticks_ (_,w)) = T.pack w

instance HasExtent Ticks_ where
  extentOf (Ticks_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Ticks_ (_,w)) =
    (bumpChars w loc, Ticks_ (loc,w))

instance Arbitrary Ticks_ where
  arbitrary = curry Ticks_
    <$> pure origin
    <*> arbitraryCase "ticks"





newtype Trait_ = Trait_
  { unTrait_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Trait_ where
  render (Trait_ (_,w)) = T.pack w

instance HasExtent Trait_ where
  extentOf (Trait_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Trait_ (_,w)) =
    (bumpChars w loc, Trait_ (loc,w))

instance Arbitrary Trait_ where
  arbitrary = curry Trait_
    <$> pure origin
    <*> arbitraryCase "trait"





newtype Try_ = Try_
  { unTry_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Try_ where
  render (Try_ (_,w)) = T.pack w

instance HasExtent Try_ where
  extentOf (Try_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Try_ (_,w)) =
    (bumpChars w loc, Try_ (loc,w))

instance Arbitrary Try_ where
  arbitrary = curry Try_
    <$> pure origin
    <*> arbitraryCase "try"





newtype Unset_ = Unset_
  { unUnset_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Unset_ where
  render (Unset_ (_,w)) = T.pack w

instance HasExtent Unset_ where
  extentOf (Unset_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Unset_ (_,w)) =
    (bumpChars w loc, Unset_ (loc,w))

instance Arbitrary Unset_ where
  arbitrary = curry Unset_
    <$> pure origin
    <*> arbitraryCase "unset"





newtype Use_ = Use_
  { unUse_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Use_ where
  render (Use_ (_,w)) = T.pack w

instance HasExtent Use_ where
  extentOf (Use_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Use_ (_,w)) =
    (bumpChars w loc, Use_ (loc,w))

instance Arbitrary Use_ where
  arbitrary = curry Use_
    <$> pure origin
    <*> arbitraryCase "use"





newtype Var_ = Var_
  { unVar_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Var_ where
  render (Var_ (_,w)) = T.pack w

instance HasExtent Var_ where
  extentOf (Var_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Var_ (_,w)) =
    (bumpChars w loc, Var_ (loc,w))

instance Arbitrary Var_ where
  arbitrary = curry Var_
    <$> pure origin
    <*> arbitraryCase "var"





newtype Void_ = Void_
  { unVoid_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Void_ where
  render (Void_ (_,w)) = T.pack w

instance HasExtent Void_ where
  extentOf (Void_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Void_ (_,w)) =
    (bumpChars w loc, Void_ (loc,w))

instance Arbitrary Void_ where
  arbitrary = curry Void_
    <$> pure origin
    <*> arbitraryCase "void"





newtype While_ = While_
  { unWhile_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render While_ where
  render (While_ (_,w)) = T.pack w

instance HasExtent While_ where
  extentOf (While_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (While_ (_,w)) =
    (bumpChars w loc, While_ (loc,w))

instance Arbitrary While_ where
  arbitrary = curry While_
    <$> pure origin
    <*> arbitraryCase "while"





newtype Yield_ = Yield_
  { unYield_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Yield_ where
  render (Yield_ (_,w)) = T.pack w

instance HasExtent Yield_ where
  extentOf (Yield_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Yield_ (_,w)) =
    (bumpChars w loc, Yield_ (loc,w))

instance Arbitrary Yield_ where
  arbitrary = curry Yield_
    <$> pure origin
    <*> arbitraryCase "yield"





newtype YieldFrom_ = YieldFrom_
  { unYieldFrom_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render YieldFrom_ where
  render (YieldFrom_ (_,w)) = T.pack w

instance HasExtent YieldFrom_ where
  extentOf (YieldFrom_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (YieldFrom_ (_,w)) =
    (bumpChars w loc, YieldFrom_ (loc,w))

instance Arbitrary YieldFrom_ where
  arbitrary = curry YieldFrom_
    <$> pure origin
    <*> arbitraryCase "yield from"





newtype Xor_ = Xor_
  { unXor_ :: (Loc, String)
  } deriving (Eq, Show)

instance Render Xor_ where
  render (Xor_ (_,w)) = T.pack w

instance HasExtent Xor_ where
  extentOf (Xor_ (loc,w)) =
    IsBetween loc (bumpChars (tail w) loc)

  shiftTo loc (Xor_ (_,w)) =
    (bumpChars w loc, Xor_ (loc,w))

instance Arbitrary Xor_ where
  arbitrary = curry Xor_
    <$> pure origin
    <*> arbitraryCase "xor"





-- Helpers

arbitraryCase :: String -> Gen String
arbitraryCase str = do
  let
    arbChar :: Char -> Gen Char
    arbChar c = elements $ case c of
      'a' -> ['a','A']; 'b' -> ['b','B']; 'c' -> ['c','C']
      'd' -> ['d','D']; 'e' -> ['e','E']; 'f' -> ['f','F']
      'g' -> ['g','G']; 'h' -> ['h','H']; 'i' -> ['i','I']
      'j' -> ['j','J']; 'k' -> ['k','K']; 'l' -> ['l','L']
      'm' -> ['m','M']; 'n' -> ['n','N']; 'o' -> ['o','O']
      'p' -> ['p','P']; 'q' -> ['q','Q']; 'r' -> ['r','R']
      's' -> ['s','S']; 't' -> ['t','T']; 'u' -> ['u','U']
      'v' -> ['v','V']; 'w' -> ['w','W']; 'x' -> ['x','X']
      'y' -> ['y','Y']; 'z' -> ['z','Z']
      _   -> [c]

  mapM arbChar str
