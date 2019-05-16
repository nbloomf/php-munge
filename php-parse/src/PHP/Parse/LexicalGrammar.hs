module PHP.Parse.LexicalGrammar where



data TokenWhiteSpace
  = WS_Space
  | WS_HorizontalTab
  deriving (Eq, Show)

data TokenNewLine
  = NL_CR
  | NL_LF
  | NL_CRLF
  deriving (Eq, Show)

data TokenCharClass
  = CC_Text
  | CC_SingleLineComment
  | CC_MultiLineComment
  | CC_SingleQuotedString
  | CC_DoubleQuotedString
  | CC_Heredoc
  | CC_Nowdoc
  deriving (Eq, Show)

data TokenPhpTag
  = TG_StartLong
  | TG_StartShort
  | TG_End
  deriving (Eq, Show)

data TokenStringClass
  = SC_BinaryDigits
  | SC_OctalDigits
  | SC_HexDigits
  | SC_DecimalDigits
  | SC_FractionalDigits
  | SC_ExponentDigits
  | SC_Name
  | SC_DocStart
  | SC_DocEnd
  | SC_DocString

  | SC_OctalEscape
  | SC_HexEscape
  | SC_UnicodeEscape
  deriving (Eq, Show)

data TokenPrefix
  = PX_Octal
  | PX_Binary_LargeB
  | PX_Binary_SmallB
  | PX_Hex_LargeX
  | PX_Hex_SmallX
  | PX_String_LargeB
  | PX_String_SmallB
  | PX_Frac_SmallE
  | PX_Frac_LargeE
  deriving (Eq, Show)

data TokenEscaped
  = ES_SingleQuotedString_SingleQuote
  | ES_SingleQuotedString_Backslash

  | ES_DoubleQuotedString_DoubleQuote
  | ES_DoubleQuotedString_Backslash
  | ES_DoubleQuotedString_Dollar
  | ES_DoubleQuotedString_Escape
  | ES_DoubleQuotedString_FormFeed
  | ES_DoubleQuotedString_LineFeed
  | ES_DoubleQuotedString_CarriageReturn
  | ES_DoubleQuotedString_HorizontalTab
  | ES_DoubleQuotedString_VerticalTab

  | ES_HereDoc_Backslash
  | ES_HereDoc_Dollar
  | ES_HereDoc_Escape
  | ES_HereDoc_FormFeed
  | ES_HereDoc_LineFeed
  | ES_HereDoc_CarriageReturn
  | ES_HereDoc_HorizontalTab
  | ES_HereDoc_VerticalTab
  deriving (Eq, Show)

data TokenSymbol
  = SY_AmpAmp
  | SY_AmpEqual
  | SY_Amp
  | SY_Ast
  | SY_AstAst
  | SY_AstAstEqual
  | SY_AstEqual
  | SY_AstSlash
  | SY_At
  | SY_Backslash
  | SY_Backtick
  | SY_Bang
  | SY_BangEqual
  | SY_BangEqualEqual
  | SY_Caret
  | SY_CaretEqual
  | SY_ClosedBrace
  | SY_ClosedBrack
  | SY_ClosedParen
  | SY_Colon
  | SY_Comma
  | SY_Dollar
  | SY_DollarOpenBrace
  | SY_Dot
  | SY_DotEqual
  | SY_DoubleColon
  | SY_DoubleEqual
  | SY_DoubleGreater
  | SY_DoubleGreaterEqual
  | SY_DoubleLess
  | SY_DoubleLessEqual
  | SY_DoubleMinus
  | SY_DoublePipe
  | SY_DoublePlus
  | SY_DoubleQuestion
  | SY_DoubleQuote
  | SY_DoubleSlash
  | SY_Ellipsis
  | SY_Equal
  | SY_EqualGreater
  | SY_Greater
  | SY_GreaterEqual
  | SY_Less
  | SY_LessEqual
  | SY_LessGreater
  | SY_Minus
  | SY_MinusEqual
  | SY_MinusGreater
  | SY_Octothorpe
  | SY_OpenBrace
  | SY_OpenBrack
  | SY_OpenParen
  | SY_Percent
  | SY_PercentEqual
  | SY_Pipe
  | SY_PipeEqual
  | SY_Plus
  | SY_PlusEqual
  | SY_Question
  | SY_Semicolon
  | SY_SingleQuote
  | SY_Slash
  | SY_SlashAst
  | SY_SlashEqual
  | SY_Spaceship
  | SY_Tilde
  | SY_TripleEqual
  | SY_TripleLess
  deriving (Eq, Show)

data TokenKeyword
  = KW_Abstract
  | KW_And
  | KW_Array
  | KW_As
  | KW_Binary
  | KW_Bool
  | KW_Boolean
  | KW_Break
  | KW_Callable
  | KW_Case
  | KW_Catch
  | KW_Class
  | KW_Clone
  | KW_Const
  | KW_Construct
  | KW_Continue
  | KW_Declare
  | KW_Default
  | KW_Destruct
  | KW_Die
  | KW_Do
  | KW_Double
  | KW_Echo
  | KW_Else
  | KW_Elseif
  | KW_Empty
  | KW_Encoding
  | KW_Enddeclare
  | KW_Endforeach
  | KW_Endfor
  | KW_Endif
  | KW_Endswitch
  | KW_Endwhile
  | KW_Eval
  | KW_Exit
  | KW_Extends
  | KW_Final
  | KW_Finally
  | KW_Float
  | KW_Foreach
  | KW_For
  | KW_Function
  | KW_Global
  | KW_Goto
  | KW_If
  | KW_Implements
  | KW_Include
  | KW_IncludeOnce
  | KW_Instanceof
  | KW_Insteadof
  | KW_Int
  | KW_Integer
  | KW_Interface
  | KW_Isset
  | KW_Iterable
  | KW_List
  | KW_Namespace
  | KW_New
  | KW_Object
  | KW_Or
  | KW_Parent
  | KW_Print
  | KW_Private
  | KW_Protected
  | KW_Public
  | KW_Real
  | KW_Require
  | KW_RequireOnce
  | KW_Return
  | KW_Self
  | KW_Static
  | KW_StrictTypes
  | KW_String
  | KW_Switch
  | KW_Throw
  | KW_Ticks
  | KW_Trait
  | KW_Try
  | KW_Unset
  | KW_Use
  | KW_Var
  | KW_Void
  | KW_While
  | KW_Yield
  | KW_YieldFrom
  | KW_Xor
  deriving (Eq, Show)
