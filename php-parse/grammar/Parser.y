{
-- Work around https://github.com/simonmar/happy/issues/109
#undef __GLASGOW_HASKELL__
#define __GLASGOW_HASKELL__ 709

module PHP.Parse.Parser (
    Parser
  , testParser
  , debugParser

  -- * Scripts
  , pScript
  , pScriptSection
  , pText
  , pStartTag
  , pEndTag
  , pWhiteSpaces
  , pWhiteSpace
  , pNewLine

  -- * Comments
  , pComment
  , pSingleLineComment
  , pMultiLineComment

  -- * Strings
  , pStringLiteral
  , pSingleQuotedString
  , pDoubleQuotedString
  , pHeredocString
  , pNowdocString

  -- * Ints
  , pIntegerLiteral
  , pDecimalLiteral
  , pHexLiteral
  , pBinaryLiteral
  , pOctalLiteral

  -- * Floats
  , pFloatingLiteral
  , pExponentPart

  -- * Variables
  , pVariable
  , pCallableVariable
  , pSimpleVariable
  , pVariableName
  , pName
  , pSubscriptExpression
  , pMemberCallExpression
  , pScopedCallExpression
  , pScopeResolutionQualifier
  , pQualifiedName
  , pNamespaceNameAsAPrefix
  , pFunctionCallExpression
  , pCallableExpression
  , pScopedPropertyAccessExpression
  , pMemberAccessExpression
  , pDereferencableExpression

  -- * Expressions
  , pExpression
  , pIncludeExpression
  , pIncludeOnceExpression
  , pRequireExpression
  , pRequireOnceExpression
  , pLogicalIncOrExpression2
  , pLogicalExcOrExpression
  , pLogicalAndExpression2
  , pPrintExpression
  , pYieldExpression
  , pAssignmentExpression
  , pSimpleAssignmentExpression
  , pCompoundAssignmentExpression
  , pConditionalExpression
  , pCoalesceExpression
  , pLogicalIncOrExpression1
  , pLogicalAndExpression1
  , pBitwiseIncOrExpression
  , pBitwiseExcOrExpression
  , pBitwiseAndExpression
  , pEqualityExpression
  , pRelationalExpression
  , pShiftExpression
  , pAdditiveExpression
  , pMultiplicativeExpression
  , pLogicalNotExpression
  , pInstanceofExpression
  , pUnaryExpression
  , pErrorControlExpression
  , pCastExpression
  , pCastType
  , pUnaryOpExpression
  , pExponentiationExpression
  , pCloneExpression
  , pPrimaryExpression
  , pLiteral
  , pPostfixIncrementExpression
  , pPostfixDecrementExpression
  , pPrefixIncrementExpression
  , pPrefixDecrementExpression
  , pIntrinsic
  , pEmptyIntrinsic
  , pEvalIntrinsic
  , pIssetIntrinsic
  , pExitIntrinsic
  , pClassConstantAccessExpression
  , pAnonymousFunctionCreationExpression
  , pArrayCreationExpression
  , pObjectCreationExpression
  , pShellCommandExpression
  , pByrefAssignmentExpression

  -- * Statements
  , pStatementList
  , pStatement
  , pCompoundStatement
  , pNamedLabelStatement
  , pExpressionStatement
  , pNamespaceDefinition
  , pJumpStatement
  , pGotoStatement
  , pContinueStatement
  , pBreakStatement
  , pReturnStatement
  , pThrowStatement
  , pSelectionStatement
  , pIfStatement
  , pElseifClauses1
  , pElseClause1
  , pElseifClauses2
  , pElseClause2
  , pSwitchStatement
  , pTryStatement
  , pIterationStatement
  , pWhileStatement
  , pDoStatement
  , pForStatement
  , pForeachStatement
  , pDeclareStatement
  , pEchoStatement
  , pUnsetStatement
  , pConstDeclaration
  , pConstantExpression
  , pFunctionDefinition
  , pDefaultArgumentSpecifier
  , pScalarType
  , pClassDeclaration
  , pTraitDeclaration
  , pConstructorDeclaration
  , pDestructorDeclaration
  , pGlobalDeclaration
  , pNamespaceUseDeclaration
  , pFunctionStaticDeclaration
  , pFunctionStaticInitializer
) where



import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import PHP.Parse.Render
import PHP.Parse.Lexer
import PHP.Parse.Loc
import qualified PHP.Parse.LexicalGrammar as Lex
import PHP.Parse.ConcreteSyntax
import PHP.Parse.ConcreteSyntax.Seq
import PHP.Parse.ConcreteSyntax.Keyword
import PHP.Parse.ConcreteSyntax.Symbol
import PHP.Parse.ConcreteSyntax.WhiteSpaces
import PHP.Parse.ConcreteSyntax.IntegerLiteral
import PHP.Parse.ConcreteSyntax.FloatingLiteral
import PHP.Parse.ConcreteSyntax.Name
}



-- ================ --
-- Exported Parsers --
-- ================ --

-- Scripts

%name pScript                              script
%name pScriptSection                       script_section
%name pText                                text
%name pStartTag                            start_tag
%name pEndTag                              end_tag
%name pWhiteSpaces                         whitespaces
%name pWhiteSpace                          whitespace
%name pNewLine                             newline

-- Comments

%name pComment                             comment
%name pSingleLineComment                   single_line_comment
%name pMultiLineComment                    multi_line_comment

-- Strings

%name pStringLiteral                       string_literal
%name pSingleQuotedString                  single_quoted_string
%name pDoubleQuotedString                  double_quoted_string
%name pHeredocString                       heredoc_string
%name pNowdocString                        nowdoc_string

-- Ints

%name pIntegerLiteral                      integer_literal
%name pDecimalLiteral                      decimal_literal
%name pHexLiteral                          hex_literal
%name pBinaryLiteral                       binary_literal
%name pOctalLiteral                        octal_literal

-- Floats

%name pFloatingLiteral                     floating_literal
%name pExponentPart                        exponent_part

-- Variables

%name pVariable                            variable
%name pCallableVariable                    callable_variable
%name pSimpleVariable                      simple_variable
%name pVariableName                        variable_name
%name pName                                name
%name pSubscriptExpression                 subscript_expr
%name pMemberCallExpression                member_call_expression
%name pScopedCallExpression                scoped_call_expression
%name pScopeResolutionQualifier            scope_resolution_qualifier
%name pQualifiedName                       qualified_name
%name pNamespaceNameAsAPrefix              namespace_name_as_a_prefix
%name pFunctionCallExpression              function_call_expr
%name pCallableExpression                  callable_expr
%name pScopedPropertyAccessExpression      scoped_property_access_expression
%name pMemberAccessExpression              member_access_expression
%name pDereferencableExpression            dereferencable_expr

-- Expressions

%name pExpression                          expression
%name pIncludeExpression                   include_expression
%name pIncludeOnceExpression               include_once_expression
%name pRequireExpression                   require_expression
%name pRequireOnceExpression               require_once_expression
%name pLogicalIncOrExpression2             logical_inc_or_expr_2
%name pLogicalExcOrExpression              logical_exc_or_expr
%name pLogicalAndExpression2               logical_and_expr_2
%name pPrintExpression                     print_expr
%name pYieldExpression                     yield_expr
%name pAssignmentExpression                assignment_expr
%name pSimpleAssignmentExpression          simple_assignment_expr
%name pCompoundAssignmentExpression        compound_assignment_expr
%name pConditionalExpression               conditional_expr
%name pCoalesceExpression                  coalesce_expr
%name pLogicalIncOrExpression1             logical_inc_or_expr_1
%name pLogicalAndExpression1               logical_and_expr_1
%name pBitwiseIncOrExpression              bitwise_inc_or_expr
%name pBitwiseExcOrExpression              bitwise_exc_or_expr
%name pBitwiseAndExpression                bitwise_and_expr
%name pEqualityExpression                  equality_expr
%name pRelationalExpression                relational_expr
%name pShiftExpression                     shift_expr
%name pAdditiveExpression                  additive_expr
%name pMultiplicativeExpression            multiplicative_expr
%name pLogicalNotExpression                logical_not_expr
%name pInstanceofExpression                instanceof_expr
%name pUnaryExpression                     unary_expr
%name pErrorControlExpression              error_control_expression
%name pCastExpression                      cast_expression
%name pCastType                            cast_type
%name pUnaryOpExpression                   unary_op_expression
%name pExponentiationExpression            exponentiation_expr
%name pCloneExpression                     clone_expr
%name pPrimaryExpression                   primary_expr
%name pLiteral                             literal
%name pPostfixIncrementExpression          postfix_increment_expr
%name pPostfixDecrementExpression          postfix_decrement_expr
%name pPrefixIncrementExpression           prefix_increment_expr
%name pPrefixDecrementExpression           prefix_decrement_expr
%name pIntrinsic                           intrinsic
%name pEmptyIntrinsic                      empty_intrinsic
%name pEvalIntrinsic                       eval_intrinsic
%name pIssetIntrinsic                      isset_intrinsic
%name pExitIntrinsic                       exit_intrinsic
%name pClassConstantAccessExpression       class_constant_access_expression
%name pAnonymousFunctionCreationExpression anonymous_function_creation_expression
%name pArrayCreationExpression             array_creation_expression
%name pObjectCreationExpression            object_creation_expression
%name pShellCommandExpression              shell_command_expr
%name pByrefAssignmentExpression           byref_assignment_expr

-- Statements

%name pStatementList                       statement_list
%name pStatement                           statement
%name pCompoundStatement                   compound_statement
%name pNamedLabelStatement                 named_label_statement
%name pExpressionStatement                 expression_stmt
%name pNamespaceDefinition                 namespace_definition
%name pJumpStatement                       jump_statement
%name pGotoStatement                       goto_statement
%name pContinueStatement                   continue_statement
%name pBreakStatement                      break_statement
%name pReturnStatement                     return_statement
%name pThrowStatement                      throw_statement
%name pSelectionStatement                  selection_statement
%name pIfStatement                         if_statement
%name pElseifClauses1                      elseif_clauses_1
%name pElseClause1                         else_clause_1
%name pElseifClauses2                      elseif_clauses_2
%name pElseClause2                         else_clause_2
%name pSwitchStatement                     switch_statement
%name pTryStatement                        try_statement
%name pIterationStatement                  iteration_stmt
%name pWhileStatement                      while_stmt
%name pDoStatement                         do_statement
%name pForStatement                        for_stmt
%name pForeachStatement                    foreach_stmt
%name pDeclareStatement                    declare_stmt
%name pEchoStatement                       echo_stmt
%name pUnsetStatement                      unset_stmt
%name pConstDeclaration                    const_declaration
%name pConstantExpression                  constant_expr
%name pFunctionDefinition                  function_definition
%name pDefaultArgumentSpecifier            default_argument_specifier
%name pScalarType                          scalar_type
%name pClassDeclaration                    class_declaration
%name pTraitDeclaration                    trait_declaration
%name pConstructorDeclaration              constructor_declaration
%name pDestructorDeclaration               destructor_declaration
%name pGlobalDeclaration                   global_declaration
%name pNamespaceUseDeclaration             namespace_use_declaration
%name pFunctionStaticDeclaration           function_static_declaration
%name pFunctionStaticInitializer           function_static_initializer



%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { Parser }





-- ========= --
-- Terminals --
-- ========= --

%token

-- Line Endings --
------------------

  cr             { TokenEndLine Lex.NL_CR                 $$ }
  lf             { TokenEndLine Lex.NL_LF                 $$ }
  crlf           { TokenEndLine Lex.NL_CRLF               $$ }

-- Whitespace --
----------------

  space          { TokenWhiteSpace Lex.WS_Space           $$ }
  htab           { TokenWhiteSpace Lex.WS_HorizontalTab   $$ }

-- Escape Sequences

  sqt_esc_single_quote    { TokenEscaped Lex.ES_SingleQuotedString_SingleQuote    $$ }
  sqt_esc_backslash       { TokenEscaped Lex.ES_SingleQuotedString_Backslash      $$ }

  dqt_esc_double_quote    { TokenEscaped Lex.ES_DoubleQuotedString_DoubleQuote    $$ }
  dqt_esc_backslash       { TokenEscaped Lex.ES_DoubleQuotedString_Backslash      $$ }
  dqt_esc_dollar          { TokenEscaped Lex.ES_DoubleQuotedString_Dollar         $$ }
  dqt_esc_escape          { TokenEscaped Lex.ES_DoubleQuotedString_Escape         $$ }
  dqt_esc_form_feed       { TokenEscaped Lex.ES_DoubleQuotedString_FormFeed       $$ }
  dqt_esc_line_feed       { TokenEscaped Lex.ES_DoubleQuotedString_LineFeed       $$ }
  dqt_esc_carriage_return { TokenEscaped Lex.ES_DoubleQuotedString_CarriageReturn $$ }
  dqt_esc_horizontal_tab  { TokenEscaped Lex.ES_DoubleQuotedString_HorizontalTab  $$ }
  dqt_esc_vertical_tab    { TokenEscaped Lex.ES_DoubleQuotedString_VerticalTab    $$ }

-- Literal Prefixes

  '0'            { TokenPrefix Lex.PX_Octal               $$ }
  '0B'           { TokenPrefix Lex.PX_Binary_LargeB       $$ }
  '0b'           { TokenPrefix Lex.PX_Binary_SmallB       $$ }
  '0X'           { TokenPrefix Lex.PX_Hex_LargeX          $$ }
  '0x'           { TokenPrefix Lex.PX_Hex_SmallX          $$ }
  'b'            { TokenPrefix Lex.PX_String_SmallB       $$ }
  'B'            { TokenPrefix Lex.PX_String_LargeB       $$ }
  'e'            { TokenPrefix Lex.PX_Frac_SmallE         $$ }
  'E'            { TokenPrefix Lex.PX_Frac_LargeE         $$ }

-- Character Classes

  txt_char       { TokenAnyChar Lex.CC_Text               $$ }
  slc_char       { TokenAnyChar Lex.CC_SingleLineComment  $$ }
  mlc_char       { TokenAnyChar Lex.CC_MultiLineComment   $$ }
  sqt_char       { TokenAnyChar Lex.CC_SingleQuotedString $$ }
  dqt_char       { TokenAnyChar Lex.CC_DoubleQuotedString $$ }
  hdc_char       { TokenAnyChar Lex.CC_Heredoc            $$ }
  ndc_char       { TokenAnyChar Lex.CC_Nowdoc             $$ }

-- String Classes

  bin_chars      { TokenString Lex.SC_BinaryDigits        $$ }
  oct_chars      { TokenString Lex.SC_OctalDigits         $$ }
  hex_chars      { TokenString Lex.SC_HexDigits           $$ }
  dec_chars      { TokenString Lex.SC_DecimalDigits       $$ }
  name_chars     { TokenString Lex.SC_Name                $$ }
  frac_chars     { TokenString Lex.SC_FractionalDigits    $$ }
  exp_chars      { TokenString Lex.SC_ExponentDigits      $$ }

  doc_start      { TokenString Lex.SC_DocStart            $$ }
  doc_end        { TokenString Lex.SC_DocEnd              $$ }
  doc_chars      { TokenString Lex.SC_DocString           $$ }

  oct_esc        { TokenString Lex.SC_OctalEscape         $$ }
  hex_esc        { TokenString Lex.SC_HexEscape           $$ }
  uni_esc        { TokenString Lex.SC_UnicodeEscape       $$ }

-- PHP Tags

  '<?php'        { TokenPhpTag Lex.TG_StartLong           $$ }
  '<?='          { TokenPhpTag Lex.TG_StartShort          $$ }
  '?>'           { TokenPhpTag Lex.TG_End                 $$ }



-- Symbols --
-------------

  '&&'           { TokenSymbol Lex.SY_AmpAmp              $$ }
  '&='           { TokenSymbol Lex.SY_AmpEqual            $$ }
  '&'            { TokenSymbol Lex.SY_Amp                 $$ }
  '*'            { TokenSymbol Lex.SY_Ast                 $$ }
  '**'           { TokenSymbol Lex.SY_AstAst              $$ }
  '**='          { TokenSymbol Lex.SY_AstAstEqual         $$ }
  '*='           { TokenSymbol Lex.SY_AstEqual            $$ }
  '*/'           { TokenSymbol Lex.SY_AstSlash            $$ }
  '@'            { TokenSymbol Lex.SY_At                  $$ }
  '\\'           { TokenSymbol Lex.SY_Backslash           $$ }
  '`'            { TokenSymbol Lex.SY_Backtick            $$ }
  '!'            { TokenSymbol Lex.SY_Bang                $$ }
  '!='           { TokenSymbol Lex.SY_BangEqual           $$ }
  '!=='          { TokenSymbol Lex.SY_BangEqualEqual      $$ }
  '^'            { TokenSymbol Lex.SY_Caret               $$ }
  '^='           { TokenSymbol Lex.SY_CaretEqual          $$ }
  '}'            { TokenSymbol Lex.SY_ClosedBrace         $$ }
  ']'            { TokenSymbol Lex.SY_ClosedBrack         $$ }
  ')'            { TokenSymbol Lex.SY_ClosedParen         $$ }
  ':'            { TokenSymbol Lex.SY_Colon               $$ }
  ','            { TokenSymbol Lex.SY_Comma               $$ }
  '$'            { TokenSymbol Lex.SY_Dollar              $$ }
  '${'           { TokenSymbol Lex.SY_DollarOpenBrace     $$ }
  '.'            { TokenSymbol Lex.SY_Dot                 $$ }
  '.='           { TokenSymbol Lex.SY_DotEqual            $$ }
  '::'           { TokenSymbol Lex.SY_DoubleColon         $$ }
  '=='           { TokenSymbol Lex.SY_DoubleEqual         $$ }
  '>>'           { TokenSymbol Lex.SY_DoubleGreater       $$ }
  '>>='          { TokenSymbol Lex.SY_DoubleGreaterEqual  $$ }
  '<<'           { TokenSymbol Lex.SY_DoubleLess          $$ }
  '<<='          { TokenSymbol Lex.SY_DoubleLessEqual     $$ }
  '--'           { TokenSymbol Lex.SY_DoubleMinus         $$ }
  '||'           { TokenSymbol Lex.SY_DoublePipe          $$ }
  '++'           { TokenSymbol Lex.SY_DoublePlus          $$ }
  '??'           { TokenSymbol Lex.SY_DoubleQuestion      $$ }
  '"'            { TokenSymbol Lex.SY_DoubleQuote         $$ }
  '//'           { TokenSymbol Lex.SY_DoubleSlash         $$ }
  '...'          { TokenSymbol Lex.SY_Ellipsis            $$ }
  '='            { TokenSymbol Lex.SY_Equal               $$ }
  '=>'           { TokenSymbol Lex.SY_EqualGreater        $$ }
  '>'            { TokenSymbol Lex.SY_Greater             $$ }
  '>='           { TokenSymbol Lex.SY_GreaterEqual        $$ }
  '<'            { TokenSymbol Lex.SY_Less                $$ }
  '<='           { TokenSymbol Lex.SY_LessEqual           $$ }
  '<>'           { TokenSymbol Lex.SY_LessGreater         $$ }
  '-'            { TokenSymbol Lex.SY_Minus               $$ }
  '-='           { TokenSymbol Lex.SY_MinusEqual          $$ }
  '->'           { TokenSymbol Lex.SY_MinusGreater        $$ }
  '#'            { TokenSymbol Lex.SY_Octothorpe          $$ }
  '{'            { TokenSymbol Lex.SY_OpenBrace           $$ }
  '['            { TokenSymbol Lex.SY_OpenBrack           $$ }
  '('            { TokenSymbol Lex.SY_OpenParen           $$ }
  '%'            { TokenSymbol Lex.SY_Percent             $$ }
  '%='           { TokenSymbol Lex.SY_PercentEqual        $$ }
  '|'            { TokenSymbol Lex.SY_Pipe                $$ }
  '|='           { TokenSymbol Lex.SY_PipeEqual           $$ }
  '+'            { TokenSymbol Lex.SY_Plus                $$ }
  '+='           { TokenSymbol Lex.SY_PlusEqual           $$ }
  '?'            { TokenSymbol Lex.SY_Question            $$ }
  ';'            { TokenSymbol Lex.SY_Semicolon           $$ }
  '\''           { TokenSymbol Lex.SY_SingleQuote         $$ }
  '/'            { TokenSymbol Lex.SY_Slash               $$ }
  '/*'           { TokenSymbol Lex.SY_SlashAst            $$ }
  '/='           { TokenSymbol Lex.SY_SlashEqual          $$ }
  '<=>'          { TokenSymbol Lex.SY_Spaceship           $$ }
  '~'            { TokenSymbol Lex.SY_Tilde               $$ }
  '==='          { TokenSymbol Lex.SY_TripleEqual         $$ }
  '<<<'          { TokenSymbol Lex.SY_TripleLess          $$ }



-- Keywords --
--------------

  'abstract'     { TokenKeyword Lex.KW_Abstract           $$ }
  'and'          { TokenKeyword Lex.KW_And                $$ }
  'array'        { TokenKeyword Lex.KW_Array              $$ }
  'as'           { TokenKeyword Lex.KW_As                 $$ }
  'binary'       { TokenKeyword Lex.KW_Binary             $$ }
  'bool'         { TokenKeyword Lex.KW_Bool               $$ }
  'boolean'      { TokenKeyword Lex.KW_Boolean            $$ }
  'break'        { TokenKeyword Lex.KW_Break              $$ }
  'callable'     { TokenKeyword Lex.KW_Callable           $$ }
  'case'         { TokenKeyword Lex.KW_Case               $$ }
  'catch'        { TokenKeyword Lex.KW_Catch              $$ }
  'class'        { TokenKeyword Lex.KW_Class              $$ }
  'clone'        { TokenKeyword Lex.KW_Clone              $$ }
  'const'        { TokenKeyword Lex.KW_Const              $$ }
  '__construct'  { TokenKeyword Lex.KW_Construct          $$ }
  'continue'     { TokenKeyword Lex.KW_Continue           $$ }
  'declare'      { TokenKeyword Lex.KW_Declare            $$ }
  'default'      { TokenKeyword Lex.KW_Default            $$ }
  '__destruct'   { TokenKeyword Lex.KW_Destruct           $$ }
  'die'          { TokenKeyword Lex.KW_Die                $$ }
  'do'           { TokenKeyword Lex.KW_Do                 $$ }
  'double'       { TokenKeyword Lex.KW_Double             $$ }
  'echo'         { TokenKeyword Lex.KW_Echo               $$ }
  'else'         { TokenKeyword Lex.KW_Else               $$ }
  'elseif'       { TokenKeyword Lex.KW_Elseif             $$ }
  'empty'        { TokenKeyword Lex.KW_Empty              $$ }
  'encoding'     { TokenKeyword Lex.KW_Encoding           $$ }
  'enddeclare'   { TokenKeyword Lex.KW_Enddeclare         $$ }
  'endforeach'   { TokenKeyword Lex.KW_Endforeach         $$ }
  'endfor'       { TokenKeyword Lex.KW_Endfor             $$ }
  'endif'        { TokenKeyword Lex.KW_Endif              $$ }
  'endswitch'    { TokenKeyword Lex.KW_Endswitch          $$ }
  'endwhile'     { TokenKeyword Lex.KW_Endwhile           $$ }
  'eval'         { TokenKeyword Lex.KW_Eval               $$ }
  'exit'         { TokenKeyword Lex.KW_Exit               $$ }
  'extends'      { TokenKeyword Lex.KW_Extends            $$ }
  'final'        { TokenKeyword Lex.KW_Final              $$ }
  'finally'      { TokenKeyword Lex.KW_Finally            $$ }
  'float'        { TokenKeyword Lex.KW_Float              $$ }
  'foreach'      { TokenKeyword Lex.KW_Foreach            $$ }
  'for'          { TokenKeyword Lex.KW_For                $$ }
  'function'     { TokenKeyword Lex.KW_Function           $$ }
  'global'       { TokenKeyword Lex.KW_Global             $$ }
  'goto'         { TokenKeyword Lex.KW_Goto               $$ }
  'if'           { TokenKeyword Lex.KW_If                 $$ }
  'implements'   { TokenKeyword Lex.KW_Implements         $$ }
  'include'      { TokenKeyword Lex.KW_Include            $$ }
  'include_once' { TokenKeyword Lex.KW_IncludeOnce        $$ }
  'instanceof'   { TokenKeyword Lex.KW_Instanceof         $$ }
  'insteadof'    { TokenKeyword Lex.KW_Insteadof          $$ }
  'int'          { TokenKeyword Lex.KW_Int                $$ }
  'integer'      { TokenKeyword Lex.KW_Integer            $$ }
  'interface'    { TokenKeyword Lex.KW_Interface          $$ }
  'isset'        { TokenKeyword Lex.KW_Isset              $$ }
  'iterable'     { TokenKeyword Lex.KW_Iterable           $$ }
  'list'         { TokenKeyword Lex.KW_List               $$ }
  'namespace'    { TokenKeyword Lex.KW_Namespace          $$ }
  'new'          { TokenKeyword Lex.KW_New                $$ }
  'object'       { TokenKeyword Lex.KW_Object             $$ }
  'or'           { TokenKeyword Lex.KW_Or                 $$ }
  'parent'       { TokenKeyword Lex.KW_Parent             $$ }
  'print'        { TokenKeyword Lex.KW_Print              $$ }
  'private'      { TokenKeyword Lex.KW_Private            $$ }
  'protected'    { TokenKeyword Lex.KW_Protected          $$ }
  'public'       { TokenKeyword Lex.KW_Public             $$ }
  'real'         { TokenKeyword Lex.KW_Real               $$ }
  'require'      { TokenKeyword Lex.KW_Require            $$ }
  'require_once' { TokenKeyword Lex.KW_RequireOnce        $$ }
  'return'       { TokenKeyword Lex.KW_Return             $$ }
  'self'         { TokenKeyword Lex.KW_Self               $$ }
  'static'       { TokenKeyword Lex.KW_Static             $$ }
  'strict_types' { TokenKeyword Lex.KW_StrictTypes        $$ }
  'string'       { TokenKeyword Lex.KW_String             $$ }
  'switch'       { TokenKeyword Lex.KW_Switch             $$ }
  'throw'        { TokenKeyword Lex.KW_Throw              $$ }
  'ticks'        { TokenKeyword Lex.KW_Ticks              $$ }
  'trait'        { TokenKeyword Lex.KW_Trait              $$ }
  'try'          { TokenKeyword Lex.KW_Try                $$ }
  'unset'        { TokenKeyword Lex.KW_Unset              $$ }
  'use'          { TokenKeyword Lex.KW_Use                $$ }
  'var'          { TokenKeyword Lex.KW_Var                $$ }
  'void'         { TokenKeyword Lex.KW_Void               $$ }
  'while'        { TokenKeyword Lex.KW_While              $$ }
  'yield'        { TokenKeyword Lex.KW_Yield              $$ }
  'yield from'   { TokenKeyword Lex.KW_YieldFrom          $$ }
  'xor'          { TokenKeyword Lex.KW_Xor                $$ }

%%





-- ============ --
-- Nonterminals --
-- ============ --

-----------------------
-- Production Macros --
-----------------------

-- Macros are a powerful feature of Happy that give us one of the key benefits of parser combinators -- the ability to define production rules with abstract parameters.

opt(p)
  : p                                                  { Just $1 }
  |                                                    { Nothing }

seqOf(p)
  : p                                                  { Seq_Head $1 }
  | seqOf(p) p                                         { Seq_Snoc ($1,$2) }

seqOfSep(sep,p)
  : p                                                  { SeqSep_Head $1 }
  | seqOfSep(sep,p) sep p                              { SeqSep_Snoc ($1,$2,$3) }

kw(p)
  : p opt(whitespaces)                                 { Keyword ($1,$2) }

sy(p)
  : p opt(whitespaces)                                 { Symbol ($1,$2) }





-------------
-- Scripts --
-------------

script ::                                              { Script }
  : opt(text) seqOf(script_section)                    { Script ($1,$2) }

script_section ::                                      { ScriptSection }
  : start_tag opt(statement_list)                      { ScriptSection ($1, $2, Nothing, Nothing) }
  | start_tag opt(statement_list) end_tag opt(text)    { ScriptSection ($1, $2, Just $3, $4) }



text ::                                                { Text }
  : seqOf(text_char)                                   { Text $1 }

text_char ::                                           { TextChar }
  : txt_char                                           { TextChar $1 }



start_tag ::                                           { StartTag }
  : '<?php' whitespaces                                { StartTag_Long ($1,$2) }
  | '<?=' whitespaces                                  { StartTag_Short ($1,$2) }

end_tag ::                                             { EndTag }
  : '?>'                                               { EndTag $1 }



whitespaces ::                                         { WhiteSpaces }
  : seqOf(whitespace)                                  { WhiteSpaces $1 }

whitespace ::                                          { WhiteSpace }
  : space                                              { WhiteSpace_Space $1 }
  | htab                                               { WhiteSpace_HorizontalTab $1 }
  | newline                                            { WhiteSpace_NewLine $1 }
  | comment                                            { WhiteSpace_Comment $1 }

newline ::                                             { NewLine }
  : crlf                                               { NewLine_CRLF $1 }
  | cr                                                 { NewLine_CR $1 }
  | lf                                                 { NewLine_LF $1 }





--------------
-- Comments --
--------------

comment ::                                             { Comment }
  : single_line_comment                                { Comment_SingleLine $1 }
  | multi_line_comment                                 { Comment_MultiLine $1 }



-- Single Line Comments --
--------------------------

single_line_comment ::                                 { SingleLineComment }
  : '//' opt(single_line_comment_chars)                { SingleLineComment_Backslash ($1,$2) }
  | '#' opt(single_line_comment_chars)                 { SingleLineComment_Octothorpe ($1,$2) }

single_line_comment_chars ::                           { SingleLineCommentChars }
  : seqOf(single_line_comment_char)                    { SingleLineCommentChars $1 }

single_line_comment_char ::                            { SingleLineCommentChar }
  : slc_char                                           { SingleLineCommentChar $1 }



-- Multi Line Comments --
-------------------------

multi_line_comment ::                                  { MultiLineComment }
  : '/*' opt(multi_line_comment_chars) '*/'            { MultiLineComment ($1,$2) }

multi_line_comment_chars ::                            { MultiLineCommentChars }
  : seqOf(multi_line_comment_char)                     { MultiLineCommentChars $1 }

multi_line_comment_char ::                             { MultiLineCommentChar }
  : mlc_char                                           { MultiLineCommentChar $1 }





-------------
-- Strings --
-------------

string_literal ::                                      { StringLiteral }
  : single_quoted_string                               { StringLiteral_SingleQuoted $1 }
  | double_quoted_string                               { StringLiteral_DoubleQuoted $1 }
  | heredoc_string                                     { StringLiteral_Heredoc $1 }
  | nowdoc_string                                      { StringLiteral_Nowdoc $1 }

b_prefix ::                                            { BPrefix }
  : 'B'                                                { BPrefix_Large $1 }
  | 'b'                                                { BPrefix_Small $1 }



-- Single Quoted Strings --
---------------------------

single_quoted_string ::                                { SingleQuotedString }
  : opt(b_prefix) '\'' opt(sq_string_chars) '\''       { SingleQuotedString ($1, SingleQuote_ $2, $3, SingleQuote_ $4) }

sq_string_chars ::                                     { SingleQuotedStringChars }
  : seqOf(sq_string_char)                              { SingleQuotedStringChars $1 }

sq_string_char ::                                      { SingleQuotedStringChar }
  : sqt_char                                           { SingleQuotedStringChar_Char $1 }
  | sq_string_escaped                                  { SingleQuotedStringChar_Escape $1 }

sq_string_escaped ::                                   { SingleQuotedStringEscape }
  : sqt_esc_single_quote                               { SingleQuotedStringEscape_SingleQuote $1 }
  | sqt_esc_backslash                                  { SingleQuotedStringEscape_Backslash $1 }



-- Double Quoted Strings --
---------------------------

double_quoted_string ::                                { DoubleQuotedString }
  : opt(b_prefix) '"' opt(dq_string_chars) '"'         { DoubleQuotedString ($1, DoubleQuote_ $2, $3, DoubleQuote_ $4) }

dq_string_chars ::                                     { DoubleQuotedStringChars }
  : seqOf(dq_string_char)                              { DoubleQuotedStringChars $1 }

dq_string_char ::                                      { DoubleQuotedStringChar }
  : dqt_char                                           { DoubleQuotedStringChar_Char $1 }
  | dq_escape_sequence                                 { DoubleQuotedStringChar_Escape $1 }
  | dq_string_variable                                 { DoubleQuotedStringChar_Variable $1 }

dq_escape_sequence :: { DoubleQuotedEscapeSequence }
  : dq_simple_escape_sequence                          { DoubleQuotedEscapeSequence_Simple  $1 }
  | dq_octal_escape_sequence                           { DoubleQuotedEscapeSequence_Octal   $1 }
  | dq_hex_escape_sequence                             { DoubleQuotedEscapeSequence_Hex     $1 }
  | dq_unicode_escape_sequence                         { DoubleQuotedEscapeSequence_Unicode $1 }

dq_simple_escape_sequence ::                           { DoubleQuotedSimpleEscape }
  : dqt_esc_double_quote                               { DoubleQuotedSimpleEscape_DoubleQuote    $1 }
  | dqt_esc_backslash                                  { DoubleQuotedSimpleEscape_Backslash      $1 }
  | dqt_esc_dollar                                     { DoubleQuotedSimpleEscape_Dollar         $1 }
  | dqt_esc_escape                                     { DoubleQuotedSimpleEscape_Escape         $1 }
  | dqt_esc_form_feed                                  { DoubleQuotedSimpleEscape_FormFeed       $1 }
  | dqt_esc_line_feed                                  { DoubleQuotedSimpleEscape_LineFeed       $1 }
  | dqt_esc_carriage_return                            { DoubleQuotedSimpleEscape_CarriageReturn $1 }
  | dqt_esc_horizontal_tab                             { DoubleQuotedSimpleEscape_HorizontalTab  $1 }
  | dqt_esc_vertical_tab                               { DoubleQuotedSimpleEscape_VerticalTab    $1 }

dq_octal_escape_sequence ::                            { DoubleQuotedOctalEscape }
  : oct_esc                                            { DoubleQuotedOctalEscape $1 }

dq_hex_escape_sequence ::                              { DoubleQuotedHexEscape }
  : hex_esc                                            { DoubleQuotedHexEscape $1 }

dq_unicode_escape_sequence ::                          { DoubleQuotedUnicodeEscape }
  : uni_esc                                            { DoubleQuotedUnicodeEscape $1 }

dq_string_variable ::                                  { DoubleQuotedStringVariable }
  : variable_name_in_string opt(offset_or_property)    { DoubleQuotedStringVariable_Simple ($1, $2) }
  | sy('${') expression '}'                            { DoubleQuotedStringVariable_Complex (fmap DollarOpenBrace_ $1, $2, ClosedBrace_ $3) }
  | '{' variable_name_in_string '}'                    { DoubleQuotedStringVariable_Simplex (OpenBrace_ $1, $2, ClosedBrace_ $3) }

offset_or_property ::                                  { OffsetOrProperty }
  : offset_in_string                                   { OffsetOrProperty_Offset $1 }
  | property_in_string                                 { OffsetOrProperty_Property $1 }

offset_in_string ::                                    { OffsetInString }
  : sy('[') name ']'                                   { OffsetInString_Name (fmap OpenBrack_ $1, $2, ClosedBrack_ $3) }
  | sy('[') variable_name ']'                          { OffsetInString_Variable (fmap OpenBrack_ $1,$2,ClosedBrack_ $3) }
  | sy('[') integer_literal opt(whitespaces) ']'       { OffsetInString_Integer (fmap OpenBrack_ $1,$2,$3,ClosedBrack_ $4) }

property_in_string ::                                  { PropertyInString }
  : '->' name_in_string                                { PropertyInString (MinusGreater_ $1,$2) }



-- Heredocs --
--------------

heredoc_string ::                                      { HeredocString }
  : opt(b_prefix) '<<<' hd_start newline opt(hd_body) hd_end opt(';') newline { HeredocString ($1, TripleLess_ $2, $3, $4, $5, $6, fmap Semicolon_ $7, $8) }

hd_start ::                                            { HeredocStartIdentifier }
  : '"' doc_start '"'                                  {% _reduce "HeredocStartIdentifier_Quoted" $ HeredocStartIdentifier_Quoted (DoubleQuote_ $1, LocString $2, DoubleQuote_ $3) }
  | doc_start                                          {% _reduce "HeredocStartIdentifier_Plain"  $ HeredocStartIdentifier_Plain (LocString $1) }

hd_end ::                                              { HeredocEndIdentifier }
  : doc_end                                            {% _reduce "HeredocEndIdentifier" $ HeredocEndIdentifier (LocString $1) }

hd_body ::                                             { HeredocBody }
  : opt(hd_string_chars) newline                       {% _reduce "HeredocBody" $ HeredocBody ($1, $2) }

hd_string_chars ::                                     { HeredocChars }
  : seqOf(hd_string_char)                              {% _reduce "HeredocChars" $ HeredocChars $1 }

hd_string_char ::                                      { HeredocChar }
  : hdc_char                                           {% _reduce "HeredocChar_Char"     $ HeredocChar_Char $1 }
  | doc_chars                                          {% _reduce "HeredocChar_Chars"    $ HeredocChar_Chars $1 }
  | hd_escape_sequence                                 { HeredocChar_Escape $1 }
  | dq_string_variable                                 {% _reduce "HeredocChar_Variable" $ HeredocChar_Variable $1 }

hd_escape_sequence ::                                  { HeredocEscapeSequence }
  : hd_simple_escape_sequence                          { HeredocEscapeSequence_Simple $1 }
  | dq_octal_escape_sequence                           { HeredocEscapeSequence_Octal $1 }
  | dq_hex_escape_sequence                             { HeredocEscapeSequence_Hex $1 }
  | dq_unicode_escape_sequence                         { HeredocEscapeSequence_Unicode $1 }

hd_simple_escape_sequence ::                           { HeredocSimpleEscape }
  : dqt_esc_backslash                                  { HeredocSimpleEscape_Backslash $1 }
  | dqt_esc_dollar                                     { HeredocSimpleEscape_Dollar $1 }
  | dqt_esc_escape                                     { HeredocSimpleEscape_Escape $1 }
  | dqt_esc_form_feed                                  { HeredocSimpleEscape_FormFeed $1 }
  | dqt_esc_line_feed                                  { HeredocSimpleEscape_LineFeed $1 }
  | dqt_esc_carriage_return                            { HeredocSimpleEscape_CarriageReturn $1 }
  | dqt_esc_horizontal_tab                             { HeredocSimpleEscape_HorizontalTab $1 }
  | dqt_esc_vertical_tab                               { HeredocSimpleEscape_VerticalTab $1 }



-- Nowdocs --
-------------

nowdoc_string ::  { NowdocString }
  : opt(b_prefix) '<<<' '\'' name '\'' newline opt(hd_body) name opt(';') newline { NowdocString ($1, TripleLess_ $2, SingleQuote_ $3, $4, SingleQuote_ $5, $6, $7, $8, fmap Semicolon_ $9, $10) }





----------
-- Ints --
----------

integer_literal ::                                     { IntegerLiteral }
  : decimal_literal                                    { IntegerLiteral_Decimal $1 }
  | hex_literal                                        { IntegerLiteral_Hex $1 }
  | binary_literal                                     { IntegerLiteral_Binary $1 }
  | octal_literal                                      { IntegerLiteral_Octal $1 }



-- Decimal --
-------------

decimal_literal ::                                     { DecimalLiteral }
  : dec_chars                                          { DecimalLiteral $1 }



-- Hexadecimal --
-----------------

hex_literal ::                                         { HexadecimalLiteral }
  : hex_prefix hex_digits                              { HexadecimalLiteral ($1,$2) }

hex_prefix ::                                          { HexadecimalPrefix }
  : '0X'                                               { HexadecimalPrefix_Large $1 }
  | '0x'                                               { HexadecimalPrefix_Small $1 }

hex_digits ::                                          { HexadecimalDigits }
  : hex_chars                                          { HexadecimalDigits $1 }



-- Binary --
------------

binary_literal ::                                      { BinaryLiteral }
  : binary_prefix binary_digits                        { BinaryLiteral ($1,$2) }

binary_prefix ::                                       { BinaryPrefix }
  : '0B'                                               { BinaryPrefix_Large $1 }
  | '0b'                                               { BinaryPrefix_Small $1 }

binary_digits ::                                       { BinaryDigits }
  : bin_chars                                          { BinaryDigits $1 }



-- Octal --
-----------

octal_literal ::                                       { OctalLiteral }
  : octal_prefix octal_digits                          { OctalLiteral ($1,$2) }

octal_prefix ::                                        { OctalPrefix }
  : '0'                                                { OctalPrefix $1 }

octal_digits ::                                        { OctalDigits }
  : oct_chars                                          { OctalDigits $1 }





------------
-- Floats --
------------

floating_literal ::                                    { FloatingLiteral }
  : floating_digit_sequence opt(exponent_part)         { FloatingLiteral ($1,$2) }

floating_digit_sequence ::                             { FloatingDigitSequence }
  : frac_chars                                         { FloatingDigitSequence $1 }

exponent_part ::                                       { ExponentPart }
  : 'E' opt(sign) digit_sequence                       { ExponentPart_Large (LargeE_ $1, $2, $3) }
  | 'e' opt(sign) digit_sequence                       { ExponentPart_Small (SmallE_ $1, $2, $3) }

digit_sequence ::                                      { DigitSequence }
  : exp_chars                                          { DigitSequence $1 }

sign ::                                                { Sign }
  : '+'                                                { Sign_Positive (Plus_ $1) }
  | '-'                                                { Sign_Negative (Minus_ $1) }





---------------
-- Variables --
---------------

variable ::                                            { Variable }
  : callable_variable                                  { Variable_Callable $1 }
  | scoped_property_access_expression                  { Variable_Scoped $1 }
  | member_access_expression                           { Variable_Member $1 }

callable_variable ::                                   { CallableVariable }
  : simple_variable                                    { CallableVariable_Simple    $1 }
  | subscript_expr                                     { CallableVariable_Subscript $1 }
  | member_call_expression                             { CallableVariable_Member    $1 }
  | scoped_call_expression                             { CallableVariable_Scoped    $1 }
  | function_call_expr                                 { CallableVariable_Function  $1 }

simple_variable ::                                     { SimpleVariable }
  : variable_name                                      { SimpleVariable_Name $1 }
  | '$' simple_variable                                { SimpleVariable_Simple (Dollar_ $1, $2) }
  | '$' sy('{') expression sy('}')                     { SimpleVariable_Expression (Dollar_ $1, fmap OpenBrace_ $2, $3, fmap ClosedBrace_ $4) }

variable_name ::                                       { VariableName }
  : '$' name                                           { VariableName ($1,$2) }

name ::                                                { Name }
  : name_chars opt(whitespaces)                        { Name ($1,$2) }

variable_name_in_string ::                             { VariableNameInString }
  : '$' name_in_string                                 { VariableNameInString ($1, $2) }

name_in_string ::                                      { NameInString }
  : name_chars                                         { NameInString $1 }

subscript_expr ::                                      { SubscriptExpression }
  : dereferencable_expr sy('[') opt(expression) sy(']') { SubscriptExpression_Brack ($1, fmap OpenBrack_ $2, $3, fmap ClosedBrack_ $4) }
  | dereferencable_expr sy('{') expression sy('}')     { SubscriptExpression_Brace ($1, fmap OpenBrace_ $2, $3, fmap ClosedBrace_ $4) }

member_call_expression ::                              { MemberCallExpression }
  : dereferencable_expr sy('->') member_name sy('(') member_call_expression_args { MemberCallExpression ($1, fmap MinusGreater_ $2, $3, fmap OpenParen_ $4, $5) }

member_call_expression_args ::                         { MemberCallExpressionArgs }
  : sy(')')                                            { MemberCallExpressionArgs_Empty (fmap ClosedParen_ $1) }
  | argument_expr_list opt(sy(',')) sy(')')            { MemberCallExpressionArgs_List ($1, fmap (fmap Comma_) $2, fmap ClosedParen_ $3) }

scoped_call_expression ::                              { ScopedCallExpression }
  : scope_resolution_qualifier sy('::') member_name sy('(') opt(argument_expr_list) sy(')')   { ScopedCallExpression_List ($1, fmap DoubleColon_ $2, $3, fmap OpenParen_ $4, $5, fmap ClosedParen_ $6) }
  | scope_resolution_qualifier sy('::') member_name sy('(') argument_expr_list sy(',') sy(')')   { ScopedCallExpression_Comma ($1, fmap DoubleColon_ $2, $3, fmap OpenParen_ $4, $5, fmap Comma_ $6, fmap ClosedParen_ $7) }

scope_resolution_qualifier ::                          { ScopeResolutionQualifier }
  : relative_scope                                     { ScopeResolutionQualifier_Relative  $1 }
  | qualified_name                                     { ScopeResolutionQualifier_Qualified $1 }
  | dereferencable_expr                                { ScopeResolutionQualifier_Deref     $1 }

relative_scope ::                                      { RelativeScope }
  : kw('self')                                         { RelativeScope_Self   (fmap Self_   $1) }
  | kw('static')                                       { RelativeScope_Static (fmap Static_ $1) }
  | kw('parent')                                       { RelativeScope_Parent (fmap Parent_ $1) }

qualified_name ::                                      { QualifiedName }
  : opt(namespace_name_as_a_prefix) name               { QualifiedName ($1, $2) }

namespace_name_as_a_prefix ::                          { NamespaceNameAsAPrefix }
  : sy('\\') opt(namespace_name_with_backslash)        { NamespaceNameAsAPrefix_Root (fmap Backslash_ $1, $2) }
  | namespace_name_with_backslash                      { NamespaceNameAsAPrefix_Relative $1 }
  | kw('namespace') sy('\\') opt(namespace_name_with_backslash)  { NamespaceNameAsAPrefix_Namespace (fmap Namespace_ $1, fmap Backslash_ $2, $3) }

namespace_name_with_backslash ::                       { NamespaceNameWithBackslash }
  : namespace_name sy('\\')                            { NamespaceNameWithBackslash ($1, fmap Backslash_ $2) }

function_call_expr ::                                  { FunctionCallExpression }
  : qualified_name sy('(') opt(argument_expr_list) sy(')')  { FunctionCallExpression_NameList ($1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }
  | qualified_name sy('(') argument_expr_list sy(',') sy(')')  { FunctionCallExpression_NameComma ($1, fmap OpenParen_ $2, $3, fmap Comma_ $4, fmap ClosedParen_ $5) }
  | callable_expr sy('(') opt(argument_expr_list) sy(')')  { FunctionCallExpression_CallList ($1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }
  | callable_expr sy('(') argument_expr_list sy(',') sy(')')   { FunctionCallExpression_CallComma ($1, fmap OpenParen_ $2, $3, fmap Comma_ $4, fmap ClosedParen_ $5) }

callable_expr ::                                       { CallableExpression }
  : callable_variable                                  { CallableExpression_Var $1 }
  | sy('(') expression sy(')')                         { CallableExpression_Nest (fmap OpenParen_ $1, $2, fmap ClosedParen_ $3) }
  | array_creation_expression                          { CallableExpression_Array $1 }
  | string_literal                                     { CallableExpression_String $1 }

scoped_property_access_expression ::                   { ScopedPropertyAccessExpression }
  : scope_resolution_qualifier sy('::') simple_variable { ScopedPropertyAccessExpression ($1, fmap DoubleColon_ $2, $3) }

member_access_expression ::                            { MemberAccessExpression }
  : dereferencable_expr sy('->') member_name           { MemberAccessExpression ($1, fmap MinusGreater_ $2, $3) }

member_name ::                                         { MemberName }
  : name                                               { MemberName_Name $1 }
  | simple_variable                                    { MemberName_Variable $1 }
  | sy('{') expression sy('}')                         { MemberName_Nest (fmap OpenBrace_ $1, $2, fmap ClosedBrace_ $3) }

argument_expr_list ::                                  { ArgumentExpressionList }
  : seqOfSep(sy(','), argument_expr)                   { ArgumentExpressionList (mapSep (fmap Comma_) $1) }

argument_expr ::                                       { ArgumentExpression }
  : variadic_unpacking                                 { ArgumentExpression_Variadic $1 }
  | expression                                         { ArgumentExpression_Expression $1 }

variadic_unpacking ::                                  { VariadicUnpacking }
  : sy('...') expression                               { VariadicUnpacking (fmap Ellipsis_ $1, $2) }

dereferencable_expr ::                                 { DereferencableExpression }
  : variable                                           { DereferencableExpression_Variable $1 }
  | sy('(') expression sy(')')                         { DereferencableExpression_Expression (fmap OpenParen_ $1, $2, fmap ClosedParen_ $3) }
  | array_creation_expression                          { DereferencableExpression_Array $1 }
  | string_literal                                     { DereferencableExpression_String $1 }





-----------------
-- Expressions --
-----------------

expression ::                                          { Expression }
  : include_expression                                 { Expression_Include $1 }
  | include_once_expression                            { Expression_IncludeOnce $1 }
  | require_expression                                 { Expression_Require $1 }
  | require_once_expression                            { Expression_RequireOnce $1 }
  | logical_inc_or_expr_2                              { Expression_IncOr $1 }

include_expression ::                                  { IncludeExpression }
  : kw('include')  expression                          { IncludeExpression (fmap Include_ $1,$2) }

include_once_expression ::                             { IncludeOnceExpression }
  : kw('include_once')  expression                     { IncludeOnceExpression (fmap IncludeOnce_ $1, $2) }

require_expression ::                                  { RequireExpression }
  : kw('require') expression                           { RequireExpression (fmap Require_ $1,$2) }

require_once_expression ::                             { RequireOnceExpression }
  : kw('require_once') expression                      { RequireOnceExpression (fmap RequireOnce_ $1, $2) }

logical_inc_or_expr_2 ::                               { LogicalIncOrExpression2 }
  : logical_exc_or_expr                                { LogicalIncOrExpression2_Xor $1 }
  | logical_inc_or_expr_2 kw('or') logical_exc_or_expr { LogicalIncOrExpression2_Or ($1,fmap Or_ $2,$3) }

logical_exc_or_expr ::                                 { LogicalExcOrExpression }
  : logical_and_expr_2                                 { LogicalExcOrExpression_And $1 }
  | logical_exc_or_expr kw('xor') logical_and_expr_2   { LogicalExcOrExpression_Xor ($1,fmap Xor_ $2,$3) }

logical_and_expr_2 ::                                  { LogicalAndExpression2 }
  : print_expr                                         { LogicalAndExpression2_Print $1 }
  | logical_and_expr_2 kw('and') yield_expr            { LogicalAndExpression2_And ($1,fmap And_ $2,$3) }

print_expr ::                                          { PrintExpression }
  : kw('print') yield_expr                             { PrintExpression_Print (fmap Print_ $1,$2) }
  | yield_expr                                         { PrintExpression_Yield $1 }

yield_expr ::                                          { YieldExpression }
  : assignment_expr                                    { YieldExpression_Assignment $1 }
  | kw('yield')                                        { YieldExpression_Plain (fmap Yield_ $1) }
  | kw('yield') assignment_expr                        { YieldExpression_Yield (fmap Yield_ $1, $2) }
  | kw('yield') yield_expr sy('=>') yield_expr         { YieldExpression_Arrow (fmap Yield_ $1, $2, fmap EqualGreater_ $3, $4) }
  | yield_from_expression                              { YieldExpression_From $1 }

yield_from_expression ::                               { YieldFromExpression }
  : kw('yield from') assignment_expr                   { YieldFromExpression (fmap YieldFrom_ $1, $2) }

assignment_expr ::                                     { AssignmentExpression }
  : conditional_expr                                   { AssignmentExpression_Conditional $1 }
  | simple_assignment_expr                             { AssignmentExpression_Simple $1 }
  | compound_assignment_expr                           { AssignmentExpression_Compound $1 }

simple_assignment_expr ::                              { SimpleAssignmentExpression }
  : variable sy('=') assignment_expr                   { SimpleAssignmentExpression_Var ($1, fmap Equal_ $2, $3) }
  | list_intrinsic sy('=') assignment_expr             { SimpleAssignmentExpression_List ($1, fmap Equal_ $2, $3) }

list_intrinsic ::                                      { ListIntrinsic }
  : kw('list') sy('(') list_expression_list sy(')')    { ListIntrinsic (fmap List_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }

list_expression_list ::                                { ListExpressionList }
  : unkeyed_list_expression_list                       { ListExpressionList_Unkeyed $1 }
  | keyed_list_expression_list opt(sy(','))            { ListExpressionList_Keyed ($1, fmap (fmap Comma_) $2) }

unkeyed_list_expression_list ::                        { UnkeyedListExpressionList }
  : list_or_variable                                   { UnkeyedListExpressionList_Head $1 }
  | sy(',')                                            { UnkeyedListExpressionList_Comma (fmap Comma_ $1) }
  | unkeyed_list_expression_list sy(',') opt(list_or_variable) { UnkeyedListExpressionList_Snoc ($1, fmap Comma_ $2, $3) }

keyed_list_expression_list ::                          { KeyedListExpressionList }
  : expression sy('=>') list_or_variable               { KeyedListExpressionList_Head ($1, fmap EqualGreater_ $2, $3) }
  | keyed_list_expression_list sy(',') expression sy('=>') list_or_variable { KeyedListExpressionList_Snoc ($1, fmap Comma_ $2, $3, fmap EqualGreater_ $4, $5) }

list_or_variable ::                                    { ListOrVariable }
  : list_intrinsic                                     { ListOrVariable_List $1 }
  | opt(sy('&')) variable                              { ListOrVariable_Var (fmap (fmap Amp_) $1, $2) }

compound_assignment_expr ::                            { CompoundAssignmentExpression }
  : variable compound_assignment_operator assignment_expr { CompoundAssignmentExpression ($1, $2, $3) }

compound_assignment_operator ::                        { CompoundAssignmentOperator }
  : sy('**=')                                          { CompoundAssignmentOperator_AstAst (fmap AstAstEqual_ $1) }
  | sy('*=')                                           { CompoundAssignmentOperator_Ast (fmap AstEqual_ $1) }
  | sy('/=')                                           { CompoundAssignmentOperator_Slash (fmap SlashEqual_ $1) }
  | sy('%=')                                           { CompoundAssignmentOperator_Percent (fmap PercentEqual_ $1) }
  | sy('+=')                                           { CompoundAssignmentOperator_Plus (fmap PlusEqual_ $1) }
  | sy('-=')                                           { CompoundAssignmentOperator_Minus (fmap MinusEqual_ $1) }
  | sy('.=')                                           { CompoundAssignmentOperator_Dot (fmap DotEqual_ $1) }
  | sy('<<=')                                          { CompoundAssignmentOperator_LessLess (fmap DoubleLessEqual_ $1) }
  | sy('>>=')                                          { CompoundAssignmentOperator_GreaterGreater (fmap DoubleGreaterEqual_ $1) }
  | sy('&=')                                           { CompoundAssignmentOperator_Amp (fmap AmpEqual_ $1) }
  | sy('^=')                                           { CompoundAssignmentOperator_Caret (fmap CaretEqual_ $1) }
  | sy('|=')                                           { CompoundAssignmentOperator_Pipe (fmap PipeEqual_ $1) }

conditional_expr ::                                    { ConditionalExpression }
  : coalesce_expr                                      { ConditionalExpression_Coalesce $1 }
  | conditional_expr sy('?') opt(expression) sy(':') coalesce_expr { ConditionalExpression_Conditional ($1,fmap Question_ $2,$3,fmap Colon_ $4,$5) }

coalesce_expr ::                                       { CoalesceExpression }
  : logical_inc_or_expr_1                              { CoalesceExpression_Or $1 }
  | logical_inc_or_expr_1 sy('??') coalesce_expr       { CoalesceExpression_Coalesce ($1,fmap DoubleQuestion_ $2,$3) }

logical_inc_or_expr_1 ::                               { LogicalIncOrExpression1 }
  : logical_and_expr_1                                 { LogicalIncOrExpression1_And $1 }
  | logical_inc_or_expr_1 sy('||') logical_and_expr_1  { LogicalIncOrExpression1_Or ($1, fmap DoublePipe_ $2,$3) }

logical_and_expr_1 ::                                  { LogicalAndExpression1 }
  : bitwise_inc_or_expr                                { LogicalAndExpression1_BitwiseOr $1 }
  | logical_and_expr_1 sy('&&') bitwise_inc_or_expr    { LogicalAndExpression1_And ($1, fmap AmpAmp_ $2,$3) }

bitwise_inc_or_expr ::                                 { BitwiseIncOrExpression }
  : bitwise_exc_or_expr                                { BitwiseIncOrExpression_ExcOr $1 }
  | bitwise_inc_or_expr sy('|') bitwise_exc_or_expr    { BitwiseIncOrExpression_IncOr ($1, fmap Pipe_ $2,$3) }

bitwise_exc_or_expr ::                                 { BitwiseExcOrExpression }
  : bitwise_and_expr                                   { BitwiseExcOrExpression_And $1 }
  | bitwise_exc_or_expr sy('^') bitwise_and_expr       { BitwiseExcOrExpression_ExcOr ($1, fmap Caret_ $2,$3) }

bitwise_and_expr ::                                    { BitwiseAndExpression }
  : equality_expr                                      { BitwiseAndExpression_Equality $1 }
  | bitwise_and_expr sy('&') equality_expr             { BitwiseAndExpression_And ($1, fmap Amp_ $2,$3) }

equality_expr ::                                       { EqualityExpression }
  : relational_expr                                    { EqualityExpression_Relational $1 }
  | equality_expr sy('==')  relational_expr            { EqualityExpression_DoubleEqual    ($1, fmap DoubleEqual_    $2, $3) }
  | equality_expr sy('!=')  relational_expr            { EqualityExpression_BangEqual      ($1, fmap BangEqual_      $2, $3) }
  | equality_expr sy('<>')  relational_expr            { EqualityExpression_LessGreater    ($1, fmap LessGreater_    $2, $3) }
  | equality_expr sy('===') relational_expr            { EqualityExpression_TripleEqual    ($1, fmap TripleEqual_    $2, $3) }
  | equality_expr sy('!==') relational_expr            { EqualityExpression_BangEqualEqual ($1, fmap BangEqualEqual_ $2, $3) }

relational_expr ::                                     { RelationalExpression }
  : shift_expr                                         { RelationalExpression_Shift $1 }
  | relational_expr sy('<') shift_expr                 { RelationalExpression_Less         ($1, fmap Less_         $2, $3) }
  | relational_expr sy('>') shift_expr                 { RelationalExpression_Greater      ($1, fmap Greater_      $2, $3) }
  | relational_expr sy('<=') shift_expr                { RelationalExpression_LessEqual    ($1, fmap LessEqual_    $2, $3) }
  | relational_expr sy('>=') shift_expr                { RelationalExpression_GreaterEqual ($1, fmap GreaterEqual_ $2, $3) }
  | relational_expr sy('<=>') shift_expr               { RelationalExpression_Spaceship    ($1, fmap Spaceship_    $2, $3) }

shift_expr ::                                          { ShiftExpression }
  : additive_expr                                      { ShiftExpression_Additive $1 }
  | shift_expr sy('<<') additive_expr                  { ShiftExpression_Left  ($1, fmap DoubleLess_    $2, $3) }
  | shift_expr sy('>>') additive_expr                  { ShiftExpression_Right ($1, fmap DoubleGreater_ $2, $3) }

additive_expr ::                                       { AdditiveExpression }
  : multiplicative_expr                                { AdditiveExpression_Multiplicative $1 }
  | additive_expr sy('+') multiplicative_expr          { AdditiveExpression_Plus  ($1, fmap Plus_  $2, $3) }
  | additive_expr sy('-') multiplicative_expr          { AdditiveExpression_Minus ($1, fmap Minus_ $2, $3) }
  | additive_expr sy('.') multiplicative_expr          { AdditiveExpression_Dot   ($1, fmap Dot_   $2, $3) }

multiplicative_expr ::                                 { MultiplicativeExpression }
  : logical_not_expr                                   { MultiplicativeExpression_Not $1 }
  | multiplicative_expr sy('*') logical_not_expr       { MultiplicativeExpression_Ast     ($1, fmap Ast_     $2, $3) }
  | multiplicative_expr sy('/') logical_not_expr       { MultiplicativeExpression_Slash   ($1, fmap Slash_   $2, $3) }
  | multiplicative_expr sy('%') logical_not_expr       { MultiplicativeExpression_Percent ($1, fmap Percent_ $2, $3) }

logical_not_expr ::                                    { LogicalNotExpression }
  : instanceof_expr                                    { LogicalNotExpression_Instanceof $1 }
  | sy('!') instanceof_expr                            { LogicalNotExpression_Not (fmap Bang_ $1, $2) }

instanceof_expr ::                                     { InstanceofExpression }
  : unary_expr                                         { InstanceofExpression_Unary $1 }
  | instanceof_subject kw('instanceof') class_type_designator { InstanceofExpression_Instanceof ($1, fmap Instanceof_ $2, $3) }

instanceof_subject ::                                  { InstanceofSubject }
  : instanceof_expr                                    { InstanceofSubject $1 }

unary_expr ::                                          { UnaryExpression }
  : exponentiation_expr                                { UnaryExpression_Exp   $1 }
  | unary_op_expression                                { UnaryExpression_Op    $1 }
  | error_control_expression                           { UnaryExpression_Error $1 }
  | cast_expression                                    { UnaryExpression_Cast  $1 }

error_control_expression ::                            { ErrorControlExpression }
  : sy('@') unary_expr                                 { ErrorControlExpression (fmap At_ $1, $2) }

cast_expression ::                                     { CastExpression }
  : sy('(') cast_type sy(')') unary_expr               { CastExpression (fmap OpenParen_ $1, $2, fmap ClosedParen_ $3, $4) }

cast_type ::                                           { CastType }
  : kw('array')                                        { CastType_Array   (fmap Array_   $1) }
  | kw('binary')                                       { CastType_Binary  (fmap Binary_  $1) }
  | kw('bool')                                         { CastType_Bool    (fmap Bool_    $1) }
  | kw('boolean')                                      { CastType_Boolean (fmap Boolean_ $1) }
  | kw('double')                                       { CastType_Double  (fmap Double_  $1) }
  | kw('int')                                          { CastType_Int     (fmap Int_     $1) }
  | kw('integer')                                      { CastType_Integer (fmap Integer_ $1) }
  | kw('float')                                        { CastType_Float   (fmap Float_   $1) }
  | kw('object')                                       { CastType_Object  (fmap Object_  $1) }
  | kw('real')                                         { CastType_Real    (fmap Real_    $1) }
  | kw('string')                                       { CastType_String  (fmap String_  $1) }
  | kw('unset')                                        { CastType_Unset   (fmap Unset_   $1) }

unary_op_expression ::                                 { UnaryOpExpression }
  : unary_operator unary_expr                          { UnaryOpExpression ($1, $2) }

unary_operator ::                                      { UnaryOperator }
  : sy('+')                                            { UnaryOperator_Plus  (fmap Plus_  $1) }
  | sy('-')                                            { UnaryOperator_Minus (fmap Minus_ $1) }
  | sy('~')                                            { UnaryOperator_Tilde (fmap Tilde_ $1) }

exponentiation_expr ::                                 { ExponentiationExpression }
  : clone_expr                                         { ExponentiationExpression_Clone $1 }
  | clone_expr sy('**') exponentiation_expr            { ExponentiationExpression_Exp ($1, fmap AstAst_ $2, $3) }

clone_expr ::                                          { CloneExpression }
  : primary_expr                                       { CloneExpression_Primary $1 }
  | kw('clone') primary_expr                           { CloneExpression_Clone (fmap Clone_ $1, $2) }

primary_expr ::                                        { PrimaryExpression }
  : variable                                           { PrimaryExpression_Variable   $1 }
  | literal                                            { PrimaryExpression_Literal    $1 }
  | postfix_increment_expr                             { PrimaryExpression_PostfixInc $1 }
  | postfix_decrement_expr                             { PrimaryExpression_PostfixDec $1 }
  | prefix_increment_expr                              { PrimaryExpression_PrefixInc  $1 }
  | prefix_decrement_expr                              { PrimaryExpression_PrefixDec  $1 }
  | intrinsic                                          { PrimaryExpression_Intrinsic  $1 }
  | class_constant_access_expression                   { PrimaryExpression_Class      $1 }
  | constant_access_expression                         { PrimaryExpression_Constant   $1 }
  | anonymous_function_creation_expression             { PrimaryExpression_Anonymous  $1 }
  | array_creation_expression                          { PrimaryExpression_Array      $1 }
  | object_creation_expression                         { PrimaryExpression_Object     $1 }
  | shell_command_expr                                 { PrimaryExpression_Shell      $1 }
  | byref_assignment_expr                              { PrimaryExpression_Byref      $1 }
  | sy('(') expression sy(')')                         { PrimaryExpression_Expression (fmap OpenParen_ $1,$2, fmap ClosedParen_ $3) }

literal ::                                             { Literal }
  : integer_literal                                    { Literal_Int    $1 }
  | floating_literal                                   { Literal_Float  $1 }
  | string_literal                                     { Literal_String $1 }

postfix_increment_expr ::                              { PostfixIncrementExpression }
  : variable sy('++')                                  { PostfixIncrementExpression ($1, fmap DoublePlus_ $2) }

postfix_decrement_expr ::                              { PostfixDecrementExpression }
  : variable sy('--')                                  { PostfixDecrementExpression ($1, fmap DoubleMinus_ $2) }

prefix_increment_expr ::                               { PrefixIncrementExpression }
  : sy('++') variable                                  { PrefixIncrementExpression (fmap DoublePlus_ $1, $2) }

prefix_decrement_expr ::                               { PrefixDecrementExpression }
  : sy('--') variable                                  { PrefixDecrementExpression (fmap DoubleMinus_ $1, $2) }

intrinsic ::                                           { Intrinsic }
  : empty_intrinsic                                    { Intrinsic_Empty $1 }
  | eval_intrinsic                                     { Intrinsic_Eval  $1 }
  | isset_intrinsic                                    { Intrinsic_Isset $1 }
  | exit_intrinsic                                     { Intrinsic_Exit  $1 }

empty_intrinsic ::                                     { EmptyIntrinsic }
  : kw('empty') sy('(') expression sy(')')             { EmptyIntrinsic (fmap Empty_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }

eval_intrinsic ::                                      { EvalIntrinsic }
  : kw('eval') sy('(') expression sy(')')              { EvalIntrinsic (fmap Eval_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }

isset_intrinsic ::                                     { IssetIntrinsic }
  : kw('isset') sy('(') variable_list opt(sy(',')) sy(')')  { IssetIntrinsic (fmap Isset_ $1, fmap OpenParen_ $2, $3, fmap (fmap Comma_) $4, fmap ClosedParen_ $5) }

exit_intrinsic ::                                      { ExitIntrinsic }
  : kw('exit')                                         { ExitIntrinsic_Exit (fmap Exit_ $1) }
  | kw('exit') sy('(') opt(expression) sy(')')         { ExitIntrinsic_ExitArg (fmap Exit_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }
  | kw('die')                                          { ExitIntrinsic_Die (fmap Die_ $1) }
  | kw('die') sy('(') opt(expression) sy(')')          { ExitIntrinsic_DieArg (fmap Die_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }

class_constant_access_expression ::                    { ClassConstantAccessExpression }
  : scope_resolution_qualifier sy('::') name           { ClassConstantAccessExpression ($1, fmap DoubleColon_ $2, $3) }

constant_access_expression ::                          { ConstantAccessExpression }
  : qualified_name                                     { ConstantAccessExpression $1 }

anonymous_function_creation_expression ::              { AnonymousFunctionCreationExpression }
  : opt(kw('static')) kw('function') opt(sy('&')) sy('(') opt(parameter_declaration_list) sy(')') opt(anonymous_function_use_clause) opt(return_type) compound_statement  { AnonymousFunctionCreationExpression (fmap (fmap Static_) $1, fmap Function_ $2, fmap (fmap Amp_) $3, fmap OpenParen_ $4, $5, fmap ClosedParen_ $6, $7, $8, $9) }

use_variable_name_list ::                              { UseVariableNameList }
  : opt(sy('&')) variable_name { UseVariableNameList_Head (fmap (fmap Amp_) $1, $2) }
  | use_variable_name_list sy(',') opt(sy('&')) variable_name { UseVariableNameList_Snoc ($1, fmap Comma_ $2, fmap (fmap Amp_) $3, $4) }

anonymous_function_use_clause ::                       { AnonymousFunctionUseClause }
  : kw('use') sy('(') use_variable_name_list sy(')')   { AnonymousFunctionUseClause (fmap Use_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }

array_creation_expression ::                           { ArrayCreationExpression }
  : kw('array') sy('(') opt(array_initializer) sy(')') { ArrayCreationExpression_Array (fmap Array_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4) }
  | sy('[') opt(array_initializer) sy(']')             { ArrayCreationExpression_Brack (fmap OpenBrack_ $1, $2, fmap ClosedBrack_ $3) }

array_initializer ::                                   { ArrayInitializer }
  : array_initializer_list opt(sy(','))                { ArrayInitializer ($1, fmap (fmap Comma_) $2) }

array_initializer_list ::                              { ArrayInitializerList }
  : array_element_initializer                          { ArrayInitializerList_Head $1 }
  | array_element_initializer sy(',') array_initializer_list { ArrayInitializerList_Cons ($1, fmap Comma_ $2, $3) }

array_element_initializer ::                           { ArrayElementInitializer }
  : opt(sy('&')) element_value                         { ArrayElementInitializer_Plain (fmap (fmap Amp_) $1, $2) }
  | element_key sy('=>') opt(sy('&')) element_value    { ArrayElementInitializer_Arrow ($1, fmap EqualGreater_ $2, fmap (fmap Amp_) $3, $4) }

element_key ::                                         { ElementKey }
  : expression                                         { ElementKey $1 }

element_value ::                                       { ElementValue }
  : expression                                         { ElementValue $1 }

object_creation_expression ::                          { ObjectCreationExpression }
  : kw('new') class_type_designator sy('(') opt(argument_expr_list) sy(')')  { ObjectCreationExpression_List (fmap New_ $1, $2, fmap OpenParen_ $3, $4, fmap ClosedParen_ $5) }
  | kw('new') class_type_designator sy('(') argument_expr_list opt(sy(',')) sy(')')  { ObjectCreationExpression_Comma (fmap New_ $1, $2, fmap OpenParen_ $3, $4, fmap (fmap Comma_) $5, fmap ClosedParen_ $6) }
  | kw('new') class_type_designator  { ObjectCreationExpression_Empty (fmap New_ $1, $2) }
  | kw('new') kw('class') sy('(') opt(argument_expr_list) sy(')') opt(class_base_clause) opt(class_interface_clause) sy('{') opt(class_member_declarations) sy('}')  { ObjectCreationExpression_Args (fmap New_ $1, fmap Class_ $2, fmap OpenParen_ $3, $4, fmap ClosedParen_ $5, $6, $7, fmap OpenBrace_ $8, $9, fmap ClosedBrace_ $10) }
  | kw('new') kw('class') opt(class_base_clause) opt(class_interface_clause) sy('{') opt(class_member_declarations) sy('}')   { ObjectCreationExpression_Plain (fmap New_ $1, fmap Class_ $2, $3, $4, fmap OpenBrace_ $5, $6, fmap ClosedBrace_ $7) }

class_type_designator ::                               { ClassTypeDesignator }
  : qualified_name                                     { ClassTypeDesignator_Qualified $1 }
  | new_variable                                       { ClassTypeDesignator_New $1 }

new_variable ::                                        { NewVariable }
  : simple_variable                                    { NewVariable_Simple $1 }
  | new_variable sy('[') opt(expression) sy(']')       { NewVariable_Brack ($1, fmap OpenBrack_ $2, $3, fmap ClosedBrack_ $4) }
  | new_variable sy('{') expression sy('}')            { NewVariable_Brace ($1, fmap OpenBrace_ $2, $3, fmap ClosedBrace_ $4) }
  | new_variable sy('->') member_name                  { NewVariable_Member ($1, fmap MinusGreater_ $2, $3) }
  | qualified_name sy('::') simple_variable            { NewVariable_Qualified ($1, fmap DoubleColon_ $2, $3) }
  | relative_scope sy('::') simple_variable            { NewVariable_Relative ($1, fmap DoubleColon_ $2, $3) }
  | new_variable sy('::') simple_variable              { NewVariable_Colon ($1, fmap DoubleColon_ $2, $3) }

class_base_clause ::                                   { ClassBaseClause }
  : kw('extends') qualified_name                       { ClassBaseClause (fmap Extends_ $1, $2) }

class_interface_clause ::                              { ClassInterfaceClause }
  : kw('implements') qualified_name                    { ClassInterfaceClause_Implements (fmap Implements_ $1, $2) }
  | class_interface_clause sy(',') qualified_name      { ClassInterfaceClause_List ($1, fmap Comma_ $2, $3) }

class_member_declarations ::                           { ClassMemberDeclarations }
  : seqOf(class_member_declaration)                    { ClassMemberDeclarations $1 }

class_member_declaration ::                            { ClassMemberDeclaration }
  : class_const_declaration                            { ClassMemberDeclaration_Const     $1 }
  | property_declaration                               { ClassMemberDeclaration_Property  $1 }
  | method_declaration                                 { ClassMemberDeclaration_Method    $1 }
  | constructor_declaration                            { ClassMemberDeclaration_Construct $1 }
  | destructor_declaration                             { ClassMemberDeclaration_Destruct  $1 }
  | trait_use_clause                                   { ClassMemberDeclaration_Trait     $1 }

class_const_declaration ::                             { ClassConstDeclaration }
  : opt(visibility_modifier) kw('const') const_elements sy(';') { ClassConstDeclaration ($1, fmap Const_ $2, $3, fmap Semicolon_ $4) }

shell_command_expr ::                                  { ShellCommandExpression }
  : '`' opt(dq_string_chars) '`'                       { ShellCommandExpression (Backtick_ $1, $2, Backtick_ $3) }

byref_assignment_expr ::                               { ByrefAssignmentExpression }
  : variable sy('=') sy('&') variable                  { ByrefAssignmentExpression ($1, fmap Equal_ $2, fmap Amp_ $3, $4) }





----------------
-- Statements --
----------------

statement_list ::                                      { StatementList }
  : seqOf(statement)                                   { StatementList $1 }

statement ::                                           { Statement }
  : compound_statement                                 { Statement_CompoundStatement   $1 }
  | named_label_statement                              { Statement_NamedLabelStatement $1 }
  | expression_stmt                                    { Statement_Expression          $1 }
  | namespace_definition                               { Statement_Namespace           $1 }
  | jump_statement                                     { Statement_JumpStatement       $1 }
  | selection_statement                                { Statement_Selection           $1 }
  | try_statement                                      { Statement_TryStatement        $1 }
  | iteration_stmt                                     { Statement_Iteration           $1 }
  | declare_stmt                                       { Statement_Declare             $1 }
  | echo_stmt                                          { Statement_Echo                $1 }
  | unset_stmt                                         { Statement_Unset               $1 }
  | const_declaration                                  { Statement_Const               $1 }
  | function_definition                                { Statement_Function            $1 }
  | class_declaration                                  { Statement_Class               $1 }
  | interface_declaration                              { Statement_Interface           $1 }
  | trait_declaration                                  { Statement_Trait               $1 }
  | global_declaration                                 { Statement_Global              $1 }
  | namespace_use_declaration                          { Statement_NamespaceUse        $1 }
  | function_static_declaration                        { Statement_FunctionStatic      $1 }

compound_statement ::                                  { CompoundStatement }
  : sy('{') opt(statement_list) sy('}')                { CompoundStatement (fmap OpenBrace_ $1, $2, fmap ClosedBrace_ $3) }

named_label_statement ::                               { NamedLabelStatement }
  : name sy(':')                                       { NamedLabelStatement ($1, fmap Colon_ $2) }

expression_stmt ::                                     { ExpressionStatement }
  : opt(expression) sy(';')                            { ExpressionStatement ($1, fmap Semicolon_ $2) }

namespace_definition ::                                { NamespaceDefinition }
  : kw('namespace') name sy(';')                       { NamespaceDefinition_Name (fmap Namespace_ $1, $2, fmap Semicolon_ $3) }
  | kw('namespace') opt(name) compound_statement       { NamespaceDefinition_Compound (fmap Namespace_ $1, $2, $3) }



-- Jump Statements --
---------------------

jump_statement ::                                      { JumpStatement }
  : goto_statement                                     { JumpStatement_Goto $1 }
  | continue_statement                                 { JumpStatement_Continue $1 }
  | break_statement                                    { JumpStatement_Break $1 }
  | return_statement                                   { JumpStatement_Return $1 }
  | throw_statement                                    { JumpStatement_Throw $1 }

goto_statement ::                                      { GotoStatement }
  : kw('goto') name sy(';')                            { GotoStatement (fmap Goto_ $1, $2, fmap Semicolon_ $3) }

continue_statement ::                                  { ContinueStatement }
  : kw('continue') opt(breakout_level) sy(';')         { ContinueStatement (fmap Continue_ $1, $2, fmap Semicolon_ $3) }

break_statement ::                                     { BreakStatement }
  : kw('break') opt(breakout_level) sy(';')            { BreakStatement (fmap Break_ $1, $2, fmap Semicolon_ $3) }

breakout_level ::                                      { BreakoutLevel }
  : integer_literal                                    { BreakoutLevel_Int $1 }
  | sy('(') breakout_level sy(')')                     { BreakoutLevel_Nest (fmap OpenParen_ $1, $2, fmap ClosedParen_ $3) }

return_statement ::                                    { ReturnStatement }
  : kw('return') opt(expression) sy(';')               { ReturnStatement (fmap Return_ $1, $2, fmap Semicolon_ $3) }

throw_statement ::                                     { ThrowStatement }
  : kw('throw') expression sy(';')                     { ThrowStatement (fmap Throw_ $1, $2, fmap Semicolon_ $3) }



-- Selection Statements --
--------------------------

selection_statement ::                                 { SelectionStatement }
  : if_statement                                       { SelectionStatement_If $1 }
  | switch_statement                                   { SelectionStatement_Switch $1 }



if_statement ::                                        { IfStatement }
  : kw('if') sy('(') expression sy(')') if_statement_body { IfStatement (fmap If_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4, $5) }

if_statement_body ::                                   { IfStatementBody }
  : statement opt(elseif_clauses_1) opt(else_clause_1) { IfStatementBody_Brace ($1, $2, $3) }
  | sy(':') statement_list opt(elseif_clauses_2) opt(else_clause_2) kw('endif') sy(';') { IfStatementBody_Colon (fmap Colon_ $1, $2, $3, $4, fmap Endif_ $5, fmap Semicolon_ $6) }

elseif_clauses_1 ::                                    { ElseifClauses1 }
  : seqOf(elseif_clause_1)                             { ElseifClauses1 $1 }

elseif_clause_1 ::                                     { ElseifClause1 }
  : kw('elseif') sy('(') expression sy(')') statement  { ElseifClause1 (fmap Elseif_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4, $5) }

else_clause_1 ::                                       { ElseClause1 }
  : kw('else') statement                               { ElseClause1 (fmap Else_ $1, $2) }

elseif_clauses_2 ::                                    { ElseifClauses2 }
  : seqOf(elseif_clause_2)                             { ElseifClauses2 $1 }

elseif_clause_2 ::                                     { ElseifClause2 }
  : kw('elseif') sy('(') expression sy(')') sy(':') statement_list { ElseifClause2 (fmap Elseif_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4, fmap Colon_ $5, $6) }

else_clause_2 ::                                       { ElseClause2 }
  : kw('else') sy(':') statement_list                  { ElseClause2 (fmap Else_ $1, fmap Colon_ $2, $3) }



switch_statement ::                                    { SwitchStatement }
  : kw('switch') sy('(') expression sy(')') switch_statement_body    { SwitchStatement (fmap Switch_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4, $5) }

switch_statement_body ::                               { SwitchStatementBody }
  : sy('{') opt(case_statements) sy('}') { SwitchStatementBody_Brace (fmap OpenBrace_ $1, $2, fmap ClosedBrace_ $3) }
  | sy(':') opt(case_statements) kw('endswitch') sy(';')  { SwitchStatementBody_Colon (fmap Colon_ $1, $2, fmap Endswitch_ $3, fmap Semicolon_ $4) }

case_statements ::                                     { CaseStatements }
  : case_statement opt(case_statements)                { CaseStatements_Case ($1, $2) }
  | default_statement opt(case_statements)             { CaseStatements_Default ($1, $2) }

case_statement ::                                      { CaseStatement }
  : kw('case') expression case_default_label_terminator opt(statement_list)  { CaseStatement (fmap Case_ $1, $2, $3, $4) }

default_statement ::                                   { DefaultStatement }
  : kw('default') case_default_label_terminator opt(statement_list)  { DefaultStatement (fmap Default_ $1, $2, $3) }

case_default_label_terminator ::                       { CaseDefaultLabelTerminator }
  : sy(':')                                            { CaseDefaultLabelTerminator_Colon (fmap Colon_ $1) }
  | sy(';')                                            { CaseDefaultLabelTerminator_Semicolon (fmap Semicolon_ $1) }



-- Try Statements --
--------------------

try_statement ::                                       { TryStatement }
  : kw('try') compound_statement try_statement_body    { TryStatement (fmap Try_ $1, $2, $3) }

try_statement_body ::                                  { TryStatementBody }
  : catch_clauses opt(finally_clause)                  { TryStatementBody_Catch ($1, $2) }
  | finally_clause                                     { TryStatementBody_Finally $1 }

catch_clauses ::                                       { CatchClauses }
  : seqOf(catch_clause)                                { CatchClauses $1 }

catch_clause ::                                        { CatchClause }
  : kw('catch') sy('(') catch_name_list variable_name sy(')') compound_statement { CatchClause (fmap Catch_ $1, fmap OpenParen_ $2, $3, $4, fmap ClosedParen_ $5, $6) }

catch_name_list ::                                     { CatchNameList }
  : seqOfSep(sy('|'), qualified_name)                  { CatchNameList (mapSep (fmap Pipe_) $1) }

finally_clause ::                                      { FinallyClause }
  : kw('finally') compound_statement                   { FinallyClause (fmap Finally_ $1, $2) }



-- Iteration Statements --
--------------------------

iteration_stmt ::                                      { IterationStatement }
  : while_stmt                                         { IterationStatement_While   $1 }
  | do_statement                                       { IterationStatement_Do      $1 }
  | for_stmt                                           { IterationStatement_For     $1 }
  | foreach_stmt                                       { IterationStatement_Foreach $1 }



while_stmt ::                                          { WhileStatement }
  : kw('while') sy('(') expression sy(')') while_stmt_body { WhileStatement (fmap While_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4, $5) }

while_stmt_body ::                                     { WhileStatementBody }
  : statement                                          { WhileStatementBody_Brace $1 }
  | sy(':') statement_list kw('endwhile') sy(';')      { WhileStatementBody_Colon (fmap Colon_ $1, $2, fmap Endwhile_ $3, fmap Semicolon_ $4) }



do_statement ::                                        { DoStatement }
  : kw('do') statement kw('while') sy('(') expression sy(')') sy(';')  { DoStatement (fmap Do_ $1, $2, fmap While_ $3, fmap OpenParen_ $4, $5, fmap ClosedParen_ $6, fmap Semicolon_ $7) }



for_stmt ::                                            { ForStatement }
  : kw('for') sy('(') opt(for_initializer) sy(';') opt(for_control) sy(';') opt(for_end_of_loop) sy(')') for_stmt_body { ForStatement (fmap For_ $1, fmap OpenParen_ $2, $3, fmap Semicolon_ $4, $5, fmap Semicolon_ $6, $7, fmap ClosedParen_ $8, $9) }

for_stmt_body ::                                       { ForStatementBody }
  : statement                                          { ForStatementBody_Brace $1 }
  | sy(':') statement_list kw('endfor') sy(';')        { ForStatementBody_Colon (fmap Colon_ $1, $2, fmap Endfor_ $3, fmap Semicolon_ $4) }

for_initializer ::                                     { ForInitializer }
  : for_expression_group                               { ForInitializer $1 }

for_control ::                                         { ForControl }
  : for_expression_group                               { ForControl $1 }

for_end_of_loop ::                                     { ForEndOfLoop }
  : for_expression_group                               { ForEndOfLoop $1 }

for_expression_group ::                                { ForExpressionGroup }
  : seqOfSep(sy(','), expression)                      { ForExpressionGroup (mapSep (fmap Comma_) $1) }



foreach_stmt ::                                        { ForeachStatement }
  : kw('foreach') sy('(') foreach_collection_name kw('as') opt(foreach_key) foreach_value sy(')') foreach_stmt_body { ForeachStatement (fmap Foreach_ $1, fmap OpenParen_ $2, $3, fmap As_ $4, $5, $6, fmap ClosedParen_ $7, $8) }

foreach_stmt_body ::                                   { ForeachStatementBody }
  : statement                                          { ForeachStatementBody_Brace $1 }
  | sy(':') statement_list kw('endforeach') sy(';')    { ForeachStatementBody_Colon (fmap Colon_ $1, $2, fmap Endforeach_ $3, fmap Semicolon_ $4) }

foreach_collection_name ::                             { ForeachCollectionName }
  : expression                                         { ForeachCollectionName $1 }

foreach_key ::                                         { ForeachKey }
  : expression sy('=>')                                { ForeachKey ($1, fmap EqualGreater_ $2) }

foreach_value ::                                       { ForeachValue }
  : opt(sy('&')) expression                            { ForeachValue_Expr (fmap (fmap Amp_) $1, $2) }
  | list_intrinsic                                     { ForeachValue_List $1 }



-- Declare Statements --
------------------------

declare_stmt ::                                        { DeclareStatement }
  : kw('declare') sy('(') declare_directive sy(')') declare_stmt_body { DeclareStatement (fmap Declare_ $1, fmap OpenParen_ $2, $3, fmap ClosedParen_ $4, $5) }

declare_stmt_body ::                                   { DeclareStatementBody }
  : statement                                          { DeclareStatementBody_Plain $1 }
  | sy(':') statement_list kw('enddeclare') sy(';')    { DeclareStatementBody_Colon (fmap Colon_ $1, $2, fmap Enddeclare_ $3, fmap Semicolon_ $4) }
  | sy(';')                                            { DeclareStatementBody_Empty (fmap Semicolon_ $1) }

declare_directive ::                                   { DeclareDirective }
  : kw('ticks') sy('=') literal                        { DeclareDirective_Ticks (fmap Ticks_ $1, fmap Equal_ $2, $3) }
  | kw('encoding') sy('=') literal                     { DeclareDirective_Encoding (fmap Encoding_ $1, fmap Equal_ $2, $3) }
  | kw('strict_types') sy('=') literal                 { DeclareDirective_StrictTypes (fmap StrictTypes_ $1, fmap Equal_ $2, $3) }



-- Echo Statements --
---------------------

echo_stmt ::                                           { EchoStatement }
  : kw('echo') expression_list sy(';')                 { EchoStatement (fmap Echo_ $1, $2, fmap Semicolon_ $3) }

expression_list ::                                     { ExpressionList }
  : seqOfSep(sy(','), expression)                      { ExpressionList (mapSep (fmap Comma_) $1) }



-- Unset Statements --
----------------------

unset_stmt ::                                          { UnsetStatement }
  : kw('unset') sy('(') variable_list opt(sy(',')) sy(')') sy(';')  { UnsetStatement (fmap Unset_ $1, fmap OpenParen_ $2, $3, fmap (fmap Comma_) $4, fmap ClosedParen_ $5, fmap Semicolon_ $6) }

variable_list ::                                       { VariableList }
  : seqOfSep(sy(','), variable)                        { VariableList (mapSep (fmap Comma_) $1) }



-- Const Declarations --
------------------------

const_declaration ::                                   { ConstDeclaration }
  : kw('const') const_elements sy(';')                 { ConstDeclaration (fmap Const_ $1, $2, fmap Semicolon_ $3) }

const_elements ::                                      { ConstElements }
  : seqOfSep(sy(','), const_element)                   { ConstElements (mapSep (fmap Comma_) $1) }

const_element ::                                       { ConstElement }
  : name sy('=') constant_expr                         { ConstElement ($1, fmap Equal_ $2, $3) }

constant_expr ::                                       { ConstantExpression }
  : expression                                         { ConstantExpression $1 }



-- Function Definition --
-------------------------

function_definition ::                                 { FunctionDefinition }
  : function_definition_header compound_statement      { FunctionDefinition ($1, $2) }

function_definition_header ::                          { FunctionDefinitionHeader }
  : kw('function') opt(sy('&')) name sy('(') opt(parameter_declaration_list) sy(')') opt(return_type) { FunctionDefinitionHeader (fmap Function_ $1, fmap (fmap Amp_) $2, $3, fmap OpenParen_ $4, $5, fmap ClosedParen_ $6, $7) }

parameter_declaration_list ::                          { ParameterDeclarationList }
  : simple_parameter_declaration_list                  { ParameterDeclarationList_Simple $1 }
  | variadic_declaration_list                          { ParameterDeclarationList_Variadic $1 }

simple_parameter_declaration_list ::                   { SimpleParameterDeclarationList }
  : seqOfSep(sy(','), parameter_declaration)           { SimpleParameterDeclarationList (mapSep (fmap Comma_) $1) }

parameter_declaration ::                               { ParameterDeclaration }
  : opt(type_declaration) opt(sy('&')) variable_name opt(default_argument_specifier)  { ParameterDeclaration ($1, fmap (fmap Amp_) $2, $3, $4) }

type_declaration ::                                    { TypeDeclaration }
  : opt(sy('?')) base_type_declaration                 { TypeDeclaration (fmap (fmap Question_) $1, $2) }

base_type_declaration ::                               { BaseTypeDeclaration }
  : kw('array')                                        { BaseTypeDeclaration_Array (fmap Array_ $1) }
  | kw('callable')                                     { BaseTypeDeclaration_Callable (fmap Callable_ $1) }
  | kw('iterable')                                     { BaseTypeDeclaration_Iterable (fmap Iterable_ $1) }
  | scalar_type                                        { BaseTypeDeclaration_Scalar $1 }
  | qualified_name                                     { BaseTypeDeclaration_Name $1 }

scalar_type ::                                         { ScalarType }
  : kw('bool')                                         { ScalarType_Bool   (fmap Bool_   $1) }
  | kw('float')                                        { ScalarType_Float  (fmap Float_  $1) }
  | kw('int')                                          { ScalarType_Int    (fmap Int_    $1) }
  | kw('string')                                       { ScalarType_String (fmap String_ $1) }

default_argument_specifier ::                          { DefaultArgumentSpecifier }
  : sy('=') constant_expr                              { DefaultArgumentSpecifier (fmap Equal_ $1, $2) }

variadic_declaration_list ::                           { VariadicDeclarationList }
  : simple_parameter_declaration_list sy(',') variadic_parameter { VariadicDeclarationList_Simple ($1, fmap Comma_ $2, $3) }
  | variadic_parameter { VariadicDeclarationList_Variadic $1 }

variadic_parameter ::                                  { VariadicParameter }
  : opt(type_declaration) opt(sy('&')) sy('...') variable_name { VariadicParameter ($1, fmap (fmap Amp_) $2, fmap Ellipsis_ $3, $4) }

return_type ::                                         { ReturnType }
  : sy(':') type_declaration                           { ReturnType_Type (fmap Colon_ $1, $2) }
  | sy(':') kw('void')                                 { ReturnType_Void (fmap Colon_ $1, fmap Void_ $2) }



-- Class Declarations --
-----------------------

class_declaration ::                                   { ClassDeclaration }
  : opt(class_modifier) kw('class') name opt(class_base_clause) opt(class_interface_clause) sy('{') opt(class_member_declarations) sy('}')   { ClassDeclaration ($1, fmap Class_ $2, $3, $4, $5, fmap OpenBrace_ $6, $7, fmap ClosedBrace_ $8) }

class_modifier ::                                      { ClassModifier }
  : kw('abstract')                                     { ClassModifier_Abstract (fmap Abstract_ $1) }
  | kw('final')                                        { ClassModifier_Final (fmap Final_ $1) }




-- Interface Declarations --
----------------------------

interface_declaration ::                               { InterfaceDeclaration }
 : kw('interface') name opt(interface_base_clause) sy('{') opt(interface_member_declarations) sy('}')   { InterfaceDeclaration (fmap Interface_ $1, $2, $3, fmap OpenBrace_ $4, $5, fmap ClosedBrace_ $6) }

interface_base_clause ::                               { InterfaceBaseClause }
  : kw('extends') qualified_name                       { InterfaceBaseClause_Extends (fmap Extends_ $1, $2) }
  | interface_base_clause sy(',') qualified_name       { InterfaceBaseClause_List ($1, fmap Comma_ $2, $3) }

interface_member_declarations ::                       { InterfaceMemberDeclarations }
  : seqOf(interface_member_declaration)                { InterfaceMemberDeclarations $1 }

interface_member_declaration ::                        { InterfaceMemberDeclaration }
  : class_const_declaration                            { InterfaceMemberDeclaration_Const $1 }
  | method_declaration                                 { InterfaceMemberDeclaration_Method $1 }

method_declaration ::                                  { MethodDeclaration }
  : opt(method_modifiers) function_definition          { MethodDeclaration_Function ($1, $2) }
  | method_modifiers function_definition_header sy(';') { MethodDeclaration_Header ($1, $2, fmap Semicolon_ $3) }

method_modifiers ::                                    { MethodModifiers }
  : seqOf(method_modifier)                             { MethodModifiers $1 }

method_modifier ::                                     { MethodModifier }
  : visibility_modifier                                { MethodModifier_Visibility $1 }
  | static_modifier                                    { MethodModifier_Static     $1 }
  | class_modifier                                     { MethodModifier_Class      $1 }

visibility_modifier ::                                 { VisibilityModifier }
  : kw('public')                                       { VisibilityModifier_Public    (fmap Public_    $1) }
  | kw('private')                                      { VisibilityModifier_Private   (fmap Private_   $1) }
  | kw('protected')                                    { VisibilityModifier_Protected (fmap Protected_ $1) }

static_modifier ::                                     { StaticModifier }
  : kw('static')                                       { StaticModifier (fmap Static_ $1) }



-- Trait Declarations --
------------------------

trait_declaration ::                                   { TraitDeclaration }
  : kw('trait') name sy('{') opt(trait_member_declarations) sy('}')  { TraitDeclaration (fmap Trait_ $1, $2, fmap OpenBrace_ $3, $4, fmap ClosedBrace_ $5) }

trait_member_declarations ::                           { TraitMemberDeclarations }
  : seqOf(trait_member_declaration)                    { TraitMemberDeclarations $1 }

trait_member_declaration ::                            { TraitMemberDeclaration }
  : property_declaration                               { TraitMemberDeclaration_Property $1 }
  | method_declaration                                 { TraitMemberDeclaration_Method $1 }
  | constructor_declaration                            { TraitMemberDeclaration_Construct $1 }
  | destructor_declaration                             { TraitMemberDeclaration_Destruct $1 }
  | trait_use_clauses                                  { TraitMemberDeclaration_Trait $1 }

property_declaration ::                                { PropertyDeclaration }
  : property_modifier property_elements sy(';')        { PropertyDeclaration ($1, $2, fmap Semicolon_ $3) }

property_modifier ::                                   { PropertyModifier }
  : kw('var')                                          { PropertyModifier_Var (fmap Var_ $1) }
  | visibility_modifier opt(static_modifier)           { PropertyModifier_Visibility ($1, $2) }
  | static_modifier opt(visibility_modifier)           { PropertyModifier_Static ($1, $2) }

property_elements ::                                   { PropertyElements }
  : seqOf(property_element)                            { PropertyElements $1 }

property_element ::                                    { PropertyElement }
  : variable_name opt(property_initializer) sy(';')    { PropertyElement ($1, $2, fmap Semicolon_ $3) }

property_initializer ::                                { PropertyInitializer }
  : sy('=') constant_expr                              { PropertyInitializer (fmap Equal_ $1, $2) }

constructor_declaration ::                             { ConstructorDeclaration }
  : method_modifiers kw('function') opt(sy('&')) kw('__construct') sy('(') opt(parameter_declaration_list) sy(')') compound_statement { ConstructorDeclaration ($1, fmap Function_ $2, fmap (fmap Amp_) $3, fmap Construct_ $4, fmap OpenParen_ $5, $6, fmap ClosedParen_ $7, $8) }

destructor_declaration ::                              { DestructorDeclaration }
  : method_modifiers kw('function') opt(sy('&')) kw('__destruct') sy('(') sy(')') compound_statement { DestructorDeclaration ($1, fmap Function_ $2, fmap (fmap Amp_) $3, fmap Destruct_ $4, fmap OpenParen_ $5, fmap ClosedParen_ $6, $7) }

trait_use_clauses ::                                   { TraitUseClauses }
  : seqOf(trait_use_clause)                            { TraitUseClauses $1 }

trait_use_clause ::                                    { TraitUseClause }
  : kw('use') trait_name_list trait_use_specification  { TraitUseClause (fmap Use_ $1, $2, $3) }

trait_use_specification ::                             { TraitUseSpecification }
  : sy(';')                                            { TraitUseSpecification_Empty (fmap Semicolon_ $1) }
  | sy('{') opt(trait_select_and_alias_clauses) sy('}') { TraitUseSpecification_Alias (fmap OpenBrace_ $1, $2, fmap ClosedBrace_ $3) }

trait_select_and_alias_clauses ::                      { TraitSelectAndAliasClauses }
  : seqOf(trait_select_and_alias_clause)               { TraitSelectAndAliasClauses $1 }

trait_select_and_alias_clause ::                       { TraitSelectAndAliasClause }
  : trait_select_insteadof_clause sy(';')              { TraitSelectAndAliasClause_Select ($1, fmap Semicolon_ $2) }
  | trait_alias_as_clause sy(';')                      { TraitSelectAndAliasClause_Alias ($1, fmap Semicolon_ $2) }

trait_select_insteadof_clause ::                       { TraitSelectInsteadofClause }
  : qualified_name sy('::') name kw('insteadof') trait_name_list { TraitSelectInsteadofClause ($1, fmap DoubleColon_ $2, $3, fmap Insteadof_ $4, $5) }

trait_alias_as_clause ::                               { TraitAliasAsClause }
  : name kw('as') opt(visibility_modifier) name        { TraitAliasAsClause_Visibility ($1, fmap As_ $2, $3, $4) }
  | name kw('as') visibility_modifier opt(name)        { TraitAliasAsClause_Name ($1, fmap As_ $2, $3, $4) }

trait_name_list ::                                     { TraitNameList }
  : seqOfSep(sy(','), qualified_name)                  { TraitNameList (mapSep (fmap Comma_) $1) }



-- Global Declarations --
-------------------------

global_declaration ::                                  { GlobalDeclaration }
  : kw('global') variable_name_list sy(';')            { GlobalDeclaration (fmap Global_ $1, $2, fmap Semicolon_ $3) }

variable_name_list ::                                  { VariableNameList }
  : seqOfSep(sy(','), simple_variable)                 { VariableNameList (mapSep (fmap Comma_) $1) }



-- Namespace Use Declaration --
-------------------------------

namespace_use_declaration ::                           { NamespaceUseDeclaration }
  : kw('use') opt(namespace_function_or_const) namespace_use_clauses sy(';')   { NamespaceUseDeclaration_Use (fmap Use_ $1, $2, $3, fmap Semicolon_ $4) }
  | kw('use') namespace_function_or_const opt(sy('\\')) namespace_name sy('\\') sy('{') namespace_use_group_clauses_1 sy('}') sy(';')   { NamespaceUseDeclaration_Function (fmap Use_ $1, $2, fmap (fmap Backslash_) $3, $4, fmap Backslash_ $5, fmap OpenBrace_ $6, $7, fmap ClosedBrace_ $8, fmap Semicolon_ $9) }
  | kw('use') opt(sy('\\')) namespace_name sy('\\') sy('{') namespace_use_group_clauses_2 sy('}') sy(';')   { NamespaceUseDeclaration_Name (fmap Use_ $1, fmap (fmap Backslash_) $2, $3, fmap Backslash_ $4, fmap OpenBrace_ $5, $6, fmap ClosedBrace_ $7, fmap Semicolon_ $8) }

namespace_function_or_const ::                         { NamespaceFunctionOrConst }
  : kw('function')                                     { NamespaceFunctionOrConst_Function (fmap Function_ $1) }
  | kw('const')                                        { NamespaceFunctionOrConst_Const (fmap Const_ $1) }

namespace_name ::                                      { NamespaceName }
  : seqOfSep(sy('/'), name)                            { NamespaceName (mapSep (fmap Slash_) $1) }

namespace_use_clauses ::                               { NamespaceUseClauses }
  : seqOfSep(sy(','), namespace_use_clause)            { NamespaceUseClauses (mapSep (fmap Comma_) $1) }

namespace_use_clause ::                                { NamespaceUseClause }
  : qualified_name opt(namespace_aliasing_clause)      { NamespaceUseClause ($1, $2) }

namespace_aliasing_clause ::                           { NamespaceAliasingClause }
  : kw('as') name                                      { NamespaceAliasingClause (fmap As_ $1, $2) }

namespace_use_group_clauses_1 ::                       { NamespaceUseGroupClauses1 }
  : seqOfSep(sy(','), namespace_use_group_clause_1)    { NamespaceUseGroupClauses1 (mapSep (fmap Comma_) $1) }

namespace_use_group_clause_1 ::                        { NamespaceUseGroupClause1 }
  : namespace_name opt(namespace_aliasing_clause)      { NamespaceUseGroupClause1 ($1, $2) }

namespace_use_group_clauses_2 ::                       { NamespaceUseGroupClauses2 }
  : seqOfSep(sy(','), namespace_use_group_clause_2)    { NamespaceUseGroupClauses2 (mapSep (fmap Comma_) $1) }

namespace_use_group_clause_2 ::                        { NamespaceUseGroupClause2 }
  : opt(namespace_function_or_const) namespace_name opt(namespace_aliasing_clause) { NamespaceUseGroupClause2 ($1, $2, $3) }



-- Function Static Declaration --
---------------------------------

function_static_declaration ::                         { FunctionStaticDeclaration }
  : kw('static') static_variable_name_list sy(';')     { FunctionStaticDeclaration (fmap Static_ $1, $2, fmap Semicolon_ $3) }

static_variable_name_list ::                           { StaticVariableNameList }
  : seqOfSep(sy(','), static_variable_declaration)     { StaticVariableNameList (mapSep (fmap Comma_) $1) }

static_variable_declaration ::                         { StaticVariableDeclaration }
  : variable_name opt(function_static_initializer)     { StaticVariableDeclaration ($1, $2) }

function_static_initializer ::                         { FunctionStaticInitializer }
  : sy('=') constant_expr                              { FunctionStaticInitializer (fmap Equal_ $1, $2) }









{

_reduce :: String -> a -> Parser a
_reduce msg a = do
  Parser $ fmap Right $ trace $ T_Reduce msg
  return a


testParser :: LexMode -> Parser a -> String -> Either Error a
testParser mode p s =
  case runAlex s (pushMode mode >> unParser p) of
    Left msg -> Left $ LexError msg
    Right zs -> case zs of
      Left err -> Left $ ParseError err
      Right ok -> Right ok

debugParser :: (Show a, Render a) => LexMode -> Parser a -> String -> IO ()
debugParser mode p input = do
  putStrLn "\n===> Input:"
  putStrLn $ show input
  case debugAlex input (pushMode mode >> unParser p) of
    Left err -> do
      putStrLn "\n===> Lexical Error:"
      putStrLn err
    Right (st,zs) -> do
      case zs of
        Left tok -> do
          putStrLn "\n===> Parse Error at token:"
          putStrLn $ prettyDebug tok
        Right ok -> do
          putStrLn "\n===> Parse Result:"
          putStrLn "-----> Roundtrip:"
          putStrLn $ show (input == (T.unpack $ render ok)) ++ "\n"
          putStrLn "-----> Rendered:"
          putStrLn $ (show $ render ok) ++ "\n"
          putStrLn "-----> Structured:"
          putStrLn $ show ok
      putStrLn "\n===> Debug:"
      putStrLn $ prettyDebug st

data Error
  = LexError String
  | ParseError Token
  deriving Eq

instance Show Error where
  show x = "\n\n" ++ case x of
    LexError err -> "Lexical Error:\n" ++ err
    ParseError tok -> "Parse Error:\n" ++ show tok

_test :: (Show a) => LexMode -> Parser a -> String -> IO ()
_test mode p s = do
  let result = testParser mode p s
  case result of
    Right x  -> putStrLn $ show x
    Left err -> putStrLn $ show err

_test' :: (Render a) => LexMode -> Parser a -> String -> IO ()
_test' mode p s = do
  let result = testParser mode p s
  case result of
    Right x  -> T.putStrLn $ render x
    Left err -> putStrLn $ show err

parseError :: Token -> Parser a
parseError = Parser . return . Left

}
