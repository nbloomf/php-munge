module Main where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           System.Directory
import           System.IO
import           System.Environment
import           Data.List

import PHP.Parse

main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "10000"
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "100"
  setEnv "TASTY_NUM_THREADS" "4"
  putStrLn "Testing php-parse"
  defaultMain $ testGroup "All Tests"
    [ test_RecognitionTests
    , test_RoundTripTests
    ]



test_RecognitionTests :: TestTree
test_RecognitionTests = testGroup "Recognition Tests"
  [ testGroup "Text Mode"
    [ test_recognizer "pText"               0   pText
    , test_recognizer "pStartTag"           0   pStartTag
    ]
  , testGroup "Single Line Comment Mode"
    [ test_recognizer "pSingleLineComment"  cmd pSingleLineComment
    ]
  , testGroup "Multi Line Comment Mode"
    [ test_recognizer "pMultiLineComment"   cmd pMultiLineComment
    ]
  , testGroup "Single Quoted String Mode"
    [ test_recognizer "pSingleQuotedString" cmd pSingleQuotedString
    ]
  , testGroup "Command Mode"
    [ test_recognizer "pNewLine"            cmd pNewLine
    , test_recognizer "pWhiteSpace"         cmd pWhiteSpace
    , test_recognizer "pWhiteSpaces"        cmd pWhiteSpaces
    , test_recognizer "pEndTag"             cmd pEndTag
    ]
  ]



test_RoundTripTests :: TestTree
test_RoundTripTests = testGroup "Round Trip Tests"
  [ testGroup "Scripts"
    [ test_roundtrip "pScript"                              0   pScript
    , test_roundtrip "pScriptSection"                       0   pScriptSection
    , test_roundtrip "pText"                                0   pText
    , test_roundtrip "pStartTag"                            0   pStartTag
    , test_roundtrip "pEndTag"                              cmd pEndTag
    , test_roundtrip "pWhiteSpaces"                         cmd pWhiteSpaces
    , test_roundtrip "pWhiteSpace"                          cmd pWhiteSpace
    , test_roundtrip "pNewLine"                             cmd pNewLine
    ]
  , testGroup "Comments"
    [ test_roundtrip "pComment"                             cmd pComment
    , test_roundtrip "pSingleLineComment"                   cmd pSingleLineComment
    , test_roundtrip "pMultiLineComment"                    cmd pMultiLineComment
    ]
  , testGroup "Strings"
    [ test_roundtrip "pSingleQuotedString"                  cmd pSingleQuotedString
    , test_roundtrip "pDoubleQuotedString"                  cmd pDoubleQuotedString
    , test_roundtrip "pHeredocString"                       cmd pHeredocString
    , test_roundtrip "pNowdocString"                        cmd pNowdocString
    ]
  , testGroup "Ints"
    [ test_roundtrip "pIntegerLiteral"                      cmd pIntegerLiteral
    , test_roundtrip "pDecimalLiteral"                      cmd pDecimalLiteral
    , test_roundtrip "pHexLiteral"                          cmd pHexLiteral
    , test_roundtrip "pBinaryLiteral"                       cmd pBinaryLiteral
    , test_roundtrip "pOctalLiteral"                        cmd pOctalLiteral
    ]
  , testGroup "Floats"
    [ test_roundtrip "pFloatingLiteral"                     cmd pFloatingLiteral
    , test_roundtrip "pExponentPart"                        pow pExponentPart
    ]
  , testGroup "Variables"
    [ test_roundtrip "pVariable"                            cmd pVariable
    , test_roundtrip "pCallableVariable"                    cmd pCallableVariable
    , test_roundtrip "pSimpleVariable"                      cmd pSimpleVariable
    , test_roundtrip "pVariableName"                        cmd pVariableName
    , test_roundtrip "pName"                                cmd pName
    , test_roundtrip "pSubscriptExpression"                 cmd pSubscriptExpression
    , test_roundtrip "pMemberCallExpression"                cmd pMemberCallExpression
    , test_roundtrip "pScopedCallExpression"                cmd pScopedCallExpression
    , test_roundtrip "pScopeResolutionQualifier"            cmd pScopeResolutionQualifier
    , test_roundtrip "pQualifiedName"                       cmd pQualifiedName
    , test_roundtrip "pNamespaceNameAsAPrefix"              cmd pNamespaceNameAsAPrefix
    , test_roundtrip "pFunctionCallExpression"              cmd pFunctionCallExpression
    , test_roundtrip "pCallableExpression"                  cmd pCallableExpression
    , test_roundtrip "pScopedPropertyAccessExpression"      cmd pScopedPropertyAccessExpression
    , test_roundtrip "pMemberAccessExpression"              cmd pMemberAccessExpression
    , test_roundtrip "pDereferencableExpression"            cmd pDereferencableExpression
    ]
  , testGroup "Expressions"
    [ test_roundtrip "pExpression"                          cmd pExpression
    , test_roundtrip "pIncludeExpression"                   cmd pIncludeExpression
    , test_roundtrip "pIncludeOnceExpression"               cmd pIncludeOnceExpression
    , test_roundtrip "pRequireExpression"                   cmd pRequireExpression
    , test_roundtrip "pRequireOnceExpression"               cmd pRequireOnceExpression
    , test_roundtrip "pLogicalIncOrExpression2"             cmd pLogicalIncOrExpression2
    , test_roundtrip "pLogicalExcOrExpression"              cmd pLogicalExcOrExpression
    , test_roundtrip "pLogicalAndExpression2"               cmd pLogicalAndExpression2
    , test_roundtrip "pPrintExpression"                     cmd pPrintExpression
    , test_roundtrip "pYieldExpression"                     cmd pYieldExpression
    , test_roundtrip "pAssignmentExpression"                cmd pAssignmentExpression
    , test_roundtrip "pSimpleAssignmentExpression"          cmd pSimpleAssignmentExpression
    , test_roundtrip "pCompoundAssignmentExpression"        cmd pCompoundAssignmentExpression
    , test_roundtrip "pConditionalExpression"               cmd pConditionalExpression
    , test_roundtrip "pCoalesceExpression"                  cmd pCoalesceExpression
    , test_roundtrip "pLogicalIncOrExpression1"             cmd pLogicalIncOrExpression1
    , test_roundtrip "pLogicalAndExpression1"               cmd pLogicalAndExpression1
    , test_roundtrip "pBitwiseIncOrExpression"              cmd pBitwiseIncOrExpression
    , test_roundtrip "pBitwiseExcOrExpression"              cmd pBitwiseExcOrExpression
    , test_roundtrip "pBitwiseAndExpression"                cmd pBitwiseAndExpression
    , test_roundtrip "pEqualityExpression"                  cmd pEqualityExpression
    , test_roundtrip "pRelationalExpression"                cmd pRelationalExpression
    , test_roundtrip "pShiftExpression"                     cmd pShiftExpression
    , test_roundtrip "pAdditiveExpression"                  cmd pAdditiveExpression
    , test_roundtrip "pMultiplicativeExpression"            cmd pMultiplicativeExpression
    , test_roundtrip "pLogicalNotExpression"                cmd pLogicalNotExpression
    , test_roundtrip "pInstanceofExpression"                cmd pInstanceofExpression
    , test_roundtrip "pUnaryExpression"                     cmd pUnaryExpression
    , test_roundtrip "pErrorControlExpression"              cmd pErrorControlExpression
    , test_roundtrip "pCastExpression"                      cmd pCastExpression
    , test_roundtrip "pCastType"                            cmd pCastType
    , test_roundtrip "pUnaryOpExpression"                   cmd pUnaryOpExpression
    , test_roundtrip "pExponentiationExpression"            cmd pExponentiationExpression
    , test_roundtrip "pCloneExpression"                     cmd pCloneExpression
    , test_roundtrip "pPrimaryExpression"                   cmd pPrimaryExpression
    , test_roundtrip "pLiteral"                             cmd pLiteral
    , test_roundtrip "pPostfixIncrementExpression"          cmd pPostfixIncrementExpression
    , test_roundtrip "pPostfixDecrementExpression"          cmd pPostfixDecrementExpression
    , test_roundtrip "pPrefixIncrementExpression"           cmd pPrefixIncrementExpression
    , test_roundtrip "pPrefixDecrementExpression"           cmd pPrefixDecrementExpression
    , test_roundtrip "pIntrinsic"                           cmd pIntrinsic
    , test_roundtrip "pEmptyIntrinsic"                      cmd pEmptyIntrinsic
    , test_roundtrip "pEvalIntrinsic"                       cmd pEvalIntrinsic
    , test_roundtrip "pIssetIntrinsic"                      cmd pIssetIntrinsic
    , test_roundtrip "pExitIntrinsic"                       cmd pExitIntrinsic
    , test_roundtrip "pClassConstantAccessExpression"       cmd pClassConstantAccessExpression
    , test_roundtrip "pAnonymousFunctionCreationExpression" cmd pAnonymousFunctionCreationExpression
    , test_roundtrip "pArrayCreationExpression"             cmd pArrayCreationExpression
    , test_roundtrip "pObjectCreationExpression"            cmd pObjectCreationExpression
    , test_roundtrip "pShellCommandExpression"              cmd pShellCommandExpression
    , test_roundtrip "pByrefAssignmentExpression"           cmd pByrefAssignmentExpression
    ]

  , testGroup "Statements"
    [ test_roundtrip "pStatementList"                       cmd pStatementList
    , test_roundtrip "pStatement"                           cmd pStatement
    , test_roundtrip "pCompoundStatement"                   cmd pCompoundStatement
    , test_roundtrip "pNamedLabelStatement"                 cmd pNamedLabelStatement
    , test_roundtrip "pJumpStatement"                       cmd pJumpStatement
    , test_roundtrip "pGotoStatement"                       cmd pGotoStatement
    , test_roundtrip "pContinueStatement"                   cmd pContinueStatement
    , test_roundtrip "pBreakStatement"                      cmd pBreakStatement
    , test_roundtrip "pReturnStatement"                     cmd pReturnStatement
    , test_roundtrip "pThrowStatement"                      cmd pThrowStatement
    , test_roundtrip "pSelectionStatement"                  cmd pSelectionStatement
    , test_roundtrip "pIfStatement"                         cmd pIfStatement
    , test_roundtrip "pElseifClauses1"                      cmd pElseifClauses1
    , test_roundtrip "pElseClause1"                         cmd pElseClause1
    , test_roundtrip "pElseifClauses2"                      cmd pElseifClauses2
    , test_roundtrip "pElseClause2"                         cmd pElseClause2
    , test_roundtrip "pSwitchStatement"                     cmd pSwitchStatement
    , test_roundtrip "pTryStatement"                        cmd pTryStatement
    , test_roundtrip "pIterationStatement"                  cmd pIterationStatement
    , test_roundtrip "pWhileStatement"                      cmd pWhileStatement
    , test_roundtrip "pDoStatement"                         cmd pDoStatement
    , test_roundtrip "pForStatement"                        cmd pForStatement
    , test_roundtrip "pForeachStatement"                    cmd pForeachStatement
    , test_roundtrip "pDeclareStatement"                    cmd pDeclareStatement
    , test_roundtrip "pEchoStatement"                       cmd pEchoStatement
    , test_roundtrip "pUnsetStatement"                      cmd pUnsetStatement
    , test_roundtrip "pConstDeclaration"                    cmd pConstDeclaration
    , test_roundtrip "pConstantExpression"                  cmd pConstantExpression
    , test_roundtrip "pFunctionDefinition"                  cmd pFunctionDefinition
    , test_roundtrip "pDefaultArgumentSpecifier"            cmd pDefaultArgumentSpecifier
    , test_roundtrip "pScalarType"                          cmd pScalarType
    , test_roundtrip "pClassDeclaration"                    cmd pClassDeclaration
    , test_roundtrip "pTraitDeclaration"                    cmd pTraitDeclaration
    , test_roundtrip "pConstructorDeclaration"              cmd pConstructorDeclaration
    , test_roundtrip "pDestructorDeclaration"               cmd pDestructorDeclaration
    , test_roundtrip "pGlobalDeclaration"                   cmd pGlobalDeclaration
    , test_roundtrip "pNamespaceUseDeclaration"             cmd pNamespaceUseDeclaration
    , test_roundtrip "pFunctionStaticDeclaration"           cmd pFunctionStaticDeclaration
    , test_roundtrip "pFunctionStaticInitializer"           cmd pFunctionStaticInitializer
    ]
  ]



-- Very simple test asserting that a given parser accepts the
-- string in a file. Case data is under test/case/parse.
-- To add tests for additional parsers, add a line to the
-- Parser Tests tree. To add additional test cases for a given
-- parser, just create new files under test/case/recognize/$PARSERNAME.

test_recognizer
  :: ( Show a )
  => String -> LexMode -> Parser a -> TestTree
test_recognizer name mode p = testCase name $
  test_parser_on_directory ("test/case/recognize/" ++ name ++ "/") mode p

test_parser_on_directory
  :: ( Show a )
  => FilePath -> LexMode -> Parser a -> IO ()
test_parser_on_directory dir mode p = do
  files <- filter (/= ".DS_Store") <$> listDirectory dir
  mapM_ (test_parser_on_file mode p dir) files

test_parser_on_file
  :: ( Show a )
  => LexMode -> Parser a -> FilePath -> FilePath -> IO ()
test_parser_on_file mode p dir file = do
  text <- readFile (dir ++ file)
  let result = testParser mode p text
  if isPrefixOf "test." file
    then case result of
      Right _  -> return ()
      Left err -> assertFailure $ show err
    else case result of
      Left _  -> return ()
      Right _ -> assertFailure "expected parse failure"



-- Test assering that for any concrete syntax object type,
-- parse is a left inverse of render. That is, for any data x,
-- rendering x yields a string which successfully parses back to x.

test_roundtrip
  :: ( Show a, Render a, Arbitrary a, HasExtent a, Eq a )
  => String -> LexMode -> Parser a -> TestTree
test_roundtrip name mode p = testProperty ("parse . render === id: " ++ name) $
  test_roundtrip_on mode p

test_roundtrip_on
  :: ( Render a, HasExtent a, Eq a )
  => LexMode -> Parser a -> a -> Bool
test_roundtrip_on mode p x =
  let result = testParser mode p (T.unpack $ render x) in
  case result of
    Left _  -> False
    Right y -> y == (snd $ shiftTo origin x)
