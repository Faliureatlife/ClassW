-- parser.hs
-- Exports a parsing function that takes a read file
--  as input and parse it into the full syntax, producing either a syntax
--  tree or a list of error-message lines as output
-- Imports: full.hs

module Parser where

import Full
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Data.Char (isSpace)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace
import System.IO
import Text.Megaparsec.Debug

type Parser = Parsec Void String

-- Using state so we can store a string that determines which paradigm
-- we are using for parsing based on user input.
type ParserT a = ParsecT Void String (State String) a




-- statements choice to determine which statement parsers are being included
stmtChoice :: String -> [ParserT FStmt]
stmtChoice c = case c of
  "functional" -> funcStmts
  "declarative" -> declStmts
  "declarative threaded" -> declThreadStmts
  "stateful" -> stateStmts
  "stateful threaded" -> stateThreadStmts
  where
    funcStmts = [pSkip, pBind, pLocal, pIf, pCase, pFunDef]
    declStmts = funcStmts ++ [pProcApp, pProcDef]
    declThreadStmts = declStmts ++ [pThread, pByNeed]
    stateStmts = declStmts ++ [pExchange, pNewCell, pSetCell]
    stateThreadStmts = stateStmts ++ [pThread, pByNeed]

-- parse a statement
pStatement :: ParserT [FStmt]
pStatement = some (notFollowedBy pTerminator >> pSingleStatement)

-- This is the top-level parser for the whole file.
pStatementF :: ParserT [FStmt]
pStatementF = between sc eof pStatement


pSkip :: ParserT FStmt
pSkip = do
  rwordSC "skip"
  -- Fail after this point
  choice [pSkipB, pSkipS, pSkipF, pSkipC, pSkipSt, pSkipBr]
  where
    pSkipB = do (FSkip FBasic) <$ string "Basic" <* sc
    pSkipS = do (FSkip FStore) <$ string "Store" <* sc
    pSkipF = do (FSkip FFull) <$ string "Full" <* sc
    pSkipC = do (FSkip FCheck) <$ string "Check" <* sc
    pSkipSt = do (FSkip FStack) <$ string "Stack" <* sc
    pSkipBr = do
      string "Browse" <* sc
      FSkip <$> FBrowse <$> pIdentifierSC

withErrorContext :: String -> ParserT a -> ParserT a
withErrorContext name p = do
  pos <- getSourcePos
  let ctx = "the '" ++ name ++ "' statement starting at " ++ sourcePosPretty pos
  p <?> ("'end' to close " ++ ctx)

pLocal :: ParserT FStmt
pLocal = withErrorContext "local" $ do
  rwordSC "local"
  -- fail after this point
  inStmt <- pInStatement
  rwordSC "end"
  return $ FLocal inStmt -- the variable introduction is stored in inStmt

pBind :: ParserT FStmt
pBind = do
  id1 <- try $ do pPattern <* charSC '=' -- make atomic using do/try
  -- fail after this point
  ex <- pExp
  return $ FEq id1 (Ex ex)

pIf :: ParserT FStmt
pIf = withErrorContext "if" $ do
  rwordSC "if"
  -- Fail after this point
  exp1 <- pExp
  rwordSC "then"
  inStm <- pInStatement
  nestedElseIf <- many pElseIF
  optElseCheck <- optional (rwordSC "else") -- optional else statement
  optElse <- case optElseCheck of
    (Just s) -> do Just <$> pInStatement
    Nothing  -> do Nothing <$ sc
  rwordSC "end"
  return $ FIf exp1 inStm nestedElseIf optElse
  where
    pElseIF = do
      rwordSC "elseif"
      exp1 <- pExp
      rwordSC "then"
      inStm <- pInStatement
      return (exp1, inStm)

pCase :: ParserT FStmt
pCase = withErrorContext "case" $ do
  rwordSC "case"
  -- Fail after this point
  ex <- pExp
  rwordSC "of"
  patt <- pPattern -- Cannot be Variable! This is checked in elab
  rwordSC "then"
  inStmt <- pInStatement
  nestedCase <- many pNestCase
  optElseCheck <- optional (rwordSC "else")
  optElse <- case optElseCheck of
    Just s -> do Just <$> pInStatement
    Nothing  -> do Nothing <$ sc
  rwordSC "end"
  return $ FCase ex patt inStmt nestedCase optElse
  where
    pNestCase = do
      string "[]" <* sc
      patt <- pPattern
      rwordSC "then"
      inStmt <- pInStatement
      return (patt, inStmt)

pProcApp :: ParserT FStmt
pProcApp = do
  charSC '{'
  -- Fail after this point
  procID <- pIdentifierSC
  params <- many (try (notFollowedBy (char '}') >> pExp))
  charSC '}'
  return $ FApply procID params

-- read a proc definition and convert it to the FEq type
pProcDef :: ParserT FStmt
pProcDef = withErrorContext "proc" $ do
  rwordSC "proc"
  -- Fail after this point
  charSC '{'
  procID <- pIdentifierSC
  params <- many pIdentifierSC
  charSC '}'
  body <- pInStatement
  rwordSC "end"
  return $ FEq (PtVar procID) $ Pr params body

-- read a func definition and convert it to the FEq type
pFunDef :: ParserT FStmt
pFunDef = withErrorContext "fun" $ do
  rwordSC "fun"
  -- Fail after this point
  charSC '{'
  funID <- pIdentifierSC
  params <- many pIdentifierSC
  charSC '}'
  body <- pInExpression
  rwordSC "end"
  return $ FEq (PtVar funID) $ Fn params body

pThread :: ParserT FStmt
pThread = withErrorContext "thread" $ do
  rwordSC "thread"
  -- Fail after this point
  inStmt <- pInStatement
  rwordSC "end"
  return $ FThread inStmt

pByNeed :: ParserT FStmt
pByNeed = do
  rwordSC "byNeed"
  -- Fail after this point
  ex <- pExp
  id1 <- pIdentifierSC
  return $ FByNeed ex id1

pNewCell :: ParserT FStmt
pNewCell = do
  rwordSC "newCell"
  -- Fail after this point
  ex  <- pExp
  id1 <- pIdentifierSC
  return $ FNewCell ex id1

pExchange :: ParserT FStmt
pExchange = do
  rwordSC "exchange"
  -- Fail after this point
  id1 <- pIdentifierSC
  id2 <- pIdentifierSC
  ex  <- pExp
  return $ FExchange id1 id2 ex

pSetCell :: ParserT FStmt
pSetCell = do
  id1 <- try $ pIdentifierSC <* stringSC ":=" -- make atomic using try
  ex <- pExp
  return $ FSetCell id1 ex

-- TRY CATCH AND RAISE STATEMNETS


-- InStatements
data DecChoice = Bnd (Patt, Exp) | Vr FVar

-- Helper for splitting declaration choices
splitDecChoice :: [DecChoice] -> ([FVar], [(Patt, Exp)])
splitDecChoice [] = ([],[])
splitDecChoice ((Bnd b):l) = let (as,bs) = splitDecChoice l in (as,b:bs)
splitDecChoice ((Vr v):l) = let (as,bs) = splitDecChoice l in (v:as,bs)

-- The parser for a single declaration, now at the top level
pDecl :: ParserT DecChoice
pDecl = try pNamedProcDecl
    <|> try pNamedFunDecl
    <|> (Bnd <$> try ((,) <$> pPattern <* charSC '=' <*> pExp))
    <|> (Vr <$> pIdentifierSC)
  where
    pNamedProcDecl = withErrorContext "proc" $ do
      rwordSC "proc"
      charSC '{'
      procID <- pIdentifierSC
      params <- many pIdentifierSC
      charSC '}'
      body <- pInStatement
      rwordSC "end"
      return $ Bnd (PtVar procID, EProc params body)

    pNamedFunDecl = withErrorContext "fun" $ do
      rwordSC "fun"
      charSC '{'
      funID <- pIdentifierSC
      params <- many pIdentifierSC
      charSC '}'
      body <- pInExpression
      rwordSC "end"
      return $ Bnd (PtVar funID, EFun params body)

-- The pDecls function is now much simpler
pDecls :: ParserT ([FVar], [(Patt, Exp)])
pDecls = do
  choices <- some pDecl
  return $ splitDecChoice choices

-- Your pInStatement, with the fix, will now compile successfully
pInStatement :: ParserT InStmt
pInStatement = pExStatements <|> ((Nothing,) <$> pStatement)
  where
    pExStatements = try $ do
      decls <- manyTill pDecl (lookAhead (rwordSC "in"))
      let (vars, binds) = splitDecChoice decls
      rwordSC "in"
      stmts <- pStatement
      return (Just (vars, binds), stmts)

-- Expressions

pBody :: ParserT ([FStmt], Exp)
pBody =
      try (do
        stmt <- head <$> pStatementA
        (rest_stmts, ex) <- pBody
        return (stmt : rest_stmts, ex)
      )
      <|>
      (do
        ex <- pExp
        return ([], ex)
      )

pInExpression :: ParserT InExp
pInExpression = pExExpression <|> pSimpleExpression
  where
    pExExpression = try $ do
      (vars, binds) <- pDecls
      rwordSC "in"
      (stmts, ex) <- pBody
      return (Just (vars, binds), stmts, ex)
    pSimpleExpression = do
      ex <- pExp
      return (Nothing, [], ex)

pStatementA :: ParserT [FStmt]
pStatementA = do
  c <- get
  let stmts = stmtChoice c
  s <- choice stmts <?> "Statement"
  return [s]


rbos :: [Char]
rbos = ['+','-','*','/','<','>']

opPairs :: [(String,String)]
opPairs = [("+","IntPlus"),("-","IntMinus"),("*","IntMultiply"),("/","Divide"),("<","LT"),(">","GT"),
  ("div","DivideInt"),("mod","Mod"),("==","Eq"),("\\=","NotEqual"),("=<","LessThanEq"),("=>","GreaterThanEq")]

rBinOp :: ParserT String
rBinOp = (:[]) <$> satisfy (\x-> elem x rbos)
  <|> string "div"
  <|> string "mod"
  <|> string "=="
  <|> string "=<"
  <|> string "\\="
  <|> string ">="

-- remove Maybe constructors
optStmt :: [Maybe FStmt] -> [FStmt]
optStmt ((Just s):ss) = s:(optStmt ss)
optStmt (Nothing:ss) = []

pExp :: ParserT Exp
pExp = do
  c <- get
  let exprs = exprChoice c
  choice exprs
  where
    -- Select parsers to use for the expression based on user input string
    funcExps = [pENum, pEVar, pParen, pExpRcd, pExpAtom, pExpList, pLocal, pIf, pCase, pFun, pFunApp]
    declExps = funcExps ++ [pProc]
    declThreadExps = declExps ++ [pThread]
    stateExps = declExps ++ [pNewCell, pAtCell]
    stateThreadExps = stateExps ++ [pThread]
    exprChoice :: String -> [ParserT Exp]
    exprChoice c = case c of
      "functional" -> funcExps
      "declarative" -> declExps
      "declarative threaded" -> declThreadExps
      "stateful" -> stateExps
      "stateful threaded" -> stateThreadExps
    pENum = do ENum <$> pNum <* sc
    pEVar = do EVar <$> pIdentifierSC
    pExpList = do
      charSC '['
      elems <- many pExp
      charSC ']'
      return $ EList elems
    pParen = do
      charSC '('
      e1 <- pExp
      op <- (rBinOp <|> (:[]) <$> rcBinOp) <* sc -- operation or # or |
      e2 <- pExp
      charSC ')'
      return $ EParen e1 e2 op
    pExpRcd = do
      label <- try $ do pAtom <* charSC '('
      es <- many $ (,) <$> pFeatureSC <* charSC ':' <*> pExp
      charSC ')'
      return $ ERcd label es
    pExpAtom = do
      label <- pAtom <* sc
      return $ ERcd label []
    pFun = withErrorContext "fun" $ do
      rwordSC "fun"
      charSC '{'
      charSC '$'
      params <- many pIdentifierSC
      charSC '}'
      body <- pInExpression
      rwordSC "end"
      return $ EFun params body
    pProc = withErrorContext "proc" $ do
      rwordSC "proc"
      charSC '{'
      charSC '$'
      params <- many pIdentifierSC
      charSC '}'
      body <- pInStatement
      rwordSC "end"
      return $ EProc params body
    pLocal = withErrorContext "local" $ do
      rwordSC "local"
      -- fail after this point
      inExp <- pInExpression
      rwordSC "end"
      return $ ELocal inExp
    pIf = withErrorContext "if" $ do
      rwordSC "if"
      -- Fail after this point
      exp1 <- pExp
      rwordSC "then"
      inExp <- pInExpression
      nestedElseIf <- many pElseIF
      optElseCheck <- optional (rwordSC "else") -- optional else statement
      optElse <- case optElseCheck of
        (Just s) -> do Just <$> pInExpression
        Nothing  -> do Nothing <$ sc
      rwordSC "end"
      return $ EIf exp1 inExp nestedElseIf optElse
      where
        pElseIF = do
          rwordSC "elseif"
          exp1 <- pExp
          rwordSC "then"
          inExp <- pInExpression
          return (exp1, inExp)
    pCase = withErrorContext "case" $ do
      rwordSC "case"
      -- Fail after this point
      ex <- pExp
      rwordSC "of"
      patt <- pPattern
      rwordSC "then"
      inExp <- pInExpression
      nestedCase <- many pNestCase
      optElseCheck <- optional (rwordSC "else")
      optElse <- case optElseCheck of
        (Just s) -> do Just <$> pInExpression
        Nothing  -> do Nothing <$ sc
      rwordSC "end"
      return $ ECase ex patt inExp nestedCase optElse
      where
        pNestCase = do
          string "[]" <* sc
          patt <- pPattern -- Update to be more versatile
          rwordSC "then"
          inExp <- pInExpression
          return (patt, inExp)
    pThread = withErrorContext "thread" $ do
      rwordSC "thread"
      -- Fail after this point
      inExp <- pInExpression
      rwordSC "end"
      return $ EThread inExp
    pNewCell = do
      rwordSC "newCell"
      -- Fail after this point
      ex  <- pExp
      return $ ENewCell ex
    pFunApp = do
      charSC '{'
      funID <- pIdentifierSC
      params <- many (try (notFollowedBy (char '}') >> pExp))
      charSC '}'
      return $ EFunApp funID params
    pAtCell = do
      char '@'
      id1 <- pIdentifierSC
      return $ EAtCell id1



-- Patterns

rcbos :: [Char]
rcbos = ['#','|']

rcBinOp :: ParserT Char
rcBinOp = satisfy (\x-> elem x rcbos)

pPattern:: ParserT Patt
pPattern =
  (( PtVar <$> pIdentifierSC) -- Var included for inDeclaration, not used for case
    <|> pPattList
    <|> pPattRcd
    <|> pParen
    <|> pPAtom) <?> "Pattern"
  where
    pPattList = do
      charSC '['
      elems <- many pPattern
      charSC ']'
      return $ PtList elems
    pParen = do
      charSC '('
      pt1 <- pPattern
      op <- rcBinOp <* sc
      pt2 <- pPattern
      charSC ')'
      return $ PtParen pt1 pt2 op
    pPattRcd = do
      label <- try $ do pAtom <* charSC '('
      ps <- many $ (,) <$> pFeatureSC <* charSC ':' <*> pPattern
      charSC ')'
      return $ PtRec label ps
    pPAtom = do
      label <- pAtom <* sc
      return $ PtRec label []


-- Basic helper functions for parsing: white space consumer and values

-- MarkKaprov tutorial (parsing a simple imperative language)
-- space consumer
sc :: ParserT ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- wrap lexeme so it parses white space after it
lexeme :: ParserT a -> ParserT a
lexeme = L.lexeme sc

-- parse string and whitespace after
symbol :: String -> ParserT String
symbol = L.symbol sc

-- char wrapped with space consumer
charSC :: Char -> ParserT Char
charSC c = char c <* sc

stringSC :: String -> ParserT String
stringSC s = string s <* sc

-- reserved word parsers followed by at least one space
rword :: String -> ParserT String
rword w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

rwordSC :: String -> ParserT String
rwordSC w = rword w <* sc

-- reserved words
rws :: [String]
rws = ["skip","local","in","end","case","of","if","else","then","proc","Browse","elseif","Garb","thread","byNeed","newCell", "fun"]

-- parsing an identifier
-- starts with upper case letter, cannot be a reserved word
-- no underscore, no quote names
pIdentifier :: ParserT String
pIdentifier = try (p >>= check)
  where
    p = (:) <$> upperChar <*> many alphaNumChar
    check x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x

pIdentifierSC :: ParserT String
pIdentifierSC = pIdentifier <* sc <?> "Identifier" -- expected identifier

-- parsing an atom
-- starts with lower case letter, cannot be reserved word
-- OR
-- starts with single quote, ends with single quote
pAtom :: ParserT String
pAtom = try ( (p >>= check) <|> pSingleQuotes) <?> "Atom"
  where
    p = (:) <$> lowerChar <*> many alphaNumChar
    check x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an atom"
          else return x
    pSingleQuotes = do
      x <- singleQuotes
      return ("'"++x++"'")
    singleQuotes = between (char '\'') (char '\'') (many (satisfy (not . (== '\''))))

-- A parser that looks for keywords that end a statement block without consuming them.
pTerminator :: ParserT ()
pTerminator = lookAhead (void (rword "end") <|>
                         void (rword "else") <|>
                         void (rword "elseif") <|>
                         void (string "[]")) -- for case statements

-- A helper to parse exactly one statement.
pSingleStatement :: ParserT FStmt
pSingleStatement = do
  c <- get
  choice (stmtChoice c) <?> "Statement"


-- https://mmhaskell.com/parsing-4 for number example

pNum :: ParserT FPNum
pNum = (FPFloat <$> pFloat
  <|> FPInt <$> pInteger
  <|> pNegative)  <?> "Number"
  where
    pFloat :: ParserT Float
    pFloat = try $ do
      whole <- many digitChar <* (char '.')
      fractional <- many digitChar
      let num = read $ whole ++ ('.':fractional)
      return num
    pInteger :: ParserT Int
    pInteger = try $ do
      int <- some digitChar
      let num = read int
      return num
    pNegative :: ParserT FPNum
    pNegative = try $ do
      void (char '-')
      num <- many digitChar
      dec <- (optional . try) (char '.')
      case dec of
        Just _ -> do
          fractional <- many digitChar
          let fnum = read $ num ++ ('.':fractional)
          return $ FPFloat (-1 * fnum)
        Nothing -> return $ FPInt (-1* (read num))


pLiteral :: ParserT String
pLiteral = pAtom <?> "Literal"

pFeatureSC :: ParserT String
pFeatureSC = do (pFeature <* sc) <?> "Feature"

pFeature :: ParserT String
pFeature =
  try pAtom <|>
  do
    n1 <- digitChar
    num <- many digitChar
    return (n1:num)
