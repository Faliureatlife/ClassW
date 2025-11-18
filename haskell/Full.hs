-- full.hs
-- Defines the full syntax data type
-- Imports:

module Full where

type FVar = String
type FAtom = String
type FFeature = String

data FPNum = FPInt Int | FPFloat Float
  deriving (Show, Eq)

data FStmt
  = FSkip SkipType
  | FThread InStmt
  | FLocal InStmt
  | FEq Patt FBindRHS
  | FIf Exp InStmt [ElseIf] (Maybe InStmt)
  | FCase Exp Patt InStmt [(Patt, InStmt)] (Maybe InStmt)
  | FApply String [Exp]
  | FNewCell Exp FVar
  | FExchange FVar FVar Exp
  | FSetCell FVar Exp
  | FByNeed Exp FVar
  deriving (Show, Eq)

data FBindRHS = Ex Exp | Pr [FVar] InStmt | Fn [FVar] InExp
  deriving (Show, Eq)

type InStmt = (Maybe DecPart, [FStmt])
type InExp = (Maybe DecPart, [FStmt], Exp)
type ElseIf = (Exp, InStmt)
type ElseIfExp = (Exp, InExp)
type DecPart = ([FVar], [(Patt, Exp)])

data Exp
  = ENum FPNum
  | EVar FVar
  | EList [Exp]
  | EParen Exp Exp String
  | ERcd FAtom [(FFeature, Exp)]
  | EFun [FVar] InExp
  | EProc [FVar] InStmt
  | ELocal InExp
  | EIf Exp InExp [ElseIfExp] (Maybe InExp)
  | ECase Exp Patt InExp [(Patt, InExp)] (Maybe InExp)
  | EThread InExp
  | ENewCell Exp
  | EAtCell FVar
  | EFunApp String [Exp]
  deriving (Show, Eq)

data Patt
  = PtVar FVar
  | PtList [Patt]
  | PtParen Patt Patt Char
  | PtRec FAtom [(FFeature, Patt)]
  deriving (Show, Eq)

data SkipType
  = FBasic 
  | FStore 
  | FFull 
  | FCheck 
  | FBrowse String
  | FStack
  deriving (Show, Eq)
