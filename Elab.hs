-- Elab.hs
-- Exports a function that elaborates (converts) full to kernel.
-- Imports: kernel.hs, full.hs

module Elab where

import Kernel
import Full
import Data.List (union, (\\))

-- Input: list of statements from full, integer for creating new unique names
-- Output: list of statements in the kernel syntax

-- Naming. Integer cnt is passed recursively through function calls, being
-- incremented whenever a new name is created. Naming is done individually:
--    e.g., exID = "EXU" ++ show (cnt+1)
-- or in a list:
--    e.g., exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length exps)]]
-- Expressions use EXU, NewCell use NCU, ByNeed BNU, Patterns use PTU,
-- SetCell use SCU and GarbU (unused variable in exchange)
-- Expressions are renamed by default so " if X then ..."
-- becomes :
--    local EXU1 in
--      EXU1 = X
--      if EXU1 then ...
--  Special case for unecessary renaming of identifiers is not handled


-- Recursively move through statements, using pattern matching to distinguish
-- statement types.
elab :: [FStmt] -> Int -> ([Stmt], Int)
elab [] cnt = ([], cnt)
elab (f:fs) cnt = (elabFront ++ elabRest, cnt'')
  where
    (elabFront, cnt') = elabS f cnt
    (elabRest, cnt'') = elab fs cnt'

elabS :: FStmt -> Int -> ([Stmt], Int)
elabS f cnt = case f of
      FSkip skipType -> ([Skip (sTrans skipType)], cnt)
      FThread inStmt -> ([Thread (fst $ inStmtH cnt inStmt)], cnt)
      FLocal inStmt -> inStmtH cnt inStmt
      FEq (PtVar x) (Ex ex) -> (expH cnt ex x, cnt)
      FEq patt (Ex (EVar v)) -> (pattBinds ++ [EqVar mainPattVar v], cnt + length pattVars)
        where
          (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
      FEq patt (Ex ex) -> (fullK, cnt + length pattVars + 1)
        where
          (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
          exStmts = expH (cnt + length pattVars) ex mainPattVar
          fullK = if null pattVars then pattBinds ++ exStmts else [Local pattVars (pattBinds ++ exStmts)]
      FEq (PtVar x) (Pr params body) -> ([EqVal x (PProc params (fst $ inStmtH cnt body))], cnt)
      FEq (PtVar x) (Fn params body) -> fullK
        where
          exID = "EXU" ++ show (cnt+1)
          (bodyN,_) = inExpH body (cnt+1) exID
          proc = PProc (params ++ [exID]) bodyN
          fullK = ([EqVal x proc], cnt+1)
      FIf (EVar exID) inStmt [] mElse -> ([ifStmt], cnt)
        where
          (inStmts,_) = inStmtH cnt inStmt
          oElse = case mElse of
            Nothing -> [Skip Basic]
            Just (inStmt) -> fst $ inStmtH cnt inStmt
          ifStmt = If exID inStmts oElse
      FIf ex inStmt [] mElse -> (fullK, cnt + 1)
        where
          exID = "EXU" ++ show (cnt+1)
          exStmts = expH (cnt + 1) ex exID
          (inStmts,_) = inStmtH cnt inStmt
          oElse = case mElse of
            Nothing -> [Skip Basic]
            Just (inStmt) -> fst $ inStmtH cnt inStmt
          ifStmt = If exID inStmts oElse
          fullK = [Local [exID] (exStmts ++ [ifStmt])]
      FIf (EVar exID) inStmt ((ex2,inStmt2):elseIfs) mElse -> ([ifStmt], cnt)
        where
          (inStmts,_) = inStmtH cnt inStmt
          (nestStmts,_) = elab [(FIf ex2 inStmt2 elseIfs mElse)] (cnt+1)
          ifStmt = If exID inStmts nestStmts
      FIf ex inStmt ((ex2,inStmt2):elseIfs) mElse -> (fullK, cnt + 1)
        where
          exID = "EXU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex exID
          (inStmts,_) = inStmtH cnt inStmt
          (nestStmts,_) = elab [(FIf ex2 inStmt2 elseIfs mElse)] (cnt+1)
          ifStmt = If exID inStmts nestStmts
          fullK = [Local [exID] (exStmts ++ [ifStmt])]
      FCase (EVar exID) patt inStmt [] mElse -> (fullK, cnt)
        where
          (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
          (recLit, pattBinds') = case reverse pattBinds of
            (EqVal _ (PRec x)):ps -> (x, reverse ps)
            _ -> error "case called on non record pattern"
          (thenBodyStmts, _) = inStmtH cnt inStmt
          oElse = case mElse of
            Nothing -> [Skip Basic]
            Just (inStmt) -> fst $ inStmtH cnt inStmt
          nestedCases = buildNestedCases pattBinds' thenBodyStmts oElse
          caseStmt = Case exID recLit nestedCases oElse
          fullK = [caseStmt]
          buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
          buildNestedCases [] successBranch _ = successBranch
          buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
            [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
      FCase ex patt inStmt [] mElse -> (fullK, cnt + 1)
        where
          exID = "EXU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex exID
          (introVars, pattVars, pattBinds, mainPattVar) = pattH (cnt+2) patt
          (recLit, pattBinds') = case reverse pattBinds of
            (EqVal _ (PRec x)):ps -> (x, reverse ps)
            _ -> error "case called on non record pattern"
          (thenBodyStmts, _) = inStmtH (cnt+2) inStmt
          oElse = case mElse of
            Nothing -> [Skip Basic]
            Just (inStmt) -> fst $ inStmtH (cnt+2) inStmt
          nestedCases = buildNestedCases pattBinds' thenBodyStmts oElse
          caseStmt = Case exID recLit nestedCases oElse
          fullK = [Local [exID] (exStmts ++ [caseStmt])]
          buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
          buildNestedCases [] successBranch _ = successBranch
          buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
            [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
      FCase (EVar exID) patt inStmt ((patt2,inStmt2):opts) mElse -> (fullK, cnt)
        where
          (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
          (recLit, pattBinds') = case reverse pattBinds of
            (EqVal _ (PRec x)):ps -> (x, reverse ps)
            _ -> error "case called on non record pattern"
          (thenBodyStmts, _) = inStmtH cnt inStmt
          (oElse, _) = elab [(FCase (EVar exID) patt2 inStmt2 opts mElse)] cnt
          nestedCases = buildNestedCases pattBinds' thenBodyStmts oElse
          caseStmt = Case exID recLit nestedCases oElse
          fullK = [caseStmt]
          buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
          buildNestedCases [] successBranch _ = successBranch
          buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
            [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
      FCase ex patt inStmt ((patt2,inStmt2):opts) mElse -> (fullK, cnt + 1)
        where
          exID = "EXU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex exID
          (introVars, pattVars, pattBinds, mainPattVar) = pattH (cnt+2) patt
          (recLit, pattBinds') = case reverse pattBinds of
            (EqVal _ (PRec x)):ps -> (x, reverse ps)
            _ -> error "case called on non record pattern"
          (thenBodyStmts, _) = inStmtH (cnt+2) inStmt
          (oElse, _) = elab [(FCase (EVar exID) patt2 inStmt2 opts mElse)] (cnt+2)
          nestedCases = buildNestedCases pattBinds' thenBodyStmts oElse
          caseStmt = Case exID recLit nestedCases oElse
          fullK = [Local [exID] (exStmts ++ [caseStmt])]
          buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
          buildNestedCases [] successBranch _ = successBranch
          buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
            [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
          buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
      FApply procID params -> (fullK, cnt + length (concat newVars))
        where
          (exIDs, exStmts, newVars) = unzip3 $ map (elabP cnt) (zip [1..] params)
          applyStmt = Apply procID exIDs
          fullK = if null (concat newVars) then concat exStmts ++ [applyStmt]
            else [Local (concat newVars) (concat exStmts ++ [applyStmt])]
      FNewCell (EVar v) id1 -> ([NewCell v id1], cnt)
      FNewCell ex id1 -> (fullK, cnt + 1)
        where
          id2 = "NCU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex id2
          fullK = [Local [id2] (exStmts ++ [NewCell id2 id1])]
      FExchange id1 id2 (EVar v) -> ([Exchange id1 id2 v], cnt)
      FExchange id1 id2 ex -> (fullK, cnt + 1)
        where
          id3 = "NCU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex id3
          fullK = [Local [id3] (exStmts ++ [Exchange id1 id2 id3])]
      FSetCell id1 (EVar v) -> (fullK, cnt + 1)
        where
          id2 = "GarbU" ++ show (cnt+1)
          fullK = [Local [id2] [Exchange id1 id2 v]]
      FSetCell id1 ex -> (fullK, cnt + 1)
        where
          exID = "SCU" ++ show (cnt+1)
          id2 = "GarbU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex exID
          fullK = [Local [exID,id2] (exStmts ++ [Exchange id1 id2 exID])]
      FByNeed (EVar v) id1 -> ([ByNeed v id1], cnt)
      FByNeed ex id1 -> (fullK, cnt + 1)
        where
          id2 = "BNU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex id2
          fullK = [Local [id2] (exStmts ++ [ByNeed id2 id1])]

sTrans :: Full.SkipType -> Kernel.SkipType
sTrans FBasic = Basic
sTrans FStore = Store
sTrans FFull = Full
sTrans FCheck = Check
sTrans (FBrowse s) = Browse s
sTrans FStack = Stack

inStmtH :: Int -> InStmt -> ([Stmt], Int)
inStmtH cnt (Just decPart ,fstmts) = decPartH decPart cnt (elab fstmts cnt)
inStmtH cnt (Nothing, fstmts) = elab fstmts cnt

decPartH :: DecPart -> Int -> ([Stmt], Int) -> ([Stmt], Int)
decPartH ([], binds) cnt (ss, ss_cnt) = bindsH binds cnt ss
decPartH (ids, binds) cnt (ss, ss_cnt) = ([Local ids bindStmts], cnt')
  where
    (bindStmts, cnt') = bindsH binds cnt ss

bindsH :: [(Patt, Exp)] -> Int -> [Stmt] -> ([Stmt], Int)
bindsH [] cnt ss = (ss, cnt)
bindsH ((patt, EVar v):binds) cnt ss =
    let (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
        vars = introVars ++ pattVars
        bindStmts = pattBinds ++ [EqVar mainPattVar v]
        (recBinds, cnt') = bindsH binds (cnt + length pattVars) ss
        fullB = if null vars
                then bindStmts ++ recBinds
                else [Local vars (bindStmts ++ recBinds)]
    in (fullB, cnt')
bindsH ((patt,ex):binds) cnt ss =
    let (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
        expBinds = expH (cnt + length pattVars) ex mainPattVar
        vars = introVars ++ pattVars
        bindStmts = pattBinds ++ expBinds
        (recBinds, cnt') = bindsH binds (cnt + length pattVars + 1) ss
        fullB = if null vars
                then bindStmts ++ recBinds
                else [Local vars (bindStmts ++ recBinds)]
    in (fullB, cnt')

inExpH :: InExp -> Int-> String -> ([Stmt], Int)
inExpH (Just decPart, fstmts, ex) cnt expID
  = let (stmts, cnt') = elab fstmts cnt
        (binds, cnt'') = decPartH decPart cnt' (stmts ++ expH cnt' ex expID, cnt')
    in (binds, cnt'')
inExpH (Nothing, fstmts, ex) cnt expID
  = let (stmts, cnt') = elab fstmts cnt
    in (stmts ++ expH cnt' ex expID, cnt')

opPairs :: [(String,String)]
opPairs = [("+","IntPlus"),("-","IntMinus"),("*","IntMultiply"),("/","Divide"),("<","LT"),(">","GT"),
  ("div","DivideInt"),("mod","Mod"),("==","Eq"),("\\=","NotEqual"),("=<","LessThanEq"),("=>","GreaterThanEq")]

expH :: Int -> Exp -> String -> [Stmt]
expH cnt ex id2bind = case ex of
  ENum (FPInt x) -> [EqVal id2bind (PInt x)]
  ENum (FPFloat x) -> [EqVal id2bind (PFloat x)]
  EVar exID -> [EqVar id2bind exID]
  EList [] -> [EqVal id2bind (PRec ("nil",[]))]
  EList exps -> [Local exIDs (exStmts ++ listStmts)]
    where
      exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length exps)]]
      exStmts = concat $ map (\(a,b) -> expH (cnt + length exps) a b) $ zip exps exIDs
      listStmts = expH (cnt + length exps) (listbuild (head exIDs) (tail exIDs)) id2bind
  ERcd label featNexps -> fullK
    where
      (feats, exps) = unzip featNexps
      (exIDs, exStmts, newVars) = unzip3 $ map (elabP cnt) (zip [1..] exps)
      lRcd = PRec (label, zip feats exIDs)
      bnd = EqVal id2bind lRcd
      fullK = if null (concat newVars) then concat exStmts ++ [bnd]
        else [Local (concat newVars) (concat exStmts ++ [bnd])]
  EParen ex1 ex2 "#" -> expH cnt (ERcd "'#'" [("1",ex1),("2",ex2)]) id2bind
  EParen ex1 ex2 "|" -> expH cnt (ERcd "'|'" [("1",ex1),("2",ex2)]) id2bind
  EParen (EVar v1) (EVar v2) op -> case lookup op opPairs of
    Just opName -> [Apply opName [v1, v2, id2bind]]
    Nothing -> error $ "Unsupported binary operator: " ++ op
  EParen ex1 (EVar v2) op -> case lookup op opPairs of
    Just opName -> [Local [ex1ID] (ex1Stmts ++ [Apply opName [ex1ID, v2, id2bind]])]
    Nothing -> error $ "Unsupported binary operator: " ++ op
    where
      ex1ID = "EXU" ++ show (cnt+1)
      ex1Stmts = expH (cnt+2) ex1 ex1ID
  EParen (EVar v1) ex2 op -> case lookup op opPairs of
    Just opName -> [Local [ex2ID] (ex2Stmts ++ [Apply opName [v1, ex2ID, id2bind]])]
    Nothing -> error $ "Unsupported binary operator: " ++ op
    where
      ex2ID = "EXU" ++ show (cnt+1)
      ex2Stmts = expH (cnt+2) ex2 ex2ID
  EParen ex1 ex2 op -> case lookup op opPairs of
    Just opName -> [Local [ex1ID, ex2ID] (ex1Stmts ++ ex2Stmts ++ [Apply opName [ex1ID, ex2ID, id2bind]])]
    Nothing -> error $ "Unsupported binary operator: " ++ op
    where
      (ex1ID,ex2ID) = ("EXU" ++ show (cnt+1),"EXU" ++ show (cnt+2))
      (ex1Stmts, ex2Stmts) = (expH (cnt+3) ex1 ex1ID, expH (cnt+3) ex2 ex2ID)
  EFunApp funID params -> fullK
    where
      (exIDs, exStmts, newVars) = unzip3 $ map (elabP cnt) (zip [1..] params)
      applyStmt = Apply funID (exIDs++[id2bind])
      fullK = if null (concat newVars) then concat exStmts ++ [applyStmt]
        else [Local (concat newVars) (concat exStmts ++ [applyStmt])]
  EFun params inExp -> [EqVal id2bind (PProc pParams pBody)]
    where
      exID = "EXU" ++ show (cnt+1)
      pParams = params ++ [exID]
      pBody = fst $ inExpH inExp (cnt+1) exID
  EProc params inStmt -> [EqVal id2bind (PProc params (fst $ inStmtH cnt inStmt))]
  ELocal inExp -> fst $ inExpH inExp cnt id2bind
  EIf (EVar exID) inExp [] mElse -> fullK
    where
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      oElse = case mElse of
        Nothing -> [Skip Basic]
        Just (inExp) -> fst $ inExpH inExp (cnt+1) id2bind
      ifStmt = If exID inExps oElse
      fullK = [ifStmt]
  EIf ex inExp [] mElse -> fullK
    where
      exID = "EXU" ++ show (cnt+1)
      exStmts = expH (cnt+1) ex exID
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      oElse = case mElse of
        Nothing -> [Skip Basic]
        Just (inExp) -> fst $ inExpH inExp (cnt+1) id2bind
      ifStmt = If exID inExps oElse
      fullK = [Local [exID] (exStmts ++ [ifStmt])]
  EIf (EVar exID) inExp ((ex2,inExp2):elseIfs) mElse -> fullK
    where
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      nestStmts = expH (cnt+1) (EIf ex2 inExp2 elseIfs mElse) id2bind
      ifStmt = If exID inExps nestStmts
      fullK = [ifStmt]
  EIf ex inExp ((ex2,inExp2):elseIfs) mElse -> fullK
    where
      exID = "EXU" ++ show (cnt+1)
      exStmts = expH (cnt+1) ex exID
      (inExps,_) = inExpH inExp (cnt+1) id2bind
      nestStmts = expH (cnt+1) (EIf ex2 inExp2 elseIfs mElse) id2bind
      ifStmt = If exID inExps nestStmts
      fullK = [Local [exID] (exStmts ++ [ifStmt])]
  ECase (EVar exID) patt inExp [] mElse -> [caseStmt]
    where
      (p1, p2, p3, p4) = pattH cnt patt
      (recLit, p3') = case reverse p3 of
        (EqVal _ (PRec x)):ps -> (x, reverse ps)
        _ -> error "case called on non record pattern"
      (caseBody,_) = inExpH inExp cnt id2bind
      oElse = case mElse of
        Nothing -> [Skip Basic]
        Just (inExp) -> fst $ inExpH inExp cnt id2bind
      nestedCases = buildNestedCases p3' caseBody oElse
      caseStmt = Case exID recLit nestedCases oElse
      buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
      buildNestedCases [] successBranch _ = successBranch
      buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
        [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
      buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
  ECase ex patt inExp [] mElse -> fullK
    where
      exID = "EXU" ++ show (cnt+1)
      exStmts = expH (cnt+1) ex exID
      (p1, p2, p3, p4) = pattH (cnt+2) patt
      (recLit, p3') = case reverse p3 of
        (EqVal _ (PRec x)):ps -> (x, reverse ps)
        _ -> error "case called on non record pattern"
      (caseBody,_) = inExpH inExp (cnt+2) id2bind
      oElse = case mElse of
        Nothing -> [Skip Basic]
        Just (inExp) -> fst $ inExpH inExp (cnt+2) id2bind
      nestedCases = buildNestedCases p3' caseBody oElse
      caseStmt = Case exID recLit nestedCases oElse
      fullK = [Local [exID] (exStmts ++ [caseStmt])]
      buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
      buildNestedCases [] successBranch _ = successBranch
      buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
        [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
      buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
  ECase (EVar exID) patt inExp ((patt2,inExp2):opts) mElse -> [caseStmt]
    where
      (p1, p2, p3, p4) = pattH cnt patt
      (recLit, p3') = case reverse p3 of
        (EqVal _ (PRec x)):ps -> (x, reverse ps)
        _ -> error "case called on non record pattern"
      (caseBody,_) = inExpH inExp cnt id2bind
      oElse = expH cnt (ECase (EVar exID) patt2 inExp2 opts mElse) id2bind
      nestedCases = buildNestedCases p3' caseBody oElse
      caseStmt = Case exID recLit nestedCases oElse
      buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
      buildNestedCases [] successBranch _ = successBranch
      buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
        [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
      buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
  ECase ex patt inExp ((patt2,inExp2):opts) mElse -> fullK
    where
      exID = "EXU" ++ show (cnt+1)
      exStmts = expH (cnt+1) ex exID
      (p1, p2, p3, p4) = pattH (cnt+2) patt
      (recLit, p3') = case reverse p3 of
        (EqVal _ (PRec x)):ps -> (x, reverse ps)
        _ -> error "case called on non record pattern"
      (caseBody,_) = inExpH inExp (cnt+2) id2bind
      oElse = expH (cnt+2) (ECase (EVar exID) patt2 inExp2 opts mElse) id2bind
      nestedCases = buildNestedCases p3' caseBody oElse
      caseStmt = Case exID recLit nestedCases oElse
      fullK = [Local [exID] (exStmts ++ [caseStmt])]
      buildNestedCases :: [Stmt] -> [Stmt] -> [Stmt] -> [Stmt]
      buildNestedCases [] successBranch _ = successBranch
      buildNestedCases ((EqVal var (PRec lit)):rest) successBranch failBranch =
        [Case var lit (buildNestedCases rest successBranch failBranch) failBranch]
      buildNestedCases _ _ _ = error "Impossible: pattBind not a record"
  EThread inExp -> [Thread (fst $ inExpH inExp cnt id2bind)]
  ENewCell (EVar v) -> [NewCell v id2bind]
  ENewCell ex -> fullK
    where
      exID = "NCU" ++ show (cnt+1)
      exStmts = expH (cnt + 1) ex exID
      newCell = [NewCell exID id2bind]
      fullK = [Local [exID] (exStmts ++ newCell)]
  EAtCell id1 -> [Exchange id1 id2bind id2bind]

pattH :: Int -> Patt -> ([String], [String], [Stmt], String)
pattH cnt patt = case patt of
  PtVar v -> ([v],[], [], v)
  PtList [] -> ([], ["PTU"++show cnt], [EqVal ("PTU"++show cnt) (PRec ("nil",[] ))], "PTU"++show cnt)
  PtList patts -> (concat p1s, concat p2s++["PTU"++show cnt], concat p3s++lstmts, "PTU"++show cnt)
    where
      (p1s,p2s,p3s,p4s) = unZip4 $ map (\(a,b) -> pattH a b) $ zip [cnt+i| i<-[1..(length patts)]] patts
      lstmts = expH (cnt+1) (listbuild (head p4s) (tail p4s)) ("PTU"++show cnt)
  PtParen p1 p2 op -> (p11++p21 ,p12++p22++["PTU"++show cnt] ,p13++p23++[bnd], "PTU"++show cnt)
    where
      (p11, p12, p13, p14) = pattH (cnt+1) p1
      (p21, p22, p23, p24) = pattH (cnt+2) p2
      pRcd = PRec ('\'':op:'\'':[], [("1",p14),("2",p24)])
      bnd = EqVal ("PTU"++show cnt) pRcd
  PtRec label featNpatts -> (concat p1s, concat p2s++["PTU"++show cnt], concat p3s++[bnd], "PTU"++show cnt)
    where
      (feats, patts) = unzip featNpatts
      (p1s,p2s,p3s,p4s) = unZip4 $ map (\(a,b) -> pattH a b) $ zip [cnt+i| i<-[1..(length patts)]] patts
      lRcd = PRec (label, zip feats p4s)
      bnd = EqVal ("PTU"++show cnt) lRcd

listbuild :: String -> [String] -> Exp
listbuild h [] = ERcd "'|'" [("1",EVar h),("2",ERcd "nil" [])]
listbuild h (t:ts) = ERcd "'|'" [("1",EVar h),("2",listbuild t ts)]

unZip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unZip4 [] = ([],[],[],[])
unZip4 ((a,b,c,d):l) = (a:as,b:bs,c:cs,d:ds)
  where
    (as,bs,cs,ds) = unZip4 l

elabP :: Int -> (Int, Exp) -> (String, [Stmt], [String])
elabP cnt (i, EVar v) = (v, [], [])
elabP cnt (i, ex) = (exID, exStmts, [exID])
  where
    exID = "EXU" ++ show (cnt+i)
    exStmts = expH (cnt+i) ex exID
    
