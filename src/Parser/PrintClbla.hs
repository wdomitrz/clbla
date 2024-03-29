{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Parser.PrintClbla where

-- pretty-printer generated by the BNF converter

import Parser.AbsClbla
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print UIdent where
  prt _ (UIdent i) = doc (showString ( i))


instance Print LIdent where
  prt _ (LIdent i) = doc (showString ( i))


instance Print InfixFunctionNameA where
  prt _ (InfixFunctionNameA i) = doc (showString ( i))


instance Print InfixFunctionNameB where
  prt _ (InfixFunctionNameB i) = doc (showString ( i))


instance Print InfixFunctionNameC where
  prt _ (InfixFunctionNameC i) = doc (showString ( i))


instance Print InfixFunctionNameD where
  prt _ (InfixFunctionNameD i) = doc (showString ( i))


instance Print InfixFunctionNameE where
  prt _ (InfixFunctionNameE i) = doc (showString ( i))


instance Print InfixFunctionNameF where
  prt _ (InfixFunctionNameF i) = doc (showString ( i))


instance Print InfixFunctionNameG where
  prt _ (InfixFunctionNameG i) = doc (showString ( i))


instance Print InfixFunctionNameH where
  prt _ (InfixFunctionNameH i) = doc (showString ( i))


instance Print InfixFunctionNameI where
  prt _ (InfixFunctionNameI i) = doc (showString ( i))


instance Print InfixFunctionNameJ where
  prt _ (InfixFunctionNameJ i) = doc (showString ( i))


instance Print InfixFunctionNameK where
  prt _ (InfixFunctionNameK i) = doc (showString ( i))



instance Print Programme where
  prt i e = case e of
    Prog extensions imports environment -> prPrec i 0 (concatD [prt 0 extensions, prt 0 imports, prt 0 environment])

instance Print Extension where
  prt i e = case e of
    Ext uident -> prPrec i 0 (concatD [doc (showString "{# LANGUAGE"), prt 0 uident, doc (showString "#}")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Import where
  prt i e = case e of
    Imp uident -> prPrec i 0 (concatD [doc (showString "import"), prt 0 uident])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Environment where
  prt i e = case e of
    Env instructions -> prPrec i 0 (concatD [prt 0 instructions])

instance Print Instruction where
  prt i e = case e of
    InstructionTypeDefinition typedefinition -> prPrec i 0 (concatD [prt 0 typedefinition])
    InstructionFunctionDeclaration functiondeclaration -> prPrec i 0 (concatD [prt 0 functiondeclaration])
    InstructionFunctionDefinition functiondefinition -> prPrec i 0 (concatD [prt 0 functiondefinition])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print TypeDefinition where
  prt i e = case e of
    TDef uident parameters typeconstructors -> prPrec i 0 (concatD [doc (showString "data"), prt 0 uident, prt 0 parameters, doc (showString "="), prt 0 typeconstructors])

instance Print Parameter where
  prt i e = case e of
    ParamVar lident -> prPrec i 0 (concatD [prt 0 lident])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print TypeConstructor where
  prt i e = case e of
    TConst uident types -> prPrec i 0 (concatD [prt 0 uident, prt 2 types])
    TConstEmpty uident -> prPrec i 0 (concatD [prt 0 uident])
    TInfixConst type_ typeinfixconstructorname types -> prPrec i 0 (concatD [prt 0 type_, prt 0 typeinfixconstructorname, prt 2 types])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "|"), prt 0 xs])
instance Print Type where
  prt i e = case e of
    TFun type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])
    TNamed uident types -> prPrec i 1 (concatD [prt 0 uident, prt 2 types])
    TNamedNoParam uident -> prPrec i 2 (concatD [prt 0 uident])
    TVar lident -> prPrec i 2 (concatD [prt 0 lident])
  prtList 2 [x] = (concatD [prt 2 x])
  prtList 2 (x:xs) = (concatD [prt 2 x, prt 2 xs])
instance Print FunctionDeclaration where
  prt i e = case e of
    FDecl functionbasename type_ -> prPrec i 0 (concatD [prt 0 functionbasename, doc (showString "::"), prt 0 type_])

instance Print FunctionDefinition where
  prt i e = case e of
    FDef functionbasename expression -> prPrec i 0 (concatD [prt 0 functionbasename, doc (showString "="), prt 0 expression])
    FDefWhere functionbasename expression environment -> prPrec i 0 (concatD [prt 0 functionbasename, doc (showString "="), prt 0 expression, doc (showString "where"), doc (showString "{"), prt 0 environment, doc (showString "}")])

instance Print Expression where
  prt i e = case e of
    EOpA expression1 infixfunctionnamea expression2 -> prPrec i 0 (concatD [prt 1 expression1, prt 0 infixfunctionnamea, prt 0 expression2])
    EOpB expression1 infixfunctionnameb expression2 -> prPrec i 1 (concatD [prt 2 expression1, prt 0 infixfunctionnameb, prt 1 expression2])
    EOpC expression1 infixfunctionnamec expression2 -> prPrec i 2 (concatD [prt 3 expression1, prt 0 infixfunctionnamec, prt 2 expression2])
    EOpD expression1 infixfunctionnamed expression2 -> prPrec i 3 (concatD [prt 4 expression1, prt 0 infixfunctionnamed, prt 3 expression2])
    EOpE expression1 infixfunctionnamee expression2 -> prPrec i 4 (concatD [prt 5 expression1, prt 0 infixfunctionnamee, prt 4 expression2])
    EOpF expression1 infixfunctionnamef expression2 -> prPrec i 5 (concatD [prt 6 expression1, prt 0 infixfunctionnamef, prt 5 expression2])
    EOpG expression1 infixfunctionnameg expression2 -> prPrec i 6 (concatD [prt 7 expression1, prt 0 infixfunctionnameg, prt 6 expression2])
    EOpH expression1 infixfunctionnameh expression2 -> prPrec i 7 (concatD [prt 8 expression1, prt 0 infixfunctionnameh, prt 7 expression2])
    EOpI expression1 infixfunctionnamei expression2 -> prPrec i 8 (concatD [prt 9 expression1, prt 0 infixfunctionnamei, prt 8 expression2])
    EOpJ expression1 infixfunctionnamej expression2 -> prPrec i 9 (concatD [prt 10 expression1, prt 0 infixfunctionnamej, prt 9 expression2])
    EOpK expression1 infixfunctionnamek expression2 -> prPrec i 10 (concatD [prt 11 expression1, prt 0 infixfunctionnamek, prt 10 expression2])
    ELet environment expression -> prPrec i 1 (concatD [doc (showString "let"), doc (showString "{"), prt 0 environment, doc (showString "}"), doc (showString "in"), prt 1 expression])
    EApp expression1 expression2 -> prPrec i 11 (concatD [prt 11 expression1, prt 12 expression2])
    EVar functionname -> prPrec i 12 (concatD [prt 0 functionname])

instance Print FunctionBaseName where
  prt i e = case e of
    FBName lident -> prPrec i 0 (concatD [prt 0 lident])
    FIBName functioninfixname -> prPrec i 0 (concatD [doc (showString "("), prt 0 functioninfixname, doc (showString ")")])

instance Print FunctionName where
  prt i e = case e of
    FName functionbasename -> prPrec i 0 (concatD [prt 0 functionbasename])
    FTCName uident -> prPrec i 0 (concatD [prt 0 uident])
    FITCName typeinfixconstructorname -> prPrec i 0 (concatD [doc (showString "("), prt 0 typeinfixconstructorname, doc (showString ")")])

instance Print FunctionInfixName where
  prt i e = case e of
    FunctionInfixNameInfixFunctionNameA infixfunctionnamea -> prPrec i 0 (concatD [prt 0 infixfunctionnamea])
    FunctionInfixNameInfixFunctionNameB infixfunctionnameb -> prPrec i 0 (concatD [prt 0 infixfunctionnameb])
    FunctionInfixNameInfixFunctionNameC infixfunctionnamec -> prPrec i 0 (concatD [prt 0 infixfunctionnamec])
    FunctionInfixNameInfixFunctionNameD infixfunctionnamed -> prPrec i 0 (concatD [prt 0 infixfunctionnamed])
    FunctionInfixNameInfixFunctionNameE infixfunctionnamee -> prPrec i 0 (concatD [prt 0 infixfunctionnamee])
    FunctionInfixNameInfixFunctionNameG infixfunctionnameg -> prPrec i 0 (concatD [prt 0 infixfunctionnameg])
    FunctionInfixNameInfixFunctionNameH infixfunctionnameh -> prPrec i 0 (concatD [prt 0 infixfunctionnameh])
    FunctionInfixNameInfixFunctionNameI infixfunctionnamei -> prPrec i 0 (concatD [prt 0 infixfunctionnamei])
    FunctionInfixNameInfixFunctionNameJ infixfunctionnamej -> prPrec i 0 (concatD [prt 0 infixfunctionnamej])
    FunctionInfixNameInfixFunctionNameK infixfunctionnamek -> prPrec i 0 (concatD [prt 0 infixfunctionnamek])

instance Print TypeInfixConstructorName where
  prt i e = case e of
    TIConstName infixfunctionnamef -> prPrec i 0 (concatD [prt 0 infixfunctionnamef])


