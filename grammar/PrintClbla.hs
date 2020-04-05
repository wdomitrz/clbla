{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintClbla.
--   Generated by the BNF converter.

module PrintClbla where

import qualified AbsClbla
import Data.Char

-- | The top-level printing method.

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
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
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
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsClbla.UpperCaseString where
  prt _ (AbsClbla.UpperCaseString i) = doc (showString i)

instance Print AbsClbla.LowerCaseString where
  prt _ (AbsClbla.LowerCaseString i) = doc (showString i)

instance Print AbsClbla.Env where
  prt i e = case e of
    AbsClbla.Enviorment instructions -> prPrec i 0 (concatD [prt 0 instructions])

instance Print [AbsClbla.Instruction] where
  prt = prtList

instance Print AbsClbla.Instruction where
  prt i e = case e of
    AbsClbla.InstructionTypeDefinition typedefinition -> prPrec i 0 (concatD [prt 0 typedefinition])
    AbsClbla.InstructionFunctionDefinition functiondefinition -> prPrec i 0 (concatD [prt 0 functiondefinition])
    AbsClbla.InstructionFunctionDeclaration functiondeclaration -> prPrec i 0 (concatD [prt 0 functiondeclaration])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print AbsClbla.TypeDefinition where
  prt i e = case e of
    AbsClbla.TypeDefinition typename parameters algebraictypebody -> prPrec i 0 (concatD [doc (showString "data"), prt 0 typename, prt 0 parameters, doc (showString "="), prt 0 algebraictypebody])

instance Print AbsClbla.TypeName where
  prt i e = case e of
    AbsClbla.TypeName uppercasestring -> prPrec i 0 (concatD [prt 0 uppercasestring])

instance Print AbsClbla.Parameters where
  prt i e = case e of
    AbsClbla.Parameters variables -> prPrec i 0 (concatD [prt 0 variables])

instance Print [AbsClbla.Variable] where
  prt = prtList

instance Print AbsClbla.Variable where
  prt i e = case e of
    AbsClbla.Variable lowercasestring -> prPrec i 0 (concatD [prt 0 lowercasestring])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsClbla.AlgebraicTypeBody where
  prt i e = case e of
    AbsClbla.AlgebraicTypeBody algebraictypebodyelements -> prPrec i 0 (concatD [prt 0 algebraictypebodyelements])

instance Print [AbsClbla.AlgebraicTypeBodyElement] where
  prt = prtList

instance Print AbsClbla.AlgebraicTypeBodyElement where
  prt i e = case e of
    AbsClbla.AlgebraicTypeBodyElement typeconstructor types -> prPrec i 0 (concatD [prt 0 typeconstructor, prt 0 types])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print AbsClbla.TypeConstructor where
  prt i e = case e of
    AbsClbla.TypeConstructor uppercasestring -> prPrec i 0 (concatD [prt 0 uppercasestring])

instance Print [AbsClbla.Type] where
  prt = prtList

instance Print AbsClbla.Type where
  prt i e = case e of
    AbsClbla.NamedType typename parameters -> prPrec i 0 (concatD [prt 0 typename, prt 0 parameters])
    AbsClbla.TypeVariable variable -> prPrec i 0 (concatD [prt 0 variable])
    AbsClbla.FunctionType type_1 type_2 -> prPrec i 0 (concatD [prt 0 type_1, doc (showString "->"), prt 0 type_2])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsClbla.FunctionDeclaration where
  prt i e = case e of
    AbsClbla.FunctionDeclaration variable type_ -> prPrec i 0 (concatD [prt 0 variable, doc (showString "::"), prt 0 type_])

instance Print AbsClbla.FunctionName where
  prt i e = case e of
    AbsClbla.FunctionName variable -> prPrec i 0 (concatD [prt 0 variable])

instance Print AbsClbla.FunctionDefinition where
  prt i e = case e of
    AbsClbla.FunctionDefinition variable exp -> prPrec i 0 (concatD [prt 0 variable, doc (showString "="), prt 0 exp])

instance Print AbsClbla.Exp where
  prt i e = case e of
    AbsClbla.Application exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 exp2])
    AbsClbla.VariableExp variable -> prPrec i 0 (concatD [prt 0 variable])

