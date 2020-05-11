{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParClbla where
import Parser.AbsClbla
import Parser.LexClbla
import Parser.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (UIdent)
happyIn4 :: (UIdent) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (LIdent)
happyIn5 :: (LIdent) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (InfixFunctionNameA)
happyIn6 :: (InfixFunctionNameA) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (InfixFunctionNameB)
happyIn7 :: (InfixFunctionNameB) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (InfixFunctionNameC)
happyIn8 :: (InfixFunctionNameC) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (InfixFunctionNameD)
happyIn9 :: (InfixFunctionNameD) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (InfixFunctionNameE)
happyIn10 :: (InfixFunctionNameE) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (InfixFunctionNameF)
happyIn11 :: (InfixFunctionNameF) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (InfixFunctionNameG)
happyIn12 :: (InfixFunctionNameG) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (InfixFunctionNameH)
happyIn13 :: (InfixFunctionNameH) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (InfixFunctionNameI)
happyIn14 :: (InfixFunctionNameI) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (InfixFunctionNameJ)
happyIn15 :: (InfixFunctionNameJ) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (InfixFunctionNameK)
happyIn16 :: (InfixFunctionNameK) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (Programme)
happyIn17 :: (Programme) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ([Extension])
happyIn18 :: ([Extension]) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Extension)
happyIn19 :: (Extension) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ([Import])
happyIn20 :: ([Import]) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Import)
happyIn21 :: (Import) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Environment)
happyIn22 :: (Environment) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ([Instruction])
happyIn23 :: ([Instruction]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (Instruction)
happyIn24 :: (Instruction) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (TypeDefinition)
happyIn25 :: (TypeDefinition) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 ([Parameter])
happyIn26 :: ([Parameter]) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (Parameter)
happyIn27 :: (Parameter) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ([TypeConstructor])
happyIn28 :: ([TypeConstructor]) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (TypeConstructor)
happyIn29 :: (TypeConstructor) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (Type)
happyIn30 :: (Type) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (Type)
happyIn31 :: (Type) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (Type)
happyIn32 :: (Type) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ([Type])
happyIn33 :: ([Type]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (FunctionDeclaration)
happyIn34 :: (FunctionDeclaration) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (FunctionDefinition)
happyIn35 :: (FunctionDefinition) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (Expression)
happyIn36 :: (Expression) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (Expression)
happyIn37 :: (Expression) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (Expression)
happyIn38 :: (Expression) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Expression)
happyIn39 :: (Expression) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Expression)
happyIn40 :: (Expression) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (Expression)
happyIn41 :: (Expression) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (Expression)
happyIn42 :: (Expression) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (Expression)
happyIn43 :: (Expression) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (Expression)
happyIn44 :: (Expression) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (Expression)
happyIn45 :: (Expression) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (Expression)
happyIn46 :: (Expression) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (Expression)
happyIn47 :: (Expression) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (Expression)
happyIn48 :: (Expression) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (FunctionBaseName)
happyIn49 :: (FunctionBaseName) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (FunctionName)
happyIn50 :: (FunctionName) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (FunctionInfixName)
happyIn51 :: (FunctionInfixName) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (TypeInfixConstructorName)
happyIn52 :: (TypeInfixConstructorName) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x10\x0c\x00\x00\x00\x00\x00\x00\x00\x20\x08\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x20\x00\x30\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x40\xf0\xff\x01\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x20\x00\x30\x00\x00\x00\x00\x00\x00\x00\x80\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x20\x00\x30\x00\x00\x00\x00\x00\x00\x00\x80\x00\xc1\x00\x00\x00\x00\x00\x00\x00\x00\x02\x04\x03\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x20\x08\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x08\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x40\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgramme","UIdent","LIdent","InfixFunctionNameA","InfixFunctionNameB","InfixFunctionNameC","InfixFunctionNameD","InfixFunctionNameE","InfixFunctionNameF","InfixFunctionNameG","InfixFunctionNameH","InfixFunctionNameI","InfixFunctionNameJ","InfixFunctionNameK","Programme","ListExtension","Extension","ListImport","Import","Environment","ListInstruction","Instruction","TypeDefinition","ListParameter","Parameter","ListTypeConstructor","TypeConstructor","Type","Type1","Type2","ListType2","FunctionDeclaration","FunctionDefinition","Expression","Expression1","Expression2","Expression3","Expression4","Expression5","Expression6","Expression7","Expression8","Expression9","Expression10","Expression11","Expression12","FunctionBaseName","FunctionName","FunctionInfixName","TypeInfixConstructorName","'#}'","'('","')'","'->'","'::'","';'","'='","'data'","'import'","'in'","'let'","'where'","'{'","'{# LANGUAGE'","'|'","'}'","L_UIdent","L_LIdent","L_InfixFunctionNameA","L_InfixFunctionNameB","L_InfixFunctionNameC","L_InfixFunctionNameD","L_InfixFunctionNameE","L_InfixFunctionNameF","L_InfixFunctionNameG","L_InfixFunctionNameH","L_InfixFunctionNameI","L_InfixFunctionNameJ","L_InfixFunctionNameK","%eof"]
        bit_start = st * 82
        bit_end = (st + 1) * 82
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..81]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x06\x00\x00\x00\xfb\xff\x40\x00\x15\x00\x0c\x00\x45\x00\x5b\x00\x00\x00\x59\x00\x00\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x11\x00\x3e\x00\x7f\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x9c\x01\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x00\x83\x00\x87\x00\x82\x00\xc8\x00\x85\x00\x8d\x00\x84\x00\xc5\x00\xc9\x00\xc4\x00\x72\x01\x00\x00\x00\x00\x00\x00\x80\x01\xdd\x00\x74\x01\x00\x00\x00\x00\xdf\x00\x00\x00\x74\x01\x00\x00\x08\x00\x00\x00\x00\x00\x74\x01\xe6\x00\x74\x01\x00\x00\x74\x01\x00\x00\x9d\x01\x00\x00\xe8\x00\xeb\x00\x00\x00\x9f\x01\x00\x00\x9f\x01\x9f\x01\x9f\x01\x9f\x01\x9f\x01\x9f\x01\x9f\x01\x9f\x01\x9c\x01\x9c\x01\xe4\x00\xa0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\x00\x00\x00\x00\x00\x00\x00\x8f\x00\x00\x00\x11\x01\x09\x01\xa2\x01\xa2\x01\xd6\x00\x1a\x01\xe5\x00\x00\x00\x9c\x01\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x03\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x1d\x01\x27\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\x00\x28\x01\x2b\x01\x00\x00\x00\x00\x00\x00\x0f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x57\x01\x12\x00\x3c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x01\x3b\x01\x43\x01\x44\x01\x47\x01\x26\x01\x4d\x01\x50\x01\x4c\x01\x56\x01\xef\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x29\x01\x00\x00\x00\x00\x00\x00\x00\x00\x62\x01\x00\x00\xdc\x00\x00\x00\x00\x00\x8e\x00\x00\x00\x65\x01\x00\x00\x41\x01\x00\x00\x21\x01\x00\x00\x00\x00\x00\x00\x00\x00\xec\x00\x00\x00\xe7\x00\xe1\x00\xda\x00\xab\x00\xa2\x00\x1f\x00\x98\x00\x8c\x00\x4f\x00\x41\x00\x00\x00\x25\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x01\x00\x00\x00\x00\x18\x01\x53\x01\x5f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf0\xff\x00\x00\xfe\xff\x00\x00\xed\xff\x00\x00\xe9\xff\x00\x00\x00\x00\xb3\xff\x00\x00\xf1\xff\xea\xff\xe8\xff\xe6\xff\xe5\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xef\xff\xeb\xff\xe2\xff\xae\xff\xad\xff\xac\xff\xab\xff\xaa\xff\xa9\xff\xa8\xff\xa7\xff\xa6\xff\xa5\xff\x00\x00\xfc\xff\xfb\xff\xfa\xff\xf9\xff\xf8\xff\xf6\xff\xf5\xff\xf4\xff\xf3\xff\xf2\xff\x00\x00\x00\x00\xe9\xff\xec\xff\xee\xff\xe7\xff\xb0\xff\xd0\xff\xce\xff\xcc\xff\xc9\xff\xc7\xff\xc5\xff\xc3\xff\xc1\xff\xbf\xff\xbd\xff\xbb\xff\xb9\xff\xb7\xff\xb1\xff\xb4\xff\x00\x00\x00\x00\xd5\xff\xd4\xff\xd1\xff\xda\xff\xd8\xff\x00\x00\xb2\xff\x00\x00\xe0\xff\xe1\xff\x00\x00\x00\x00\x00\x00\xd5\xff\xd3\xff\xd7\xff\xe9\xff\xa4\xff\x00\x00\x00\x00\xf7\xff\x00\x00\xb6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xcd\xff\xcb\xff\xc8\xff\xc6\xff\xc4\xff\xc2\xff\xc0\xff\xbe\xff\xbc\xff\xba\xff\xb8\xff\xaf\xff\xb5\xff\x00\x00\xd2\xff\xd9\xff\xd6\xff\xd5\xff\xe3\xff\xdf\xff\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\x00\x00\xcf\xff\x00\x00\xde\xff\xdb\xff\xca\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x02\x00\x07\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x08\x00\x09\x00\x05\x00\x11\x00\x07\x00\x1e\x00\x12\x00\x06\x00\x0f\x00\x10\x00\x12\x00\x00\x00\x01\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x00\x00\x01\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x0e\x00\x00\x00\x01\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x11\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x00\x00\x01\x00\x06\x00\x06\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x00\x00\x01\x00\x00\x00\x01\x00\x11\x00\x02\x00\x0c\x00\x03\x00\x02\x00\x06\x00\x13\x00\x15\x00\x00\x00\x01\x00\x08\x00\x14\x00\x17\x00\x19\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x12\x00\x18\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x1e\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x00\x00\x01\x00\x06\x00\x01\x00\x16\x00\x1a\x00\x1c\x00\x00\x00\x01\x00\x04\x00\x1b\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x03\x00\x0d\x00\x03\x00\x00\x00\x01\x00\x03\x00\x00\x00\x01\x00\x0d\x00\x10\x00\x17\x00\x1e\x00\x10\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x0c\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2c\x00\x2d\x00\x2e\x00\x01\x00\x07\x00\x0f\x00\x18\x00\x01\x00\x2f\x00\x0a\x00\x16\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x07\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x12\x00\x13\x00\x14\x00\x15\x00\x12\x00\x13\x00\x14\x00\x15\x00\x1e\x00\x1f\x00\x01\x00\x03\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x1e\x00\x1f\x00\x1c\x00\x1d\x00\x04\x00\x30\x00\x05\x00\x2d\x00\x00\x00\x01\x00\x06\x00\x2d\x00\x13\x00\x14\x00\x15\x00\x2d\x00\x00\x00\x01\x00\x08\x00\x0a\x00\x00\x00\x01\x00\x09\x00\x1e\x00\x1f\x00\x02\x00\x1c\x00\x1d\x00\x00\x00\x01\x00\x0b\x00\x00\x00\x01\x00\xff\xff\x00\x00\x01\x00\x1c\x00\x1d\x00\x2d\x00\xff\xff\x11\x00\x12\x00\xff\xff\xff\xff\x1c\x00\x1d\x00\x1a\x00\x1b\x00\x1c\x00\x02\x00\xff\xff\x02\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1a\x00\x1b\x00\x1c\x00\x1a\x00\x1b\x00\x1c\x00\x02\x00\x11\x00\x12\x00\x11\x00\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x02\x00\x02\x00\xff\xff\x02\x00\x02\x00\xff\xff\x02\x00\x08\x00\xff\xff\x0b\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x12\x00\x11\x00\x12\x00\x12\x00\x11\x00\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x34\x00\x09\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x57\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x13\x00\x51\x00\x03\x00\x04\x00\x34\x00\x09\x00\x14\x00\x15\x00\x2f\x00\x03\x00\x30\x00\xff\xff\x16\x00\x17\x00\x05\x00\x06\x00\x16\x00\x34\x00\x09\x00\x58\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x23\x00\x59\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x34\x00\x09\x00\x6d\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x08\x00\x34\x00\x09\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x03\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x33\x00\x34\x00\x09\x00\x32\x00\x31\x00\x69\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x6a\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x87\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x34\x00\x09\x00\x7a\x00\x47\x00\x03\x00\x4c\x00\x68\x00\x4d\x00\x13\x00\xdc\xff\x25\x00\x27\x00\x34\x00\x09\x00\x14\x00\x26\x00\x29\x00\x2a\x00\xdc\xff\xdc\xff\x03\x00\x16\x00\x34\x00\x09\x00\x16\x00\x5b\x00\x7b\x00\x7c\x00\x7d\x00\x49\x00\x4a\x00\x34\x00\x09\x00\xdc\xff\x6b\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x6c\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x6e\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x6f\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x34\x00\x09\x00\xdd\xff\x4e\x00\x28\x00\x2b\x00\x2d\x00\x34\x00\x09\x00\x53\x00\x2c\x00\xdd\xff\xdd\xff\x34\x00\x09\x00\x7a\x00\x57\x00\x76\x00\x34\x00\x09\x00\x75\x00\x34\x00\x09\x00\x69\x00\x82\x00\x4f\x00\xdd\xff\x84\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x5b\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x70\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x71\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x72\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x73\x00\x40\x00\x41\x00\x42\x00\x43\x00\x5c\x00\x42\x00\x43\x00\x09\x00\x57\x00\x80\x00\x5b\x00\x09\x00\x23\x00\x85\x00\x4d\x00\x09\x00\x08\x00\x18\x00\x53\x00\x47\x00\x17\x00\x66\x00\x61\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x76\x00\x0c\x00\x0d\x00\x0e\x00\x82\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x09\x00\x65\x00\x0f\x00\x10\x00\x53\x00\x47\x00\x0f\x00\x10\x00\x54\x00\x55\x00\x64\x00\x7e\x00\x63\x00\x11\x00\x53\x00\x47\x00\x62\x00\x11\x00\x33\x00\x0d\x00\x0e\x00\x11\x00\x53\x00\x47\x00\x60\x00\x5e\x00\x46\x00\x47\x00\x5f\x00\x0f\x00\x10\x00\x4c\x00\x54\x00\x77\x00\x7a\x00\x47\x00\x5d\x00\x46\x00\x47\x00\x00\x00\x46\x00\x47\x00\x54\x00\x80\x00\x11\x00\x00\x00\x03\x00\x16\x00\x00\x00\x00\x00\x54\x00\x86\x00\x48\x00\x49\x00\x4a\x00\x45\x00\x00\x00\x4c\x00\x85\x00\x7c\x00\x7d\x00\x49\x00\x4a\x00\x51\x00\x49\x00\x4a\x00\x78\x00\x49\x00\x4a\x00\x45\x00\x03\x00\x16\x00\x03\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x03\x00\x16\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x5b\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x45\x00\x13\x00\x00\x00\x45\x00\x13\x00\x00\x00\x4c\x00\x14\x00\x00\x00\x46\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x16\x00\x16\x00\x03\x00\x16\x00\x16\x00\x03\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 91) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91)
	]

happy_n_terms = 31 :: Int
happy_n_nonterms = 49 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_UIdent happy_var_1)) -> 
	happyIn4
		 (UIdent (happy_var_1)
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_LIdent happy_var_1)) -> 
	happyIn5
		 (LIdent (happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameA happy_var_1)) -> 
	happyIn6
		 (InfixFunctionNameA (happy_var_1)
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameB happy_var_1)) -> 
	happyIn7
		 (InfixFunctionNameB (happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameC happy_var_1)) -> 
	happyIn8
		 (InfixFunctionNameC (happy_var_1)
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameD happy_var_1)) -> 
	happyIn9
		 (InfixFunctionNameD (happy_var_1)
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameE happy_var_1)) -> 
	happyIn10
		 (InfixFunctionNameE (happy_var_1)
	)}

happyReduce_8 = happySpecReduce_1  7# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameF happy_var_1)) -> 
	happyIn11
		 (InfixFunctionNameF (happy_var_1)
	)}

happyReduce_9 = happySpecReduce_1  8# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameG happy_var_1)) -> 
	happyIn12
		 (InfixFunctionNameG (happy_var_1)
	)}

happyReduce_10 = happySpecReduce_1  9# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameH happy_var_1)) -> 
	happyIn13
		 (InfixFunctionNameH (happy_var_1)
	)}

happyReduce_11 = happySpecReduce_1  10# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameI happy_var_1)) -> 
	happyIn14
		 (InfixFunctionNameI (happy_var_1)
	)}

happyReduce_12 = happySpecReduce_1  11# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameJ happy_var_1)) -> 
	happyIn15
		 (InfixFunctionNameJ (happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  12# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_InfixFunctionNameK happy_var_1)) -> 
	happyIn16
		 (InfixFunctionNameK (happy_var_1)
	)}

happyReduce_14 = happySpecReduce_3  13# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn17
		 (Parser.AbsClbla.Prog (reverse happy_var_1) (reverse happy_var_2) happy_var_3
	)}}}

happyReduce_15 = happySpecReduce_0  14# happyReduction_15
happyReduction_15  =  happyIn18
		 ([]
	)

happyReduce_16 = happySpecReduce_3  14# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut19 happy_x_2 of { (HappyWrap19 happy_var_2) -> 
	happyIn18
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_17 = happySpecReduce_3  15# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	happyIn19
		 (Parser.AbsClbla.Ext happy_var_2
	)}

happyReduce_18 = happySpecReduce_0  16# happyReduction_18
happyReduction_18  =  happyIn20
		 ([]
	)

happyReduce_19 = happySpecReduce_3  16# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	case happyOut21 happy_x_2 of { (HappyWrap21 happy_var_2) -> 
	happyIn20
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_20 = happySpecReduce_2  17# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	happyIn21
		 (Parser.AbsClbla.Imp happy_var_2
	)}

happyReduce_21 = happySpecReduce_1  18# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn22
		 (Parser.AbsClbla.Env happy_var_1
	)}

happyReduce_22 = happySpecReduce_0  19# happyReduction_22
happyReduction_22  =  happyIn23
		 ([]
	)

happyReduce_23 = happySpecReduce_1  19# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn23
		 ((:[]) happy_var_1
	)}

happyReduce_24 = happySpecReduce_3  19# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	happyIn23
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_25 = happySpecReduce_1  20# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn24
		 (Parser.AbsClbla.InstructionTypeDefinition happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  20# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn24
		 (Parser.AbsClbla.InstructionFunctionDeclaration happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  20# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn24
		 (Parser.AbsClbla.InstructionFunctionDefinition happy_var_1
	)}

happyReduce_28 = happyReduce 5# 21# happyReduction_28
happyReduction_28 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	case happyOut28 happy_x_5 of { (HappyWrap28 happy_var_5) -> 
	happyIn25
		 (Parser.AbsClbla.TDef happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_29 = happySpecReduce_0  22# happyReduction_29
happyReduction_29  =  happyIn26
		 ([]
	)

happyReduce_30 = happySpecReduce_2  22# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut27 happy_x_2 of { (HappyWrap27 happy_var_2) -> 
	happyIn26
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_31 = happySpecReduce_1  23# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn27
		 (Parser.AbsClbla.ParamVar happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  24# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn28
		 ((:[]) happy_var_1
	)}

happyReduce_33 = happySpecReduce_3  24# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn28
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_34 = happySpecReduce_2  25# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	happyIn29
		 (Parser.AbsClbla.TConst happy_var_1 happy_var_2
	)}}

happyReduce_35 = happySpecReduce_1  25# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn29
		 (Parser.AbsClbla.TConstEmpty happy_var_1
	)}

happyReduce_36 = happySpecReduce_3  25# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut33 happy_x_3 of { (HappyWrap33 happy_var_3) -> 
	happyIn29
		 (Parser.AbsClbla.TInfixConst happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_37 = happySpecReduce_1  26# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  26# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn30
		 (Parser.AbsClbla.TFun happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_1  27# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_40 = happySpecReduce_2  27# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	happyIn31
		 (Parser.AbsClbla.TNamed happy_var_1 happy_var_2
	)}}

happyReduce_41 = happySpecReduce_3  28# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { (HappyWrap30 happy_var_2) -> 
	happyIn32
		 (happy_var_2
	)}

happyReduce_42 = happySpecReduce_1  28# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn32
		 (Parser.AbsClbla.TNamedNoParam happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  28# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn32
		 (Parser.AbsClbla.TVar happy_var_1
	)}

happyReduce_44 = happySpecReduce_1  29# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn33
		 ((:[]) happy_var_1
	)}

happyReduce_45 = happySpecReduce_2  29# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
	happyIn33
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_46 = happySpecReduce_3  30# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn34
		 (Parser.AbsClbla.FDecl happy_var_1 happy_var_3
	)}}

happyReduce_47 = happySpecReduce_3  31# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut36 happy_x_3 of { (HappyWrap36 happy_var_3) -> 
	happyIn35
		 (Parser.AbsClbla.FDef happy_var_1 happy_var_3
	)}}

happyReduce_48 = happyReduce 7# 31# happyReduction_48
happyReduction_48 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut36 happy_x_3 of { (HappyWrap36 happy_var_3) -> 
	case happyOut22 happy_x_6 of { (HappyWrap22 happy_var_6) -> 
	happyIn35
		 (Parser.AbsClbla.FDefWhere happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_1  32# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_50 = happySpecReduce_3  32# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut6 happy_x_2 of { (HappyWrap6 happy_var_2) -> 
	case happyOut36 happy_x_3 of { (HappyWrap36 happy_var_3) -> 
	happyIn36
		 (Parser.AbsClbla.EOpA happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_51 = happySpecReduce_1  33# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn37
		 (happy_var_1
	)}

happyReduce_52 = happySpecReduce_3  33# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn37
		 (Parser.AbsClbla.EOpB happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_53 = happyReduce 6# 33# happyReduction_53
happyReduction_53 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	case happyOut37 happy_x_6 of { (HappyWrap37 happy_var_6) -> 
	happyIn37
		 (Parser.AbsClbla.ELet happy_var_3 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_54 = happySpecReduce_1  34# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_55 = happySpecReduce_3  34# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	case happyOut8 happy_x_2 of { (HappyWrap8 happy_var_2) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 (Parser.AbsClbla.EOpC happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_56 = happySpecReduce_1  35# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_3  35# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut39 happy_x_3 of { (HappyWrap39 happy_var_3) -> 
	happyIn39
		 (Parser.AbsClbla.EOpD happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_58 = happySpecReduce_1  36# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  36# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	case happyOut40 happy_x_3 of { (HappyWrap40 happy_var_3) -> 
	happyIn40
		 (Parser.AbsClbla.EOpE happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_60 = happySpecReduce_1  37# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  37# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut11 happy_x_2 of { (HappyWrap11 happy_var_2) -> 
	case happyOut41 happy_x_3 of { (HappyWrap41 happy_var_3) -> 
	happyIn41
		 (Parser.AbsClbla.EOpF happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_62 = happySpecReduce_1  38# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  38# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	happyIn42
		 (Parser.AbsClbla.EOpG happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_64 = happySpecReduce_1  39# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  39# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 (Parser.AbsClbla.EOpH happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_66 = happySpecReduce_1  40# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_67 = happySpecReduce_3  40# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn44
		 (Parser.AbsClbla.EOpI happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_68 = happySpecReduce_1  41# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  41# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	case happyOut45 happy_x_3 of { (HappyWrap45 happy_var_3) -> 
	happyIn45
		 (Parser.AbsClbla.EOpJ happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_70 = happySpecReduce_1  42# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_71 = happySpecReduce_3  42# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut16 happy_x_2 of { (HappyWrap16 happy_var_2) -> 
	case happyOut46 happy_x_3 of { (HappyWrap46 happy_var_3) -> 
	happyIn46
		 (Parser.AbsClbla.EOpK happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_72 = happySpecReduce_1  43# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_73 = happySpecReduce_2  43# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn47
		 (Parser.AbsClbla.EApp happy_var_1 happy_var_2
	)}}

happyReduce_74 = happySpecReduce_3  44# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	happyIn48
		 (happy_var_2
	)}

happyReduce_75 = happySpecReduce_1  44# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn48
		 (Parser.AbsClbla.EVar happy_var_1
	)}

happyReduce_76 = happySpecReduce_1  45# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn49
		 (Parser.AbsClbla.FBName happy_var_1
	)}

happyReduce_77 = happySpecReduce_3  45# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	happyIn49
		 (Parser.AbsClbla.FIBName happy_var_2
	)}

happyReduce_78 = happySpecReduce_1  46# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn50
		 (Parser.AbsClbla.FName happy_var_1
	)}

happyReduce_79 = happySpecReduce_1  46# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn50
		 (Parser.AbsClbla.FTCName happy_var_1
	)}

happyReduce_80 = happySpecReduce_3  46# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	happyIn50
		 (Parser.AbsClbla.FITCName happy_var_2
	)}

happyReduce_81 = happySpecReduce_1  47# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameA happy_var_1
	)}

happyReduce_82 = happySpecReduce_1  47# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameB happy_var_1
	)}

happyReduce_83 = happySpecReduce_1  47# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameC happy_var_1
	)}

happyReduce_84 = happySpecReduce_1  47# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameD happy_var_1
	)}

happyReduce_85 = happySpecReduce_1  47# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameE happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  47# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameG happy_var_1
	)}

happyReduce_87 = happySpecReduce_1  47# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameH happy_var_1
	)}

happyReduce_88 = happySpecReduce_1  47# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameI happy_var_1
	)}

happyReduce_89 = happySpecReduce_1  47# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameJ happy_var_1
	)}

happyReduce_90 = happySpecReduce_1  47# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn51
		 (Parser.AbsClbla.FunctionInfixNameInfixFunctionNameK happy_var_1
	)}

happyReduce_91 = happySpecReduce_1  48# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn52
		 (Parser.AbsClbla.TIConstName happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 30# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (T_UIdent happy_dollar_dollar) -> cont 17#;
	PT _ (T_LIdent happy_dollar_dollar) -> cont 18#;
	PT _ (T_InfixFunctionNameA happy_dollar_dollar) -> cont 19#;
	PT _ (T_InfixFunctionNameB happy_dollar_dollar) -> cont 20#;
	PT _ (T_InfixFunctionNameC happy_dollar_dollar) -> cont 21#;
	PT _ (T_InfixFunctionNameD happy_dollar_dollar) -> cont 22#;
	PT _ (T_InfixFunctionNameE happy_dollar_dollar) -> cont 23#;
	PT _ (T_InfixFunctionNameF happy_dollar_dollar) -> cont 24#;
	PT _ (T_InfixFunctionNameG happy_dollar_dollar) -> cont 25#;
	PT _ (T_InfixFunctionNameH happy_dollar_dollar) -> cont 26#;
	PT _ (T_InfixFunctionNameI happy_dollar_dollar) -> cont 27#;
	PT _ (T_InfixFunctionNameJ happy_dollar_dollar) -> cont 28#;
	PT _ (T_InfixFunctionNameK happy_dollar_dollar) -> cont 29#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 30# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgramme tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap17 x') = happyOut17 x} in x'))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8371_0/ghc_2.h" #-}
































































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
