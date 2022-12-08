{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language KindSignatures #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}

module X86.Encode.Nasm
  ( instruction
  ) where

import Data.Builder.Catenable.Text (Builder,pattern (:<))
import Data.Int (Int32)
import Topaz.Rec (Rec(RecCons,RecNil))
import X86.Label (Label)
import X86.Labeled (Instruction(..),Operand(..))
import X86.Unlabeled (Scaled(..))

import qualified Data.Builder.Catenable.Text as Builder
import qualified Data.Text.Short as TS
import qualified Topaz.Rec as Rec
import qualified X86.Label as Label
import qualified X86.Operator as Operator

-- encodeIntelVoid :: Instruction Void -> Builder
-- encodeIntelVoid = encodeIntel Data.Void.absurd
-- 
-- encodeIntelShortText :: Instruction ShortText -> Builder
-- encodeIntelShortText = encodeIntel (\t -> t :< Builder.Empty)

instruction :: Instruction -> Builder
instruction (Instruction op args0) =
  Operator.encode op 
  :<
  ( case args0 of
      RecNil -> Builder.Empty
      RecCons arg0 args -> " "
        <> encodeIntelOperand elemSz simdSz arg0
        <> Rec.foldMap (\x -> encodeIntelSuccessorOperand elemSz simdSz x) args
  )
  where
  elemSz = Operator.elementWidth op
  elemCount = Operator.elementCount op
  simdSz = elemSz + elemCount

-- register64 :: Operand 'Reg -> Builder
-- register64 (Reg_ w) = TS.pack ('r' : show w) :< Builder.Empty

encodeIntelOperand :: Word -> Word -> Operand arg -> Builder
encodeIntelOperand elemSz simdSz op = case op of
  Reg_ w -> TS.pack ('r' : shows w (sizeToString elemSz)) :< Builder.Empty
  Simd_ w -> TS.pack (simdSizeToChar simdSz : 'm' : 'm' : show w) :< Builder.Empty
  Imm8 i -> TS.pack (show i) :< Builder.Empty
  Imm16 i -> TS.pack (show i) :< Builder.Empty
  Imm32 i -> TS.pack (show i) :< Builder.Empty
  Imm64 i -> TS.pack (show i) :< Builder.Empty
  Rel8 label -> Label.encode label
  Rel16 label -> Label.encode label
  Rel32 label -> Label.encode label

encodeIntelSuccessorOperand :: Word -> Word -> Operand arg -> Builder
encodeIntelSuccessorOperand elemSz simdSz op = case op of
  Reg_ w -> TS.pack (',' : ' ' : 'r' : shows w (sizeToString elemSz)) :< Builder.Empty
  Simd_ w -> TS.pack (',' : ' ' : simdSizeToChar simdSz : 'm' : 'm' : show w) :< Builder.Empty
  Imm8 i -> TS.pack (',' : ' ' : show i) :< Builder.Empty
  Imm16 i -> TS.pack (',' : ' ' : show i) :< Builder.Empty
  Imm32 i -> TS.pack (',' : ' ' : show i) :< Builder.Empty
  Imm64 i -> TS.pack (',' : ' ' : show i) :< Builder.Empty
  Mask_ k -> TS.pack ('{' : 'k' : shows k "}") :< Builder.Empty
  Merging -> mempty :< Builder.Empty
  Zeroing -> "{z}" :< Builder.Empty
  Address64 disp base scaled -> TS.pack (',' : ' ' : encodeAddress64 disp base scaled simdSz) :< Builder.Empty

encodeAddress64 :: Int32 -> Word -> Scaled -> Word -> [Char]
encodeAddress64 !disp !base scaled sz = case disp of
  0 -> case scaled of
    ScaledNone -> encodePtrSize sz ++ " [r" ++ show base ++ "]"
    ScaledSome reg mult -> case mult of
      0 -> encodePtrSize sz ++ " [r" ++ show base ++ " + r" ++ show reg ++ "]"
      _ -> encodePtrSize sz ++ " [r" ++ show base ++ " + " ++ show ((2 :: Word) ^ mult) ++ "*r" ++ show reg ++ "]"
  _ -> error "encodeAddress64: write this part"


encodePtrSize :: Word -> [Char]
encodePtrSize = \case
  0 -> "BYTE"
  1 -> "WORD"
  2 -> "DWORD"
  3 -> "QWORD"
  4 -> "OWORD"
  5 -> "YWORD"
  6 -> "ZWORD"

sizeToString :: Word -> String
sizeToString = \case
  0 -> "b"
  1 -> "w"
  2 -> "d"
  _ -> ""

simdSizeToChar :: Word -> Char
simdSizeToChar = \case
  4 -> 'x'
  5 -> 'y'
  6 -> 'z'
