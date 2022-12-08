{-# language BangPatterns #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (replicate)

import Data.Text.Short (ShortText)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))
import X86.Labeled
import X86.Unlabeled

import qualified Test.Tasty.HUnit as THU
import qualified Data.Builder.Catenable.Text as Builder
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified X86.Encode.Nasm as Nasm

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ THU.testCase "001-ADD-BYTE" $
    "ADD r2b, r5b"
    @=?
    enc (apply2 AddReg8Reg8 (Reg_ 2) (Reg_ 5))
  , THU.testCase "002-ADD-WORD" $
    "ADD r2w, r5w"
    @=?
    enc (apply2 AddReg16Reg16 (Reg_ 2) (Reg_ 5))
  , THU.testCase "003-VPADD-MASK-4x32-ZERO" $
    "VPADDD xmm3{k6}{z}, xmm11, xmm12"
    @=?
    enc (apply5 VpaddMask4x32Reg (Simd_ 3) (Mask_ 6) Zeroing (Simd_ 11) (Simd_ 12))
  , THU.testCase "004-VPADD-MASK-4x32-MERGE" $
    "VPADDD xmm3{k6}, xmm11, xmm12"
    @=?
    enc (apply5 VpaddMask4x32Reg (Simd_ 3) (Mask_ 6) Merging (Simd_ 11) (Simd_ 12))
  , THU.testCase "005-VPADD-2x64" $
    "VPADDD xmm3, xmm11, xmm12"
    @=?
    enc (apply3 Vpadd4x32Reg (Simd_ 3) (Simd_ 11) (Simd_ 12))
  , THU.testCase "006-VPADD-32x16" $
    "VPADDW zmm3, zmm11, zmm12"
    @=?
    enc (apply3 Vpadd32x16Reg (Simd_ 3) (Simd_ 11) (Simd_ 12))
  , THU.testCase "007-JZ-LABEL" $
    "JZ .target"
    @=?
    enc (apply1 Jz (Rel32 ".target"))
  , THU.testCase "008-MOV-QWORD" $
    "MOV r10, QWORD [r2 + 2*r3]"
    @=?
    enc (apply2 MovReg64Mem64 (Reg_ 10) (Address64 0 2 (ScaledSome 3 1)))
  , THU.testCase "009-MOV-DWORD" $
    "MOV r10d, DWORD [r2 + r3]"
    @=?
    enc (apply2 MovReg32Mem32 (Reg_ 10) (Address64 0 2 (ScaledSome 3 0)))
  , THU.testCase "010-MOV-BYTE" $
    "MOV r10b, BYTE [r15]"
    @=?
    enc (apply2 MovReg8Mem8 (Reg_ 10) (Address64 0 15 ScaledNone))
  ]

enc :: Instruction -> ShortText
enc i = case Ascii.toShortTextU $ Chunks.concatU $ Builder.run $ Nasm.instruction i of
  Nothing -> error "whoops: non-ascii output when encoding instruction"
  Just t -> t
