# x86-instructions

This is broken up into 3 libraries internally:

* Default public library. The modules `X86.Unlabeled` and
  `X86.Operator` have types and functions that do not deal
  with labels.
* `labeled`. The module `X86.Labeled` has types `Operand` and
  `Instruction`. Some of the data constructors of `Operand` reference
  labels.
* `encode`. Functions for encoding instructions. This difference
  between this and the `labeled` library is that here the indefinite
  module `X86.Label` requires that it be possible to encode the label.

This library is designed for generating x86-64 code, not for disassembly.
The `Operator` type in `src/X86/Unlabed.hs` is nothing clever. Every
variant of each instruction gets its own data constructor.
