module X86.Label.ShortText
  ( Label
  , encode
  ) where

import Data.Builder.Catenable.Text (Builder(Empty,Cons))
import Data.Text.Short (ShortText)

type Label = ShortText

encode :: ShortText -> Builder
encode t = Cons t Empty
