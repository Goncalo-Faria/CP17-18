module H2 where

import Cp
import BMP
import Nat
import Codec.Picture.Types
import Graphics.Gloss
import Data.List

data QTree a = Cell a Int Int | Block (QTree a) (QTree a) (QTree a) (QTree a) deriving(Eq,Show)


------------------------ Funções auxiliares ---------------------------------------------------------------------------------

uncr3 f (a,(b,c)) = f a b c
cr3 f a b c = f (a,(b,c))

uncr4 f (a,(b,(c,d))) = f a b c d
cr4 f a b c d = f (a,(b,(c,d)))

----------------------- Funções principais ----------------------------------------------------------------------------------

inQTree = either (uncr3 Cell) (uncr4 Block)
outQTree (Cell a b c) = cr3 i1 a b c
outQTree (Block a b c d) = cr4 i2 a b c d
baseQTree f g = (f >< id) -|- (g >< (g >< (g >< g)))
recQTree = baseQTree id 
cataQTree g = g . recQTree (cataQTree g) . outQTree
anaQTree g = inQTree . recQTree (anaQTree g) . g
hyloQTree h g = cataQTree h . anaQTree g

instance Functor QTree where
    fmap f = cataQTree (inQTree . baseQTree f id)

rotateQTree = cataQTree (inQTree . ((id >< swap) -|- mySwap)) where mySwap (a,(b,(c,d))) = (c,(a,(d,b)))
scaleQTree n = cataQTree (inQTree . (scl n -|- id)) where scl n = id >< ((n*) >< (n*))

invertQTree :: QTree PixelRGBA8 -> QTree PixelRGBA8
invertQTree = fmap inv 
    where inv (PixelRGBA8 a b c d) = let minus = (255-) in PixelRGBA8 (minus a) (minus b) (minus c) d
-------
--anaQTree

compressQTree = flip ( cataNat.uncurry either. split const (const (anaQTree gene)))

gene = either i1 verifica . outQTree

verifica = cond evCell (i1 . merge .(id><(p2.p2))) i2

evCell (a,(b,(c,d))) = all isCell [a,b,c,d]

merge ((Cell a1 b1 c1),(Cell a2 b2 c2)) = (a1,((b1+b2),(c1+c2)))

isCell Cell{} = True
isCell _ = False

outlineQTree = undefined

y = Block
 (Cell 0 4 4) (Block
  (Cell 0 2 2) (Cell 0 2 2) (Cell 1 2 2) (Block
   (Cell 1 1 1) (Cell 0 1 1) (Cell 0 1 1) (Cell 0 1 1)))
 (Cell 1 4 4)
 (Block
  (Cell 1 2 2) (Cell 0 2 2) (Cell 0 2 2) (Block
   (Cell 0 1 1) (Cell 0 1 1) (Cell 0 1 1) (Cell 1 1 1)))