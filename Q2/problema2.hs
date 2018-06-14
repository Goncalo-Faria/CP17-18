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
recQTree g = baseQTree id g
cataQTree g = g . recQTree (cataQTree g) . outQTree
anaQTree g = inQTree . recQTree (anaQTree g) . g
hyloQTree h g = cataQTree h . anaQTree g

instance Functor QTree where
    fmap f = cataQTree (inQTree . baseQTree f id)

rotateQTree = cataQTree (inQTree . ((id >< swap) -|- mySwap)) where mySwap (a,(b,(c,d))) = (c,(a,(d,b)))
scaleQTree n = cataQTree (inQTree . ((scl n) -|- id)) 
    where scl n = id >< ((n*) >< (n*))

invertQTree :: QTree PixelRGBA8 -> QTree PixelRGBA8
invertQTree = fmap inv 
    where inv (PixelRGBA8 a b c d) = let minus = uncurry (-) . (split (const 255) id) in PixelRGBA8 (minus a) (minus b) (minus c) d

compressQTree = undefined
outlineQTree = undefined

y = Block
 (Cell 0 4 4) (Block
  (Cell 0 2 2) (Cell 0 2 2) (Cell 1 2 2) (Block
   (Cell 1 1 1) (Cell 0 1 1) (Cell 0 1 1) (Cell 0 1 1)))
 (Cell 1 4 4)
 (Block
  (Cell 1 2 2) (Cell 0 2 2) (Cell 0 2 2) (Block
   (Cell 0 1 1) (Cell 0 1 1) (Cell 0 1 1) (Cell 1 1 1)))


gene = either f g
    where f (a,b) = b*b  :: Int
          g (a,(b,(c,d))) = a+b+c+d :: Int


alturaQ = cataQTree lgen  
lgen = either (const 1) (succ . m . (id >< (m . (id >< m ))))
    where m = uncurry max 

gencmp n = either ( a ) ( b n )


stop  = split inQtree (split (const 0) (getV . p1) ) 

goon n (x1,(x2,(x3,x4))) = if ((equals (x1,(x2,(x3,x4)))) && (n == (p1.p2 x1)))
  
 --- a ::  ( Cell ) -> (Qtree, (0 , vector ) )
getV (PixelRGBA8 a b c d) = [a,b,c,d]
equals = (==1).length.group.cons.(p1.p2><cons.(p1.p2><cons.(p1.p2><singleton.p1.p2)))

