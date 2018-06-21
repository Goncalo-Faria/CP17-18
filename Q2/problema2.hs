module H2 where

import Cp
import BMP
import Nat
import Codec.Picture.Types
import Graphics.Gloss
import Data.List
import Data.Matrix

data QTree a = Cell a Int Int | Block (QTree a) (QTree a) (QTree a) (QTree a) deriving(Eq,Show)

invertQTree :: QTree PixelRGBA8 -> QTree PixelRGBA8
------------------------ Funções auxiliares ---------------------------------------------------------------------------------

uncr3 f (a,(b,c)) = f a b c
cr3 f a b c = f (a,(b,c))

uncr4 f (a,(b,(c,d))) = f a b c d
cr4 f a b c d = f (a,(b,(c,d)))

operate g = g >< (g >< (g >< g))

----------------------- Funções principais ----------------------------------------------------------------------------------

inQTree = either (uncr3 Cell) (uncr4 Block)
outQTree (Cell a b c) = cr3 i1 a b c
outQTree (Block a b c d) = cr4 i2 a b c d
baseQTree f g = (f >< id) -|- operate g
recQTree = baseQTree id 
cataQTree g = g . recQTree (cataQTree g) . outQTree
anaQTree g = inQTree . recQTree (anaQTree g) . g
hyloQTree h g = h . recQTree ( hyloQTree h g ) . g

instance Functor QTree where
    fmap f = cataQTree (inQTree . baseQTree f id)
----------------------- Questão A -------------------------------------------------------------------------------------------
rotateQTree = cataQTree (inQTree . ((id >< swap) -|- mySwap)) where mySwap (a,(b,(c,d))) = (c,(a,(d,b)))
----------------------- Questão B -------------------------------------------------------------------------------------------
scaleQTree n = cataQTree (inQTree . (scl n -|- id)) where scl n = id >< ((n*) >< (n*))
----------------------- Questão C -------------------------------------------------------------------------------------------
invertQTree = let inv (PixelRGBA8 a b c d) = PixelRGBA8 (255-a) (255-b) (255-c) d
              in fmap inv 
----------------------- Questão D -------------------------------------------------------------------------------------------
compressQTree = flip (cataNat.uncurry either. split const (const compressUnit)) where

                        compressUnit = let g = cond evCell (i1 . mjoin .(id><(p2.p2))) i2
                                       in anaQTree (either i1 g . outQTree) 
                        mjoin (Cell a1 b1 c1,Cell a2 b2 c2) = (a1,(b1+b2,c1+c2))
                        evCell x    = let (a,(b,(c,d))) = operate isCell x 
                                      in a && b && c && d 
                                            where isCell Cell{} = True
                                                  isCell _ = False
                                                  
-- bothQTree g == cataQTree (inQTree . either i1 g) == andQTree (either i1 g . outQTree) 

----------------------- Questão E -------------------------------------------------------------------------------------------
outlineQTree f = uncurry (elementwise (curry (cond p1 p2 p1))).
                                cataQTree (either (baser f) mulkernel) where

            baser f (k,(i,j)) = (matrix j i false , matrix j i (const (f k)))
            mulkernel = (engage border >< engage id) .
                                                split (operate p1) (operate p2)      
            engage f x = let (a,(b,(c,d))) = operate f x 
                         in (a <|> b) <-> (c <|> d)
            border = let drawQ funct f par = uncurry (funct f) .
                                split par (funct f 1)
                     in drawQ mapCol (const true) ncols . 
                                drawQ mapRow (const true) nrows
