-- (c) MP-I (1998/9-2006/7) and CP (2005/6-2018/19)

module PTree where

import Graphics.Gloss
import Cp
import List
import Data.Monoid
import Control.Applicative
import Nat


data FTree a b = Unit b | Comp a (FTree a b) (FTree a b) deriving (Eq , Show)
type PTree = FTree Square Square
type Square = Float


inPTree :: Either Square (Square , (PTree ,PTree)) -> PTree
inPTree (Left x) = Unit x
inPTree (Right (x,(a,b)) ) = (Comp x  a b)

outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outPTree (Unit x) = i1 x
outPTree (Comp x  a b) = i2 (x,(a,b)) 


-- (2) Ana + cata + hylo -------------------------------------------------------

--recPTree g = id -|- (id >< (g >< g))

inFTree (Left x) = Unit x
inFTree (Right (x,(a,b)) ) = (Comp x  a b)

--outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outFTree (Unit x) = i1 x
outFTree (Comp x  a b) = i2 (x,(a,b))

recFTree = baseFTree id id

cataFTree g = g . recFTree (cataFTree g) . outFTree

anaFTree g = inFTree . recFTree (anaFTree g) . g

hyloFTree h g = cataFTree h . anaFTree g

baseFTree f o g = o -|- (f >< (g >< g))

--Funoes auxiliares---


instance BiFunctor FTree where
    bmap f g =  cataFTree ( inFTree . baseFTree f g id)

generatePTree = anaFTree gene
            where gene = zero -|- split succ (split id id) . outNat

contaNodos :: PTree -> Integer
contaNodos = cataFTree gene 
				where gene = either (const 1) (add . (const 1 >< add) ) 

--print_image :: IO()
--print_image = display window white (rectangleSolid 2 2)

-- desenhar em fundo branco
window :: Display
window = InWindow "Gloss" (800,600) (0,0)
-- numa janela com 800x600 pixels

ex1 ::Picture
ex1 = rectangleSolid 10 10
--w um círculo cheio com 100 pixels de raio
ex2 ::Picture
ex2 = pictures [color red (circleSolid 100),color white (rectangleSolid 100 50)]

--drawPTree :: PTree -> [Picture]
--drawPTree (Unit x) = singl( uncurry rectangleSolid (my_dup x))
--drawPTree (Comp x a b) = uncurry (++) ((uncurry (++) (singl(rectangleSolid x x) , drawPTree a)), drawPTree b)

--drawPTree = cataFTree gene
--		where gene = either (singl . (uncurry rectangleSolid) . dup) (cons . ( (uncurry rectangleSolid) . dup >< concat ) )


valor :: PTree -> Square
valor = either id p1 . outFTree  

drawPTree = recolherPicTree . modifyPTree


recolherPicTree :: FTree Picture Picture -> [Picture]
recolherPicTree = cataFTree (either singl (cons . (id >< concat )))

modifyPTree :: PTree -> FTree Picture Picture
modifyPTree = (uncurry ( (uncurry bmap ) . (dup.submax) ) ) . dup 


-- modifyPTree = (uncurry ((flip bmap) id) ). (split submax id)


submax :: PTree -> Square -> Picture
submax x = getRect . ( uncurry (**) ) . ( split (const golden) ( (uncurry (-) ) . (split (const (valor x)) id) ) )
			where golden = (sqrt 2)/2
				
getRect :: Square -> Picture
getRect = uncurry rectangleSolid . dup 




-- B(X,Y) = (X) -|- (X) >< ((Y) >< (Y))
-- B(id,f)
-- (X) -|- (X) >< ([PICTURES] >< [PICTURES])

--      (cons . ( (uncurry rectangleSolid) . mydup >< (uncurry (++)) ) )

-- (3) Map ---------------------------------------------------------------------

--instance Functor (FTree x) where
--	 fmap f  = cataPTree ( inPTree . basePTree f id )



{-
number :: Int -> (Float , Float)
number x = (x , (sqrt 2)/x+1) 

--Conta numero de Nodos --------------------------------------------------------

contaNodos :: PTree -> Int 
contaNodos gene= cataPTree gene 
				where gene = either (const 1) (add . (const 1 >< add) )


--Soma numero de Nodos ---------------------------------------------------------
somaNodos :: PTree -> Int
somaNodos gene = cataPTree gene
				where gene = either (id) (add . (id >< add))

-}








