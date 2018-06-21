-- (c) MP-I (1998/9-2006/7) and CP (2005/6-2018/19)

module Main(main) where


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
inPTree (Right (x,(a,b)) ) = Comp x  a b

outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outPTree (Unit x) = i1 x
outPTree (Comp x  a b) = i2 (x,(a,b)) 


-- (2) Ana + cata + hylo -------------------------------------------------------

--recPTree g = id -|- (id >< (g >< g))

inFTree (Left x) = Unit x
inFTree (Right (x,(a,b)) ) = Comp x a b

--outPTree :: PTree -> Either Square (Square , (PTree ,PTree))
outFTree (Unit x) = i1 x
outFTree (Comp x  a b) = i2 (x,(a,b))

recFTree = baseFTree id id

cataFTree g = g . recFTree (cataFTree g) . outFTree

anaFTree g = inFTree . recFTree (anaFTree g) . g

hyloFTree h g = h . recFTree ( hyloFTree h g ) . g

baseFTree f o g = o -|- (f >< (g >< g))

--Funcoes auxiliares---

instance BiFunctor FTree where
    bmap f g =  cataFTree ( inFTree . baseFTree f g id)

generatePTree =  modifyPTree . anaFTree gene . toInteger
        where gene = ( zero -|- split succ (split id id) ) . outNat
              
              modifyPTree = ap.( (ap.(ap><id).assocl) >< id ) .
                                        (split (const bmap) (dup.submax) >< id ) . dup

              submax x = let valor  = either id p1 . outFTree  
                         in  uncurry (**) . 
                                    split (const (sqrt 2 / 2)) ( fromIntegral . uncurry (-) .
                                                                        split (const (valor x)) id )

drawPTree = cataFTree gene
    where   gene = either (singl . square) 
                                ( cons . ( square >< conc ) . engage)

            engage = let pad x y = fmap . uncurry (.) .
                                split (uncurry translate . split x id) (const (rotate (y 45)))
                     in split p1 ((uncurry (pad (negate.(/2)) negate) >< uncurry (pad (/2) id)) .
                                                                                     split (id><p1) (id><p2))



--submax :: PTree -> Square -> Picture
--submax x = getRect . uncurry (**) . split (const golden) ( uncurry (-) . split (const (valor x)) id )
  --          where golden = sqrt 2 /2
            
getRect :: Square -> Picture
getRect = uncurry rectangleSolid . dup 


window = (InWindow "CP" (800,800) (0,0))
square s = rectangleSolid s s

main = animatePTree 20

animatePTree :: Integer -> IO ()
animatePTree n = animate window white draw
    where
    pics = pictures $ drawPTree (bmap (80*) (80*) (generatePTree n) )
    draw t = pics 

-- B(X,Y) = (X) -|- (X) >< ((Y) >< (Y))
-- B(id,f)
-- (X) -|- (X) >< ([PICTURES] >< [PICTURES])

--      (cons . ( square >< (uncurry (++)) ) )

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








