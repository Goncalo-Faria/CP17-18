module Q5 where 
    
import Cp
import Probability
import List
import BTree
import Nat

data Bag a = B [(a,Int)] deriving Show

singletonbag = B. singl . split id (const 1)

muB = B. concatMap gene . unB
    where gene = let special f = map (id >< f) . unB
                 in Cp.ap . swap .(id >< special.(*))
                 
instance Functor Bag where 
    fmap f = B . map (f >< id) . unB

instance Monad Bag where
x >= f = (muB . fmap f) x 
        where return = singletonbag

unB (B l) = l