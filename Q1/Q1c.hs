
import Q1
import BTree
import List
import Cp
import Data.List

isValidMagicNr = (uncurry (==)) . (split (length.group) length) .
                        qSort . ( cataBlockchain (either (singl . p1) (cons. (p1 >< id)) )) 



groupy []            = [[]] 
groupy (h:t)| x == h = let (y:ys) = groupy x t in (h:y):ys
            | x != h = let l = groupy x t in h:l
