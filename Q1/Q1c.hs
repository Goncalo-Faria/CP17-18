import Q1
import BTree
import List
import Cp
import Data.List
import LTree

isValidMagicNr = (hyloLTree (either id and) eqSep) .
                             (cataBlockchain (either (singl . p1) (cons. (p1 >< id)))) 

-- criar Leaf Tree
eqSep = (either (i1.true) ((cond ((False==).p2) (i1.p2) (i2.p1)).(uncurry finders))) . outList

finders p = cataList (either (const (([],[]),True)) (segment p))
    where segment p (h,((s,l),b)) | p < h     = ((h:s,l), b)
                                  | p > h     = ((s,h:l), b)
                                  | otherwise = (([],[]),False)