module Q1a where
import Q1
import Cp
import List
import BTree
 
-------------------------------------------------------------------------------------------------------------
allTransactions :: Blockchain -> Transactions
allTransactions =  cataBlockchain ( uncurry ccat . split (p2 . p2 . either id p1) (either nil p2 ))
-------------------------------------------------------------------------------------------------------------