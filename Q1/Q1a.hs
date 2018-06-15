module Q1a where
import Q1
import Cp
import List
import BTree
 
--- [ bloco  ,   (bloco , r )]
--- Cons . [ swap . < id , const NIL > , id ]
---- Raciocinio 

-- 1 ) Obtêm apenas os blocos.

gBl :: Either Block ( Block , [ Block] ) -> [ Block ]
gBl = cons .  either ( split id nil  ) id 

listBl = cataBlockchain gBl

--- 2) faz map e obtêm a lista de Transações.

listlistTr = map (p2 . p2) 

--- 3) faz concat e obtèm uma unica lista de transações.

listTrans =  concat . listlistTr . listBl 

--- Implementação eficiente.


-- tGene = cons .  ( either ( split (p2 . p2) nil  ) ( split (p2 . p2 ) id ) )

-- cons . [ < p2. p2 , NIL > ,  ( p2.p2 >< id ) ]

---   lei da troca.
-- cons . <  p2 . p2 . [ id , p2 ] , [NIL , id ] >


--- ccat . < p2 . p2 . [id , p2 ] , [NIL , id ] >

tGene = uncurry ccat . split (p2 . p2 . either id p1 ) (either nil p2 ) 

getTrans = cataBlockchain tGene

-------------------------------------------------------------------------------------------------------------
allTransactions :: Blockchain -> Transactions
allTransactions =  cataBlockchain ( uncurry ccat . split (p2 . p2 . either id p1) (either nil p2 ))
-------------------------------------------------------------------------------------------------------------