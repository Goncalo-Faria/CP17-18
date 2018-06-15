import Q1
import Q1a
import Cp
import List
import BTree

--- (A)  JOINING METHOD

-- Desc:
    -- Cria uma lista com o log de transações 0(N). (tamnanho 2N).
    -- criar uma árvore com saldo. e ao fazer o partition soma o valor das transações -> 0(2N)
    -- de todos os elementos iguais. 
    -- faz inord transversal. NlogN <- i think


    --
        -- depois percorre a lista (0)N
        -- e em cada iteração faz a contabilidade da transação na árvore. O(2 log2(k))
              -- onde k é o numero de elementos do conjunto formado pelos elementos da árvore
    -- ( 2N log2(k) )

------- Resultados teoricos 

-- segP pivot = [ < <nil ,nil >, const 0 > , magia (const pivot) ] . outList

-- r = (( [] , []),0)
-- segP pivot = [ < <nil ,nil >, const 0 > , magia (const pivot) ] . outList
-- magia (const pivot) = cont ( (<) . < p1 . (const pivot) , p1 > ) f secondif

--  firstif (const pivot) = cond ( (<) . < p1 . (const pivot) , p1 > ) f secondif
--  f = ( < < cons . (id >< p1.p1) , p2.p1.p2 > , p2.p2> )
--  secondif (const pivot) = cond ( (==) . < p1 . (const pivot) , p1 > ) g h

-- g = ( < p1.p2 , (uncurry (+)) . <  p2 . p1 ,p2.p2> > )
-- h = ( < < p1.p1.p2 , cons . (id >< p2.p1) > , p2.p2> )

-----

--- pointfree


--cataList (segP)
--pcond pivot = ( ( uncurry (==) ) . ( split (p1 . pivot) p1 ) )



--yo = segP ( const ("ula",4))

--uj = anaList yo
--( either (split ( split nil nil) (const 0) ) ( (uncurry anaList ) . ( (segP . const) >< id) ) ) . outList


-- partit = uncurry midtion 
--- midtion pivot = cataList ( segP (const pivot) )
-- segP pivot = either (split ( split nil nil) (const 0) ) (firstif pivot) 
    
-- firstif pivot = cond ( (uncurry (<)) . (split (p1.pivot) (p1.p1) ) ) op1 (secondif pivot)
-- secondif pivot = cond ( ( uncurry (==) ) . ( split (p1.pivot) (p1.p1) ) ) op2 op3
    
-- op1 = split (split (cons . ( id >< (p1.p1) ) ) (p2.p1.p2)) (p2.p2)
-- op2 = split (p1.p2) ( (uncurry (+)) . (split (p2.p1) (p2.p2)) )
-- op3 = split (split (p1.p1.p2) (cons . ( id >< (p2.p1) )) ) (p2.p2)


---------------------------------------------------------------------------------------

--- o segP separa e soma o valor das entidades iguais.

-- < p1 , swap . <p1 p2. segP > >                   
        
ledger = (either nil bind).outList.changes.allTransactions

changes = cataList (either nil (cons.(id><cons).assocr.(aux><id)))
    where aux = split (swap.(((-1)*)><id).p2) (id><p1)

bind = cons.(id><uncurry ccat).
    split (split (p1.p1) (add.( p2 ><p1 ))) (p2.p2)
            (split p1 (swap.partit))

partit p = cataList (either (const (([],[]),0)) (segv p))
      where segv p (h,((s,l),e))  | p1 p < p1 h  = ((h:s,l), e)
                                  | p1 p == p1 h = ((s,l), p2 h + e)
                                  | otherwise    = ((s,h:l), e)   

--(B) ACCOUNTING METHOD -------------------------------------------------------------------------------
-- TWICE FASTER.

----- origem-> valor ->destino.
----- type Transaction = (Entity, (Value,Entity))
----- type Ledger = [(Entity, Value)]


--- (1) Listar transações // Obter entidades // Obter a contabilidade destas

-- ledger :: Blockchain -> Ledger
-- ledger = undefined

-- sem repetidos


-- entityOnly = cataList either Empty (cons . (id >< cons) . assocr  . ( (id >< p2) >< id ) )

--qdupsep :: Ord t => [t] -> Either () ( (t , Int), ([t], [t]))
--qdupsep []    = Left ()
--qdupsep (h:t) = Right ((h,0),(s,l)) 
  --where (s,l) = p1 (party h t)

  -- qd = [ const () , < <p1, const 0 > , p1 . (uncurry party) > ] . outList
--ledger l = 
  --  let tree = getTree l


  -- Desc:
    -- cria a árvore com todos os elementos. (sem repetições) 0 (N)

    --
        -- depois percorre a lista (0)N
        -- e em cada iteração faz a contabilidade da transação na árvore. O(2 log2(k))
              -- onde k é o numero de elementos do conjunto formado pelos elementos da árvore
    -- ( 2N log2(k) )

party:: Ord a => (a , [a]) -> ( ([a], [a]) ,[a])
party (p , [])                = (([],[]),[])
party (p , (h:t)) | p < h     = let ((s,l),e) = party (p,t) in ((h:s,l),e)
                  | p == h    = let ((s,l),e) = party (p,t) in ((s,l),h:e)
                  | otherwise = let ((s,l),e) = party (p,t) in ((s,h:l),e)
  
--qd :: [Entity] -> Either ()  ( (Entity , Value) , ( [Entity] , [Entity] ) )
qd = ( (!) -|- ( split ( split p1 (const 0) ) (p1 . party  ) ) ) . outList
  
-- gatherEntity :: Transactions -> [ Entity ]
gatherEntity = cataList ( either nil ( cons . (id >< cons) . assocr . ( (id >< p2 ) >< id ) )  )
  

-- isto é um cata também.
addBTree :: (Ord a, Num t) => (a, t) -> BTree (a, t) -> BTree (a, t)
addBTree (a,val) Empty  = Empty
addBTree (a,val) ( Node ( (x,cval), (l , r))) | a == x = Node ((x,cval+val),(l,r))
                                              | a > x  = Node ((x,cval),(l,addBTree (a,val) r))
                                              | a < x  = Node ((x,cval),(addBTree (a,val) l,r))
  
--getTree :: Transactions -> BTree ( Entity , Value )
getTree = (anaBTree qd) . gatherEntity
  
--accounting :: (Num t, Ord b) => BTree (b, t) -> [(b, (t, b))] -> BTree (b, t)
accounting tree [] = tree
accounting tree (h:t) = accounting ( addBTree ( (p2.p2) h, (p1.p2) h) (addBTree ( p1 h , (-( (p1 . p2) h))) tree)) t
  
  -- fgene = ( addTree >< addTree ) . < swap . p2 , (id >< neg .p1) >
  -- fgene = ( addBTree >< addBTree ) . ( split (swap . p2) (id >< (neg.p1)) )
    
  -- ledger = ( uncurry (foldr fgene) ) . ( split getTree id ) . allTransactions
  
  -- neg = ((-1)*)
-- ledgerb :: Blockchain -> Ledger
ledgerb = inordt . (uncurry accounting) . ( split getTree id ) . allTransactions