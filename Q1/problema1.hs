module Q1 where

import Cp
import List
import BTree
import LTree

--- Definições elementares --------------------------------------------------------------------------------- 
type MagicNo = String
type Time = Int -- em milisegundos
type Entity = String
type Value = Int
--- Definições intermédias ---------------------------------------------------------------------------------
type Transaction = (Entity, (Value,Entity))

type Ledger = [(Entity, Value)]

type Transactions = [Transaction]

type Block = (MagicNo, (Time, Transactions))

data Blockchain = Bc { bc :: Block } | Bcs {bcs :: (Block, Blockchain) } deriving (Show,Eq)
--- Exemplos -----------------------------------------------------------------------------------------------
t1 = ("ent1",(100, "ent2"))
t2 = ("ent2",(50 , "ent1"))

bl1, bl2, bl3 :: Block
bl1 = ("b1", (1, []))
bl2 = ("b2", (2, [t1]))
bl3 = ("b3", (3, [t2]))

bs1, bs2 :: Blockchain
bs1 = Bcs (bl1, Bcs (bl2, Bc bl3))
bs2 = Bcs (bl3, Bcs (bl2, Bc bl2))
bs3 = Bc bl2
bs4 = Bcs (bl1, Bc bl3)

bloc =  ( " sffatd" , ( 4 , replicate 10 ("entity", (4 , "entity") ) ) )
--- Algebra and coAlgebra -----------------------------------------------------------------------------------
inBlockchain = either Bc Bcs

outBlockchain (Bc a) = i1 a
outBlockchain (Bcs (a,b)) = i2 (a,b)
--- Functor -------------------------------------------------------------------------------------------------
recBlockchain g = id -|- (id >< g )
-----Cata && Ana && Hylo ------------------------------------------------------------------------------------
cataBlockchain g = g . recBlockchain (cataBlockchain g) . outBlockchain    

anaBlockchain g = inBlockchain . recBlockchain (anaBlockchain g) . g

hyloBlockchain h g = cataBlockchain h . anaBlockchain g
--- Funções Auxiliares --------------------------------------------------------------------------------------
segment r f b p = cataList (either b (segv r f p))
    where segv r f p (h,(e,(s,l))) | p < r h      = (e,(h:s,l))
                                   | p > r h      = (e,(s,h:l))
                                   | otherwise    = f h e (s,l)
--- (A) -----------------------------------------------------------------------------------------------------
allTransactions =  cataBlockchain ( uncurry ccat . split (p2 . p2 . either id p1) (either nil p2 ))
--- (B) -----------------------------------------------------------------------------------------------------
    -- Cria uma lista com o log de transações 0(N). (tamnanho 2N).
    -- criar uma árvore com saldo. e ao fazer o partition soma o valor das transações -> 0(nLogn)
    -- de todos os elementos iguais. 
    -- faz preord transversal. 0(N) <- i think                
        
ledger = hyloBTree preord account . cataList changes . allTransactions

changes = either nil (cons . (id><cons) . assocr . 
                    (split ( swap . (((-1)*)><id) . p2 ) (id><p1) >< id ) )
    
account = either (i1.(!)) 
                    (i2 . condense . split p1 (uncurry partit . (p1><id))) .
                                    outList
    
condense ((el,num1),(num2,c)) = ((el,num1+num2),c)
    
partit = segment p1 
                (\ h e b -> (p2 h + e, b)) 
                        (const (0,([],[])))
--- (C) -----------------------------------------------------------------------------------------------------
isValidMagicNr = hyloLTree (either id and) eqSep .
                            cataBlockchain (either (singl . p1) (cons. (p1 >< id)))

eqSep = either (i1.true) 
            (cond ((False==).p1) (i1.p1) (i2.p2) . uncurry finders) 
                    . outList
      
finders = segment id 
                (const.const.const (False,([],[]))) 
                                (const (True,([],[])))