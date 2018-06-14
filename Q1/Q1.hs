module Q1 where

import Cp
import List
import BTree

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
cataBlockchain g = g . ( recBlockchain ( cataBlockchain g) ) . outBlockchain    

anaBlockchain g = inBlockchain . ( recBlockchain (anaBlockchain g) ) . g

hyloBlockchain h g = cataBlockchain h . anaBlockchain g
-------------------------------------------------------------------------------------------------------------