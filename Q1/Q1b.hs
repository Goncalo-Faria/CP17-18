import Q1
import Q1a
import Cp
import List
import BTree

--- (A)  JOINING METHOD

-- Desc:
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

partit p = cataList (either (const (0,([],[]))) (segv p))
    where segv p (h,(e,(s,l))) | p < p1 h     = (e,(h:s,l))
                               | p == p1 h    = ( p2 h + e ,(s,l))
                               | otherwise    = ( e ,(s,h:l))   
