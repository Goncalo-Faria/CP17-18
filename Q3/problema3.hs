import Nat
import Cp

--- <f , l > k = cata . ( [ const 1 , mul . ( l k  id ) ] ><  [  const (k+1)  , succ ] )

--yuy k = cataNat . ( ( either (const 1) (mul . ( (l k) >< id ) ) ) >< ( either (const (k+1) ) succ )) 
  --  where l k = succ . ( (uncurry (+) ) k) 



-- bot k = cataNat . ( either ( cons ( (1,1) , (1,succ k) ) )  (  (split id id) . ( split mul (succ . p2) ) ) )




-- (1) Defining f -------------------------------------------------------------------

-- pointfree
-- f :: (Enum t, Eq a, Num t, Num a) => t -> a -> t
-- f k n = a where
   -- (a , _) = aux k n
   -- aux k 0 = (1 , k + 1)
   -- aux k n = let (a,b) = aux k (n-1) in (b*a, succ b)

-- pointwisef

-- p1 . (| [  const . (1, k +1) , <mul, succ. p2 > ]  |)

f k = p1 . ( cataNat ( uva k ) )
    where uva k = either (const (1,succ k)) (split mul (succ.p2) )

-- (2) Defining l -------------------------------------------------------------------

-- pointwise
-- l :: (Eq a, Num a, Num a1) => a1 -> a -> a1
-- l k 0 = k+1
-- l k n = (l k (n-1)) + 1

-- point free
-- l k = cataNat ( ula k )
   -- where ula k = ( either (const (succ k) ) succ)

l k = p2 . ( cataNat ( uva k ) )
   where uva k = either (const (1,succ k)) (split mul (succ.p2) )

--(3) Defining < fk , lk > ----------------------------------------------------------

-- Como se pode ver  l k está defenido como a projeção da segunda componente e f k  da primeira.

-- logo 
spt_fg k = cataNat ( uva k )
    where uva k = either (const (1,succ k)) (split mul (succ.p2) )

-- (4) Defining g -----------------------------------------------------------------

--- pointwise
-- g 0 = 1
-- g d = d * g (d -1)

-- g = p1 . aux  
  --  where aux 0 = (1 , 1)
    --      aux d = let(a,b) = aux (d-1) in (a*b,succ b)

g = p1 . cataNat (either (const (1,1)) (split mul (succ.p2)) )

-- (5) Defining s -----------------------------------------------------------------

-- pointwise
--s 0 = 1
--s d = s (d-1) + 1

s = p2 .  cataNat (either (const (1,1)) (split mul (succ.p2)) )

--(6) Defining < g k , s k > ----------------------------------------------------------

-- Como se pode ver  g k está defenido como a projeção da segunda componente e s k  da primeira.

-- logo,
spt_gs = cataNat ( either ( const (1,1) ) ( split mul (succ.p2) ) )


-- (5) Defining h -----------------------------------------------------------------

-- h k = < spt_fg k , spt_gs >

h k = for loop (base k) 

base k = ( 1, k+1, 1, 1 )

loop (a,b,c,d) = flat (curry p a b) (curry p c d)
    where flat (a,b) (c,d) = (a,b,c,d)
          p = split mul (succ. p2)


-- (5) Defining Binomial -----------------------------------------------------------------

-- (sem banana split) --
-- gsol k n = flat (spt_fg k (n-k) ) ( spt_gs (n-k) )
   -- where flat (a,b) (c,d) = (a,b,c,d)

-- (com banana split) --
binomial n k = let (a,_,b,_) = h k (n-k) in div a b
