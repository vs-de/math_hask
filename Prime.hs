module Prime where

import StdLib
import Data.Ratio
import Data.List
import PrimeConst

-- [OLD STUFF] --

--getLowDivisors = (\x -> filter (\n -> (mod x n) == 0 ) [2..(floor $ sqrt (fromIntegral x))] )

--changing my old stuff...
divisors n = filter ((==0).(mod n)) [1..(div n 2)]
low_divisors n = filter ((==0).(mod n)) [1..(isqrt n)]

isOddPrime 0 = False
isOddPrime 1 = False
isOddPrime 2 = True
--isOddPrime x = not $ or $ map (\n -> (mod x n) == 0 ) [3,5..(floor $ sqrt (fromIntegral x))]

isOddPrime x = not $ or $ map (\n -> (mod x n) == 0 ) $ primeCandidatesTo (floor $ sqrt (fromIntegral x))

--isPrime :: (Integral b) => b -> Bool
isPrime 2 = True
isPrime x = if mod x 2 == 0 then False else isOddPrime x


prime_e_sieve :: [Integer] -> [Integer]

prime_e_sieve [] = []
--if it's called by [0/1..]
--prime_e_sieve (0:xs) = prime_e_sieve xs
--prime_e_sieve (1:xs) = prime_e_sieve xs
prime_e_sieve (x:xs) = [x] ++ prime_e_sieve (filter (\z -> mod z x /= 0) xs)

-- both are slow

-- primes = prime_e_sieve [2..]
--
--

--test for primeCandidatesFrom, must all be zero
nextCands n = filter (\k -> k >= n) primeCandidates
testPC = mapM (\n -> putStrLn $ show $ map (\tp -> (fst tp) - (snd tp)) $ zip (take 50 $ primeCandidatesFrom n) (take 50 $ nextCands n)) [19201..20100]


--not needed any more, but who knows? future will show
nextIntMul n = (\k -> if (k `mod` n /= 0) then k+n-(k `mod` n) else k)

--primeCandidatesFrom n = concatMap (\n -> (n-1:[(n+1)])) [n,(n+6)..]


--next is this

prime_cycle_13' = take 5760 $ listDiffs $ filter (\n -> n `mod` 13 /= 0) $ scanl (+) 13 $cycle prime_cycle_11


-- [OLD STUFF ENDS] --


-- [NEW STUFF] --


--infinite prime cycle without cycle in the outer loop (ok, don't take this def too serious ;)
prime_cycle' :: Integer -> [Integer]
prime_cycle' 0 = cycle [1]
prime_cycle' k = listDiffs.(filter (\n -> mod n (nth_p k) /= 0)).(scanl (+) (nth_p k)) $ prime_cycle' (k-1)
        where nth_p n = primes !! (fromInteger.pred) n
--so let's generalize it, and we come to to the sieve of erath. again
-- but on the way we saw some nice connections

--so the finite prime_cycle

--how many to take until it repeats?
--the following is fucking slow, so let's take knowledge to get the primecycle counts
p_tk' n = length $ takeWhile (\(a,b) -> a < prod_ n) $ pair_scanl (\a b c d -> (a+c,d)) (0,0) $ dup zip (prime_cycle' $ toInteger n)
        where prod_ = (fromInteger.product.(`take` primes))

-- with knowledge...
p_tk = ((((scanl (*) 1).(map (pred)).primesFrom) 3) !!).fromInteger

--p(n) is simply the nth prime:
p = (primes !!).fromInteger
p' = p.pred


-- ** trying to improve the p performance

prime_base_2 = [(1,5),(2,11),(3,23),(4,59),(5,137),(6,313),(7,727),(8,1621),(9,3673),(10,8167),(11,17881),(12,38891),(13,84047),(14,180511),(15,386117),(16,821647),(17,1742539),(18,3681149),(19,7754081),(20,16290073),(21,34136059),(22,71378603),(23,148948141),(24,310248251)]

prime' k = (\(i,p) -> last $ take' ((.) succ toInteger k-(2^i)) $ primesFrom p).last $ takeWhile (\(a,b) -> a <= (iLog2) k) prime_base_2

--lets use p_step in Const
-- ...iterate prime_succ instead of primesFrom?  - slower
prime k = (\(i,p) -> last $ take' ((.) succ toInteger k-i) $ primesFrom p).last $ takeWhile (\(a,b) -> a <= k) p_step_2_16

-- ** --

--fuck, take takes an Int, so i have a 64bit maximum, so i have to define a take that takes Integer (free size)
--i don't expect to reach this in any realistic scenario, but i want this to be implemented
--biggest Int on MY (64) System is 9223372036854775807
--atm i don't know if it's because of my arch, or if it's haskell std

biggest_int = 2^63-1
take' n lst     | n <= bi       = take (fI n) lst
                | otherwise     = (take (fI bi) lst) ++ (take' (n-bi) $ drop (fI bi) lst)
        where   bi = biggest_int
                fI = fromInteger

prime_cycle n = (.) (`take'` prime_cycle' n) p_tk (n-1)
prime_candidates n = scanl (+) (p n) $ cycle cyc_
        where cyc_ = pcyc n --prime_cycle n

--another version is this:
prime_cands'     n = (:) (p n) $ map (\f -> f (p n)) (scanl1 (.) $ map (+) $ (.) cycle pcyc n)


--how many primes in prime_candidates
--this is exactly the number of primes in [p,p(n)]
--so you can say, that the first num you miss is always the square of your starting num!... OF COURSE!
pipc = map (length.(takeWhile isPrime).prime_candidates) [1..]
pipc_80 = take' 80 pipc
-- so, refering to what i mentioned above, there must be a faster way to calc these nums
-- BUT, this way successive primes WOULD BE INVOLVED, but wait, no way, WE HAVEN'T to use them
-- just go as long as you don't reach the square, oh man, trivial if you think about
-- blah, blah, let's do it

pc n = takeWhile (<((toInteger (primes !! (fromInteger n)))^2)) $ prime_candidates n

--
-- --
--

pcyc n = take' (p_tk (n-1)) $ prime_cycle' n
pcyc7 = pcyc 7
p_init7 = pcyc7
p_init = pcyc

--mini prime list
mp = take 5 primes
is_prime 0      = False
is_prime 1      = False
is_prime n      = not $ or $ map 
                        (\k -> (mod n k) == 0) $ 
                        takeWhile (<= ((floor.sqrt.fromIntegral) n)) $ 
                        mp++(prime_candidates.toInteger $
                        length mp)

--not really useful yet
--pipcyc n = (.) (`take'` prime_cycle' n) toInteger (pipc180 !! (fromInteger n))

--this is SLOWER than to just calc the primes between p and p^2
--but we come to it avoiding heavy use of primes
--k, intermediate result
--it's like you can't compress information that is already totaly compressed
--the prime numbers are heavy compressed, also totaly?
--now, perhaps we can combine it

--so, i need this slower and wrong prime_cands to get a big repeating cycle, that NEVER misses a prime.
--go ahead
-- for instance, let's start at 

--hard coded pipc init
--pipc180 --moved to Const

primes' = filter is_prime $ 2:3:5:7:11:(prime_candidates 5)
prime_succ n    | is_prime (succ n) = succ n
                | otherwise = (.) prime_succ succ n
next_prime = prime_succ

prime_pred n    | is_prime (pred n) = pred n
                | otherwise = (.) prime_pred pred n
prev_prime = prime_pred


-- [NEW STUFF ENDS] --

-- [OLD STUFF] --


--the old thing...
primeCycle = [2,4,2,4,6,2,6,4]
primeCycleSum = sum primeCycle --30

pcIndex i = head $ (\n -> filter (\k -> foldr (+) 0 (take k primeCycle) >= n) [0..8] ) i

primeCandidates = 2:3:5:7:(scanl (\n k -> n+k) 11 (prime_preload++(cycle primeCycle)))


primeCandidatesFrom 2 = 2:3:5:(primeCandidatesFrom 7)
primeCandidatesFrom 3 = 3:5:(primeCandidatesFrom 7)
primeCandidatesFrom 4 = 5:(primeCandidatesFrom 7)
primeCandidatesFrom 5 = 5:(primeCandidatesFrom 7)
primeCandidatesFrom n   | (n < 2) = primeCandidatesFrom 2
                        | otherwise = scanl (\n k -> n+k)
                        ((\(a,b) -> 11+30*a+(sum $ take (pcIndex b) primeCycle)) (divRes))
                        (cycle $ rotate primeCycle $ (if (snd divRes) == 0 then 0 else pcIndex (snd divRes)))
                where divRes = (n-11) `divMod` 30

primeCandidatesTo n = maybe [] (\k -> take k lst) $ findIndex (\k -> k>n) lst where lst = primes
--primeCandidates = primeCandidatesFrom 6
primes = filter isOddPrime primeCandidates

primesFrom n = filter isOddPrime $ primeCandidatesFrom n

primesTo n = filter isOddPrime $ primeCandidatesTo n

primeSucc n = head $ primesFrom (succ n)
nextPrime = primeSucc

--of course, we could invert the primeSucc with primesTo, but, ... uuhh, this would be bad;)
primePred n = if isPrime (pred n) then (pred n) else primePred (pred n)
prevPrime = primePred


-- [OLD STUFF ENDS] --


{-BETTER SOLUTION, see below
listDiffs [] = []
listDiffs [a] = []
--listDiffs lst = (abs ((head lst) - (head (tail lst)))) : (listDiffs (tail lst))
listDiffs lst = ((head (tail lst))- (head lst)) : (listDiffs (tail lst))
listDiffsAbs lst = map abs $ listDiffs lst
-}



--bad implemented...
primePairs = map (\n -> 0+(if (isPrime (n-1)) then 1 else 0)+(if (isPrime (n+1)) then 1 else 0)) [0,6..]

primeCol pos = map (\l -> l !! ((length l)-pos)) $ map (\n -> listBitsZP pos n) primes

visualBinPrimesInv = concatMap (\n -> map (\ch -> if (or [ch == '1', ch == ',', ch == '[', ch == ']']) then ' ' else if ch == '0' then 'X' else ch) (show (listBitsCutTo 6 n)) ++ ['\n']) $ take 6542 primes

visualBinPrimes = concatMap (\n -> map (\ch -> if (or [ch == '0', ch == ',', ch == '[', ch == ']']) then ' ' else if ch == '1' then 'X' else ch) (show (listBitsCutTo 6 n)) ++ ['\n']) $ take 6542 primes

--prime factors

--bad implemented again :(
--have 2 check the partners and have to testmul the stuff for faster
--end-recognition
{-
pFacs n = map (\(a, lst) -> (a, last lst)) $
          filter (\(a, lst) -> (a, lst) /= (a,[])) $ 
          map (\i -> (i, (takeWhile (\k -> mod n (i^k) == 0) [1..]))) $
          primesTo $ fst (divMod n 2)
--}

pFacs n = map (\(a, lst) -> (a, last lst)) $
          filter (\(a, lst) -> (a, lst) /= (a,[])) $ 
          map (\i -> (i, (takeWhile (\k -> mod n (i^k) == 0) [1..]))) $
          primesTo $ floor $ sqrt (fromIntegral n)


--not eats Primes, weak stomache
pFacs' n ((p, k):xs) =  --if fromFacts lst == n 
                        if mod n (p^k) == 0 then 1 else 1 --unfinished
                        where lst = (p, k):xs

--inv of pFacts
fromFacts lst = product $ map (\(a, b) -> a^b) lst

--sophie germain
sg_primes = filter (\n -> isPrime (2*n+1)) primes

--mersenne


-- very slow
--mrs_primes = filter isPrime (map (\n -> 2^n-1) primes)
-- also too slow
--mrs_primes = filter (\n -> n `mod` (2^(1+(floor (logBase 2 (fromIntegral n))))-1) == 0) primes

-- Lucas-Lehmer-Test
ll_mrs_test n = (foldl (\k i -> (k^2-2) `mod` (2^n-1)) 4 [1..(n-2)]) == 0

mrs_primes_idc = filter ll_mrs_test primes
mrs_primes = map (\n -> 2^n-1) mrs_primes_idc


-- [EXAMPLE/FUN STUFF] --


--example polynoms
no_prime_pol4_11_a = (\n -> 4*n^2+11*n+7)
no_prime_pol4_11_b = (\n -> 4*n^2+11*n+8)

prime_pol4_2 = (\n -> 4*n^2+2*n+17)
--moved 1 position
prime_pol4_m6 = (\n -> 4*n^2-6*n+19)

nice_prime_counts = map (\k -> length $ filter isPrime $ map(\n -> 4*n^2+11*n+k) [1..1000]) [1..]
unholy_sequence = nice_prime_counts -- ;-)

--biggest prime found until today, = biggest mersenne prime found (no. 46)
-- takes a while for output, be patient ;)
biggest_prime = 2^43112609-1


-- [EXAMPLE/FUN STUFF ENDS] --


--proth
proth_term_idc k = filter (\n -> 2^n > k ) [2..]

proth_term k = if (mod k 2 == 0) then [] else  map (\n -> k * 2^n + 1) $ proth_term_idc k

proth_primes k = filter isPrime $ proth_term k

--sierpinsky
--smallest known srp number
srp_num = 78557
srp_term k = (\n -> k*2^n+1)

--riesel
--smallest known rsl number
rsl_num = 509203
rsl_term k = (\n -> k*2^n-1)


--primorials
-- 1st definition

primorial = (\n -> product $ take n primes)

primorials = map primorial [0..]

-- 2nd definition
primorial' = (\n -> product $ takeWhile (\k -> k <= n) primes)

primorials'= map primorial' [1..]



--the following list is taken from A0005234 from OEIS
primorial_primes_ = [2,3,5,7,11,31,379,1019,1021,2657,3229,4547,4787,11549,13649,18523,23801,24029,42209,145823,366439,392113]

--THESE ARE REAL BIG PRIMES
primorial_primes = map (succ.primorial') primorial_primes_


--even perfects (if there are odd perfects at all, then they would be > 10^500 so i just ignore them ;) )
perfects = map (\n -> 2^(n-1)*(2^n-1)) mrs_primes_idc

--------------------------------------------------------------------------------
--some lesser interresting lists
--prime successors of 2^n from 0 upto 64
p_2e_ = [2,3,5,11,17,37,67,131,257,521,1031,2053,4099,8209,16411,32771,65537,131101,262147,524309,1048583,2097169,4194319,8388617,16777259,33554467,67108879,134217757,268435459,536870923,1073741827,2147483659,4294967311,8589934609,17179869209,34359738421,68719476767,137438953481,274877906951,549755813911,1099511627791,2199023255579,4398046511119,8796093022237,17592186044423,35184372088891,70368744177679,140737488355333,281474976710677,562949953421381,1125899906842679,2251799813685269,4503599627370517,9007199254740997,18014398509482143,36028797018963971,72057594037928017,144115188075855881,288230376151711813,576460752303423619,1152921504606847009,2305843009213693967,4611686018427388039,9223372036854775837,18446744073709551629]

p_3e_ = [2,5,11,29,83,251,733,2203,6563,19687,59051,177167,531457,1594331,4782971,14348909,43046747,129140197,387420499,1162261523,3486784409,10460353259,31381059613,94143178859,282429536483,847288609457,2541865828331,7625597485003,22876792454987,68630377365013,205891132094653,617673396283963,1853020188851911]

p_4e_ = [2,5,17,67,257,1031,4099,16411,65537,262147,1048583,4194319,16777259,67108879,268435459,1073741827,4294967311,17179869209,68719476767,274877906951,1099511627791,4398046511119,17592186044423,70368744177679,281474976710677,1125899906842679,4503599627370517,18014398509482143,72057594037928017,288230376151711813,1152921504606847009,4611686018427388039,18446744073709551629]

p_7e_ = [2,11,53,347,2411,16811,117659,823547,5764817,40353611,282475267,1977326753,13841287217,96889010447,678223072853,4747561510009,33232930569607]
--------------------------------------------------------------------------------

--slightly more interresting
--------------------------------------------------------------------------------

stat_mon_primes = map (\k -> count True $ map is_prime $ map (\n -> k*n+1) [0..10000]) [0..100]
--this gives
smp_10e4        = [0,1229,2261,1610,2085,1274,3014,1153,1935,1438,2387,1044,2793,1008,2170,1718,1823,965,2718,943,2233,1556,1952,927,2611,1083,1903,1318,2018,900,3230,885,1724,1402,1829,1236,2544,886,1777,1363,2090,869,2938,858,1843,1557,1736,863,2482,951,2077,1312,1763,838,2469,1134,1891,1289,1708,841,3039,824,1660,1429,1618,1093,2662,839,1718,1257,2336,827,2400,794,1647,1505,1663,1002,2593,831,1993,1186,1631,801,2747,1067,1622,1223,1734,798,2950,996,1661,1212,1596,1034,2341,791,1826,1280,1964]

--now i want to get an ordered list of k's, so that the order represents the number of primes you get using that coeff in k*n+1

smp_10e4_k      = concatMap (\n -> map toInteger $ elemIndices n smp_10e4) $ (.) nub sort smp_10e4

--if we look at
--map (length.fpf) smp_10e3_k
--we see that it seems like that there are more primes, if we have more prime facts in k
--
--in general
count_map_primes f n = ((count True).map (is_prime.f)) [0..n]


--how many primes in monomials...

--map on coeff
--ok, this means:
-- tr: translation, for instance (+1) (as always: take id if you don't want it)
--  b: base of monomial
--  e: exp of monomial
--  n: how many to test
count_mon_primes_c tr b e n = count_map_primes (tr.(\c -> c*b^e)) n

-- so
-- count_mon_primes_c (+(-1)) 6 1 10000
-- > 3041
-- count_mon_primes_c (+1) 6 1 10000
-- > 3014
-- added together is 6055
-- this must be the number of all primes < 60001 = 6*10000+1
-- got it? --if you got it don't be angry, because it's too simple
-- and it is actually 6057, because 2 and 3 are the only primes that fuck on the 6n+/-1 rule;)


--so let's map on base next

--map on base
count_mon_primes_b tr c e n = count_map_primes (tr.(\b -> c*b^e)) n

-- if e=1 than cmp_c and cmp_b are the same

--map on exp
count_mon_primes_e tr c b n = count_map_primes (tr.(\e -> c*b^e)) n


--ok, now let's get the above story for 10^5
--using the new funcs
-- so cmp_calc (10^4) 100 = smp_10e4

cmp_calc k n    = map (\n -> count_mon_primes_b (+1) n 1 k) [0..n]
cmp_10e5_calc n = cmp_calc (10^5) n
--gives
cmp_10e5        = [0,9592,17983,12970,16900,10386,24524,9406,15942,11840,19617,8565,23187,8290,17862,14219,15109,7996,22537,7829,18586,12955,16253,7709,21925,9143,15802,10899,16936,7536,27041,7403,14355,11797,15208,10412,21323,7325,15005,11487,17645,7206,24643,7243,15430,13152,14652,7100,20915,8139,17417,11107,15048,7049,20778,9513,16067,10928,14305,6964,25719,6926,14150,11950,13686,9240,22503,6925,14449,10669,19821,6870,20330,6868,13944,12685,14272,8642,21956,6885,16845,10081,13724,6795,23409,8932,13713,10431,14711,6770,25164,8422,13915,10265,13656,8816,19924,6704,15548,10933,16573]

--now i want the k's
cmp_idc lst = concatMap (\n -> map toInteger $ elemIndices n lst) $ (.) nub sort lst
cmp_calc_k lst = cmp_idc lst

euler_pol_a = (\n -> n^2+n+17)
euler_pol_b = (\n -> n^2-n+41)

--------------------------------------------------------------------------------

--pi: how many primes are <= n
--exactly
p_pi n = count True $ map isPrime [2..n]

--approx funcs
--legendre 
leg_pi x = x / (log x - 1.08366)

--grrrr, the hask fucking float log :(
leg_pi_rat x    = (x%1) / (((%(2^127)) $ (fromIntegral.floor) (log x' * (2^127))) - (54183 % 50000))
        where x' = fromInteger x

--floor leg_pi and floor leg_pi_rat should be equal


-- to compare them
compare_pi      = map (\n -> (p_pi n, floor $ leg_pi_rat n)) [1..1000]

-- TODO: add logarithmic integral Li(x)
--

--all relevant entries in mult. table from GF p
--(all entries not producable by mirroring table entries as far as i can recognize it)
gf_entries p = map (\k -> cut_ (pred k) $ map (\n -> mod (n*k) p) [1..(n)]) [1..(div n 2)]
        where   n = pred p
                cut_ k = (.) (repf k init) (repf k tail)

gf_part p = part_ $ (\n -> map (\k -> (k, count k $ concat $ gf_entries n)) [1..(n-1)]) p
        where   part_ ((a,b):xs) = partition (\(a,x) -> b/=x) ((a,b):xs)

--which ones are not in the mult-table
-- only relevant are the primes, where (p-1)/2 is even

--listDiffs HERE
p_100 = map (\p -> map fst $ fst $ gf_part p) $ filter (\p -> even $ numerator ((p-1)%2)) $ dropWhile (<3)$ takeWhile (<100) primes
-- listDIffs HERE

-- even $ numerator (%2)...<=> `mod` 4 == 0
--OK, now, when im looking at the above toy, i try to express what im doing here
--i think on the connection of the "relevant" inner set of the gf mult. table
--relevant here means, the smallest set of entries you need, to build all others through a "simple" algorithm
--if the prime is not of the form 4n+1 then the # of all nums in this set is the same
--whats with the primes of the form 4n+1?
--so p_100 is just a listing of the tbl part of the elems with lower occurance.
--btw the partition sizes are always equal


--
-- so, now some funcs, that are definitly relevant in prime-math
--


o_r_d n p =  succ $ length $ (\n p -> takeWhile (\a -> a `mod` p /= 1) $ map (n^) [1..]) n p

--so let's take an primitive algo for the primitive root ;)

primitive_root n = head (dropWhile (\k -> (o_r_d k n) /= fromInteger (n-1)) [1..])
-- oh, this fromInteger was EVIL! the bad side of haskell :(
p_root = primitive_root

--as the chinese remainder theorem states, there must be an a to solve the equation system:
--a cong. a_1 mod n_1, a cong. a_2 mod n_2...
--if all n_k are pairwise coprime
--simple solution for a given list of pairs (a_k,n_k)
mod_solve [] = 0
mod_solve lst = head $ (foldl1 (.) $ map (\(a,b) -> filter (\n -> mod n b == a)) (tail lst)) $
                map (\n -> (fst (head lst))+n*(snd (head lst))) [1..]


--moebius
--
-- uses fpf
moeb 1 = 1
moeb n  | (any ((>1) . snd) . fpf) n = 0
        | otherwise = ((-1)^) $ length $ fpf n

--nat nums as list of prime exponents


--TODO! complete/improve this
--ok, lets comlete it...
--this is the infinite one
prime_exps' n = map (\i -> length $ takeWhile (\k -> mod n (i^k) == 0) [1..]) primes -- $ (map (\a -> (a,a))) primes


--prime_exps n = scanl1 (*) findIndices (>0) prime_exps'


--now we build one with end recognition
-- just integrate a multiplication scanl :) what? yes, just do it.
--ok, again we first make an infinite list, but with tuples
prime_facs' n   =       map (uncurry (\a b -> 
                                (a,
                                last' $ takeWhile (\k -> mod n (b^k) == 0) [1..])
                        )) $
                        dup zip primes
        where   last' []        = 0
                last' l         = last l

--now we interprete it...
{-
prime_exps n    =       pair_scanl (\a b c d ->
                                ( (a^b)*(c^d) , 1 )
                        ) (1,1) $ prime_exps' n
-}
--easier
--Hey, think about (\_->(>0)) <=> const (>0) <=> const.(>0)
--fascinating ;)
--so, if you're interested only in the snd part of [(,)], "uncurry const" is your friend...
--
--YO! Here is my prime factorization algo :)

prime_facs n  | is_prime n    = [(n,1)]
              | otherwise     = fst $ head $ dropWhile (uncurry (\_->(<n))) $ pair_scanl (\a b a' b' ->
                                (a++a',b*b')
                        ) ([],1) $
                        uncurry zip $ dup (\lst0 lst1 -> 
                                (map (\a -> [a]) lst0, map (uncurry (\p e -> p^e)) lst1)
                        ) $ (.) (filter (uncurry (\_->(>0)))) prime_facs' n

--the above construnct has a big speed penalty at the moment, but we'll see...


--
--ok, the std algo everyone would first think about
--good, that it heavily depends on the prime generation&detection, which is relatively fast :)
--
--Fast Prime Facts (fpf)
fpf_ n  | is_prime n     = [n]
        | otherwise      = ffac:(fpf_ (div n ffac))
        --find fac
        where ffac = head $ dropWhile ((/= 0).mod n) primes

fpf = map (\l -> (head l,(.) toInteger length l)) . (.) group fpf_


--so, if we want to assemble it back:
pf_inv  = foldl1 (*) .  map (uncurry (^))


--10^7 is just an estimation, could be optimized, don't know, tests will show
--some tests have shown that
--is_prime is fast at bigger primes, where isPrime is faster at small ones.

primes_downFrom n       | even n        = (.) primes_downFrom pred n
                        | n == 1        = [2]
                        | (n < (10^7))  = (filter isPrime.down) n ++ [2]
                        | otherwise     = (\lst -> 
                                                (init lst)++primes_downFrom (last lst)
                                        ) (take 10 $ filter is_prime (down n))
                        where down n = [n,((pred.pred) n)..1]


--so, now we can build the fpf_sqrt func
--

fpf_sqrt n = filter ((== 0) . mod n) $ primes_downFrom (isqrt n) --((toInteger.floor.sqrt.fromInteger) n)

fpfs = head.fpf_sqrt


--this is nice for:
--euler phi
-- counts all m <= n which have gcd == 1
--simple implementation
--phi_e 1 = 1
phi_e = product.(map (uncurry (\p k -> p^(pred k)*(pred p)))).fpf


--fermat method for factorization
fm_fac n = head.dropWhile (not.is_square) $ tail $ map (\k -> ((-n+).(^2).(+k).succ.isqrt) n) [0..]
-- does not give a factor, it just gives the relevant square, do the rest by hand :]]


--quadratic stories
-- beginning quad sieve

--first we need a factor base
factor_base size n = take size $ filter (\p -> any ((==) (mod n p)) (quad_residues p)) primes

--first of all, i had this on my shell
--(\k -> filter (and.(map (\(b,e) -> elem b (factor_base 20 k)) ).fpf.(+(-n)).(^2)) [isqrt k..isqrt k + 20])
--it gives possible values with the factor base (not very fast)
--name it:
--elems_for_base size base = filter (and.(map (\(b,e) -> elem b base) )

-- this isn't nice, but useful
-- just gives all (prime)facs up to biggest prime values. lists null-exponents

--general factors func
base_elems base n = filter (and.(map (\(b,e) -> elem b base)).fpf.(+(-n)).(^2)) [isqrt n..]

factors base n = (\lst -> map (\p -> maybe (p,0) id $ find ((p==).fst) lst) $ takeWhile (((>=).fst.last) lst) base) $ fpf n

fac_vec base = (map ((`mod` 2).snd)).factors base


--let's build the matrix
build_matrix size n = 
    let base = factor_base size n
        --cands = [(succ.isqrt) n..n] --candidates
        cands = base_elems base n
    in
        map ((fac_vec base).(+(-n)).(^2)) cands

-- this was quite shitty ... ;)

--
-- konsi session

--  let pairs = perm_pairs (take 8 $ base_elems base 87463)
--  let pairs' = zip ((transpose pairs) !! 0) ((transpose pairs) !! 1)
--  map (\(a,b) -> let y = ((floor.sqrt.fromInteger) b) in gcd (y-a) n) $ filter (\(a,b) -> is_square b) $ map (\(a,b) -> ((a*b `mod` n),(a^2-n)*(b^2-n))) pairs'
--
--

-- ok, just a simple square mod trial here:
-- this is the beginning of the optimization fun.
fac_sqrts n = tail $ iterate (\k -> head $ dropWhile (not . is_square . (`mod` n) . (^2)) [succ k..]) ((.) pred isqrt' n)

fs2fac n f = gcd n (f - isqrt ((f^2) `mod` n))

factorize_simple n = head $ keep_map (fs2fac n) (fac_sqrts n)

--thinking back to cross_mul

cross_primes = (\i -> map (\n -> (primes !! n) * (primes !! (i-n))) [0..i])


--remainders of all pred primes of nth prime
prime_remainders' = prime_remainders.p
prime_remainders  = (\n -> map (mod n) $ primesTo n)

----------------------------------
--how are nums composed by primes?
----------------------------------
n_pfac_nums_general f n = filter (f.fpf) [n..]

-- k prime facs a b, so that a^i * b^j = k, for i,j > 1, starting at n
n_pfac_nums' k n        = n_pfac_nums_general ((==k).length) n

-- k prime facs a, b, so that a*b = k, starting at n
n_pfac_nums k n         = n_pfac_nums_general
                                (\lst -> ((.) (==k) length lst) && 
                                        (and $ map 
                                        (\a -> snd a == 1) lst)
                        ) n

pp_pseudo_primes = 2:3:(filter (\n -> mod (n^2) 24 == 1) [1..])

--visualization
--

p_facs n = (reverse.(map snd).((.) take' p n).prime_facs')
show_facs_to n          = mapM (putStrLn.show) $ map (p_facs n) [1..n]
show_facs_to' n         = mapM (putStrLn.show) $ map ((dropWhile (==0)).(p_facs n)) [1..n]

--printListLn lst = (mapM (putStrLn.show)) lst
-- ouch! this is just:
printListLn lst = mapM print lst -- greetings from -XNoMonomorphismRestriction

------------------------------- JFF ;) -------------------------------

{--
 -
 -  -- D  E  M  O --
 -
 -}

fac_demo_at n k = mapM (\n -> mapM (putStr.(\n -> if n == "0" then " " else n).show) n >> putStrLn "") $ map (p_facs k) [n..]
fac_demo        = fac_demo_at 1 36
fac_demo_at' n k = mapM (\n -> mapM (putStr.(\n -> if n == "0" then " " else n).show) n >> putStrLn "") $ map (p_facs' k) [n..]
p_facs' n = (reverse.(map snd).(take' n).prime_facs')


-- playing with prime gaps

-- first gap greater or equal
gap_ge n = head $ dropWhile ((<n).fst) $ (keep_apply (listDiffs)) primes
-- equal
gap_eq n = head $ dropWhile (uncurry (\a b -> a /= n)) $ (keep_apply (listDiffs)) primes

