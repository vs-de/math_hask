module StdLib where

{-
 -
 - this is the inner chaos of all other well-structured libs ;)
 -
 -}

import Ratio
import Numeric
import List
--import Data.Map (Map)
import qualified Data.Map as Map
--import Prime (pf_inv)
--import Data.Set (toList, fromList) 
import Data.Bits((.|.),(.&.), shiftR)
import Control.Concurrent
import qualified Data.ByteString as BS
import Char

-- own libs
import BaseConst
import Visual

apply f v = f v

has_all_criteria thing = and . map (\f -> f thing)

std_base_func n = if (n<10) then chr(0x30+n) else chr(ord('A')+n-10)
baseShow b num = showIntAtBase b std_base_func num ""
show32 num = baseShow 32 num
show30 num = baseShow 30 num
show24 num = baseShow 24 num

--argh, have to zero-pad values

showPaddedHex8 n
    | n < 16 = "0" ++ showHex n ""
    | otherwise = showHex n ""

bs2str = map (chr.fromIntegral) . BS.unpack
str2bs = BS.pack . map (fromIntegral.ord)

str2hex = concatMap (showPaddedHex8 . ord)
bs2hex = concatMap (showPaddedHex8 . ord) . bs2str

--monomorphism restriction without a
int2bs a = BS.pack.(\n -> map (fromIntegral.(.&. 0xff).(shiftR n)) [0,8..56]) $  a


sleep = threadDelay.floor.(*1000000)

-- [LIST STUFF] --


--last successor in list
lastSucc (x:[]) = x
lastSucc (x:xs) = if (head xs) == (succ x) then lastSucc xs else x

--splitAt for a list of indices to split at, resulting in a list of partial lists
--indices must be ascending!
splitAt_ = splitAt.fromInteger
splitAt'' str [] = [[]]
splitAt'' str (x:[]) = ((\(a,b)->[a,b]).(splitAt_ x)) str
splitAt'' str (x:xs) = ((\(a,b)->[a]++splitAt'' b xs).(splitAt_ x)) str

--splitAt'' is not meant for ext. use

--so, now we go
splitAt' str [] = [str]
splitAt' str idcs = splitAt'' str ((head idcs):(listDiffs idcs))

--split list without delim

split lst delim = (\(x:xs) -> x:(map tail xs)) $ splitAt' lst $ sublist lst delim

--split with delim as part
split' lst delim = (\(x:xs) -> x:(concatMap (\(y:ys) -> [[y],ys]) xs)) $ splitAt' lst $ sublist lst delim

substitute (sub1, sub2) str = replaceAll str sub1 sub2

subst (sub1, sub2) str = substitute (sub1, sub2) str    --alias. how to make this more simple in haskell? 
                                                        -- i just want to write subst = substitute

--sublist
--returns a list of indices, indicating the beginnings of where the sublists are found
sublist lst sublst = getIndices $ map (\f -> f lst) (map elemIndices_ sublst)

                where   getIndices (x:xs) = map (\n -> x !! (fromInteger n)) $ 
                                elemIndices_ True $ 
                                map (\n -> and $ zipWith (\a b -> elem a b) [1+n..] xs) x
                        elemIndices_ n l= (map toInteger) $ elemIndices n l

replace lst srch repl = (\(a,b) -> a++repl++drop (length srch) b) $
                        splitAt_ (head $ sublist lst srch) lst

-- not the best solution, because indices are repeatedly generated
replaceAll lst srch repl = head $ dropWhile (\lst -> sublist lst srch /= []) $ iterate (\a -> replace a srch repl) lst

--replaceAll lst srch repl = map (\i -> (\(a,b) -> a++repl++drop (length srch) b) $ splitAt i lst) (sublist lst srch)
-- now start with the first list, for each element here we search for a successor in the next list, next succ in next...
-- if all lists contain the next successor, then we have found a sublist.
-- would be interresting if we could use transpose in some way

substring haystack needle = sublist haystack needle -- ;)

-- divide into chunks of a given size
chunks n lst = (takeWhile (/=[]) . splitAt' lst) [n,(2*n)..]
--kills duplicates
uniq a = nub a--lst = (toList.fromList) lst

-- [LIST STUFF ENDS] --


-- [GENERAL STUFF] --
-- cool fac ;)
fac n = product $ reverse [2..n]

--highest number k, so that k! <= n
unfac n = pred $ head $ dropWhile (\k -> (fac k) <= n) [1..]


--faster if we know it's a fac
--unfac' n = lastSucc $ getLowDivisors n 
-- so what?



--just for (de-;)visualization
bool2bin = map (\b -> if b then 1 else 0)
bin2bool = map (== 1)

showBits :: Integer -> String

showBits 0 = []
showBits n = showBits (floor (n%2)) ++ [head (show (mod n 2))]

--listBits :: Integer -> [Integer]
--this function doesn't work on big ints
--listBits 0 = []
--listBits n = listBits (floor ((fromInteger n) / 2)) ++ [(mod n 2)]
--  better:

listBits 0 = []
listBits n = listBits (floor (n%2)) ++ [(mod n 2)]

--zero padding
listBitsZP s n = let lst = listBits n in (take (s-(length lst)) (repeat 0)) ++ lst
--only last s bits
listBitsCutTo s n = let lst = listBits n in drop ((length lst)-s) lst

--bit list -2- int
bL2Int lst = sum $ map (\n -> 2^((length lst)-n-1)) (elemIndices 1 lst)

get1Ratio n = (fromIntegral  (length $ filter (\x -> x == 1) (listBits n)) ) / (fromIntegral  (length (listBits n)) )


--better would be infinite list without param
--dual_exps = 

-- the slow recursive ackermann func
ackermann 0 b = succ b
ackermann a 0 = ackermann (pred a) a
ackermann a b = ackermann (pred a) $ ackermann a (pred b)

--mersenne twister library test (System.Random.Mersenne)
--mt_test = getStdRandom (\rnd -> (randoms rnd) >>=(\lst -> return (filter (\n -> (fromEnum n) > 10^20 ) $ map abs lst)))

--simple function repeater
--sure, somewhere such thing is in the lib, but i don't remember where (or if it exists at all)

-- i think id makes sense here, we'll see

repf 0 f = id
repf 1 f = f --ok, this isn't necessary, yo, just neat to read :)
repf n f = f.(repf (n-1) f)

--i found it, it's iterate but has no count param, so in some cases this even fits better
--iterate gives complete result list
----to comment my comment again, iterate is simply more general
------ to comment my comment of the comment again, there is a need for a
-- special iterate, not just increasing the apply of f()
-- if the function is monadic we want the real output of the former apply

-- this should be similar to the default definition: iter f a = [f a] ++ iter f (f a)
-- same behaviour here: iter f a = (\fa -> [fa] ++ iter f fa) $ f a

-- DAMN, also THIS doesn't stop the lazy evaluation! ???
-- i'm out of ideas. is it because of recursion?
-- seems to be when f is monadic
iter f a = (\x -> [x] ++ iter f x) $! (f a)

--simple count, could be implemented by just:
--length $ elemIndices x lst
--but:
--look at the very last lst, its the 2nd arg to the lambda expression. Learned something
--count x lst = (.) (\f lst -> length (f lst) ) elemIndices x lst
count x = toInteger.length.elemIndices x

--not needed anymore, concat does the same
--flatten = foldl1 (++)


-- use the "guard" style for just using it ;)
--BETTER SOLUTION, see below
{-
listDivs (x:[]) = []
listDivs lst | lst == [] = []
             | otherwise = (head.tail) lst `div` head lst : (listDivs.tail) lst

listRatios (x:[]) = []
listRatios lst | lst == [] = []
             | otherwise = (head.tail) lst % head lst : (listRatios.tail) lst

-}


--find least common multiple of denominator
denom_lcm lst = ((foldl1 lcm).map denominator) lst

listExpandedNumerators lst = map (\n -> (div lcd_ (denominator n)) * (numerator n) ) lst
        where lcd_ = denom_lcm lst --l. c. denom.

en_ = listExpandedNumerators

-- now generalizing the list... funcs

-- build sequential pairs from a list
seq_pairs [] = []
seq_pairs lst = (.) (zip lst) tail lst

seq_pairs_rev [] = []
seq_pairs_rev lst = (.) zip tail lst lst

-- put operator between all elements and returns the resulting list
pair_map f lst = (((map.uncurry) f) . seq_pairs) lst
pair_map_rev f lst = (((map.uncurry) f).seq_pairs_rev) lst

-- now we define our listDiffs and such funcs more elegantly:
--i.e.:
listDiffs = pair_map_rev (-)

listDivs = pair_map_rev div

listRatios = pair_map_rev (%)

--powerset [] = [[]]
--powerset (x:xs) = [[x],(powerset xs)]


--yo, let's define a function that gets the fold on tuples
--like this
--foldl (uncurry (\a -> (\b -> uncurry (\c -> (\d -> (a+b,c+d)))))) (1,2) [(2,3),(5,6)]

--so, cleaned it by point-free aka eta-reduction
--    n  i  c  e   : - )
pair_foldl = (foldl.(\f -> uncurry.uncurry f))
pair_scanl = (scanl.(\f -> uncurry.uncurry f))

--meta_op makes the "next operation of higher order", so for instance (+) becomes (*)
meta_op = (\f n k -> foldl f n $ take (pred k) $ repeat n)

--just makes 2 copies from one list and gives it to f
dup f = (uncurry f).unzip.(map (\a -> (a,a)))
--ok, that was a bit senseless :S
--i just used it as dup zip

--map two independant functions on a list and get the resulting tuples
--in the following "keep" means, keep the _preimage_
double_map f g  = map (\x -> (f x,g x))
--keep original
keep_map f      = map (\x -> (x, f x))
--map f, and g(f) to a list
dk_map f g      = map ((\x -> (x,g x)).f)

--with list functions
keep_apply lf   = (\x -> zip (lf x) x)

map_applied f lf  = (map f).lf

apply_mapped lf f = lf . map f


--another small helper func
surround a lst = a : lst ++ [a]
--embedding
imbed a = (.) (surround a) (intersperse a)

--1st solution for the 0,1,0,2,... list
dual_exps' n = (\lst -> lst++(n:lst)) $ takeWhile (\a -> a < n) $ (foldl1 (.) $ List.map (\n -> imbed n) [0..n] ) []

--apropos foldl1 (.) ...:
--(foldl1 (.) (replicate 99 succ )) 1 == [1..100] -- True :)

push (a, lst) = lst++[a]
pop lst = (head lst,tail lst)
--some curry lesson ;)
--look at the output of these
--1.
test_curry1 = map (\n -> push (n, [])) [1..10]
--2.
test_curry2 = map (\n -> (curry push) n []) [1..10]
--3. --important! only added 2 parantheses to show the main idea
--
test_curry3 = map (\n -> ((curry push) n) []) [1..10]
--
--4. now we have it
--
test_curry4 = (foldl1 (.) $ map (\n -> ((curry push) n)) [1..10]) []
--

--this is a small quest to myself again
--i want to be able to write exactly this in my ghci:
--summation from 1 to 10 (\k -> k^2)
--
--sum_ = (\k n -> sum (map (\k -> k) [k..n]))
summation i n f g h = sum $ i n f g h

from n f g h = map h [n..(f g)]

to = id

--ok, quick and ugly solution works.
--
--cleaned (point-free like):
sum_ = (.) sum
from_ = map
--TODO: finish this

-- c&p from inet:
rotate xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

------------ N X T stuff missing. new coding sessions ------------

-- fast binary exponantiation,
-- calcs n^k
bin_exp n k = foldl (\a b -> 
    if b == 1 then
        a^2*n
    else
        a^2
    ) n $ drop 1 $ listBits k

bin_exp_mod n k m = foldl (\a b ->
    if b == 1 then
        a^2*n `mod` m
    else
        a^2 `mod` m
    ) n $ drop 1 $ listBits k


integize2 f = (. toInteger) . f . toInteger
intOr = integize2 (.|.)
intAnd = integize2 (.&.)

--don't know the reason...
cross_mul = (\i -> map (\n -> n*(i-n)) [1..(i-1)])
--the following is (n-1)!^2
fs = product.cross_mul

-- ;)
owl = (.)$(.)

--not really useful right now
zeta_summands n = map (\k -> 1%(k^n)) [1..]

--------------------
--      Ratio stuff
--------------------


-- ** ** --

--start easy
--integer log for BIG nums
iLogBase b n = last $ takeWhile (\k -> b^k <= n) [0..]
iLog2 = iLogBase 2

-- ** ** --

--just a thought, not for use
exp_r' = (\r -> exp ((.) fromIntegral denominator r) / exp (fromIntegral (denominator r)-(fromRational r)))

r_exp_slow r' n  = sum $ map (\n -> (r^n)/(fac n % 1)) [0..n]
                where r = toRational r'
--this gives the following fraction for e using 2000 summands
--defined in Const...
--r_e_2000
--"ratio e"

--no faster idea at the moment, not my point of interrest now
--r_exp r' n = (sum $ map (r^) [0..n]
--        where   r = toRational r'
                --num = numerator r
                --den = denominator r



-- idea 
-- (\n -> sum $ (\lst -> zipWith (*) (map (\k -> div (fac n) (fac k)) lst)  (map (2^) lst)) [0..n]) 10
-- divide with fac 10
-- gives exp 2

--takes only integers
--n is accuracy
i2r_exp n r = (sum $ zipWith (*) (scanl div (fac n) [1..n]) (map (r^) [0..n])) % fac n

--this has the following minimum accuracy:
--(r_exp 2000) has 5650 correct digits
--(r_exp 8000) has 27700 correct digits
r_exp n r' = (sum $ zipWith (*) (map fromInteger $ scanl div (fac n) $ map toInteger [1..n]) (map (r^) [0..n])) / (fac n % 1)
        where r = toRational r'

rr_exp' n r' = (r_exp den n / r_exp (floor ((den%1)-r)) n, r-(floor r%1))
        where   r       = toRational r'
                den     = denominator r

--this is the slow rational log
r_log_slow m r = sum $ (\x -> map (\n -> 2*(1%n)*(((x-1)%(x+1))^n)) [1,3..m]) r

{-
-- this is bullshit
r_log m r = r_log_slow m r
rr_log m r = (r_log num m) - (r_log den m)
        where   den = denominator r
                num = numerator r
-}

-- change the method
--double log to estimate real big numbers
r_dlog n = length $ takeWhile (<0) $ map (\k -> (i2r_exp 2000 (floor $ i2r_exp 200 k ))-(n)) [1..]
r_log_bad n = length $ takeWhile (<0) $ map (\k -> (i2r_exp 1000 k)-(n)) [(floor $ i2r_exp 100 1)..]


r_dd_mod f r = f (numerator r*2) % (denominator r * 2)
        --where r = toRational r'

rd_succ = r_dd_mod succ
rd_pred = r_dd_mod pred

--function list
--ratio inv determine
rid_f_list = keep_map (\f n -> map (\f -> (.) f n) [f.rd_pred,f,f.rd_succ])

--rid_arg_list = keep_apply ()

--BAD IMPLEMENTATIONS
rid_arg_img f n = keep_map f [rd_pred n, n, rd_succ n]

rid_best_arg' v n f =   (\(la,ld) -> la !! ((head.elemIndices (minimum ld)) ld) ) $ 
                        unzip $ map (\(a,i) -> (a,abs (v-i))) $ rid_arg_img f n

--with Map
rid_best_arg v n f =    snd $ (\n -> Map.findMin $ Map.fromList $ map swap $ 
                        keep_map (\n -> abs $ v - f n) [rd_pred n, n, rd_succ n]) n
                        where swap = (\(a,b) -> (b,a))

rid_best_img v n f =    (\(li,ld) -> li !! ((head.elemIndices (minimum ld)) ld) ) $ 
                        unzip $ map (\(a,i) -> (i,abs (v-i))) $ rid_arg_img f n


rd_dir v n f =  if (abs((f.rd_pred) n - v)) < (abs (f n) - v) then
                        LT
                else compare (abs((f.succ) n - v)) (abs (f n) - v)

--ratio closest arg
--not really useful yet :(
r_cl_arg v n f f' =     if cmp == EQ then 
                                Nothing else
                                if cmp == GT then 
                                        Just False else
                                        Just True
                        where cmp = compare (abs (v - f' n)) (abs (v - f n))


r_asym'  n k f = if (n - v1) > (n - v2) then v1 else v2
        where   v1 = f ((k/2))
                v2 = f ((k/2))
                nm = numerator n

--not sure, strange type of interpolation? have to read about it...
--TODO: read about it ;)
--r_ipl v =       map (\(s,f) -> head$dropWhile (\n -> s*n < s*v) $ map f [1..]) $ 
--                zip 
--                       (cycle [1,-1]) 
--                       [(\n -> 2^2^n), (\n -> 2^n), (\n -> n^2), (\n -> 2*n), id]


--it wasn't that bad
--give scanl a chance
-- v is target value
-- g is the function
-- i is initial value
i_ipl g i v =
	let funcs =
		[(\n -> 2^2^n), (\n -> 2^n)]
			++
		(map (\k -> (\n -> n^k)) $ [2048,2044..256]++[252,250..2])
		  	++
		map (\k -> (\n -> 2^k*n)) [64,56..0]
	in

	scanl (\k f -> 
		(\lst -> if lst == [] then k else last lst) $ 
		fst.span ((<=v).g) $ map ((k+).f) [0..] -- you see the g? ;)
	) i $ concatMap ((take 4).repeat) funcs ++ [id]

i_ipl1 g v = i_ipl g 1 v

--i_ipl is nice, but builds too big values if used with (2^)
--so i build a less aggressive one
i_ipl_soft g i v =  
	scanl (\k f -> 
		(\lst -> if lst == [] then k else last lst) $ 
		fst.span ((<=v).g) $ map ((k+).f) [0..] -- you see the g? ;)
	) i $ concatMap ((take 2).repeat) (map (*) [8,4,3,2]) ++ (map (+) [16,15..1])++[id]
--iexp again
i_ipl_soft1 g v = i_ipl_soft g 1 v

ilog base = last.(i_ipl_soft1 (base^))

ipl g i = last . i_ipl g i

--now we have a fast isqrt
isqrt 0 = 0
isqrt n = ipl (^2) (2^(floor.(%2).pred.length.listBits) n) n

isqrt' n 
            | (.) (^2) isqrt n == n = isqrt n
            | otherwise = succ $ ipl (^2) (2^(floor.(%2).pred.length.listBits) n) n

--check if a num is a (perfect) square
is_square n 
                | n == 0 = True
		| elem (n `intAnd` 0xFF) last_square_bits_8 = (isqrt n)^2 == n
		| otherwise = False


quad_residues m = map ((.) (flip mod m) (^2)) [0..floor(m%2)]
-- r_log again

-- precision k means : go in 1/(2^k) steps
r_nlog k n = head $ dropWhile (\i -> (r_exp 32) i < n) $ map (*(1%(2^k))) [0..]


-- very simple isqrt, bad at big nums
--isqrt n = (pred.head.dropWhile ((<=n).(^2))) [(2^(floor.(%2).pred.length.listBits) n)..]


--
-- next chapter
-- data types / math structures...
--
--
--FGroup = finite group
--ok, first let's realize ZGroup (modulo)
data Structure = ZGroup Size --((Eq a) => [a->a])
        | FMonoid Size
        deriving (Show)
type Size = Int

--in_struc2 (Structure s) f a b = 
struc_func (ZGroup s) = (`mod` 7)

infix +*

(+*) = (\a b -> (\struc -> (struc_func struc) (a+b)))

--data GF = GF Size
  --plus (G)
--elements

--data GF_Element = GElement Int Int



--VISUALIZE Ratio
--
--
--TODO: Finish this
showRatio lst = (\(a,b) -> 
                (mapM (putStr.(imbed ' ').show) a) >> 
                (putStrLn "") >> 
                (mapM (putStr.(imbed ' ').show) b) >> 
                (putStrLn "")) $ 
                unzip $ 
                map (\n -> (numerator n, denominator n)) lst


-- [COMBINATORICS/STOCHASTICS]


--of course, the well-known choose function
--choose n k = numerator $  (fac n)%((fac k)*(fac (n-k)))

--slightly better in my opinion:
choose n k = div (product $ takeWhile (>mk) [n,(pred n)..] ) $ fac (n-mk)
        where mk = max k (n-k)
--ok, let's make it recursive
--
--recursive, VERY SLOW!
--choose n 0 = 1
--choose n k | n == k = 1
--choose n k | (k > n) || (k < 0) = 0
--choose n k = choose (pred n) (pred k) + choose (pred n) k
--poor :( , to be continued ...

permutations ([]) = [[]]
permutations list =
  concatMap (\e ->
    map (\lst -> (fst e):lst) $ permutations $ tail $ rotate list (snd e)
  ) $ zip list [0..]

--perm_apply perm = 
--build all possible pairs

perm_pairs = concatMap (\lst -> map (\e -> [head lst, e]) $ tail lst).(takeWhile ((>1).length)). tails
