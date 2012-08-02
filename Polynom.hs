module Polynom where

import StdLib
import Data.Ratio
import Data.List


--polynom detection
--
--at the beginning we take only 2nd grade polynomials
--
--first we have to extract the square coefficient 


sq_coeff lst = numerator $ ((head $ listDiffs $ listDiffs lst)%2)

-- 2nd coeff through list delta
-- next time using dropWhile instead of filter
snd_coeff lst = (head $ listDiffs lst)-(head $ filter (\n -> n >= (head $ listDiffs lst)-cc) $ listDiffs $ map (\n -> cc*n^2) [1..])
  where cc = (sq_coeff lst)
-- 3rd coeff through final delta, ok, ugly implemented
thrd_coeff lst = maybe 0 (\n -> (head lst)-(pol (toInteger n))) $ elemIndex (head $ listDiffs lst) $ listDiffs $ map pol [0..]
  where pol = (\n -> (sq_coeff lst)*n^2+(snd_coeff lst)*n)

--gives the detected coeffs
sqCoeffs lst = (sq_coeff lst, snd_coeff lst, thrd_coeff lst)

--returns the square polynomial as a function
detect_sq_pol lst = (\n -> (\(a,b,c) -> a*n^2+b*n+c) $ sqCoeffs lst)
show_sq_pol lst = (\(a,b,c) -> "(\\n -> ("++(show a)++")*n^2+("++(show b)++")*n+"++(show c)++")") $ sqCoeffs lst
show_sqCoeffs lst = (\(a,b,c) -> "("++(show a)++","++(show b)++","++(show c)++")") $ sqCoeffs lst


--this should work on infinite lists
-- must be ordered (asc)

closest_elem_idx' n (x:[]) = 0
closest_elem_idx' n lst = --if or $ map (\k -> length lst' == k) [0,1] then
                          if lst' == [] then
                            pred $ length lst
                          else
                            if idx_ == 0 then
                              0
                            else
                              if abs(n-(lst !! idx_)) < abs (n-(lst !! (pred idx_))) then
                                idx_
                              else
                                pred idx_
  where lst' = dropWhile (\k -> k < n) lst
        idxOf e xs = head $ elemIndices e xs --avoiding maybe, because i know, elem exists
        idx_ = idxOf (head lst') lst

closest_elem' n lst = lst !! (closest_elem_idx' n lst)

-- for finite Lists...
-- here is a working but fucking slow implementation wich is very "UNDRY"
-- apply only to finite lists!

closest_elem_idx n lst = head $ elemIndices m diffs
  where m = minimum diffs
        diffs = map (\k -> abs(k-n)) lst

closest_elem n lst = lst !! (closest_elem_idx n lst)

--
-- if i have some time i will improve it, promised ;)
--
-- oh, this is beauty
--detect_poly_degree lst =  unfac $ head $ head $
--                          dropWhile (\lst -> ((length lst) > 1) && ((head lst) /= (head (tail lst))) ) $
--                          map (\n -> repf n listDiffs $ lst) [1..]
--oh, i forgot the first coeff
--how to calc this out? -shit
--have to count the applications of listDiffs

detect_poly_degree lst =  fst $ head $
                          dropWhile (\(n, lst) -> ((length lst) > 1) && ((head lst) /= (head (tail lst))) ) $
                          map (\n -> (n, repf n listDiffs $ lst)) [1..]

-- if we also want to calc the coeff
detect_poly_degree' lst = head $
                          dropWhile (\(n, lst) -> ((length lst) > 1) && ((head lst) /= (head (tail lst))) ) $
                          map (\n -> (n, repf n listDiffs $ lst)) [1..]


--simple monomial with coeff
smon c e = (\n -> c*n^e)

--1. get degree = highest exp
--2. get 1st coeff -> numerator (head $ repf degr...) % fac degr
--3. build 1st polynomial ... closestElement.
--4. goto 2.
--
--in other words, just:
-- poly -> list -> poly -> list ...
{-
detect_poly_coeffs lst = foldl1 addpol $ map (\(a,b) -> smon b a) init_
        where   dgr = detect_poly_degree' lst
                addpol = (\f f' n -> (+)(f n) (f' n))
                smonFor n = closest_elem' (head $ repf n listDiffs $ lst)
                --polFor n = foldl1 addpol 
                init_ = map (\n -> ((numerator $ (snd dgr)%(fac (fst dgr))), n)) $ reverse [1..(fst dgr)]
--}

--find_pc (x:[]) xs' = []

--YEAH, here comes the recursive polynomial detector

find_monomials lst ((c,e):xs) = 
        if e == (0) then
                (c, e):xs
        else
                find_monomials lst (next_:nl_)

        where 
                diffs_ n = repf n listDiffs lst
                elem_ = head $ diffs_ (pred e)
                next_ = (numerator $ ((-) elem_ (closest_elem' elem_ $
                        repf (pred e) listDiffs $ map poly_ [0..]))%(fac (pred e)), e-1)
                nl_ = (c,e):xs
                addpol = (\f f' n -> (+)(f n)(f' n))
                poly_ = foldl1 addpol $ map (\(c,e) -> smon c e) nl_
                

detect_monomials' lst =
                reverse $ find_monomials lst [init_]
        
        where   dgr = detect_poly_degree' lst
                init_ = (numerator $ (head $ snd dgr)%(fac (fst dgr)), (fst dgr))

detect_monomials = (\lst -> filter (\(a,b) -> a /= 0) $ detect_monomials' lst)

detect_polynomial lst = poly_ $ detect_monomials lst
        where   addpol = (\f f' n -> (+)(f n)(f' n))
                poly_ = \lst -> foldl1 addpol $ map (\(c,e) -> smon c e) lst


--this is not ready
detect_exp lst = (mult_, base_)
        where base_ = head $ pair_map_rev (/) rlst
              mult_ = (.) head (zipWith (/) rlst) (map plain_f [1,2])
              plain_f = (\n -> base_^n)
              rlst = map toRational lst


