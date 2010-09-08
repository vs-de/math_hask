module TRS where

import List
import Data.Tree
import StdLib

--type constructors
--process (Rule a) str = a

--matches :: Rule -> SubList-> String -> Bool

--example rules
--simple group axioms
r0 = [
        ("f(f(x,y),z)","f(x,f(y,z))"),
        ("f(e,x)","x"),
        ("f(i(x),x)","e")
    ]

-- (l,r): rule
-- sub_f: substitution
-- (s,t): terms s->t
-- without position, mmhh...

f_sym = ["f","g","h","i","m","n","p","q"]
c_sym = ["a","b","c","d","e"]
v_sym = ["u","v","w","x","y","z"]
signs = [")","(",","," "]

expl0 = "f(u,v,g(x,i(y)),z,h(i(g(y)),q(x),y),z)"

--divide terms
tdivide trm = (\(x:xs) -> x:(concatMap (\(y:ys) -> [[y],ys]) xs)) $ splitAt' trm all_idcs
                where all_idcs = sort $ concatMap (\str -> sublist trm str) signs

createNode a lst = Node {rootLabel = a, subForest = lst }


pos_parse trm' = map (\str -> ()) trm
        where trm = tdivide trm'

t2tree' []       = ([],[])
t2tree' (t:ts)  | (elem t c_sym) || (elem t v_sym) = ([createNode t []]++ fst_ ,snd_)
                | (t == ")") = ([], ts)
                | (elem t signs) = (fst $ t2tree' ts,snd_)
                | otherwise = ([createNode t fst_] ++ (fst $ t2tree' snd_),snd $ t2tree' snd_)
        where   fst_ = fst $ t2tree' ts
                snd_ = snd $ t2tree' ts

t2tree = head.fst.t2tree'.tclean.tdivide

showTermTree = putStr.drawTree.t2tree
tclean = filter (\t -> t /= " " && t /= "")

--list of positions
--

tposl' [] pos = []
tposl' (t:ts) pos       | t == "("      = tposl' ts (pos++[0])          -- depth increase
                        | t == ")"      = tposl' ts $ init pos          -- depth decrease
                        | t == ","      = tposl' ts pos                 -- ignore
                        | pos == []     = (t,[]):(tposl' ts [])         -- start pos (epsilon/lambda)
                        | otherwise     = (t, init pos++[succ (last pos)]) :
                                          (tposl' ts ((init pos)++[succ (last pos)]))


--long version of a brace checker
brace_parse' =   (.) (scanl1 (+))
                (map (\c -> if c == "(" then 1
                        else if c == ")" then (-1) 
                                else 0))

brace_parse = brace_parse'.tclean.tdivide
check_brace = (\lst -> last lst == 0 && maybe True (const False) (find (<0) lst)).brace_parse

--tposl lst = tposl' (tclean $ tdivide lst) []
--
--better is the point free style :)
tposl = ( `tposl'` []) . tclean . tdivide

symAt posl trm= fst $ head $(dropWhile (\(a,b) -> b /= posl) (tposl trm))

--apply_sub lst = 

tcmp' t1 t2 = map (uncurry (\a b -> 
    if a == b then 
        "" 
    else 
        if (.) elem fst b v_sym then
            fst a
        else
            "--E--"
    )) $ zip (tposl t1) (tposl t2)


