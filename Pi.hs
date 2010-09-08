module Pi where

import StdLib
import Ratio


-- BBP --


-- subtraction part

bbp_sub = (\k -> (4/(8*k+1)) - (2/(8*k+4)) - (1/(8*k+5)) - (1/(8*k+6)) )

-- main sum
bbp_main_summand = (\k -> (1/(16**k)) * (bbp_sub k) )

bbp_sum n = sum $ map bbp_main_summand [0..n]

bbp_seq_summand = (\n -> (120*n^2 - 89*n+16) % (512*n^4-1024*n^3+712*n^2-206*n+21))

bbp_seq_fact 0 = 0
bbp_seq_fact k = 16*(bbp_seq_fact (k-1))+(bbp_seq_summand k)

bbp_seq_raw n = 16 * (bbp_seq_fact n)

bbp_seq n = floor (16 * (bbp_seq_fact n))




-- +++++++++++++++++++
-- nth hex-digit of pi
-- +++++++++++++++++++
-- 4 S(1) - 2 S(4) - S(5) - S(6)
-- for computational use, this is more helpful:
-- 16^n S(j) = 

--pi_S :: (Integral a) => a -> Ratio a
--pi_S n = pi_std_part0 0 n

--pi_std :: Ratio a
--
--
pi_std = 4*(pi_S 1) - 2*(pi_S 4) - (pi_S 5)  - (pi_S 6)

pi_S j = (pi_std_part0 100 j) + (pi_std_part1 100 j)

pi_std_part0 n j = (sum $ map (\k -> ((16^(n-k)) `mod` (8*k+j))%(8*k+j)) [0..n])

-- unlimited part 2
pi_std_part1 :: (Integral a) => a -> a -> Ratio a
pi_std_part1 n j = sum $ map (\k -> 1 % (16^(k-n) * (8*k+j))) [(n+1)..(n+10001)]

--psp1 = (\k -> 1 % (16^(k-n) * (8*k+j)))





-- first 1000 hex-digits of pi
big_pi = 0x3243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89452821e638d01377be5466cf34e90c6cc0ac29b7c97c50dd3f84d5b5b54709179216d5d98979fb1bd1310ba698dfb5ac2ffd72dbd01adfb7b8e1afed6a267e96ba7c9045f12c7f9924a19947b3916cf70801f2e2858efc16636920d871574e69a458fea3f4933d7e0d95748f728eb658718bcd5882154aee7b54a41dc25a59b59c30d5392af26013c5d1b023286085f0ca417918b8db38ef8e79dcb0603a180e6c9e0e8bb01e8a3ed71577c1bd314b2778af2fda55605c60e65525f3aa55ab945748986263e8144055ca396a2aab10b6b4cc5c341141e8cea15486af7c72e993b3ee1411636fbc2a2ba9c55d741831f6ce5c3e169b87931eafd6ba336c24cf5c7a325381289586773b8f48986b4bb9afc4bfe81b6628219361d809ccfb21a991487cac605dec8032ef845d5de98575b1dc262302eb651b8823893e81d396acc50f6d6ff383f442392e0b4482a484200469c8f04a9e1f9b5e21c66842f6e96c9a670c9c61abd388f06a51a0d2d8542f68960fa728ab5133a36eef0b6c137a3be4ba3bf0507efb2a98a1f1651d39af017666ca593e82430e888cee8619456f9fb47d84a5c33b8b5ebee06f75d885c12073401a449f56c16aa64ed3aa62363f77061bfedf72429b023d37d0d724d00a1248db0fead3

-- +++++++++++
-- Alt Methods
-- +++++++++++
-- small tests first
pi_alt0_short = fromRational $ sum $ map (\k -> (((-1)^k)%(4^k)) * (2%(4*k+1)+2%(4*k+2)+1%(4*k+3))) [0..50]
pi_alt0_100 = floor $ 10^100 * (sum $ map (\k -> (((-1)^k)%(4^k)) * (2%(4*k+1)+2%(4*k+2)+1%(4*k+3))) [0..500])

-- ok...

pi_alt0_part0 k = (-1)^k % (4^k)
pi_alt0_part1 k = 2%(4*k+1) + 2%(4*k+2) + 1%(4*k+3)
pi_alt0_sum_to n = sum $ map (\k -> pi_alt0_part0 k * pi_alt0_part1 k) [0..n]

pi_var0_inner = (\k -> 1%(4^k) * (pi_alt0_part1 k))
pi_var0_neg_sum n = sum $ map pi_var0_inner [1,3..n]
pi_var0_pos_sum n = sum $ map pi_var0_inner [0,2..n]
pi_var0_sum_to n = (pi_var0_pos_sum n) - (pi_var0_neg_sum n)

--
--pi_var1_part0_numerator k = 80*k^2 + 84*k + 20
--pi_var1_part0_denominator k = 64*k^3+96*k^2+44*k+6
--

--shortend:
pi_var1_part0_numerator k = 40*k^2 + 42*k + 10
pi_var1_part0_denominator k = 32*k^3+48*k^2+22*k+3

-- factor for denominator
pi_den_fact k = 4^k

pi_var1_part0_ratio n = (%) (pi_var1_part0_numerator n) (pi_var1_part0_denominator n)

pi_var1_ratio n = (%) (pi_var1_part0_numerator n) ((pi_den_fact n) * (pi_var1_part0_denominator n))


--with part0:
--pi_var1_inner = (\k -> 1%(4^k) * (pi_var1_part0_ratio k))

pi_var1_inner = pi_var1_ratio
pi_var1_neg_sum n = sum $ map pi_var1_inner [1,3..n]
pi_var1_pos_sum n = sum $ map pi_var1_inner [0,2..n]
pi_var1_sum_to n = (pi_var1_pos_sum n) - (pi_var1_neg_sum n)

-- BBP END --


