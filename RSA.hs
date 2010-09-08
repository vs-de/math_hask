module RSA where

import Prime
import Random
import StdLib

data RSAKey = 

        PrivateKey {
	    rsa_mod :: Integer,
	    pub_exp :: Integer,
            prv_exp :: Integer,
	    fst_prime :: Integer,
	    snd_prime :: Integer,
	    dmp1 :: Integer,
	    dmq1 :: Integer,
	    iqmp :: Integer
	}
    |
        PublicKey {
            rsa_mod :: Integer,
            pub_exp :: Integer
}
    deriving (Eq, Show)
    

-- lambdabot's version of (p-1)*(q-1)
phi = (. pred).(*).pred

gen_facs bits = randomRIO (2^pred bits,2^bits) >>=
        (\p -> 
            randomRIO (2^pred bits, 2^bits) 
            >>= (\q ->return (next_prime p, next_prime q))
        )

--mult. inverse to mod m
inv_mod n m = (`div` n).succ.(*m).fst.head $ dropWhile (\(a,b) -> b /= pred n) $ keep_map (\k -> mod (k*m) n) [1..]

new_key_pair x y = 
    (
        PrivateKey {
	    rsa_mod = x*y,
	    pub_exp = e,
	    prv_exp = d,
	    fst_prime = x,
	    snd_prime = y,
	    dmp1 = d `mod` pred x,
	    dmq1 = d `mod` pred y,
	    iqmp = 0 -- TODO: add this
	},
	PublicKey {
	    rsa_mod = (x*y),
	    pub_exp = e
	}
    )
    where d = inv_mod e (phi x y)
          e = 65537

rsa_enc (PublicKey m e) plain = bin_exp_mod plain e m
rsa_dec pk secret = bin_exp_mod secret (prv_exp pk) (rsa_mod pk)

gen_rsa bits =
    gen_facs bits >>= return.uncurry new_key_pair

