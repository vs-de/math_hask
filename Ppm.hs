-- prediction by partial matching

-- p position, b back
-- slide p b =

slides n str = map (\k -> (drop (k-n) . take (k)) str ) [0..]

