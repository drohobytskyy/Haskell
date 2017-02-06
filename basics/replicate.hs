replicatel :: Int -> a -> [a]
replicatel 0 x = []
replicatel n x = x : replicatel (n-1) x
