type Assoc k v = [(k,v)]

--find :: Eq k => k -> Assoc k v -> v
find :: Eq k => k -> [(k, v)] -> v
find k t = head [v | (k', v) <- t, k == k']

table = [(1, "Donald"), (2, "Cindy"), (3, "An")]

