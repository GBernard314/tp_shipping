module Maps where
groupe :: [String]
groupe = ["DESOOMER Pierre", "BERNARD Guillaume"]

-- | Maps are lists of key/value pairs

-- !IMPORTANT! keys must be kept unique!
type MyMap k v = [(k, v)]

-- | The empty map
lempty :: MyMap k v
lempty = []

-- | The map with a single key/value pair
lsingleton :: k -> v -> MyMap k v
lsingleton k v = [(k,v)]

-- | Insert a key value pair into the map.
-- On key collision, the value is overwritten.
linsert
  :: Eq k
  => k
  -> v
  -> MyMap k v
  -> MyMap k v
linsert key value m = case m of
  [] -> lsingleton key value
  (k,v):t' -> if(key == k)
    then (k,value):t'
    else (k,v) : linsert key value t'

   

-- | Remove a key/value pair based on a key.
ldelete
  :: Eq k
  => k
  -> MyMap k v
  -> MyMap k v
ldelete key m = case m of 
  [] -> m
  (k,v) : m' -> if k == key
    then m'
    else (k,v) : ldelete key m'


-- | Finds the value associated with a key, if it exists.
llookup
  :: Eq k
  => k
  -> MyMap k v
  -> Maybe v
llookup key m = case m of 
  [] -> Nothing
  (k,v) : m' -> if k==key
    then Just v
    else llookup key m'

-- | Finds the largest value, if it exists.
lmaximum
  :: Ord v
  => MyMap k v
  -> Maybe v
lmaximum m = case m of
  [] -> Nothing
  (k,v) : m' -> case lmaximum m' of
    Nothing -> Just v
    Just mx -> Just (max v mx)  

-- | Create a Map from a list. Althrough they have the same type,
-- the "MyMap" must preserve the unicity key invariant. Keys that appear
-- earlier in the input list are overwritten on collision.
fromList
  :: Eq k
  => [(k, v)]
  -> MyMap k v
fromList l = go lempty l
  where
    go acc l = 
      case l of 
        [] -> acc
        (k,v):xs -> go (linsert k v acc) xs
  

-- | Do you understand what this function is? Even if you do not, can you
-- write it?
lfold :: (v -> a -> a)
      -> a
      -> MyMap k v
      -> a
lfold f z np = case np of 
  [] -> z
  (_, v):mp' -> lfold f (f v z) mp'
-- | Same thing, with values
lfoldWithKeys
  :: (k -> v -> a -> a)
  -> a
  -> MyMap k v
  -> a
lfoldWithKeys = undefined

-- | Sum of all the values in the map. Implement it with lfold.
lsum
  :: MyMap k Integer
  -> Integer
lsum = undefined

-- | Finds the smallest value, if it exists. Implement it with lfoldWithKeys.
lminimum
  :: Ord v
  => MyMap k v
  -> Maybe v
lminimum = undefined

-- | Intersection of two maps. You should use lfoldWithKeys.
intersectionWith
  :: Eq k
  => (a -> b -> c)
  -> MyMap k a
  -> MyMap k b
  -> MyMap k c
intersectionWith = undefined

-- | optional assignment, only for the brave!
mergeA
  :: (Eq k, Applicative f)
  => (k -> a -> f c)  -- when it is only in map 1
  -> (k -> b -> f c)  -- when it is only in map 2
  -> (k -> a -> b -> f c) -- when it is in both maps
  -> MyMap k a -- first map
  -> MyMap k b -- second map
  -> f (MyMap k c) -- merged map
mergeA = undefined
