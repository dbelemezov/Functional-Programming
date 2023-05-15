main :: IO()
main = do
 --print $ isPrimeDictionary t1 vocabulary-- → False
 --print $ isPrimeDictionary t2 vocabulary --→ False
 --print $ isPrimeDictionary t3 vocabulary --→ True

type Vocabulary = [String]

data BTree = Nil | Node Char BTree BTree
 deriving (Show)

vocabulary :: Vocabulary
vocabulary = ["the", "a", "Some", "swimming", "liStS", "lisp"]

t1 :: BTree
t1 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 'S' Nil Nil)) (Node 'a' (Node 't' Nil Nil) (Node 'S' Nil Nil)))

t2 :: BTree
t2 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 's' Nil Nil)) (Node 'p' (Node 'p' Nil Nil) (Node 'S' Nil Nil)))

t3 :: BTree
t3 = Node 'a' (Node 't' (Node 'l' Nil Nil) (Node 'i' Nil Nil)) (Node 'h' (Node 's' Nil Nil) (Node 'p' Nil Nil))

substring :: String -> String ->Bool
substring [] [] = True
substring _ [] = False
substring s1@(x:xs) s2@(y:ys)
 | x==y=xs == take (length xs) ys || substring s1 ys
 | length xs == length ys = s1==s2
 | otherwise = substring s1 ys

getLevel :: BTree -> Int-> [Char]
getLevel Nil _ = []
getLevel (Node value left right) k
 | k==0 = [value]
 | otherwise = getLevel left (k-1) ++ getLevel right (k-1)

isPrime :: Int -> Bool
isPrime 1 = False 
isPrime n 
 | n<1 = error "error"
 | otherwise = helper 2 
  where
      helper :: Int -> Bool 
      helper current 
       | current ==n = True 
       | mod n current == 0 = False 
       | otherwise = helper (current +1)

height :: BTree -> Int 
height Nil = 0
height (Node _ l r)=1+max(height l)(height r)

isPrimeDictionary :: BTree -> Vocabulary -> Bool
isPrimeDictionary Nil - = False 
isPrimeDictionary btree@(Node val l r) xs = isPrime helper btree 
 where 
     helper :: 
     helper tree....

