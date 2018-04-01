
--ASSIGNMENT #1 Rustam Quliyev e225047 
--PART 2
--    |
--    |
--    |
--    |
--    |
--    |
--    v
data Tree2 a b c = Node String Int Float [Tree2 String Int Float] deriving (Show, Eq)
-- The definition of my Tree structure 



magic :: Int->Int->[a]->[a]
magic i take xs = [ xs!!x|x<-[i..(i+take)]]
-- This magic function takes "i" which is the index of the element to be taken "take" which is the number of elements to be taken after the index "i" and xs which is the actual array
manage :: Int->[Int]->Int->Int->[a]->[a]
manage i (b:bs) hash take (x:xs) = (magic (i-1) take xs)++ (manage (new_i) ((bs!!(i-1)):(drop hash bs)) (sum_hash)     (new_take)      ((xs!!(i-1)):(drop hash xs)) )      
                        where sum_hash = sum[ bs!!x|x<-[0..(hash-1)]]
                              new_take = sum[bs!!x| x<-[(i-1)..(i-1+take)]]
                              new_i = if(i==1) then  1 
                                      else (sum_hash - (sum [ bs!!x|x<-[i-1..hash-1]]) +1) 
-- This manage function is actually the main function to be noted. This function takes an array and several other parameter listed below and returns the parsed version of the same array but with all the child nodes.
{-
    Parameters::
          "i" (Int) This is the index of child relative to which we are going to parse the entire array
          "(b:bs)" (Array of INT) Is my interger array of the number of childs of each node. We need this in order to compute the number of childs of each node and parse respectively 
          "hash" (Int) It is the number of elements which share the same level(depth) as the element that I am dealing right now(recursive step)
          "take" (Int) This is the number of childs That my node should have. Note that we we first call this function in the managementtree2 function take is 0 Since on the first run we are taking only 1 child
          "(X:xs)" (Array of type a) This is the array which We are parsing Note it can be an array of any type since our tree definition can have multiple types 
          Note that in the manage function I also have some auxillary functions. Namely "sum_hash", "new_i","new_take"
          Sum_hash function returns the number of elements on one level below so when I recursively call my function to a level below if knows the numbers of nodes in the same depth.
          new_i Rreturns the number of elements that will belong to the node on which I am operating on one level below 
``        New_take returns the number of element that are needed to be included in the new level or in other words during the next step of recursion



-}
managementtree2 :: [String] ->[Int]->[Float]->Tree2 String Int Float
managementtree2 a b c = Node (head a) (head b) (head c) [ managementtree2 (manage i b (head b) 0 a) (manage i b (head b) 0 b) (manage i b (head b) 0 c)  |i<-[1..head b]]
--A simple function definition of managemenettree2 using list comprehention. 



my_tree2=managementtree2 ["Rector", "Vice President", "Dean Social Sciences", "Dean Engineering", "Vice President Board of Social Sciences", "Vice President Board of Engineering", "Officer Research Coordination" , "Director of Human Resources", "Director of Accounting", "Director of Student Affairs" ] [3,0,1,3,0,0,0,2,0,0] [100000, 80000, 70000, 70000, 50000, 50000, 30000, 50000, 40000, 40000]



--    ^
--    |
--    |
--    |
--    |
--    |
--    |
--PART 2




--PART 1
--    |
--    |
--    |
--    |
--    |
--    |
--    v

--NOTE: In the Tree definition I have used RECORD SYNTAX so output will be slightly different(more detailed) but the pattern works correctly !


data FullBTree = Leaf {name :: String, salary :: Float}| Vertex  {name :: String, salary :: Float, left :: FullBTree, right:: FullBTree}  deriving (Show)

-- The definition of my Tree structure 

-- 0 Left
-- 1 Right


managementtree a b [] [] = Leaf (last a) (last b)
managementtree a b c d  = Vertex (last c) (last d) (  managementtree (take (length a `div` 2) a) (take (length b `div` 2) b) (modify (reverse c)  0) (modify (reverse d) 0) ) (  managementtree (drop (length a `div` 2) a) (drop (length b `div` 2) b) (modify (reverse c) 1) (modify (reverse d) 1) ) where
            modify a 1= reverse [ a!!j | i<-[1.. truncate (logBase 2.0 (fromIntegral (length a)))], j<- [(((2^i)-1)) ..  (((2^i + 2^(i-1))-2))] , j< length a ]
            modify a 0= reverse [ a!!j | i<-[1.. truncate (logBase 2.0 (fromIntegral (length a)))],j<- [(((2^i) + (2^(i-1)) -1)) ..  ((2^(i+1)-2))], j< length a]
-- This modify function takes an array and takes either the elements for the right subtree (input 1) or left subtree (input 0) 
--The key point in the managementtree function is the implementation of the modify tree function 

my_tree=managementtree ["Vice President Board of Social Sciences", "Director of Human Resources", "Vice President Board of Engineering", "Officer Research Coordination"] [50000, 50000, 50000, 30000] ["Dean Social Sciences", "Dean Engineering", "Rector"] [70000, 70000, 100000]
--Tree has been created!!!!

supervisor:: FullBTree->String->String -- Definition 
supervisor (Leaf _ _) n = ""
supervisor t n | (name t) == n = "No supervisor"
               | ((name(right t))==n ||(name(left t))==n) = show (name t)
               | n==[] = "Incorrect input"
               | otherwise= (supervisor (right t) n) ++ (supervisor (left t) n) -- The recursive algorithm


peers (Leaf _ _) n = ""
peers t n | (name t)==n = "No peers"
          | name(right t)==n = name(left t)
          | name(left t) == n = name(right t)
          | otherwise = (peers (right t) n) ++ (peers (left t) n)


manager (Leaf _ _) n = []
manager t n = if(name t ==n ) then "No manager " 
              else if ( (name (right t))==n || (name (left t)) ==n ) then name t
              else if (manager (right t) n /=[]) then manager (right t) n++" "++  name t 
              else if (manager (left t) n /=[] ) then manager (left t) n++" "++ name t
              else ""


avgsalary t = salaryy t / nodes t where 
                                  nodes (Leaf _ _)= 1
                                  nodes t = 1 + nodes (left t) + nodes(right t)
                                  salaryy (Leaf _ b) = b
                                  salaryy t = ((salary t)+ salaryy (left t) + salaryy (right t)) --I have used 2 local function in order to evaluate the average salary 




                            