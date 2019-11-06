import Data.List (length)

--data N = N Char
--data T = T Char
-- = T | N

data Rule = Rule Char [Char]
            deriving (Show,Read,Eq)
data Graph = Vertix [Edge] Bool
            | Leaf Bool
            deriving (Show,Read,Eq)
data Edge = Edge Graph Char [Char]
            deriving (Show,Read,Eq)

creategraph :: [Rule] -> Graph
creategraph = foldr (addRule) (Leaf False)

addRule :: Rule -> Graph -> Graph
addRule (Rule _ []) (Leaf _) = Leaf True
addRule (Rule _ []) (Vertix edges _) = Vertix edges True
addRule (Rule nt (symb:symbs)) (Leaf cond) = Vertix [Edge (addRule (Rule nt symbs) (Leaf False)) symb [nt]] cond
addRule (Rule nt (symb:symbs)) (Vertix edges cond) | (foldr (||) False (map (isNtAndSymb symb nt) edges) ) == True = Vertix (map (rebuildEdgeSnN symb nt symbs) edges) cond
                                                   | (foldr (||) False (map (isSymb symb) edges) ) == True = Vertix (map (rebuildEdgeS symb nt symbs) edges) cond
                                                   | otherwise = Vertix (edges ++ [Edge (addRule (Rule nt symbs) (Leaf False)) symb [nt]]) cond
isNtAndSymb :: Char -> Char -> Edge -> Bool
isNtAndSymb symb nt (Edge _ s nts) = ((symb == s) && (isIn nt nts))

isSymb :: Char -> Edge -> Bool
isSymb symb (Edge _ s nts) = (symb == s)

rebuildEdgeSnN :: Char -> Char -> [Char] -> Edge -> Edge
rebuildEdgeSnN symb nt symbs (Edge g s nts) | (s == symb) = Edge (addRule (Rule nt symbs) g) s nts
                                            | otherwise = (Edge g s nts)

rebuildEdgeS :: Char -> Char -> [Char] -> Edge -> Edge
rebuildEdgeS symb nt symbs (Edge g s nts) | (s == symb) = Edge (addRule (Rule nt symbs) g) s (nts ++ [nt])
                                          | otherwise = (Edge g s nts)


isIn a [] = False
isIn a [b] | a==b = True
           | otherwise = False
isIn a (b:bs) | a==b = True
              | otherwise = isIn a bs
{-rebuildEdge :: Char -> Char -> [Char] -> Edge
rebuildEdge symb nt symbs -}
                          {-addRule (Rule nt (symb:symbs)) x = if (isSymbInEdges symb x) then (if (isThisEdge symb nt x) then (addInGraphByGoTo symb nt x) else (addInGraphByAddingNT symb nt x)) else (addInGraphByNewEdge symb nt x)
                                    where monad State check this --
                                        isSymbInEdges :: Char -> Graph -> Bool
                                        isThisEdge :: Char -> Char -> Graph -> Bool
                                        addInGraphByGoTo :: Char -> Char -> Graph -> Graph
                                        addInGraphByAddingNT :: Char -> Char -> Graph -> Graph
                                        addInGraphByNewEdge :: Char -> Char -> Graph -> Graph
                                        isSymbInEdges symb (Vertix edges _) = (foldr (checksymb symb) False) edges
                                                        where
                                                            checksymb::Char -> Edge -> Bool -> Bool
                                                            checksymb symb (Edge _ symb1 _) False = if (symb == symb1) then True else False
                                                            checksymb _ _ True = True
                                        isThisEdge symb nt (Vertix edges _) = (foldr (checksymbplusnt symb nt) False) edges
                                                        where
                                                            checksymbplusnt :: Char -> Char -> Edge -> Bool -> Bool
                                                            checksymbplusnt _ _ _ True = True
                                                            checksymbplusnt symb nt (Edge _ symb1 nts) False = if ((symb == symb1) && (isIn nt nts)) then True else False
                                                                    where
                                                                        isIn :: Char -> [Char] -> Bool
                                                                        isIn x = foldr (thesame x) False
                                                                            where
                                                                                thesame :: Char -> Char -> Bool -> Bool
                                                                                thesame _ _ True = True
                                                                                thesame x y False = if (x == y) then True else False

                                        addInGraphByGoTo symb nt x = x
                                        addInGraphByAddingNT symb nt x = x
                                        addInGraphByNewEdge symb nt x = x-}



{-
data Literal = Var Int
             | Not Int
             deriving (Show,Read,Eq)
type Disj = [Literal]
type Array = [Disj]
f :: Disj -> Disj -> Disj
f [] b = b
f _ [] = []
f [a] (b:bs) | a == snot b = bs
             | bs==[] =[b]
             | otherwise = [b] ++ (f [a] bs)
f (a:as) (b:bs) | f [a] (b:bs) /= (b:bs) =  as ++ (f [a] (b:bs))
                | test (a:as) (b:bs) ==False = (b:bs)
                | otherwise =  a : f as (b : bs)

simplify::Disj->Disj
simplify [] = []
simplify [a] = [a]
simplify (a:as) | test [snot a] as == True = simplify as
                | otherwise = a : simplify as


union:: Array->Disj->Array
union [a] b = [f b a]
union (a:as) b = (f b a) : union as b

sunion:: Array->Disj->Array
sunion a b = a ++ union a b


testAr:: Array->Disj->Bool
testAr [a] b | a == b = True
             | otherwise = False
testAr (a:as) b | a == b = True
                | otherwise = testAr as b





test::Disj->Disj->Bool
test [] _ = False
test _ [] = False
test [a] (b:bs) | a == snot b = True
                | bs== [] = False
                | otherwise = test [a] bs
test (a:as) (b:bs) | test [a] (b:bs) = True
                   | otherwise = test as (b:bs)

proof:: Array -> Bool
supproof:: Array -> Array
newbuf:: Array -> Array -> Array -> Array -> Array
isIn:: Disj -> Array -> Bool
isEmp:: Array -> Bool
isEmp [] = False
isEmp [a] | a == []  = True
          | otherwise = False
isEmp (b:bs) | b == [] = True
             | otherwise = isEmp bs


isIn a [] = False
isIn a [b] | a==b = True
           | otherwise = False
isIn a (b:bs) | a==b = True
              | otherwise = isIn a bs

supproof [a] = []
supproof as =  newbuf as as as []

newbuf as [] [] buf = buf
newbuf as [a] [] buf = buf
newbuf as [a] (b:bs) buf | (isIn (f a b) as) == True = newbuf as [a] bs buf
                         | otherwise = newbuf as [a] bs ((f a b) : buf)
newbuf as (b:bs) (c:cs) buf = newbuf as bs (c:cs) (newbuf as [b] (c:cs) buf)


proof as |  isEmp as == True = True
         |   isEmp (supproof as) == True =True
         |  (supproof as) == [] = False
         | otherwise  = proof  (as ++ (supproof as))

--commet--
snot :: Literal->Literal
snot (Var x) = (Not x)
snot (Not x) = (Var x)

first :: Disj->Disj
first (a:as) = [a]

remove :: Disj->Disj
remove (a:as) = as


len1:: Disj-> Bool
len2::Disj-> Bool
len3:: Disj->Bool

len1 a | length a == 1 = True
       | otherwise = False

len2 a | length a == 2 = True
       | otherwise = False

len3 a | length a > 2 = True
       | otherwise = False

-}
