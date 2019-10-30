import Data.List (length)
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
