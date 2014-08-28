import Data.List 

--Define types for Variable and Environment and Query as per specification 
type Variable = [Char]
type Environment = [(Variable, Bool)]

data Query
    = S_var Variable
    | S_nand Query Query
    | S_not Query
    deriving(Show)

--Define equality for Query data type, where commutativity holds 
instance Eq Query where
    S_var a == S_var b = a == b
    S_not a == S_not b = a == b
    S_nand a b == S_nand c d = (a == c && b == d) || (a == d && b == c)
    _ == _ = False 

------------------------------------------
--Question 1------------------------------
------------------------------------------

-- |findVars returns a list of unique Variables in the given query 
findVars :: Query -> [Variable] 
findVars (S_var v) = [v]
findVars (S_nand q1 q2) = nub (findVars q1 ++ findVars q2)      --Removes duplicates
findVars (S_not q)  = findVars q


------------------------------------------
--Question 2------------------------------
------------------------------------------

-- |transform constructs a lamdba expression to evaluate a query given an environment, 
-- it can be invoked partially 
transform :: Query -> Environment -> Bool
transform (S_var v) env = case lookup v env of 
                            Nothing -> error "No variable found" 
                            Just b -> b

transform (S_not q) env = not (transform q env)
transform (S_nand q1 q2) env = not (and [transform q1 env, transform q2 env])


------------------------------------------
--Question 3------------------------------
------------------------------------------

-- |simplifyOnce will parse the given query once to reduce redundant expressions 
simplifyOnce :: Query -> Query
simplifyOnce (S_var v) = S_var v
simplifyOnce (S_not (S_not q)) = simplifyOnce q
simplifyOnce (S_not q) = S_not (simplifyOnce q)
simplifyOnce (S_nand q1 q2) = 
    let x = simplifyOnce q1
        y = simplifyOnce q2
    in if x == y 
        then S_not x
        else S_nand x y         

-- |simplify will recursively call simplifyOnce until no further reductions can be made
simplify :: Query -> Query
simplify q = 
    let x = simplifyOnce q
    in if x == q
        then x
        else simplify x


-------------------------------------------
--Testing----------------------------------
-------------------------------------------


myEnv = [("a", True), ("b", False), ("c", True)]
myEnv1 = [("a", False), ("b", True), ("c", True)]
a = "a"
b = "b" 
c = "c"
myQ = S_nand (S_var a) (S_nand (S_nand (S_var b) (S_var c)) (S_var a))
myQ1 = S_nand (S_not ( S_not ( S_var a))) (S_var b)
myQ2 = S_nand (S_var a) (S_var a)
myQ3 = S_nand (S_not  (S_var b)) (S_not (S_var b))
myQ4 = S_nand 
        (S_nand (S_not (S_var b)) 
                (S_not (S_not (S_var b)))
        ) 
        (S_nand (S_not (S_var a)) 
                (S_not (S_var a))
        )

myQ5 = S_nand 
        (S_nand (S_not  (S_nand 
                                (S_var b)
                                (S_var b))) 
                (S_not (S_not (S_var b)))) 
        (S_nand (S_not (S_var a)) 
                (S_not (S_var a)))
myQ6 = S_nand (S_var a) (S_var b)
myQ7 = S_nand (S_var b) (S_var a)
myQ8 = S_nand myQ6 myQ7


x = [myQ, myQ1, myQ2, myQ3, myQ4, myQ5, myQ8] 
e = [myEnv, myEnv1]

testQ :: IO()
testQ = do 
    print "Testing Question 1 : "
    print (map findVars x)
    print "Testing Question 2 : "
    let tq = map transform x
    print . map ($ myEnv) $ tq
    print . map ($ myEnv1) $ tq
    print "Testing Question 3 : "
    print (map simplify x) 
