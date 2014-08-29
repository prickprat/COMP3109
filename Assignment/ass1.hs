import Data.List 
import Test.HUnit

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


a = "a"
b = "b" 
c = "c"
myEnv = [(a, True), (b, False), (c, True)]
myEnv1 = [(a, False), (b, True), (c, True)]
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


queries = [myQ, myQ1, myQ2, myQ3, myQ4, myQ5, myQ8] 

--Build Unit tests for findVars
expected = [[a,b,c], 
            [a,b],
            [a],
            [b],
            [b,a],
            [b,a],
            [a,b],
            [b,a],
            [a,b]]

fvTests = TestList ["test" ++ show i ~: "(findVars : " ++ show q ++ ")" ~: e ~=? (findVars q) | (i,e,q) <- zip3 [0..8] expected queries]

--Build Unit tests for transform
expectedEnv = [True,True,False,False,False,False,False]
expectedEnv1 = [True,True,True,True,True,True,False]

transTests1 = TestList ["test" ++ show i ~: "(transform : " ++ show q ++ ")" ~: e ~=? (transform q myEnv) | (i,e,q) <- zip3 [0..8] expectedEnv queries]

transTests2 = TestList ["test" ++ show i ~: "(transform : " ++ show q ++ ")" ~: e ~=? (transform q myEnv1) | (i,e,q) <- zip3 [0..8] expectedEnv1 queries]

--Build unit Tests for Simplify
expectedSimp = [S_nand (S_var "a") (S_nand (S_nand (S_var "b") (S_var "c")) (S_var "a")),
            S_nand (S_var "a") (S_var "b"),S_not (S_var "a"),
            S_var "b",
            S_nand (S_nand (S_not (S_var "b")) (S_var "b")) (S_var "a"),
            S_nand (S_not (S_var "b")) (S_var "a"),
            S_not (S_nand (S_var "a") (S_var "b"))]


simpTests = TestList ["test" ++ show i ~: "(simplify : " ++ show q ++ ")" ~: e ~=? (simplify q) | (i,e,q) <- zip3 [0..8] expectedSimp queries]

--Run Unit Tests
runFVTests :: IO Counts
runFVTests = do 
    print "Testing findVars :: "
    runTestTT fvTests
    print "Testing transform :: "
    runTestTT transTests1
    runTestTT transTests2
    print "Testing simplify :: "
