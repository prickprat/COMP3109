--Testing for Assignmen 1
import Test.HUnit
import Assignment
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
myQ4 = S_nand (S_not (S_var b)) (S_not (S_not (S_var b)))
myQ5 = S_nand   (myQ4) 
                (S_nand (S_not (S_var a)) 
                        (S_not (S_var a)))

myQ6 = S_nand 
        (S_nand (S_not  (S_nand 
                                (S_var b)
                                (S_var b))) 
                (S_not (S_not (S_var b)))) 
        (S_nand (S_not (S_var a)) 
                (S_not (S_var a)))
myQ7 = S_nand (S_var a) (S_var b)
myQ8 = S_nand (S_var b) (S_var a)
myQ9 = S_nand myQ7 myQ8


queries = [myQ, myQ1, myQ2, myQ3, myQ4, myQ5, myQ6, myQ7, myQ8, myQ9]
tNums = [0..9]

--Build Unit tests for findVars
expected = [[a,b,c], 
            [a,b],
            [a],
            [b],
            [b],
            [b,a],
            [b,a],
            [a,b],
            [b,a],
            [a,b]]

fvTests = TestList ["test" ++ show i ~: "(findVars : " ++ show q ++ ")" ~: e ~=? (findVars q) | (i,e,q) <- zip3 tNums expected queries]

--Build Unit tests for transform
expectedEnv = [True,True,False,False,True,False,False,True,True,False]
expectedEnv1 = [True,True,True,True,True,True,True,True,True,False]

transTests1 = TestList ["test" ++ show i ~: "(transform : " ++ show q ++ ")" ~: e ~=? (transform q myEnv) | (i,e,q) <- zip3 tNums expectedEnv queries]

transTests2 = TestList ["test" ++ show i ~: "(transform : " ++ show q ++ ")" ~: e ~=? (transform q myEnv1) | (i,e,q) <- zip3 tNums expectedEnv1 queries]

--Build unit Tests for Simplify
expectedSimp = [S_nand (S_var "a") (S_nand (S_nand (S_var "b") (S_var "c")) (S_var "a")),
            S_nand (S_var "a") (S_var "b"),
            S_not (S_var "a"),
            S_var "b",
            TRUE,
            S_nand TRUE (S_var "a"),
            S_nand (S_not (S_var "b")) (S_var "a"),
            S_nand (S_var "a") (S_var "b"),
            S_nand (S_var "b") (S_var "a"),
            S_not (S_nand (S_var "a") (S_var "b"))]


simpTests = TestList ["test" ++ show i ~: "(simplify : " ++ show q ++ ")" ~: e ~=? (simplify q) | (i,e,q) <- zip3 tNums expectedSimp queries]

--Run Unit Tests
runTests :: IO Counts
runTests = do 
    print "Testing findVars :: "
    runTestTT fvTests
    print "Testing transform :: "
    runTestTT transTests1
    runTestTT transTests2
    print "Testing simplify :: "
    runTestTT simpTests
