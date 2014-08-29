-- @author : ricky ratnyake 
module Assignment
(Variable, Environment, Query(TRUE,FALSE,S_var,S_nand,S_not),
findVars, 
transform,
simplify)
where


import Data.List 

--Define types for Variable and Environment and Query as per specification
--Variables are defined as strings of any length
type Variable = [Char]
type Environment = [(Variable, Bool)]

data Query
    = TRUE          --Added to support extended simplification in Question3
    | FALSE
    | S_var Variable
    | S_nand Query Query
    | S_not Query
    deriving(Show)

--Define equality for Query data type, where commutativity holds 
instance Eq Query where
    S_var a == S_var b = a == b
    S_not a == S_not b = a == b
    S_nand a b == S_nand c d = (a == c && b == d) || (a == d && b == c)
    TRUE == TRUE = True
    FALSE == FALSE = True
    _ == _ = False 

------------------------------------------
--Question 1------------------------------
------------------------------------------

-- |findVars returns a list of unique Variables in the given query, in the order they first appear in the query 
findVars :: Query -> [Variable] 
findVars q = nub $ findAllVars q                --Removes duplicates in the list
    where   findAllVars TRUE = []
            findAllVars FALSE = []
            findAllVars (S_var v) = [v]
            findAllVars (S_nand q1 q2) = findAllVars q1 ++ findAllVars q2   
            findAllVars (S_not q)  = findAllVars q


------------------------------------------
--Question 2------------------------------
------------------------------------------

-- |transform evaluates a given query using an environment,
-- it can be invoked partially without providing an environment
transform :: Query -> Environment -> Bool
transform TRUE env = True
transform FALSE env = False
transform (S_var v) env = case lookup v env of 
                            Nothing -> error "No variable found" 
                            Just b -> b

transform (S_not q) env = not $ transform q env
transform (S_nand q1 q2) env = not $ and [transform q1 env, transform q2 env]


------------------------------------------
--Question 3------------------------------
------------------------------------------

-- |simplifyOnce will parse the given query once to reduce redundant expressions 
-- checks for the following cases !(!A) => A , !(A & A) => !A , !(A & !A) => Fasle
simplifyOnce :: Query -> Query
simplifyOnce TRUE = TRUE
simplifyOnce FALSE = FALSE
simplifyOnce (S_var v) = S_var v
simplifyOnce (S_not (S_not q)) = simplifyOnce q
simplifyOnce (S_not q) = S_not (simplifyOnce q)
simplifyOnce (S_nand q1 q2) = 
    let x = simplifyOnce q1                         
        y = simplifyOnce q2
    in if x == y                                 
        then S_not x                
        else if isNegated x y 
                then TRUE
                else S_nand x y         

-- |simplify will recursively call simplifyOnce until no further reductions can be made. This is intended to catch any possible reductions that are made possible after a previous reduction
simplify :: Query -> Query
simplify q = 
    let x = simplifyOnce q
    in if x == q
        then x
        else simplify x

-- |Checks if one query is a negated version of the other
isNegated :: Query -> Query -> Bool
isNegated q1 q2 = (S_not q1 == q2) || (q1 == S_not q2)


