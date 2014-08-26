import Data.List 

type Variable = [Char]

type Environment = [(Variable, Bool)]


data Query
    = S_var Variable
    | S_nand Query Query
    | S_not Query
    deriving(Show)

findVars :: Query -> [Variable] 
findVars (S_var v) = [v]
findVars (S_nand q1 q2) = nub (findVars q1 ++ findVars q2)
findVars (S_not q)  = findVars q

myEnv = [("a", True), ("b", False), ("c", True)]
a = "a"
b = "b" 
c = "c"

myQ = S_nand (S_var a) (S_nand (S_nand (S_var b) (S_var c)) (S_var a))
myQ1 = S_nand (S_not ( S_not ( S_var a))) (S_var b)

transform :: Query -> Environment -> Bool
transform (S_var v) = (\e -> case lookup v e of 
                                        Nothing -> error "No variable found" 
                                        Just b -> b)
transform (S_not q) = \e -> not (transform q e)
transform (S_nand q1 q2) = \e -> not (and [transform q1 e, transform q2 e])


simplify :: Query -> Query
simplify (S_var v) = S_var v
simplify (S_not (S_not q)) = simplify q
simplify (S_nand q1 q2) = S_nand (simplify q1) (simplify q2)


