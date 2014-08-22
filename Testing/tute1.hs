doubleMe x = x + x

doubleUS x y = doubleMe x + doubleMe y

doubleSmaller x = if x < 100
                    then x*2
                    else x

boomBang xs = [ if x > 5 
                    then "BOOM!"
                    else "Bang!" | x <- xs , odd x]

length' xs = sum [ 1 | _ <- xs]

removeNonUpperCase cs = [ c | c <- cs , elem c ['A'..'Z']]

rightTriangles x = [(a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], c^2 == b^2 + a^2]
