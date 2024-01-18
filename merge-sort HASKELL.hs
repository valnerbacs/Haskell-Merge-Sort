import Debug.Trace ()
import Data.List ( intercalate )

main :: IO ()
main = do
        putStrLn "Input the number of elements: "
        n <- getLine
        let x = (read n :: Int)
        xs <- inputelems x
        putStrLn "Sorting..."
        let sub = msort xs
        putStrLn "Sorted Array"
        print (show sub)
        
inputelems 0 = return []
inputelems n =
 do
    num <- getLine
    xs <- inputelems(n-1)
    let x = (read num::Int)
    return (x:xs)
    
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
                    
halve :: [a] -> ([a],[a])
halve xs = (take lhx xs, drop lhx xs)
            where lhx = length xs `div` 2
            
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
           where (left,right) = halve xs