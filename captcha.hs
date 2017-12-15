
padList :: [Int] -> [Int]
padList ls = ls ++ [head ls]


splitInPairs :: [Int] -> Int
splitInPairs ls = recurse 0 (padList ls)
                    where
                        recurse :: Int -> [Int] -> Int
                        recurse acc (x:y:xs) = recurse (if (x == y) then x + acc else acc) (y:xs)
                        recurse acc _ = acc