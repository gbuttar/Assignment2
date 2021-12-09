member :: Int -> [Int] -> Int 
member a [] = 0
member a x:xs = 
    if a == x 
    then 0 else member a xs 

remove :: Int -> [Int] -> [Int]
remove a [] = []
remove a x:xs = 
    if a == x 
    then xs else x : remove a xs 

sum :: [Int] -> Int 
sum [] = 0
sum (x:xs) = x + sum xs 

prod :: [Int] -> Int
prod [] = 1 
prod (x:xs) = x * prod xs 

plus_two :: Int -> Int 
plus_two x = x + 2 

map :: (Int -> Int) -> [Int] -> [Int]
map a [] = []
map a (x:xs) = (a x) : (map a xs)

insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x:xs) 
    if e <= x 
    then e:x:xs 
    else y: insert e xs 

sort :: (Int -> [Int] -> [Int]) -> [Int] - [Int]
sort a [] = []
sort a (x:xs) = a x (sort a xs)

main = do 
    print (sum [1,2,3,4])
    print (prod [1,2,3,4])
    print (map plus_two [1,2,3,4])
    print (sort insert [1,2,3,1,2,3])