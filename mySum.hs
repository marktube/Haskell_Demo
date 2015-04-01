--列表类型
--一列整数 [1, 2, 3],[]::[Integer]
--[1,2,3]相当于1:[2,3],1:(2:3:[])

mySum :: [Integer]->Integer
mySum []=0
mySum (x:xs) = mySum xs + x

myLength :: [a] -> Int --多态类型，myLength是多态函数
myLength [] = 0
myLength (x:xs) = myLength xs + 1

myMultip :: [Integer]->Integer
myMultip [] = 1
myMultip (x:xs) = (myMultip xs)*x

--给一个列表的每一个元素加倍：[2*x|x<-[1..100]]
doubleAll :: [Int]->[Int]
doubleAll xs = [2*x| x<-xs]

--将一个列表中的偶数构成的列表表示出来：[x|x<-[1..10],mod x 2 == 0]
--将一个列表中的偶数加倍：[if(mod x 2 == 0) then 2*x else x |x<-[1..10]]

element::Int->[Int]
element n = [x|x<-[1..n],mod n x == 0]

myDouble::[Int]->[Int]
myDouble xs = [2*x|x<-xs]

quickSort::Ord a => [a]->[a]
quickSort [] = []
quickSort (x:xs) = quickSort [y|y<-xs,y<x] ++ [x] ++ quickSort [z|z<-xs,z>x]

isPrime::Int->Bool
isPrime n = if((element n) ==[1, n]) then True else False
