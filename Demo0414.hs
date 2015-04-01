import Pictures

mypic0 :: Picture
mypic0 = flipV pic_1

mypic1 :: Picture
mypic1 = quartet pic_2

--高阶函数 多态

--lamda表达式

f = \x -> (x*x)

g = \a b -> (a*b)

--函数的符合运算: .

f2 = \x -> (x*2) 

g2 = f2.f -- g2 = f2(f x)

flipVH = flipV . flipH -- flipV(flipH Pic)

--higher order functions

myMap::(a->b) -> [a] -> [b]
myMap f xs = [f x| x<-xs]

isEven x = mod x 2 == 0
--检验高阶函数filter isEven [1..20]
--还有其它系统自带高阶函数takeWhile iterate

--map printPic [pic1,pic2,pic3] --[printPic pic1,printPic pic2,printPic pic3] ::[IO()],IO()

--produc [1..5] = 1*2*..*5

--foldr op zs (x:y:z:u:[]) = x 'op' y 'op' z 'op' u
--zs为初始值
--show n

--product sum函数

getintfromlist::[Int]->Int
get n = n!!0

--!!用于获取list元素，从0开始