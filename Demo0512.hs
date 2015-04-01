--Lazy evaluation

import Graphics.HGL

ones = 1 : ones

numFrom n = n : numFrom(n+1)

fib = 1 : 1 : [x+y|(x,y)<-zip fib (tail fib)]

--zip 拉链函数 将两个序列合并

--埃拉托斯特尼筛法(The Sieve of Eratosthenes)是一种求素数的算法

--删除所有2的倍数后第一个未被删掉的数为3，3是素数， 
--然后删除3的倍数后第一个未被删掉的数为5，5是素数，

sieve::[Int]->[Int]
sieve [] = []
sieve (x:xs) = x : sieve [y|y<-xs,mod y x /= 0]

helloWorld :: IO()
helloWorld = runGraphics (do
  w <- openWindow "Hello World Window" (300,300)
  drawInWindow w (text (100, 100) "Hello")
  drawInWindow w (text (100, 200) "World")
  getKey w
  closeWindow w
  )