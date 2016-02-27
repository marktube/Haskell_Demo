
--计算平方根的Newton Raphson公式

module Squareroot where 

squareroot2::Float->Integer->Float
squareroot2 x 0 = x
squareroot2 x n = squareroot2 ((x+2/x)/2) (n-1)

--运用递归实现Newton－Raphson公式求2的算数平方根

squareroot::Float->Float->Integer->Float
squareroot x y 0 = x
squareroot x y n = squareroot ((x+y/x)/2) y (n-1)

--计算y的近似平方根

sqrtSeq::Float->Float->Integer->[Float]
sqrtSeq x y 0 = x:[]
sqrtSeq x y n = x:(sqrtSeq ((x+y/x)/2) y (n-1))

--近似序列 版本2.0 直接递归， 版本1.0 利用上面的sqrtroot函数实现，此处不表

sqrtroot'::Float->Float->Float->Float
sqrtroot' x y epsilon = if(abs(next-x)<=epsilon) then next else (sqrtroot' next y epsilon) where next = (x+y/x)/2    

--x－处置 y－底数 epsilon－近似误差 

--通过与sqrt（2），sqrt（n）比较测试是否实现
