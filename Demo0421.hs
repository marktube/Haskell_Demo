import Data.Char
import Data.List
--import Test.QuickCheck

--重载 overloading 同一个运算符表达不同类型上的（类似）运算

-- scala , python

elemInt::Int->[Int]->Bool
elemInt x [] = False
elemInt x (y:ys) = x==y || elemInt x ys

elemChar::Char->[Char]->Bool
elemChar c [] = False
elemChar c (x:xs) = c==x || elemChar c xs

--elemInt和elemChar，这种处理模式只依赖于类型的比较运算

elemn ::Eq a => a -> [a] -> Bool
elemn x [] =False
elemn x (y:ys) = x==y || elemn x ys

--Eq是一个class，或类型分类,它包含了定义了相等运算的类型

--定义一个类

class Visible a where --Visible为类名
  toString::a->String
  size::a->Int

instance Visible Bool where --实例化
  toString True = "True"
  toString False = "False"
  size True = 1
  size False = 0

printName::Visible a => a->String 
printName x = toString x 

--Show Ord也是比较常用的类

--子类 class Eq a=> Ord a where .... Ord是Eq的子类

--instance (Visible a,Visible b) where
--(x,y)==(u,v) = (x==u)&&(y==v)