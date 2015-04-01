--刘彦超 12353133, 1065362168@qq.com, 移动信息工程学院； 
--实现分数的常用运算
module MyFraction where 

import Test.QuickCheck

type Fraction = (Integer,Integer) --Type define

ratplus::Fraction->Fraction->Fraction
ratplus (a,b) (c,d) |b==0||d==0 = error "Math error" |otherwise = ((div (a*d+b*c) k) ,(div (b*d) k)) where k = (gcd (a*d+b*c) (b*d))

(<+>) :: Fraction -> Fraction -> Fraction
(a, b) <+> (c, d) = ratplus (a,b) (c,d)

--addition define and use <+> to add

ratminus::Fraction->Fraction->Fraction
ratminus (a,0) (b,c) = error "Math error"
ratminus (a,b) (c,0) = error "Math error"
ratminus (a,b) (c,d) = ((div (a*d-b*c) k) ,(div (b*d) k)) where k = (gcd (a*d-b*c) (b*d))

(<->) :: Fraction -> Fraction -> Fraction
(a, b) <-> (c, d) = ratminus (a,b) (c,d)

--subtraction define and use <-> to subtract

rattimes::Fraction->Fraction->Fraction
rattimes (a,b) (c,d) |b==0||d==0 = error "Math error" |otherwise = ((div (a*c) k) ,(div (b*d) k)) where k = (gcd (a*c) (b*d))

(<*>) :: Fraction -> Fraction -> Fraction
(a, b) <*> (c, d) = rattimes (a,b) (c,d)

--Multiply define and use <*> to multiply

ratdiv::Fraction->Fraction->Fraction
ratdiv (a,b) (c,d) |b==0||d==0 = error "Math error"  |otherwise = if(c/=0) then ((div (a*d) k) ,(div (b*c) k)) else (error"Divide by zero!!!") where k = (gcd (a*d) (b*c))

(</>) :: Fraction -> Fraction -> Fraction
(a, b) </> (c, d) = ratdiv (a,b) (c,d)

--division define and use </> to divide

ratfloor::Fraction -> Integer
ratfloor (a,b) = (div a b)

--get the greatest integer which is less than the rational number

rateq::Fraction->Fraction->Bool
rateq (a,b) (c,d) = if((ratdiv (a,b) (c,d))==(1,1)) then True else False

(<==>) :: Fraction->Fraction->Bool
(a, b) <==> (c, d) = rateq (a,b) (c,d)

--judge whether the two numbers are equal and use <==> to decide

ratfloat::Fraction->Float
ratfloat (a,b) = (fromInteger a)/(fromInteger b)

--format the rational number to float

prop_ratplus_and_ratminus (a,b) (c,d) = ((a,b) <+> (c,d) <-> (c,d))==(a,b)

--check the addition and subtraction

prop_rattimes_and_ratdiv (a,b) (c,d) = ((a,b) <*> (c,d) </> (c,d))==(a,b)

--check the multiply and division

prop_ratplus_rattimes (a,b) (c,d) (e,f) = ((a,b) <+> (c,d) <*> (e,f)) == ((a,b) <+> ((c,d) <*> (e,f))) 