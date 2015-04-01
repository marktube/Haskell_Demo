class Xxx a  where
  xxx :: a->a

instance Xxx  Bool where
  xxx True = False
  xxx False = True

--属于Show这个class的元素都可以用print打印出来，可以用:i查看相关信息

type Borrower = String
type Book = String
type Card = (Borrower, Book)
type Database = [Card]

example::Database
example = [("Liu Xiang","The Old Man and The Sea")]

borrow::Borrower->Book->Database->Database
borrow name book  db = (name, book) : db

ret::Borrower->Book->Database->Database
ret  name book  db = [(n,b)|(n,b)<-db,(n,b)/=(name,book)]

example1=borrow "Liu Xuebo" "Warcraft" example
example2=ret "Liu Xiang" "The Old Man and The Sea" example1

--12 months, 7 days, 4 seasons

data Months = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec

days::Months->Int
days Feb = 28
days Apr = 30
days Jun = 30
days Sep = 30
days Nov = 30
days _ = 31

instance Eq Months where
  Jan == Jan = True
  Feb == Feb = True
  Mar == Mar = True
  Apr == Apr = True
  May == May = True
  Jun == Jun = True
  Jul == Jul = True
  Aug == Aug = True
  Sep == Sep = True
  Oct == Oct = True
  Nov == Nov = True
  Dec == Dec = True
  _ == _ = False

instance Show Months where
  show Jan = "Janaury"
  show Feb = "February"
--余下类似

data Seasons = Spring|Summer|Autumn|Winter deriving (Eq,Show)

{-instance Eq Seasons where
  Spring == Spring = True
  Summer == Summer = True
  Autumn == Autumn = True
  Winter == Winter = True
  _ == _ = False
-}

--type是已有类型的组合添加别名data是新的类型
--deriving让系统自己做

--[Int]

--整数列表IntList可以用两条规则说明其所有元素：
--1.空列表是一个列表
--2.如果x：xs，xs是整数列表，则x和xs构成新的列表

data IntList = Empty | Cons Int IntList

--Empty,Cons 2 Empty

data Exp = 
       Num Int
     | Var Char
     | Add Exp Exp
     | Mul Exp Exp

instance Show Exp where
  show (Num n) = show n
  show (Var c) = [c]
  show (Add x y) = "(" ++ show x ++ "+" ++ show y ++")"
  show (Mul x y) = show x ++ "*" ++ show y

diff::Exp->Char->Exp
diff (Num n) x = (Num 0)
diff (Var var) x = if(var==x) then (Num 1) else (Num 0)
diff (Add v1 v2) x = Add (diff v1 x) (diff v2 x)
diff (Mul v1 v2) x = Add (Mul v1 (diff v2 x)) (Mul v2 (diff v1 x))

{-eval :: Exp -> Int
eval (Num n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)
-}

eval::Exp->[(Char,Int)]->Int
eval (Num n) sub = n
eval (Var c) sub = head [k|(v,k)<-sub,c==v]
eval (Add x y) sub = (eval x sub) + (eval y sub)
eval (Mul x y) sub = (eval x sub) * (eval y sub)