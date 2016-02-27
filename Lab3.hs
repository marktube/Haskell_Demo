--以p1、p2、p3作为测试均能通过

module Lab3(
  Prop,
  isTaut
)where

data Prop = Cons Bool
           |Var Char
           |Not Prop
           |And Prop Prop
           |Or Prop Prop
           |Imply Prop Prop
           deriving Eq

instance Show Prop where
  show (Cons b) = show b
  show (Var c) = [c]
  show (Not p) = "~"++show p
  show (And p1 p2) = "(" ++ show p1 ++ "&&" ++ show p2 ++ ")"
  show (Or p1 p2) = "(" ++ show p1 ++ "||" ++ show p2 ++ ")"
  show (Imply p1 p2) = "(" ++ show p1 ++ "=>" ++ show p2 ++ ")"

imply::Bool->Bool->Bool
imply True False = False
imply _ _ = True

p1::Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2::Prop
p2 = Or (Var 'A') (Not (Var 'A'))

p3::Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

type Subst = [(Char,Bool)]

eval::Subst->Prop->Bool
eval s (Cons b) = b
eval s (Var c) = if(l == 0) then error "variable not defined"
                 else if(l>1) then error "variable has multiple define"
                 else head res
                 where res = [b|(n,b)<-s,c==n]
                       l=length res
eval s (Not p) = not (eval s p)
eval s (And pa pb) = (eval s pa)&&(eval s pb)
eval s (Or pa pb) = (eval s pa)||(eval s pb)
eval s (Imply pa pb) = imply (eval s pa) (eval s pb)

vars::Prop->[Char]
vars (Cons b) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And pa pb) = (vars pa)++[b|a<-(vars pa),b<-(vars pb),a/=b]
vars (Or pa pb) =  (vars pa)++[b|a<-(vars pa),b<-(vars pb),a/=b]
vars (Imply pa pb) =  (vars pa)++[b|a<-(vars pa),b<-(vars pb),a/=b]

subsub::[Char]->[Subst]
subsub [x] = [[(x,True)],[(x,False)]]
subsub (x:xs) = [(x,True):a|a<-(subsub xs)]++[(x,False):b|b<-(subsub xs)]

substs::Prop->[Subst]
substs p = subsub (vars p)

--通过substs生成所有的情况代入eval函数中得到结果，再通过所有结果的合取作为函数的输出

isTaut::Prop->Bool
isTaut p = (foldr (&&) True [(eval s p)|s<-(substs p)])
