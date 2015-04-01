largerRoot:: Float->Float->Float->Float
largerRoot a b c = if(d<0) then 99999999 else if(a>0) then (-b+sqrt(d))/(2*a)
else (-b-sqrt(d))/(2*a) where d=b*b-4*a*c