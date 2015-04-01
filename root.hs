module Root where --module首字母大写
root::Double->Double->(Double->(Double,Double,String))
root a b c |d<0 = error "No real root"
           |otherwise = ((-b+sqrt(d))/(2*a),(-b-sqrt(d))/(2*a),"are Real Roots") where d=b*b-4*a*c