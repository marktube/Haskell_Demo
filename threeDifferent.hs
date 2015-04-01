import Test.QuickCheck     
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = if((a==b)||(b==c)||(a==c)) then False else True
prop_threeDifferent x y z = if (x/=y && x/=z && y /= z) then (threeDifferent x y z == True) else (threeDifferent x y z == False)