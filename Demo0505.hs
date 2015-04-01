type Name = String

type Number = String

type Phones = [(Name,Number)]

--表达结果是某个电话号码，或者不存在

--Maybe Bool : Nothing , Just True, Just False
--Maybe String : Nothing , Just "1232", ...

find::Phones->Name->Maybe Number
find [] name = Nothing
find ((na,nu):rem) name |na==name = Just nu
                        |otherwise = (find rem name)
--putChar::IO() putStrLn() putStr()
--return v return::a->IO a

--putStrLn "Hello" >> putStr "world!"

--do x <- getChar  y <- getChar

strlen::IO()
strlen = do putStrLn "Enter a string:"
            s <- getLine
            putStr "The length of your string is:"
            putStrLn (show (length s))

--read "12" :: Int

getInt :: IO Int
getInt = do x <- getLine
            return (read x :: Int)

addint::IO()
addint = do putStrLn "Enter an integer"
            sub1 <- getInt
            putStrLn "Enter another integer"
            sub2 <- getInt
            putStrLn "The result is:"
            print (sub1+sub2)
