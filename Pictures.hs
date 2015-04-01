module Pictures(
  Picture, --String
  flipH,
  flipV,
  quartet,
  printPic,
  sideByside,
  above,
  pic_1,
  pic_2,
) where

type Picture = [String]

--rotate90::Picture->Picture

flipH::Picture->Picture
flipH pic=[reverse line|line<-pic]

flipV::Picture->Picture
flipV pic=reverse pic

printPic::Picture-> IO()
printPic pic = putStr (unlines(pic))

above::Picture->Picture->Picture
above pic1 pic2 = pic1++pic2

sideByside::Picture->Picture->Picture
sideByside [] qs = qs
sideByside ps [] = ps
sideByside (p:ps) (q:qs) = (p++q):sideByside ps qs

quartet::Picture->Picture
quartet pic =above pic1 pic1 where pic1=sideByside pic pic

--enlargeH::Picture->Int->Picture
--enlargeH pic n = 

pic_1=["  ..  ","  ..  ","......","......","  ..  ","  ..  ","  ..  "]

pic_2 = 
   ["    A    ",
    "  A A A  ",
    "A A A A A",
    "AAAAAAAAA"]