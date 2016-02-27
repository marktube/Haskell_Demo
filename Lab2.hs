--由于用到pictures里的sideByside函数所以也把Pictures.hs文件也附上了，字母放大的图形好多，有些做得不是很好看，老师多多包涵。

module Lab2 where

import Pictures

import Data.Char

import Data.List

triads::Int->[(Int, Int, Int)]
triads n = [(x,y,z)|x<-[1..n],y<-[1..n],z<-[1..n],x*x+y*y==z*z]

sayit::String->IO()
sayit = putStr . say

say::String->String
say s = unlines (foldr sideByside [""] [enlarge x|x<-s])  

enlarge::Char->Picture
enlarge x |a==48 = pic0
          |a==49 = pic1
          |a==50 = pic2
          |a==51 = pic3
          |a==52 = pic4
          |a==53 = pic5
          |a==54 = pic6
          |a==55 = pic7
          |a==56 = pic8
          |a==57 = pic9
          |a==65 = pica
          |a==66 = picb
          |a==67 = picc
          |a==68 = picd
          |a==69 = pice
          |a==70 = picf
          |a==71 = picg
          |a==72 = pich
          |a==73 = pici
          |a==74 = picj
          |a==75 = pick
          |a==76 = picl
          |a==77 = picm
          |a==78 = picn
          |a==79 = pico
          |a==80 = picp
          |a==81 = picq
          |a==82 = picr
          |a==83 = pics
          |a==84 = pict
          |a==85 = picu
          |a==86 = picv
          |a==87 = picw
          |a==88 = picx
          |a==89 = picy
          |a==90 = picz
          |otherwise =  [""]
 where a = if(isLower(x)) then ((ord x)-32) else (ord x) 

pic0=["  0000000  ",
      "  0     0  ",
      "  0     0  ",
      "  0     0  ",
      "  0000000  "]

pic1=["    111    ",
      "   1 11    ",
      "     11    ",
      "     11    ",
      "   111111  "]

pic2=["  2222222  ",
      " 2    22   ",
      "    22     ",
      "  22       ",
      "  2222222  "]

pic3=[" 333333333 ",
      "      333  ",
      "  33333    ",
      "      333  ",
      "333333333  "]

pic4=["44      44 ",
      "44      44 ",
      "4444444444 ",
      "        44 ",
      "        44 "]

pic5=[" 55555555  ",
      " 55        ",
      " 55555555  ",
      "       55  ",
      " 55555555  "]

pic6=[" 66666666  ",
      " 66        ",
      " 66666666  ",
      " 66    66  ",
      " 66666666  "]

pic7=[" 777777777 ",
      "      777  ",
      "     77    ",
      "    77     ",
      "   77      "]

pic8=[" 888888888 ",
      " 88     88 ",
      " 888888888 ",
      " 88     88 ",
      " 888888888 "]

pic9=[" 999999999 ",
      " 99     99 ",
      " 999999999 ",
      "        99 ",
      " 999999999 "]

pica=["     A     ",
      "   A   A   ",
      "  A     A  ",
      " A  A A  A ",
      "A         A"]

picb=["BBBBBBBBBB ",
      "BB        B",
      "BBBBBBBBBB ",
      "BB        B",
      "BBBBBBBBBB "]

picc=["   CCCCCCCC",
      " CC        ",
      "CC         ",
      " C         ",
      "   CCCCCCCC"]

picd=["DDDDDDDD   ",
      "D        D ",
      "D         D",
      "D        D ",
      "DDDDDDDD   "]

pice=["EEEEEEEEEEE",
      "EE         ",
      "EEEEEEEEEEE",
      "EE         ",
      "EEEEEEEEEEE"]

picf=["FFFFFFFFFFF",
      "FF         ",
      "FFFFFFFFFFF",
      "FF         ",
      "FF         "]

picg=["GGGGGGGGGGG",
      "GG         ",
      "GG  GGGGGGG",
      "GG  GG   GG",
      "GGGGGGGGGGG"]

pich=["HH       HH",
      "HH       HH",
      "HHHHHHHHHHH",
      "HH       HH",
      "HH       HH"]

pici=["   IIIIII  ",
      "     II    ",
      "     II    ",
      "     II    ",
      "   IIIIII  "]

picj=["  JJJJJJJ  ",
      "     JJ    ",
      "     JJ    ",
      "  J  JJ    ",
      "   JJJ     "]

pick=["KK     KK  ",
      "KK  KK     ",
      "KKKK       ",
      "KK  KK     ",
      "KK     KK  "]

picl=["LL         ",
      "LL         ",
      "LL         ",
      "LL         ",
      "LLLLLLLL   "]

picm=["MM       MM",
      "MM M   M MM",
      "MM   M   MM",
      "MM       MM",
      "MM       MM"]

picn=["NN       NN",
      "NN N     NN",
      "NN   N   NN",
      "NN     N NN",
      "NN       NN"]

pico=["   OOOO   ",
      "  OO  OO  ",
      "  O    O  ",
      "  OO  OO  ",
      "   OOOO   "]

picp=["PPPPPPPPP ",
      "PP      PP",
      "PPPPPPPPP ",
      "PP        ",
      "PP        "]

picq=["   QQQQ   ",
      "  QQ  QQ  ",
      "  Q  Q Q  ",
      "  QQ  QQ  ",
      "   OQQ QQ "]

picr=["RRRRRRR   ",
      "RR    RR  ",
      "RRRRRRR   ",
      "RR  RR    ",
      "RR   RRR  "]

pics=["  SSSSSS  ",
      " SS       ",
      "   SSSS   ",
      "       SS ",
      "  SSSSSS  "]

pict=["TTTTTTTTTT",
      "    TT    ",
      "    TT    ",
      "    TT    ",
      "    TT    "]

picu=["UU      UU",
      "UU      UU",
      "UU      UU",
      " UU    UU ",
      "  UUUUUU  "]

picv=["VVV    VVV",
      " V      V ",
      "  V    V  ",
      "   V  V   ",
      "    VV    "]

picw=["W       W ",
      "W   W   W ",
      " W  W  W  ",
      "  W   W   ",
      "  W   W   "]

picx=["XXX    XXX",
      "  XX  XX  ",
      "    XX    ",
      "  XX  XX  ",
      "XXX    XXX"]

picy=["YYY    YYY",
      "  YY  YY  ",
      "    YY    ",
      "    YY    ",
      "    YY    "]

picz=["ZZZZZZZZZZ",
      "      ZZZ ",
      "    ZZ    ",
      " ZZZ      ",
      "ZZZZZZZZZZ"]
