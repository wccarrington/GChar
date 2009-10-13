import Data.List (foldl')
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C

data Character = Character String [Level] deriving Show
data Level = Attribute Attr Int
           | BAdvantage String Int --binary advantage
           | LAdvantage String Int Int --leveled advantage
           | Skill String Attr Difficulty Int
  deriving Show
data Attr = ST | DX | IQ | HT | HP | FP | Will | Per | BasicSpeed | Move
  deriving (Show, Eq)
data Difficulty = Easy | Avg | Hard | VHard deriving Show

zeroPointCharacter = Character "" []

addLevel :: Character -> Level -> Character
addLevel (Character n ls) a = Character n $ mergeLevels a ls

mergeLevels k@(Attribute a n) l@((Attribute b m):ls) =
  if a == b then Attribute a (n+m) : ls else head l : mergeLevels k ls
mergeLevels k@(BAdvantage n c) l@((BAdvantage m b):ls) =
  if n == m then l else head l : mergeLevels k ls
mergeLevels k@(LAdvantage n1 l1 c1) l@((LAdvantage n2 l2 c2):ls) =
  if n1 == n2 then LAdvantage n1 (l1+l2) c1 : ls else head l : mergeLevels k ls
mergeLevels k@(Skill n1 a1 d1 l1) l@((Skill n2 a2 d2 l2):ls) =
  if n1 == n2 then k : ls else head l : mergeLevels k ls
mergeLevels l [] = [l]
mergeLevels k (l:ls) = l : mergeLevels k ls

addLevels as c = foldl' addLevel c as

loop :: IO ()
loop = CH.getKey C.refresh >>= 
       \k -> case k of C.KeyChar 'q' -> return ()
                       _             -> loop

main = CH.start >>
       loop >>
       CH.end

