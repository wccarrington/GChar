import Data.List (foldl')
import Control.Monad.State
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C

type GChar = StateT Character IO

normalStyle = CH.mkCursesStyle []
reverseStyle = CH.mkCursesStyle [CH.Reverse]

data Character = Character String [Level] deriving Show
data Level = Attribute Attr Int
           | BAdvantage String Int --binary advantage
           | LAdvantage String Int Int --leveled advantage
           | Skill String Attr Difficulty Int
  deriving Show
data Attr = ST | DX | IQ | HT | HP | FP | Will | Per | BasicSpeed | Move
  deriving (Show, Eq)
data Difficulty = Easy | Avg | Hard | VHard deriving (Show, Eq)

zeroPointCharacter :: Character
zeroPointCharacter = Character "" []

nameCharacter :: String -> Character -> Character
nameCharacter n (Character _ ls) = Character n ls

characterName :: Character -> String
characterName (Character name _) = name

addLevel :: Character -> Level -> Character
addLevel (Character n ls) a = Character n $ mergeLevels a ls

mergeLevels :: Level -> [Level] -> [Level]
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

getAttribute :: Attr -> Character -> Int
getAttribute a (Character _ ls) = helper a ls
  where helper a1 (Attribute a2 n:ls) = if a1 == a2 then n else helper a1 ls
        helper a (_:ls) = helper a ls
        helper a [] = 0

addLevels :: [Level] -> Character -> Character
addLevels as c = foldl' addLevel c as

addStr :: String -> IO ()
addStr = C.wAddStr C.stdScr

drawCharacter :: GChar ()
drawCharacter =
  get >>= \c -> let name = characterName c in
  liftIO $ CH.setStyle reverseStyle >>
  (C.move 0 0) >> addStr name >>
  CH.setStyle normalStyle >>
  (C.move 1 0) >> addStr "ST " >> (addStr $ show $ getAttribute ST c+10) >>
  (C.move 1 7) >> addStr "HP " >> 
  (addStr $ show $ getAttribute ST c + getAttribute HP c + 10) >>
  (C.move 2 0) >> addStr "DX " >> (addStr $ show $ getAttribute DX c+10) >>
  (C.move 2 7) >> addStr "BS " >>
  (addStr $ take 3 $ show $ (fromIntegral (getAttribute DX c) + 
                    fromIntegral (getAttribute HT c) + 
                    fromIntegral (getAttribute BasicSpeed c)+ 20)/4) >>
  (C.move 2 14) >> addStr "BM " >>
  (addStr $ show $ truncate $ (fromIntegral (getAttribute DX c) + 
                    fromIntegral (getAttribute HT c) +
                    fromIntegral (getAttribute BasicSpeed c) + 20)/4 +
                    fromIntegral (getAttribute Move c)) >>
  (C.move 3 0) >> addStr "IQ " >> (addStr $ show $ getAttribute IQ c+10) >>
  (C.move 3 7) >> addStr "WL " >>
  (addStr $ show $ getAttribute IQ c + getAttribute Will c + 10) >>
  (C.move 3 14) >> addStr "PR " >>
  (addStr $ show $ getAttribute IQ c + getAttribute Per c + 10) >>
  (C.move 4 0) >> addStr "HT " >> (addStr $ show $ getAttribute HT c+10) >>
  (C.move 4 7) >> addStr "FP " >> 
  (addStr $ show $ getAttribute HT c + getAttribute FP c + 10) >>
  C.refresh

sampleChar = addLevels [Attribute ST 2, BAdvantage "Absolute Direction" 5,
                        Attribute IQ 2, Attribute HP 2, Attribute HT (-1),
                        Attribute BasicSpeed 1, Attribute Move (-1)] $ 
             Character "Bob" []

loop :: GChar ()
loop = liftIO (CH.getKey C.refresh) >>= \k -> 
    case k of C.KeyChar '\ESC' -> return ()
              _                -> drawCharacter >> loop

main :: IO ()
main = CH.start >> runStateT (drawCharacter >> loop) sampleChar >> CH.end

