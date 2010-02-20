module Main where

import Data.List (foldl')
import Control.Monad.State
import qualified UI.HSCurses.CursesHelper as CH
import qualified UI.HSCurses.Curses as C

type GChar = StateT Int (StateT Character IO)

getCharacter :: GChar Character
getCharacter = lift get

putCharacter :: Character -> GChar ()
putCharacter c = lift $ put c

normalStyle :: CH.CursesStyle
normalStyle = CH.mkCursesStyle []

reverseStyle :: CH.CursesStyle
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
mergeLevels k@(BAdvantage n _) l@((BAdvantage m _):ls) =
  if n == m then l else head l : mergeLevels k ls
mergeLevels k@(LAdvantage n1 l1 c1) l@((LAdvantage n2 l2 _):ls) =
  if n1 == n2 then LAdvantage n1 (l1+l2) c1 : ls else head l : mergeLevels k ls
mergeLevels k@(Skill n1 _ _ _) l@((Skill n2 _ _ _):ls) =
  if n1 == n2 then k : ls else head l : mergeLevels k ls
mergeLevels l [] = [l]
mergeLevels k (l:ls) = l : mergeLevels k ls

getAttribute :: Attr -> Character -> Int
getAttribute attr (Character _ levels) = fromIntegral $ helper attr levels
  where helper a1 (Attribute a2 n:ls) = if a1 == a2 then n else helper a1 ls
        helper a (_:ls) = helper a ls
        helper _ [] = 0

addLevels :: [Level] -> Character -> Character
addLevels as c = foldl' addLevel c as

getReadable :: Attr -> Character -> Int
getReadable a c
    | a == ST || a == DX || a == IQ || a == HT = getAttribute a c + 10
    | a == HP   = getAttribute HP   c + getReadable ST c
    | a == FP   = getAttribute FP   c + getReadable HT c
    | a == Will || a == Per = getAttribute a c + getReadable IQ c
    | a == Move = truncate (getBasicSpeed c) + getAttribute Move c
    | otherwise = error "Cannot get BasicSpeed with getReadable, use getBasicSpeed instead."

getBasicSpeed :: Character -> Float
getBasicSpeed c = (fromIntegral (getAttribute BasicSpeed c) + fromIntegral (getReadable HT c) + 
                   fromIntegral (getReadable DX c)) / 4.0

addStr :: String -> IO ()
addStr = C.wAddStr C.stdScr

sampleChar :: Character
sampleChar = addLevels [Attribute ST 2, BAdvantage "Absolute Direction" 5,
                        Attribute IQ 2, Attribute HP 2, Attribute HT (-1),
                        Attribute BasicSpeed 3, Attribute Move (-1)] $ 
             Character "Bob" []

drawCharacter :: Character -> IO ()
drawCharacter c@(Character name _) =
  C.erase >>
  CH.setStyle reverseStyle >>
  (C.move 0 0) >> addStr name >>
  CH.setStyle normalStyle >>
  (C.move 1 0)  >> addStr "ST " >> (addStr $ show $ getReadable ST c) >>
  (C.move 1 7)  >> addStr "HP " >> (addStr $ show $ getReadable HP c) >>
  (C.move 2 0)  >> addStr "DX " >> (addStr $ show $ getReadable DX c) >>
  (C.move 2 7)  >> addStr "BS " >> (addStr $ take 3 $ show $ getBasicSpeed c) >>
  (C.move 2 14) >> addStr "BM " >> (addStr $ show $ getReadable Move c) >>
  (C.move 3 0)  >> addStr "IQ " >> (addStr $ show $ getReadable IQ c) >>
  (C.move 3 7)  >> addStr "WL " >> (addStr $ show $ getReadable Will c) >>
  (C.move 3 14) >> addStr "PR " >> (addStr $ show $ getReadable Per c) >>
  (C.move 4 0)  >> addStr "HT " >> (addStr $ show $ getReadable HT c) >>
  (C.move 4 7)  >> addStr "FP " >> (addStr $ show $ getReadable FP c) >>
  C.refresh

drawScreen :: GChar ()
drawScreen = getCharacter >>= \c -> liftIO $ drawCharacter c

input :: C.Key -> GChar ()
input (C.KeyChar '\ESC') = return ()
input (C.KeyChar '=') = getCharacter >>= \c -> putCharacter (addLevel c (Attribute ST 1)) >>
                                      drawScreen >> loop
input (C.KeyChar '-') = getCharacter >>= \c -> putCharacter (addLevel c (Attribute ST (-1))) >>
                                      drawScreen >> loop
input c = (liftIO $ C.move 6 0) >> (liftIO $ addStr $ show c) >> 
          drawScreen >> loop

loop :: GChar ()
loop = liftIO (CH.getKey C.refresh) >>= input

main :: IO ()
main = CH.start >> runStateT (runStateT (drawScreen >> loop) 1) sampleChar >> CH.end

