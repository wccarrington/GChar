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

allAttributes :: [Attr]
allAttributes = [ST, DX, IQ, HT, HP, BasicSpeed, Will, FP, Move, Per]

zeroPointCharacter :: Character
zeroPointCharacter = Character "" []

nameCharacter :: String -> Character -> Character
nameCharacter n (Character _ ls) = Character n ls

characterName :: Character -> String
characterName (Character name _) = name

addLevel :: Character -> Level -> Character
addLevel (Character n ls) a = Character n $ mergeLevels a ls

mergeLevels :: Level -> [Level] -> [Level]
mergeLevels k@(Attribute a1 n1) l@((Attribute a2 n2):ls) | a1 == a2 = Attribute a1 (n1+n2) : ls
mergeLevels k@(BAdvantage n1 _) l@((BAdvantage n2 _):ls) | n1 == n2 = l
mergeLevels k@(LAdvantage n1 l1 c1) l@((LAdvantage n2 l2 _):ls) | n1 == n2 = 
  LAdvantage n1 (l1+l2) c1 : ls
mergeLevels k@(Skill n1 _ _ _) l@((Skill n2 _ _ _):ls) | n1 == n2 = k : ls
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
sampleChar = Character "Bob" []

highlight :: IO a -> IO a
highlight a = CH.setStyle reverseStyle >> a

normal :: IO a -> IO a
normal a = CH.setStyle normalStyle >> a

pickStyle :: (Eq a) => a -> a -> IO b -> IO b
pickStyle a b | a == b = highlight
              | otherwise = normal

drawAttr :: Character -> Attr -> Attr -> String -> IO ()
drawAttr c h a s = pickStyle h a (addStr s >> (addStr $ show $ getReadable a c))

drawCharacter :: Character -> Attr -> IO ()
drawCharacter c@(Character name _) a =
  C.erase >>
  highlight (C.move 0 0 >> addStr name) >>
  (C.move 1 0)  >> drawAttr c a ST "ST " >>
  (C.move 1 7)  >> drawAttr c a HP "HP " >>
  (C.move 2 0)  >> drawAttr c a DX "DX " >>
  (C.move 2 7)  >> pickStyle BasicSpeed a (addStr "BS " >> 
                                           (addStr $ take 3 $ show $ getBasicSpeed c)) >>
  (C.move 2 14) >> drawAttr c a Move "BM " >>
  (C.move 3 0)  >> drawAttr c a IQ "IQ " >>
  (C.move 3 7)  >> drawAttr c a Will "WL " >>
  (C.move 3 14) >> drawAttr c a Per "PR " >>
  (C.move 4 0)  >> drawAttr c a HT "HT " >>
  (C.move 4 7)  >> drawAttr c a FP "FP " >>
  C.refresh

drawScreen :: GChar ()
drawScreen = get >>= \i -> getCharacter >>= \c -> liftIO $ drawCharacter c (allAttributes !! i)

clamp :: (Ord a) => a -> a -> a -> a
clamp low high i = if i < low then low else if i > high then high else i

input :: C.Key -> GChar ()
input (C.KeyChar '\ESC') = return ()
input C.KeyUp   = get >>= \i -> 
                  getCharacter >>= \c -> putCharacter (addLevel c (Attribute (allAttributes !! i) 1)) >>
                                      drawScreen >> loop
input C.KeyDown = get >>= \i ->
                  getCharacter >>= \c -> putCharacter (addLevel c (Attribute (allAttributes !! i) (-1))) >>
                                      drawScreen >> loop
input C.KeyLeft  = get >>= \i -> put (clamp 0 (length allAttributes - 1) (i-1)) >> drawScreen >> loop
input C.KeyRight = get >>= \i -> put (clamp 0 (length allAttributes - 1) (i+1)) >> drawScreen >> loop
input c = (liftIO $ C.move 6 0) >> (liftIO $ addStr $ show c) >> 
          drawScreen >> loop

loop :: GChar ()
loop = liftIO (CH.getKey C.refresh) >>= input

main :: IO ()
main = CH.start >> runStateT (runStateT (drawScreen >> loop) 1) sampleChar >> CH.end

