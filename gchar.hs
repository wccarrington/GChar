import Data.List (foldl')

data Character = Character String [Level] deriving Show
data Level = Attribute Attr Int
           | Advantage String Int
           | LAdvantage String Int Int
           | Skill String Attr Difficulty Int
  deriving Show
data Attr = ST | DX | IQ | HT | HP | FP | Will | Per | BasicSpeed | Move
  deriving (Show, Eq)
data Difficulty = Easy | Avg | Hard | VHard deriving Show

zeroPointCharacter = Character "" []

addLevel :: Character -> Level -> Character
addLevel (Character n ls) a = Character n $ a:ls

addLevels as c = foldl' addLevel c as

main = print $ addLevels 
        [Attribute DX (-1), 
         Attribute ST 1,
         Advantage "Combat Reflexes" 15,
         LAdvantage "Independent Income" 5 2, --II 5, 2/level
         Skill "Sword" DX Avg 1] zeroPointCharacter

