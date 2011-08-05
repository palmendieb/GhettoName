module GhettoName (
          ghettoName
        , prettyGhettoName
        , charGhettoM
        , sentence
        ) where

import Data.Char (toUpper, toLower)
import Data.List (isInfixOf)
import Control.Monad

data GhettoName = GhettoName {
       gRealName :: String,
       gName :: String
      } deriving (Show, Eq, Ord)

ghettoName :: String -> GhettoName
ghettoName realName = GhettoName realName $ prettyGhettoName realName

prettyGhettoName name = let ghettoName = nameToGhetto name
                        in (toUpper $ head ghettoName) : tail ghettoName


nameToGhetto name = concat $ map lookupNKeepC name

sentence = unwords . map prettyGhettoName . words

charGhettoM = [('A', "sha"),
               ('B', "ni"),
               ('C', "ki"),
               ('D', "que"),
               ('E', "nay"),
               ('F', "qui"),
               ('G', "ti"),
               ('H', "la"),
               ('I', "kay"),
               ('J', "ri"),
               ('K', "barack"),
               ('L', "obama"),
               ('M', "di"),
               ('N', "ta"),
               ('O', "ee"),
               ('P', "ray"),
               ('Q', "cli"),
               ('R', "gurl"),
               ('S', "na"),
               ('T', "qua"),
               ('U', "kwa"),
               ('V', "ise"),
               ('W', "fi"),
               ('X', "quee"),
               ('Y', "mi"),
               ('Z', "si")]

ghettoCharM = map (\(a, b) -> (b, a)) charGhettoM

lookupC :: Char -> Maybe String
lookupC c = lookup (toUpper c) charGhettoM 

-- Replace chars from a-z with equivalent string, everything else is kept
-- untransformed.
lookupNKeepC :: Char -> String
lookupNKeepC c = let result = lookupC c
                 in case result of
                    Nothing -> return c
                    Just str -> str
                                

splitGhettoName :: String -> [String]
splitGhettoName ghettoName = filter (`isInfixOf` ghettoName) (map snd charGhettoM)

--TODO: add ghettoName -> realName
--TODO: add main bla
