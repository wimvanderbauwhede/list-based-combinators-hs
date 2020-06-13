module Main where

import qualified Data.Map.Strict as H hiding (filter)
 
type Matches = [Match]

data Match = Match String | TaggedMatches String Matches | UndefinedMatch deriving (Show, Eq)

-- So we have a potentially nested structure ms 


ms :: Matches
ms =
    [
    Match "Example",
    UndefinedMatch,
    UndefinedMatch,
    TaggedMatches "Sentence"
    [Match "hello", 
        UndefinedMatch, 
        TaggedMatches "Adj" [
            Match "brave",
            UndefinedMatch,
            Match "new"
            ],
         Match "world",
         UndefinedMatch
    ],
    Match "End",
    UndefinedMatch
    ]

-- In Perl, I return a map with all the keys. It might be most convenient to do the same in Haskell
-- H.Map String Matches
-- The main question is what to do with nested tags. In Perl, again, these become nested Maps. 
-- I suppose this is what we should do then: 
-- H.Map String TaggedEntry where data TaggedEntry = String | H.Map String TaggedEntry
-- But that means that we go through the tree again, and we combine all  TaggedMatches in the Matches list into a Map
-- So in a way there's no need to remove the UndefinedMatch, rather we only keep the TaggedMatches
-- So I guess the output will 


_remove_undefined_values ms = let
        ms'  = filter (/=UndefinedMatch) ms 
    in 
        map (\m -> case m of
            Match _ -> m
            TaggedMatches t ms'' -> TaggedMatches t (_remove_undefined_values ms'')
            ) ms'

_tagged_matches_only :: Matches -> Matches -- H.Map String TaggedEntry 
_tagged_matches_only ms = let
        ms' =  filter (\m -> case m of 
                        TaggedMatches _ _ -> True
                        _ -> False
                        ) ms
    in 
        if length ms' == 0 
            then ms 
            else
                map (\(TaggedMatches t ms'') ->  TaggedMatches t (_tagged_matches_only ms'')) ms'

-- data MatchTup = (String, [MatchTup])

data TaggedEntry = Val [String] | ValMap (H.Map String TaggedEntry) deriving (Show)

{-
A list of TaggedMatches must be translated into a Map of TaggedEntry's
hm = 
        
-}
_tagged_matches_to_map ::  Matches -> TaggedEntry 
_tagged_matches_to_map ms = let
-- if there are no TaggedMatches in ms, we should unpack the String from the Match and pack it into a Val [String]
        ms' =  filter (\m -> case m of 
                        TaggedMatches _ _ -> True
                        _ -> False
                        ) ms
        in
            if length ms' == 0 then 
                Val $ concatMap (\(Match str) -> [str]) ms
            else 
                ValMap $ foldl (\hm (TaggedMatches t ms') -> H.insert t ( _tagged_matches_to_map ms') hm) H.empty ms

                
getParseTree ms = let
        ms' = _remove_undefined_values ms
        ms'' =  _tagged_matches_only ms'
        ms''' = _tagged_matches_to_map ms''
    in
        (\(ValMap vm) -> vm) ms'''
        
main = do
    print ms
    print ""
    let
        ms' =  _remove_undefined_values ms
--    print $
        ms'' =  _tagged_matches_only ms'
    print $ (\(ValMap vm) -> vm) $ _tagged_matches_to_map ms''
