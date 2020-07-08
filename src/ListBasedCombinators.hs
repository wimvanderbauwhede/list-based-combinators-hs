{-# LANGUAGE LambdaCase #-}
module ListBasedCombinators (
    ListBasedCombinators.sequence,
    choice,
    try,
    ListBasedCombinators.maybe,
    regex,
    parens,
    char,
    sepBy,
    sepByChar,
    oneOf,
    word,
    mixedCaseWord,
    natural,
    symbol,
    apply,
    greedyUpto,
    upto,
    ListBasedCombinators.many,
    ListBasedCombinators.many1,
    whiteSpace,
    comma,
    semi,
-- matches,
-- unwrap,
-- empty
    getParseTree,   
    LComb(..),
    MTup(..),
    Match(..),
    TaggedEntry(..)
) where

import Control.Applicative
import Control.Monad
import Text.Regex.PCRE
import qualified Data.Map.Strict as H

type Status = Integer
type Matches = [Match]
-- (Status,String,Matches)
emptyMatches :: Matches
emptyMatches = []
newtype MTup s = MTup (Int, s, Matches) deriving (Show)


data Match = Match String | TaggedMatches String [Match] | UndefinedMatch deriving (Eq,Show)


data LComb = Seq [LComb] | Comb (String -> MTup String) | Tag String LComb -- deriving (Show)

-- unM (Match ms) = ms
-- unM (TMatch _ ms) = ms
-- unM [UndefinedMatch] = []

-- I could of course use Seq instead, but I prefer the function visually    
sequence :: [LComb] -> LComb
sequence pseq = 
    Comb $ \str -> let
                f acc p = let
                    MTup (status1,str1,matches) = acc
                    MTup (status2,str2,ms) = apply p str1
                    in
                        if (status2*status1==0) 
                            then
                                MTup (0,str1,emptyMatches)
                            else
                                MTup (1,str2,(matches++ms))
                MTup (status, str', matches') = foldl f (MTup (1,str,emptyMatches)) pseq
                -- matches' = concat matches -- FIXME
            in
                if status == 0 then
                    MTup (0,str',emptyMatches)
                else
                    MTup (1,str',matches')

apply :: LComb -> String -> MTup String
apply p str = case p of
    Comb p' -> p' str
    Seq p' -> let
            Comb p'' = ListBasedCombinators.sequence p'
        in
            p'' str
    Tag k pp -> let
                MTup (status, str2, mms) = apply pp str
                matches = [TaggedMatches k mms] 
            in
                MTup (status, str2, matches)

word :: LComb
word = 
    Comb $ \str -> let
            (_,m,str') = str =~ "^([a-z_]\\w*)" :: (String,String,String)
            in
                if m /=  ""    
                    then
                        let
                            str2= trimStart str'
                        in
                            MTup (1,str2, [Match m])
                    else 
                        MTup (0,str, [UndefinedMatch])      

mixedCaseWord :: LComb
mixedCaseWord = 
    Comb $ \str -> let
            (_,m,str') = str =~ "^([a-zA-Z_]\\w*)" :: (String,String,String)
            in
                if m /=  ""    
                    then
                        let
                            str2= trimStart str'
                        in
                            MTup (1,str2, [Match m])
                    else 
                        MTup (0,str, [UndefinedMatch])                                                    

natural :: LComb
natural = 
    Comb $ \str -> let
            (_,m,str') = str =~ "^(\\d+)" :: (String,String,String)
            in
                if m /=  ""    
                    then
                        let
                            str''= trimStart str'
                        in
                            MTup (1,str'', [Match m])
                    else 
                        MTup (0,str, [UndefinedMatch])                            

symbol :: String -> LComb
-- As in Parsec, parses a literal and removes trailing whitespace
symbol lit_str = 
    -- lit_str =~  s/(\W)/\\$1/g FIXME! Escape non-word characters in the literal string
    let
        esc_lit_str = foldl esc_non_word_chars "" lit_str
        -- esc_non_word_chars acc c 
        --     | c `elem` (['a' .. 'z']++['A' .. 'Z']++['0' .. '9']++['_']) = acc++[c]
        --     | otherwise = acc++['\\',c]
        esc_non_word_chars acc c 
            | c `elem` ['+','-','*','.','?','$','/','\\','^','|','{','}','[',']','(',')','<','>'] = acc++['\\',c]
            | otherwise = acc++[c]

    in
        Comb $  \str -> let
            status=0
            (b,m,str')  = str =~ ("^\\s*"++esc_lit_str++"\\s*") :: (String,String,String)
        in
            if m/=""
                then
                    MTup (1,str', [Match lit_str])
                else
                    MTup (0,str, [UndefinedMatch])

parens :: LComb -> LComb
parens p =
    Comb $ \str -> let
            MTup (status, str3, ch) = apply (char '(') str
        in
            if status==1 then
                let
                    str4 = trimStart str3 
                    MTup (st,str4s,matches)= apply p str4
                    status'=status*st
                in
                    if status'==1 then
                        let
                            MTup (st, str5, ch)=apply (char ')') str4s
                            status'' = status'*st
                        in
                            if status''==1 then  -- OK!
                                let
                                    str6 = trimStart str5
                                in
                                    MTup (1,str6,matches)
                            else -- parse failed on closing paren
                                MTup (0,str5,matches)                
                    else -- parse failed on $ref
                        MTup (0,str4,matches)            
            else -- parse failed on opening paren
                MTup (0,str3,[UndefinedMatch])

char :: Char -> LComb
char ch = 
    Comb $ \str -> let
            c:cs = str
        in            
            if c == ch
                then
                    MTup (1,cs,[Match [ch]])
                else
                    MTup (0,str,[UndefinedMatch])

sepBy :: LComb -> LComb -> LComb
sepBy sep p =
    Comb $ \str -> let         
        MTup ( status, str1, m ) = apply p str
        in
            if status==1 then
                let
                    MTup (st',str2,m') = apply sep str1
                in 
                    if st' == 1 then
                        whileMatches p sep (MTup (1,str2,m++m'))
                    else
                        MTup (0,str1,m)
            else    -- first match failed.
                MTup ( 0, str1, [UndefinedMatch])

whileMatches p sep (MTup (st,str,m)) =
    let
            str2s = trimStart str
            MTup ( st', str', m' ) = apply p str2s
        in
            if st'==1 then let
                    MTup (status'', str2, m'' ) = apply sep str'
                in
                    if status''==1 then
                        whileMatches p sep (MTup (1,str2,m++m'++m''))                
                    else
                        MTup (1, str', m++m' )
            else
                MTup (0,str,m)    

sepByChar :: Char -> LComb -> LComb
sepByChar sep p =
    Comb $ \str -> let         
        MTup ( status, str1, m ) = apply p str
        in
            if status==1 then
                let
                    MTup (st',str2,m') = apply (char sep) str1
                in 
                    if st' == 1 then
                        whileMatchesChar p sep (MTup (1,str2,m++m'))
                    else
                        MTup (0,str1,m)
            else    -- first match failed.
                MTup ( 0, str1, [UndefinedMatch])

        
whileMatchesChar p sep (MTup (st,str,m)) =
    let
            str2s = trimStart str
            MTup ( st', str', m' ) = apply p str2s
        in
            if st'==1 then let
                    MTup (status'', str2, m'' ) = apply (char sep) str'
                in
                    if status''==1 then
                        whileMatchesChar p sep (MTup (1,str2,m++m'++m''))                
                    else
                        MTup (1, str', m++m' )
            else
                MTup (0,str,m)                            

-- Choice: try every parser in the list until one succeeds or return fail.  '<|>' in Parsec
-- FIXME: Prototype does not guarantee that parens can be omitted. Should make it binary for that.
choice :: [LComb] -> LComb
choice parsers = 
    Comb $ \str -> choice_helper parsers str
                

choice_helper [] str = MTup (0, str, [])
choice_helper (p:ps) str = let
    MTup (status, str', matches) = apply p str
    in
        if status ==1 then
            MTup (status, str', matches)
        else 
            choice_helper ps str

-- Normally, when a parser parses a string, it removes the portion that matched. If you want to keep the string, wrap the parser in try
try :: LComb -> LComb
try p =
    Comb $ \str -> let
            MTup (status, rest, matches) = apply p str
        in
            if status==1 then
                    MTup (1, rest, matches)
                else
                    MTup (0, str, matches)

-- maybe() is like try() but always succeeds
-- it returns the matches and the consumed string or the orig string and no matches
maybe :: LComb -> LComb
maybe p =
    Comb $ \str -> let
            MTup (status, rest, matches) = apply p str
        in
            if status==1 then
                    MTup (1, rest, matches)
                else
                    MTup (1, str, [UndefinedMatch])

oneOf :: String -> LComb
oneOf patt_lst =
    Comb $ \str -> loopOver str patt_lst

loopOver str (c:cs)  = let
        MTup (status, str', matches ) = apply (char c) str
    in
        if status==1 then
            MTup ( 1, str', matches )
        else
            if cs == "" then
                    MTup (0,str,[UndefinedMatch])
                else
                    loopOver str cs 

-- `many`, as in Parsec, parses 0 or more the specified parsers
many parser = 
     Comb $ \str -> let
            MTup ( status, str', m ) = apply parser str
        in
          if status==1 then
            doMany parser str' m 
          else     -- first match failed.
            MTup ( 1, str, [UndefinedMatch] )

-- `many1`, as in Parsec, parses 1 or more the specified parsers
many1 parser = 
     Comb $ \str -> let
            MTup ( status, str', m ) = apply parser str
        in
          if status==1 then
            doMany parser str' m 
          else     -- first match failed.
            MTup ( 0, str, [UndefinedMatch] )
                        
doMany p str ms = let
    MTup ( status, str', m ) = apply p str
    in
    if status == 1 then
        doMany p str' (ms++m)
        else
            MTup (1,str,ms)



-- This parser parses anything up to the first occurrence of a given literal and trailing whitespace
upto lit_str =
    Comb $ \str -> let
            (_,m,str') = str =~ "^(.*?)\\s*lit_str\\s*" :: (String,String,String)
        in
        if m /= "" then 
            MTup ( 1, str', [Match m] )
        else
            MTup ( 0, str, [UndefinedMatch] )

-- This parser parses anything up to the last occurrence of a given literal and trailing whitespace
greedyUpto lit_str =
    Comb $ \str -> let
            (_,m,str') = str =~ "^(.*)\\s*lit_str\\s*" :: (String,String,String)
        in
        if m /= "" then 
            MTup ( 1, str', [Match m] )
        else
            MTup ( 0, str, [UndefinedMatch] )

-- Matches a comma with optional whitespace
comma = 
    Comb $ \str -> let
        str' = trimStart str
        in
        if head str' == ',' then
            let
                str'' = trimStart (tail str')
            in
                MTup (1, str'', [UndefinedMatch] )
        else
            MTup (0, str, [UndefinedMatch])

semi = 
    Comb $ \str -> let
        str' = trimStart str
        in
        if head str' == ';' then
            let
                str'' = trimStart (tail str')
            in
                MTup (1, str'', [UndefinedMatch] )
        else
            MTup (0, str, [UndefinedMatch])



-- Enough rope: this parser will parse whatever the regex is, stripping trailing whitespace
regex :: String -> LComb
regex regex_str = 
    Comb $ \str -> let
            (_,m,str') = str =~ regex_str :: (String,String,String)        
        in
            if m /= "" then
                MTup ( 1, trimStart str', [Match m] )
            else         
                MTup ( 0, str, [UndefinedMatch] ) -- assumes $status is 0|1, $str is string, $matches is [string]


-- strip leading whitespace, always success
whiteSpace =
    Comb $ \str -> 
        let
            spaces = str =~ "^\\s+" :: String
        in
            MTup (1, drop (length spaces) str, [Match spaces])

trimStart :: String -> String
-- trimStart = dropWhile (\c  -> [c] =~ "^\\s" :: Bool)
trimStart str = let
    spaces = str =~ "^\\s+" :: String
    in
        drop (length spaces) str



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

data TaggedEntry = Val [String] | ValMap [(String, TaggedEntry)] deriving (Show)
-- data TaggedEntry = Val [String] | ValMap (H.Map String TaggedEntry) deriving (Show)

{-
A list of TaggedMatches must be translated into a Map of TaggedEntry's
hm = 
        
-}
_tagged_matches_to_map ::  Matches -> TaggedEntry 
_tagged_matches_to_map ms = let
-- if there are no TaggedMatches in ms, we should unpack the String from the Match and pack it into a Val [String]
        ms' =  filter (\case  
                        TaggedMatches _ _ -> True
                        _ -> False
                        ) ms
        in
            if null ms'  then 
                Val $ concatMap (\(Match str) -> [str]) ms
            else 
                ValMap $ foldl (\hm (TaggedMatches t ms'') -> 
                    hm++[(t, _tagged_matches_to_map ms'')]) [] ms'
                -- ValMap $ foldl (\hm (TaggedMatches t ms'') -> 
                --     H.insert t ( _tagged_matches_to_map ms'') hm) H.empty ms'

                
getParseTree ms = let
        ms' = _remove_undefined_values ms
        ms'' =  _tagged_matches_only ms'
        ms''' = _tagged_matches_to_map ms''
    in
        case ms''' of
            ValMap vm -> vm
            Val v -> []

-- ================================================================================================================================
-- Monad stuff, just for fun

{-

-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
-- So this bit of boilerplate makes it work
instance Functor MTup where
  fmap = liftM

instance Applicative MTup where
  pure  = return
  (<*>) = ap


instance Monad MTup where


--Sequentially compose two actions, discarding any value produced by the first, like sequencing operators (such as the semicolon) in imperative languages.
--  (>>) :: forall a b. m a -> m b -> m b 
  (>>) = (*>)


--Inject a value into the monadic type.
--  return :: a -> m a
  return str = MTup (1,str,[])


--fail :: String -> m a    


--Sequentially compose two actions, passing any value produced by the first as an argument to the second.
 -- (>>=) :: forall a b. m a -> (a -> m b) -> m b 

  mt >>=  p = 
        let
            MTup (st1,str2,m1) = mt                        
        in            
            if st1==1 then
                let
                    MTup (st2,str3,m2) = apply p str2
                in
                    MTup (st2,str3,m1++m2)                                
            else 
                let
                    MTup (st2,str3,m2) = apply p str2
                in
                    MTup (0,str3,m1)                  
                
-}        
after mt p = 
    let
        MTup (st1,str2,m1) = mt                        
    in            
        if st1==1 then
            let
                MTup (st2,str3,m2) = apply p str2
            in
                MTup (st2,str3,m1++m2)                                
        else 
            let
                MTup (st2,str3,m2) = apply p str2
            in
                MTup (0,str3,m1)   
    
