
module ListBasedCombinators (
    ListBasedCombinators.sequence,
    choice,
    try,
    ListBasedCombinators.maybe,
-- regex
    parens,
    char,
    sepBy,
    oneOf,
    word,
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
-- getParseTree,
    LComb(..)
) where

import Text.Regex.PCRE
import Control.Monad
import Control.Applicative

type Status = Integer
type Matches = [Match]
data Match = Match String | TaggedMatches String [Match] | UndefinedMatch deriving (Show)


data LComb = Seq [LComb] | Comb (String -> (Status,String,Matches)) | Tag String LComb -- deriving (Show)


-- unM (Match ms) = ms
-- unM (TMatch _ ms) = ms
-- unM [UndefinedMatch] = []
    
sequence :: [LComb] -> LComb
sequence pseq = 
    Comb $ \str -> let
                f acc p = let
                        (status1,str1,matches) = acc
                        (status2,str2,ms) = apply p str1
                    in
                        if (status2*status1==0) 
                            then
                                (0,str1,[])
                            else
                                (1,str2,(matches++[ms]))
                (status, str', matches) = foldl f (1,str,[]) pseq
                matches' = concat matches -- FIXME
            in
                if status == 0 then
                    (0,str',[])
                else
                    (1,str',matches')

        
apply p str = case p of
    Comb p' -> p' str
    Seq p' -> let
            Comb p'' = ListBasedCombinators.sequence p'
        in
            p'' str
    Tag k pp -> let
                (status, str2, mms) = apply pp str
                matches = [TaggedMatches k mms] 
            in
                (status, str2, matches)

word :: LComb
word = 
    Comb $ \str -> let
            (_,m,str') = str =~ "^(\\w+)" :: (String,String,String)
            in
                if m /=  ""    
                    then
                        let
                            str2= trimStart str'
                        in
                            (1,str2, [Match m])
                    else 
                        (0,str, [UndefinedMatch])      
                        
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
                            (1,str'', [Match m])
                    else 
                        (0,str, [UndefinedMatch])                            

symbol :: String -> LComb
-- As in Parsec, parses a literal and removes trailing whitespace
symbol lit_str = 
    -- lit_str =~  s/(\W)/\\$1/g FIXME! Escape non-word characters in the literal string
    Comb $  \str -> let
        status=0
        (b,m,str')  = str =~ ("^\\s*"++lit_str++"\\s*") :: (String,String,String)
      in
        if m/=""
            then
                (1,str', [Match lit_str])
            else
                (0,str, [UndefinedMatch])

parens :: LComb -> LComb
parens p =
    Comb $ \str -> let
            (status, str3, ch) = apply (char '(') str
        in
            if (status==1) then
                let
                    str4 = trimStart str3 
                    (st,str4s,matches)= apply p str4
                    status'=status*st
                in
                    if (status'==1) then
                        let
                            (st, str5, ch)=apply (char ')') str4s
                            status'' = status'*st
                        in
                            if status''==1 then  -- OK!
                                let
                                    str6 = trimStart str5
                                in
                                    (1,str6,matches)
                            else -- parse failed on closing paren
                                (0,str5,matches)                
                    else -- parse failed on $ref
                        (0,str4,matches)            
            else -- parse failed on opening paren
                (0,str3,[UndefinedMatch])

char :: Char -> LComb
char ch = 
    Comb $ \str -> let
            c:cs = str
        in            
            if (c == ch) 
                then
                    (1,cs,[Match [ch]])
                else
                    (0,str,[UndefinedMatch])

sepBy :: LComb -> LComb -> LComb
sepBy sep p =
    Comb $ \str -> let         
        ( status, str1, m ) = apply p str
        in
            if status==1 then
                let
                    (st',str2,m') = apply sep str1
                in 
                    if st' == 1 then
                        whileMatches p sep (1,str2,m++m')
                    else
                        (0,str1,m)
            else    -- first match failed.
                ( 0, str1, [UndefinedMatch])
        
whileMatches p sep (st,str,m) =
    let
            str2s = trimStart str
            ( st', str', m' ) = apply p str2s
        in
            if st'==1 then let
                    (status'', str2, m'' ) = apply sep str'
                in
                    if status''==1 then
                        whileMatches p sep (1,str2,m++m'++m'')                
                    else
                        (1, str', m++m' )
            else
                    (0,str,m)                            

-- Choice: try every parser in the list until one succeeds or return fail.  '<|>' in Parsec
-- FIXME: Prototype does not guarantee that parens can be omitted. Should make it binary for that.
choice :: [LComb] -> LComb
choice parsers = 
    Comb $ \str -> choice_helper parsers str
                

choice_helper [] str = (0, str, [])
choice_helper (p:ps) str = let
    (status, str', matches) = apply p str
    in
        if status ==1 then
            (status, str', matches)
        else 
            choice_helper ps str

-- Normally, when a parser parses a string, it removes the portion that matched. If you want to keep the string, wrap the parser in try
try :: LComb -> LComb
try p =
    Comb $ \str -> let
            (status, rest, matches) = apply p str
        in
            if status==1 then
                    (1, rest, matches)
                else
                    (0, str, matches)

-- maybe() is like try() but always succeeds
-- it returns the matches and the consumed string or the orig string and no matches
maybe :: LComb -> LComb
maybe p =
    Comb $ \str -> let
            (status, rest, matches) = apply p str
        in
            if status==1 then
                    (1, rest, matches)
                else
                    (1, str, [UndefinedMatch])

oneOf :: String -> LComb
oneOf patt_lst =
    Comb $ \str -> loopOver str patt_lst

loopOver str (c:cs)  = let
        (status, str', matches ) = apply (char c) str
    in
        if status==1 then
                ( 1, str', matches )
        else
            if cs == "" then
                    (0,str,[UndefinedMatch])
                else
                    loopOver str cs 

-- `many`, as in Parsec, parses 0 or more the specified parsers
many parser = 
	Comb $ \str -> let
        ( status, str', m ) = apply parser str
        in
		if status==1 then
            doMany parser str' m 
		else     -- first match failed.
			( 1, str, [UndefinedMatch] )

-- `many1`, as in Parsec, parses 1 or more the specified parsers
many1 parser = 
	Comb $ \str -> let
        ( status, str', m ) = apply parser str
        in
		if status==1 then
            doMany parser str' m 
		else     -- first match failed.
            ( 0, str, [UndefinedMatch] )
                        
doMany p str ms = let
    ( status, str', m ) = apply p str
    in
    if status == 1 then
        doMany p str' (ms++m)
        else
            (1,str,ms)



-- This parser parses anything up to the first occurrence of a given literal and trailing whitespace
upto lit_str =
    Comb $ \str -> let
            (_,m,str') = str =~ "^(.*?)\\s*lit_str\\s*" :: (String,String,String)
        in
        if m /= "" then 
            ( 1, str', [Match m] )
        else
            ( 0, str, [UndefinedMatch] )

-- This parser parses anything up to the last occurrence of a given literal and trailing whitespace
greedyUpto lit_str =
    Comb $ \str -> let
            (_,m,str') = str =~ "^(.*)\\s*lit_str\\s*" :: (String,String,String)
        in
        if m /= "" then 
            ( 1, str', [Match m] )
        else
            ( 0, str, [UndefinedMatch] )

-- Enough rope: this parser will parse whatever the regex is, stripping trailing whitespace
-- regex regex_str =
-- 	Comb $ \str -> let
-- 		regex_str=~s/\*/\\\*/g;
--         matches = undef
--         in
-- 		if str =~ s/($regex_str)\s*//  then
-- 			m = $1
-- 			matches =m
-- 			 ( 1,str,matches )
-- 		else
--     		 ( 0, str, matches )
-- Matches a comma with optional whitespace
comma = 
    Comb $ \str -> let
        str' = trimStart str
        in
        if head str' == ',' then
            let
                str'' = trimStart (tail str')
            in
                (1, str'', [UndefinedMatch] )
        else
            (0, str, [UndefinedMatch])

semi = 
    Comb $ \str -> let
        str' = trimStart str
        in
        if head str' == ';' then
            let
                str'' = trimStart (tail str')
            in
                (1, str'', [UndefinedMatch] )
        else
            (0, str, [UndefinedMatch])


-- strip leading whitespace, always success
whiteSpace =
    Comb $ \str -> 
        let
            spaces = str =~ "^\\s+" :: String
        in
            (1, drop (length spaces) str, [Match spaces])

trimStart :: String -> String
-- trimStart = dropWhile (\c  -> [c] =~ "^\\s" :: Bool)
trimStart str = let
    spaces = str =~ "^\\s+" :: String
    in
        drop (length spaces) str


{-
data ParseState s = ParseState (Int,s,Matches)

-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
-- So this bit of boilerplate makes it work
instance Functor ParseState where
  fmap = liftM

instance Applicative ParseState where
  pure  = return
  (<*>) = ap


instance Monad ParseState where

(>>=) :: forall a b. m a -> (a -> m b) -> m b 

Sequentially compose two actions, passing any value produced by the first as an argument to the second.

(>>) :: forall a b. m a -> m b -> m b infixl 1Source#

Sequentially compose two actions, discarding any value produced by the first, like sequencing operators (such as the semicolon) in imperative languages.

return :: a -> m a

Inject a value into the monadic type.

fail :: String -> m a    


    mt >>=  p = 
        let
            ParseState (st1,str2,m1) = mt                        
        in            
            if st1==1 then
                let
                    ParseState (st2,str3,m2) = p str2
                in
                    ParseState (st2,str3,m1++m2)                                
            else 
                let
                    ParseState (st2,str3,m2) = p str2
                in
                    ParseState (0,str3,m1)                  
                
        
    return str = ParseState (0,str,[])        

    (>>) = (*>)
    
-}