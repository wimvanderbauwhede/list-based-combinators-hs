module Main where
import Prelude hiding (sequence, maybe)
import ListBasedCombinators (
        sequence,
        word,
        apply,
        whiteSpace,
        symbol,
        natural,
        maybe,
        parens,
        char,
        sepBy,
        sepByChar,
        oneOf,
        choice,
        parens,
        getParseTree,
        LComb(..),
        MTup(..)
    )

main :: IO ()
main = do
    print $ apply test_parser "   hello world   spaces =  7188 ."
    let
        MTup (tpst,tpstr,tpms) = apply type_parser type_str    
    print tpms
    print $ apply (sepBy (symbol "=>") word) "Int => Bool => String"    
    print $ apply (sequence [oneOf "aeiou", word]) "aTest"    
    let
        MTup (st,str,ms) = apply (sequence [word, symbol "=", word,parens word]) "answer = hello(world)"  
    print (st,str,ms)        
    print $ getParseTree tpms

type_str = "integer(kind=8), "
test_parser = 
  sequence [
    whiteSpace,
    Tag "Type" word, 
    word,
    word,
    symbol "=",
    natural
  ]

type_parser =     
    sequence [
        Tag "Type" word,
        maybe $ parens $ 
            choice [
                Tag "Kind" natural,
                sequence [
                    symbol "kind",
                    symbol "=",
                    Tag "Kind" natural
                ] 
            ]
        ]

-- could I do something like:
-- Tag (forall a . a ) LComb
-- and a being for example (\x -> Var x)
-- What is missing of course is that in the end the tag will apply to either String or [String] assuming we can pass this on to
-- data Match = Match String | TaggedMatches (forall a . a) [Match] | UndefinedMatch deriving (Eq,Show)    
-- And that means we need to find a way to go from the String to the type expected by the Term alternative
-- So we actually want String -> Term as the signature, perhaps. And shift the question to how we can go from String to the thing in the alternative
-- I guess it will be easier to write a dedicated TaggedEntry to Term function
