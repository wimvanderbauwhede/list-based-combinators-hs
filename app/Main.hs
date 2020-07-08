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
        MTup(..),
        TaggedEntry(..)
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
    let
        MTup (tpst',tpstr',tpms') = apply term_parser term_str    
    print tpms'    
    print $ getParseTree tpms'
    let
        term =  taggedEntryToTerm $ head $ getParseTree tpms'
    print term

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

term_str = "a*x^2+ 4*b*x +c"

term_parser =  Tag "Add" $ sequence [
            Tag "Mult" $ sequence [
                Tag "Par" word,
                symbol "*",
                Tag "Pow" $ sequence [
                    Tag  "Var" word,
                    symbol "^",
                    Tag "Const" natural
                ]
            ],          
            symbol "+",
            Tag "Mult" $ sequence [
                Tag "Const" natural,
                symbol "*",
                Tag "Par" word,
                symbol "*",
                Tag "Var" word
                ],     
            symbol "+",       
            Tag "Par" word
        ]

data Term =
    Var String
    | Par String
    | Const Int
    | Pow Term Int
    | Add [Term]
    | Mult [Term]
    deriving (Show)
    -- [("Add",ValMap [("Mult",ValMap [("Par",Val ["a"]),("Pow",ValMap [("Var",Val ["x"]),("Const",Val ["2"])])]),("Mult",ValMap [("Const",Val ["4"]),("Par",Val ["b"]),("Var",Val ["x"])]),("Par",Val ["c"])])]
    
-- data TaggedEntry = Val [String] | ValMap [(String, TaggedEntry)] deriving (Show)



taggedEntryToTerm ("Var" ,Val strs) = Var $ head strs
taggedEntryToTerm ("Par" ,Val strs) = Par $ head strs
taggedEntryToTerm ("Const" ,Val strs) = Const $  read (head strs) 
taggedEntryToTerm ("Pow" , ValMap [t1,(_,Val [v2])]) = Pow (taggedEntryToTerm t1) (read v2)        
taggedEntryToTerm ("Add" , ValMap hmap) = Add $ map taggedEntryToTerm hmap
taggedEntryToTerm ("Mult" , ValMap hmap) = Mult $ map taggedEntryToTerm hmap
        