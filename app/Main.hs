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
        oneOf,
        choice,
        parens,
        LComb(..)
    )

main :: IO ()
main = do
    print $ apply test_parser "   hello world   spaces =  7188 ."
    print $ apply type_parser type_str    
    print $ apply (sepBy (symbol "=>") word) "Int => Bool => String"    
    print $ apply (sequence [oneOf "aeiou", word]) "aTest"    
    print $ apply (sequence [word, symbol "=", word,parens word]) "answer = hello(world)"  

type_str = "integer(kind=8), "
test_parser = 
  sequence [
    whiteSpace,
    (Tag "Type" word), 
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
                    (Tag "Kind" natural)
                ] 
            ]
        ]

    