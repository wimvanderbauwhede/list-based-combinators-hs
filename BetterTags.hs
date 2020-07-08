module TagsToTerms where
data Term =
    Var String
    | Par String
    | Const Int
    | Pow Term Int
    | Add [Term]
    | Mult [Term]

    -- [("Add",ValMap [("Mult",ValMap [("Par",Val ["a"]),("Pow",ValMap [("Var",Val ["x"]),("Const",Val ["2"])])]),("Mult",ValMap [("Const",Val ["4"]),("Par",Val ["b"]),("Var",Val ["x"])]),("Par",Val ["c"])])]
    
-- data TaggedEntry = Val [String] | ValMap [(String, TaggedEntry)] deriving (Show)



taggedEntryToTerm ("Var" ,Val strs) = Var $ head strs
taggedEntryToTerm ("Par" ,Val strs) = Par $ head strs
taggedEntryToTerm ("Const" ,Val strs) = Const $  read (head strs) 

-- taggedEntryToTerm "" (ValMap hmap) = let
--     (k,v) = getTup hmap
--     in
--         taggedEntryToTerm k v

taggedEntryToTerm "Pow" (ValMap [t1,(_,v2)]) = Pow (taggedEntryToTerm t1) (read v2)
         
taggedEntryToTerm "Add" (ValMap hmap) = Add $ map taggedEntryToTerm hmap

taggedEntryToTerm "Mult" (ValMap hmap) = Mult $ map taggedEntryToTerm hmap
