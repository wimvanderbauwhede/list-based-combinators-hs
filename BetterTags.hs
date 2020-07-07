module Main where
data Term =
    Var String
    | Par String
    | Const Int
    | Pow Term Int
    | Add [Term]
    | Mult [Term]

Tag "Add" sequence [
    Tag "Mult" sequence ["Const" natural,"Var" word],
    Tag "Mult" sequence ["Const" natural,"Var" word],
    Tag "Mult" sequence ["Const" natural,"Var" word]
]

data TaggedEntry = Val [String] | ValMap [(String, TaggedEntry)] deriving (Show)



taggedEntryToTerm "Var" (Val strs) = Var $ head strs
taggedEntryToTerm "Par" (Val strs) = Par $ head strs
taggedEntryToTerm "Const" (Val strs) = Const $  read (head strs) 
taggedEntryToTerm "" (ValMap hmap) = let
    (k,v) = getTup hmap
    in
        taggedEntryToTerm k v
taggedEntryToTerm "Pow" (ValMap hmap) = let
    (k,v1:v2:[]) = getTup hmap
    in
        Pow (taggedEntryToTerm k v1) (read v2) 
taggedEntryToTerm "Add" (ValMap hmap) = let
    (k,v1:v2:[]) = getTup hmap
    in
        Pow (taggedEntryToTerm k v1) (read v2) 

