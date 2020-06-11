(Int, Str, Matches) -> (Str -> (Int, Str, Matches)) -> (Int, Str, Matches)

(>>=) mt@(st1, str1, ms1) p
| st == 0 = mt
| otherwise = let
    (st2,str2,ms2) = p str1 
    in
       if st2==0 then mt else (1,str2,ms1++ms2)
