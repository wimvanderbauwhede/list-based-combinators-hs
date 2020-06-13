module F95VarDeclParser where
import Prelude hiding (sequence, maybe)
import ListBasedCombinators (
        sequence,
        word,
        mixedCaseWord,
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
        try,
        parens,
        comma,
        regex,
        getParseTree,
        LComb(..),
        MTup(..),
        Match(..)
    )

parse_F95_var_decl str = let
        p = f95_var_decl_parser 
        MTup (st, rest, matches) = apply p str
        -- print "\n" if VV
        -- print 'REST:'. "\n".Dumper(rest),"\n"  if VV    
        -- print 'MATCHES:'.Dumper(matches),"\n"  if VV
        pt = getParseTree matches
        -- print 'PARSE TREE:'.Dumper(pt),"\n" if VV
{-            
        typetup = pt->{TypeTup}
        if (exists typetup->{'Type'}{'Opt'}) {
            typetup->{'Type'}= typetup->{'Type'}{'Main'}. ' ' . typetup->{'Type'}{'Opt'}
        } else {
            typetup->{'Type'}= typetup->{'Type'}{'Main'}
        }
        if (ref(typetup) eq 'ARRAY') {
            
            pt->{TypeTup} = { each %{typetup->[0]}, Kind => 4}
        } elsif (exists pt->{TypeTup}{Kind}) {
            
            pt->{TypeTup}{Kind}*=1 unless pt->{TypeTup}{Kind} eq '*'
        }
        if ((exists pt->{Attributes}) && (ref(pt->{Attributes}) eq 'HASH')) {
            if ( exists pt->{Attributes}{Dim}) {
               dim = pt->{Attributes}{Dim}    
               if (ref(dim) ne 'ARRAY') {
                  pt->{Attributes}{Dim} = [dim]
                }
            } else {
                pt->{Attributes}{Dim} = [0]
            }
        }
    
        varlist = pt->{Vars} 
        if (ref(varlist) ne 'ARRAY') {
            pt->{Vars} = [varlist]
        }
    
        parlist = pt->{'Pars'}
        lhs =parlist->{Lhs}
        rhs =parlist->{Rhs}
             pt->{'Pars'} = {Var => lhs, Val => rhs}
        if (not exists pt->{AccPragma}) {
            pt->{AccPragma} = {AccKeyword => 'ArgMode', AccVal => 'ReadWrite'}
        }
        
    if (exists  pt->{VarsDims} && exists  pt->{VarsDims}{'Dim'} ) {
        if (ref(pt->{VarsDims}{'Dim'}) eq 'ARRAY' and  @{  pt->{VarsDims}{'Dim'} } > 0 ) {
        
            my @dims = map { [  map { ':' } @{ _->{'Sep'} }] } @{pt->{VarsDims}{Dim}}
            pt->{Attributes}{Dim}=\@dims
            pt->{'Vars'}= pt->{VarsDims}{Var}
            delete  pt->{VarsDims} 
        } else {
            my @dims = ( [  map { ':' } @{ pt->{VarsDims}{Dim}{Sep} } ] )
            pt->{Attributes}{Dim}=\@dims
            pt->{'Vars'}= [pt->{VarsDims}{Var}]
            delete  pt->{VarsDims} 
        }
-}
    in
       pt
        

f95_var_decl_parser =
    sequence [
        whiteSpace,
        Tag "TypeTup"  type_parser,
        (maybe $
        sequence [
        comma,
        Tag "Attributes"  (sepByChar ',' attribute_parser)
        ]
        ),
        varlist_parser,
        (maybe openacc_pragma_parser) 
    ] 

-- where
attribute_parser =     
    choice [dim_parser, intent_parser, parameter_parser, allocatable_parser, volatile_parser]

type_parser =
        sequence [
        Tag "Type" $ sequence [Tag "Main" word, maybe $ Tag "Opt" word ],
        choice [
            sequence [symbol "*",Tag "Kind"  natural ],
            maybe $ parens $ choice
                    [Tag "Kind"  natural,
                    sequence [
                        choice [symbol"kind", symbol "len"] ,
                        symbol "=",
                        Tag "Kind" $ choice [natural, char '*']
                        ] 
                    ]
            ] 
        ]

dim_parser =
        sequence [
            symbol "dimension" ,
-- This is a very ugly hack, it works only for 1 pair of parens
-- What this matches is "start with anything except ",)(", then "(" whatever ")"
            Tag "Dim" $ parens comma_sep_expr_list 
        ] 

intent_parser =
        sequence [
        symbol "intent",
        Tag "Intent" $ parens mixedCaseWord
        ]

parameter_parser = symbol "parameter"

volatile_parser = symbol "volatile"

allocatable_parser = Tag "Allocatable" $ symbol "allocatable"

varlist_parser =
    sequence [
    symbol "::",        
    choice [
         Tag "Pars" $ try $ sepBy comma param_assignment
        ,Tag "VarsDims" $ sepByChar ',' $
                sequence [
                    Tag "Var" word,
                    Tag "Dim" $ parens $
                    Tag "Sep" $ sepBy comma (symbol ":")
                ]
        ,Tag "Vars" $ sepByChar ',' word
        ]
    ]

param_assignment =
    sequence [
        Tag "Lhs" word,
        symbol "=",              
        Tag "Rhs" $ choice [word,regex "[\\-\\.\\dedq]+"] --FIXME  weak !
    ] 

openacc_pragma_parser = sequence [
        char '!',
        whiteSpace,
        choice [symbol "$ACC" , symbol "$acc"],
        Tag "AccPragma" $ sequence [
            Tag "AccKeyWord" word,
            Tag "AccVal"  word
            ] 
        ]

-- matches an unsigned integer
comma_sep_expr_list =
     Comb $ \str -> let
        (matches, remainder) = _parse_comma_sep_expr_list str 
        in
            if  length matches > 0  then            
                MTup ( 1, remainder, map Match matches )            
            else 
                MTup ( 0, str, [UndefinedMatch] )   -- assumes status is 0|1, str is string, matches is [string]

-- _parse_comma_sep_expr_list str = ([],"")
_parse_comma_sep_expr_list str = __loopOverIdx 0 (str,(length) str - 1, 0,0,"", [],0)

__loopOverIdx ch_idx (chars,ncharsm1,found_parens,parens_count,matched_str, matched_strs,last)
    | last == 1 = (matched_strs,chars)
    | ch_idx == ncharsm1 = (matched_strs,chars)
    | otherwise = let
            ch:chars' = chars -- eat a char
            (chars'',found_parens',parens_count',matched_str', matched_strs',last) =
                if ch == '(' then -- do next
                    (chars'',1,parens_count+1,matched_str++[ch], matched_strs,0)
                else if ch == ')' then 
                    -- see if we found parens before
                    if found_parens == 1 && parens_count == 1 then
                        -- adapt parens count
                        (chars',0,parens_count-1,matched_str++[ch],matched_strs++[matched_str],0)
                    else if parens_count<0 then
                        -- this means we should stop
                        (chars'++")",found_parens',parens_count',matched_str++[ch],matched_strs,1)                        
                    else 
                        -- add the paren to the matched str
                        (chars',found_parens,parens_count,matched_str++[ch],matched_strs,0)
                else if ( ch == ',' && found_parens == 0 ) then
                    -- we found a comma so we have a string, add it to matched_strs and reset
                    (chars'',found_parens',parens_count',"", matched_strs++[matched_str],0)
                else if ch /= ' ' then
                    -- add the character to the string
                    (chars',found_parens,parens_count,matched_str++[ch], matched_strs,last)
                else
                    -- skip spaces
                    (chars',found_parens,parens_count,matched_str, matched_strs,last)                
            -- if we're at the end of the string, return whatever remains
            matched_strs'' = if ch_idx == ncharsm1 then
                    matched_strs' ++ [matched_str]
                else
                    matched_strs'
        in
            __loopOverIdx (ch_idx+1) (chars'',ncharsm1,found_parens',parens_count',matched_str', matched_strs'',last)
            