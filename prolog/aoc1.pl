go :-
        File = '../data/data1.txt',
        read_file_to_string(File,Str,[]),
        split_string("\n\n",Str,Lines),
        maplist(get_total,Lines,Totals),

        % Part 1
        max_list(Totals,Max),        
        writeln(Max),

        % Part 2
        msort(Totals,TotalsSorted1), % We don't want to remove duplicates
        reverse(TotalsSorted1,TotalsSorted),
        
        take(TotalsSorted,3,ThreeLargest),
        sum_list(ThreeLargest,ThreeLargestTotal),
        writeln(ThreeLargestTotal),
        nl.

%%
%% Get the totals
%%
get_total(Str,Total) :-
        split_string("\n",Str,L),
        maplist(number_chars,Ns,L),
        sum_list(Ns,Total).


/*
   take(L,N,NFirst)
   Take the first N elements from list L.
*/
take(L,N,NFirst) :-
        take(L,N,[],NFirst).
take(_,0,NFirst,NFirst) :- !.
take([H|T],N,F0,[H|F]) :-
        N > 0,
        N1 is N-1,
        take(T,N1,F0,F).


/*
  split_string_nl(Str,Lines)
  Split string Str on separator Sep (e.g. "\n\n").
  
*/
split_string(Sep,Str,Lines) :-
        re_split(Sep,Str,Lines1,[]),
        delete(Lines1,Sep,Lines).