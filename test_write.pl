/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_rdf_write,
          [ test_write/0,
            run_tests/0,
            run_tests/1
          ]).

:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(foreign, '../semweb')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

:- use_module(library(plunit)).
:- use_module(library(rdf_write)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf).

test_write :-
    run_tests([ rdf_write
              ]).


                 /*******************************
                 *          ROUND TRIP          *
                 *******************************/

test_graph(Triples) :-
    tmp_file(rdf, Tmp),
    open(Tmp, write, Out, [encoding(utf8)]),
    rdf_write_xml(Out, Triples),
    close(Out),
    cat(Tmp),
    load_rdf(Tmp, ReadTriples),
    delete_file(Tmp),
    compare_triples(Triples, ReadTriples, _).

cat(File) :-
    debugging(rdf_write),
    !,
    open(File, read, In, [encoding(utf8)]),
    copy_stream_data(In, current_output),
    close(In).
cat(_).

                 /*******************************
                 *           COMPARING          *
                 *******************************/

%       compare_triples(+PlRDF, +NTRDF, -Substitions)
%
%       Compare two models and if they are equal, return a list of
%       PlID = NTID, mapping NodeID elements.


compare_triples(A, B, Substitutions) :-
    compare_list(A, B, [], Substitutions),
    !.

compare_list([], [], S, S).
compare_list([H1|T1], In2, S0, S) :-
    select(H2, In2, T2),
    compare_triple(H1, H2, S0, S1),
    compare_list(T1, T2, S1, S).

compare_triple(rdf(Subj1,P1,O1), rdf(Subj2, P2, O2), S0, S) :-
    compare_field(Subj1, Subj2, S0, S1),
    compare_field(P1, P2, S1, S2),
    compare_field(O1, O2, S2, S).

compare_field(X, X, S, S) :- !.
compare_field(literal(X), xml(X), S, S) :- !. % TBD
compare_field(rdf:Name, Atom, S, S) :-
    atom(Atom),
    rdf_parser:rdf_name_space(NS),
    atom_concat(NS, Name, Atom),
    !.
compare_field(NS:Name, Atom, S, S) :-
    atom(Atom),
    atom_concat(NS, Name, Atom),
    !.
compare_field(X, Id, S, S) :-
    memberchk(X=Id, S),
    !.
compare_field(X, Y, S, [X=Y|S]) :-
    \+ memberchk(X=_, S),
    rdf_is_bnode(X),
    rdf_is_bnode(Y),
    debug(bnode, 'Assume ~w = ~w~n', [X, Y]).


                 /*******************************
                 *            TESTS             *
                 *******************************/

:- begin_tests(rdf_write).

test(1, true) :-
    test_graph([ rdf(s, p, o)
               ]).
test(anon_s, true) :-
    test_graph([ rdf('_:s', p, o)
               ]).
test(anon_o, true) :-
    test_graph([ rdf(s, p, '_:o')
               ]).
test(anon_loop, blocked('NodeID map must check for cycles')) :-
    test_graph([ rdf('_:r1', p1, '_:r2'),
                 rdf('_:r2', p1, '_:r1')
               ]).
test(anon_loop, true) :-
    test_graph([ rdf('_:r1', p1, '_:r2'),
                 rdf('_:r1', p2, '_:r2'),
                 rdf('_:r2', p1, '_:r1'),
                 rdf('_:r2', p2, '_:r1')
               ]).
test(anon_reuse, true) :-
    test_graph([ rdf('_:s1', p1, '_:o1'),
                 rdf('_:s2', p1, '_:o1')
               ]).
test(anon_reuse, true) :-
    test_graph([ rdf('_:s1', p1, '_:o1'),
                 rdf('_:s2', p1, '_:o1'),
                 rdf('_:o1', name, literal(foo))
               ]).
test(literal, true) :-
    test_graph([ rdf(s, p, literal(hello))
               ]).
test(lang, true) :-
    test_graph([ rdf(s, p, literal(lang(en, hello)))
               ]).
test(type, true) :-
    test_graph([ rdf(s, p, literal(type(t, hello)))
               ]).
test(iri_l1, true) :-
    R = 'http://www.example.com/één_twee#r',
    test_graph([ rdf(R,R,R)
               ]).
test(iri_amp, true) :-
    R = 'http://www.example.com/een&twee#r',
    test_graph([ rdf(R,R,R)
               ]).
test(iri_space, true) :-
    R = 'http://www.example.com/een%20twee#r',
    test_graph([ rdf(R,R,R)
               ]).

:- end_tests(rdf_write).


