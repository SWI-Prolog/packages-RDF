/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2013, University of Amsterdam
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

:- module(rdf_ntriples_old,
          [ load_rdf_ntriples/2,        % +File, -Triples
            rdf_ntriple_part/4          % +Field, -Value, <DCG>
          ]).


/** <module> RDF N-triples parser (obsolete)

This module parses n-triple files as defined   by the W3C RDF working in
http://www.w3.org/TR/rdf-testcases/#ntriples.   This   format     is   a
simplified version of the RDF N3 notation   used  in the *.nt files that
are used to describe the normative outcome of the RDF test-cases.

The returned list terms are of the form

        rdf(Subject, Predicate, Object)

where

        * Subject
        is an atom or node(Id) for anonymous nodes
        * Predicate
        is an atom
        * Object
        is an atom, node(Id), literal(Atom) or xml(Atom)

@deprecated     This library will shortly be replaced with a stub that
                calls library(semweb/rdf_ntriples).
*/

%       load_rdf_ntriples(+Source, -Triples)
%
%       Load a file or stream to a list of rdf(S,P,O) triples.

load_rdf_ntriples(File, Triples) :-
    open_nt_file(File, In, Close),
    call_cleanup(stream_to_triples(In, Triples), Close).

%       open_nt_file(+Input, -Stream, -Close)
%
%       Open Input, returning Stream and a goal to cleanup Stream if it
%       was opened.

open_nt_file(stream(Stream), Stream, true) :- !.
open_nt_file(Stream, Stream, true) :-
    is_stream(Stream),
    !.
open_nt_file(Spec, Stream, close(Stream)) :-
    absolute_file_name(Spec,
                       [ access(read),
                         extensions([nt,''])
                       ], Path),
    open(Path, read, Stream).


%       rdf_ntriple_part(+Type, -Value, <DCG>)
%
%       Parse one of the fields of  an   ntriple.  This  is used for the
%       SWI-Prolog Sesame (www.openrdf.org) implementation   to  realise
%       /servlets/removeStatements. I do not think   public  use of this
%       predicate should be stimulated.

rdf_ntriple_part(subject, Subject) -->
    subject(Subject).
rdf_ntriple_part(predicate, Predicate) -->
    predicate(Predicate).
rdf_ntriple_part(object, Object) -->
    predicate(Object).


%       stream_to_triples(+Stream, -ListOfTriples)
%
%       Read Stream, returning all its triples

stream_to_triples(In, Triples) :-
    read_line_to_codes(In, Line),
    (   Line == end_of_file
    ->  Triples = []
    ;   phrase(line(Triples, Tail), Line),
        stream_to_triples(In, Tail)
    ).

line(Triples, Tail) -->
    wss,
    (   comment
    ->  {Triples = Tail}
    ;   triple(Triple)
    ->  {Triples = [Triple|Tail]}
    ).

comment -->
    "#",
    !,
    skip_rest.
comment -->
    end_of_input.

triple(rdf(Subject, Predicate, Object)) -->
    subject(Subject), ws, wss,
    predicate(Predicate), ws, wss,
    object(Object), wss, ".", wss.

subject(Subject) -->
    uniref(Subject),
    !.
subject(Subject) -->
    node_id(Subject).

predicate(Predicate) -->
    uniref(Predicate).

object(Object) -->
    uniref(Object),
    !.
object(Object) -->
    node_id(Object).
object(Object) -->
    literal(Object).


uniref(URI) -->
    "<",
    escaped_uri_codes(Codes),
    ">",
    !,
    { atom_codes(URI, Codes)
    }.

node_id(node(Id)) -->                   % anonymous nodes
    "_:",
    name_start(C0),
    name_codes(Codes),
    { atom_codes(Id, [C0|Codes])
    }.

literal(Literal) -->
    lang_string(Literal),
    !.
literal(Literal) -->
    xml_string(Literal).


%       name_start(-Code)
%       name_codes(-ListfCodes)
%
%       Parse identifier names

name_start(C) -->
    [C],
    { code_type(C, alpha)
    }.

name_codes([C|T]) -->
    [C],
    { code_type(C, alnum)
    },
    !,
    name_codes(T).
name_codes([]) -->
    [].


%       escaped_uri_codes(-CodeList)
%
%       Decode string holding %xx escaped characters.

escaped_uri_codes([]) -->
    [].
escaped_uri_codes([C|T]) -->
    "%", [D0,D1],
    { code_type(D0, xdigit(V0)),
      code_type(D1, xdigit(V1)),
      !,
      C is V0<<4 + V1
    },
    escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
    "\\u", [D0,D1,D2,D3],
    !,
    { code_type(D0, xdigit(V0)),
      code_type(D1, xdigit(V1)),
      code_type(D2, xdigit(V2)),
      code_type(D3, xdigit(V3)),
      C is V0<<12 + V1<<8 + V2<<4 + V3
    },
    escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
    "\\U", [D0,D1,D2,D3,D4,D5,D6,D7],
    !,
    { code_type(D0, xdigit(V0)),
      code_type(D1, xdigit(V1)),
      code_type(D2, xdigit(V2)),
      code_type(D3, xdigit(V3)),
      code_type(D4, xdigit(V4)),
      code_type(D5, xdigit(V5)),
      code_type(D6, xdigit(V6)),
      code_type(D7, xdigit(V7)),
      C is V0<<28 + V1<<24 + V2<<20 + V3<<16 +
           V4<<12 + V5<<8 + V6<<4 + V7
    },
    escaped_uri_codes(T).
escaped_uri_codes([C|T]) -->
    [C],
    escaped_uri_codes(T).


%       lang_string()
%
%       Process a language string

lang_string(String) -->
    "\"",
    string(Codes),
    "\"",
    !,
    { atom_codes(Atom, Codes)
    },
    (   langsep
    ->  language(Lang),
        { String = literal(lang(Lang, Atom))
        }
    ;   "^^"
    ->  uniref(Type),
        { String = literal(type(Type, Atom))
        }
    ;   { String = literal(Atom)
        }
    ).

langsep -->
    "-".
langsep -->
    "@".

%       xml_string(String)
%
%       Handle xml"..."

xml_string(xml(String)) -->
    "xml\"",                        % really no whitespace?
    string(Codes),
    "\"",
    { atom_codes(String, Codes)
    }.

string([]) -->
    [].
string([C0|T]) -->
    string_char(C0),
    string(T).

string_char(0'\\) -->
    "\\\\".
string_char(0'") -->
    "\\\"".
string_char(10) -->
    "\\n".
string_char(13) -->
    "\\r".
string_char(9) -->
    "\\t".
string_char(C) -->
    "\\u",
    '4xdigits'(C).
string_char(C) -->
    "\\U",
    '4xdigits'(C0),
    '4xdigits'(C1),
    { C is C0<<16 + C1
    }.
string_char(C) -->
    [C].

'4xdigits'(C) -->
    [C0,C1,C2,C3],
    { code_type(C0, xdigit(V0)),
      code_type(C1, xdigit(V1)),
      code_type(C2, xdigit(V2)),
      code_type(C3, xdigit(V3)),

      C is V0<<12 + V1<<8 + V2<<4 + V3
    }.

%       language(-Lang)
%
%       Return xml:lang language identifier.

language(Lang) -->
    lang_code(C0),
    lang_codes(Codes),
    { atom_codes(Lang, [C0|Codes])
    }.

lang_code(C) -->
    [C],
    { C \== 0'.,
      \+ code_type(C, white)
    }.

lang_codes([C|T]) -->
    lang_code(C),
    !,
    lang_codes(T).
lang_codes([]) -->
    [].


                 /*******************************
                 *             BASICS           *
                 *******************************/

skip_rest(_,[]).

ws -->
    [C],
    { code_type(C, white)
    }.

end_of_input([], []).


wss -->
    ws,
    !,
    wss.
wss -->
    [].
