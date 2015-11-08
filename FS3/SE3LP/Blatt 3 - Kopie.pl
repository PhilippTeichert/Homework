:- discontiguous(directory/5).
:- dynamic(file/6).

% directory(DirId,Name,ParentId,DateCreated,DateModified)

directory(1,root,0,date(2007,5,2),date(2007,5,2)).
directory(2,bilder,1,date(2007,5,2),date(2009,11,2)).
directory(3,musik,1,date(2007,5,2),date(2009,10,4)).
directory(4,dokumente,1,date(2007,5,2),date(2009,11,5)).
directory(5,urlaub,2,date(2008,6,22),date(2009,8,15)).
directory(6,hochzeit,2,date(2008,1,27),date(2008,1,31)).
directory(7,kinder,2,date(2007,5,2),date(2009,9,5)).
directory(8,klassik,3,date(2007,5,2),date(2007,5,2)).
directory(9,pop,3,date(2007,5,2),date(2009,11,5)).
directory(10,urlaub,4,date(2008,5,23),date(2008,11,1)).
directory(11,hochzeit,4,date(2007,12,4),date(2008,1,25)).
directory(12,scheidung,4,date(2009,9,2),date(2009,11,5)).

% file(FileId,DirId,Name,Size,DateCreated,DateModified)

file(1,9,in_the_summertime,56,date(2007,5,2),date(2009,11,5)).
file(2,9,i_am_so_romantic_tonight,31,date(2007,5,2),date(2009,11,5)).
file(3,9,ich_und_du_fuer_immer,67,date(2007,5,2),date(2009,11,5)).
file(4,9,paris,267,date(2008,6,3),date(2008,6,3)).
file(7,10,quartieranfrage,1,date(2007,5,2),date(2009,11,5)).
file(13,5,paris,251,date(2008,6,22),date(2008,6,17)).
file(14,5,dijon,217,date(2008,6,22),date(2008,6,17)).
file(15,5,die_bruecke_von_avignon,191,date(2008,6,22),date(2008,6,17)).
file(21,6,polterabend,238,date(2008,1,27),date(2008,1,31)).
file(22,6,standesamt,244,date(2008,1,27),date(2008,1,31)).
file(23,6,kirche,158,date(2008,1,28),date(2008,1,31)).
file(24,6,festessen,151,date(2008,1,28),date(2008,1,31)).
file(25,11,standesamt,33,date(2007,6,3),date(2007,6,3)).
file(34,12,scheidungsklage,48,date(2009,9,2),date(2009,11,5)).






%%%%%%%%%%%%%%%%%%%%%%%%% datenverzeichnis.pl ^^^^^^
%%%%%%%%%%%%%%%%%%%%%%%%% unser Stuff vvvvvvv



%%%%% Aufgabe 1

% Syntax:
%     Die Syntax beschreibt die Regeln, die den Aufbau der Sprache festlegen.

% Semantik:
%     Die Semantik beschreibt die Bedeutung der Strukturen.

% denotationelle Semantik:
%     Denotationelle Semantik abstrahiert Programmausführung und ist mathematisch einfacher beweisbar.

% operationale Semantik:
%     Operationelle Semantik führt jeden Schritt der Programmausführung auf.

% syntaktische Fehler:
%     Der Compiler fliegt dir um die Ohnren.

% semantische Fehler:
%     Der Compiler meckert nichzt, aber es kommt (meistens) nicht das richtige Ergebnis raus.


%%%%% Aufgabe 2.1


:- dynamic(nameUndIdFile/2).


% nameUndIdFile(?,?)
% nameUndIdFile(name, id)

nameUndIdFile(X, Y) :- file(Y, _, X, _, _, _), print("Name: "), print(X), print(" ID: "), print(Y).


%%%%% Aufgabe 2.2


:- dynamic(nameUndIdDirectory/2).


% nameUndIdDirectory(?,?)
% nameUndIdDirectory(name, id)

nameUndIdDirectory(X, Y) :- directory(Y, X, _, _, _), print("Name: "), print(X), print(" ID: "), print(Y).


%%%%% Aufgabe 2.3

:- dynamic(dateiToVerzeichnis/1).

% dateiToVerzeichnis(+)
% dateiToVerzeichnis(Dateiname)

dateiToVerzeichnis(X) :- file(_, Y, X, _, _, _), directory(Y, Z, _, _, _), print("Schlussel: "), print(Y), print(" Verzeichnisname: "), print(Z).


%%%%% Aufgabe 2.4

:- dynamic(parentFolder/1).

% parentFolder(+)
% parentFolder(Verzeichnisname)

parentFolder(X) :- directory(_, X, Y, _, _), directory(Y, Z, _, _, _), print("Schlussel: "), print(Y), print(" Verzeichnisname: "), print(Z).


%%%%% Aufgabe 3.1

:- dynamic(verzeichnisinhalt/1).

% verzeichnisinhalt(+)
% verzeichnisinhalt(Verzeichnisschlüssel)

verzeichnisinhalt(X) :- findall(Name, file(_, X, Name, _, _, _), Inhalt), print("Inhalt: "), print(Inhalt).


%%%%% Aufgabe 3.2

:- dynamic(unterverzeichnisse/1).

% unterverzeichnisse(+)
% unterverzeichnisse(Verzeichnisschlüssel)

unterverzeichnisse(X) :- findall(Name, directory(_, Name, X, _, _), Unterverzeichnisse), print("Unterverzeichnisse: "), print(Unterverzeichnisse).


%%%%% Aufgabe 3.3

:- dynamic(dateienanzahl/1).

% dateienanzahl(+)
% dateienanzahl(Verzeichnisschlüssel)

dateienanzahl(X) :- findall(FileID, file(FileID, X, _, _, _, _), Dateien), length(Dateien, Länge), print("Dateien: "), print(Länge).


%%%%% Aufgabe 4.1

directory(100,test,100,date(2015,11,5),date(2015,11,5)).


:- dynamic(änderungsdatum/1).

% änderungsdatum(+)
% änderungsdatum(Verzeichnisschlüssel)

änderungsdatum(X) :- if(directory(X, _, _, _, _), (date(Today), directory(X, A, B, C, _), schreibe(directory(X, A, B, C, Today)), print("Transaktion erfolgreich"), print(Today)), (print("Verzeichnis "), print(x), print(" existiert nicht"))).




























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% stuff für das if



/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sicstus,
	  [ (block)/1,			% +Heads

	    if/3,			% :If, :Then, :Else

	    use_module/3,		% ?Module, ?File, +Imports

	    bb_put/2,			% :Key, +Value
	    bb_get/2,			% :Key, -Value
	    bb_delete/2,		% :Key, -Value
	    bb_update/3,		% :Key, -Old, +New

	    create_mutable/2,		% ?Value, -Mutable
	    get_mutable/2,		% ?Value, +Mutable
	    update_mutable/2,		% ?Value, !Mutable

	    read_line/1,		% -Codes
	    read_line/2,		% +Stream, -Codes

	    trimcore/0,

%	    call_residue/2,		% :Goal, -Residue

	    prolog_flag/3,		% +Flag, -Old, +New
	    prolog_flag/2,		% +Flag, -Value

	    op(1150, fx, (block))
	  ]).

:- use_module(sicstus/block).
:- use_module(library(occurs)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(arithmetic)).


/** <module> SICStus compatibility library

This library is intended to be activated   using  the directive below in
files that are designed for use with  SICStus Prolog. The changes are in
effect until the end of the file and in each file loaded from this file.

    ==
    :- expects_dialect(sicstus).
    ==

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package.
*/

:- multifile
	system:goal_expansion/2.


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_sicstus_library
%
%	Pushes searching for dialect/sicstus in   front of every library
%	directory that contains such as sub-directory.

push_sicstus_library :-
	(   absolute_file_name(library(dialect/sicstus), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, sicstus))),
	    fail
	;   true
	).


:- push_sicstus_library.


		 /*******************************
		 *	      OPERATORS		*
		 *******************************/

%	declare all operators globally

system:goal_expansion(op(Pri,Ass,Name),
		      op(Pri,Ass,user:Name)) :-
	\+ qualified(Name),
	prolog_load_context(dialect, sicstus).

qualified(Var) :- var(Var), !, fail.
qualified(_:_).


%%	setup_dialect
%
%	Further dialect initialization.

setup_dialect.


		 /*******************************
		 *	      CONTROL		*
		 *******************************/

:- meta_predicate
	if(0,0,0).

system:goal_expansion(if(If,Then,Else),
		      (If *-> Then ; Else)) :-
	prolog_load_context(dialect, sicstus),
	\+ (sub_term(X, [If,Then,Else]), X == !).

%%	if(:If, :Then, :Else)
%
%	Same  as  SWI-Prolog  soft-cut  construct.   Normally,  this  is
%	translated using goal-expansion. If either term contains a !, we
%	use meta-calling for full compatibility (i.e., scoping the cut).

if(If, Then, Else) :-
	(   If
	*-> Then
	;   Else
	).


		 /*******************************
		 *	  LIBRARY MODULES	*
		 *******************************/

%%	rename_module(?SICStusModule, ?RenamedSICSTusModule) is nondet.
%
%	True if RenamedSICSTusModule is the  name   that  we use for the
%	SICStus native module SICStusModule. We do  this in places where
%	the module-name conflicts. All explicitely   qualified goals are
%	mapped to the SICStus equivalent of the module.

:- multifile
	rename_module/2.

system:goal_expansion(M:Goal, SicstusM:Goal) :-
	atom(M),
	rename_module(M, SicstusM),
	prolog_load_context(dialect, sicstus).


		 /*******************************
		 *	     MODULES		*
		 *******************************/

% SICStus use_module/1 does not require the target to be a module.

system:goal_expansion(use_module(File), load_files(File, [if(changed)])).

%%	use_module(+Module, -File, +Imports) is det.
%%	use_module(-Module, +File, +Imports) is det.
%
%	This predicate can be used to import   from a named module while
%	the file-location of the module is unknown   or to get access to
%	the module-name loaded from a file.
%
%	If both Module and File are  given,   we  use  Module and try to
%	unify File with the absolute  canonical   path  to the file from
%	which Module was loaded. However, we   succeed regardless of the
%	success of this unification.

use_module(Module, File, Imports) :-
	atom(Module), !,
	module_property(Module, file(Path)),
	use_module(Path, Imports),
	ignore(File = Path).
use_module(Module, File, Imports) :-
	ground(File), !,
	absolute_file_name(File, Path,
			   [ file_type(prolog),
			     access(read)
			   ]),
	use_module(Path, Imports),
	module_property(Module, file(Path)).
use_module(Module, _, _Imports) :-
	instantiation_error(Module).


		 /*******************************
		 *	 FOREIGN RESOURCES      *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SICStus uses foreign_resource(Name, Functions) and predicate definitions
similar to Quintus. qpforeign can generate  the   glue  code that can be
linked with swipl-ld. This  part  of   the  emulation  merely  skips the
declarations and Maps load_foreign_resource   to load_foreign_resource/2
from library(qpforeign).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

system:term_expansion(
	   (:- load_foreign_resource(Base)),
	   (:- initialization(load_foreign_resource(M:Base, Source), now))) :-
	prolog_load_context(source, Source),
	prolog_load_context(module, M).
system:term_expansion(
	   (:- module(Name, Exports, Options)),
	   [ (:- module(Name, Exports))
	   | Declarations
	   ]) :-
	prolog_load_context(dialect, sicstus),
	phrase(sicstus_module_decls(Options), Declarations).

sicstus_module_decls([]) --> [].
sicstus_module_decls([H|T]) -->
	sicstus_module_decl(H),
	sicstus_module_decls(T).

sicstus_module_decl(hidden(true)) --> !,
	[(:- set_prolog_flag(generate_debug_info, false))].
sicstus_module_decl(_) -->
	[].


		 /*******************************
		 *	       BB_*		*
		 *******************************/

:- meta_predicate
	bb_put(:, +),
	bb_get(:, -),
	bb_delete(:, -),
	bb_update(:, -, +).

system:goal_expansion(bb_put(Key, Value), nb_setval(Atom, Value)) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_get(Key, Value), nb_current(Atom, Value)) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_delete(Key, Value),
		      (	  nb_current(Atom, Value),
			  nb_delete(Atom)
		      )) :-
	bb_key(Key, Atom).
system:goal_expansion(bb_update(Key, Old, New),
		      (	  nb_current(Atom, Old),
			  nb_setval(Atom, New)
		      )) :-
	bb_key(Key, Atom).

bb_key(Module:Key, Atom) :-
	atom(Module), !,
	atomic(Key),
	atomic_list_concat([Module, Key], :, Atom).
bb_key(Key, Atom) :-
	atomic(Key),
	prolog_load_context(module, Module),
	atomic_list_concat([Module, Key], :, Atom).

%%	bb_put(:Name, +Value) is det.
%%	bb_get(:Name, -Value) is semidet.
%%	bb_delete(:Name, -Value) is semidet.
%%	bb_update(:Name, -Old, +New) is semidet.
%
%	SICStus compatible blackboard routines. The implementations only
%	deal with cases where the module-sensitive   key  is unknown and
%	meta-calling. Simple cases are  directly   mapped  to SWI-Prolog
%	non-backtrackable global variables.

bb_put(Key, Value) :-
	bb_key(Key, Name),
	nb_setval(Name, Value).
bb_get(Key, Value) :-
	bb_key(Key, Name),
	nb_current(Name, Value).
bb_delete(Key, Value) :-
	bb_key(Key, Name),
	nb_current(Name, Value),
	nb_delete(Name).
bb_update(Key, Old, New) :-
	bb_key(Key, Name),
	nb_current(Name, Old),
	nb_setval(Name, New).


		 /*******************************
		 *	     MUTABLES		*
		 *******************************/

%%	create_mutable(?Value, -Mutable) is det.
%
%	Create a mutable term with the given initial Value.
%
%	@compat sicstus

create_mutable(Value, '$mutable'(Value,_)).

%%	get_mutable(?Value, +Mutable) is semidet.
%
%	True if Value unifies with the current value of Mutable.
%
%	@compat sicstus

get_mutable(Value, '$mutable'(Value,_)).

%%	update_mutable(?Value, !Mutable) is det.
%
%	Set the value of Mutable to Value.  The old binding is
%	restored on backtracking.
%
%	@see setarg/3.
%	@compat sicstus

update_mutable(Value, Mutable) :-
	functor(Mutable, '$mutable', 2), !,
	setarg(1, Mutable, Value).
update_mutable(_, Mutable) :-
	type_error(mutable, Mutable).


		 /*******************************
		 *	   LINE READING		*
		 *******************************/

%%	read_line(-Codes) is det.
%%	read_line(+Stream, -Codes) is det.
%
%	Read a line from the given or  current input. The line read does
%	_not_ include the line-termination character. Unifies Codes with
%	=end_of_file= if the end of the input is reached.
%
%	@compat sicstus
%	@see	The SWI-Prolog primitive is read_line_to_codes/2.

read_line(Codes) :-
    read_line_to_codes(current_input, Codes).

read_line(Stream, Codes) :-
    read_line_to_codes(Stream, Codes).


		 /*******************************
		 *  COROUTINING & CONSTRAINTS	*
		 *******************************/

/* This is more complicated.  Gertjan van Noord decided to use
   copy_term/3 in Alpino.

%%	call_residue(:Goal, -Residue) is nondet.
%
%	Residue is a list of VarSet-Goal.  Note that this implementation
%	is   incomplete.   Please   consult     the   documentation   of
%	call_residue_vars/2 for known issues.

:- meta_predicate
	call_residue(0, -).

call_residue(Goal, Residue) :-
	call_residue_vars(Goal, Vars),
	(   Vars == []
	->  Residue = []
	;   copy_term(Vars, _AllVars, Goals),
	    phrase(vars_by_goal(Goals), Residue)
	).

vars_by_goal((A,B)) --> !,
	vars_by_goal(A),
	vars_by_goal(B).
vars_by_goal(Goal) -->
	{ term_attvars(Goal, AttVars),
	  sort(AttVars, VarSet)
	},
	[ VarSet-Goal ].
*/

%%	trimcore
%
%	Trims the stacks.  Other tasks of the SICStus trimcore/0 are
%	automatically scheduled by SWI-Prolog.

trimcore :-
	trim_stacks.


		 /*******************************
		 *	       FLAGS		*
		 *******************************/

%%	prolog_flag(+Flag, -Old, +New) is semidet.
%
%	Query and set a Prolog flag. Use the debug/1 topic =prolog_flag=
%	to find the flags accessed using this predicate.

prolog_flag(Flag, Old, New) :-
	debug(prolog_flag, 'prolog_flag(~q, ~q, ~q)', [Flag, Old, New]),
	current_prolog_flag(Flag, Old),
	set_prolog_flag(Flag, New).

%%	prolog_flag(+Flag, -Value) is semidet.
%
%	Query a Prolog flag, mapping SICSTus flags to SWI-Prolog flags

prolog_flag(Flag, Value) :-
	debug(prolog_flag, 'prolog_flag(~q, ~q)', [Flag, Value]),
	sicstus_flag(Flag, Value).

sicstus_flag(system_type, Type) :- !,
	(   current_prolog_flag(saved_program, true)
	->  Type = runtime
	;   Type = development
	).
sicstus_flag(Name, Value) :-
	current_prolog_flag(Name, Value).


		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

% Provide (#)/2 as arithmetic function.  Ideally, we should be able to
% bind multiple names to built-in functions.  This is rather slow.  We
% could also consider adding # internally, but not turning it into an
% operator.

:- op(500, yfx, #).

:- arithmetic_function(user:(#)/2).
:- arithmetic_function(user:(\)/2).

user:(#(X,Y,R)) :-				% SICStus 3
	R is xor(X,Y).
user:(\(X,Y,R)) :-				% SICStus 4
	R is xor(X,Y).


		 /*******************************
		 *	       HACKS		*
		 *******************************/

%%	prolog:'$breaklevel'(-BreakLevel, Unknown)
%
%	Query the current break-level

prolog:'$breaklevel'(BreakLevel, _) :-
	current_prolog_flag(break_level, BreakLevel), !.
prolog:'$breaklevel'(0, _).
