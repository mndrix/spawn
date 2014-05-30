# Synopsis

    :- use_module(library(spawn)).

    retirement(EmployeeId,Name,HireDate) :-
        % start long-running computations in the background
        spawn(query_company_ldap(EmployeeId,Employee)),
        spawn(query_hr_database(EmployeeId,Job)),

        do_something_productive_while_waiting,

        % extract desired details from Employee and Job
        % (blocks until background computations are done)
        Employee = employee(Name,_,_),
        Job = job(_,HireDate,_).

    some_numbers(PiN,Rand) :-
        % start long-running computations in the background
        async(calculate_nth_digit_of_pi(21_734,PiN),PiToken),
        async(ask_randomorg_for_a_number(Rand), RandToken),

        % explicitly block until they're done
        await(PiToken),
        writeln(PiN),   % now it has a value
        await(RandToken),
        writeln(Rand).  % now it has a value too

# Description

SWI Prolog has great concurrency primitives. For some common
operations, those primitives are too low level. You end up reading
through a lot of message queue and threading boilerplate code. This
library provides high level concurrency predicates which handle all the
messy details.

Our aim is to make this library as easy to use as call/1.

# Changes in this Version

  * First public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(spawn).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/spawn

@author Michael Hendricks <michael@ndrix.org>
@license unlicense
