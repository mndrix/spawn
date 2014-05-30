:- use_module(library(spawn)).

delayed_unify(X,Y) :-
    sleep(1),
    X = Y.

one_solution(foo(bar)).

two_solutions(thing(one)).
two_solutions(thing(two)).

exceptional(_) :-
    throw(oh_no).

fails(_) :-
    fail.

thread_count(N) :-
    bagof(Id,S^thread_property(Id,status(S)),Ids),
    length(Ids,N).

:- use_module(library(tap)).

% sleep only happens when necessary
timing :-
    get_time(Start),
    lazy(delayed_unify(A,a)),
    get_time(Mid),
    Mid - Start < 0.1, % lazy/1 returns immediately

    % lazy computation hasn't run yet
    var(A),

    % request the computation
    A = a,
    get_time(End),

    % make sure the timings are reasonable
    End - Start < 1.1,
    End - Start > 0.9.


% unification with a single solution leaves no choicepoints
single :-
    lazy(one_solution(X)),
    X = foo(Y),
    Y == bar.


% unification with first or second solution is OK
double_first :-
    lazy(two_solutions(X)),
    X = thing(N),
    N == one,
    !.  % trim choicepoint representing thing(two)

double_second :-
    lazy(two_solutions(X)),
    X = thing(N),
    N == two.


% unification fails when there are no solutions
zero(fail) :-
    lazy(fails(X)),
    X = never_will_suceed.


% unification rethrows its goal's exception
an_exception(throws(oh_no)) :-
    lazy(exceptional(X)),
    X = never_will_suceed.


appending :-
    lazy(atom_codes(hello,Hello)),
    append(Hello,` world`, Message),
    Message = `hello world`.
