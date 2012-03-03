## Requirements ##

- Recent install of [Erlang/OTP](http://www.erlang.org/)
- Recent version of [PropEr](http://proper.softlab.ntua.gr/) for the list delete example
- Recent version of [QuickCheck](http://quviq.com/) (Full QuickCheck trial version) for the C queue example

## How to run the list delete example ##

First, start Erlang in the PropEr directory:

    $ cd PropEr
    $ erl

Compile and load the listdel module:

    c(listdel).

Run unit tests:

    listdel:test().

Test each properties (100 tests by default):

    proper:quickcheck(listdel:prop_delete()). 
    proper:quickcheck(listdel:prop_delete2()). 
    proper:quickcheck(listdel:prop_delete3()). 
    proper:quickcheck(listdel:prop_delete4()). 

To run 1000 tests:

    proper:quickcheck(listdel:prop_delete3(), 1000).

Quit Erlang:

    q().


## How to run the C queue example ##

First, start Erlang as root in the QuickCheck directory:

    $ cd QuickCheck
    $ sudo erl

Start QuickCheck:

    eqc:start().

Compile and load the queue module:

    eqc_c:start(q).

Compile and load the QuickCheck FSM module:

    c(q_eqc).

Check the queue (100 times by default):

    eqc:quickcheck(q_eqc:prop_q()).

Run more tests:

    eqc:quickcheck(eqc:numtests(1000, q_eqc:prop_q())).

Quit Erlang:

    q().