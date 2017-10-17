#!/bin/sh

./elvis rock && ./rebar3 eunit && cd c_src && make memory-test
