#!/bin/bash  
SUITES=`ls ebin`
SUITES=${SUITES//.beam/}
SUITES="${SUITES[*]}"
SUITES_STR=""
for SUITE in $SUITES; do SUITES_STR="$SUITES_STR, $SUITE"; done
erl -noshell -pa ebin -eval "case eunit:test([${SUITES_STR#, }], [verbose]) of ok -> init:stop(0); {error,_} -> init:stop(1) end."
# erl -noshell -pa ebin -eval "eunit:test(pmap_SUITE, [verbose])" -s init stop
