@echo off
cd ebin
erl -noshell -eval "case eunit:test({dir, "".""}, [verbose]) of ok -> init:stop(0); {error,_} -> init:stop(1) end."
cd ..