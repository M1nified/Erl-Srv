@echo off
cd ebin
start erl -noshell -eval "server:run()."
cd ..