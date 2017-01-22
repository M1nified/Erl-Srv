@echo off
cd ebin
start erl -noshell -eval "terminal:init()."
cd ..