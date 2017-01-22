@echo off
cd ebin
start erl -noshell -eval "main_terminal:main(make_ref())."
cd ..