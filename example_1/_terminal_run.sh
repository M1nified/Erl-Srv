#!/bin/bash
cd ebin
erl -noshell -eval "main_terminal:main(make_ref())."
cd ..