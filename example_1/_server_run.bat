@echo off
cd ebin
start erl -noshell -eval "main_server:main(tcp_server)."
cd ..