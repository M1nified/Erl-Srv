#!/bin/bash
cd ebin
erl -noshell -eval "main_server:main(tcp_server)."
cd ..