#!/bin/bash
set -xe

sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-one -s $1
sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-two -s $1
sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-two -s $1
pushd benchmark
pushd tex
sudo pdflatex benchmarks.tex
