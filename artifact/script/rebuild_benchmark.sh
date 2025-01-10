#!/usr/env/bin bash
set -xe

sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-one
sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-two
sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-two
pushd benchmark
pushd tex
sudo pdflatex benchmarks.tex
