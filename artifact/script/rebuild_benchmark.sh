#!/bin/bash
set -xe

sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-one -s benchmark/benchmarks_artifact.csv
sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-two -s benchmark/benchmarks_artifact.csv
sudo /usr/src/python/bin/python3 script/python/plot_bench.py -t paper -b table-two -s benchmark/benchmarks_artifact.csv
pushd benchmark
pushd tex
sudo pdflatex benchmarks.tex
