import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import argparse


def parse(test_names, column_order):
  # Read benchmark csv
  benchmarks = pd.read_csv('Benchmarks/benchmarks.csv', skipinitialspace=True, delimiter=',', index_col='Test-Name')

  # Extract test names of interest
  df = pd.DataFrame(benchmarks.loc[test_names])
  print(df[column_order])

  # Reorder benchmark columns
  # column_colors = ['#0d6b12', '#93dbb5', '#3e3875', '#b8bef5', '#910303', '#e84d4d', '#e8bceb', '#5073a1']

  # Plot as bar chart
  df[column_order].plot(  kind="bar"
                        # , color=column_colors
                        , ylabel="Milliseconds", rot=0)

  # Inserting a coloured horizontal line just to make clearer which columns have zero values
  plt.ylim(bottom=-10)
  plt.gca().axhline(0, lw=0.3, color='blue', label="Zero accuracy")

  plt.show()

def decompose_list(input_str):
  inner = input_str.split(", ")
  return inner

def test_names(test_str):
  if test_str == "expensive":
    return ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart', 'slicing/dtw/compute-dtw']
  elif test_str == "graphics":
    return ['graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
  elif test_str == "convolution":
    return ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian']
  else:
    return decompose_list(test_str)

def bench_names(bench_str):
  if bench_str == "all":
    return ['Trace-Eval', 'Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-BwdDual', 'Graph-BwdAll', 'Graph-Fwd', 'Graph-FwdDual', 'Graph-FwdAsDeMorgan']
  elif bench_str == "bwd":
    return ['Trace-Eval','Trace-Bwd', 'Graph-Eval', 'Graph-Bwd']
  elif bench_str == "fwd":
    return ['Trace-Eval', 'Trace-Fwd', 'Graph-Eval', 'Graph-Fwd', 'Graph-FwdAsDeMorgan']
  elif bench_str == "standard":
    return ['Trace-Eval','Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-Fwd']
  else:
    return decompose_list(bench_str)

parser = argparse.ArgumentParser()  
parser.add_argument("-s", "--Suite", help = "Premade testsuite")
parser.add_argument("-t", "--Tests", help = "Specify list of tests")
parser.add_argument("-b", "--Benches", help = "Specify list of benchmarks to show")

args = parser.parse_args()

if args.Suite:
  test_cases = []
  bench_names = []
  
  if args.Suite == "all":
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names = ['Trace-Eval', 'Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-BwdDual', 'Graph-BwdAll', 'Graph-Fwd', 'Graph-FwdDual', 'Graph-FwdAsDeMorgan']
  elif args.Suite == "bwd":
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names = ['Trace-Eval','Trace-Bwd', 'Graph-Eval', 'Graph-Bwd']
  elif args.Suite == "fwd":
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names = ['Trace-Eval', 'Trace-Fwd', 'Graph-Eval', 'Graph-Fwd', 'Graph-FwdAsDeMorgan']
  else:
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names =  ['Trace-Eval','Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-Fwd']
  
  parse(test_cases, bench_names) 
elif args.Tests and args.Benches:
  tests = test_names(args.Tests)
  benches = bench_names(args.Benches)
    
  parse(tests, benches)