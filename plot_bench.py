import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import argparse


test_sets = {
  'expensive': ['slicing/convolution/edgeDetect', 'slicing/convolution/emboss', 'slicing/convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart', 'slicing/dtw/compute-dtw'],
  'graphics': ['graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart'],
  'convolution': ['slicing/convolution/edgeDetect', 'slicing/convolution/emboss', 'slicing/convolution/gaussian'],
}

bench_sets = {
  'all': ['Trace-Eval', 'Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-BwdDual', 'Graph-BwdAll', 'Graph-Fwd', 'Graph-FwdDual', 'Graph-FwdAsDeMorgan'],
  'bwd': ['Trace-Eval','Trace-Bwd','Graph-Eval', 'Graph-Bwd'],
  'fwd': ['Trace-Eval', 'Trace-Fwd', 'Graph-Eval', 'Graph-Fwd', 'Graph-FwdAsDeMorgan'],
  'standard': ['Trace-Eval','Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-Fwd'],
}

def parse(test_names, column_order, cap, lab, dest='recent.png'):
  # Read benchmark csv
  benchmarks = pd.read_csv('Benchmarks/benchmarks.csv', skipinitialspace=True, delimiter=',', index_col='Test-Name')

  # Extract test names of interest
  df = pd.DataFrame(benchmarks.loc[test_names]).round(1)
  tex = df[column_order].to_latex(float_format="%.2f", caption = cap, label = lab)
  print(df[column_order])
  print(tex)
  # Reorder benchmark columns
  # column_colors = ['#0d6b12', '#93dbb5', '#3e3875', '#b8bef5', '#910303', '#e84d4d', '#e8bceb', '#5073a1']
  # Plot as bar chart
  df[column_order].plot(  kind="bar"
                        # , color=column_colors
                        , ylabel="Milliseconds", rot=0
                        , figsize=(16,6))
  
  # Inserting a coloured horizontal line just to make clearer which columns have zero values
  plt.ylim(bottom=-10)
  plt.gca().axhline(0, lw=0.3, color='blue', label="Zero accuracy")
  dest_png = dest + '.png'
  dest_tex = dest + '.tex'
  tex_f = open(dest_tex, "w")
  tex_f.write(tex)
  tex_f.close()
  plt.savefig(dest_png)
  # plt.show()

def decompose_list(input_str):
  inner = input_str.split(", ")
  return inner

def test_names(test_str):
  if test_str in test_sets:
    return test_sets[test_str]
  else:
    return decompose_list(test_str)

def bench_names(bench_str):
  if bench_str in bench_sets:
    return bench_sets[bench_str]
  else:
    return decompose_list(bench_str)

parser = argparse.ArgumentParser()
parser.add_argument("-t", "--Tests", help = "Specify list of tests")
parser.add_argument("-b", "--Benches", help = "Specify list of benchmarks to show")
parser.add_argument("-d", "--Dest", help = "Specify where to save plot")
args = parser.parse_args()

if args.Tests and args.Benches:
  tests = test_names(args.Tests)
  benches = bench_names(args.Benches)
  capt = "Tests: " + args.Tests + ", Benches: " + args.Benches
  lab = args.Tests + '-' + args.Benches
  if args.Dest:    
    parse(tests, benches, capt, lab, dest=args.Dest)
  else:
    parse(tests, benches, capt, lab)