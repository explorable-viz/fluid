import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def parse(test_names, column_order):
  # Read benchmark csv
  benchmarks = pd.read_csv('Benchmarks/benchmarks.csv', skipinitialspace=True, delimiter=',', index_col='Test-Name')

  # Extract test names of interest
  df = pd.DataFrame(benchmarks.loc[test_names])
  print(df)

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


def plot_benches(suite):
  test_cases = []
  bench_names = []
  
  if suite == "all":
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names = ['Trace-Eval', 'Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-BwdDual', 'Graph-BwdAll', 'Graph-Fwd', 'Graph-FwdDual', 'Graph-FwdAsDeMorgan']
  elif suite == "bwd":
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names = ['Trace-Eval','Trace-Bwd', 'Graph-Eval', 'Graph-Bwd']
  elif suite == "fwd":
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names = ['Trace-Eval', 'Trace-Fwd', 'Graph-Eval', 'Graph-Fwd', 'Graph-FwdAsDeMorgan']
  else:
    test_cases = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
    bench_names =  ['Trace-Eval','Trace-Bwd', 'Trace-Fwd', 'Graph-Eval', 'Graph-Bwd', 'Graph-Fwd']
  
  parse(test_cases, bench_names)  

plot_benches("all")