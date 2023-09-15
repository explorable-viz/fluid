import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def parse():
  # Read benchmark csv
  benchmarks = pd.read_csv('Benchmarks/benchmarks.csv', skipinitialspace=True, delimiter=',', index_col='Test-Name')

  # Extract test names of interest
  test_names = ['convolution/edgeDetect', 'convolution/emboss', 'convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart']
  df = pd.DataFrame(benchmarks.loc[test_names])
  print(df)

  # Reorder benchmark columns
  column_order = ['Trace-Eval', 'Graph-Eval', 'Trace-Bwd', 'Graph-Bwd', 'Trace-Fwd', 'Graph-Fwd', 'Graph-FwdDeMorgan']
  column_colors = ['#0d6b12', '#93dbb5', '#3e3875', '#b8bef5', '#910303', '#e84d4d', '#e8bceb']
  
  # Plot as bar chart
  df[column_order].plot(kind="bar", color=column_colors, ylabel="Milliseconds", rot=0)

  # Inserting a coloured horizontal line just to make clearer which columns have zero values
  plt.ylim(bottom=-10)
  plt.gca().axhline(0, lw=0.3, color='blue', label="Zero accuracy")

  plt.show()
parse()