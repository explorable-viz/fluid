import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def parse():
  benchmarks = pd.read_csv('Benchmarks/benchmarks.csv', skipinitialspace=True, delimiter=',', index_col='Test-Name')
  test_names = ['convolution/edgeDetect', 'convolution/emboss']
  df = pd.DataFrame(benchmarks.loc[test_names])
  column_order = ['Trace-Eval', 'Graph-Eval', 'Trace-Bwd', 'Graph-Bwd', 'Trace-Fwd', 'Graph-Fwd', 'Graph-DeMorgan']
  column_colors = ['#c77e1a', '#e8c18b', '#248a5a', '#32f097', '#355a8f', '#7aabf0', '#e39adc']

  df[column_order].plot(kind="bar", color=column_colors, ylabel="Miliseconds", rot=0)
  # df = pd.DataFrame({
  #     'Name': ['John', 'Sammy', 'Joe'],
  #     'Age': [45, 38, 90],
  #     'Height(in cm)': [150, 180, 160]
  # })

  # # plotting graph
  # df.plot(x="Name", y=["Age", "Height(in cm)"], kind="bar")
  plt.show()
parse()