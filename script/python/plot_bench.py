import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import argparse
import re

test_sets = {
  'expensive': ['slicing/convolution/edgeDetect', 'slicing/convolution/emboss', 'slicing/convolution/gaussian', 'graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart', 'slicing/linked-outputs/bar-chart-line-chart', 'slicing/linked-outputs/stacked-bar-scatter-plot'],
  'graphics': ['graphics/grouped-bar-chart', 'graphics/line-chart', 'graphics/stacked-bar-chart'],
  'convolution': ['slicing/convolution/edgeDetect', 'slicing/convolution/emboss', 'slicing/convolution/gaussian'],
}

bench_sets = {
  'table-one': ['T-Eval','G-Eval' ],
  'table-two': ['T-Demands', 'G-Demands'],
  'table-three': ['T-DemBy', 'G-DemBy-Dir', 'G-DemBy-Suff'],
}

def splitListEntry(entry):
  match = re.match(r"\(([\d.eE+-]+)\s*:\s*([\d.eE+-]+)\s*:\s*Nil\)", entry.strip())
  if match: 
    mean = float(match.group(1))
    std_dev = float(match.group(2))
    return f"{mean:.1f} (±{std_dev:.1f})"
  return entry

def splitFormattedEntry(entry):
  match = re.match(r"([\d.eE+-]+)\s*\(±[\d.eE+-]+\)", entry.strip())
  return float(match.group(1))

def parse(test_names, column_order, cap, lab):
  benchmarks = pd.read_csv('benchmarksOut.csv', skipinitialspace=True, delimiter=',', index_col='Test-Name')
  df = pd.DataFrame(benchmarks.loc[test_names, bench_sets[column_order]]).round(1).map(splitListEntry)

  if column_order == 'table-one':
    t_eval = df['T-Eval'].apply(splitFormattedEntry)
    g_eval = df['G-Eval'].apply(splitFormattedEntry)
    df['EvalSlowdown'] = g_eval / t_eval
    df['EvalSlowdown'] = df['EvalSlowdown'].round(2)
    print(df)
  elif column_order == 'table-two':
    t_demands = df['T-Demands'].apply(splitFormattedEntry)
    g_demands = df['G-Demands'].apply(splitFormattedEntry)
    df['Bwd-Speedup'] = t_demands / g_demands
    df['Bwd-Speedup'] = df['Bwd-Speedup'].round(2)
    print(df)
  elif column_order == 'table-three':
    t_demby = df['T-DemBy'].apply(splitFormattedEntry)
    g_demby = df['G-DemBy-Dir'].apply(splitFormattedEntry)
    g_demby_suff = df['G-DemBy-Suff'].apply(splitFormattedEntry)
    df['S'] = t_demby / g_demby
    df['S'] = df['S'].round(2)
    df['S\''] = t_demby / g_demby_suff
    df['S\''] = df['S\''].round(2)
    print(df)
  with open('benchmark/tex/' + column_order + '.tex', 'w') as tex_file:
    tex = df.to_latex(float_format ="%.2f", caption = "cap", label=lab)
    tex_file.write(tex)
    tex_file.close()


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
args = parser.parse_args()

if args.Tests and args.Benches:
  tests = test_names(args.Tests)
  capt = "Tests: " + args.Tests + ", Benches: " + args.Benches
  lab = args.Tests + '-' + args.Benches
  parse(tests, args.Benches, capt, lab)