import pandas as pd
import matplotlib.pyplot as plt
import argparse
import sys
import scienceplots

def print_error(msg):
  print(f"\033[91m{msg}\033[0m")

def print_warning(msg):
  print(f"\033[93m{msg}\033[0m")

def print_success(msg):
  print(f"\033[92m{msg}\033[0m")

def print_info(msg):
  print(f"\033[94m{msg}\033[0m")

def concat_dfs(files, output_file):
  dfs = []
  for file in files:
    df = pd.read_csv(file)
    dfs.append(df)
  result = pd.concat(dfs)
  result.to_csv(output_file, index=False)

############################################################################################################
#                                           Single-query                                                   #
############################################################################################################

def plot_QF_FP_single(file, output_file):
  data = pd.read_csv(file)

  z3_solver = data[data['prover'] == 'z3']
  z3_smtml = data[data['prover'] == 'smtml-z3']
  cvc5_smtml = data[data['prover'] == 'smtml-cvc5']
  cvc5_solver = data[data['prover'] == 'cvc5']
  colibri2_smtml = data[data['prover'] == 'smtml-colibri2']
  colibri2 = data[data['prover'] == 'colibri2']
  bitwuzla_smtml = data[data['prover'] == 'smtml-bitwuzla']
  bitwuzla = data[data['prover'] == 'bitwuzla']

  z3_solver_sorted = z3_solver.sort_values(by='rtime')
  z3_smtml_sorted = z3_smtml.sort_values(by='rtime')
  cvc5_smtml_sorted = cvc5_smtml.sort_values(by='rtime')
  cvc5_solver_sorted = cvc5_solver.sort_values(by='rtime')
  colibri2_sorted = colibri2_smtml.sort_values(by='rtime')
  colibri2_solver_sorted = colibri2.sort_values(by='rtime')
  bitwuzla_smtml_sorted = bitwuzla_smtml.sort_values(by='rtime')
  bitwuzla_sorted = bitwuzla.sort_values(by='rtime')

  z3_solver_sorted['cumulative_runtime'] = z3_solver_sorted['rtime'].cumsum()
  z3_smtml_sorted['cumulative_runtime'] = z3_smtml_sorted['rtime'].cumsum()
  cvc5_smtml_sorted['cumulative_runtime'] = cvc5_smtml_sorted['rtime'].cumsum()
  cvc5_solver_sorted['cumulative_runtime'] = cvc5_solver_sorted['rtime'].cumsum()
  colibri2_sorted['cumulative_runtime'] = colibri2_sorted['rtime'].cumsum()
  colibri2_solver_sorted['cumulative_runtime'] = colibri2_solver_sorted['rtime'].cumsum()
  bitwuzla_smtml_sorted['cumulative_runtime'] = bitwuzla_smtml_sorted['rtime'].cumsum()
  bitwuzla_sorted['cumulative_runtime'] = bitwuzla_sorted['rtime'].cumsum()

  z3_solver_sorted['problems_solved'] = range(1, len(z3_solver_sorted) + 1)
  z3_smtml_sorted['problems_solved'] = range(1, len(z3_smtml_sorted) + 1)
  cvc5_smtml_sorted['problems_solved'] = range(1, len(cvc5_smtml_sorted) + 1)
  cvc5_solver_sorted['problems_solved'] = range(1, len(cvc5_solver_sorted) + 1)
  colibri2_sorted['problems_solved'] = range(1, len(colibri2_sorted) + 1)
  colibri2_solver_sorted['problems_solved'] = range(1, len(colibri2_solver_sorted) + 1)
  bitwuzla_smtml_sorted['problems_solved'] = range(1, len(bitwuzla_smtml_sorted) + 1)
  bitwuzla_sorted['problems_solved'] = range(1, len(bitwuzla_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_solver_sorted['cumulative_runtime'], z3_solver_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_smtml_sorted['cumulative_runtime'], z3_smtml_sorted['problems_solved'], label='Smt.ml - Z3', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(cvc5_solver_sorted['cumulative_runtime'], cvc5_solver_sorted['problems_solved'], label='cvc5', color='#BC4B51', linewidth=2, linestyle=':')
  plt.plot(cvc5_smtml_sorted['cumulative_runtime'], cvc5_smtml_sorted['problems_solved'], label='Smt.ml - cvc5', color='#BC4B51', linewidth=2, linestyle='-')
  plt.plot(bitwuzla_sorted['cumulative_runtime'], bitwuzla_sorted['problems_solved'], label='Bitwuzla', color='#168aad', linewidth=2, linestyle=':')
  plt.plot(bitwuzla_smtml_sorted['cumulative_runtime'], bitwuzla_smtml_sorted['problems_solved'], label='Smt.ml - Bitwuzla', color='#168aad', linewidth=2, linestyle='-')
  plt.plot(colibri2_solver_sorted['cumulative_runtime'], colibri2_solver_sorted['problems_solved'], label='Colibri2', color='#F4A259', linewidth=2, linestyle=':')
  plt.plot(colibri2_sorted['cumulative_runtime'], colibri2_sorted['problems_solved'], label='Smt.ml - Colibri2', color='#F4A259', linewidth=2, linestyle='-')
  plt.title('QF_FP', fontsize=14)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_FP plot generated successfully and saved to {output_file}.pdf")  

def plot_QF_S_single(file, output_file):
  data = pd.read_csv(file)

  z3 = data[data['prover'] == 'z3']
  smtml_z3 = data[data['prover'] == 'smtml-z3']
  cvc5 = data[data['prover'] == 'cvc5']
  smtml_cvc5 = data[data['prover'] == 'smtml-cvc5']

  z3_sorted = z3.sort_values(by='rtime')
  smtml_z3_sorted = smtml_z3.sort_values(by='rtime')
  cvc5_sorted = cvc5.sort_values(by='rtime')
  smtml_cvc5_sorted = smtml_cvc5.sort_values(by='rtime')

  z3_sorted['cumulative_runtime'] = z3_sorted['rtime'].cumsum()
  smtml_z3_sorted['cumulative_runtime'] = smtml_z3_sorted['rtime'].cumsum()
  cvc5_sorted['cumulative_runtime'] = cvc5_sorted['rtime'].cumsum()
  smtml_cvc5_sorted['cumulative_runtime'] = smtml_cvc5_sorted['rtime'].cumsum()

  z3_sorted['problems_solved'] = range(1, len(z3_sorted) + 1)
  smtml_z3_sorted['problems_solved'] = range(1, len(smtml_z3_sorted) + 1)
  cvc5_sorted['problems_solved'] = range(1, len(cvc5_sorted) + 1)
  smtml_cvc5_sorted['problems_solved'] = range(1, len(smtml_cvc5_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_sorted['cumulative_runtime'], z3_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(smtml_z3_sorted['cumulative_runtime'], smtml_z3_sorted['problems_solved'], label='Smt.ml - Z3', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(cvc5_sorted['cumulative_runtime'], cvc5_sorted['problems_solved'], label='cvc5', color='#BC4B51', linewidth=2, linestyle=':')
  plt.plot(smtml_cvc5_sorted['cumulative_runtime'], smtml_cvc5_sorted['problems_solved'], label='Smt.ml - cvc5', color='#BC4B51', linewidth=2, linestyle='-')
  plt.title('QF_S', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=16, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_S plot generated successfully and saved to {output_file}.pdf")

def plot_QF_SLIA_single(file, output_file):
  data = pd.read_csv(file)

  z3 = data[data['prover'] == 'z3']
  smtml_z3 = data[data['prover'] == 'smtml-z3']
  cvc5 = data[data['prover'] == 'cvc5']
  smtml_cvc5 = data[data['prover'] == 'smtml-cvc5']

  z3_sorted = z3.sort_values(by='rtime')
  smtml_z3_sorted = smtml_z3.sort_values(by='rtime')
  cvc5_sorted = cvc5.sort_values(by='rtime')
  smtml_cvc5_sorted = smtml_cvc5.sort_values(by='rtime')

  z3_sorted['cumulative_runtime'] = z3_sorted['rtime'].cumsum()
  smtml_z3_sorted['cumulative_runtime'] = smtml_z3_sorted['rtime'].cumsum()
  cvc5_sorted['cumulative_runtime'] = cvc5_sorted['rtime'].cumsum()
  smtml_cvc5_sorted['cumulative_runtime'] = smtml_cvc5_sorted['rtime'].cumsum()

  z3_sorted['problems_solved'] = range(1, len(z3_sorted) + 1)
  smtml_z3_sorted['problems_solved'] = range(1, len(smtml_z3_sorted) + 1)
  cvc5_sorted['problems_solved'] = range(1, len(cvc5_sorted) + 1)
  smtml_cvc5_sorted['problems_solved'] = range(1, len(smtml_cvc5_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_sorted['cumulative_runtime'], z3_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(smtml_z3_sorted['cumulative_runtime'], smtml_z3_sorted['problems_solved'], label='Smt.ml - Z3', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(cvc5_sorted['cumulative_runtime'], cvc5_sorted['problems_solved'], label='cvc5', color='#BC4B51', linewidth=2, linestyle=':')
  plt.plot(smtml_cvc5_sorted['cumulative_runtime'], smtml_cvc5_sorted['problems_solved'], label='Smt.ml - cvc5', color='#BC4B51', linewidth=2, linestyle='-')
  plt.title('QF_SLIA', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=16, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_SLIA plot generated successfully and saved to {output_file}.pdf")

def plot_QF_BV_single(file, output_file):
  data = pd.read_csv(file)
  z3 = data[data['prover'] == 'z3']
  smtml_z3 = data[data['prover'] == 'smtml-z3']
  bitwuzla = data[data['prover'] == 'bitwuzla']
  smtml_bitwuzla = data[data['prover'] == 'smtml-bitwuzla']
  cvc5 = data[data['prover'] == 'cvc5']
  smtml_cvc5 = data[data['prover'] == 'smtml-cvc5']
  colibri2 = data[data['prover'] == 'colibri2']
  smtml_colibri2 = data[data['prover'] == 'smtml-colibri2']

  smtml_z3 = smtml_z3[smtml_z3['res'] != 'error']
  smtml_bitwuzla = smtml_bitwuzla[smtml_bitwuzla['res'] != 'error']
  smtml_cvc5 = smtml_cvc5[smtml_cvc5['res'] != 'error']
  smtml_colibri2 = smtml_colibri2[smtml_colibri2['res'] != 'error']
  colibri2 = colibri2[colibri2['res'] != 'timeout']
  z3 = z3[z3['file'].isin(smtml_z3['file'])]
  bitwuzla = bitwuzla[bitwuzla['file'].isin(smtml_bitwuzla['file'])]

  z3_sorted = z3.sort_values(by='rtime')
  smtml_z3_sorted = smtml_z3.sort_values(by='rtime')
  bitwuzla_sorted = bitwuzla.sort_values(by='rtime')
  smtml_bitwuzla_sorted = smtml_bitwuzla.sort_values(by='rtime')
  cvc5_sorted = cvc5.sort_values(by='rtime')
  smtml_cvc5_sorted = smtml_cvc5.sort_values(by='rtime')
  colibri2_sorted = colibri2.sort_values(by='rtime')
  smtml_colibri2_sorted = smtml_colibri2.sort_values(by='rtime')

  z3_sorted['cumulative_runtime'] = z3_sorted['rtime'].cumsum()
  smtml_z3_sorted['cumulative_runtime'] = smtml_z3_sorted['rtime'].cumsum()
  bitwuzla_sorted['cumulative_runtime'] = bitwuzla_sorted['rtime'].cumsum()
  smtml_bitwuzla_sorted['cumulative_runtime'] = smtml_bitwuzla_sorted['rtime'].cumsum()
  cvc5_sorted['cumulative_runtime'] = cvc5_sorted['rtime'].cumsum()
  smtml_cvc5_sorted['cumulative_runtime'] = smtml_cvc5_sorted['rtime'].cumsum()
  colibri2_sorted['cumulative_runtime'] = colibri2_sorted['rtime'].cumsum()
  smtml_colibri2_sorted['cumulative_runtime'] = smtml_colibri2_sorted['rtime'].cumsum()

  z3_sorted['problems_solved'] = range(1, len(z3_sorted) + 1)
  smtml_z3_sorted['problems_solved'] = range(1, len(smtml_z3_sorted) + 1)
  bitwuzla_sorted['problems_solved'] = range(1, len(bitwuzla_sorted) + 1)
  smtml_bitwuzla_sorted['problems_solved'] = range(1, len(smtml_bitwuzla_sorted) + 1)
  cvc5_sorted['problems_solved'] = range(1, len(cvc5_sorted) + 1)
  smtml_cvc5_sorted['problems_solved'] = range(1, len(smtml_cvc5_sorted) + 1)
  colibri2_sorted['problems_solved'] = range(1, len(colibri2_sorted) + 1)
  smtml_colibri2_sorted['problems_solved'] = range(1, len(smtml_colibri2_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_sorted['cumulative_runtime'], z3_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(smtml_z3_sorted['cumulative_runtime'], smtml_z3_sorted['problems_solved'], label='Smt.ml - Z3', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(cvc5_sorted['cumulative_runtime'], cvc5_sorted['problems_solved'], label='cvc5', color='#BC4B51', linewidth=2, linestyle=':')
  plt.plot(smtml_cvc5_sorted['cumulative_runtime'], smtml_cvc5_sorted['problems_solved'], label='Smt.ml - cvc5', color='#BC4B51', linewidth=2, linestyle='-')
  plt.plot(bitwuzla_sorted['cumulative_runtime'], bitwuzla_sorted['problems_solved'], label='Bitwuzla', color='#168aad', linewidth=2, linestyle=':')
  plt.plot(smtml_bitwuzla_sorted['cumulative_runtime'], smtml_bitwuzla_sorted['problems_solved'], label='Smt.ml - Bitwuzla', color='#168aad', linewidth=2, linestyle='-')
  plt.plot(colibri2_sorted['cumulative_runtime'], colibri2_sorted['problems_solved'], label='Colibri2', color='#F4A259', linewidth=2, linestyle=':')
  plt.plot(smtml_colibri2_sorted['cumulative_runtime'], smtml_colibri2_sorted['problems_solved'], label='Smt.ml - Colibri2', color='#F4A259', linewidth=2, linestyle='-')
  plt.title('QF_BV', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=16, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_BV plot generated successfully and saved to {output_file}.pdf")

def plot_QF_LIA_single(file, output_file):
  data = pd.read_csv(file)

  z3 = data[data['prover'] == 'z3']
  smtml_z3 = data[data['prover'] == 'smtml-z3']
  cvc5 = data[data['prover'] == 'cvc5']
  smtml_cvc5 = data[data['prover'] == 'smtml-cvc5']
  colibri2 = data[data['prover'] == 'colibri2']
  smtml_colibri2 = data[data['prover'] == 'smtml-colibri2']

  z3_sorted = z3.sort_values(by='rtime')
  smtml_z3_sorted = smtml_z3.sort_values(by='rtime')
  cvc5_sorted = cvc5.sort_values(by='rtime')
  smtml_cvc5_sorted = smtml_cvc5.sort_values(by='rtime')
  colibri2_sorted = colibri2.sort_values(by='rtime')
  smtml_colibri2_sorted = smtml_colibri2.sort_values(by='rtime')

  z3_sorted['cumulative_runtime'] = z3_sorted['rtime'].cumsum()
  smtml_z3_sorted['cumulative_runtime'] = smtml_z3_sorted['rtime'].cumsum()
  cvc5_sorted['cumulative_runtime'] = cvc5_sorted['rtime'].cumsum()
  smtml_cvc5_sorted['cumulative_runtime'] = smtml_cvc5_sorted['rtime'].cumsum()
  colibri2_sorted['cumulative_runtime'] = colibri2_sorted['rtime'].cumsum()
  smtml_colibri2_sorted['cumulative_runtime'] = smtml_colibri2_sorted['rtime'].cumsum()

  z3_sorted['problems_solved'] = range(1, len(z3_sorted) + 1)
  smtml_z3_sorted['problems_solved'] = range(1, len(smtml_z3_sorted) + 1)
  cvc5_sorted['problems_solved'] = range(1, len(cvc5_sorted) + 1)
  smtml_cvc5_sorted['problems_solved'] = range(1, len(smtml_cvc5_sorted) + 1)
  colibri2_sorted['problems_solved'] = range(1, len(colibri2_sorted) + 1)
  smtml_colibri2_sorted['problems_solved'] = range(1, len(smtml_colibri2_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_sorted['cumulative_runtime'], z3_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(smtml_z3_sorted['cumulative_runtime'], smtml_z3_sorted['problems_solved'], label='Smt.ml - Z3', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(cvc5_sorted['cumulative_runtime'], cvc5_sorted['problems_solved'], label='cvc5', color='#BC4B51', linewidth=2, linestyle=':')
  plt.plot(smtml_cvc5_sorted['cumulative_runtime'], smtml_cvc5_sorted['problems_solved'], label='Smt.ml - cvc5', color='#BC4B51', linewidth=2, linestyle='-')
  plt.plot(colibri2_sorted['cumulative_runtime'], colibri2_sorted['problems_solved'], label='Colibri2', color='#F4A259', linewidth=2, linestyle=':')
  plt.plot(smtml_colibri2_sorted['cumulative_runtime'], smtml_colibri2_sorted['problems_solved'], label='Smt.ml - Colibri2', color='#F4A259', linewidth=2, linestyle='-')
  plt.title('QF_LIA', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_LIA plot generated successfully and saved to {output_file}.pdf")

############################################################################################################
#                                        Multi-query vs Single-query                                       #
############################################################################################################

def plot_QF_FP_multi(file_multi_z3, file_single_z3, file_multi_bt, file_single_bt, output_file):
  data_multi_z3 = pd.read_csv(file_multi_z3)
  data_single_z3 = pd.read_csv(file_single_z3)
  data_multi_bt = pd.read_csv(file_multi_bt)
  data_single_bt = pd.read_csv(file_single_bt)

  bt_multi = data_multi_bt[data_multi_bt['prover'] == 'smtml-bitwuzla']
  bt_single = data_single_bt[
    (data_single_bt['file'].isin(data_multi_bt['benchmark'])) & 
    (data_single_bt['prover'] == 'bitwuzla')
  ]
  z3_multi = data_multi_z3[data_multi_z3['prover'] == 'smtml-z3']
  z3_single = data_single_z3[
    (data_single_z3['file'].isin(z3_multi['benchmark'])) & 
    (data_single_z3['prover'] == 'z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')
  bt_multi_sorted = bt_multi.sort_values(by='rtime')
  bt_single_sorted = bt_single.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()
  bt_multi_sorted['cumulative_runtime'] = bt_multi_sorted['rtime'].cumsum()
  bt_single_sorted['cumulative_runtime'] = bt_single_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)
  bt_multi_sorted['problems_solved'] = range(1, len(bt_multi_sorted) + 1)
  bt_single_sorted['problems_solved'] = range(1, len(bt_single_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(bt_single_sorted['cumulative_runtime'], bt_single_sorted['problems_solved'], label='Bitwuzla', color='#1e6091', linewidth=2, linestyle=':')
  plt.plot(bt_multi_sorted['cumulative_runtime'], bt_multi_sorted['problems_solved'], label='Smt.ml - Bitwuzla (multi-query)', color='#1e6091', linewidth=2, linestyle='-')
  plt.title('QF_FP', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_FP multi-query plot generated successfully and saved to {output_file}.pdf")

def plot_QF_BV_multi(file_multi_z3, file_single_z3, file_multi_bt, file_single_bt, output_file):
  data_multi_z3 = pd.read_csv(file_multi_z3)
  data_single_z3 = pd.read_csv(file_single_z3)
  data_multi_bt = pd.read_csv(file_multi_bt)
  data_single_bt = pd.read_csv(file_single_bt)

  z3_multi = data_multi_z3[data_multi_z3['prover'] == 'smtml-z3']
  z3_single = data_single_z3[
    (data_single_z3['file'].isin(data_multi_z3['benchmark'])) & 
    (data_single_z3['prover'] == 'z3')
  ]
  bt_multi = data_multi_bt[data_multi_bt['prover'] == 'smtml-bitwuzla']
  bt_single = data_single_bt[
    (data_single_bt['file'].isin(data_multi_bt['benchmark'])) & 
    (data_single_bt['prover'] == 'bitwuzla')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')
  bt_multi_sorted = bt_multi.sort_values(by='rtime')
  bt_single_sorted = bt_single.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()
  bt_multi_sorted['cumulative_runtime'] = bt_multi_sorted['rtime'].cumsum()
  bt_single_sorted['cumulative_runtime'] = bt_single_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)
  bt_multi_sorted['problems_solved'] = range(1, len(bt_multi_sorted) + 1)
  bt_single_sorted['problems_solved'] = range(1, len(bt_single_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#8CB369', linewidth=2, linestyle='-')
  plt.plot(bt_single_sorted['cumulative_runtime'], bt_single_sorted['problems_solved'], label='Bitwuzla', color='#1e6091', linewidth=2, linestyle=':')
  plt.plot(bt_multi_sorted['cumulative_runtime'], bt_multi_sorted['problems_solved'], label='Smt.ml - Bitwuzla (multi-query)', color='#1e6091', linewidth=2, linestyle='-')
  plt.title('QF_BV', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_BV multi-query plot generated successfully and saved to {output_file}.pdf")

def plot_QF_LIA_multi(file_multi, file_single, output_file):
  data_multi = pd.read_csv(file_multi)
  data_single = pd.read_csv(file_single)

  z3_multi = data_multi[data_multi['prover'] == 'smtml-z3']
  z3_single = data_single[
    (data_single['file'].isin(data_multi['benchmark'])) &
    (data_single['prover'] == 'z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#8CB369', linewidth=2, linestyle='-')
  plt.title('QF_LIA', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_LIA multi-query plot generated successfully and saved to {output_file}.pdf")

def plot_QF_S_multi(file_multi, file_single, output_file):
  data_multi = pd.read_csv(file_multi)
  data_single = pd.read_csv(file_single)

  z3_multi = data_multi[data_multi['prover'] == 'smtml-z3']
  z3_single = data_single[
    (data_single['file'].isin(data_multi['benchmark'])) &
    (data_single['prover'] == 'z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#8CB369', linewidth=2, linestyle='-')
  plt.title('QF_S', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_S multi-query plot generated successfully and saved to {output_file}.pdf")

def plot_QF_SLIA_multi(file_multi, file_single, output_file):
  data_multi = pd.read_csv(file_multi)
  data_single = pd.read_csv(file_single)

  z3_multi = data_multi[data_multi['prover'] == 'smtml-z3']
  z3_single = data_single[
    (data_single['file'].isin(data_multi['benchmark'])) &
    (data_single['prover'] == 'z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#8CB369', linewidth=2, linestyle='-')
  plt.title('QF_SLIA', fontsize=12)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"QF_SLIA multi-query plot generated successfully and saved to {output_file}.pdf")

############################################################################################################
#                                          Test-Comp 2023 plots                                            #
############################################################################################################

def plot_multi_testcomp_array(file_multi, file_single_solver, file_single_smtml, output_file):
  data_multi = pd.read_csv(file_multi)
  data_single = pd.read_csv(file_single_solver)
  data_single_smtml = pd.read_csv(file_single_smtml)

  z3_multi = data_multi[data_multi['prover'] == 'smtml-z3']
  z3_single = data_single[
    (data_single['file'].isin(data_multi['benchmark'])) & 
    (data_single['prover'] == 'z3')
  ]
  z3_single_smtml = data_single_smtml[
    (data_single_smtml['file'].isin(data_multi['benchmark'])) & 
    (data_single_smtml['prover'] == 'smtml-z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')
  z3_single_smtml_sorted = z3_single_smtml.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()
  z3_single_smtml_sorted['cumulative_runtime'] = z3_single_smtml_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)
  z3_single_smtml_sorted['problems_solved'] = range(1, len(z3_single_smtml_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#3A7133', linewidth=2, linestyle='-')
  plt.plot(z3_single_smtml_sorted['cumulative_runtime'], z3_single_smtml_sorted['problems_solved'], label='Smt.ml - Z3 (single-query)', color='#3A7133', linewidth=2, linestyle='--')

  plt.title('array-examples', fontsize=16)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"Test-Comp 2023 [array-examples] plot generated successfully and saved to {output_file}.pdf")

def plot_multi_testcomp_array_industry(file_multi, file_single_solver, file_single_smtml, output_file):
  data_multi = pd.read_csv(file_multi)
  data_single = pd.read_csv(file_single_solver)
  data_single_smtml = pd.read_csv(file_single_smtml)

  z3_multi = data_multi[data_multi['prover'] == 'smtml-z3']
  z3_single = data_single[
    (data_single['file'].isin(data_multi['benchmark'])) & 
    (data_single['prover'] == 'z3')
  ]
  z3_single_smtml = data_single_smtml[
    (data_single_smtml['file'].isin(data_multi['benchmark'])) & 
    (data_single_smtml['prover'] == 'smtml-z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')
  z3_single_smtml_sorted = z3_single_smtml.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()
  z3_single_smtml_sorted['cumulative_runtime'] = z3_single_smtml_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)
  z3_single_smtml_sorted['problems_solved'] = range(1, len(z3_single_smtml_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#3A7133', linewidth=2, linestyle='-')
  plt.plot(z3_single_smtml_sorted['cumulative_runtime'], z3_single_smtml_sorted['problems_solved'], label='Smt.ml - Z3 (single-query)', color='#3A7133', linewidth=2, linestyle='--')
  plt.title('array-industry-pattern', fontsize=16)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"Test-Comp 2023 [array-industry-pattern] plot generated successfully and saved to {output_file}.pdf")

def plot_multi_testcomp_eca(file_multi, file_single_solver, file_single_smtml, output_file):
  data_multi = pd.read_csv(file_multi)
  data_single = pd.read_csv(file_single_solver)
  data_single_smtml = pd.read_csv(file_single_smtml)

  z3_multi = data_multi[data_multi['prover'] == 'smtml-z3']
  z3_single = data_single[
    (data_single['file'].isin(data_multi['benchmark'])) & 
    (data_single['prover'] == 'z3')
  ]
  z3_single_smtml = data_single_smtml[
    (data_single_smtml['file'].isin(data_multi['benchmark'])) & 
    (data_single_smtml['prover'] == 'smtml-z3')
  ]

  z3_multi_sorted = z3_multi.sort_values(by='rtime')
  z3_single_sorted = z3_single.sort_values(by='rtime')
  z3_single_smtml_sorted = z3_single_smtml.sort_values(by='rtime')

  z3_multi_sorted['cumulative_runtime'] = z3_multi_sorted['rtime'].cumsum()
  z3_single_sorted['cumulative_runtime'] = z3_single_sorted['rtime'].cumsum()
  z3_single_smtml_sorted['cumulative_runtime'] = z3_single_smtml_sorted['rtime'].cumsum()

  z3_multi_sorted['problems_solved'] = range(1, len(z3_multi_sorted) + 1)
  z3_single_sorted['problems_solved'] = range(1, len(z3_single_sorted) + 1)
  z3_single_smtml_sorted['problems_solved'] = range(1, len(z3_single_smtml_sorted) + 1)

  plt.style.use(['science','ieee'])
  plt.style.use(['no-latex'])
  plt.figure(figsize=(6, 6))
  plt.plot(z3_single_sorted['cumulative_runtime'], z3_single_sorted['problems_solved'], label='Z3', color='#8CB369', linewidth=2, linestyle=':')
  plt.plot(z3_multi_sorted['cumulative_runtime'], z3_multi_sorted['problems_solved'], label='Smt.ml - Z3 (multi-query)', color='#3A7133', linewidth=2, linestyle='-')
  plt.plot(z3_single_smtml_sorted['cumulative_runtime'], z3_single_smtml_sorted['problems_solved'], label='Smt.ml - Z3 (single-query)', color='#3A7133', linewidth=2, linestyle='--')
  plt.title('eca-rers20218', fontsize=16)
  plt.xlim(0)
  plt.ylim(0)
  plt.xlabel('Cumulative Time (s)', fontsize=16)
  plt.ylabel('Number of problems solved', fontsize=16)
  plt.yticks(fontsize=12)
  plt.xticks(fontsize=12)
  plt.legend(fontsize=15, loc='lower right')
  plt.grid(True, linestyle=':', linewidth=0.2)
  plt.minorticks_on()
  plt.savefig(f'{output_file}.pdf')
  print_success(f"Test-Comp 2023 [eca-rers20218] plot generated successfully and saved to {output_file}.pdf")

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Generate plots for Smt.ml benchmarks')
  parser.add_argument('--output', type=str, help='Output file name')
  parser.add_argument('--multi', action='store_true', help='Generate plots for single-query results')
  parser.add_argument('--single', action='store_true', help='Generate plots for single-query results')
  parser.add_argument('--testcomp', action='store_true', help='Generate plots for Test-Comp 2023 benchmarks')
  parser.add_argument('--QF-FP', action='store_true', help='Run QF_FP benchmarks')
  parser.add_argument('--QF-LIA', action='store_true', help='Run QF_LIA benchmarks')
  parser.add_argument('--QF-BV', action='store_true', help='Run QF_BV benchmarks')
  parser.add_argument('--QF-S', action='store_true', help='Run QF_S benchmarks')
  parser.add_argument('--QF-SLIA', action='store_true', help='Run QF_SLIA benchmarks')
  parser.add_argument('--array-examples', action='store_true', help='Run array-examples benchmarks')
  parser.add_argument('--array-industry-pattern', action='store_true', help='Run array-industry-pattern benchmarks')
  parser.add_argument('--eca-rers20218', action='store_true', help='Run eca-rers20218 benchmarks')
  parser.add_argument('--concat', action='store_true', help='Concatenate the results')
  parser.add_argument('--files', nargs='+', help='List of input files')
  args = parser.parse_args()

  if not args.output:
    print_error("Output file name not provided.")
    print("Use --output to specify the output file name.")
    sys.exit(1)

  if args.concat:
    print_info("Concatenating the results")
    concat_dfs(args.files, args.output)
    print_success("Results concatenated successfully.")
    sys.exit(0)

  if args.single:
    if args.QF_FP:
      print_info("Generating plots for QF_FP")
      plot_QF_FP_single(args.files[0], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_LIA:
      print_info("Generating plots for QF_LIA")
      plot_QF_LIA_single(args.files[0], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_BV:
      print_info("Generating plots for QF_BV")
      plot_QF_BV_single(args.files[0], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_S:
      print_info("Generating plots for QF_S")
      plot_QF_S_single(args.files[0], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_SLIA:
      print_info("Generating plots for QF_SLIA")
      plot_QF_SLIA_single(args.files[0], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    else:
      print_error("No benchmark selected.")
      sys.exit(1)
  elif args.multi:
    if args.QF_FP:
      print_info("Generating plots for QF_FP")
      plot_QF_FP_multi(args.files[0], args.files[1], args.files[2], args.files[3], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_LIA:
      print_info("Generating plots for QF_LIA")
      plot_QF_LIA_multi(args.files[0], args.files[1], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_BV:
      print_info("Generating plots for QF_BV")
      plot_QF_BV_multi(args.files[0], args.files[1], args.files[2], args.files[3], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_S:
      print_info("Generating plots for QF_S")
      plot_QF_S_multi(args.files[0], args.files[1], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.QF_SLIA:
      print_info("Generating plots for QF_SLIA")
      plot_QF_SLIA_multi(args.files[0], args.files[1], args.output)
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    else:
      print_error("No benchmark selected.")
      sys.exit(1)
  elif args.testcomp:
    if args.array_examples:
      print_info("Generating plots for array-examples category")
      plot_multi_testcomp_array('benchmarks/testcomp2023_array.csv', 'benchmarks/array-examples.csv', 'benchmarks/array-examples_smtml.csv', f'{args.output}_array')
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.array_industry_pattern:
      print_info("Generating plots for array-industry-pattern category")
      plot_multi_testcomp_array_industry('benchmarks/testcomp2023_array_industry.csv', 'benchmarks/array-industry-pattern.csv', 'benchmarks/array-industry-pattern_smtml.csv', f'{args.output}_array_industry')
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    elif args.eca_rers20218:
      print_info("Generating plots for eca-rers20218 category")
      plot_multi_testcomp_eca('benchmarks/testcomp2023_eca.csv', 'benchmarks/eca-rers20218.csv', 'benchmarks/eca-rers20218_smtml.csv', f'{args.output}_eca')
      print_success(f"Plot generated successfully. Saved to {args.output}.pdf")
    else:
      print_error("No category selected.")
      sys.exit(1)
  else:
    print_error("No option selected.")
    print("Use --single to generate plots for single-query results")
    print("Use --multi to generate plots for multi-query results")
    print("Use --testcomp to generate plots for Test-Comp 2023 benchmarks")
    sys.exit(1)
