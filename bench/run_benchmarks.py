import os
import subprocess
import csv
import sqlite3
import argparse
import glob
import pandas as pd

def print_error(msg):
  print(f"\033[91m{msg}\033[0m")

def print_warning(msg):
  print(f"\033[93m{msg}\033[0m")

def print_success(msg):
  print(f"\033[92m{msg}\033[0m")

def print_info(msg):
  print(f"\033[94m{msg}\033[0m")

def run_benchpress(directory_to_benchmark, config_file, prover, threads=6, timeout=30):
  try:
    print_info(f"Running benchpress on directory: {directory_to_benchmark}")
    command = ['benchpress', 'run', '-c', config_file, 'j', str(threads), '-t', str(timeout), '-p', prover, directory_to_benchmark]
    subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    print_success("Benchpress completed successfully.")
    return True
  except subprocess.CalledProcessError as e:
    print_error(f"Error running benchpress: {e.stderr.decode()}")

def get_latest_sqlite_file(benchpress_data_dir):
  files = [f for f in os.listdir(benchpress_data_dir) if f.endswith('.sqlite')]
  if not files:
    raise FileNotFoundError("No .sqlite files found in benchpress data directory.")

  latest_file = max(files, key=lambda x: os.path.getmtime(os.path.join(benchpress_data_dir, x)))
  return os.path.join(benchpress_data_dir, latest_file)

def to_csv(sqlite_file, output_csv_dir, output_csv_name, table_name="prover_res"):
  conn = sqlite3.connect(sqlite_file)
  cursor = conn.cursor()

  cursor.execute(f"SELECT * FROM {table_name}")
  rows = cursor.fetchall()

  column_names = [description[0] for description in cursor.description]

  csv_file_path = os.path.join(output_csv_dir, f"{output_csv_name}.csv")
  with open(csv_file_path, 'w', newline='') as file:
      writer = csv.writer(file)
      writer.writerow(column_names)
      writer.writerows(rows)
  conn.close()

def automate_benchpress_single(directory_to_benchmark, output_csv_dir, output_csv_name, config, prover):
  benchpress_data_dir = "/home/smtml/.local/share/benchpress"
  if run_benchpress(directory_to_benchmark, config, prover):
    try:
      sqlite_file = get_latest_sqlite_file(benchpress_data_dir)
      if not os.path.exists(output_csv_dir):
        os.makedirs(output_csv_dir)
      to_csv(sqlite_file, output_csv_dir, output_csv_name)
      print_success(f"CSV files have been successfully stored in {output_csv_dir}.")
    except Exception as e:
      print_error(f"Error during SQLite to CSV process: {e}")

def get_latest_n_csvs(n):
  result = subprocess.run(['ls', '-d', os.path.join('/home/smtml/smtml/bench', 'res-multi-query-*')],
                            stdout=subprocess.PIPE, text=True, shell=True)
  query_dirs = result.stdout.splitlines()
  query_dirs = query_dirs[-n:]
  csv_files = []
  for query_dir in query_dirs:
        csv_file = glob.glob(os.path.join(query_dir, '*.csv'))
        if csv_file:
            csv_files.append(csv_file[0])
  return csv_files

def concatenate_csvs(csv_paths, output_dir, output_file):
  df_list = [pd.read_csv(csv_file) for csv_file in csv_paths]
  concatenated_df = pd.concat(df_list, ignore_index=True)
  concatenated_df.to_csv(os.path.join(output_dir, output_file), index=False)

def split_file(input_file, output_prefix, lines_per_file=1000):
  with open(input_file, 'r') as file:
      lines = file.readlines()
  total_files = len(lines) // lines_per_file + (1 if len(lines) % lines_per_file else 0)
  aux_files = []
  for i in range(total_files):
      start_index = i * lines_per_file
      end_index = start_index + lines_per_file
      aux_lines = lines[start_index:end_index]
      output_file = f"{output_prefix}_{i+1}.txt"
      aux_files.append(output_file)
      with open(output_file, 'w') as aux_file:
          aux_file.writelines(aux_lines)
  return aux_files

def run_multi_query(list_file, prover):
  try:
    command = ['dune', 'exec', '--', './runner/runner.exe', 'multi-query', '-p', prover, '-F', list_file]
    subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return True
  except subprocess.CalledProcessError as e:
    print_error(f"Error running multi-query: {e.stderr.decode()}")

def automate_multi_query(list_file, output_csv_dir, output_csv_name, prover):
  try:
    aux_files = split_file(list_file, "aux")
    for i, file in enumerate(aux_files):
      run_multi_query(file, prover)
    csv_files = get_latest_n_csvs(i+1)
    concatenate_csvs(csv_files, output_csv_dir, output_csv_name)
    [os.remove(file) for file in aux_files]
    print_success(f"CSV file has been successfully stored in {output_csv_dir}.")
  except Exception as e:
    print_error(f"Error during multi-query process: {e}")

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Automate benchpress and convert the output to CSV.")
  parser.add_argument("--dir", type=str, help="The directory to benchmark.")
  parser.add_argument("--output-dir", type=str, help="The directory to store the CSV files.")
  parser.add_argument("--output-filename", type=str, default="benchpress_output", help="The name of the CSV file.")
  parser.add_argument("--config", type=str, default="/home/smtml/smtml/bench/benchpress.sexp", help="The benchpress config file.")
  parser.add_argument("--prover", type=str, help="The solver to be used.")
  parser.add_argument("--single", action="store_true", help="Run single-query mode.")
  parser.add_argument("--multi", action="store_true", help="Run multi-query mode.")
  parser.add_argument("-j", type=int, help="Number of threads to use.")
  parser.add_argument("-t", type=int, help="Timeout (in seconds).")
  parser.add_argument("-F", type=str, help="The file with the list of benchmarks.")
  args = parser.parse_args()

  if args.single:
    automate_benchpress_single(args.dir, args.output_dir, args.output_filename, args.config, args.prover)
  elif args.multi:
    automate_multi_query(args.F, args.output_dir, args.output_filename, args.prover)
  else:
    print_error("Please specify --single or --multi.")
