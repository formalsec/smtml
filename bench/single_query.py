import os
import subprocess
import csv
import sqlite3
import argparse

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

def automate_benchpress(directory_to_benchmark, output_csv_dir, output_csv_name, config, prover):
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

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description="Automate benchpress and convert the output to CSV.")
  parser.add_argument("--dir", type=str, help="The directory to benchmark.")
  parser.add_argument("--output-dir", type=str, help="The directory to store the CSV files.")
  parser.add_argument("--output-filename", type=str, default="benchpress_output", help="The name of the CSV file.")
  parser.add_argument("--config", type=str, default="/home/smtml/smtml/bench/benchpress.sexp", help="The name of the CSV file.")
  parser.add_argument("--prover", type=str, help="The solver to be used.")
  parser.add_argument("-j", type=int, help="Number of threads to use.")
  parser.add_argument("-t", type=int, help="Timeout (in seconds).")
  args = parser.parse_args()
  automate_benchpress(args.dir, args.output_dir, args.output_filename, args.config, args.prover)
