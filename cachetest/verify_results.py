import os

def ensure_dir(directory):
    """Ensure the directory exists. If not, create it."""
    if not os.path.exists(directory):
        os.makedirs(directory)

def get_first_line(filename):
    with open(filename, 'r') as f:
        return f.readline().strip().replace(";", "").strip()  # Remove ';' and trim any surrounding spaces

def get_results_from_outputfile(filename):
    with open(filename, 'r') as f:
        return [line.strip() for line in f if line.strip() in ["sat", "unsat"]]

def write_to_desired_output(subfolder_name, results):
    output_path = os.path.join("desired_output", subfolder_name + ".out")
    with open(output_path, 'w') as f:
        for result in results:
            f.write(result + '\n')

def verify_results(input_folder, output_folder):
    all_matched = True  # A flag to check if everything matched as expected
    
    # Ensure the desired_output folder exists
    ensure_dir("desired_output")

    # Traverse subfolders in alphabetical order
    for subfolder in sorted(os.listdir(input_folder)):
        full_subfolder_path = os.path.join(input_folder, subfolder)
        
        if os.path.isdir(full_subfolder_path):
            # Find the matching output file for the current subfolder
            output_files = [f for f in os.listdir(output_folder) if subfolder in f]
            if not output_files:
                print(f"No output file found for subfolder {subfolder}")
                all_matched = False
                continue

            outputfile_path = os.path.join(output_folder, output_files[0])
            output_results = get_results_from_outputfile(outputfile_path)
            desired_results = []
            
            # Traverse subsubfolders inside subfolder in alphabetical order
            for subsubfolder in sorted(os.listdir(full_subfolder_path)):
                full_subsubfolder_path = os.path.join(full_subfolder_path, subsubfolder)

                if os.path.isdir(full_subsubfolder_path):
                    # Traverse files inside subsubfolder in alphabetical order
                    for filename in sorted(os.listdir(full_subsubfolder_path)):
                        full_file_path = os.path.join(full_subsubfolder_path, filename)
                        
                        if os.path.isfile(full_file_path) and full_file_path.endswith('.smt2'):
                            expected_result = get_first_line(full_file_path)
                            actual_result = output_results.pop(0) if output_results else None
                            desired_results.append(expected_result)

                            if expected_result != actual_result:
                                print(f"Mismatch in file {full_file_path}. Expected {expected_result}, but got {actual_result}.")
                                all_matched = False

            # Write the desired results to the desired_output folder
            write_to_desired_output(subfolder, desired_results)

    if all_matched:
        print("All results matched as expected!")

if __name__ == "__main__":
    input_folder = "../../queries/collections-c_incremental"  # Replace with your input folder path
    output_folder = "results"  # Replace with your output folder path

    verify_results(input_folder, output_folder)
