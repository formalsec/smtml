import os
import re
import pandas as pd

def parse_output(file_path):
    with open(file_path, 'r') as file:
        content = file.read()
        
        results = re.findall(r'\b(unsat|sat)\b', content)
        cache_hits = re.findall(r'Cache Hit', content)
        time_match = re.search(r'real\t(.+)', content)  # Only real time is extracted
        
        if time_match:
            real_time = time_match.group(1)
        else:
            real_time = 'N/A'
        
        return {
            'results': results,
            'cache_hit_count': len(cache_hits),
            'real_time': real_time
        }

def extract_folders_and_modes(results_dir):
    output_files = os.listdir(results_dir)
    folders = set()
    modes = set()
    
    for output_file in output_files:
        # Splitting the file name using the '_' and '.' as the format is Name_Mode.out
        split_name = output_file.rsplit('_', 1)[0]
        mode = output_file.rsplit('_', 1)[1].replace(".out", "")
        folders.add(split_name)
        modes.add(mode)
        
    return list(folders), list(modes)

# Directory containing the output files
results_dir = 'results'

# Extract folder and mode information from file names
folders, modes = extract_folders_and_modes(results_dir)

# Data extraction and comparison
comparison_data = {}
for folder in folders:
    comparison_data[folder] = {}
    for mode in modes:
        output_filename = f"{folder}_{mode}.out"
        output_filepath = os.path.join(results_dir, output_filename)
        
        if os.path.exists(output_filepath):
            parsed_data = parse_output(output_filepath)
            comparison_data[folder][mode] = parsed_data
        else:
            print(f"Warning: File {output_filename} does not exist.")
            
# Tabulating using pandas
data_for_table = []

for folder, modes_data in comparison_data.items():
    for mode, parsed_data in modes_data.items():
        row = {
            'Folder': folder,
            'Mode': mode,
            'Result Count': len(parsed_data['results']),
            'Cache Hits': parsed_data['cache_hit_count'],
            'Real Time': parsed_data['real_time']
        }
        data_for_table.append(row)

df = pd.DataFrame(data_for_table)

# Optionally: Save the table to an Excel file
df.to_excel("comparison_results.xlsx", index=False)

# Print the table to console
print(df)
