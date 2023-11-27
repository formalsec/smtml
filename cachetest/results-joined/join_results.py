import os
import re
import numpy as np

def parse_time(time_str):
    """Converts time string of format '0m1.705s' to seconds."""
    match = re.match(r'(\d+)m([\d.]+)s', time_str)
    if match:
        minutes, seconds = map(float, match.groups())
        return 60 * minutes + seconds
    else:
        return None

def format_time(seconds):
    """Formats seconds back into '0m1.705s' format."""
    return f"{int(seconds // 60)}m{seconds % 60:.3f}s"

def process_files(directory):
    files = sorted([f for f in os.listdir(directory) if f.startswith('results-') and f.endswith('.txt')])
    lines_data = {}
    max_times = 0

    for filename in files:
        with open(os.path.join(directory, filename), 'r') as file:
            lines = file.readlines()[1:]  # Skip header line
            for i, line in enumerate(lines):
                if line.strip() == "":  # Skip empty lines
                    continue

                parts = line.split()
                if len(parts) < 6:
                    continue  # Skip lines that don't have enough parts

                time_str = parts[5]
                time_in_seconds = parse_time(time_str)

                if time_in_seconds is not None:
                    if i not in lines_data:
                        lines_data[i] = {
                            'folder': parts[0],
                            'mode': parts[1],
                            'result_count': parts[3],
                            'cache_hits': parts[4],
                            'times': []
                        }
                    lines_data[i]['times'].append(time_in_seconds)
                    max_times = max(max_times, len(lines_data[i]['times']))

    # Calculate average time for each line
    for line_data in lines_data.values():
        average_time = np.mean(line_data['times'])
        line_data['average_time'] = format_time(average_time)

    return lines_data, max_times

def write_results(directory, lines_data, max_times):
    header = "Folder Mode ResultCount CacheHits " + " ".join([f"Time{i+1}" for i in range(max_times)]) + " TimeAvg\n"
    format_str = "{:<8} {:<8} {:<12} {:<10} " + " ".join(["{:<10}" for _ in range(max_times)]) + " {}\n"

    with open(os.path.join(directory, 'combined_results.txt'), 'w') as file:
        file.write(header)
        for line_data in lines_data.values():
            times_str = [format_time(t) for t in line_data['times']] + [""] * (max_times - len(line_data['times']))
            line = format_str.format(line_data['folder'], line_data['mode'], line_data['result_count'], line_data['cache_hits'], *times_str, line_data['average_time'])
            file.write(line)

def main():
    directory = '.'  # Replace with your directory path
    lines_data, max_times = process_files(directory)
    write_results(directory, lines_data, max_times)

if __name__ == "__main__":
    main()
