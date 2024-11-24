# Load the parallel package
library(parallel)

# Function to be applied to each file
process_file <- function(file_name) {
  # Your function code here
  # For example, just print the file name
  print(paste("Processing", file_name))
}

# List of file names (example)
file_names <- c("file1.txt", "file2.txt", "file3.txt")

# Get the number of available cores (this depends on your system)
num_cores <- detectCores()

# Use mclapply to parallelize the function calls
# mclapply will run the process_file function on each file in parallel
result <- mclapply(file_names, process_file, mc.cores = num_cores)

# Check the results
print(result)
