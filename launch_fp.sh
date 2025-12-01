#!/bin/bash

# Set the output file path
output_file="$HOME/.fp_mount.dat"

# Set the duration for 1 hour (in seconds) and interval of 5 seconds
duration=$((60 * 6))  # 1 hour = 3600 seconds
interval=5  # 5 seconds

# Get the start time
start_time=$(date +%s)

# Background function to log mount command every 5 seconds
log_mount() {
  while [ $(($(date +%s) - start_time)) -lt $duration ]; do
    # Run the 'mount' command and write the output to the file
    mount > "$output_file"
    
    # Wait for 5 seconds before running again
    sleep $interval
  done
}

# Start the mount logging in the background
log_mount &

# Run the Docker container
docker run -v $HOME:/root -v /Users/:/Users/ --workdir $PWD -p 3838:3838 --name test --rm -i -t price0416/fp_docker:3.2.0 /bin/bash

