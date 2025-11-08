#!/bin/bash

# Exit the script on any command with a non-zero exit code.
set -e

# Build the Docker image
docker build -t slang-build .

# Run the Docker container and build the project
# This command will run the container, but it will exit once the build is complete
container_id=$(docker create slang-build)

# Start the container and wait for it to complete
docker start -a $container_id

rm -rf ../slang/lib/x86_64-unknown-linux-gnu
mkdir -p ../slang/lib/x86_64-unknown-linux-gnu

# Copy the build artifacts from the container to the host machine
# The paths inside the container are relative to the container's file system
docker cp $container_id:/workspace/slang/build/Release/lib/libslang-compiler.a ../slang/lib/x86_64-unknown-linux-gnu/
docker cp $container_id:/workspace/slang/build/Release/lib/libcompiler-core.a ../slang/lib/x86_64-unknown-linux-gnu/
docker cp $container_id:/workspace/slang/build/Release/lib/libcore.a ../slang/lib/x86_64-unknown-linux-gnu/
docker cp $container_id:/workspace/slang/build/external/miniz/libminiz.a ../slang/lib/x86_64-unknown-linux-gnu/
docker cp $container_id:/workspace/slang/build/external/lz4/build/cmake/liblz4.a ../slang/lib/x86_64-unknown-linux-gnu/

# Clean up the container (optional but recommended)
docker rm $container_id

# Notify user of success
echo "Build artifacts have been copied to ../slang/lib/x86_64-unknown-linux-gnu/"
