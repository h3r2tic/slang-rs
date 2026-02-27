# Use a modern Ubuntu LTS base with up-to-date build tooling
FROM ubuntu:24.04

# Set the environment variable to avoid interactive prompts during the build
ENV DEBIAN_FRONTEND=noninteractive

# Install build dependencies from Ubuntu's native repositories
RUN apt-get update && apt-get install -y \
    ca-certificates \
    cmake \
    g++ \
    gcc \
    git \
    ninja-build \
    && rm -rf /var/lib/apt/lists/*

# Set up a directory for the build
WORKDIR /workspace

# Clone the repo
RUN git clone https://github.com/h3r2tic/slang.git --recursive --single-branch --branch fixes

# Move into the project directory
WORKDIR /workspace/slang

# Check out specific version
RUN git checkout 1b4db085
RUN git submodule sync
RUN git submodule update --init --recursive

RUN cmake -B build -G Ninja \
    -DCMAKE_CXX_STANDARD=20 \
    -DSLANG_LIB_TYPE=STATIC \
    -DSLANG_ENABLE_DXIL=0 \
    -DSLANG_ENABLE_SLANGD=0 \
    -DSLANG_ENABLE_SLANGI=0 \
    -DSLANG_ENABLE_SLANGRT=0 \
    -DSLANG_ENABLE_SLANG_GLSLANG=0 \
    -DSLANG_ENABLE_TESTS=0 \
    -DSLANG_ENABLE_RELEASE_DEBUG_INFO=0 \
    -DSLANG_ENABLE_EXAMPLES=0 \
    -DSLANG_ENABLE_SLANG_RHI=0 \
    -DSLANG_SLANG_LLVM_FLAVOR=DISABLE

RUN cmake --build build -j

# Set the build directory as the working directory
WORKDIR /workspace/slang/build
