# Use a base image with the necessary tools
FROM ubuntu:20.04

# Set the environment variable to avoid interactive prompts during the build
ENV DEBIAN_FRONTEND=noninteractive

# Base utilities needed for adding repos
RUN apt-get update && apt-get install -y \
    software-properties-common \
    ca-certificates \
    gnupg \
    wget \
 && rm -rf /var/lib/apt/lists/*

# Add repos: Toolchain PPA (GCC 13) and Kitware (new CMake)
RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc | apt-key add -
RUN apt-add-repository 'deb https://apt.kitware.com/ubuntu/ focal main'

# Install CMake (new), then compilers, then common build tools
RUN apt-get update && apt-get install -y cmake \
 && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    gcc-13 g++-13 \
 && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    git \
    ninja-build \
 && rm -rf /var/lib/apt/lists/*

# Set up a directory for the build
WORKDIR /workspace

# Clone the repo
RUN git clone https://github.com/shader-slang/slang --recursive --single-branch --branch master

# Move into the project directory
WORKDIR /workspace/slang

# Check out specific version
# v2025.21.2; many versions prior to this ship with broken IR deserialization
RUN git checkout 6bf200902e5dc2ae2517303efae2a1ad2c36b04d
RUN git submodule sync
RUN git submodule update --init --recursive

RUN cmake -B build -G Ninja \
    -DCMAKE_C_COMPILER=/usr/bin/gcc-13 \
    -DCMAKE_CXX_COMPILER=/usr/bin/g++-13 \
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
