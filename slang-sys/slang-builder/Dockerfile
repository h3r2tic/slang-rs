# Use a base image with the necessary tools
FROM ubuntu:20.04

# Set the environment variable to avoid interactive prompts during the build
ENV DEBIAN_FRONTEND=noninteractive

# Install required packages and add Kitware's APT repository for the latest CMake
RUN apt-get update && apt-get install -y \
    git \
    ninja-build \
    clang \
    libstdc++-10-dev \
    python3 \
    python3-pip \
    wget \
    gpg \
    software-properties-common \
    && wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc | apt-key add - \
    && apt-add-repository 'deb https://apt.kitware.com/ubuntu/ focal main' \
    && apt-get update && apt-get install -y cmake \
    && rm -rf /var/lib/apt/lists/*

# Set up a directory for the build
WORKDIR /workspace

# Clone the repo
RUN git clone https://github.com/shader-slang/slang --recursive --single-branch --branch master

# Move into the project directory
WORKDIR /workspace/slang

# Check out specific version
RUN git checkout 391d35d63e83293855180b1cce061ca6b73a8b56
RUN git submodule sync
RUN git submodule update --init --recursive

RUN cmake -B build -G Ninja \
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
