FROM rockylinux:8

ARG SLANG_REPO_URL=https://github.com/h3r2tic/slang.git
ARG SLANG_REPO_BRANCH=fixes
ARG SLANG_COMMIT=1b4db085
ARG SLANG_BUILD_JOBS=8
ARG GCC_TOOLSET_VERSION=11

SHELL ["/bin/bash", "-lc"]

# Rocky 8 gives us an older glibc baseline; GCC Toolset can be tuned for the libstdc++ floor.
RUN dnf install -y \
    ca-certificates \
    cmake \
    gcc-toolset-${GCC_TOOLSET_VERSION} \
    git \
    make \
    python3 \
    which \
    && dnf clean all

WORKDIR /workspace

RUN git clone --recursive --single-branch --branch "${SLANG_REPO_BRANCH}" "${SLANG_REPO_URL}" slang

WORKDIR /workspace/slang

RUN git checkout "${SLANG_COMMIT}" \
    && git submodule sync \
    && git submodule update --init --recursive

RUN source /opt/rh/gcc-toolset-${GCC_TOOLSET_VERSION}/enable \
    && cmake -B build -G "Unix Makefiles" \
        -DCMAKE_BUILD_TYPE=Release \
        -DSLANG_LIB_TYPE=STATIC \
        -DSLANG_ENABLE_DXIL=0 \
        -DSLANG_ENABLE_SLANGD=0 \
        -DSLANG_ENABLE_SLANGI=0 \
        -DSLANG_ENABLE_SLANGRT=0 \
        -DSLANG_ENABLE_SLANG_GLSLANG=0 \
        -DSLANG_ENABLE_REPLAYER=0 \
        -DSLANG_ENABLE_TESTS=0 \
        -DSLANG_ENABLE_RELEASE_DEBUG_INFO=0 \
        -DSLANG_ENABLE_EXAMPLES=0 \
        -DSLANG_ENABLE_SLANG_RHI=0 \
        -DSLANG_SLANG_LLVM_FLAVOR=DISABLE

RUN source /opt/rh/gcc-toolset-${GCC_TOOLSET_VERSION}/enable \
    && cmake --build build --parallel "${SLANG_BUILD_JOBS}"

WORKDIR /workspace/slang/build
