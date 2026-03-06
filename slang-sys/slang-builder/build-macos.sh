#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
CRATE_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
OUTPUT_DIR="${CRATE_DIR}/slang/lib/aarch64-apple-darwin"

SLANG_REPO_URL=${SLANG_REPO_URL:-https://github.com/h3r2tic/slang.git}
SLANG_REPO_BRANCH=${SLANG_REPO_BRANCH:-fixes}
SLANG_COMMIT=${SLANG_COMMIT:-1b4db085}
SLANG_BUILD_JOBS=${SLANG_BUILD_JOBS:-8}
MACOSX_DEPLOYMENT_TARGET=${MACOSX_DEPLOYMENT_TARGET:-11.0}
CMAKE_OSX_ARCHITECTURES=${CMAKE_OSX_ARCHITECTURES:-arm64}
if [[ -n "${WORK_DIR:-}" ]]; then
    OWN_WORK_DIR=0
else
    WORK_DIR=$(mktemp -d)
    OWN_WORK_DIR=1
fi
SLANG_DIR="${WORK_DIR}/slang"

cleanup() {
    if [[ "${OWN_WORK_DIR}" == "1" && -d "${WORK_DIR}" ]]; then
        rm -rf "${WORK_DIR}"
    fi
}

trap cleanup EXIT

git clone --recursive --single-branch --branch "${SLANG_REPO_BRANCH}" "${SLANG_REPO_URL}" "${SLANG_DIR}"

pushd "${SLANG_DIR}" >/dev/null
git checkout "${SLANG_COMMIT}"
git submodule sync
git submodule update --init --recursive

cmake -B build -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_OSX_ARCHITECTURES="${CMAKE_OSX_ARCHITECTURES}" \
    -DCMAKE_OSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET}" \
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

cmake --build build --parallel "${SLANG_BUILD_JOBS}"
popd >/dev/null

rm -rf "${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}"

artifacts=(
    "${SLANG_DIR}/build/Release/lib/libslang-compiler.a"
    "${SLANG_DIR}/build/Release/lib/libcompiler-core.a"
    "${SLANG_DIR}/build/Release/lib/libcore.a"
    "${SLANG_DIR}/build/external/miniz/libminiz.a"
    "${SLANG_DIR}/build/external/lz4/build/cmake/liblz4.a"
)

for artifact in "${artifacts[@]}"; do
    cp "${artifact}" "${OUTPUT_DIR}/"
done

if command -v strip >/dev/null 2>&1; then
    strip -S "${OUTPUT_DIR}"/*.a || true
fi

echo "Build artifacts have been copied to ${OUTPUT_DIR}"
