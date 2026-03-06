#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
CRATE_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
OUTPUT_DIR="${CRATE_DIR}/slang/lib/x86_64-unknown-linux-gnu"

CONTAINER_RUNTIME=${CONTAINER_RUNTIME:-docker}
IMAGE_NAME=${IMAGE_NAME:-slang-build-linux}
SLANG_REPO_URL=${SLANG_REPO_URL:-https://github.com/h3r2tic/slang.git}
SLANG_REPO_BRANCH=${SLANG_REPO_BRANCH:-fixes}
SLANG_COMMIT=${SLANG_COMMIT:-1b4db085}
SLANG_BUILD_JOBS=${SLANG_BUILD_JOBS:-8}
GCC_TOOLSET_VERSION=${GCC_TOOLSET_VERSION:-10}

cleanup() {
    if [[ -n "${CONTAINER_ID:-}" ]]; then
        "${CONTAINER_RUNTIME}" rm -f "${CONTAINER_ID}" >/dev/null 2>&1 || true
    fi
}

trap cleanup EXIT

"${CONTAINER_RUNTIME}" build \
    --build-arg "SLANG_REPO_URL=${SLANG_REPO_URL}" \
    --build-arg "SLANG_REPO_BRANCH=${SLANG_REPO_BRANCH}" \
    --build-arg "SLANG_COMMIT=${SLANG_COMMIT}" \
    --build-arg "SLANG_BUILD_JOBS=${SLANG_BUILD_JOBS}" \
    --build-arg "GCC_TOOLSET_VERSION=${GCC_TOOLSET_VERSION}" \
    -t "${IMAGE_NAME}" \
    "${SCRIPT_DIR}"

CONTAINER_ID=$("${CONTAINER_RUNTIME}" create "${IMAGE_NAME}")
"${CONTAINER_RUNTIME}" start -a "${CONTAINER_ID}"

rm -rf "${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}"

artifacts=(
    /workspace/slang/build/Release/lib/libslang-compiler.a
    /workspace/slang/build/Release/lib/libcompiler-core.a
    /workspace/slang/build/Release/lib/libcore.a
    /workspace/slang/build/external/miniz/libminiz.a
    /workspace/slang/build/external/lz4/build/cmake/liblz4.a
)

for artifact in "${artifacts[@]}"; do
    "${CONTAINER_RUNTIME}" cp "${CONTAINER_ID}:${artifact}" "${OUTPUT_DIR}/"
done

echo "Build artifacts have been copied to ${OUTPUT_DIR}"
