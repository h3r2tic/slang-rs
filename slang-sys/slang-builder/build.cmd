@echo off
setlocal

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "CRATE_DIR=%%~fI"
set "OUTPUT_DIR=%CRATE_DIR%\slang\lib\x86_64-pc-windows-msvc"

if not defined SLANG_REPO_URL set "SLANG_REPO_URL=https://github.com/h3r2tic/slang.git"
if not defined SLANG_REPO_BRANCH set "SLANG_REPO_BRANCH=fixes"
if not defined SLANG_COMMIT set "SLANG_COMMIT=1b4db085"
if not defined SLANG_BUILD_JOBS set "SLANG_BUILD_JOBS=8"
if not defined SLANG_CMAKE_GENERATOR set "SLANG_CMAKE_GENERATOR=Visual Studio 17 2022"
if not defined SLANG_CMAKE_ARCH set "SLANG_CMAKE_ARCH=x64"
if not defined SLANG_CMAKE_TOOLSET set "SLANG_CMAKE_TOOLSET=ClangCL"
if not defined WORK_DIR (
    set "WORK_DIR=%TEMP%\slang-builder-%RANDOM%%RANDOM%"
    set "OWN_WORK_DIR=1"
) else (
    set "OWN_WORK_DIR=0"
)

set "SLANG_DIR=%WORK_DIR%\slang"

if exist "%WORK_DIR%" rmdir /s /q "%WORK_DIR%"
mkdir "%WORK_DIR%" || exit /b 1

git clone --recursive --single-branch --branch "%SLANG_REPO_BRANCH%" "%SLANG_REPO_URL%" "%SLANG_DIR%" || exit /b 1

pushd "%SLANG_DIR%" || exit /b 1
git checkout "%SLANG_COMMIT%" || exit /b 1
git submodule sync || exit /b 1
git submodule update --init --recursive || exit /b 1

set "CMAKE_TOOLSET_ARG="
if defined SLANG_CMAKE_TOOLSET set "CMAKE_TOOLSET_ARG=-T %SLANG_CMAKE_TOOLSET%"

set "CMAKE_C_FLAGS_ARG="
set "CMAKE_CXX_FLAGS_ARG="
if /I "%SLANG_CMAKE_TOOLSET%"=="ClangCL" (
    set "CMAKE_C_FLAGS_ARG=-DCMAKE_C_FLAGS=-w"
    set "CMAKE_CXX_FLAGS_ARG=-DCMAKE_CXX_FLAGS=-w /EHsc"
)

cmake -B build -G "%SLANG_CMAKE_GENERATOR%" -A "%SLANG_CMAKE_ARCH%" %CMAKE_TOOLSET_ARG% "%CMAKE_C_FLAGS_ARG%" "%CMAKE_CXX_FLAGS_ARG%" -DCMAKE_BUILD_TYPE=Release -DCMAKE_POLICY_DEFAULT_CMP0141=NEW -DCMAKE_MSVC_DEBUG_INFORMATION_FORMAT= -DSLANG_LIB_TYPE=STATIC -DSLANG_ENABLE_DXIL=0 -DSLANG_ENABLE_SLANGD=0 -DSLANG_ENABLE_SLANGI=0 -DSLANG_ENABLE_SLANGRT=0 -DSLANG_ENABLE_SLANG_GLSLANG=0 -DSLANG_ENABLE_REPLAYER=0 -DSLANG_ENABLE_TESTS=0 -DSLANG_ENABLE_RELEASE_DEBUG_INFO=0 -DSLANG_ENABLE_EXAMPLES=0 -DSLANG_ENABLE_SLANG_RHI=0 -DSLANG_SLANG_LLVM_FLAVOR=DISABLE || exit /b 1
cmake --build build --config Release --parallel %SLANG_BUILD_JOBS% || exit /b 1
popd

if exist "%OUTPUT_DIR%" rmdir /s /q "%OUTPUT_DIR%"
mkdir "%OUTPUT_DIR%" || exit /b 1

copy "%SLANG_DIR%\build\Release\lib\compiler-core.lib" "%OUTPUT_DIR%\" || exit /b 1
copy "%SLANG_DIR%\build\Release\lib\core.lib" "%OUTPUT_DIR%\" || exit /b 1
copy "%SLANG_DIR%\build\Release\lib\slang-compiler.lib" "%OUTPUT_DIR%\" || exit /b 1
copy "%SLANG_DIR%\build\external\lz4\build\cmake\Release\lz4.lib" "%OUTPUT_DIR%\" || exit /b 1
copy "%SLANG_DIR%\build\external\miniz\Release\miniz.lib" "%OUTPUT_DIR%\" || exit /b 1

where llvm-strip >nul 2>&1
if %ERRORLEVEL% EQU 0 (
    for %%F in ("%OUTPUT_DIR%\*.lib") do llvm-strip -g "%%~fF"
)

if "%OWN_WORK_DIR%"=="1" rmdir /s /q "%WORK_DIR%"

echo Build artifacts have been copied to %OUTPUT_DIR%
