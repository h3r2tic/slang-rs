git clone https://github.com/shader-slang/slang --recursive --single-branch --branch master
cd slang
git checkout 391d35d63e83293855180b1cce061ca6b73a8b56
git submodule sync
git submodule update --init --recursive
cmake -B build -G "Ninja" -DCMAKE_BUILD_TYPE=Release -DSLANG_LIB_TYPE=STATIC -DSLANG_ENABLE_DXIL=0 -DSLANG_ENABLE_SLANGD=0 -DSLANG_ENABLE_SLANGI=0 -DSLANG_ENABLE_SLANGRT=0 -DSLANG_ENABLE_SLANG_GLSLANG=0 -DSLANG_ENABLE_TESTS=0 -DSLANG_ENABLE_RELEASE_DEBUG_INFO=0 -DSLANG_ENABLE_EXAMPLES=0 -DSLANG_ENABLE_SLANG_RHI=0 -DSLANG_SLANG_LLVM_FLAVOR=DISABLE
cmake --build build --parallel

rem The regular install process fails, so let's just grab the libs we need.
rem cmake --install build --prefix ../slang-install

mkdir ..\..\slang\lib\x86_64-pc-windows-msvc
copy build\Release\lib\*.lib ..\..\slang\lib\x86_64-pc-windows-msvc\
copy build\external\miniz\miniz.lib ..\..\slang\lib\x86_64-pc-windows-msvc\
copy build\external\lz4\build\cmake\lz4.lib ..\..\slang\lib\x86_64-pc-windows-msvc\

cd ..
rmdir /s /q slang
