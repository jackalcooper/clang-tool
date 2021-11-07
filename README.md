# clang-tool
framework for a c++ tools using LLVM libtooling

## Getting started
To develop clang tools, here are the steps to follow:

### build LLVM and Clang libraries

replace "llvm-del-tool_directory" with the directory you would like all the clang/llvm header files/libraries to be installed.

```bash
mkdir build && cd build
cmake -G Ninja -DCMAKE_CXX_COMPILER=c++ -DCMAKE_C_COMPILER=cc -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;libc;libclc;libcxx;libcxxabi" -DLLVM_TARGETS_TO_BUILD=X86 ../llvm -DLLDB_INCLUDE_TESTS=off -DCMAKE_BUILD_TYPE:STRING=Release -DLLVM_LIBC_ENABLE_LINTING=OFF -DCMAKE_INSTALL_PREFIX=<llvm-dev-tool _directory>
ninja
ninja install
```

### setup building environment
To build the example file, use this command
```bash
mkdir build && build
cmake -G Ninja -DClang_DIR=<llvm-dev-tool _directory>/lib/cmake/clang ../
ninja
```

or if you are using vscode, add following lines to your .vscode/settings.json
```json
{
        "cmake.sourceDirectory": "${workspaceFolder}/.",
        "cmake.configureSettings": {
                "Clang_DIR":"<llvm-dev-tool _directory>/lib/cmake/clang",
        }
}
```