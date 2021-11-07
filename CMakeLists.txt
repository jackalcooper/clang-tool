# "LLVM_DIR":"/home/dzheng/opensource/llvm12/build-tool/lib/cmake/llvm"
cmake_minimum_required(VERSION 3.15.0)
project(SimpleProject LANGUAGES CXX)

include(CMakePrintHelpers)
cmake_print_variables(LLVM_ROOT)

set(CMAKE_FIND_PACKAGE_PREFER_CONFIG ON)

# Find CMake file for Clang
find_package(Clang REQUIRED
            ONLY_CMAKE_FIND_ROOT_PATH
            NO_DEFAULT_PATH)

# Add path to LLVM modules
set(CMAKE_MODULE_PATH
  ${CMAKE_MODULE_PATH}
  "${LLVM_CMAKE_DIR}"
  )

# import LLVM CMake functions
include(AddLLVM)


# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.
include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})

# separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
# add_definitions(${LLVM_DEFINITIONS_LIST})

add_definitions(${LLVM_DEFINITIONS})
add_definitions(${CLANG_DEFINITIONS})

# Now build our tools
# add_executable(simple-tool tool.cpp)
add_llvm_executable(simple-tool tool.cpp)

set_property(TARGET simple-tool PROPERTY CXX_STANDARD 17)
target_link_libraries(simple-tool PRIVATE 
LLVMCore
LLVMIRReader
LLVMSupport
clangFrontend
clangSerialization
clangDriver
clangParse
clangSema
clangAnalysis
clangAST
clangBasic
clangEdit
clangLex
clangTransformer
clangTooling)