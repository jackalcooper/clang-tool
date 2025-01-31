# "LLVM_DIR":"/home/dzheng/opensource/llvm12/build-tool/lib/cmake/llvm"
cmake_minimum_required(VERSION 3.15.0)
project(SimpleProject LANGUAGES CXX)
set(CMAKE_BUILD_TYPE RelWithDebInfo)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_COLOR_NINJA ON)

include(CMakePrintHelpers)
cmake_print_variables(LLVM_ROOT)

set(CMAKE_FIND_PACKAGE_PREFER_CONFIG ON)
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)
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
LLVM
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

set(ONEFLOW_SRC /home/caishenghang/oneflow-table-gen)
set(COMPILATION_DATABASE_DIR ${ONEFLOW_SRC}/build CACHE STRING "" FORCE)
set(TEST_OP_SRC ${ONEFLOW_SRC}/oneflow/user/ops/arg_sort_op.cpp CACHE STRING "" FORCE)
set(REFACTORED_ONEFLOW_SRC /home/caishenghang/oneflow-conda)
set(REFACTORED_COMPILATION_DATABASE_DIR ${REFACTORED_ONEFLOW_SRC}/build)
set(REFACTORED_OP_SRC ${REFACTORED_ONEFLOW_SRC}/oneflow/user/ops/relu_op.cpp CACHE STRING "" FORCE)
get_filename_component(REFACTORED_OP_SRC_BASE_NAME ${REFACTORED_OP_SRC} NAME)
get_filename_component(TEST_OP_SRC_BASE_NAME ${TEST_OP_SRC} NAME)

file(GLOB_RECURSE op_files RELATIVE ${REFACTORED_ONEFLOW_SRC} LIST_DIRECTORIES false "${REFACTORED_ONEFLOW_SRC}/oneflow/user/ops/*.cpp")
add_custom_target(fix-all)

foreach(op_file ${op_files})
  get_filename_component(TargetSuffix ${op_file} NAME_WLE)
  set(target_name Move__${TargetSuffix})
  add_custom_target(${target_name}
  DEPENDS simple-tool
  COMMAND "$<TARGET_FILE:simple-tool>" -p=${REFACTORED_COMPILATION_DATABASE_DIR} ${op_file} > ${op_file}.gen
  WORKING_DIRECTORY ${REFACTORED_ONEFLOW_SRC}
  )
add_dependencies(fix-all ${target_name})
endforeach()

add_custom_target(move-op
  DEPENDS simple-tool
  COMMAND "$<TARGET_FILE:simple-tool>" -p=${COMPILATION_DATABASE_DIR} ${TEST_OP_SRC}
  USES_TERMINAL
)

add_custom_target(dump-op
  COMMAND clang-check -p=${REFACTORED_COMPILATION_DATABASE_DIR} -ast-dump -ast-dump-filter=ReluOp --extra-arg="-fno-color-diagnostics" ${REFACTORED_OP_SRC}  > ${REFACTORED_OP_SRC_BASE_NAME}.ast
  COMMAND clang-check -p=${COMPILATION_DATABASE_DIR} -ast-dump -ast-dump-filter=g_register_trigger0 --extra-arg="-fno-color-diagnostics" ${TEST_OP_SRC}  > ${TEST_OP_SRC_BASE_NAME}.ast
)
