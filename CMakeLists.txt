cmake_minimum_required (VERSION 3.0)
project (pa_function)

set (CMAKE_CXX_STANDARD 20)

include_directories(
  /
)

add_library(pa_function STATIC
  "${pa_function_SOURCE_DIR}/pa_function.hpp"
)

set_target_properties(pa_function PROPERTIES LINKER_LANGUAGE CXX)


add_executable(pa_function_example
  "${pa_function_SOURCE_DIR}/example.cpp"
)
