cmake_minimum_required(VERSION 3.15)
project(main)

set(Python_FIND_VIRTUALENV ONLY)
find_package(Python COMPONENTS Interpreter Development) 
message(STATUS "Python path: ${Python_EXECUTABLE}")
find_package(pybind11 REQUIRED)

add_executable(main main.cpp functions.cpp)
target_link_libraries(main pybind11::embed)