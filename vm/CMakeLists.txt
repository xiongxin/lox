# cmake 最低版本需求
cmake_minimum_required(VERSION 3.13)

# 工程名称
project(vm)

# 设置
SET(CMAKE_CXX_COMPILER "gcc")

#设定可执行二进制文件的目录
SET( EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin) 

#设定存放编译出来的库文件的目录
SET( LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib) 

#并且把该目录设为连接目录
LINK_DIRECTORIES( ${PROJECT_SOURCE_DIR}/lib)

INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/include/)

FILE(GLOB vm_SRC ${PROJECT_SOURCE_DIR}/src/*.c )

# 编译源码生成目标
add_executable(vm ${vm_SRC})