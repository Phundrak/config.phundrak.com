cmake_minimum_required(VERSION 3.10)

project("PROJECTNAME"
        VERSION 0.1
        DESCRIPTION "Description of PROJECTNAME"
        HOMEPAGE_URL "https://labs.phundrak.fr/phundrak/PROJECTNAME"
        LANGUAGES C)

set(CMAKE_C_COMPILER /usr/bin/clang)
file(GLOB SRC_FILES "src/*.c")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
include(functions)

enable_c_compiler_flag_if_supported("-Wall")
enable_c_compiler_flag_if_supported("-pedantic")
enable_c_compiler_flag_if_supported("-Wextra")
enable_c_compiler_flag_if_supported("-Wfloat-equal")
enable_c_compiler_flag_if_supported("-Wwrite-strings")
enable_c_compiler_flag_if_supported("-Wpointer-arith")
enable_c_compiler_flag_if_supported("-Wcast-qual")
enable_c_compiler_flag_if_supported("-Wcast-align")
enable_c_compiler_flag_if_supported("-Wconversion")
enable_c_compiler_flag_if_supported("-Wshadow")
enable_c_compiler_flag_if_supported("-Wreduntant-decls")
enable_c_compiler_flag_if_supported("-Wdouble-promotion")
enable_c_compiler_flag_if_supported("-Winit-self")
enable_c_compiler_flag_if_supported("-Wswitch-default")
enable_c_compiler_flag_if_supported("-Wswitch-enum")
enable_c_compiler_flag_if_supported("-Wundef")
enable_c_compiler_flag_if_supported("-Winline")
enable_c_compiler_flag_if_supported("-Wpedantic")
enable_c_compiler_flag_if_supported("-Wsign-conversion")
enable_c_compiler_flag_if_supported("-Wnull-dereference")
enable_c_compiler_flag_if_supported("-Wuseless-cast")
enable_c_compiler_flag_if_supported("-Wformat=2")
enable_c_compiler_flag_if_supported("-Wlifetime")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  enable_c_compiler_flag_if_supported("-g")
else()
  enable_c_compiler_flag_if_supported("-O3")
  enable_c_compiler_flag_if_supported("-flto")
endif()

# include_directories(<PUBLIC HEADER DIRECTORIES>)

# Main software
set(TGT PROJECTNAME)
set(EXECUTABLE_OUTPUT_PATH  ${PROJECT_SOURCE_DIR}/build/bin)
add_executable(${TGT} ${SRC_FILES})
target_compile_features(${TGT} PRIVATE c_std_11)
target_include_directories(${TGT} PRIVATE include/PROJECTNAME)
#target_link_libraries(${TGT})

# Tests, -DTESTS=True to activate
if(TESTS)
  set(TESTTGT PROJECTNAME-tests)
  file(GLOB TEST_FILES "tests/tests.c")
  add_executable(${TESTTGT} ${TEST_FILES})
  set_property(TARGET ${TESTTGT} PROPERTY C_STANDARD 11)
  target_include_directories(${TESTTGT} PRIVATE include/PROJECTNAME)
endif()

# OS specific instructions.
if(APPLE)
elseif(WIN32)
    # Windows developer environment specific instructions.
    if(MINGW)
    elseif(MSYS)
    elseif(CYGWIN)
    endif()
elseif(UNIX)
else()
endif()
