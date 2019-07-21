cmake_minimum_required(VERSION 3.14)

project("PROJECTNAME"
        VERSION 0.1
        DESCRIPTION "Description of PROJECTNAME"
        HOMEPAGE_URL "https://labs.phundrak.fr/phundrak/PROJECTNAME"
        LANGUAGES CXX)

set(CMAKE_CXX_COMPILER /usr/bin/clang++)
file(GLOB SRC_FILES "src/*.cc")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(functions)
include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

enable_cxx_compiler_flag_if_supported("-Wall")
enable_cxx_compiler_flag_if_supported("-pedantic")
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  enable_cxx_compiler_flag_if_supported("-g")
else()
  enable_cxx_compiler_flag_if_supported("-O3")
  enable_cxx_compiler_flag_if_supported("-flto")
endif()

# include_directories(<PUBLIC HEADER DIRECTORIES>)

# Main software
set(TGT PROJECTNAME)
add_executable(${TGT} ${SRC_FILES})
target_compile_features(${TGT} PRIVATE cxx_std_17)
target_include_directories(${TGT} PRIVATE include/PROJECTNAME)
target_link_libraries(${TGT} ${CONAN_LIBS})

# Tests, -DTESTS=True to activate
if(TESTS)
  set(TESTTGT PROJECTNAME-tests)
  file(GLOB TEST_FILES "tests/tests.cc")
  add_executable(${TESTTGT} ${TEST_FILES})
  target_compile_features(${TESTTGT} PRIVATE cxx_std_17)
  target_include_directories(${TESTTGT} PRIVATE include/PROJECTNAME)
  target_link_libraries(${TESTTGT} ${CONAN_LIBS})
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
