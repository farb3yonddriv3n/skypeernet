cmake_minimum_required(VERSION 2.8.9)

project(peer C)
project(tracker C)

if(NOT UNIX)
    message(FATAL_ERROR "Only *nix like systems are supported.")
endif()

# set default build type if not specified by user
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE debug)
endif()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -std=gnu11")
set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG")
set(CMAKE_C_FLAGS_DEBUG "-g -O0 -DDEBUG")

find_package(PkgConfig REQUIRED)
if (PKG_CONFIG_FOUND)
else()
    message(FATAL_ERROR "pkg-config not found")
endif()

set(srcs
src/aes.c
src/backtrace.c
src/base64.c
src/block.c
src/cli.c
src/config.c
src/data.c
src/distfs.c
src/file.c
src/group.c
src/job.c
src/list.c
src/net.c
src/os.c
src/packet.c
src/peer.c
src/root.c
src/rsa.c
src/task.c
src/traffic.c
src/transaction.c
src/utils.c
src/world.c
src/payload/ack.c
src/payload/announce.c
src/payload/fileask.c
src/payload/file.c
src/payload/message.c
src/payload/payload.c
src/payload/ping.c
src/transaction/file_add.c
)

add_library(skypeernetobj OBJECT ${srcs})

add_executable(peer $<TARGET_OBJECTS:skypeernetobj> src/mpeer.c)
add_executable(tracker $<TARGET_OBJECTS:skypeernetobj> src/mtracker.c)

include_directories(src/include)

target_link_libraries(peer m)
target_link_libraries(tracker m)

find_package(OpenSSL REQUIRED)
target_link_libraries(peer ${OPENSSL_LIBRARIES})
target_link_libraries(tracker ${OPENSSL_LIBRARIES})
include_directories(${OPENSSL_INCLUDE_DIR})

include(libev.cmake)
include_directories(${LIBEV_INCLUDE_DIR})
target_link_libraries(peer ${LIBEV_LIBRARY})
target_link_libraries(tracker ${LIBEV_LIBRARY})

include(json-c.cmake)
include_directories(${JSON-C_INCLUDE_DIR})
target_link_libraries(peer ${JSON-C_LIBRARY})
target_link_libraries(tracker ${JSON-C_LIBRARY})

include(readline.cmake)
include_directories(${Readline_INCLUDE_DIR})
target_link_libraries(peer ${Readline_LIBRARY})
target_link_libraries(tracker ${Readline_LIBRARY})

find_package(Threads REQUIRED)
target_link_libraries(peer ${CMAKE_THREAD_LIBS_INIT})
