cmake_minimum_required(VERSION 2.8.9)

project(spnpeer C)
project(spntracker C)
project(spnvalidate C)
project(spntest C)

if(NOT UNIX)
    message(FATAL_ERROR "Only *nix like systems are supported.")
endif()

# set default build type if not specified by user
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE release)
endif()

execute_process (
    COMMAND bash tools/version/main.sh
)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -std=gnu11")
set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG")
set(CMAKE_C_FLAGS_DEBUG "-g -O0 -DDEBUG")

find_package(PkgConfig REQUIRED)
if (NOT PKG_CONFIG_FOUND)
    message(FATAL_ERROR "pkg-config not found")
endif()

set(srcs
src/aes.c
src/api.c
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
src/rogue.c
src/root.c
src/rsa.c
src/task.c
src/traffic.c
src/transaction.c
src/utils.c
src/version.c
src/world.c
src/payload/ack.c
src/payload/announce.c
src/payload/auth.c
src/payload/auth_reply.c
src/payload/fileask.c
src/payload/file.c
src/payload/message.c
src/payload/payload.c
src/payload/ping.c
src/transaction/file_add.c
)

set(srcs_test
test/t1.c
test/t2.c
test/t3.c
test/t4.c
)

add_library(skypeernetobj OBJECT ${srcs})
add_library(skypeernetobjtest OBJECT ${srcs_test})

add_executable(spnpeer $<TARGET_OBJECTS:skypeernetobj> src/mpeer.c)
add_executable(spntracker $<TARGET_OBJECTS:skypeernetobj> src/mtracker.c)
add_executable(spnvalidate $<TARGET_OBJECTS:skypeernetobj> tools/validate/main.c)
add_executable(spntest $<TARGET_OBJECTS:skypeernetobj> $<TARGET_OBJECTS:skypeernetobjtest> test/test.c)

include_directories(src/include)
include_directories(test/)

target_link_libraries(spnpeer m)
target_link_libraries(spntracker m)
target_link_libraries(spnvalidate m)
target_link_libraries(spntest m)

find_package(Threads REQUIRED)
target_link_libraries(spnpeer ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(spntracker ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(spnvalidate ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(spntest ${CMAKE_THREAD_LIBS_INIT})

find_package(OpenSSL REQUIRED)
target_link_libraries(spnpeer ${OPENSSL_LIBRARIES})
target_link_libraries(spntracker ${OPENSSL_LIBRARIES})
target_link_libraries(spnvalidate ${OPENSSL_LIBRARIES})
target_link_libraries(spntest ${OPENSSL_LIBRARIES})
include_directories(${OPENSSL_INCLUDE_DIR})

include(libev.cmake)
if(NOT LIBEV_FOUND)
    message(FATAL_ERROR "libev not found")
endif()
include_directories(${LIBEV_INCLUDE_DIR})
target_link_libraries(spnpeer ${LIBEV_LIBRARY})
target_link_libraries(spntracker ${LIBEV_LIBRARY})
target_link_libraries(spnvalidate ${LIBEV_LIBRARY})
target_link_libraries(spntest ${LIBEV_LIBRARY})

include(json-c.cmake)
if(NOT JSON-C_FOUND)
    message(FATAL_ERROR "libjson-c not found")
endif()
include_directories(${JSON-C_INCLUDE_DIR})
target_link_libraries(spnpeer ${JSON-C_LIBRARY})
target_link_libraries(spntracker ${JSON-C_LIBRARY})
target_link_libraries(spnvalidate ${JSON-C_LIBRARY})
target_link_libraries(spntest ${JSON-C_LIBRARY})

include(readline.cmake)
if(NOT READLINE_FOUND)
    message(FATAL_ERROR "libreadline not found")
endif()
include_directories(${Readline_INCLUDE_DIR})
target_link_libraries(spnpeer ${Readline_LIBRARY})
target_link_libraries(spntracker ${Readline_LIBRARY})
target_link_libraries(spnvalidate ${Readline_LIBRARY})
target_link_libraries(spntest ${Readline_LIBRARY})

include(cunit.cmake)
if(NOT CUNIT_FOUND)
    message(FATAL_ERROR "libcunit not found")
endif()
include_directories(${CUNIT_INCLUDE_DIR})
target_link_libraries(spntest ${CUNIT_LIBRARY})

install(TARGETS spnpeer spntracker DESTINATION /usr/local/bin/)
install(FILES config/settings.cfg DESTINATION /etc/skypeernet/
        RENAME peer.cfg)
install(FILES config/settings.cfg DESTINATION /etc/skypeernet/
        RENAME tracker.cfg)
