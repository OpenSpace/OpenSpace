cmake_minimum_required(VERSION 2.8.0)
project(Lua)

mark_as_advanced(CMAKE_BACKWARDS_COMPATIBILITY)

if (POLICY CMP0011)
   cmake_policy(SET CMP0011 OLD) # or even better, NEW
endif (POLICY CMP0011)

if (NOT LUA_ROOT_DIR)
    set(LUA_ROOT_DIR ${PROJECT_SOURCE_DIR})
endif ()

# LUA_USE_APICHECK  in full debug mode

set (LUA_SOURCE
    ${PROJECT_SOURCE_DIR}/src/lapi.c
    ${PROJECT_SOURCE_DIR}/src/lauxlib.c
    ${PROJECT_SOURCE_DIR}/src/lbaselib.c
    ${PROJECT_SOURCE_DIR}/src/lbitlib.c
    ${PROJECT_SOURCE_DIR}/src/lcode.c
    ${PROJECT_SOURCE_DIR}/src/lcorolib.c
    ${PROJECT_SOURCE_DIR}/src/lctype.c
    ${PROJECT_SOURCE_DIR}/src/ldblib.c
    ${PROJECT_SOURCE_DIR}/src/ldebug.c
    ${PROJECT_SOURCE_DIR}/src/ldo.c
    ${PROJECT_SOURCE_DIR}/src/ldump.c
    ${PROJECT_SOURCE_DIR}/src/lfunc.c
    ${PROJECT_SOURCE_DIR}/src/lgc.c
    ${PROJECT_SOURCE_DIR}/src/linit.c
    ${PROJECT_SOURCE_DIR}/src/liolib.c
    ${PROJECT_SOURCE_DIR}/src/llex.c
    ${PROJECT_SOURCE_DIR}/src/lmathlib.c
    ${PROJECT_SOURCE_DIR}/src/lmem.c
    ${PROJECT_SOURCE_DIR}/src/loadlib.c
    ${PROJECT_SOURCE_DIR}/src/lobject.c
    ${PROJECT_SOURCE_DIR}/src/lopcodes.c
    ${PROJECT_SOURCE_DIR}/src/loslib.c
    ${PROJECT_SOURCE_DIR}/src/lparser.c
    ${PROJECT_SOURCE_DIR}/src/lstate.c
    ${PROJECT_SOURCE_DIR}/src/lstring.c
    ${PROJECT_SOURCE_DIR}/src/lstrlib.c
    ${PROJECT_SOURCE_DIR}/src/ltable.c
    ${PROJECT_SOURCE_DIR}/src/ltablib.c
    ${PROJECT_SOURCE_DIR}/src/ltm.c
    ${PROJECT_SOURCE_DIR}/src/lundump.c
    ${PROJECT_SOURCE_DIR}/src/lvm.c
    ${PROJECT_SOURCE_DIR}/src/lzio.c
    )

set (LUA_HEADER
    ${PROJECT_SOURCE_DIR}/include/lapi.h
    ${PROJECT_SOURCE_DIR}/include/lauxlib.h
    ${PROJECT_SOURCE_DIR}/include/lcode.h
    ${PROJECT_SOURCE_DIR}/include/lctype.h
    ${PROJECT_SOURCE_DIR}/include/ldebug.h
    ${PROJECT_SOURCE_DIR}/include/ldo.h
    ${PROJECT_SOURCE_DIR}/include/lfunc.h
    ${PROJECT_SOURCE_DIR}/include/lgc.h
    ${PROJECT_SOURCE_DIR}/include/llex.h
    ${PROJECT_SOURCE_DIR}/include/llimits.h
    ${PROJECT_SOURCE_DIR}/include/lmem.h
    ${PROJECT_SOURCE_DIR}/include/lobject.h
    ${PROJECT_SOURCE_DIR}/include/lopcodes.h
    ${PROJECT_SOURCE_DIR}/include/lparser.h
    ${PROJECT_SOURCE_DIR}/include/lstate.h
    ${PROJECT_SOURCE_DIR}/include/lstring.h
    ${PROJECT_SOURCE_DIR}/include/ltable.h
    ${PROJECT_SOURCE_DIR}/include/ltm.h
    ${PROJECT_SOURCE_DIR}/include/lua.h
    ${PROJECT_SOURCE_DIR}/include/lua.hpp.h
    ${PROJECT_SOURCE_DIR}/include/luaconf.h
    ${PROJECT_SOURCE_DIR}/include/lualib.h
    ${PROJECT_SOURCE_DIR}/include/lundump.h
    ${PROJECT_SOURCE_DIR}/include/lvm.h
    ${PROJECT_SOURCE_DIR}/include/lzio.h
    )

if (WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

add_library(Lua ${LUA_SOURCE})

set (LUA_INCLUDE_DIR "${LUA_HEADER}")
include_directories("${LUA_INCLUDE_DIR}")