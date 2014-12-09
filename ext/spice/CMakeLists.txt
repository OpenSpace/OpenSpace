cmake_minimum_required (VERSION 2.8) 

project (Spice)

if (NOT SPICE_ROOT_DIR)
    set(SPICE_ROOT_DIR ${PROJECT_SOURCE_DIR})
endif ()

file(GLOB cspice_SRC
    "src/cspice/*.c"
)
file(GLOB csupport_SRC
    "src/csupport/*.c"
)

include_directories ("${SPICE_ROOT_DIR}/include")

if(NOT UNIX)
	add_definitions( -DKR_headers )
endif(NOT UNIX)

if(MSVC)
    add_definitions( -DMSDOS  )
elseif(APPLE)
    add_definitions( -Wno-return-type  )
endif(MSVC)


add_definitions( -w )

#add_library( Spice ${cspice_SRC} ${csupport_SRC}  )
add_library( Spice STATIC ${cspice_SRC} )


#SET_TARGET_PROPERTIES(cspice PROPERTIES LINKER_LANGUAGE C)
#SET_TARGET_PROPERTIES(csupport PROPERTIES LINKER_LANGUAGE C)
