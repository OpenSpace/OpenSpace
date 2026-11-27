include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

set(HEADER_FILES
    rendering/renderablegaussiansplat.h
)

set(SOURCE_FILES
    rendering/renderablegaussiansplat.cpp
)

set(SHADER_FILES
    shaders/splat_vs.glsl
    shaders/splat_fs.glsl
)

create_new_module(
    "GaussianSplatting"
    gaussiansplatting_module
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
