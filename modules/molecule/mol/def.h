#pragma once

namespace mol::rep {

enum class Type {
    SpaceFill,
    Licorice,
    Ribbons,
    Cartoon,
};

enum class Color {
    // Uniform,
    Cpk,
    AtomIndex,
    ResId,
    ResIndex,
    ChainId,
    ChainIndex,
    SecondaryStructure,
    // Property
};

}
