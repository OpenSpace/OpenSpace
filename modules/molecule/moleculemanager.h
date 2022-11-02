#pragma once

#include <string_view>

struct md_molecule_t;
struct md_trajectory_i;

// If you want to go full blown cplusplus bananas mode, you can use smart pointers (shared external + weak internal) to keep some type of reference counting going
// and free resources when they are not held by any external pointers anymore. I'm not going to care about that right now -Robin

namespace mol_manager {
    const md_molecule_t* get_molecule(std::string_view path_to_file);
    const md_trajectory_i* get_trajectory(std::string_view path_to_file);
}
