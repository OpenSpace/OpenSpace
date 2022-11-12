#include <md_molecule.h>
#include <md_trajectory.h>
#include <md_util.h>
#include <core/md_allocator.h>
#include "viamd/loader.h"

#include <string>
#include <unordered_map>
#include <ghoul/logging/logmanager.h>

// Really C++? Really?
struct string_hash {
    using is_transparent = void;
    using hash_type = std::hash<std::string_view>;  // just a helper local type

    size_t operator()(std::string_view txt) const   { return hash_type{}(txt); }
    size_t operator()(const std::string& txt) const { return hash_type{}(txt); }
    size_t operator()(const char* txt) const        { return hash_type{}(txt); }
};

static std::unordered_map<std::string, md_molecule_t, string_hash, std::equal_to<>> molecules;
static std::unordered_map<std::string, md_trajectory_i*, string_hash, std::equal_to<>> trajectories;

namespace mol_manager {

constexpr const char* _loggerCat = "MoleculeManager";

const md_molecule_t* get_molecule(std::string_view path_to_file) {

    // @TODO: Remove string when C++ 20 is supported
    std::string path(path_to_file);
    auto entry = molecules.find(path);
    if (entry != molecules.end()) {
        return &entry->second;
    }

    if (path_to_file.empty() || path_to_file == "") {
        return nullptr;
    }

    str_t str = {path_to_file.data(), (int64_t)path_to_file.length()};
    md_molecule_api* api = load::mol::get_api(str);
    if (!api) {
        LERROR("Failed to find appropriate molecule loader api for file");
        return nullptr;
    }

    md_molecule_t mol = {};
    if (!api->init_from_file(&mol, str, default_allocator)) {
        LERROR("Failed to load molecule");
        return nullptr;
    }

    md_util_postprocess_molecule(&mol, default_allocator, MD_UTIL_POSTPROCESS_ALL);

    return &(molecules[path] = mol);
}

const md_trajectory_i* get_trajectory(std::string_view path_to_file) {

    // @TODO: Remove string when C++ 20 is supported
    std::string path(path_to_file);
    auto entry = trajectories.find(path);
    if (entry != trajectories.end()) {
        return entry->second;
    }

    if (path_to_file.empty() || path_to_file == "") {
        return nullptr;
    }

    str_t str = {path_to_file.data(), (int64_t)path_to_file.length()};
    md_trajectory_api* api = load::traj::get_api(str);
    if (!api) {
        LERROR("Failed to find appropriate trajectory loader api for file");
        return nullptr;
    }

    md_trajectory_i* traj = api->create(str, default_allocator);
    if (!traj) {
        LERROR("Failed to load trajectory");
        return nullptr;
    }

    return (trajectories[path] = traj);
}

}
