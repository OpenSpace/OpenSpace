
#include <iostream>
#include <regex>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/filesystem/directory.h>

using Directory = ghoul::filesystem::Directory;
using Recursive = ghoul::filesystem::Directory::Recursive;
using Sort = ghoul::filesystem::Directory::Sort;

namespace openspace::dataloader::helpers {

namespace {
    constexpr const char* _loggerCat = "Helper";
} 

std::string getDirLeaf(std::string dir) {
    std::regex dirLeaf_regex("([^/]+)/?$");
    std::smatch dirLeaf_match;

    if (std::regex_search(dir, dirLeaf_match, dirLeaf_regex)) {
        return dirLeaf_match[0].str();
    } else {
        LWARNING("Found no match in " + dir + ".");
    }
}

std::string findStateFile(std::string absPathToItem) {
    Directory itemDirectory(absPathToItem);

    std::vector<std::string> itemFiles = itemDirectory.readFiles(Recursive::No, Sort::No);
    std::string stateFile = "";

    // Find (first) file with a .state extension
    std::regex stateExtRegex("^.*\.(state)$");
    std::smatch stateMatch;
    for (auto file : itemFiles) {
        LINFO("searching " + file + " in " + absPathToItem);
        if (std::regex_search(file, stateMatch, stateExtRegex)) {
            stateFile = file;
            break;
        }
    }

    ghoul_assert(!stateFile.empty(), "Couldn't find a .state file in " + absPathToItem);

    return stateFile;
}

}
