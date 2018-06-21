
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

/**
 * Get first file with the supplied extension in item folder
 */
std::string getFileWithExtensionFromItemFolder(std::string absPathToItem, std::string extension) {
    Directory itemDirectory(absPathToItem);

    std::vector<std::string> itemFiles = itemDirectory.readFiles(Recursive::No, Sort::Yes);
    std::string filePath = "";

    // Find (first) file with the extension
    std::regex extRegex("^.*\.(" + extension + ")$");
    std::smatch match;
    for (auto file : itemFiles) {
        if (std::regex_search(file, match, extRegex)) {
            filePath = file;
            break;
        }
    }

    ghoul_assert(!filePath.empty(), "Couldn't find a file with ." + extension + " extension in " + absPathToItem);

    return filePath;
}

}
