
#include <iostream>
#include <regex>

#include <ghoul/logging/logmanager.h>

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
}
