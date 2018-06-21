#include <iostream>

namespace openspace::dataloader::helpers {
    std::string getDirLeaf(std::string dir);

    std::string getFileWithExtensionFromItemFolder(std::string absPathToItem, std::string extension);
}
