#include <iostream>

namespace openspace::dataloader::helpers {
    std::string getDirLeaf(std::string dir);

    std::string getFileWithExtensionFromItemFolder(std::string absPathToItem, std::string extension);

    std::string getFileBaseName(std::string absPathToFile);

    void replaceDoubleBackslashesWithForward(std::string &path);
}
