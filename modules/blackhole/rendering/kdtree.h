#ifndef __OPENSPACE_MODULE_BLACKHOLE___KDTREE___H__
#define __OPENSPACE_MODULE_BLACKHOLE___KDTREE___H__
#include <glm/glm.hpp>
#include <vector>
#include <string>
#include <limits>

namespace openspace {
    class StarMaps {
    public:
        StarMaps() {};

        StarMaps(std::string const& filePath, glm::dvec3 const& localWorldCenter, std::vector<std::pair<float, float>> const& renderSpans = {});

        size_t mapsSize() { return _flatTrees.size(); };

        void* mapsData() {
            return _flatTrees.data();
        }

        size_t indicesSize() { return _treeStartIndices.size(); };

        void* indicesData() {
            return _treeStartIndices.data();
        }

        void build(
            std::string const& constfilePath,
            glm::dvec3 const&  localWorldCenter,
            std::vector<std::pair<float, float>> const& renderSpans = {}
        );

    private:
        struct Node {
            glm::fvec3 position{};
            float color{};
            float lum{};
            float absMag{};
        };
        std::vector<float> _flatTrees{};
        std::vector<int> _treeStartIndices;
    };
}

#endif
