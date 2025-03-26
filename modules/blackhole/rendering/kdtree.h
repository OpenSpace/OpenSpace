#ifndef __OPENSPACE_MODULE_BLACKHOLE___KDTREE___H__
#define __OPENSPACE_MODULE_BLACKHOLE___KDTREE___H__
#include <glm/glm.hpp>
#include <vector>
#include <string>
#include <limits>

namespace openspace {
    namespace kdtree {}
    class KDTree {
    public:
        KDTree() {};

        KDTree(std::string const& filePath, glm::vec3 const& localWorldCenter, float const renderDistance = std::numeric_limits<float>::max());

        size_t size() { return flatTree.size(); };

        void* data() {
            return flatTree.data();
        }

        void build(
            std::string const& constfilePath,
            glm::vec3 const&  localWorldCenter,
            float const renderDistance = std::numeric_limits<float>::max()
        );

    private:
        struct Node {
            glm::fvec3 position{};
            float color{};
            float lum{};
            float absMag{};
        };
        std::vector<float> flattenTree(std::vector<Node> const& tree) const;
        std::vector<float> flatTree{};
    };
}

#endif
