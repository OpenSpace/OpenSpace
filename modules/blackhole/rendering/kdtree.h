#ifndef __OPENSPACE_MODULE_BLACKHOLE___KDTREE___H__
#define __OPENSPACE_MODULE_BLACKHOLE___KDTREE___H__
#include <glm/glm.hpp>
#include <vector>
#include <string>

namespace openspace {
    namespace kdtree{}
    class KDTree {
    public:
        KDTree() {};

        size_t size() { return tree.size() * 6; };

        void build(std::string const& filePath, glm::vec3 const & origin);

        std::vector<float> flatTree() const;

    private:
        struct Node {
            glm::fvec3 position;
            float color;
            float lum;
            float absMag;
        };

        std::vector<Node> tree{};
    };
}

#endif
