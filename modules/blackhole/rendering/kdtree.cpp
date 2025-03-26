#include "kdtree.h"
#include <ghoul/filesystem/filesystem.h>
#include <openspace/data/dataloader.h>
#include <algorithm>

#include <queue>

namespace {
    glm::vec3 cartesianToSpherical(glm::vec3 const& cartesian) {
        float radius = glm::length(cartesian);
        float theta = std::atan2(glm::sqrt(cartesian.x * cartesian.x + cartesian.y * cartesian.y), cartesian.z);
        float phi = std::atan2(cartesian.y, cartesian.x);

        return glm::vec3(radius, theta, phi);
    }
}
namespace openspace {
    KDTree::KDTree(std::string const& filePath, glm::vec3 const& localWorldCenter, float const renderDistance) {
        build(filePath, localWorldCenter, renderDistance);
    }

    void KDTree::build(std::string const&  filePath, glm::vec3 const& localWorldCenter, float const renderDistance)
    {
        std::vector<Node> tree{};
        const std::filesystem::path file{ absPath(filePath) };
        dataloader::Dataset dataset = dataloader::data::loadFileWithCache(file);


#pragma omp parallel for
        for (auto& entry : dataset.entries) {
            entry.position = cartesianToSpherical(entry.position - localWorldCenter);
        }

        std::erase_if(dataset.entries, [renderDistance](const dataloader::Dataset::Entry& e) {
            return e.position.x > renderDistance;
            });

        size_t numberOfStars = dataset.entries.size();
        tree.resize(numberOfStars);
        struct NodeInfo {
            size_t index;
            size_t depth;
            size_t start;
            size_t end;
        };

        std::queue<NodeInfo> q;
        q.emplace(0, 0, 0, numberOfStars);
        while (!q.empty()) {
            NodeInfo node{ q.front() };
            q.pop();

            if (node.start >= node.end) continue;

            int axis = node.depth % 2 + 1;
            auto comparetor = [axis](dataloader::Dataset::Entry const& a, dataloader::Dataset::Entry const& b) -> bool {
                return a.position[axis] < b.position[axis];
                };

            // Sort to find median
            std::sort(
                dataset.entries.begin() + node.start,
                dataset.entries.begin() + node.end,
                comparetor
            );
            size_t medianIndex{ (node.start + node.end) / 2 };

            dataloader::Dataset::Entry const& entry{ dataset.entries[medianIndex] };
            glm::vec3 const& position{ entry.position };
            float const color{ entry.data[0] };
            float const lum{ entry.data[1] };
            float const absMag{ entry.data[2] };

            if (node.index >= tree.size()) {
                tree.resize(std::max(tree.size() * 2, node.index + 1));
            }
            tree.emplace(
                tree.begin() + node.index, position, color, lum, absMag
            );

            // Enqueue left and right children
            q.emplace(2 * node.index + 1, node.depth + 1, node.start, medianIndex);
            q.emplace(2 * node.index + 2, node.depth + 1, medianIndex + 1, node.end);
        }
        flatTree = flattenTree(tree);
    }

    std::vector<float> KDTree::flattenTree(std::vector<Node> const& tree) const {
        std::vector<float> flatData;
        flatData.resize(tree.size() * 6);

        for (int i = 0; i < tree.size(); i++) {
            Node const& node{ tree[i] };

            size_t index = i * 6;
            flatData[index] = node.position.x;
            flatData[index + 1] = node.position.y;
            flatData[index + 2] = node.position.z;
            flatData[index + 3] = node.color;
            flatData[index + 4] = node.lum;
            flatData[index + 5] = node.absMag;
        }

        return flatData;
    }
}
