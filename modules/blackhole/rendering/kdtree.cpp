#include "kdtree.h"
#include <ghoul/filesystem/filesystem.h>
#include <openspace/data/dataloader.h>
#include <algorithm>

#include <queue>

namespace {
    glm::vec3 cartesianToSpherical(const glm::vec3& cartesian) {
        float radius = glm::length(cartesian);
        float theta = std::atan2(glm::sqrt(cartesian.x * cartesian.x + cartesian.y * cartesian.y), cartesian.z);
        float phi = std::atan2(cartesian.y, cartesian.x);

        return glm::vec3(radius, theta, phi);
    }
}
namespace openspace {
    void KDTree::build(const std::string& filePath, const glm::vec3& localWorldCenter) {
        const std::filesystem::path file{ absPath(filePath) };
        dataloader::Dataset dataset = dataloader::data::loadFileWithCache(file);
        size_t numberOfStars =  dataset.entries.size();
        tree.resize(numberOfStars);
        struct NodeInfo {
            size_t index;
            size_t depth;
            size_t start;
            size_t end;
        };

        #pragma omp parallel for
        for (auto& entry : dataset.entries) {
            entry.position = cartesianToSpherical(entry.position - localWorldCenter);
        }

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
            float const color{ entry.data[0]};
            float const lum{ entry.data[1]};
            float const absMag{ entry.data[2]};

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
    }

    std::vector<float> KDTree::flatTree() const {
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
