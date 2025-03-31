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
    StarMaps::StarMaps(std::string const& filePath, glm::vec3 const& localWorldCenter, std::vector<std::pair<float, float>> const& renderSpans) {
        build(filePath, localWorldCenter, renderSpans);
    }

    void StarMaps::build(std::string const& filePath, glm::vec3 const& localWorldCenter,
        std::vector<std::pair<float, float>> const& renderSpans)
    {
        const std::filesystem::path file{ absPath(filePath) };
        dataloader::Dataset dataset = dataloader::data::loadFileWithCache(file);

        // Convert positions to spherical coordinates
#pragma omp parallel for
        for (auto& entry : dataset.entries) {
            entry.position = cartesianToSpherical(entry.position - localWorldCenter);
        }

        size_t numEntries = dataset.entries.size();
        size_t numTrees = renderSpans.size();

        // Preallocate space for KD-tree partitions
        std::vector<std::vector<Node>> kdTrees(numTrees, std::vector<Node>{});
        for (auto& tree : kdTrees) {
            tree.reserve(numEntries / numTrees); // Approximate initial allocation
        }

        // Partition data into the correct KD-tree
        for (const auto& entry : dataset.entries) {
            float radius = entry.position.x;
            for (size_t i = 0; i < numTrees; ++i) {
                if (radius >= renderSpans[i].first && radius < renderSpans[i].second) {
                    kdTrees[i].emplace_back(entry.position, entry.data[0], entry.data[1], entry.data[2]);
                    break;
                }
            }
        }

        // Estimate total storage size for the flat array
        size_t estimatedNodes = 0;
        for (const auto& tree : kdTrees) {
            estimatedNodes += tree.size();
        }
        _flatTrees.reserve(estimatedNodes * 6);

        // Build KD-trees compactly
        _treeStartIndices.reserve(numTrees);

        struct NodeInfo {
            size_t index;
            size_t depth;
            size_t start;
            size_t end;
        };

        for (size_t i = 0; i < kdTrees.size(); ++i) {
            if (kdTrees[i].empty()) continue;
            _treeStartIndices.push_back(_flatTrees.size()); // Mark start index of this KD-tree

            std::queue<NodeInfo> q;
            q.emplace(0, 0, 0, kdTrees[i].size());

            while (!q.empty()) {
                NodeInfo node = q.front();
                q.pop();
                if (node.start >= node.end) continue;

                int axis = node.depth % 2 + 1;
                auto comparator = [axis](const Node& a, const Node& b) {
                    return a.position[axis] < b.position[axis];
                    };

                std::sort(kdTrees[i].begin() + node.start, kdTrees[i].begin() + node.end, comparator);
                size_t medianIndex = (node.start + node.end) / 2;
                const Node& entry = kdTrees[i][medianIndex];

                _flatTrees.push_back(entry.position.x);
                _flatTrees.push_back(entry.position.y);
                _flatTrees.push_back(entry.position.z);
                _flatTrees.push_back(entry.color);
                _flatTrees.push_back(entry.lum);
                _flatTrees.push_back(entry.absMag);

                // Enqueue left and right children
                q.emplace(2 * node.index + 1, node.depth + 1, node.start, medianIndex);
                q.emplace(2 * node.index + 2, node.depth + 1, medianIndex + 1, node.end);
            }
        }
        _flatTrees.shrink_to_fit();
    }
}
