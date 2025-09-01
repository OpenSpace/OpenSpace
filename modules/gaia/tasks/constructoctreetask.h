/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_MODULE_GAIA___CONSTRUCTOCTREETASK___H__
#define __OPENSPACE_MODULE_GAIA___CONSTRUCTOCTREETASK___H__

#include <openspace/util/task.h>

#include <modules/gaia/rendering/octreeculler.h>
#include <modules/gaia/rendering/octreemanager.h>
#include <filesystem>

namespace openspace {

namespace documentation { struct Documentation; }

class ConstructOctreeTask : public Task {
public:
    explicit ConstructOctreeTask(const ghoul::Dictionary& dictionary);
    ~ConstructOctreeTask() override = default;

    std::string description() override;
    void perform(const Task::ProgressCallback& onProgress) override;
    static documentation::Documentation Documentation();

private:
    const int RENDER_VALUES = 8;

    /**
     * Reads a single binary file with preprocessed star data and insert the render values
     * into an octree structure (if star data passed all defined filters). Stores the
     * entire octree in one binary file.
     */
    void constructOctreeFromSingleFile(const Task::ProgressCallback& progressCallback);

    /**
     * Reads binary star data from 8 preprocessed files (one per branch) in specified
     * folder, prepared by ReadFitsTask, and inserts star render data into an octree (if
     * star data passed all defined filters). Stores octree structure in a binary index
     * file and stores all render data separate files, one file per node in the octree.
     */
    void constructOctreeFromFolder(const Task::ProgressCallback& progressCallback);

    /**
     * Checks all defined filter ranges and returns true if any of the corresponding
     * \p filterValues are outside of the defined range.
     *
     * \param filterValues Are all read filter values in binary file
     *
     * \return `false` if value should be inserted into Octree
     */
    bool checkAllFilters(const std::vector<float>& filterValues);

    /**
     * \p range contains ]min, max[ and \p filterValue corresponding value in star. Star
     * is filtered either if `min = max = filterValue` or if `filterValue < min` (when
     * `min != 0.0`) or `filterValue > max` (when `max != 0.0`).
     *
     * \return `true` if star should be filtered away and `false` if all filters passed
     */
    bool filterStar(const glm::vec2& range, float filterValue, float normValue = 0.f);

    std::filesystem::path _inFileOrFolderPath;
    std::filesystem::path _outFileOrFolderPath;
    int _maxDist = 0;
    int _maxStarsPerNode = 0;
    bool _singleFileInput = false;

    std::shared_ptr<OctreeManager> _octreeManager;
    std::shared_ptr<OctreeManager> _indexOctreeManager;

    // Filter params
    glm::vec2 _posX = glm::vec2(0.f);
    bool _filterPosX = false;
    glm::vec2 _posY = glm::vec2(0.f);
    bool _filterPosY = false;
    glm::vec2 _posZ = glm::vec2(0.f);
    bool _filterPosZ = false;
    glm::vec2 _gMag = glm::vec2(0.f);
    bool _filterGMag = false;
    glm::vec2 _bpRp = glm::vec2(0.f);
    bool _filterBpRp = false;
    glm::vec2 _velX = glm::vec2(0.f);
    bool _filterVelX = false;
    glm::vec2 _velY = glm::vec2(0.f);
    bool _filterVelY = false;
    glm::vec2 _velZ = glm::vec2(0.f);
    bool _filterVelZ = false;
    glm::vec2 _bpMag = glm::vec2(0.f);
    bool _filterBpMag = false;
    glm::vec2 _rpMag = glm::vec2(0.f);
    bool _filterRpMag = false;
    glm::vec2 _bpG = glm::vec2(0.f);
    bool _filterBpG = false;
    glm::vec2 _gRp = glm::vec2(0.f);
    bool _filterGRp = false;
    glm::vec2 _ra = glm::vec2(0.f);
    bool _filterRa = false;
    glm::vec2 _raError = glm::vec2(0.f);
    bool _filterRaError = false;
    glm::vec2 _dec = glm::vec2(0.f);
    bool _filterDec = false;
    glm::vec2 _decError = glm::vec2(0.f);
    bool _filterDecError = false;
    glm::vec2 _parallax = glm::vec2(0.f);
    bool _filterParallax = false;
    glm::vec2 _parallaxError = glm::vec2(0.f);
    bool _filterParallaxError = false;
    glm::vec2 _pmra = glm::vec2(0.f);
    bool _filterPmra = false;
    glm::vec2 _pmraError = glm::vec2(0.f);
    bool _filterPmraError = false;
    glm::vec2 _pmdec = glm::vec2(0.f);
    bool _filterPmdec = false;
    glm::vec2 _pmdecError = glm::vec2(0.f);
    bool _filterPmdecError = false;
    glm::vec2 _rv = glm::vec2(0.f);
    bool _filterRv = false;
    glm::vec2 _rvError = glm::vec2(0.f);
    bool _filterRvError = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIA___CONSTRUCTOCTREETASK___H__
