/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GAIAMISSION___CONSTRUCTOCTREETASK___H__
#define __OPENSPACE_MODULE_GAIAMISSION___CONSTRUCTOCTREETASK___H__

#include <openspace/util/task.h>
#include <modules/gaiamission/rendering/octreemanager.h>

namespace openspace {

namespace documentation { struct Documentation; }

class ConstructOctreeTask : public Task {
public:
    ConstructOctreeTask(const ghoul::Dictionary& dictionary);
    virtual ~ConstructOctreeTask();
    
    std::string description() override;
    void perform(const Task::ProgressCallback& onProgress) override;
    static documentation::Documentation Documentation();

private:
    const int RENDER_VALUES = 8;

    void constructOctreeFromSingleFile(const Task::ProgressCallback& progressCallback);
    void constructOctreeFromFolder(const Task::ProgressCallback& progressCallback);

    /**
     * Checks all defined filter ranges and returns true if any of the corresponding 
     * filterValues are outside of the defined range. 
     * Returns false if value should be inserted into Octree. 
     * <filterValues> are all read filter values in binary file.
     */
    bool checkAllFilters(const std::vector<float>& filterValues);
    
    /**
     * Returns true if star should be filtered away and false if all filters passed.
     * <range> contains ]min, max[ and <filterValue> corresponding value in star. 
     * Star is filtered either if min = max = filterValue or 
     * if filterValue =< min (when min != 0.0) or filterValue >= max (when max != 0.0).
     */
    bool filterStar(const glm::vec2& range, const float& filterValue, 
        const float& normValue =  0.f);
    
    std::string _inFileOrFolderPath;
    std::string _outFileOrFolderPath;
    int _maxDist;
    int _maxStarsPerNode;
    bool _singleFileInput;

    std::shared_ptr<OctreeManager> _octreeManager;
    std::shared_ptr<OctreeManager> _indexOctreeManager;

    // Filter params
    glm::vec2 _posX;
    bool _filterPosX;
    glm::vec2 _posY;
    bool _filterPosY;
    glm::vec2 _posZ;
    bool _filterPosZ;
    glm::vec2 _gMag;
    bool _filterGMag;
    glm::vec2 _bpRp;
    bool _filterBpRp;
    glm::vec2 _velX;
    bool _filterVelX;
    glm::vec2 _velY;
    bool _filterVelY;
    glm::vec2 _velZ;
    bool _filterVelZ;
    glm::vec2 _bpMag;
    bool _filterBpMag;
    glm::vec2 _rpMag;
    bool _filterRpMag;
    glm::vec2 _bpG;
    bool _filterBpG;
    glm::vec2 _gRp;
    bool _filterGRp;
    glm::vec2 _ra;
    bool _filterRa;
    glm::vec2 _raError;
    bool _filterRaError;
    glm::vec2 _dec;
    bool _filterDec;
    glm::vec2 _decError;
    bool _filterDecError;
    glm::vec2 _parallax;
    bool _filterParallax;
    glm::vec2 _parallaxError;
    bool _filterParallaxError;
    glm::vec2 _pmra;
    bool _filterPmra;
    glm::vec2 _pmraError;
    bool _filterPmraError;
    glm::vec2 _pmdec;
    bool _filterPmdec;
    glm::vec2 _pmdecError;
    bool _filterPmdecError;
    glm::vec2 _rv;
    bool _filterRv;
    glm::vec2 _rvError;
    bool _filterRvError;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAIAMISSION___CONSTRUCTOCTREETASK___H__
