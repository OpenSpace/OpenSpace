/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_TOUCH___DIRECTINPUT_SOLVER___H__
#define __OPENSPACE_MODULE_TOUCH___DIRECTINPUT_SOLVER___H__


#include <modules/touch/ext/levmarq.h>

#include <modules/touch/ext/libTUIO11/TUIO/TuioCursor.h>

#include <vector>


namespace openspace {

class Camera;
class SceneGraphNode;

// Stores the selected node, the cursor ID as well as the surface coordinates the
// cursor touched
struct SelectedBody {
    long id;
    SceneGraphNode* node;
    glm::dvec3 coordinates;
};

class DirectInputSolver {
public:
    DirectInputSolver();
    bool solve(const std::vector<TUIO::TuioCursor>& list,
            const std::vector<SelectedBody>& selectedBodies,
            std::vector<double>* calculatedValues,
            const Camera& camera);
    int getNDof() { return _nDof; }

    const LMstat& getLevMarqStat() { return _lmstat; }
    void setLevMarqVerbosity(bool verbose) { _lmstat.verbose = verbose; }

private:
    int _nDof;
    LMstat _lmstat;
};



} // openspace namespace

#endif // __OPENSPACE_MODULE_TOUCH___DIRECTINPUT_SOLVER___H__

