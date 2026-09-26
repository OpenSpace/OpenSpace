/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

 /****************************************************************************************
  * Modified parts of the code from Ron Babich is used in the following code.            *
  ***************************************************************************************/

 /**
  * levmarq.c, levmarq.h, and examples are provided under the MIT license.
  *
  * Copyright (c) 2008-2016 Ron Babich
  *
  * Permission is hereby granted, free of charge, to any person
  * obtaining a copy of this software and associated documentation
  * files (the "Software"), to deal in the Software without
  * restriction, including without limitation the rights to use,
  * copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the
  * Software is furnished to do so, subject to the following
  * conditions:
  *
  * The above copyright notice and this permission notice shall be
  * included in all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  * OTHER DEALINGS IN THE SOFTWARE.
  */

#ifndef __GHOUL___LEVMARQ_SOLVER___H__
#define __GHOUL___LEVMARQ_SOLVER___H__

#include <ghoul/glm.h>
#include <string>
#include <vector>

namespace ghoul {

typedef struct {
    bool verbose;
    std::string data;
    std::vector<glm::dvec2> pos;
    int maxIt;
    double initLambda;
    double upFactor;
    double downFactor;
    double targetDerr;
    int finalIt;
    double finalErr;
    double finalDerr;
} LMstat;

/**
 * Initialize parameters to good values.
 */
void initializeLevmarqStats(LMstat* lmstat);

/**
 * Main function call, finds appropriate values for par[nDOF] that manipulates the camera
 * correctly to direct-manipulation.
 */
bool levmarq(int npar, double* par, int ny, double* dysq,
    double (*func)(double*, int, void*, LMstat*),
    void (*grad)(double*, double*, int, void*, LMstat*),
    void* fdata, LMstat* lmstat);

} // namespace ghoul

#endif // __GHOUL___LEVMARQ_SOLVER___H__
