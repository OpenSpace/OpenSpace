/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___MATCHINGFIELDLINEHELPER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___MATCHINGFIELDLINEHELPER___H__

 #include <ghoul/glm.h>
 #include <string>
 #include <vector>


namespace openspace {

    class FieldlinesState;

    namespace fls {
        //Simon: Test to see if we can edit our seedpoints
        struct pointsWithEigVec {
            std::vector<glm::vec3> cPoints;
            std::vector<glm::vec3> eigVals;
            std::vector<glm::vec3> eigVecs;
        };

        bool convertCdfToMovingFieldlinesState(FieldlinesState& state, const std::string& cdfPath,
            const std::vector<glm::vec3>& seedMap,
            double manualTimeOffset, const std::string& tracingVar,
            std::vector<std::string>& extraVars, std::vector<std::string>& extraMagVars,
            const size_t nPointsOnPathLine, const size_t nPointsOnFieldLines);


        std::vector<glm::vec3> getDataFromFileIntoVectorvec3f(std::ifstream&);
        pointsWithEigVec getCriticalPoints();
        glm::vec3 moveSeedpointInEigenvectorDirection(const glm::vec3& const pointInSpace, const glm::vec3& const eigenvector, const float& direction);


    } // namespace fls
} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___MOVINGFIELDLINEHELPER___H__

    /*
    Will get the point and eigenvector and then return a position
    moved a small step (epsilon) in the eigenvectors direction
        TODO: get the eigenvector of the points
    */
//glm::vec3 moveSeedpointInEigenvectorDirection(const glm::vec3& const pointInSpace, const glm::vec3& const eigenvector, const float& direction)
//{
//    glm::vec3 step = eigenvector * FLT_EPSILON * direction;
//    glm::vec3 movedPoint = pointInSpace + step;
//
//    return glm::vec3(movedPoint.x, movedPoint.y, movedPoint.z);
//}

/*
    Test function that can be extracted and divided into several functions.
    Used to push the eigenvectors and critpoints into a struct with vectors
    where matching index means they are in cooperation with eachother.
*/
//openspace::fls::pointsWithEigVec getCriticalPoints() {
//    openspace::fls::pointsWithEigVec result;
//    std::ifstream cpFile("C:/Users/HEM/Documents/Liu/Data_Fieldlines/DataProcessing/Processed_data/test_criticalpoints.txt");
//    std::ifstream eigValFile("C:/Users/HEM/Documents/Liu/Data_Fieldlines/DataProcessing/Processed_data/test_eigvalues");
//    std::ifstream eigVecFile("C:/Users/HEM/Documents/Liu/Data_Fieldlines/DataProcessing/Processed_data/test_eigvectors");
//
//    result.cPoints = getDataFromFileIntoVectorvec3f(cpFile);
//    result.eigVals = getDataFromFileIntoVectorvec3f(eigValFile);
//    result.eigVecs = getDataFromFileIntoVectorvec3f(eigVecFile);
//
//    return result;
//}

/*
    Takes a file with three values per row and will return a vector of three
    dimensional points as floats
*/
//std::vector<glm::vec3> getDataFromFileIntoVectorvec3f(std::ifstream& file) {
//    std::vector<glm::vec3> result;
//    std::string line{};
//
//    while (file >> line) {
//        std::string s;
//        std::vector<float> toAssemble;
//        for (int i = 0; i < line.substr().length(); ++i) {
//            s = line.substr(i, line.find(" "));
//            toAssemble.push_back(std::stof(s));
//        }
//        result.push_back({ toAssemble[0], toAssemble[1], toAssemble[2] });
//    }
//
//    return result;
//}

////tracing from a seedpoint and having it as finish point
//ccmc::Fieldline uPerpBPathLine;
//uPerpBPathLine = tracer.unidirectionalTrace(
//    tracingVar,
//    seed.x,
//    seed.y,
//    seed.z,
//    ccmc::Tracer::Direction::REVERSE
//).reverseOrder();
////this code will trace backwards in the magnetic field 
////from a given seedpoint 
////without reverseOrder() the fieldline will move backwards as time porgresses
////forward. With bidirectionalTrace() a flowline can be traced in both directions
////from the given seedpoint.
