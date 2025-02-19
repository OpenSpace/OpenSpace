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

#ifndef __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__
#define __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__

#include <openspace/util/task.h>

#include <openspace/rendering/colormappingcomponent.h>
#include <filesystem>
#include <string>

namespace openspace::neoviz {

class ImpactCorridorTask : public Task {
public:
    explicit ImpactCorridorTask(const ghoul::Dictionary& dictionary);

    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;

    static documentation::Documentation documentation();

private:
    struct ImpactCoordinate {
        int id = 0;
        std::string time;
        double latitude = 0.0;  // Degrees
        double longitude = 0.0;
    };

    /**
     * Use Spice to find any impacts and their locations on Earth for all variants. Any
     * impact found is added into the _impactCoordinates list.
     *
     * \param progressCallback To comunicate progress amount
     * \param nVariants The total number of variants to process
     */
    void findImpacts(const Task::ProgressCallback& progressCallback, int nVariants);

    /**
     * Retrieve the index in the flat data list of pixels that cooresponds to a given
     * pixel coordinate. If the given pixel coordinate is outside the bounds of the image
     * then it will be properly wrapped/mirrored around the image considering that it
     * represents a map. If the pixel croses the international day time line then it will
     * be wrapped around to the other side. If instead the pixel crosses the pole, the it
     * will be mirrored around Greenwich.
     *
     * \param pixelW The horizontal coordinate for the pixel in the image
     * \param pixelH The vertical coordinate for the pixel in the image
     * \param imageWidth The total width of the image in pixels
     * \param imageHeight The total height of the image in pixels
     * \return The index of the first channel of the given pixel in the flat data list
     */
    int pixelIndex(int pixelW, int pixelH, int imageWidth, int imageHeight);

    /**
     * Create a raw impact map image data list that only have information with impact
     * probability. Each impact is painted onto the image with a guasian blob, and
     * overlapping blobs are added together.
     *
     * \param progressCallback To comunicate progress amount
     * \param Size The vertical coordinate for the pixel in the image
     * \return The raw impact map with impact probability as a flat data list
     */
    std::vector<double> rawImpactImage(const Task::ProgressCallback& progressCallback,
        const unsigned int Size);

    std::filesystem::path _kernelDirectory;
    std::filesystem::path _outputFilename;
    int _impactDistance;
    double _stepSize;
    std::string _timeIntervalStart;
    std::string _timeIntervalEnd;
    std::vector<ImpactCoordinate> _impactCoordinates;
    int _imageWidth;
    int _imageHeight;
    int _brushSize;
    int _brushSaturation;
    double _filterStrength;
    std::unique_ptr<ColorMappingComponent> colorMapping;
};

} // namespace openspace::neoviz

#endif // __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__
