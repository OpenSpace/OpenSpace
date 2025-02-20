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

#include <modules/neoviz/tasks/impactcorridortask.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <filesystem>
#include <format>
#include <fstream>
#include <numbers>
#include <stb_image.h>
#include <stb_image_write.h>

namespace {
    constexpr std::string_view _loggerCat = "ImpactCorridorTask";
    constexpr unsigned int NChannels = 4;
    constexpr double DefaultBrushSize = 15;
    constexpr double DefaultBrushSaturation = 100;
    constexpr double DefaultFilterStrength = 1;

    std::vector<std::vector<double>> gaussianFilter(int kernelSize, double filterStrength) {
        ghoul_assert(kernelSize % 2 == 1, "Kernel size must be odd");

        double standardDeviation = filterStrength;
        double sig = 2.0 * standardDeviation * standardDeviation;
        double sum = 0.0;
        std::vector<std::vector<double>> kernel = std::vector<std::vector<double>>(
            kernelSize,
            std::vector<double>(kernelSize, 0.0)
        );

        // Generating a kernelSize x kernelSize kernel
        int halfSize = kernelSize / 2; // Intentionally rounded down
        int distanceSqared = 0;
        for (int x = -halfSize; x <= halfSize; x++) {
            for (int y = -halfSize; y <= halfSize; y++) {
                distanceSqared = x * x + y * y;
                kernel[x + halfSize][y + halfSize] =
                    (exp(-(distanceSqared) / sig)) / (std::numbers::pi * sig);
                sum += kernel[x + halfSize][y + halfSize];
            }
        }

        // No need to normalize the kernel, as that will be done before the image is
        // saved to file anyway

        return kernel;
    }

    struct [[codegen::Dictionary(ImpactCorridorTask)]] Parameters {
        // The name of the asteroid to find possible impacts for.
        std::string asteroidName;

        // Path to the file that stores the impact coordinates to plot on the map image.
        std::string impactFile;

        // Path to the output impact corridor map image file. Need to include the
        // .png extension.
        std::string outputFilename;

        // The width in pixels of the resulting impact corridor map image
        int imageWidth [[codegen::greater(0)]];

        // The height in pixels of the resulting impact corridor map image
        int imageHeight [[codegen::greater(0)]];

        // The size of the brush used to plot impacts to the image. This is also used as
        // the kernel size for the gaussian filter, which means that this value needs to
        // be odd. If not given, then a default value of 15 is used.
        std::optional<int> brushSize;

        // The saturation of paint on the brush used to plot impacts to the image. If not
        // given, then a default value of 100 is used.
        std::optional<int> brushSaturation;

        // The strength of the gaussian filter used to smooth the impact image. If not
        // given, then a default value of 1.0 is used.
        std::optional<double> filterStrength;

        // Path to the a colormap to use for coloring the impact corridor map.
        std::optional<std::string> colorMap;
    };
#include "impactcorridortask_codegen.cpp"
} // namespace

namespace openspace::neoviz {

documentation::Documentation ImpactCorridorTask::documentation() {
    return codegen::doc<Parameters>("neoviz_documentation_impactcorridortask");
}

ImpactCorridorTask::ImpactCorridorTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _asteroidName = p.asteroidName;
    _impactFile = absPath(p.impactFile);
    _outputFilename = absPath(p.outputFilename);
    _imageWidth = p.imageWidth;
    _imageHeight = p.imageHeight;
    _brushSize = p.brushSize.value_or(DefaultBrushSize);
    _brushSaturation = p.brushSaturation.value_or(DefaultBrushSaturation);
    _filterStrength = p.filterStrength.value_or(DefaultFilterStrength);

    if (p.colorMap.has_value()) {
        _hasColorMapFile = true;
        _colorMap = absPath(*p.colorMap);
        ghoul_assert(std::filesystem::exists(_colorMap), "Cannot find color map file");
    }

    ghoul_assert(_brushSize % 2 == 1, "Brush size must be an odd number");
}

std::string ImpactCorridorTask::description() {
    return "Return the NEOviz impact corridor map image for the given input";
}

void ImpactCorridorTask::perform(const Task::ProgressCallback& progressCallback) {
    // Read all impact coordinates from the file
    LINFO("Reading impact file...");
    readImpactFile();

    // Settings for the images
    const unsigned int NumPixels = _imageWidth * _imageHeight;
    const unsigned int Size = NumPixels * NChannels;

    // Create a raw impact image that only have information about impact saturation
    LINFO("Creating raw image...");
    std::vector<double> rawData = rawImpactData(progressCallback, NumPixels);
    auto it = std::max_element(rawData.begin(), rawData.end());
    if (it == rawData.end()) {
        LDEBUG("Could not find max element in raw image");
    }
    double maxRawSaturation = *it;
    progressCallback(0.0);

    // Apply a transfer function to bring color to the image
    if (_hasColorMapFile) {
        LINFO("Processing image...");
        int colorMapWidth = 0;
        int colorMapHeight = 0;
        int colorMapChannels = 0;
        unsigned char* colorMapData = stbi_load(
            _colorMap.string().c_str(),
            &colorMapWidth,
            &colorMapHeight,
            &colorMapChannels,
            0
        );
        ghoul_assert(colorMapHeight != 1, "Color map needs to be 1D");

        progressCallback(0.0);
    }

    // @TODO: Use the night layer image to estimate impact risk due to population density

    // Convert the raw image to a normalized image that can be written to file
    LINFO("Finalizing image...");
    std::vector<GLubyte> image = std::vector<GLubyte>(Size, 0);
    for (unsigned int i = 0, p = 0; p < NumPixels && i < Size; ++p, i += NChannels) {
        // Note that this normalizes all channels equally
        double saturation = rawData[p] / maxRawSaturation;

        // Watch out for overflow
        double clampedSaturation = std::min(std::round(saturation * 255.0), 255.0);

        // Save the normalized saturation to all color channels
        for (int c = 0; c < NChannels; ++c) {
            image[i + c] = static_cast<GLubyte>(clampedSaturation);
        }
        progressCallback(p / static_cast<float>(NumPixels));
    }

    // Create and save the resulting impact map image
    stbi_write_png(
        _outputFilename.string().c_str(),
        _imageWidth,
        _imageHeight,
        NChannels,
        reinterpret_cast<void*>(image.data()),
        0
    );
}

void ImpactCorridorTask::readImpactFile() {
    std::ifstream file(_impactFile);
    if (!file) {
        LERROR(std::format("Could not open file {} to read impacts", _impactFile));
        return;
    }

    // Header
    std::string asteroidName;
    std::string header;
    ghoul::getline(file, asteroidName);
    ghoul::getline(file, header);

    if (!file) {
        LERROR(std::format("Could not read header from impact file {}", _impactFile));
        return;
    }

    // Check that the data match with the current task
    if (asteroidName != _asteroidName) {
        LERROR(std::format(
            "Data and task does not match. Data is for {} while the task is for {}",
            asteroidName, _asteroidName
        ));
        return;
    }

    ImpactCoordinate impact = readImpactCoordinate(file);
    while (file) {
        _impactCoordinates.push_back(impact);
        impact = readImpactCoordinate(file);
    }
}

ImpactCorridorTask::ImpactCoordinate ImpactCorridorTask::readImpactCoordinate(
                                                                      std::ifstream& file)
{
    ImpactCoordinate impact;
    std::string line;

    ghoul::getline(file, line);
    std::stringstream ss(line);

    ss >> impact.id >> impact.latitude >> impact.longitude;
    std::getline(ss >> std::ws, impact.time);

    return impact;
}

int ImpactCorridorTask::pixelIndex(int pixelW, int pixelH, int numChannels, int imageWidth,
                                   int imageHeight)
{
    if (pixelH < 0) {
        // Mirror in the noth pole line
        pixelH = std::abs(pixelH);

        // Mirror in the 0 longitude line (Greenwich)
        int greenwich = static_cast<int>(std::round(imageWidth / 2.0));
        pixelW = greenwich + (greenwich - pixelW);

    }
    else if (pixelH >= imageHeight) {
        // Mirror in the south pole line
        pixelH = imageHeight - (pixelH - imageHeight);

        // Mirror in the 0 longitude line (Greenwich)
        int greenwich = static_cast<int>(std::round(imageWidth / 2.0));
        pixelW = greenwich + (greenwich - pixelW);
    }

    if (pixelW < 0) {
        // Go over to the left side of the international daytime line
        pixelW = imageWidth - std::abs(pixelW);
    }
    else if (pixelW >= imageWidth) {
        // Go over to the right side of the international daytime line
        pixelW = pixelW - imageWidth;
    }

    int pixelIndex = numChannels * pixelW + numChannels * imageWidth * pixelH;
    if (pixelIndex < 0 || pixelIndex >= (imageWidth * imageHeight * numChannels)) {
        throw ghoul::RuntimeError("Pixel index out of bounds");
    }
    return pixelIndex;
}

std::vector<double> ImpactCorridorTask::rawImpactData(
                                           const Task::ProgressCallback& progressCallback,
                                                             const unsigned int numPixels)
{
    // Create a pixel list to then plot the impact coordinates on
    std::vector<double> rawData = std::vector<double>(numPixels, 0.0);

    // Tools to plot the impact coordinates on the pixel list
    const int brushRadius = _brushSize / 2; // Intentionally rounded down

    // Use a gaussian kernel to create a smooth circle around the impact location
    const std::vector<std::vector<double>> filter =
        gaussianFilter(_brushSize, _filterStrength);

    // Plot all impact coordinates on the pixel list
    int impactCounter = 0;
    for (const ImpactCoordinate& impact : _impactCoordinates) {
        // Find the pixel in the texture data list for the impact
        double latitudeNorm = (impact.latitude + 90.0) / 180.0;
        double longitudeNorm = (impact.longitude + 180.0) / 360.0;
        int pixelH = static_cast<int>(std::round(latitudeNorm * _imageHeight));
        int pixelW = static_cast<int>(std::round(longitudeNorm * _imageWidth));

        // Draw a circle around the impact point
        for (int w = -brushRadius; w <= brushRadius; w++) {
            for (int h = -brushRadius; h <= brushRadius; h++) {
                int index = pixelIndex(
                    pixelW + w,
                    pixelH + h,
                    1,
                    _imageWidth,
                    _imageHeight
                );

                // Get the eucledian distance from the center of the brush and calculate
                // the total saturation for this pixel
                /*double distance = std::sqrt(w * w + h * h);
                double saturation =
                    std::max(_brushSaturation * (1.0 - distance / _brushRadius), 0.0);*/

                // Use the gaussian kernel to create a smooth circle around the impact
                double saturation =
                    _brushSaturation * filter[w + brushRadius][h + brushRadius];

                // Add the saturation to the pixel data
                rawData[index] += saturation;
            }
        }

        ++impactCounter;
        progressCallback(impactCounter / static_cast<float>(_impactCoordinates.size()));
    }

    return rawData;
}

} // namespace openspace::neoviz
