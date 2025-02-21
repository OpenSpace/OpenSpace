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

#include <openspace/data/dataloader.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
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
                double value = (exp(-(distanceSqared) / sig)) / (std::numbers::pi * sig);
                kernel[x + halfSize][y + halfSize] = value;

                if (x == 0 && y == 0) {
                    sum = value;
                }
                //sum += kernel[x + halfSize][y + halfSize];
            }
        }

        // Normalize so the middle of the kernel is 1
        for (int i = 0; i < kernelSize; ++i) {
            for (int j = 0; j < kernelSize; ++j) {
                kernel[i][j] /= sum;
            }
        }

        return kernel;
    }

    struct [[codegen::Dictionary(ImpactCorridorTask)]] Parameters {
        // The name of the asteroid to find possible impacts for.
        std::string asteroidName;

        // Path to the file that stores the impact coordinates to plot on the map image.
        std::string impactFile;

        // Path to the output impact corridor map image file. Cannot include the .png
        // extension.
        std::string outputFilename;

        // The width in pixels of the resulting impact corridor map image
        int imageWidth [[codegen::greater(0)]];

        // The height in pixels of the resulting impact corridor map image
        int imageHeight [[codegen::greater(0)]];

        // Path to the a colormap to use for coloring the impact corridor map.
        std::string colorMap;

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

        // Whether or not to invert the color map. If not given, then the color map is not
        // inverted.
        std::optional<bool> invertColorMap;
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
    _colorMap = absPath(p.colorMap);
    _brushSize = p.brushSize.value_or(DefaultBrushSize);
    _brushSaturation = p.brushSaturation.value_or(DefaultBrushSaturation);
    _filterStrength = p.filterStrength.value_or(DefaultFilterStrength);
    _invertColorMap = p.invertColorMap.value_or(false);

    if (!std::filesystem::exists(_colorMap)) {
        throw ghoul::RuntimeError(
            std::format("Cannot find color map file {}", _colorMap)
        );
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
    std::vector<glm::vec4> pixels = std::vector<glm::vec4>(NumPixels, glm::vec4(0.0));

    // Read the color map
    openspace::dataloader::ColorMap colorMap = dataloader::color::loadFile(_colorMap);

    // Create a raw impact data list that only have information about impact probability
    LINFO("Creating raw pixel data...");
    progressCallback(0.0);
    std::vector<double> rawData = rawImpactData(progressCallback, NumPixels);
    auto it = std::max_element(rawData.begin(), rawData.end());
    if (it == rawData.end() || std::abs(*it) < std::numeric_limits<double>::epsilon()) {
        LDEBUG("Could not find max element in raw data");
    }
    double maxRawSaturation = *it;

    // Create an impact corridor image with impact probability as the data
    LINFO("Processing impact probability image...");
    progressCallback(0.0);

    // Apply the transfer function for each pixel and store the resulting color
    for (int p = 0; p < NumPixels; ++p) {
        double normalizedValue = rawData[p] / maxRawSaturation;
        if (std::abs(normalizedValue) < std::numeric_limits<double>::epsilon()) {
            progressCallback((p + 1) / static_cast<float>(NumPixels));
            continue;
        }

        // Find the index in the color map that cooresponds to the value
        int colorMapIndex = static_cast<int>(std::round(
            normalizedValue * (colorMap.entries.size() - 1)
        ));

        // Invert the color map if needed
        if (_invertColorMap) {
            colorMapIndex = (colorMap.entries.size() - 1) - colorMapIndex;
        }

        // Get and save the color that this value cooresponds to in the color map
        glm::vec4 color = colorMap.entries[colorMapIndex];
        color.a = static_cast<float>(normalizedValue);
        pixels[p] = color;

        progressCallback((p + 1)/ static_cast<float>(NumPixels));
    }

    // Write the impact image to file
    writeFinalImage(
        progressCallback,
        _outputFilename.string() + "-probability.png",
        NumPixels,
        Size,
        pixels
    );

    // @TODO: Use the night layer image to estimate impact risk due to population density
    // and color the image based on that instead of impact proability

    // @TODO: Use the color map to color the time of impact difference instead of impact
    // probability
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
                                   int imageHeight, bool allowWrap)
{
    if (allowWrap) {
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
                    _imageHeight,
                    true
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

void ImpactCorridorTask::writeFinalImage(const Task::ProgressCallback& progressCallback,
                                         const std::string& filename,
                                         unsigned int numPixels, unsigned int size,
                                         const std::vector<glm::vec4>& pixels)
{
    LINFO("Finalizing image...");
    progressCallback(0.0);
    std::vector<GLubyte> image = std::vector<GLubyte>(size, 0);

    for (int p = 0, i = 0; p < numPixels && i < size; ++p, i += NChannels) {
        glm::vec4 color = pixels[p];

        for (int c = 0; c < NChannels; ++c) {
            // Watch out for overflow
            double clampedSaturation = std::min(std::round(color[c] * 255.0), 255.0);
            image[i + c] = static_cast<GLubyte>(clampedSaturation);
        }
        progressCallback((p + 1) / static_cast<float>(numPixels));
    }

    LINFO("Saving image to file...");
    int res = stbi_write_png(
        filename.c_str(),
        _imageWidth,
        _imageHeight,
        NChannels,
        reinterpret_cast<void*>(image.data()),
        0
    );

    if (!res) {
        LERROR(std::format("Could not save image {}", filename));
    }
}

} // namespace openspace::neoviz
