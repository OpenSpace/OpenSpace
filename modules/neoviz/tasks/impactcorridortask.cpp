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
    constexpr std::string_view TimeStringFormat = "YYYY MON DD HR:MN:SC.###";
    constexpr unsigned int TimeStringLength = 41;

    std::vector<std::vector<double>> gaussianKernel(int kernelSize,
                                                    double standardDeviation)
    {
        ghoul_assert(kernelSize % 2 == 1, "Kernel size must be odd");

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

    glm::vec3 rgbToHsl(glm::vec3 rgbColor) {
        float max = std::max(std::max(rgbColor.r, rgbColor.g), rgbColor.b);
        float min = std::min(std::min(rgbColor.r, rgbColor.g), rgbColor.b);

        float L = (max + min) / 2.f;

        float S = 0.f;
        if (std::abs(max - min) > std::numeric_limits<float>::epsilon()) {
            if (L <= 0.5f) {
                S = (max - min) / (max + min);
            }
            else {
                S = (max - min) / (2.f - max - min);
            }
        }

        float H = 0.f;
        if (S > 0.f) {
            if (rgbColor.r > rgbColor.g && rgbColor.r > rgbColor.b) {
                // Red is max
                H = (rgbColor.g - rgbColor.b) / (max - min);
            }
            else if (rgbColor.g > rgbColor.r && rgbColor.g > rgbColor.b) {
                // Green is max
                H = 2.f + (rgbColor.b - rgbColor.r) / (max - min);
            }
            else {
                // Blue is max
                H = 4.f + (rgbColor.r - rgbColor.g) / (max - min);
            }

            H *= 60.f;
            if (H < 0.f) {
                H += 360.f;
            }
        }

        return glm::vec3(H, S, L);
    }

    glm::vec3 hslToRgb(glm::vec3 hslColor) {
        if (hslColor.y < std::numeric_limits<float>::epsilon()) {
            return glm::vec3(hslColor.z);
        }
        else if (hslColor.y > 1.f) {
            hslColor.y = 1.f;
        }

        float temp1 = 0.f;
        if (hslColor.z < 0.5f) {
            temp1 = hslColor.z * (1.f + hslColor.y);
        }
        else {
            temp1 = hslColor.z + hslColor.y - hslColor.z * hslColor.y;
        }

        float temp2 = 2.f * hslColor.z - temp1;
        hslColor.x /= 360.f;

        glm::vec3 rgbColor = glm::vec3(0.f);
        rgbColor.r = hslColor.x + 1.f / 3.f;
        rgbColor.g = hslColor.x;
        rgbColor.b = hslColor.x - 1.f / 3.f;

        if (rgbColor.r < 0.f) {
            rgbColor.r += 1.f;
        }
        else if (rgbColor.g < 0.f) {
            rgbColor.g += 1.f;
        }
        else if (rgbColor.b < 0.f) {
            rgbColor.b += 1.f;
        }

        if (rgbColor.r > 1.f) {
            rgbColor.r -= 1.f;
        }
        else if (rgbColor.g > 1.f) {
            rgbColor.g -= 1.f;
        }
        else if (rgbColor.b > 1.f) {
            rgbColor.b -= 1.f;
        }

        for (int i = 0; i < 3; ++i) {
            if (6.f * rgbColor[i] < 1.f) {
                rgbColor[i] = temp2 + (temp1 - temp2) * 6.f * rgbColor[i];
            }
            else if (2.f * rgbColor[i] < 1.f) {
                rgbColor[i] = temp1;
            }
            else if (3.f * rgbColor[i] < 2.f) {
                rgbColor[i] = temp2 + (temp1 - temp2) * (2.f / 3.f - rgbColor[i]) * 6.f;
            }
            else {
                rgbColor[i] = temp2;
            }
        }

        return rgbColor;
    }

    int convertLatitude(double latitude, int imageHeight) {
        double latitudeNorm = (latitude + 90.0) / 180.0;
        return static_cast<int>(std::round(latitudeNorm * imageHeight));
    }

    int convertLongitude(double longitude, int imageWidth) {
        double longitudeNorm = (longitude + 180.0) / 360.0;
        return static_cast<int>(std::round(longitudeNorm * imageWidth));
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

        // Path to the a colormap to use for coloring the time componend of the impact
        // corridor map. This is the lower part of the color map that will be combined
        // with the upper part to create a full color map.
        std::string timeLowerColorMap;

        // Path to the a colormap to use for coloring the time componend of the impact
        // corridor map. This is the upper part of the color map that will be combined
        // with the lower part to create a full color map.
        std::string timeUpperColorMap;

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

        // Path to the night layer map, used to do a risk analysis based on population
        std::string nightMap;
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
    if (!std::filesystem::exists(_colorMap)) {
        throw ghoul::RuntimeError(
            std::format("Cannot find color map file {}", _colorMap)
        );
    }
    _timeLowerColorMap = absPath(p.timeLowerColorMap);
    if (!std::filesystem::exists(_timeLowerColorMap)) {
        throw ghoul::RuntimeError(
            std::format("Cannot find color map file {}", _timeLowerColorMap)
        );
    }
    _timeUpperColorMap = absPath(p.timeUpperColorMap);
    if (!std::filesystem::exists(_timeUpperColorMap)) {
        throw ghoul::RuntimeError(
            std::format("Cannot find color map file {}", _timeUpperColorMap)
        );
    }

    _brushSize = p.brushSize.value_or(DefaultBrushSize);
    if (_brushSize % 2 == 0) {
        throw ghoul::RuntimeError("Brush size must be an odd number");
    }

    _brushSaturation = p.brushSaturation.value_or(DefaultBrushSaturation);
    _filterStrength = p.filterStrength.value_or(DefaultFilterStrength);
    _invertColorMap = p.invertColorMap.value_or(false);

    _nightMap = absPath(p.nightMap);
    if (!std::filesystem::exists(_nightMap)) {
        throw ghoul::RuntimeError(
            std::format("Cannot find night map file {}", _nightMap)
        );
    }
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

    // Read the color maps
    openspace::dataloader::ColorMap colorMap = dataloader::color::loadFile(_colorMap);
    openspace::dataloader::ColorMap timeLowerColorMap =
        dataloader::color::loadFile(_timeLowerColorMap);
    openspace::dataloader::ColorMap timeUpperColorMap =
        dataloader::color::loadFile(_timeUpperColorMap);

    // Read the night layer map
    int nightWidth = 0;
    int nightHeight = 0;
    int nightChannels = 0;
    unsigned char* nightImageData = stbi_load(
        _nightMap.string().c_str(),
        &nightWidth,
        &nightHeight,
        &nightChannels,
        0
    );
    if (!nightImageData) {
        throw ghoul::RuntimeError(std::format(
            "Error: {} reading night map {}", stbi_failure_reason(), _nightMap
        ));
    }

    // Plot all impacts on a map and calculate all data related to it, stor ethis in a
    // flat list of data for each pixel
    LINFO("Creating pixel data...");
    progressCallback(0.0);
    std::vector<ImpactPixel> data = plotImpactData(
        progressCallback,
        NumPixels,
        nightImageData,
        nightWidth,
        nightHeight,
        nightChannels
    );
    stbi_image_free(nightImageData);

    // Create the impact corridor image with impact probability as the data
    std::vector<glm::vec4> pixels = std::vector<glm::vec4>(NumPixels, glm::vec4(0.0));
    processImage(
        progressCallback,
        DataType::Probability,
        data,
        NumPixels,
        Size,
        colorMap,
        timeLowerColorMap,
        timeUpperColorMap,
        pixels
    );


    // and color the image based on that instead of impact proability
    // Clear all previous data from the pixel list
    pixels = std::vector<glm::vec4>(NumPixels, glm::vec4(0.0));
    processImage(
        progressCallback,
        DataType::Risk,
        data,
        NumPixels,
        Size,
        colorMap,
        timeLowerColorMap,
        timeUpperColorMap,
        pixels
    );

    // Use the color map to color the time of impact difference
    pixels = std::vector<glm::vec4>(NumPixels, glm::vec4(0.0));
    processImage(
        progressCallback,
        DataType::Time,
        data,
        NumPixels,
        Size,
        colorMap,
        timeLowerColorMap,
        timeUpperColorMap,
        pixels
    );

    // Debug
    pixels = std::vector<glm::vec4>(NumPixels, glm::vec4(0.0));
    processImage(
        progressCallback,
        DataType::Debug,
        data,
        NumPixels,
        Size,
        colorMap,
        timeLowerColorMap,
        timeUpperColorMap,
        pixels
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
    while (file && impact.id > 0) {
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
    if (line.empty()) {
        return impact;
    }

    std::stringstream ss(line);
    ss >> impact.id >> impact.latitude >> impact.longitude;
    std::getline(ss >> std::ws, impact.time);

    return impact;
}

int ImpactCorridorTask::pixelIndex(int pixelW, int pixelH, int numChannels,
                                   int imageWidth, int imageHeight,
                                   bool allowWrap)
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

std::vector<ImpactCorridorTask::ImpactPixel> ImpactCorridorTask::plotImpactData(
                                           const Task::ProgressCallback& progressCallback,
                                                                     unsigned int nPixels,
                                                           const unsigned char* nightData,
                                                                           int nightWidth,
                                                                          int nightHeight,
                                                                        int nightChannels)
{
    std::vector<ImpactPixel> data = std::vector<ImpactPixel>(nPixels);
    const int brushRadius = _brushSize / 2; // Intentionally rounded down

    // Use a gaussian kernel to create a smooth circle around the impact location
    const std::vector<std::vector<double>> kernel =
        gaussianKernel(_brushSize, _filterStrength);

    // Plot all impact coordinates on the pixel list
    int impactCounter = 0;
    for (const ImpactCoordinate& impact : _impactCoordinates) {
        // Find the pixel in the texture data list for the impact
        int pixelH = convertLatitude(impact.latitude, _imageHeight);
        int pixelW = convertLongitude(impact.longitude, _imageWidth);

        // Convert the time to a J2000 time
        double time = SpiceManager::ref().ephemerisTimeFromDate(impact.time);

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
                data[index].hasImpact = true;

                // Use the gaussian kernel to create a smooth circle around the impact
                double intensity = kernel[w + brushRadius][h + brushRadius];
                data[index].intensity += intensity;

                // Probability
                double probability = _brushSaturation * intensity;
                data[index].impactProbability += probability;

                // Time
                if (data[index].impactTime > 0.0) {
                    double alpha0 = intensity + data[index].intensity * (1 - intensity);
                    double smmothed = time * intensity +
                        data[index].impactTime * data[index].intensity * (1 - intensity);
                    smmothed /= alpha0;
                    data[index].impactTime = smmothed;
                }
                else {
                    data[index].impactTime = time;
                }
            }
        }

        ++impactCounter;
        progressCallback(impactCounter / static_cast<float>(_impactCoordinates.size()));
    }

    // Count the number of color channels in the night layer image
    int nightColorChannels = nightChannels > 3 ? 3 : nightChannels;

    // For the risk we need the final impact probability to already be in place
    impactCounter = 0;
    LINFO("Creating pixel impact risk data...");
    progressCallback(0.0);
    for (const ImpactCoordinate& impact : _impactCoordinates) {
        // Find the pixel in the texture data list for the impact
        int pixelH = convertLatitude(impact.latitude, _imageHeight);
        int pixelW = convertLongitude(impact.longitude, _imageWidth);

        // Use the night layer image to estimate impact risk due to population density
        // Sample the surrounding area in the night map to estimate the risk of the impact
        float risk = 0.0;
        for (int w = -brushRadius; w <= brushRadius; w++) {
            for (int h = -brushRadius; h <= brushRadius; h++) {
                int nightIndex = pixelIndex(
                    pixelW + w,
                    pixelH + h,
                    nightChannels,
                    nightWidth,
                    nightHeight,
                    true
                );
                if (!(nightData + nightIndex)) {
                    throw ghoul::RuntimeError(std::format(
                        "Cannot find night map index {}", nightIndex
                    ));
                }

                // To avoid the blue or water and red of deserts, take the lowest color
                // channel value
                float pixelRisk = static_cast<float>(*(nightData + nightIndex)) / std::numeric_limits<uint8_t>::max();
                for (int i = 1; i < nightColorChannels; ++i) {
                    pixelRisk = std::min(
                        pixelRisk,
                        static_cast<float>(*(nightData + nightIndex + i)) / std::numeric_limits<uint8_t>::max()
                    );
                }

                // Add the night color as is to the debug color
                int index = pixelIndex(
                    pixelW + w,
                    pixelH + h,
                    1,
                    _imageWidth,
                    _imageHeight,
                    true
                );
                data[index].debugColor = glm::vec4(
                    static_cast<float>(*(nightData + nightIndex)) / std::numeric_limits<uint8_t>::max(),
                    static_cast<float>(*(nightData + nightIndex + 1)) / std::numeric_limits<uint8_t>::max(),
                    static_cast<float>(*(nightData + nightIndex + 2)) / std::numeric_limits<uint8_t>::max(),
                    static_cast<float>(*(nightData + nightIndex + 3)) / std::numeric_limits<uint8_t>::max()
                );

                risk += pixelRisk;
            }
        }
        risk /= static_cast<float>(_brushSize * _brushSize);

        // Multiply the risk with the final impact probability to get the final risk value
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

                // Risk
                float impactRisk = _brushSaturation * risk;
                data[index].impactRisk = std::max(
                    impactRisk * data[index].impactProbability,
                    data[index].impactRisk
                );
            }
        }

        ++impactCounter;
        progressCallback(impactCounter / static_cast<float>(_impactCoordinates.size()));
    }

    return data;
}

void ImpactCorridorTask::processImage(const Task::ProgressCallback& progressCallback,
                                      DataType dataType,
                                      const std::vector<ImpactPixel>& data,
                                      unsigned int nPixels, unsigned int size,
                                      const openspace::dataloader::ColorMap& colorMap,
                                 const openspace::dataloader::ColorMap& timeLowerColorMap,
                                 const openspace::dataloader::ColorMap& timeUpperColorMap,
                                                           std::vector<glm::vec4>& pixels)
{
    // Find the minimum and maximum values for the chosen data type
    std::string dataTypeName;
    double minValue = std::numeric_limits<double>::max();
    double maxValue = 0.0;
    switch (dataType) {
        case DataType::Probability:
            dataTypeName = "probability";
            for (int i = 0; i < nPixels; ++i) {
                if (!data[i].hasImpact) {
                    continue;
                }
                minValue = std::min(minValue, data[i].impactProbability);
                maxValue = std::max(maxValue, data[i].impactProbability);
            }
            break;
        case DataType::Risk:
            dataTypeName = "risk";

            for (int i = 0; i < nPixels; ++i) {
                if (!data[i].hasImpact) {
                    continue;
                }
                minValue = std::min(minValue, data[i].impactRisk);
                maxValue = std::max(maxValue, data[i].impactRisk);
            }
            break;
        case DataType::Time:
            dataTypeName = "time";
            for (int i = 0; i < nPixels; ++i) {
                if (!data[i].hasImpact) {
                    continue;
                }
                minValue = std::min(minValue, data[i].impactTime);
                maxValue = std::max(maxValue, data[i].impactTime);
            }
            break;
        case DataType::Debug:
            dataTypeName = "debug";
            minValue = 0.0;
            maxValue = 1.0;
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    // Save the minimum and maximum values for the chosen data type to a file so that we
    // can create a color legend later
    std::string rangeFilename =
        std::format("{}-{}-range.txt", _outputFilename.string(), dataTypeName);
    std::ofstream file(rangeFilename);
    if (!file) {
        LERROR(std::format("Could not open file {} to store value range", rangeFilename));
        return;
    }

    if (dataType == DataType::Time) {
        // Convert the time to a readable string
        SpiceChar readableTime[TimeStringLength];

        timout_c(
            minValue,                // Time in J2000 seconds
            TimeStringFormat.data(), // Format for the output string
            TimeStringLength,        // Length of the output string plus 1
            readableTime             // Result
        );
        file << std::format("Min {:<15} {:<30}\n", dataTypeName, readableTime);

        timout_c(
            maxValue,                // Time in J2000 seconds
            TimeStringFormat.data(), // Format for the output string
            TimeStringLength,        // Length of the output string plus 1
            readableTime             // Result
        );
        file << std::format("Max {:<15} {:<30}\n", dataTypeName, readableTime);
    }
    else {
        file << std::format("Min {:<15} {:<20}\n", dataTypeName, minValue);
        file << std::format("Max {:<15} {:<20}\n", dataTypeName, maxValue);
    }
    file.close();

    // Find the maximum intensity value
    double maxIntensity = 0.0;
    for (int i = 0; i < nPixels; ++i) {
        if (!data[i].hasImpact) {
            continue;
        }
        maxIntensity = std::max(maxIntensity, data[i].intensity);
    }

    // Apply the transfer function for each pixel and store the resulting color
    LINFO(std::format("Processing impact {} image...", dataTypeName));
    progressCallback(0.0);
    applyColorMap(
        progressCallback,
        dataType,
        data,
        nPixels,
        minValue,
        maxValue,
        maxIntensity,
        colorMap,
        timeLowerColorMap,
        timeUpperColorMap,
        pixels
    );

    // Write the impact image to file
    writeFinalImage(
        progressCallback,
        std::format("{}-{}.png", _outputFilename.string(), dataTypeName),
        nPixels,
        size,
        pixels
    );
}

void ImpactCorridorTask::applyColorMap(const Task::ProgressCallback& progressCallback,
                                       DataType dataType,
                                       const std::vector<ImpactPixel>& data,
                                       int nPixels, double minValue, double maxValue,
                                       double maxIntensity,
                                       const openspace::dataloader::ColorMap& colorMap,
                                 const openspace::dataloader::ColorMap& timeLowerColorMap,
                                 const openspace::dataloader::ColorMap& timeUpperColorMap,
                                                           std::vector<glm::vec4>& pixels)
{
    for (int p = 0; p < nPixels; ++p) {
        if (!data[p].hasImpact) {
            progressCallback((p + 1) / static_cast<float>(nPixels));
            continue;
        }

        // Get the selected data type
        double dataValue = 0.0;
        switch (dataType) {
            case DataType::Probability:
                dataValue = data[p].impactProbability;
                break;
            case DataType::Risk:
                dataValue = data[p].impactRisk;
                break;
            case DataType::Time:
                dataValue = data[p].impactTime;
                break;
            case DataType::Debug:
                break;
            default:
                throw ghoul::MissingCaseException();
        }

        // Find the index in the color map that cooresponds to the value
        double normalizedValue = (dataValue - minValue) / (maxValue - minValue);
        int colorMapIndex = static_cast<int>(std::round(
            normalizedValue * (static_cast<int>(colorMap.entries.size()) - 1)
        ));
        if (colorMapIndex < 0) {
            colorMapIndex = 0;
        }
        else if (colorMapIndex >= colorMap.entries.size()) {
            colorMapIndex = colorMap.entries.size() - 1;
        }

        // Invert the color map if needed
        if (_invertColorMap) {
            colorMapIndex = (colorMap.entries.size() - 1) - colorMapIndex;
        }

        // Get and save the color that this value cooresponds to in the color map
        glm::vec4 color = colorMap.entries[colorMapIndex];
        color.a = static_cast<float>(data[p].intensity / maxIntensity);

        // Special case for the time data type
        if (dataType == DataType::Time) {
            // Combine two sequential color maps to become one custom diverging color map
            if (normalizedValue < 0.5) {
                // Use the lower color map, this is inverted relative the upper one
                double value = 1.0 - (normalizedValue / 0.5);

                colorMapIndex = static_cast<int>(std::round(
                    value * (static_cast<int>(timeLowerColorMap.entries.size()) - 1)
                ));

                // Invert the color map if needed
                if (_invertColorMap) {
                    colorMapIndex = (timeLowerColorMap.entries.size() - 1) - colorMapIndex;
                }

                color = timeLowerColorMap.entries[colorMapIndex];
            }
            else if (normalizedValue > 0.5) {
                // Use the upper color map
                double value = (normalizedValue / 0.5) - 1.0;
                colorMapIndex = static_cast<int>(std::round(
                    value * (static_cast<int>(timeUpperColorMap.entries.size()) - 1)
                ));

                // Invert the color map if needed
                if (_invertColorMap) {
                    colorMapIndex = (timeUpperColorMap.entries.size() - 1) - colorMapIndex;
                }

                color = timeUpperColorMap.entries[colorMapIndex];
            }
            else {
                // Use pure white
                color = glm::vec4(1.f);
            }
            color.a = static_cast<float>(data[p].intensity / maxIntensity);

            // Bump up the saturation a bit for the time map
            glm::vec3 hslColor = rgbToHsl(glm::vec3(color.r, color.g, color.b));
            hslColor.y *= 1.0f;

            glm::vec3 rgbColor = hslToRgb(hslColor);
            color.r = rgbColor.r;
            color.g = rgbColor.g;
            color.b = rgbColor.b;
        }

        if (dataType == DataType::Debug) {
            pixels[p] = data[p].debugColor;
        }
        else {
            pixels[p] = color;
        }

        progressCallback((p + 1) / static_cast<float>(nPixels));
    }
}

void ImpactCorridorTask::writeFinalImage(const Task::ProgressCallback& progressCallback,
                                         const std::string& filename,
                                         unsigned int nPixels, unsigned int size,
                                         const std::vector<glm::vec4>& pixels)
{
    LINFO("Finalizing image...");
    progressCallback(0.0);
    std::vector<GLubyte> image = std::vector<GLubyte>(size, 0);

    for (int p = 0, i = 0; p < nPixels && i < size; ++p, i += NChannels) {
        glm::vec4 color = pixels[p];

        for (int c = 0; c < NChannels; ++c) {
            // Watch out for overflow
            double clampedSaturation = std::min(std::round(color[c] * 255.0), 255.0);
            image[i + c] = static_cast<GLubyte>(clampedSaturation);
        }
        progressCallback((p + 1) / static_cast<float>(nPixels));
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
