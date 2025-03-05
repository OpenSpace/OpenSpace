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

#include <openspace/data/dataloader.h>
#include <ghoul/glm.h>
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

    struct ImpactPixel {
        double impactProbability = 0.0;
        double impactRisk = 0.0;
        double impactTime = 0.0;
        double intensity = 0.0;
        bool hasImpact = false;
        glm::vec4 debugColor = glm::vec4(0.0f);
    };

    enum class DataType {
        Probability = 0,
        Risk,
        Time,
        Debug
    };

    /**
     * Read the entire impact file and store all impact coordinates in the
     * _impactCoordinates list. Use the ghouls getline function to handle line endings.
     */
    void readImpactFile();

    /**
     * Read a single line in the impact file and return it. Use the ghouls getline
     * function to handle line endings.
     *
     * \param file The impact file stream to read from
     * \return The impact coordinate read from the file
     */
    ImpactCoordinate readImpactCoordinate(std::ifstream& file);

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
     * \param numChannels The number of color channels in the image
     * \param imageWidth The total width of the image in pixels
     * \param imageHeight The total height of the image in pixels
     * \param allowWrap Whether to allow the pixel to wrap around the image if it is
     *        located outside
     * \return The index of the first channel of the given pixel in the flat data list
     */
    int pixelIndex(int pixelW, int pixelH, int numChannels, int imageWidth,
        int imageHeight, bool allowWrap);

    /**
     * Plot all impacts onto a flat list of pixels with a gausian filter for smmothness,
     * overlapping plots are added together or averaged depending on the type of data.
     * This list only contain one channel per pixel which is the summer data contirbution
     * for that resulting pixel. Color channels are added in a later step based on a color
     * map and the type of data. Multiple types of data is plotted at the same time into a
     * storage for each pixel.
     *
     * \param progressCallback To comunicate progress amount
     * \param nPixels The total number of pixels in the image
     * \param nightData The pixel data for the night layer map
     * \param nightWidth The width of the night layer map in pixels
     * \param nightHeight The height of the night layer map in pixels
     * \param nightChannels The number of color channels in the night layer map
     * \return The flat list of pixel data with the summed data contribution from the
     *         impacts in several aspects
     */
    std::vector<ImpactPixel> plotImpactData(
        const Task::ProgressCallback& progressCallback, unsigned int nPixels,
        const unsigned char* nightData, int nightWidth, int nightHeight,
        int nightChannels);

    /**
     * Take the input data, normalize it, apply the color map and finaly save the final
     * image to file.
     *
     * \param progressCallback To comunicate progress amount
     * \param dataType The type of data to plot
     * \param data The flat list of pixel data
     * \param nPixels The total number of pixels in the image
     * \param size The total size of the image when considering the color channels
     * \param colorMap The color map used to color the data
     * \param pixels The pixel data to save as an image file
     */
    void processImage(const Task::ProgressCallback& progressCallback, DataType dataType,
        const std::vector<ImpactPixel>& data, unsigned int nPixels, unsigned int size,
        const openspace::dataloader::ColorMap& colorMap, std::vector<glm::vec4>& pixels);

    /**
     * Apply the color map to the flat list of pixel saturation data.
     *
     * \param progressCallback To comunicate progress amount
     * \param dataType The type of data to plot
     * \param data The flat list of pixel data
     * \param nPixels The total number of pixels in the image
     * \param minValue The minimum scaling value for the color map
     * \param maxValue The maximum scaling value for the color map
     * \param colorMap The color map used to color the data
     * \param pixels The pixel data to store the applied color in
     */
    void applyColorMap(const Task::ProgressCallback& progressCallback, DataType dataType,
        const std::vector<ImpactPixel>& data, int nPixels, double minValue, double maxValue,
        double maxIntensity, const openspace::dataloader::ColorMap& colorMap,
        std::vector<glm::vec4>& pixels);

    /**
     * Write the given image to file in the PNG format.
     *
     * \param progressCallback To comunicate progress amount
     * \param filename The name of the .png image file to save. This name must include
     *        the .png extension
     * \param nPixels The total number of pixels in the image
     * \param size The total size of the image when considering the color channels
     * \param pixels The pixel data to save as an image file
     */
    void writeFinalImage(const Task::ProgressCallback& progressCallback,
        const std::string& filename, unsigned int nPixels, unsigned int size,
        const std::vector<glm::vec4>& pixels);

    std::string _asteroidName;
    std::filesystem::path _impactFile;
    std::vector<ImpactCoordinate> _impactCoordinates;
    std::filesystem::path _outputFilename;
    int _imageWidth;
    int _imageHeight;
    int _brushSize;
    int _brushSaturation;
    double _filterStrength;
    std::filesystem::path _colorMap;
    bool _invertColorMap;
    bool _hasNightMap;
    std::filesystem::path _nightMap;
};

} // namespace openspace::neoviz

#endif // __OPENSPACE_MODULE_NEOVIZ___IMPACTCORRIDORTASK___H__
