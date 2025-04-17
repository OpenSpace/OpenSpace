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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSDATAPREPARATIONTASK___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSDATAPREPARATIONTASK___H__

#include <openspace/util/task.h>

#include <modules/exoplanets/datastructure.h>
#include <openspace/properties/vector/vec3property.h>
#include <filesystem>
#include <string>

namespace openspace::exoplanets {

class ExoplanetsDataPreparationTask : public Task {
public:
    struct PlanetData {
        std::string host;
        std::string name;
        std::string component;
        ExoplanetDataEntry dataEntry;
    };

    explicit ExoplanetsDataPreparationTask(const ghoul::Dictionary& dictionary);

    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation documentation();

    /**
     * The exoplanet file includes a set of commented lines in the beginning. This
     * function skips those lines, as well as any empty lines, and continues reading until
     * it find the first valid data line (which will be the column names).
     *
     * \param file The input file stream used for reading the file
     * \return The content of the first data row (i.e. the column names)
     */
    static std::vector<std::string> readFirstDataRow(std::ifstream& file);

    /**
     * Parse a row in the CSV file of exoplanets. Assumes the same format and column names
     * as provided by the NASA Exoplanet Archive.
     *
     * \param row The row to parse, given as a string
     * \param columnNames The list of column names in the file, from the CSV header
     * \param positionSourceFile A SPECK file to use for getting the position of the star.
     *        This is used to make sure the position of the star matches those of other
     *        star datasets. If no file is provided, the position from the CSV data file
     *        is read and used instead
     * \param bvFromTeffConversionFile A text file containing a mapping between effective
     *        temperature (teff) values and B-V color index values. Each line should
     *        include two values separated by a comma: first the teff value and then the
     *        B-V value
     * \return An object containing the parsed information
     *
     * /sa https://exoplanetarchive.ipac.caltech.edu/
     */
    static PlanetData parseDataRow(const std::string& row,
        const std::vector<std::string>& columnNames,
        const std::filesystem::path& positionSourceFile,
        const std::filesystem::path& bvFromTeffConversionFile);

private:
    std::filesystem::path _inputDataPath;
    std::filesystem::path _inputSpeckPath;
    std::filesystem::path _outputBinPath;
    std::filesystem::path _outputLutPath;
    std::filesystem::path _teffToBvFilePath;

    /**
     * Try to find the star position from an input speck file. If not found, the
     * returned position will contain NaN values.
     *
     * \param starName The name of the star to look for in the file
     * \param sourceFile The name of the file in which to look
     * \return The resulting star position, given in galactix XYZ
     */
    static glm::vec3 starPosition(const std::string& starName,
        const std::filesystem::path& sourceFile);

    // Compute b-v color from teff value using a conversion file
    static float bvFromTeff(float teff, const std::filesystem::path& conversionFile);
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSDATAPREPARATIONTASK___H__
