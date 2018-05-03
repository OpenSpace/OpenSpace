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
#include <modules/fitsfilereader/include/fitsfilereader.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/fmt.h>

#include <openspace/util/distanceconversion.h>
#include <CCfits>
#include <fstream>

using namespace CCfits;

namespace {
    const char* _loggerCat = "FitsFileReader";
}

namespace openspace {

FitsFileReader::FitsFileReader(bool verboseMode) {
    _verboseMode = verboseMode;
    FITS::setVerboseMode(_verboseMode);
}

FitsFileReader::~FitsFileReader() {
    if (_infile) {
        _infile->destroy();
        _infile = nullptr;
    }
}

bool FitsFileReader::isPrimaryHDU() {
    return _infile->extension().size() == 0;
}

template <typename T>
std::shared_ptr<ImageData<T>> FitsFileReader::readImage(const std::string& path) {
    try {
        _infile = std::make_unique<FITS>(path, Read, true);
        // Primary HDU Object
        if (isPrimaryHDU()) {
            const std::shared_ptr<ImageData<T>> im
                  = readImageInternal<T>(_infile->pHDU());
            return std::move(im);
        }
        // Extension HDU Object
        const std::shared_ptr<ImageData<T>> im
              = readImageInternal<T>(_infile->currentExtension());
        return std::move(im);
    } catch (FitsException& e){
        LERROR("Could not read FITS image from table. " + e.message() );
    }

    return nullptr;
}

template <typename T>
std::shared_ptr<std::unordered_map<std::string, T>>
      FitsFileReader::readHeader(std::vector<std::string>& keywords)
{
    try {
        HDU* image = isPrimaryHDU() ? static_cast<HDU*>(&_infile->pHDU()) :
                                      static_cast<HDU*>(&_infile->currentExtension());

        std::vector<T> values;
        image->readKeys(keywords, values);

        if (values.size() != keywords.size()) {
            LERROR("Number of keywords does not match number of values");
        }

        std::unordered_map<std::string, T> result;
        std::transform(keywords.begin(), keywords.end(), values.begin(),
                       std::inserter(result, result.end()),
                       [](std::string key, T value) {
            return std::make_pair(key, value);
        });
        return std::make_shared<std::unordered_map<std::string, T>>(std::move(result));
    } catch (const FitsException& e) {
        LERROR("Could not read FITS header. " + e.message() );
    }
    return nullptr;
}

template <typename T>
std::shared_ptr<T> FitsFileReader::readHeaderValue(const std::string key) {
    try {
        HDU* image = isPrimaryHDU() ? static_cast<HDU*>(&_infile->pHDU()) :
                                      static_cast<HDU*>(&_infile->currentExtension());

        T value;
        image->readKey(key, value);
        return std::make_unique<T>(value);
    } catch (FitsException& e) {
        LERROR("Could not read FITS key. " + e.message() );
    }
    return nullptr;
}

// Read specified table columns from fits file. 
// If readAll variable is set to true the entire table will be read before the 
// selected columns, which makes the function take a lot longer if it's a big file. 
// If no HDU index is given the currentExtension will be read from. 
template<typename T>
std::shared_ptr<TableData<T>> FitsFileReader::readTable(std::string& path,
    const std::vector<std::string>& columnNames, int startRow, int endRow, 
    int hduIdx, bool readAll) {

    // We need to lock reading when using multithreads because CCfits can't handle multiple I/O drivers.
    _mutex.lock();

    try {
        _infile = std::make_unique<FITS>(path, Read, readAll);

        // Make sure FITS file is not a Primary HDU Object (aka an image).
        if (!isPrimaryHDU()) {

            ExtHDU& table = _infile->extension(hduIdx);
            int numCols = columnNames.size();
            int numRowsInTable = table.rows();
            std::unordered_map<string, std::vector<T>> contents;
            LINFO("Read file: " + _infile->name());

            int firstRow = startRow < 1 ? 1 : startRow;

            if (endRow < firstRow) { endRow = numRowsInTable; }

            for (int i = 0; i < numCols; ++i) {
                std::vector<T> columnData;
                //LINFO("Read column: " + columnNames[i]);
                table.column(columnNames[i]).read(columnData, firstRow, endRow);
                contents[columnNames[i]] = columnData;
            }

            // Create TableData object of table contents.
            TableData<T> loadedTable = {
                std::move(contents), table.rows(), table.getRowsize(), table.name()
            };

            _mutex.unlock();
            return std::make_shared<TableData<T>>(loadedTable);
        }
    }
    catch (FitsException& e) {
        LERROR("Could not read FITS table from file. Make sure it's not an image file. " 
            + e.message() );
    }
    return nullptr;
}

std::vector<float> FitsFileReader::readFitsFile(std::string filePath, int& nValuesPerStar, int firstRow, 
    int lastRow, std::vector<std::string> filterColumnNames, int multiplier) {
    
    std::vector<float> fullData;
    srand(1234567890);
    if (firstRow <= 0) firstRow = 1;

    // Define what columns to read. Append additional filter parameters to default rendering parameters.
    auto allColumnNames = std::vector<std::string>({
        "Position_X",
        "Position_Y",
        "Position_Z",
        "Velocity_X",
        "Velocity_Y",
        "Velocity_Z",
        "Gaia_Parallax",
        "Gaia_G_Mag",
        "Tycho_B_Mag",
        "Tycho_V_Mag"
        });
    allColumnNames.insert(allColumnNames.end(), filterColumnNames.begin(), filterColumnNames.end());

    std::string allNames = "Columns to read: \n";
    for (auto colName : allColumnNames) {
        allNames += colName + "\n";
    }
    LINFO(allNames);

    // Read columns from FITS file. If rows aren't specified then full table will be read.
    auto table = readTable<float>(filePath, allColumnNames, firstRow, lastRow);

    if (!table) {
        throw ghoul::RuntimeError(fmt::format("Failed to open Fits file '{}'", filePath));
    }

    int nStars = table->readRows - firstRow + 1;

    int nNullArr = 0;
    size_t nColumnsRead = allColumnNames.size();
    size_t defaultCols = 17; // Number of columns that are copied by predefined code.  
    if (nColumnsRead != defaultCols) {
        LINFO("Additional columns will be read! Consider add column in code for significant speedup!");
    }
    // Declare how many values to save per star
    nValuesPerStar = nColumnsRead + 1; // +1 for B-V color value.

    // Copy columns to local variables.
    std::unordered_map<std::string, std::vector<float>>& tableContent = table->contents;

    // Default render parameters! 
    std::vector<float> posXcol = std::move(tableContent[allColumnNames[0]]);
    std::vector<float> posYcol = std::move(tableContent[allColumnNames[1]]);
    std::vector<float> posZcol = std::move(tableContent[allColumnNames[2]]);
    std::vector<float> velXcol = std::move(tableContent[allColumnNames[3]]);
    std::vector<float> velYcol = std::move(tableContent[allColumnNames[4]]);
    std::vector<float> velZcol = std::move(tableContent[allColumnNames[5]]);
    std::vector<float> parallax = std::move(tableContent[allColumnNames[6]]);
    std::vector<float> magCol = std::move(tableContent[allColumnNames[7]]);
    std::vector<float> tycho_b = std::move(tableContent[allColumnNames[8]]);
    std::vector<float> tycho_v = std::move(tableContent[allColumnNames[9]]);

    // Default filter parameters - must follow the same order as Lua script!
    // Additional filter parameters are handled as well but slows down reading tremendously!
    std::vector<float> parallax_err = std::move(tableContent[allColumnNames[10]]);
    std::vector<float> pr_mot_ra = std::move(tableContent[allColumnNames[11]]);
    std::vector<float> pr_mot_ra_err = std::move(tableContent[allColumnNames[12]]);
    std::vector<float> pr_mot_dec = std::move(tableContent[allColumnNames[13]]);
    std::vector<float> pr_mot_dec_err = std::move(tableContent[allColumnNames[14]]);
    std::vector<float> tycho_b_err = std::move(tableContent[allColumnNames[15]]);
    std::vector<float> tycho_v_err = std::move(tableContent[allColumnNames[16]]);

    // Construct data array. OBS: ORDERING IS IMPORTANT! This is where slicing happens.
    for (int i = 0; i < nStars * multiplier; ++i) {
        std::vector<float> values(nValuesPerStar);
        size_t idx = 0;

        // Default order for rendering:
        // Position [X, Y, Z]
        // Absolute Magnitude
        // B-V Color
        // Velocity [X, Y, Z]

        // Store positions.
        values[idx++] = posXcol[i%nStars];
        values[idx++] = posYcol[i%nStars];
        values[idx++] = posZcol[i%nStars];

        // Return early if star doesn't have a measured position.
        if (values[0] == -999 && values[1] == -999 && values[2] == -999) {
            nNullArr++;
            continue;
        }

        // Store color values.
        values[idx++] = magCol[i%nStars];
        values[idx++] = tycho_b[i%nStars] - tycho_v[i%nStars];

        // Store velocity. Convert it to m/s with help by parallax.
        values[idx++] = convertMasPerYearToMeterPerSecond(velXcol[i%nStars], parallax[i%nStars]);
        values[idx++] = convertMasPerYearToMeterPerSecond(velYcol[i%nStars], parallax[i%nStars]);
        values[idx++] = convertMasPerYearToMeterPerSecond(velZcol[i%nStars], parallax[i%nStars]);

        // Store additional parameters to filter by.
        values[idx++] = parallax[i%nStars];
        values[idx++] = parallax_err[i%nStars];
        values[idx++] = pr_mot_ra[i%nStars];
        values[idx++] = pr_mot_ra_err[i%nStars];
        values[idx++] = pr_mot_dec[i%nStars];
        values[idx++] = pr_mot_dec_err[i%nStars];
        values[idx++] = tycho_b[i%nStars];
        values[idx++] = tycho_b_err[i%nStars];
        values[idx++] = tycho_v[i%nStars];
        values[idx++] = tycho_v_err[i%nStars];

        // Read extra columns, if any. This will slow down the sorting tremendously!
        for (size_t col = defaultCols; col < nColumnsRead; ++col) {
            std::vector<float> vecData = std::move(tableContent[allColumnNames[col]]);
            values[idx++] = vecData[i];
        }

        for (size_t j = 0; j < nValuesPerStar; ++j) {
            // The astronomers in Vienna use -999 as default value. Change it to 0.
            if (values[j] == -999) {
                values[j] = 0.f;
            }
            else if (multiplier > 1) {
                values[j] *= static_cast<float>(rand()) / static_cast<float>(RAND_MAX);
            }
        }

        fullData.insert(fullData.end(), values.begin(), values.end());
    }

    LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) + " read stars were nullArrays.");
    LINFO("Multiplier: " + std::to_string(multiplier));
    
    return fullData;
}

std::vector<float> FitsFileReader::readSpeckFile(std::string filePath, int& nRenderValues) {
    auto fullData = std::vector<float>();

    std::ifstream fileStream(filePath);

    if (!fileStream.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", filePath));
        return fullData;
    }

    int nValuesPerStar = 0;
    int nNullArr = 0;
    size_t nStars = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', 'texture' and 'maxcomment')
    std::string line = "";
    while (true) {
        std::streampos position = fileStream.tellg();
        std::getline(fileStream, line);

        if (line[0] == '#' || line.empty()) {
            continue;
        }

        if (line.substr(0, 7) != "datavar" &&
            line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture" &&
            line.substr(0, 10) != "maxcomment")
        {
            // We read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line.
            fileStream.seekg(position);
            break;
        }

        if (line.substr(0, 7) == "datavar") {
            // datavar lines are structured as follows:
            // datavar # description
            // where # is the index of the data variable; so if we repeatedly overwrite
            // the 'nValues' variable with the latest index, we will end up with the total
            // number of values (+3 since X Y Z are not counted in the Speck file index)
            std::stringstream str(line);

            std::string dummy;
            str >> dummy;
            str >> nValuesPerStar;
            nValuesPerStar += 1; // We want the number, but the index is 0 based
        }
    }

    nValuesPerStar += 3; // X Y Z are not counted in the Speck file indices

    // Order in Speck file:
    // 0 BVcolor
    // 1 lum
    // 2 Vabsmag
    // 3 Vappmag
    // 4 distly
    // 5 distpcPctErr
    // 6 U
    // 7 V
    // 8 W
    // 9 speed
    // 10 sptypeindex
    // 11 lumclassindex
    // 12 catsource
    // 13 texture

    do {
        std::vector<float> readValues(nValuesPerStar);
        nStars++;

        std::getline(fileStream, line);
        std::stringstream str(line);

        // Read values. 
        for (int i = 0; i < nValuesPerStar; ++i) {
            str >> readValues[i];
        }

        // Check if star is a nullArray.
        bool nullArray = true;
        for (size_t i = 0; i < readValues.size(); ++i) {
            if (readValues[i] != 0.0) {
                nullArray = false;
                break;
            }
        }

        // Insert to data if we found some values.
        if (!nullArray) {

            // Re-order data here because Octree expects the data in correct order when read.
            // Default order for rendering:
            // Position [X, Y, Z]
            // Absolute Magnitude
            // B-V Color
            // Velocity [X, Y, Z]

            nRenderValues = 8;
            std::vector<float> renderValues(nRenderValues);

            // Gaia DR1 data from AMNH measures positions in Parsec, but RenderableGaiaStars
            // expects kiloParsec (because fits file from Vienna had in kPc).
            // Thus we need to convert positions twice atm.
            renderValues[0] = readValues[0] / 1000.0; // PosX
            renderValues[1] = readValues[1] / 1000.0; // PosY
            renderValues[2] = readValues[2] / 1000.0; // PosZ
            renderValues[3] = readValues[5]; // AbsMag
            renderValues[4] = readValues[3]; // B-V color
            renderValues[5] = readValues[9]; // Vel X
            renderValues[6] = readValues[10]; // Vel Y
            renderValues[7] = readValues[11]; // Vel Z

            fullData.insert(fullData.end(), renderValues.begin(), renderValues.end());
        }
        else {
            nNullArr++;
        }

    } while (!fileStream.eof());

    LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) + 
        " read stars were nullArrays");
    
    return fullData;
}


// This is pretty annoying, the read method is not derived from the HDU class
// in CCfits - need to explicitly cast to the sub classes to access read
template<typename T>
const std::shared_ptr<ImageData<T>> FitsFileReader::readImageInternal(ExtHDU& image) {
   try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {std::move(contents), image.axis(0), image.axis(1)};
        return std::make_shared<ImageData<T>>(im);
    } catch (FitsException& e){
        LERROR("Could not read FITS image EXTHDU. " + e.message() );
    }
    return nullptr;
}

template<typename T>
const std::shared_ptr<ImageData<T>> FitsFileReader::readImageInternal(PHDU& image) {
    try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {std::move(contents), image.axis(0), image.axis(1)};
        return std::make_shared<ImageData<T>>(im);
    } catch (FitsException& e){
        LERROR("Could not read FITS image PHDU. " + e.message() );
    }
    return nullptr;
}

// Type deductions
template std::shared_ptr<ImageData<unsigned long>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<long>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<unsigned int>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<int>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<double>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<float>> FitsFileReader::readImage(const std::string& path);
template std::shared_ptr<ImageData<unsigned char>> FitsFileReader::readImage(const std::string& path);

template std::shared_ptr<std::unordered_map<std::string, float>> FitsFileReader::readHeader(std::vector<std::string>& keywords);
template std::shared_ptr<std::unordered_map<std::string, std::string>> FitsFileReader::readHeader(std::vector<std::string>& keywords);

template std::shared_ptr<float> FitsFileReader::readHeaderValue(const std::string key);
template std::shared_ptr<int> FitsFileReader::readHeaderValue(const std::string key);
template std::shared_ptr<std::string> FitsFileReader::readHeaderValue(const std::string key);

template std::shared_ptr<TableData<float>> FitsFileReader::readTable(std::string& path, 
    const std::vector<std::string>& columnNames, int startRow, int endRow, 
    int hduIdx, bool readAll);
} // namespace openspace
