/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/util/distanceconversion.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <fstream>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4100) // unreferenced formal parameter
#pragma warning (disable : 4267) // conversion from 'size_t' to 'int'
#pragma warning (disable : 4456) // declaration of 'col' hides previous local declaration
#endif // WIN32

#include <CCfits>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32


using namespace CCfits;

namespace {
    constexpr std::string_view _loggerCat = "FitsFileReader";
} // namespace

namespace openspace {

FitsFileReader::FitsFileReader(bool verboseMode)
    : _verboseMode(verboseMode)
{
    FITS::setVerboseMode(_verboseMode);
}

FitsFileReader::~FitsFileReader() {
    if (_infile) {
        _infile->destroy();
        _infile = nullptr;
    }
}

bool FitsFileReader::isPrimaryHDU() {
    return _infile->extension().empty();
}

template <typename T>
std::shared_ptr<ImageData<T>> FitsFileReader::readImage(const std::filesystem::path& path)
{
    try {
        _infile = std::make_unique<FITS>(path, Read, true);
        // Primary HDU Object
        if (isPrimaryHDU()) {
            return readImageInternal<T>(_infile->pHDU());
        }
        // Extension HDU Object
        return readImageInternal<T>(_infile->currentExtension());
    } catch (const FitsException& e){
        LERROR("Could not read FITS image from table. " + e.message());
    }

    return nullptr;
}

template <typename T>
std::shared_ptr<std::unordered_map<std::string, T>> FitsFileReader::readHeader(
                                                       std::vector<std::string>& keywords)
{
    try {
        HDU& image = isPrimaryHDU() ?
            static_cast<HDU&>(_infile->pHDU()) :
            static_cast<HDU&>(_infile->currentExtension());

        std::vector<T> values;
        image.readKeys(keywords, values);

        if (values.size() != keywords.size()) {
            LERROR("Number of keywords does not match number of values");
        }

        std::unordered_map<std::string, T> result;
        std::transform(
            keywords.begin(),
            keywords.end(),
            values.begin(),
            std::inserter(
                result,
                result.end()
            ),
            [](std::string key, T value) { return std::make_pair(key, value); }
        );
        return std::make_shared<std::unordered_map<std::string, T>>(std::move(result));
    } catch (const FitsException& e) {
        LERROR("Could not read FITS header. " + e.message());
    }
    return nullptr;
}

template <typename T>
std::shared_ptr<T> FitsFileReader::readHeaderValue(const std::string key) {
    try {
        HDU& image = isPrimaryHDU() ?
            static_cast<HDU&>(_infile->pHDU()) :
            static_cast<HDU&>(_infile->currentExtension());

        T value;
        image.readKey(key, value);
        return std::make_unique<T>(value);
    } catch (FitsException& e) {
        LERROR("Could not read FITS key. " + e.message());
    }
    return nullptr;
}

template<typename T>
std::shared_ptr<TableData<T>> FitsFileReader::readTable(const std::filesystem::path& path,
                                              const std::vector<std::string>& columnNames,
                                                                             int startRow,
                                                                               int endRow,
                                                                               int hduIdx,
                                                                             bool readAll)
{
    // We need to lock reading when using multithreads because CCfits can't handle
    // multiple I/O drivers.
    const std::lock_guard g(_mutex);

    try {
        _infile = std::make_unique<FITS>(path.string(), Read, readAll);

        // Make sure FITS file is not a Primary HDU Object (aka an image).
        if (!isPrimaryHDU()) {
            const ExtHDU& table = _infile->extension(hduIdx);
            const int numCols = static_cast<int>(columnNames.size());
            const int numRowsInTable = static_cast<int>(table.rows());
            std::unordered_map<string, std::vector<T>> contents;
            //LINFO("Read file: " + _infile->name());

            const int firstRow = std::max(startRow, 1);

            if (endRow < firstRow) {
                endRow = numRowsInTable;
            }

            for (int i = 0; i < numCols; i++) {
                std::vector<T> columnData;
                //LINFO("Read column: " + columnNames[i]);
                table.column(columnNames[i]).read(columnData, firstRow, endRow);
                contents[columnNames[i]] = columnData;
            }

            // Create TableData object of table contents.
            TableData<T> loadedTable = {
                .contents = std::move(contents),
                .readRows = static_cast<int>(table.rows()),
                .optimalRowsize = table.getRowsize(),
                .name = table.name()
            };

            return std::make_shared<TableData<T>>(std::move(loadedTable));
        }
    }
    catch (FitsException& e) {
        LERROR(std::format(
            "Could not read FITS table from file '{}'. Make sure it's not an image file",
            e.message()
        ));
    }
    return nullptr;
}

std::vector<float> FitsFileReader::readFitsFile(std::filesystem::path filePath,
                                                int& nValuesPerStar, int firstRow,
                                                int lastRow,
                                               std::vector<std::string> filterColumnNames,
                                                                           int multiplier)
{
    std::vector<float> fullData;
    srand(1234567890);
    if (firstRow <= 0) {
        firstRow = 1;
    }

    // Define what columns to read.
    std::vector<std::string> allColumnNames = {
        "Position_X",
        "Position_Y",
        "Position_Z",
        "Velocity_X",
        "Velocity_Y",
        "Velocity_Z",
        "Gaia_Parallax",
        "Gaia_G_Mag",
        "Tycho_B_Mag",
        "Tycho_V_Mag",
        "Gaia_Parallax_Err",
        "Gaia_Proper_Motion_RA",
        "Gaia_Proper_Motion_RA_Err",
        "Gaia_Proper_Motion_Dec",
        "Gaia_Proper_Motion_Dec_Err",
        "Tycho_B_Mag_Err",
        "Tycho_V_Mag_Err"
    };

    // Append additional filter parameters to default rendering parameters.
    allColumnNames.insert(
        allColumnNames.end(),
        filterColumnNames.begin(),
        filterColumnNames.end()
    );

    std::string allNames = "Columns to read: \n";
    for (const std::string& colName : allColumnNames) {
        allNames += colName + "\n";
    }
    LINFO(allNames);

    // Read columns from FITS file. If rows aren't specified then full table will be read
    const std::shared_ptr<TableData<float>> table = readTable<float>(
        filePath,
        allColumnNames,
        firstRow,
        lastRow
    );

    if (!table) {
        throw ghoul::RuntimeError(std::format("Failed to open Fits file '{}'", filePath));
    }

    int nStars = table->readRows - firstRow + 1;

    int nNullArr = 0;
    const int nColumnsRead = static_cast<int>(allColumnNames.size());
    const int defaultCols = 17; // Number of columns that are copied by predefined code
    if (nColumnsRead != defaultCols) {
        LINFO("Additional columns will be read! Consider add column in code for "
            "significant speedup");
    }
    // Declare how many values to save per star
    nValuesPerStar = nColumnsRead + 1; // +1 for B-V color value

    // Copy columns to local variables
    std::unordered_map<std::string, std::vector<float>>& tableContent = table->contents;

    // Default render parameters
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

    // Default filter parameters
    // Additional filter parameters are handled as well but slows down reading
    std::vector<float> parallax_err = std::move(tableContent[allColumnNames[10]]);
    std::vector<float> pr_mot_ra = std::move(tableContent[allColumnNames[11]]);
    std::vector<float> pr_mot_ra_err = std::move(tableContent[allColumnNames[12]]);
    std::vector<float> pr_mot_dec = std::move(tableContent[allColumnNames[13]]);
    std::vector<float> pr_mot_dec_err = std::move(tableContent[allColumnNames[14]]);
    std::vector<float> tycho_b_err = std::move(tableContent[allColumnNames[15]]);
    std::vector<float> tycho_v_err = std::move(tableContent[allColumnNames[16]]);

    // Construct data array. OBS: ORDERING IS IMPORTANT! This is where slicing happens.
    for (int i = 0; i < nStars * multiplier; i++) {
        std::vector<float> values(nValuesPerStar);
        size_t idx = 0;

        // Default order for rendering:
        // Position [X, Y, Z]
        // Absolute Magnitude
        // B-V Color
        // Velocity [X, Y, Z]

        // Store positions.
        values[idx++] = posXcol[i % nStars];
        values[idx++] = posYcol[i % nStars];
        values[idx++] = posZcol[i % nStars];

        // Return early if star doesn't have a measured position.
        if (values[0] == -999 && values[1] == -999 && values[2] == -999) {
            nNullArr++;
            continue;
        }

        // Store color values.
        values[idx++] = magCol[i % nStars] == -999 ? 20.f : magCol[i % nStars];
        values[idx++] = tycho_b[i % nStars] - tycho_v[i % nStars];

        // Store velocity. Convert it to m/s with help by parallax.
        values[idx++] = convertMasPerYearToMeterPerSecond(
            velXcol[i % nStars],
            parallax[i % nStars]
        );
        values[idx++] = convertMasPerYearToMeterPerSecond(
            velYcol[i % nStars],
            parallax[i % nStars]
        );
        values[idx++] = convertMasPerYearToMeterPerSecond(
            velZcol[i % nStars],
            parallax[i % nStars]
        );

        // Store additional parameters to filter by.
        values[idx++] = parallax[i % nStars];
        values[idx++] = parallax_err[i % nStars];
        values[idx++] = pr_mot_ra[i % nStars];
        values[idx++] = pr_mot_ra_err[i % nStars];
        values[idx++] = pr_mot_dec[i % nStars];
        values[idx++] = pr_mot_dec_err[i % nStars];
        values[idx++] = tycho_b[i % nStars];
        values[idx++] = tycho_b_err[i % nStars];
        values[idx++] = tycho_v[i % nStars];
        values[idx++] = tycho_v_err[i % nStars];

        // Read extra columns, if any. This will slow down the sorting tremendously!
        for (int col = defaultCols; col < nColumnsRead; ++col) {
            std::vector<float> vecData = std::move(tableContent[allColumnNames[col]]);
            values[idx++] = vecData[i];
        }

        for (int j = 0; j < nValuesPerStar; j++) {
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

    // Define what columns to read.
    /*auto allColumnNames = std::vector<std::string>({
        "ra",
        "dec",
        "parallax",
        "pmra",
        "pmdec",
        "phot_g_mean_mag",
        "phot_bp_mean_mag",
        "phot_rp_mean_mag",
        "radial_velocity",
        });
    // Append additional filter parameters to default rendering parameters.
    allColumnNames.insert(allColumnNames.end(), filterColumnNames.begin(),
        filterColumnNames.end());

    std::string allNames = "Columns to read: \n";
    for (auto colName : allColumnNames) {
        allNames += colName + "\n";
    }
    LINFO(allNames);

    // Read columns from FITS file. If rows aren't specified then full table will be read.
    auto table = readTable<float>(filePath, allColumnNames, firstRow, lastRow);

    if (!table) {
        throw ghoul::RuntimeError(std::format("Failed to open Fits file '{}'", filePath));
    }

    int nStars = table->readRows - firstRow + 1;

    int nNullArr = 0;
    size_t nColumnsRead = allColumnNames.size();
    size_t defaultCols = 9; // Number of columns that are copied by predefined code.
    if (nColumnsRead != defaultCols) {
        LINFO("Additional columns will be read! Consider add column in code for "
            "significant speedup");
    }
    // Declare how many values to save per star
    nValuesPerStar = 8;

    // Copy columns to local variables.
    std::unordered_map<std::string, std::vector<float>>& tableContent = table->contents;

    std::vector<float> ra = std::move(tableContent[allColumnNames[0]]);
    std::vector<float> dec = std::move(tableContent[allColumnNames[1]]);
    std::vector<float> parallax = std::move(tableContent[allColumnNames[2]]);
    std::vector<float> pmra = std::move(tableContent[allColumnNames[3]]);
    std::vector<float> pmdec = std::move(tableContent[allColumnNames[4]]);
    std::vector<float> meanMagG = std::move(tableContent[allColumnNames[5]]);
    std::vector<float> meanMagBp = std::move(tableContent[allColumnNames[6]]);
    std::vector<float> meanMagRp = std::move(tableContent[allColumnNames[7]]);
    std::vector<float> radial_vel = std::move(tableContent[allColumnNames[8]]);

    // Construct data array. OBS: ORDERING IS IMPORTANT! This is where slicing happens.
    for (int i = 0; i < nStars; i++) {
        std::vector<float> values(nValuesPerStar);
        size_t idx = 0;

        // Default order for rendering:
        // Position [X, Y, Z]
        // Mean G-band Magnitude
        // Bp-Rp Color
        // Velocity [X, Y, Z]

        // Return early if star doesn't have a measured position.
        if (std::isnan(ra[i]) || std::isnan(dec[i])) {
            nNullArr++;
            continue;
        }

        // Store positions. Set to a default distance if parallax doesn't exist.
        float radiusInKiloParsec = 9.0;
        if (!std::isnan(parallax[i])) {
            // Parallax is in milliArcseconds -> distance in kiloParsecs
            // https://gea.esac.esa.int/archive/documentation/GDR2/Gaia_archive/
            // chap_datamodel/sec_dm_main_tables/ssec_dm_gaia_source.html
            radiusInKiloParsec = 1.0 / parallax[i];
        }
        // Convert to Galactic Coordinates from Galactic Lon & Lat.
        // https://gea.esac.esa.int/archive/documentation/GDR2/Data_processing/
        // chap_cu3ast/sec_cu3ast_intro/ssec_cu3ast_intro_tansforms.html#SSS1
        //values[idx++] = radiusInKiloParsec * cos(glm::radians(b_latitude[i])) *
        //cos(glm::radians(l_longitude[i])); // Pos X
        //values[idx++] = radiusInKiloParsec * cos(glm::radians(b_latitude[i])) *
        //sin(glm::radians(l_longitude[i])); // Pos Y
        //values[idx++] = radiusInKiloParsec * sin(glm::radians(b_latitude[i])); // Pos Z


        // Convert ICRS Equatorial Ra and Dec to Galactic latitude and longitude.
        glm::mat3 aPrimG = glm::mat3(
            // Col 0
            glm::vec3(-0.0548755604162154, 0.4941094278755837, -0.8676661490190047),
            // Col 1
            glm::vec3(-0.8734370902348850, -0.4448296299600112, -0.1980763734312015),
            // Col 2
            glm::vec3(-0.4838350155487132, 0.7469822444972189, 0.4559837761750669)
        );
        glm::vec3 rICRS = glm::vec3(
            cos(glm::radians(ra[i])) * cos(glm::radians(dec[i])),
            sin(glm::radians(ra[i])) * cos(glm::radians(dec[i])),
            sin(glm::radians(dec[i]))
        );
        glm::vec3 rGal = aPrimG * rICRS;
        values[idx++] = radiusInKiloParsec * rGal.x; // Pos X
        values[idx++] = radiusInKiloParsec * rGal.y; // Pos Y
        values[idx++] = radiusInKiloParsec * rGal.z; // Pos Z

        // Store magnitude render value. (Set default to high mag = low brightness)
        values[idx++] = std::isnan(meanMagG[i]) ? 20.f : meanMagG[i]; // Mean G-band Mag

        // Store color render value. (Default value is bluish stars)
        values[idx++] = std::isnan(meanMagBp[i]) && std::isnan(meanMagRp[i]) ? 0.f
            : meanMagBp[i] - meanMagRp[i]; // Bp-Rp Color

        // Store velocity.
        if (std::isnan(pmra[i])) pmra[i] = 0.f;
        if (std::isnan(pmdec[i])) pmdec[i] = 0.f;

        // Convert Proper Motion from ICRS [Ra,Dec] to Galactic Tanget Vector [l,b].
        glm::vec3 uICRS = glm::vec3(
            -sin(glm::radians(ra[i])) * pmra[i] -
            cos(glm::radians(ra[i])) * sin(glm::radians(dec[i])) * pmdec[i],
            cos(glm::radians(ra[i])) * pmra[i] -
            sin(glm::radians(ra[i])) * sin(glm::radians(dec[i])) * pmdec[i],
            cos(glm::radians(dec[i]))  * pmdec[i]
        );
        glm::vec3 pmVecGal = aPrimG * uICRS;

        // Convert to Tangential vector [m/s] from Proper Motion vector [mas/yr]
        float tanVelX = 1000.0 * 4.74 * radiusInKiloParsec * pmVecGal.x;
        float tanVelY = 1000.0 * 4.74 * radiusInKiloParsec * pmVecGal.y;
        float tanVelZ = 1000.0 * 4.74 * radiusInKiloParsec * pmVecGal.z;

        // Calculate True Space Velocity [m/s] if we have the radial velocity
        if (!std::isnan(radial_vel[i])) {
            // Calculate Radial Velocity in the direction of the star.
            // radial_vel is given in [km/s] -> convert to [m/s].
            float radVelX = 1000.0 * radial_vel[i] * rGal.x;
            float radVelY = 1000.0 * radial_vel[i] * rGal.y;
            float radVelZ = 1000.0 * radial_vel[i] * rGal.z;

            // Use Pythagoras theorem for the final Space Velocity [m/s].
            values[idx++] = sqrt(pow(radVelX, 2) + pow(tanVelX, 2)); // Vel X [U]
            values[idx++] = sqrt(pow(radVelY, 2) + pow(tanVelY, 2)); // Vel Y [V]
            values[idx++] = sqrt(pow(radVelZ, 2) + pow(tanVelZ, 2)); // Vel Z [W]
        }
        // Otherwise use the vector [m/s] we got from proper motion.
        else {
            radial_vel[i] = 0.f;
            values[idx++] = tanVelX; // Vel X [U]
            values[idx++] = tanVelY; // Vel Y [V]
            values[idx++] = tanVelZ; // Vel Z [W]
        }

        fullData.insert(fullData.end(), values.begin(), values.end());
    }*/

    LINFO(std::format("{} out of {} read stars were null arrays", nNullArr, nStars));
    LINFO(std::format("Multiplier: {}", multiplier));

    return fullData;
}

std::vector<float> FitsFileReader::readSpeckFile(const std::filesystem::path& filePath,
                                                 int& nRenderValues)
{
    std::vector<float> fullData;

    std::ifstream fileStream(filePath);

    if (!fileStream.good()) {
        LERROR(std::format("Failed to open Speck file '{}'", filePath));
        return fullData;
    }

    int nValuesPerStar = 0;
    int nNullArr = 0;
    size_t nStars = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', 'texture' and 'maxcomment')
    std::string line;
    while (true) {
        const std::streampos position = fileStream.tellg();
        std::getline(fileStream, line);

        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 7) != "datavar" && line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture" && line.substr(0, 10) != "maxcomment")
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

    // Order in DR1 file:       DR2 - GaiaGroupMembers:
    // 0 BVcolor                0 color
    // 1 lum                    1 lum
    // 2 Vabsmag                2 absmag
    // 3 Vappmag                3 Gmag
    // 4 distly                 4 distpc
    // 5 distpcPctErr           5 plx
    // 6 U                      6 ra
    // 7 V                      7 dec
    // 8 W                      8 RadVel
    // 9 speed                  9 Teff
    // 10 sptypeindex           10 vx
    // 11 lumclassindex         11 vy
    // 12 catsource             12 vz
    // 13 texture               13 speed
    //                          14 texture

    do {
        std::vector<float> readValues(nValuesPerStar);
        nStars++;

        std::getline(fileStream, line);
        std::stringstream str(line);

        // Read values.
        for (int i = 0; i < nValuesPerStar; i++) {
            str >> readValues[i];
        }

        // Check if star is a nullArray.
        bool nullArray = true;
        for (const float f : readValues) {
            if (f != 0.f) {
                nullArray = false;
                break;
            }
        }

        // Insert to data if we found some values.
        if (!nullArray) {
            // Re-order data here because Octree expects the data in correct order when
            // read.
            // Default order for rendering:
            // Position [X, Y, Z]
            // Absolute Magnitude
            // B-V Color
            // Velocity [X, Y, Z]

            nRenderValues = 8;
            std::vector<float> renderValues(nRenderValues);

            // Gaia DR1 data from AMNH measures positions in Parsec, but
            // RenderableGaiaStars expects kiloParsec (because fits file from Vienna had
            // in kPc).
            // Thus we need to convert positions twice atm.
            renderValues[0] = readValues[0] / 1000.f; // PosX
            renderValues[1] = readValues[1] / 1000.f; // PosY
            renderValues[2] = readValues[2] / 1000.f; // PosZ
            renderValues[3] = readValues[6]; // AbsMag
            renderValues[4] = readValues[3]; // color
            renderValues[5] = readValues[13] * readValues[16]; // Vel X
            renderValues[6] = readValues[14] * readValues[16]; // Vel Y
            renderValues[7] = readValues[15] * readValues[16]; // Vel Z

            fullData.insert(fullData.end(), renderValues.begin(), renderValues.end());
        }
        else {
            nNullArr++;
        }

    } while (!fileStream.eof());

    LINFO(std::format("{} out of {} read stars were null arrays", nNullArr, nStars));

    return fullData;
}

// This is pretty annoying, the read method is not derived from the HDU class
// in CCfits - need to explicitly cast to the sub classes to access read
template<typename T>
std::shared_ptr<ImageData<T>> FitsFileReader::readImageInternal(ExtHDU& image) {
   try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {
            .contents = std::move(contents),
            .width = image.axis(0),
            .height = image.axis(1)
        };
        return std::make_shared<ImageData<T>>(im);
    }
   catch (const FitsException& e) {
        LERROR("Could not read FITS image EXTHDU. " + e.message());
    }
    return nullptr;
}

template<typename T>
std::shared_ptr<ImageData<T>> FitsFileReader::readImageInternal(PHDU& image) {
    try {
        std::valarray<T> contents;
        image.read(contents);
        ImageData<T> im = {
            .contents = std::move(contents),
            .width = image.axis(0),
            .height = image.axis(1)
        };
        return std::make_shared<ImageData<T>>(im);
    }
    catch (const FitsException& e) {
        LERROR("Could not read FITS image PHDU. " + e.message());
    }
    return nullptr;
}

} // namespace openspace
