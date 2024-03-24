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

#include <modules/gaia/tasks/readfilejob.h>

#include <openspace/util/distanceconversion.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

namespace {
    constexpr std::string_view _loggerCat = "ReadFileJob";
} // namespace

namespace openspace::gaia {

ReadFileJob::ReadFileJob(std::string filePath, std::vector<std::string> allColumns,
                         int firstRow, int lastRow, size_t nDefaultCols,
                         int nValuesPerStar, std::shared_ptr<FitsFileReader> fitsReader)
    : _inFilePath(std::move(filePath))
    , _firstRow(firstRow)
    , _lastRow(lastRow)
    , _nDefaultCols(nDefaultCols)
    , _nValuesPerStar(nValuesPerStar)
    , _allColumns(std::move(allColumns))
    , _fitsFileReader(std::move(fitsReader))
    , _octants(8)
{}

void ReadFileJob::execute() {
    // Read columns from FITS file. If rows aren't specified then full table will be read.
    const std::shared_ptr<TableData<float>> table = _fitsFileReader->readTable<float>(
        _inFilePath,
        _allColumns,
        _firstRow,
        _lastRow
    );

    if (!table) {
        throw ghoul::RuntimeError(
            std::format("Failed to open Fits file '{}'", _inFilePath
        ));
    }

    const int nStars = table->readRows - _firstRow + 1;

    const size_t nColumnsRead = _allColumns.size();
    if (nColumnsRead != _nDefaultCols) {
        LINFO(
            "Additional columns will be read! Consider add column in code for "
            "significant speedup"
        );
    }

    // Copy columns to local variables.
    std::unordered_map<std::string, std::vector<float>>& tableContent = table->contents;

    // Default columns parameters.
    //std::vector<float> l_longitude = std::move(tableContent[_allColumns[0]]);
    //std::vector<float> b_latitude = std::move(tableContent[_allColumns[1]]);
    std::vector<float> ra = std::move(tableContent[_allColumns[0]]);
    std::vector<float> ra_err = std::move(tableContent[_allColumns[1]]);
    std::vector<float> dec = std::move(tableContent[_allColumns[2]]);
    std::vector<float> dec_err = std::move(tableContent[_allColumns[3]]);
    std::vector<float> parallax = std::move(tableContent[_allColumns[4]]);
    std::vector<float> parallax_err = std::move(tableContent[_allColumns[5]]);
    std::vector<float> pmra = std::move(tableContent[_allColumns[6]]);
    std::vector<float> pmra_err = std::move(tableContent[_allColumns[7]]);
    std::vector<float> pmdec = std::move(tableContent[_allColumns[8]]);
    std::vector<float> pmdec_err = std::move(tableContent[_allColumns[9]]);
    std::vector<float> meanMagG = std::move(tableContent[_allColumns[10]]);
    std::vector<float> meanMagBp = std::move(tableContent[_allColumns[11]]);
    std::vector<float> meanMagRp = std::move(tableContent[_allColumns[12]]);
    std::vector<float> bp_rp = std::move(tableContent[_allColumns[13]]);
    std::vector<float> bp_g = std::move(tableContent[_allColumns[14]]);
    std::vector<float> g_rp = std::move(tableContent[_allColumns[15]]);
    std::vector<float> radial_vel = std::move(tableContent[_allColumns[16]]);
    std::vector<float> radial_vel_err = std::move(tableContent[_allColumns[17]]);


    // Construct data array. OBS: ORDERING IS IMPORTANT! This is where slicing happens.
    for (int i = 0; i < nStars; i++) {
        std::vector<float> values(_nValuesPerStar);
        size_t idx = 0;

        // Default order for rendering:
        // Position [X, Y, Z]
        // Mean G-band Magnitude
        // -- Mean Bp-band Magnitude
        // -- Mean Rp-band Magnitude
        // Bp-Rp Color
        // -- Bp-G Color
        // -- G-Rp Color
        // Velocity [X, Y, Z]

        // Return early if star doesn't have a measured position.
        if (std::isnan(ra[i]) || std::isnan(dec[i])) {
            continue;
        }

        // Store positions. Set to a default distance if parallax doesn't exist.
        float radiusInKiloParsec = 9.0;
        if (!std::isnan(parallax[i])) {
            // Parallax is in milliArcseconds -> distance in kiloParsecs
            // https://gea.esac.esa.int/archive/documentation/GDR2/Gaia_archive/
            // chap_datamodel/sec_dm_main_tables/ssec_dm_gaia_source.html
            //LINFO("Parallax: " + std::to_string(parallax[i]));
            radiusInKiloParsec = 1.f / parallax[i];
        }
        /*// Convert to Galactic Coordinates from Galactic Lon & Lat.
        // https://gea.esac.esa.int/archive/documentation/GDR2/Data_processing/
        // chap_cu3ast/sec_cu3ast_intro/ssec_cu3ast_intro_tansforms.html#SSS1
        values[idx++] = radiusInKiloParsec * cos(glm::radians(b_latitude[i])) *
            cos(glm::radians(l_longitude[i])); // Pos X
        values[idx++] = radiusInKiloParsec * cos(glm::radians(b_latitude[i])) *
            sin(glm::radians(l_longitude[i])); // Pos Y
        values[idx++] = radiusInKiloParsec * sin(glm::radians(b_latitude[i])); // Pos Z
        */

        // Convert ICRS Equatorial Ra and Dec to Galactic latitude and longitude.
        const glm::mat3 aPrimG = glm::mat3(
            // Col 0
            glm::vec3(-0.0548755604162154, 0.4941094278755837, -0.8676661490190047),
            // Col 1
            glm::vec3(-0.8734370902348850, -0.4448296299600112, -0.1980763734312015),
            // Col 2
            glm::vec3(-0.4838350155487132, 0.7469822444972189, 0.4559837761750669)
        );
        const glm::vec3 rICRS = glm::vec3(
            std::cos(glm::radians(ra[i])) * std::cos(glm::radians(dec[i])),
            std::sin(glm::radians(ra[i])) * std::cos(glm::radians(dec[i])),
            std::sin(glm::radians(dec[i]))
        );
        const glm::vec3 rGal = aPrimG * rICRS;
        values[idx++] = radiusInKiloParsec * rGal.x; // Pos X
        values[idx++] = radiusInKiloParsec * rGal.y; // Pos Y
        values[idx++] = radiusInKiloParsec * rGal.z; // Pos Z

        /*if (abs(rGal.x - values[0]) > 1e-5 || abs(rGal.y - values[1]) > 1e-5 ||
        abs(rGal.z - values[2]) > 1e-5) {
        LINFO("rGal: " + std::to_string(rGal) +
        " - LB: [" + std::to_string(values[0]) + ", " + std::to_string(values[1]) +
        ", " + std::to_string(values[2]) + "]");
        }*/

        // Store magnitude render value. (Set default to high mag = low brightness)
        values[idx++] = std::isnan(meanMagG[i]) ? 20.f : meanMagG[i]; // Mean G-band Mag

        // Store color render value. (Default value is bluish stars)
        values[idx++] = std::isnan(bp_rp[i]) ? 0.f : bp_rp[i]; // Bp-Rp Color


        // Store velocity.
        if (std::isnan(pmra[i])) {
            pmra[i] = 0.f;
        }
        if (std::isnan(pmdec[i])) {
            pmdec[i] = 0.f;
        }

        // Convert Proper Motion from ICRS [Ra,Dec] to Galactic Tanget Vector [l,b].
        const glm::vec3 uICRS = glm::vec3(
            -std::sin(glm::radians(ra[i])) * pmra[i] -
                std::cos(glm::radians(ra[i])) * std::sin(glm::radians(dec[i])) * pmdec[i],
            std::cos(glm::radians(ra[i])) * pmra[i] -
                std::sin(glm::radians(ra[i])) * std::sin(glm::radians(dec[i])) * pmdec[i],
            std::cos(glm::radians(dec[i]))  * pmdec[i]
        );
        const glm::vec3 pmVecGal = aPrimG * uICRS;

        // Convert to Tangential vector [m/s] from Proper Motion vector [mas/yr]
        const float tanVelX = 1000.f * 4.74f * radiusInKiloParsec * pmVecGal.x;
        const float tanVelY = 1000.f * 4.74f * radiusInKiloParsec * pmVecGal.y;
        const float tanVelZ = 1000.f * 4.74f * radiusInKiloParsec * pmVecGal.z;

        // Calculate True Space Velocity [m/s] if we have the radial velocity
        if (!std::isnan(radial_vel[i])) {
            // Calculate Radial Velocity in the direction of the star.
            // radial_vel is given in [km/s] -> convert to [m/s].
            const float radVelX = 1000.f * radial_vel[i] * rGal.x;
            const float radVelY = 1000.f * radial_vel[i] * rGal.y;
            const float radVelZ = 1000.f * radial_vel[i] * rGal.z;

            // Use Pythagoras theorem for the final Space Velocity [m/s].
            values[idx++] = static_cast<float>(
                std::sqrt(std::pow(radVelX, 2) + std::pow(tanVelX, 2)) // Vel X [U]
            );
            values[idx++] = static_cast<float>(
                std::sqrt(std::pow(radVelY, 2) + std::pow(tanVelY, 2)) // Vel Y [V]
            );
            values[idx++] = static_cast<float>(
                std::sqrt(std::pow(radVelZ, 2) + std::pow(tanVelZ, 2)) // Vel Z [W]
            );
        }
        // Otherwise use the vector [m/s] we got from proper motion.
        else {
            radial_vel[i] = 0.f;
            values[idx++] = tanVelX; // Vel X [U]
            values[idx++] = tanVelY; // Vel Y [V]
            values[idx++] = tanVelZ; // Vel Z [W]
        }

        // Store additional parameters to filter by.
        values[idx++] = std::isnan(meanMagBp[i]) ? 20.f : meanMagBp[i];
        values[idx++] = std::isnan(meanMagRp[i]) ? 20.f : meanMagRp[i];
        values[idx++] = std::isnan(bp_g[i]) ? 0.f : bp_g[i];
        values[idx++] = std::isnan(g_rp[i]) ? 0.f : g_rp[i];
        values[idx++] = ra[i];
        values[idx++] = std::isnan(ra_err[i]) ? 0.f : ra_err[i];
        values[idx++] = dec[i];
        values[idx++] = std::isnan(dec_err[i]) ? 0.f : dec_err[i];
        values[idx++] = std::isnan(parallax[i]) ? 0.f : parallax[i];
        values[idx++] = std::isnan(parallax_err[i]) ? 0.f : parallax_err[i];
        values[idx++] = pmra[i];
        values[idx++] = std::isnan(pmra_err[i]) ? 0.f : pmra_err[i];
        values[idx++] = pmdec[i];
        values[idx++] = std::isnan(pmdec_err[i]) ? 0.f : pmdec_err[i];
        values[idx++] = radial_vel[i];
        values[idx++] = std::isnan(radial_vel_err[i]) ? 0.f : radial_vel_err[i];

        // Read extra columns, if any. This will slow down the sorting tremendously!
        for (size_t col = _nDefaultCols; col < nColumnsRead; ++col) {
            std::vector<float> vecData = std::move(tableContent[_allColumns[col]]);
            values[idx++] = std::isnan(vecData[col]) ? 0.f : vecData[col];
        }

        size_t index = 0;
        if (values[0] < 0.0) {
            index += 1;
        }
        if (values[1] < 0.0) {
            index += 2;
        }
        if (values[2] < 0.0) {
            index += 4;
        }

        _octants[index].insert(_octants[index].end(), values.begin(), values.end());
    }
}

std::vector<std::vector<float>> ReadFileJob::product() {
    return _octants;
}

} // namespace openspace::gaia
