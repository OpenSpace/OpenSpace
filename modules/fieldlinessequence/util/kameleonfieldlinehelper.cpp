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

#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>

#include <modules/fieldlinessequence/util/commons.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <memory>

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef _MSC_VER
#pragma warning (push)
// Boost throws #pragma warning: there is no warning number '4675'
#pragma warning (disable : 4619)
#endif // _MSC_VER

#include <ccmc/Kameleon.h>
#include <ccmc/KameleonInterpolator.h>
#include <ccmc/Tracer.h>
#include <modules/kameleon/include/kameleonhelper.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

namespace {
    constexpr std::string_view _loggerCat = "FieldlinesSequence[ Kameleon ]";

    constexpr std::string_view TAsPOverRho = "T = p/rho";
    constexpr std::string_view JParallelB  = "Current: mag(J||B)";
    // [nPa]/[amu/cm^3] * ToKelvin => Temperature in Kelvin
    constexpr float ToKelvin = 72429735.6984f;
} // namespace

namespace openspace::fls {

// -------------------- DECLARE FUNCTIONS USED (ONLY) IN THIS FILE -------------------- //
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    bool addLinesToState(ccmc::Kameleon* kameleon, const std::vector<glm::vec3>& seeds,
        const std::string& tracingVar, FieldlinesState& state);
    void addExtraQuantities(ccmc::Kameleon* kameleon,
        std::vector<std::string>& extraScalarVars, std::vector<std::string>& extraMagVars,
        FieldlinesState& state);
    void prepareStateAndKameleonForExtras(ccmc::Kameleon* kameleon,
        std::vector<std::string>& extraScalarVars, std::vector<std::string>& extraMagVars,
        FieldlinesState& state);
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
// ------------------------------------------------------------------------------------ //

bool convertCdfToFieldlinesState(FieldlinesState& state, const std::string& cdfPath,
                                 const std::unordered_map<std::string,
                                 std::vector<glm::vec3>>& seedMap,
                                 double manualTimeOffset,
                                 const std::string& tracingVar,
                                 std::vector<std::string>& extraVars,
                                 std::vector<std::string>& extraMagVars)
{
#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
    LERROR("CDF inputs provided but Kameleon module is deactivated");
    return false;
#else // OPENSPACE_MODULE_KAMELEON_ENABLED
    // Create Kameleon object and open CDF file!
    std::unique_ptr<ccmc::Kameleon> kameleon = kameleonHelper::createKameleonObject(
        cdfPath
    );

    state.setModel(fls::stringToModel(kameleon->getModelName()));
    double cdfDoubleTime = kameleonHelper::getTime(kameleon.get(), manualTimeOffset);
    state.setTriggerTime(cdfDoubleTime);

    // get time as string.
    std::string cdfStringTime = SpiceManager::ref().dateFromEphemerisTime(
        cdfDoubleTime, "YYYYMMDDHRMNSC::RND"
    );

    // use time as string for picking seedpoints from seedm
    std::vector<glm::vec3> seedPoints = seedMap.at(cdfStringTime);
    bool success = addLinesToState(kameleon.get(), seedPoints, tracingVar, state);
    if (success) {
        // The line points are in their RAW format (unscaled & maybe spherical)
        // Before we scale to meters (and maybe cartesian) we must extract
        // the extraQuantites, as the iterpolator needs the unaltered positions
        addExtraQuantities(kameleon.get(), extraVars, extraMagVars, state);
        switch (state.model()) {
            case fls::Model::Batsrus:
                state.scalePositions(fls::ReToMeter);
                break;
            case fls::Model::Enlil:
                state.convertLatLonToCartesian(fls::AuToMeter);
                break;
            default:
                break;
        }

        return true;
    }

    return false;
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
}

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
/**
 * Traces and adds line vertices to state.
 * Vertices are not scaled to meters nor converted from spherical into cartesian
 * coordinates.
 * Note that extraQuantities will NOT be set!
 */
bool addLinesToState(ccmc::Kameleon* kameleon, const std::vector<glm::vec3>& seedPoints,
                     const std::string& tracingVar, FieldlinesState& state) {

    float innerBoundaryLimit;

    switch (state.model()) {
        case fls::Model::Batsrus:
            innerBoundaryLimit = 2.5f;  // TODO specify in Lua?
            break;
        case fls::Model::Enlil:
            innerBoundaryLimit = 0.11f; // TODO specify in Lua?
            break;
        default:
            LERROR(
                "OpenSpace's fieldlines sequence currently only supports CDFs from the "
                "BATSRUS and ENLIL models"
            );
            return false;
    }

    // ---------------------------- LOAD TRACING VARIABLE ---------------------------- //
    if (!kameleon->loadVariable(tracingVar)) {
        LERROR("Failed to load tracing variable: " + tracingVar);
        return false;
    }

    bool success = false;
    LINFO("Tracing field lines");
    // LOOP THROUGH THE SEED POINTS, TRACE LINES, CONVERT POINTS TO glm::vec3 AND STORE //
    for (const glm::vec3& seed : seedPoints) {
        //--------------------------------------------------------------------------//
        // We have to create a new tracer (or actually a new interpolator) for each //
        // new line, otherwise some issues occur                                    //
        //--------------------------------------------------------------------------//
        auto interpolator = std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
        ccmc::Tracer tracer(kameleon, interpolator.get());
        tracer.setInnerBoundary(innerBoundaryLimit); // TODO specify in Lua?
        ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace(
            tracingVar,
            seed.x,
            seed.y,
            seed.z
        );
        const std::vector<ccmc::Point3f>& positions = ccmcFieldline.getPositions();

        const size_t nLinePoints = positions.size();

        std::vector<glm::vec3> vertices;
        vertices.reserve(nLinePoints);
        for (const ccmc::Point3f& p : positions) {
            vertices.emplace_back(p.component1, p.component2, p.component3);
        }
        state.addLine(vertices);
        success |= (nLinePoints > 0);
    }

    return success;
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

/**
 * Loops through state's _vertexPositions and extracts corresponding 'extraQuantities'
 * from the kameleon object using a ccmc::interpolator.
 * Note that the positions MUST be unaltered (NOT scaled NOR converted to a different
 * coordinate system)!
 *
 * @param kameleon raw pointer to an already opened Kameleon object
 * @param extraScalarVars vector of strings. Strings should be names of a scalar
 * quantities to load into _extraQuantites; such as: "T" for temperature or "rho" for
 * density.
 * @param extraMagVars vector of strings. Size must be multiple of 3. Strings should be
 * names of the components needed to calculate magnitude. E.g. {"ux", "uy", "uz"} will
 * calculate: sqrt(ux*ux + uy*uy + uz*uz). Magnitude will be stored in _extraQuantities
 * @param state, The FieldlinesState which the extra quantities should be added to.
 */
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
void addExtraQuantities(ccmc::Kameleon* kameleon,
                        std::vector<std::string>& extraScalarVars,
                        std::vector<std::string>& extraMagVars,
                        FieldlinesState& state)
{

    prepareStateAndKameleonForExtras(kameleon, extraScalarVars, extraMagVars, state);

    const size_t nXtraScalars = extraScalarVars.size();
    const size_t nXtraMagnitudes = extraMagVars.size() / 3;

    auto interpolator = std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

    // ------ Extract all the extraQuantities from kameleon and store in state! ------ //
    for (const glm::vec3& p : state.vertexPositions()) {
        // Load the scalars!
        for (size_t i = 0; i < nXtraScalars; i++) {
            float val;
            if (extraScalarVars[i] == TAsPOverRho) {
                val = interpolator->interpolate("p", p.x, p.y, p.z);
                val *= ToKelvin;
                val /= interpolator->interpolate("rho", p.x, p.y, p.z);
            }
            else {
                val = interpolator->interpolate(extraScalarVars[i], p.x, p.y, p.z);

                // When measuring density in ENLIL CCMC multiply by the radius^2
                if (extraScalarVars[i] == "rho" && state.model() == fls::Model::Enlil) {
                    val *= std::pow(p.x * fls::AuToMeter, 2.0f);
                }
            }
            state.appendToExtra(i, val);
        }
        // Calculate and store the magnitudes!
        for (size_t i = 0; i < nXtraMagnitudes; i++) {
            const size_t idx = i*3;

            const float x = interpolator->interpolate(extraMagVars[idx]  , p.x, p.y, p.z);
            const float y = interpolator->interpolate(extraMagVars[idx+1], p.x, p.y, p.z);
            const float z = interpolator->interpolate(extraMagVars[idx+2], p.x, p.y, p.z);
            float val;
            // When looking at the current's magnitude in Batsrus, CCMC staff are
            // only interested in the magnitude parallel to the magnetic field
            if (state.extraQuantityNames()[nXtraScalars + i] == JParallelB) {
                const glm::vec3 normMagnetic =  glm::normalize(glm::vec3(
                        interpolator->interpolate("bx", p.x, p.y, p.z),
                        interpolator->interpolate("by", p.x, p.y, p.z),
                        interpolator->interpolate("bz", p.x, p.y, p.z)));
                // Magnitude of the part of the current vector that's parallel to
                // the magnetic field vector!
                val = glm::dot(glm::vec3(x,y,z), normMagnetic);

            }
            else {
                val = std::sqrt(x*x + y*y + z*z);
            }
            state.appendToExtra(i + nXtraScalars, val);
        }
    }
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

/** Validate the provided extra quantity variables -> load the data from the validated
 *  quantities into the kameleon object & add the quantity names into the state's
 *  _extraQuantityNames vector.
 *
 *  @param kameleon, raw pointer to an already opened kameleon object
 *  @param extraScalarVars, names of scalar quantities to add to state; e.g "rho" for
 *         density
 *  @param extraMagVars, names of the variables used for calculating magnitudes. Must be
 *         multiple of 3.
 */
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
void prepareStateAndKameleonForExtras(ccmc::Kameleon* kameleon,
                                      std::vector<std::string>& extraScalarVars,
                                      std::vector<std::string>& extraMagVars,
                                      FieldlinesState& state)
{
    std::vector<std::string> extraQuantityNames;
    fls::Model model = fls::stringToModel(kameleon->getModelName());

    // Load the existing SCALAR variables into kameleon.
    // Remove non-existing variables from vector
    for (int i = 0; i < static_cast<int>(extraScalarVars.size()); i++) {
        std::string& str = extraScalarVars[i];
        bool success = kameleon->doesVariableExist(str) && kameleon->loadVariable(str);
        if (!success &&
            (model == fls::Model::Batsrus &&
                (str == TAsPOverRho || str == "T" || str == "t"))
            )
        {
            LDEBUG(
                "BATSRUS doesn't contain variable T for temperature. Trying to calculate "
                "it using the ideal gas law: T = pressure/density"
            );
            constexpr const char p[] = "p";
            constexpr const char r[] = "rho";
            success = kameleon->doesVariableExist(p) && kameleon->loadVariable(p) &&
                      kameleon->doesVariableExist(r) && kameleon->loadVariable(r);
            str = TAsPOverRho;
        }
        if (!success) {
            LWARNING(std::format("Failed to load extra variable '{}'. Ignoring", str));
            extraScalarVars.erase(extraScalarVars.begin() + i);
            --i;
        }
        else {
           extraQuantityNames.push_back(str);
        }
    }

    // Load the existing magnitude variables (should be provided in multiple of 3)
    // into kameleon. Remove non-existing variables from vector
    if (extraMagVars.size() % 3 == 0) {
        for (size_t i = 0; i < extraMagVars.size(); i += 3) {
            const std::string& s1 = extraMagVars[i];
            const std::string& s2 = extraMagVars[i+1];
            const std::string& s3 = extraMagVars[i+2];
            bool success = kameleon->doesVariableExist(s1) &&
                           kameleon->doesVariableExist(s2) &&
                           kameleon->doesVariableExist(s3) &&
                           kameleon->loadVariable(s1) &&
                           kameleon->loadVariable(s2) &&
                           kameleon->loadVariable(s3);
            std::string name = "Magnitude of (" + s1 + ", " + s2 + ", " + s3 + ")";
            if (success && model == fls::Model::Batsrus &&
                s1 == "jx" && s2 == "jy" && s3 == "jz")
            {
                // CCMC isn't really interested in the magnitude of current, but by the
                // magnitude of the part of the current's vector that is parallel to the
                // magnetic field => ensure that the magnetic variables are loaded
                success =  kameleon->doesVariableExist("bx") &&
                           kameleon->doesVariableExist("by") &&
                           kameleon->doesVariableExist("bz") &&
                           kameleon->loadVariable("bx") &&
                           kameleon->loadVariable("by") &&
                           kameleon->loadVariable("bz");
                name = JParallelB;
            }
            if (!success) {
                LWARNING(std::format(
                    "Failed to load at least one of the magnitude variables: '{}', '{}', "
                    "'{}'. Removing ability to store corresponding magnitude",
                    s1, s2, s3
                ));
                extraMagVars.erase(
                    extraMagVars.begin() + i,
                    extraMagVars.begin() + i + 3
                );
                i -= 3;
            }
            else {
                extraQuantityNames.push_back(std::move(name));
            }
        }
    }
    else {
        // WRONG NUMBER OF MAGNITUDE VARIABLES.. REMOVE ALL!
        extraMagVars.clear();
        LWARNING(std::format(
            "Wrong number of variables provided for storing magnitudes. Expects multiple "
            "of 3 but {} are provided",
            extraMagVars.size()
        ));
    }
    state.setExtraQuantityNames(std::move(extraQuantityNames));
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

} // namespace openspace::fls
