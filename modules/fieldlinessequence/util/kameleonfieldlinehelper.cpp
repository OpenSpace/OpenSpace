/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <modules/kameleon/include/kameleonhelper.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

namespace {
    constexpr const char* _loggerCat = "FieldlinesSequence[ Kameleon ]";

    constexpr const char* TAsPOverRho = "T = p/rho";
    constexpr const char* JParallelB = "Current: mag(J||B)";
    constexpr const char* UPerpB = "u_perp_b";
    constexpr const char* U = "u";
    // [nPa]/[amu/cm^3] * ToKelvin => Temperature in Kelvin
    constexpr const float ToKelvin = 72429735.6984f;
} // namespace

namespace openspace::fls {

    // -------------------- DECLARE FUNCTIONS USED (ONLY) IN THIS FILE -------------------- //
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    bool addLinesToState(ccmc::Kameleon* kameleon, std::vector<glm::vec3>& seeds,
        const std::string& tracingVar, FieldlinesState& state);
    void addExtraQuantities(ccmc::Kameleon* kameleon,
        std::vector<std::string>& extraScalarVars, std::vector<std::string>& extraMagVars,
        FieldlinesState& state);
    void prepareStateAndKameleonForExtras(ccmc::Kameleon* kameleon,
        std::vector<std::string>& extraScalarVars, std::vector<std::string>& extraMagVars,
        FieldlinesState& state);
    void computeVelocities(std::vector<glm::vec3> path, std::vector<float>& velocities,
        ccmc::Kameleon* kameleon);
    void computeTimes(std::vector<glm::vec3> path, std::vector<float>& times,
        std::vector<float> velocities);
    glm::vec3 trimPathFindLastVertex(std::vector<glm::vec3>& path, std::vector<float>& times,
        std::vector<float>& velocities, float cdfLength);


#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
    // ------------------------------------------------------------------------------------ //

    /** Traces field lines from the provided cdf file using kameleon and stores the data in
     * the provided FieldlinesState.
     * Returns `false` if it fails to create a valid state. Requires the kameleon module to
     * be activated!
     * \param state, FieldlineState which should hold the extracted data
     * \param cdfPath, std::string of the absolute path to a .cdf file
     * \param seedPoints, vector of seed points from which to trace field lines
     * \param tracingVar, which quantity to trace lines from. Typically "b" for magnetic field
     *        lines and "u" for velocity flow lines
     * \param extraVars, extra scalar quantities to be stored in the FieldlinesState; e.g. "T"
     *        for temperature, "rho" for density or "P" for pressure
     * \param extraMagVars, variables which should be used for extracting magnitudes, must be
     *        a multiple of 3; e.g. "ux", "uy" & "uz" to get the magnitude of the velocity
     *        vector at each line vertex
     */

    void computeVelocities(std::vector<glm::vec3> path, std::vector<float>& velocities,
        ccmc::Kameleon* kameleon) {

        std::unique_ptr<ccmc::Interpolator> interpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

        for (const glm::vec3 p : path) {
            //compute u_perp_b with variables u and b
            //normalized b vector
            const glm::vec3 normBVec = glm::normalize(glm::vec3(
                interpolator->interpolate("bx", p.x, p.y, p.z),
                interpolator->interpolate("by", p.x, p.y, p.z),
                interpolator->interpolate("bz", p.x, p.y, p.z)));

            const glm::vec3 uVec = glm::vec3(
                interpolator->interpolate("ux", p.x, p.y, p.z),
                interpolator->interpolate("uy", p.x, p.y, p.z),
                interpolator->interpolate("uz", p.x, p.y, p.z));

            float u_dot_b = glm::dot(normBVec, uVec);

            //multiply by 1000 since the data is in km/s and openspace uses m/s
            glm::vec3 u_perp_b = (uVec - (normBVec * u_dot_b)) * 1000.0f;

            float magnitude = glm::length(u_perp_b);

            velocities.push_back(magnitude);
        }
    }

    void computeTimes(std::vector<glm::vec3> path, std::vector<float>& times, 
        std::vector<float> velocities) {

        for (int i = 0; i < path.size() - 1; i++) {

            //distance to next interpolation
            float d = glm::length(path[i + 1]*fls::ReToMeter - path[i]*fls::ReToMeter);

            //time to next interpolation = distance / velocity
            float time = d / velocities[i];

            times.push_back(time);
        }

    }

    glm::vec3 trimPathFindLastVertex(std::vector<glm::vec3>& path, std::vector<float>& times,
        std::vector<float>& velocities, float cdfLength) {
        float totalTime = 0.0f;
        for (int i = 0; i < times.size(); i++) {

            totalTime += times[i];
            if (totalTime > cdfLength) {
                //erase the remaining elements:
                path.erase(path.begin() + i, path.end());
                times.erase(times.begin() + i, times.end());
                velocities.erase(velocities.begin() + i, velocities.end());

                float remainder = totalTime - cdfLength;
                glm::vec3 lastVertex = path[i - 1] + (velocities[i - 1] / fls::ReToMeter)
                    * remainder;
                return lastVertex;
            }
        }
    }

    bool convertCdfToFieldlinesState(FieldlinesState& state, const std::string& cdfPath,
        std::vector<glm::vec3>& seedPoints,
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
        state.setTriggerTime(kameleonHelper::getTime(kameleon.get()));


        if (addLinesToState(kameleon.get(), seedPoints, tracingVar, state)) {
            // The line points are in their RAW format (unscaled & maybe spherical)
            // Before we scale to meters (and maybe cartesian) we must extract
            // the extraQuantites, as the iterpolator needs the unaltered positions
            //addExtraQuantities(kameleon.get(), extraVars, extraMagVars, state);
            switch (state.model()) {
            case fls::Model::Batsrus:
                //state.scalePositions(fls::ReToMeter);
                state.scaleflowline(fls::ReToMeter);
                state.scaleFieldlines(fls::ReToMeter);
                //state.computeTimes();
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
    bool addLinesToState(ccmc::Kameleon* kameleon, std::vector<glm::vec3>& seedPoints,
        const std::string& tracingVar, FieldlinesState& state) {

        float innerBoundaryLimit;

        switch (state.model()) {
        case fls::Model::Batsrus:
            innerBoundaryLimit = 0.5f;  // TODO specify in Lua?
            break;
        case fls::Model::Enlil:
            innerBoundaryLimit = 0.11f; // TODO specify in Lua?
            break;
        default:
            LERROR(
                "OpenSpace's fieldlines sequence currently only supports CDFs from the "
                "BATSRUS and ENLIL models!"
            );
            return false;
        }

        // ---------------------------- LOAD TRACING VARIABLE --------------------------//
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            return false;
        }

        // ---------------------------- LOAD TRACING VARIABLE --------------------------//
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            return false;
        }

        bool success = false;

        std::string mainTraceVar;
        std::string secondaryTraceVar;

        mainTraceVar = tracingVar;
        secondaryTraceVar = "b";
        float cdfLength = 60.0f;

        LINFO("Tracing field lines!");
        // LOOP  SEED POINTS, TRACE LINES, CONVERT POINTS TO glm::vec3 AND STORE //
        int i = 0;
        for (glm::vec3& seed : seedPoints) {
            float totalTime = 0.0f;
            //--------------------------------------------------------------------------//
            // We have to create a new tracer (or actually a new interpolator) for each //
            // new line, otherwise some issues occur                                    //
            //--------------------------------------------------------------------------//
            std::unique_ptr<ccmc::Interpolator> interpolator =
                std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
            ccmc::Tracer tracer(kameleon, interpolator.get());
            tracer.setInnerBoundary(innerBoundaryLimit); // TODO specify in Lua?

            //traces with "u" and "u_perp_b" use unidirectionalTrace, 
            //while "b" uses bidirectionalTrace
            ccmc::Fieldline pathline;

            pathline = tracer.unidirectionalTrace(
                mainTraceVar,
                seed.x,
                seed.y,
                seed.z);

            //necessary to map fieldlines to a certain amount of vertices
            pathline.getDs();
            pathline.measure();
            pathline.integrate();

            //change second arg to increase/decrease amount of interpolations.
            ccmc::Fieldline mappedPath = pathline.interpolate(1, 100);

            std::vector<ccmc::Point3f> positions = mappedPath.getPositions();

            const size_t nLinePoints = positions.size();

            std::vector<glm::vec3> path = {};
            std::vector<float> times = {};


            for (const ccmc::Point3f& p : positions) {
                path.emplace_back(p.component1, p.component2, p.component3);
            }
            std::vector<float> velocities;
            computeVelocities(path, velocities, kameleon);
            computeTimes(path, times, velocities);
            seed = trimPathFindLastVertex(path, times, velocities, cdfLength);
            state.addTimes(times, i);
            state.addPath(path, i);

            std::vector<int> closed = {};
            std::vector<int> open = {};
            int j = 0;
            std::vector<std::vector<glm::vec3>> interpolations = {};
            for (const glm::vec3& p : path) {
                std::vector<glm::vec3> vertices;

                //add vertices to path line
                //path.emplace_back(p.component1, p.component2, p.component3);

                std::unique_ptr<ccmc::Interpolator> interpolator2 =
                    std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
                ccmc::Tracer tracer2(kameleon, interpolator2.get());
                tracer2.setInnerBoundary(innerBoundaryLimit); // TODO specify in Lua?

                //traces with "u" or "u_perp_b" use unidirectioalTrace, while "b" uses 
                //bidirectionalTrace.
                ccmc::Fieldline fieldline;
                fieldline = tracer2.bidirectionalTrace(
                    secondaryTraceVar,
                    p.x,
                    p.y,
                    p.z
                );

                //necessary to map fieldlines to a certain amount of vertices
                fieldline.getDs();
                fieldline.measure();
                fieldline.integrate();
                ccmc::Fieldline mapped = fieldline.interpolate(1, 100);

                const std::vector<ccmc::Point3f>& positions2 = mapped.getPositions();
                for (const ccmc::Point3f& p2 : positions2) {
                    vertices.emplace_back(p2.component1, p2.component2, p2.component3);
                }
                //check if field line is open or closed
                if (glm::length(vertices.front()) < 1.5f) closed.push_back(j);
                else if (glm::length(vertices.back()) < 1.5f) open.push_back(j);

                interpolations.push_back(vertices);

                //we only want to render the first field line from the seed point
                //if (j == 0) {
                //    state.addLine(vertices);
                //}
                j++;
            }
            state.addOpenIndices(open, i);
            state.addClosedIndices(closed, i);
            state.addFieldLines(interpolations, i);
            //the flow line from the seed point, 
            //used to compute how fast the field lines move
            //state.addPath(path);

            success |= (nLinePoints > 0);
            i++;
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

        std::unique_ptr<ccmc::Interpolator> interpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

        //if (state.extraQuantityNames()[nXtraScalars] == "u_perp_b") {

        //    for (const std::vector<glm::vec3> v : state.vertexPath()) {
        //        std::vector<float> velocities;
        //        for (const glm::vec3 p : v) {
        //            //compute u_perp_b with variables u and b
        //            //normalized b vector
        //            const glm::vec3 normBVec = glm::normalize(glm::vec3(
        //                interpolator->interpolate("bx", p.x, p.y, p.z),
        //                interpolator->interpolate("by", p.x, p.y, p.z),
        //                interpolator->interpolate("bz", p.x, p.y, p.z)));

        //            const glm::vec3 uVec = glm::vec3(
        //                interpolator->interpolate("ux", p.x, p.y, p.z),
        //                interpolator->interpolate("uy", p.x, p.y, p.z),
        //                interpolator->interpolate("uz", p.x, p.y, p.z));

        //            float u_dot_b = glm::dot(normBVec, uVec);

        //            //multiply by 1000 since the data is in km/s and openspace uses m/s
        //            glm::vec3 u_perp_b = (uVec - (normBVec * u_dot_b)) * 1000.0f;

        //            float magnitude = glm::length(u_perp_b);

        //            velocities.push_back(magnitude);
        //        }
        //        state.addVelocities(velocities);
        //    }      
        //}
        
        // ------ Extract all the extraQuantities from kameleon and store in state! //
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

            for (size_t i = 0; i < nXtraMagnitudes; ++i) {
                const size_t idx = i * 3;

                const float x =
                    interpolator->interpolate(extraMagVars[idx], p.x, p.y, p.z);
                const float y =
                    interpolator->interpolate(extraMagVars[idx + 1], p.x, p.y, p.z);
                const float z =
                    interpolator->interpolate(extraMagVars[idx + 2], p.x, p.y, p.z);
                float val;
                // When looking at the current's magnitude in Batsrus, CCMC staff are
                // only interested in the magnitude parallel to the magnetic field
                if (state.extraQuantityNames()[nXtraScalars + i] == JParallelB) {
                    const glm::vec3 normMagnetic = glm::normalize(glm::vec3(
                        interpolator->interpolate("bx", p.x, p.y, p.z),
                        interpolator->interpolate("by", p.x, p.y, p.z),
                        interpolator->interpolate("bz", p.x, p.y, p.z)));
                    // Magnitude of the part of the current vector that's parallel to
                    // the magnetic field vector!
                    val = glm::dot(glm::vec3(x, y, z), normMagnetic);

                }
                else {
                    val = std::sqrt(x * x + y * y + z * z);
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
        for (int i = 0; i < static_cast<int>(extraScalarVars.size()); ++i) {
            std::string& str = extraScalarVars[i];
            bool success = kameleon->doesVariableExist(str) && kameleon->loadVariable(str);
            if (!success &&
                (model == fls::Model::Batsrus && (str == TAsPOverRho || str == "T")))
            {
                LDEBUG("BATSRUS doesn't contain variable T for temperature. Trying to "
                    "calculate it using the ideal gas law: T = pressure/density");
                constexpr const char* p = "p";
                constexpr const char* r = "rho";
                success = kameleon->doesVariableExist(p) && kameleon->loadVariable(p) &&
                    kameleon->doesVariableExist(r) && kameleon->loadVariable(r);
                str = TAsPOverRho;
            }
            if (!success) {
                LWARNING(fmt::format(
                    "Failed to load extra variable: '{}'. Ignoring", str
                ));
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
                const std::string& s2 = extraMagVars[i + 1];
                const std::string& s3 = extraMagVars[i + 2];

                bool success;
                std::string name = "Magnitude of (" + s1 + ", " + s2 + ", " + s3 + ")";
                //if extraMagVar is u_perp_b, variables u and b are needed
                if (s1 == "u_perp_b_x" && s2 == "u_perp_b_y" && s3 == "u_perp_b_z") {
                    success = kameleon->doesVariableExist("ux") &&
                        kameleon->doesVariableExist("uy") &&
                        kameleon->doesVariableExist("uz") &&
                        kameleon->doesVariableExist("bx") &&
                        kameleon->doesVariableExist("by") &&
                        kameleon->doesVariableExist("bz") &&
                        kameleon->loadVariable("ux") &&
                        kameleon->loadVariable("uy") &&
                        kameleon->loadVariable("uz") &&
                        kameleon->loadVariable("bx") &&
                        kameleon->loadVariable("by") &&
                        kameleon->loadVariable("bz");

                    if (success) name = UPerpB;
                }
                else if (s1 == "ux" && s2 == "uy" && s3 == "uz") {
                    success = kameleon->doesVariableExist("ux") &&
                        kameleon->doesVariableExist("uy") &&
                        kameleon->doesVariableExist("uz") &&
                        kameleon->loadVariable("ux") &&
                        kameleon->loadVariable("uy") &&
                        kameleon->loadVariable("uz");


                    if (success) name = U;
                }
                else {
                    success = kameleon->doesVariableExist(s1) &&
                        kameleon->doesVariableExist(s2) &&
                        kameleon->doesVariableExist(s3) &&
                        kameleon->loadVariable(s1) &&
                        kameleon->loadVariable(s2) &&
                        kameleon->loadVariable(s3);
                }
                
                if (success && model == fls::Model::Batsrus &&
                    s1 == "jx" && s2 == "jy" && s3 == "jz")
                {
                    // CCMC isn't really interested in the magnitude of current, but by the
                    // magnitude of the part of the current's vector that is parallel to the
                    // magnetic field => ensure that the magnetic variables are loaded
                    success = kameleon->doesVariableExist("bx") &&
                        kameleon->doesVariableExist("by") &&
                        kameleon->doesVariableExist("bz") &&
                        kameleon->loadVariable("bx") &&
                        kameleon->loadVariable("by") &&
                        kameleon->loadVariable("bz");
                    name = JParallelB;
                }

                if (!success) {
                    LWARNING(fmt::format(
                        "Failed to load at least one of the magnitude variables: {}, {}, {} "
                        "& {}. Removing ability to store corresponding magnitude",
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
            LWARNING(fmt::format(
                "Wrong number of variables provided for storing magnitudes. Expects multiple "
                "of 3 but {} are provided",
                extraMagVars.size()
            ));
        }
        state.setExtraQuantityNames(std::move(extraQuantityNames));
    }
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED



} // namespace openspace::fls
