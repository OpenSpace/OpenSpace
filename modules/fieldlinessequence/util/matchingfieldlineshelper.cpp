#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
#error "CDF inputs provided but Kameleon module is deactivated"
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#include <modules/fieldlinessequence/util/matchingfieldlineshelper.h>

#include <modules/fieldlinessequence/util/movingfieldlinehelper.cpp>
#include <modules/fieldlinessequence/util/commons.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>

#include <iostream>
#include <cmath>

namespace openspace::fls {

    // *** ALIASES ***
    using seedPointPair = std::pair<glm::vec3, glm::vec3>;

    // *** DECLARATIONS ***

    ccmc::Fieldline traceAndCreateMappedPathLine(const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const glm::vec3& seedPoint,
        const size_t nPointsOnPathLine,
        ccmc::Tracer::Direction direction = ccmc::Tracer::Direction::FOWARD);

    std::vector<glm::vec3> concatenatePathLines(
        const std::vector<ccmc::Point3f>& firstPart,
        const std::vector<ccmc::Point3f>& secondPart);

    std::vector<glm::vec3> convertPoint3fToVec3(
        const std::vector<ccmc::Point3f>& point3f);

    double calculateDistance(
        double x1,
        double y1,
        double z1,
        double x2,
        double y2,
        double z2
    );

    bool traceAndAddMatchingLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<seedPointPair>& matchingSeedPoints,
        const std::vector<double>& birthTimes,
        const std::string& tracingVar,
        FieldlinesState& state,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines
    );

    void traceAndCreateKeyFrame(std::vector<glm::vec3>& keyFrame,
        std::vector<float>& lengths,
        const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerbounds,
        size_t nPointsOnFieldlines
    );

    std::vector<Seedpoint> validateAndModifySeedPointsRecursive(
        int i,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        std::vector<Seedpoint>& seedPoint,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        double &accuracy,
        size_t& _nPointsOnFieldLine
    );

    void createTextFileWithFieldlineCoordinates(
        std::vector<glm::vec3> flowlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        std::string fileName
    );

    bool checkIfFieldlineIsOpen(
        std::vector<glm::vec3>& fieldlinePositions
    );

    bool checkIfFieldlineIsClosed(
        std::vector<glm::vec3>& fieldlinePositions
    );

    bool checkIfFieldlineIsIMF(
        std::vector<glm::vec3>& fieldlinePositions
    );

    bool keepCheckingFlowlinesFieldline(
        std::vector<glm::vec3>& flowlinePos,
       int& flowlineIndex
    );

    Seedpoint moveSeedpointTowardsCriticalPoint(
        Seedpoint& seedPoint,
        glm::vec3& directionToCriticalPoint,
        float& stepLength
    );

    Seedpoint moveSeedpointAwayFromCriticalPoint(
        Seedpoint& seedPoint,
        glm::vec3& directionToCriticalPoint,
        float& stepLength
    );

    std::vector<glm::vec3> getPositionsFromLine(
        ccmc::Fieldline& seedPointFlowline
    );

    glm::vec3 modifySeedpointOpen(
        Seedpoint& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        size_t& _nPointsOnFieldLine,
        glm::vec3& directionToCriticalPoint,
        float& stepLength,
        double& accuracy,
        bool moveAwayFromCP,
        Seedpoint& lastWorkingSeedpoint
    );

    glm::vec3 modifySeedpointClosed(
        Seedpoint& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        size_t& _nPointsOnFieldLine,
        glm::vec3& directionToCriticalPoint,
        float& stepLength,
        double& accuracy,
        bool closerToEarth,
        Seedpoint& lastWorkingSeedpoint
    );

    glm::vec3 modifySeedpointIMF(
        Seedpoint& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        size_t& _nPointsOnFieldLine,
        glm::vec3& directionToCriticalPoint,
        float& stepLength,
        double& accuracy,
        bool moveAwayFromCP,
        Seedpoint& lastWorkingSeedpoint
    );

    // *** DEFINITIONS ***

    bool openspace::fls::convertCdfToMatchingFieldlinesState(
        FieldlinesState& state,
        ccmc::Kameleon* kameleon,
        std::vector<Seedpoint>& seedPoints,
        const std::vector<double>& birthTimes,
        double manualTimeOffset,
        const std::string& tracingVar,
        std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldLines)
    {
        std::vector<seedPointPair> matchingSeedPoints;
        for (size_t i = 0; i < seedPoints.size(); i += 2) {
            matchingSeedPoints.push_back(
                { seedPoints[i].seedPoint, seedPoints[i + 1].seedPoint }
            );
        }

        bool isSuccessful = openspace::fls::traceAndAddMatchingLinesToState(
            kameleon,
            matchingSeedPoints,
            birthTimes,
            tracingVar,
            state,
            nPointsOnPathLine,
            nPointsOnFieldLines
        );

        return isSuccessful;
    }

    /*
    * Dayside algorithm
    */
    std::vector<Seedpoint> validateAndModifyAllSeedPoints(
        std::vector<Seedpoint>& seedPoints,
        const std::string& tracingVar,
        ccmc::Kameleon* kameleon,
        const size_t& nPointsOnPathLine,
        double& accuracy
    )
    {
        if (tracingVar != "u_perp_b") {
            std::cout << "aint working " << std::endl;
        }
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            std::cout << "aint working " << std::endl;
        }
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            std::cout << "aint working " << std::endl;
        }

        std::unique_ptr<ccmc::Interpolator> interpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
        ccmc::Tracer tracer(kameleon, interpolator.get());
        size_t _nPointsOnFieldLine = 2;
        float innerBoundaryLimit = 0.5f;
        int i = 0;

        seedPoints = validateAndModifySeedPointsRecursive(
            i,
            tracingVar,
            tracer,
            seedPoints,
            nPointsOnPathLine,
            kameleon,
            innerBoundaryLimit,
            accuracy,
            _nPointsOnFieldLine
        );

        return seedPoints;
    }

    /*
    * Reccursive method to validate and move seed points
    */
    std::vector<Seedpoint> validateAndModifySeedPointsRecursive(
        int i,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        std::vector<Seedpoint>& seedPoints,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        double& accuracy,
        size_t& _nPointsOnFieldLine
        )
    {
        if (i >= seedPoints.size())
        {
            return seedPoints;
        }
        else
        {
            std::cout
                << "****************************************"
                << std::endl;
            std::cout
                << "Seed point intended fieldline topology :"
                << seedPoints[i].topology << std::endl;
            std::cout
                << "Seed point original position :"
                << seedPoints[i].seedPoint.x << " "
                << seedPoints[i].seedPoint.y << " "
                << seedPoints[i].seedPoint.z << " "
                << std::endl;

            glm::vec3 directionToCriticalPoint;
            directionToCriticalPoint.x =
                seedPoints[i].criticalPoint.x - seedPoints[i].seedPoint.x;
            directionToCriticalPoint.y =
                seedPoints[i].criticalPoint.y - seedPoints[i].seedPoint.y;
            directionToCriticalPoint.z =
                seedPoints[i].criticalPoint.z - seedPoints[i].seedPoint.z;

            if (seedPoints[i].topology == "OPEN_NORTH" ||
                seedPoints[i].topology == "OPEN_SOUTH"
            )
            {
                std::cout
                    << std::endl
                    << seedPoints[i].topology << " Seed point validation start."
                    << std::endl;

                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    seedPoints[i].seedPoint,
                    nPointsOnPathLine,
                    ccmc::Tracer::Direction::FOWARD);

                std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(flowline);

                int counter = 1;
                while(keepCheckingFlowlinesFieldline(flowlinePositions, counter))
                {
                    std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                        flowlinePositions[counter],
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine
                    );


                    if (checkIfFieldlineIsClosed(fieldlinePositions))
                    {
                        float stepLength = 2;
                        Seedpoint lastWorkingSeedpoint;

                        std::cout << "CLOSED! - Move seed point" << std::endl;
                        glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            true,
                            lastWorkingSeedpoint
                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;

                        break;
                    }
                    else if(checkIfFieldlineIsIMF(fieldlinePositions))
                    {
                        float stepLength = 2;
                        Seedpoint lastWorkingSeedpoint;

                        std::cout << "IMF! - Move seed point closer to critical point" << std::endl;
                        glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            true,
                            lastWorkingSeedpoint
                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;

                        break;
                    }
                    else if (!checkIfFieldlineIsOpen(fieldlinePositions))
                    {
                        float stepLength = 2;
                        Seedpoint lastWorkingSeedpoint;

                        std::cout << "NOT OPEN! - Move seed point" << std::endl;
                        glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            true,
                            lastWorkingSeedpoint
                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;
                        break;
                    }
                    counter++;
                }
                std::cout
                    << seedPoints[i].topology
                    << ": seed point validation completed."
                    << std::endl << std::endl;
            }
            else if (seedPoints[i].topology == "CLOSED")
            {
                std::cout << std::endl << "CLOSED seed point validation start." << std::endl;

                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    seedPoints[i].seedPoint,
                    nPointsOnPathLine,
                    ccmc::Tracer::Direction::REVERSE);

                std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(flowline);

                int counter2 = 1;
                while (counter2 < nPointsOnPathLine &&
                    flowlinePositions[flowlinePositions.size() - counter2].x > -0.5)
                {
                    std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                        flowlinePositions[flowlinePositions.size() - counter2],
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine
                    );

                    if (!checkIfFieldlineIsClosed(fieldlinePositions))
                    {
                        float stepLength = 0.9;
                        Seedpoint lastWorkingSeedpoint;

                        glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            true,
                            lastWorkingSeedpoint
                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;
                        break;
                    }

                    /*
                    * else //If it is a Closed fieldline
                    {
                        float stepLength = 0.9;
                        Seedpoint lastWorkingSeedpoint;

                        glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            false,
                            lastWorkingSeedpoint
                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;
                        break;
                    }
                    */

                    counter2++;
                }

                std::cout
                    << seedPoints[i].topology
                    << ": seed point validation completed."
                    << std::endl << std::endl;

            }
            else if (seedPoints[i].topology == "IMF")
            {
                std::cout << std::endl << "IMF seed point validation start." << std::endl;

                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    seedPoints[i].seedPoint,
                    nPointsOnPathLine,
                    ccmc::Tracer::Direction::REVERSE);

                std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(flowline);
                int counter2 = 1;
                while (counter2 < (nPointsOnPathLine))
                {

                    std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                        flowlinePositions[flowlinePositions.size() - counter2],
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine
                    );

                    if (!checkIfFieldlineIsIMF(fieldlinePositions))
                    {
                        float stepLength = 0.5;
                        Seedpoint lastWorkingSeedpoint;

                        glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            false,
                            lastWorkingSeedpoint

                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;
                        break;
                    }
                    else
                    {
                        float stepLength = 0.5;
                        Seedpoint lastWorkingSeedpoint;

                        glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            directionToCriticalPoint,
                            stepLength,
                            accuracy,
                            true,
                            lastWorkingSeedpoint
                        );

                        seedPoints[i].seedPoint = modifiedSeedpoint;
                        break;
                    }
                    counter2++;
                }

                std::cout
                    << seedPoints[i].topology
                    << ": seed point validation completed."
                    << std::endl << std::endl;
            }

            seedPoints = validateAndModifySeedPointsRecursive(
                i + 1,
                tracingVar,
                tracer,
                seedPoints,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                accuracy,
                _nPointsOnFieldLine
            );
        }
    return seedPoints;
    }

    /*
    * Experimental method used for extracting positions of fieldlines to a .txt file
    * from a flowline
    */
    void createTextFileWithFieldlineCoordinates(
        std::vector<glm::vec3> flowlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        std::string fileName) {

        std::vector<std::vector<glm::vec3>> testFieldlinePositions;
        int flowlineIndex = 0;
        bool onlyDaysideLimit = false;

        if (onlyDaysideLimit)
        {
            if (fileName == "CLOSED_BEFORE.txt" || fileName == "CLOSED_AFTER.txt")
            {
                int counter2 = 1;
                while (counter2 < _nPointsOnFieldLine && flowlinePositions[flowlinePositions.size() - counter2].x > -0.5)
                {
                    std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                        flowlinePositions[flowlinePositions.size() - counter2],
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine
                    );
                    testFieldlinePositions.push_back(fieldlinePositions);

                    counter2++;
                }
            }
            else if (fileName == "IMF_BEFORE.txt" || fileName == "IMF_AFTER.txt")
            {
                int counter2 = 1;
                while (counter2 < _nPointsOnFieldLine)
                {
                    std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                        flowlinePositions[flowlinePositions.size() - counter2],
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine
                    );
                    testFieldlinePositions.push_back(fieldlinePositions);

                    counter2++;
                }
            }
            else
            {
                flowlineIndex = 0;
                while (flowlineIndex < flowlinePositions.size() && keepCheckingFlowlinesFieldline(flowlinePositions, flowlineIndex))
                {

                    std::vector<glm::vec3> fieldlinePositions2 = fls::getFieldlinePositions(
                        flowlinePositions[flowlineIndex],
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine
                    );

                    testFieldlinePositions.push_back(fieldlinePositions2);

                    flowlineIndex++;
                }
            }
        }
        else
        {
            for (int i = 0; i < flowlinePositions.size(); i++)
            {
                std::vector<glm::vec3> fieldlinePositions2 = fls::getFieldlinePositions(
                    flowlinePositions[i],
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine
                );
                testFieldlinePositions.push_back(fieldlinePositions2);
            }
        }

        std::ofstream output_file("C:/Dev/" + fileName);

        if (output_file.is_open()) {
            for (const auto& subvec : testFieldlinePositions) {
                for (const auto& vec : subvec) {
                    output_file << vec.x << " " << vec.y << " " << vec.z << " " << std::endl;
                }
                //output_file << std::endl << std::endl << "NEW FIELD LINE " << std::endl;
            }
            output_file.close();
        }
        else {
            std::cerr << "Unable to open file" << std::endl;
        }
    }

    /*
    * Check if fieldline has Open topology
    */
    bool checkIfFieldlineIsOpen(std::vector<glm::vec3>& fieldlinePositions)
    {
        std::pair<double, double> threshold_start_z = { std::make_pair(10.0, -10.0) };
        std::pair<double, double> threshold_end_z = { std::make_pair(-0.6, 0.6) };

        if (fieldlinePositions[0].z > threshold_start_z.first ||
            fieldlinePositions[0].z < threshold_start_z.second &&
            fieldlinePositions[fieldlinePositions.size() - 1].z > threshold_end_z.first &&
            fieldlinePositions[fieldlinePositions.size() - 1].z < threshold_end_z.second)
        {
            return true;

        } else if (fieldlinePositions[fieldlinePositions.size() - 1].z > threshold_start_z.first
            || fieldlinePositions[fieldlinePositions.size() - 1].z < threshold_start_z.second
            && fieldlinePositions[0].z > threshold_end_z.first
            && fieldlinePositions[0].z < threshold_end_z.second)
        {
            return true;
        }

        return false;
}

    /*
    * Check if fieldline has Closed topology
    */
    bool checkIfFieldlineIsClosed(std::vector<glm::vec3>& fieldlinePositions)
    {
        std::pair<double, double> threshold_end_z = { std::make_pair(-0.6, 0.6) };

        if (fieldlinePositions[0].z > threshold_end_z.first &&
            fieldlinePositions[0].z < threshold_end_z.second &&
            fieldlinePositions[fieldlinePositions.size() - 1].z > threshold_end_z.first &&
            fieldlinePositions[fieldlinePositions.size() - 1].z < threshold_end_z.second) {

            return true;
        }
        return false;
    }

    /*
    * Check if fieldline has IMF topology
    */
    bool checkIfFieldlineIsIMF(std::vector<glm::vec3>& fieldlinePositions)
    {
        std::pair<double, double> threshold_start_z = { std::make_pair(25.0, -25.0) };

        if (fieldlinePositions[0].z > threshold_start_z.first &&
            fieldlinePositions[fieldlinePositions.size() - 1].z < threshold_start_z.second) {

            return true;
        }
        else if (fieldlinePositions[0].z < threshold_start_z.second &&
            fieldlinePositions[fieldlinePositions.size() - 1].z > threshold_start_z.first)
        {
            return true;
        }

        return false;
    }

    /*
    * Move seed point along the direction of a vector, towards the critical point
    */
    Seedpoint moveSeedpointTowardsCriticalPoint(
        Seedpoint& seedPoint, glm::vec3& directionToCriticalPoint, float& stepLength)
    {
        try {
            seedPoint.seedPoint.x = seedPoint.seedPoint.x + directionToCriticalPoint.x * stepLength;
            seedPoint.seedPoint.y = seedPoint.seedPoint.y + directionToCriticalPoint.y * stepLength;
            seedPoint.seedPoint.z = seedPoint.seedPoint.z + directionToCriticalPoint.z * stepLength;

            std::cout << seedPoint.criticalPoint.x << " " << seedPoint.criticalPoint.y << " " << seedPoint.criticalPoint.z << " ";
            std::cout << "| " << seedPoint.seedPoint.x << " " << seedPoint.seedPoint.y << " " << seedPoint.seedPoint.z << " ";
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Move seed point along the direction of a vector, away from the critical point
    */
    Seedpoint moveSeedpointAwayFromCriticalPoint(
        Seedpoint& seedPoint, glm::vec3& directionToCriticalPoint, float& stepLength)
    {
        try {
            seedPoint.seedPoint.x = seedPoint.seedPoint.x - directionToCriticalPoint.x * stepLength;
            seedPoint.seedPoint.y = seedPoint.seedPoint.y - directionToCriticalPoint.y * stepLength;
            seedPoint.seedPoint.z = seedPoint.seedPoint.z - directionToCriticalPoint.z * stepLength;

            std::cout << seedPoint.criticalPoint.x << " " << seedPoint.criticalPoint.y << " " << seedPoint.criticalPoint.z << " " ;
            std::cout << "| " << seedPoint.seedPoint.x << " " << seedPoint.seedPoint.y << " " << seedPoint.seedPoint.z << " ";
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Checks if the position of the flowLine has gone past set criteria
    * If it has we do not have to check the rest of the fieldlines since these will not
    * be in the visualization due to the fade/transition to nightside seed points.
    */
    bool keepCheckingFlowlinesFieldline(
        std::vector<glm::vec3>& flowlinePos,
        int& flowlineIndex)
    {
        if (flowlinePos[flowlineIndex].x > - 2)
        {
            return true;
        }
        return false;
    }

    /*
    * Extract vector containing positions for a ccmc fieldline
    */
    std::vector<glm::vec3> getPositionsFromLine(ccmc::Fieldline& seedPointFlowline)
    {
        std::vector<ccmc::Point3f> seedPointFlowlinePositions
            = seedPointFlowline.getPositions();
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3
            = convertPoint3fToVec3(seedPointFlowlinePositions);
        return seedPointFlowlinePositionsVec3;
    }

    /*
    * Validate and move the on and os seed points
    */
    glm::vec3 modifySeedpointOpen(
        Seedpoint& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        size_t& _nPointsOnFieldLine,
        glm::vec3& directionToCriticalPoint,
        float& stepLength,
        double& accuracy,
        bool moveAwayFromCP,
        Seedpoint& lastWorkingSeedpoint)
    {
        std::vector<glm::vec3> fieldlinePositions;

        if (moveAwayFromCP)
        {
            seedPoint = moveSeedpointAwayFromCriticalPoint(
                seedPoint,
                directionToCriticalPoint,
                stepLength
            );
        }
        else if (!moveAwayFromCP)
        {
            seedPoint = moveSeedpointTowardsCriticalPoint(
                seedPoint,
                directionToCriticalPoint,
                stepLength
            );
        }

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.seedPoint,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::FOWARD);

        std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(seedPointFlowline);

        int flowlineIndex = 1;
        while (keepCheckingFlowlinesFieldline(flowlinePositions, flowlineIndex))
        {
            fieldlinePositions = fls::getFieldlinePositions(
                flowlinePositions[flowlineIndex],
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine
            );

            if (checkIfFieldlineIsClosed(fieldlinePositions))
            {
                stepLength = stepLength/2;

                if (stepLength < 0.001) {
                    seedPoint = lastWorkingSeedpoint;
                    break;
                }

                std::cout << "Closed fieldline: Traced none Open fieldline "
                    <<"- Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    directionToCriticalPoint,
                    stepLength,
                    accuracy,
                    true,
                    lastWorkingSeedpoint
                );
            } else if (checkIfFieldlineIsIMF(fieldlinePositions))
            {
                stepLength = stepLength / 2;

                if (stepLength < 0.001) {
                    seedPoint = lastWorkingSeedpoint;
                    break;
                }

                std::cout << "IMF: Traced none Open fieldline - Move seed point"
                    << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    directionToCriticalPoint,
                    stepLength,
                    accuracy,
                    false,
                    lastWorkingSeedpoint
                );

            } else if (!checkIfFieldlineIsOpen(fieldlinePositions))
            {
                stepLength = stepLength / 2;

                if (stepLength < 0.001) {
                    seedPoint = lastWorkingSeedpoint;
                    break;
                }

                std::cout << "Traced none Open fieldline - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    directionToCriticalPoint,
                    stepLength,
                    accuracy,
                    true,
                    lastWorkingSeedpoint
                );

            }
            flowlineIndex++;
        }

        if (stepLength > accuracy)
        {
            stepLength = stepLength / 2;
            lastWorkingSeedpoint = seedPoint;

            std::cout << "All fieldlines along flowline were approved on topology " <<
                " - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointOpen(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                directionToCriticalPoint,
                stepLength,
                accuracy,
                false,
                lastWorkingSeedpoint
            );
        }
        return seedPoint.seedPoint;
    }

    /*
    * Validate and move the closed seed point
    */
    glm::vec3 modifySeedpointClosed(
        Seedpoint& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        size_t& _nPointsOnFieldLine,
        glm::vec3& directionToCriticalPoint,
        float& stepLength,
        double& accuracy,
        bool moveAwayFromCP,
        Seedpoint& lastWorkingSeedpoint)
    {
        if (moveAwayFromCP)
        {
            seedPoint = moveSeedpointAwayFromCriticalPoint(
                seedPoint,
                directionToCriticalPoint,
                stepLength);
        }
        else if (!moveAwayFromCP)
        {
            seedPoint = moveSeedpointTowardsCriticalPoint(
                seedPoint,
                directionToCriticalPoint,
                stepLength);
        }

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.seedPoint,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::REVERSE);

        std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(seedPointFlowline);

        int counter2 = 1;
        while (
            counter2 < nPointsOnPathLine &&
            flowlinePositions[flowlinePositions.size() - counter2].x > -0.5
            )
        {
            std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                flowlinePositions[flowlinePositions.size()-counter2],
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine
            );

            if (!checkIfFieldlineIsClosed(fieldlinePositions))
            {
                stepLength = stepLength / 2;

                if (stepLength < 0.001){
                    seedPoint = lastWorkingSeedpoint;
                    break;
                }

                std::cout << "Traced none Closed fieldline - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    directionToCriticalPoint,
                    stepLength,
                    accuracy,
                    true,
                    lastWorkingSeedpoint
                );
            }
            counter2++;
        }

        if (stepLength > accuracy)
        {
            stepLength = stepLength / 2;
            lastWorkingSeedpoint = seedPoint;

            std::cout << "All fieldlines along flowline were approved on topology " <<
                " - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                directionToCriticalPoint,
                stepLength,
                accuracy,
                false,
                lastWorkingSeedpoint
            );
        }

        return seedPoint.seedPoint;
    }

    /*
    *  Validate and move IMF seed point
    */
    glm::vec3 modifySeedpointIMF(
        Seedpoint& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t& nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit,
        size_t& _nPointsOnFieldLine,
        glm::vec3& directionToCriticalPoint,
        float& stepLength,
        double& accuracy,
        bool moveAwayFromCP,
        Seedpoint& lastWorkingSeedpoint)
    {
        if (moveAwayFromCP)
        {
            seedPoint = moveSeedpointAwayFromCriticalPoint(
                seedPoint,
                directionToCriticalPoint,
                stepLength
            );
        }
        else if (!moveAwayFromCP)
        {
            seedPoint = moveSeedpointTowardsCriticalPoint(
                seedPoint,
                directionToCriticalPoint,
                stepLength
            );
        }

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.seedPoint,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::REVERSE);

        std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(seedPointFlowline);

        int counter2 = 1;
        while (counter2 < nPointsOnPathLine)
        {
            std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                flowlinePositions[flowlinePositions.size() - counter2],
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine
            );

            if (!checkIfFieldlineIsIMF(fieldlinePositions))
            {
                stepLength = stepLength / 2;
                if (stepLength < 0.001) {
                    seedPoint = lastWorkingSeedpoint;
                    break;
                }

                std::cout << "Traced none IMF - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    directionToCriticalPoint,
                    stepLength,
                    accuracy,
                    false,
                    lastWorkingSeedpoint
                );
            }
            counter2++;
        }

        if (stepLength > accuracy)
        {
            stepLength = stepLength / 2;
            lastWorkingSeedpoint = seedPoint;

            std::cout << "All fieldlines along flowline were approved on topology " <<
                " - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                directionToCriticalPoint,
                stepLength,
                accuracy,
                true,
                lastWorkingSeedpoint
            );
        }
        return seedPoint.seedPoint;
    }

    /*
    * Identify the flowline[index] where the safe tracing switches from ON to IMF and
    * return that index
    */
    int findFirstIMFIndex(
        std::vector<glm::vec3>& flowlinePositions,
        int startIndex,
        ccmc::Kameleon* kameleon,
        float& innerBoundaryLimit
    )
    {
        std::string previousTopology = "";
        int indexOfFirstIMF = 0;

        while (startIndex < flowlinePositions.size() - 1)
        {
            std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                flowlinePositions[startIndex + 1],
                kameleon,
                innerBoundaryLimit,
                2
            );

            if (checkIfFieldlineIsIMF(fieldlinePositions))
            {
                if (previousTopology == "OPEN_NORTH")
                {
                    indexOfFirstIMF = startIndex;
                    break;
                }
                else {
                    previousTopology = "OPEN_NORTH";
                }
            }
            startIndex++;
        }
        return indexOfFirstIMF;
    }

    /*
    * Validate and move seed points on the nightside
    */
    bool validateAndMoveSeedPoint
    (
        glm::vec3& seedPoint,
        glm::vec3& directionVector,
        double& factor,
        ccmc::Kameleon* kameleon,
        const std::string& tracingVar,
        const size_t& nPointsOnPathLine,
        float& innerBoundaryLimit,
        ccmc::Tracer& tracer,
        std::string topology
    )
    {
        bool validMovement = true;
        glm::vec3 tempSeedPoint;

        tempSeedPoint.x = seedPoint.x - directionVector.x * factor;
        tempSeedPoint.y = seedPoint.y - directionVector.y * factor;
        tempSeedPoint.z = seedPoint.z - directionVector.z * factor;

        std::cout << seedPoint.x << " " << seedPoint.y << " " << seedPoint.z << std::endl;

        int counter;

        if (topology == "IMF")
        {
            ccmc::Fieldline tempFlowline = traceAndCreateMappedPathLine(
                tracingVar,
                tracer,
                tempSeedPoint,
                200,
                ccmc::Tracer::Direction::FOWARD);

            std::vector<glm::vec3> tempflowlinePositions
                = getPositionsFromLine(tempFlowline);

            counter = 0;
            while (counter < tempflowlinePositions.size())
            {
                std::vector<glm::vec3> imfFieldlinePos = fls::getFieldlinePositions(
                    tempflowlinePositions[counter],
                    kameleon,
                    innerBoundaryLimit,
                    2
                );

                if (!checkIfFieldlineIsIMF(imfFieldlinePos))
                {
                    return false;
                }

                counter++;
            }
        }
        else if (topology == "OPEN_NORTH" || topology == "OPEN_SOUTH")
        {
            ccmc::Fieldline tempFlowline = traceAndCreateMappedPathLine(
                tracingVar,
                tracer,
                tempSeedPoint,
                200,
                ccmc::Tracer::Direction::REVERSE);

            std::vector<glm::vec3> tempflowlinePositions
                = getPositionsFromLine(tempFlowline);

            counter = tempflowlinePositions.size() - 1;
            while (counter > 0)
            {
                std::vector<glm::vec3> imfFieldlinePos = fls::getFieldlinePositions(
                    tempflowlinePositions[counter],
                    kameleon,
                    innerBoundaryLimit,
                    2
                );

                if (topology == "OPEN_NORTH")
                {
                    if (!checkIfFieldlineIsOpen(imfFieldlinePos))
                    {
                        return false;
                    }
                }
                else if (topology == "OPEN_SOUTH")
                {
                    if (!checkIfFieldlineIsOpen(imfFieldlinePos))
                    {
                        return false;
                    }
                }
                counter--;
            }
        }
        else if (topology == "CLOSED")
        {
            std::vector<glm::vec3> fieldlinePos = fls::getFieldlinePositions(
                tempSeedPoint,
                kameleon,
                innerBoundaryLimit,
                2
            );

            if (checkIfFieldlineIsClosed(fieldlinePos))
            {
                ccmc::Fieldline tempFlowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    tempSeedPoint,
                    200,
                    ccmc::Tracer::Direction::FOWARD);

                std::vector<glm::vec3> tempflowlinePositions
                    = getPositionsFromLine(tempFlowline);

                counter = 0;
                while (counter < tempflowlinePositions.size())
                {
                    std::vector<glm::vec3> fieldlinePosFL = fls::getFieldlinePositions(
                        tempflowlinePositions[counter],
                        kameleon,
                        innerBoundaryLimit,
                        2
                    );

                    if (tempflowlinePositions[counter].x > 0)
                    {
                        seedPoint.x = tempSeedPoint.x;
                        seedPoint.y = tempSeedPoint.y;
                        seedPoint.z = tempSeedPoint.z;
                        return false;
                    }

                    if (!checkIfFieldlineIsClosed(fieldlinePosFL))
                    {
                        std::cout << counter << std::endl;
                        break;
                    }

                    counter++;
                }
                if (counter == tempflowlinePositions.size())
                {
                    seedPoint.x = tempSeedPoint.x;
                    seedPoint.y = tempSeedPoint.y;
                    seedPoint.z = tempSeedPoint.z;
                    return false;
                }
            }
        }

        seedPoint.x = tempSeedPoint.x;
        seedPoint.y = tempSeedPoint.y;
        seedPoint.z = tempSeedPoint.z;
        return true;
    }

    /*
    * Calculate the distance between two points in 3d space
    */
    double calculateDistance(
        double x1, double y1, double z1,
        double x2, double y2, double z2
    )
    {
        double dx = x2 - x1;
        double dy = y2 - y1;
        double dz = z2 - z1;
        return std::sqrt(dx * dx + dy * dy + dz * dz);
    }

    /*
    * Nightside seed point finder
    */
    std::vector<Seedpoint> findAndAddNightsideSeedPoints(
        std::vector<Seedpoint>& seedPoints,
        std::vector<double>& birthTimes,
        double startTime,
        ccmc::Kameleon* kameleon,
        const std::string& tracingVar,
        const size_t nPointsOnPathLine)
    {
        std::vector<Seedpoint> nightsideSeedPoints;
        glm::vec3 imfSeedPointNightside;
        glm::vec3 closedSeedPointNightside;
        glm::vec3 onSeedPointNightside;
        glm::vec3 osSeedPointNightside;
        bool validMovement;

        if (tracingVar != "u_perp_b") {
            std::cout << "aint working " << std::endl;
        }
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            std::cout << "aint working " << std::endl;
        }
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            std::cout << "aint working " << std::endl;
        }

        float innerBoundaryLimit = 0.5f;
        std::unique_ptr<ccmc::Interpolator> interpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
        ccmc::Tracer tracer(kameleon, interpolator.get());
        int nPointsOnFlowline = 20000;

        std::cout << "Finding last ON and first IMF" << std::endl;

        // go through each seed point
        for (int i = 0; i < seedPoints.size(); i++)
        {
            // for each seed point with the topology open north
            if (seedPoints[i].topology == "OPEN_NORTH")
            {
                // lets try and find corresponding nightside seed points

                // first, lets trace the open north seed point fieldline
                std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                    seedPoints[i].seedPoint,
                    kameleon,
                    innerBoundaryLimit,
                    100
                );

                // now, lets take a point close to the edge of that fieldline WEAK SPOT
                glm::vec3 firstPosOfFieldline = fieldlinePositions[1];

                // trace a flowline from the new point/edge position
                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    firstPosOfFieldline,
                    nPointsOnFlowline,
                    ccmc::Tracer::Direction::FOWARD);

                // get all the positions of the traced flowline
                std::vector<glm::vec3> flowlinePositions
                    = getPositionsFromLine(flowline);

                // find the last open north fieldline and first IMF of the traced flowline
                std::vector<std::vector<glm::vec3>> lastON;
                std::vector<std::vector<glm::vec3>> firstIMF;

                // optimiezstar
                int n_points_on_flowline = 20;
                // trace a flowline from the new point/edge position
                ccmc::Fieldline flowline2 = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    firstPosOfFieldline,
                    n_points_on_flowline,
                    ccmc::Tracer::Direction::FOWARD);

                std::vector<glm::vec3> flowlinePositions2
                    = getPositionsFromLine(flowline2);

                double indexFlowlinePos2 = findFirstIMFIndex(flowlinePositions2, 0, kameleon, innerBoundaryLimit);
                double referencePercentage = indexFlowlinePos2 / (flowlinePositions2.size() + 3);
                int indexFlowlinePos = referencePercentage * flowlinePositions.size();

                std::cout << "Finding reference index - Completed" << std::endl;

                indexFlowlinePos = findFirstIMFIndex(flowlinePositions, indexFlowlinePos, kameleon, innerBoundaryLimit);

                std::vector<glm::vec3> fieldlinePositionsIMF = fls::getFieldlinePositions(
                    flowlinePositions[indexFlowlinePos],
                    kameleon,
                    innerBoundaryLimit,
                    nPointsOnFlowline
                );

                std::vector<glm::vec3> fieldlinePositionsON = fls::getFieldlinePositions(
                    flowlinePositions[indexFlowlinePos - 1],
                    kameleon,
                    innerBoundaryLimit,
                    nPointsOnFlowline
                );

                lastON.push_back(fieldlinePositionsON);
                firstIMF.push_back(fieldlinePositionsIMF);

                std::cout << fieldlinePositionsON[0].z << " " << fieldlinePositionsON[fieldlinePositionsON.size() - 1].z << std::endl;
                std::cout << fieldlinePositionsIMF[0].z << " " << fieldlinePositionsIMF[fieldlinePositionsIMF.size() - 1].z << std::endl;

                std::cout << "Finding last ON and first IMF - Complete" << std::endl;
                std::cout << "Finding IMF Nightside Seed Point" << std::endl;

                float leastXValue = -1000;

                for (int i = 0; i < firstIMF.size(); i++)
                {
                    for (int j = 0; j < nPointsOnFlowline; j++)
                    {
                        if (firstIMF[i][j].x > leastXValue)
                        {
                            leastXValue = firstIMF[i][j].x;
                            imfSeedPointNightside = firstIMF[i][j];
                        }
                    }
                }

                std::cout << "Finding IMF Nightside Seed Point - Complete" << std::endl;
                std::cout << "Finding ON Nightside Seed Point" << std::endl;

                double shortestDistance = 1000;

                for (int i = 0; i < lastON.size(); i++)
                {
                    for (int j = 0; j < nPointsOnFlowline; j++)
                    {
                        double distance = calculateDistance(
                            lastON[i][j].x,
                            lastON[i][j].y,
                            lastON[i][j].z,
                            imfSeedPointNightside.x,
                            imfSeedPointNightside.y,
                            imfSeedPointNightside.z
                        );

                        if (distance < shortestDistance)
                        {
                            shortestDistance = distance;
                            onSeedPointNightside = lastON[i][j];
                        }
                    }
                }

                std::cout << "Finding ON Nightside Seed Point - Complete" << std::endl;
                std::cout << "Finding OS Nightside Seed Point" << std::endl;

                osSeedPointNightside = onSeedPointNightside;
                osSeedPointNightside.z = onSeedPointNightside.z * -1;

                std::cout << "Finding OS Nightside Seed Point - Complete" << std::endl;
                std::cout << "Finding Closed Nightside Seed Point" << std::endl;

                double factor = 0.02;

                glm::vec3 earthPos;
                earthPos.x = 0;
                earthPos.y = 0;
                earthPos.z = 0;

                glm::vec3 vectorIE;
                vectorIE.x = imfSeedPointNightside.x - earthPos.x;
                vectorIE.y = imfSeedPointNightside.y - earthPos.y;
                vectorIE.z = imfSeedPointNightside.z - earthPos.z;

                closedSeedPointNightside.x = imfSeedPointNightside.x - vectorIE.x * 0.07;
                closedSeedPointNightside.y = imfSeedPointNightside.y - vectorIE.y * 0.07;
                closedSeedPointNightside.z = imfSeedPointNightside.z - vectorIE.z * 0.07;

                bool moveIMF = true;
                bool moveClosed = true;
                bool moveOS = true;
                bool moveON = true;

                moveClosed = validateAndMoveSeedPoint(
                    closedSeedPointNightside,
                    vectorIE,
                    factor,
                    kameleon,
                    tracingVar,
                    nPointsOnPathLine,
                    innerBoundaryLimit,
                    tracer,
                    "CLOSED"
                );

                while (closedSeedPointNightside.x < 0 && moveClosed)
                {
                    if (moveIMF)
                    {
                        moveIMF = validateAndMoveSeedPoint(
                            imfSeedPointNightside,
                            vectorIE,
                            factor,
                            kameleon,
                            tracingVar,
                            nPointsOnPathLine,
                            innerBoundaryLimit,
                            tracer,
                            "IMF"
                        );
                    }
                    if (moveOS)
                    {
                        moveOS = validateAndMoveSeedPoint(
                            osSeedPointNightside,
                            vectorIE,
                            factor,
                            kameleon,
                            tracingVar,
                            nPointsOnPathLine,
                            innerBoundaryLimit,
                            tracer,
                            "OPEN_SOUTH"
                        );
                    }
                    if (moveON)
                    {
                        moveON = validateAndMoveSeedPoint(
                            onSeedPointNightside,
                            vectorIE,
                            factor,
                            kameleon,
                            tracingVar,
                            nPointsOnPathLine,
                            innerBoundaryLimit,
                            tracer,
                            "OPEN_NORTH"
                        );
                    }

                    moveClosed = validateAndMoveSeedPoint(
                        closedSeedPointNightside,
                        vectorIE,
                        factor,
                        kameleon,
                        tracingVar,
                        nPointsOnPathLine,
                        innerBoundaryLimit,
                        tracer,
                        "CLOSED"
                    );
                }

                glm::vec3 weDontKnowThePositionOfTheCP;
                nightsideSeedPoints.push_back(
                    { onSeedPointNightside, "OPEN_NORTH", weDontKnowThePositionOfTheCP }
                );
                nightsideSeedPoints.push_back(
                    { osSeedPointNightside, "OPEN_SOUTH", weDontKnowThePositionOfTheCP }
                );
                nightsideSeedPoints.push_back(
                    { imfSeedPointNightside, "IMF", weDontKnowThePositionOfTheCP }
                );
                nightsideSeedPoints.push_back(
                    { closedSeedPointNightside, "CLOSED", weDontKnowThePositionOfTheCP }
                );
            }
        }
        for (int i = 0; i < nightsideSeedPoints.size(); i++)
        {
            seedPoints.push_back(nightsideSeedPoints[i]);
            if ((i + 1) % 4 == 0)
            {
                birthTimes.push_back(startTime + 1000);
            }
        }
        return nightsideSeedPoints;
    }

    /**
    * Uses the tracer to trace and create a ccmc::Fieldline and returns.
    * Default direction is forward tracing
    */
    ccmc::Fieldline traceAndCreateMappedPathLine(const std::string& tracingVar,
        ccmc::Tracer &tracer,
        const glm::vec3& seedPoint,
        const size_t nPointsOnPathLine,
        ccmc::Tracer::Direction direction) {


        ccmc::Fieldline uPerpBPathLine;
        uPerpBPathLine = tracer.unidirectionalTrace(
            tracingVar,
            seedPoint.x,
            seedPoint.y,
            seedPoint.z,
            direction
        );

        if (direction == ccmc::Tracer::Direction::REVERSE) {
            uPerpBPathLine = uPerpBPathLine.reverseOrder();
        }

        uPerpBPathLine.getDs();
        uPerpBPathLine.measure();
        uPerpBPathLine.integrate();

        ccmc::Fieldline mappedPath = uPerpBPathLine.interpolate(1, nPointsOnPathLine);

        return mappedPath;
    }

    /**
    * Concatenates the two vectors of pathline vertices into a new vector.
    * Converts from point3f to glm::vec3.
    */
    std::vector<glm::vec3> concatenatePathLines(
        const std::vector<ccmc::Point3f>& firstPart,
        const std::vector<ccmc::Point3f>& secondPart) {

        std::vector<glm::vec3> concatenated;
        for (const ccmc::Point3f& p : firstPart) {
            concatenated.emplace_back(p.component1, p.component2, p.component3);
        }
        for (const ccmc::Point3f& p : secondPart) {
            concatenated.emplace_back(p.component1, p.component2, p.component3);
        }

        return concatenated;
    }

    /**
    * Concatenates the two vectors of pathline vertices into a new vector.
    * Converts from point3f to glm::vec3.
    */
    std::vector<glm::vec3> convertPoint3fToVec3(
        const std::vector<ccmc::Point3f>& point3f) {

        std::vector<glm::vec3> vec3;
        for (const ccmc::Point3f& p : point3f) {
            vec3.emplace_back(p.component1, p.component2, p.component3);
        }

        return vec3;
    }

    bool traceAndAddMatchingLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<seedPointPair>& matchingSeedPoints,
        const std::vector<double>& birthTimes,
        const std::string& tracingVar,
        FieldlinesState& state,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines) {

        float innerBoundaryLimit;   // Defines the endpoint distance from Earth's center
        switch (state.model()) {
        case fls::Model::Batsrus:
            innerBoundaryLimit = 0.5f;  // TODO specify in Lua?
            break;
        default:
            LERROR(
                "OpenSpace's moving fieldlines currently only supports CDFs "
                "from the BATSRUS model"
            );
            return false;
        }

        // For each seedpoint, one line gets created, tracked with u perpendicular b.
        // then for each, and at each, vertex on that pathline, fieldlines are tracked
        if (tracingVar != "u_perp_b") {
            return false;
        }
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            return false;
        }
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            return false;
        }


        for (size_t i = 0; i < matchingSeedPoints.size() / 2; i++) {
            std::unique_ptr<ccmc::Interpolator> interpolator =
                std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
            ccmc::Tracer tracer(kameleon, interpolator.get());
            tracer.setInnerBoundary(innerBoundaryLimit);

            // Create pathlines (IMF and CF) for matching fieldlines
            // 11 is first part of first path line, 12 is the second part.
            // same for the second path line with 21 and 22
            size_t firstSeedId = i * 2;
            size_t secondSeedId = i * 2 + 1;
            ccmc::Fieldline mappedPath11 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[firstSeedId].first, nPointsOnPathLine, ccmc::Tracer::Direction::REVERSE);
            ccmc::Fieldline mappedPath12 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[secondSeedId].first, nPointsOnPathLine);

            ccmc::Fieldline mappedPath21 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[firstSeedId].second, nPointsOnPathLine, ccmc::Tracer::Direction::REVERSE);
            ccmc::Fieldline mappedPath22 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[secondSeedId].second, nPointsOnPathLine);


            // Get the vertex positions from the mapped pathlines
            std::vector<ccmc::Point3f> pathPositions11 = mappedPath11.getPositions();
            std::vector<ccmc::Point3f> pathPositions12 = mappedPath12.getPositions();

            std::vector<ccmc::Point3f> pathPositions21 = mappedPath21.getPositions();
            std::vector<ccmc::Point3f> pathPositions22 = mappedPath22.getPositions();

            // compute time
            size_t lengthToConcatenation1 = pathPositions11.size();
            size_t lengthToConcatenation2 = pathPositions21.size();

            // Here we concatenate the pathline pairs 11 + 12 and 21 + 22
            std::vector<glm::vec3> pathLine1 = concatenatePathLines(pathPositions11, pathPositions12);
            std::vector<glm::vec3> pathLine2 = concatenatePathLines(pathPositions21, pathPositions22);

            std::vector<glm::vec3>::const_iterator concatenationPointPathLine1 =
                pathLine1.begin() + lengthToConcatenation1;

            std::vector<glm::vec3>::const_iterator concatenationPointPathLine2 =
                pathLine2.begin() + lengthToConcatenation2;

            double birthTime = birthTimes[i];

            // Here all points on the pathLine will be used at seedpoints for
            // the actual fieldlines (traced with "b" by default)
            // - 1 because arrays start at 0
            state.addMatchingPathLines(std::move(pathLine1), lengthToConcatenation1 - 1,
                std::move(pathLine2), lengthToConcatenation2 - 1, birthTime);

            double timeToDaysideReconnection1 = 0.0;
            double timeToDaysideReconnection2 = 0.0;
            for (size_t j = 0; j < pathLine1.size(); ++j) {

                std::vector<glm::vec3> keyFrame1;
                std::vector<float> keyFrameLength1;
                traceAndCreateKeyFrame(keyFrame1, keyFrameLength1, pathLine1[j], kameleon, innerBoundaryLimit, nPointsOnFieldlines);
                std::vector<glm::vec3> keyFrame2;
                std::vector<float> keyFrameLength2;
                traceAndCreateKeyFrame(keyFrame2, keyFrameLength2, pathLine2[j], kameleon, innerBoundaryLimit, nPointsOnFieldlines);

                // timeToNextKeyFrame is -1 if at last pathline vertex
                // Vi vill tracea tid baklänges för den pathline del som räknats ut baklänges
                // Vi tror vi kan byta plats på this och next vertex
                float timeToNextKeyFrame1;
                float timeToNextKeyFrame2;

                timeToNextKeyFrame1 = j + 1 == pathLine1.size() ?
                    -1.f : openspace::fls::computeTime(pathLine1[j + 1], pathLine1[j], kameleon);

                bool isBeforeReconnection = j < (pathLine1.size() / 2 - 1);
                if (isBeforeReconnection)
                    timeToDaysideReconnection1 += timeToNextKeyFrame1;

                timeToNextKeyFrame2 = j + 1 == pathLine2.size() ?
                    -1.f : openspace::fls::computeTime(pathLine2[j + 1], pathLine2[j], kameleon);

                if (isBeforeReconnection)
                    timeToDaysideReconnection2 += timeToNextKeyFrame2;

                state.addMatchingKeyFrames(std::move(keyFrame1), std::move(keyFrame2),
                    timeToNextKeyFrame1, timeToNextKeyFrame2, std::move(keyFrameLength1), std::move(keyFrameLength2), i);
            }

            double deathTime = 0;
            double lifeTimeAfterReconnection = 10;
            double lifeTime = timeToDaysideReconnection1 < timeToDaysideReconnection2 ?
                timeToDaysideReconnection1 + lifeTimeAfterReconnection :
                timeToDaysideReconnection2 + lifeTimeAfterReconnection;

            // for the sake of the smurfsaft
            // match the death of dayside to the birth of nigthside
            if (i < matchingSeedPoints.size() / 4) {
                deathTime = birthTime + 1000;
            }
            else {
                deathTime = birthTime + 3000; // increase time
            }

            // TODO: Make it work dynamically
            state.setDeathTimes(deathTime, deathTime, i);
            //state.setDeathTimes(birthTime + lifeTime, birthTime + lifeTime, i);
        }
        bool isSuccessful = state.getAllMatchingFieldlines().size() > 0;
        return isSuccessful;
    }

    /// <summary>
    /// Will trace and create a keyframe
    /// </summary>
    /// <param name="keyFrame">The keyframe that will be constructed</param>
    /// <param name="seedPoint">From where the trace starts</param>
    /// <param name="kameleon"></param>
    /// <param name="innerBoundaryLimit"></param>
    /// <param name="nPointsOnFieldlines">how many points to be created in the trace</param>
    void traceAndCreateKeyFrame(std::vector<glm::vec3>& keyFrame,
        std::vector<float>& lengths,
        const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t nPointsOnFieldlines) {
        std::unique_ptr<ccmc::Interpolator> newInterpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

        ccmc::Tracer tracer(kameleon, newInterpolator.get());
        tracer.setInnerBoundary(innerBoundaryLimit);

        //Elon: replace "secondary trace var"
        std::string tracingVar = "b";
        ccmc::Fieldline fieldline = tracer.bidirectionalTrace(
            tracingVar,
            seedPoint.x,
            seedPoint.y,
            seedPoint.z
        );

        fieldline.getDs();
        fieldline.measure();
        fieldline.integrate();

        ccmc::Fieldline mappedFieldline =
            fieldline.interpolate(1, nPointsOnFieldlines);
        const std::vector<ccmc::Point3f>& fieldlinePositions =
            mappedFieldline.getPositions();


        for (const ccmc::Point3f& pt : fieldlinePositions) {
            keyFrame.emplace_back(pt.component1, pt.component2, pt.component3);
        }
    }

    std::vector<glm::vec3> getFieldlinePositions(
        glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t nPointsOnFieldlines) {

        std::vector<glm::vec3> keyFrame;

        std::unique_ptr<ccmc::Interpolator> newInterpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

        ccmc::Tracer tracer(kameleon, newInterpolator.get());
        tracer.setInnerBoundary(innerBoundaryLimit);

        //Elon: replace "secondary trace var"
        std::string tracingVar = "b";

        ccmc::Fieldline fieldline = tracer.bidirectionalTrace(
            tracingVar,
            seedPoint.x,
            seedPoint.y,
            seedPoint.z
        );

        fieldline.getDs();
        fieldline.measure();
        fieldline.integrate();

        ccmc::Fieldline mappedFieldline =
            fieldline.interpolate(1, nPointsOnFieldlines);
        const std::vector<ccmc::Point3f>& fieldlinePositions =
            mappedFieldline.getPositions();


        for (const ccmc::Point3f& pt : fieldlinePositions) {
            keyFrame.emplace_back(pt.component1, pt.component2, pt.component3);
        }

        return keyFrame;
    }
} // openspace::fls
