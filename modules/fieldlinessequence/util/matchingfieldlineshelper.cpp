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

    // ALIASES

    using seedPointPair = std::pair<glm::vec3, glm::vec3>;

    // DECLARATIONS

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

    std::vector<std::pair<glm::vec3, std::string>>  validateAndModifySeedPointsRecursive(
        int i,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        std::vector<std::pair<glm::vec3, std::string>>& seedPoint,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        double &accuracy,
        size_t _nPointsOnFieldLine
    );

    void createTextFileWithFieldlineCoordinates(
        std::vector<glm::vec3> flowlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine);

    bool checkIfFieldlineIsOpen(std::vector<glm::vec3> fieldlinePositions);
    bool checkIfFieldlineIsClosed(std::vector<glm::vec3> fieldlinePositions);
    bool checkIfFieldlineIsIMF(std::vector<glm::vec3> fieldlinePositions);

    bool keepCheckingFlowlinesFieldline(std::vector<glm::vec3> flowlinePos, int flowlineIndex);

    std::pair<glm::vec3, std::string> moveSeedpointDown(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength);

    std::pair<glm::vec3, std::string> moveSeedpointUp(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength);

    std::pair<glm::vec3, std::string> moveSeedPositiveX(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength);

    std::pair<glm::vec3, std::string> moveSeedNegativeX(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength);

    std::vector<glm::vec3> getPositionsFromLine(ccmc::Fieldline seedPointFlowline);

    std::vector <std::vector<glm::vec3>> addFieldLinePositionsToVector(
        std::vector <std::vector<glm::vec3>>& seedPointFieldlinePositions,
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine);

    glm::vec3 modifySeedpoint(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        float& stepLength,
        double& accuracy,
        bool up);

    glm::vec3 modifySeedpointClosed(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        float& stepLength,
        double& accuracy,
        bool closerToEarth);

    glm::vec3 modifySeedpointIMF(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        float& stepLength,
        double& accuracy,
        bool closerToEarth);

    // DEFINITIONS

    bool openspace::fls::convertCdfToMatchingFieldlinesState(
        FieldlinesState& state,
        ccmc::Kameleon* kameleon,
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        const std::vector<double>& birthTimes,
        double manualTimeOffset,
        const std::string& tracingVar,
        std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldLines)
    {

        // TODO: Check if an even amount of seed points
        std::vector<seedPointPair> matchingSeedPoints;
        for (size_t i = 0; i < seedPoints.size(); i += 2) {
            matchingSeedPoints.push_back({ seedPoints[i].first, seedPoints[i + 1].first });
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

    int findLastOpenNorthFieldlineIndex(
        std::vector<glm::vec3> flowlinePositions,
        int startIndex,
        ccmc::Kameleon* kameleon
    )
    {
        int indexOfLastOpenNorthFieldline = 0;
        double innerBoundaryLimit = 0.5;
        std::string previousTopology = "";

        while (startIndex < flowlinePositions.size() - 1)
        {
            // Print progress
            std::cout << "Checking fieldline topology of the " << startIndex + 1 << " point out of " << flowlinePositions.size() << " on the flowline" << std::endl;

            // Get fieldline of current + 1 position on flowline
            std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                flowlinePositions[startIndex + 1],
                kameleon,
                innerBoundaryLimit,
                2
            );

            // check if traced fieldline is IMF
            if (checkIfFieldlineIsIMF(fieldlinePositions)) //fieldlinePositions[0].z < 0 || fieldlinePositions[fieldlinePositions.size() - 1].z < 0)
            {
                if (previousTopology == "OPEN_NORTH")
                {
                    indexOfLastOpenNorthFieldline = startIndex;
                    break;
                }
                else {
                    previousTopology = "OPEN_NORTH";
                }
            }
            startIndex++;
        }

        return indexOfLastOpenNorthFieldline;
    }

    std::vector<std::pair<glm::vec3, std::string>> findAndAddNightsideSeedPoints (
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        std::vector<double>& birthTimes,
        ccmc::Kameleon* kameleon,
        const std::string& tracingVar,
        const size_t nPointsOnPathLine
    )
    {
        // initialize
        std::vector<std::pair<glm::vec3, std::string>> nightsideSeedPoints;
        glm::vec3 imfSeedPointNightside;
        glm::vec3 closedSeedPointNightside;
        glm::vec3 onSeedPointNightside;
        glm::vec3 osSeedPointNightside;

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

        int testNPoints = 20000;

        std::cout << "Finding last ON and first IMF" << std::endl;

        // go through each seed point
        for (int i = 0; i < seedPoints.size(); i++)
        {
            // for each seed point with the topology open north
            if (seedPoints[i].second == "OPEN_NORTH")
            {
                // lets try and find corresponding nightside seed points

                // first, lets trace the open north seed points fieldline
                std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                    seedPoints[i].first,
                    kameleon,
                    innerBoundaryLimit,
                    100
                );

                // now, lets figure out the edge position of that fieldline
                glm::vec3 firstPosOfFieldline = fieldlinePositions[20];

                std::cout << "fieldlinePositions: " << fieldlinePositions[0].x << " "
                    << fieldlinePositions[0].y << " "
                    << fieldlinePositions[0].z << " "
                    << std::endl;

                // trace a flowline from the new point/edge position
                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    firstPosOfFieldline,
                    testNPoints,
                    ccmc::Tracer::Direction::FOWARD);

                std::vector<glm::vec3> flowlinePositions
                    = getPositionsFromLine(flowline);

                // find the last open north fieldline and first IMF of the traced flowline
                std::vector<std::vector<glm::vec3>> lastON;
                std::vector<std::vector<glm::vec3>> firstIMF;

                double indexFlowlinePos2 = 0;

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

                indexFlowlinePos2 = findLastOpenNorthFieldlineIndex(flowlinePositions2, 0, kameleon);

                std::cout << indexFlowlinePos2 << " " << flowlinePositions2.size() << " " << indexFlowlinePos2 / flowlinePositions2.size() << std::endl;
                double referencePercentage = indexFlowlinePos2 / flowlinePositions2.size();
                int indexFlowlinePos = referencePercentage * flowlinePositions.size();

                std::cout << "Finding reference index - Completed" << std::endl;

                indexFlowlinePos = findLastOpenNorthFieldlineIndex(flowlinePositions, indexFlowlinePos, kameleon);

                std::vector<glm::vec3> fieldlinePositionsIMF = fls::getFieldlinePositions(
                    flowlinePositions[indexFlowlinePos], // fix, should be + 1
                    kameleon,
                    innerBoundaryLimit,
                    testNPoints
                );

                std::vector<glm::vec3> fieldlinePositionsON = fls::getFieldlinePositions(
                    flowlinePositions[indexFlowlinePos -1], // fix, should be 0
                    kameleon,
                    innerBoundaryLimit,
                    testNPoints
                );

                lastON.push_back(fieldlinePositionsON);
                firstIMF.push_back(fieldlinePositionsIMF);

                std::cout << fieldlinePositionsON[0].z << " " << fieldlinePositionsON[fieldlinePositionsON.size()-1].z << std::endl;
                std::cout << fieldlinePositionsIMF[0].z << " " << fieldlinePositionsIMF[fieldlinePositionsIMF.size()-1].z << std::endl;

                std::cout << "Finding last ON and first IMF - Complete" << std::endl;
                std::cout << "Finding IMF Nightside Seed Point" << std::endl;

                float leastXValue = -1000;

                // find point on the imf with smallest x-value (closest to earth) O(n)
                // TODO use min-heap here instead
                    for (int i = 0; i < firstIMF.size(); i++)
                    {
                        for (int j = 0; j < testNPoints; j++)
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

                // find point on the on fieldline closest to the imfNightsideSeedPoint
                // TODO optimize from O(n)
                for (int i = 0; i < lastON.size(); i++)
                {
                    for (int j = 0; j < testNPoints; j++)
                    {
                        // calculate distance

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

                double factor = 0.01;

                glm::vec3 earthPos;
                earthPos.x = 0;
                earthPos.y = 0;
                earthPos.z = 0;

                glm::vec3 vectorIE;
                vectorIE.x = imfSeedPointNightside.x - earthPos.x;
                vectorIE.y = imfSeedPointNightside.y - earthPos.y;
                vectorIE.z = imfSeedPointNightside.z - earthPos.z;

                closedSeedPointNightside.x = imfSeedPointNightside.x - vectorIE.x * 0.06;
                closedSeedPointNightside.y = imfSeedPointNightside.y - vectorIE.y * 0.06;
                closedSeedPointNightside.z = imfSeedPointNightside.z - vectorIE.z * 0.06;

                while (closedSeedPointNightside.x < 0)
                {
                    // move seed point towards earh
                    closedSeedPointNightside.x = closedSeedPointNightside.x - vectorIE.x * factor;
                    closedSeedPointNightside.y = closedSeedPointNightside.y - vectorIE.y * factor;
                    closedSeedPointNightside.z = closedSeedPointNightside.z - vectorIE.z * factor;

                    // trace fieldline from closedSeedPoint
                    std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                        closedSeedPointNightside,
                        kameleon,
                        innerBoundaryLimit,
                        2
                    );

                    std::cout << "Moving closed seed point closer to Earth " <<
                        closedSeedPointNightside.x << std::endl;

                    // check if closed
                    if (checkIfFieldlineIsClosed(fieldlinePositions))
                    {

                        std::cout << "Closed seed point traced closed fieldline "
                            << std::endl;

                        std::cout << "Checking so all points on flowline traces closed fiedlines" << std::endl;

                        // trace flowline
                        ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                            tracingVar,
                            tracer,
                            closedSeedPointNightside,
                            100,
                            ccmc::Tracer::Direction::FOWARD);

                        // get postions from flowline
                        std::vector<glm::vec3> flowlinePositions
                            = getPositionsFromLine(flowline);

                        bool closedSeedPointIsClosedFieldline = true;
                        int indexFlowlinePos2 = 0;

                        while (closedSeedPointIsClosedFieldline && indexFlowlinePos2 < 50) //99
                        {
                            // trace fieldline from closedSeedPoint
                            std::vector<glm::vec3> fieldlinePositions2 = fls::getFieldlinePositions(
                                flowlinePositions[indexFlowlinePos2],
                                kameleon,
                                innerBoundaryLimit,
                                2
                            );

                            if (!checkIfFieldlineIsClosed(fieldlinePositions2))
                            {
                                closedSeedPointIsClosedFieldline = false;
                                std::cout << indexFlowlinePos2 <<
                                    "point out of " << "50" << " not approved" << std::endl;
                                break;
                            }

                            std::cout << indexFlowlinePos2 <<
                                "point out of " << "50" << " approved" << std::endl;
                            indexFlowlinePos2++;
                        }

                        // we've found only closed fieldlines
                        if (closedSeedPointIsClosedFieldline)
                        {
                            std::cout << "Finding Closed Nightside Seed Point - Complete" << std::endl;
                            break;
                        }
                    }
                }

                nightsideSeedPoints.push_back({ onSeedPointNightside, "OPEN_NORTH" });
                nightsideSeedPoints.push_back({ osSeedPointNightside, "OPEN_NORTH" });
                nightsideSeedPoints.push_back({ imfSeedPointNightside, "IMF" });
                nightsideSeedPoints.push_back({ closedSeedPointNightside, "CLOSED" });
            }
        }

        // add nightside seedpoints to seedPoints that will enter the rendering phace
        for (int i = 0; i < nightsideSeedPoints.size(); i++)
        {
            seedPoints.push_back(nightsideSeedPoints[i]);
            if ((i+1) % 4 == 0)
            {
                birthTimes.push_back(-28000);
            }
        }
        return nightsideSeedPoints;
    }

    double calculateDistance(double x1, double y1, double z1, double x2, double y2, double z2) {
        double dx = x2 - x1;
        double dy = y2 - y1;
        double dz = z2 - z1;

        return std::sqrt(dx * dx + dy * dy + dz * dz);
    }

    /*
    * Traces the field line of a given seedpoint and returns the
    * points postions of that fieldline
    */
    std::vector<std::pair<glm::vec3, std::string>> validateAndModifyAllSeedPoints(
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        const std::string& tracingVar,
        ccmc::Kameleon* kameleon,
        const size_t nPointsOnPathLine,
        double& accuracy)
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


    std::vector<std::pair<glm::vec3, std::string>> validateAndModifySeedPointsRecursive(
        int i,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        double& accuracy,
        size_t _nPointsOnFieldLine
        )
    {
        if (i >= seedPoints.size())
        {
            return seedPoints;
        }
        else
        {
            if (seedPoints[i].second == "OPEN_NORTH" || seedPoints[i].second == "OPEN_SOUTH")
            {
                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    seedPoints[i].first,
                    nPointsOnPathLine,
                    ccmc::Tracer::Direction::FOWARD);

                std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(flowline);

                //createTextFileWithFieldlineCoordinates(flowlinePositions, kameleon, innerBoundaryLimit, _nPointsOnFieldLine);

                int counter = 0;

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
                        float stepLength = 5;

                        std::cout << "CLOSED! - Move seed point" << std::endl;
                        glm::vec3 modifiedSeedpoint = modifySeedpoint(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            stepLength,
                            accuracy,
                            true
                        );

                        seedPoints[i].first = modifiedSeedpoint;

                        break;
                    }
                    else if(checkIfFieldlineIsIMF(fieldlinePositions))
                    {
                        float stepLength = 5;

                        std::cout << "IMF! - Move seed point closer to critical point" << std::endl;
                        glm::vec3 modifiedSeedpoint = modifySeedpoint(
                            seedPoints[i],
                            tracingVar,
                            tracer,
                            nPointsOnPathLine,
                            kameleon,
                            innerBoundaryLimit,
                            _nPointsOnFieldLine,
                            stepLength,
                            accuracy,
                            true
                        );

                        seedPoints[i].first = modifiedSeedpoint;

                        break;
                    }
                    counter++;
                }
            }
            else if (seedPoints[i].second == "CLOSED")
            {

                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    seedPoints[i].first,
                    nPointsOnPathLine,
                    ccmc::Tracer::Direction::REVERSE);

                std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(flowline);

                std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                    flowlinePositions[flowlinePositions.size()-4],
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine
                );

                if (!checkIfFieldlineIsClosed(fieldlinePositions))
                {
                    float stepLength = 0.2;

                    glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                        seedPoints[i],
                        tracingVar,
                        tracer,
                        nPointsOnPathLine,
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine,
                        stepLength,
                        accuracy,
                        true
                    );

                    seedPoints[i].first = modifiedSeedpoint;
                }
                else //If it is a Closed fieldline
                {
                    float stepLength = 0.1;
                    glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                        seedPoints[i],
                        tracingVar,
                        tracer,
                        nPointsOnPathLine,
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine,
                        stepLength,
                        accuracy,
                        false
                    );
                }

                float newOpenXvalue = (seedPoints[i].first.x + seedPoints[i - 1].first.x) / 2;
                seedPoints[i + 1].first.x = newOpenXvalue;
                seedPoints[i + 2].first.x = newOpenXvalue;

            }
            else if (seedPoints[i].second == "IMF")
            {
                ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                    tracingVar,
                    tracer,
                    seedPoints[i].first,
                    nPointsOnPathLine,
                    ccmc::Tracer::Direction::REVERSE);

                std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(flowline);

                std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                    flowlinePositions[flowlinePositions.size() - 1],
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine
                );

                if (!checkIfFieldlineIsIMF(fieldlinePositions))
                {
                    float stepLength = 0.5;

                    glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                        seedPoints[i],
                        tracingVar,
                        tracer,
                        nPointsOnPathLine,
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine,
                        stepLength,
                        accuracy,
                        false
                    );

                    seedPoints[i].first = modifiedSeedpoint;
                }
                else //If it is a IMF
                {
                    float stepLength = 0.5;

                    glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                        seedPoints[i],
                        tracingVar,
                        tracer,
                        nPointsOnPathLine,
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine,
                        stepLength,
                        accuracy,
                        true
                    );

                    seedPoints[i].first = modifiedSeedpoint;
                }
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

    void createTextFileWithFieldlineCoordinates(
        std::vector<glm::vec3> flowlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine) {

        std::vector<std::vector<glm::vec3>> testFieldlinePositions;

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

        std::ofstream output_file("C:/Users/alundkvi/Documents/DataOpenSpace/simon&maans/NotWorking.txt");

        if (output_file.is_open()) {
            for (const auto& subvec : testFieldlinePositions) {
                for (const auto& vec : subvec) {
                    output_file << vec.x << " " << vec.y << " " << vec.z << " " << std::endl;
                }
                output_file << std::endl << std::endl << "NEW FIELD LINE " << std::endl;
            }
            output_file.close();
        }
        else {
            std::cerr << "Unable to open file" << std::endl;
        }
    }

    bool checkIfFieldlineIsOpen(std::vector<glm::vec3> fieldlinePositions)
    {
        std::pair<double, double> threshold_start_z = { std::make_pair(25.0, -25.0) };
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

    bool checkIfFieldlineIsClosed(std::vector<glm::vec3> fieldlinePositions)
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

    bool checkIfFieldlineIsIMF(std::vector<glm::vec3> fieldlinePositions)
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
    * Move seed z-coordinate down by stepLength
    */
    std::pair<glm::vec3, std::string> moveSeedpointDown(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength)
    {
        try {
            if (seedPoint.second == "OPEN_NORTH")
            {
                seedPoint.first.z = seedPoint.first.z - stepLength;

                std::cout << "Modifying.... Down by:" << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
            else if (seedPoint.second == "OPEN_SOUTH")
            {
                seedPoint.first.z = seedPoint.first.z + stepLength;
                std::cout << "Modifying.... Down by: " << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Move seed z-coordinate up by stepLength
    */
    std::pair<glm::vec3, std::string> moveSeedpointUp(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength)
    {
        try {
            if (seedPoint.second == "OPEN_NORTH")
            {
                seedPoint.first.z = seedPoint.first.z + stepLength;

                std::cout << "Modifying.... Up by: " << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
            else if (seedPoint.second == "OPEN_SOUTH")
            {
                seedPoint.first.z = seedPoint.first.z - stepLength;

                std::cout << "Modifying.... Up by: " << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Move seed by negative x-coordinate (closer to earth)
    */
    std::pair<glm::vec3, std::string> moveSeedNegativeX(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength)
    {
        try {
            seedPoint.first.x = seedPoint.first.x - stepLength;

            std::cout << "Modifying.... Closer to Earth by: " << stepLength << std::endl;
            std::cout << "Top " << seedPoint.second << ", x value: " << seedPoint.first.x << std::endl;
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Move seed by positive x-coordinate (closer to earth)
    */
    std::pair<glm::vec3, std::string> moveSeedPositiveX(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength)
    {
        try {
            seedPoint.first.x = seedPoint.first.x + stepLength;

            std::cout << "Modifying.... Closer to Critical point by: " << stepLength << std::endl;
            std::cout << "Top " << seedPoint.second << ", x value: " << seedPoint.first.x << std::endl;
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Checks if the position of the flowLine has gone past set criteria
    */
    bool keepCheckingFlowlinesFieldline(std::vector<glm::vec3> flowlinePos, int flowlineIndex) {
        if (flowlinePos[flowlineIndex].x > -4)
        {
            return true;
        }
        return false;
    }

    /*
    * Adds all coordinates for fieldline positions to a vector from a given flowline
    */
    std::vector <std::vector<glm::vec3>> addFieldLinePositionsToVector(
        std::vector <std::vector<glm::vec3>>& seedPointFieldlinePositions,
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine)
    {
        seedPointFieldlinePositions.clear();

        std::vector<glm::vec3> fieldlinePositions2;

        for (int i = 0; i < seedPointFlowlinePositionsVec3.size(); i++)
        {
            fieldlinePositions2 = fls::getFieldlinePositions(
                seedPointFlowlinePositionsVec3[i],
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine
            );

            seedPointFieldlinePositions.push_back(fieldlinePositions2);
        }
        return seedPointFieldlinePositions;
    }

    std::vector<glm::vec3> getPositionsFromLine(ccmc::Fieldline seedPointFlowline)
    {
        std::vector<ccmc::Point3f> seedPointFlowlinePositions
            = seedPointFlowline.getPositions();
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3
            = convertPoint3fToVec3(seedPointFlowlinePositions);

        return seedPointFlowlinePositionsVec3;
    }

    glm::vec3 modifySeedpoint(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        float& stepLength,
        double& accuracy,
        bool up)
    {
        std::vector<glm::vec3> fieldlinePositions;

        if (up)
        {
            seedPoint = moveSeedpointUp(seedPoint, stepLength);
        }
        else if (!up)
        {
            seedPoint = moveSeedpointDown(seedPoint, stepLength);
        }

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.first,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::FOWARD);

        std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(seedPointFlowline);

        int flowlineIndex = 0;

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
                stepLength = stepLength / 2;

                std::cout << "CLOSED! - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpoint(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    stepLength,
                    accuracy,
                    true
                );
            } else if (checkIfFieldlineIsIMF(fieldlinePositions))
            {
                stepLength = stepLength / 2;

                std::cout << "IMF! - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpoint(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    stepLength,
                    accuracy,
                    false
                );
                break;
            } else if (!checkIfFieldlineIsOpen(fieldlinePositions))
            {
                stepLength = stepLength / 2;

                std::cout << "NOT OPEN! - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpoint(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    stepLength,
                    accuracy,
                    true
                );
                break;
            }
            flowlineIndex++;
        }

        if (stepLength > accuracy)
        {
            stepLength = stepLength / 2;

            std::cout << "Good fieldline - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpoint(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                stepLength,
                accuracy,
                false
            );
        }

        return seedPoint.first;
    }

    glm::vec3 modifySeedpointClosed(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        float& stepLength,
        double& accuracy,
        bool closerToEarth)
    {
        if (closerToEarth)
        {
            seedPoint = moveSeedNegativeX(seedPoint, stepLength);
        }
        else if (!closerToEarth)
        {
            seedPoint = moveSeedPositiveX(seedPoint, stepLength);
        }

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.first,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::REVERSE);

        std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(seedPointFlowline);

        std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
            flowlinePositions[flowlinePositions.size()-4],
            kameleon,
            innerBoundaryLimit,
            _nPointsOnFieldLine
        );

        if (!checkIfFieldlineIsClosed(fieldlinePositions))
        {
            stepLength = stepLength / 2;

            std::cout << "NOT CLOSED! - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                stepLength,
                accuracy,
                true
            );
        }
        if (stepLength > accuracy)
        {
            stepLength = stepLength / 2;

            std::cout << "Good fieldline - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointClosed(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                stepLength,
                accuracy,
                false
            );
        }

        return seedPoint.first;
    }


    glm::vec3 modifySeedpointIMF(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        float& stepLength,
        double& accuracy,
        bool closerToEarth)
    {
        if (closerToEarth)
        {
            seedPoint = moveSeedNegativeX(seedPoint, stepLength);
        }
        else if (!closerToEarth)
        {
            seedPoint = moveSeedPositiveX(seedPoint, stepLength);
        }

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.first,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::REVERSE);

        std::vector<glm::vec3> flowlinePositions = getPositionsFromLine(seedPointFlowline);

        std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
            flowlinePositions[flowlinePositions.size() - 1],
            kameleon,
            innerBoundaryLimit,
            _nPointsOnFieldLine
        );

        if (!checkIfFieldlineIsIMF(fieldlinePositions))
        {
            stepLength = stepLength / 2;

            std::cout << "NOT IMF! - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                stepLength,
                accuracy,
                false
            );
        }
        if (stepLength > accuracy)
        {
            stepLength = stepLength / 2;

            std::cout << "Good fieldline - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpointIMF(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                stepLength,
                accuracy,
                true
            );
        }
        return seedPoint.first;
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
                deathTime = birthTime + 700;
            }
            else {
                deathTime = birthTime + 2000;
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
