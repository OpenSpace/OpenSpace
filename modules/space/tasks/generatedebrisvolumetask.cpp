/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <modules/space/tasks/generatedebrisvolumetask.h>

#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumemetadata.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/util/spicemanager.h>

#include <openspace/documentation/verifier.h>

#include <ghoul/misc/dictionaryluaformatter.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/logging/logmanager.h>
//#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/misc/defer.h>

#include <fstream>
#include <queue>



namespace {
    constexpr const char* ProgramName = "RenderableSatellites";
    constexpr const char* _loggerCat = "SpaceDebris";

    constexpr const char* KeyRawVolumeOutput = "RawVolumeOutput";
    constexpr const char* KeyDictionaryOutput = "DictionaryOutput";
    constexpr const char* KeyDimensions = "Dimensions";
    constexpr const char* KeyStartTime = "StartTime";
    constexpr const char* KeyTimeStep = "TimeStep";
    constexpr const char* KeyEndTime = "EndTime";
    constexpr const char* KeyInputPath = "InputPath";
    constexpr const char* KeyGridType = "GridType";

    // constexpr const char* KeyInputPath1 = "InputPath1";
    // constexpr const char* KeyInputPath2 = "InputPath2";
    // constexpr const char* KeyInputPath3 = "InputPath3";
    // constexpr const char* KeyInputPath4 = "InputPath4";

     constexpr const char* KeyLowerDomainBound = "LowerDomainBound";
     constexpr const char* KeyUpperDomainBound = "UpperDomainBound";
} // namespace

namespace openspace {
namespace volume {
// The list of leap years only goes until 2056 as we need to touch this file then
// again anyway ;)
const std::vector<int> LeapYears = {
    1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996,
    2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 2036, 2040,
    2044, 2048, 2052, 2056
};
// Count the number of full days since the beginning of 2000 to the beginning of
// the parameter 'year'
int countDays(int year) {
    // Find the position of the current year in the vector, the difference
    // between its position and the position of 2000 (for J2000) gives the
    // number of leap years
    constexpr const int Epoch = 2000;
    constexpr const int DaysRegularYear = 365;
    constexpr const int DaysLeapYear = 366;

    if (year == Epoch) {
        return 0;
    }

    // Get the position of the most recent leap year
    const auto lb = std::lower_bound(LeapYears.begin(), LeapYears.end(), year);

    // Get the position of the epoch
    const auto y2000 = std::find(LeapYears.begin(), LeapYears.end(), Epoch);

    // The distance between the two iterators gives us the number of leap years
    const int nLeapYears = static_cast<int>(std::abs(std::distance(y2000, lb)));

    const int nYears = std::abs(year - Epoch);
    const int nRegularYears = nYears - nLeapYears;

    // Get the total number of days as the sum of leap years + non leap years
    const int result = nRegularYears * DaysRegularYear + nLeapYears * DaysLeapYear;
    return result;
}

// Returns the number of leap seconds that lie between the {year, dayOfYear}
// time point and { 2000, 1 }
int countLeapSeconds(int year, int dayOfYear) {
    // Find the position of the current year in the vector; its position in
    // the vector gives the number of leap seconds
    struct LeapSecond {
        int year;
        int dayOfYear;
        bool operator<(const LeapSecond& rhs) const {
            return std::tie(year, dayOfYear) < std::tie(rhs.year, rhs.dayOfYear);
        }
    };

    const LeapSecond Epoch = { 2000, 1 };

    // List taken from: https://www.ietf.org/timezones/data/leap-seconds.list
    static const std::vector<LeapSecond> LeapSeconds = {
        { 1972,   1 },
        { 1972, 183 },
        { 1973,   1 },
        { 1974,   1 },
        { 1975,   1 },
        { 1976,   1 },
        { 1977,   1 },
        { 1978,   1 },
        { 1979,   1 },
        { 1980,   1 },
        { 1981, 182 },
        { 1982, 182 },
        { 1983, 182 },
        { 1985, 182 },
        { 1988,   1 },
        { 1990,   1 },
        { 1991,   1 },
        { 1992, 183 },
        { 1993, 182 },
        { 1994, 182 },
        { 1996,   1 },
        { 1997, 182 },
        { 1999,   1 },
        { 2006,   1 },
        { 2009,   1 },
        { 2012, 183 },
        { 2015, 182 },
        { 2017,   1 }
    };

    // Get the position of the last leap second before the desired date
    LeapSecond date { year, dayOfYear };
    const auto it = std::lower_bound(LeapSeconds.begin(), LeapSeconds.end(), date);

    // Get the position of the Epoch
    const auto y2000 = std::lower_bound(
        LeapSeconds.begin(),
        LeapSeconds.end(),
        Epoch
    );

    // The distance between the two iterators gives us the number of leap years
    const int nLeapSeconds = static_cast<int>(std::abs(std::distance(y2000, it)));
    return nLeapSeconds;
}

double calculateSemiMajorAxis(double meanMotion) {
    constexpr const double GravitationalConstant = 6.6740831e-11;
    constexpr const double MassEarth = 5.9721986e24;
    constexpr const double muEarth = GravitationalConstant * MassEarth;

    // Use Kepler's 3rd law to calculate semimajor axis
    // a^3 / P^2 = mu / (2pi)^2
    // <=> a = ((mu * P^2) / (2pi^2))^(1/3)
    // with a = semimajor axis
    // P = period in seconds
    // mu = G*M_earth
    double period = std::chrono::seconds(std::chrono::hours(24)).count() / meanMotion;

    const double pisq = glm::pi<double>() * glm::pi<double>();
    double semiMajorAxis = pow((muEarth * period*period) / (4 * pisq), 1.0 / 3.0);

    // We need the semi major axis in km instead of m
    return semiMajorAxis / 1000.0;
}

double epochFromSubstring(const std::string& epochString) {
    // The epochString is in the form:
    // YYDDD.DDDDDDDD
    // With YY being the last two years of the launch epoch, the first DDD the day
    // of the year and the remaning a fractional part of the day

    // The main overview of this function:
    // 1. Reconstruct the full year from the YY part
    // 2. Calculate the number of seconds since the beginning of the year
    // 2.a Get the number of full days since the beginning of the year
    // 2.b If the year is a leap year, modify the number of days
    // 3. Convert the number of days to a number of seconds
    // 4. Get the number of leap seconds since January 1st, 2000 and remove them
    // 5. Adjust for the fact the epoch starts on 1st Januaray at 12:00:00, not
    // midnight

    // According to https://celestrak.com/columns/v04n03/
    // Apparently, US Space Command sees no need to change the two-line element
    // set format yet since no artificial earth satellites existed prior to 1957.
    // By their reasoning, two-digit years from 57-99 correspond to 1957-1999 and
    // those from 00-56 correspond to 2000-2056. We'll see each other again in 2057!

    // 1. Get the full year
    std::string yearPrefix = [y = epochString.substr(0, 2)](){
        int year = std::atoi(y.c_str());
        return year >= 57 ? "19" : "20";
    }();
    const int year = std::atoi((yearPrefix + epochString.substr(0, 2)).c_str());
    const int daysSince2000 = countDays(year);

    // 2.
    // 2.a
    double daysInYear = std::atof(epochString.substr(2).c_str());

    // 2.b
    const bool isInLeapYear = std::find(
        LeapYears.begin(),
        LeapYears.end(),
        year
    ) != LeapYears.end();
    if (isInLeapYear && daysInYear >= 60) {
        // We are in a leap year, so we have an effective day more if we are
        // beyond the end of february (= 31+29 days)
        --daysInYear;
    }

    // 3
    using namespace std::chrono;
    const int SecondsPerDay = static_cast<int>(seconds(hours(24)).count());
    //Need to subtract 1 from daysInYear since it is not a zero-based count
    const double nSecondsSince2000 = (daysSince2000 + daysInYear - 1) * SecondsPerDay;

    // 4
    // We need to remove additionbal leap seconds past 2000 and add them prior to
    // 2000 to sync up the time zones
    const double nLeapSecondsOffset = -countLeapSeconds(
        year,
        static_cast<int>(std::floor(daysInYear))
    );

    // 5
    const double nSecondsEpochOffset = static_cast<double>(
        seconds(hours(12)).count()
    );

    // Combine all of the values
    const double epoch = nSecondsSince2000 + nLeapSecondsOffset - nSecondsEpochOffset;
    return epoch;
}

std::vector<KeplerParameters> readTLEFile(const std::string& filename){
    ghoul_assert(FileSys.fileExists(filename), "The filename must exist");

    std::vector<KeplerParameters> data;

    std::ifstream file;
    file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
    file.open(filename);

    int numberOfLines = std::count(std::istreambuf_iterator<char>(file),
                                   std::istreambuf_iterator<char>(), '\n' );
    file.seekg(std::ios_base::beg); // reset iterator to beginning of file

    // 3 because a TLE has 3 lines per element/ object.
    int numberOfObjects = numberOfLines/3;

    std::string line = "-";
    for (int i = 0; i < numberOfObjects; i++) {

        std::getline(file, line); // get rid of title

        KeplerParameters keplerElements;

        std::getline(file, line);
        if (line[0] == '1') {
            // First line
            // Field Columns   Content
            //     1   01-01   Line number
            //     2   03-07   Satellite number
            //     3   08-08   Classification (U = Unclassified)
            //     4   10-11   International Designator (Last two digits of launch year)
            //     5   12-14   International Designator (Launch number of the year)
            //     6   15-17   International Designator(piece of the launch)    A
            //     7   19-20   Epoch Year(last two digits of year)
            //     8   21-32   Epoch(day of the year and fractional portion of the day)
            //     9   34-43   First Time Derivative of the Mean Motion divided by two
            //    10   45-52   Second Time Derivative of Mean Motion divided by six
            //    11   54-61   BSTAR drag term(decimal point assumed)[10] - 11606 - 4
            //    12   63-63   The "Ephemeris type"
            //    13   65-68   Element set  number.Incremented when a new TLE is generated
            //    14   69-69   Checksum (modulo 10)
            keplerElements.epoch = epochFromSubstring(line.substr(18, 14));
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "File {} @ line {} does not have '1' header", filename // linNum + 1
            ));
        }

        std::getline(file, line);
        if (line[0] == '2') {
            // Second line
            // Field    Columns   Content
            //     1      01-01   Line number
            //     2      03-07   Satellite number
            //     3      09-16   Inclination (degrees)
            //     4      18-25   Right ascension of the ascending node (degrees)
            //     5      27-33   Eccentricity (decimal point assumed)
            //     6      35-42   Argument of perigee (degrees)
            //     7      44-51   Mean Anomaly (degrees)
            //     8      53-63   Mean Motion (revolutions per day)
            //     9      64-68   Revolution number at epoch (revolutions)
            //    10      69-69   Checksum (modulo 10)

            std::stringstream stream;
            stream.exceptions(std::ios::failbit);

            // Get inclination
            stream.str(line.substr(8, 8));
            stream >> keplerElements.inclination;
            stream.clear();

            // Get Right ascension of the ascending node
            stream.str(line.substr(17, 8));
            stream >> keplerElements.ascendingNode;
            stream.clear();

            // Get Eccentricity
            stream.str("0." + line.substr(26, 7));
            stream >> keplerElements.eccentricity;
            stream.clear();

            // Get argument of periapsis
            stream.str(line.substr(34, 8));
            stream >> keplerElements.argumentOfPeriapsis;
            stream.clear();

            // Get mean anomaly
            stream.str(line.substr(43, 8));
            stream >> keplerElements.meanAnomaly;
            stream.clear();

            // Get mean motion
            stream.str(line.substr(52, 11));
            stream >> keplerElements.meanMotion;
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "File {} @ line {} does not have '2' header", filename  // , lineNum + 2
            ));
        }

        // Calculate the semi major axis based on the mean motion using kepler's laws
        keplerElements.semiMajorAxis = calculateSemiMajorAxis(keplerElements.meanMotion);

        using namespace std::chrono;
        double period = seconds(hours(24)).count() / keplerElements.meanMotion;
        keplerElements.period = period;

        data.push_back(keplerElements);

    } // !for loop
    file.close();
    return data;
}

glm::dvec3 cartesianToSphericalCoord(glm::dvec3 position){
    glm::dvec3 sphericalPosition;
    // r [0, MaxApogee]
    sphericalPosition.x = sqrt(pow(position.x,2)+pow(position.y,2)+pow(position.z,2));
    // theta [0, pi]
    sphericalPosition.y = acos(position.z/sphericalPosition.x);
    // phi [-pi, pi] -> [0, 2*pi]
    sphericalPosition.z = atan2(position.y,position.x);
    sphericalPosition.z += glm::pi<double>();
    return sphericalPosition;
}

std::vector<glm::dvec3> getPositionBuffer(std::vector<KeplerParameters> tleData,
                                          double timeInSeconds, std::string gridType)
{
    float minTheta = 0.0;
    float minPhi = 0.0;
    float maxTheta = 0.0;
    float maxPhi = 0.0;

    std::vector<glm::dvec3> positionBuffer;
    for(const auto& orbit : tleData) {
        KeplerTranslation keplerTranslator;
        keplerTranslator.setKeplerElements(
            orbit.eccentricity,
            orbit.semiMajorAxis,
            orbit.inclination,
            orbit.ascendingNode,
            orbit.argumentOfPeriapsis,
            orbit.meanAnomaly,
            orbit.period,
            orbit.epoch
        );
        glm::dvec3 position = keplerTranslator.position({
            {},
            Time(timeInSeconds),
            Time(0.0),
            false
        });
        // LINFO(fmt::format("cart: {} ", position));
        glm::dvec3 sphPos;
        if( gridType == "Spherical"){
            sphPos = cartesianToSphericalCoord(position);

            if(sphPos.y < minTheta){
                minTheta = sphPos.y;
            }
            if(sphPos.z < minPhi){
                minPhi = sphPos.z;
            }
            if(sphPos.y > maxTheta){
                maxTheta = sphPos.y;
            }
            if(sphPos.z > maxPhi){
                maxPhi = sphPos.z;
            }
            // LINFO(fmt::format("pos: {} ", sphPos));
            positionBuffer.push_back(sphPos);

        }
        else
        {
            positionBuffer.push_back(position);
        }


    }
    LINFO(fmt::format("max theta: {} ", maxTheta));
    LINFO(fmt::format("max phi: {} ", maxPhi));
    LINFO(fmt::format("min theta: {} ", minTheta));
    LINFO(fmt::format("min phi: {} ", minPhi));

    return positionBuffer;
}

// std::vector<glm::dvec3> generatePositions(int numberOfPositions) {
//     std::vector<glm::dvec3> positions;

//     float radius = 700000;   // meter
//     float degreeStep = 360 / numberOfPositions;

//     for(int i=0 ; i<= 360 ; i += degreeStep){
//         glm::dvec3 singlePosition = glm::dvec3(radius* sin(i), radius*cos(i), 0.0);
//         positions.push_back(singlePosition);
//     }
//     return positions;
// }

float getDensityAt(glm::uvec3 cell,  double* densityArray, RawVolume<float>& raw) {
    float value;
    // return value at position cell from _densityPerVoxel
    size_t index = raw.coordsToIndex(cell);
    value = static_cast<float>(densityArray[index]);
    //LINFO(fmt::format("indensity: {} ", index));

    return value;
}

float getMaxApogee(std::vector<KeplerParameters> inData){
    double maxApogee = 0.0;
    for (const auto& dataElement : inData){
        double ah = dataElement.semiMajorAxis * (1 + dataElement.eccentricity);
        if (ah > maxApogee)
            maxApogee = ah;
    }

    return static_cast<float>(maxApogee*1000);  // * 1000 for meters
}

int getIndexFromPosition(glm::dvec3 position, glm::uvec3 dim, float maxApogee, std::string gridType){
    // epsilon is to make sure that for example if newPosition.x/maxApogee = 1,
    // then the index for that dimension will not exceed the range of the grid.
    float epsilon = static_cast<float>(0.000000001);
    if(gridType == "Cartesian"){ //|| gridType == "Spherical"){
        glm::dvec3 newPosition = glm::dvec3(position.x + maxApogee
                                        ,position.y + maxApogee
                                        ,position.z + maxApogee);

        glm::uvec3 coordinateIndex = glm::uvec3(static_cast<int>(newPosition.x * dim.x / (2 * (maxApogee + epsilon)))
                                                ,static_cast<int>(newPosition.y * dim.y / (2 * (maxApogee + epsilon)))
                                                ,static_cast<int>(newPosition.z * dim.z / (2 * (maxApogee + epsilon))));


        return coordinateIndex.z * (dim.x * dim.y) + coordinateIndex.y * dim.x + coordinateIndex.x;
    }
    else if(gridType == "Spherical"){

        if(position.y >= 3.1415926535897932384626433832795028){
            position.y = 0;
        }
        if(position.z >= (2 * 3.1415926535897932384626433832795028)){
            position.z = 0;
        }

        glm::uvec3 coordinateIndex = glm::uvec3(static_cast<int>(position.x * dim.x / (maxApogee))
                                                ,static_cast<int>(position.y * dim.y / (3.14159265358979323846264338327950288))
                                                ,static_cast<int>(position.z * dim.z / (2*3.14159265358979323846264338327950288)));


        // LINFO(fmt::format("index coords: {} ", coordinateIndex));
        // LINFO(fmt::format("index dim: {} ", dim));
        // LINFO(fmt::format("index va: {} ", coordinateIndex.y * (dim.x * dim.y) + coordinateIndex.z * dim.x + coordinateIndex.x));

        return coordinateIndex.z * (dim.x * dim.y) + coordinateIndex.y * dim.x + coordinateIndex.x;
    }

    return -1;
}

double getVoxelVolume(int index, RawVolume<float>& raw, glm::uvec3 dim, float maxApogee){
    // get coords from index
    glm::uvec3 coords = raw.indexToCoords(index);

    double rMax = maxApogee / dim.x;
    double thetaMax = 3.141592 / dim.y;
    double phiMax = (2 * 3.141592) / dim.z;
    //use coords to calc volume
    //integral(dTheta) * integral(r^2 dr) * integral(sin(phi) dPhi)
    double rIntegral = (pow(((coords.x + 1) * rMax),3) - pow(((coords.x) * rMax),3)) / 3;
    double thetaIntegral = -cos((coords.y + 1) * thetaMax) +  cos(coords.y * thetaMax);
    double phiIntegral = ((coords.z + 1) - coords.z) * phiMax;

    return rIntegral * thetaIntegral * phiIntegral;
    //return volume

}

double* mapDensityToVoxels(double* densityArray, std::vector<glm::dvec3> positions, glm::uvec3 dim, float maxApogee, std::string gridType, RawVolume<float>& raw) {

    for(const glm::dvec3& position : positions) {
        //LINFO(fmt::format("pos: {} ", position));
        int index = getIndexFromPosition(position, dim, maxApogee, gridType);
        //LINFO(fmt::format("index: {} ", index));
        if(gridType == "Cartesian"){
            ++densityArray[index];
        }
        else if(gridType == "Spherical"){
            double voxelVolume = getVoxelVolume(index, raw, dim, maxApogee); //something like this
            densityArray[index] += 1/voxelVolume;
        }
    }

    return densityArray;
}

GenerateDebrisVolumeTask::GenerateDebrisVolumeTask(const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "GenerateDebrisVolumeTask"
    );

    _rawVolumeOutputPath = absPath(dictionary.value<std::string>(KeyRawVolumeOutput));
    _dictionaryOutputPath = absPath(dictionary.value<std::string>(KeyDictionaryOutput));
    _dimensions = dictionary.value<glm::vec3>(KeyDimensions); // must not be <glm::uvec3> for some reason.
    _startTime = dictionary.value<std::string>(KeyStartTime);
    _timeStep = dictionary.value<std::string>(KeyTimeStep); // Todo: send KeyTimeStep in as a int or float correctly.
    _endTime = dictionary.value<std::string>(KeyEndTime);
    // since _inputPath is past from task,
    // there will have to be either one task per dataset,
    // or you need to combine the datasets into one file.
    _inputPath = absPath(dictionary.value<std::string>(KeyInputPath));
    _gridType = dictionary.value<std::string>(KeyGridType);
    _lowerDomainBound = dictionary.value<glm::vec3>(KeyLowerDomainBound);
    _upperDomainBound = dictionary.value<glm::vec3>(KeyUpperDomainBound);

    _TLEDataVector = readTLEFile(_inputPath);
    _maxApogee = getMaxApogee(_TLEDataVector);
}

std::string GenerateDebrisVolumeTask::description() {
    return "todo:: description";
}

void GenerateDebrisVolumeTask::perform(const Task::ProgressCallback& progressCallback) {
    SpiceManager::KernelHandle kernel =
    SpiceManager::ref().loadKernel(absPath("${DATA}/assets/spice/naif0012.tls"));

    defer {
        SpiceManager::ref().unloadKernel(kernel);
    };

    //////////
    //1. read TLE-data and position of debris elements.
    // std::vector<KeplerParameters>TLEDataVector = readTLEFile(_inputPath);
    // std::vector<KeplerParameters>TLEDataVector1 = readTLEFile(_inputPath1);
    // std::vector<KeplerParameters>TLEDataVector2 = readTLEFile(_inputPath2);
    // std::vector<KeplerParameters>TLEDataVector3 = readTLEFile(_inputPath3);
    // std::vector<KeplerParameters>TLEDataVector4 = readTLEFile(_inputPath4);

    // _TLEDataVector.reserve( TLEDataVector.size() + TLEDataVector1.size() + TLEDataVector2.size() + TLEDataVector3.size() + TLEDataVector4.size());
    // _TLEDataVector.insert(_TLEDataVector.end(), TLEDataVector.begin(), TLEDataVector.end());
    // _TLEDataVector.insert(_TLEDataVector.end(), TLEDataVector1.begin(), TLEDataVector1.end());
    // _TLEDataVector.insert(_TLEDataVector.end(), TLEDataVector2.begin(), TLEDataVector2.end());
    // _TLEDataVector.insert(_TLEDataVector.end(), TLEDataVector3.begin(), TLEDataVector3.end());
    // _TLEDataVector.insert(_TLEDataVector.end(), TLEDataVector4.begin(), TLEDataVector4.end());
    // ----- or ----- if only one

        // _TLEDataVector = readTLEFile(_inputPath);

    //////////
    VolumeGridType GridType = VolumeGridType::Cartesian;
    if(_gridType == "Spherical"){
        GridType = VolumeGridType::Spherical;
    }
    else if(_gridType != "Cartesian"){
        // TODO:: Error message
        return;
    }

    // float maxApogee = getMaxApogee(_TLEDataVector);
    LINFO(fmt::format("Max Apogee: {} ", _maxApogee));

    /**  SEQUENCE
    *   1. handle timeStep
    *       1.1 either ignore last timeperiod from the latest whole timestep to _endTime
    *       1.2 or extend endTime to be equal to next full timestep
    *   2. loop to create a rawVolume for each timestep.
    */

    // 1    // todo: handle if endTime is earlyer than startTime
    double startTimeInSeconds = Time::convertTime(_startTime);
    double endTimeInSeconds = Time::convertTime(_endTime);
    double timeSpan = endTimeInSeconds - startTimeInSeconds;

    float timeStep = std::stof(_timeStep);

    // 1.1
     int numberOfIterations = static_cast<int>(timeSpan/timeStep);
    LINFO(fmt::format("timestep: {} ", numberOfIterations));

    std::queue<volume::RawVolume<float>> rawVolumeQueue = {};
    const int size = _dimensions.x *_dimensions.y *_dimensions.z;
    float minVal = std::numeric_limits<float>::max();
    float maxVal = std::numeric_limits<float>::min();
    // 2.
    for(int i=0 ; i<=numberOfIterations ; ++i) {

        std::vector<glm::dvec3> startPositionBuffer = getPositionBuffer(_TLEDataVector, startTimeInSeconds+(i*timeStep), _gridType);   //+(i*timeStep)
        //LINFO(fmt::format("pos: {} ", startPositionBuffer[4]));

        double *densityArrayp = new double[size]();
        //densityArrayp = mapDensityToVoxels(densityArrayp, generatedPositions, _dimensions, maxApogee);
        volume::RawVolume<float> rawVolume(_dimensions);

        densityArrayp = mapDensityToVoxels(densityArrayp, startPositionBuffer, _dimensions, _maxApogee, _gridType, rawVolume);
        /*std::vector<glm::dvec3> testBuffer;
        testBuffer.push_back(glm::dvec3(0,0,0));
        testBuffer.push_back(glm::dvec3(1,1.5,1.5));
        testBuffer.push_back(glm::dvec3(1,3,3));
        testBuffer.push_back(glm::dvec3(3,5,3));
        //testBuffer.push_back(glm::dvec3(10000,1000000000,1000000000));


        densityArrayp = mapDensityToVoxels(densityArrayp, testBuffer, _dimensions, _maxApogee, _gridType);
        */
        // create object rawVolume

        //glm::vec3 domainSize = _upperDomainBound - _lowerDomainBound;

        // TODO: Create a forEachSatallite and set(cell, value) to combine mapDensityToVoxel
        //      and forEachVoxel for less time complexity.
        rawVolume.forEachVoxel([&](glm::uvec3 cell, float) {
        //     glm::vec3 coord = _lowerDomainBound +
        //        glm::vec3(cell) / glm::vec3(_dimensions) * domainSize;
            float value = getDensityAt(cell, densityArrayp, rawVolume);   // (coord)

            rawVolume.set(cell, value);

            minVal = std::min(minVal, value);
            maxVal = std::max(maxVal, value);
            /*LINFO(fmt::format("min: {} ", minVal));
            LINFO(fmt::format("max: {} ", maxVal));*/
        });
        rawVolumeQueue.push(rawVolume);
        delete[] densityArrayp;
    }

    // two loops is used to get a global min and max value for voxels.
    for(int i=0 ; i<=numberOfIterations ; ++i){
        // LINFO(fmt::format("raw file output name: {} ", _rawVolumeOutputPath));

        size_t lastIndex = _rawVolumeOutputPath.find_last_of(".");
        std::string rawOutputName = _rawVolumeOutputPath.substr(0, lastIndex);
        rawOutputName += std::to_string(i) + ".rawvolume";

        lastIndex = _dictionaryOutputPath.find_last_of(".");
        std::string dictionaryOutputName = _dictionaryOutputPath.substr(0, lastIndex);
        dictionaryOutputName += std::to_string(i) + ".dictionary";

        ghoul::filesystem::File file(rawOutputName);
        const std::string directory = file.directoryName();
        if (!FileSys.directoryExists(directory)) {
            FileSys.createDirectory(directory, ghoul::filesystem::FileSystem::Recursive::Yes);
        }

        volume::RawVolumeWriter<float> writer(rawOutputName);
        writer.write(rawVolumeQueue.front());
        rawVolumeQueue.pop();

        RawVolumeMetadata metadata;
        // alternatively metadata.hasTime = false;
        metadata.time = Time::convertTime(_startTime)+(i*timeStep);
        metadata.dimensions = _dimensions;
        metadata.hasDomainUnit = false;
        metadata.hasValueUnit = false;
        metadata.gridType = GridType;
        metadata.hasDomainBounds = true;
        metadata.lowerDomainBound = _lowerDomainBound;
        metadata.upperDomainBound = _upperDomainBound;
        metadata.hasValueRange = true;
        metadata.minValue = minVal;
        metadata.maxValue = maxVal;

        /*LINFO(fmt::format("min2: {} ", minVal));
        LINFO(fmt::format("max2: {} ", maxVal));*/

        ghoul::Dictionary outputDictionary = metadata.dictionary();
        ghoul::DictionaryLuaFormatter formatter;
        std::string metadataString = formatter.format(outputDictionary);

        std::fstream f(dictionaryOutputName, std::ios::out);
        f << "return " << metadataString;
        f.close();
    }
}

documentation::Documentation GenerateDebrisVolumeTask::documentation() {
    using namespace documentation;
    return {
        "GenerateDebrisVolumeTask",
        "generate_debris_volume_task",
        {
            {
                "Type",
                new StringEqualVerifier("GenerateDebrisVolumeTask"),
                Optional::No,
                "The type of this task",
            },
            {
                KeyStartTime,
                new StringAnnotationVerifier("start time"),
                Optional::No,
                "start time",
            },
            {
                KeyInputPath,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "Input path to the TLE-data",
            },
            {
                KeyRawVolumeOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The raw volume file to export data to",
            },
            {
                KeyDictionaryOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The lua dictionary file to export metadata to",
            },
            {
                KeyDimensions,
                new DoubleVector3Verifier,
                Optional::No,
                "A vector representing the number of cells in each dimension",
            },
             {
                 KeyLowerDomainBound,
                 new DoubleVector3Verifier,
                 Optional::No,
                 "A vector representing the lower bound of the domain"
             },
             {
                 KeyUpperDomainBound,
                 new DoubleVector3Verifier,
                 Optional::No,
                 "A vector representing the upper bound of the domain"
             }
        }
    };
}

} // namespace volume
} // namespace openspace
