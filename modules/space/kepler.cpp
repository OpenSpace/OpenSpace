/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/space/kepler.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <scn/scn.h>
#include <scn/tuple_return.h>
#include <fstream>
#include <optional>
#include <sstream>

namespace {
    constexpr std::string_view _loggerCat = "Kepler";
    constexpr int8_t CurrentCacheVersion = 1;

    // The list of leap years only goes until 2056 as we need to touch this file then
    // again anyway ;)
    constexpr const std::array<int, 36> LeapYears = {
        1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996,
        2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028, 2032, 2036, 2040,
        2044, 2048, 2052, 2056
    };
    constexpr const std::array<int, 12> DaysOfMonths = {
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
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
        // Find the position of the current year in the vector; its position in the vector
        // gives the number of leap seconds
        struct LeapSecond {
            int year;
            int dayOfYear;
            bool operator<(const LeapSecond& rhs) const {
                return std::tie(year, dayOfYear) < std::tie(rhs.year, rhs.dayOfYear);
            }
        };

        constexpr const LeapSecond LeapEpoch = { 2000, 1 };

        // List taken from: https://www.ietf.org/timezones/data/leap-seconds.list
        constexpr const std::array<LeapSecond, 28> LeapSeconds = {
            LeapSecond{ 1972,   1 },
            LeapSecond{ 1972, 183 },
            LeapSecond{ 1973,   1 },
            LeapSecond{ 1974,   1 },
            LeapSecond{ 1975,   1 },
            LeapSecond{ 1976,   1 },
            LeapSecond{ 1977,   1 },
            LeapSecond{ 1978,   1 },
            LeapSecond{ 1979,   1 },
            LeapSecond{ 1980,   1 },
            LeapSecond{ 1981, 182 },
            LeapSecond{ 1982, 182 },
            LeapSecond{ 1983, 182 },
            LeapSecond{ 1985, 182 },
            LeapSecond{ 1988,   1 },
            LeapSecond{ 1990,   1 },
            LeapSecond{ 1991,   1 },
            LeapSecond{ 1992, 183 },
            LeapSecond{ 1993, 182 },
            LeapSecond{ 1994, 182 },
            LeapSecond{ 1996,   1 },
            LeapSecond{ 1997, 182 },
            LeapSecond{ 1999,   1 },
            LeapSecond{ 2006,   1 },
            LeapSecond{ 2009,   1 },
            LeapSecond{ 2012, 183 },
            LeapSecond{ 2015, 182 },
            LeapSecond{ 2017,   1 }
        };
        // Get the position of the last leap second before the desired date
        LeapSecond date{ year, dayOfYear };
        const auto it = std::lower_bound(LeapSeconds.begin(), LeapSeconds.end(), date);

        // Get the position of the Epoch
        const auto y2000 = std::lower_bound(
            LeapSeconds.begin(),
            LeapSeconds.end(),
            LeapEpoch
        );

        // The distance between the two iterators gives us the number of leap years
        const int nLeapSeconds = static_cast<int>(std::abs(std::distance(y2000, it)));
        return nLeapSeconds;
    }

    int daysIntoGivenYear(int year, int month, int dayOfMonth) {
        // month and dayCount are zero-based.
        month -= 1;
        int dayCount = dayOfMonth - 1;
        constexpr int February = 1;
        const bool isInLeapYear =
            std::find(LeapYears.begin(), LeapYears.end(), year) != LeapYears.end();

        for (int m = 0; m < month; ++m) {
            dayCount += DaysOfMonths[m];
            if (m == February && isInLeapYear) {
                dayCount += 1;
            }
        }
        return dayCount;
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
        const double period =
            std::chrono::seconds(std::chrono::hours(24)).count() / meanMotion;

        constexpr const double pisq = glm::pi<double>() * glm::pi<double>();
        const double semiMajorAxis = pow(
            (muEarth * period * period) / (4 * pisq),
            1.0 / 3.0
        );

        // We need the semi major axis in km instead of m
        return semiMajorAxis / 1000.0;
    }


    double epochFromSubstring(const std::string& epoch) {
        // The epochString is in the form:
        // YYDDD.DDDDDDDD
        // With YY being the last two years of the launch epoch, the first DDD the day of
        // the year and the remaning a fractional part of the day

        // The main overview of this function:
        // 1. Reconstruct the full year from the YY part
        // 2. Calculate the number of days since the beginning of the year
        // 3. Convert the number of days to a number of seconds
        // 4. Get the number of leap seconds since January 1st, 2000 and remove them
        // 5. Adjust for the fact the epoch starts on 1st Januaray at 12:00:00, not
        // midnight

        // According to https://celestrak.com/columns/v04n03/
        // Apparently, US Space Command sees no need to change the two-line element set
        // format yet since no artificial earth satellites existed prior to 1957. By their
        // reasoning, two-digit years from 57-99 correspond to 1957-1999 and those from
        // 00-56 correspond to 2000-2056. We'll see each other again in 2057!

        // 1,2. Get the full year and days
        auto [res, year, daysInYear] = scn::scan_tuple<int, double>(epoch, "{:2}{}");
        if (!res) {
            throw ghoul::RuntimeError(fmt::format("Error parsing epoch '{}'", epoch));
        }
        year += year > 57 ? 1900 : 2000;
        const int daysSince2000 = countDays(year);

        // 3
        using namespace std::chrono;
        const int SecondsPerDay = static_cast<int>(seconds(hours(24)).count());
        //Need to subtract 1 from daysInYear since it is not a zero-based count
        const double nSecondsSince2000 = (daysSince2000 + daysInYear - 1) * SecondsPerDay;

        // 4
        // We need to remove additional leap seconds past 2000 and add them prior to
        // 2000 to sync up the time zones
        const double nLeapSecondsOffset = countLeapSeconds(
            year,
            static_cast<int>(std::floor(daysInYear))
        );

        // 5
        const double nSecondsEpochOffset =
            static_cast<double>(seconds(hours(12)).count());

        // Combine all of the values
        return nSecondsSince2000 - nLeapSecondsOffset - nSecondsEpochOffset;
    }

    double epochFromYMDdSubstring(const std::string& epoch) {
        // The epochString is in the form:
        // YYYYMMDD.ddddddd
        // With YYYY as the year, MM the month (1 - 12), DD the day of month (1-31),
        // and dddd the fraction of that day.

        // The main overview of this function:
        // 1. Read the year value
        // 2. Calculate the number of days since the beginning of the year
        // 3. Convert the number of days to a number of seconds
        // 4. Get the number of leap seconds since January 1st, 2000 and remove them
        // 5. Adjust for the fact the epoch starts on 1st January at 12:00:00, not
        // midnight

        // 1, 2
        auto [res, year, monthNum, dayOfMonthNum, fractionOfDay] =
            scn::scan_tuple<int, int, int, double>(epoch, "{:4}{:2}{:2}{}");
        if (!res) {
            throw ghoul::RuntimeError(fmt::format("Error parsing epoch '{}'", epoch));
        }
        const int daysSince2000 = countDays(year);
        int wholeDaysInto = daysIntoGivenYear(year, monthNum, dayOfMonthNum);
        double daysInYear = static_cast<double>(wholeDaysInto) + fractionOfDay;

        // 3
        using namespace std::chrono;
        const int SecondsPerDay = static_cast<int>(seconds(hours(24)).count());
        //Need to subtract 1 from daysInYear since it is not a zero-based count
        const double nSecondsSince2000 = (daysSince2000 + daysInYear - 1) * SecondsPerDay;

        // 4
        // We need to remove additional leap seconds past 2000 and add them prior to
        // 2000 to sync up the time zones
        const double nLeapSecondsOffset = -countLeapSeconds(
            year,
            static_cast<int>(std::floor(daysInYear))
        );

        // 5
        const double offset = static_cast<double>(seconds(hours(12)).count());

        // Combine all of the values
        return nSecondsSince2000 + nLeapSecondsOffset - offset;
    }

    double epochFromOmmString(const std::string& epoch) {
        // The epochString is in the form:
        // YYYY-MM-DDThh:mm:ss[.d->d][Z]
        // or
        // YYYY-DDDThh:mm:ss[.d->d][Z]

        // The main overview of this function:
        // 0. Determine which type it is
        // 1. Read the year value
        // 2. Calculate the number of days since the beginning of the year
        // 3. Convert the number of days to a number of seconds
        // 4. Get the number of leap seconds since January 1st, 2000 and remove them
        // 5. Add the hh:mm:ss component
        // 6. Adjust for the fact the epoch starts on 1st January at 12:00:00, not
        // midnight

        std::string e = epoch;
        if (e.back() == 'Z') {
            e.pop_back();
        }

        struct {
            int year;
            int nDays;
            int hours;
            int minutes;
            double seconds;
        } date;

        // 1, 2
        const size_t pos = epoch.find('T');
        if (pos == 10) {
            // We have the first form
            int month;
            int days;
            auto res = scn::scan(
                epoch, "{:4}-{:2}-{:2}T{:2}:{:2}:{}",
                date.year, month, days, date.hours, date.minutes, date.seconds
            );
            if (!res) {
                throw ghoul::RuntimeError(fmt::format("Error parsing epoch '{}'", epoch));
            }
            date.nDays = daysIntoGivenYear(date.year, month, days);
        }
        else if (pos == 8) {
            // We have the second form

            auto res = scn::scan(
                epoch, "{:4}-{:3}T{:2}:{:2}:{}",
                date.year, date.nDays, date.hours, date.minutes, date.seconds
            );
            if (!res) {
                throw ghoul::RuntimeError(fmt::format("Error parsing epoch '{}'", epoch));
            }
        }
        else {
            throw ghoul::RuntimeError(fmt::format("Malformed epoch string '{}'", epoch));
        }

        const int daysSince2000 = countDays(date.year);

        // 3
        using namespace std::chrono;
        const int SecondsPerDay = static_cast<int>(seconds(hours(24)).count());
        const double nSecondsSince2000 = (daysSince2000 + date.nDays) * SecondsPerDay;

        // 4
        // We need to remove additional leap seconds past 2000 and add them prior to
        // 2000 to sync up the time zones
        const double nLeapSecondsOffset = -countLeapSeconds(
            date.year,
            static_cast<int>(std::floor(date.nDays))
        );

        // 5
        const long long totalSeconds =
            std::chrono::seconds(std::chrono::hours(date.hours)).count() +
            std::chrono::seconds(std::chrono::minutes(date.minutes)).count();

        // 6
        const long long offset = std::chrono::seconds(std::chrono::hours(12)).count();

        // Combine all of the values
        return
            nSecondsSince2000 + totalSeconds + nLeapSecondsOffset - offset + date.seconds;
    }
} // namespace

namespace openspace::kepler {

std::vector<Parameters> readTleFile(std::filesystem::path file) {
    ghoul_assert(std::filesystem::is_regular_file(file), "File must exist");

    std::vector<Parameters> result;

    std::ifstream f;
    f.open(file);

    int lineNum = 1;

    std::string header;
    while (std::getline(f, header)) {
        Parameters p;

        // Header
        p.name = header;

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
        std::string firstLine;
        std::getline(f, firstLine);
        if (f.bad() || firstLine[0] != '1') {
            throw ghoul::RuntimeError(fmt::format(
                "Malformed TLE file '{}' at line {}", file, lineNum + 1
            ));
        }
        // The id only contains the last two digits of the launch year, so we have to
        // patch it to the full year
        {
            std::string id = firstLine.substr(9, 6);
            std::string prefix = [y = id.substr(0, 2)](){
                int year = std::atoi(y.c_str());
                return year >= 57 ? "19" : "20";
            }();
            p.id = fmt::format("{}{}-{}", prefix, id.substr(0, 2), id.substr(3));
        }
        p.epoch = epochFromSubstring(firstLine.substr(18, 14)); // should be 13?


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
        std::string secondLine;
        std::getline(f, secondLine);
        if (f.bad() || secondLine[0] != '2') {
            throw ghoul::RuntimeError(fmt::format(
                "Malformed TLE file '{}' at line {}", file, lineNum + 1
            ));
        }

        std::stringstream stream;
        stream.exceptions(std::ios::failbit);

        // Get inclination
        stream.str(secondLine.substr(8, 8));
        stream >> p.inclination;
        stream.clear();

        // Get Right ascension of the ascending node
        stream.str(secondLine.substr(17, 8));
        stream >> p.ascendingNode;
        stream.clear();

        // Get Eccentricity
        stream.str("0." + secondLine.substr(26, 7));
        stream >> p.eccentricity;
        stream.clear();

        // Get argument of periapsis
        stream.str(secondLine.substr(34, 8));
        stream >> p.argumentOfPeriapsis;
        stream.clear();

        // Get mean anomaly
        stream.str(secondLine.substr(43, 8));
        stream >> p.meanAnomaly;
        stream.clear();

        // Get mean motion
        stream.str(secondLine.substr(52, 11));
        float meanMotion;
        stream >> meanMotion;

        p.semiMajorAxis = calculateSemiMajorAxis(meanMotion);
        p.period = std::chrono::seconds(std::chrono::hours(24)).count() / meanMotion;

        result.push_back(p);

        lineNum = lineNum + 3;
    }

    return result;
}

std::vector<Parameters> readOmmFile(std::filesystem::path file) {
    ghoul_assert(std::filesystem::is_regular_file(file), "File must exist");

    std::vector<Parameters> result;

    std::ifstream f;
    f.open(file);

    int lineNum = 1;
    std::optional<Parameters> current = std::nullopt;
    std::string line;
    while (std::getline(f, line)) {
        if (line.empty() || line == "\r") {
            continue;
        }

        // Tokenize the line
        std::vector<std::string> parts = ghoul::tokenizeString(line, '=');
        for (std::string& p : parts) {
            ghoul::trimWhitespace(p);
        }

        if (parts.size() != 2) {
            throw ghoul::RuntimeError(fmt::format(
                "Malformed line '{}' at {}", line, lineNum
            ));
        }

        if (parts[0] == "CCSDS_OMM_VERS") {
            if (parts[1] != "2.0") {
                LWARNINGC(
                    "OMM",
                    fmt::format(
                        "Only version 2.0 is currently supported but found {}. "
                        "Parsing might fail",
                        parts[1]
                    )
                );
            }

            // We start a new value so we need to store the last one...
            if (current.has_value()) {
                result.push_back(*current);
            }

            // ... and start a new one
            current = Parameters();
        }

        ghoul_assert(current.has_value(), "No current element");

        if (parts[0] == "OBJECT_NAME") {
            current->name = parts[1];
        }
        else if (parts[0] == "OBJECT_ID") {
            current->id = parts[1];
        }
        else if (parts[0] == "EPOCH") {
            current->epoch = epochFromOmmString(parts[1]);
        }
        else if (parts[0] == "MEAN_MOTION") {
            float mm = std::stof(parts[1]);
            current->semiMajorAxis = calculateSemiMajorAxis(mm);
            current->period = std::chrono::seconds(std::chrono::hours(24)).count() / mm;
        }
        else if (parts[0] == "SEMI_MAJOR_AXIS") {

        }
        else if (parts[0] == "ECCENTRICITY") {
            current->eccentricity = std::stof(parts[1]);
        }
        else if (parts[0] == "INCLINATION") {
            current->inclination = std::stof(parts[1]);
        }
        else if (parts[0] == "RA_OF_ASC_NODE") {
            current->ascendingNode = std::stof(parts[1]);
        }
        else if (parts[0] == "ARG_OF_PERICENTER") {
            current->argumentOfPeriapsis = std::stof(parts[1]);
        }
        else if (parts[0] == "MEAN_ANOMALY") {
            current->meanAnomaly = std::stof(parts[1]);
        }
    }

    if (current.has_value()) {
        result.push_back(*current);
    }

    return result;
}

std::vector<Parameters> readSbdbFile(std::filesystem::path file) {
    constexpr int NDataFields = 9;
    constexpr std::string_view ExpectedHeader = "full_name,epoch_cal,e,a,i,om,w,ma,per";

    ghoul_assert(std::filesystem::is_regular_file(file), "File must exist");

    std::ifstream f;
    f.open(file);

    std::string line;
    std::getline(f, line);
    if (line != ExpectedHeader) {
        throw ghoul::RuntimeError(fmt::format(
            "Expected JPL SBDB file to start with '{}' but found '{}' instead",
            ExpectedHeader, line
        ));
    }

    std::vector<Parameters> result;
    while (std::getline(f, line)) {
        constexpr double AuToKm = 1.496e8;

        std::vector<std::string> parts = ghoul::tokenizeString(line, ',');
        if (parts.size() != NDataFields) {
            throw ghoul::RuntimeError(fmt::format(
                "Malformed line {}, expected 8 data fields, got {}", line, parts.size()
            ));
        }
        Parameters p;

        ghoul::trimWhitespace(parts[0]);
        p.name = parts[0];

        p.epoch = epochFromYMDdSubstring(parts[1]);
        p.eccentricity = std::stod(parts[2]);
        p.semiMajorAxis = std::stod(parts[3]) * AuToKm;

        auto importAngleValue = [](const std::string& angle) {
            if (angle.empty()) {
                return 0.0;
            }

            double output = std::stod(angle);
            output = std::fmod(output, 360.0);
            if (output < 0.0) {
                output += 360.0;
            }
            return output;
        };

        p.inclination = importAngleValue(parts[4]);
        p.ascendingNode = importAngleValue(parts[5]);
        p.argumentOfPeriapsis = importAngleValue(parts[6]);
        p.meanAnomaly = importAngleValue(parts[7]);
        p.period =
            std::stod(parts[8]) * std::chrono::seconds(std::chrono::hours(24)).count();

        result.push_back(std::move(p));
    }
    return result;
}

void saveCache(const std::vector<Parameters>& params, std::filesystem::path file) {
    std::ofstream stream(file, std::ofstream::binary);

    stream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

    uint32_t size = static_cast<uint32_t>(params.size());
    stream.write(reinterpret_cast<const char*>(&size), sizeof(uint32_t));
    for (const Parameters& param : params) {
        uint32_t nameLength = static_cast<uint32_t>(param.name.size());
        stream.write(reinterpret_cast<const char*>(&nameLength), sizeof(uint32_t));
        stream.write(param.name.data(), nameLength * sizeof(char));

        uint32_t idLength = static_cast<uint32_t>(param.id.size());
        stream.write(reinterpret_cast<const char*>(&idLength), sizeof(uint32_t));
        stream.write(param.id.data(), idLength * sizeof(char));

        stream.write(reinterpret_cast<const char*>(&param.inclination), sizeof(double));
        stream.write(reinterpret_cast<const char*>(&param.semiMajorAxis), sizeof(double));
        stream.write(reinterpret_cast<const char*>(&param.ascendingNode), sizeof(double));
        stream.write(reinterpret_cast<const char*>(&param.eccentricity), sizeof(double));
        stream.write(
            reinterpret_cast<const char*>(&param.argumentOfPeriapsis),
            sizeof(double)
        );
        stream.write(reinterpret_cast<const char*>(&param.meanAnomaly), sizeof(double));
        stream.write(reinterpret_cast<const char*>(&param.epoch), sizeof(double));
        stream.write(reinterpret_cast<const char*>(&param.period), sizeof(double));
    }
}

std::optional<std::vector<Parameters>> loadCache(std::filesystem::path file) {
    std::ifstream stream(file, std::ifstream::binary);

    int8_t version = 0;
    stream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
    if (version != CurrentCacheVersion) {
        LINFO("The format of the cached file has changed");
        return std::nullopt;
    }

    uint32_t size = 0;
    stream.read(reinterpret_cast<char*>(&size), sizeof(uint32_t));
    std::vector<Parameters> res;
    res.reserve(size);
    for (uint32_t i = 0; i < size; i++) {
        Parameters param;

        uint32_t nameLength = 0;
        stream.read(reinterpret_cast<char*>(&nameLength), sizeof(uint32_t));
        param.name.resize(nameLength);
        stream.read(param.name.data(), nameLength * sizeof(char));

        uint32_t idLength = 0;
        stream.read(reinterpret_cast<char*>(&idLength), sizeof(uint32_t));
        param.id.resize(idLength);
        stream.read(param.id.data(), idLength * sizeof(char));

        stream.read(reinterpret_cast<char*>(&param.inclination), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.semiMajorAxis), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.ascendingNode), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.eccentricity), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.argumentOfPeriapsis), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.meanAnomaly), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.epoch), sizeof(double));
        stream.read(reinterpret_cast<char*>(&param.period), sizeof(double));

        res.push_back(std::move(param));
    }

    return res;
}

std::vector<Parameters> readFile(std::filesystem::path file, Format format) {
    std::filesystem::path cachedFile = FileSys.cacheManager()->cachedFilename(file);
    if (std::filesystem::is_regular_file(cachedFile)) {
        LINFO(fmt::format(
            "Cached file {} used for Kepler file {}", cachedFile, file
        ));

        std::optional<std::vector<Parameters>> res = loadCache(cachedFile);
        if (res.has_value()) {
            return *res;
        }

        // If there is no value in the optional, the cached loading failed
    }

    std::vector<Parameters> res;
    switch (format) {
        case Format::TLE:
            res = readTleFile(file);
            break;
        case Format::OMM:
            res = readOmmFile(file);
            break;
        case Format::SBDB:
            res = readSbdbFile(file);
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    LINFO(fmt::format("Saving cache {} for Kepler file {}", cachedFile, file));
    saveCache(res, cachedFile);
    return res;
}

} // namespace openspace::kepler
