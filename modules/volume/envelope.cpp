#include <modules/volume/envelope.h>

namespace openspace {
    namespace volume {
        Envelope::Envelope() { }

        Envelope::Envelope(glm::vec3 col, std::vector<std::pair<float, float>> vec) {
            color = col;
            _points = vec;
        }

        bool Envelope::operator!=(const Envelope& env) const {
            const double minDist = 0.0001;
            if (color != env.color)
                return true;
            
            if (_points.size() != env._points.size())
                return true;

            auto iter = _points.begin();
            auto envIter = env._points.begin();
            for (; iter != _points.end(); ++iter, ++envIter) {
                if (abs(iter->first - envIter->first) > minDist || abs(iter->second - envIter->second) > minDist)
                    return true;
            }
            return false;
        }

        void Envelope::setColor(std::string hex) {
            this->color = hexadecimalToRGBConversion(hex);
        }

        void Envelope::setPoints(std::vector<std::pair<float, float>> vec) {
            this->_points = vec;
        }

        bool Envelope::isValueInEnvelope(float pos) const {
            if (!_points.empty()) {
                if (_points.front().first <= pos && _points.back().first >= pos)
                    return true;
            }
            return false;
        }

        glm::vec4 Envelope::getValueAtPosition(float pos) const {
            if (!isValueInEnvelope(pos))
                return glm::vec4{ 0.f, 0.f , 0.f , 0.f };

            auto afterIter = _points.begin();
            while (afterIter->first <= pos) { ++afterIter; }
            if (afterIter->first == pos) {
                return glm::vec4{ color, afterIter->second };
            }
            auto beforeIter = --afterIter;

            float dist = afterIter->first - beforeIter->first;
            float alpha = beforeIter->second *(abs(pos - afterIter->first)/dist) + afterIter->second *(abs(pos - afterIter->first)/dist);
            return glm::vec4{ color, alpha * 255.f};
        }

        int Envelope::HexadecimalToDecimal(std::string hex) const {
            int hexLength = hex.length();
            double dec = 0;
            for (int i = 0; i < hexLength; ++i)
            {
                char b = hex[i];

                if (b >= 48 && b <= 57)
                    b -= 48;
                else if (b >= 65 && b <= 70)
                    b -= 55;
                else if (b >= 97 && b <= 102)
                    b -= 87;
                dec += b * pow(16, ((hexLength - i) - 1));
            }
            return (int)dec;
        }

        std::string Envelope::DecimalToHexadecimal(int dec) const {
            if (dec < 1) return "00";

            int hex = dec;
            std::string hexStr = "";

            while (dec > 0)
            {
                hex = dec % 16;

                if (hex < 10)
                    hexStr = hexStr.insert(0, std::string(1, (hex + 48)));
                else
                    hexStr = hexStr.insert(0, std::string(1, (hex + 55)));
                dec /= 16;
            }
            return hexStr;
        }

        glm::vec3 Envelope::hexadecimalToRGBConversion(std::string hex) const {
            float r = static_cast<float>(HexadecimalToDecimal(hex.substr(1, 2)));
            float g = static_cast<float>(HexadecimalToDecimal(hex.substr(3, 2)));
            float b = static_cast<float>(HexadecimalToDecimal(hex.substr(5, 2)));

            return glm::vec3(r, g, b);
        }

        std::string Envelope::getHexadecimal() const {

            std::string r = DecimalToHexadecimal(static_cast<int>(color.r));
            std::string g = DecimalToHexadecimal(static_cast<int>(color.g));
            std::string b = DecimalToHexadecimal(static_cast<int>(color.b));

            return ("#" + r + g + b);
        }

        json Envelope::getSerializedPoints() const {
            json j;
            for (int i = 0; i < _points.size(); i++) {
                j[i] = {
                    { "position",{
                        { "x", _points.at(i).first },
                        { "y", _points.at(i).second }
                    }
                    }
                };
            }
            return j;
        }

        json Envelope::getSerializedEnvelope() const {
            json j;
            j["color"] = getHexadecimal();
            j["points"] = getSerializedPoints();
            return j;
        }

    }//namespace volume
}//namespace openspace
