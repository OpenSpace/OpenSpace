/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

namespace openspace {

template <typename T>
bool ConfigurationManager::getValue(const std::string& key, T& value) {
    // If none of the specializations fit, we don't know what to do
    LERROR("Unsupported type for key '" << key << "'");
    return false;
}

template <typename T>
void ConfigurationManager::setValue(const std::string& key, const T& value) {
    // If none of the specializations fit, we don't know what to do
    LERROR("Unsupported type for key '" << key << "'");
}

extern template void ConfigurationManager::setValue<>(const std::string& key, const char& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const signed char& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const unsigned char& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const wchar_t& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const short& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const unsigned short& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const int& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const unsigned int& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const long& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const unsigned long& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const long long& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const unsigned long long& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const float& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const double& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const long double& value);
extern template void ConfigurationManager::setValue<>(const std::string& key, const std::string& value);

extern template bool ConfigurationManager::getValue<>(const std::string& key, char& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, signed char& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, unsigned char& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, wchar_t& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, short& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, unsigned short& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, int& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, unsigned int& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, long& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, unsigned long& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, long long& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, unsigned long long& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, float& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, double& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, long double& value);
extern template bool ConfigurationManager::getValue<>(const std::string& key, std::string& value);

} // namespace openspace
