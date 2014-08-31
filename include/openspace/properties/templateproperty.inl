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
namespace properties {

#define REGISTER_TEMPLATEPROPERTY_HEADER(CLASS_NAME, TYPE)                               \
    typedef TemplateProperty<TYPE> CLASS_NAME;                                           \
    template <>                                                                          \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className();                   \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::defaultValue<TYPE>();

#define REGISTER_TEMPLATEPROPERTY_SOURCE(CLASS_NAME, TYPE, DEFAULT_VALUE)                \
    template <>                                                                          \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className() {                  \
        return #CLASS_NAME;                                                              \
    \
}                                                                                 \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::defaultValue<TYPE>() {                \
        return DEFAULT_VALUE;                                                            \
    \
}

// Delegating constructors are necessary; automatic template deduction cannot
// deduce template argument for 'U' if 'default' methods are used as default values in
// a single constructor

template <typename T>
TemplateProperty<T>::TemplateProperty(std::string identifier, std::string guiName)
    : TemplateProperty<T>(std::move(identifier), std::move(guiName),
                          PropertyDelegate<TemplateProperty<T>>::template defaultValue<T>()) {
}

template <typename T>
TemplateProperty<T>::TemplateProperty(std::string identifier, std::string guiName,
                                      T value)
    : Property(std::move(identifier), std::move(guiName))
    , _value(value) {
}

template <typename T>
std::string TemplateProperty<T>::className() const {
    return PropertyDelegate<TemplateProperty<T>>::className();
}

template <typename T>
TemplateProperty<T>::operator T() {
    return _value;
}

template <typename T>
TemplateProperty<T>& TemplateProperty<T>::operator=(T val) {
    setValue(val);
    return *this;
}

template <typename T>
T openspace::properties::TemplateProperty<T>::value() const
{
    return _value;
}

template <typename T>
void openspace::properties::TemplateProperty<T>::setValue(T val)
{
    _value = val;
}


template <typename T>
boost::any TemplateProperty<T>::get() const {
    return boost::any(_value);
}

template <typename T>
void TemplateProperty<T>::set(boost::any value) {
    try {
        _value = boost::any_cast<T>(std::move(value));
		notifyListeners();
    }
    catch (boost::bad_any_cast&) {
        LERRORC("TemplateProperty", "Illegal cast from '" << value.type().name()
			<< "' to '" << typeid(T).name() << "'");
    }
}

template <typename T>
const std::type_info& TemplateProperty<T>::type() const {
    return typeid(T);
}

}  // namespace properties
}  // namespace openspace
