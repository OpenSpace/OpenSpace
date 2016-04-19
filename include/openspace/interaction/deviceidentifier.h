#ifndef DEVICEIDENTIFIER_H
#define DEVICEIDENTIFIER_H

// std includes
#include <array>
#include <mutex>
#include <memory>

namespace openspace {

#define MAXDEVICES 16

enum class InputDevice {NONE, UNKNOWN, SPACENAVIGATOR, XBOX}; 

class DeviceIdentifier {
public:
    static DeviceIdentifier& ref();
    virtual ~DeviceIdentifier();

    static void init();
    static void deinit();
    static bool isInitialized();
    
    void scanDevices();
    const int numberOfDevices() const;
    const InputDevice type(const int device) const;
    
    void update();
    void update(const int device);

    const int getButtons(const int device, unsigned char **buttons = nullptr) const;
    const int getAxes(const int device, float **axespos = nullptr) const;
    void get(const int device, unsigned char **buttons, float **axespos) const;
    
private:
    // singleton
    static DeviceIdentifier* this_;
    DeviceIdentifier(void);
    DeviceIdentifier(const DeviceIdentifier& src);
    DeviceIdentifier& operator=(const DeviceIdentifier& rhs);


    // member variables
    int devices_;
    std::array<InputDevice, MAXDEVICES> inputDevice_;
    std::array<int, MAXDEVICES> numberOfAxes_;
    std::array<int, MAXDEVICES> numberOfButtons_;
    std::array<float *, MAXDEVICES> axesPos_;
    std::array<unsigned char *, MAXDEVICES> buttons_;

};

} // namespace openspace

#endif