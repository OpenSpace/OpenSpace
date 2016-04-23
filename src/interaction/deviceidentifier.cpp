
// open space includes
#include <openspace/interaction/deviceidentifier.h>

#include <assert.h>

namespace openspace {


DeviceIdentifier* DeviceIdentifier::this_ = nullptr;

DeviceIdentifier::DeviceIdentifier() {

    // scan for devices on init
    devices_ = 0;
    for(int i = 0; i < MAXDEVICES; ++i) {
        inputDevice_[i] = InputDevice::NONE;
    }
}

DeviceIdentifier::~DeviceIdentifier() {

    // deallocates memory on exit
    for(int i = 0; i < MAXDEVICES; ++i) {
        if(inputDevice_[i] != InputDevice::NONE) {
            delete axesPos_[i];
            delete buttons_[i];
        }
    }
}

void DeviceIdentifier::init() {
    assert( ! this_);
    this_ = new DeviceIdentifier();
}

void DeviceIdentifier::deinit() {
    assert(this_);
    delete this_;
    this_ = nullptr;
}

DeviceIdentifier& DeviceIdentifier::ref() {
    assert(this_);
    return *this_;
}

bool DeviceIdentifier::isInitialized() {
    return this_ != nullptr;
}

void DeviceIdentifier::scanDevices() {
    assert(this_);

    // sgct/glfw supports 16 joysticks, scans all of them
    for (int i = 0; i < MAXDEVICES; ++i)
    {
        void* joystickName = NULL;
        if( joystickName != NULL ) {

            // allocate 
            axesPos_[i] = new float[numberOfAxes_[i]];
            buttons_[i] = new unsigned char[numberOfButtons_[i]];
            
            // increment the device count
            ++devices_;
            
            // identify what device it is
            if(numberOfAxes_[i] == 6 && numberOfButtons_[i] == 10) {
                printf("XBOX controller ");
                inputDevice_[i] = InputDevice::XBOX;
            } else if(numberOfAxes_[i] == 6 && numberOfButtons_[i] == 4) {
                printf("SPACENAVIGATOR ");
                inputDevice_[i] = InputDevice::SPACENAVIGATOR;
            } else {
                printf("UNKNOWN device ");
                inputDevice_[i] = InputDevice::UNKNOWN;
            }
            printf("found at position %i, b=%i, a=%i\n", i, numberOfButtons_[i], numberOfAxes_[i]);
            
            
        } else {
            inputDevice_[i] = InputDevice::NONE;
        }
        
    }
}

const int DeviceIdentifier::numberOfDevices() const {
    assert(this_);
    return devices_;
}

const InputDevice DeviceIdentifier::type(const int device) const {
    assert(this_);
    return inputDevice_[device];
}

void DeviceIdentifier::update() {
    assert(this_);
    for(int i = 0; i < devices_; ++i) {
        update(i);
    }
}

void DeviceIdentifier::update(const int device) {
    assert(this_);
    if(inputDevice_[device] != InputDevice::NONE) {
    }
}

const int DeviceIdentifier::getButtons(const int device, unsigned char **buttons) const {
    assert(this_);
    if(inputDevice_[device] != InputDevice::NONE) {
        if(buttons)
            *buttons = buttons_[device];
        return numberOfButtons_[device];
    }
    return 0;
}

const int DeviceIdentifier::getAxes(const int device, float **axespos) const {
    assert(this_);
    if(inputDevice_[device] != InputDevice::NONE) {
        if(axespos)
            *axespos = axesPos_[device];
        return numberOfAxes_[device];
    }
    return 0;
}

void DeviceIdentifier::get(const int device, unsigned char **buttons, float **axespos) const {
    assert(this_);
    if(inputDevice_[device] != InputDevice::NONE) {
         *axespos = axesPos_[device];
         *buttons = buttons_[device];
    }
}

} // namespace openspace
