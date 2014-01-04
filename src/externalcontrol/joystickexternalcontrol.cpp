//#include "externalcontrol/joystickexternalcontrol.h"
//#include "deviceidentifier.h"
//
//// workaround since Python debug does not work on windows
//#ifdef _DEBUG
//#undef _DEBUG
//#include <Python.h>
//#define _DEBUG
//#else
//#include <Python.h>
//#endif
//
//namespace openspace {
//
//
//JoystickExternalControl::JoystickExternalControl(const char *filename): PythonExternalControl(filename) {
//}
//
//void JoystickExternalControl::setInputDevice(const int device) {
//	if(device >= 0 && device <= 16) {
//		inputDevice_ = device;
//		numberOfButtons_ = DeviceIdentifier::ref().getButtons(inputDevice_);
//		numberOfAxes_ = DeviceIdentifier::ref().getAxes(inputDevice_);
//		clear();
//		pyarrSize_ = numberOfButtons_ + numberOfAxes_;
//		pyarr_ = new PyObject*[pyarrSize_];
//	}
//	
//}
//
//void JoystickExternalControl::update() {
//	
//	if(inputDevice_ != -1) {
//		float *axesPos;
//		unsigned char *buttons;
//		DeviceIdentifier::ref().getButtons(inputDevice_, &buttons);
//		DeviceIdentifier::ref().getAxes(inputDevice_, &axesPos);
//
//		// init array
//		for(int i = 0; i < numberOfButtons_; ++i){
//			pyarr_[i] = PyLong_FromLong(buttons[i]);
//		}
//		for(int i = 0; i < numberOfAxes_; ++i){
//			pyarr_[i+numberOfButtons_] = PyFloat_FromDouble(axesPos[i]);
//		}
//	} 
//
//	run();
//
//	if(inputDevice_ != -1) {
//		// cleanup
//		for(int i = 0; i < pyarrSize_; ++i) {
//			Py_DECREF(pyarr_[i]);
//		}
//	}
//	
//}
//
//JoystickExternalControl::~JoystickExternalControl() {
//}
//
//} // namespace openspace
//
