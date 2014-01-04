//#include "externalcontrol/keyboardexternalcontrol.h"
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
//
//namespace openspace {
//
//
//KeyboardExternalControl::KeyboardExternalControl(const char *filename): PythonExternalControl(filename) {
//	clear();
//	pyarrSize_ = 'Z' - 'A' + 80; // all letters, 69 special keys, space and 10 numbers
//	pyarr_ = new PyObject*[pyarrSize_];
//	for(int i = 0; i < pyarrSize_; ++i) {
//		pyarr_[i] = PyLong_FromLong(0);
//	}
//}
//
//void KeyboardExternalControl::keyboardCallback(int key, int action) {
//	
//	//printf("key: %i\n",key);
//	int pos = -1;
//	if(key >= '0' && key <= '9') {
//		pos = key - '0';
//		Py_XDECREF(pyarr_[pos]);
//		pyarr_[pos] = PyLong_FromLong(action);
//	} else if(key >= 'A' && key <= 'Z') {
//		pos = key - 'A' + 10;
//		Py_XDECREF(pyarr_[pos]);
//		pyarr_[pos] = PyLong_FromLong(action);
//	} else if (key > 256 && key < 256+69) {
//		pos = key - 256 + 'Z'-'A' +10; 
//		Py_XDECREF(pyarr_[pos]);
//		pyarr_[pos] = PyLong_FromLong(action);
//	} else if (key == 32) {
//		pos = 'Z' - 'A' + 11;
//		Py_XDECREF(pyarr_[pos]);
//		pyarr_[pos] = PyLong_FromLong(action);
//	}
//	//printf("pos: %i\n",pos);
//}
//
//
//
//void KeyboardExternalControl::update() {
//	run();
//}
//
//KeyboardExternalControl::~KeyboardExternalControl() {
//	for(int i = 0; i < pyarrSize_; ++i) {
//		Py_XDECREF(pyarr_[i]);
//	}
//}
//
//} // namespace openspace
//
