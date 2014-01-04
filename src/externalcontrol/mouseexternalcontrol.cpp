//#include "externalcontrol/mouseexternalcontrol.h"
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
//MouseExternalControl::MouseExternalControl(const char *filename): PythonExternalControl(filename) {
//	clear();
//	pyarrSize_ = 6*2;
//	pyarr_ = new PyObject*[pyarrSize_];
//	x_ = 0;
//	y_ = 0;
//	pos_ = 0;
//	button1_ = 0;
//	button2_ = 0;
//	button3_ = 0;
//	for(int i = 0; i < pyarrSize_; ++i) {
//		pyarr_[i] = PyLong_FromLong(0);;
//	}
//
//}
//
//void MouseExternalControl::mouseButtonCallback(int key, int action) {
//	if(key == 0)
//		button1_ = action;
//	if(key == 1)
//		button2_ = action;
//	if(key == 2)
//		button3_ = action;
//}
//
//void MouseExternalControl::mousePosCallback(int x, int y) {
//	x_ = x;
//	y_ = y;
//}
//	
//void MouseExternalControl::mouseScrollCallback(int pos) {
//	pos_ = pos;
//}
//
//void MouseExternalControl::update() {
//	
//	pyarr_[6] = pyarr_[0];
//	pyarr_[7] = pyarr_[1];
//	pyarr_[8] = pyarr_[2];
//	pyarr_[9] = pyarr_[3];
//	pyarr_[10] = pyarr_[4];
//	pyarr_[11] = pyarr_[5];
//	pyarr_[0] = PyLong_FromLong(button1_);
//	pyarr_[1] = PyLong_FromLong(button2_);
//	pyarr_[2] = PyLong_FromLong(button3_);
//	pyarr_[3] = PyLong_FromLong(pos_);
//	pyarr_[4] = PyLong_FromLong(x_);
//	pyarr_[5] = PyLong_FromLong(y_);
//
//	run();
//
//	// cleanup
//	for(int i = pyarrSize_ / 2; i < pyarrSize_; ++i) {
//		Py_XDECREF(pyarr_[i]);
//	}
//	
//}
//
//MouseExternalControl::~MouseExternalControl() {
//	
//}
//
//} // namespace openspace
//
