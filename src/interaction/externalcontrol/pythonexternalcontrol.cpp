//#include "externalcontrol/pythonexternalcontrol.h"
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
//#include <iostream>
//#include <fstream>
//#include <string>
//#include <cstdio>
//#include "deviceidentifier.h"
//
//#include "util/pss.h"
//#include "externalcontrol/externalcontrol.h"
//#include "interactionhandler.h"
//
//namespace openspace {
//
//
//// defining python callback functions
//static PyObject* pyexcontrol_numargs(PyObject *self, PyObject *args) {
//    if(!PyArg_ParseTuple(args, ":pyexcontrol_numargs"))
//        return NULL;
//    return PyLong_FromLong(10);
//}
//
//static PyObject* pyexcontrol_message(PyObject *self, PyObject *args)
//{
//    char* text = 0;
//	 if(!PyArg_ParseTuple(args, "s:pyexcontrol_yeah",&text))
//        Py_RETURN_NONE;
//	
//	PythonExternalControl * ext = 0;
//	ext->message(text);
//
//	Py_RETURN_NONE;
//}
//
//static PyObject* pyexcontrol_rotateCamera(PyObject *self, PyObject *args)
//{
//	char* text = 0;
//	float f1 = 0;
//	float f2 = 0;
//	float f3 = 0;
//	float f4 = 0;
//	if(!PyArg_ParseTuple(args, "fff:pyexcontrol_rotateCamera",&f1,&f2,&f3))
//		Py_RETURN_NONE;
//	
//	double dt = InteractionHandler::ref().getDt();
//	glm::vec3 EulerAngles(f1*dt,f2*dt, f3*dt);
//	glm::quat rot = glm::quat(EulerAngles);
//	ExternalControl * ext = 0;
//	ext->rotate(rot);
//
//	Py_RETURN_NONE;
//}
//
//static PyObject* pyexcontrol_orbitCamera(PyObject *self, PyObject *args)
//{
//	char* text = 0;
//	float f1 = 0;
//	float f2 = 0;
//	float f3 = 0;
//	if(!PyArg_ParseTuple(args, "fff:pyexcontrol_rotateCamera",&f1,&f2,&f3))
//		Py_RETURN_NONE;
//	
//	double dt = InteractionHandler::ref().getDt();
//	glm::vec3 EulerAngles(f1*dt,f2*dt, f3*dt);
//	glm::quat rot = glm::quat(EulerAngles);
//	ExternalControl * ext = 0;
//	ext->orbit(rot);
//	
//	Py_RETURN_NONE;
//}
//
//static PyObject* pyexcontrol_distance(PyObject *self, PyObject *args)
//{
//	char* text = 0;
//	float f1 = 0;
//	float f2 = 0;
//	if(!PyArg_ParseTuple(args, "ff:pyexcontrol_rotateCamera",&f1,&f2))
//		Py_RETURN_NONE;
//	
//	float dt = static_cast<float>(InteractionHandler::ref().getDt());
//	pss dist(f1*dt,f2);
//	ExternalControl * ext = 0;
//	ext->distance(dist);
//	
//	Py_RETURN_NONE;
//}
//
//PyMethodDef* PythonExternalControl::getMethodDef() {
//	// creating the python callback function table
//	static PyMethodDef pyexcontrol_methods[] = {
//		{"numargs", pyexcontrol_numargs, METH_VARARGS, "function"},
//		{"message", pyexcontrol_message, METH_VARARGS, "function"},
//		{"rotate", pyexcontrol_rotateCamera, METH_VARARGS, "function"},
//		{"orbit", pyexcontrol_orbitCamera, METH_VARARGS, "function"},
//		{"distance", pyexcontrol_distance, METH_VARARGS, "function"},
//		 {NULL, NULL, 0, NULL}
//	};
//	return pyexcontrol_methods;
//}
//
//void PythonExternalControl::message(const char *text) {
//	
//	 printf("Input message from PythonScript: %s\n", text);
//}
//
//PythonExternalControl::PythonExternalControl(const char *filename) {
//	pyarr_ = nullptr;
//	ps_.load(filename, true);
//}
//
//
//void PythonExternalControl::update() {
//	run();
//}
//
//void PythonExternalControl::run() {
//	if(pyarrSize_ > 0)
//		ps_.run(pyarrSize_, pyarr_);
//	else
//		ps_.run();
//}
//
//
//void PythonExternalControl::clear() {
//	if(pyarr_ != nullptr) {
//		// cleanup
//		delete pyarr_;
//		pyarr_ = nullptr;
//		pyarrSize_ = 0;
//	}
//}
//
//PythonExternalControl::~PythonExternalControl() {
//	clear();
//}
//
//} // namespace openspace
//
