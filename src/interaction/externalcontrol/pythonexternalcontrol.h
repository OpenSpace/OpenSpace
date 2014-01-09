//#ifndef PYTHONEXTERNALCONTROL_H
//#define PYTHONEXTERNALCONTROL_H
//
//#include <thread>
//#include <mutex>
//
//#include "externalcontrol/externalcontrol.h"
//#include "python/pythonscript.h"
//
//namespace openspace {
//
//class PythonExternalControl: public ExternalControl {
//public:
//
//	// constructors & destructor
//	PythonExternalControl(const char *filename);
//	~PythonExternalControl();
//
//	static PyMethodDef* getMethodDef();
//	
//	void message(const char *text);
//	virtual void update();
//	void clear();
//private:
//	PythonScript ps_;
//
//protected:
//	void run();
//	int pyarrSize_;
//	PyObject **pyarr_;
//};
//
//} // namespace openspace
//
//#endif