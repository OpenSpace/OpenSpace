#ifndef ENGINETIME_H
#define ENGINETIME_H

// open space includes
#include "Object.h"


namespace openspace
{

class Time: public Object {
public:
	virtual ~Time();

	static void init();
	static void deinit();
    static Time& ref();
	static bool isInitialized();

	void setTime(const char* stringTime);
	double getTime();

private:
	static Time* this_;
    Time(void);
    Time(const Time& src);
    Time& operator=(const Time& rhs);

	double time_;
};


} // namespace openspace

#endif