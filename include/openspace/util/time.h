#ifndef ENGINETIME_H
#define ENGINETIME_H

namespace openspace
{

class Time {
public:
	virtual ~Time();

	static void init();
	static void deinit();
    static Time& ref();
	static bool isInitialized();

	void setTime(const char* stringTime);
	void setTime(double intTime);
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