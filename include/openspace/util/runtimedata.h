#ifndef __RUNTIMEDATA_H__
#define __RUNTIMEDATA_H__

enum increment{
	YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, MILLISECOND
};

struct RuntimeData{
public:
	RuntimeData() :_openspaceTime(0.0){
	};
	void   setTime(double time) { _openspaceTime = time; }
	double getTime() { return _openspaceTime; }

	// cant come up with proper user friendly-method, this will do. 
	void advanceTimeBy(double nr, increment incr){
		switch (incr){
			case YEAR:        _openspaceTime += (nr * year);     break;
			case MONTH:       _openspaceTime += (nr * month);    break;
			case DAY:         _openspaceTime += (nr * day);      break;
			case HOUR:        _openspaceTime += (nr * hour);     break;
			case MINUTE:      _openspaceTime += (nr * minute);   break;
			case MILLISECOND: _openspaceTime += (nr * millisec); break;
			default :         _openspaceTime += (nr);
		}
	}

private:
	double _openspaceTime;

	double year     = 3.154*pow(10, 7);
	double month    = 2.628*pow(10, 6);
	double day      = 86400;
	double hour     = 3600;
	double minute   = 60;
	double millisec = 0.001;
};

#endif // __RUNTIMEDATA_H__
