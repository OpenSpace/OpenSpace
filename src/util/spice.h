#ifndef SPICE_H
#define SPICE_H

// open space includes
#include "Object.h"

// std includes
#include <string>

namespace openspace
{

class Spice: public Object {
public:
	virtual ~Spice();

	static void init();
	static void deinit();
    static Spice& ref();
	static bool isInitialized();

	void loadDefaultKernels();
	bool loadKernel(const std::string &path);

	void bod_NameToInt(const std::string &name, int *id, int *success);
	bool getRadii(const std::string & name, double radii[3], int *n);
	
	bool spk_getPosition(const std::string &target, const std::string &origin, double state[3]);
	void spk_getPosition(int target, int origin, double state[3]);
	bool spk_getOrientation(const std::string &target, double state[3][3]);

private:
	static Spice* this_;
    Spice(void);
    Spice(const Spice& src);
    Spice& operator=(const Spice& rhs);



};


} // namespace openspace

#endif