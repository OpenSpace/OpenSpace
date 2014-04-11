/*
Auhtor: Victor Sand (victor.sand@gmail.com)
Global helper functions and utilities
*/

#ifndef UTILS_H_
#define UTILS_H_

#include <iostream>

 namespace osp {

#define INFO(x) std::cout << x << std::endl;
#define DEBUG(x) std::cout << "DEBUG: " << x << std::endl
#define ERROR(x) std::cout << "ERROR: " << x << std::endl
#define WARNING(x) std::cout << "WARNING: " << x << std::endl

// Print the last GL error if there is one, function returns the code for
// the error so that it can be compared to GL_NO_ERROR, for example
unsigned int CheckGLError(std::string _location);

}

#endif

