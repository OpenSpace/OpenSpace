//1
//2
//3
//4
/** MicroSoft Visual C++ required macros, pragmas, and functions.  The
    macros are defined so alternate code can be conditionally compiled
    to work around the defects.  The pragmas are used to turn off some
    frequently seen warning messages.  The functions defined are part
    of the standard of the standard C++ library, but are missing from
    the MicroSoft library.

    This file is included directly when autoconf/automake is not being used
    on a MicroSoft Windows platform.  If autoconf/automake is being
    used, then it is also included in config.h.

    @todo @b Oded: Write a main program so that HippoDraw can be installed on
    the desktop. This reqires setting the path to the shared libraries
    correctly.

    @todo @b Oded: Associate icons with executable and document files.
    .bmp files for this purpose are in the images subdirectory.

    $Id: MSconfig.h,v 3.5 2010/11/15 17:16:21 gordon Exp $

    Author: Paul_Kunz@slac.stanford.edu */

// Use "#pragma once" instead of "ifndef MSCONFIG_H, define MSCONFIG_H,
// #endif" if you know you are using a Microsoft compiler, which we are
// if this file is being included. Using "pragma once" is much faster
// and less error prone.
#pragma once

// _MSC_VER == 1300 is the first release of Microsoft Visual C++ .NET.
// There are much fewer problems with this version.  So much of this
// file will be skipped

/** Define if STL functions don't work with IteratorBase.  This is
    a defect in the IteratorBase class rather than a defect in the
    compiler.*/
#define ITERATORBASE_DEFECT 1

/** Define if declaration of specialized template functions is not
allowed.  This problem was frist seen with Visual Studio.NET
compiler.  */
#if _MSC_VER >= 1300 
#define SPEC_TEMPLATE_DECL_DEFECT 1
#endif

/** Define if compiler can not resolve ambiguity in overloaded
    template functions.  This was first seen with Visual Studio
    6.0 and presists in 7.0 to a less degree.  Use this one for VC++ 7.0
	TEMPLATE_AMBIG_DEFECT for VC++ 6.0. */
#define TEMPLATE_AMBIG7_DEFECT 1

/** Define if compiler library has <strstream> instead of <sstream>. */
#undef SSTREAM_DEFECT

# pragma warning(disable:4244)  // conversion from double to float
# pragma warning(disable:4305)  // truncation from const double to const float
# pragma warning(disable:4800)  // forcing value to bool (performance warning)

#if _MSC_VER < 1300

/* Turn off annoying warning. */

# pragma warning(disable:4250)  // inherits via dominance
# pragma warning(disable:4786)  // '255' characters in the debug information


/** Define the value of pi which appears to be missing from Dev Studio. */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/** Define if specialized template implemeenation must appear within
    the corresponding header file and be inline.  This problem first
    appeared with VC++ 6.0. */
#define SPEC_TEMPLATE_IMP_DEFECT 1

/** Define if compiler can not resolve ambiguity in overloaded
    template functions.  This was first seen with Visual Studio
    6.0 and presists in 7.0 to a less degree. */
#define TEMPLATE_AMBIG_DEFECT 1

/** Define if function `terminate()' does not exist in <exception>.   This was first found in VC++ 6.0. */
#define TERMINATE_DEFECT

/** Define if bind2nd doesn't work calling member function that returns
   void. This works around the errror 
       error C2562: '()' : 'void' function returning a value
   which results when compiling bind2nd template function.  */
#define BIND2ND_DEFECT 1

/** Define if a function in derived class overrides a function in a
    base class only by the return type, and the return type in the
    derived class only differs because it is a derived type of the one
    defined in the base class.  This works around the error 
         error C2555:
    with MS VC++ 6.0 sp5 and earlier. */
#define CLONE_DEFECT 1

/** Define if vector <T>::iterator doesn't work unless using namespace
    std; statement is made. */
#define ITERATOR_MEMBER_DEFECT 1

/** Define if STL functions doesn't work with IteratorBase.  This is
    probably a defect in IteratorBase class than a defect in the
    compiler.*/
#define ITERATORBASE_DEFECT 1

/** Define if mem_fun doesn't always work when used to call member
    with one argument and the obsolete mem_fun1 must be used.. */
#define MEMFUN1_DEFECT 1

/** Define if STL transform function doesn't work with unary 
    functions in cmath */
#define TRANSFORM_DEFECT 1

/** Define if compiler prefers someting like std::cos ( const valarray<T> & ) 
    instead of std::cos ( double ). */
#define VALARRAY_DEFECT 1 

namespace std {

/** definition of standard C++ library max() function for compilers
    that don't supply it.  Note should do only less than comparison. */
template < class T >
inline const T& max ( const T & a, const T & b ) 
{
  // Break this into two lines to avoid an incorrect warning with
  // Cfront-based compilers.
  const T & retval = a < b ? b : a;

  return retval;
}

/** definition of standard C++ library min() function for compilers
    that don't supply it. Note should do only less than comparison. */
template < class T >
inline const T& min ( const T & a, const T & b) 
{
  // Break this into two lines to avoid an incorrect warning with
  // Cfront-based compilers.
  const T & retval = b < a ? b : a;

  return retval;
}

/** definition of standard C++ library abs() function for compilers
    that don't supply it. */
template < class T >
inline const T & abs ( const T & a )
{
    const T & retval = a < 0 ? -a : a;

    return retval;
}

} //end namespace std::

#endif
