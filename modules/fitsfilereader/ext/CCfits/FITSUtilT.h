//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef FITSUTILT_H
#define FITSUTILT_H

#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#endif


#include "FITSUtil.h"

#include<typeinfo>
#include<iostream>

#ifdef SSTREAM_DEFECT
#include <strstream>
#else
#include<sstream>
#endif

namespace CCfits
{

        namespace FITSUtil
        {

                // vector to vector conversion.

	        template <typename S, typename T> 
	        void 
	        fill(std::vector<S>& outArray, const std::vector<T>& inArray, size_t first, size_t last)
	        {
       		        // vector to vector assign. stdlib takes care of deletion.
                        int range = last - first + 1;
                        if (outArray.size() != static_cast<size_t>(range)) outArray.resize(range);
                        for (size_t j = first - 1; j < last; ++j)
                        {
                                outArray[j - first + 1] = static_cast<S>(inArray[j]);
                        }
	        }  

                // vector to valarray conversion. 

	        template <typename S, typename T> 
	        void fill(std::valarray<S>& outArray, const std::vector<T>& inArray, size_t first, size_t last)
	        {
     		        // vector to valarray assign
     		        int range = last - first + 1;
     		        if (outArray.size() != static_cast<size_t>(range)) outArray.resize(range); 
                        for (size_t j = first - 1; j < last; ++j)
                        {
                                outArray[j - first + 1] = static_cast<S>(inArray[j]);
                        }
	        }
                // valarray to valarray conversion.


	        template <typename S, typename T> 
	        void fill(std::valarray<S>& outArray, const std::valarray<T>& inArray)
	        {
                         size_t n = inArray.size();
       		         if (outArray.size() !=  n) outArray.resize(n);           
                         for (size_t j = 0;j < n; ++j) outArray[j] 
                                 = static_cast<S>(inArray[j]);
	        }

#ifdef TEMPLATE_AMBIG7_DEFECT
	        template <typename S, typename T> 
	        void fillMSva(std::vector<S>& outArray, const std::valarray<T>& inArray)
	        {
                         size_t n = inArray.size();
       		         if (outArray.size() !=  n) outArray.resize(n);           
                         for (size_t j = 0;j < n; ++j) outArray[j] 
                                 = static_cast<S>(inArray[j]);
	        }

#else
	        template <typename S, typename T> 
	        void fill(std::vector<S>& outArray, const std::valarray<T>& inArray)
	        {
                         size_t n = inArray.size();
       		         if (outArray.size() !=  n) outArray.resize(n);           
                         for (size_t j = 0;j < n; ++j) outArray[j] 
                                 = static_cast<S>(inArray[j]);
	        }
#endif

                // throw exceptions for string conversions to anything other than string.


	        template <typename T> 
	        void 
	        fill(std::vector<string>& outArray, const std::vector<T>& inArray, size_t first, size_t last)
	        {
                        first = 0;
                        last  = 0;
                        throw InvalidConversion(errorMessage(outArray,inArray),false);

	        } 

 	        template <typename T> 
	        void fill(std::vector<T>& outArray, const std::vector<string>& inArray, size_t first, size_t last)
	        {
                        first = 0;
                        last  = 0;
                        throw InvalidConversion(errorMessage(outArray,inArray),false);
	        }  




                template<typename S, typename T>
                string errorMessage( const S& out, const T& in)
                {
#ifdef SSTREAM_DEFECT
	                std::ostrstream errMsg;
#else
                        std::ostringstream errMsg;
#endif
                        errMsg << " Error: no conversion from " << typeid(in).name() << " to "
                               << typeid(out).name() << std::endl;
                        return errMsg.str();

                }

        }

} // namespace CCfits


#endif
