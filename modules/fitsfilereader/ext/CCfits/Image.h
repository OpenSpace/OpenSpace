//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef IMAGE_H
#define IMAGE_H 1

// functional
#include <functional>
// valarray
#include <valarray>
// vector
#include <vector>
// numeric
#include <numeric>
#include <sstream>

#ifdef _MSC_VER
#include "MSconfig.h" //form std::min
#endif
#include "CCfits.h"
#include "FitsError.h"
#include "FITSUtil.h"


namespace CCfits {



  template <typename T>
  class Image 
  {

    public:
        Image(const Image< T > &right);
        Image (const std::valarray<T>& imageArray = std::valarray<T>());
        ~Image();
        Image< T > & operator=(const Image< T > &right);

        const std::valarray<T>& readImage (fitsfile* fPtr, long first, long nElements, T* nullValue, const std::vector<long>& naxes, bool& nulls);
        const std::valarray<T>& readImage (fitsfile* fPtr, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, T* nullValue, const std::vector<long>& naxes, bool& nulls);
        // If write operation causes an expansion of the image's outer-most dimension, newNaxisN will be set to the new value.  Else it will be 0.
        void writeImage (fitsfile* fPtr, long first, long nElements, const std::valarray<T>& inData, const std::vector<long>& naxes, long& newNaxisN, T* nullValue = 0);
        void writeImage (fitsfile* fPtr, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, const std::valarray<T>& inData, const std::vector<long>& naxes, long& newNaxisN);
        void writeImage (fitsfile* fPtr, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::valarray<T>& inData, const std::vector<long>& naxes, long& newNaxisN);
        bool isRead () const;
        // This allows higher level classes to notify Image that a user-input
        //   scaling value has changed.  Image can then decide how this
        //   should affect reading from cache vs. disk.
        void scalingHasChanged();
        // Give the user (via higher level classes) a way to explicitly set the m_isRead flag
        //   to false, thus providing a fail-safe override of reading from the cache.
        void resetRead();
        const std::valarray< T >& image () const;

      // Additional Public Declarations

    protected:
      // Additional Protected Declarations

    private:
        std::valarray<T>& image ();
        void prepareForSubset (const std::vector<long>& naxes, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, const std::valarray<T>& inData, std::valarray<T>& subset);
        void loop (size_t iDim, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, size_t iPos, const std::vector<size_t>& incr, const std::valarray<T>& inData, size_t& iDat, const std::vector<size_t>& subIncr, std::valarray<T>& subset, size_t iSub);
        bool isNullValChanged(T* newNull) const;
        void setLastNullInfo(T* newNull);

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        // When m_isRead = true, assume m_fullImageCache contains the full image from the file.
        bool m_isRead;
        
      // Information regarding the usage of null values for the
      // most recent read operation.
        bool m_usingNullVal;
        T m_lastNullVal;

      // Data Members for Associations
        std::valarray< T > m_fullImageCache;
        std::valarray<T> m_currentRead;

      // Additional Implementation Declarations

  };

  // Parameterized Class CCfits::Image 

  template <typename T>
  inline bool Image<T>::isRead () const
  {
    return m_isRead;
  }

  template <typename T>
  inline const std::valarray< T >& Image<T>::image () const
  {
    return m_fullImageCache;
  }


  // Parameterized Class CCfits::Image 

  template <typename T>
  Image<T>::Image(const Image<T> &right)
        : m_isRead(right.m_isRead),
          m_usingNullVal(right.m_usingNullVal),
          m_lastNullVal(right.m_lastNullVal),
          m_fullImageCache(right.m_fullImageCache),
          m_currentRead(right.m_currentRead)
  {
  }

  template <typename T>
  Image<T>::Image (const std::valarray<T>& imageArray)
        : m_isRead(false),
          m_usingNullVal(false),
          m_lastNullVal(0),
          m_fullImageCache(imageArray),
          m_currentRead()
  {
  }


  template <typename T>
  Image<T>::~Image()
  {
  }


  template <typename T>
  Image<T> & Image<T>::operator=(const Image<T> &right)
  {
      // all stack allocated.
     m_isRead = right.m_isRead;
     m_usingNullVal = right.m_usingNullVal,
     m_lastNullVal = right.m_lastNullVal,
     m_fullImageCache.resize(right.m_fullImageCache.size());
     m_fullImageCache = right.m_fullImageCache;
     m_currentRead.resize(right.m_currentRead.size());
     m_currentRead = right.m_currentRead;
     return *this;
  }


  template <typename T>
  const std::valarray<T>& Image<T>::readImage (fitsfile* fPtr, long first, long nElements, T* nullValue, const std::vector<long>& naxes, bool& nulls)
  {
     if (!naxes.size())
     {
        m_currentRead.resize(0);
        return m_currentRead;
     }
     unsigned long init(1);
     unsigned long nTotalElements(std::accumulate(naxes.begin(),naxes.end(),init,
                     std::multiplies<long>()));

     if (first <= 0)
     {
        string errMsg("*** CCfits Error: For image read, lowest allowed value for first element is 1.\n");
        bool silent = false;
        throw FitsException(errMsg, silent);
     }
     // 0-based index for slice
     unsigned long start = (unsigned long)first - 1;
     if (start >= nTotalElements)
     {
        string errMsg("*** CCfits Error: For image read, starting element is out of range.\n");
        bool silent = false;
        throw FitsException(errMsg, silent);
     }
     if (nElements < 0)
     {
        string errMsg("*** CCfits Error: Negative nElements value specified for image read.\n");
        bool silent = false;
        throw FitsException(errMsg, silent);
     }
     const unsigned long elementsRequested = (unsigned long)nElements;

     int status(0);
     int any (0);
     FITSUtil::MatchType<T> imageType;

     // truncate to valid array size if too much data asked for.
     unsigned long elementsToRead(std::min(elementsRequested,
                     nTotalElements - start));
     if ( elementsToRead < elementsRequested)
     {
             std::cerr << 
                     "***CCfits Warning: data request exceeds image size, truncating\n"; 
     }
     const bool isFullRead = (elementsToRead == nTotalElements);
     const bool isDifferentNull = isNullValChanged(nullValue);
     if (!m_isRead || isDifferentNull)
     {
        // Must perform a read from disk.
        m_isRead = false;
        if (isFullRead)
        {
           m_fullImageCache.resize(elementsToRead);
           if (fits_read_img(fPtr,imageType(),first,elementsToRead,
               nullValue,&m_fullImageCache[0],&any,&status) != 0) throw FitsError(status);
           m_isRead = true;
           // For this case only, we'll pass m_fullImageCache back up (to be
           //   copied into user-supplied array).  This spares having to do
           //   what may be a very large copy into m_currentRead.
        }
        else
        {
           m_fullImageCache.resize(0);
           m_currentRead.resize(elementsToRead);
           if (fits_read_img(fPtr,imageType(),first,elementsToRead,
               nullValue,&m_currentRead[0],&any,&status) != 0) throw FitsError(status);
        }       
        nulls = (any != 0);
        setLastNullInfo(nullValue);
     }
     else
     {
         if (!isFullRead)
         {
            m_currentRead.resize((size_t)elementsToRead);
            // This may be a costly copy, though should still be faster
            //    than disk read.
            m_currentRead = m_fullImageCache[std::slice((size_t)start, (size_t)elementsToRead,1)];
         }
     }
     if (isFullRead)
        return m_fullImageCache;
     
     return m_currentRead;

  }

  template <typename T>
  const std::valarray<T>& Image<T>::readImage (fitsfile* fPtr, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, T* nullValue, const std::vector<long>& naxes, bool& nulls)
  {
     const size_t N = naxes.size();
     if (!N)
     {
        m_currentRead.resize(0);
        return m_currentRead;
     }
     if (N != firstVertex.size() || N != lastVertex.size() || N != stride.size())
     {
        string errMsg("*** CCfits Error: Image read function requires that naxes, firstVertex,");
        errMsg += "       \nlastVertex, and stride vectors all be the same size.\n";
        bool silent = false;
        throw FitsException(errMsg, silent);
     }

     FITSUtil::CVarray<long> carray;
     int any(0);
     int status(0);
     long requestedSize=1;
     long nTotalSize=1;

     for (size_t j = 0; j < N; ++j)
     {
        // Intentional truncation during division.
        requestedSize *= ((lastVertex[j] - firstVertex[j])/stride[j] + 1);
        nTotalSize *= naxes[j];
        if (firstVertex[j] < 1 || lastVertex[j] > naxes[j])
        {
           string errMsg("*** CCfits Error: Out-of-bounds vertex value.\n");
           bool silent=false;
           throw FitsException(errMsg,silent);
        }      
        if (firstVertex[j] > lastVertex[j])
        {
           string errMsg("*** CCfits Error: firstVertex values must not be larger than corresponding lastVertex values.\n");
           bool silent = false;
           throw FitsException(errMsg,silent);
        }      
     }
     const bool isFullRead = (requestedSize == nTotalSize);
     const bool isDifferentNull = isNullValChanged(nullValue);
     if (!m_isRead || isDifferentNull)
     {
        // Must perform a read from disk.
        FITSUtil::auto_array_ptr<long> pFpixel(carray(firstVertex));
        FITSUtil::auto_array_ptr<long> pLpixel(carray(lastVertex));
        FITSUtil::auto_array_ptr<long> pStride(carray(stride));

        FITSUtil::MatchType<T> imageType;
        m_isRead = false;
        if (isFullRead)
        {       
           m_fullImageCache.resize(requestedSize);
           if (fits_read_subset(fPtr,imageType(),
                                   pFpixel.get(),pLpixel.get(),
                                   pStride.get(),nullValue,&m_fullImageCache[0],&any,&status) != 0)
              throw FitsError(status);        
           m_isRead = true;
        }
        else
        {
           m_currentRead.resize(requestedSize);
           if (fits_read_subset(fPtr,imageType(),
                                   pFpixel.get(),pLpixel.get(),
                                   pStride.get(),nullValue,&m_currentRead[0],&any,&status) != 0)
              throw FitsError(status);        
        }
        nulls = (any != 0);
        setLastNullInfo(nullValue); 
     }
     else
     {
        if (!isFullRead)
        {
           // Must convert firstVertex,lastVertex,stride to gslice parameters.
           // Note that in cfitsio, the NAXIS1 dimension varies the fastest 
           // when laid out in an array in memory (ie. Fortran style).  Therefore NAXISn 
           // ordering must be reversed to C style before passing to gslice.
           size_t startPos=0;
           std::valarray<size_t> gsliceLength(size_t(0),N);
           std::valarray<size_t> gsliceStride(size_t(0),N);

           std::vector<long> naxesProducts(N);
           long accum=1;
           for (size_t i=0; i<N; ++i)
           {
              naxesProducts[i] = accum;
              accum *= naxes[i];
           }

           for (size_t i=0; i<N; ++i)
           {
              startPos += static_cast<size_t>((firstVertex[i]-1)*naxesProducts[i]);
              // Here's where we reverse the order:
              const size_t gsPos = N-1-i;
              // Division truncation is intentional.
              gsliceLength[gsPos] = static_cast<size_t>((1 + (lastVertex[i]-firstVertex[i])/stride[i]));
              gsliceStride[gsPos] = static_cast<size_t>(stride[i]*naxesProducts[i]);
           }
           m_currentRead.resize(requestedSize);
           m_currentRead = m_fullImageCache[std::gslice(startPos, gsliceLength, gsliceStride)];  
        }      
     }
     if (isFullRead)
        return m_fullImageCache;
     return m_currentRead;
  }

  template <typename T>
  void Image<T>::writeImage (fitsfile* fPtr, long first, long nElements, const std::valarray<T>& inData, const std::vector<long>& naxes, long& newNaxisN, T* nullValue)
  {
     int status(0);
     if (first < 1 || nElements < 1)
     {
        string errMsg("*** CCfits Error: first and nElements values must be > 0\n");
        bool silent = false;
        throw FitsException(errMsg, silent);
     }
     FITSUtil::CAarray<T> convert;
     FITSUtil::auto_array_ptr<T>    pArray(convert(inData));                     
     T* array = pArray.get();

     m_isRead = false;
     newNaxisN = 0;
     
     FITSUtil::MatchType<T> imageType;
     long type(imageType());

     if (fits_write_imgnull(fPtr,type,first,nElements,array,
                     nullValue,&status)!= 0)
     {
        throw FitsError(status);        
     }
     const size_t nDim=naxes.size();
     long origTotSize=1;
     for (size_t i=0; i<nDim; ++i)
        origTotSize *= naxes[i];
     const long highestOutputElem = first + nElements - 1;
     if (highestOutputElem > origTotSize)
     {
        // NAXIS(nDIM) may have increased.
        std::ostringstream oss;
        oss <<"NAXIS" << nDim;
        string keyname(oss.str());
        long newVal = 1 + (highestOutputElem-1)/(origTotSize/naxes[nDim-1]);
        if (newVal != naxes[nDim-1])
        {
           if (fits_update_key(fPtr,TLONG,(char *)keyname.c_str(),&newVal,0,&status) != 0)
           {
              throw FitsError(status);
           }
           newNaxisN = newVal;
        }
     }
     if (fits_flush_file(fPtr,&status) != 0)
        throw FitsError(status);

  }

  template <typename T>
  void Image<T>::writeImage (fitsfile* fPtr, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, const std::valarray<T>& inData, const std::vector<long>& naxes, long& newNaxisN)
  {
        // input vectors' size equality will be verified in prepareForSubset.
        const size_t nDim = naxes.size();
        FITSUtil::auto_array_ptr<long> pFPixel(new long[nDim]);
        FITSUtil::auto_array_ptr<long> pLPixel(new long[nDim]);
        std::valarray<T> subset;
        m_isRead = false;
        newNaxisN = 0;
        prepareForSubset(naxes,firstVertex,lastVertex,stride,inData,subset);

        long* fPixel = pFPixel.get();
        long* lPixel = pLPixel.get();
        for (size_t i=0; i<nDim; ++i)
        {
           fPixel[i] = firstVertex[i];
           lPixel[i] = lastVertex[i];
        }

        FITSUtil::CAarray<T> convert;
        FITSUtil::auto_array_ptr<T> pArray(convert(subset));
        T* array = pArray.get();
        FITSUtil::MatchType<T> imageType;        
        int status(0);

        if ( fits_write_subset(fPtr,imageType(),fPixel,lPixel,array,&status) )
            throw FitsError(status);
        
        if (lPixel[nDim-1] > naxes[nDim-1])
        {
           std::ostringstream oss;
           oss << "NAXIS" << nDim;
           string keyname(oss.str());
           long newVal = lPixel[nDim-1];
           if (fits_update_key(fPtr,TLONG,(char *)keyname.c_str(),&newVal,0,&status) != 0)
           {
              throw FitsError(status);
           }           
           newNaxisN = lPixel[nDim-1];
        }
        if (fits_flush_file(fPtr,&status) != 0)
           throw FitsError(status);
                        
  }

  template <typename T>
  std::valarray<T>& Image<T>::image ()
  {

    return m_fullImageCache;
  }

  template <typename T>
  void Image<T>::prepareForSubset (const std::vector<long>& naxes, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, const std::valarray<T>& inData, std::valarray<T>& subset)
  {

    // naxes, firstVertex, lastVertex, and stride must all be the same size.
    const size_t N = naxes.size();
    if (N != firstVertex.size() || N != lastVertex.size() || N != stride.size())
    {
       string errMsg("*** CCfits Error: Image write function requires that naxes, firstVertex,");
       errMsg += "       \nlastVertex, and stride vectors all be the same size.\n";
       bool silent = false;
       throw FitsException(errMsg, silent);
    }
    for (size_t i=0; i<N; ++i)
    {
       if (naxes[i] < 1)
       {
          bool silent = false;
          throw FitsException("*** CCfits Error: Invalid naxes value sent to image write function.\n", silent);
       }
       string rangeErrMsg("*** CCfits Error: Out-of-range value sent to image write function in arg: ");
       if (firstVertex[i] < 1 || (firstVertex[i] > naxes[i] && i != N-1)) 
       {
          bool silent = false;
          rangeErrMsg += "firstVertex\n";
          throw FitsException(rangeErrMsg, silent);
       }
       if (lastVertex[i] < firstVertex[i] || (lastVertex[i] > naxes[i] && i != N-1))
       {
          bool silent = false;
          rangeErrMsg += "lastVertex\n";
          throw FitsException(rangeErrMsg, silent);
       }
       if (stride[i] < 1)
       {
          bool silent = false;
          rangeErrMsg += "stride\n";
          throw FitsException(rangeErrMsg, silent);
       }
    }

    // nPoints refers to the subset of the image INCLUDING the zero'ed elements 
    // resulting from the stride parameter.  
    // subSizeWithStride refers to the same subset, not counting the zeros.
    size_t subSizeWithStride = 1;
    size_t nPoints = 1;
    std::vector<size_t> subIncr(N);
    for (size_t i=0; i<N; ++i)
    {
       subIncr[i] = nPoints;
       nPoints *= static_cast<size_t>(1+lastVertex[i]-firstVertex[i]);
       subSizeWithStride *= static_cast<size_t>(1+(lastVertex[i]-firstVertex[i])/stride[i]);
    }
    subset.resize(nPoints, 0);

    if (subSizeWithStride != inData.size())
    {
       bool silent = false;
       string errMsg("*** CCfits Error: Data array size is not consistent with the values");
       errMsg += "\n      in range and stride vectors sent to the image write function.\n";
       throw FitsException(errMsg, silent);
    }

    size_t startPoint = 0;
    size_t dimMult = 1;
    std::vector<size_t> incr(N);
    for (size_t j = 0; j < N; ++j)
    {
       startPoint += dimMult*(firstVertex[j]-1);
       incr[j] = dimMult;
       dimMult *= static_cast<size_t>(naxes[j]);
    }

    size_t inDataPos = 0;
    size_t iSub = 0;
    loop(N-1, firstVertex, lastVertex, stride, startPoint, incr, inData, inDataPos, subIncr, subset, iSub);           
  }

  template <typename T>
  void Image<T>::loop (size_t iDim, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, size_t iPos, const std::vector<size_t>& incr, const std::valarray<T>& inData, size_t& iDat, const std::vector<size_t>& subIncr, std::valarray<T>& subset, size_t iSub)
  {
     size_t start = static_cast<size_t>(firstVertex[iDim]);
     size_t stop = static_cast<size_t>(lastVertex[iDim]);
     size_t skip = static_cast<size_t>(stride[iDim]);
     if (iDim == 0)
     {
        size_t length = stop - start + 1;
        for (size_t i=0; i<length; i+=skip)
        {
           subset[i+iSub] = inData[iDat++];
        }
     }
     else
     {
        size_t jump = incr[iDim]*skip;
        size_t subJump = subIncr[iDim]*skip;
        for (size_t i=start; i<=stop; i+=skip)
        {
           loop(iDim-1, firstVertex, lastVertex, stride, iPos, incr, inData, iDat, subIncr, subset, iSub);
           iPos += jump;
           iSub += subJump;
        }
     }
  }
  
  template <typename T>
  bool Image<T>::isNullValChanged(T* newNull) const
  {
     bool isChanged = false;
     if (m_usingNullVal)
     {
        // If m_usingNullVal is true, we can assume m_lastNullVal != 0.
        if (newNull)
        {
           T newVal = *newNull;
           if (newVal != m_lastNullVal)
              isChanged = true;
        }
        else
           isChanged = true;
     }
     else
     {
        if (newNull && (*newNull != 0))
           isChanged = true;
     }
     
     return isChanged;
  }
  
  template <typename T>
  void Image<T>::setLastNullInfo(T* newNull)
  {
     if (!newNull || *newNull == 0)
     {
        m_usingNullVal = false;
        m_lastNullVal = 0;
     }
     else
     {
        m_usingNullVal = true;
        m_lastNullVal = *newNull;
     }
  }

  template <typename T>
  void Image<T>::writeImage (fitsfile* fPtr, const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::valarray<T>& inData, const std::vector<long>& naxes, long& newNaxisN)
  {
     std::vector<long> stride(firstVertex.size(), 1);
     writeImage(fPtr, firstVertex, lastVertex, stride, inData, naxes, newNaxisN);
  }
  
  template <typename T>
  void Image<T>::scalingHasChanged()
  {
     m_isRead = false;
  }
  
  template <typename T>
  void Image<T>::resetRead()
  {
     m_isRead = false;
  }

  // Additional Declarations

} // namespace CCfits


#endif
