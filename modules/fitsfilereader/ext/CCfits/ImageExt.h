//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef IMAGEEXT_H
#define IMAGEEXT_H 1

// ExtHDU
#include "ExtHDU.h"
// HDUCreator
#include "HDUCreator.h"
// Image
#include "Image.h"
// FITSUtil
#include "FITSUtil.h"
#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#endif


namespace CCfits {

  /*! \class ImageExt<T>

       ImageExt<T> is a subclass of ExtHDU that contains image data
       of type T.   
  */

  /*! \fn virtual ImageExt<T>::~ImageExt();
       \brief destructor
  */

  /*! \fn  virtual void ImageExt<T>::readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>());
          \brief read Image extension HDU data

          Called by FITS ctor, not intended for general use.
          parameters control how much gets read on initialization. 

          \param readFlag read the image data if true
          \param key      a vector of strings of keyword names to be read from the HDU                        


  */




  template <typename T>
  class ImageExt : public ExtHDU  //## Inherits: <unnamed>%3804A11121D8
  {

    public:
        virtual ~ImageExt();

        virtual ImageExt<T> * clone (FITSBase* p) const;
        virtual void readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>());
        virtual void zero (double value);
        virtual void scale (double value);
        virtual double zero () const;
        virtual double scale () const;
        virtual void suppressScaling(bool toggle = true);
        virtual void resetImageRead ();

      // Additional Public Declarations

    protected:
        ImageExt (FITSBase* p, const String &hduName, bool readDataFlag = false, const std::vector<String>& keys = std::vector<String>(), int version = 1);
        ImageExt (FITSBase* p, const String &hduName, int bpix, int naxis, const std::vector<long>& naxes, int version = 1);

      // Additional Protected Declarations
        virtual void checkExtensionType() const;
    private:
        ImageExt(const ImageExt< T > &right);
        ImageExt< T > & operator=(const ImageExt< T > &right);

        virtual void initRead ();
        virtual std::ostream & put (std::ostream &s) const;
        const std::valarray<T>& readImage (long first, long nElements, T* nullValue);
        const std::valarray<T>& readImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride,T* nullValue);
        void writeImage (long first, long nElements, const std::valarray<T>& inData, T* nullValue = 0);
        void writeImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::valarray<T>& inData);
        const Image<T>& data () const;

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Associations
        Image<T> m_data;

      // Additional Implementation Declarations
      friend class ExtHDU;
      friend class HDUCreator;
  };

  // Parameterized Class CCfits::ImageExt 

  template <typename T>
  inline std::ostream & ImageExt<T>::put (std::ostream &s) const
  {
  s << "Image Extension::  "  <<  " Name: " << name() << " Extension: " << xtension() 
          << " BITPIX "<< bitpix() << '\n';

  s <<  " Axis Lengths: \n";
  for (size_t j =1; j <= static_cast<size_t>( axes() ) ; j++)
  {
        s << " Axis: " << j << "  " << axis(j-1) << '\n';  
  }



  s << "Image Extension:: Version: " << version() << " HDU number: " <<  index() << '\n';

  s << " HISTORY: " << history() << '\n';
  s << " COMMENTS: " <<comment() << '\n';

  s << "BinTable:: nKeywords: " << keyWord().size() << '\n';

    return s;
  }

  template <typename T>
  inline const Image<T>& ImageExt<T>::data () const
  {
    return m_data;
  }

  // Parameterized Class CCfits::ImageExt 

  template <typename T>
  ImageExt<T>::ImageExt(const ImageExt<T> &right)
      : ExtHDU(right), m_data(right.m_data)
  {
  }

  template <typename T>
  ImageExt<T>::ImageExt (FITSBase* p, const String &hduName, bool readDataFlag, const std::vector<String>& keys, int version)
      : ExtHDU(p,ImageHdu,hduName,version),  m_data()
  {
  initRead();
  if (readDataFlag || keys.size() ) readData(readDataFlag,keys);  
  }

  template <typename T>
  ImageExt<T>::ImageExt (FITSBase* p, const String &hduName, int bpix, int naxis, const std::vector<long>& naxes, int version)
      : ExtHDU(p,ImageHdu,hduName,bpix,naxis,naxes,version), m_data()
  {
  // resize m_image according to naxes, and data according to m_image,
  // and equate them. Valarray = must be performed on items of the same
  // size according to the standard.
  int status (0);
  FITSUtil::CVarray<long> convert;
  FITSUtil::auto_array_ptr<long> axis(convert(naxes));
  static char EXTNAME[] = "EXTNAME";
  static char HDUVERS[] = "HDUVERS";

          if ( fits_create_img(fitsPointer(), bpix, naxis, axis.get(), &status) )
          {

                throw FitsError(status);
          } 
          else
          {
                char * comment = 0;
                if (fits_write_key(fitsPointer(),Tstring,EXTNAME,
                                const_cast<char*>(hduName.c_str()), comment,&status)) 
                {
                        throw FitsError(status);
                }                
                if (version != 0 && fits_write_key(fitsPointer(),Tint,HDUVERS,&version,
                                        comment,&status)) throw FitsError(status);     
          }      
  }


  template <typename T>
  ImageExt<T>::~ImageExt()
  {
  }


  template <typename T>
  void ImageExt<T>::initRead ()
  {
  }

  template <typename T>
  ImageExt<T> * ImageExt<T>::clone (FITSBase* p) const
  {
  ImageExt<T>* cloned = new ImageExt<T>(*this);
  cloned->parent() = p;
  return cloned;
  }

  template <typename T>
  void ImageExt<T>::readData (bool readFlag, const std::vector<String>& keys)
  {
  // Default reading mode. Read everything if readFlag is true.
  // this is identical to the equivalent method for PrimaryHDU<T>,
  // so will one day turn this into a simple call that shares the code.
  makeThisCurrent();

  if ( keys.size() > 0) 
  {
        std::list<string> keyList;
        // keys is converted to a list so that any keys not in the header
        // can be easily erased. internally an exception will be thrown,
        // on a missing key, and its catch clause will print a message.
        for (std::vector<string>::const_iterator j = keys.begin(); j != keys.end(); ++j)
        {
                keyList.push_back(*j);
        } 
        readKeywords(keyList);
  }

  if ( readFlag)  // read the entire image, setting null values to FLT_MIN.
  {

        FITSUtil::FitsNullValue<T> null;
        T nulval = null();
        long first(1);
        long nelements(1);
        for (size_t i = 0; i < naxes().size(); i++) nelements *= naxes(i);
        m_data.readImage(fitsPointer(),first,nelements,&nulval,naxes(),anynul());

    }
  }


  template <typename T>
  const std::valarray<T>& ImageExt<T>::readImage (long first, long nElements,T* nullValue)
  {
    checkExtensionType();
    return m_data.readImage(fitsPointer(),first,nElements,nullValue,naxes(),anynul());
  }

  template <typename T>
  const std::valarray<T>& ImageExt<T>::readImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, T* nullValue)
  {
    checkExtensionType();
    return m_data.readImage(fitsPointer(),firstVertex,lastVertex,stride,nullValue,naxes(),anynul());
  }

  template <typename T>
  void ImageExt<T>::writeImage (long first, long nElements, const std::valarray<T>& inData, T* nullValue)
  {
    checkExtensionType();
    long newNaxisN=0;
    m_data.writeImage(fitsPointer(),first,nElements,inData,naxes(),newNaxisN,nullValue);
    if (newNaxisN)
       naxes(naxes().size()-1,newNaxisN);
  }

  template <typename T>
  void ImageExt<T>::writeImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::valarray<T>& inData)
  {
    checkExtensionType();
    long newNaxisN=0;
    m_data.writeImage(fitsPointer(),firstVertex,lastVertex,inData,naxes(),newNaxisN);
    if (newNaxisN)
       naxes(naxes().size()-1,newNaxisN);
  }

  template <typename T>
  void ImageExt<T>::zero (double value)
  {
    makeThisCurrent();
    if (checkImgDataTypeChange(value, scale()))
    {
       if (naxis())
       {
           int status(0);
           if (fits_update_key(fitsPointer(), Tdouble, BZERO, &value, 0, &status))
              throw FitsError(status);
           fits_flush_file(fitsPointer(), &status);
           HDU::zero(value);
       }
    }
    else
    {
       bool silent=false;
       string msg("CCfits Error: Cannot set BZERO to a value which will change image data\n");
           msg += "              from integer type to floating point type.";
       throw FitsException(msg,silent);
    }
    m_data.scalingHasChanged();
  }

  template <typename T>
  void ImageExt<T>::scale (double value)
  {
    makeThisCurrent();
    if (checkImgDataTypeChange(zero(), value))
    {
       if (naxis())
       {
           int status(0);
           if (fits_update_key(fitsPointer(), Tdouble, BSCALE, &value, 0, &status))
              throw FitsError(status);
           fits_flush_file(fitsPointer(), &status);
           HDU::scale(value);
       } 
    }
    else
    {
       bool silent=false;
       string msg("CCfits Error: Cannot set BSCALE to a value which will change image data\n");
           msg += "              from integer type to floating point type.";
       throw FitsException(msg,silent);
    }
    m_data.scalingHasChanged();
  }

  template <typename T>
  double ImageExt<T>::zero () const
  {

    return HDU::zero();
  }

  template <typename T>
  double ImageExt<T>::scale () const
  {

    return HDU::scale();
  }

  template <typename T>
  void ImageExt<T>::suppressScaling (bool toggle)
  {
     HDU::suppressScaling(toggle);
     m_data.scalingHasChanged();
  }
  
  template <typename T>
  void ImageExt<T>::resetImageRead()
  {
     m_data.resetRead();
  }
   
  // Additional Declarations
    template <typename T>
    inline void ImageExt<T>::checkExtensionType() const
    {

    }
} // namespace CCfits


#endif
