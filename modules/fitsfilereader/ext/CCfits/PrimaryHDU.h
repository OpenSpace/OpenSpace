//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef PRIMARYHDU_H
#define PRIMARYHDU_H 1

// valarray
#include <valarray>
// PHDU
#include "PHDU.h"
// HDUCreator
#include "HDUCreator.h"
// Image
#include "Image.h"
// FITS
#include "FITS.h"
#include "CCfits.h"
#include <functional>
#include <numeric>
#include <memory>


namespace CCfits {



  template <typename T>
  class PrimaryHDU : public PHDU  //## Inherits: <unnamed>%394E6F870338
  {

    public:
        virtual PrimaryHDU<T> * clone (FITSBase* p) const;
        //	Read data reads the image if readFlag is true and
        //	optional keywords if supplied. Thus, with no arguments,
        //	readData() does nothing.
        virtual void readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>());
        const std::valarray<T>& image () const;
        const std::valarray<T>& readImage (long first, long nElements, T* nullValue);
        const std::valarray<T>& readImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride,T* nullValue);
        void writeImage (long first, long nElements, const std::valarray<T>& inData, T* nullValue = 0);
        void writeImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, const std::valarray<T>& inData);
        virtual void zero (double value);
        virtual void scale (double value);
        virtual void suppressScaling(bool toggle = true);
        virtual void resetImageRead ();

      // Additional Public Declarations

    protected:
        //	Constructor for new FITS objects, takes as arguments
        //	the required keywords for a primary HDU.
        PrimaryHDU (FITSBase* p, const int bitpix, const int naxis, const std::vector<long>& naxes, const std::valarray<T>& data = std::valarray<T>());
        //	Custom constructor. Allows specification of data to be read and whether to read data at
        //	construction or wait until the image data are requested. The default is 'lazy initialization:'
        //	wait until asked.
        PrimaryHDU (FITSBase* p, bool readFlag = false, const std::vector<String>& keys = std::vector<String>());

      // Additional Protected Declarations

    private:
        PrimaryHDU(const PrimaryHDU< T > &right);
        PrimaryHDU< T > & operator=(const PrimaryHDU< T > &right);

        virtual std::ostream & put (std::ostream &s) const;
        const Image<T>& data () const;

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Associations
        Image<T> m_data;

      // Additional Implementation Declarations
      friend class HDUCreator;
      friend class PHDU;
  };

  // Parameterized Class CCfits::PrimaryHDU 

  template <typename T>
  inline std::ostream & PrimaryHDU<T>::put (std::ostream &s) const
  {
  s << "PrimaryHDU:: Simple? " << simple() << " Extend?: " << extend() << 
    " Bitpix: " << bitpix() << " naxis = " << axes() << "\n";
  s << "Axis Lengths: \n";



  for (int i=0; i < axes(); i++)
     s  << " axis[" << i << "] " << axis(i) << "\n";

 s << "\nNumber of keywords read: " << keyWord().size() <<  "\n";

  for (std::map<String,Keyword*>::const_iterator ki = keyWord().begin();
        ki != keyWord().end(); ki++)
  {
        s << *((*ki).second) << std::endl;              
  }  


  s << " HISTORY: " << history() << '\n';
  s << " COMMENTS: " <<comment() << '\n';
  return s;  
  }

  template <typename T>
  inline const Image<T>& PrimaryHDU<T>::data () const
  {
    return m_data;
  }

  // Parameterized Class CCfits::PrimaryHDU 

  template <typename T>
  PrimaryHDU<T>::PrimaryHDU(const PrimaryHDU<T> &right)
      : PHDU(right), m_data(right.m_data)
  {
  }

  template <typename T>
  PrimaryHDU<T>::PrimaryHDU (FITSBase* p, const int bitpix, const int naxis, const std::vector<long>& naxes, const std::valarray<T>& data)
        : PHDU(p,bitpix,naxis,naxes),m_data(data)
  {
  }

  template <typename T>
  PrimaryHDU<T>::PrimaryHDU (FITSBase* p, bool readFlag, const std::vector<String>& keys)
        : PHDU(p), m_data()
  {
  initRead();

  if (readFlag || keys.size()) readData(readFlag,keys);  

  }


  template <typename T>
  PrimaryHDU<T> * PrimaryHDU<T>::clone (FITSBase* p) const
  {
  PrimaryHDU<T>* cloned = new PrimaryHDU<T>(*this);
  cloned->parent() = p;
  return cloned;
  }

  template <typename T>
  void PrimaryHDU<T>::readData (bool readFlag, const std::vector<String>& keys)
  {

  // Default reading mode. Read everything if readFlag is true.
  makeThisCurrent();

  if ( keys.size() > 0) 
  {
        std::list<String> keyList(keys.size());
        // keys is converted to a list so that any keys not in the header
        // can be easily erased. internally an exception will be thrown,
        // on a missing key, and its catch clause will print a message.
        std::copy(keys.begin(),keys.end(),keyList.begin());
        readKeywords(keyList);
  }
  // read the entire image, setting null values to the
  // return value from FitsNullValue<T>. It would be easy to make the null value
  // a user defined input, but that's not implemented yet.
  if ( readFlag && (naxis() > 0) )  
  {
        FITSUtil::FitsNullValue<T> null;
        long init(1);
        T nulValue(null());
        long nelements(std::accumulate(naxes().begin(),naxes().end(),init,std::multiplies<long>() ));
        readImage(1,nelements,&nulValue);

    }
  }

  template <typename T>
  const std::valarray<T>& PrimaryHDU<T>::image () const
  {

    return m_data.image();
  }


  template <typename T>
  const std::valarray<T>& PrimaryHDU<T>::readImage (long first, long nElements, T* nullValue)
  {
    makeThisCurrent();
    return m_data.readImage(fitsPointer(),first,nElements,nullValue,naxes(),anynul());
  }

  template <typename T>
  const std::valarray<T>& PrimaryHDU<T>::readImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride,T* nullValue)
  {
    makeThisCurrent();
    return m_data.readImage(fitsPointer(),firstVertex,lastVertex,stride,nullValue,naxes(),anynul());
  }

  template <typename T>
  void PrimaryHDU<T>::writeImage (long first, long nElements, const std::valarray<T>& inData, T* nullValue)
  {
    long newNaxisN=0;
    m_data.writeImage(fitsPointer(),first,nElements,inData,naxes(),newNaxisN,nullValue);
    if (newNaxisN)
       naxes(naxes().size()-1,newNaxisN);
  }

  template <typename T>
  void PrimaryHDU<T>::writeImage (const std::vector<long>& firstVertex, const std::vector<long>& lastVertex, const std::vector<long>& stride, const std::valarray<T>& inData)
  {
    long newNaxisN=0;
    m_data.writeImage(fitsPointer(),firstVertex,lastVertex,stride,inData,naxes(),newNaxisN);
    if (newNaxisN)
       naxes(naxes().size()-1,newNaxisN);
 }
 
  template <typename T>
  void PrimaryHDU<T>::scale (double value)
  {
     PHDU::scale(value);
     m_data.scalingHasChanged();
  }

  template <typename T>
  void PrimaryHDU<T>::zero (double value)
  {
     PHDU::zero(value);
     m_data.scalingHasChanged();
  }

  template <typename T>
  void PrimaryHDU<T>::suppressScaling (bool toggle)
  {
     HDU::suppressScaling(toggle);
     m_data.scalingHasChanged();
  }
   
  template <typename T>
  void PrimaryHDU<T>::resetImageRead()
  {
     m_data.resetRead();
  }
   
  
  // Additional Declarations

} // namespace CCfits


#endif
