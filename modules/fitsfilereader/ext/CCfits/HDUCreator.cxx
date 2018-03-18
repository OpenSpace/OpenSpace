//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman
#ifdef _MSC_VER
#include "MSconfig.h" //for truncation warning
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef SSTREAM_DEFECT
#include <strstream>
#else
#include <sstream>
#endif

#include <float.h>
#include <algorithm>

// HDU
#include "HDU.h"
// PHDU
#include "PHDU.h"
// ExtHDU
#include "ExtHDU.h"
// FitsError
#include "FitsError.h"
// FITSBase
#include "FITSBase.h"
// ImageExt
#include "ImageExt.h"
// HDUCreator
#include "HDUCreator.h"
// PrimaryHDU
#include "PrimaryHDU.h"
// AsciiTable
#include "AsciiTable.h"
// BinTable
#include "BinTable.h"
// FITS
#include "FITS.h"



namespace CCfits {
    char BSCALE[7] = {"BSCALE"};
    char BZERO[6]  = {"BZERO"};

  // Class CCfits::HDUCreator 

  HDUCreator::HDUCreator (FITSBase* p)
  : m_hdu(0), m_parent(p) 
  {
  }


  HDUCreator::~HDUCreator()
  {
  }


  PHDU * HDUCreator::createImage (int bitpix, long naxis, const std::vector<long>& naxes)
  {
    return MakeImage(bitpix,naxis,naxes);
  }

  PHDU * HDUCreator::MakeImage (int bpix, int naxis, const std::vector<long>& naxes)
  {
    m_hdu = m_parent->pHDU();
    if (!m_hdu) 
    {
       switch (bpix)
       {
         case BYTE_IMG:
              m_hdu = new PrimaryHDU<unsigned char>(m_parent,bpix,naxis,naxes);
              break; 
         case SHORT_IMG:
              m_hdu = new PrimaryHDU<short>(m_parent,bpix,naxis,naxes);
              break; 
         case LONG_IMG:
              m_hdu = new PrimaryHDU<INT32BIT>(m_parent,bpix,naxis,naxes);
              break; 
         case FLOAT_IMG:
              m_hdu = new PrimaryHDU<float>(m_parent,bpix,naxis,naxes);
              break; 
         case DOUBLE_IMG:
              m_hdu = new PrimaryHDU<double>(m_parent,bpix,naxis,naxes);
              break; 
         case USHORT_IMG:
              m_hdu =  new PrimaryHDU<unsigned short>(m_parent,bpix,naxis,naxes);
              m_hdu->bitpix(SHORT_IMG);
              m_hdu->zeroInit(USBASE);
              break; 
         case ULONG_IMG:
              m_hdu =  new PrimaryHDU<unsigned INT32BIT>(m_parent,bpix,naxis,naxes);
              m_hdu->bitpix(LONG_IMG);
              m_hdu->zeroInit(ULBASE);
              break;
	 case LONGLONG_IMG:
	      m_hdu = new PrimaryHDU<LONGLONG>(m_parent,bpix,naxis,naxes);
              break; 
         default:
             throw HDU::InvalidImageDataType("FitsError: invalid data type for FITS I/O");
       } 
    }  
    return static_cast<PHDU*>(m_hdu);
  }

  HDU * HDUCreator::Make (const String& hduName, bool readDataFlag, const std::vector<String> &keys, bool primary, int version)
  {
    int status = 0;
    bool isExtFound = true;
    int extNum = -1;
    // Are we dealing with a fake hduName ?
    bool isFake = hduName.find(ExtHDU::missHDU()) == 0 &&
                        hduName.length() > ExtHDU::missHDU().length();
    if (isFake)
    {
#ifdef SSTREAM_DEFECT
       std::istrstream extNumStr (hduName.substr(ExtHDU::missHDU().length().c_str()));
#else
       std::istringstream extNumStr (hduName.substr(ExtHDU::missHDU().length()));
#endif
       extNumStr >> extNum;
       if (fits_movabs_hdu(m_parent->fptr(), extNum+1, 0, &status)) 
       {
          isExtFound = false;
       }
    }
    else if ( !primary && fits_movnam_hdu(m_parent->fptr(),ANY_HDU, 
             const_cast<char*>(hduName.c_str()),version,&status) )
    {
       isExtFound = false;
    }

    if (!isExtFound)
    {
#ifdef SSTREAM_DEFECT
        std::ostrstream msg;
#else
        std::ostringstream msg;
#endif
        msg << "Cannot access HDU name ";
        if (isFake)
        {
           msg << "(No name)  " << "Index no. " << extNum;      
        } 
        else
        {
           msg << hduName ;
        }

        if (version) msg << " version " << version;
#ifdef SSTREAM_DEFECT
	msg << std::ends;
#endif
	throw FITS::NoSuchHDU(msg.str());
    }
    return commonMake(hduName, readDataFlag, keys, primary, version);
  } // end Make
  
  
  HDU* HDUCreator::commonMake(const String& hduName, bool readDataFlag, const std::vector<String> &keys, bool primary, int version)
  {
    int status = 0;
    long imgType = 0;
    int htype = -1;
    if ( fits_get_hdu_type(m_parent->fptr(),&htype,&status) )
							throw FitsError(status);

    HduType xtype = HduType(htype);

    m_hdu = m_parent->pHDU();
    switch(xtype)
    {
        case ImageHdu:
        {
            int tmpBpix=0;
            if (fits_get_img_type(m_parent->fptr(), &tmpBpix, &status))
               throw FitsError(status);
            imgType = static_cast<long>(tmpBpix);
            double unsignedZero(0);
            double scale(1);
            // getScaling may change imgType from signed int to unsigned,
            // which does not have its own bitpix value, or to a float
            // type, which does have a unique bitpix value.  To put this 
            // another way, we must take care to not set m_hdu->bitpix()
            // to an unsigned value.
            getScaling(imgType,unsignedZero,scale);
            // ImageHDU types are templated
            switch (imgType)
            {
               case Ibyte:
                  if (primary) 
                  {
                     if (!m_hdu) m_hdu = 
                         new PrimaryHDU<unsigned char>(m_parent,readDataFlag,keys);
                  }
                  else
                  {
                     m_hdu = new ImageExt<unsigned char>
                                 (m_parent,hduName,readDataFlag,keys,version);   
                  }
                  break;   
               case Ishort:
                  if (primary) 
                  {
                     if (!m_hdu) 
                     {           
                        m_hdu =  new PrimaryHDU<short>
                                        (m_parent,readDataFlag,keys);     
                     }
                  }
                  else
                  {
                     m_hdu =  new ImageExt<short>
                                    (m_parent,hduName,readDataFlag,keys,version);     
                  }
		  break;
               case Iushort:
                  if (primary)
                  {
                     if (!m_hdu)
                     {
                        m_hdu = new PrimaryHDU<unsigned short>
                                           (m_parent,readDataFlag,keys);
                     }
                  }
                  else
                  {
                     m_hdu =  new ImageExt<unsigned short>
                                    (m_parent,hduName,readDataFlag,keys,version);
                  }
                  imgType = Ishort;
                  break;
               case Ilong:
                  if (primary) 
                  {
                     if (!m_hdu) 
                     {           
                        m_hdu =  new PrimaryHDU<INT32BIT>
                                        (m_parent,readDataFlag,keys);     
                     }
                  }
                  else
                  {
                     m_hdu =  new ImageExt<INT32BIT>
                                    (m_parent,hduName,readDataFlag,keys,version);     
                  }
                  break;
               case Iulong:
                  if (primary)
                  {
                     if (!m_hdu)
                     {
                        m_hdu = new PrimaryHDU<unsigned INT32BIT>
                                     (m_parent,readDataFlag,keys);
                     }
                  }
                  else
                  {
                     m_hdu =  new ImageExt<unsigned INT32BIT>
                                    (m_parent,hduName,readDataFlag,keys,version);
                  }
                  imgType = Ilong;
                  break;
	       case Ilonglong:
                  if (primary)
		  {
		     if (!m_hdu)
		     {
			m_hdu = new PrimaryHDU<LONGLONG>(m_parent,readDataFlag,keys);
		     }
		  }
		  else
		  {
		     m_hdu = new ImageExt<LONGLONG>(m_parent,hduName,readDataFlag,keys,version);
		  }
                  break;
               case Ifloat:
                  if (primary) 
                  {
                     if (!m_hdu) m_hdu = 
                           new PrimaryHDU<float>(m_parent,readDataFlag,keys);
                  }
                  else
                  {
                     m_hdu = new ImageExt<float>
                                 (m_parent,hduName,readDataFlag,keys,version);   
                  }
                  break;
               case Idouble:
                  if (primary) 
                  {
                     if (!m_hdu) m_hdu = 
                           new PrimaryHDU<double>(m_parent,readDataFlag,keys);
                  }
                  else
                  {
                     m_hdu = new ImageExt<double>
                             (m_parent,hduName,readDataFlag,keys,version);   
                  }
                  break;
// dummy code to avoid SEGV in Solaris. This is supposed to trick the 
// compiler into instantiating PrimaryHDU<int, unsigned int>, ImageExt<int, unsigned int>
// so that it doesn't throw a SEGV if the user tries to read integer data into an
// integer array. But Tint is not actually an acceptable value for bitpix.
               case Tint:
                  if (primary) 
                  {
                     if (!m_hdu) 
                     {           
                        if (unsignedZero == ULBASE && scale == 1)
                        { 
                           m_hdu =  new PrimaryHDU<unsigned int>
                                           (m_parent,readDataFlag,keys);
                        }
                        else
                        {
                           m_hdu =  new PrimaryHDU<int>
                                           (m_parent,readDataFlag,keys);     
                        }
                     }
                  }
                  else
                  {
                     if (unsignedZero == ULBASE && scale == 1)
                     { 
                        m_hdu =  new ImageExt<unsigned int>
                                   (m_parent,hduName,readDataFlag,keys,version);
                     }
                     else
                     {
                        m_hdu =  new ImageExt<int>
                                   (m_parent,hduName,readDataFlag,keys,version);     
                     }
                  }
               default:
                  throw HDU::InvalidImageDataType(" invalid data type for FITS Image I/O");
            }  
            m_hdu->bitpix(imgType);
            if (unsignedZero != 0.0)
            {
               m_hdu->zeroInit(unsignedZero);
            }
            if (scale != 1.0)
            {
               m_hdu->scaleInit(scale);
            }
        }
	    break;
    case AsciiTbl:
	m_hdu = new AsciiTable(m_parent, hduName, readDataFlag, keys, version);
        m_hdu->bitpix(8);
	break;

    case BinaryTbl:
	m_hdu = new BinTable(m_parent, hduName, readDataFlag, keys, version);
        m_hdu->bitpix(8);
	break;

    default:
        throw HDU::InvalidImageDataType("FitsError: invalid data type for FITS I/O");
    }
     return m_hdu;
  } // end commonMake
  

  HDU* HDUCreator::MakeTable (const String &name, HduType xtype, int rows, const std::vector<String>& colName, const std::vector<String>& colFmt, const std::vector<String>& colUnit, int version)
  {
    switch (xtype)
    {
       case AsciiTbl:
          m_hdu = new AsciiTable(m_parent,name,rows,colName,colFmt,colUnit,version); 
          break;
       case BinaryTbl:
          m_hdu = new BinTable(m_parent,name,rows,colName,colFmt,colUnit,version);                 
          break;
       default:
          throw HDU::InvalidExtensionType("unexpected");
    }
    return m_hdu;
  }

  HDU * HDUCreator::Make (int index, bool readDataFlag, const std::vector<String> &keys)
  {
    bool primary = (index == 0);
    String hduName;
    int version = 0;
    if (!primary) 
       ExtHDU::readHduName(m_parent->fptr(),index,hduName,version );
    return commonMake(hduName,readDataFlag,keys,primary,version);
  }

  ExtHDU * HDUCreator::createImage (const String &name, int bitpix, long naxis, const std::vector<long>& naxes, int version)
  {
        return MakeImage(name,bitpix,naxis,naxes,version);
  }

  ExtHDU * HDUCreator::MakeImage (const String &name, int bpix, long naxis, const std::vector<long>& naxes, int version)
  {
  ExtHDU* newImage = 0;
  switch (bpix)
  {
     case BYTE_IMG:
          newImage =  new ImageExt<unsigned char>(m_parent,name,bpix,naxis,naxes,version);   
          break; 
     case SHORT_IMG:
          newImage =  new ImageExt<short>(m_parent,name,bpix,naxis,naxes,version);   
          break; 
     case LONG_IMG:
          newImage =  new ImageExt<INT32BIT>(m_parent,name,bpix,naxis,naxes,version);   
          break; 
     case FLOAT_IMG:
          newImage =  new ImageExt<float>(m_parent,name,bpix,naxis,naxes,version);   
          break; 
     case DOUBLE_IMG:
          newImage =  new ImageExt<double>(m_parent,name,bpix,naxis,naxes,version);   
          break; 
     case USHORT_IMG:
          newImage =  
                  new ImageExt<unsigned short>(m_parent,name,bpix,naxis,naxes,version);
          newImage->bitpix(SHORT_IMG);
          newImage->zeroInit(USBASE);
          break; 
     case ULONG_IMG:
          newImage =  
                  new ImageExt<unsigned INT32BIT>(m_parent,name,bpix,naxis,naxes,version);  
          newImage->bitpix(LONG_IMG);
          newImage->zeroInit(ULBASE); 
          break; 
     case LONGLONG_IMG:
          newImage =  new ImageExt<LONGLONG>(m_parent,name,bpix,naxis,naxes,version);   
	  break;
     default:
        throw HDU::InvalidImageDataType("FitsError: invalid data type for FITS I/O");
  } 
  return newImage;
  }

  void HDUCreator::getScaling (long& type, double& zero, double& scale) const
  {
    float tmpScale(1.);
    float zval (.0);
    int status (0);
    fits_read_key_flt(m_parent->fptr(),BZERO,&zval,0,&status);
    if (status)
        zval = .0;
    status = 0;
    fits_read_key_flt(m_parent->fptr(),BSCALE,&tmpScale,0,&status);
    if (status)
       tmpScale = 1.0;
    zero = zval;
    scale = tmpScale;
    // if there is no effective scaling ... 
    if (zero == 0.0 && scale == 1.0)
    {
       return;
    }
    else 
    {
       int newType = 0;
       fits_get_img_equivtype(m_parent->fptr(),&newType,&status);
       if (!status)
          type = newType;          
    }                
  }

  // Additional Declarations

} // namespace CCfits
