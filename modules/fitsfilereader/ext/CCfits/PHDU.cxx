//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// FITS
#include "FITS.h"
// FITSBase
#include "FITSBase.h"
// PHDU
#include "PHDU.h"



namespace CCfits {

  // Class CCfits::PHDU 

  PHDU::PHDU(const PHDU &right)
        : HDU(right), m_simple(true),
          m_extend(true)
  {
  }

  PHDU::PHDU (FITSBase* p, int bpix, int naxis, const std::vector<long>& axes)
      : HDU(p,bpix,naxis, axes), m_simple(true), m_extend(true)
  {
   int  status (0);


   // If file name explicitly asks for compression, don't create an
   // image here.  Primaries can't hold compressed images.  The FITS
   // image ctor will have to create an image extension instead.
   string::size_type compressSpecifier = 
        FITSUtil::checkForCompressString(p->name());
   if (compressSpecifier == string::npos)
   {
      long *naxesArray = 0;
      FITSUtil::CVarray<long> convert;
      naxesArray = convert(axes);
      if (fits_create_img(fitsPointer(), bpix, naxis, naxesArray, &status) != 0)
      {
         delete [] naxesArray;  
         throw FitsError(status);
      }
      if (fits_flush_file(fitsPointer(),&status)) 
      {
         delete [] naxesArray;  
         throw FitsError(status);
      }
      delete [] naxesArray;  
   }
   index(0);
  }

  PHDU::PHDU (FITSBase* p)
  //! Reading Primary HDU constructor.
  /*! Constructor used  when reading the primary HDU from an existing file.
  *  Does nothing except initialize, with the real work done by the subclass
  *  PrimaryHDU<T>.
  */       
        : HDU(p), m_simple(true),
        m_extend(true)
  {
  }


  PHDU::~PHDU()
  {
  //! Destructor
  }


  void PHDU::initRead ()
  {
           //! Read image header and update fits pointer accordingly. 
           /*! Private: called by ctor.  
           */

           long  pcount=0, gcount=0;
           int   status=0;
           int   simp = 0;
           int   xtend = 0;
           int   numAxes = 0;

           if (fits_get_img_dim(fitsPointer(), &numAxes, &status) != 0)  
           {
                   throw FitsError(status);
           }

           naxis() = numAxes;

           FITSUtil::auto_array_ptr<long> pAxes(0); 
           if (numAxes > 0) pAxes.reset(new long[numAxes]);
           long* axes = pAxes.get();
           int bpix = 0;

           if (fits_read_imghdr(fitsPointer(), MAXDIM, &simp, &bpix, &numAxes,
			             axes, &pcount, &gcount, &xtend, &status) != 0)  
               throw FitsError(status);

           bitpix(bpix);
           simple(simp != 0);
           extend(xtend != 0);

           if  (numAxes > 0)
           {
                naxes().resize(naxis()); 
                std::copy(&axes[0],&axes[numAxes],naxes().begin());
           }
  }

  void PHDU::zero (double value)
  {
    makeThisCurrent();
    if (checkImgDataTypeChange(value, scale()))
    {
       if ( naxis())
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
  }

  void PHDU::scale (double value)
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
  }

  double PHDU::zero () const
  {

    return HDU::zero();
  }

  double PHDU::scale () const
  {

    return HDU::scale();
  }

  // Additional Declarations

} // namespace CCfits
