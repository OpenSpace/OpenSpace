//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman
#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef SSTREAM_DEFECT
#include <strstream>
using std::ostrstream;
#else
#include <sstream>
#endif

#include <cstring>

// Column
#include "Column.h"
// FITS
#include "FITS.h"
// FITSBase
#include "FITSBase.h"
// ExtHDU
#include "ExtHDU.h"
// FitsError
#include "FitsError.h"
// FITSUtil
#include "FITSUtil.h"



namespace CCfits {

  // Class CCfits::ExtHDU::WrongExtensionType 

  ExtHDU::WrongExtensionType::WrongExtensionType (const String& msg, bool silent)
  : FitsException("Fits Error: wrong extension type: ",silent)
  {
     addToMessage(msg);
  std::cerr << msg << '\n'; 
  }


  // Class CCfits::ExtHDU 
  String ExtHDU::s_missHDU = "$HDU$";

  ExtHDU::ExtHDU(const ExtHDU &right)
      : HDU(right),
	m_pcount(right.m_pcount),
	m_gcount(right.m_gcount),
	m_version(right.m_version),
	m_xtension(right.m_xtension),
	m_name(right.m_name)
  {
  }

  ExtHDU::ExtHDU (FITSBase* p, HduType xtype, const String &hduName, int version)
      : HDU(p),
	m_pcount(0),
	m_gcount(1),
        m_version(version),
        m_xtension(xtype),
        m_name(hduName)
  {
  int number = -1;


  if ( hduName.substr(0,5) == s_missHDU ) 
  {
#ifdef SSTREAM_DEFECT
    std::istrstream fakeName( hduName.substr(5).c_str() );
#else
        std::istringstream fakeName(hduName.substr(5));
#endif
        fakeName  >> number;   
  }
  else
  {
        fits_get_hdu_num(fitsPointer(),&number);
        // Retrieve the HDU number. checkXtension tests against this.
        index(number-1);
  }




  // check we got the right kind of extension, since otherwise
  // we cannot proceed. CheckXtension throws an exception if
  // not, which is caught by the concrete class ctors.
  checkXtension();
  }

  ExtHDU::ExtHDU (FITSBase* p, HduType xtype, const String &hduName, int bitpix, int naxis, const std::vector<long>& axes, int version)
      : HDU(p, bitpix, naxis, axes),
   	m_pcount(0),
	m_gcount(1),
        m_version(version),
	m_xtension(xtype), 
        m_name(hduName)
  {
  // writing constructor. Extension must be supplied
  // since we must know what type of object to instantiate.
  }

  ExtHDU::ExtHDU (FITSBase* p, HduType xtype, int number)
      : HDU(p), 
        m_pcount(0),
        m_gcount(1),
        m_version(1),
        m_xtension(xtype),
        m_name("")
  {
  // set current HDU number. This is required for makeThisCurrent.

  index(number+1);
  makeThisCurrent();
  // set name and version.
  readHduName(fitsPointer(),number,m_name,m_version);
  // finally, check we got the right type of extension and throw an
  // exception otherwise.
  checkXtension();
  }


  ExtHDU::~ExtHDU()
  {
  }


  void ExtHDU::checkXtension ()
  {
  int   status=0;
  int hType = -1;

  if (fits_get_hdu_type(fitsPointer(),  &hType,  &status) ) throw FitsError(status);

  if (HduType(hType) != m_xtension) 
          throw HDU::InvalidExtensionType
            (" extension type mismatch between request and disk file ",true);
  }

  void ExtHDU::readHduName (const fitsfile* fptr, int hduIndex, String& hduName, int& hduVersion)
  {

   // get the name of the extension. If there is neither a HDUNAME
   // or an EXTNAME, make a name key for the multimap from the HDU number.

   int   status=0;
   FITSUtil::auto_array_ptr<char> pHduCstr(new char[FLEN_KEYWORD]);
   char* hduCstr = pHduCstr.get();
   int htype = -1;

   String key = "EXTNAME";
   char* extnm = const_cast<char*>(key.c_str());

   // C requires fptr to be non-const, because the fitsfile pointer 
   // saves the file state.
   fitsfile* cfptr = const_cast<fitsfile*>(fptr);

   if (fits_movabs_hdu(cfptr,hduIndex+1,&htype,&status) != 0) throw FitsError(status);


   status = fits_read_key_str(cfptr,  extnm, hduCstr, NULL, &status);
   if (status != 0)
   {
      strcpy(hduCstr,"");
      status = 0;
      key = String("HDUNAME");
      extnm = const_cast<char*>(key.c_str());
      status = fits_read_key_str(cfptr,  extnm, hduCstr, NULL, &status);
   }



   if (strlen(hduCstr) > 0) 
   {
        hduName = String(hduCstr);
        long hduV = 1;
        hduVersion = hduV;
        // get the version number.
        key = String("EXTVER");
        char* extv = const_cast<char*>(key.c_str());
        status = fits_read_key_lng(cfptr, extv, &hduV, NULL, &status);
        if (status == 0) hduVersion = hduV;
   }
   else
   {
#ifdef SSTREAM_DEFECT
       std::ostrstream fakeKey;       
#else
       std::ostringstream fakeKey;       
#endif
       fakeKey << s_missHDU << hduIndex;
#ifdef SSTREAM_DEFECT
       msg << std::ends;
#endif

       hduName = fakeKey.str();
   }
  }

  void ExtHDU::makeThisCurrent () const
  {
  HDU::makeThisCurrent();
  String tname("");
  int tvers = 0;
  ExtHDU::readHduName(fitsPointer(),index(),tname,tvers);
  parent()->currentExtensionName() = tname;       
  }

  Column& ExtHDU::column (const String& colName, bool caseSensitive) const
  {
  // if not overridden, throw an exception.
  // there might be a similar default implementation for function
  // that returns image data.
  throw WrongExtensionType(name());
  }

  void ExtHDU::setColumn (const String& colname, Column* value)
  {
  throw WrongExtensionType(name());
  }

  Column& ExtHDU::column (int colIndex) const
  {
  throw WrongExtensionType(name());
  }

  long ExtHDU::rows () const
  {
    String msg(" rows function can only be called for Tables - HDU: ");
    msg += name();
    throw WrongExtensionType(msg);
  }

  void ExtHDU::checkExtensionType () const
  {
    // throw if not overridden.
    throw WrongExtensionType(name());
  }

  void ExtHDU::addColumn (ValueType type, const String& columnName, long repeatWidth, const String& colUnit, long decimals, size_t columnNumber)
  {
    // overridden separately in AsciiTable and BinTable
    throw WrongExtensionType(name());

  }
  
  void ExtHDU::copyColumn(const Column& inColumn, int colIndx, bool insertNewCol)
  {
     throw WrongExtensionType(name());
  }

  void ExtHDU::deleteColumn (const String& columnName)
  {
    // overridden in Table.
    throw WrongExtensionType(name());
  }

  int ExtHDU::getVersion ()
  {
    // the version keyword is not stored as a keyword object but
    // as a data attribute so we don't "addKeyword" but fits_read_key instead.
    static char EXTVER[] = {"EXTVER"};
    int status(0);
    long vers(1);
    fits_read_key_lng(fitsPointer(),EXTVER,&vers,0,&status);
    if (status == 0)
    {
        m_version = vers;
    }
    else
    {
        if (status == KEY_NO_EXIST) 
        {
                m_version = 1;
        }
        else throw FitsError(status);

    }
    return m_version;
  }

  long ExtHDU::getRowsize () const
  {
     // This should be overridden for Table classes, otherwise throw.
     throw WrongExtensionType("getRowsize can only be called for Table files");

     return 0;
  }

  int ExtHDU::numCols () const
  {
    // overridden in Table.
    throw WrongExtensionType(name());

    return 0;
  }

  const ColMap& ExtHDU::column () const
  {
    // overridden in Table.
    throw WrongExtensionType(name());
  }

  bool ExtHDU::isCompressed() const
  {
     int status=0;
     checkExtensionType();
     return static_cast<bool>(fits_is_compressed_image(fitsPointer(), &status));
  }
  // Additional Declarations

} // namespace CCfits
