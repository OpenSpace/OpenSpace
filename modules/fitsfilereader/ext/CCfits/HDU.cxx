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
#else
#include <sstream>
#endif

#include <memory>
#include <cstring>
#include <iterator>

// FITS
#include "FITS.h"
// FITSBase
#include "FITSBase.h"
// HDU
#include "HDU.h"

#include "FITSUtil.h"
#include <functional>


namespace CCfits {
    // TYP_* are defined in fitsio.h
    const size_t HDU::s_nCategories = 5;
    const int HDU::s_iKeywordCategories[5] =
          {TYP_USER_KEY,TYP_WCS_KEY,TYP_CMPRS_KEY,TYP_CKSUM_KEY,TYP_REFSYS_KEY};

  // Class CCfits::HDU::InvalidImageDataType 

  HDU::InvalidImageDataType::InvalidImageDataType (const string& diag, bool silent)
  : FitsException("Fits Error: Invalid Data Type for Image ",silent)
  {
     addToMessage(diag);
  if (!silent) std::cerr << diag << '\n';
  }


  // Class CCfits::HDU::InvalidExtensionType 

  HDU::InvalidExtensionType::InvalidExtensionType (const string& diag, bool silent)
  : FitsException
     ("Fits Error: Extension Type: ",
     silent)
  {
        addToMessage(diag);
        if (!silent) std::cerr << diag << '\n';
  }


  // Class CCfits::HDU::NoSuchKeyword 

  HDU::NoSuchKeyword::NoSuchKeyword (const string& diag, bool silent)
  : FitsException("Fits Error: Keyword not found: ",silent)
  {
     addToMessage(diag);
  if (!silent || FITS::verboseMode()) std::cerr << diag << '\n';
  }


  // Class CCfits::HDU::NoNullValue 

  HDU::NoNullValue::NoNullValue (const string& diag, bool silent)
    : FitsException("Fits Error: No Null Pixel Value specified for Image ",silent)
  {
    addToMessage(diag);
    if (!silent || FITS::verboseMode() ) std::cerr << diag << '\n';    
  }


  // Class CCfits::HDU 

  HDU::HDU(const HDU &right)
      : m_naxis(right.m_naxis),
        m_bitpix(right.m_bitpix),
        m_index(right.m_index),
        m_anynul(right.m_anynul),
        m_history(right.m_history),
        m_comment(right.m_comment),
        m_zero(right.m_zero),
        m_scale(right.m_scale),        
        m_keyWord(),
        m_parent(right.m_parent),
        m_naxes(right.m_naxes)
  {
   // ensure deep copy.
   copyKeys(right);
  }

  HDU::HDU (FITSBase* p)
      : m_naxis(0),
        m_bitpix(8),
        m_index(0),
        m_anynul(false),
        m_history(""),
        m_comment(""),
        m_zero(0),
        m_scale(1.0),        
        m_keyWord(),
        m_parent(p),
        m_naxes()
  {
   readHduInfo();  // read bitpix, naxis and naxes keywords 
   int hduIndex = 0;
   fits_get_hdu_num(fitsPointer(), &hduIndex);
   m_index = hduIndex - 1;
  }

  HDU::HDU (FITSBase* p, int bitpix, int naxis, const std::vector<long>& axes)
      : m_naxis(naxis),
        m_bitpix(bitpix),
        m_index(0),
        m_anynul(false),
        m_history(""),
        m_comment(""),
        m_zero(0),
        m_scale(1.0),        
        m_keyWord(),
        m_parent(p),
        m_naxes(axes)
  {
  }


  HDU::~HDU()
  {
  // garbage collection for heap allocated objects.
  clearKeys();
  }


  bool HDU::operator==(const HDU &right) const
  {
   return compare(right);
  }

  bool HDU::operator!=(const HDU &right) const
  {
  return !compare(right);
  }


  void HDU::clearKeys ()
  {
  for (std::map<String,Keyword*>::iterator key = m_keyWord.begin(); 
          key != m_keyWord.end(); ++key)
  {
        delete (*key).second;
  }

  m_keyWord.erase(m_keyWord.begin(),m_keyWord.end());
  }

  void HDU::readHduInfo ()
  {

  // Read BITPIX and NAXIS keywords 
  int status = 0;
  char* comment = 0;

  int htype = -1;
  if (fits_get_hdu_type(fitsPointer(),&htype,&status))
     throw FitsError(status);
  HduType xtype = HduType(htype);
  if (xtype == ImageHdu)
  {
     // Do not try and read BITPIX or NAXIS directly.  If this is a
     // compressed image, the relevant information will need to come
     // from ZBITPIX and ZNAXIS instead.
     int temp = 0;
     if (fits_get_img_type(fitsPointer(), &temp, &status))
        throw FitsError(status);
     m_bitpix = temp;
     if (fits_get_img_dim(fitsPointer(), &temp, &status))
        throw FitsError(status);
     m_naxis = temp;
  }
  else
  {
     // m_bitpix will retain its default value, 8.
     string keyname("NAXIS");
     if (fits_read_key_lng(fitsPointer(),const_cast<char*>(keyname.c_str()),
                &m_naxis, comment, &status))
        throw FitsError(status);
  }

  // okay, we have the number of axes and therefore the number of values
  // (z)NAXISn.

  FITSUtil::auto_array_ptr<long> axes(new long[m_naxis]);
  long* pAxes = axes.get();

  if (xtype == ImageHdu)
  {
     if (fits_get_img_size(fitsPointer(), m_naxis, pAxes, &status))
        throw FitsError(status);
  }
  else
  {
     // create strings NAXIS1 - NAXISn, where n = NAXIS, and read keyword.
     for (int i=0; i <m_naxis; i++)
     {  
        string keyname("NAXIS");
        #ifdef SSTREAM_DEFECT
           std::ostrstream axisKey;
	   axisKey << keyname << i+1 << std::ends;
           if (fits_read_key_lng(fitsPointer(),axisKey.str(),&pAxes[i],comment,&status) ) 
                   throw FitsError(status);
        #else
	   std::ostringstream axisKey;
	   axisKey << keyname << i+1;
           if (fits_read_key_lng(fitsPointer(),const_cast<char*>(axisKey.str().c_str()),
                                                                   &pAxes[i],comment,&status) ) 
                   throw FitsError(status);
        #endif
     }

  }
  std::copy(&pAxes[0],&pAxes[m_naxis], std::back_inserter(m_naxes));
  }

  Keyword& HDU::readKeyword (const String &keyname)
  {

  try       
  {
  	using std::pair;  
        Keyword* newKey(KeywordCreator::getKeyword(keyname,this));
        std::map<String,Keyword*>::value_type refToEntry(keyname,newKey);
        std::map<String,Keyword*>::iterator itOld = m_keyWord.find(keyname);
        if (itOld != m_keyWord.end())
        {
           delete itOld->second;
           m_keyWord.erase(itOld);
        }
        m_keyWord.insert(refToEntry);
  	return *(refToEntry.second);

  }
  catch (FitsError)
  {     
        throw NoSuchKeyword(keyname);
  }  
  }

  void HDU::readKeywords (std::list<String>& keynames)
  {
  // the scoop is: this and the previous function - which reads
  // a single key - are designed to be called by HDU and its subclasses
  // in constructors etc., and by the public member functions
  // addKey<T>, readKey<T> and readKeys<T>. These functions read keywords
  // and either return nothing or return Keyword references from which another
  // function call is necessary to retrieve the value. These are both things
  // that implementers need but are not necessary to provide for the user.
  std::list<String>::iterator ikey = keynames.begin();
  while ( ikey != keynames.end())
  {
     try
     {
        readKeyword(*ikey);
	++ikey;
     }
     catch (NoSuchKeyword)
     {
        ikey = keynames.erase(ikey);
     }
  }


  }

  Keyword* HDU::addKeyword (Keyword* newKey)
  {
  	using std::pair;
  	newKey->write();
        const String& keyname = newKey->name(); 
        std::map<String,Keyword*>::value_type refToEntry(keyname,newKey);
        std::map<String,Keyword*>::iterator itOld = m_keyWord.find(keyname);
        if (itOld != m_keyWord.end())
        {
           delete itOld->second;
           m_keyWord.erase(itOld);
        }
        m_keyWord.insert(refToEntry);
  	return refToEntry.second;
  }

  bool HDU::compare (const HDU &right) const
  {
    if (fitsPointer() != right.fitsPointer()) return false;
    if (m_index != right.m_index) return false;
    return true;
  }

  fitsfile* HDU::fitsPointer () const
  {

    return m_parent->fptr();
  }

  FITSBase* HDU::parent () const
  {

    return m_parent;
  }

  void HDU::makeThisCurrent () const
  {
  int status = 0;
  int hduType = 0;
  // m_index contains the value of fptr->Fptr->curhdu. movabs_hdu moves to
  // 1 - this unless it's at the beginning, so it must be m_index+1 [Check!!!]
  if (fits_movabs_hdu(fitsPointer(),m_index+1, &hduType, &status) != 0) throw FitsError(status);
  m_parent->currentExtensionName()  = "";
  }

  void HDU::copyKeys (const HDU& right)
  {
    // copy all keys that have already been read.
  for (std::map<String,Keyword*>::const_iterator key = right.m_keyWord.begin(); 
          key != right.m_keyWord.end(); key++)
  {
        std::map<String,Keyword*>::iterator itOld = m_keyWord.find(key->first);
        if (itOld != m_keyWord.end())
        {
           delete itOld->second;
           m_keyWord.erase(itOld);
        }
        m_keyWord[(*key).first] = ((*key).second)->clone();
  }
  }

  String HDU::getNamedLines (const String& name)
  {
  int fitsStatus = 0;
  int numKeys = 0;
  static char searchKey[FLEN_KEYWORD];

  makeThisCurrent();


  String content("");
  char* card = new char[81];
  FITSUtil::auto_array_ptr<char> pcard(card);
  int where = 1;
  int keylen = 0;

  fits_get_hdrpos(fitsPointer(),&numKeys,&where,&fitsStatus);
  // Reset 'where' to top prior to starting search.
  where = 0;
  if (fitsStatus != 0 ) throw FitsError(fitsStatus,false);

  // search through the header to get the first "name" keyword.
  while ( where < numKeys )
  {
        fits_read_record(fitsPointer(),++where,card,&fitsStatus);
	fits_get_keyname(card,searchKey,&keylen,&fitsStatus);
	// on FitsError, always print a message (!silent).
	if (fitsStatus != 0 ) throw FitsError(fitsStatus,false);
	// require exact match
        if (strcmp(name.c_str(),searchKey) == 0) 
        {
        	// this should work for standard compliant
        	// string implementation, where the length added
        	// to content is determined by traits::length()
                content += card + 8;
                content += "\n";  
        }
  }

  // print a message if there are no "name" type records.
  if (content.size() == 0) throw NoSuchKeyword(name);




  return content; 
  }

  const String& HDU::getComments ()
  {
  try
  {
          m_comment = getNamedLines("COMMENT");
  }
  catch (HDU::NoSuchKeyword) {}
  catch (FitsError) {}
  catch (...) { throw; }
  return m_comment;
  }

  void HDU::writeComment (const String& comment)
  {
    int status(0);
    makeThisCurrent();
    if (fits_write_comment(fitsPointer(),const_cast<char*>(comment.c_str()),&status))
                throw FitsError(status);

    m_comment = comment;             
  }

  const String& HDU::getHistory ()
  {

  try
  {
          m_history = getNamedLines("HISTORY");
  }
  catch (HDU::NoSuchKeyword) {}
  catch (FitsError) {}
  catch (...) { throw; }
  return m_history;
  }

  void HDU::writeHistory (const String& history)
  {
    int status(0);
    makeThisCurrent();
    if (fits_write_history(fitsPointer(),const_cast<char*>(history.c_str()),&status))
                throw FitsError(status);
    m_history = history;             
  }

  void HDU::writeDate ()
  {
    static const String DATE("DATE");
    int status(0);
    makeThisCurrent();
    if (fits_write_date(fitsPointer(),&status)) throw FitsError(status);
    FITSUtil::auto_array_ptr<char> pDate(new char[FLEN_KEYWORD]);
    FITSUtil::auto_array_ptr<char> pDateComment(new char[FLEN_COMMENT]);
    char* date = pDate.get();
    if (fits_read_key_str(fitsPointer(),
        const_cast<char*>(DATE.c_str()),date,pDateComment.get(),&status))   throw FitsError(status);
    NewKeyword<String> creator(this,String(date));
    Keyword* newKey = creator.MakeKeyword(DATE,String(pDateComment.get()));
    std::map<String,Keyword*>::iterator itOld = m_keyWord.find(DATE);
    if (itOld != m_keyWord.end())
    {
       delete itOld->second;
       m_keyWord.erase(itOld);
    }
    m_keyWord[DATE] = newKey;               

  }

  void HDU::suppressScaling (bool toggle)
  {
     makeThisCurrent();
     int status=0;
     if (toggle)
     {
        // Ignore scale keyword values and replace with defaults.
        fits_set_bscale(m_parent->fptr(), 1.0, 0.0, &status);
     }
     else
     {
       // Restore the use of the scale keyword values by re-reading 
       // the header keywords.
        fits_set_hdustruc(m_parent->fptr(), &status);
     }
  }

  void HDU::writeChecksum ()
  {
     fitsfile *fPtr = m_parent->fptr();
     makeThisCurrent();
     int status=0;
     if (fits_write_chksum(fPtr, &status))
        throw FitsError(status);
  }

  void HDU::updateChecksum ()
  {
     fitsfile *fPtr = m_parent->fptr();
     makeThisCurrent();
     int status=0;
     if (fits_update_chksum(fPtr, &status))
        throw FitsError(status);
  }

  std::pair<int,int> HDU::verifyChecksum () const
  {
     std::pair<int,int> sumsOK(0,0);
     // Note that within this const function m_parent is a const
     // pointer but not a pointer-to-const, which is how we can
     // get away with this.  
     fitsfile *fPtr = m_parent->fptr();
     makeThisCurrent();
     int status=0;
     if (fits_verify_chksum(fPtr, &sumsOK.first, &sumsOK.second, &status))
        throw FitsError(status);
     return sumsOK;
  }

  std::pair<unsigned long,unsigned long> HDU::getChecksum () const
  {
     std::pair<unsigned long,unsigned long> sums(0,0);
     fitsfile *fPtr = m_parent->fptr();
     makeThisCurrent();
     int status=0;
     if (fits_get_chksum(fPtr, &sums.first, &sums.second, &status))
        throw FitsError(status);
     return sums;
  }

  void HDU::deleteKey (const String& doomed)
  {
    Keyword& k = keyWord(doomed); //throws no such keyword if there isn't.
    int status(0);
    if (fits_delete_key(fitsPointer(),const_cast<char*>(k.name().c_str()),&status)) throw FitsError(status);
    std::map<String,Keyword*>::iterator ki(m_keyWord.find(doomed));
    delete (*ki).second;
    m_keyWord.erase(ki);
  }

  void HDU::readAllKeys ()
  {
    makeThisCurrent();
    int status(0);
    int n (0);
    int dum(0);
    if (fits_get_hdrpos(fitsPointer(),&n,&dum,&status)) throw FitsError(status);

    for (int j = 1; j <= n; ++j)
    {
       // If the reading of a particular keyword fails, most likely due
       // to an undefined value, simply skip and continue to next
       // keyword.
       try
       {
          // get keywords by header position and store all the keys that
          // are neither metadata for columns nor history/comment cards.
          std::auto_ptr<Keyword> key(KeywordCreator::getKeyword(j,this));
          // comment/history cards have no value field and return 0 here.
          if (key.get() != 0)
          {
              int keyClass = fits_get_keyclass(const_cast<char*>(key->name().c_str()));
              for (size_t i=0; i<s_nCategories; ++i)
              {
                 if (keyClass == s_iKeywordCategories[i])
                 {
                    saveReadKeyword(key.get());
                    break;
                 }
              }
          }       
       }
       catch (FitsError& )
       {
       }
    }

    getHistory();
    getComments();
  }

  void HDU::copyAllKeys (const HDU* inHdu)
  {
     if (this != inHdu)
     {
        makeThisCurrent();
        std::map<string,Keyword*>::const_iterator itInKeys =
                        inHdu->keyWord().begin();
        std::map<string,Keyword*>::const_iterator itInKeysEnd =
                        inHdu->keyWord().end();
        while (itInKeys != itInKeysEnd)
        {
           // User may have read structure-related keys that normally shouldn't
           // go into keyWord map.  Therefore check each key before copying it.
           int keyClass = fits_get_keyclass(const_cast<char*>(itInKeys->first.c_str()));
           for (size_t i=0; i<s_nCategories; ++i)
           {
              if (keyClass == s_iKeywordCategories[i])
              {
                 addKey(itInKeys->second);
                 break;
              }
           }                      
           ++itInKeys;            
        }                
     }
  }

  std::vector<int> HDU::keywordCategories ()
  {
     // This is not the most efficient way to grant public access to
     // the category int[], but it seems better than storing
     // a static vector<int> in addition to the int[].
     std::vector<int> tmpCategories(&s_iKeywordCategories[0],
                &s_iKeywordCategories[s_nCategories]);
     // DO NOT return by reference.
     return tmpCategories;
  }

  void HDU::zeroInit (double value)
  {
     // This is a private counterpart to the public bzero set functions.
     // It does not check whether this setting would cause a change
     // in the data type of the image, since presumably it is only
     // called immediately after this object is first created in
     // HDUCreator.  It also does not write a keyword since it is
     // called either for case of pre-existing HDU (which already
     // has its keyword) or new unsigned-type HDU, in which case
     // CFITSIO fits_create_img automatically sets the keyword.
     m_zero = value;
  }

  void HDU::scaleInit (double value)
  {
     // See notes in zeroInit function.
     m_scale = value;
  }

  bool HDU::checkImgDataTypeChange (double zero, double scale) const
  {
     // This ASSUMES this is an image hdu and is also the current hdu.
     // Return false if the new zero/scale will cause a change from
     // a non-float/double to a float/double type.  
     bool isOK = true;
     if (m_bitpix != FLOAT_IMG && m_bitpix != DOUBLE_IMG)
     {
        // Save original state of zero/scale keywords
        bool isZeroKey = true;
        bool isScaleKey = true;
        double savZero = 0.0;
        double savScale = 1.0;
        int status = 0;
        fitsfile* fp = m_parent->fptr();
        if (fits_read_key_dbl(fp, BZERO, &savZero, 0, &status))
        {
           isZeroKey = false;
           savZero = 0.0;
           status = 0;
        }
        if (fits_read_key_dbl(fp, BSCALE, &savScale, 0, &status))
        {
           isScaleKey = false;
           savScale = 1.0;
           status = 0;
        }

        if (fits_update_key(fp, Tdouble, BZERO, &zero, 0, &status))
        {
           throw FitsError(status, false);
        }
        if (fits_update_key(fp, Tdouble, BSCALE, &scale, 0, &status))
        {
           int iErr = status;
           status = 0;
           if (isZeroKey)
              fits_update_key(fp, Tdouble, BZERO, &savZero, 0, &status);
           else
              fits_delete_key(fp, BZERO, &status);
           throw FitsError(iErr, false);
        }

        int newBitpix=0;
        fits_get_img_equivtype(fp, &newBitpix, &status);
        if (status || newBitpix == FLOAT_IMG || newBitpix == DOUBLE_IMG)
        {
           isOK = false;
        }
        int iErr = status;
        status = 0;

        // Now reset things back the way they were, whether or not there
        // was a status error above.
        if (isZeroKey)
           fits_update_key(fp, Tdouble, BZERO, &savZero, 0, &status);
        else
           fits_delete_key(fp, BZERO, &status);
        if (isScaleKey)
           fits_update_key(fp, Tdouble, BSCALE, &savScale, 0, &status);
        else
           fits_delete_key(fp, BSCALE, &status);

        if (iErr)
           throw FitsError(iErr, false);
     }
     return isOK;
  }

  // Additional Declarations
    Keyword& HDU::addKey(const string& name, const char* charString, const String& comment)
    {
        return addKey(name,String(charString),comment);      
    }

    Keyword* HDU::addKey(const Keyword* inKeyword)
    {
       Keyword* newKeyword = inKeyword->clone();
       newKeyword->setParent(this);
       makeThisCurrent();
       const String& keyname = newKeyword->name();
       std::map<String,Keyword*>::value_type refToEntry(keyname,newKeyword);
       std::map<String,Keyword*>::iterator itOld = m_keyWord.find(keyname);
       if (itOld != m_keyWord.end())
       {
          delete itOld->second;
          m_keyWord.erase(itOld);
       }
       m_keyWord.insert(refToEntry);
       newKeyword->write();
       return newKeyword;
    }
    
    Keyword& HDU::readNextKey(const std::vector<String>& incList,
                             const std::vector<String>& excList,
                             bool searchFromBeginning)
    {
       const size_t nInc = incList.size();
       const size_t nExc = excList.size();
       bool silent=false;
       if (!nInc)
          throw FitsException("***CCfits Error: No keyword names specified for search.",silent);
       // Length check is perhaps not necessary since we're doing
       // dynamic allocation and cfitsio has its own internal checks.
       bool badLength=false;      
       for (size_t i=0; i<nInc && !badLength; ++i)
       {
          if (incList[i].length() >= FLEN_KEYWORD)
             badLength=true; 
       }
       for (size_t i=0; i<nExc && !badLength; ++i)
       {
          if (excList[i].length() >= FLEN_KEYWORD)
             badLength=true; 
       }
       if (badLength)
       {
          throw FitsException("***CCfits Error: Keyword names exceeds allowed legnth in readNextKey",silent);
       }
       
       char **inc = new char*[nInc];
       for (size_t i=0; i<nInc; ++i)
       {
          const size_t len = incList[i].length();
          inc[i] = new char[len+1];
          incList[i].copy(inc[i],len,0);
          inc[i][len] = 0;
       }
       char **exc=0;
       if (nExc)
       {
          exc = new char*[nExc];
          for (size_t i=0; i<nExc; ++i)
          {
             const size_t len = excList[i].length();
             exc[i] = new char[len+1];
             excList[i].copy(exc[i],len,0);
             exc[i][len] = 0;
          }
       }
       
       // If this is not the current header, make it so and
       //  place keyword pointer to the beginning regardless of
      //   'searchFromBeginning' flag setting.
       int status=0;
       char card[FLEN_CARD];
       int num=0;
       try
       {
          fits_get_hdu_num(fitsPointer(),&num);
          if (num != m_index+1)
          {
             makeThisCurrent();
             // This merely resets the cfitsio pointer to the beginning 
             //    of the HDU.  It doesn't retrieve card.
             fits_read_record(fitsPointer(),0,card,&status);
          } 

          if (searchFromBeginning)
          {
             fits_read_record(fitsPointer(),0,card,&status);
          }
          fits_find_nextkey(fitsPointer(),inc,(int)nInc,exc,(int)nExc,card,&status);
       }
       catch (...)
       {
          for (size_t i=0; i<nInc; ++i)
             delete [] inc[i];
          delete [] inc;
          if (nExc)
          {
             for (size_t i=0; i<nExc; ++i)
                delete [] exc[i];
             delete [] exc;
          }       
          throw;
       }
       for (size_t i=0; i<nInc; ++i)
          delete [] inc[i];
       delete [] inc;
       if (nExc)
       {
          for (size_t i=0; i<nExc; ++i)
             delete [] exc[i];
          delete [] exc;
       }       
       if (status)
       {
          throw FitsError(status);
       }
       
       // This can throw       
       Keyword* newKey = KeywordCreator::getKeywordFromCard(card, this);
       
       const string& keyname = newKey->name();
       std::map<String,Keyword*>::value_type refToEntry(keyname,newKey);
       std::map<String,Keyword*>::iterator itOld = m_keyWord.find(keyname);
       if (itOld != m_keyWord.end())
       {
          delete itOld->second;
          m_keyWord.erase(itOld);
       }
       m_keyWord.insert(refToEntry);
       return *(refToEntry.second);
    }
} // namespace CCfits
