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

// HDU
#include "HDU.h"
// KeywordCreator
#include "KeywordCreator.h"
//#include "HDU.h"
using std::cout;
using std::endl;


namespace CCfits {

  // Class CCfits::KeywordCreator 

  KeywordCreator::KeywordCreator (HDU* p)
      : m_keyword(0), m_forHDU(p)
  {
  }


  KeywordCreator::~KeywordCreator()
  {
  }


  Keyword* KeywordCreator::getKeyword (const String& keyName, HDU* p)
  {
   int     status=0;
   FITSUtil::auto_array_ptr<char> pCard(new char[FLEN_CARD]);
   char* card = pCard.get();

   if (fits_read_card(p->fitsPointer(), const_cast<char*>(keyName.c_str()), card,&status) ) 
           throw FitsError(status);
   return getKeywordFromCard(card, p, keyName);
  }

  Keyword* KeywordCreator::createKeyword (const String& keyName, const String& comment)
  {
  if (m_keyword == 0) m_keyword = MakeKeyword(keyName,comment);
  return m_keyword;
  }

  Keyword* KeywordCreator::getKeyword (const String& keyName, ValueType keyType, HDU* p)
  {
   int status = 0;
   String val = "";
   bool bvalue = 0;
   float fvalue = 0;
   double dvalue = 0;
   std::complex<float> xvalue;
   std::complex<double> zvalue;
   int ivalue = 0;
   unsigned int uvalue = 0;
   short isvalue = 0;
   unsigned short usvalue = 0;
   long ilvalue = 0;
   unsigned long ulvalue = 0;
   unsigned char byteVal = 0;

   FITSUtil::auto_array_ptr<char> pCard(new char[FLEN_CARD]);
   FITSUtil::auto_array_ptr<char> pV(new char[FLEN_VALUE]);
   FITSUtil::auto_array_ptr<char> pCom(new char[FLEN_COMMENT]);
   char* card = pCard.get();
   char* v = pV.get();
   char* com = pCom.get();
   Keyword* readKey = 0;

   if (fits_read_card(p->fitsPointer(), const_cast<char*>(keyName.c_str()), card,&status) ) 
           throw FitsError(status);

   if (fits_parse_value(card, v, com, &status))  throw FitsError(status);


   String value(v);
   String comment(com);

   if (KeywordCreator::isContinued(value))
   {
      bool restoreQuote = value[0] == '\'';
      KeywordCreator::getLongValueString(p, keyName, value);
      if (restoreQuote)
      {
         value = '\'' + value + '\'';
      }
   }

#ifdef SSTREAM_DEFECT
   std::istrstream vstream( value.c_str() );
#else
   std::istringstream vstream(value);
#endif

   switch (keyType)
   {
        case Tstring: 
	        readKey = new KeyData<String>(keyName, Tstring, value, p, comment);
	        break;
        case Tlogical: 
                vstream >> bvalue;
	        readKey = new KeyData<bool>(keyName, Tlogical, bvalue, p, comment);
	        break;
        case Tbyte: 
                vstream >> byteVal;
	        readKey = new KeyData<unsigned char>(keyName, Tbyte, byteVal, p, comment);
	        break;
        case Tfloat: 
                vstream >> fvalue;
	        readKey = new KeyData<float>(keyName, Tfloat, fvalue, p, comment);
	        break;
        case Tdouble: 
                vstream >> dvalue;
	        readKey = new KeyData<double>(keyName, Tdouble, dvalue, p, comment);
	        break;
        case Tcomplex: 
                vstream >> xvalue;
	        readKey = new KeyData<std::complex<float> >(keyName, Tcomplex, xvalue, p, comment);
	        break;
        case Tdblcomplex: 
                vstream >> zvalue;
	        readKey = new KeyData<std::complex<double> >(keyName, Tdblcomplex, zvalue, p, comment);
	        break;
        case Tint: 
                vstream >> ivalue;
	        readKey = new KeyData<int>(keyName, Tint, ivalue, p, comment);
	        break;
        case Tuint: 
                vstream >> uvalue;
	        readKey = new KeyData<unsigned int>(keyName, Tuint, uvalue, p, comment);
	        break;
        case Tshort: 
                vstream >> isvalue;
	        readKey = new KeyData<short>(keyName, Tshort, isvalue, p, comment);
	        break;
        case Tushort: 
                vstream >> usvalue;
	        readKey = new KeyData<unsigned short>(keyName, Tushort, usvalue, p, comment);
	        break;
        case Tlong: 
                vstream >> ilvalue;
	        readKey = new KeyData<long>(keyName, Tlong, ilvalue, p, comment);
	        break;
        case Tulong: 
                vstream >> ulvalue;
	        readKey = new KeyData<unsigned long>(keyName, Tulong, ulvalue, p, comment);
	        break;
        default:
                std::cerr << "Unknown keyword type\n";

   }
   return readKey;
  }

  Keyword* KeywordCreator::getKeyword (int keyNumber, HDU* p)
  {
    FITSUtil::auto_array_ptr<char> pValue(new char[FLEN_VALUE]);    
    FITSUtil::auto_array_ptr<char> pKey(new char[FLEN_KEYWORD]);
    FITSUtil::auto_array_ptr<char> pComment(new char[FLEN_COMMENT]);
    char* key = pKey.get();
    char* comment = pComment.get();
    char* value = pValue.get();
    int status(0);
    if (fits_read_keyn(p->fitsPointer(),keyNumber,key,value,comment,&status)) 
    {
            throw FitsError(status);    
    }
    String valString(value);
    if (KeywordCreator::isContinued(valString))
    {
       bool restoreQuote = valString[0] == '\'';
       KeywordCreator::getLongValueString(p, String(key), valString);
       if (restoreQuote)
       {
          valString = '\'' + valString + '\'';
       }
    }
    int keyClass (fits_get_keyclass(key));
    if ( keyClass == TYP_COMM_KEY || keyClass == TYP_CONT_KEY)
    {
        return 0;
    }
    else
    {
        return parseRecord(String(key),valString,String(comment),p);
    }
  }

  Keyword* KeywordCreator::parseRecord (const String& name, const String& valueString, const String& comment, HDU* hdu)
  {

     char keyType('\0');
     String value ("");
     bool bvalue (0);
     double dvalue (0);
     int ivalue (0);
     std::complex<float> xvalue(0,0);
     int status (0);

     if ( valueString[0] == '\'')
     {
          value = valueString.substr(1,valueString.length()-2);
     }
     else
     {
         value = valueString;       
     }
     
     if (fits_get_keytype(const_cast<char*>(valueString.c_str()), &keyType, &status))   
     {
             throw FitsError(status);
     }
     // If input float or complex<float> is using 'D' syntax for exponent,
     //  must convert to 'E'.  FITS allows 'D', but C++ stringtream
     //  output conversion won't properly handle it.
     if (keyType == 'F' || keyType == 'X')
     {
        string::size_type locD=value.find('D');           
        if (locD !=string::npos)
        {
           value[locD] = 'E';
           if (keyType == 'X')
           {
              locD=value.find('D');
              if (locD != string::npos)
                 value[locD] = 'E';
           }
        }
     }
     
#ifdef SSTREAM_DEFECT
     std::istrstream vstream( value.c_str() );
#else   
     std::istringstream vstream(value);
#endif

     switch(keyType)
     {
        case 'L':
           value == "T" ? bvalue = true : bvalue = false;
	   return  new KeyData<bool>(name, Tlogical, bvalue, hdu, comment);
	   break;
        case 'F':
           vstream >> dvalue;
           return  new KeyData<double>(name, Tdouble, dvalue, hdu, comment);
	   break;
        case 'I':
        case 'T':
           vstream >> ivalue;
	   return  new KeyData<int>(name, Tint, ivalue, hdu, comment);
	   break;
        case 'X':
           vstream >> xvalue;
	   return new KeyData<std::complex<float> >(name, Tcomplex, xvalue, hdu, comment);
        case 'C':
        default:
	   return  new KeyData<String>(name, Tstring, 
                        value.substr(0,value.find_last_not_of(" ")+1),hdu, comment);                 
     }
     return 0;
  }

  bool KeywordCreator::isContinued (const String& value)
  {
    // Check whether the last non-whitespace char in the value string
    // is an ampersand, which indicates the value string is to be
    // continued on the next line.
    bool status = false;
    String::size_type ampTest = value.find_last_not_of(String(" \n\t'"));
    if (ampTest != String::npos)
    {
       if (value[ampTest] == '&')
       {
          status = true;
       }
    }

    return status;
  }

  void KeywordCreator::getLongValueString (HDU* p, const String& keyName, String& value)
  {
    char* lv = 0;
    int status = 0;
    // The following function actually allocates the memory for the
    // lv string - very unusual for cfitsio.
    if (fits_read_key_longstr(p->fitsPointer(), const_cast<char*>(keyName.c_str()), 
                &lv, 0, &status))
    {
       free(lv);
       throw FitsError(status);
    }
    value = String(lv);
    free(lv);
  }
  
  Keyword* KeywordCreator::getKeywordFromCard(char* card, HDU* p, const String& keyName)
  {
     FITSUtil::auto_array_ptr<char> pV(new char[FLEN_VALUE]);
     FITSUtil::auto_array_ptr<char> pCom(new char[FLEN_COMMENT]);
     char* v = pV.get();
     char* com = pCom.get();
     int status=0;
      
     String keywordName(keyName); 
     if (!keywordName.length())
     {
        int nameLength=0;
        FITSUtil::auto_array_ptr<char> pName(new char[FLEN_KEYWORD]);
        char *name = pName.get();
        if (fits_get_keyname(card, name, &nameLength, &status))
            throw FitsError(status);
        keywordName = String(name);         
     }
     if (fits_parse_value(card, v, com, &status))  
             throw FitsError(status);

     String valString(v);
     if (KeywordCreator::isContinued(valString))
     {
        bool restoreQuote = valString[0] == '\'';
        KeywordCreator::getLongValueString(p, keywordName, valString);
        if (restoreQuote)
        {
           valString = '\'' + valString + '\'';
        }
     }

     return parseRecord(keywordName,valString,String(com),p);
  }

  // Additional Declarations

} // namespace CCfits
