//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman
#include <iosfwd>

// FitsError
#include "FitsError.h"
// HDU
#include "HDU.h"
// Keyword
#include "Keyword.h"
// FITS
#include "FITS.h"

#include "KeyData.h"


namespace CCfits {

  // Class CCfits::Keyword::WrongKeywordValueType 

  Keyword::WrongKeywordValueType::WrongKeywordValueType (const String& diag, bool silent)
  : FitsException("Error: attempt to read keyword into variable of incorrect type",silent) 
  {
     addToMessage(string("\nKeyname: ") + diag);
  if ( FITS::verboseMode() || !silent) std::cerr << "\nKeyname: " << diag << "\n";
  }


  // Class CCfits::Keyword 

  Keyword::Keyword(const Keyword &right)
	: m_keytype(right.m_keytype),
        m_parent(0), // Can only assign parent when added to an HDU.
	m_comment(right.m_comment),
        m_name(right.m_name)
  {
  }

  Keyword::Keyword (const String &keyname, ValueType keytype, HDU* p, const String &comment)
	: m_keytype(keytype),
        m_parent(p),
	m_comment(comment),
        m_name(keyname)
  {
  }


  Keyword::~Keyword()
  {
  }


  Keyword & Keyword::operator=(const Keyword &right)
  {
   if (this != &right) copy(right);
   return *this;
  }


  bool Keyword::operator==(const Keyword &right) const
  {
   return compare(right);
  }

  bool Keyword::operator!=(const Keyword &right) const
  {
  return !compare(right);
  }


  void Keyword::copy (const Keyword& right)
  {
  m_name = right.m_name;
  m_comment = right.m_comment;
  m_keytype  = right.m_keytype;
  // Can only assign parent when added to an HDU.  Do not change
  // m_parent during an assign.
  }

  bool Keyword::compare (const Keyword &right) const
  {
  if (m_name != right.m_name) return false;
  if (m_keytype != right.m_keytype) return false;
  if (m_comment != right.m_comment) return false;
  // parent not relevant to compare.
  //if (m_parent  != right.m_parent) return false;
  return true;
  }

  void Keyword::write ()
  {
     // Just perform parent pointer checking in here.  KeyData<T>
     // subclass will actually handle the writing.
     if (!m_parent)
     {
        bool silent=true;
        throw FitsException("***CCfits Warning: Keyword must be added to an HDU before it can be written.",silent);
     }
     m_parent->makeThisCurrent();
  }

  fitsfile* Keyword::fitsPointer () const
  {
    fitsfile* fPtr = 0;
    if (m_parent)
       fPtr = m_parent->fitsPointer();
    return fPtr;
  }

  // Additional Declarations

} // namespace CCfits
namespace CCfits
{
#ifndef SPEC_TEMPLATE_IMP_DEFECT
#ifndef SPEC_TEMPLATE_DECL_DEFECT
   template<>
   float& Keyword::value(float& val) const
   {
      double dval=.0;
      val = static_cast<float>(value(dval));
      return val;
   }

   template<>
   double& Keyword::value(double& val) const
   {
      switch (m_keytype)
      {
         case Tint:
            {
               const KeyData<int>& thisKey = static_cast<const KeyData<int>&>(*this);
               val = thisKey.keyval();
            }
            break;
         case Tfloat:
            {
               const KeyData<float>& thisKey = static_cast<const KeyData<float>&>(*this);
               val = thisKey.keyval();
            }
            break;
         case Tdouble:
            {
               // Note: if val is of type float some precision will be lost here,
               // but allow anyway.  Presumably the user doesn't mind or they
               // wouldn't be using single precision.
               const KeyData<double>& thisKey = static_cast<const KeyData<double>&>(*this);
               val = thisKey.keyval();
            }
            break;
         case Tstring:
            {
               // Allow only if string can be converted to an integer.
               const KeyData<String>& thisKey = static_cast<const KeyData<String>&>(*this); 
               std::istringstream testStream(thisKey.keyval());
               int stringInt = 0;
               if (!(testStream >> stringInt) || !testStream.eof())
               {
                  throw Keyword::WrongKeywordValueType(name());
               }
               val = stringInt;
            }
            break;
         default:
            throw Keyword::WrongKeywordValueType(name());
            break;
      }
      return val;
   }

   template <>
   int& Keyword::value(int& val) const
   {
         if (m_keytype == Tstring)
         {
            // Allow only if string can be converted to an integer.
            const KeyData<String>& thisKey = static_cast<const KeyData<String>&>(*this); 
            std::istringstream testStream(thisKey.keyval());
            int stringInt = 0;
            if (!(testStream >> stringInt) || !testStream.eof())
            {
               throw Keyword::WrongKeywordValueType(name());
            }
            val = stringInt;         
         } 
         else if (m_keytype == Tint)
         {
            const KeyData<int>& thisKey = static_cast<const KeyData<int>&>(*this);
            val = thisKey.keyval();
         } 
         else
         {
            throw Keyword::WrongKeywordValueType(name());
         }
         return val;    
   }

   template <>
   String& Keyword::value(String& val) const
   {
      switch (m_keytype)
      {
         case Tint:
           {
              const KeyData<int>& thisKey = static_cast<const KeyData<int>&>(*this);
              std::ostringstream oss;
              oss << thisKey.keyval();
              val = oss.str();              
           }
            break;
         case Tfloat:
           {
              const KeyData<float>& thisKey = static_cast<const KeyData<float>&>(*this);
              std::ostringstream oss;
              oss << thisKey.keyval();
              val = oss.str();              
           }
            break;
         case Tdouble:
           {
              const KeyData<double>& thisKey = static_cast<const KeyData<double>&>(*this);
              std::ostringstream oss;
              oss << thisKey.keyval();
              val = oss.str();              
           }
            break;
         case Tstring:
           {
              const KeyData<String>& thisKey = static_cast<const KeyData<String>&>(*this);
              val = thisKey.keyval();
           }
            break;
         default:
            throw Keyword::WrongKeywordValueType(name());
      }
      return val;
   }

   template <>
   void Keyword::setValue(const float& newValue)
   {
      if (m_keytype == Tfloat)
      {
         KeyData<float>& thisKey = static_cast<KeyData<float>&>(*this);
         thisKey.keyval(newValue);
         thisKey.write();
      }
      else if (m_keytype == Tdouble)
      {
         KeyData<double>& thisKey = static_cast<KeyData<double>&>(*this);
         thisKey.keyval(static_cast<double>(newValue));
         thisKey.write();
      }
      else
      {
         throw Keyword::WrongKeywordValueType(name());
      }
   }

   template <>
   void Keyword::setValue(const double& newValue)
   {
      if (m_keytype == Tdouble)
      {
         KeyData<double>& thisKey = static_cast<KeyData<double>&>(*this);
         thisKey.keyval(newValue);
         thisKey.write();
      }
      else if (m_keytype == Tfloat)
      {
         // This will lose precision but allow it anyway.
         KeyData<float>& thisKey = static_cast<KeyData<float>&>(*this);
         thisKey.keyval(static_cast<float>(newValue));
         thisKey.write();
      }
      else
      {
         throw Keyword::WrongKeywordValueType(name());
      }

   }

   template <>
   void Keyword::setValue(const int& newValue)
   {
      if (m_keytype == Tint)
      {
         KeyData<int>& thisKey = static_cast<KeyData<int>&>(*this);
         thisKey.keyval(newValue);
         thisKey.write();
      }
      else if (m_keytype == Tfloat)
      {
         KeyData<float>& thisKey = static_cast<KeyData<float>&>(*this);
         thisKey.keyval(static_cast<float>(newValue));
         thisKey.write();
      }
      else if (m_keytype == Tdouble)
      {
         KeyData<double>& thisKey = static_cast<KeyData<double>&>(*this);
         thisKey.keyval(static_cast<double>(newValue));
         thisKey.write();
      }
      else if (m_keytype == Tstring)
      {
         KeyData<String>& thisKey = static_cast<KeyData<String>&>(*this);
         std::ostringstream oss;
         oss << newValue;
         thisKey.keyval(oss.str());
         thisKey.write();
      }
      else
      {
         throw Keyword::WrongKeywordValueType(name());
      }

   }

#endif
#endif
}
