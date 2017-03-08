//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef KEYWORDT_H
#define KEYWORDT_H
#include "KeyData.h"
#include "HDU.h"
#include <typeinfo>
#include <sstream>

#ifdef _MSC_VER
#include "MSconfig.h"
#endif

// contains definitions of templated member functions for Keyword. This separate
// file organization is necessary to break cyclic dependency of Keyword on its
// subclass, KeyData.


namespace CCfits 
{

   template <typename T>
   T& Keyword::value (T& val) const
   {
      try
      {
            const KeyData<T>& thisKey = dynamic_cast<const KeyData<T>&>(*this);
	    val = thisKey.keyval();
      }
      catch (std::bad_cast)
      {
         throw Keyword::WrongKeywordValueType(name());
      }
      return val;
   }

   template <typename T>
   void Keyword::setValue (const T& newValue)
   {
           try
           {
                   KeyData<T>& thisKey = dynamic_cast<KeyData<T>&>(*this);
		   thisKey.keyval(newValue);
                   thisKey.write();
           }
           catch (std::bad_cast)
           {
                   throw Keyword::WrongKeywordValueType(name());
           }

   }

#if SPEC_TEMPLATE_IMP_DEFECT || SPEC_TEMPLATE_DECL_DEFECT
   template<>
   inline double& Keyword::value(double& val) const
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

   // NOTE: This function actually instantiates Keyword::value<double>
   // and therefore must be defined AFTER the specialized 
   // definition/declaration.
   template<>
   inline float& Keyword::value(float& val) const
   {
      double dval=.0;
      val = static_cast<float>(value(dval));
      return val;
   }

   template <>
   inline int& Keyword::value(int& val) const
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
   inline String& Keyword::value(String& val) const
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
   inline void Keyword::setValue(const float& newValue)
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
   inline void Keyword::setValue(const double& newValue)
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
   inline void Keyword::setValue(const int& newValue)
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
} // namespace CCfits

#endif
