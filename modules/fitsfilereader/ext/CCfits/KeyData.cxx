//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// KeyData
#include "KeyData.h"
#include <complex>
#include <iomanip>

namespace CCfits
{   
#ifndef SPEC_TEMPLATE_IMP_DEFECT
#ifndef SPEC_TEMPLATE_DECL_DEFECT
        template<>
        void KeyData<bool>::write() 
        {
           Keyword::write();
           int status = 0;
           int value(0);
           if (m_keyval) value = 1; 
           if (fits_update_key(fitsPointer(), Tlogical, 
			           const_cast<char *>(name().c_str()),
			           &value,
			           const_cast<char *>(comment().c_str()), 
			           &status)) throw FitsError(status);

        }

        template<>
        void KeyData<String>::write() 
        {
           Keyword::write();
           int status = 0;
           if (fits_update_key(fitsPointer(), Tstring, 
			           const_cast<char *>(name().c_str()),
			           const_cast<char*>(m_keyval.c_str()),
			           const_cast<char *>(comment().c_str()), 
			           &status)) throw FitsError(status);

        }

        template<>
        void KeyData<std::complex<float> >::write() 
        {
             Keyword::write();
             int status = 0;
             FITSUtil::auto_array_ptr<float> keyVal( new float[2]);
             keyVal[0] = m_keyval.real(); 
             keyVal[1] = m_keyval.imag(); 
             if (fits_update_key(fitsPointer(), Tcomplex, 
        			   const_cast<char *>(name().c_str()),
        			   keyVal.get(),
         			   const_cast<char *>(comment().c_str()), 
        			   &status)) throw FitsError(status);

        }

        template<>
        void KeyData<std::complex<double> >::write() 
        {
             Keyword::write();
             int status = 0;
             FITSUtil::auto_array_ptr<double> keyVal(new double[2]);
             keyVal[0] = m_keyval.real(); 
             keyVal[1] = m_keyval.imag(); 
             if (fits_update_key(fitsPointer(), Tdblcomplex, 
        			   const_cast<char *>(name().c_str()),
        			   keyVal.get(),
         			   const_cast<char *>(comment().c_str()), 
        			   &status)) throw FitsError(status);

        }


        template <>
        std::ostream & KeyData<String>::put (std::ostream &s) const
        {
   		using std::setw;
   		s << "Keyword Name: " << setw(10) << name() << "  Value: " << setw(14) 
                  << keyval() << " Type: " << setw(20) << " string "  << " Comment: " << comment();
          return s;
        }
#endif
#endif

        template  <>
        void KeyData<String>::keyval(const String& newVal)
        {
                m_keyval = newVal;

        }

#ifndef SPEC_TEMPLATE_IMP_DEFECT
#ifndef SPEC_TEMPLATE_DECL_DEFECT
        template <>
        std::ostream & KeyData<std::complex<float> >::put (std::ostream &s) const
        {
   		using std::setw;
           	s << "Keyword Name: " << name() << " Value: " << m_keyval.real() << " +   i " 
             	  << m_keyval.imag() <<   " Type: " <<  setw(20) << " complex<float> " 
             	  << " Comment: " << comment()   << std::endl;
          return s;
        }

        template <>
        std::ostream & KeyData<std::complex<double> >::put (std::ostream &s) const
        {
   		using std::setw;
           	s << "Keyword Name: " << name() << " Value: " << m_keyval.real() << " +   i " 
             	  << m_keyval.imag() <<   " Type: " <<  setw(20) << " complex<double> " 
             	  << " Comment: " << comment()   << std::endl;

	          return s;
        }

        template <>
        std::ostream & KeyData<bool>::put (std::ostream &s) const
        {
   		using std::setw;
   		s << "Keyword Name: " << setw(10) << name() 
                  << "  Value: " << std::boolalpha  << setw(8) << keyval() 
                  << "  Type: " << setw(20) << " logical "  << " Comment: " << comment();
                return s;
        }


#endif
#endif

#ifndef SPEC_TEMPLATE_DECL_DEFECT
        template  <>
        const String& KeyData<String>::keyval() const
        {
                return m_keyval;

        }
#endif

#ifndef SPEC_TEMPLATE_DECL_DEFECT
        template  <>
        void KeyData<std::complex<float> >::keyval(const std::complex<float>&  newVal)
        {
                m_keyval = newVal;

        }
        template  <>
        void KeyData<std::complex<double> >::keyval(const std::complex<double>&  newVal)
        {
                m_keyval = newVal;

        }

        template  <>
        const std::complex<float>& KeyData<std::complex<float> >::keyval() const
        {
                return m_keyval;

        }

        template  <>
        const std::complex<double>& KeyData<std::complex<double> >::keyval() const
        {
                return m_keyval;

        }

#endif


} // namespace CCfits
