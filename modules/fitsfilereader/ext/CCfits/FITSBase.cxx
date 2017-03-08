//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// PHDU
#include "PHDU.h"
// ExtHDU
#include "ExtHDU.h"
// FITSBase
#include "FITSBase.h"
// FITSUtil
#include "FITSUtil.h"



namespace CCfits {
  typedef  std::multimap<string,ExtHDU*> ExtMap;
  typedef  std::multimap<string,ExtHDU*>::iterator ExtMapIt;
  typedef  std::multimap<string,ExtHDU*>::const_iterator ExtMapConstIt;

  // Class CCfits::FITSBase 

  FITSBase::FITSBase (const String& fileName, RWmode rwmode)
    : m_currentCompressionTileDim(0),
      m_mode(rwmode), m_currentExtensionName(""), m_name(fileName),
      m_pHDU(0), m_extension(), m_fptr(0)
  {
  }


  FITSBase::~FITSBase()
  {

    destroyPrimary();    
    destroyExtensions();

    int status=0;
    if (m_fptr)
    {
       fits_close_file(m_fptr, &status);
    }
  }


  void FITSBase::destroyPrimary ()
  {
    delete m_pHDU;
    m_pHDU = 0;
  }

  void FITSBase::destroyExtensions ()
  {
        ExtMapIt endList = m_extension.end();

        for (ExtMapIt hdu = m_extension.begin();  hdu != endList; hdu++)
        {
                delete (*hdu).second;
        }

        m_extension.clear();
  }

  // Additional Declarations

} // namespace CCfits
