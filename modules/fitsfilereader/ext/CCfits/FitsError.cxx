//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman
#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#include <cassert>
#endif

// FITS
#include "FITS.h"
// FitsError
#include "FitsError.h"


namespace CCfits {

  // Class CCfits::FitsError 

  FitsError::FitsError (int errornum, bool silent)
  : FitsException("FITS Error: ", silent)
  {
     printMsg(errornum);
     if (FITS::verboseMode() || !silent) 
        std::cerr << message() << "\n";
  }


  void FitsError::printMsg (int error)
  {
     char cMessage[FLEN_ERRMSG];

     fits_get_errstatus(error, cMessage);
     addToMessage(string(cMessage));
  }

  // Class CCfits::FitsException 

  FitsException::FitsException (const string& msg, bool& silent)
    : m_message(msg)
  {
  if (FITS::verboseMode() || !silent) 
  {
          std::cerr << '\n' << msg;
          // set this to false for the purpose of this exception.
          // does NOT change verbose mode setting so this value is thrown
          // away after the exception completes.
          silent = false;
  }
  }


  void FitsException::addToMessage (const string& msgQual)
  {
     m_message += msgQual;
  }

  // Class CCfits::FitsFatal 

  FitsFatal::FitsFatal (const string& diag)
  {
     std::cerr << "*** CCfits Fatal Error: " << diag 
               << " please report this to xanprob@olegacy.gsfc.nasa.gov\n";

#ifdef TERMINATE_DEFECT
     // terminate() is not there as documented
	 assert ( false );
#else
	 std::terminate();
#endif
  }


} // namespace CCfits
