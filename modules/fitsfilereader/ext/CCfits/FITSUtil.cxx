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

// FITSUtil
#include "FITSUtil.h"
#include "FitsError.h"
#include "FITS.h"
#include <algorithm>
#include <cstring>
#include <cctype>
using std::string;


namespace CCfits {
  const unsigned long USBASE = 1 << 15;
  const unsigned long ULBASE = (unsigned long)1 << 31;

  namespace FITSUtil {
  InvalidConversion::InvalidConversion (const string& diag, bool silent)
  : FitsException(string("Fits Error: Attempt to perform invalid implicit conversion "),silent)
  {
        addToMessage(diag);       
	if (FITS::verboseMode() || !silent) std::cerr << diag << '\n';
  }

  char** CharArray (const std::vector<string>& inArray)
  {
      size_t n = inArray.size();
      if (n == 0) return 0;
      char** c  = new char*[n];

      for (size_t i = 0; i < n; ++i)
      {
         size_t m(inArray[i].length());
	 c[i] = new char[m+1];
	 strncpy(c[i],inArray[i].c_str(),m + 1);
      }

      return c;      
  }
#if TEMPLATE_AMBIG_DEFECT || TEMPLATE_AMBIG7_DEFECT
  void fillMSvsvs (std::vector<string>& outArray, const std::vector<string>& inArray, size_t first, size_t last)
  {
          // recall behavior of iterators: last has to be one-past-the-last entry.
          // if last == first, this will still do something. the "first-1" is a zero based indexing
          // correction. 
          outArray.assign(inArray.begin()+first-1,inArray.begin()+last);  
  }        
#endif

  void fill (std::vector<string>& outArray, const std::vector<string>& inArray, size_t first, size_t last)
  {
          // recall behavior of iterators: last has to be one-past-the-last entry.
          // if last == first, this will still do something. the "first-1" is a zero based indexing
          // correction. 
          outArray.assign(inArray.begin()+first-1,inArray.begin()+last);  
  }        

  string lowerCase(const string& inputString)
  {
        const size_t n(inputString.length());
        string outputString(n,' ');
        for (size_t l = 0 ; l < n; ++l)
        {
                 outputString[l] = tolower(inputString[l]);          
        }
        return outputString;  
  }

  string upperCase(const string& inputString)
  {
        const size_t n(inputString.length());
        string outputString(n,' ');        
        for (size_t l = 0 ; l < n; ++l)
        {
                 outputString[l] = toupper(inputString[l]);          
        }
        return outputString;  
  }

  string::size_type checkForCompressString(const string& fileName)
  {
     // Simply look for the first occurrence of the form "[compress....".
     const string leadIndicator("[compress");
     string::size_type start = fileName.find(leadIndicator);
     return start;
  }

  string FITSType2String ( int typeInt )
  {
    string keyString("");

    switch (typeInt)        
    {
        default:
        case Tnull:
                keyString = "Unknown";
                break;
        case Tbit:
                keyString = "bit";
                break;
        case Tbyte:
                keyString = "byte";
                break;
        case Tlogical:
                keyString = "logical";
                break;
        case Tstring:
                keyString = "string";
                break;
        case Tushort:
                keyString = "unsigned short";
                break;
        case Tshort:
                keyString = "short";
                break;
        case Tuint:
                keyString = "unsigned integer";
                break;
        case Tint:
                keyString = "integer";
                break;
        case Tulong:
                keyString = "unsigned long";
                break;
        case Tlong:
                keyString = "long";
                break;
        case Tlonglong:
                keyString = "long long";
                break;
        case Tfloat:
                keyString = "float";
                break;
        case Tdouble:
                keyString = "double";
                break;
        case Tcomplex:
                keyString = "float complex";
                break;
        case Tdblcomplex:
                keyString = "double complex";
                break;
    }

    return keyString;     
  }

  bool MatchStem::operator()(const string& left, const string& right) const
  {
          static const string DIGITS("0123456789");
          size_t n(left.find_last_not_of(DIGITS));
          if ( n != string::npos ) return (left.substr(0,n) == right);
          else return (left == right);
  }



// VF<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
	void 
	fillMSvfvf(std::vector<std::complex<float> >& outArray, const std::vector<std::complex<float> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = inArray[j];

				}
	}
#else

	void 
	fill(std::vector<std::complex<float> >& outArray, const std::vector<std::complex<float> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = inArray[j];
                }
	}
#endif


#ifdef TEMPLATE_AMBIG_DEFECT
// VF<-VD
	void 
	fillMSvfvd(std::vector<std::complex<float> >& outArray, const std::vector<std::complex<double> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = std::complex<float>(inArray[j].real(),
                                                inArray[j].imag());
                }
	}
#else

// VF<-VD
	void 
	fill(std::vector<std::complex<float> >& outArray, const std::vector<std::complex<double> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = std::complex<float>(inArray[j].real(),
                                                inArray[j].imag());
                }
	}
#endif

// VD<-VD  
#ifdef TEMPLATE_AMBIG_DEFECT
	void 
	fillMSvdvd(std::vector<std::complex<double> >& outArray, const std::vector<std::complex<double> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = inArray[j];
                }

	}  
#else
	void 
	fill(std::vector<std::complex<double> >& outArray, const std::vector<std::complex<double> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = inArray[j];
                }

	}
#endif

// VD<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
	void 
	fillMSvdvf(std::vector<std::complex<double> >& outArray, const std::vector<std::complex<float> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = std::complex<double>(inArray[j].real(),
                                                inArray[j].imag());
                }
	}  
#else
	void 
	fill(std::vector<std::complex<double> >& outArray, const std::vector<std::complex<float> >& inArray, 
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = std::complex<double>(inArray[j].real(),
                                                inArray[j].imag());
                }
	}  
#endif

// AF<-VF

// AF<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
	void 
	fillMSafvf(std::valarray<std::complex<float> >& outArray, 
                        const std::vector<std::complex<float> >& inArray,
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1 ] = inArray[j];
                }
	} 
#else
	void 
	fill(std::valarray<std::complex<float> >& outArray, 
                        const std::vector<std::complex<float> >& inArray,
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1 ] = inArray[j];
                }
	}
#endif

// AF<-VD
 	void 
	fill(std::valarray<std::complex<float> >& outArray, 
                        const std::vector<std::complex<double> >& inArray,
                        size_t first, size_t last)
        {                
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1 ] 
                                        = std::complex<float>(inArray[j].real(),inArray[j].imag());
                }
	}                  
// AD<-VD
#ifdef TEMPLATE_AMBIG_DEFECT
	void 
	fillMSadvd(std::valarray<std::complex<double> >& outArray, 
                        const std::vector<std::complex<double> >& inArray,
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = inArray[j];
                }
	}
#else
	void 
	fill(std::valarray<std::complex<double> >& outArray, 
                        const std::vector<std::complex<double> >& inArray,
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1] = inArray[j];
                }
	}                  
#endif

// AD<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
	void 
	fillMSadvf(std::valarray<std::complex<double> >& outArray, 
                        const std::vector<std::complex<float> >& inArray,
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1 ] 
                                        = std::complex<float>(inArray[j].real(),inArray[j].imag());
                }
	}
#else
	void 
	fill(std::valarray<std::complex<double> >& outArray, 
                        const std::vector<std::complex<float> >& inArray,
                        size_t first, size_t last)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t range = last - first + 1;
                if (outArray.size() != range) outArray.resize(range);
                for (size_t j = first - 1; j < last; ++j)
                {
                        outArray[j - first + 1 ] 
                                        = std::complex<float>(inArray[j].real(),inArray[j].imag());
                }
	}
#endif
// AF<-AF
	void 
	fill(std::valarray<std::complex<float> >& outArray, 
                        const std::valarray<std::complex<float> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                outArray = inArray;
	}   

// AD<-AD        
	void 
	fill(std::valarray<std::complex<double> >& outArray, 
                        const std::valarray<std::complex<double> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                outArray = inArray;
	}  

// AF<-AD
	void 
	fill(std::valarray<std::complex<float> >& outArray, 
                        const std::valarray<std::complex<double> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                for (size_t j = 0; j < N; ++j ) 
                {
                        outArray[j] = std::complex<float>(inArray[j].real(),inArray[j].imag());
                }
	}                  
// AD<-AF        
	void 
	fill(std::valarray<std::complex<double> >& outArray, 
                        const std::valarray<std::complex<float> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                for (size_t j = 0; j < N; ++j ) 
                {
                        outArray[j] = std::complex<double>(inArray[j].real(),inArray[j].imag());
                }
	}                  

// VF<-AF
	void 
	fill(std::vector<std::complex<float> >& outArray, 
                        const std::valarray<std::complex<float> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                for (size_t j = 0; j < N; ++j) outArray[j] = inArray[j];
	}   

// VD<-AD        
	void 
	fill(std::vector<std::complex<double> >& outArray, 
                        const std::valarray<std::complex<double> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                for (size_t j = 0; j < N; ++j) outArray[j] = inArray[j];
	}  

// VF<-AD
	void 
	fill(std::vector<std::complex<float> >& outArray, 
                        const std::valarray<std::complex<double> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                for (size_t j = 0; j < N; ++j ) 
                {
                        outArray[j] = std::complex<float>(inArray[j].real(),inArray[j].imag());
                }
	}                  
// VD<-AF        
	void 
	fill(std::vector<std::complex<double> >& outArray, 
                        const std::valarray<std::complex<float> >& inArray)
	{
       		// vector to vector assign. stdlib takes care of deletion.
                size_t N (inArray.size());
                if (outArray.size() != N) outArray.resize(N);
                for (size_t j = 0; j < N; ++j ) 
                {
                        outArray[j] = std::complex<double>(inArray[j].real(),inArray[j].imag());
                }
	}       

    // Parameterized Class CCfits::FITSUtil::FitsNullValue 

    // Class CCfits::FITSUtil::UnrecognizedType 

    UnrecognizedType::UnrecognizedType (string diag, bool silent)
      : FitsException(" Invalid type for FITS I/O ",silent)
    {
        addToMessage(diag);
        std::cerr << diag << '\n';
    }


  } // namespace FITSUtil
} // namespace CCfits
