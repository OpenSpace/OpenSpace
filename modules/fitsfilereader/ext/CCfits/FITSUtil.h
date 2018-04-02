//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef FITSUTIL_H
#define FITSUTIL_H 1
#include "CCfits.h"

// functional
#include <functional>
// complex
#include <complex>
// valarray
#include <valarray>
// vector
#include <vector>
// string
#include <string>
// FitsError
#include "FitsError.h"
#include <typeinfo>


namespace CCfits {

  namespace FITSUtil {

/*! \namespace FITSUtil

\brief FITSUtil is a namespace containing functions used internally by CCfits, but
which might be of use for other applications. 

*/



/*! \class  MatchName

    \brief  predicate for classes that have a name attribute; match input string with instance name.

    Usage: MatchName<NamedClass> Ex;

    list<NamedClass> ListObject;

    //
    ...
    ...
    //

    find_if(ListObject.begin(),ListObject().end(),bind2nd(Ex,"needle"));


    Since most of the classes within CCfits are not implemented with lists, these
    functions are now of little direct use.

*/



/*! \class  MatchPtrName

    \brief  as for MatchName, only with the input class a pointer.


*/

/*! \class  MatchNum

    \brief  predicate for classes that have an index attribute; match input index with instance value.

    Usage: MatchName<IndexedClass> Ex;

    list<NamedClass> ListObject;

    //
    ...
    ...
    //

    find_if(ListObject.begin(),ListObject().end(),bind2nd(Ex,5));


    Since most of the classes within CCfits are implemented with std::maps
	 rather than lists, these functions are now of little direct use.
*/

/*! \class  MatchPtrNum
    \brief  as for MatchNum, only with the input class a pointer.
*/

/*! \class  MatchType

    \brief  function object that returns the FITS ValueType corresponding to an input intrinsic type

    This is particularly useful inside templated class instances where calls to cfitsio
    need to supply a value type. With this function one can extract the value type from
    the class type.

    <I>
    usage:

    MatchType<T> type;

    ValueType dataType = type();
    </I>

    Uses run-time type information (RTTI) methods.

*/

/*! \class  UnrecognizedType
    @ingroup FITSexcept
    @brief  exception thrown by MatchType if it encounters data type incompatible with cfitsio.

*/



/*! \class  auto_array_ptr

   \brief A class that mimics the std:: library auto_ptr class, but works
   with arrays.

   This code was written by Jack Reeves and first appeared C++ Report, March 1996
   edition. Although some authors think one shouldn't need such a contrivance, there
   seems to be a need for it when wrapping C code.

   Usage: replace

   float* <i>f</i> = new float[200];

   with

   FITSUtil::auto_array_ptr<float> <i>f(new float[200])</i>;

   Then the memory will be managed correctly in the presence of exceptions, and
   delete will be called automatically for <i>f</i> when leaving scope.

*/   

/*! \fn          explicit auto_array_ptr<X>::auto_array_ptr (X* p = 0) throw ();

        \brief constructor. allows creation of pointer to null, can be modified by reset()
*/

/*! \fn          explicit auto_array_ptr<X>::auto_array_ptr (auto_array_ptr<X>& right) throw ();
        \brief copy constructor
*/

/*!\fn          auto_array_ptr<X>::~auto_array_ptr();
        \brief destructor.
*/


/*!\fn            void auto_array_ptr<X>::operator = (auto_array_ptr<X>& right);
                \brief assignment operator: transfer of ownership semantics

*/



/*!\fn            X& auto_array_ptr<X>::operator * () throw ();
                \brief deference operator


*/

/*!\fn            X& auto_array_ptr<X>::operator [] (size_t i) throw ();
                \brief return a reference to the ith element of the array
*/

/*!\fn            X auto_array_ptr<X>::operator [] (size_t i) const throw ();
                \brief return a copy of the ith element of the array

*/

/*!\fn            X* auto_array_ptr<X>::get () const;
        \brief return a token for the underlying content of *this


*/

/*!\fn            X* auto_array_ptr<X>::release () throw ();

        \brief return underlying content of  *this, transferring memory ownership


*/

/*!\fn            X* auto_array_ptr<X>::reset (X* p) throw ();

        \brief change the content of the  auto_array_ptr to p

*/

/*!\fn            static void auto_array_ptr<X>::remove (X*& x) throw ();

        \brief utility function to delete the memory owned by x and set it to null.

*/

/*! \fn  char** CharArray(const std::vector<string>& inArray)

    \brief  function object that returns a C-array of strings from a 
    std::vector< std::vector<std::string> > object, such as used in a string valued Table column.

    This exists because the return type for a specialization (T**) is incompatible
    with typenames T other than string, which return a T*.
*/                        

/*! \class CVarray

        \brief Function object class for returning C arrays from standard library objects used
        in the FITS library implementation. 

        There are 3 versions which convert std::vector<T>, std::valarray<T>, 
        and std::vector<std::valarray<T> > objects to pointers to T, called
        CVarray, CAarray, and CVAarray.

        An alternative function, CharArray, is provided to deal with the special case of
        vector string arrays.

*/

/*! \fn  T* CVarray<T>::operator () (const std::vector<T>& inArray);

                \brief operator returning C array for use with scalar column data.
*/

/*! \class CAarray
        \brief  function object returning C array from a valarray. see CVarray for
        details                            
*/




/*! \fn  T* CAarray<T>::operator () (const std::valarray<T>& inArray);

                \brief operator returning C array for use with image data.

*/  

/*! \class CVAarray
        \brief  function object returning C array from a vector of valarrays. see CVarray for
        details                            
*/                                

/*! \fn  T* CVAarray<T>::operator () (const std::vector< std::valarray<T> >& inArray);

                \brief operator returning C array for use with vector column data.

*/


/*! \fn template <typename S, typename T> void fill(std::vector< S > &outArray, const std::vector< T > &inArray, size_t first, size_t last);

  \brief Convert input vector of type T to output vector of type S.

   The functions FITSUtil::fill do conversions between CCfits' internal representation
and user's input types. They encapsulate the task of
memory allocation also, facilitating support of implicit
conversions between the users data type and representation and what is
required by the CCfits implementation. For example a user can create a
std::vector<int> for storage in a single row of a Binary Table column,
of type long.  The internal representation is a std::valarray<long>
object.

\param outArray  output data
\param inArray  input data
\param first    first element of inArray to be written to outArray
\param last     last element of inArray to be written to outArray

*/ 

#ifdef _MSC_VER
#include "MSconfig.h" // for truncation double to float warning
#endif

     template <typename T>
    void swap(T& left,T& right);

    template <typename T>
    void swap(std::vector<T>& left, std::vector<T>& right);

    string lowerCase(const string& inputString);

    string upperCase(const string& inputString);

  // Check if a file name includes an image compression specifier,
  // and return its location if it exists.
    string::size_type checkForCompressString(const string& fileName);

  struct InvalidConversion : public FitsException
  {
		InvalidConversion(const string& diag, bool silent=false);

  };

  struct MatchStem : public std::binary_function<string,string,bool>
  {
          bool operator()(const string& left, const string& right) const;
  };

  static const  double d1(0);
  static const  float  f1(0);
  static const  std::complex<float> c1(0.);
  static const  std::complex<double> d2(0.);
  static const  string s1("");
  static const  int    i1(0);
  static const  unsigned int  u1(0);        
  static const  long l1(0);
  static const  unsigned long ul1(0);
  static const  LONGLONG ll1(0);
  static const  short s2(0);
  static const  unsigned short us1(0); 
  static const  bool b1(false);
  static const  unsigned char b2(0);  

  char** CharArray(const std::vector<string>& inArray);

  string FITSType2String( int typeInt );


  template <typename S, typename T> 
  void fill(std::vector<S>& outArray, const std::vector<T>& inArray,size_t first, size_t last);

  template <typename S, typename T> 
  void fill(std::valarray<S>& outArray, const std::valarray<T>& inArray);

  template <typename S, typename T> 
  void fill(std::valarray<S>& outArray, const std::vector<T>& inArray,size_t first, size_t last);


  template <typename S, typename T> 
  void fill(std::vector<S>& outArray, const std::valarray<T>& inArray);

  // VF<-AF
   void fill(std::vector<std::complex<float> >& outArray, 
                  const std::valarray<std::complex<float> >& inArray);

  // VF<-AD
  void fill(std::vector<std::complex<float> >& outArray, 
                  const std::valarray<std::complex<double> >& inArray);

  // VD<-AD
  void fill(std::vector<std::complex<double> >& outArray, 
                  const std::valarray<std::complex<double> >& inArray);


  // VD<-AF
  void fill(std::vector<std::complex<double> >& outArray, 
                  const std::valarray<std::complex<float> >& inArray);

  template <typename T>
  void fill(std::vector<string>& outArray, const std::vector<T>& inArray, size_t first, size_t last);

  template <typename T>
  void fill(std::vector<T>& outArray, const std::vector<string>& inArray, size_t first, size_t last);

  template <typename S> 
  void fill(std::valarray<S>& outArray, const std::vector<string>& inArray,size_t first, size_t last);

//  template <typename S, typename T>
//  void fill(std::valarray<std::complex<S> >& outArray, const std::valarray<std::complex<T> >& inArray);            
  // seems no other way of doing this.

  // VF<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
  void fillMSvfvf(std::vector<std::complex<float> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);
#endif

  void fill(std::vector<std::complex<float> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);

  // VF<-VD
#ifdef TEMPLATE_AMBIG_DEFECT
    void fillMSvfvd(std::vector<std::complex<float> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);
#endif

   void fill(std::vector<std::complex<float> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);

   // VD<-VD
#ifdef TEMPLATE_AMBIG_DEFECT
 void fillMSvdvd(std::vector<std::complex<double> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);
#endif

   void fill(std::vector<std::complex<double> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);

#ifdef TEMPLATE_AMBIG_DEFECT
  void fillMSvdvf(std::vector<std::complex<double> >& outArray, 
				const std::vector<std::complex<float> >& inArray, 
                        size_t first, size_t last);
#else
  void fill(std::vector<std::complex<double> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);
#endif

  // AF<-VD
  void fill(std::valarray<std::complex<float> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);

  // AF<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
 void fillMSafvf(std::valarray<std::complex<float> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);
#else
 void fill(std::valarray<std::complex<float> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);
#endif

 // AD<-VF
#ifdef TEMPLATE_AMBIG_DEFECT
  void fillMSadvf(std::valarray<std::complex<double> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);
#else
  void fill(std::valarray<std::complex<double> >& outArray, 
                  const std::vector<std::complex<float> >& inArray, size_t first, size_t last);
#endif

  // AD<-VD
#ifdef TEMPLATE_AMBIG_DEFECT
  void fillMSadvd(std::valarray<std::complex<double> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);
#else
  void fill(std::valarray<std::complex<double> >& outArray, 
                  const std::vector<std::complex<double> >& inArray, size_t first, size_t last);
#endif

  // AF<-AF
  void fill(std::valarray<std::complex<float> >& outArray,  
                  const std::valarray<std::complex<float> >& inArray);
  // AD<-AD
  void fill(std::valarray<std::complex<double> >& outArray,  
                  const std::valarray<std::complex<double> >& inArray);
  // AF<-AD
  void fill(std::valarray<std::complex<float> >& outArray, 
                  const std::valarray<std::complex<double> >& inArray);
  // AD<-AF
  void fill(std::valarray<std::complex<double> >& outArray,  
                  const std::valarray<std::complex<float> >& inArray);

#if TEMPLATE_AMBIG_DEFECT || TEMPLATE_AMBIG7_DEFECT
  void fillMSvsvs(std::vector<string>& outArray, const std::vector<string>& inArray, size_t first, size_t last);
#endif


  void fill(std::vector<string>& outArray, const std::vector<string>& inArray, size_t first, size_t last);

  template <typename S, typename T>
  string errorMessage(const S& out, const T& in);



    template <class T>
    struct MatchPtrName : public std::binary_function<T,std::string,bool>  //## Inherits: <unnamed>%39491BC9025D
    {
          //	Parameterized Class MatchPtrName
          bool operator () (const T& left, const string& right) const;

      public:
      protected:
      private:
      private: //## implementation
    };



    template <class T>
    struct MatchName : public std::binary_function<T,std::string,bool>  //## Inherits: <unnamed>%39491BC50121
    {
          bool operator () (const T& left, const string& right) const;

      public:
      protected:
      private:
      private: //## implementation
    };



    template <class T>
    struct MatchNum : public std::binary_function<T,int,bool>  //## Inherits: <unnamed>%39491BCE01C0
    {
          bool operator () (const T& left, const int& right) const;

      public:
      protected:
      private:
      private: //## implementation
    };



    template <typename T>
    struct MatchType 
    {
          ValueType operator () ();

      public:
      protected:
      private:
      private: //## implementation
    };



    template <typename T>
    struct CVarray 
    {
          T* operator () (const std::vector<T>& inArray);

      public:
      protected:
      private:
      private: //## implementation
    };



    template <typename T>
    struct FitsNullValue 
    {
          T operator () ();

      public:
      protected:
      private:
      private: //## implementation
    };



    template <typename T>
    struct MatchImageType 
    {
          ImageType operator () ();

      public:
      protected:
      private:
      private: //## implementation
    };



    template <class T>
    struct MatchPtrNum : public std::binary_function<T,int,bool>  //## Inherits: <unnamed>%39491BD3034B
    {
          bool operator () (const T& left, const int& right) const;

      public:
      protected:
      private:
      private: //## implementation
    };
    //	auto_ptr analogue for arrays.



    template <typename X>
    class auto_array_ptr 
    {
      public:
          explicit auto_array_ptr (X* p = 0) throw ();
          explicit auto_array_ptr (auto_array_ptr<X>& right) throw ();
          ~auto_array_ptr();

          void operator = (auto_array_ptr<X>& right);
          X& operator * () throw ();
          X& operator [] (size_t i) throw ();
          X operator [] (size_t i) const throw ();
          X* get () const;
          X* release () throw ();
          X* reset (X* p) throw ();
          static void remove (X*& x);

      protected:
      private:
      private: //## implementation
        // Data Members for Class Attributes
          X* m_p;

    };



    template <typename T>
    struct ComparePtrIndex : public std::binary_function<T,T,bool>  //## Inherits: <unnamed>%3B24DB930299
    {
          bool operator () (const T* left, const T* right);

      public:
      protected:
      private:
      private: //## implementation
    };



    template <typename T>
    struct CAarray 
    {
          T* operator () (const std::valarray<T>& inArray);

      public:
      protected:
      private:
      private: //## implementation
    };



    template <typename T>
    struct CVAarray 
    {
          T* operator () (const std::vector< std::valarray<T> >& inArray);

      public:
      protected:
      private:
      private: //## implementation
    };



    class UnrecognizedType : public FitsException  //## Inherits: <unnamed>%3CE143AB00C6
    {
      public:
          UnrecognizedType (string diag, bool silent = true);

      protected:
      private:
      private: //## implementation
    };

    // Parameterized Class CCfits::FITSUtil::MatchPtrName 

    template <class T>
    inline bool MatchPtrName<T>::operator () (const T& left, const string& right) const
    {
       return left->name() == right;
    }

    // Parameterized Class CCfits::FITSUtil::MatchName 

    template <class T>
    inline bool MatchName<T>::operator () (const T& left, const string& right) const
    {
       return left.name() == right;
    }

    // Parameterized Class CCfits::FITSUtil::MatchNum 

    template <class T>
    inline bool MatchNum<T>::operator () (const T& left, const int& right) const
    {
    return left.index() == right;
    }

    // Parameterized Class CCfits::FITSUtil::MatchType 

    // Parameterized Class CCfits::FITSUtil::CVarray 

    template <typename T>
    inline T* CVarray<T>::operator () (const std::vector<T>& inArray)
    {

      // convert std containers used commonly in FITS to C arrays in an exception
      // safe manner that is also clear about resource ownership.      
      auto_array_ptr<T> pC(new T[inArray.size()]);
      T* c = pC.get();
      std::copy(inArray.begin(),inArray.end(),&c[0]);
      return pC.release();
    }

    // Parameterized Class CCfits::FITSUtil::FitsNullValue 

    template <typename T>
    inline T FitsNullValue<T>::operator () ()
    {
       // This works for int types.  Float, complex, and string types 
       //   are handled below with specialized templates.
       return 0;
    }

    // Parameterized Class CCfits::FITSUtil::MatchImageType 

    // Parameterized Class CCfits::FITSUtil::MatchPtrNum 

    template <class T>
    inline bool MatchPtrNum<T>::operator () (const T& left, const int& right) const
    {
    return left->index() == right;
    }

    // Parameterized Class CCfits::FITSUtil::auto_array_ptr 

    // Parameterized Class CCfits::FITSUtil::ComparePtrIndex 

    // Parameterized Class CCfits::FITSUtil::CAarray 

    // Parameterized Class CCfits::FITSUtil::CVAarray 

    // Class CCfits::FITSUtil::UnrecognizedType 

    // Parameterized Class CCfits::FITSUtil::MatchPtrName 

    // Parameterized Class CCfits::FITSUtil::MatchName 

    // Parameterized Class CCfits::FITSUtil::MatchNum 

    // Parameterized Class CCfits::FITSUtil::MatchType 

    template <typename T>
    ValueType MatchType<T>::operator () ()
    {

    if ( typeid(T) == typeid(d1) ) return Tdouble;
    if ( typeid(T) == typeid(f1) ) return Tfloat;
    if ( typeid(T) == typeid(c1) ) return Tcomplex;
    if ( typeid(T) == typeid(d2) ) return Tdblcomplex;
    if ( typeid(T) == typeid(s1) ) return Tstring;
    if ( typeid(T) == typeid(i1) ) return Tint;
    if ( typeid(T) == typeid(u1) ) return Tuint;
    if ( typeid(T) == typeid(s2) ) return Tshort;
    if ( typeid(T) == typeid(us1) ) return Tushort;
    if ( typeid(T) == typeid(b1) ) return Tlogical;
    if ( typeid(T) == typeid(b2) ) return Tbyte;
    if ( typeid(T) == typeid(l1) ) return Tlong;
    if ( typeid(T) == typeid(ul1) ) return Tulong;
    // Carefull, on some compilers LONGLONG == long,
    // so this should go after test for long.
    if ( typeid(T) == typeid(ll1) ) return Tlonglong;
    throw UnrecognizedType("Invalid data type for FITS Data I/O\n");    
    }

    // Parameterized Class CCfits::FITSUtil::CVarray 

    // Parameterized Class CCfits::FITSUtil::MatchImageType 

    template <typename T>
    ImageType MatchImageType<T>::operator () ()
    {
    if ( typeid(T) == typeid(b2) ) return Ibyte;    
    if ( typeid(T) == typeid(s2) ) return Ishort;
    if ( typeid(T) == typeid(l1) ) return Ilong;
    if ( typeid(T) == typeid(f1) ) return Ifloat;
    if ( typeid(T) == typeid(d1) ) return Idouble;
    if ( typeid(T) == typeid(us1) ) return Iushort;
    if ( typeid(T) == typeid(ul1) ) return Iulong;
    if ( typeid(T) == typeid(ll1) ) return Ilonglong;
    MatchType<T> errType;
    string diag ("Image: ");
    diag += FITSType2String(errType());
    throw UnrecognizedType(diag);        
    }

    // Parameterized Class CCfits::FITSUtil::MatchPtrNum 

    // Parameterized Class CCfits::FITSUtil::auto_array_ptr 

    template <typename X>
    auto_array_ptr<X>::auto_array_ptr (X* p) throw ()
          : m_p(p)
    {
    }

    template <typename X>
    auto_array_ptr<X>::auto_array_ptr (auto_array_ptr<X>& right) throw ()
          : m_p(right.release())
    {
    }


    template <typename X>
    auto_array_ptr<X>::~auto_array_ptr()
    {
      delete [] m_p;
    }


    template <typename X>
    void auto_array_ptr<X>::operator = (auto_array_ptr<X>& right)
    {
      if (this != &right)
      {
                remove(m_p);
                m_p = right.release();       
      }
    }

    template <typename X>
    X& auto_array_ptr<X>::operator * () throw ()
    {
      return *m_p;
    }

    template <typename X>
    X& auto_array_ptr<X>::operator [] (size_t i) throw ()
    {
      return m_p[i];
    }

    template <typename X>
    X auto_array_ptr<X>::operator [] (size_t i) const throw ()
    {
      return m_p[i];
    }

    template <typename X>
    X* auto_array_ptr<X>::get () const
    {
      return m_p;
    }

    template <typename X>
    X* auto_array_ptr<X>::release () throw ()
    {
      return reset(0);
    }

    template <typename X>
    X* auto_array_ptr<X>::reset (X* p) throw ()
    {
      // set the auto_ptr to manage p and return the old pointer it was managing.
      X* __tmp = m_p; 
      m_p = p;
      return __tmp;
    }

    template <typename X>
    void auto_array_ptr<X>::remove (X*& x)
    {
      X* __tmp(x);
      x = 0;
      delete [] __tmp;
    }

    // Parameterized Class CCfits::FITSUtil::ComparePtrIndex 

    template <typename T>
    bool ComparePtrIndex<T>::operator () (const T* left, const T* right)
    {
      return (left->index() < right->index());
    }

    // Parameterized Class CCfits::FITSUtil::CAarray 

    template <typename T>
    T* CAarray<T>::operator () (const std::valarray<T>& inArray)
    {
      size_t n(inArray.size());
      auto_array_ptr<T> pC(new T[n]);
      T* c= pC.get();
      for (size_t j = 0; j < n; ++j) c[j] = inArray[j];
      return pC.release();      
    }

    // Parameterized Class CCfits::FITSUtil::CVAarray 

    template <typename T>
    T* CVAarray<T>::operator () (const std::vector< std::valarray<T> >& inArray)
    {
      size_t  sz(0);
      size_t  n(inArray.size());

      std::vector<size_t> nr(n);

      size_t i = 0; // for MS VC++ bug
      for ( i = 0; i < n; ++i)
      {
         nr[i] = inArray[i].size();
	 sz += nr[i];

      }
      auto_array_ptr<T> pC(new T[sz]);
      T* c = pC.get();

      size_t k(0);
      for ( i = 0; i < n; ++i)
      {
         size_t& m = nr[i];
         const std::valarray<T>& current = inArray[i];
	 for (size_t j=0; j < m ; ++j) c[k++] = current[j];
      }

      return pC.release();      
    }

  } // namespace FITSUtil
} // namespace CCfits

namespace CCfits
{

   namespace FITSUtil
   {                

      template <typename T>
      void swap(T& left, T& right)
      {
              T temp(left);
              left = right;
              right = temp;                
      }

      template <typename T>
      void swap(std::vector<T>& left, std::vector<T>& right)
      {
              left.swap(right);        
      }

      template <>
      inline string FitsNullValue<string>::operator () ()
      {
	 return string("");
      }
      
      template <>
      inline float FitsNullValue<float>::operator () ()
      {
         return FLOATNULLVALUE;
      }

      template <>
      inline double FitsNullValue<double>::operator () ()
      {
         return DOUBLENULLVALUE;
      }
      
      template <>
      inline std::complex<float> FitsNullValue<std::complex<float> >::operator () ()
      {
         return std::complex<float>(FLOATNULLVALUE);
      }

      template <>
      inline std::complex<double> FitsNullValue<std::complex<double> >::operator () ()
      {
         return std::complex<double>(DOUBLENULLVALUE);
      }
      
   } // end namespace FITSUtil
} // end namespace CCfits



#endif
