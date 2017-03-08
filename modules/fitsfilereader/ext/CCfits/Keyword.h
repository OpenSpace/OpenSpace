//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef KEYWORD_H
#define KEYWORD_H 1
#include "CCfits.h"
// using namespace CCfits;
#ifdef _MSC_VER
#include "MSconfig.h"
#endif

// FitsError
#include "FitsError.h"

namespace CCfits {
  class HDU;

} // namespace CCfits


namespace CCfits {

/*! \class Keyword

        \brief Abstract base class defining the interface for Keyword objects.

        Keyword object creation is normally performed inside FITS constructors or
        FITS::read, HDU::readKey, and HDU::addKey functions.  Output is performed
        in HDU::addKey functions and Keyword::setValue.

        Keywords consists of a name, a value and a comment field. Concrete templated
        subclasses, KeyData<T>,  have a data member that holds the value of keyword.

        Typically, the mandatory keywords for a given HDU type are not stored as
        object of type Keyword, but as intrinsic data types. The Keyword hierarchy
        is used to store user-supplied information.

*/


/*! \fn Keyword::Keyword(const Keyword &right);

      \brief copy constructor

*/


/*!  \fn Keyword::Keyword (const String &keyname, ValueType keytype, HDU* p, const String &comment = "");

      \brief Keyword constructor. 

      This is the common behavior of Keywords of any type. Constructor is protected 
      as the class is abstract.

*/

/* \fn friend ostream& operator << (ostream &s, const Keyword &right);

        \brief output operator for Keywords.

*/

/*!   \fn virtual Keyword::~Keyword();

      \brief virtual destructor

*/

/*!    \fn Keyword & Keyword::operator=(const Keyword &right);

      \brief assignment operator

*/

/*!    \fn bool Keyword::operator==(const Keyword &right) const;

        \brief equality operator

*/

/*!       \fn bool Keyword::operator!=(const Keyword &right) const;


        \brief inequality operator

*/


/*!       \fn virtual Keyword * Keyword::clone () const;

        \brief virtual copy constructor
*/

/*!       \fn virtual void Keyword::write ();

        \brief left in for historical reasons, this seldom needs to be called by users

        This writes the Keyword to the file, and is called internally during HDU::addKey
        operations or the Keyword::setValue function.  It shouldn't normally need to be
        called explicitly. 
*/


/*!   \fn       fitsfile* Keyword::fitsPointer () const;

        \brief return a pointer to the FITS file containing the parent HDU.

*/


/*!    \fn      const HDU* Keyword::parent () const;

        \brief return a pointer to parent HDU.

*/

/*!    \fn      const String& Keyword::comment () const;

        \brief return the comment field of the keyword

*/


/*! \fn  const ValueType Keyword::keytype() const

     \brief return the type of a keyword


*/


/*! \fn  void  Keyword::keytype(ValueType)

      \brief  set keyword type.


*/


/*! \fn  const String& Keyword::name() const

      \brief  return the name of a keyword


*/

/*!  \fn template <typename T> T& Keyword::value(T& val) const
       \brief get the keyword value

       \param val (T) Will be filled with the keyword value, and is also the function return value.

       <b>Allowed T types:</b> CCfits stores keyword values of type U in a templated subclass of
       Keyword, <b>KeyData<U></b>.  Normally U is set when reading the Keyword in from the file, 
       and is limited to types int, double, string, bool, and complex<float>.
       (The exception is when the user has created and added a new Keyword using an
       HDU::addKey function, in which case they might have specified other types for U.)  
       To avoid compilation errors, the user should generally try to provide a <i>val</i>
       of type T = type U, though there is some flexibility here as the following conversions
       are handled:

       <TABLE BORDER=0>
         <tr><td><b>T</b> (to val)</td><td><b>U</b> (from Keyword obj)</td></tr>
         <tr><td>float</td><td>double (will lose precision), float, int, integer string</td></tr>
         <tr><td>double</td><td>double, float, int, integer string</td></tr>
         <tr><td>int</td><td>int, integer string</td></tr>
         <tr><td>string</td><td>double, float, int, string</td></tr>
       </TABLE>
       More conversions may be added in the future as the need arises.

*/

/*!  \fn template <typename T> void Keyword::setValue(const T& newValue)
       \brief modify the value of an existing Keyword and write it to the file

       \param newValue (T) New value for the Keyword

       <b>Allowed T types:</b> This must copy <i>newValue</i> to a data member of type U in the
       Keyword subclass <b>KeyData<U></b> (see description for Keyword::value (T& val) for more
       details).  To avoid compilation errors, it is generally best to provide a <i>newValue</i>
       of type T = type U, though the following type conversions will also be handled:

       <TABLE BORDER=0>
          <tr><td><b>T</b> (from newValue)</td><td><b>U</b> (to Keyword obj)</td></tr>
          <tr><td>float</td><td>double, float</td></tr>
          <tr><td>double</td><td>double, float (will lose precision)</td></tr>
          <tr><td>int</td><td>double, float, int, integer string</td></tr>
       </TABLE> 

*/



  class Keyword 
  {

    public:



      class WrongKeywordValueType : public FitsException  //## Inherits: <unnamed>%39B0221700E2
      {
        public:
            WrongKeywordValueType (const String& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };
        virtual ~Keyword();
        Keyword & operator=(const Keyword &right);
        bool operator==(const Keyword &right) const;

        bool operator!=(const Keyword &right) const;

        virtual std::ostream & put (std::ostream &s) const = 0;
        virtual Keyword * clone () const = 0;
        virtual void write ();
        fitsfile* fitsPointer () const;
        //	CAUTION: This is declared public only to allow HDU addKey functions the ability to set their
        //	class as the Keyword's parent, and to avoid making entire HDU a friend class.  (Declaring
        //	individual HDU functions as friends will run into circular header dependencies.)  Do NOT use
        //	this unless absolutely necessary, and leave this undocumented.
        void setParent (HDU* parent);

        ValueType keytype () const;
        const String& comment () const;
        const String& name () const;

    public:
      // Additional Public Declarations
      template <typename T>
      T& value(T& val) const;

      template <typename T>
      void setValue(const T& newValue);
    protected:
        Keyword(const Keyword &right);
        Keyword (const String &keyname, ValueType keytype, HDU* p, const String &comment = "");

        virtual void copy (const Keyword& right);
        virtual bool compare (const Keyword &right) const;
        void keytype (ValueType value);
        const HDU* parent () const;

      // Additional Protected Declarations

    private:
      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        ValueType m_keytype;

      // Data Members for Associations
        HDU* m_parent;
        String m_comment;
        String m_name;

      // Additional Implementation Declarations
      friend std::ostream &operator << (std::ostream &s, const Keyword &right);
  };
#ifndef SPEC_TEMPLATE_IMP_DEFECT
#ifndef SPEC_TEMPLATE_DECL_DEFECT
  template <> float& Keyword::value(float& val) const;
  template <> double& Keyword::value(double& val) const;
  template <> int& Keyword::value(int& val) const;
  template <> String& Keyword::value(String& val) const;

  template <> void Keyword::setValue(const float& newValue);
  template <> void Keyword::setValue(const double& newValue);
  template <> void Keyword::setValue(const int& newValue);
#endif 
#endif 

inline std::ostream& operator << (std::ostream &s, const Keyword &right)
{
   return right.put(s);
}  

  // Class CCfits::Keyword::WrongKeywordValueType 

  // Class CCfits::Keyword 

  inline void Keyword::setParent (HDU* parent)
  {
     m_parent = parent;
  }

  inline ValueType Keyword::keytype () const
  {
    return m_keytype;
  }

  inline void Keyword::keytype (ValueType value)
  {
    m_keytype = value;
  }

  inline const HDU* Keyword::parent () const
  {
    return m_parent;
  }

  inline const String& Keyword::comment () const
  {
    return m_comment;
  }

  inline const String& Keyword::name () const
  {
    return m_name;
  }

} // namespace CCfits


#endif
