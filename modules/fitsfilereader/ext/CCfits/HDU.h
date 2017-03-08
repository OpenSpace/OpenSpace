//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef HDU_H
#define HDU_H 1
#include <map>

// vector
#include <vector>
#include <list>
// CCfitsHeader
#include "CCfits.h"
// Keyword
#include "Keyword.h"
// NewKeyword
#include "NewKeyword.h"
// FitsError
#include "FitsError.h"
// FITSUtil
#include "FITSUtil.h"

namespace CCfits {
  class FITS;
  class FITSBase;

} // namespace CCfits
namespace CCfits {
   class HDUCreator; // Needed for friend declaration
}

#ifdef _MSC_VER
#include "MSconfig.h" // for truncation warning
#endif
#include "KeywordT.h"


namespace CCfits {
        /*! \class HDU

                \brief Base class for all HDU [Header-Data Unit] objects.

                HDU objects in CCfits are either PHDU (Primary HDU objects) or	
		ExtHDU (Extension HDU) objects. Following the behavior.
ExtHDUs are further subclassed into ImageExt or Table objects, which are
finally AsciiTable or BinTable objects.

		HDU's public interface gives access to properties that are
common to all HDUs, largely required keywords, and functions that  are common
to all HDUs, principally the manipulation of keywords and their values.

		HDUs must be constructed by HDUCreator objects which are
called by FITS methods. Each HDU has an embedded pointer to a FITSBase
object, which is private to FITS [FITSBase is a pointer encapsulating the
resources of FITS. For details of this coding idiom see Exceptional C++
by Herb Sutter (2000) and references therein].


        */


        /*! \class HDU::InvalidExtensionType

		@ingroup FITSexcept
                @brief exception to be thrown if user requests extension type that can not be understood as 
                ImageExt, AsciiTable or BinTable.
        */


/*! \fn HDU::InvalidExtensionType::InvalidExtensionType (const string& diag, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: Extension Type: " before the specific message.

        \param diag A specific diagnostic message
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        

        /*! \class HDU::InvalidImageDataType

		@ingroup FITSexcept
                @brief exception to be thrown if user requests creation of an image of type not
                supported by cfitsio.
        */

/*! \fn HDU::InvalidImageDataType::InvalidImageDataType (const string& diag, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: Invalid Data Type for Image " before the specific message.

        \param diag A specific diagnostic message
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        

/*! \class HDU::NoSuchKeyword

		@ingroup FITSexcept
                @brief exception to be thrown on seek errors for keywords.
*/

/*! \fn HDU::NoSuchKeyword::NoSuchKeyword (const string& diag, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: Keyword not found:  " before the specific message.

        \param diag A specific diagnostic message, usually the name of the keyword requested.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        

/*! \class HDU::NoNullValue

		@ingroup FITSexcept
                @brief exception to be thrown on seek errors for keywords.
*/

/*! \fn HDU::NoNullValue::NoNullValue (const string& diag, bool silent);

        \brief Exception ctor, prefixes the string "Fits Error: No Null Pixel Value specified for Image " before the specific message.

        \param diag A specific diagnostic message, the name of the HDU if not the primary.
        \param silent if true, print message whether FITS::verboseMode is set or not.

*/        

        /*! \fn      HDU::HDU(const HDU &right);

        \brief copy constructor

        */

        /*! \fn       virtual HDU::~HDU();

        \brief destructor

        */


        /*! \fn       bool HDU::operator==(const HDU &right) const;

        \brief equality operator

        */

        /*! \fn       bool HDU::operator!=(const HDU &right) const;

        \brief inequality operator

        */


        /*! \fn  const String& HDU::getHistory();
                \brief read the history information from the HDU and add it to the FITS object.

                The history string found in the header is concatenated and returned
                to the calling function
        */                

         /*! \fn  const String& HDU::getComments();
                \brief read the comments from the HDU and add it to the FITS object.

                The comment string found in the header is concatenated and returned
                to the calling function

        */   

        /*! \fn void HDU::readAllKeys();
                \brief read all of the keys in the header

                This member function reads keys that are not meta data for
                columns or image information, [which are considered to be
                part of the column or image objects]. Also, history and 
                comment keys are read and returned by getHistory() and getComment().
                The exact list of keyword classes this will read is returned by 
                the function keywordCategories().  

                Note that readAllKeys can only construct keys of type
                string, double, complex<float>, integer, and bool because
                the FITS header records do not encode exact type information.



        */

        /*! \fn void HDU::copyAllKeys (const HDU* inHdu);
              \brief copy all keys from another header

              Parameters:
              \param inHdu (const HDU*) An existing HDU whose keys will be copied.

              This will copy all keys that exist in the keyWord map
              of <i>inHDU</i>, and which belong to one of the keyword classes
              returned by the keywordCategories() function.  This is the same
              group of keyword classes used by readAllKeys().  

        */

        /*! \fn Keyword& HDU::readNextKey(const std::vector<String>& incList, const std::vector<String>& excList, bool searchFromBeginning);
              \brief Read the next key in the HDU which matches a string in incList, and does not match string in excList

              Parameters:
              \param incList Vector of strings specifying keyword names to search.
              \param excList Vector of strings specifying names to exclude from search.  This may be left empty.
              \param searchFromBeginning If 'true', search will be conducted from the start of the HDU.  Otherwise it starts from the current position.

              This is a wrapper around the CFITSIO fits_find_nextkey function.  It reads in and returns the next keyword whose
              name matches matches one of the strings in incList, which may contain wild card characters (*,?, and #). It will
              exclude keywords whose name matches a string in excList.  If no keyword is found, a FitError is thrown.
              
              By default the search is conducted from the current keyword position in the HDU.  If searchFromBeginning 
              is set to 'true', search will start from the beginning of the HDU. If HDU is not the currently open extension, 
              this will make it so and start the keyword search from the beginning. 
        */

        /*! \fn  static std::vector<int> HDU::keywordCategories ();
              \brief return the enumerated keyword categories used by readAllKeys() and copyAllKeys()

              This returns a vector of integers indicating which categories of keywords
              apply for the readAllKeys and copyAllKeys functions.  The list of categories
              currently hardcoded is:  TYP_CMPRS_KEY (20), TYP_CKSUM_KEY (100),
              TYP_WCS_KEY (110), TYP_REFSYS_KEY (120), and TYP_USER_KEY (150).

              For the list of ALL keyword categories, see the CFITSIO documentation for
              the fits_get_keyclass function.
        */

         /*! \fn void HDU::writeComment(const String& comment = "Generic Comment");                      
                \brief write a comment string. 

                A default value for the string is given ("Generic Comment String") 
                so users can put a placeholder call to this function in their code.
        */

        /*! \fn  void HDU::writeHistory (const String& history = "Generic History String");
                \brief write a history string. 

                A default value for the string is given ("Generic History String") 
                so users can put a placeholder call to this function in their code.
        */            

        /*! \fn void HDU::writeDate();
                \brief write a date string to *this.

        */ 

         /*! \fn void HDU::deleteKey(const String& doomed)
                \brief delete a keyword from the header 


                removes <i>doomed</i> from the FITS file and from the FITS object

         */

         /*! \fn const String& HDU::history() const;
                \brief return the history string previously read by getHistory()

        */  

        /*! \fn const String& HDU::comment() const;
                \brief return the comment string previously read by getComment()

        */  


        /*! \fn   template <typename T> void HDU::readKey(const String& keyName, T& val);


        \brief read a  keyword of specified type from the header of a disk  FITS file
and return its value. 

        T is one of the types String, double, float, int, std::complex<float>, and bool.
        If a Keyword object with the name <i>keyName</i> already exists in this HDU due to a previous
        read call, then this will re-read from the file and create a new Keyword object to replace
        the existing one.

        */

        /*! \fn  template <typename T> void HDU::readKeys(std::vector<String>& keyNames, std::vector<T>& vals);

        \brief read a set of specified keywords of the same data type
		from the header of a disk  FITS file and return their values

        T is one of the types String, double, float, int, std::complex<float>, and bool.


        */


        /*! \fn       virtual HDU * HDU::clone (FITSBase* p) const = 0;

        \brief virtual copy constructor, to be implemented in subclasses.

        */


        /*! \fn       void HDU::makeThisCurrent () const;

              \brief move the fitsfile pointer to this current HDU. 

		This function should never need to be called by the user
		since it is called internally whenever required.

        */


        /*! \fn       Keyword& HDU::keyWord (const String& keyName);

        \brief  return a (previously read) keyword from the HDU object.

        */


        /*! \fn       int HDU::index () const;

        \brief return the HDU number

        */

        /*! \fn       void HDU::index (int value);

        \brief set the HDU number

        */


        /*! \fn       const std::map<string,Keyword*>& HDU::keyWord () const;

        \brief  return the associative array containing the HDU Keywords that have been read so far.

        */

        /*! \fn      const Keyword& HDU::keyWord (const string& keyname) const;

        \brief return a (previously read) keyword from the HDU object. const version

        */

        /*! \fn       HDU::HDU (FITSBase* p = 0);

        \brief default constructor, called by HDU subclasses that read from FITS files.

        */

        /*! \fn       HDU::HDU (FITSBase* p, int bitpix,int naxis, const std::vector<long>& axes);

        \brief constructor for creating new HDU objects, called by HDU subclasses writing to FITS files.

        */



        /*! \fn       virtual bool HDU::compare (const HDU &right) const;

        \brief internal implementation of equality operations.

        */


        /*! \fn       fitsfile* HDU::fitsPointer () const;

        \brief return the fitsfile pointer for the FITS object containing the HDU

        */

        /*! \fn       std::map<String, Keyword*>& HDU::keyWord ();

        \brief return the associative  array containing the HDU keywords so far read.

        */



        /*! \fn       long HDU::axis (size_t index) const;

        \brief return the size of axis numbered index [zero based].

        */

        /*! \fn       void HDU::axes () const;

        \brief  return the number of axes in the HDU data section (always 2 for tables).

        */


        /*! \fn       const long HDU::bitpix () const;

        \brief return the data type keyword. 

        Takes values denoting the image data type for images, and takes the fixed value 8 for
        tables.

        */

        /*! \fn       const FITSBase *& HDU::parent () const;

        \brief return reference to the pointer representing the FITSBase object containing the HDU

        */



        /*! \fn       std::vector< long >& HDU::naxes ();

        \brief return the HDU data axis array.


        */


        /*! \fn       long HDU::axis (size_t i) const;

                \brief return the length of HDU data axis i.

        */


        /*! \fn   virtual double HDU::scale () const;

                \brief return the BSCALE keyword value

        */

        /*! \fn   virtual void HDU::scale (double value);
              \brief set the BSCALE keyword value for images (see warning for images of int type)

              For primary HDUs and image extensions, this will add (or update)
              the BSCALE keyword in the header.  The new setting will affect future 
              image array read/writes as described in section 4.7 Data Scaling of the CFITSIO manual.
              For table extensions this function does nothing. 

              WARNING: If the image contains <b>integer-type data</b> (as indicated by the bitpix() return value),
              the new scale and zero value combination must not be such that the scaled data would require a
              floating-point type (this uses the CFITSIO function fits_get_img_equivtype to make the
              determination).  If this situation occurs, the function will throw a FitsException.

        */   

        /*! \fn   virtual double HDU::zero () const;

                \brief return the BZERO keyword value

        */

        /*! \fn   virtual double HDU::zero (double value);
             \brief set the BZERO keyword value for images (see warning for images of int type)

              For primary HDUs and image extensions, this will add (or update)
              the BZERO keyword in the header.  The new setting will affect future 
              image array read/writes as described in section 4.7 Data Scaling of the CFITSIO manual.
              For table extensions this function does nothing.  

              WARNING: If the image contains <b>integer-type data</b> (as indicated by the bitpix() return value),
              the new scale and zero value combination must not be such that the scaled data would require a
              floating-point type (this uses the CFITSIO function fits_get_img_equivtype to make the
              determination).  If this situation occurs, the function will throw a FitsException.              

        */
        
        /*! \fn virtual void HDU::resetImageRead ();
              \brief force next image reading operation to read from file instead of object cache.
              
              [Note: It is not necessary to call this function for normal image reading operations.]
              For primary HDUs and image extensions, this forces the next read operation
              to retrieve data from the file regardless of whether the data has
              already been read and stored in the HDU's internal arrays.  This does nothing
              if the HDU does not contain an image.
        
        */

         /*! \fn void HDU::suppressScaling (bool toggle = true);
             \brief turn off image scaling regardless of the BSCALE and BZERO keyword values

             For <i>toggle</i> = true, this turns off image scaling for future read/writes
             by resetting the scale and zero to 1.0 and 0.0 respectively.  It does NOT
             modify the BSCALE and BZERO keywords.  If <i>toggle</i> = false, the
             scale and zero values will be restored to the keyword values.

         */


        /*! \fn       template <typename T> Keyword& HDU::addKey(const String& name, T value,  const String& comment) ;

                \brief create a new keyword in the HDU with specified value and comment fields

                The function returns a reference to keyword object just created.  If a keyword
                with this name already exists, it will be overwritten.  Note that this is mostly intended
                for adding user-defined keywords.  It should not be used to add keywords for which there
                are already specific HDU functions, such as scaling or checksum. Nor should it be used for
                image or column structural keywords, such as BITPIX, NAXIS, TFORMn, etc.  As a general rule,
                it is best to use this for keywords belonging to the same categories listed in the
                keywordCategories() function.

                Parameters:
                \param name (String) The keyword name
                \param value (Recommended T = String, double, std::complex<float>, int, or bool
                \param comment (String) the keyword value

                It is possible to create a keyword with a value of any of the allowed
                data types in fitsio (see the cfitsio manual section 4.3).  However one should
                be aware that if this keyword value is read in from the file at a later time, it will 
                be stored in a templated Keyword subclass (KeyData<T>) where T will be one of
                the recommended types listed above.  Also see Keyword::value (T& val) for
                more details.                               

        */

        /*!  \fn Keyword* HDU::addKey(const Keyword* inKeyword);
            \brief create a copy of an existing Keyword and add to HDU

            This is particularly useful for copying Keywords from one HDU
            to another.  For example the inKeyword pointer might come from
            a different HDU's std::map<string,Keyword*>.  If a keyword with this
            name already exists, it will be overwritten. The return value
            is a pointer to the newly created Keyword inserted into this HDU.
            Also see copyAllKeys().
        */

        /*! \fn std::ostream& operator << (std::ostream& s, const CCfits::HDU& right);

                \brief Output operator for HDU objects.  Primarily for testing purposes.

        */  

        /*! \fn   void HDU::writeChecksum ();
                \brief compute and write the DATASUM and CHECKSUM keyword values

                Wrapper for the CFITSIO function fits_write_chksum: This performs the datasum
                and checksum calculations for this HDU, as described in the CFITSIO manual.  
                If either the DATASUM or CHECKSUM keywords already exist, their values will be updated.
        */

        /*! \fn   void HDU::updateChecksum ();
                \brief update the CHECKSUM keyword value, assuming DATASUM exists and is correct

                Wrapper for the CFITSIO function fits_update_chksum: This recomputes and writes
                the CHECKSUM value with the assumption that the DATASUM value is correct.  If the 
                DATASUM keyword doesn't yet exist or is not up-to-date, use the HDU::writeChecksum 
                function instead.  This will throw a FitsError exception if called when there is 
                no DATASUM keyword in the header.
        */

        /*! \fn   std::pair<int,int> HDU::verifyChecksum () const;
                \brief verify the HDU by computing the checksums and comparing them with the 
                   CHECKSUM/DATASUM keywords

                Wrapper for the CFITSIO function fits_verify_chksum: The data unit is verified
                correctly if the computed checksum equals the DATASUM keyword value, and the
                HDU is verified if the entire checksum equals zero (see the CFITSIO manual for
                further details).  

                This returns a std::pair<int,int> where the pair's  first data member = DATAOK 
                and second = HDUOK.  DATAOK and HDUOK values will be = 1 if verified correctly,
                0 if the keyword is missing, and -1 if the computed checksum is not correct.
        */

        /*! \fn   std::pair<unsigned long,unsigned long> HDU::getChecksum () const;
                \brief compute and return the checksum values for the HDU without creating or
                   modifying the CHECKSUM/DATASUM keywords.

                Wrapper for the CFITSIO function fits_get_chksum: This returns a 
                std::pair<unsigned long, unsigned long> where the pair's first data member 
                holds the datasum value and second holds the hdusum value.  
        */



  class HDU 
  {

    public:



      class InvalidImageDataType : public FitsException  //## Inherits: <unnamed>%394FBA12005C
      {
        public:
            InvalidImageDataType (const string& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class InvalidExtensionType : public FitsException  //## Inherits: <unnamed>%3964C1D00352
      {
        public:
            InvalidExtensionType (const string& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class NoSuchKeyword : public FitsException  //## Inherits: <unnamed>%398865D10264
      {
        public:
            NoSuchKeyword (const string& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };



      class NoNullValue : public FitsException  //## Inherits: <unnamed>%3B0D58CE0306
      {
        public:
            NoNullValue (const string& diag, bool silent = true);

        protected:
        private:
        private: //## implementation
      };
        HDU(const HDU &right);
        bool operator==(const HDU &right) const;

        bool operator!=(const HDU &right) const;

        virtual HDU * clone (FITSBase* p) const = 0;
        fitsfile* fitsPointer () const;
        FITSBase* parent () const;
        //	By all means necessary, set the fitsfile pointer so that
        //	this HDU is the current HDU.
        //
        //	This would appear to be a good candidate for the public
        //	interface.
        virtual void makeThisCurrent () const;
        const String& getComments ();
        const string& comment () const;
        //	Write a history string. A default value for the string is given
        //	"GenericComment" so users can put a placeholder call
        //	to this function in their code before knowing quite what should go in it.
        void writeComment (const String& comment = "Generic Comment");
        const String& getHistory ();
        const string& history () const;
        //	Write a history string. A default value for the string is given
        //	"Generic History String" so users can put a placeholder call
        //	to this function in their code before knowing quite what should go in it.
        void writeHistory (const String& history = "Generic History String");
        //	Write a date card.
        void writeDate ();
        friend std::ostream& operator << (std::ostream& s, const CCfits::HDU& right);
        long axes () const;
        long axis (size_t index) const;
        void index (int value);
        int index () const;
        long bitpix () const;
        virtual double scale () const;
        virtual void scale (double value);
        virtual double zero () const;
        virtual void zero (double value);
        virtual void resetImageRead ();
        virtual void suppressScaling (bool toggle = true);
        void writeChecksum ();
        void updateChecksum ();
        std::pair<int,int> verifyChecksum () const;
        std::pair<unsigned long,unsigned long> getChecksum () const;
        void deleteKey (const String& doomed);
        void readAllKeys ();
        void copyAllKeys (const HDU* inHdu);
        std::map<String, Keyword*>& keyWord ();
        Keyword& keyWord (const String& keyName);
        static std::vector<int> keywordCategories ();
        const std::map<string,Keyword*>& keyWord () const;
        const Keyword& keyWord (const string& keyname) const;
        Keyword& readNextKey(const std::vector<String>& incList,
                             const std::vector<String>& excList,
                             bool searchFromBeginning = false);

    public:
      // Additional Public Declarations
      template <typename T>
      void readKey(const String& keyName, T& val);

      template <typename T>
      void readKeys(std::vector<String>& keyNames, std::vector<T>& vals);

      template <typename T>
      Keyword& addKey(const String& name, T val,  const String& comment);

      // This non-template function could be entered with Rose, but
      // it's instead placed with the other addKey function to
      // simplify the Doxygen generated doc file output.
      Keyword* addKey(const Keyword* inKeyword);

      Keyword& addKey(const String& name, const char* charString, const String& comment);

#ifdef TEMPLATE_AMBIG_DEFECT
      inline void readKeyMS(const String& keyName, int & val);
      inline void readKeys(std::vector<String>& keyNames, std::vector<String>& vals);

#endif
    protected:
        //	Functions as the default constructor, which is required for
        //	the map container class.
        HDU (FITSBase* p = 0);
        HDU (FITSBase* p, int bitpix, int naxis, const std::vector<long>& axes);
        virtual ~HDU();

        Keyword& readKeyword (const String &keyname);
        void readKeywords (std::list<String>& keynames);
        virtual std::ostream & put (std::ostream &s) const = 0;
        void bitpix (long value);
        bool checkImgDataTypeChange (double zero, double scale) const;
        long& naxis ();
        void naxis (const long& value);
        //	Flags whether there were any null values found in the
        //	last read operation.
        bool& anynul ();
        void anynul (const bool& value);
        FITSBase*& parent ();
        std::vector< long >& naxes ();
        long& naxes (size_t index);
        void naxes (size_t index, const long& value);

      // Additional Protected Declarations

    private:
        //	clear the FITS Keyword map. To be called by
        //	the dtor and the copy/assignment operations.
        void clearKeys ();
        virtual void initRead () = 0;
        void readHduInfo ();
        Keyword* addKeyword (Keyword* newKey);
        virtual bool compare (const HDU &right) const;
        //	clear the FITS Keyword map. To be called by
        //	the dtor and the copy/assignment operations.
        void copyKeys (const HDU& right);
        String getNamedLines (const String& name);
        //	save keyword found by read all keys into the array of keywords that have been read.
        //	Similar to addKeyword except there's no write and no returned value. For use by readAllKeys()
        void saveReadKeyword (Keyword* newKey);
        void zeroInit (double value);
        void scaleInit (double value);

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        long m_naxis;
        long m_bitpix;
        int m_index;
        bool m_anynul;
        string m_history;
        string m_comment;
        double m_zero;
        //	Floating point scale factor for image data that takes
        //	the value of the BSCALE parameter.
        double m_scale;

      // Data Members for Associations
        std::map<string,Keyword*> m_keyWord;
        FITSBase* m_parent;
        std::vector< long > m_naxes;

      // Additional Implementation Declarations
        static const size_t s_nCategories;
        static const int s_iKeywordCategories[];

      friend class HDUCreator;        
      friend Keyword* KeywordCreator::getKeyword(const String& keyname, HDU* p);
      friend Keyword* KeywordCreator::getKeyword(const String& keyname, ValueType keyType, HDU* p);
  };
  template <typename T>
  Keyword& HDU::addKey(const String& name, T value,  const String& comment)
  {
              makeThisCurrent();
              NewKeyword<T> keyCreator(this,value);
              Keyword& newKey = *(addKeyword(keyCreator.createKeyword(name,comment)));
              return newKey;
  } 

  template <typename T>
  void HDU::readKey(const String& keyName, T& val)
  {
          makeThisCurrent();
          Keyword& key = readKeyword(keyName);
          key.value(val);
  }


  template <typename T>
  void HDU::readKeys(std::vector<String>& keyNames, std::vector<T>& vals)
  {
          size_t nRead = keyNames.size();

          std::list<String> valKeys;
          std::list<T>      valList;
          for (size_t i = 0; i < nRead; i++) valKeys.push_back(keyNames[i]);
          // read all the keys requested, rejecting those that don't exist.

          readKeywords(valKeys);

          // get the values of all of the requested keys, rejecting those of the
          // wrong type.

          T current;
          std::list<String>::iterator it = valKeys.begin(); 
          while (it != valKeys.end())
          {
                  try
                  {
                          m_keyWord[*it]->value(current);
                          valList.push_back(current);       
			  ++it;
                  }
                  catch ( Keyword::WrongKeywordValueType )
                  {
                          it = valKeys.erase(it);                         
                  }
          }

          keyNames.erase(keyNames.begin(),keyNames.end());       

          if (!valList.empty())
          {
                  if (valList.size() != vals.size()) vals.resize(valList.size());

                  size_t i=0;
                  for (typename std::list<T>::const_iterator it1 
                                  = valList.begin(); it1 != valList.end(); ++it1,++i)
                  {
                          vals[i] = *it1;
                  }
                  for (std::list<String>::const_iterator it1= valKeys.begin(); it1 != valKeys.end(); ++it1)
                  {
                          keyNames.push_back(*it1);
                  }
          }

  }

  // Class CCfits::HDU::InvalidImageDataType 

  // Class CCfits::HDU::InvalidExtensionType 

  // Class CCfits::HDU::NoSuchKeyword 

  // Class CCfits::HDU::NoNullValue 

  // Class CCfits::HDU 

  inline const string& HDU::comment () const
  {
    return m_comment;
  }

  inline const string& HDU::history () const
  {
    return m_history;
  }

  inline std::ostream& operator << (std::ostream& s, const CCfits::HDU& right)
  {
     return right.put(s);
  }

  inline long HDU::axes () const
  {

    return m_naxis;
  }

  inline long HDU::axis (size_t index) const
  {

    return m_naxes[index];
  }

  inline void HDU::index (int value)
  {

    m_index = value;
  }

  inline int HDU::index () const
  {
    return m_index;
  }

  inline long HDU::bitpix () const
  {
    return m_bitpix;
  }

  inline void HDU::bitpix (long value)
  {
    m_bitpix = value;
  }

  inline double HDU::scale () const
  {
    return m_scale;
  }

  inline void HDU::scale (double value)
  {
    m_scale = value;
  }

  inline double HDU::zero () const
  {
    return m_zero;
  }

  inline void HDU::zero (double value)
  {
    m_zero = value;
  }
  
  inline void HDU::resetImageRead ()
  {
  }

  inline void HDU::saveReadKeyword (Keyword* newKey)
  {
    m_keyWord.insert(std::map<String,Keyword*>::value_type(newKey->name(),newKey->clone()));
  }

  inline std::map<String, Keyword*>& HDU::keyWord ()
  {

    return m_keyWord;
  }

  inline Keyword& HDU::keyWord (const String& keyName)
  {
  std::map<String,Keyword*>::iterator key = m_keyWord.find(keyName);
  if (key == m_keyWord.end()) throw HDU::NoSuchKeyword(keyName);
  return *((*key).second);
  }

  inline long& HDU::naxis ()
  {
    return m_naxis;
  }

  inline void HDU::naxis (const long& value)
  {
    m_naxis = value;
  }

  inline bool& HDU::anynul ()
  {
    return m_anynul;
  }

  inline void HDU::anynul (const bool& value)
  {
    m_anynul = value;
  }

  inline const std::map<string,Keyword*>& HDU::keyWord () const
  {
    return m_keyWord;
  }

  inline const Keyword& HDU::keyWord (const string& keyname) const
  {
  std::map<String,Keyword*>::const_iterator key = m_keyWord.find(keyname);
  if (key == m_keyWord.end()) throw HDU::NoSuchKeyword(keyname);
  return *((*key).second);
  }

  inline FITSBase*& HDU::parent ()
  {
    return m_parent;
  }

  inline std::vector< long >& HDU::naxes ()
  {
    return m_naxes;
  }

  inline long& HDU::naxes (size_t index)
  {
    return m_naxes[index];
  }

  inline void HDU::naxes (size_t index, const long& value)
  {
    m_naxes[index] = value;
  }

} // namespace CCfits
#ifdef SPEC_TEMPLATE_IMP_DEFECT
namespace CCfits {

  inline void HDU::readKeyMS(const String& keyName, int & val)
  {
          makeThisCurrent();
          Keyword& key = readKeyword(keyName);
          key.value(val);
  }

  inline void HDU::readKeys(std::vector<String>& keyNames, std::vector<String>& vals)
  {
          size_t nRead = keyNames.size();

          std::list<String> valKeys;
          std::list<String>      valList;
          for (size_t i = 0; i < nRead; i++) valKeys.push_back(keyNames[i]);
          // read all the keys requested, rejecting those that don't exist.

          readKeywords(valKeys);

          // get the values of all of the requested keys, rejecting those of the
          // wrong type.

          String current;
          std::list<String>::iterator it = valKeys.begin(); 
          while (it != valKeys.end())
          {
                  try
                  {
                          m_keyWord[*it]->value(current);
                          valList.push_back(current);       
			  ++it;
                  }
                  catch ( Keyword::WrongKeywordValueType )
                  {
                          it = valKeys.erase(it);                         
                  }
          }

          keyNames.erase(keyNames.begin(),keyNames.end());       

          if (!valList.empty())
          {
                  if (valList.size() != vals.size()) vals.resize(valList.size());

                  size_t i=0;
				  std::list<String>::const_iterator it1 = valList.begin();
                  for ( ; it1 != valList.end(); ++it1,++i)
                  {
                          vals[i] = *it1;
                  }
                  for ( it1= valKeys.begin(); it1 != valKeys.end(); ++it1)
                  {
                          keyNames.push_back(*it1);
                  }
          }

  }
}
 #endif


#endif
