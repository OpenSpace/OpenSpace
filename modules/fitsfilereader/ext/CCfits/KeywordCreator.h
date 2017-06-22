//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef KEYWORDCREATOR_H
#define KEYWORDCREATOR_H 1

// KeyData
#include "KeyData.h"
// FitsError
#include "FitsError.h"

namespace CCfits {
  class HDU;

} // namespace CCfits


namespace CCfits {



  class KeywordCreator 
  {

    public:
        KeywordCreator (HDU* p);
        virtual ~KeywordCreator();

        virtual Keyword* MakeKeyword (const String& keyName, const String& comment = String("")) = 0;
        static Keyword* getKeyword (const String& keyName, HDU* p);
        //	Additional Public Declarations
        virtual void reset ();
        virtual Keyword* createKeyword (const String& keyName, const String& comment = String(""));
        //	This version of getKeyword is for reading a keyword
        //	in with a specified type.
        static Keyword* getKeyword (const String& keyName, ValueType keyType, HDU* p);
        static Keyword* getKeyword (int keyNumber, HDU* p);
        // If calling function already has the keyword name, it can send it in as the
        //   3rd argument and the function will make use of it.  Otherwise leave it
        //   empty and the function will just extract the keyword name from the card.
        //   This function does not take ownership of the memory allocated to card.
        static Keyword* getKeywordFromCard(char* card, HDU* p, const String& keyName=string(""));

      // Additional Public Declarations

    protected:
        HDU* forHDU ();

      // Additional Protected Declarations

    private:
        KeywordCreator(const KeywordCreator &right);
        KeywordCreator & operator=(const KeywordCreator &right);

        static Keyword* parseRecord (const String& name, const String& valueString, const String& comment, HDU* hdu);
        static bool isContinued (const String& value);
        static void getLongValueString (HDU* p, const String& keyName, String& value);

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        Keyword *m_keyword;

      // Data Members for Associations
        HDU* m_forHDU;

      // Additional Implementation Declarations

  };

  // Class CCfits::KeywordCreator 

  inline void KeywordCreator::reset ()
  {
    m_keyword=0;

  }

  inline HDU* KeywordCreator::forHDU ()
  {
    return m_forHDU;
  }

} // namespace CCfits


#endif
