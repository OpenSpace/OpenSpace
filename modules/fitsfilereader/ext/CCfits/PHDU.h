//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef PHDU_H
#define PHDU_H 1

// valarray
#include <valarray>
// HDU
#include "HDU.h"
// FITS
#include "FITS.h"
// FITSUtil
#include "FITSUtil.h"

namespace CCfits {
  class FITSBase;

} // namespace CCfits
// for CLONE_DEFECT
#ifdef _MSC_VER
#include "MSconfig.h"
#endif


namespace CCfits {

  /*! \class PHDU
        \brief class representing the primary HDU for a FITS file.

        A PHDU object is automatically instantiated and added to a FITS
        object when a FITS file is accessed in any way. If a new file
        is created without specifying the data type for the header, CCfits
        assumes that the file is to be used for table extensions and creates
        a dummy header. PHDU instances are <i>only</i> created by FITS
        ctors. In the first release of CCfits, the Primary cannot be changed once
        declared.

        PHDU and ExtHDU provide the same interface to writing images: multiple
        overloads of the templated PHDU::read and PHDU::write operations provide
        for (a) writing image data specified in a number of ways [C-array, std::vector, 
        std::valarray] and with input location specified by
         initial pixel, by n-tuple, and by rectangular subset [generalized slice];
        (b) reading image data specified similarly to the write options into a std::valarray.

        \todo Implement functions that allow replacement of the primary image

  */

  /*! \fn PHDU::PHDU (FITSBase* p, int bpix, int naxis, const std::vector<long>& axes)

  \brief Writing Primary HDU constructor, called by PrimaryHDU<T> class

   Constructor used for creating new PHDU (i.e. for writing data to FITS).
   also doubles as default constructor since all arguments have default values,
  which are passed to the HDU constructor



  */

/* !\fn  PHDU::PHDU (FITSBase* p)

    \brief Reading Primary HDU constructor.
   Constructor used  when reading the primary HDU from an existing file.
    Does nothing except initialize, with the real work done by the subclass
    PrimaryHDU<T>.

*/

/*! \fn PHDU::~PHDU ()
    \brief destructor
*/
/*! \fn  virtual void PHDU::readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>())  = 0;
        \brief read primary HDU data

        Called by FITS ctor, not intended for general use.
        parameters control how much gets read on initialization. An abstract function,
        implemented in the subclasses.

        \param readFlag read the image data if true
        \param key      a vector of strings of keyword names to be read from the primary HDU                        



*/

/*! \fn  virtual PHDU::clone(FITSbase* p) const = 0;

        \brief virtual copy constructor for Primary HDUs. 

        The operation is used when creating a copy of a FITS object.


*/

/*! \fn  bool PHDU::simple () const;

       \brief Returns the value of the Primary's SIMPLE keyword.

*/

/*! \fn  bool PHDU::extend () const;

       \brief Returns the value of the Primary's EXTEND keyword.

*/


/*! \fn      template<typename S> void PHDU::read (std::valarray<S>& image, 
                                        long first,
                                        long nElements, 
                                        S* nullValue) ; 

                \brief read part of an image array, processing null values.

                Implicit data conversion is supported (i.e. user does not need to know the 
                type of the data stored. A WrongExtensionType extension is thrown
                if *this is not an image. 

                \param image The receiving container, a std::valarray reference 
                \param first The first pixel from the array to read [a long value]
                \param nElements The number of values to read
                \param nullValue A pointer containing the value in the table to be 
                        considered as undefined. See cfitsio for details


*/      

/*! \fn      template<typename S> void PHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& first, 
		                long nElements, 
                                S* nullValue) ;

             \brief read part of an image array, processing null values.

             As above except for 

             \param first a vector<long> representing an n-tuple giving the
                    coordinates in the image of the first pixel.


*/      
/*! \fn      template<typename S> void PHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& firstVertex, 
		                const std::vector<long>& lastVertex, 
		                const std::vector<long>& stride, 
                                S* nullValue) ; 


             \brief read an image subset into valarray image, processing null values

                The image subset is defined by two vertices and a stride
                indicating the 'denseness' of the values to be picked in each
                dimension (a stride = (1,1,1,...) means picking every 
                pixel in every dimension, whereas stride = (2,2,2,...)
                means picking every other value in each dimension.


*/      
/*! \fn      template<typename S> void PHDU::read (std::valarray<S>& image, 
                                long first,
                                long nElements); 

              \brief read an image section starting at a specified pixel

*/      

/*! \fn      template<typename S> void PHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& first,
                                long nElements); 

              \brief read an image section starting at a location specified by an n-tuple
*/      
/*! \fn      template<typename S> void PHDU::read (std::valarray<S>& image, 
                                const std::vector<long>& firstVertex, 
		                const std::vector<long>& lastVertex, 
		                const std::vector<long>& stride);    


                \brief read an image subset

*/

/*! \fn      template <typename S> void PHDU::write(const std::vector<long>& first,
                    long nElements,
                    const std::valarray<S>& data,
                    S* nullValue);

        \brief Write a set of pixels to an image extension with the first pixel specified by an n-tuple, processing undefined data

        All the overloaded versions of PHDU::write perform operations on *this if
        it is an image and throw a WrongExtensionType exception if not.
        Where appropriate, alternate versions allow undefined data to be processed

        \param first an n-tuple of dimension equal to the image dimension specifying the 
                first pixel in the range to be written
        \param nElements number of pixels to be written
        \param data array of data to be written
        \param nullValue pointer to null value (data with this value written as undefined; needs
                the BLANK keyword to have been specified).

*/


/*! \fn template <typename S> void PHDU::write(long first,
                    long nElements, const std::valarray<S>& data, S* nullValue);

        \brief write array to image starting with a specified pixel and allowing undefined data to be processed

        parameters after the first are as for version with n-tuple specifying first element.
        these two version are equivalent, except that it is possible for the
        first pixel number to exceed the range of 32-bit integers, which is
        how long datatype is commonly implemented.                
*/

/*! \fn template <typename S> void PHDU::write(const std::vector<long>& first,
                    long nElements, const std::valarray<S>& data);

        \brief write array starting from specified n-tuple, without undefined data processing
*/

/*! \fn template <typename S> void PHDU::write(long first, long nElements,
                    const std::valarray<S>& data);

        \brief write array starting from specified pixel number, without undefined data processing


*/

/*! \fn      template <typename S> void PHDU::write(const std::vector<long>& firstVertex,
                    const std::vector<long>& lastVertex,
                    const std::vector<long>& stride,
                    const std::valarray<S>& data);     

              \brief write a subset (generalize slice) of data to the image

                A generalized slice/subset is a subset of the image (e.g. one plane
                of a data cube of size <= the dimension of the cube). It is specified
                by two opposite vertices. The equivalent cfitsio call does not support
                undefined data processing so there is no version that allows a null
                value to be specified.

                \param firstVertex The coordinates specifying lower and upper vertices of the n-dimensional slice 
                \param lastVertex
                \param stride Pixels to skip in each to dimension, e.g. stride = (1,1,1,...)
                means picking every pixel in every dimension, whearas stride = (2,2,2,...)
                means picking every other value in each dimension.
                \param data The data to be written              
*/

/*! \fn  PHDU::PHDU(const PHDU& right)
        \brief copy constructor

        required for cloning primary HDUs when copying FITS files.


*/



  class PHDU : public HDU  //## Inherits: <unnamed>%394E6F9800C3
  {

    public:
        virtual ~PHDU();

        //	Read data reads the image if readFlag is true and
        //	optional keywords if supplied. Thus, with no arguments,
        //	readData() does nothing.
        virtual void readData (bool readFlag = false, const std::vector<String>& keys = std::vector<String>()) = 0;
        virtual PHDU * clone (FITSBase* p) const = 0;
        virtual void zero (double value);
        virtual void scale (double value);
        virtual double zero () const;
        virtual double scale () const;

        bool simple () const;
        bool extend () const;

    public:
      // Additional Public Declarations
      // image reading/writing interface. 

      // The S template parameter, like for Column, denotes the fact that
      // the type of the input array and the object to be read may not match.


      // the rw interface for images consists of equivalents for fits_read_img,
      // fits_read_pix, and fits_read_subset.

      // the paradigm for reading is that the image object (a valarray<T> type)
      // is the size of the data already read.

      // write_subset has no null value aware analogue.
        template <typename S>
        void write(const std::vector<long>& first,
                        long nElements,
                        const std::valarray<S>& data,
                        S* nullValue);


        template <typename S>
        void write(long first,
                        long nElements,
                        const std::valarray<S>& data,
                        S* nullValue);


        template <typename S>
        void write(const std::vector<long>& first,
                        long nElements,
                        const std::valarray<S>& data);


        template <typename S>
        void write(long first,
                        long nElements,
                        const std::valarray<S>& data);

        template <typename S>
        void write(const std::vector<long>& firstVertex,
                        const std::vector<long>& lastVertex,
                        const std::vector<long>& stride, 
                        const std::valarray<S>& data); 

        // read image data and return an array. Can't return a reference
        // because the type conversion needs to allocate a new object in general.

        template<typename S>
        void read(std::valarray<S>& image) ; 

 	template<typename S>
        void  read (std::valarray<S>& image, long first,long nElements); 

	template<typename S>
        void read (std::valarray<S>& image, long first,long nElements, S* nullValue) ; 

	template<typename S>
        void read (std::valarray<S>& image, const std::vector<long>& first,long nElements)  ; 

	template<typename S>
        void read (std::valarray<S>& image, const std::vector<long>& first, long nElements, 
                                        S* nullValue); 

        template<typename S>
        void read (std::valarray<S>& image, const std::vector<long>& firstVertex, 
				const std::vector<long>& lastVertex, 
				const std::vector<long>& stride)  ;      

        template<typename S>
        void read (std::valarray<S>& image, const std::vector<long>& firstVertex, 
				const std::vector<long>& lastVertex, 
				const std::vector<long>& stride, 
                                S* nullValue) ; 


    protected:
        PHDU(const PHDU &right);
        //	Constructor for new FITS objects, takes as arguments
        //	the required keywords for a primary HDU.
        PHDU (FITSBase* p, int bpix, int naxis, const std::vector<long>& axes);
        //	Custom constructor. Allows specification of data to be read and whether to read data at
        //	construction or wait until the image data are requested. The default is 'lazy initialization:'
        //	wait until asked.
        PHDU (FITSBase* p = 0);

        virtual void initRead ();
        void simple (bool value);
        void extend (bool value);

      // Additional Protected Declarations

    private:
      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        bool m_simple;
        bool m_extend;

      // Additional Implementation Declarations

  };

  // Class CCfits::PHDU 

  inline bool PHDU::simple () const
  {
    return m_simple;
  }

  inline void PHDU::simple (bool value)
  {
    m_simple = value;
  }

  inline bool PHDU::extend () const
  {
    return m_extend;
  }

  inline void PHDU::extend (bool value)
  {
    m_extend = value;
  }

} // namespace CCfits


#endif
