//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef COLUMNDATA_H
#define COLUMNDATA_H 1
#include "CCfits.h"

// vector
#include <vector>
// Column
#include "Column.h"
#ifdef _MSC_VER
#include "MSconfig.h"
#endif

#include <complex>
#include <memory>
#include <iterator>
#include "FITSUtil.h"
using std::complex;
#include "FITS.h"


namespace CCfits {



  template <typename T>
  class ColumnData : public Column  //## Inherits: <unnamed>%385E51565EE8
  {

    public:
        ColumnData(const ColumnData< T > &right);
        ColumnData (Table* p = 0);
        ColumnData (int columnIndex, const string &columnName, ValueType type, const String &format, const String &unit, Table* p, int rpt = 1, long w = 1, const String &comment = "");
        ~ColumnData();

        virtual ColumnData<T>* clone () const;
        virtual void readData (long firstRow, long nelements, long firstElem = 1);
        void setDataLimits (T* limits);
        const T minLegalValue () const;
        void minLegalValue (T value);
        const T maxLegalValue () const;
        void maxLegalValue (T value);
        const T minDataValue () const;
        void minDataValue (T value);
        const T maxDataValue () const;
        void maxDataValue (T value);
        const std::vector<T>& data () const;
        void setData (const std::vector<T>& value);
        T data (int i);
        void data (int i, T value);

      // Additional Public Declarations
        friend class Column;
    protected:
      // Additional Protected Declarations

    private:
        ColumnData< T > & operator=(const ColumnData< T > &right);

        void readColumnData (long firstRow, long nelements, T* nullValue = 0);
        virtual bool compare (const Column &right) const;
        virtual std::ostream& put (std::ostream& s) const;
        void writeData (T* indata, long nRows = 1, long firstRow = 1, T* nullValue = 0);
        void writeData (const std::vector<T>& indata, long firstRow = 1, T* nullValue = 0);
        //	Insert one or more blank rows into a FITS column.
        virtual void insertRows (long first, long number = 1);
        virtual void deleteRows (long first, long number = 1);

      // Additional Private Declarations

    private: //## implementation
      // Data Members for Class Attributes
        T m_minLegalValue;
        T m_maxLegalValue;
        T m_minDataValue;
        T m_maxDataValue;

      // Data Members for Associations
        std::vector<T> m_data;

      // Additional Implementation Declarations

  };

  // Parameterized Class CCfits::ColumnData 

  template <typename T>
  inline void ColumnData<T>::readData (long firstRow, long nelements, long firstElem)
  {
   readColumnData(firstRow,nelements,static_cast<T*>(0));
  }

  template <typename T>
  inline const T ColumnData<T>::minLegalValue () const
  {
    return m_minLegalValue;
  }

  template <typename T>
  inline void ColumnData<T>::minLegalValue (T value)
  {
    m_minLegalValue = value;
  }

  template <typename T>
  inline const T ColumnData<T>::maxLegalValue () const
  {
    return m_maxLegalValue;
  }

  template <typename T>
  inline void ColumnData<T>::maxLegalValue (T value)
  {
    m_maxLegalValue = value;
  }

  template <typename T>
  inline const T ColumnData<T>::minDataValue () const
  {
    return m_minDataValue;
  }

  template <typename T>
  inline void ColumnData<T>::minDataValue (T value)
  {
    m_minDataValue = value;
  }

  template <typename T>
  inline const T ColumnData<T>::maxDataValue () const
  {
    return m_maxDataValue;
  }

  template <typename T>
  inline void ColumnData<T>::maxDataValue (T value)
  {
    m_maxDataValue = value;
  }

  template <typename T>
  inline const std::vector<T>& ColumnData<T>::data () const
  {
    return m_data;
  }

  template <typename T>
  inline void ColumnData<T>::setData (const std::vector<T>& value)
  {
    m_data = value;
  }

  template <typename T>
  inline T ColumnData<T>::data (int i)
  {
    // return data stored in the ith row, which is in the i-1 th location in the array.
    return m_data[i - 1];
  }

  template <typename T>
  inline void ColumnData<T>::data (int i, T value)
  {
    // assign data to i-1 th location in the array, representing the ith row.
    m_data[i - 1] = value;
  }

  // Parameterized Class CCfits::ColumnData 

  template <typename T>
  ColumnData<T>::ColumnData(const ColumnData<T> &right)
      :Column(right),
       m_minLegalValue(right.m_minLegalValue),
       m_maxLegalValue(right.m_maxLegalValue),
       m_minDataValue(right.m_minDataValue),
       m_maxDataValue(right.m_maxDataValue),
       m_data(right.m_data)
  {
  }

  template <typename T>
  ColumnData<T>::ColumnData (Table* p)
  : Column(p),
       m_minLegalValue(),
       m_maxLegalValue(),
       m_minDataValue(),
       m_maxDataValue(), 
       m_data()
  {
  }

  template <typename T>
  ColumnData<T>::ColumnData (int columnIndex, const string &columnName, ValueType type, const String &format, const String &unit, Table* p, int rpt, long w, const String &comment)
        : Column(columnIndex,columnName,type,format,unit,p,rpt,w,comment), 
        m_minLegalValue(),
        m_maxLegalValue(),
        m_minDataValue(),
        m_maxDataValue(),
        m_data()
  {
  }


  template <typename T>
  ColumnData<T>::~ColumnData()
  {
  }


  template <typename T>
  void ColumnData<T>::readColumnData (long firstRow, long nelements, T* nullValue)
  {
  if ( rows() < nelements ) 
  {
        std::cerr << "CCfits: More data requested than contained in table. ";
        std::cerr << "Extracting complete column.\n";
        nelements = rows();
   }   

   int   status(0);
   int   anynul(0);

   FITSUtil::auto_array_ptr<T> array(new T[nelements]); 

   makeHDUCurrent();

   if ( fits_read_col(fitsPointer(),type(),  index(), firstRow, 1, 
  	nelements, nullValue, array.get(), &anynul, &status) ) throw FitsError(status);


   if (m_data.size() != static_cast<size_t>( rows() ) ) m_data.resize(rows());

   std::copy(&array[0],&array[nelements],m_data.begin()+firstRow-1);
   if (nelements == rows()) isRead(true); 
  }

  template <typename T>
  bool ColumnData<T>::compare (const Column &right) const
  {
  if ( !Column::compare(right) ) return false;
  const ColumnData<T>& that = static_cast<const ColumnData<T>&>(right);
  unsigned int n = m_data.size();
  if ( that.m_data.size() != n ) return false;
  for (unsigned int i = 0; i < n ; i++)
  {
        if (m_data[i] != that.m_data[i]) return false;   
  }
  return true;
  }

  template <typename T>
  ColumnData<T>* ColumnData<T>::clone () const
  {
        return new ColumnData<T>(*this);
  }

  template <typename T>
  std::ostream& ColumnData<T>::put (std::ostream& s) const
  {
  Column::put(s);
  if (FITS::verboseMode() && type() != Tstring)
  {
        s << " Column Legal limits: ( " << m_minLegalValue << "," << m_maxLegalValue << " )\n" 
        << " Column Data  limits: ( " << m_minDataValue << "," << m_maxDataValue << " )\n";
  }
  if (!m_data.empty())
  {
        std::ostream_iterator<T> output(s,"\n");
        // output each row on a separate line.
        // user can supply manipulators to stream for formatting.
        std::copy(m_data.begin(),m_data.end(),output);
  }

    return s;
  }

  template <typename T>
  void ColumnData<T>::writeData (T* indata, long nRows, long firstRow, T* nullValue)
  {

          // set columnData's data member to equal what's written to file.
          // indata has size nRows: elements firstRow to firstRow + nRows - 1 will be written.
          // if this exceeds the current rowlength of the HDU, update the return value for
          // rows() in the parent after the fitsio call.
          int status(0);
          long elementsToWrite(nRows + firstRow -1);
          // get a copy for restorative action.   
          std::vector<T> __tmp(m_data);


          if (elementsToWrite > static_cast<long>(m_data.size())) 
          {

                  m_data.resize(elementsToWrite,T());
          }

          std::copy(&indata[0],&indata[nRows],m_data.begin()+firstRow-1);

          // if successful, write to disk.

          try
          {
             if (nullValue)
             {
                if (fits_write_colnull(fitsPointer(), type(), index(), firstRow, 1, nRows,
			          indata, nullValue, &status) != 0) throw FitsError(status);
             }
             else
             {
                if (fits_write_col(fitsPointer(), type(), index(), firstRow, 1, nRows,
			          indata, &status) != 0) throw FitsError(status);
             }

                // tell the Table that the number of rows has changed
                parent()->updateRows();
          }
          catch (FitsError) // the only thing that can throw here.
          {
                  // reset to original content and rethrow the exception.
                  m_data = __tmp;
                  if (status == NO_NULL) throw NoNullValue(name());
                  else throw;
          }      
  }

  template <typename T>
  void ColumnData<T>::writeData (const std::vector<T>& indata, long firstRow, T* nullValue)
  {
        FITSUtil::CVarray<T> convert;
        FITSUtil::auto_array_ptr<T> pcolData (convert(indata));
        T* columnData  = pcolData.get();
        writeData(columnData,indata.size(),firstRow,nullValue);
  }

  template <typename T>
  void ColumnData<T>::insertRows (long first, long number)
  {
    FITSUtil::FitsNullValue<T> blank;
    typename std::vector<T>::iterator in;
    if (first !=0) 
    {
            in = m_data.begin()+first;
    }
    else
    {
            in = m_data.begin();
    }           

    // non-throwing operations.
    m_data.insert(in,number,blank());
  }

  template <typename T>
  void ColumnData<T>::deleteRows (long first, long number)
  {
    m_data.erase(m_data.begin()+first-1,m_data.begin()+first-1+number);
  }

  template <typename T>
  void ColumnData<T>::setDataLimits (T* limits)
  {
    m_minLegalValue = limits[0];
    m_maxLegalValue = limits[1];
    m_minDataValue = std::max(limits[2],limits[0]);
    m_maxDataValue = std::min(limits[3],limits[1]);
  }

  // Additional Declarations

  // all functions that operate on strings or complex data that call cfitsio 
  // need to be specialized.

#if SPEC_TEMPLATE_IMP_DEFECT || SPEC_TEMPLATE_DECL_DEFECT
template <>
inline void ColumnData<complex<float> >::setDataLimits (complex<float>* limits)
       {
                m_minLegalValue = limits[0];
                m_maxLegalValue = limits[1];
                m_minDataValue =  limits[2];
                m_maxDataValue =  limits[3];
        }
#else
template <>
  void ColumnData<complex<float> >::setDataLimits (complex<float>* limits);
#endif

#if SPEC_TEMPLATE_IMP_DEFECT || SPEC_TEMPLATE_DECL_DEFECT
template <>
inline void ColumnData<complex<double> >::setDataLimits (complex<double>* limits)
        {
                m_minLegalValue = limits[0];
                m_maxLegalValue = limits[1];
                m_minDataValue =  limits[2];
                m_maxDataValue =  limits[3];
        }
#else
 template <>
  void ColumnData<complex<double> >::setDataLimits (complex<double>* limits);
#endif


#if SPEC_TEMPLATE_IMP_DEFECT || SPEC_TEMPLATE_DECL_DEFECT
        template <>
        inline void ColumnData<string>::readColumnData (long firstRow, 
                                        long nelements, 
                                        string* nullValue)
        {
           // nelements = nrows to read.
           if (nelements < 1)
              throw Column::InvalidNumberOfRows((int)nelements);
           if (firstRow < 1 || (firstRow+nelements-1)>rows())
              throw Column::InvalidRowNumber(name());
           
           int status = 0;
           int   anynul = 0;
           
           char** array = new char*[nelements];
           // Initialize pointers to NULL so we can safely delete
           //  during error handling, even if they haven't been allocated.           
           for (long i=0; i<nelements; ++i)
              array[i]=static_cast<char*>(0);
           bool isError = false;

           // Strings are unusual.  The variable length case is still
           //  handled by a ColumnData class, not a ColumnVectorData.
           char* nulval = 0;
           if (nullValue) 
           {
              nulval = const_cast<char*>(nullValue->c_str());
           }
           else
           {
              nulval = new char;
              *nulval = '\0';       
           }
           makeHDUCurrent();
           if (varLength())
           {
              long* strLengths = new long[nelements];
              long* offsets = new long[nelements];
              if (fits_read_descripts(fitsPointer(), index(), firstRow,
                   nelements, strLengths, offsets, &status))
              {
                 isError = true;
              }
              else
              {
                 // For variable length cols, must read 1 and only 1 row
                 //  at a time into array.
                 for (long j=0; j<nelements; ++j)
                 {
                    array[j] = new char[strLengths[j] + 1];
                 }
                 
                 const long lastRow = firstRow+nelements-1;
                 for (long iRow=firstRow; !isError && iRow<=lastRow; ++iRow)
                 {
                    if (fits_read_col_str(fitsPointer(),index(), iRow, 1, 1,
                      nulval, &array[iRow-firstRow], &anynul,&status) )
                       isError=true;
                 }
              }
              delete [] strLengths;
              delete [] offsets;              
           }
           else
           {
              // Fixed length strings, length is stored in Column's m_width.
              for (long j=0; j<nelements; ++j)
              {
                  array[j] = new char[width() + 1];
              }
              if (fits_read_col_str(fitsPointer(),index(), firstRow,1,nelements,
                nulval,array, &anynul,&status))
                isError=true;
           }
           
           if (isError)
           {
              // It's OK to do this even if error occurred before
              //  array rows were allocated.  In that case their pointers
              //  were set to NULL.
              for (long j = 0; j < nelements; ++j)
              {
                 delete [] array[j];
              }     
              delete [] array; 
              delete nulval;
              throw FitsError(status); 
           }

          if (m_data.size() != static_cast<size_t>(rows()))
              setData(std::vector<String>(rows(),String(nulval)));

          for (long j=0; j<nelements; ++j)
          {
             m_data[j - 1 + firstRow] = String(array[j]);
          }

          for (long j=0; j<nelements; j++)
          {
             delete [] array[j];
          }     
          delete [] array; 
          delete nulval; 
          if (nelements == rows()) isRead(true); 

        }
#else 
 template <>
void ColumnData<string>::readColumnData (long firstRow, long nelements, string* nullValue);
#endif


#if SPEC_TEMPLATE_IMP_DEFECT || SPEC_TEMPLATE_DECL_DEFECT
        template <>
        inline void ColumnData<complex<float> >::readColumnData (long firstRow,
                                                long nelements,
                                                complex<float>* nullValue)
        {
          // specialization for ColumnData<string> 
          int status(0);
          int   anynul(0);
          FITSUtil::auto_array_ptr<float> pArray(new float[nelements*2]); 
          float* array = pArray.get();
          float nulval(0);
          makeHDUCurrent();


          if (fits_read_col_cmp(fitsPointer(),index(), firstRow,1,nelements,
                  nulval,array, &anynul,&status) ) throw FitsError(status);


          if (m_data.size() != rows()) m_data.resize(rows());

          // the 'j -1 ' converts to zero based indexing.

          for (int j = 0; j < nelements; ++j)
          {

                m_data[j - 1 + firstRow] = std::complex<float>(array[2*j],array[2*j+1]);
          }
          if (nelements == rows()) isRead(true); 

        }
#else
template <> 
void ColumnData<complex<float> >::readColumnData (long firstRow, long nelements,complex<float>* nullValue );
#endif

#if SPEC_TEMPLATE_IMP_DEFECT || SPEC_TEMPLATE_DECL_DEFECT
        template <>
        inline void ColumnData<complex<double> >::readColumnData (long firstRow, 
                                                        long nelements,
                                                        complex<double>* nullValue)
        {
          // specialization for ColumnData<complex<double> > 
           int status(0);
           int   anynul(0);
           FITSUtil::auto_array_ptr<double> pArray(new double[nelements*2]); 
           double* array = pArray.get();
           double nulval(0);
           makeHDUCurrent();


          if (fits_read_col_dblcmp(fitsPointer(), index(), firstRow,1,nelements,
                  nulval,array, &anynul,&status) ) throw FitsError(status);




          if (m_data.size() != rows()) setData(std::vector<complex<double> >(rows(),nulval));

          // the 'j -1 ' converts to zero based indexing.

          for (int j = 0; j < nelements; j++)
          {

                m_data[j - 1 + firstRow] = std::complex<double>(array[2*j],array[2*j+1]);
          }
          if (nelements == rows()) isRead(true); 

        }
#else
template <>
void ColumnData<complex<double> >::readColumnData (long firstRow, long nelements,complex<double>* nullValue);
#endif

#if SPEC_TEMPLATE_DECL_DEFECT
  template <>
  inline void ColumnData<string>::writeData (const std::vector<string>& indata, 
				      long firstRow, string* nullValue)
  {
    int    status=0;
    char** columnData=FITSUtil::CharArray(indata);

    if ( fits_write_colnull(fitsPointer(), TSTRING, index(), firstRow, 1, indata.size(),
			    columnData, 0, &status) != 0 )
      throw FitsError(status);
    unsigned long elementsToWrite (indata.size() + firstRow - 1);
    std::vector<string> __tmp(m_data);
    if (m_data.size() < elementsToWrite) 
      {
	m_data.resize(elementsToWrite,"");
	std::copy(__tmp.begin(),__tmp.end(),m_data.begin());
      }
    std::copy(indata.begin(),indata.end(),m_data.begin()+firstRow-1);


    for (size_t i = 0; i < indata.size(); ++i)
      {
	delete [] columnData[i];
      }
    delete [] columnData;
  }  
#else
template <>
void ColumnData<string>::writeData (const std::vector<string>& inData, long firstRow, string* nullValue);
#endif

#ifdef SPEC_TEMPLATE_DECL_DEFECT
  template <>
  inline void ColumnData<complex<float> >::writeData (const std::vector<complex<float> >& inData, 
					       long firstRow, 
					       complex<float>* nullValue)
  {
    int status(0);
    int nRows (inData.size());
    FITSUtil::auto_array_ptr<float> pData(new float[nRows*2]);
    float* Data = pData.get();
    std::vector<complex<float> > __tmp(m_data);
    for (int j = 0; j < nRows; ++j)
      {
	Data[ 2*j] = inData[j].real();
	Data[ 2*j + 1] = inData[j].imag();
      }     

    try
      {

	if (fits_write_col_cmp(fitsPointer(), index(), firstRow, 1, 
			       nRows,Data, &status) != 0) throw FitsError(status);
	long elementsToWrite(nRows + firstRow -1);
	if (elementsToWrite > static_cast<long>(m_data.size())) 
	  {

	    m_data.resize(elementsToWrite);
	  }

	std::copy(inData.begin(),inData.end(),m_data.begin()+firstRow-1);

	// tell the Table that the number of rows has changed
	parent()->updateRows();
      }
    catch (FitsError) // the only thing that can throw here.
      {
	// reset to original content and rethrow the exception.
	m_data.resize(__tmp.size());
	m_data = __tmp;
      }      

  }

#else
template <>
void ColumnData<complex<float> >::writeData (const std::vector<complex<float> >& inData, long firstRow, 
                                complex<float>* nullValue);
#endif

#ifdef SPEC_TEMPLATE_DECL_DEFECT
  template <>
  inline void ColumnData<complex<double> >::writeData (const std::vector<complex<double> >& inData, 
						long firstRow, 
						complex<double>* nullValue)
  {
    int status(0);
    int nRows (inData.size());
    FITSUtil::auto_array_ptr<double> pData(new double[nRows*2]);
    double* Data = pData.get();
    std::vector<complex<double> > __tmp(m_data);
    for (int j = 0; j < nRows; ++j)
      {
	pData[ 2*j] = inData[j].real();
	pData[ 2*j + 1] = inData[j].imag();
      }     

    try
      {

	if (fits_write_col_dblcmp(fitsPointer(), index(), firstRow, 1, 
                                  nRows,Data, &status) != 0) throw FitsError(status);
	long elementsToWrite(nRows + firstRow -1);
	if (elementsToWrite > static_cast<long>(m_data.size())) 
	  {

	    m_data.resize(elementsToWrite);
	  }

	std::copy(inData.begin(),inData.end(),m_data.begin()+firstRow-1);

	// tell the Table that the number of rows has changed
	parent()->updateRows();
      }
    catch (FitsError) // the only thing that can throw here.
      {
	// reset to original content and rethrow the exception.
	m_data.resize(__tmp.size());
	m_data = __tmp;
      }      

  }

#else
template <>
void ColumnData<complex<double> >::writeData (const std::vector<complex<double> >& inData, long firstRow, 
                                complex<double>* nullValue);

#endif
} // namespace CCfits


#endif
