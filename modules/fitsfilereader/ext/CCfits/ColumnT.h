//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

#ifndef COLUMNT_H
#define COLUMNT_H

#ifdef _MSC_VER
#include "MSconfig.h"
#endif

#include "ColumnData.h"
#include "ColumnVectorData.h"
#include "FITSUtil.h"
#include <typeinfo>
#include <vector>
#include <algorithm>
#include "NewKeyword.h"

#ifdef SSTREAM_DEFECT
#       include <strstream>
#else
#       include <sstream>
#endif


// by design, if the data are not read yet we will return an exception.
// here the test is if the entire column has already been read. 
using std::complex;
using std::valarray;

// get specified elements of a scalar column. These two functions allow the
// user to return either a vector or a valarray depending on the input container.

namespace CCfits 
{
   template <typename S>
   void Column::read(std::vector<S>& vals, long first, long last) 
   {
           read(vals,first,last,static_cast<S*>(0));
   }


   template <typename S>
   void Column::read(std::vector<S>& vals, long first, long last, S* nullValue) 
   {
           // problem: S does not give the type of the Column, but the return type,
           // so the user must specify this.
           parent()->makeThisCurrent();
           long nelements = numberOfElements(first,last);

           if  (ColumnData<S>* col = dynamic_cast<ColumnData<S>*>(this))
           {
                   // fails if user requested outputType different from input type.


                   if (!isRead()) col->readColumnData(first,nelements,nullValue);
                   // scalar column with vector output can just be assigned.
                   FITSUtil::fill(vals,col->data(),first,last);
           }
           else
           {
                   FITSUtil::MatchType<S> outputType;
                   if ( outputType() == type() ) 
                   { 
                           // in this case user tried to read vector data from scalar,
                           // (i.e. first argument was vector<valarray<S> >.
                           // since the cast won't fail on template parameter grounds.
                           throw Column::WrongColumnType(name());
                   }

                   try
                   {
                       // about exceptions. The dynamic_casts could throw
                       // std::bad_cast. If this happens something is seriously
                       // wrong since the Column stores the value of type() 
                       // appropriate to each of the casts on construction.
                       // 
                       // the InvalidDataType exception should not be possible.
                       if  ( type() == Tdouble )
		       {
                               ColumnData<double>& col 
                                         = dynamic_cast<ColumnData<double>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements);                                  
                               FITSUtil::fill(vals,col.data(),first,last);

                       }
		       else if (type() == Tfloat)
		       {
                               ColumnData<float>& col 
                                       = dynamic_cast<ColumnData<float>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tint)
		       {
                               int nullVal(0);
                               if (nullValue) nullVal = static_cast<int>(*nullValue);
                               ColumnData<int>& col  
                                  = dynamic_cast<ColumnData<int>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
		       }
		       else if (type() == Tshort)
                       {
                               short nullVal(0);
                               if (nullValue) nullVal = static_cast<short>(*nullValue);
                               ColumnData<short>& col 
                                = dynamic_cast<ColumnData<short>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tlong)
		       {	
                               long nullVal(0);
                               if (nullValue) nullVal = static_cast<long>(*nullValue); 
                               ColumnData<long>& col 
                                 = dynamic_cast<ColumnData<long>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tlonglong)
		       {	
                               LONGLONG nullVal(0);
                               if (nullValue) nullVal = static_cast<LONGLONG>(*nullValue); 
                               ColumnData<LONGLONG>& col 
                                 = dynamic_cast<ColumnData<LONGLONG>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tlogical)
		       {	
                               bool nullVal(0);
                               if (nullValue) nullVal = static_cast<bool>(*nullValue); 
                               ColumnData<bool>& col 
                                 = dynamic_cast<ColumnData<bool>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
		       }
		       else if (type() == Tbit || type() == Tbyte)
		       {
                               unsigned char nullVal(0);
                               if (nullValue) nullVal = static_cast<unsigned char>(*nullValue); 
                               ColumnData<unsigned char>& col 
                                       = dynamic_cast<ColumnData<unsigned char>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal); 
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tushort)
                       {
                               unsigned short nullVal(0);
                               if (nullValue) nullVal= static_cast<unsigned short>(*nullValue);
                               ColumnData<unsigned short>& col 
                                       = dynamic_cast<ColumnData<unsigned short>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tuint)
                       {
                               unsigned int nullVal(0);
                               if (nullValue) nullVal = static_cast<unsigned int>(*nullValue);
                               ColumnData<unsigned int>& col 
                                       = dynamic_cast<ColumnData<unsigned int>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
		       }
		       else if (type() == Tulong)
                       {
                               unsigned long nullVal(0);
                               if (nullValue) nullVal = static_cast<unsigned long>(*nullValue);
                               ColumnData<unsigned long>& col 
                                        = dynamic_cast<ColumnData<unsigned long>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else
                       {
                                 throw InvalidDataType(name());

                       }

                   }
                   catch (std::bad_cast)
                   {
			   throw WrongColumnType(name());
                   }
           }

   }

   template <typename S>
   void Column::read(std::valarray<S>& vals, long first, long last) 
   {
           read(vals,first,last,static_cast<S*>(0));
   }


   template <typename S>
   void Column::read(std::valarray<S>& vals, long first, long last, S* nullValue) 
   {        
           // require the whole scalar column to have been read.


           long nelements = numberOfElements(first,last);
           parent()->makeThisCurrent();                
           if ( ColumnData<S>* col = dynamic_cast<ColumnData<S>*>(this))
           {
                   // fails if user requested outputType different from input type.


                   if (!isRead()) col->readColumnData(first,nelements,nullValue);                                  
                   FITSUtil::fill(vals,col->data(),first,last);

           }
           else
           {
                   FITSUtil::MatchType<S> outputType;
                   if ( outputType() == type() ) 
                   { 
                           // in this case user tried to read vector data from scalar,
                           // (i.e. first argument was vector<valarray<S> >.
                           // since the cast won't fail on template parameter grounds.
                           throw Column::WrongColumnType(name());
                   }

                   try
                   {
                       // about exceptions. The dynamic_casts could throw
                       // std::bad_cast. If this happens something is seriously
                       // wrong since the Column stores the value of type() 
                       // appropriate to each of the casts on construction.
                       // 
                       // the InvalidDataType exception should not be possible.
                       if  ( type() == Tdouble )
		       {
                               ColumnData<double>& col 
                                         = dynamic_cast<ColumnData<double>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tfloat)
		       {
                               ColumnData<float>& col 
                                       = dynamic_cast<ColumnData<float>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements);                                  
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tint)
		       {
                               int nullVal(0);
                               if (nullValue) nullVal = static_cast<int>(*nullValue); 
                               ColumnData<int>& col  
                                       = dynamic_cast<ColumnData<int>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
		       }
		       else if (type() == Tshort)
                       {
                               short nullVal(0);
                               if (nullValue) nullVal = static_cast<short>(*nullValue); 
                               ColumnData<short>& col 
                                       = dynamic_cast<ColumnData<short>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tlong)
		       {	
                               long nullVal(0);
                               if (nullValue) nullVal = static_cast<long>(*nullValue); 
                               ColumnData<long>& col 
                                       = dynamic_cast<ColumnData<long>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tlonglong)
		       {	
                               LONGLONG nullVal(0);
                               if (nullValue) nullVal = static_cast<LONGLONG>(*nullValue); 
                               ColumnData<LONGLONG>& col 
                                       = dynamic_cast<ColumnData<LONGLONG>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tlogical)
		       {	
                               bool nullVal(0);
                               if (nullValue) nullVal = static_cast<bool>(*nullValue); 
                               ColumnData<bool>& col 
                                       = dynamic_cast<ColumnData<bool>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
		       }
		       else if (type() == Tbit || type() == Tbyte)
		       {
                               unsigned char nullVal(0);
                               if (nullValue) nullVal = static_cast<unsigned char>(*nullValue); 
                               ColumnData<unsigned char>& col 
                                       = dynamic_cast<ColumnData<unsigned char>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tushort)
                       {
                               unsigned short nullVal(0);
                               if (nullValue) nullVal 
                                       = static_cast<unsigned short>(*nullValue); 
                               ColumnData<unsigned short>& col 
                                       = dynamic_cast<ColumnData<unsigned short>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else if (type() == Tuint)
                       {
                               unsigned int nullVal(0);
                               if (nullValue) nullVal 
                                       = static_cast<unsigned int>(*nullValue); 
                               ColumnData<unsigned int>& col 
                                       = dynamic_cast<ColumnData<unsigned int>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
		       }
		       else if (type() == Tulong)
                       {
                               unsigned long nullVal(0);
                               if (nullValue) nullVal 
                                       = static_cast<unsigned long>(*nullValue); 
                               ColumnData<unsigned long>& col 
                                       = dynamic_cast<ColumnData<unsigned long>&>(*this);
                               if (!isRead()) col.readColumnData(first,nelements,&nullVal);                                    
                               FITSUtil::fill(vals,col.data(),first,last);
                       }
		       else
                       {
                                 throw InvalidDataType(name());

                       }

                   }
                   catch (std::bad_cast)
                   {
		        throw WrongColumnType(name());
                   }
               }

   }

   // get a single row from a vector column. There's no default row number, must
   // be specified.
   template <typename S>
   void Column::read(std::valarray<S>& vals, long row) 
   {
           read(vals,row,static_cast<S*>(0));
   }
   template <typename S>
   void Column::read(std::vector<S>& vals, long row) 
   {
           read(vals,row,static_cast<S*>(0));
   }

   template <typename S>
   void Column::read(std::vector<S>& vals, long row, S* nullValue) 
   {
           if (row > parent()->rows())
           {
              throw Column::InvalidRowNumber(name());
           }
           parent()->makeThisCurrent();                
           // isRead() returns true if the data were read in the ctor.
           if ( ColumnVectorData<S>* col = dynamic_cast<ColumnVectorData<S>*>(this))
           {
                   // fails if user requested outputType different from input type.


                   if (!isRead()) col->readRow(row,nullValue);
                   FITSUtil::fill(vals,col->data(row));
           }
           else
           {
                   FITSUtil::MatchType<S> outputType;
                   if ( outputType() == type() ) 
                   { 
                           // in this case user tried to read vector row from scalar column.
                           // one could be charitable and return a valarray of size 1,
                           // but... I'm going to throw an exception suggesting the user
                           // might not have meant that.

                           throw Column::WrongColumnType(name());
                   }

                   // the InvalidDataType exception should not be possible.
                   try
                   {
                       // about exceptions. The dynamic_casts could throw
                       // std::bad_cast. If this happens something is seriously
                       // wrong since the Column stores the value of type() 
                       // appropriate to each of the casts on construction.
                       // 
                       // the InvalidDataType exception should not be possible.
                       if  ( type() == Tdouble || type() == VTdouble )
		       {
                               ColumnVectorData<double>& col 
                                         = dynamic_cast<ColumnVectorData<double>&>(*this);
                               if (!isRead()) col.readRow(row);                                  
                               FITSUtil::fill(vals,col.data(row));

                       }
		       else if (type() == Tfloat  || type() == VTfloat )
		       { 
                               ColumnVectorData<float>& col 
                                     = dynamic_cast<ColumnVectorData<float>&>(*this);
                               if (!isRead()) col.readRow(row); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tint  || type() == VTint )
		       {
                               int nullVal(0);
                               if (nullValue) nullVal = static_cast<int>(*nullValue); 
                               ColumnVectorData<int>& col  
                                       = dynamic_cast<ColumnVectorData<int>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
		       }
		       else if (type() == Tshort  || type() == VTshort  )
                       {
                               short nullVal(0);
                               if (nullValue) nullVal = static_cast<short>(*nullValue); 
                               ColumnVectorData<short>& col 
                                       = dynamic_cast<ColumnVectorData<short>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tlong  || type() == VTlong )
		       {	
                               long nullVal(0);
                               if (nullValue) nullVal = static_cast<long>(*nullValue); 
                               ColumnVectorData<long>& col 
                                       = dynamic_cast<ColumnVectorData<long>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tlonglong  || type() == VTlonglong )
		       {	
                               LONGLONG nullVal(0);
                               if (nullValue) nullVal = static_cast<LONGLONG>(*nullValue); 
                               ColumnVectorData<LONGLONG>& col 
                                       = dynamic_cast<ColumnVectorData<LONGLONG>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tlogical  || type() == VTlogical )
		       {	
                               bool nullVal(0);
                               if (nullValue) nullVal = static_cast<bool>(*nullValue); 
                               ColumnVectorData<bool>& col 
                                       = dynamic_cast<ColumnVectorData<bool>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
		       }
		       else if (type() == Tbit || type() == Tbyte ||  
                               type() == VTbit || type() == VTbyte )
		       {
                               unsigned char nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned char>(*nullValue); 
                               ColumnVectorData<unsigned char>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned char>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tushort || type() == VTushort)
                       {
                               unsigned short nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned short>(*nullValue); 
                               ColumnVectorData<unsigned short>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned short>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tuint || type() == VTuint)
                       {
                               unsigned int nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned int>(*nullValue); 
                               ColumnVectorData<unsigned int>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned int>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
		       }
		       else if (type() == Tulong || type() == VTulong)
                       {
                               unsigned long nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned long>(*nullValue); 
                               ColumnVectorData<unsigned long>& col 
                                       = dynamic_cast<ColumnVectorData<unsigned long>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else
                       {
                               throw InvalidDataType(name());

                       }

                   }
                   catch (std::bad_cast)
                   {
                       throw WrongColumnType(name());
                   }     
            }
   }

   template <typename S>
   void Column::read(std::valarray<S>& vals, long row, S* nullValue) 
   {
           if (row > parent()->rows())
           {
              throw Column::InvalidRowNumber(name());
           }
           parent()->makeThisCurrent();                
           // isRead() returns true if the data were read in the ctor.
           if ( ColumnVectorData<S>* col = dynamic_cast<ColumnVectorData<S>*>(this))
           {
                   // fails if user requested outputType different from input type.



                   // input and output are both valarrays. Since one should not
                   // be able to call a constructor for a non-numeric valarray type,
                   // there shouldn't be any InvalidType problems. However, there
                   // is still the vector/scalar possibility and the implicit
                   // conversion request to deal with.

                   if (!isRead()) col->readRow(row,nullValue);
                   FITSUtil::fill(vals,col->data(row));
           }
           else
           {
                   FITSUtil::MatchType<S> outputType;
                   if ( outputType() == type() ) 
                   { 
                           // in this case user tried to read vector row from scalar column.
                           // one could be charitable and return a valarray of size 1,
                           // but... I'm going to throw an exception suggesting the user
                           // might not have meant that.

                           throw Column::WrongColumnType(name());
                   }

                   // the InvalidDataType exception should not be possible.
                   try
                   {
                       // about exceptions. The dynamic_casts could throw
                       // std::bad_cast. If this happens something is seriously
                       // wrong since the Column stores the value of type() 
                       // appropriate to each of the casts on construction.
                       // 
                       // the InvalidDataType exception should not be possible.
                       if  ( type() == Tdouble || type() == VTdouble )
		       {
                               ColumnVectorData<double>& col 
                                         = dynamic_cast<ColumnVectorData<double>&>(*this);
                               if (!isRead()) col.readRow(row);                                  
                               FITSUtil::fill(vals,col.data(row));

                       }
		       else if (type() == Tfloat  || type() == VTfloat )
		       { 
                               ColumnVectorData<float>& col 
                                     = dynamic_cast<ColumnVectorData<float>&>(*this);
                               if (!isRead()) col.readRow(row); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tint  || type() == VTint )
		       {
                               int nullVal(0);
                               if (nullValue) nullVal = static_cast<int>(*nullValue); 
                               ColumnVectorData<int>& col  
                                       = dynamic_cast<ColumnVectorData<int>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
		       }
		       else if (type() == Tshort  || type() == VTshort  )
                       {
                               short nullVal(0);
                               if (nullValue) nullVal = static_cast<short>(*nullValue); 
                               ColumnVectorData<short>& col 
                                       = dynamic_cast<ColumnVectorData<short>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tlong  || type() == VTlong )
		       {	
                               long nullVal(0);
                               if (nullValue) nullVal = static_cast<long>(*nullValue); 
                               ColumnVectorData<long>& col 
                                       = dynamic_cast<ColumnVectorData<long>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tlonglong  || type() == VTlonglong )
		       {	
                               LONGLONG nullVal(0);
                               if (nullValue) nullVal = static_cast<LONGLONG>(*nullValue); 
                               ColumnVectorData<LONGLONG>& col 
                                       = dynamic_cast<ColumnVectorData<LONGLONG>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tlogical  || type() == VTlogical )
		       {	
                               bool nullVal(0);
                               if (nullValue) nullVal = static_cast<bool>(*nullValue); 
                               ColumnVectorData<bool>& col 
                                       = dynamic_cast<ColumnVectorData<bool>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
		       }
		       else if (type() == Tbit || type() == Tbyte ||  
                               type() == VTbit || type() == VTbyte )
		       {
                               unsigned char nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned char>(*nullValue); 
                               ColumnVectorData<unsigned char>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned char>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tushort || type() == VTushort)
                       {
                               unsigned short nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned short>(*nullValue); 
                               ColumnVectorData<unsigned short>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned short>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else if (type() == Tuint || type() == VTuint)
                       {
                               unsigned int nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned int>(*nullValue); 
                               ColumnVectorData<unsigned int>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned int>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
		       }
		       else if (type() == Tulong || type() == VTulong)
                       {
                               unsigned long nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned long>(*nullValue); 
                               ColumnVectorData<unsigned long>& col 
                                       = dynamic_cast<ColumnVectorData<unsigned long>&>(*this);
                               if (!isRead()) col.readRow(row,&nullVal); 
                               FITSUtil::fill(vals,col.data(row));
                       }
		       else
                       {
                               throw InvalidDataType(name());

                       }

                   }
                   catch (std::bad_cast)
                   {
                       throw WrongColumnType(name());
                   }     
            }
   }

   template <typename S>
   void Column::readArrays(std::vector<std::valarray<S> >& vals, long first, long last)  
   {
           readArrays(vals,first,last,static_cast<S*>(0));
   }

   template <typename S>
   void Column::readArrays(std::vector<std::valarray<S> >& vals, 
                           long first, long last, S* nullValue)
   {

           parent()->makeThisCurrent();
           // again, can only call this if the entire column has been read from disk.
           // user expects 1 based indexing. If 0 based indices are supplied,
           // add one to both ranges.
           long range = numberOfElements(first,last);

           vals.resize(range);


           if ( ColumnVectorData<S>* col = dynamic_cast<ColumnVectorData<S>*>(this))
           {
                   for (int j = 0; j < range; ++j) 
                   {
                           if (!isRead()) col->readRow(j + first,nullValue);                             
                           FITSUtil::fill(vals[j],col->data(j+first));
                   }
           }
           else
           {
                   FITSUtil::MatchType<S> outputType;
                   if ( outputType() == type() ) 
                   { 
                           // in this case user tried to read vector data from scalar,
                           // (i.e. first argument was vector<valarray<S> >.
                           // since the cast won't fail on template parameter grounds.
                           throw Column::WrongColumnType(name());
                   }
                   // the InvalidDataType exception should not be possible.
                   try
                   {
                       if  ( type() == Tdouble || type() == VTdouble )
		       {
                               ColumnVectorData<double>& col 
                                       = dynamic_cast<ColumnVectorData<double>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tfloat || type() == VTfloat  )
		       {
                               ColumnVectorData<float>& col 
                                       = dynamic_cast<ColumnVectorData<float>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tint   || type() == VTint )
		       {
                               int nullVal(0);
                               if (nullValue) nullVal  = static_cast<int>(*nullValue); 
                               ColumnVectorData<int>& col  
                                       = dynamic_cast<ColumnVectorData<int>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tshort  || type() == VTshort )
		       {
                               short nullVal(0);
                               if (nullValue) nullVal  = static_cast<short>(*nullValue); 
                               ColumnVectorData<short>& col 
                                       = dynamic_cast<ColumnVectorData<short>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tlong   || type() == VTlong )
		       {
                               long nullVal(0);
                               if (nullValue) nullVal  = static_cast<long>(*nullValue); 
                               ColumnVectorData<long>& col 
                                       = dynamic_cast<ColumnVectorData<long>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tlonglong   || type() == VTlonglong )
		       {
                               LONGLONG nullVal(0);
                               if (nullValue) nullVal  = static_cast<LONGLONG>(*nullValue); 
                               ColumnVectorData<LONGLONG>& col 
                                       = dynamic_cast<ColumnVectorData<LONGLONG>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tlogical   || type() == VTlogical )
		       {
                               bool nullVal(0);
                               if (nullValue) nullVal   = static_cast<bool>(*nullValue); 
                               ColumnVectorData<bool>& col 
                                       = dynamic_cast<ColumnVectorData<bool>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
		       else if (type() == Tbit || type() == Tbyte ||  
                               type() == VTbit || type() == VTbyte )
		       {
                               unsigned char nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned char>(*nullValue); 
                               ColumnVectorData<unsigned char>& col 
                                      = dynamic_cast<ColumnVectorData<unsigned char>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
			       }
                       }                            
                       else if  ( type() == Tushort   || type() == VTushort )
		       {
                               unsigned short nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned short>(*nullValue); 
                               ColumnVectorData<unsigned short>& col 
                                   = dynamic_cast<ColumnVectorData<unsigned short>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tuint   || type() == VTuint )
		       {
                               unsigned int nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned int>(*nullValue); 
                               ColumnVectorData<unsigned int>& col 
                                       = dynamic_cast<ColumnVectorData<unsigned int>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       }
                       else if  ( type() == Tulong   || type() == VTulong  )
		       {
                               unsigned long nullVal(0);
                               if (nullValue) nullVal 
                                           = static_cast<unsigned long>(*nullValue); 
                               ColumnVectorData<unsigned long>& col 
                                     = dynamic_cast<ColumnVectorData<unsigned long>&>(*this);
                               for (int j = 0; j < range; ++j) 
                               {
                                   if (!isRead()) col.readRow(j + first,&nullVal); 
                                   FITSUtil::fill(vals[j],col.data(j+first));
                               }
                       } 
                       else
                       {
                               throw InvalidDataType(name());
                       }

                   }
                   catch (std::bad_cast)
                   {
                       throw WrongColumnType(name());

                   }     

           }        
   }

   template <typename S>                   
   void Column::write (const std::vector<S>& indata, long firstRow)
   {
      // nullValue is now a pointer, so this is ok. 
      // got to cast the 0 to a pointer to S to avoid
      // overloading ambiguities.      
      write(indata,firstRow,static_cast<S*>(0));
   }

   template <typename S>                   
   void Column::write (const std::valarray<S>& indata, long firstRow)
   {
      size_t n(indata.size());
      std::vector<S> __tmp(n);
      for (size_t j = 0; j < n; ++j) __tmp[j] = indata[j];
      write(__tmp,firstRow,static_cast<S*>(0));
   }

   template <typename S>                   
   void Column::write (S* indata, long nRows, long firstRow)
   {
      write(indata,nRows,firstRow,static_cast<S*>(0));                
   }


   template <typename S>                   
   void Column::write (const std::vector<S>& indata, long firstRow, S* nullValue)
   {
      // although underlying code needs to convert the input vector
      // into a C array, this must be the underlying implementation
      // [which the others call] because it accepts string arguments
      // which the version with a pointer won't. [no version that
      // translates to a char** argument].


      parent()->makeThisCurrent();
      firstRow = std::max(firstRow,static_cast<long>(1));
      if (ColumnData<S>* col = dynamic_cast<ColumnData<S>*>(this))
      {
         col->writeData(indata,firstRow,nullValue);
      }
      else
      {
         // alright, input data type has to be rewritten as output
         // data type.
         FITSUtil::MatchType<S> inType;
         if ( inType() == type()) 
         {
                 String msg("Incorrect call: writing to vector column ");
                 msg += name();
                 msg += " requires specification of # rows or vector lengths";
                 throw WrongColumnType(msg);
         }
         else
         {
             if  ( type() == Tdouble )
	     {
                 ColumnData<double>& col 
                         = dynamic_cast<ColumnData<double>&>(*this);
                 std::vector<double> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow);
             }
             else if  ( type() == Tfloat )
	     {
                 ColumnData<float>& col 
                         = dynamic_cast<ColumnData<float>&>(*this);
                 std::vector<float> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow);
             }
             else if  ( type() == Tint )
	     {
                 int nullVal = 0;
                 int* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<int>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 if (nullValue) nullVal = static_cast<int>(*nullValue); 
                 ColumnData<int>& col  
                         = dynamic_cast<ColumnData<int>&>(*this);
                 std::vector<int> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tshort )
	     {
                 short nullVal(0);
                 short* pNullVal = 0;
                 if (nullValue) 
                 {
                    nullVal = static_cast<short>(*nullValue); 
                    pNullVal = &nullVal;
                 }
                 ColumnData<short>& col 
                         = dynamic_cast<ColumnData<short>&>(*this);
                 std::vector<short> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tlong )
	     {
                 long nullVal(0);
                 long* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<long>(*nullValue); 
                    pNullVal = &nullVal;
                 }
                 ColumnData<long>& col 
                         = dynamic_cast<ColumnData<long>&>(*this);
                 std::vector<long> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tlonglong )
	     {
                 LONGLONG nullVal(0);
                 LONGLONG* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<LONGLONG>(*nullValue); 
                    pNullVal = &nullVal;
                 }
                 ColumnData<LONGLONG>& col 
                         = dynamic_cast<ColumnData<LONGLONG>&>(*this);
                 std::vector<LONGLONG> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tlogical )
	     {
                 bool nullVal(0);
                 bool* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<bool>(*nullValue); 
                    pNullVal = &nullVal;
                 }
                 ColumnData<bool>& col 
                         = dynamic_cast<ColumnData<bool>&>(*this);
                 std::vector<bool> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tbyte )
	     {
                 unsigned char nullVal(0);
                 unsigned char* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned char>(*nullValue); 
                    pNullVal = &nullVal;
                 }
                 ColumnData<unsigned char>& col 
                         = dynamic_cast<ColumnData<unsigned char>&>(*this);
                 std::vector<unsigned char> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }                            
             else if  ( type() == Tushort )
	     {
                 unsigned short nullVal(0);
                 unsigned short* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned short>(*nullValue);
                    pNullVal = &nullVal;
                 } 
                 ColumnData<unsigned short>& col 
                         = dynamic_cast<ColumnData<unsigned short>&>(*this);
                 std::vector<unsigned short> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tuint )
	     {
                 unsigned int nullVal(0);
                 unsigned int* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned int>(*nullValue);
                    pNullVal = &nullVal;
                 } 
                 ColumnData<unsigned int>& col 
                         = dynamic_cast<ColumnData<unsigned int>&>(*this);
                 std::vector<unsigned int> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tulong )
	     {
                 unsigned long nullVal(0);
                 unsigned long* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned long>(*nullValue); 
                    pNullVal = &nullVal;
                 }
                 ColumnData<unsigned long>& col 
                         = dynamic_cast<ColumnData<unsigned long>&>(*this);
                 std::vector<unsigned long> __tmp;
                 FITSUtil::fill(__tmp,indata,1,indata.size());
                 col.writeData(__tmp,firstRow,pNullVal);
             } 
             else
             {
                     throw InvalidDataType(name());
             }
         }
      }
   }


   template <typename S>                   
   void Column::write (const std::valarray<S>& indata, long firstRow, S* nullValue)
   {
      // for scalar columns.        
      std::vector<S> __tmp;
      FITSUtil::fill(__tmp,indata);    
      write(__tmp,firstRow,nullValue);          
   }

   template <typename S>                   
   void Column::write (S* indata, long nRows, long firstRow, S* nullValue)
   {
      // for scalar columns, data specified with C array
      if (nRows <= 0) throw InvalidNumberOfRows(nRows);
      std::vector<S> __tmp(nRows);
      std::copy(&indata[0],&indata[nRows],__tmp.begin());
      write(__tmp,firstRow, nullValue);

   }

   template <typename S>
   void Column::write (const std::valarray<S>& indata, const std::vector<long>& vectorLengths,  
                           long firstRow)
   {
      // variable length arrays written from an input valarray.
      // does not allow NULL value.

      using namespace std;
      const size_t nRows = vectorLengths.size();
      // Calculate the sums of the vector lengths first simply to do a 
      //   check against the size of indata.
      vector<long> sums(nRows+1);
      sums[0] = 0;
      vector<long>::iterator itSums = sums.begin() + 1;
      partial_sum(vectorLengths.begin(), vectorLengths.end(), itSums);
      if (indata.size() < static_cast<size_t>(sums[nRows]))
      {
#ifdef SSTREAM_DEFECT
         ostrstream msgStr;
#else
         ostringstream msgStr;
#endif            
         msgStr << " input data size: " << indata.size() << " vector length sum: " << sums[nRows];
#ifdef SSTREAM_DEFECT
         msgStr << std::ends;
#endif            

         String msg(msgStr.str());
         throw Column::InsufficientElements(msg);     
      }
      vector<valarray<S> > vvArray(nRows);
      for (size_t iRow=0; iRow<nRows; ++iRow)
      {
         valarray<S>& vArray = vvArray[iRow];
         long first = sums[iRow];
         long last = sums[iRow+1];
         vArray.resize(last - first);
         for (long iElem=first; iElem<last; ++iElem)
         {
            vArray[iElem - first] = indata[iElem];
         }
      }
      writeArrays(vvArray, firstRow, static_cast<S*>(0));  
   }

template <typename S>
void Column::write (const std::vector<S>& indata,const std::vector<long>& vectorLengths,
                      long firstRow)
   {
      // variable length write
      // implement as valarray version
      std::valarray<S> __tmp(indata.size());
      std::copy(indata.begin(),indata.end(),&__tmp[0]);
      write(__tmp,vectorLengths,firstRow);  

   }

   template <typename S>
   void Column::write (S* indata, long nelements, const std::vector<long>& vectorLengths,
                                   long firstRow)
   {
      // implement as valarray version, which will also check array size.
      std::valarray<S> __tmp(indata,nelements);
      write(__tmp,vectorLengths,firstRow);
   }        

   template <typename S>
   void Column::write (const std::valarray<S>& indata, long nRows, long firstRow)
   {
      write(indata,nRows,firstRow,static_cast<S*>(0));
   }

   template <typename S>
   void Column::write (const std::vector<S>& indata, long nRows, long firstRow)
   {
      write(indata,nRows,firstRow,static_cast<S*>(0));  
   }

   template <typename S>
   void Column::write (S* indata, long nelements, long nRows, long firstRow)
   {
      write(indata,nelements,nRows,firstRow,static_cast<S*>(0));              
   }        



   template <typename S>
   void Column::write (const std::valarray<S>& indata, long nRows, long firstRow,
                           S* nullValue)
   {
      // Write equivalent lengths of data to rows of a vector column.
      // The column may be either fixed or variable width, but if it's fixed
      // width the lengths must equal the column's repeat value or an
      // exception is thrown.
      if (nRows <= 0)
         throw InvalidNumberOfRows(nRows);
      firstRow = std::max(firstRow,static_cast<long>(1));
#ifdef SSTREAM_DEFECT
      std::ostrstream msgStr;
#else
      std::ostringstream msgStr;
#endif 
      const size_t numRows = static_cast<size_t>(nRows);           
      if (indata.size() % numRows)
      {
         msgStr << "To use this write function, input array size"
            <<"\n must be exactly divisible by requested num rows: "
            << nRows;
         throw InsufficientElements(msgStr.str());
      }
      
      const size_t cellsize = indata.size()/numRows;
      if (!varLength() && cellsize != repeat() )
      {      
         msgStr << "column: " << name() 
                <<  "\n input data size: " << indata.size() 
                << " required: " << nRows*repeat();
         String msg(msgStr.str());
         throw InsufficientElements(msg);     
      }
      
      std::vector<std::valarray<S> > vvArray(numRows);
      for (size_t i=0; i<numRows; ++i)
      {
         vvArray[i].resize(cellsize);
         vvArray[i] = indata[std::slice(cellsize*i,cellsize,1)];
      }
      writeArrays(vvArray, firstRow, nullValue);
   }

   template <typename S>
   void Column::write (const std::vector<S>& indata, long nRows, long firstRow, S* nullValue)
   {
      // fixed length write of vector
      // implement as valarray version
      if (nRows <= 0) throw InvalidNumberOfRows(nRows);
      std::valarray<S> __tmp(indata.size());
      std::copy(indata.begin(),indata.end(),&__tmp[0]);
      write(__tmp,nRows,firstRow, nullValue);  
   }

   template <typename S>
   void Column::write (S* indata, long nelements, long nRows, long firstRow, S* nullValue)
   {
      // fixed length write of C-array
      // implement as valarray version
      if (nRows <= 0) throw InvalidNumberOfRows(nRows);
      std::valarray<S> __tmp(indata,nelements);
      write(__tmp,nRows,firstRow, nullValue);              
   }        


   template <typename S>
   void Column::writeArrays (const std::vector<std::valarray<S> >& indata, long firstRow)
   {
      // vector<valarray>, no null value. 
      writeArrays(indata,firstRow,static_cast<S*>(0));
   } 

   template <typename S>
   void Column::writeArrays (const std::vector<std::valarray<S> >& indata, long firstRow,
                                   S* nullValue)
   {
      // vector<valarray>, null value. primary


      using std::valarray;
      using std::vector;
      parent()->makeThisCurrent();
      firstRow = std::max(firstRow,static_cast<long>(1));
      if (ColumnVectorData<S>* col = dynamic_cast<ColumnVectorData<S>*>(this))
      {
          col->writeData(indata,firstRow,nullValue);
      }
      else
      {
         // alright, input data type has to be rewritten as output
         // data type.
         FITSUtil::MatchType<S> inType;
         if ( inType() == type()) 
         {
                 String msg("Incorrect call: writing vectors to scalar column ");
                 throw WrongColumnType(msg);
         }
         else
         {
             size_t n(indata.size());                            
             if  ( type() == Tdouble || type() == VTdouble)
	     {
                 ColumnVectorData<double>& col 
                         = dynamic_cast<ColumnVectorData<double>&>(*this);
                 vector<valarray<double> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }
                 col.writeData(__tmp,firstRow);
             }
             else if  ( type() == Tfloat || type() == VTfloat)
	     {
                 ColumnVectorData<float>& col 
                         = dynamic_cast<ColumnVectorData<float>&>(*this);
                 vector<valarray<float> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow);
             }
             else if  ( type() == Tint || type() == VTint)
	     {
                 ColumnVectorData<int>& col  
                         = dynamic_cast<ColumnVectorData<int>&>(*this);
                 vector<valarray<int> > __tmp(n);
                 int nullVal(0);
                 int* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<int>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tshort || type() == VTshort)
	     {
                 ColumnVectorData<short>& col 
                         = dynamic_cast<ColumnVectorData<short>&>(*this);
                 vector<valarray<short> > __tmp(n);
                 short nullVal(0);
                 short* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<short>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tlong || type() == VTlong)
	     {
                 ColumnVectorData<long>& col 
                         = dynamic_cast<ColumnVectorData<long>&>(*this);
                 vector<valarray<long> > __tmp(n);
                 long nullVal(0);
                 long* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<long>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tlonglong || type() == VTlonglong)
	     {
                 ColumnVectorData<LONGLONG>& col 
                         = dynamic_cast<ColumnVectorData<LONGLONG>&>(*this);
                 vector<valarray<LONGLONG> > __tmp(n);
                 LONGLONG nullVal(0);
                 LONGLONG* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<LONGLONG>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tlogical || type() == VTlogical)
	     {
                 ColumnVectorData<bool>& col 
                         = dynamic_cast<ColumnVectorData<bool>&>(*this);
                 bool nullVal(0);
                 bool* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<bool>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 vector<valarray<bool> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tbyte || type() == VTbyte)
	     {
                 ColumnVectorData<unsigned char>& col 
                         = dynamic_cast<ColumnVectorData<unsigned char>&>(*this);
                 unsigned char nullVal(0);
                 unsigned char* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned char>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 vector<valarray<unsigned char> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }                            
             else if  ( type() == Tushort || type() == VTushort)
	     {
                 ColumnVectorData<unsigned short>& col 
                         = dynamic_cast<ColumnVectorData<unsigned short>&>(*this);
                 unsigned short nullVal(0);
                 unsigned short* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned short>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 vector<valarray<unsigned short> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tuint || type() == VTuint)
	     {
                 ColumnVectorData<unsigned int>& col 
                         = dynamic_cast<ColumnVectorData<unsigned int>&>(*this);
                 unsigned int nullVal(0);
                 unsigned int* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned int>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 vector<valarray<unsigned int> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             }
             else if  ( type() == Tulong || type() == VTulong)
	     {
                 ColumnVectorData<unsigned long>& col 
                         = dynamic_cast<ColumnVectorData<unsigned long>&>(*this);
                 unsigned long nullVal(0);
                 unsigned long* pNullVal = 0;
                 if (nullValue)
                 {
                    nullVal = static_cast<unsigned long>(*nullValue);
                    pNullVal = &nullVal;
                 }
                 vector<valarray<unsigned long> > __tmp(n);
                 for (size_t i = 0; i < n; ++i)
                 {
                         FITSUtil::fill(__tmp[i],indata[i]);
                 }                                
                 col.writeData(__tmp,firstRow,pNullVal);
             } 
             else
             {
                     throw InvalidDataType(name());
             }
         }
      }
   } 


   template <typename T>
   void Column::addNullValue(T nullVal)
   {
      parent()->makeThisCurrent();
      int status(0);
#ifdef SSTREAM_DEFECT
      std::ostrstream keyName;
      keyName << "TNULL" << index() << std::ends;
      char* nullKey = const_cast<char*>(keyName.str());
#else
      std::ostringstream keyName;          
      keyName << "TNULL" << index();
      String keyNameStr = keyName.str();
      char* nullKey = const_cast<char*>(keyNameStr.c_str());
#endif


      FITSUtil::MatchType<T> inputType;
      int dataType = static_cast<int>(inputType());
      if (dataType == static_cast<int>(Tstring))
         throw InvalidDataType("attempting to set TNULLn to a string.");

      // update key but don't add to keyword list because it's really a column
      // property not a table metadata property. And it needs to be automatically
      // renumbered if columns are inserted or deleted.
      if (fits_update_key(fitsPointer(),dataType,nullKey,&nullVal,0,&status))
              throw FitsError(status);

      // The following is called to make sure the HDU struct is immediately 
      // updated in case a column write operation is performed shortly after this
      // function exits. 
      if (fits_set_hdustruc(fitsPointer(),&status)) throw FitsError(status); 

   }

   template <typename T>
   bool Column::getNullValue(T* nullVal) const
   {
      parent()->makeThisCurrent();
#ifdef SSTREAM_DEFECT
      std::ostrstream keyName;
      keyName << "TNULL" << index() << std::ends;
      char* nullKey = const_cast<char*>(keyName.str());
#else
      std::ostringstream keyName;          
      keyName << "TNULL" << index();
      String keyNameStr = keyName.str();
      char* nullKey = const_cast<char*>(keyNameStr.c_str());
#endif

      int status=0;
      FITSUtil::MatchType<T> inputType;
      int dataType = static_cast<int>(inputType());
      if (dataType == static_cast<int>(Tstring))
         throw InvalidDataType("attempting to read TNULLn into a string.");
      T tmpVal(*nullVal);

      bool keyExists = false;
      if (fits_read_key(m_parent->fitsPointer(), dataType, nullKey, &tmpVal, 0, &status))
      {
         if (status == KEY_NO_EXIST  || status == VALUE_UNDEFINED)
            return keyExists;
         else
            throw FitsError(status);
      }
      keyExists = true;
      *nullVal = tmpVal;       
      return keyExists;
   }

} // namespace CCfits

#endif
