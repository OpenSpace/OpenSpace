//	Astrophysics Science Division,
//	NASA/ Goddard Space Flight Center
//	HEASARC
//	http://heasarc.gsfc.nasa.gov
//	e-mail: ccfits@legacy.gsfc.nasa.gov
//
//	Original author: Ben Dorman

// Column
#include "Column.h"
// Table
#include "Table.h"
// ColumnData
#include "ColumnData.h"
// ColumnVectorData
#include "ColumnVectorData.h"
// ColumnCreator
#include "ColumnCreator.h"

#include <stdlib.h>
#include <complex>
using std::complex;


namespace CCfits {

  // Class CCfits::ColumnCreator 

  ColumnCreator::ColumnCreator (Table* p)
        : m_parent(p)
  {
  }


  ColumnCreator::~ColumnCreator()
  {
  }


  Column * ColumnCreator::MakeColumn (const int index, const String &name, const String &format, const String &unit, const long repeat, const long width, const String &comment, const int decimals)
  {
    return 0;
  }

  Column * ColumnCreator::getColumn (int number, const String& name, const String& format, const String& unit)
  {
   long  repeat=1;
   long  width=1;
   int   type=0;
   double tscale = 1;
   double tzero  = 0;

   getScaling(number, type, repeat, width, tscale, tzero);
   return createColumn(number,ValueType(type),name,format,unit,repeat,width,tscale,tzero);
  }

  Column * ColumnCreator::createColumn (int number, ValueType type, const String &name, const String &format, const String &unit, long repeat, long width, double scaleFactor, double offset, const String &comment)
  {



   switch(type)
   {
      case Tstring:
                if (repeat/width > 1)
                {
                   // A vector column of strings.  Not currently supported in CCfits,
                   //  but for backward compatibility do not throw -- user may be
                   //  loading a table with no intention of reading or writing to the
                   //  vector string column.
                   std::cerr << "\n***CCfits Warning: Column " << number << " is detected to be a vector column of strings.\n"
                     <<"      CCfits does not currently support reading and writing to vector string columns.\n"<<std::endl;
                   
                }
      case VTstring:
	        m_column = new ColumnData<String>(number, name, type, format, unit, 
                                                                             m_parent, 1, width);
	        break;
      case VTushort:
	        m_column = new ColumnVectorData<unsigned short>(number, name,  
                                        type, format,unit,  m_parent, repeat); 
                m_column->zero(USBASE);
                m_column->scale(1);
                break;      
      case Tushort:
                if (repeat == 1)
                {
	                m_column = new ColumnData<unsigned short>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<unsigned short>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                m_column->zero(USBASE);
                m_column->scale(1);
                break;
      case VTshort:
	        m_column = new ColumnVectorData<short>(number, name, 
                                	type, format,unit,  m_parent, repeat);  
                break;      
      case Tshort:
                if (repeat == 1)
                {
	                m_column = new ColumnData<short>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<short>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      case VTlogical:
	        m_column = new ColumnVectorData<bool>(number, name,  
                                        type, format,unit,  m_parent, repeat); 
                break;                 
      case Tlogical:  
               if (repeat == 1)
                {
	                m_column = new ColumnData<bool>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<bool>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      case VTbyte:
      case VTbit:
                m_column = new ColumnVectorData<unsigned char>(number, name,  
                                        type, format,unit,  m_parent, repeat);     
                break;     
      case Tbit:
      case Tbyte:
                if (repeat == 1)
                {
	                m_column = new ColumnData<unsigned char>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<unsigned char>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      case VTint:	                
                m_column = new ColumnVectorData<int>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        	
                break;
      case Tint:
                if (repeat == 1)
                {
	                m_column = new ColumnData<int>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<int>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      case VTuint:
	        m_column = new ColumnVectorData<unsigned int>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        	
                m_column->zero(ULBASE);
                m_column->scale(1);
                break;
      case Tuint:
                if (repeat == 1)
                {
	                m_column = new ColumnData<unsigned int>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<unsigned int>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                m_column->zero(ULBASE);
                m_column->scale(1);
                break;
      case VTlong:
	        m_column = new ColumnVectorData<long>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        	
                break;
      case Tlong:
                if (repeat == 1)
                {
	                m_column = new ColumnData<long>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<long>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      case VTlonglong:
                m_column = new ColumnVectorData<LONGLONG>(number, name,  
                                        type, format,unit,  m_parent, repeat);
                break;
      case Tlonglong:
                if (repeat == 1)
                {
	                m_column = new ColumnData<LONGLONG>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<LONGLONG>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;      
      case VTulong:          
	        m_column = new ColumnVectorData<unsigned long>(number, name,  
                                        type, format,unit,  m_parent, repeat);   
                m_column->zero(ULBASE);
                m_column->scale(1);                                  
                break;
      case Tulong:
                if (repeat == 1)
                {
	                m_column = new ColumnData<unsigned long>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<unsigned long>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                m_column->zero(ULBASE);
                m_column->scale(1);
                break;
      case VTfloat:
	        m_column = new ColumnVectorData<float>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                break;
      case Tfloat:
                if (repeat == 1)
                {
	                m_column = new ColumnData<float>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<float>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      case VTdouble:
	        m_column = new ColumnVectorData<double>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        	
                break;      
      case Tdouble:
                if (repeat == 1)
                {
	                m_column = new ColumnData<double>(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<double>(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
       case VTcomplex:
	        m_column = new ColumnVectorData<complex<float> >(number, name,  
                                        type, format,unit,  m_parent, repeat);                        	
                break;
       case Tcomplex:
                if (repeat == 1)
                {
	                m_column = new ColumnData<complex<float> >(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<complex<float> >(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
       case VTdblcomplex:
	        m_column = new ColumnVectorData<complex<double> >(number, name,  
                                        type, format,unit,  m_parent, repeat);                        	
                break;
       case Tdblcomplex:
                if (repeat == 1)
                {
	                m_column = new ColumnData<complex<double> >(number, name,  
                                        type, format,unit,  m_parent);
                }
                else
                {
	                m_column = new ColumnVectorData<complex<double> >(number, name,  
                                        type, format,unit,  m_parent, repeat);                        
                }	
                break;
      default:
         // replace with exception.
         throw FitsFatal("Unknown ValueType in ColumnCreator");
   }    
   if ( scaleFactor != 1)
   {
        m_column->scale(scaleFactor);
        m_column->zero(offset); 
   }   
   return m_column;
  }

  void ColumnCreator::getScaling (int index, int& type, long& repeat, long& width, double& tscale, double& tzero)
  {
      int status (0);      


      if (fits_get_coltype(m_parent->fitsPointer(), index, &type, &repeat, &width, &status)) 
      { 
              throw FitsError(status);
      }

      int absType (std::abs(type)) ;


      FITSUtil::auto_array_ptr<char> pKeyname( new char[FLEN_KEYWORD]);
      char* keyname = pKeyname.get();
      sprintf(keyname, "%s%d",Column::TSCAL().c_str(),index);
      bool scalePresent (!fits_read_key(m_parent->fitsPointer(),Tdouble,keyname,&tscale,0,&status));

      // reset the status flag if there was no scale factor so cfitsio will not
      // propagate the error code.
      if (!scalePresent) status = 0;
      sprintf(keyname, "%s%d", Column::TZERO().c_str(),index);
      fits_read_key(m_parent->fitsPointer(),Tdouble,keyname,&tzero,0,&status);
      // if there is no BSCALE key present or BSCALE == 1 ... 
      if (!scalePresent || (scalePresent &&  tscale == 1))
      {
           if ( !status )
           {
                   switch (absType)
                   {
                           case Tshort:
                                  if (tzero == USBASE)
                                  { 
                                        type > 0 ? type = Tushort : type = VTushort ;
                                  }
                                  break;
                           case Tint:
                                   if (tzero == ULBASE)
                                   {
                                        type > 0 ? type = Tuint : type = VTuint ;
                                   }
                                   break;
                           case Tlong:
                                   if (tzero == ULBASE)
                                   {
                                        type > 0 ? type = Tulong : type = VTulong ;
                                   }
                                   break;
                           default:
                                   break;
                   }
           }       
     }
     else // scale present and tmpScale != 1.
     {
             switch (absType)
             {
                 case Tbit:
                 case Tbyte:
                 case Tint:
                 case Tshort:
                         type > 0 ? type = Tfloat: type = VTfloat;
                         break;
                 case Tlong:
                 case Tlonglong:
                         type > 0 ? type = Tdouble: type = VTdouble;
                         break; 
                 default:
                         break; // do nothing.    
             }
     }                
  }

  // Additional Declarations

} // namespace CCfits
