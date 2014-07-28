KPL/FK
 
 
Cassini Spacecraft Frame Definitions Kernel
==============================================================================
 
   This frame kernel contains the Cassini spacecraft, science instrument, and
   communication antennae frame definitions.
 
 
Version and Date
----------------------------------------------------------
 
   The TEXT_KERNEL_ID stores version information of loaded project text
   kernels. Each entry associated with the keyword is a string that consists
   of four parts: the kernel name, version, entry date, and type. For example,
   the ISS I-kernel might have an entry as follows:
 
           TEXT_KERNEL_ID += 'CASSINI_ISS V0.0.0 29-SEPTEMBER-1999 IK'
                                  |          |         |            |
                                  |          |         |            |
              KERNEL NAME <-------+          |         |            |
                                             |         |            V
                             VERSION <-------+         |       KERNEL TYPE
                                                       |
                                                       V
                                                  ENTRY DATE
 
   Cassini Frame Kernel Version:
 
           \begindata
 
           TEXT_KERNEL_ID += 'CASSINI_FRAMES V3.7.0 20-NOVEMBER-2003 FK'
 
           \begintext
 
   Version 3.7 -- November 20, 2003 -- Lee Elson
 
            --   Updated CASSINI_XBAND per Diane Conner's request email. This
                 was done by modifying the CASSINI_KABAND boresight vector.
                 See [48] for details.
 
   Version 3.6 -- April 18, 2003 -- Lee Elson
 
            --   Modified CASSINI_XBAND frame definition so that its values
                 are the same as CASSINI_KABAND. Added a new frame called
                 CASSINI_XBAND_TRUE (NAIF ID -82108) that has the same
                 definition parameters as the old CASSINI_XBAND. Also modified
                 text structure so that changes are dated and stand out better
                 for the human reader. See [46] and [47] for details.
 
            --   Modified descriptive text structure so that changes are dated
                 and stand out better for the human reader.
 
   Version 3.5 -- September 4, 2002 -- Scott Turner, Richard West, and Rick
   McCloskey
 
            --   Entries for CASSINI_VIMS_IR_SOL, CASSINI_RADAR_2, and
                 CASSINI_RADAR_4 were updated to reflect current values. See
                 [42], [43], and [44] for details.
 
            --   CASSINI_VIMS_IR_SOL is now referenced directly to the
                 spacecraft frame, CASSINI_SC_COORD, rather than
                 CASSINI_VIMS_IR.
 
   Version 3.4 -- June 11, 2002 -- Scott Turner and Joshua Colwell
 
            --   Entries for CASSINI_KABAND were updated per Diane Conner's
                 request email. See [38] for details.
 
            --   Updated entries for CASSINI_CIRS_FPB, CASSINI_CIRS_FP1,
                 CASSINI_CIRS_FP3, and CASSINI_CIRS_FP4 based on updated
                 alignment information provided in ECR 100515. See [39] for
                 details.
 
            --   Modified the entries for CASSINI_UVIS_HSP, CASSINI_UVIS_FUV,
                 and CASSINI_UVIS_EUV to match the body vector table provided
                 by Alain Jouchoux in May 3, 2002 e-mail. This same data set
                 is in the CASPER UVIS definition file. Verified that CASPER
                 and PDT give consistent results using this frame kernel.
 
            --   Updated the entry for CASSINI_VIMS_IR to match the body
                 vector table entry provided by Rick McCloskey. See [41] for
                 details.
 
   Version 3.3 -- February 20, 2002 -- Scott Turner
 
            --   Updated the frame entry and documentation for
                 CASSINI_VIMS_RAD as a result of ECR 101029 and documentation
                 submitted with it. See [36] for details.
 
   Version 3.2 -- January 22, 2002 -- Scott Turner
 
            --   Updated frame entries for CASSINI_XBAND and CASSINI_KABAND as
                 a result of SCR 490. See [35] for details.
 
   Version 3.1 -- August 9, 2001 -- Scott Turner
 
            --   Updated frame entries for CASSINI_XBAND and CASSINI_KABAND as
                 a result of SCR 468. See [34] for details.
 
   Version 3.0 -- April 23, 2001 -- Scott Turner
 
            --   Restructured the articulating frames: CASSINI_MIMI_LEMMS1,
                 CASSINI_MIMI_LEMMS2, CASSINI_CDA, CASSINI_CAPS. They now
                 allow for multiple paths from the instrument frames to the
                 spacecraft frame depending on what C-kernels are available.
                 Use caution when loading conflicting C-kernels.
 
            --   Renamed CASSINI_SRU to CASSINI_SRU-A and added CASSINI_SRU-B,
                 CASSINI_SRU-A_RAD, and CASSINI_SRU-B_RAD.
 
            --   Added the CASSINI_UVIS_SOLAR frame definition to support the
                 UVIS solar occultation port FOV.
 
            --   Removed CASSINI_UVIS_EUV_OCC and CASSINI_UVIS_FUV_OCC frame
                 definitions since the FOVs they support are actually tied to
                 CASSINI_UVIS_EUV and CASSINI_UVIS_FUV frames respectively.
 
            --   Added the CASSINI_VIMS_IR_SOL frame definition to support the
                 IR channel solar port FOV.
 
   Version 2.9 -- November 16, 2000 -- Scott Turner
 
            --   Corrected the definition of CASSINI_MIMI_INCA to account for
                 the 9.5 degree offset from the spacecraft -Y axis.
 
            --   Corrected the definition of CASSINI_INMS. The Z-axis of this
                 frame is now co-aligned with the -X axis of CASSINI_SC_COORD.
 
   Version 2.8 -- October 9, 2000 -- Scott Turner
 
            --   Updated CASSINI_ISS_NAC and CASSINI_ISS_WAC to reflect the
                 updates associated with the Fomalhaut images taken on
                 September 18, 2000.
 
            --   Migrated the CASSINI_ISS_NAC_RAD, CASSINI_ISS_WAC_RAD,
                 CASSINI_VIMS_RAD, CASSINI_CIRS_RAD, CASSINI_CAPS,
                 CASSINI_CDA, CASSINI_INMS, CASSINI_MAG_PLUS,
                 CASSINI_MAG_MINUS, CASSINI_MIMI_CHEMS, CASSINI_MIMI_INCA,
                 CASSINI_MIMI_LEMMS1, CASSINI_MIMI_LEMMS2, CASSINI_RADAR_1,
                 CASSINI_RADAR_2, CASSINI_RADAR_3, CASSINI_RADAR_4,
                 CASSINI_RADAR_5, CASSINI_RPWS, CASSINI_RPWS_EXPLUS,
                 CASSINI_RPWS_EXMINUS, CASSINI_EZPLUS, CASSINI_RPWS_LP,
                 CASSINI_KUBAND, and CASSINI_SBAND frames from the prototype
                 section.
 
            --   Updated CASSINI_XBAND and CASSINI_KABAND as the result of SCR
                 367. These frames were migrated from the prototype section as
                 well.
 
   Version 2.7 -- July 7, 2000 -- Scott Turner
 
            --   Added the following frame entries RPWS requested:
                 CASSINI_RPWS_EXPLUS, CASSINI_RPWS_EXMINUS,
                 CASSINI_RPWS_EZPLUS, CASSINI_RPWS_LP to the prototype frame
                 section. See [14] for details.
 
            --   Changed the following frame names: CASSINI_HGA_X ->
                 CASSINI_XBAND, CASSINI_HGA_S -> CASSINI_SBAND, CASSINI_HGA_KA
                 -> CASSINI_KABAND, CASSINI_HGA_KU -> CASSINI_KUBAND.
 
            --   Halved the Euler angles associated with the CASSINI_CIRS_FP3
                 and CASSINI_FP4 frames. See [15] for details.
 
   Version 2.6 -- June 26, 2000 -- Scott Turner
 
            --   The RSS frame entries in the prototype section were renamed
                 to HGA based frames.
 
            --   Removed the CASSINI_MAG frame and replaced it with the
                 CASSINI_MAG_PLUS and CASSINI_MAG_MINUS frames.
 
   Version 2.5 -- April 2, 2000 -- Scott Turner
 
            --   Added CASSINI_VIMS.
 
            --   Added CASSINI_UVIS_FUV, CASSINI_UVIS_EUV,
                 CASSINI_UVIS_FUV_OCC, CASSINI_UVIS_EUV_OCC, CASSINI_UVIS_HSP,
                 and CASSINI_UVIS_HDAC.
 
            --   Fixed the keywords defining the CASSINI_HGA frame to use the
                 proper ID code, -82101.
 
            --   Updated CASSINI_ISS_NAC and CASSINI_ISS_WAC to reflect the
                 latest boresight information available in ECR's 100078 and
                 100079.
 
   Version 2.4 -- March 27, 2000 -- Scott Turner
 
            --   Added the CIRS Focal Plane Boresight frame, CASSINI_CIRS_FPB.
 
            --   CASSINI_CIRS_FP1, CASSINI_CIRS_FP3, CASSINI_CIRS_FP2 are no
                 longer relative to CASSINI_SC_COORD but to the intermediate
                 frame CASSINI_CIRS_FPB.
 
            --   Migrated the CASSINI_UVIS frame from the prototype section
                 and added the CASSINI_UVIS_OCC frame.
 
            --   Added the TEXT_KERNEL_ID keyword to make version information
                 accessible to programs at runtime.
 
   Version 2.3 -- March 9, 2000 -- Scott Turner
 
            --   Updated the Euler angles for CASSINI_CIRS_FP1,
                 CASSINI_CIRS_FP3, and CASSINI_CIRS_FP4. Migrated them from
                 the prototype section into the CIRS Section of the FK.
 
   Version 2.2 -- September 10, 1999 -- Scott Turner
 
            --   Removed TKFRAME_[ID]_BORESIGHT keyword for all but the
                 antenna frames present. This information can now be found in
                 the instrument kernel with the keyword: INS[ID]_BORESIGHT.
 
            --   Added a frame for the Stellar Reference Unit (SRU).
 
            --   Added prototype frame entries for several instruments. The
                 transformations stored here for these frames are NOT for any
                 real calculations, and in some cases are not connected with
                 the actual instrument pointing at all. These frames will
                 migrate from the prototype section as the kernel evolves.
 
            --   Changed CASSINI_SC_BUS to CASSINI_SC_COORD.
 
            --   Changed the LGA frame name definitions to CASSINI_LGA1 and
                 CASSINI_LGA2 to accomodate simple translation to flight
                 software frame names.
 
            --   Changed NAC and WAC ID codes from -82010 and -82020 to -82360
                 and -82361 respectively. This is to conform to the new ID
                 code scheme proposed by Jeff Boyer.
 
            --   Altered the textual description of the spacecraft coordinate
                 system to conform with [8].
 
            --   Added some text from [8] to the ISS_NAC frame description.
 
   Version 2.1 -- July 14, 1999 -- Scott Turner
 
            --   Fixed incorrect comments regarding the NAC images.
 
            --   Fixed an improperly specified transformation for LGA2.
 
            --   Added TKFRAME_[ID]_BORESIGHT keyword for the frames present.
 
   Version 2.0 -- May 5, 1999 -- Scott Turner
 
            --   Added ISS NAC and WAC instrument frames.
 
   Version 1.0 -- May 14, 1998 -- Jeff Bytof
 
            --   Initial Release.
 
 
References
----------------------------------------------------------
 
            1.   ``C-kernel Required Reading''
 
            2.   ``Kernel Pool Required Reading''
 
            3.   ``Frames Required Reading''
 
            4.   Cassini spacecraft blueprints. Provided by Kevin Tong, JPL.
 
            5.   ``Cassini Science Instruments and Investigations'', Revised
                 Second Printing. Stephen J. Edberg.
 
            6.   ``Determination of the ISS Boresights in Cassini Spacecraft
                 Coordinate System.'' Carolyn Porco and Vance Haemmerle.
 
            7.   Email from Vance Haemmerle regarding WAC alignment.
 
            8.   Cassini Document No. 699-406 ``Project Guidance Analysis
                 Book''
 
            9.   CASPER CIRS I-kernel Version 3.2
 
           10.   CIRS Fields-of-View PDF attached in an email from Stephen
                 Edberg to Diane Conner.
 
           11.   Cassini Engineering Change Request #100078
 
           12.   Cassini Engineering Change Request #100079
 
           13.   CASPER VIMS I-kernel Version Version 4.2
 
           14.   Email from Terry Averkamp regarding new RPWS frame entries.
 
           15.   Email from Richard Achterberg regarding the CIRS frame
                 entries.
 
           16.   Email from Vance Haemmerle regarding the Fomalhaut updates to
                 the ISS NAC and WAC alignments.
 
           17.   Email from Jeff Boyer regarding radiator boresight
                 alignments, MIMI_CHEMS orientation, and RPWS orientation.
 
           18.   Email from Sascha Kempf regarding CDA articulation.
 
           19.   CASPER INMS I-kernel Version 5.0
 
           20.   Email from Marcia Burton regarding the MAG field of views and
                 frame definitions.
 
           21.   CASPER MAG I-kernel Version 6.0
 
           22.   CASPER MIMI I-kernel Version 4.0
 
           23.   CASPER RADAR I-kernel Version 2.2
 
           24.   Email from Terry Averkamp discussing the new RPWS frame
                 entries.
 
           25.   Email from Thomas Burk regarding the updates to CASSINI_XBAND
                 and CASSINI_KABAND frames that were the result of SCR 367.
 
           26.   Email from Deborah Bass regarding a correct in the
                 CASSINI_INMS frame definition.
 
           27.   Email from Donald Mitchell regarding a correction in the
                 CASSINI_MIMI_INCA frame definition.
 
           28.   Email from Rick McCloskey regarding updates and additions to
                 the VIMS frame set.
 
           29.   Email from Joshua Colwell regarding the CASSINI_UVIS_SOLAR
                 frame definition.
 
           30.   Email from Joshua Colwell verifying the CASSINI_UVIS_SOLAR
                 frame definition.
 
           31.   Email from Rick McCloskey confirming the CASSINI_VIMS_V,
                 CASSINI_VIMS_IR, CASSINI_VIMS_IR_SOL frame definitions.
 
           32.   Email from Jeff Boyer providing CASSINI_SRU_RAD frame
                 definition.
 
           33.   Email from Don Mitchell describing the CASSINI_MIMI_LEMMS1
                 and CASSINI_MIMI_LEMMS2 articulation characteristics.
 
           34.   Email from Trina Ray describing updates for the CASSINI_XBAND
                 and CASSINI_KABAND frame definitions.
 
           35.   Email from Diane Conner describing updates for the
                 CASSINI_XBAND and CASSINI_KABAND boresights.
 
           36.   Cassini ECR 101029 - Change CASSINI_VIMS_RAD frame
                 definition.
 
           37.   Cassini ECR 10325-B -- Change VIMS Sun Viewing Constraints
                 Flight Rule FF37B2.
 
           38.   Email from Diane Conner regarding CASSINI_KABAND updated
                 boresight information.
 
           39.   Page 28 from ECR 100515 listing updated alignment information
                 for CASSINI_CIRS detectors.
 
           40.   Joshua Colwell's updated version 3.3.1 Cassini Spacecraft
                 Frame Definition kernel.
 
           41.   Email from Rick McCloskey regarding the values in the body
                 vector table for CASSINI_VIMS_IR.
 
           42.   Email from Rick McCloskey regarding the Euler angles for
                 CASSINI_VIMS_IR_SOL, the VIMS solar port.
 
           43.   Email correction from Rick McCloskey regarding the Euler
                 angles for CASSINI_VIMS_IR_SOL.
 
           44.   Email from Richard West regarding the CASSINI_RADAR_2 and
                 CASSINI_RADAR_4 Euler angles.
 
           45.   Spreadsheet (gnumeric format) from Rick McCloskey regarding
                 the Euler angles for CASSINI_VIMS_IR_SOL.
 
           46.   Email from Nicole Rappaport outlining needed changes to frame
                 and radio science instrument kernel due to project use of Ka
                 band data for X band pointing.
 
           47.   Cassini ECR number 102788 -- Additional Frame and FOV
                 definitions to SPICE FK & IK Files for RSS
 
           48.   Cassini R/SCR NO: 613 -- Update the Onboard XBAND body vector
                 table entries for GWE#3
 
 
Contact Information
----------------------------------------------------------
 
   Direct questions, comments, or concerns about the contents of this kernel
   to:
 
           Lee Elson, NAIF/JPL, (818)-354-4223, Lee.Elson@jpl.nasa.gov
 
 
Implementation Notes
----------------------------------------------------------
 
   This file is used by the SPICE system as follows: programs that make use of
   this frame kernel must `load' the kernel, normally during program
   initialization. Loading the kernel associates data items with their names
   in a data structure called the `kernel pool'. The SPICELIB routine LDPOOL
   loads a kernel file into the pool as shown below:
 
           CALL LDPOOL ( frame_kernel_name )
 
   In order for a program or subroutine to extract data from the pool, the
   SPICELIB routines GDPOOL and GIPOOL are used. See [2] for more details.
 
   This file was created and may be updated with a text editor or word
   processor.
 
   Note: the keyword TKFRAME_[ID]_BORESIGHT defines the instrument or antenna
   boresight axis in the instrument or antenna frame.
 
 
Cassini Frames
----------------------------------------------------------
 
   The following Cassini frames are defined in this kernel file:
 
           Frame Name                Relative To              Type     NAIF ID
           =======================   ===================      =======  =======
 
           AACS Body Frame:
           ----------------
            CASSINI_SC_COORD         J2000                    CK       -82000
            CASSINI_SRU-A            CASSINI_SC_COORD         FIXED    -82001
            CASSINI_SRU-B            CASSINI_SC_COORD         FIXED    -82002
            CASSINI_SRU-A_RAD        CASSINI_SC_COORD         FIXED    -82008
            CASSINI_SRU-B_RAD        CASSINI_SC_COORD         FIXED    -82009
 
           Antenna Frames (-821xx):
           ------------------------
            CASSINI_HGA              CASSINI_SC_COORD         FIXED    -82101
            CASSINI_LGA1             CASSINI_SC_COORD         FIXED    -82102
            CASSINI_LGA2             CASSINI_SC_COORD         FIXED    -82103
            CASSINI_XBAND            CASSINI_SC_COORD         FIXED    -82104
            CASSINI_KABAND           CASSINI_SC_COORD         FIXED    -82105
            CASSINI_KUBAND           CASSINI_SC_COORD         FIXED    -82106
            CASSINI_SBAND            CASSINI_SC_COORD         FIXED    -82107
            CASSINI_XBAND_TRUE       CASSINI_SC_COORD         FIXED    -82108
 
 
           ISS Frames (-8236x):
           ------------------------
            CASSINI_ISS_NAC          CASSINI_SC_COORD         FIXED    -82360
            CASSINI_ISS_WAC          CASSINI_SC_COORD         FIXED    -82361
            CASSINI_ISS_NAC_RAD      CASSINI_SC_COORD         FIXED    -82368
            CASSINI_ISS_WAC_RAD      CASSINI_SC_COORD         FIXED    -82369
 
           CIRS Frames (-8289x):
           ------------------------
            CASSINI_CIRS_FP1         CASSINI_CIRS_FPB         FIXED    -82890
            CASSINI_CIRS_FP3         CASSINI_CIRS_FPB         FIXED    -82891
            CASSINI_CIRS_FP4         CASSINI_CIRS_FPB         FIXED    -82892
            CASSINI_CIRS_FPB         CASSINI_SC_COORD         FIXED    -82893
            CASSINI_CIRS_RAD         CASSINI_SC_COORD         FIXED    -82898
 
           UVIS Frames (-8284x):
           ------------------------
            CASSINI_UVIS_FUV         CASSINI_SC_COORD         FIXED    -82840
            CASSINI_UVIS_EUV         CASSINI_SC_COORD         FIXED    -82842
            CASSINI_UVIS_SOLAR       CASSINI_SC_COORD         FIXED    -82843
            CASSINI_UVIS_HSP         CASSINI_SC_COORD         FIXED    -82844
            CASSINI_UVIS_HDAC        CASSINI_SC_COORD         FIXED    -82845
 
           VIMS Frames (-8283x):
           ------------------------
            CASSINI_VIMS_V           CASSINI_SC_COORD         FIXED    -82370
            CASSINI_VIMS_IR          CASSINI_SC_COORD         FIXED    -82371
            CASSINI_VIMS_IR_SOL      CASSINI_SC_COORD         FIXED    -82372
            CASSINI_VIMS_RAD         CASSINI_SC_COORD         FIXED    -82378
 
           CAPS Frames (-8282x):
           ------------------------
            CASSINI_CAPS_BASE        CASSINI_SC_COORD         FIXED    -82822
            CASSINI_CAPS_ART         CASSINI_CAPS_BASE        CK       -82821
            CASSINI_CAPS             CASSINI_CAPS_ART         CK       -82820
                                     CASSINI_SC_COORD         CK       -82820
 
           CDA Frames (-8279x):
           ------------------------
            CASSINI_CDA_BASE         CASSINI_SC_COORD         FIXED    -82792
            CASSINI_CDA_ART          CASSINI_CDA_BASE         CK       -82971
            CASSINI_CDA              CASSINI_CDA_ART          CK       -82790
                                     CASSINI_SC_COORD         CK       -82790
 
           INMS Frames (-8274x):
           ------------------------
            CASSINI_INMS             CASSINI_SC_COORD         FIXED    -82740
 
           MAG Frames (-8235x):
           ------------------------
            CASSINI_MAG_PLUS         CASSINI_SC_COORD         FIXED    -82350
            CASSINI_MAG_MINUS        CASSINI_SC_COORD         FIXED    -82351
 
           MIMI Frames (-8276x):
           ------------------------
            CASSINI_MIMI_CHEMS       CASSINI_SC_COORD         FIXED    -82760
            CASSINI_MIMI_INCA        CASSINI_SC_COORD         FIXED    -82761
            CASSINI_MIMI_LEMMS_BASE  CASSINI_SC_COORD         FIXED    -82765
            CASSINI_MIMI_LEMMS_ART   CASSINI_MIMI_LEMMS_BASE  CK       -82764
            CASSINI_MIMI_LEMMS1      CASSINI_MIMI_LEMMS_ART   CK       -82762
                                     CASSINI_SC_COORD         CK       -82762
            CASSINI_MIMI_LEMMS2      CASSINI_MIMI_LEMMS_ART   CK       -82763
                                     CASSINI_SC_COORD         CK       -82763
 
           RADAR Frames (-8281x):
           ------------------------
            CASSINI_RADAR_1          CASSINI_SC_COORD         FIXED    -82810
            CASSINI_RADAR_2          CASSINI_SC_COORD         FIXED    -82811
            CASSINI_RADAR_3          CASSINI_SC_COORD         FIXED    -82812
            CASSINI_RADAR_4          CASSINI_SC_COORD         FIXED    -82813
            CASSINI_RADAR_5          CASSINI_SC_COORD         FIXED    -82814
 
           RPWS Frames (-8273x):
           ------------------------
            CASSINI_RPWS             CASSINI_SC_COORD         FIXED    -82730
            CASSINI_RPWS_EXPLUS      CASSINI_SC_COORD         FIXED    -82731
            CASSINI_RPWS_EXMINUS     CASSINI_SC_COORD         FIXED    -82732
            CASSINI_RPWS_EZPLUS      CASSINI_SC_COORD         FIXED    -82733
            CASSINI_RPWS_LP          CASSINI_SC_COORD         FIXED    -82734
 
 
   where: the frame ID codes are built from the spacecraft ID code, the
   instrument subsystem number, and the instrument number in a multiple
   instrument subsystem. The numbers 8 and 9 are reserved for the radiators.
   For example the ISS frame IDs are constructed as follows:
 
              CASSINI_ISS_WAC ID = -82 36  1
                                    |   |  |
                                    |   |  |
           SPACECRAFT ID CODE <-----+   |  +----> INSTRUMENT NUMBER
                                        |
                                        V
                          INSTRUMENT SUBSYSTEM NUMBER
 
 
Cassini Frames Hierarchy
----------------------------------------------------------
 
   Notes:
 
   This diagram is subject to major revisions as this kernel evolves to suit
   the needs of each instrument.
 
   The articulating instrument frames have two paths back to the spacecraft
   frame. The first is a direct path via a single C-kernel connecting the
   instrument frame to the spacecraft frame. The second is one that utilizes a
   fixed offset C-kernel to rotate the instrument frame into the articulation
   frame, and then an articulation C-kernel and a base frame. For details see
   the sections for CASSINI_CDA, CASSINI_CAPS, and CASSINI_MIMI_LEMMS.
 
   The diagram below shows the Cassini frames hierarchy:
 
 
             'IAU_EARTH' (EARTH BODY FIXED)
              |
              |<--- pck
              |
           'J2000' INERTIAL
              |
              |<--- ck
              |
             'CASSINI_SC_COORD'
                  |
                 'CASSINI_SRU-A'
                  |
                 'CASSINI_SRU-B'
                  |
                 'CASSINI_SRU-A_RAD'
                  |
                 'CASSINI_SRU-B_RAD'
                  |
                 'CASSINI_HGA'
                  |
                 'CASSINI_XBAND'
                  |
                 'CASSINI_KABAND'
                  |   |
                  |  'CASSINI_XBAND_TRUE'
                  |
                 'CASSINI_KUBAND'
                  |
                 'CASSINI_SBAND'
                  |
                 'CASSINI_LGA1'
                  |
                 'CASSINI_LGA2'
                  |
                 'CASSINI_ISS_NAC'
                  |
                 'CASSINI_ISS_WAC'
                  |
                 'CASSINI_ISS_NAC_RAD'
                  |
                 'CASSINI_ISS_WAC_RAD'
                  |
                 'CASSINI_CIRS_FPB'
                  |   |
                  |  'CASSINI_CIRS_FP1'
                  |   |
                  |  'CASSINI_CIRS_FP3'
                  |   |
                  |  'CASSINI_CIRS_FP4'
                  |
                 'CASSINI_CIRS_RAD'
                  |
                 'CASSINI_UVIS_FUV'
                  |
                 'CASSINI_UVIS_EUV'
                  |
                 'CASSINI_UVIS_SOLAR'
                  |
                 'CASSINI_UVIS_HSP'
                  |
                 'CASSINI_UVIS_HDAC'
                  |
                 'CASSINI_VIMS_V'
                  |
                 'CASSINI_VIMS_IR'
                  |
                 'CASSINI_VIMS_IR_SOL'
                  |
                 'CASSINI_VIMS_RAD'
                  |
                 'CASSINI_CAPS_BASE'
                  |   |
                  |   |<--- ck
                  |   |
                  |  'CASSINI_CAPS_ART'
                  |       |
                  |       |<--- ck
                  |       |
                  o------'CASSINI_CAPS'
                  |  ^
                  |  |
                  |  + ck
                  |
                 'CASSINI_CDA_BASE'
                  |   |
                  |   |<--- ck
                  |   |
                  |  'CASSINI_CDA_ART'
                  |       |
                  |       |<--- ck
                  |       |
                  o------'CASSINI_CDA'
                  |  ^
                  |  |
                  |  + ck
                  |
                 'CASSINI_INMS'
                  |
                 'CASSINI_MAG_PLUS'
                  |
                 'CASSINI_MAG_MINUS'
                  |
                 'CASSINI_MIMI_CHEMS'
                  |
                 'CASSINI_MIMI_INCA'
                  |
                 'CASSINI_MIMI_LEMMS_BASE'
                  |   |
                  |   |<--- ck
                  |   |
                  |  'CASSINI_MIMI_LEMMS_ART'
                  |       |
                  |       |<--- ck
                  |       |
                  o------'CASSINI_MIMI_LEMMS1'
                  |  ^    |
                  |  |    |
                  |  + ck |
                  |       |
                  o------'CASSINI_MIMI_LEMMS2'
                  |  ^
                  |  |
                  |  + ck
                  |
                 'CASSINI_RADAR_1'
                  |
                 'CASSINI_RADAR_2'
                  |
                 'CASSINI_RADAR_3'
                  |
                 'CASSINI_RADAR_4'
                  |
                 'CASSINI_RADAR_5'
                  |
                 'CASSINI_RPWS'
                  |
                 'CASSINI_RPWS_EXPLUS'
                  |
                 'CASSINI_RPWS_EXMINUS'
                  |
                 'CASSINI_RPWS_EZPLUS'
                  |
                 'CASSINI_RPWS_LP'
 
 
 
 
Spacecraft Frame
----------------------------------------------------------
 
   From [8]: (Note: The figures referenced below can not be reproduced here.
   There is a diagram below that basically illustrates what is contained
   there.)
 
   ``The Stellar reference Unit (SRU) detector is a CCD. Its coordinate system
   is defined according to the geometry of the detector. Figure 2.1.2a depicts
   the SRU orientation and coordinates relative to the S/C coordinates. From
   the ACS point of view, the S/C coordinate system is defined with respect to
   the SRU coordinate frame, such that :
 
   +X = +b (SRU boresight)
 
   +Y = +v
 
   +Z = -h
 
   Therefore, by definition, there are no misalignments between the SRU and
   the S/C coordinate frames.
 
   The SRU coordinate system is defined by the pixel and line shift directions
   defined in Figure 2.1.2b. These directions are represented by unit vectors
   h and v respectively. Both h and v pass through the origin which is located
   at the exact center of the 1024 x 1024 array. As indicated in Figure
   2.1.2b, the SRU boresight b passes through this point, is normal to both h
   and v, and points outward through the optics towards the scene being
   viewed.''
 
   Stellar Reference Unit Frame:
 
 
                                     Cassini Spacecraft
 
                                            /\
                             ----------------------------------
                             \                                /
                              \                              /    HGA
                               \                            /
               MAG Boom          --------------------------
            ... =================|                        |
                                 |           h            |
                                  \          ^           /
                                   |         |          |
                                   |         |          |
                       Y   <-------|   v <---o          |
                        sc         |           b, X     |
                                   |               sc   |
                                   |                    |
                                   |                    |
                                   |                    |
                                   |                    |
                                   ----------------------
                                          /      \
                                         /        \  Main Rocket Engine
                                         ----------
                                             |
                                             |
                                             |
                                             V
 
                                             Z
                                              sc
 
           where b and X   point out of the screen or page.
                        sc
 
 
   From [8]:
 
   ``The spacecraft basebody coordinate system is a body fixed coordinate
   system. It is a structural coordinate system defined when the spacecraft is
   assembled. The primary geometrical and mass properties are fixed to this
   system. The (X,Y,Z) coordinate system is not observable in space.
 
   Referring to Figure 2.1.1, the origin of the spacecraft coordinate system
   lies at the center of the field joint between the bus and the upper
   equipment module (UEM) upper shell structure assembly [7]. This location is
   defined by bolt holes A, D, and H (as shown on the Configuration lay out
   10129891, Figure 3). The Z-axis emanates from the origin and is
   perpendicular to a plane generated by the mating surfaces of the bus at
   bolt holes A, D, and H. The +Z-axis is on the propulsion module side of the
   interface. The X-axis emanates from the origin and is parallel to the line
   through the true centers of bolt holes A and H at the bus and the UEM upper
   shell structure assembly interface. The -X-axis points towards the Huygens
   probe. The Y-axis is mutually perpendicular to the X and Z axes, with the
   +Y axis oriented along the magnetometer boom.''
 
   Spacecraft bus attitude with respect to an inertial frame is provided by a
   C kernel (see [1] for more information).
 
           \begindata
 
           FRAME_CASSINI_SC_COORD   = -82000
           FRAME_-82000_NAME        = 'CASSINI_SC_COORD'
           FRAME_-82000_CLASS       = 3
           FRAME_-82000_CLASS_ID    = -82000
           FRAME_-82000_CENTER      = -82
           CK_-82000_SCLK           = -82
           CK_-82000_SPK            = -82
 
           \begintext
 
   The nominal definition of the Stellar Reference Unit-A frame is displayed
   below. As described above and in [8], the boresight axis lies along the
   spacecraft +X axis. The rotation matrix that takes vectors from the SRU-A
   frame into the spacecraft frame is computed:
 
           [     ]    [     ]  [       ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [     ]  [       ]  [     ]
                             Z          Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_SRU-A      = -82001
           FRAME_-82001_NAME        = 'CASSINI_SRU-A'
           FRAME_-82001_CLASS       = 4
           FRAME_-82001_CLASS_ID    = -82001
           FRAME_-82001_CENTER      = -82
           TKFRAME_-82001_SPEC      = 'ANGLES'
           TKFRAME_-82001_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82001_ANGLES    = ( 0.0,  -90.0,  0.0 )
           TKFRAME_-82001_AXES      = (  3,       2,    1 )
           TKFRAME_-82001_UNITS     = 'DEGREES'
 
           \begintext
 
   The nominal definition of the Stellar Reference Unit-B frame is displayed
   below. Nominally SRU-A and SRU-B are aligned, so the boresight axis lies
   along the spacecraft +X axis. The rotation matrix that takes vectors from
   the SRU-B frame into the spacecraft frame is computed:
 
           [     ]    [     ]  [       ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [     ]  [       ]  [     ]
                             Z          Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_SRU-B      = -82002
           FRAME_-82002_NAME        = 'CASSINI_SRU-B'
           FRAME_-82002_CLASS       = 4
           FRAME_-82002_CLASS_ID    = -82002
           FRAME_-82002_CENTER      = -82
           TKFRAME_-82002_SPEC      = 'ANGLES'
           TKFRAME_-82002_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82002_ANGLES    = ( 0.0,  -90.0,  0.0 )
           TKFRAME_-82002_AXES      = (  3,       2,    1 )
           TKFRAME_-82002_UNITS     = 'DEGREES'
 
           \begintext
 
   The nominal definition of the Stellar Reference Unit-A Radiator frame is
   displayed below. As described in [32], the rotation matrix that takes
   vectors from the SRU-A_RAD frame into the spacecraft frame is computed:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ 180.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                 X          Y        Z
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
           FRAME_CASSINI_SRU-A_RAD  = -82008
           FRAME_-82008_NAME        = 'CASSINI_SRU-A_RAD'
           FRAME_-82008_CLASS       = 4
           FRAME_-82008_CLASS_ID    = -82008
           FRAME_-82008_CENTER      = -82
           TKFRAME_-82008_SPEC      = 'ANGLES'
           TKFRAME_-82008_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82008_ANGLES    = ( 180.0, -90.0, 0.0 )
           TKFRAME_-82008_AXES      = (     3,     1,   3 )
           TKFRAME_-82008_UNITS     = 'DEGREES'
           \begintext
 
   The nominal definition of the Stellar Reference Unit-B Radiator frame is
   displayed below. As with the SRU-B frame, this is nominally the same frame
   as SRU-A_RAD. The rotation matrix that takes vectors from the SRU-B_RAD
   frame into the spacecraft frame is computed:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ 180.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                 X          Y        Z
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
           FRAME_CASSINI_SRU-B_RAD  = -82009
           FRAME_-82009_NAME        = 'CASSINI_SRU-B_RAD'
           FRAME_-82009_CLASS       = 4
           FRAME_-82009_CLASS_ID    = -82009
           FRAME_-82009_CENTER      = -82
           TKFRAME_-82009_SPEC      = 'ANGLES'
           TKFRAME_-82009_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82009_ANGLES    = ( 180.0, -90.0, 0.0 )
           TKFRAME_-82009_AXES      = (     3,     1,   3 )
           TKFRAME_-82009_UNITS     = 'DEGREES'
           \begintext
 
 
Antenna Frame Definitions
----------------------------------------------------------
 
   This section of the frames kernel defines the Cassini spacecraft antenna
   frames. The ID codes associated with each of the frames are determined by
   subtracting the three digit antenna code (101-103) from the DSN Cassini
   spacecraft bus ID code (-82000).
 
   Note the angles in the frame definitions are specified for the "from
   antenna to (relative to) base frame" transformation.
 
 
High Gain Antenna (HGA)
 
   The high gain antenna points nominally along the spacecraft -Z axis. As
   such the rotation matrix required that takes vectors represented in the
   high gain antenna frame into the spacecraft frame is constructed as
   follows:
 
           [     ]    [     ]  [        ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ +180.0 ]  [ 0.0 ]
           [     ]    [     ]  [        ]  [     ]
                             Z           Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_HGA        = -82101
           FRAME_-82101_NAME        = 'CASSINI_HGA'
           FRAME_-82101_CLASS       = 4
           FRAME_-82101_CLASS_ID    = -82101
           FRAME_-82101_CENTER      = -82
           TKFRAME_-82101_SPEC      = 'ANGLES'
           TKFRAME_-82101_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82101_ANGLES    = ( 0.0,  180.0,  0.0 )
           TKFRAME_-82101_AXES      = (  3,      2,    1  )
           TKFRAME_-82101_UNITS     = 'DEGREES'
           TKFRAME_-82101_BORESIGHT = ( 0.0, 0.0, 1.0 )
 
           \begintext
 
   The XBAND, XBAND_TRUE, KABAND, KUBAND, and SBAND frames are all frames
   associated with the orbiter's High Gain Antenna. These names were chosen
   for reasons of consistency with AACS, PDT, and sequencing software.
 
 
High Gain Antenna X Band (XBAND)
 
   The high gain antenna is capable of operating in several bands, each of
   which may be calibrated and adjusted independently. The nominal frame
   definition for the XBAND is displayed below:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 180.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_XBAND       = -82104
           FRAME_-82104_NAME         = 'CASSINI_XBAND'
           FRAME_-82104_CLASS        = 4
           FRAME_-82104_CLASS_ID     = -82104
           FRAME_-82104_CENTER       = -82
           TKFRAME_-82104_SPEC       = 'ANGLES'
           TKFRAME_-82104_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82104_ANGLES     = (   0.0,  0.0,  180.0 )
           TKFRAME_-82104_AXES       = (     3,    2,      1 )
           TKFRAME_-82104_UNITS      = 'DEGREES'
 
 
   From [25], the XBAND boresight has been adjusted to the following vector in
   spacecraft coordinates:
 
                                    [  0.0005000  ]
           XBAND Boresight Vector = [  0.0004000  ]
                                    [ -0.9999998  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [             ]  [             ]  [     ]
           [ ROT ]  = [ -128.659808 ]  [ -179.963313 ]  [ 0.0 ]
           [     ]    [             ]  [             ]  [     ]
                                     Z                X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           First Updated Frame Definition:
 
           FRAME_CASSINI_XBAND       = -82104
           FRAME_-82104_NAME         = 'CASSINI_XBAND'
           FRAME_-82104_CLASS        = 4
           FRAME_-82104_CLASS_ID     = -82104
           FRAME_-82104_CENTER       = -82
           TKFRAME_-82104_SPEC       = 'ANGLES'
           TKFRAME_-82104_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82104_ANGLES     = (   -128.659808,  -179.963313,   0.0 )
           TKFRAME_-82104_AXES       = (      3,            1,          3   )
           TKFRAME_-82104_UNITS      = 'DEGREES'
 
 
   From [34], the XBAND boresight has been adjusted again to the following
   vector in spacecraft coordinates:
 
                                    [  0.0005200  ]
           XBAND Boresight Vector = [  0.0005800  ]
                                    [ -0.9999997  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -138.12213046232 ]  [ -179.95536809121 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
 
           Second Updated Frame Definition:
 
           FRAME_CASSINI_XBAND       = -82104
           FRAME_-82104_NAME         = 'CASSINI_XBAND'
           FRAME_-82104_CLASS        = 4
           FRAME_-82104_CLASS_ID     = -82104
           FRAME_-82104_CENTER       = -82
           TKFRAME_-82104_SPEC       = 'ANGLES'
           TKFRAME_-82104_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82104_ANGLES     = ( -138.12213046232,
                                         -179.95536809121,
                                            0.0            )
           TKFRAME_-82104_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82104_UNITS      = 'DEGREES'
 
 
   From [35], the XBAND boresight has been adjusted to the following vector in
   spacecraft coordinates:
 
                                    [  0.0004839  ]
           XBAND Boresight Vector = [  0.0001745  ]
                                    [ -0.9999999  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -109.82989689352 ]  [ -179.97052693372 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Third Updated Frame Definition:
 
 
           FRAME_CASSINI_XBAND       = -82104
           FRAME_-82104_NAME         = 'CASSINI_XBAND'
           FRAME_-82104_CLASS        = 4
           FRAME_-82104_CLASS_ID     = -82104
           FRAME_-82104_CENTER       = -82
           TKFRAME_-82104_SPEC       = 'ANGLES'
           TKFRAME_-82104_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82104_ANGLES     = ( -109.82989689352,
                                         -179.97052693372,
                                            0.0            )
           TKFRAME_-82104_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82104_UNITS      = 'DEGREES'
 
 
 
   ______________________________________________________________
   ++++++++++++++++++++March 18, 2003++++++++++++++++++++++++++++
 
   From [46], the XBAND boresight has been adjusted so that it is co-aligned
   with the KABAND boresight. By defining the frame relative to
   'CASSINI_KABAND', the following frame definition is valid:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [     0.0          ]  [         0.0      ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
 
           \begindata
 
           FRAME_CASSINI_XBAND       = -82104
           FRAME_-82104_NAME         = 'CASSINI_XBAND'
           FRAME_-82104_CLASS        = 4
           FRAME_-82104_CLASS_ID     = -82104
           FRAME_-82104_CENTER       = -82
           TKFRAME_-82104_SPEC       = 'ANGLES'
           TKFRAME_-82104_RELATIVE   = 'CASSINI_KABAND'
           TKFRAME_-82104_ANGLES     = (    0.0,
                                            0.0,
                                            0.0            )
           TKFRAME_-82104_AXES       = (    3,
                                            1,
                                            2              )
           TKFRAME_-82104_UNITS      = 'DEGREES'
 
           \begintext
 
 
High Gain Antenna X Band True (XBAND_TRUE)
______________________________________________________________
++++++++++++++++++++March 18, 2003++++++++++++++++++++++++++++
 
   In order to preserve the original boresight information for the XBAND
   antenna, a new frame is defined containing that information. The change
   history is documented above under the XBAND frame.
 
   From [35], the XBAND boresight has been adjusted to the following vector in
   spacecraft coordinates:
 
                                    [  0.0004839  ]
           XBAND Boresight Vector = [  0.0001745  ]
                                    [ -0.9999999  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -109.82989689352 ]  [ -179.97052693372 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_XBAND_TRUE  = -82108
           FRAME_-82108_NAME         = 'CASSINI_XBAND_TRUE'
           FRAME_-82108_CLASS        = 4
           FRAME_-82108_CLASS_ID     = -82108
           FRAME_-82108_CENTER       = -82
           TKFRAME_-82108_SPEC       = 'ANGLES'
           TKFRAME_-82108_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82108_ANGLES     = ( -109.82989689352,
                                         -179.97052693372,
                                            0.0            )
           TKFRAME_-82108_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82108_UNITS      = 'DEGREES'
 
           \begintext
 
 
High Gain Antenna KA Band (KABAND)
 
   The high gain antenna is capable of operating in several bands, each of
   which may be calibrated and adjusted independently. The nominal frame
   definition for the KABAND is displayed below:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 180.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_KABAND      = -82105
           FRAME_-82105_NAME         = 'CASSINI_KABAND'
           FRAME_-82105_CLASS        = 4
           FRAME_-82105_CLASS_ID     = -82105
           FRAME_-82105_CENTER       = -82
           TKFRAME_-82105_SPEC       = 'ANGLES'
           TKFRAME_-82105_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82105_ANGLES     = (   0.0,  0.0, 180.0 )
           TKFRAME_-82105_AXES       = (     3,    2,     1 )
           TKFRAME_-82105_UNITS      = 'DEGREES'
 
 
   From [25], the KABAND boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                     [  0.0005000  ]
           KABAND Boresight Vector = [  0.0004000  ]
                                     [ -0.9999998  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [             ]  [             ]  [     ]
           [ ROT ]  = [ -128.659808 ]  [ -179.963313 ]  [ 0.0 ]
           [     ]    [             ]  [             ]  [     ]
                                     Z                X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           First Updated Frame Definition:
 
           FRAME_CASSINI_KABAND      = -82105
           FRAME_-82105_NAME         = 'CASSINI_KABAND'
           FRAME_-82105_CLASS        = 4
           FRAME_-82105_CLASS_ID     = -82105
           FRAME_-82105_CENTER       = -82
           TKFRAME_-82105_SPEC       = 'ANGLES'
           TKFRAME_-82105_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82105_ANGLES     = (   -128.659808,  -179.963313,   0.0 )
           TKFRAME_-82105_AXES       = (      3,            1,          3   )
           TKFRAME_-82105_UNITS      = 'DEGREES'
 
 
   From [34], the KABAND boresight has been adjusted again to the following
   vector in spacecraft coordinates:
 
                                     [  0.0005300  ]
           KABAND Boresight Vector = [  0.0006600  ]
                                     [ -0.9999996  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -141.23448009520 ]  [ -179.95150122158 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Second Updated Frame Definition:
 
           FRAME_CASSINI_KABAND      = -82105
           FRAME_-82105_NAME         = 'CASSINI_KABAND'
           FRAME_-82105_CLASS        = 4
           FRAME_-82105_CLASS_ID     = -82105
           FRAME_-82105_CENTER       = -82
           TKFRAME_-82105_SPEC       = 'ANGLES'
           TKFRAME_-82105_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82105_ANGLES     = ( -141.23448009520,
                                         -179.95150122158,
                                            0.0            )
           TKFRAME_-82105_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82105_UNITS      = 'DEGREES'
 
 
   From [35], the KABAND boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                     [  0.0004839  ]
           KABAND Boresight Vector = [  0.0001745  ]
                                     [ -0.9999999  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -109.82989689352 ]  [ -179.97052693372 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
 
           FRAME_CASSINI_KABAND      = -82105
           FRAME_-82105_NAME         = 'CASSINI_KABAND'
           FRAME_-82105_CLASS        = 4
           FRAME_-82105_CLASS_ID     = -82105
           FRAME_-82105_CENTER       = -82
           TKFRAME_-82105_SPEC       = 'ANGLES'
           TKFRAME_-82105_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82105_ANGLES     = ( -109.82989689352,
                                         -179.97052693372,
                                            0.0            )
           TKFRAME_-82105_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82105_UNITS      = 'DEGREES'
 
 
   From [38], the KABAND boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                     [  0.0005280  ]
           KABAND Boresight Vector = [  0.0003500  ]
                                     [ -0.9999998  ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -123.53955356526 ]  [ -179.96370485104 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
 
 
           FRAME_CASSINI_KABAND      = -82105
           FRAME_-82105_NAME         = 'CASSINI_KABAND'
           FRAME_-82105_CLASS        = 4
           FRAME_-82105_CLASS_ID     = -82105
           FRAME_-82105_CENTER       = -82
           TKFRAME_-82105_SPEC       = 'ANGLES'
           TKFRAME_-82105_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82105_ANGLES     = ( -123.53955356526,
                                         -179.96370485104,
                                            0.0            )
           TKFRAME_-82105_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82105_UNITS      = 'DEGREES'
 
 
   ______________________________________________________________
   ++++++++++++++++++++November 20, 2003++++++++++++++++++++++++++++
 
   From [48], the KABAND boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                     [  0.0004273  ]
           KABAND Boresight Vector = [  0.0008606 ]
                                     [ -0.9999995 ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [                  ]  [                  ]  [     ]
           [ ROT ]  = [ -153.59495523828 ]  [ -179.94494778906 ]  [ 0.0 ]
           [     ]    [                  ]  [                  ]  [     ]
                                          Z                     X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_KABAND      = -82105
           FRAME_-82105_NAME         = 'CASSINI_KABAND'
           FRAME_-82105_CLASS        = 4
           FRAME_-82105_CLASS_ID     = -82105
           FRAME_-82105_CENTER       = -82
           TKFRAME_-82105_SPEC       = 'ANGLES'
           TKFRAME_-82105_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82105_ANGLES     = ( -153.59495523828,
                                         -179.94494778906,
                                            0.0            )
           TKFRAME_-82105_AXES       = (    3,
                                            1,
                                            3              )
           TKFRAME_-82105_UNITS      = 'DEGREES'
 
           \begintext
 
 
High Gain Antenna KU Band (KUBAND)
 
   The high gain antenna is capable of operating in several bands, each of
   which may be calibrated and adjusted independently. The nominal frame
   definition for the KUBAND is displayed below:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 180.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_KUBAND      = -82106
           FRAME_-82106_NAME         = 'CASSINI_KUBAND'
           FRAME_-82106_CLASS        = 4
           FRAME_-82106_CLASS_ID     = -82106
           FRAME_-82106_CENTER       = -82
           TKFRAME_-82106_SPEC       = 'ANGLES'
           TKFRAME_-82106_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82106_ANGLES     = (   0.0,  0.0,  180.0 )
           TKFRAME_-82106_AXES       = (     3,    2,      1 )
           TKFRAME_-82106_UNITS      = 'DEGREES'
 
           \begintext
 
 
High Gain Antenna S Band (SBAND)
 
   The high gain antenna is capable of operating in several bands, each of
   which may be calibrated and adjusted independently. The nominal frame
   definition for the SBAND is displayed below:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 180.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_SBAND       = -82107
           FRAME_-82107_NAME         = 'CASSINI_SBAND'
           FRAME_-82107_CLASS        = 4
           FRAME_-82107_CLASS_ID     = -82107
           FRAME_-82107_CENTER       = -82
           TKFRAME_-82107_SPEC       = 'ANGLES'
           TKFRAME_-82107_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82107_ANGLES     = (   0.0, 0.0, 180.0 )
           TKFRAME_-82107_AXES       = (     3,   2,     1 )
           TKFRAME_-82107_UNITS      = 'DEGREES'
 
           \begintext
 
 
Low Gain Antenna One (LGA1)
 
   The first low gain antenna points nominally along the spacecraft -Z axis.
   As such the rotation matrix required that takes vectors represented in the
   first low gain antenna frame into the spacecraft frame is constructed as
   follows:
 
           [     ]    [     ]  [        ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ +180.0 ]  [ 0.0 ]
           [     ]    [     ]  [        ]  [     ]
                             Z           Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_LGA1       = -82102
           FRAME_-82102_NAME        = 'CASSINI_LGA1'
           FRAME_-82102_CLASS       = 4
           FRAME_-82102_CLASS_ID    = -82102
           FRAME_-82102_CENTER      = -82
           TKFRAME_-82102_SPEC      = 'ANGLES'
           TKFRAME_-82102_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82102_ANGLES    = ( 0.0,   180.0,  0.0 )
           TKFRAME_-82102_AXES      = (  3,      2,     1  )
           TKFRAME_-82102_UNITS     = 'DEGREES'
           TKFRAME_-82102_BORESIGHT = ( 0.0, 0.0, 1.0 )
 
           \begintext
 
 
Low Gain Antenna Two (LGA2)
 
   The second low gain antenna points nominally along the spacecraft -X axis.
   As such the rotation matrix required that takes vectors represented in the
   second low gain antenna frame into the spacecraft frame is constructed as
   follows:
 
           [     ]    [     ]  [      ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ 90.0 ]  [ 0.0 ]
           [     ]    [     ]  [      ]  [     ]
                             Z         Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_LGA2       = -82103
           FRAME_-82103_NAME        = 'CASSINI_LGA2'
           FRAME_-82103_CLASS       = 4
           FRAME_-82103_CLASS_ID    = -82103
           FRAME_-82103_CENTER      = -82
           TKFRAME_-82103_SPEC      = 'ANGLES'
           TKFRAME_-82103_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82103_ANGLES    = ( 0.0,     90.0,     0.0 )
           TKFRAME_-82103_AXES      = (   3,       2,       1  )
           TKFRAME_-82103_UNITS     = 'DEGREES'
           TKFRAME_-82103_BORESIGHT = ( 0.0, 0.0, 1.0 )
 
           \begintext
 
 
ISS Frames
----------------------------------------------------------
 
   The Narrow Angle Camera (NAC) and Wide Angle Camera (WAC) are mounted on
   the remote sensing pallet on the +X side of the Cassini spacecraft, and
   nominally directed along the -Y axis of the AACS body frame.
 
   Note the angles in the frame definitions are specified for the "from
   instrument to (relative to) base frame" transformation.
 
 
Imaging Science Subsystem Narrow Angle Camera (ISS_NAC)
 
   The ISS NAC points nominally along the spacecraft -Y axis. The following
   frame definition encapsulates this nominal frame.
 
   From [8]:
 
   ``The Narrow Angle Camera (NAC) detector is a CCD. Its coordinate system is
   defined according to the geometry of the detector. The narrow angle
   coordinate system is defined in the same manner as the SRU coordinate
   systems defined above and the four central pixels of center of the full CCD
   are selected for the definition of the origin of the coordinate system.
 
   The Narrow Angle Camera is the primary instrument on the Remote Sensing
   Pallet (RSP). AACS is responsible for providing pointing knowledge of the
   boresight vector of this instrument. All other RSP instruments use the
   pointing provided to the NAC as their reference for determining their
   pointing.''
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_ISS_NAC    = -82360
           FRAME_-82360_NAME        = 'CASSINI_ISS_NAC'
           FRAME_-82360_CLASS       = 4
           FRAME_-82360_CLASS_ID    = -82360
           FRAME_-82360_CENTER      = -82
           TKFRAME_-82360_SPEC      = 'ANGLES'
           TKFRAME_-82360_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82360_ANGLES    = (   -90.0,     0.0,     90.0 )
           TKFRAME_-82360_AXES      = (     1,       2,        3   )
           TKFRAME_-82360_UNITS     = 'DEGREES'
 
 
   [6] describes the inflight calibration of the ISS that was the result of
   the CICLOPS (Cassini Imaging Central Laboratory for Operations) analysis of
   8 NAC images that were taken during ICO (Instrument Checkout). The rotation
   matrix that takes vectors represented in the ISS_NAC frame into the
   spacecraft frame follows:
 
           [     ]    [            ]  [              ]  [           ]
           [ ROT ]  = [ -90.024236 ]  [ -0.047029483 ]  [ 89.892082 ]
           [     ]    [            ]  [              ]  [           ]
                                    X                 Y              Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The angles were taken directly from [6].
 
 
           FRAME_CASSINI_ISS_NAC    = -82360
           FRAME_-82360_NAME        = 'CASSINI_ISS_NAC'
           FRAME_-82360_CLASS       = 4
           FRAME_-82360_CLASS_ID    = -82360
           FRAME_-82360_CENTER      = -82
           TKFRAME_-82360_SPEC      = 'ANGLES'
           TKFRAME_-82360_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82360_ANGLES    = ( -90.024236, -0.047029483,  89.892082 )
           TKFRAME_-82360_AXES      = (   1,         2,             3        )
           TKFRAME_-82360_UNITS     = 'DEGREES'
 
 
   From [10]:
 
   ``The NAC boresight is not precisely aligned with the S/C -Y body vector.
   Its alignment was determined during ICO-1 ISS observations of Spica, when
   the spacecraft was using SRU-B for orientation determination. The alignment
   parameters cited under Change Requested take into account the offset, as
   determined by AACS, between SRU-A and SRU-B.''
 
   [10] also describes a series of frame transformations that convert the
   CASSINI_ISS_NAC frame into the CASSINI_SC_COORD frame, accounting for the
   offset between SRU-A and SRU-B. This results in following frame definition:
 
   The rotation matrix that takes vectors represented in the ISS_NAC frame
   into the spacecraft frame follows:
 
           [     ]    [              ]  [             ]  [             ]
           [ ROT ]  = [ -89.99231636 ]  [ -0.03586589 ]  [ 89.93339682 ]
           [     ]    [              ]  [             ]  [             ]
                                      X                Y                Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
 
           FRAME_CASSINI_ISS_NAC    = -82360
           FRAME_-82360_NAME        = 'CASSINI_ISS_NAC'
           FRAME_-82360_CLASS       = 4
           FRAME_-82360_CLASS_ID    = -82360
           FRAME_-82360_CENTER      = -82
           TKFRAME_-82360_SPEC      = 'ANGLES'
           TKFRAME_-82360_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82360_ANGLES    = (-89.99231636, -0.03586589, 89.93339682)
           TKFRAME_-82360_AXES      = (  1,           2,           3         )
           TKFRAME_-82360_UNITS     = 'DEGREES'
 
 
   From [16]:
 
   ``The following results were obtained by using 3 long exposure Fomalhaut
   NAC frames, each with 6 stars and using a 3 parameter fit (shift in line,
   shift in sample and rotation about optic axis).''
 
              AACS       NAC boresight
 
               X         0.0005760  +/- 0.0000018
               Y        -0.99999982 +/- 0.00000001
               Z        -0.0001710  +/- 0.0000016
 
   The results of the Fomalhaut image calibrations produced the following
   update to the ISS_NAC frame defintion:
 
   The rotation matrix that takes vectors represented in the ISS_NAC frame
   into the spacecraft frame follows:
 
           [     ]    [            ]  [          ]  [         ]
           [ ROT ]  = [ -90.009796 ]  [ -0.03300 ]  [ 89.9148 ]
           [     ]    [            ]  [          ]  [         ]
                                    X             Y            Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_ISS_NAC    = -82360
           FRAME_-82360_NAME        = 'CASSINI_ISS_NAC'
           FRAME_-82360_CLASS       = 4
           FRAME_-82360_CLASS_ID    = -82360
           FRAME_-82360_CENTER      = -82
           TKFRAME_-82360_SPEC      = 'ANGLES'
           TKFRAME_-82360_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82360_ANGLES    = (-90.009796, -0.03300, 89.9148 )
           TKFRAME_-82360_AXES      = (  1,         2,        3      )
           TKFRAME_-82360_UNITS     = 'DEGREES'
 
           \begintext
 
 
Imaging Science Subsystem Wide Angle Camera (ISS_WAC)
 
   The ISS WAC points nominally along the spacecraft -Y axis. The following
   frame definition encapsulates this nominal frame.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_ISS_WAC    = -82361
           FRAME_-82361_NAME        = 'CASSINI_ISS_WAC'
           FRAME_-82361_CLASS       = 4
           FRAME_-82361_CLASS_ID    = -82361
           FRAME_-82361_CENTER      = -82
           TKFRAME_-82361_SPEC      = 'ANGLES'
           TKFRAME_-82361_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82361_ANGLES    = (   -90.0,     0.0,     90.0 )
           TKFRAME_-82361_AXES      = (     1,       2,        3   )
           TKFRAME_-82361_UNITS     = 'DEGREES'
 
 
   [6] describes the inflight calibration of ISS that was the result of the
   CICLOPS (Cassini Imaging Central Laboratory for Operations) analysis of 36
   WAC images taken during ICO (Instrument Checkout). At this time the images
   taken were only sufficient to develop the location of the WAC's optical
   axis. There are three determinations of this axes location in the
   spacecraft frame. In [7] V.Haemmerle suggests that the 2-parameter fit
   average coupled with nominal twist would be the safest assumption to
   determine the frame transformation from ISS_WAC to the AACS body frame. The
   rotation matrix that takes ISS_WAC vectors into the spacecraft frame would
   be constructed as follows:
 
           [     ]    [             ]  [              ]  [     ]
           [ ROT ]  = [ +89.9116120 ]  [ -90.00059931 ]  [ 0.0 ]
           [     ]    [             ]  [              ]  [     ]
                                     Z                 Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   These angles were computed using the assumption that the WAC optical axis
   lies along the vector:
 
                                     [  0.00154266 ]
           WAC Optical Axis Vector = [ -0.99999881 ]
                                     [ -0.00001046 ]
 
   in AACS body coordinates. Further we assume nominal twist, hence the first
   rotation about Z is 0.0 degrees.
 
 
           FRAME_CASSINI_ISS_WAC    = -82361
           FRAME_-82361_NAME        = 'CASSINI_ISS_WAC'
           FRAME_-82361_CLASS       = 4
           FRAME_-82361_CLASS_ID    = -82361
           FRAME_-82361_CENTER      = -82
           TKFRAME_-82361_SPEC      = 'ANGLES'
           TKFRAME_-82361_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82361_ANGLES    = (   89.9116120,  -90.00059931,  0.0 )
           TKFRAME_-82361_AXES      = (            3,             2,    3 )
           TKFRAME_-82361_UNITS     = 'DEGREES'
 
 
   From [11]:
 
   ``The WAC boresight is not precisely aligned with the S/C -Y body vector.
   Its alignment was determined during ICO-1 ISS observations of Spica, when
   the spacecraft was using SRU-B for orientation determination. The alignment
   parameters cited under Change Request take into account the offset, as
   determined by AACS, between SRU-A and SRU-B.''
 
   Taking the boresight from the ECR ([11]):
 
                                     [  0.0013481161  ]
           WAC Optical Axis Vector = [ -0.99999894    ]
                                     [  0.00054612156 ]
 
   and assuming no twist, we derive the following angles:
 
           [     ]    [             ]  [              ]  [     ]
           [ ROT ]  = [ +89.9227586 ]  [ -89.96870954 ]  [ 0.0 ]
           [     ]    [             ]  [              ]  [     ]
                                     Z                 Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   These angles were computed using the assumption that the WAC optical axis
   lies along the vector listed above. Further we assume nominal twist, hence
   the first rotation about Z is 0.0 degrees.
 
 
           FRAME_CASSINI_ISS_WAC    = -82361
           FRAME_-82361_NAME        = 'CASSINI_ISS_WAC'
           FRAME_-82361_CLASS       = 4
           FRAME_-82361_CLASS_ID    = -82361
           FRAME_-82361_CENTER      = -82
           TKFRAME_-82361_SPEC      = 'ANGLES'
           TKFRAME_-82361_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82361_ANGLES    = (   89.9227586,  -89.96870954,  0.0 )
           TKFRAME_-82361_AXES      = (            3,             2,    3 )
           TKFRAME_-82361_UNITS     = 'DEGREES'
 
 
   From [16]:
 
   ``The following results were obtained by using 3 long exposure Fomalhaut
   WAC frames, each using 12 stars near the center of frame and using a 3
   parameter fit (shift in line, shift in sample and rotation about optic
   axis).''
 
              AACS       WAC boresight
 
               X         0.00121834 +/- 0.00000078
               Y        -0.99999923 +/- 0.00000001
               Z         0.00025445 +/- 0.00000094
 
   The results of the Fomalhaut image calibrations produced the following
   update to the ISS_WAC frame defintion:
 
   The rotation matrix that takes vectors represented in the ISS_NAC frame
   into the spacecraft frame follows:
 
           [     ]    [            ]  [           ]  [         ]
           [ ROT ]  = [ -89.985421 ]  [ -0.069806 ]  [ 89.9736 ]
           [     ]    [            ]  [           ]  [         ]
                                    X              Y            Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_ISS_WAC    = -82361
           FRAME_-82361_NAME        = 'CASSINI_ISS_WAC'
           FRAME_-82361_CLASS       = 4
           FRAME_-82361_CLASS_ID    = -82361
           FRAME_-82361_CENTER      = -82
           TKFRAME_-82361_SPEC      = 'ANGLES'
           TKFRAME_-82361_RELATIVE  = 'CASSINI_SC_COORD'
           TKFRAME_-82361_ANGLES    = ( -89.985421,  -0.069806,  89.9736 )
           TKFRAME_-82361_AXES      = (   1,          2,          3      )
           TKFRAME_-82361_UNITS     = 'DEGREES'
 
           \begintext
 
 
ISS Radiators (ISS_NAC_RAD and ISS_WAC_RAD)
 
   The ISS radiators are nominally oriented with their +Z axes directed down
   the spacecraft +X axis.
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   As [17] indicates, both the ISS_NAC_RAD and ISS_WAC_RAD boresights are
   nominally aligned with the +X axis in the spacecraft frame. By the
   methodology outlined above, this produces the following frame definitions:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ -90.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               Z          X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_ISS_NAC_RAD = -82368
           FRAME_-82368_NAME         = 'CASSINI_ISS_NAC_RAD'
           FRAME_-82368_CLASS        = 4
           FRAME_-82368_CLASS_ID     = -82368
           FRAME_-82368_CENTER       = -82
           TKFRAME_-82368_SPEC       = 'ANGLES'
           TKFRAME_-82368_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82368_ANGLES     = ( -90.0,   -90.0,     0.0 )
           TKFRAME_-82368_AXES       = (     3,       1,       3 )
           TKFRAME_-82368_UNITS      = 'DEGREES'
 
           \begintext
 
   Since the boresights are the same, both frame definitions are also the
   same, thus we have:
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_ISS_WAC_RAD = -82369
           FRAME_-82369_NAME         = 'CASSINI_ISS_WAC_RAD'
           FRAME_-82369_CLASS        = 4
           FRAME_-82369_CLASS_ID     = -82369
           FRAME_-82369_CENTER       = -82
           TKFRAME_-82369_SPEC       = 'ANGLES'
           TKFRAME_-82369_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82369_ANGLES     = ( -90.0,   -90.0,     0.0 )
           TKFRAME_-82369_AXES       = (     3,       1,       3 )
           TKFRAME_-82369_UNITS      = 'DEGREES'
 
           \begintext
 
 
CIRS Frames
----------------------------------------------------------
 
   The Composite Infrared Spectrometer (CIRS) is mounted on the remote sensing
   pallet on the +X side of the Cassini spacecraft, and nominally directed
   along the -Y axis of the AACS body frame.
 
   Note the angles in the frame definitions are specified for the "from
   instrument to (relative to) base frame" transformation.
 
 
Composite Infrared Spectrometer Focal Plane Boresight (CIRS_FPB)
 
   The CIRS FPB points nominally along the spacecraft -Y axis. The rotation
   matrix that takes vectors represented in the CIRS_FPB frame into the
   spacecraft frame follows:
 
           [     ]    [     ]  [      ]  [     ]
           [ ROT ]  = [ 0.0 ]  [-90.0 ]  [ 0.0 ]
           [     ]    [     ]  [      ]  [     ]
                             Z         X        Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The following frame definition encapsulates this nominal frame:
 
           Nominal Frame Definition
 
           FRAME_CASSINI_CIRS_FPB    = -82893
           FRAME_-82893_NAME         = 'CASSINI_CIRS_FPB'
           FRAME_-82893_CLASS        = 4
           FRAME_-82893_CLASS_ID     = -82893
           FRAME_-82893_CENTER       = -82
           TKFRAME_-82893_SPEC       = 'ANGLES'
           TKFRAME_-82893_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82893_ANGLES     = (  0.0,    -90.0,    0.0 )
           TKFRAME_-82893_AXES       = (    3,        1,      2 )
           TKFRAME_-82893_UNITS      = 'DEGREES'
 
 
   ECR 100515 [39] included mounting alignment updates for all CIRS focal
   planes. The optical boresight has moved from it's nominal configuration of
   the -Y axis in the spacecraft frame 1.7 milliradians towards +X and -0.04
   milliradians towards -Z. The rotation matrix that takes vectors represented
   in the CIRS_FPB frame into the spacecraft frame follows:
 
           [     ]    [     ]  [                  ]  [                ]
           [ ROT ]  = [ 0.0 ]  [ -90.002291831180 ]  [ -0.09740282517 ]
           [     ]    [     ]  [                  ]  [                ]
                             Z                     X                   Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_CIRS_FPB    = -82893
           FRAME_-82893_NAME         = 'CASSINI_CIRS_FPB'
           FRAME_-82893_CLASS        = 4
           FRAME_-82893_CLASS_ID     = -82893
           FRAME_-82893_CENTER       = -82
           TKFRAME_-82893_SPEC       = 'ANGLES'
           TKFRAME_-82893_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82893_ANGLES     = (
                                          0.0
                                        -90.0022918311805233
                                         -0.097402825172240,
                                       )
           TKFRAME_-82893_AXES       = (  3,  1,  2 )
 
           TKFRAME_-82893_UNITS      = 'DEGREES'
 
           \begintext
 
 
Composite Infrared Spectrometer Focal Plane #1 (CIRS_FP1)
 
   The CIRS FP1 points nominally along the spacecraft -Y axis. The following
   frame definition encapsulates this nominal frame.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_CIRS_FP1    = -82890
           FRAME_-82890_NAME         = 'CASSINI_CIRS_FP1'
           FRAME_-82890_CLASS        = 4
           FRAME_-82890_CLASS_ID     = -82890
           FRAME_-82890_CENTER       = -82
           TKFRAME_-82890_SPEC       = 'ANGLES'
           TKFRAME_-82890_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82890_ANGLES     = (  -90.0,     0.0,    90.0 )
           TKFRAME_-82890_AXES       = (      1,       2,       3 )
           TKFRAME_-82890_UNITS      = 'DEGREES'
 
 
   [9] and [10] describe the most up to date values the orientation of the
   CIRS focal planes. The rotation matrix that takes vectors represented in
   the CIRS_FP1 frame into the CIRS_FPB frame follows:
 
           [     ]    [     ]  [     ]  [            ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 0.23319382 ]
           [     ]    [     ]  [     ]  [            ]
                             Z        X               Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The angles were computed from [10].
 
 
           FRAME_CASSINI_CIRS_FP1    = -82890
           FRAME_-82890_NAME         = 'CASSINI_CIRS_FP1'
           FRAME_-82890_CLASS        = 4
           FRAME_-82890_CLASS_ID     = -82890
           FRAME_-82890_CENTER       = -82
           TKFRAME_-82890_SPEC       = 'ANGLES'
           TKFRAME_-82890_RELATIVE   = 'CASSINI_CIRS_FPB'
           TKFRAME_-82890_ANGLES     = (  0.0,     0.0,   -0.23319382 )
           TKFRAME_-82890_AXES       = (    3,       1,             2 )
           TKFRAME_-82890_UNITS      = 'DEGREES'
 
 
   [39] introduces new offsets for the FP1 from the optical boresight (FPB).
   They are: 3.98 milliradians towards +X in the spacecraft frame and 0.07
   milliradians towards +Z in the spacecraft frame. These offsets result in
   the following rotation matrix:
 
           [     ]    [     ]  [                 ]  [                ]
           [ ROT ]  = [ 0.0 ]  [ 0.0040107045659 ]  [ -0.22803720246 ]
           [     ]    [     ]  [                 ]  [                ]
                             Z                    X                   Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_CIRS_FP1    = -82890
           FRAME_-82890_NAME         = 'CASSINI_CIRS_FP1'
           FRAME_-82890_CLASS        = 4
           FRAME_-82890_CLASS_ID     = -82890
           FRAME_-82890_CENTER       = -82
           TKFRAME_-82890_SPEC       = 'ANGLES'
           TKFRAME_-82890_RELATIVE   = 'CASSINI_CIRS_FPB'
           TKFRAME_-82890_ANGLES     = (
                                          0.0,
                                          4.0107045659158E-03,
                                         -2.2803720246207E-01
                                       )
           TKFRAME_-82890_AXES       = (  3,  1,  2 )
           TKFRAME_-82890_UNITS      = 'DEGREES'
 
           \begintext
 
 
Composite Infrared Spectrometer Focal Plane #3 (CIRS_FP3)
 
   The CIRS FP3 points nominally along the spacecraft -Y axis. The following
   frame definition encapsulates this nominal frame.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_CIRS_FP3    = -82891
           FRAME_-82891_NAME         = 'CASSINI_CIRS_FP3'
           FRAME_-82891_CLASS        = 4
           FRAME_-82891_CLASS_ID     = -82891
           FRAME_-82891_CENTER       = -82
           TKFRAME_-82891_SPEC       = 'ANGLES'
           TKFRAME_-82891_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82891_ANGLES     = (  -90.0,     0.0,    90.0 )
           TKFRAME_-82891_AXES       = (      1,       2,       3 )
           TKFRAME_-82891_UNITS      = 'DEGREES'
 
 
   [9] and [10] describe the most up to date values the orientation of the
   CIRS focal planes. The rotation matrix that takes vectors represented in
   the CIRS_FP3 frame into the CIRS_FPB frame follows:
 
           [     ]    [     ]  [     ]  [             ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 0.002549662 ]
           [     ]    [     ]  [     ]  [             ]
                             Z        X                Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The angles were computed from [10] with updates from [15].
 
 
           FRAME_CASSINI_CIRS_FP3    = -82891
           FRAME_-82891_NAME         = 'CASSINI_CIRS_FP3'
           FRAME_-82891_CLASS        = 4
           FRAME_-82891_CLASS_ID     = -82891
           FRAME_-82891_CENTER       = -82
           TKFRAME_-82891_SPEC       = 'ANGLES'
           TKFRAME_-82891_RELATIVE   = 'CASSINI_CIRS_FPB'
           TKFRAME_-82891_ANGLES     = ( 0.0,     0.0,   0.02549662 )
           TKFRAME_-82891_AXES       = (   3,       1,            2 )
           TKFRAME_-82891_UNITS      = 'DEGREES'
 
 
   [39] includes an update to the offset of CASSINI_CIRS_FP3 from
   CASSINI_CIRS_FPB. Instead of 0.445 milliradians, the new value is 0.47
   milliradians of separation between the optical boresight and focal plane
   3's boresight. The rotation matrix that takes vectors represented in the
   CIRS_FP3 frame into the CIRS_FPB frame follows:
 
           [     ]    [     ]  [     ]  [             ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 0.002692902 ]
           [     ]    [     ]  [     ]  [             ]
                             Z        X                Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_CIRS_FP3    = -82891
           FRAME_-82891_NAME         = 'CASSINI_CIRS_FP3'
           FRAME_-82891_CLASS        = 4
           FRAME_-82891_CLASS_ID     = -82891
           FRAME_-82891_CENTER       = -82
           TKFRAME_-82891_SPEC       = 'ANGLES'
           TKFRAME_-82891_RELATIVE   = 'CASSINI_CIRS_FPB'
           TKFRAME_-82891_ANGLES     = ( 0.0,  0.0,  2.6929016371149E-02 )
           TKFRAME_-82891_AXES       = (   3,    1,                    2 )
           TKFRAME_-82891_UNITS      = 'DEGREES'
 
           \begintext
 
 
Composite Infrared Spectrometer Focal Plane #4 (CIRS_FP4)
 
   The CIRS FP4 points nominally along the spacecraft -Y axis. The following
   frame definition encapsulates this nominal frame.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_CIRS_FP4    = -82892
           FRAME_-82892_NAME         = 'CASSINI_CIRS_FP4'
           FRAME_-82892_CLASS        = 4
           FRAME_-82892_CLASS_ID     = -82892
           FRAME_-82892_CENTER       = -82
           TKFRAME_-82892_SPEC       = 'ANGLES'
           TKFRAME_-82892_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82892_ANGLES     = (  -90.0,     0.0,    90.0 )
           TKFRAME_-82892_AXES       = (      1,       2,       3 )
           TKFRAME_-82892_UNITS      = 'DEGREES'
 
 
   [9] and [10] describe the most up to date values the orientation of the
   CIRS focal planes. The rotation matrix that takes vectors represented in
   the CIRS_FP4 frame into the CIRS_FPB frame follows:
 
           [     ]    [     ]  [     ]  [             ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -0.02549662 ]
           [     ]    [     ]  [     ]  [             ]
                             Z        X                Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The angles were taken directly from [10] with updates from [15].
 
 
           FRAME_CASSINI_CIRS_FP4    = -82892
           FRAME_-82892_NAME         = 'CASSINI_CIRS_FP4'
           FRAME_-82892_CLASS        = 4
           FRAME_-82892_CLASS_ID     = -82892
           FRAME_-82892_CENTER       = -82
           TKFRAME_-82892_SPEC       = 'ANGLES'
           TKFRAME_-82892_RELATIVE   = 'CASSINI_CIRS_FPB'
           TKFRAME_-82892_ANGLES     = ( 0.0,     0.0,   -0.02549662 )
           TKFRAME_-82892_AXES       = (   3,       1,             2 )
           TKFRAME_-82892_UNITS      = 'DEGREES'
 
 
   [39] includes an update to the offset of CASSINI_CIRS_FP4 from
   CASSINI_CIRS_FPB. Instead of 0.445 milliradians, the new value is 0.47
   milliradians of separation between the optical boresight and focal plane
   4's boresight. The rotation matrix that takes vectors represented in the
   CIRS_FP4 frame into the CIRS_FPB frame follows:
 
           [     ]    [     ]  [     ]  [              ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -0.002692902 ]
           [     ]    [     ]  [     ]  [              ]
                             Z        X                 Y
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_CIRS_FP4    = -82892
           FRAME_-82892_NAME         = 'CASSINI_CIRS_FP4'
           FRAME_-82892_CLASS        = 4
           FRAME_-82892_CLASS_ID     = -82892
           FRAME_-82892_CENTER       = -82
           TKFRAME_-82892_SPEC       = 'ANGLES'
           TKFRAME_-82892_RELATIVE   = 'CASSINI_CIRS_FPB'
           TKFRAME_-82892_ANGLES     = ( 0.0,  0.0,  -2.6929016371149E-02 )
           TKFRAME_-82892_AXES       = (   3,    1,                     2 )
           TKFRAME_-82892_UNITS      = 'DEGREES'
 
           \begintext
 
 
CIRS Radiator (CIRS_RAD)
 
   The CIRS radiator is nominally oriented with its +Z axis directed down the
   spacecraft +X axis.
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   As [17] indicates, the CIRS_RAD boresight is nominally aligned with the +X
   axis in the spacecraft frame. By the methodology outlined above, this
   produces the following frame definition:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ -90.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               Z          X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_CIRS_RAD    = -82898
           FRAME_-82898_NAME         = 'CASSINI_CIRS_RAD'
           FRAME_-82898_CLASS        = 4
           FRAME_-82898_CLASS_ID     = -82898
           FRAME_-82898_CENTER       = -82
           TKFRAME_-82898_SPEC       = 'ANGLES'
           TKFRAME_-82898_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82898_ANGLES     = ( -90.0,   -90.0,     0.0 )
           TKFRAME_-82898_AXES       = (     3,       1,       3 )
           TKFRAME_-82898_UNITS      = 'DEGREES'
 
           \begintext
 
 
UVIS Frames
----------------------------------------------------------
 
   The Ultraviolet Imaging Spectrograph (UVIS) is mounted on the remote
   sensing pallet on the +X side of the Cassini spacecraft, and nominally
   directed along the -Y axis of the AACS body frame.
 
   Note the angles in the frame definitions are specified for the "from
   instrument to (relative to) base frame" transformation.
 
 
Ultraviolet Imaging Spectrograph Far Ultraviolet Spectrograph (UVIS_FUV)
 
   An examination of [5] reveals that UVIS_FUV points nominally along the
   spacecraft -Y axis. The rotation matrix that takes vectors represented in
   the UVIS_FUV frame into the spacecraft frame follows:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The following frame definition describes this nominal frame:
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_UVIS_FUV    = -82840
           FRAME_-82840_NAME         = 'CASSINI_UVIS_FUV'
           FRAME_-82840_CLASS        = 4
           FRAME_-82840_CLASS_ID     = -82840
           FRAME_-82840_CENTER       = -82
           TKFRAME_-82840_SPEC       = 'ANGLES'
           TKFRAME_-82840_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82840_ANGLES     = ( 0.0,     0.0,   -90.0 )
           TKFRAME_-82840_AXES       = (   3,       2,       1 )
           TKFRAME_-82840_UNITS      = 'DEGREES'
 
 
   From [40], the UVIS_FUV boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                       [  0.0002     ]
           UVIS_FUV Boresight Vector = [ -0.99999998 ]
                                       [  0.0001     ]
 
   This leads to the following rotation matrix that takes vectors represented
   in the UVIS_FUV frame into the spacecraft frame:
 
           [     ]    [          ]  [           ]  [          ]
           [ ROT ]  = [ -89.9999 ]  [ -0.011459 ]  [ 0.005729 ]
           [     ]    [          ]  [           ]  [          ]
                                  X              Y             X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_UVIS_FUV    = -82840
           FRAME_-82840_NAME         = 'CASSINI_UVIS_FUV'
           FRAME_-82840_CLASS        = 4
           FRAME_-82840_CLASS_ID     = -82840
           FRAME_-82840_CENTER       = -82
           TKFRAME_-82840_SPEC       = 'ANGLES'
           TKFRAME_-82840_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82840_ANGLES     = (
                                         -89.999999,
                                          -0.01145916,
                                           0.005729578
                                       )
           TKFRAME_-82840_AXES       = ( 1, 2, 1 )
           TKFRAME_-82840_UNITS      = 'DEGREES'
 
           \begintext
 
 
Ultraviolet Imaging Spectrograph Extreme Ultraviolet Spectrograph (UVIS_EUV)
 
   An examination of [5] reveals that the UVIS_EUV points nominally along the
   spacecraft -Y axis. The rotation matrix that takes vectors represented in
   the UVIS_EUV frame into the spacecraft frame follows:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The following frame definition describes this nominal frame:
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_UVIS_EUV    = -82842
           FRAME_-82842_NAME         = 'CASSINI_UVIS_EUV'
           FRAME_-82842_CLASS        = 4
           FRAME_-82842_CLASS_ID     = -82842
           FRAME_-82842_CENTER       = -82
           TKFRAME_-82842_SPEC       = 'ANGLES'
           TKFRAME_-82842_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82842_ANGLES     = ( 0.0,     0.0,   -90.0 )
           TKFRAME_-82842_AXES       = (   3,       2,       1 )
           TKFRAME_-82842_UNITS      = 'DEGREES'
 
 
   From [40], the UVIS_EUV boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                       [  0.0012     ]
           UVIS_EUV Boresight Vector = [ -0.99999843 ]
                                       [  0.0013     ]
 
   This leads to the following rotation matrix that takes vectors represented
   in the UVIS_EUV frame into the spacecraft frame:
 
           [     ]    [          ]  [           ]  [          ]
           [ ROT ]  = [ -89.8984 ]  [ -0.068755 ]  [ -0.02704 ]
           [     ]    [          ]  [           ]  [          ]
                                  X              Y             X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_UVIS_EUV    = -82842
           FRAME_-82842_NAME         = 'CASSINI_UVIS_EUV'
           FRAME_-82842_CLASS        = 4
           FRAME_-82842_CLASS_ID     = -82842
           FRAME_-82842_CENTER       = -82
           TKFRAME_-82842_SPEC       = 'ANGLES'
           TKFRAME_-82842_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82842_ANGLES     = (
                                         -89.8984716,
                                          -0.068755,
                                          -0.027044457
                                       )
           TKFRAME_-82842_AXES       = ( 1, 2, 1 )
           TKFRAME_-82842_UNITS      = 'DEGREES'
 
           \begintext
 
 
Ultraviolet Imaging Spectrograph Solar Occultation Port (UVIS_SOLAR)
 
   [29] and [30] indicate that the UVIS solar occultation port points
   nominally 20 degrees offset from the nominal UVIS boresights in the -Y
   direction of the nominal instrument frames. The rotation matrix that takes
   vectors represented in the CASSINI_UVIS_SOLAR frame into the
   CASSINI_SC_COORD frame follows:
 
           [     ]    [     ]  [     ]  [        ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -110.0 ]
           [     ]    [     ]  [     ]  [        ]
                             Z        Y           X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_UVIS_SOLAR    = -82843
           FRAME_-82843_NAME           = 'CASSINI_UVIS_SOLAR'
           FRAME_-82843_CLASS          = 4
           FRAME_-82843_CLASS_ID       = -82843
           FRAME_-82843_CENTER         = -82
           TKFRAME_-82843_SPEC         = 'ANGLES'
           TKFRAME_-82843_RELATIVE     = 'CASSINI_SC_COORD'
           TKFRAME_-82843_ANGLES       = ( 0.0,     0.0,   -110.0 )
           TKFRAME_-82843_AXES         = (   3,       2,      1   )
           TKFRAME_-82843_UNITS        = 'DEGREES'
 
           \begintext
 
 
Ultraviolet Imaging Spectrograph High Speed Photometer (UVIS_HSP)
 
   An examination of [5] reveals that the UVIS_HSP points nominally along the
   spacecraft -Y axis. The rotation matrix that takes vectors represented in
   the UVIS_HSP frame into the spacecraft frame follows:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The following frame definition describes this nominal frame:
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_UVIS_HSP    = -82844
           FRAME_-82844_NAME         = 'CASSINI_UVIS_HSP'
           FRAME_-82844_CLASS        = 4
           FRAME_-82844_CLASS_ID     = -82844
           FRAME_-82844_CENTER       = -82
           TKFRAME_-82844_SPEC       = 'ANGLES'
           TKFRAME_-82844_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82844_ANGLES     = ( 0.0,     0.0,   -90.0 )
           TKFRAME_-82844_AXES       = (   3,       2,       1 )
           TKFRAME_-82844_UNITS      = 'DEGREES'
 
 
   From [40], the UVIS_HSP boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                       [  0.0012     ]
           UVIS_HSP Boresight Vector = [ -0.99999856 ]
                                       [ -0.0012     ]
 
   This leads to the following rotation matrix that takes vectors represented
   in the UVIS_HSP frame into the spacecraft frame:
 
           [     ]    [          ]  [           ]  [          ]
           [ ROT ]  = [ -89.9028 ]  [ -0.068755 ]  [ -0.16599 ]
           [     ]    [          ]  [           ]  [          ]
                                  X              Y             X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_UVIS_HSP    = -82844
           FRAME_-82844_NAME         = 'CASSINI_UVIS_HSP'
           FRAME_-82844_CLASS        = 4
           FRAME_-82844_CLASS_ID     = -82844
           FRAME_-82844_CENTER       = -82
           TKFRAME_-82844_SPEC       = 'ANGLES'
           TKFRAME_-82844_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82844_ANGLES     = (
                                         -89.9027658,
                                          -0.068755,
                                          -0.1659861
                                       )
           TKFRAME_-82844_AXES       = ( 1, 2, 1 )
           TKFRAME_-82844_UNITS      = 'DEGREES'
 
           \begintext
 
 
Ultraviolet Imaging Spectrograph Hydrogen - Deuterium Absorption Cell
(UVIS_HDAC)
 
   An examination of [5] reveals that the UVIS_HDAC points nominally along the
   spacecraft -Y axis. The rotation matrix that takes vectors represented in
   the UVIS_HSP frame into the spacecraft frame follows:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   The following frame definition describes this nominal frame:
 
           \begindata
 
           FRAME_CASSINI_UVIS_HDAC   = -82845
           FRAME_-82845_NAME         = 'CASSINI_UVIS_HDAC'
           FRAME_-82845_CLASS        = 4
           FRAME_-82845_CLASS_ID     = -82845
           FRAME_-82845_CENTER       = -82
           TKFRAME_-82845_SPEC       = 'ANGLES'
           TKFRAME_-82845_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82845_ANGLES     = ( 0.0,     0.0,   -90.0 )
           TKFRAME_-82845_AXES       = (   3,       2,       1 )
           TKFRAME_-82845_UNITS      = 'DEGREES'
 
           \begintext
 
 
VIMS Frames
----------------------------------------------------------
 
   The Visible and Infrared Mapping Spectrometer is mounted on the remote
   sensing pallet on the +X side of the Cassini spacecraft, and nominally
   directed along the -Y axis of the AACS body frame.
 
   Note the angles in the frame definitions are specified for the ``from
   instrument to (relative to) base frame'' transformation.
 
 
Visible and Infrared Mapping Spectrometer Visible (VIMS_V)
 
   The VIMS_V detector points nominally along the spacecraft -Y axis. The
   following frame definition encapsulates this nominal frame.
 
   From [13]:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_VIMS_V      = -82370
           FRAME_-82370_NAME         = 'CASSINI_VIMS_V'
           FRAME_-82370_CLASS        = 4
           FRAME_-82370_CLASS_ID     = -82370
           FRAME_-82370_CENTER       = -82
           TKFRAME_-82370_SPEC       = 'ANGLES'
           TKFRAME_-82370_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82370_ANGLES     = (  0.0,     0.0,    -90.0 )
           TKFRAME_-82370_AXES       = (    3,       2,        1 )
           TKFRAME_-82370_UNITS      = 'DEGREES'
 
           \begintext
 
 
Visible and Infrared Mapping Spectrometer Infrared (VIMS_IR)
 
   The VIMS_IR detector points nominally along the spacecraft -Y axis. The
   following frame definition encapsulates this nominal frame.
 
   From [13]:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_VIMS_IR     = -82371
           FRAME_-82371_NAME         = 'CASSINI_VIMS_IR'
           FRAME_-82371_CLASS        = 4
           FRAME_-82371_CLASS_ID     = -82371
           FRAME_-82371_CENTER       = -82
           TKFRAME_-82371_SPEC       = 'ANGLES'
           TKFRAME_-82371_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82371_ANGLES     = (  0.0,     0.0,    -90.0 )
           TKFRAME_-82371_AXES       = (    3,       2,        1 )
           TKFRAME_-82371_UNITS      = 'DEGREES'
 
 
   From [41], the VIMS_IR boresight has been adjusted to the following vector
   in spacecraft coordinates:
 
                                      [  0.0021251 ]
           VIMS_IR Boresight Vector = [ -0.9999974 ]
                                      [ -0.0008495 ]
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   By the methodology outlined above, this produces the following frame
   definition:
 
           [     ]    [             ]  [             ]  [     ]
           [ ROT ]  = [ -360.121759 ]  [ -90.0486727 ]  [ 0.0 ]
           [     ]    [             ]  [             ]  [     ]
                                     Z                X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_VIMS_IR     = -82371
           FRAME_-82371_NAME         = 'CASSINI_VIMS_IR'
           FRAME_-82371_CLASS        = 4
           FRAME_-82371_CLASS_ID     = -82371
           FRAME_-82371_CENTER       = -82
           TKFRAME_-82371_SPEC       = 'ANGLES'
           TKFRAME_-82371_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82371_ANGLES     = (
                                         -360.12175939433,
                                          -90.048672769633,
                                            0.0
                                       )
           TKFRAME_-82371_AXES       = ( 3, 1, 3 )
           TKFRAME_-82371_UNITS      = 'DEGREES'
 
           \begintext
 
 
Visible and Infrared Mapping Spectrometer Infrared Solar Port (VIMS_IR_SOL)
 
   [28] indicates that the VIMS IR channel solar port points nominally 20
   degrees offset from the VIMS IR boresight in the -Y direction of the
   VIMS_IR frame. The rotation matrix that takes vectors represented in the
   VIMS_IR_SOL frame into the VIMS_IR frame follows:
 
           [     ]    [     ]  [     ]  [       ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ -20.0 ]
           [     ]    [     ]  [     ]  [       ]
                             Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
 
           FRAME_CASSINI_VIMS_IR_SOL   = -82372
           FRAME_-82372_NAME           = 'CASSINI_VIMS_IR_SOL'
           FRAME_-82372_CLASS          = 4
           FRAME_-82372_CLASS_ID       = -82372
           FRAME_-82372_CENTER         = -82
           TKFRAME_-82372_SPEC         = 'ANGLES'
           TKFRAME_-82372_RELATIVE     = 'CASSINI_VIMS_IR'
           TKFRAME_-82372_ANGLES       = ( 0.0,     0.0,   -20.0 )
           TKFRAME_-82372_AXES         = (   3,       2,       1 )
           TKFRAME_-82372_UNITS        = 'DEGREES'
 
 
   [42] requested that CASSINI_VIMS_IR_SOL be referenced directly to
   CASSINI_SC_COORD. In addition, [42], [43], and [45] also carry updates to
   the alignment of the solar port. The rotation matrix that takes vectors
   represented in the VIMS_IR_SOL frame into the CASSINI_SC_COORD frame
   follows:
 
           [     ]    [                 ]  [     ]  [                ]
           [ ROT ]  = [ 0.0859436686699 ]  [ 0.0 ]  [ -110.630253571 ]
           [     ]    [                 ]  [     ]  [                ]
                                         Z        Y                   X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_VIMS_IR_SOL   = -82372
           FRAME_-82372_NAME           = 'CASSINI_VIMS_IR_SOL'
           FRAME_-82372_CLASS          = 4
           FRAME_-82372_CLASS_ID       = -82372
           FRAME_-82372_CENTER         = -82
           TKFRAME_-82372_SPEC         = 'ANGLES'
           TKFRAME_-82372_RELATIVE     = 'CASSINI_SC_COORD'
           TKFRAME_-82372_ANGLES       = (    0.085943668669984,
                                              0.0,
                                           -110.63025357166      )
           TKFRAME_-82372_AXES         = (    3,    2,    1      )
           TKFRAME_-82372_UNITS        = 'DEGREES'
 
           \begintext
 
 
Visible and Infrared Mapping Spectrometer Radiator (VIMS_RAD)
 
   The VIMS radiator is nominally oriented with its +Z axis directed down the
   spacecraft +X axis. This is not the case for the radiator plate itself,
   which is mounted in the housing such that it is canted by 28.05 degrees. In
   the spacecraft coordinate frame, the cant is towards the spacecraft -Y
   axis.
 
   This, however, is not the end of the story. Thermally, the radiator housing
   and the radiator plate have interaction with respect to solar heating so
   that the effective boresight for symmetric solar heating, regardless of
   direction, is not offset by 28.05 degrees from the spacecraft +X axis.
   Initially the +Z axis of the radiator frame was determined to be the +X
   axis of the spacecraft frame. This results in the following:
 
   Since only boresight information has been provided, the frame
   transformation outlined below was constructed by computing the RA and DEC
   of the boresight vector relative to the CASSINI_SC_COORD frame. These
   angles are then utilized in the following fashion to construct the frame
   definition:
 
           [     ]    [          ]  [           ]  [     ]
           [ ROT ]  = [ -(RA+90) ]  [ -(90-DEC) ]  [ 0.0 ]
           [     ]    [          ]  [           ]  [     ]
                                  Z              X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
   This produces a frame whose Z-axis agrees with the specified boresight.
 
   As [17] indicates, the VIMS_RAD boresight is nominally aligned with the +X
   axis in the spacecraft frame. By the methodology outlined above, this
   produces the following frame definition:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ -90.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               Z          X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_VIMS_RAD    = -82378
           FRAME_-82378_NAME         = 'CASSINI_VIMS_RAD'
           FRAME_-82378_CLASS        = 4
           FRAME_-82378_CLASS_ID     = -82378
           FRAME_-82378_CENTER       = -82
           TKFRAME_-82378_SPEC       = 'ANGLES'
           TKFRAME_-82378_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82378_ANGLES     = ( -90.0,   -90.0,     0.0 )
           TKFRAME_-82378_AXES       = (     3,       1,       3 )
           TKFRAME_-82378_UNITS      = 'DEGREES'
 
 
   From [36]:
 
   A solar heating analysis and identification of a solar heating
   ``effective'' radiator boresight was performed for ECR 100325-B covering
   Flight Rule FF37B2 [37]. The solar heating analysis identified that a VIMS
   radiator boresight offset from the spacecraft +X axis in the direction of
   the spacecraft -Y axis by 4.5 degrees would define a thermally
   ``effective'' boresight.
 
   To implement this change, the rotation about the Z-axis needs to be
   increased by 4.5 degrees as follows:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ -85.5 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               Z          X        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_VIMS_RAD    = -82378
           FRAME_-82378_NAME         = 'CASSINI_VIMS_RAD'
           FRAME_-82378_CLASS        = 4
           FRAME_-82378_CLASS_ID     = -82378
           FRAME_-82378_CENTER       = -82
           TKFRAME_-82378_SPEC       = 'ANGLES'
           TKFRAME_-82378_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82378_ANGLES     = ( -85.5,   -90.0,     0.0 )
           TKFRAME_-82378_AXES       = (     3,       1,       3 )
           TKFRAME_-82378_UNITS      = 'DEGREES'
 
           \begintext
 
 
CAPS Frames
----------------------------------------------------------
 
   The Cassini Plasma Spectrometer is mounted on an actuator which is in turn
   attached to the fields and particles pallet which is roughly located on the
   -X side of the Cassini spacecraft.
 
   The actuator allows the instrument to articulate, so to make proper use of
   this frame requires a C-kernel (or set of C-kernels) with appropriate
   coverage for the epochs of interest.
 
   To connect the CASSINI_CAPS frame with the spacecraft coordinate frame
   (CASSINI_SC_COORD) two possible branches exist depending on the set of
   C-kernels loaded:
 
 
               CASSINI_SC_COORD           CASSINI_SC_COORD
               ----------------           ----------------
                      |                          |
                      |<--- fixed offset         |
                      |                          |
                      V                          |
               CASSINI_CAPS_BASE                 |
               -----------------                 |
                      |                          |
                      |<--- c-kernel             |<--- c-kernel
                      |                          |
                      V                          |
               CASSINI_CAPS_ART                  |
               ----------------                  |
                      |                          |
                      |<--- c-kernel             |
                      |                          |
                      V                          V
                 CASSINI_CAPS               CASSINI_CAPS
                 ------------               ------------
 
 
   The branch illustrated on the left of the figure above utilizes a series of
   transformations to connect the spacecraft frame with the instrument frame.
   The general strategy in this branch is the following:
 
            --   Define a fixed offset frame that connects the spacecraft
                 frame to the base or 'zero-point' of the articulation of the
                 instrument.
 
            --   Define a C-kernel based frame to perform the rotation about
                 the articulation axis.
 
            --   Define a C-kernel based frame that performs the final
                 rotation necessary to produce the instrument frame.
 
   This last frame in the absence of the right branch could be another fixed
   offset frame. However, making it a C-kernel allows the branch on the right
   to exist. This alternate route up the frame tree allows the construction
   and use of C-kernels that tie the instrument frame directly back to the
   spacecraft frame. This is often convenient for science data analysis.
 
   Without further ado, the frame defintions:
 
 
Cassini Plasma Spectrometer Zero-Articulation Base Frame (CAPS_BASE)
 
   The Z-axis of this frame is the articulation axis of CAPS. The X-axis is
   constructed by taking the vector product of the CAPS articulation axis with
   the boresight in the 'zero-angle' or base position. The Y-axis completes
   the right handed frame. The articulation axis of CAPS is the Z-axis of
   CASSINI_SC_COORD, and the boresight in its 'zero-angle' configuration is
   the negative Y-axis of this spacecraft frame, so we end up with the
   following:
 
   The rotation matrix that takes vectors represented in the CAPS_BASE frame
   into the spacecraft coordinate frame follows:
 
           [     ]    [     ]  [     ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ 0.0 ]  [ 0.0 ]
           [     ]    [     ]  [     ]  [     ]
                             Z        Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_CAPS_BASE    = -82822
           FRAME_-82822_NAME         = 'CASSINI_CAPS_BASE'
           FRAME_-82822_CLASS        = 4
           FRAME_-82822_CLASS_ID     = -82822
           FRAME_-82822_CENTER       = -82
           TKFRAME_-82822_SPEC       = 'ANGLES'
           TKFRAME_-82822_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82822_ANGLES     = (   0.0,     0.0,     0.0 )
           TKFRAME_-82822_AXES       = (     3,       2,       1 )
           TKFRAME_-82822_UNITS      = 'DEGREES'
 
           \begintext
 
 
Cassini Plasma Spectrometer Articulation Frame (CAPS_ART)
 
   The Z-axis of this frame is the articulation axis of CAPS. The X-axis is
   constructed by taking the vector product of the CAPS articulation axis with
   the boresight at some articulated position. The Y-axis completes the right
   handed frame.
 
   This frame encapsulates the articulation characteristics of the CAPS
   instrument. To make use of it requires a C-kernel with coverage at the
   epochs of interest be loaded.
 
   The rotation matrix that takes vectors from the CAPS_ART frame to the
   CAPS_BASE frame follows:
 
           [     ]    [       ]
           [ ROT ]  = [ ANGLE ]
           [     ]    [       ]
                               Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i, and ANGLE is the articulation angle.
 
           \begindata
 
           FRAME_CASSINI_CAPS_ART   = -82821
           FRAME_-82821_NAME        = 'CASSINI_CAPS_ART'
           FRAME_-82821_CLASS       = 3
           FRAME_-82821_CLASS_ID    = -82821
           FRAME_-82821_CENTER      = -82
           CK_-82821_SCLK           = -82
           CK_-82821_SPK            = -82
 
           \begintext
 
 
Cassini Plasma Spectrometer Frame (CAPS)
 
   The negative Y-axis of this frame is the instrument boresight. The Z-axis
   is defined as the articulation axis of the detectors, and the X-axis
   completes the right handed frame.
 
   This frame requires one of two possible C-kernels:
 
            --   One kernel connects this instrument frame (-82820) directly
                 to the spacecraft frame (-82000).
 
            --   The other possible kernel connects this instrument frame
                 (-82820) to the articulation frame (-82821) defined above.
                 The kernel that makes this connection for all epochs after
                 launch is delivered with the kernel set. See the kernel
                 comments for details of frame construction.
 
   One should take care in the simultaneous loading of C-kernels that utilize
   different paths of the frame tree to connect CASSINI_CAPS to
   CASSINI_SC_COORD. See [1] for details regarding C-kernel precedence.
 
           \begindata
 
           FRAME_CASSINI_CAPS       = -82820
           FRAME_-82820_NAME        = 'CASSINI_CAPS'
           FRAME_-82820_CLASS       = 3
           FRAME_-82820_CLASS_ID    = -82820
           FRAME_-82820_CENTER      = -82
           CK_-82820_SCLK           = -82
           CK_-82820_SPK            = -82
 
           \begintext
 
 
CDA Frames
----------------------------------------------------------
 
   The Cosmic Dust Analyzer is mounted on the -X side of the Cassini
   spacecraft. The entire assembly is capable of articulating from it's zero
   angle position. The following describes the boresight in the spacecraft
   frame as a function of the articulation angle a:
 
           From [18]:
 
           x = 1/8 ( -1 - SQRT(3) + (-1 + SQRT(3)) COS(a) - 2 SQRT(6) SIN(a) )
           y = 1/8 (  3 + SQRT(3) + (-3 + SQRT(3)) COS(a) - 2 SQRT(2) SIN(a) )
           z = 1/4 ( -1 + SQRT(3) + ( 1 + SQRT(3)) COS(a) )
 
 
   The actuator allows the instrument to articulate, so to make proper use of
   this frame requires a C-kernel (or set of C-kernels) with appropriate
   coverage for the epochs of interest.
 
   To connect the CASSINI_CDA frame with the spacecraft coordinate frame
   (CASSINI_SC_COORD) two possible branches exist depending on the set of
   C-kernels loaded:
 
 
               CASSINI_SC_COORD           CASSINI_SC_COORD
               ----------------           ----------------
                      |                          |
                      |<--- fixed offset         |
                      |                          |
                      V                          |
               CASSINI_CDA_BASE                  |
               ----------------                  |
                      |                          |
                      |<--- c-kernel             |<--- c-kernel
                      |                          |
                      V                          |
               CASSINI_CDA_ART                   |
               ---------------                   |
                      |                          |
                      |<--- c-kernel             |
                      |                          |
                      V                          V
                 CASSINI_CDA                CASSINI_CDA
                 -----------                -----------
 
 
   The branch illustrated on the left of the figure above utilizes a series of
   transformations to connect the spacecraft frame with the instrument frame.
   The general strategy in this branch is the following:
 
            --   Define a fixed offset frame that connects the spacecraft
                 frame to the base or 'zero-point' of the articulation of the
                 instrument.
 
            --   Define a C-kernel based frame to perform the rotation about
                 the articulation axis.
 
            --   Define a C-kernel based frame that performs the final
                 rotation necessary to produce the instrument frame.
 
   This last frame in the absence of the right branch could be another fixed
   offset frame. However, making it a C-kernel allows the branch on the right
   to exist. This alternate route up the frame tree allows the construction
   and use of C-kernels that tie the instrument frame directly back to the
   spacecraft frame. This is often convenient for science data analysis.
 
   Without further ado, the frame defintions:
 
 
Cosmic Dust Analyzer Zero-Articulation Base Frame (CDA_BASE)
 
   The Z-axis of this frame is the articulation axis of CDA. The X-axis is
   constructed by taking the vector product of the CDA articulation axis with
   the boresight in the 'zero-angle' or base position. The Y-axis completes
   the right handed frame.
 
   An examination of the relationship connecting the boresight position in the
   spacecraft frame with the articulation angle, yields the following:
 
           The articulation axis of CDA in CASSINI_SC_COORD is:
 
           (+4.8296291314453E-01, -8.3651630373781E-01, -2.5881904510252E-01)
 
           The 'zero-angle' boresight in CASSINI_SC_COORD is:
 
           (-2.5000000000000E-01, 4.3301270189222E-01, 8.6602540378444E-01)
 
 
   The articulation axis points in the opposite direction of the cone swept
   out by the boresight vectors. This was done to preserve the sense of the
   positive angle in the definition provided in [18].
 
   Computing the frame described above we end up with:
 
   The rotation matrix that takes vectors represented in the CDA_BASE frame
   into the spacecraft coordinate frame follows:
 
           [     ]    [       ]  [     ]  [       ]
           [ ROT ]  = [ 150.0 ]  [ 0.0 ]  [ 105.0 ]
           [     ]    [       ]  [     ]  [       ]
                               Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_CDA_BASE    = -82792
           FRAME_-82792_NAME         = 'CASSINI_CDA_BASE'
           FRAME_-82792_CLASS        = 4
           FRAME_-82792_CLASS_ID     = -82792
           FRAME_-82792_CENTER       = -82
           TKFRAME_-82792_SPEC       = 'ANGLES'
           TKFRAME_-82792_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82792_ANGLES     = ( 150.0,     0.0,   105.0 )
           TKFRAME_-82792_AXES       = (     3,       2,       1 )
           TKFRAME_-82792_UNITS      = 'DEGREES'
 
           \begintext
 
 
Cosmic Dust Analyzer Articulation Frame (CDA_ART)
 
   The Z-axis of this frame is the articulation axis of CDA. The X-axis is
   constructed by taking the vector product of the CDA articulation axis with
   the boresight at some articulated position. The Y-axis completes the right
   handed frame.
 
   This frame encapsulates the articulation characteristics of the CDA
   instrument. To make use of it requires a C-kernel with coverage at the
   epochs of interest be loaded.
 
   The rotation matrix that takes vectors from the CDA_ART frame to the
   CDA_BASE frame follows:
 
           [     ]    [       ]
           [ ROT ]  = [ ANGLE ]
           [     ]    [       ]
                               Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i, and ANGLE is the articulation angle.
 
           \begindata
 
           FRAME_CASSINI_CDA_ART    = -82791
           FRAME_-82791_NAME        = 'CASSINI_CDA_ART'
           FRAME_-82791_CLASS       = 3
           FRAME_-82791_CLASS_ID    = -82791
           FRAME_-82791_CENTER      = -82
           CK_-82791_SCLK           = -82
           CK_-82791_SPK            = -82
 
           \begintext
 
 
Cosmic Dust Analyzer Frame (CDA)
 
   The Z-axis of this frame is the instrument boresight. The X-axis of is the
   same as the X-axis of CASSINI_CDA_ART, and the Y-axis completes the right
   handed frame.
 
   This frame requires one of two possible C-kernels:
 
            --   One kernel connects this instrument frame (-82790) directly
                 to the spacecraft frame (-82000).
 
            --   The other possible kernel connects this instrument frame
                 (-82790) to the articulation frame (-82791) defined above.
                 The kernel that makes this connection for all epochs after
                 launch is delivered with the kernel set. See the kernel
                 comments for details of frame construction.
 
   One should take care in the simultaneous loading of C-kernels that utilize
   different paths of the frame tree to connect CASSINI_CDA to
   CASSINI_SC_COORD. See [1] for details regarding C-kernel precedence.
 
           \begindata
 
           FRAME_CASSINI_CDA        = -82790
           FRAME_-82790_NAME        = 'CASSINI_CDA'
           FRAME_-82790_CLASS       = 3
           FRAME_-82790_CLASS_ID    = -82790
           FRAME_-82790_CENTER      = -82
           CK_-82790_SCLK           = -82
           CK_-82790_SPK            = -82
 
           \begintext
 
 
INMS Frames
----------------------------------------------------------
 
   The Ion and Neutral Mass Spectrometer is mounted on the fields and
   particles pallet roughly located on the -X side of the Cassini spacecraft.
   The instrument boresight is nominally directed along the -X axis of the
   AACS body frame.
 
   Note the angles in the frame definitions are specified for the ``from
   instrument to (relative to) base frame'' transformation.
 
   From [19], we have the following nominal frame definition:
 
           [     ]    [     ]  [       ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ +90.0 ]  [ 0.0 ]
           [     ]    [     ]  [       ]  [     ]
                             x          Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_INMS        = -82740
           FRAME_-82740_NAME         = 'CASSINI_INMS'
           FRAME_-82740_CLASS        = 4
           FRAME_-82740_CLASS_ID     = -82740
           FRAME_-82740_CENTER       = -82
           TKFRAME_-82740_SPEC       = 'ANGLES'
           TKFRAME_-82740_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82740_ANGLES     = (   0.0,   +90.0,     0.0 )
           TKFRAME_-82740_AXES       = (     1,       2,       3 )
           TKFRAME_-82740_UNITS      = 'DEGREES'
 
           \begintext
 
 
MAG Frames
----------------------------------------------------------
 
   The Magnetometer is mounted on the magentometer boom which protrudes from
   the spacecraft body in the direction of the +Y axis of the AACS body frame.
   [20] establishes the need for two separate frame definitions, one for the
   Plus-X directed frame, the other for the Minux-X one.
 
   Note the angles in the frame definitions are specified for the ``from
   instrument to (relative to) base frame'' transformation.
 
 
Magnetometer Plus-X (MAG_PLUS)
 
   The MAG_PLUS detector is pointed nominally in the direction of the
   spacecraft +X axis. The following definition encapsulates this frame:
 
           From [21]:
 
           [     ]    [     ]  [       ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [     ]  [       ]  [     ]
                             X          Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_MAG_PLUS    = -82350
           FRAME_-82350_NAME         = 'CASSINI_MAG_PLUS'
           FRAME_-82350_CLASS        = 4
           FRAME_-82350_CLASS_ID     = -82350
           FRAME_-82350_CENTER       = -82
           TKFRAME_-82350_SPEC       = 'ANGLES'
           TKFRAME_-82350_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82350_ANGLES     = (   0.0,   -90.0,     0.0 )
           TKFRAME_-82350_AXES       = (     1,       2,       3 )
           TKFRAME_-82350_UNITS      = 'DEGREES'
 
           \begintext
 
 
Magnetometer Minus-X (MAG_MINUS)
 
   The MAG_MINUS detector is pointed nominally in the direction of the
   spacecraft -X axis. The following definition encapsulates this frame:
 
           From [21]:
 
           [     ]    [     ]  [      ]  [     ]
           [ ROT ]  = [ 0.0 ]  [ 90.0 ]  [ 0.0 ]
           [     ]    [     ]  [      ]  [     ]
                             X         Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_MAG_MINUS   = -82351
           FRAME_-82351_NAME         = 'CASSINI_MAG_MINUS'
           FRAME_-82351_CLASS        = 4
           FRAME_-82351_CLASS_ID     = -82351
           FRAME_-82351_CENTER       = -82
           TKFRAME_-82351_SPEC       = 'ANGLES'
           TKFRAME_-82351_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82351_ANGLES     = (   0.0,    90.0,     0.0 )
           TKFRAME_-82351_AXES       = (     1,       2,       3 )
           TKFRAME_-82351_UNITS      = 'DEGREES'
 
           \begintext
 
 
MIMI Frames
----------------------------------------------------------
 
   Most of the components of the Magnetospheric Imaging Instrument are mounted
   on the fields and particles pallet roughly located on the -X side of the
   Cassini spacecraft. The one exception is the Ion and Neutral Camera which
   is mounted on the -Y side of the orbiter.
 
   Note the angles in the frame definitions are specified for the ``from
   instrument to (relative to) base frame'' transformation.
 
 
Magnetospheric Imaging Instrument Charge Energy Mass Spectrometer (MIMI_CHEMS)
 
   The MIMI_CHEMS detector is nominally pointed along the -X axis of the
   spacecraft frame. The following definition encapsulates this frame:
 
           From [17]:
 
           [     ]    [     ]  [      ]  [      ]
           [ ROT ]  = [ 0.0 ]  [ 90.0 ]  [ 90.0 ]
           [     ]    [     ]  [      ]  [      ]
                             X         Y         Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_MIMI_CHEMS  = -82760
           FRAME_-82760_NAME         = 'CASSINI_MIMI_CHEMS'
           FRAME_-82760_CLASS        = 4
           FRAME_-82760_CLASS_ID     = -82760
           FRAME_-82760_CENTER       = -82
           TKFRAME_-82760_SPEC       = 'ANGLES'
           TKFRAME_-82760_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82760_ANGLES     = (   0.0,   90.0,    90.0 )
           TKFRAME_-82760_AXES       = (     1,      2,       3 )
           TKFRAME_-82760_UNITS      = 'DEGREES'
 
           \begintext
 
 
Magnetospheric Imaging Instrument Ion and Neutral Camera (MIMI_INCA)
 
   The MIMI_INCA detector is nominally pointed along the -Y axis of the
   spacecraft frame with a 9.5 degree offset in the direction of the +X axis
   of the spacecraft. The following definition encapsulates this frame:
 
           From [22]:
 
           [     ]    [       ]  [      ]  [     ]
           [ ROT ]  = [ -90.0 ]  [ -9.5 ]  [ 0.0 ]
           [     ]    [       ]  [      ]  [     ]
                               X         Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_MIMI_INCA   = -82761
           FRAME_-82761_NAME         = 'CASSINI_MIMI_INCA'
           FRAME_-82761_CLASS        = 4
           FRAME_-82761_CLASS_ID     = -82761
           FRAME_-82761_CENTER       = -82
           TKFRAME_-82761_SPEC       = 'ANGLES'
           TKFRAME_-82761_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82761_ANGLES     = ( -90.0,  -9.5,     0.0 )
           TKFRAME_-82761_AXES       = (     1,     2,       3 )
           TKFRAME_-82761_UNITS      = 'DEGREES'
 
           \begintext
 
 
Magnetospheric Imaging Instrument Low Energy Magnetospheric Measurements
(MIMI_LEMMS)
 
   The actuator allows the detectors to articulate, so to make proper use of
   this frame requires a C-kernel (or set of C-kernels) with appropriate
   coverage for the epochs of interest.
 
   To connect the CASSINI_MIMI_LEMMS1 and CASSINI_MIMI_LEMMS2 frames with the
   spacecraft coordinate frame (CASSINI_SC_COORD) two possible branches exist
   depending on the set of C-kernels loaded:
 
 
               CASSINI_SC_COORD              CASSINI_SC_COORD
               ----------------              ----------------
                      |                      |              |
                      |<--- fixed offset     |              |
                      |                      |              |
                      V                      |              |
            CASSINI_MIMI_LEMMS_BASE          |              |
            -----------------------          |              |
                      |                      |              |
                      |<--- c-kernel         |<--- c-kernel |
                      |                      |              |
                      V                      |              |
             CASSINI_MIMI_LEMMS_ART          |              |
             ----------------------          |              |
                |                |           |              |
                |<--- c-kernel   |           | c-kernel --->|
                |                |           |              |
                V                |           V              |
            CASSINI_MIMI_LEMMS1  |     CASSINI_MIMI_LEMMS1  |
            -------------------  |     -------------------  |
                                 |                          |
                    c-kernel --->|                          |
                                 |                          |
                                 V                          V
                       CASSINI_MIMI_LEMMS2          CASSINI_MIMI_LEMMS2
                       -------------------          -------------------
 
 
   The branches illustrated on the left of the figure above utilize a series
   of transformations to connect the spacecraft frame with the detector
   frames. The general strategy in these branches is the following:
 
            --   Define a fixed offset frame that connects the spacecraft
                 frame to the base or 'zero-point' of the articulation of the
                 detectors.
 
            --   Define a C-kernel based frame to perform the rotation about
                 the articulation axis.
 
            --   Define a C-kernel based frame that performs the final
                 rotation necessary to produce either of the instrument frame.
 
   These last frames (MIMI_LEMMS1 and MIMI_LEMMS2) in the absence of the right
   branch could be another fixed offset frame. However, making them C-kernels
   allows the branches on the right to exist. This alternate route up the
   frame tree allows the construction and use of C-kernels that tie the
   individual detector frames directly back to the spacecraft frame. This is
   often convenient for science data analysis.
 
   Without further ado, the frame defintions:
 
 
Magnetospheric Imaging Instrument Low Energy Magnetospheric Measurements
Zero-Articulation Base Frame (MIMI_LEMMS_BASE)
 
   The Z-axis of this frame is the articulation axis of MIMI_LEMMS. The X-axis
   is constructed by taking the vector product of the MIMI_LEMMS articulation
   axis with the MIMI_LEMMS1 boresight in the 'zero-angle' or base position.
   The Y-axis completes the right handed frame.
 
   As [33] indicates, the articulation axis is the Y-axis in CASSINI_SC_COORD
   and the 'zero-angle' boresight of MIMI_LEMMS1 is the -Z-axis in
   CASSINI_SC_COORD. Combining this information with the frame definition laid
   out above, we have:
 
   The rotation matrix that takes vectors represented in the MIMI_LEMMS_BASE
   frame into the spacecraft coordinate frame follows:
 
           [     ]    [       ]  [     ]  [       ]
           [ ROT ]  = [ 180.0 ]  [ 0.0 ]  [ -90.0 ]
           [     ]    [       ]  [     ]  [       ]
                               Z        Y          X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_MIMI_LEMMS_BASE = -82765
           FRAME_-82765_NAME             = 'CASSINI_MIMI_LEMMS_BASE'
           FRAME_-82765_CLASS            = 4
           FRAME_-82765_CLASS_ID         = -82765
           FRAME_-82765_CENTER           = -82
           TKFRAME_-82765_SPEC           = 'ANGLES'
           TKFRAME_-82765_RELATIVE       = 'CASSINI_SC_COORD'
           TKFRAME_-82765_ANGLES         = ( 180.0,     0.0,   -90.0 )
           TKFRAME_-82765_AXES           = (     3,       2,       1 )
           TKFRAME_-82765_UNITS          = 'DEGREES'
 
           \begintext
 
 
Magnetospheric Imaging Instrument Low Energy Magnetospheric Measurements
Articulation Frame (MIMI_LEMMS_ART)
 
   The Z-axis of this frame is the articulation axis of MIMI_LEMMS. The X-axis
   is constructed by taking the vector product of the MIMI_LEMMS articulation
   axis with the MIMI_LEMMS1 boresight at some articulated position. The
   Y-axis completes the right handed frame.
 
   This frame encapsulates the articulation characteristics of the MIMI_LEMMS
   instrument. To make use of it requires a C-kernel with coverage at the
   epochs of interest be loaded.
 
   The rotation matrix that takes vectors from the MIMI_LEMMS_ART frame to the
   MIMI_LEMMS_BASE frame follows:
 
           [     ]    [       ]
           [ ROT ]  = [ ANGLE ]
           [     ]    [       ]
                               Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i, and ANGLE is the articulation angle.
 
           \begindata
 
           FRAME_CASSINI_MIMI_LEMMS_ART  = -82764
           FRAME_-82764_NAME             = 'CASSINI_MIMI_LEMMS_ART'
           FRAME_-82764_CLASS            = 3
           FRAME_-82764_CLASS_ID         = -82764
           FRAME_-82764_CENTER           = -82
           CK_-82764_SCLK                = -82
           CK_-82764_SPK                 = -82
 
           \begintext
 
 
Magnetospheric Imaging Instrument Low Energy Magnetospheric Measurements 1
(MIMI_LEMMS1)
 
   The Z-axis of this frame is the instrument boresight.
 
   This frame requires one of two possible C-kernels:
 
            --   One kernel connects this instrument frame (-82762) directly
                 to the spacecraft frame (-82000).
 
            --   The other possible kernel connects this instrument frame
                 (-82762) to the articulation frame (-82764) defined above.
                 The kernel that makes this connection for all epochs after
                 launch is delivered with the kernel set. See the kernel
                 comments for details of the frame construction.
 
   One should take care in the simultaneous loading of C-kernels that utilize
   different paths of the frame tree to connect CASSINI_MIMI_LEMMS1 to
   CASSINI_SC_COORD. See [1] for details regarding C-kernel precedence.
 
           \begindata
 
           FRAME_CASSINI_MIMI_LEMMS1 = -82762
           FRAME_-82762_NAME         = 'CASSINI_MIMI_LEMMS1'
           FRAME_-82762_CLASS        = 3
           FRAME_-82762_CLASS_ID     = -82762
           FRAME_-82762_CENTER       = -82
           CK_-82762_SCLK            = -82
           CK_-82762_SPK             = -82
 
           \begintext
 
 
Magnetospheric Imaging Instrument Low Energy Magnetospheric Measurements 2
(MIMI_LEMMS2)
 
   The Z-axis of this frame is the instrument boresight.
 
   This frame requires one of two possible C-kernels:
 
            --   One kernel connects this instrument frame (-82763) directly
                 to the spacecraft frame (-82000).
 
            --   The other possible kernel connects this instrument frame
                 (-82763) to the articulation frame (-82764) defined above.
                 The kernel that makes this connection for all epochs after
                 launch is delivered with the kernel set. See the kernel
                 comments for details of the frame construction.
 
   One should take care in the simultaneous loading of C-kernels that utilize
   different paths of the frame tree to connect CASSINI_MIMI_LEMMS2 to
   CASSINI_SC_COORD. See [1] for details regarding C-kernel precedence.
 
           \begindata
 
           FRAME_CASSINI_MIMI_LEMMS2 = -82763
           FRAME_-82763_NAME         = 'CASSINI_MIMI_LEMMS2'
           FRAME_-82763_CLASS        = 3
           FRAME_-82763_CLASS_ID     = -82763
           FRAME_-82763_CENTER       = -82
           CK_-82763_SCLK            = -82
           CK_-82763_SPK             = -82
 
           \begintext
 
 
RADAR Frames
----------------------------------------------------------
 
   Compiled from [23] and [5]:
 
   The RADAR instrument consists of 5 beams in the following configuration:
 
 
                                          ^ Xsc
                                          |
                                          |
                                Ysc       |
                                   <------o
                                          |  Zsc
                                          |
 
                                          .
                                          .
                                          .
 
              Beam 1      Beam 2      Beam 3    Beam 4      Beam 5
                                          |
             |-----x-----|-----x-----|----x----|-----x-----|-----x-----|
                                          |
                   |-- 1.35 ---|-- 0.85 --|-- 0.85 --|-- 1.35 ---|
                                          |
                                          |
                                          V
 
                                   Beam 3 Direction
 
 
   The above figure illustrates the separation in degrees between the beam
   centers and their relation to the spacecraft frame.
 
   Note the angles in the frame definitions are specified fro the ``from
   instrument to (relative to) base frame'' transformation.
 
 
RADAR Beam 1 (RADAR_1)
 
   RADAR Beam 1 is directed nominally 2.2 degrees off of the -Z axis of the
   spacecraft in the direction of the +Y axis of the spacecraft frame. The
   following definition encapsulates this frame:
 
           From [23]:
 
           [     ]    [       ]  [     ]  [     ]
           [ ROT ]  = [ 177.8 ]  [ 0.0 ]  [ 0.0 ]
           [     ]    [       ]  [     ]  [     ]
                               X        Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RADAR_1     = -82810
           FRAME_-82810_NAME         = 'CASSINI_RADAR_1'
           FRAME_-82810_CLASS        = 4
           FRAME_-82810_CLASS_ID     = -82810
           FRAME_-82810_CENTER       = -82
           TKFRAME_-82810_SPEC       = 'ANGLES'
           TKFRAME_-82810_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82810_ANGLES     = ( 177.8,     0.0,     0.0 )
           TKFRAME_-82810_AXES       = (     1,       2,       3 )
           TKFRAME_-82810_UNITS      = 'DEGREES'
 
           \begintext
 
 
RADAR Beam 2 (RADAR_2)
 
   RADAR Beam 2 is directed nominally 0.85 degrees off of the -Z axis of the
   spacecraft in the direction of the +Y axis of the spacecraft frame. The
   following definition encapsulates this frame:
 
           From [23]:
 
           [     ]    [        ]  [     ]  [     ]
           [ ROT ]  = [ 179.15 ]  [ 0.0 ]  [ 0.0 ]
           [     ]    [        ]  [     ]  [     ]
                                X        Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           FRAME_CASSINI_RADAR_2     = -82811
           FRAME_-82811_NAME         = 'CASSINI_RADAR_2'
           FRAME_-82811_CLASS        = 4
           FRAME_-82811_CLASS_ID     = -82811
           FRAME_-82811_CENTER       = -82
           TKFRAME_-82811_SPEC       = 'ANGLES'
           TKFRAME_-82811_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82811_ANGLES     = ( 179.15,     0.0,     0.0 )
           TKFRAME_-82811_AXES       = (      1,       2,       3 )
           TKFRAME_-82811_UNITS      = 'DEGREES'
 
 
   From [44], the RADAR Beam 2 reference frame is to be adjusted to the
   following:
 
           [     ]    [        ]  [      ]  [     ]
           [ ROT ]  = [ 179.15 ]  [ -1.2 ]  [ 0.0 ]
           [     ]    [        ]  [      ]  [     ]
                                X         Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_RADAR_2     = -82811
           FRAME_-82811_NAME         = 'CASSINI_RADAR_2'
           FRAME_-82811_CLASS        = 4
           FRAME_-82811_CLASS_ID     = -82811
           FRAME_-82811_CENTER       = -82
           TKFRAME_-82811_SPEC       = 'ANGLES'
           TKFRAME_-82811_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82811_ANGLES     = ( 179.15,    -1.2,     0.0 )
           TKFRAME_-82811_AXES       = (      1,       2,       3 )
           TKFRAME_-82811_UNITS      = 'DEGREES'
 
           \begintext
 
 
RADAR Beam 3 (RADAR_3)
 
   RADAR Beam 3 is directed nominally along the -Z axis of the spacecraft
   frame. The following definition encapsulates this frame:
 
           From [23]:
 
           [     ]    [       ]  [     ]  [     ]
           [ ROT ]  = [ 180.0 ]  [ 0.0 ]  [ 0.0 ]
           [     ]    [       ]  [     ]  [     ]
                               X        Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RADAR_3     = -82812
           FRAME_-82812_NAME         = 'CASSINI_RADAR_3'
           FRAME_-82812_CLASS        = 4
           FRAME_-82812_CLASS_ID     = -82812
           FRAME_-82812_CENTER       = -82
           TKFRAME_-82812_SPEC       = 'ANGLES'
           TKFRAME_-82812_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82812_ANGLES     = ( 180.0,     0.0,     0.0 )
           TKFRAME_-82812_AXES       = (     1,       2,       3 )
           TKFRAME_-82812_UNITS      = 'DEGREES'
 
           \begintext
 
 
RADAR Beam 4 (RADAR_4)
 
   RADAR Beam 4 is directed nominally 0.85 degrees off of the -Z axis of the
   spacecraft in the direction of the -Y axis of the spacecraft frame. The
   following definition encapsulates this frame:
 
           From [23]:
 
           [     ]    [        ]  [     ]  [     ]
           [ ROT ]  = [ 180.85 ]  [ 0.0 ]  [ 0.0 ]
           [     ]    [        ]  [     ]  [     ]
                                X        Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RADAR_4     = -82813
           FRAME_-82813_NAME         = 'CASSINI_RADAR_4'
           FRAME_-82813_CLASS        = 4
           FRAME_-82813_CLASS_ID     = -82813
           FRAME_-82813_CENTER       = -82
           TKFRAME_-82813_SPEC       = 'ANGLES'
           TKFRAME_-82813_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82813_ANGLES     = ( 180.85,     0.0,     0.0 )
           TKFRAME_-82813_AXES       = (      1,       2,       3 )
           TKFRAME_-82813_UNITS      = 'DEGREES'
 
           \begintext
 
   From [44], the RADAR Beam 4 reference frame is to be adjusted to the
   following:
 
           [     ]    [        ]  [      ]  [     ]
           [ ROT ]  = [ 180.85 ]  [ -1.2 ]  [ 0.0 ]
           [     ]    [        ]  [      ]  [     ]
                                X         Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           \begindata
 
           FRAME_CASSINI_RADAR_4     = -82813
           FRAME_-82813_NAME         = 'CASSINI_RADAR_4'
           FRAME_-82813_CLASS        = 4
           FRAME_-82813_CLASS_ID     = -82813
           FRAME_-82813_CENTER       = -82
           TKFRAME_-82813_SPEC       = 'ANGLES'
           TKFRAME_-82813_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82813_ANGLES     = ( 180.85,    -1.2,     0.0 )
           TKFRAME_-82813_AXES       = (      1,       2,       3 )
           TKFRAME_-82813_UNITS      = 'DEGREES'
 
           \begintext
 
 
RADAR Beam 5 (RADAR_5)
 
   RADAR Beam 5 is directed nominally 2.2 degrees off of the -Z axis of the
   spacecraft in the direction of the -Y axis of the spacecraft frame. The
   following definition encapsulates this frame:
 
           From [23]:
 
           [     ]    [       ]  [     ]  [     ]
           [ ROT ]  = [ 182.2 ]  [ 0.0 ]  [ 0.0 ]
           [     ]    [       ]  [     ]  [     ]
                               X        Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RADAR_5     = -82814
           FRAME_-82814_NAME         = 'CASSINI_RADAR_5'
           FRAME_-82814_CLASS        = 4
           FRAME_-82814_CLASS_ID     = -82814
           FRAME_-82814_CENTER       = -82
           TKFRAME_-82814_SPEC       = 'ANGLES'
           TKFRAME_-82814_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82814_ANGLES     = ( 182.2,     0.0,     0.0 )
           TKFRAME_-82814_AXES       = (     1,       2,       3 )
           TKFRAME_-82814_UNITS      = 'DEGREES'
 
           \begintext
 
 
RPWS Frames
----------------------------------------------------------
 
   The RPWS antennae are located roughly on the +Y side of the Cassini
   orbiter, while the RPWS Langmuir Probe is roughly located on the -X side.
 
   Note the angles in the frame definitions are specified for the ``from
   instrument to (relative to) base frame'' transformation.
 
 
Radio and Plasma Wave Science (RPWS)
 
   As [17] indicates, the ``collective'' RPWS boresight is nominally directed
   along the spacecraft +Y axis. Utilizing the Euler angles specified in this
   email, we obtain the following frame definition:
 
           From [17]:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ 180.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               X          Y        Z
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RPWS        = -82730
           FRAME_-82730_NAME         = 'CASSINI_RPWS'
           FRAME_-82730_CLASS        = 4
           FRAME_-82730_CLASS_ID     = -82730
           FRAME_-82730_CENTER       = -82
           TKFRAME_-82730_SPEC       = 'ANGLES'
           TKFRAME_-82730_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82730_ANGLES     = ( 180.0,   -90.0,     0.0 )
           TKFRAME_-82730_AXES       = (     3,       1,       3 )
           TKFRAME_-82730_UNITS      = 'DEGREES'
 
           \begintext
 
 
Radio and Plasma Wave Science Electric Antenna System (RPWS_E[AXIS][SIGN])
 
   From [24]:
 
   ``The RPWS electric antenna system is a triad of 10-meter conducting
   monopoles, symmetric about the Y-Z plane. Two of the elements are extended
   in a 120-degree "V" on either side of the magnetometer boom (i.e., the S/C
   Y-axis) and in a plane which is rotated up from the S/C X-Y plane
   containing the magnetometer boom by 37 degrees. These two elements are
   referred to as the EXPLUS and EXMINUS sensors. The third element is
   extended downward in the S/C Y-Z plane at an angleof 37 degrees from the
   S/C +Z axis. That said, it now should be explained that the "electrical"
   characteristics of these three antennas deviate from the physical alignment
   and lengths of the elements due to the complex ground plane provided by the
   spacecraft. It is these electrical characteristics that the three RPWS
   Frame definitions RPWS_EXPLUS, RPWS_EXMINUS, and RPWS_EZPLUS are intended
   to specify. Based upon model rheometry experiments, in which a model of the
   Cassini Spacecraft with fully extended RPWS antennas was immersed in a tank
   filled with an electrolytic, the following estimates have been made for the
   electrical axes of the three antenna elements:
 
           Frame                 Frame Z-Axis in CASSINI_SC_COORD "boresight"
           ------                --------------------------------
           RPWS_EXPLUS           [  0.91202578,  0.27709462, -0.30236989 ]
           RPWS_EXMINUS          [ -0.91202578,  0.27709462, -0.30236989 ]
           RPWS_EZPLUS           [ -0.01091120,  0.52089537,  0.85355080 ]
 
   These numbers may change after the RPWS Jupiter Calibrations.''
 
   Antenna Frame Definitions:
 
           From [14]:
 
           [     ]    [       ]  [        ]  [     ]
           [ ROT ]  = [ -16.9 ]  [ -107.6 ]  [ 0.0 ]
           [     ]    [       ]  [        ]  [     ]
                               Z           Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RPWS_EXPLUS = -82731
           FRAME_-82731_NAME         = 'CASSINI_RPWS_EXPLUS'
           FRAME_-82731_CLASS        = 4
           FRAME_-82731_CLASS_ID     = -82731
           FRAME_-82731_CENTER       = -82
           TKFRAME_-82731_SPEC       = 'ANGLES'
           TKFRAME_-82731_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82731_ANGLES     = ( -16.9,  -107.6,     0.0 )
           TKFRAME_-82731_AXES       = (     3,       2,       1 )
           TKFRAME_-82731_UNITS      = 'DEGREES'
 
           \begintext
 
           From [14]:
 
           [     ]    [        ]  [        ]  [     ]
           [ ROT ]  = [ -163.1 ]  [ -107.6 ]  [ 0.0 ]
           [     ]    [        ]  [        ]  [     ]
                                Z           Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RPWS_EXMINUS= -82732
           FRAME_-82732_NAME         = 'CASSINI_RPWS_EXMINUS'
           FRAME_-82732_CLASS        = 4
           FRAME_-82732_CLASS_ID     = -82732
           FRAME_-82732_CENTER       = -82
           TKFRAME_-82732_SPEC       = 'ANGLES'
           TKFRAME_-82732_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82732_ANGLES     = (-163.1,  -107.6,     0.0 )
           TKFRAME_-82732_AXES       = (     3,       2,       1 )
           TKFRAME_-82732_UNITS      = 'DEGREES'
 
           \begintext
 
           From [14]:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ -91.2 ]  [ -31.4 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               Z          Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RPWS_EZPLUS = -82733
           FRAME_-82733_NAME         = 'CASSINI_RPWS_EZPLUS'
           FRAME_-82733_CLASS        = 4
           FRAME_-82733_CLASS_ID     = -82733
           FRAME_-82733_CENTER       = -82
           TKFRAME_-82733_SPEC       = 'ANGLES'
           TKFRAME_-82733_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82733_ANGLES     = ( -91.2,   -31.4,     0.0 )
           TKFRAME_-82733_AXES       = (     3,       2,       1 )
           TKFRAME_-82733_UNITS      = 'DEGREES'
 
           \begintext
 
 
Radio and Plasma Wave Science Langmuir Probe (RPWS_LP)
 
   From [24]:
 
   ``The RPWS Langmuir Probe is on the -X side of the spacecraft and can sense
   roughly the entire hemisphere defined by the RPWS_LP Frame.''
 
           From [14]:
 
           [     ]    [       ]  [       ]  [     ]
           [ ROT ]  = [ 180.0 ]  [ -90.0 ]  [ 0.0 ]
           [     ]    [       ]  [       ]  [     ]
                               Z          Y        X
 
           where [x]  represents the rotation matrix of a given angle x about
                    i
           axis i.
 
           Nominal Frame Definition:
 
           \begindata
 
           FRAME_CASSINI_RPWS_LP     = -82734
           FRAME_-82734_NAME         = 'CASSINI_RPWS_LP'
           FRAME_-82734_CLASS        = 4
           FRAME_-82734_CLASS_ID     = -82734
           FRAME_-82734_CENTER       = -82
           TKFRAME_-82734_SPEC       = 'ANGLES'
           TKFRAME_-82734_RELATIVE   = 'CASSINI_SC_COORD'
           TKFRAME_-82734_ANGLES     = ( 180.0,   -90.0,     0.0 )
           TKFRAME_-82734_AXES       = (     3,       2,       1 )
           TKFRAME_-82734_UNITS      = 'DEGREES'
 
           \begintext
 
