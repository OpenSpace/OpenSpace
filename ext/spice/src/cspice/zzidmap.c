/* zzidmap.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZIDMAP ( Private --- SPICE body ID/name assignments ) */
/* Subroutine */ int zzidmap_(integer *bltcod, char *bltnam, ftnlen 
	bltnam_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     The default SPICE body/ID mapping assignments available */
/*     to the SPICE library. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     naif_ids.req */

/* $ Keywords */

/*     Body mappings. */

/* $ Declarations */
/* $ Abstract */

/*     This include file lists the parameter collection */
/*     defining the number of SPICE ID -> NAME mappings. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Parameters */

/*     MAXL        is the maximum length of a body name. */

/*     MAXP        is the maximum number of additional names that may */
/*                 be added via the ZZBODDEF interface. */

/*     NPERM       is the count of the mapping assignments built into */
/*                 SPICE. */

/*     MAXE        is the size of the lists and hashes storing combined */
/*                 built-in and ZZBODDEF-defined name/ID mappings. To */
/*                 ensure efficient hashing this size is the set to the */
/*                 first prime number greater than ( MAXP + NPERM ). */

/*     NROOM       is the size of the lists and hashes storing the */
/*                 POOL-defined name/ID mappings. To ensure efficient */
/*                 hashing and to provide the ability to store nearly as */
/*                 many names as can fit in the POOL, this size is */
/*                 set to the first prime number less than MAXLIN */
/*                 defined in the POOL umbrella routine. */

/* $ Required_Reading */

/*     naif_ids.req */

/* $ Keywords */

/*     BODY */
/*     CONVERSION */

/* $ Author_and_Institution */

/*     B.V. Semenov (JPL) */
/*     E.D. Wright  (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 07-MAY-2014 (BVS)(EDW) */

/*        Increased NROOM to 14983. Added a comment note explaining */
/*        NROOM and MAXE */

/* -    SPICELIB Version 1.0.0, 20-MAY-2010 (EDW) */

/*        N0064 version with MAXP = 150, NPERM = 563, */
/*        MAXE = MAXP + NPERM, and NROOM = 2000. */

/*     A script generates this file. Do not edit by hand. */
/*     Edit the creation script to modify the contents of */
/*     ZZBODTRN.INC. */


/*     Maximum size of a NAME string */


/*     Maximum number of additional names that may be added via the */
/*     ZZBODDEF interface. */


/*     Count of default SPICE mapping assignments. */


/*     Size of the lists and hashes storing the built-in and */
/*     ZZBODDEF-defined name/ID mappings. To ensure efficient hashing */
/*     this size is the set to the first prime number greater than */
/*     ( MAXP + NPERM ). */


/*     Size of the lists and hashes storing the POOL-defined name/ID */
/*     mappings. To ensure efficient hashing and to provide the ability */
/*     to store nearly as many names as can fit in the POOL, this size */
/*     is set to the first prime number less than MAXLIN defined in */
/*     the POOL umbrella routine. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BLTCOD     O  List of default integer ID codes */
/*     BLTNAM     O  List of default names */
/*     NPERM      P  Number of name/ID mappings */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     BLTCOD     The array of NPERM elements listing the body ID codes. */

/*     BLTNAM     The array of NPERM elements listing the body names */
/*                corresponding to the ID entry in BLTCOD */

/* $ Parameters */

/*     NPERM      The length of both BLTCOD, BLTNAM */
/*                (read from zzbodtrn.inc). */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Each ith entry of BLTCOD maps to the ith entry of BLTNAM. */

/* $ Examples */

/*     Simple to use, a call the ZZIDMAP returns the arrays defining the */
/*     name/ID mappings. */


/*        INCLUDE            'zzbodtrn.inc' */

/*        INTEGER             ID  ( NPERM ) */
/*        CHARACTER*(MAXL)    NAME( NPERM ) */

/*        CALL ZZIDMAP( ID, NAME ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright, 07-MAY-2014 (JPL) */

/* $ Version */

/* -    SPICELIB 1.0.8 06-MAY-2014 (EDW) */

/*         Edited text comments in Asteroids section and Comets section. */

/*         Eliminated "PI" IAU Number from "CHARON" description. */

/*         HYROKKIN (644) spelling corrected to HYRROKKIN. */

/*     Added: */

/*             -750   SPRINT-AS */
/*             -189   NSYT */
/*             -189   INSIGHT */
/*             -170   JWST */
/*             -170   JAMES WEBB SPACE TELESCOPE */
/*             -144   SOLO */
/*             -144   SOLAR ORBITER */
/*              -96   SPP */
/*              -96   SOLAR PROBE PLUS */
/*              -64   ORX */
/*              -64   OSIRIS-REX */
/*              -54   ARM */
/*              -54   ASTEROID RETRIEVAL MISSION */
/*              -12   LADEE */
/*               -3   MOM */
/*               -3   MARS ORBITER MISSION */
/*                0   SOLAR_SYSTEM_BARYCENTER */
/*                1   MERCURY_BARYCENTER */
/*                2   VENUS_BARYCENTER */
/*                3   EARTH_BARYCENTER */
/*                4   MARS_BARYCENTER */
/*                5   JUPITER_BARYCENTER */
/*                6   SATURN_BARYCENTER */
/*                7   URANUS_BARYCENTER */
/*                8   NEPTUNE_BARYCENTER */
/*                9   PLUTO_BARYCENTER */
/*              644   HYRROKKIN */
/*              904   KERBEROS */
/*              905   STYX */
/*          1003228   C/2013 A1 */
/*          1003228   SIDING SPRING */
/*          2000002   PALLAS */
/*          2000511   DAVIDA */

/*     Removed assignments: */

/*             -486   HERSCHEL */
/*             -489   PLANCK */
/*             -187   SOLAR PROBE */

/* -    SPICELIB 1.0.7 20-MAY-2010 (EDW) */

/*        Edit to vehicle ID list to correct -76 not in proper */
/*        numerical (descending) order. */

/*     Added: */

/*               -5   AKATSUKI */
/*               -5   VCO */
/*             -121   BEPICOLOMBO */
/*             -177   GRAIL-A */
/*             -181   GRAIL-B */
/*             -202   MAVEN */
/*             -205   SOIL MOISTURE ACTIVE AND PASSIVE */
/*             -205   SMAP */
/*             -362   RADIATION BELT STORM PROBE A */
/*             -362   RBSP_A */
/*             -363   RADIATION BELT STORM PROBE B */
/*             -363   RBSP_B */
/*              550   HERSE */
/*              653   AEGAEON */
/*          1000093   TEMPEL_1 */
/*          2000021   LUTETIA */
/*          2004179   TOUTATIS */

/* -    SPICELIB 1.0.6 08-APR-2009 (EDW) */

/*     Added: */

/*               -5   PLC */
/*               -5   PLANET-C */
/*              -68   MMO */
/*              -68   MERCURY MAGNETOSPHERIC ORBITER */
/*              -69   MPO */
/*              -69   MERCURY PLANETARY ORBITER */
/*          2002867   STEINS */
/*             -140   EPOCH */
/*             -140   DIXI */

/* -    SPICELIB 1.0.5 09-JAN-2008 (EDW) */

/*     Added: */

/*              -18   LCROSS */
/*              -29   NEXT */
/*              -86   CH1 */
/*              -86   CHANDRAYAAN-1 */
/*             -131   KAGUYA */
/*             -140   EPOXI */
/*             -151   CHANDRA */
/*             -187   SOLAR PROBE */
/*              636   AEGIR */
/*              637   BEBHIONN */
/*              638   BERGELMIR */
/*              639   BESTLA */
/*              640   FARBAUTI */
/*              641   FENRIR */
/*              642   FORNJOT */
/*              643   HATI */
/*              644   HYROKKIN */
/*              645   KARI */
/*              646   LOGE */
/*              647   SKOLL */
/*              648   SURTUR */
/*              649   ANTHE */
/*              650   JARNSAXA */
/*              651   GREIP */
/*              652   TARQEQ */
/*              809   HALIMEDE */
/*              810   PSAMATHE */
/*              811   SAO */
/*              812   LAOMEDEIA */
/*              813   NESO */

/*     NAIF modified the Jovian system listing to conform to the */
/*     current (as of this date) name/body mapping. */

/*              540   MNEME */
/*              541   AOEDE */
/*              542   THELXINOE */
/*              543   ARCHE */
/*              544   KALLICHORE */
/*              545   HELIKE */
/*              546   CARPO */
/*              547   EUKELADE */
/*              548   CYLLENE */
/*              549   KORE */

/*     Removed assignments: */

/*             -172   SPACETECH-3 COMBINER */
/*             -174   PLUTO-KUIPER EXPRESS */
/*             -175   PLUTO-KUIPER EXPRESS SIMULATION */
/*             -205   SPACETECH-3 COLLECTOR */
/*              514   1979J2 */
/*              515   1979J1 */
/*              516   1979J3 */
/*              610   1980S1 */
/*              611   1980S3 */
/*              612   1980S6 */
/*              613   1980S13 */
/*              614   1980S25 */
/*              615   1980S28 */
/*              616   1980S27 */
/*              617   1980S26 */
/*              706   1986U7 */
/*              707   1986U8 */
/*              708   1986U9 */
/*              709   1986U4 */
/*              710   1986U6 */
/*              711   1986U3 */
/*              712   1986U1 */
/*              713   1986U2 */
/*              714   1986U5 */
/*              715   1985U1 */
/*              718   1986U10 */
/*              901   1978P1 */

/*     Spelling correction: */

/*        MAGACLITE to MEGACLITE */

/*     Rename: */

/*        ERRIAPO to ERRIAPUS */
/*        STV-1 to STV51 */
/*        STV-2 to STV52 */
/*        STV-3 to STV53 */


/* -    SPICELIB 1.0.4 01-NOV-2006 (EDW) */

/*     NAIF removed several provisional name/ID mappings from */
/*     the Jovian system listing: */

/*     539         'HEGEMONE'              JXXXIX */
/*     540         'MNEME'                 JXL */
/*     541         'AOEDE'                 JXLI */
/*     542         'THELXINOE'             JXLII */
/*     543         'ARCHE'                 JXLIII */
/*     544         'KALLICHORE'            JXLIV */
/*     545         'HELIKE'                JXLV */
/*     546         'CARPO'                 JXLVI */
/*     547         'EUKELADE'              JXLVII */
/*     548         'CYLLENE'               JXLVIII */

/*     The current mapping set for the range 539-561: */

/*              540   ARCHE */
/*              541   EUKELADE */
/*              546   HELIKE */
/*              547   AOEDE */
/*              548   HEGEMONE */
/*              551   KALLICHORE */
/*              553   CYLLENE */
/*              560   CARPO */
/*              561   MNEME */

/*     The new mapping leaves the IDs 539, 542-545, 549, 550, 552, */
/*     554-559 unassigned. */

/*     Added: */

/*              635   DAPHNIS */
/*              722   FRANCISCO */
/*              723   MARGARET */
/*              724   FERDINAND */
/*              725   PERDITA */
/*              726   MAB */
/*              727   CUPID */
/*              -61   JUNO */
/*              -76   MSL */
/*              -76   MARS SCIENCE LABORATORY */
/*             -212   STV-1 */
/*             -213   STV-2 */
/*             -214   STV-3 */
/*              902   NIX */
/*              903   HYDRA */
/*             -85    LRO */
/*             -85    LUNAR RECON ORBITER */
/*             -85    LUNAR RECONNAISSANCE ORBITER */

/*     Spelling correction */

/*              632   METHODE to METHONE */

/* -    SPICELIB 1.0.3 14-NOV-2005 (EDW) */

/*     Added: */

/*              539   HEGEMONE */
/*              540   MNEME */
/*              541   AOEDE */
/*              542   THELXINOE */
/*              543   ARCHE */
/*              544   KALLICHORE */
/*              545   HELIKE */
/*              546   CARPO */
/*              547   EUKELADE */
/*              548   CYLLENE */
/*              631   NARVI */
/*              632   METHODE */
/*              633   PALLENE */
/*              634   POLYDEUCES */
/*          2025143   ITOKAWA */
/*              -98   NEW HORIZONS */
/*             -248   VENUS EXPRESS, VEX */
/*             -500   RSAT, SELENE Relay Satellite, SELENE Rstar, Rstar */
/*             -502   VSAT, SELENE VLBI Radio Satellite, */
/*                    SELENE VRAD Satellite, SELENE Vstar */
/*           399064   DSS-64 */

/*      Change in spelling: */

/*              623   SUTTUNG to SUTTUNGR */
/*              627   SKADI   to SKATHI */
/*              630   THRYM   to THRYMR */

/* -    SPICELIB 1.0.2 20-DEC-2004 (EDW) */

/*     Added: */

/*           Due to the previous definition of Parkes with DSS-05, */
/*           the Parkes ID remains 399005. */

/*             -486   HERSCHEL */
/*             -489   PLANCK */
/*           399049   DSS-49 */
/*           399055   DSS-55 */
/*             -203   DAWN */
/*          1000012   67P/CHURYUMOV-GERASIMENKO (1969 R1) */
/*          1000012   CHURYUMOV-GERASIMENKO */
/*          398989    NOTO */
/*             -84    PHOENIX */
/*            -131    SELENE */
/*            -238    SMART-1, S1, SM1, SMART1 */
/*            -130    HAYABUSA */

/* -    SPICELIB 1.0.1 19-DEC-2003 (EDW) */

/*     Added: */
/*              -79   SPITZER */
/*          2000216   KLEOPATRA */

/* -    SPICELIB 1.0.0 27-JUL-2003 (EDW) */

/*     Added: */
/*              -47   GNS */
/*              -74   MRO */
/*              -74   MARS RECON ORBITER */
/*             -130   MUSES-C */
/*             -142   TERRA */
/*             -154   AQUA */
/*             -159   EUROPA ORBITER */
/*             -190   SIM */
/*             -198   INTEGRAL */
/*             -227   KEPLER */
/*             -234   STEREO AHEAD */
/*             -235   STEREO BEHIND */
/*             -253   OPPORTUNITY */
/*             -254   SPIRIT */
/*              528   AUTONOE */
/*              529   THYONE */
/*              530   HERMIPPE */
/*              531   AITNE */
/*              532   EURYDOME */
/*              533   EUANTHE */
/*              534   EUPORIE */
/*              535   ORTHOSIE */
/*              536   SPONDE */
/*              537   KALE */
/*              538   PASITHEE */
/*              619   YMIR */
/*              620   PAALIAQ */
/*              621   TARVOS */
/*              622   IJIRAQ */
/*              623   SUTTUNG */
/*              624   KIVIUQ */
/*              625   MUNDILFARI */
/*              626   ALBIORIX */
/*              627   SKADI */
/*              628   ERRIAPO */
/*              629   SIARNAQ */
/*              630   THRYM */
/*              718   PROSPERO */
/*              719   SETEBOS */
/*              720   STEPHANO */
/*              721   TRINCULO */
/*           398990   NEW NORCIA */
/*          2431011   DACTYL */
/*          2000001   CERES */
/*          2000004   VESTA */

/*     Renamed: */

/*              -25   LPM to */
/*              -25   LP */

/*             -180   MUSES-C to */
/*             -130   MUSES-B */

/*             -172   STARLIGHT COMBINER to */
/*             -172   SPACETECH-3 COMBINER */

/*             -205   STARLIGHT COLLECTOR to */
/*             -205   SPACETECH-3 COLLECTOR */

/*      Removed: */
/*             -172   SLCOMB */


/* -& */
/* $ Index_Entries */

/*     body ID mapping */

/* -& */

/*     A script generates this file. Do not edit by hand. */
/*     Edit the creation script to modify the contents of */
/*     ZZIDMAP. */

    bltcod[0] = 0;
    s_copy(bltnam, "SOLAR_SYSTEM_BARYCENTER", (ftnlen)36, (ftnlen)23);
    bltcod[1] = 0;
    s_copy(bltnam + 36, "SSB", (ftnlen)36, (ftnlen)3);
    bltcod[2] = 0;
    s_copy(bltnam + 72, "SOLAR SYSTEM BARYCENTER", (ftnlen)36, (ftnlen)23);
    bltcod[3] = 1;
    s_copy(bltnam + 108, "MERCURY_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[4] = 1;
    s_copy(bltnam + 144, "MERCURY BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[5] = 2;
    s_copy(bltnam + 180, "VENUS_BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[6] = 2;
    s_copy(bltnam + 216, "VENUS BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[7] = 3;
    s_copy(bltnam + 252, "EARTH_BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[8] = 3;
    s_copy(bltnam + 288, "EMB", (ftnlen)36, (ftnlen)3);
    bltcod[9] = 3;
    s_copy(bltnam + 324, "EARTH MOON BARYCENTER", (ftnlen)36, (ftnlen)21);
    bltcod[10] = 3;
    s_copy(bltnam + 360, "EARTH-MOON BARYCENTER", (ftnlen)36, (ftnlen)21);
    bltcod[11] = 3;
    s_copy(bltnam + 396, "EARTH BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[12] = 4;
    s_copy(bltnam + 432, "MARS_BARYCENTER", (ftnlen)36, (ftnlen)15);
    bltcod[13] = 4;
    s_copy(bltnam + 468, "MARS BARYCENTER", (ftnlen)36, (ftnlen)15);
    bltcod[14] = 5;
    s_copy(bltnam + 504, "JUPITER_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[15] = 5;
    s_copy(bltnam + 540, "JUPITER BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[16] = 6;
    s_copy(bltnam + 576, "SATURN_BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[17] = 6;
    s_copy(bltnam + 612, "SATURN BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[18] = 7;
    s_copy(bltnam + 648, "URANUS_BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[19] = 7;
    s_copy(bltnam + 684, "URANUS BARYCENTER", (ftnlen)36, (ftnlen)17);
    bltcod[20] = 8;
    s_copy(bltnam + 720, "NEPTUNE_BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[21] = 8;
    s_copy(bltnam + 756, "NEPTUNE BARYCENTER", (ftnlen)36, (ftnlen)18);
    bltcod[22] = 9;
    s_copy(bltnam + 792, "PLUTO_BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[23] = 9;
    s_copy(bltnam + 828, "PLUTO BARYCENTER", (ftnlen)36, (ftnlen)16);
    bltcod[24] = 10;
    s_copy(bltnam + 864, "SUN", (ftnlen)36, (ftnlen)3);
    bltcod[25] = 199;
    s_copy(bltnam + 900, "MERCURY", (ftnlen)36, (ftnlen)7);
    bltcod[26] = 299;
    s_copy(bltnam + 936, "VENUS", (ftnlen)36, (ftnlen)5);
    bltcod[27] = 399;
    s_copy(bltnam + 972, "EARTH", (ftnlen)36, (ftnlen)5);
    bltcod[28] = 301;
    s_copy(bltnam + 1008, "MOON", (ftnlen)36, (ftnlen)4);
    bltcod[29] = 499;
    s_copy(bltnam + 1044, "MARS", (ftnlen)36, (ftnlen)4);
    bltcod[30] = 401;
    s_copy(bltnam + 1080, "PHOBOS", (ftnlen)36, (ftnlen)6);
    bltcod[31] = 402;
    s_copy(bltnam + 1116, "DEIMOS", (ftnlen)36, (ftnlen)6);
    bltcod[32] = 599;
    s_copy(bltnam + 1152, "JUPITER", (ftnlen)36, (ftnlen)7);
    bltcod[33] = 501;
    s_copy(bltnam + 1188, "IO", (ftnlen)36, (ftnlen)2);
    bltcod[34] = 502;
    s_copy(bltnam + 1224, "EUROPA", (ftnlen)36, (ftnlen)6);
    bltcod[35] = 503;
    s_copy(bltnam + 1260, "GANYMEDE", (ftnlen)36, (ftnlen)8);
    bltcod[36] = 504;
    s_copy(bltnam + 1296, "CALLISTO", (ftnlen)36, (ftnlen)8);
    bltcod[37] = 505;
    s_copy(bltnam + 1332, "AMALTHEA", (ftnlen)36, (ftnlen)8);
    bltcod[38] = 506;
    s_copy(bltnam + 1368, "HIMALIA", (ftnlen)36, (ftnlen)7);
    bltcod[39] = 507;
    s_copy(bltnam + 1404, "ELARA", (ftnlen)36, (ftnlen)5);
    bltcod[40] = 508;
    s_copy(bltnam + 1440, "PASIPHAE", (ftnlen)36, (ftnlen)8);
    bltcod[41] = 509;
    s_copy(bltnam + 1476, "SINOPE", (ftnlen)36, (ftnlen)6);
    bltcod[42] = 510;
    s_copy(bltnam + 1512, "LYSITHEA", (ftnlen)36, (ftnlen)8);
    bltcod[43] = 511;
    s_copy(bltnam + 1548, "CARME", (ftnlen)36, (ftnlen)5);
    bltcod[44] = 512;
    s_copy(bltnam + 1584, "ANANKE", (ftnlen)36, (ftnlen)6);
    bltcod[45] = 513;
    s_copy(bltnam + 1620, "LEDA", (ftnlen)36, (ftnlen)4);
    bltcod[46] = 514;
    s_copy(bltnam + 1656, "THEBE", (ftnlen)36, (ftnlen)5);
    bltcod[47] = 515;
    s_copy(bltnam + 1692, "ADRASTEA", (ftnlen)36, (ftnlen)8);
    bltcod[48] = 516;
    s_copy(bltnam + 1728, "METIS", (ftnlen)36, (ftnlen)5);
    bltcod[49] = 517;
    s_copy(bltnam + 1764, "CALLIRRHOE", (ftnlen)36, (ftnlen)10);
    bltcod[50] = 518;
    s_copy(bltnam + 1800, "THEMISTO", (ftnlen)36, (ftnlen)8);
    bltcod[51] = 519;
    s_copy(bltnam + 1836, "MAGACLITE", (ftnlen)36, (ftnlen)9);
    bltcod[52] = 520;
    s_copy(bltnam + 1872, "TAYGETE", (ftnlen)36, (ftnlen)7);
    bltcod[53] = 521;
    s_copy(bltnam + 1908, "CHALDENE", (ftnlen)36, (ftnlen)8);
    bltcod[54] = 522;
    s_copy(bltnam + 1944, "HARPALYKE", (ftnlen)36, (ftnlen)9);
    bltcod[55] = 523;
    s_copy(bltnam + 1980, "KALYKE", (ftnlen)36, (ftnlen)6);
    bltcod[56] = 524;
    s_copy(bltnam + 2016, "IOCASTE", (ftnlen)36, (ftnlen)7);
    bltcod[57] = 525;
    s_copy(bltnam + 2052, "ERINOME", (ftnlen)36, (ftnlen)7);
    bltcod[58] = 526;
    s_copy(bltnam + 2088, "ISONOE", (ftnlen)36, (ftnlen)6);
    bltcod[59] = 527;
    s_copy(bltnam + 2124, "PRAXIDIKE", (ftnlen)36, (ftnlen)9);
    bltcod[60] = 528;
    s_copy(bltnam + 2160, "AUTONOE", (ftnlen)36, (ftnlen)7);
    bltcod[61] = 529;
    s_copy(bltnam + 2196, "THYONE", (ftnlen)36, (ftnlen)6);
    bltcod[62] = 530;
    s_copy(bltnam + 2232, "HERMIPPE", (ftnlen)36, (ftnlen)8);
    bltcod[63] = 531;
    s_copy(bltnam + 2268, "AITNE", (ftnlen)36, (ftnlen)5);
    bltcod[64] = 532;
    s_copy(bltnam + 2304, "EURYDOME", (ftnlen)36, (ftnlen)8);
    bltcod[65] = 533;
    s_copy(bltnam + 2340, "EUANTHE", (ftnlen)36, (ftnlen)7);
    bltcod[66] = 534;
    s_copy(bltnam + 2376, "EUPORIE", (ftnlen)36, (ftnlen)7);
    bltcod[67] = 535;
    s_copy(bltnam + 2412, "ORTHOSIE", (ftnlen)36, (ftnlen)8);
    bltcod[68] = 536;
    s_copy(bltnam + 2448, "SPONDE", (ftnlen)36, (ftnlen)6);
    bltcod[69] = 537;
    s_copy(bltnam + 2484, "KALE", (ftnlen)36, (ftnlen)4);
    bltcod[70] = 538;
    s_copy(bltnam + 2520, "PASITHEE", (ftnlen)36, (ftnlen)8);
    bltcod[71] = 539;
    s_copy(bltnam + 2556, "HEGEMONE", (ftnlen)36, (ftnlen)8);
    bltcod[72] = 540;
    s_copy(bltnam + 2592, "MNEME", (ftnlen)36, (ftnlen)5);
    bltcod[73] = 541;
    s_copy(bltnam + 2628, "AOEDE", (ftnlen)36, (ftnlen)5);
    bltcod[74] = 542;
    s_copy(bltnam + 2664, "THELXINOE", (ftnlen)36, (ftnlen)9);
    bltcod[75] = 543;
    s_copy(bltnam + 2700, "ARCHE", (ftnlen)36, (ftnlen)5);
    bltcod[76] = 544;
    s_copy(bltnam + 2736, "KALLICHORE", (ftnlen)36, (ftnlen)10);
    bltcod[77] = 545;
    s_copy(bltnam + 2772, "HELIKE", (ftnlen)36, (ftnlen)6);
    bltcod[78] = 546;
    s_copy(bltnam + 2808, "CARPO", (ftnlen)36, (ftnlen)5);
    bltcod[79] = 547;
    s_copy(bltnam + 2844, "EUKELADE", (ftnlen)36, (ftnlen)8);
    bltcod[80] = 548;
    s_copy(bltnam + 2880, "CYLLENE", (ftnlen)36, (ftnlen)7);
    bltcod[81] = 549;
    s_copy(bltnam + 2916, "KORE", (ftnlen)36, (ftnlen)4);
    bltcod[82] = 550;
    s_copy(bltnam + 2952, "HERSE", (ftnlen)36, (ftnlen)5);
    bltcod[83] = 699;
    s_copy(bltnam + 2988, "SATURN", (ftnlen)36, (ftnlen)6);
    bltcod[84] = 601;
    s_copy(bltnam + 3024, "MIMAS", (ftnlen)36, (ftnlen)5);
    bltcod[85] = 602;
    s_copy(bltnam + 3060, "ENCELADUS", (ftnlen)36, (ftnlen)9);
    bltcod[86] = 603;
    s_copy(bltnam + 3096, "TETHYS", (ftnlen)36, (ftnlen)6);
    bltcod[87] = 604;
    s_copy(bltnam + 3132, "DIONE", (ftnlen)36, (ftnlen)5);
    bltcod[88] = 605;
    s_copy(bltnam + 3168, "RHEA", (ftnlen)36, (ftnlen)4);
    bltcod[89] = 606;
    s_copy(bltnam + 3204, "TITAN", (ftnlen)36, (ftnlen)5);
    bltcod[90] = 607;
    s_copy(bltnam + 3240, "HYPERION", (ftnlen)36, (ftnlen)8);
    bltcod[91] = 608;
    s_copy(bltnam + 3276, "IAPETUS", (ftnlen)36, (ftnlen)7);
    bltcod[92] = 609;
    s_copy(bltnam + 3312, "PHOEBE", (ftnlen)36, (ftnlen)6);
    bltcod[93] = 610;
    s_copy(bltnam + 3348, "JANUS", (ftnlen)36, (ftnlen)5);
    bltcod[94] = 611;
    s_copy(bltnam + 3384, "EPIMETHEUS", (ftnlen)36, (ftnlen)10);
    bltcod[95] = 612;
    s_copy(bltnam + 3420, "HELENE", (ftnlen)36, (ftnlen)6);
    bltcod[96] = 613;
    s_copy(bltnam + 3456, "TELESTO", (ftnlen)36, (ftnlen)7);
    bltcod[97] = 614;
    s_copy(bltnam + 3492, "CALYPSO", (ftnlen)36, (ftnlen)7);
    bltcod[98] = 615;
    s_copy(bltnam + 3528, "ATLAS", (ftnlen)36, (ftnlen)5);
    bltcod[99] = 616;
    s_copy(bltnam + 3564, "PROMETHEUS", (ftnlen)36, (ftnlen)10);
    bltcod[100] = 617;
    s_copy(bltnam + 3600, "PANDORA", (ftnlen)36, (ftnlen)7);
    bltcod[101] = 618;
    s_copy(bltnam + 3636, "PAN", (ftnlen)36, (ftnlen)3);
    bltcod[102] = 619;
    s_copy(bltnam + 3672, "YMIR", (ftnlen)36, (ftnlen)4);
    bltcod[103] = 620;
    s_copy(bltnam + 3708, "PAALIAQ", (ftnlen)36, (ftnlen)7);
    bltcod[104] = 621;
    s_copy(bltnam + 3744, "TARVOS", (ftnlen)36, (ftnlen)6);
    bltcod[105] = 622;
    s_copy(bltnam + 3780, "IJIRAQ", (ftnlen)36, (ftnlen)6);
    bltcod[106] = 623;
    s_copy(bltnam + 3816, "SUTTUNGR", (ftnlen)36, (ftnlen)8);
    bltcod[107] = 624;
    s_copy(bltnam + 3852, "KIVIUQ", (ftnlen)36, (ftnlen)6);
    bltcod[108] = 625;
    s_copy(bltnam + 3888, "MUNDILFARI", (ftnlen)36, (ftnlen)10);
    bltcod[109] = 626;
    s_copy(bltnam + 3924, "ALBIORIX", (ftnlen)36, (ftnlen)8);
    bltcod[110] = 627;
    s_copy(bltnam + 3960, "SKATHI", (ftnlen)36, (ftnlen)6);
    bltcod[111] = 628;
    s_copy(bltnam + 3996, "ERRIAPUS", (ftnlen)36, (ftnlen)8);
    bltcod[112] = 629;
    s_copy(bltnam + 4032, "SIARNAQ", (ftnlen)36, (ftnlen)7);
    bltcod[113] = 630;
    s_copy(bltnam + 4068, "THRYMR", (ftnlen)36, (ftnlen)6);
    bltcod[114] = 631;
    s_copy(bltnam + 4104, "NARVI", (ftnlen)36, (ftnlen)5);
    bltcod[115] = 632;
    s_copy(bltnam + 4140, "METHONE", (ftnlen)36, (ftnlen)7);
    bltcod[116] = 633;
    s_copy(bltnam + 4176, "PALLENE", (ftnlen)36, (ftnlen)7);
    bltcod[117] = 634;
    s_copy(bltnam + 4212, "POLYDEUCES", (ftnlen)36, (ftnlen)10);
    bltcod[118] = 635;
    s_copy(bltnam + 4248, "DAPHNIS", (ftnlen)36, (ftnlen)7);
    bltcod[119] = 636;
    s_copy(bltnam + 4284, "AEGIR", (ftnlen)36, (ftnlen)5);
    bltcod[120] = 637;
    s_copy(bltnam + 4320, "BEBHIONN", (ftnlen)36, (ftnlen)8);
    bltcod[121] = 638;
    s_copy(bltnam + 4356, "BERGELMIR", (ftnlen)36, (ftnlen)9);
    bltcod[122] = 639;
    s_copy(bltnam + 4392, "BESTLA", (ftnlen)36, (ftnlen)6);
    bltcod[123] = 640;
    s_copy(bltnam + 4428, "FARBAUTI", (ftnlen)36, (ftnlen)8);
    bltcod[124] = 641;
    s_copy(bltnam + 4464, "FENRIR", (ftnlen)36, (ftnlen)6);
    bltcod[125] = 642;
    s_copy(bltnam + 4500, "FORNJOT", (ftnlen)36, (ftnlen)7);
    bltcod[126] = 643;
    s_copy(bltnam + 4536, "HATI", (ftnlen)36, (ftnlen)4);
    bltcod[127] = 644;
    s_copy(bltnam + 4572, "HYRROKKIN", (ftnlen)36, (ftnlen)9);
    bltcod[128] = 645;
    s_copy(bltnam + 4608, "KARI", (ftnlen)36, (ftnlen)4);
    bltcod[129] = 646;
    s_copy(bltnam + 4644, "LOGE", (ftnlen)36, (ftnlen)4);
    bltcod[130] = 647;
    s_copy(bltnam + 4680, "SKOLL", (ftnlen)36, (ftnlen)5);
    bltcod[131] = 648;
    s_copy(bltnam + 4716, "SURTUR", (ftnlen)36, (ftnlen)6);
    bltcod[132] = 649;
    s_copy(bltnam + 4752, "ANTHE", (ftnlen)36, (ftnlen)5);
    bltcod[133] = 650;
    s_copy(bltnam + 4788, "JARNSAXA", (ftnlen)36, (ftnlen)8);
    bltcod[134] = 651;
    s_copy(bltnam + 4824, "GREIP", (ftnlen)36, (ftnlen)5);
    bltcod[135] = 652;
    s_copy(bltnam + 4860, "TARQEQ", (ftnlen)36, (ftnlen)6);
    bltcod[136] = 653;
    s_copy(bltnam + 4896, "AEGAEON", (ftnlen)36, (ftnlen)7);
    bltcod[137] = 799;
    s_copy(bltnam + 4932, "URANUS", (ftnlen)36, (ftnlen)6);
    bltcod[138] = 701;
    s_copy(bltnam + 4968, "ARIEL", (ftnlen)36, (ftnlen)5);
    bltcod[139] = 702;
    s_copy(bltnam + 5004, "UMBRIEL", (ftnlen)36, (ftnlen)7);
    bltcod[140] = 703;
    s_copy(bltnam + 5040, "TITANIA", (ftnlen)36, (ftnlen)7);
    bltcod[141] = 704;
    s_copy(bltnam + 5076, "OBERON", (ftnlen)36, (ftnlen)6);
    bltcod[142] = 705;
    s_copy(bltnam + 5112, "MIRANDA", (ftnlen)36, (ftnlen)7);
    bltcod[143] = 706;
    s_copy(bltnam + 5148, "CORDELIA", (ftnlen)36, (ftnlen)8);
    bltcod[144] = 707;
    s_copy(bltnam + 5184, "OPHELIA", (ftnlen)36, (ftnlen)7);
    bltcod[145] = 708;
    s_copy(bltnam + 5220, "BIANCA", (ftnlen)36, (ftnlen)6);
    bltcod[146] = 709;
    s_copy(bltnam + 5256, "CRESSIDA", (ftnlen)36, (ftnlen)8);
    bltcod[147] = 710;
    s_copy(bltnam + 5292, "DESDEMONA", (ftnlen)36, (ftnlen)9);
    bltcod[148] = 711;
    s_copy(bltnam + 5328, "JULIET", (ftnlen)36, (ftnlen)6);
    bltcod[149] = 712;
    s_copy(bltnam + 5364, "PORTIA", (ftnlen)36, (ftnlen)6);
    bltcod[150] = 713;
    s_copy(bltnam + 5400, "ROSALIND", (ftnlen)36, (ftnlen)8);
    bltcod[151] = 714;
    s_copy(bltnam + 5436, "BELINDA", (ftnlen)36, (ftnlen)7);
    bltcod[152] = 715;
    s_copy(bltnam + 5472, "PUCK", (ftnlen)36, (ftnlen)4);
    bltcod[153] = 716;
    s_copy(bltnam + 5508, "CALIBAN", (ftnlen)36, (ftnlen)7);
    bltcod[154] = 717;
    s_copy(bltnam + 5544, "SYCORAX", (ftnlen)36, (ftnlen)7);
    bltcod[155] = 718;
    s_copy(bltnam + 5580, "PROSPERO", (ftnlen)36, (ftnlen)8);
    bltcod[156] = 719;
    s_copy(bltnam + 5616, "SETEBOS", (ftnlen)36, (ftnlen)7);
    bltcod[157] = 720;
    s_copy(bltnam + 5652, "STEPHANO", (ftnlen)36, (ftnlen)8);
    bltcod[158] = 721;
    s_copy(bltnam + 5688, "TRINCULO", (ftnlen)36, (ftnlen)8);
    bltcod[159] = 722;
    s_copy(bltnam + 5724, "FRANCISCO", (ftnlen)36, (ftnlen)9);
    bltcod[160] = 723;
    s_copy(bltnam + 5760, "MARGARET", (ftnlen)36, (ftnlen)8);
    bltcod[161] = 724;
    s_copy(bltnam + 5796, "FERDINAND", (ftnlen)36, (ftnlen)9);
    bltcod[162] = 725;
    s_copy(bltnam + 5832, "PERDITA", (ftnlen)36, (ftnlen)7);
    bltcod[163] = 726;
    s_copy(bltnam + 5868, "MAB", (ftnlen)36, (ftnlen)3);
    bltcod[164] = 727;
    s_copy(bltnam + 5904, "CUPID", (ftnlen)36, (ftnlen)5);
    bltcod[165] = 899;
    s_copy(bltnam + 5940, "NEPTUNE", (ftnlen)36, (ftnlen)7);
    bltcod[166] = 801;
    s_copy(bltnam + 5976, "TRITON", (ftnlen)36, (ftnlen)6);
    bltcod[167] = 802;
    s_copy(bltnam + 6012, "NEREID", (ftnlen)36, (ftnlen)6);
    bltcod[168] = 803;
    s_copy(bltnam + 6048, "NAIAD", (ftnlen)36, (ftnlen)5);
    bltcod[169] = 804;
    s_copy(bltnam + 6084, "THALASSA", (ftnlen)36, (ftnlen)8);
    bltcod[170] = 805;
    s_copy(bltnam + 6120, "DESPINA", (ftnlen)36, (ftnlen)7);
    bltcod[171] = 806;
    s_copy(bltnam + 6156, "GALATEA", (ftnlen)36, (ftnlen)7);
    bltcod[172] = 807;
    s_copy(bltnam + 6192, "LARISSA", (ftnlen)36, (ftnlen)7);
    bltcod[173] = 808;
    s_copy(bltnam + 6228, "PROTEUS", (ftnlen)36, (ftnlen)7);
    bltcod[174] = 809;
    s_copy(bltnam + 6264, "HALIMEDE", (ftnlen)36, (ftnlen)8);
    bltcod[175] = 810;
    s_copy(bltnam + 6300, "PSAMATHE", (ftnlen)36, (ftnlen)8);
    bltcod[176] = 811;
    s_copy(bltnam + 6336, "SAO", (ftnlen)36, (ftnlen)3);
    bltcod[177] = 812;
    s_copy(bltnam + 6372, "LAOMEDEIA", (ftnlen)36, (ftnlen)9);
    bltcod[178] = 813;
    s_copy(bltnam + 6408, "NESO", (ftnlen)36, (ftnlen)4);
    bltcod[179] = 999;
    s_copy(bltnam + 6444, "PLUTO", (ftnlen)36, (ftnlen)5);
    bltcod[180] = 901;
    s_copy(bltnam + 6480, "CHARON", (ftnlen)36, (ftnlen)6);
    bltcod[181] = 902;
    s_copy(bltnam + 6516, "NIX", (ftnlen)36, (ftnlen)3);
    bltcod[182] = 903;
    s_copy(bltnam + 6552, "HYDRA", (ftnlen)36, (ftnlen)5);
    bltcod[183] = 904;
    s_copy(bltnam + 6588, "KERBEROS", (ftnlen)36, (ftnlen)8);
    bltcod[184] = 905;
    s_copy(bltnam + 6624, "STYX", (ftnlen)36, (ftnlen)4);
    bltcod[185] = -1;
    s_copy(bltnam + 6660, "GEOTAIL", (ftnlen)36, (ftnlen)7);
    bltcod[186] = -3;
    s_copy(bltnam + 6696, "MOM", (ftnlen)36, (ftnlen)3);
    bltcod[187] = -3;
    s_copy(bltnam + 6732, "MARS ORBITER MISSION", (ftnlen)36, (ftnlen)20);
    bltcod[188] = -5;
    s_copy(bltnam + 6768, "AKATSUKI", (ftnlen)36, (ftnlen)8);
    bltcod[189] = -5;
    s_copy(bltnam + 6804, "VCO", (ftnlen)36, (ftnlen)3);
    bltcod[190] = -5;
    s_copy(bltnam + 6840, "PLC", (ftnlen)36, (ftnlen)3);
    bltcod[191] = -5;
    s_copy(bltnam + 6876, "PLANET-C", (ftnlen)36, (ftnlen)8);
    bltcod[192] = -6;
    s_copy(bltnam + 6912, "P6", (ftnlen)36, (ftnlen)2);
    bltcod[193] = -6;
    s_copy(bltnam + 6948, "PIONEER-6", (ftnlen)36, (ftnlen)9);
    bltcod[194] = -7;
    s_copy(bltnam + 6984, "P7", (ftnlen)36, (ftnlen)2);
    bltcod[195] = -7;
    s_copy(bltnam + 7020, "PIONEER-7", (ftnlen)36, (ftnlen)9);
    bltcod[196] = -8;
    s_copy(bltnam + 7056, "WIND", (ftnlen)36, (ftnlen)4);
    bltcod[197] = -12;
    s_copy(bltnam + 7092, "VENUS ORBITER", (ftnlen)36, (ftnlen)13);
    bltcod[198] = -12;
    s_copy(bltnam + 7128, "P12", (ftnlen)36, (ftnlen)3);
    bltcod[199] = -12;
    s_copy(bltnam + 7164, "PIONEER 12", (ftnlen)36, (ftnlen)10);
    bltcod[200] = -12;
    s_copy(bltnam + 7200, "LADEE", (ftnlen)36, (ftnlen)5);
    bltcod[201] = -13;
    s_copy(bltnam + 7236, "POLAR", (ftnlen)36, (ftnlen)5);
    bltcod[202] = -18;
    s_copy(bltnam + 7272, "MGN", (ftnlen)36, (ftnlen)3);
    bltcod[203] = -18;
    s_copy(bltnam + 7308, "MAGELLAN", (ftnlen)36, (ftnlen)8);
    bltcod[204] = -18;
    s_copy(bltnam + 7344, "LCROSS", (ftnlen)36, (ftnlen)6);
    bltcod[205] = -20;
    s_copy(bltnam + 7380, "P8", (ftnlen)36, (ftnlen)2);
    bltcod[206] = -20;
    s_copy(bltnam + 7416, "PIONEER-8", (ftnlen)36, (ftnlen)9);
    bltcod[207] = -21;
    s_copy(bltnam + 7452, "SOHO", (ftnlen)36, (ftnlen)4);
    bltcod[208] = -23;
    s_copy(bltnam + 7488, "P10", (ftnlen)36, (ftnlen)3);
    bltcod[209] = -23;
    s_copy(bltnam + 7524, "PIONEER-10", (ftnlen)36, (ftnlen)10);
    bltcod[210] = -24;
    s_copy(bltnam + 7560, "P11", (ftnlen)36, (ftnlen)3);
    bltcod[211] = -24;
    s_copy(bltnam + 7596, "PIONEER-11", (ftnlen)36, (ftnlen)10);
    bltcod[212] = -25;
    s_copy(bltnam + 7632, "LP", (ftnlen)36, (ftnlen)2);
    bltcod[213] = -25;
    s_copy(bltnam + 7668, "LUNAR PROSPECTOR", (ftnlen)36, (ftnlen)16);
    bltcod[214] = -27;
    s_copy(bltnam + 7704, "VK1", (ftnlen)36, (ftnlen)3);
    bltcod[215] = -27;
    s_copy(bltnam + 7740, "VIKING 1 ORBITER", (ftnlen)36, (ftnlen)16);
    bltcod[216] = -29;
    s_copy(bltnam + 7776, "STARDUST", (ftnlen)36, (ftnlen)8);
    bltcod[217] = -29;
    s_copy(bltnam + 7812, "SDU", (ftnlen)36, (ftnlen)3);
    bltcod[218] = -29;
    s_copy(bltnam + 7848, "NEXT", (ftnlen)36, (ftnlen)4);
    bltcod[219] = -30;
    s_copy(bltnam + 7884, "VK2", (ftnlen)36, (ftnlen)3);
    bltcod[220] = -30;
    s_copy(bltnam + 7920, "VIKING 2 ORBITER", (ftnlen)36, (ftnlen)16);
    bltcod[221] = -30;
    s_copy(bltnam + 7956, "DS-1", (ftnlen)36, (ftnlen)4);
    bltcod[222] = -31;
    s_copy(bltnam + 7992, "VG1", (ftnlen)36, (ftnlen)3);
    bltcod[223] = -31;
    s_copy(bltnam + 8028, "VOYAGER 1", (ftnlen)36, (ftnlen)9);
    bltcod[224] = -32;
    s_copy(bltnam + 8064, "VG2", (ftnlen)36, (ftnlen)3);
    bltcod[225] = -32;
    s_copy(bltnam + 8100, "VOYAGER 2", (ftnlen)36, (ftnlen)9);
    bltcod[226] = -40;
    s_copy(bltnam + 8136, "CLEMENTINE", (ftnlen)36, (ftnlen)10);
    bltcod[227] = -41;
    s_copy(bltnam + 8172, "MEX", (ftnlen)36, (ftnlen)3);
    bltcod[228] = -41;
    s_copy(bltnam + 8208, "MARS EXPRESS", (ftnlen)36, (ftnlen)12);
    bltcod[229] = -44;
    s_copy(bltnam + 8244, "BEAGLE2", (ftnlen)36, (ftnlen)7);
    bltcod[230] = -44;
    s_copy(bltnam + 8280, "BEAGLE 2", (ftnlen)36, (ftnlen)8);
    bltcod[231] = -46;
    s_copy(bltnam + 8316, "MS-T5", (ftnlen)36, (ftnlen)5);
    bltcod[232] = -46;
    s_copy(bltnam + 8352, "SAKIGAKE", (ftnlen)36, (ftnlen)8);
    bltcod[233] = -47;
    s_copy(bltnam + 8388, "PLANET-A", (ftnlen)36, (ftnlen)8);
    bltcod[234] = -47;
    s_copy(bltnam + 8424, "SUISEI", (ftnlen)36, (ftnlen)6);
    bltcod[235] = -47;
    s_copy(bltnam + 8460, "GNS", (ftnlen)36, (ftnlen)3);
    bltcod[236] = -47;
    s_copy(bltnam + 8496, "GENESIS", (ftnlen)36, (ftnlen)7);
    bltcod[237] = -48;
    s_copy(bltnam + 8532, "HUBBLE SPACE TELESCOPE", (ftnlen)36, (ftnlen)22);
    bltcod[238] = -48;
    s_copy(bltnam + 8568, "HST", (ftnlen)36, (ftnlen)3);
    bltcod[239] = -53;
    s_copy(bltnam + 8604, "MARS PATHFINDER", (ftnlen)36, (ftnlen)15);
    bltcod[240] = -53;
    s_copy(bltnam + 8640, "MPF", (ftnlen)36, (ftnlen)3);
    bltcod[241] = -53;
    s_copy(bltnam + 8676, "MARS ODYSSEY", (ftnlen)36, (ftnlen)12);
    bltcod[242] = -53;
    s_copy(bltnam + 8712, "MARS SURVEYOR 01 ORBITER", (ftnlen)36, (ftnlen)24);
    bltcod[243] = -54;
    s_copy(bltnam + 8748, "ARM", (ftnlen)36, (ftnlen)3);
    bltcod[244] = -54;
    s_copy(bltnam + 8784, "ASTEROID RETRIEVAL MISSION", (ftnlen)36, (ftnlen)
	    26);
    bltcod[245] = -55;
    s_copy(bltnam + 8820, "ULYSSES", (ftnlen)36, (ftnlen)7);
    bltcod[246] = -58;
    s_copy(bltnam + 8856, "VSOP", (ftnlen)36, (ftnlen)4);
    bltcod[247] = -58;
    s_copy(bltnam + 8892, "HALCA", (ftnlen)36, (ftnlen)5);
    bltcod[248] = -59;
    s_copy(bltnam + 8928, "RADIOASTRON", (ftnlen)36, (ftnlen)11);
    bltcod[249] = -61;
    s_copy(bltnam + 8964, "JUNO", (ftnlen)36, (ftnlen)4);
    bltcod[250] = -64;
    s_copy(bltnam + 9000, "ORX", (ftnlen)36, (ftnlen)3);
    bltcod[251] = -64;
    s_copy(bltnam + 9036, "OSIRIS-REX", (ftnlen)36, (ftnlen)10);
    bltcod[252] = -66;
    s_copy(bltnam + 9072, "VEGA 1", (ftnlen)36, (ftnlen)6);
    bltcod[253] = -67;
    s_copy(bltnam + 9108, "VEGA 2", (ftnlen)36, (ftnlen)6);
    bltcod[254] = -68;
    s_copy(bltnam + 9144, "MMO", (ftnlen)36, (ftnlen)3);
    bltcod[255] = -68;
    s_copy(bltnam + 9180, "MERCURY MAGNETOSPHERIC ORBITER", (ftnlen)36, (
	    ftnlen)30);
    bltcod[256] = -69;
    s_copy(bltnam + 9216, "MPO", (ftnlen)36, (ftnlen)3);
    bltcod[257] = -69;
    s_copy(bltnam + 9252, "MERCURY PLANETARY ORBITER", (ftnlen)36, (ftnlen)25)
	    ;
    bltcod[258] = -70;
    s_copy(bltnam + 9288, "DEEP IMPACT IMPACTOR SPACECRAFT", (ftnlen)36, (
	    ftnlen)31);
    bltcod[259] = -74;
    s_copy(bltnam + 9324, "MRO", (ftnlen)36, (ftnlen)3);
    bltcod[260] = -74;
    s_copy(bltnam + 9360, "MARS RECON ORBITER", (ftnlen)36, (ftnlen)18);
    bltcod[261] = -76;
    s_copy(bltnam + 9396, "MSL", (ftnlen)36, (ftnlen)3);
    bltcod[262] = -76;
    s_copy(bltnam + 9432, "MARS SCIENCE LABORATORY", (ftnlen)36, (ftnlen)23);
    bltcod[263] = -77;
    s_copy(bltnam + 9468, "GLL", (ftnlen)36, (ftnlen)3);
    bltcod[264] = -77;
    s_copy(bltnam + 9504, "GALILEO ORBITER", (ftnlen)36, (ftnlen)15);
    bltcod[265] = -78;
    s_copy(bltnam + 9540, "GIOTTO", (ftnlen)36, (ftnlen)6);
    bltcod[266] = -79;
    s_copy(bltnam + 9576, "SPITZER", (ftnlen)36, (ftnlen)7);
    bltcod[267] = -79;
    s_copy(bltnam + 9612, "SPACE INFRARED TELESCOPE FACILITY", (ftnlen)36, (
	    ftnlen)33);
    bltcod[268] = -79;
    s_copy(bltnam + 9648, "SIRTF", (ftnlen)36, (ftnlen)5);
    bltcod[269] = -81;
    s_copy(bltnam + 9684, "CASSINI ITL", (ftnlen)36, (ftnlen)11);
    bltcod[270] = -82;
    s_copy(bltnam + 9720, "CAS", (ftnlen)36, (ftnlen)3);
    bltcod[271] = -82;
    s_copy(bltnam + 9756, "CASSINI", (ftnlen)36, (ftnlen)7);
    bltcod[272] = -84;
    s_copy(bltnam + 9792, "PHOENIX", (ftnlen)36, (ftnlen)7);
    bltcod[273] = -85;
    s_copy(bltnam + 9828, "LRO", (ftnlen)36, (ftnlen)3);
    bltcod[274] = -85;
    s_copy(bltnam + 9864, "LUNAR RECON ORBITER", (ftnlen)36, (ftnlen)19);
    bltcod[275] = -85;
    s_copy(bltnam + 9900, "LUNAR RECONNAISSANCE ORBITER", (ftnlen)36, (ftnlen)
	    28);
    bltcod[276] = -86;
    s_copy(bltnam + 9936, "CH1", (ftnlen)36, (ftnlen)3);
    bltcod[277] = -86;
    s_copy(bltnam + 9972, "CHANDRAYAAN-1", (ftnlen)36, (ftnlen)13);
    bltcod[278] = -90;
    s_copy(bltnam + 10008, "CASSINI SIMULATION", (ftnlen)36, (ftnlen)18);
    bltcod[279] = -93;
    s_copy(bltnam + 10044, "NEAR EARTH ASTEROID RENDEZVOUS", (ftnlen)36, (
	    ftnlen)30);
    bltcod[280] = -93;
    s_copy(bltnam + 10080, "NEAR", (ftnlen)36, (ftnlen)4);
    bltcod[281] = -94;
    s_copy(bltnam + 10116, "MO", (ftnlen)36, (ftnlen)2);
    bltcod[282] = -94;
    s_copy(bltnam + 10152, "MARS OBSERVER", (ftnlen)36, (ftnlen)13);
    bltcod[283] = -94;
    s_copy(bltnam + 10188, "MGS", (ftnlen)36, (ftnlen)3);
    bltcod[284] = -94;
    s_copy(bltnam + 10224, "MARS GLOBAL SURVEYOR", (ftnlen)36, (ftnlen)20);
    bltcod[285] = -95;
    s_copy(bltnam + 10260, "MGS SIMULATION", (ftnlen)36, (ftnlen)14);
    bltcod[286] = -96;
    s_copy(bltnam + 10296, "SPP", (ftnlen)36, (ftnlen)3);
    bltcod[287] = -96;
    s_copy(bltnam + 10332, "SOLAR PROBE PLUS", (ftnlen)36, (ftnlen)16);
    bltcod[288] = -97;
    s_copy(bltnam + 10368, "TOPEX/POSEIDON", (ftnlen)36, (ftnlen)14);
    bltcod[289] = -98;
    s_copy(bltnam + 10404, "NEW HORIZONS", (ftnlen)36, (ftnlen)12);
    bltcod[290] = -107;
    s_copy(bltnam + 10440, "TROPICAL RAINFALL MEASURING MISSION", (ftnlen)36, 
	    (ftnlen)35);
    bltcod[291] = -107;
    s_copy(bltnam + 10476, "TRMM", (ftnlen)36, (ftnlen)4);
    bltcod[292] = -112;
    s_copy(bltnam + 10512, "ICE", (ftnlen)36, (ftnlen)3);
    bltcod[293] = -116;
    s_copy(bltnam + 10548, "MARS POLAR LANDER", (ftnlen)36, (ftnlen)17);
    bltcod[294] = -116;
    s_copy(bltnam + 10584, "MPL", (ftnlen)36, (ftnlen)3);
    bltcod[295] = -121;
    s_copy(bltnam + 10620, "BEPICOLOMBO", (ftnlen)36, (ftnlen)11);
    bltcod[296] = -127;
    s_copy(bltnam + 10656, "MARS CLIMATE ORBITER", (ftnlen)36, (ftnlen)20);
    bltcod[297] = -127;
    s_copy(bltnam + 10692, "MCO", (ftnlen)36, (ftnlen)3);
    bltcod[298] = -130;
    s_copy(bltnam + 10728, "MUSES-C", (ftnlen)36, (ftnlen)7);
    bltcod[299] = -130;
    s_copy(bltnam + 10764, "HAYABUSA", (ftnlen)36, (ftnlen)8);
    bltcod[300] = -131;
    s_copy(bltnam + 10800, "SELENE", (ftnlen)36, (ftnlen)6);
    bltcod[301] = -131;
    s_copy(bltnam + 10836, "KAGUYA", (ftnlen)36, (ftnlen)6);
    bltcod[302] = -135;
    s_copy(bltnam + 10872, "DRTS-W", (ftnlen)36, (ftnlen)6);
    bltcod[303] = -140;
    s_copy(bltnam + 10908, "EPOCH", (ftnlen)36, (ftnlen)5);
    bltcod[304] = -140;
    s_copy(bltnam + 10944, "DIXI", (ftnlen)36, (ftnlen)4);
    bltcod[305] = -140;
    s_copy(bltnam + 10980, "EPOXI", (ftnlen)36, (ftnlen)5);
    bltcod[306] = -140;
    s_copy(bltnam + 11016, "DEEP IMPACT FLYBY SPACECRAFT", (ftnlen)36, (
	    ftnlen)28);
    bltcod[307] = -142;
    s_copy(bltnam + 11052, "TERRA", (ftnlen)36, (ftnlen)5);
    bltcod[308] = -142;
    s_copy(bltnam + 11088, "EOS-AM1", (ftnlen)36, (ftnlen)7);
    bltcod[309] = -144;
    s_copy(bltnam + 11124, "SOLO", (ftnlen)36, (ftnlen)4);
    bltcod[310] = -144;
    s_copy(bltnam + 11160, "SOLAR ORBITER", (ftnlen)36, (ftnlen)13);
    bltcod[311] = -146;
    s_copy(bltnam + 11196, "LUNAR-A", (ftnlen)36, (ftnlen)7);
    bltcod[312] = -150;
    s_copy(bltnam + 11232, "CASSINI PROBE", (ftnlen)36, (ftnlen)13);
    bltcod[313] = -150;
    s_copy(bltnam + 11268, "HUYGENS PROBE", (ftnlen)36, (ftnlen)13);
    bltcod[314] = -150;
    s_copy(bltnam + 11304, "CASP", (ftnlen)36, (ftnlen)4);
    bltcod[315] = -151;
    s_copy(bltnam + 11340, "AXAF", (ftnlen)36, (ftnlen)4);
    bltcod[316] = -151;
    s_copy(bltnam + 11376, "CHANDRA", (ftnlen)36, (ftnlen)7);
    bltcod[317] = -154;
    s_copy(bltnam + 11412, "AQUA", (ftnlen)36, (ftnlen)4);
    bltcod[318] = -159;
    s_copy(bltnam + 11448, "EUROPA ORBITER", (ftnlen)36, (ftnlen)14);
    bltcod[319] = -164;
    s_copy(bltnam + 11484, "YOHKOH", (ftnlen)36, (ftnlen)6);
    bltcod[320] = -164;
    s_copy(bltnam + 11520, "SOLAR-A", (ftnlen)36, (ftnlen)7);
    bltcod[321] = -165;
    s_copy(bltnam + 11556, "MAP", (ftnlen)36, (ftnlen)3);
    bltcod[322] = -166;
    s_copy(bltnam + 11592, "IMAGE", (ftnlen)36, (ftnlen)5);
    bltcod[323] = -170;
    s_copy(bltnam + 11628, "JWST", (ftnlen)36, (ftnlen)4);
    bltcod[324] = -170;
    s_copy(bltnam + 11664, "JAMES WEBB SPACE TELESCOPE", (ftnlen)36, (ftnlen)
	    26);
    bltcod[325] = -177;
    s_copy(bltnam + 11700, "GRAIL-A", (ftnlen)36, (ftnlen)7);
    bltcod[326] = -178;
    s_copy(bltnam + 11736, "PLANET-B", (ftnlen)36, (ftnlen)8);
    bltcod[327] = -178;
    s_copy(bltnam + 11772, "NOZOMI", (ftnlen)36, (ftnlen)6);
    bltcod[328] = -181;
    s_copy(bltnam + 11808, "GRAIL-B", (ftnlen)36, (ftnlen)7);
    bltcod[329] = -183;
    s_copy(bltnam + 11844, "CLUSTER 1", (ftnlen)36, (ftnlen)9);
    bltcod[330] = -185;
    s_copy(bltnam + 11880, "CLUSTER 2", (ftnlen)36, (ftnlen)9);
    bltcod[331] = -188;
    s_copy(bltnam + 11916, "MUSES-B", (ftnlen)36, (ftnlen)7);
    bltcod[332] = -189;
    s_copy(bltnam + 11952, "NSYT", (ftnlen)36, (ftnlen)4);
    bltcod[333] = -189;
    s_copy(bltnam + 11988, "INSIGHT", (ftnlen)36, (ftnlen)7);
    bltcod[334] = -190;
    s_copy(bltnam + 12024, "SIM", (ftnlen)36, (ftnlen)3);
    bltcod[335] = -194;
    s_copy(bltnam + 12060, "CLUSTER 3", (ftnlen)36, (ftnlen)9);
    bltcod[336] = -196;
    s_copy(bltnam + 12096, "CLUSTER 4", (ftnlen)36, (ftnlen)9);
    bltcod[337] = -198;
    s_copy(bltnam + 12132, "INTEGRAL", (ftnlen)36, (ftnlen)8);
    bltcod[338] = -200;
    s_copy(bltnam + 12168, "CONTOUR", (ftnlen)36, (ftnlen)7);
    bltcod[339] = -202;
    s_copy(bltnam + 12204, "MAVEN", (ftnlen)36, (ftnlen)5);
    bltcod[340] = -203;
    s_copy(bltnam + 12240, "DAWN", (ftnlen)36, (ftnlen)4);
    bltcod[341] = -205;
    s_copy(bltnam + 12276, "SOIL MOISTURE ACTIVE AND PASSIVE", (ftnlen)36, (
	    ftnlen)32);
    bltcod[342] = -205;
    s_copy(bltnam + 12312, "SMAP", (ftnlen)36, (ftnlen)4);
    bltcod[343] = -212;
    s_copy(bltnam + 12348, "STV51", (ftnlen)36, (ftnlen)5);
    bltcod[344] = -213;
    s_copy(bltnam + 12384, "STV52", (ftnlen)36, (ftnlen)5);
    bltcod[345] = -214;
    s_copy(bltnam + 12420, "STV53", (ftnlen)36, (ftnlen)5);
    bltcod[346] = -226;
    s_copy(bltnam + 12456, "ROSETTA", (ftnlen)36, (ftnlen)7);
    bltcod[347] = -227;
    s_copy(bltnam + 12492, "KEPLER", (ftnlen)36, (ftnlen)6);
    bltcod[348] = -228;
    s_copy(bltnam + 12528, "GLL PROBE", (ftnlen)36, (ftnlen)9);
    bltcod[349] = -228;
    s_copy(bltnam + 12564, "GALILEO PROBE", (ftnlen)36, (ftnlen)13);
    bltcod[350] = -234;
    s_copy(bltnam + 12600, "STEREO AHEAD", (ftnlen)36, (ftnlen)12);
    bltcod[351] = -235;
    s_copy(bltnam + 12636, "STEREO BEHIND", (ftnlen)36, (ftnlen)13);
    bltcod[352] = -236;
    s_copy(bltnam + 12672, "MESSENGER", (ftnlen)36, (ftnlen)9);
    bltcod[353] = -238;
    s_copy(bltnam + 12708, "SMART1", (ftnlen)36, (ftnlen)6);
    bltcod[354] = -238;
    s_copy(bltnam + 12744, "SM1", (ftnlen)36, (ftnlen)3);
    bltcod[355] = -238;
    s_copy(bltnam + 12780, "S1", (ftnlen)36, (ftnlen)2);
    bltcod[356] = -238;
    s_copy(bltnam + 12816, "SMART-1", (ftnlen)36, (ftnlen)7);
    bltcod[357] = -248;
    s_copy(bltnam + 12852, "VEX", (ftnlen)36, (ftnlen)3);
    bltcod[358] = -248;
    s_copy(bltnam + 12888, "VENUS EXPRESS", (ftnlen)36, (ftnlen)13);
    bltcod[359] = -253;
    s_copy(bltnam + 12924, "OPPORTUNITY", (ftnlen)36, (ftnlen)11);
    bltcod[360] = -253;
    s_copy(bltnam + 12960, "MER-1", (ftnlen)36, (ftnlen)5);
    bltcod[361] = -254;
    s_copy(bltnam + 12996, "SPIRIT", (ftnlen)36, (ftnlen)6);
    bltcod[362] = -254;
    s_copy(bltnam + 13032, "MER-2", (ftnlen)36, (ftnlen)5);
    bltcod[363] = -362;
    s_copy(bltnam + 13068, "RADIATION BELT STORM PROBE A", (ftnlen)36, (
	    ftnlen)28);
    bltcod[364] = -362;
    s_copy(bltnam + 13104, "RBSP_A", (ftnlen)36, (ftnlen)6);
    bltcod[365] = -363;
    s_copy(bltnam + 13140, "RADIATION BELT STORM PROBE B", (ftnlen)36, (
	    ftnlen)28);
    bltcod[366] = -363;
    s_copy(bltnam + 13176, "RBSP_B", (ftnlen)36, (ftnlen)6);
    bltcod[367] = -500;
    s_copy(bltnam + 13212, "RSAT", (ftnlen)36, (ftnlen)4);
    bltcod[368] = -500;
    s_copy(bltnam + 13248, "SELENE Relay Satellite", (ftnlen)36, (ftnlen)22);
    bltcod[369] = -500;
    s_copy(bltnam + 13284, "SELENE Rstar", (ftnlen)36, (ftnlen)12);
    bltcod[370] = -500;
    s_copy(bltnam + 13320, "Rstar", (ftnlen)36, (ftnlen)5);
    bltcod[371] = -502;
    s_copy(bltnam + 13356, "VSAT", (ftnlen)36, (ftnlen)4);
    bltcod[372] = -502;
    s_copy(bltnam + 13392, "SELENE VLBI Radio Satellite", (ftnlen)36, (ftnlen)
	    27);
    bltcod[373] = -502;
    s_copy(bltnam + 13428, "SELENE VRAD Satellite", (ftnlen)36, (ftnlen)21);
    bltcod[374] = -502;
    s_copy(bltnam + 13464, "SELENE Vstar", (ftnlen)36, (ftnlen)12);
    bltcod[375] = -502;
    s_copy(bltnam + 13500, "Vstar", (ftnlen)36, (ftnlen)5);
    bltcod[376] = -550;
    s_copy(bltnam + 13536, "MARS-96", (ftnlen)36, (ftnlen)7);
    bltcod[377] = -550;
    s_copy(bltnam + 13572, "M96", (ftnlen)36, (ftnlen)3);
    bltcod[378] = -550;
    s_copy(bltnam + 13608, "MARS 96", (ftnlen)36, (ftnlen)7);
    bltcod[379] = -550;
    s_copy(bltnam + 13644, "MARS96", (ftnlen)36, (ftnlen)6);
    bltcod[380] = -750;
    s_copy(bltnam + 13680, "SPRINT-A", (ftnlen)36, (ftnlen)8);
    bltcod[381] = 50000001;
    s_copy(bltnam + 13716, "SHOEMAKER-LEVY 9-W", (ftnlen)36, (ftnlen)18);
    bltcod[382] = 50000002;
    s_copy(bltnam + 13752, "SHOEMAKER-LEVY 9-V", (ftnlen)36, (ftnlen)18);
    bltcod[383] = 50000003;
    s_copy(bltnam + 13788, "SHOEMAKER-LEVY 9-U", (ftnlen)36, (ftnlen)18);
    bltcod[384] = 50000004;
    s_copy(bltnam + 13824, "SHOEMAKER-LEVY 9-T", (ftnlen)36, (ftnlen)18);
    bltcod[385] = 50000005;
    s_copy(bltnam + 13860, "SHOEMAKER-LEVY 9-S", (ftnlen)36, (ftnlen)18);
    bltcod[386] = 50000006;
    s_copy(bltnam + 13896, "SHOEMAKER-LEVY 9-R", (ftnlen)36, (ftnlen)18);
    bltcod[387] = 50000007;
    s_copy(bltnam + 13932, "SHOEMAKER-LEVY 9-Q", (ftnlen)36, (ftnlen)18);
    bltcod[388] = 50000008;
    s_copy(bltnam + 13968, "SHOEMAKER-LEVY 9-P", (ftnlen)36, (ftnlen)18);
    bltcod[389] = 50000009;
    s_copy(bltnam + 14004, "SHOEMAKER-LEVY 9-N", (ftnlen)36, (ftnlen)18);
    bltcod[390] = 50000010;
    s_copy(bltnam + 14040, "SHOEMAKER-LEVY 9-M", (ftnlen)36, (ftnlen)18);
    bltcod[391] = 50000011;
    s_copy(bltnam + 14076, "SHOEMAKER-LEVY 9-L", (ftnlen)36, (ftnlen)18);
    bltcod[392] = 50000012;
    s_copy(bltnam + 14112, "SHOEMAKER-LEVY 9-K", (ftnlen)36, (ftnlen)18);
    bltcod[393] = 50000013;
    s_copy(bltnam + 14148, "SHOEMAKER-LEVY 9-J", (ftnlen)36, (ftnlen)18);
    bltcod[394] = 50000014;
    s_copy(bltnam + 14184, "SHOEMAKER-LEVY 9-H", (ftnlen)36, (ftnlen)18);
    bltcod[395] = 50000015;
    s_copy(bltnam + 14220, "SHOEMAKER-LEVY 9-G", (ftnlen)36, (ftnlen)18);
    bltcod[396] = 50000016;
    s_copy(bltnam + 14256, "SHOEMAKER-LEVY 9-F", (ftnlen)36, (ftnlen)18);
    bltcod[397] = 50000017;
    s_copy(bltnam + 14292, "SHOEMAKER-LEVY 9-E", (ftnlen)36, (ftnlen)18);
    bltcod[398] = 50000018;
    s_copy(bltnam + 14328, "SHOEMAKER-LEVY 9-D", (ftnlen)36, (ftnlen)18);
    bltcod[399] = 50000019;
    s_copy(bltnam + 14364, "SHOEMAKER-LEVY 9-C", (ftnlen)36, (ftnlen)18);
    bltcod[400] = 50000020;
    s_copy(bltnam + 14400, "SHOEMAKER-LEVY 9-B", (ftnlen)36, (ftnlen)18);
    bltcod[401] = 50000021;
    s_copy(bltnam + 14436, "SHOEMAKER-LEVY 9-A", (ftnlen)36, (ftnlen)18);
    bltcod[402] = 50000022;
    s_copy(bltnam + 14472, "SHOEMAKER-LEVY 9-Q1", (ftnlen)36, (ftnlen)19);
    bltcod[403] = 50000023;
    s_copy(bltnam + 14508, "SHOEMAKER-LEVY 9-P2", (ftnlen)36, (ftnlen)19);
    bltcod[404] = 1000001;
    s_copy(bltnam + 14544, "AREND", (ftnlen)36, (ftnlen)5);
    bltcod[405] = 1000002;
    s_copy(bltnam + 14580, "AREND-RIGAUX", (ftnlen)36, (ftnlen)12);
    bltcod[406] = 1000003;
    s_copy(bltnam + 14616, "ASHBROOK-JACKSON", (ftnlen)36, (ftnlen)16);
    bltcod[407] = 1000004;
    s_copy(bltnam + 14652, "BOETHIN", (ftnlen)36, (ftnlen)7);
    bltcod[408] = 1000005;
    s_copy(bltnam + 14688, "BORRELLY", (ftnlen)36, (ftnlen)8);
    bltcod[409] = 1000006;
    s_copy(bltnam + 14724, "BOWELL-SKIFF", (ftnlen)36, (ftnlen)12);
    bltcod[410] = 1000007;
    s_copy(bltnam + 14760, "BRADFIELD", (ftnlen)36, (ftnlen)9);
    bltcod[411] = 1000008;
    s_copy(bltnam + 14796, "BROOKS 2", (ftnlen)36, (ftnlen)8);
    bltcod[412] = 1000009;
    s_copy(bltnam + 14832, "BRORSEN-METCALF", (ftnlen)36, (ftnlen)15);
    bltcod[413] = 1000010;
    s_copy(bltnam + 14868, "BUS", (ftnlen)36, (ftnlen)3);
    bltcod[414] = 1000011;
    s_copy(bltnam + 14904, "CHERNYKH", (ftnlen)36, (ftnlen)8);
    bltcod[415] = 1000012;
    s_copy(bltnam + 14940, "67P/CHURYUMOV-GERASIMENKO (1969 R1)", (ftnlen)36, 
	    (ftnlen)35);
    bltcod[416] = 1000012;
    s_copy(bltnam + 14976, "CHURYUMOV-GERASIMENKO", (ftnlen)36, (ftnlen)21);
    bltcod[417] = 1000013;
    s_copy(bltnam + 15012, "CIFFREO", (ftnlen)36, (ftnlen)7);
    bltcod[418] = 1000014;
    s_copy(bltnam + 15048, "CLARK", (ftnlen)36, (ftnlen)5);
    bltcod[419] = 1000015;
    s_copy(bltnam + 15084, "COMAS SOLA", (ftnlen)36, (ftnlen)10);
    bltcod[420] = 1000016;
    s_copy(bltnam + 15120, "CROMMELIN", (ftnlen)36, (ftnlen)9);
    bltcod[421] = 1000017;
    s_copy(bltnam + 15156, "D'ARREST", (ftnlen)36, (ftnlen)8);
    bltcod[422] = 1000018;
    s_copy(bltnam + 15192, "DANIEL", (ftnlen)36, (ftnlen)6);
    bltcod[423] = 1000019;
    s_copy(bltnam + 15228, "DE VICO-SWIFT", (ftnlen)36, (ftnlen)13);
    bltcod[424] = 1000020;
    s_copy(bltnam + 15264, "DENNING-FUJIKAWA", (ftnlen)36, (ftnlen)16);
    bltcod[425] = 1000021;
    s_copy(bltnam + 15300, "DU TOIT 1", (ftnlen)36, (ftnlen)9);
    bltcod[426] = 1000022;
    s_copy(bltnam + 15336, "DU TOIT-HARTLEY", (ftnlen)36, (ftnlen)15);
    bltcod[427] = 1000023;
    s_copy(bltnam + 15372, "DUTOIT-NEUJMIN-DELPORTE", (ftnlen)36, (ftnlen)23);
    bltcod[428] = 1000024;
    s_copy(bltnam + 15408, "DUBIAGO", (ftnlen)36, (ftnlen)7);
    bltcod[429] = 1000025;
    s_copy(bltnam + 15444, "ENCKE", (ftnlen)36, (ftnlen)5);
    bltcod[430] = 1000026;
    s_copy(bltnam + 15480, "FAYE", (ftnlen)36, (ftnlen)4);
    bltcod[431] = 1000027;
    s_copy(bltnam + 15516, "FINLAY", (ftnlen)36, (ftnlen)6);
    bltcod[432] = 1000028;
    s_copy(bltnam + 15552, "FORBES", (ftnlen)36, (ftnlen)6);
    bltcod[433] = 1000029;
    s_copy(bltnam + 15588, "GEHRELS 1", (ftnlen)36, (ftnlen)9);
    bltcod[434] = 1000030;
    s_copy(bltnam + 15624, "GEHRELS 2", (ftnlen)36, (ftnlen)9);
    bltcod[435] = 1000031;
    s_copy(bltnam + 15660, "GEHRELS 3", (ftnlen)36, (ftnlen)9);
    bltcod[436] = 1000032;
    s_copy(bltnam + 15696, "GIACOBINI-ZINNER", (ftnlen)36, (ftnlen)16);
    bltcod[437] = 1000033;
    s_copy(bltnam + 15732, "GICLAS", (ftnlen)36, (ftnlen)6);
    bltcod[438] = 1000034;
    s_copy(bltnam + 15768, "GRIGG-SKJELLERUP", (ftnlen)36, (ftnlen)16);
    bltcod[439] = 1000035;
    s_copy(bltnam + 15804, "GUNN", (ftnlen)36, (ftnlen)4);
    bltcod[440] = 1000036;
    s_copy(bltnam + 15840, "HALLEY", (ftnlen)36, (ftnlen)6);
    bltcod[441] = 1000037;
    s_copy(bltnam + 15876, "HANEDA-CAMPOS", (ftnlen)36, (ftnlen)13);
    bltcod[442] = 1000038;
    s_copy(bltnam + 15912, "HARRINGTON", (ftnlen)36, (ftnlen)10);
    bltcod[443] = 1000039;
    s_copy(bltnam + 15948, "HARRINGTON-ABELL", (ftnlen)36, (ftnlen)16);
    bltcod[444] = 1000040;
    s_copy(bltnam + 15984, "HARTLEY 1", (ftnlen)36, (ftnlen)9);
    bltcod[445] = 1000041;
    s_copy(bltnam + 16020, "HARTLEY 2", (ftnlen)36, (ftnlen)9);
    bltcod[446] = 1000042;
    s_copy(bltnam + 16056, "HARTLEY-IRAS", (ftnlen)36, (ftnlen)12);
    bltcod[447] = 1000043;
    s_copy(bltnam + 16092, "HERSCHEL-RIGOLLET", (ftnlen)36, (ftnlen)17);
    bltcod[448] = 1000044;
    s_copy(bltnam + 16128, "HOLMES", (ftnlen)36, (ftnlen)6);
    bltcod[449] = 1000045;
    s_copy(bltnam + 16164, "HONDA-MRKOS-PAJDUSAKOVA", (ftnlen)36, (ftnlen)23);
    bltcod[450] = 1000046;
    s_copy(bltnam + 16200, "HOWELL", (ftnlen)36, (ftnlen)6);
    bltcod[451] = 1000047;
    s_copy(bltnam + 16236, "IRAS", (ftnlen)36, (ftnlen)4);
    bltcod[452] = 1000048;
    s_copy(bltnam + 16272, "JACKSON-NEUJMIN", (ftnlen)36, (ftnlen)15);
    bltcod[453] = 1000049;
    s_copy(bltnam + 16308, "JOHNSON", (ftnlen)36, (ftnlen)7);
    bltcod[454] = 1000050;
    s_copy(bltnam + 16344, "KEARNS-KWEE", (ftnlen)36, (ftnlen)11);
    bltcod[455] = 1000051;
    s_copy(bltnam + 16380, "KLEMOLA", (ftnlen)36, (ftnlen)7);
    bltcod[456] = 1000052;
    s_copy(bltnam + 16416, "KOHOUTEK", (ftnlen)36, (ftnlen)8);
    bltcod[457] = 1000053;
    s_copy(bltnam + 16452, "KOJIMA", (ftnlen)36, (ftnlen)6);
    bltcod[458] = 1000054;
    s_copy(bltnam + 16488, "KOPFF", (ftnlen)36, (ftnlen)5);
    bltcod[459] = 1000055;
    s_copy(bltnam + 16524, "KOWAL 1", (ftnlen)36, (ftnlen)7);
    bltcod[460] = 1000056;
    s_copy(bltnam + 16560, "KOWAL 2", (ftnlen)36, (ftnlen)7);
    bltcod[461] = 1000057;
    s_copy(bltnam + 16596, "KOWAL-MRKOS", (ftnlen)36, (ftnlen)11);
    bltcod[462] = 1000058;
    s_copy(bltnam + 16632, "KOWAL-VAVROVA", (ftnlen)36, (ftnlen)13);
    bltcod[463] = 1000059;
    s_copy(bltnam + 16668, "LONGMORE", (ftnlen)36, (ftnlen)8);
    bltcod[464] = 1000060;
    s_copy(bltnam + 16704, "LOVAS 1", (ftnlen)36, (ftnlen)7);
    bltcod[465] = 1000061;
    s_copy(bltnam + 16740, "MACHHOLZ", (ftnlen)36, (ftnlen)8);
    bltcod[466] = 1000062;
    s_copy(bltnam + 16776, "MAURY", (ftnlen)36, (ftnlen)5);
    bltcod[467] = 1000063;
    s_copy(bltnam + 16812, "NEUJMIN 1", (ftnlen)36, (ftnlen)9);
    bltcod[468] = 1000064;
    s_copy(bltnam + 16848, "NEUJMIN 2", (ftnlen)36, (ftnlen)9);
    bltcod[469] = 1000065;
    s_copy(bltnam + 16884, "NEUJMIN 3", (ftnlen)36, (ftnlen)9);
    bltcod[470] = 1000066;
    s_copy(bltnam + 16920, "OLBERS", (ftnlen)36, (ftnlen)6);
    bltcod[471] = 1000067;
    s_copy(bltnam + 16956, "PETERS-HARTLEY", (ftnlen)36, (ftnlen)14);
    bltcod[472] = 1000068;
    s_copy(bltnam + 16992, "PONS-BROOKS", (ftnlen)36, (ftnlen)11);
    bltcod[473] = 1000069;
    s_copy(bltnam + 17028, "PONS-WINNECKE", (ftnlen)36, (ftnlen)13);
    bltcod[474] = 1000070;
    s_copy(bltnam + 17064, "REINMUTH 1", (ftnlen)36, (ftnlen)10);
    bltcod[475] = 1000071;
    s_copy(bltnam + 17100, "REINMUTH 2", (ftnlen)36, (ftnlen)10);
    bltcod[476] = 1000072;
    s_copy(bltnam + 17136, "RUSSELL 1", (ftnlen)36, (ftnlen)9);
    bltcod[477] = 1000073;
    s_copy(bltnam + 17172, "RUSSELL 2", (ftnlen)36, (ftnlen)9);
    bltcod[478] = 1000074;
    s_copy(bltnam + 17208, "RUSSELL 3", (ftnlen)36, (ftnlen)9);
    bltcod[479] = 1000075;
    s_copy(bltnam + 17244, "RUSSELL 4", (ftnlen)36, (ftnlen)9);
    bltcod[480] = 1000076;
    s_copy(bltnam + 17280, "SANGUIN", (ftnlen)36, (ftnlen)7);
    bltcod[481] = 1000077;
    s_copy(bltnam + 17316, "SCHAUMASSE", (ftnlen)36, (ftnlen)10);
    bltcod[482] = 1000078;
    s_copy(bltnam + 17352, "SCHUSTER", (ftnlen)36, (ftnlen)8);
    bltcod[483] = 1000079;
    s_copy(bltnam + 17388, "SCHWASSMANN-WACHMANN 1", (ftnlen)36, (ftnlen)22);
    bltcod[484] = 1000080;
    s_copy(bltnam + 17424, "SCHWASSMANN-WACHMANN 2", (ftnlen)36, (ftnlen)22);
    bltcod[485] = 1000081;
    s_copy(bltnam + 17460, "SCHWASSMANN-WACHMANN 3", (ftnlen)36, (ftnlen)22);
    bltcod[486] = 1000082;
    s_copy(bltnam + 17496, "SHAJN-SCHALDACH", (ftnlen)36, (ftnlen)15);
    bltcod[487] = 1000083;
    s_copy(bltnam + 17532, "SHOEMAKER 1", (ftnlen)36, (ftnlen)11);
    bltcod[488] = 1000084;
    s_copy(bltnam + 17568, "SHOEMAKER 2", (ftnlen)36, (ftnlen)11);
    bltcod[489] = 1000085;
    s_copy(bltnam + 17604, "SHOEMAKER 3", (ftnlen)36, (ftnlen)11);
    bltcod[490] = 1000086;
    s_copy(bltnam + 17640, "SINGER-BREWSTER", (ftnlen)36, (ftnlen)15);
    bltcod[491] = 1000087;
    s_copy(bltnam + 17676, "SLAUGHTER-BURNHAM", (ftnlen)36, (ftnlen)17);
    bltcod[492] = 1000088;
    s_copy(bltnam + 17712, "SMIRNOVA-CHERNYKH", (ftnlen)36, (ftnlen)17);
    bltcod[493] = 1000089;
    s_copy(bltnam + 17748, "STEPHAN-OTERMA", (ftnlen)36, (ftnlen)14);
    bltcod[494] = 1000090;
    s_copy(bltnam + 17784, "SWIFT-GEHRELS", (ftnlen)36, (ftnlen)13);
    bltcod[495] = 1000091;
    s_copy(bltnam + 17820, "TAKAMIZAWA", (ftnlen)36, (ftnlen)10);
    bltcod[496] = 1000092;
    s_copy(bltnam + 17856, "TAYLOR", (ftnlen)36, (ftnlen)6);
    bltcod[497] = 1000093;
    s_copy(bltnam + 17892, "TEMPEL_1", (ftnlen)36, (ftnlen)8);
    bltcod[498] = 1000093;
    s_copy(bltnam + 17928, "TEMPEL 1", (ftnlen)36, (ftnlen)8);
    bltcod[499] = 1000094;
    s_copy(bltnam + 17964, "TEMPEL 2", (ftnlen)36, (ftnlen)8);
    bltcod[500] = 1000095;
    s_copy(bltnam + 18000, "TEMPEL-TUTTLE", (ftnlen)36, (ftnlen)13);
    bltcod[501] = 1000096;
    s_copy(bltnam + 18036, "TRITTON", (ftnlen)36, (ftnlen)7);
    bltcod[502] = 1000097;
    s_copy(bltnam + 18072, "TSUCHINSHAN 1", (ftnlen)36, (ftnlen)13);
    bltcod[503] = 1000098;
    s_copy(bltnam + 18108, "TSUCHINSHAN 2", (ftnlen)36, (ftnlen)13);
    bltcod[504] = 1000099;
    s_copy(bltnam + 18144, "TUTTLE", (ftnlen)36, (ftnlen)6);
    bltcod[505] = 1000100;
    s_copy(bltnam + 18180, "TUTTLE-GIACOBINI-KRESAK", (ftnlen)36, (ftnlen)23);
    bltcod[506] = 1000101;
    s_copy(bltnam + 18216, "VAISALA 1", (ftnlen)36, (ftnlen)9);
    bltcod[507] = 1000102;
    s_copy(bltnam + 18252, "VAN BIESBROECK", (ftnlen)36, (ftnlen)14);
    bltcod[508] = 1000103;
    s_copy(bltnam + 18288, "VAN HOUTEN", (ftnlen)36, (ftnlen)10);
    bltcod[509] = 1000104;
    s_copy(bltnam + 18324, "WEST-KOHOUTEK-IKEMURA", (ftnlen)36, (ftnlen)21);
    bltcod[510] = 1000105;
    s_copy(bltnam + 18360, "WHIPPLE", (ftnlen)36, (ftnlen)7);
    bltcod[511] = 1000106;
    s_copy(bltnam + 18396, "WILD 1", (ftnlen)36, (ftnlen)6);
    bltcod[512] = 1000107;
    s_copy(bltnam + 18432, "WILD 2", (ftnlen)36, (ftnlen)6);
    bltcod[513] = 1000108;
    s_copy(bltnam + 18468, "WILD 3", (ftnlen)36, (ftnlen)6);
    bltcod[514] = 1000109;
    s_copy(bltnam + 18504, "WIRTANEN", (ftnlen)36, (ftnlen)8);
    bltcod[515] = 1000110;
    s_copy(bltnam + 18540, "WOLF", (ftnlen)36, (ftnlen)4);
    bltcod[516] = 1000111;
    s_copy(bltnam + 18576, "WOLF-HARRINGTON", (ftnlen)36, (ftnlen)15);
    bltcod[517] = 1000112;
    s_copy(bltnam + 18612, "LOVAS 2", (ftnlen)36, (ftnlen)7);
    bltcod[518] = 1000113;
    s_copy(bltnam + 18648, "URATA-NIIJIMA", (ftnlen)36, (ftnlen)13);
    bltcod[519] = 1000114;
    s_copy(bltnam + 18684, "WISEMAN-SKIFF", (ftnlen)36, (ftnlen)13);
    bltcod[520] = 1000115;
    s_copy(bltnam + 18720, "HELIN", (ftnlen)36, (ftnlen)5);
    bltcod[521] = 1000116;
    s_copy(bltnam + 18756, "MUELLER", (ftnlen)36, (ftnlen)7);
    bltcod[522] = 1000117;
    s_copy(bltnam + 18792, "SHOEMAKER-HOLT 1", (ftnlen)36, (ftnlen)16);
    bltcod[523] = 1000118;
    s_copy(bltnam + 18828, "HELIN-ROMAN-CROCKETT", (ftnlen)36, (ftnlen)20);
    bltcod[524] = 1000119;
    s_copy(bltnam + 18864, "HARTLEY 3", (ftnlen)36, (ftnlen)9);
    bltcod[525] = 1000120;
    s_copy(bltnam + 18900, "PARKER-HARTLEY", (ftnlen)36, (ftnlen)14);
    bltcod[526] = 1000121;
    s_copy(bltnam + 18936, "HELIN-ROMAN-ALU 1", (ftnlen)36, (ftnlen)17);
    bltcod[527] = 1000122;
    s_copy(bltnam + 18972, "WILD 4", (ftnlen)36, (ftnlen)6);
    bltcod[528] = 1000123;
    s_copy(bltnam + 19008, "MUELLER 2", (ftnlen)36, (ftnlen)9);
    bltcod[529] = 1000124;
    s_copy(bltnam + 19044, "MUELLER 3", (ftnlen)36, (ftnlen)9);
    bltcod[530] = 1000125;
    s_copy(bltnam + 19080, "SHOEMAKER-LEVY 1", (ftnlen)36, (ftnlen)16);
    bltcod[531] = 1000126;
    s_copy(bltnam + 19116, "SHOEMAKER-LEVY 2", (ftnlen)36, (ftnlen)16);
    bltcod[532] = 1000127;
    s_copy(bltnam + 19152, "HOLT-OLMSTEAD", (ftnlen)36, (ftnlen)13);
    bltcod[533] = 1000128;
    s_copy(bltnam + 19188, "METCALF-BREWINGTON", (ftnlen)36, (ftnlen)18);
    bltcod[534] = 1000129;
    s_copy(bltnam + 19224, "LEVY", (ftnlen)36, (ftnlen)4);
    bltcod[535] = 1000130;
    s_copy(bltnam + 19260, "SHOEMAKER-LEVY 9", (ftnlen)36, (ftnlen)16);
    bltcod[536] = 1000131;
    s_copy(bltnam + 19296, "HYAKUTAKE", (ftnlen)36, (ftnlen)9);
    bltcod[537] = 1000132;
    s_copy(bltnam + 19332, "HALE-BOPP", (ftnlen)36, (ftnlen)9);
    bltcod[538] = 1003228;
    s_copy(bltnam + 19368, "C/2013 A1", (ftnlen)36, (ftnlen)9);
    bltcod[539] = 1003228;
    s_copy(bltnam + 19404, "SIDING SPRING", (ftnlen)36, (ftnlen)13);
    bltcod[540] = 9511010;
    s_copy(bltnam + 19440, "GASPRA", (ftnlen)36, (ftnlen)6);
    bltcod[541] = 2431010;
    s_copy(bltnam + 19476, "IDA", (ftnlen)36, (ftnlen)3);
    bltcod[542] = 2431011;
    s_copy(bltnam + 19512, "DACTYL", (ftnlen)36, (ftnlen)6);
    bltcod[543] = 2000001;
    s_copy(bltnam + 19548, "CERES", (ftnlen)36, (ftnlen)5);
    bltcod[544] = 2000002;
    s_copy(bltnam + 19584, "PALLAS", (ftnlen)36, (ftnlen)6);
    bltcod[545] = 2000004;
    s_copy(bltnam + 19620, "VESTA", (ftnlen)36, (ftnlen)5);
    bltcod[546] = 2000021;
    s_copy(bltnam + 19656, "LUTETIA", (ftnlen)36, (ftnlen)7);
    bltcod[547] = 2000216;
    s_copy(bltnam + 19692, "KLEOPATRA", (ftnlen)36, (ftnlen)9);
    bltcod[548] = 2000433;
    s_copy(bltnam + 19728, "EROS", (ftnlen)36, (ftnlen)4);
    bltcod[549] = 2000511;
    s_copy(bltnam + 19764, "DAVIDA", (ftnlen)36, (ftnlen)6);
    bltcod[550] = 2000253;
    s_copy(bltnam + 19800, "MATHILDE", (ftnlen)36, (ftnlen)8);
    bltcod[551] = 2002867;
    s_copy(bltnam + 19836, "STEINS", (ftnlen)36, (ftnlen)6);
    bltcod[552] = 2009969;
    s_copy(bltnam + 19872, "1992KD", (ftnlen)36, (ftnlen)6);
    bltcod[553] = 2009969;
    s_copy(bltnam + 19908, "BRAILLE", (ftnlen)36, (ftnlen)7);
    bltcod[554] = 2004015;
    s_copy(bltnam + 19944, "WILSON-HARRINGTON", (ftnlen)36, (ftnlen)17);
    bltcod[555] = 2004179;
    s_copy(bltnam + 19980, "TOUTATIS", (ftnlen)36, (ftnlen)8);
    bltcod[556] = 2025143;
    s_copy(bltnam + 20016, "ITOKAWA", (ftnlen)36, (ftnlen)7);
    bltcod[557] = 398989;
    s_copy(bltnam + 20052, "NOTO", (ftnlen)36, (ftnlen)4);
    bltcod[558] = 398990;
    s_copy(bltnam + 20088, "NEW NORCIA", (ftnlen)36, (ftnlen)10);
    bltcod[559] = 399001;
    s_copy(bltnam + 20124, "GOLDSTONE", (ftnlen)36, (ftnlen)9);
    bltcod[560] = 399002;
    s_copy(bltnam + 20160, "CANBERRA", (ftnlen)36, (ftnlen)8);
    bltcod[561] = 399003;
    s_copy(bltnam + 20196, "MADRID", (ftnlen)36, (ftnlen)6);
    bltcod[562] = 399004;
    s_copy(bltnam + 20232, "USUDA", (ftnlen)36, (ftnlen)5);
    bltcod[563] = 399005;
    s_copy(bltnam + 20268, "DSS-05", (ftnlen)36, (ftnlen)6);
    bltcod[564] = 399005;
    s_copy(bltnam + 20304, "PARKES", (ftnlen)36, (ftnlen)6);
    bltcod[565] = 399012;
    s_copy(bltnam + 20340, "DSS-12", (ftnlen)36, (ftnlen)6);
    bltcod[566] = 399013;
    s_copy(bltnam + 20376, "DSS-13", (ftnlen)36, (ftnlen)6);
    bltcod[567] = 399014;
    s_copy(bltnam + 20412, "DSS-14", (ftnlen)36, (ftnlen)6);
    bltcod[568] = 399015;
    s_copy(bltnam + 20448, "DSS-15", (ftnlen)36, (ftnlen)6);
    bltcod[569] = 399016;
    s_copy(bltnam + 20484, "DSS-16", (ftnlen)36, (ftnlen)6);
    bltcod[570] = 399017;
    s_copy(bltnam + 20520, "DSS-17", (ftnlen)36, (ftnlen)6);
    bltcod[571] = 399023;
    s_copy(bltnam + 20556, "DSS-23", (ftnlen)36, (ftnlen)6);
    bltcod[572] = 399024;
    s_copy(bltnam + 20592, "DSS-24", (ftnlen)36, (ftnlen)6);
    bltcod[573] = 399025;
    s_copy(bltnam + 20628, "DSS-25", (ftnlen)36, (ftnlen)6);
    bltcod[574] = 399026;
    s_copy(bltnam + 20664, "DSS-26", (ftnlen)36, (ftnlen)6);
    bltcod[575] = 399027;
    s_copy(bltnam + 20700, "DSS-27", (ftnlen)36, (ftnlen)6);
    bltcod[576] = 399028;
    s_copy(bltnam + 20736, "DSS-28", (ftnlen)36, (ftnlen)6);
    bltcod[577] = 399033;
    s_copy(bltnam + 20772, "DSS-33", (ftnlen)36, (ftnlen)6);
    bltcod[578] = 399034;
    s_copy(bltnam + 20808, "DSS-34", (ftnlen)36, (ftnlen)6);
    bltcod[579] = 399042;
    s_copy(bltnam + 20844, "DSS-42", (ftnlen)36, (ftnlen)6);
    bltcod[580] = 399043;
    s_copy(bltnam + 20880, "DSS-43", (ftnlen)36, (ftnlen)6);
    bltcod[581] = 399045;
    s_copy(bltnam + 20916, "DSS-45", (ftnlen)36, (ftnlen)6);
    bltcod[582] = 399046;
    s_copy(bltnam + 20952, "DSS-46", (ftnlen)36, (ftnlen)6);
    bltcod[583] = 399049;
    s_copy(bltnam + 20988, "DSS-49", (ftnlen)36, (ftnlen)6);
    bltcod[584] = 399053;
    s_copy(bltnam + 21024, "DSS-53", (ftnlen)36, (ftnlen)6);
    bltcod[585] = 399054;
    s_copy(bltnam + 21060, "DSS-54", (ftnlen)36, (ftnlen)6);
    bltcod[586] = 399055;
    s_copy(bltnam + 21096, "DSS-55", (ftnlen)36, (ftnlen)6);
    bltcod[587] = 399061;
    s_copy(bltnam + 21132, "DSS-61", (ftnlen)36, (ftnlen)6);
    bltcod[588] = 399063;
    s_copy(bltnam + 21168, "DSS-63", (ftnlen)36, (ftnlen)6);
    bltcod[589] = 399064;
    s_copy(bltnam + 21204, "DSS-64", (ftnlen)36, (ftnlen)6);
    bltcod[590] = 399065;
    s_copy(bltnam + 21240, "DSS-65", (ftnlen)36, (ftnlen)6);
    bltcod[591] = 399066;
    s_copy(bltnam + 21276, "DSS-66", (ftnlen)36, (ftnlen)6);
    return 0;
} /* zzidmap_ */

