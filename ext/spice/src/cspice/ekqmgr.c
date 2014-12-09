/* ekqmgr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__20 = 20;
static integer c__200 = 200;
static integer c__10000 = 10000;
static integer c__500 = 500;
static integer c__100 = 100;
static integer c__24 = 24;
static integer c__11 = 11;
static integer c__10 = 10;
static integer c__1000 = 1000;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__11000 = 11000;

/* $Procedure EKQMGR  ( EK, query manager ) */
/* Subroutine */ int ekqmgr_0_(int n__, integer *cindex, integer *elment, 
	char *eqryc, doublereal *eqryd, integer *eqryi, char *fname, integer *
	row, integer *selidx, char *column, integer *handle, integer *n, char 
	*table, integer *attdsc, integer *ccount, logical *found, integer *
	nelt, integer *nmrows, logical *semerr, char *errmsg, char *cdata, 
	doublereal *ddata, integer *idata, logical *null, ftnlen eqryc_len, 
	ftnlen fname_len, ftnlen column_len, ftnlen table_len, ftnlen 
	errmsg_len, ftnlen cdata_len)
{
    /* Initialized data */

    static integer lelts[1000] = { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
    static integer oelts[10] = { 1,1,1,1,1,1,1,1,1,1 };
    static integer relts[1000] = { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };
    static char chtype[4*4] = "CHR " "DP  " "INT " "TIME";
    static integer fthead = 0;
    static integer tbhead = 0;
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10, 
	    i__11, i__12, i__13, i__14;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char tabvec[64*16];
    static integer begidx, cdscrs[5500]	/* was [11][500] */, cjrows, cjsize, 
	    cnstyp[1000], col, colptr, delseg, endidx, key, keydsc[11], 
	    ldscrs[11000]	/* was [11][1000] */, ltbidx[1000], new__, 
	    nmatch, norder, nsv, ops[1000], ordbas, nact, ntab, ptroff, rbas[
	    10], conj, rdscrs[11000]	/* was [11][1000] */, resbas, nseg, 
	    rowidx, rowvec[10], rtbidx[1000], nsel, rtotal, rwvbas, selcol[50]
	    , selctp[50], seltab[50], seg, segdsc[24], segvec[10], sgvbas, 
	    tab, next, tabidx, tbcurr, top, tptvec[16], unit, unrows;
    static logical activc[1000], activv[1000], attmch, dosort, fnd, indexd, 
	    keyfnd, nulsok, presnt, sorted;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), lnkini_(integer *, integer *), dascls_(integer *), 
	    dashlu_(integer *, integer *), zzekpgch_(integer *, char *, 
	    ftnlen), setmsg_(char *, ftnlen), errfnm_(char *, integer *, 
	    ftnlen), lnkilb_(integer *, integer *, integer *), ssizec_(
	    integer *, char *, ftnlen), validc_(integer *, integer *, char *, 
	    ftnlen), lnkfsl_(integer *, integer *, integer *), errint_(char *,
	     integer *, ftnlen);
    extern logical zzekrmch_(integer *, logical *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, char *, 
	    integer *, integer *, doublereal *, integer *, ftnlen), zzekvmch_(
	    integer *, logical *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *);
    extern /* Subroutine */ int zzeksinf_(integer *, integer *, char *, 
	    integer *, char *, integer *, ftnlen, ftnlen), zzekreqi_(integer *
	    , char *, integer *, ftnlen), zzekqtab_(integer *, char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen), ssizei_(
	    integer *, integer *), appndc_(char *, char *, ftnlen, ftnlen), 
	    appndi_(integer *, integer *), zzeksdec_(integer *), cleari_(
	    integer *, integer *), zzekqcnj_(integer *, integer *, integer *),
	     zzekqcon_(integer *, char *, doublereal *, integer *, integer *, 
	    char *, integer *, char *, integer *, integer *, char *, integer *
	    , char *, integer *, integer *, integer *, integer *, doublereal *
	    , integer *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern integer zzekesiz_(integer *, integer *, integer *, integer *);
    extern /* Subroutine */ int zzeksupd_(integer *, integer *, integer *), 
	    zzekkey_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    doublereal *, integer *, logical *, integer *, integer *, integer 
	    *, integer *, logical *, ftnlen), zzekspsh_(integer *, integer *),
	     zzekixlk_(integer *, integer *, integer *, integer *), zzekrplk_(
	    integer *, integer *, integer *, integer *), zzekjoin_(integer *, 
	    integer *, integer *, logical *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *), zzeksrd_(
	    integer *, integer *, integer *), zzekweed_(integer *, integer *, 
	    integer *), zzekvset_(integer *, integer *), zzekqsel_(integer *, 
	    char *, integer *, integer *, integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen, ftnlen), zzekstop_(integer *);
    static integer i__, cjbeg, j;
    extern integer cardc_(char *, ftnlen);
    static integer k, cbegs[1000], cjend, l, r__, t, cends[1000];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static logical cmtch;
    static integer ubase[200], fthan[20];
    static char cnams[32*500];
    static integer lxbeg, lcidx[1000];
    extern /* Subroutine */ int ekcls_(integer *);
    static integer cvlen;
    static doublereal dvals[1000];
    static integer lxend, nconj, sthan[200], ivals[1000], ncols;
    static char state[80];
    static integer ctnew;
    extern integer lnktl_(integer *, integer *);
    static integer dtnew, dtype[1000], jsize;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static integer npcol, ocols[10], otabs[10], jbase1, jbase2, rcidx[1000], 
	    rsize[200], sense[10], sizes[1000], stnew, usize;
    static logical vmtch;
    extern /* Subroutine */ int ekopr_(char *, integer *, ftnlen), lnkan_(
	    integer *, integer *), movec_(char *, integer *, char *, ftnlen, 
	    ftnlen), movei_(integer *, integer *, integer *), errch_(char *, 
	    char *, ftnlen, ftnlen), zzekjsqz_(integer *), zzekqord_(integer *
	    , char *, integer *, char *, integer *, char *, integer *, 
	    integer *, ftnlen, ftnlen, ftnlen), zzekjsrt_(integer *, integer *
	    , integer *, integer *, integer *, integer *, integer *, integer *
	    , integer *, integer *, integer *, integer *, integer *), 
	    zzekvcal_(integer *, integer *, integer *), zzekrsc_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, char *, 
	    logical *, logical *, ftnlen), zzekrsd_(integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, logical *, logical 
	    *), zzekrsi_(integer *, integer *, integer *, integer *, integer *
	    , integer *, logical *, logical *);
    extern logical failed_(void);
    static integer ctclas[500];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern integer eknseg_(integer *), lnknxt_(integer *, integer *), lnknfn_(
	    integer *);
    static integer ftpool[52]	/* was [2][26] */, tbpool[212]	/* was [2][
	    106] */, tbstpt[100], tbncol[100];
    static char tbnams[64*100];
    static integer tbctpt[100], tbfils[2000]	/* was [20][100] */, tbflsz[
	    100], stpool[412]	/* was [2][206] */, stsidx[200], stdscs[4800]	
	    /* was [24][200] */, stnrow[200], stncol[200], stdtpt[200], 
	    dtpool[20012]	/* was [2][10006] */, dtdscs[110000]	/* 
	    was [11][10000] */, ctpool[1012]	/* was [2][506] */;
    static char ctnams[32*500];
    static integer cttyps[500], ctlens[500];
    static logical ctfixd[500];
    static integer ctsizs[500];
    static logical ctindx[500], ctnull[500];
    static char cnmset[32*506], colnam[32], frmals[64*10], frmtab[64*10], 
	    lcname[32], ltname[64], problm[80], rcname[32], rtname[64], 
	    tabnam[64];

/* $ Abstract */

/*     Manage query operations on EK files. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */
/*     SEARCH */

/* $ Declarations */
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


/*     Include Section:  EK Column Attribute Descriptor Parameters */

/*        ekattdsc.inc Version 1    23-AUG-1995 (NJB) */


/*     This include file declares parameters used in EK column */
/*     attribute descriptors.  Column attribute descriptors are */
/*     a simplified version of column descriptors:  attribute */
/*     descriptors describe attributes of a column but do not contain */
/*     addresses or pointers. */


/*     Size of column attribute descriptor */


/*     Indices of various pieces of attribute descriptors: */


/*     ATTSIZ is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     ATTTYP is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     ATTLEN is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     ATTSIZ is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     ATTIDX is the location of a flag that indicates whether the column */
/*     is indexed.  The flag takes the value ITRUE if the column is */
/*     indexed and otherwise takes the value IFALSE. */


/*     ATTNFL is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     End Include Section:  EK Column Attribute Descriptor Parameters */

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


/*     Include Section:  EK Boolean Enumerated Type */


/*        ekbool.inc Version 1   21-DEC-1994 (NJB) */


/*     Within the EK system, boolean values sometimes must be */
/*     represented by integer or character codes.  The codes and their */
/*     meanings are listed below. */

/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     Character code indicating `true': */


/*     Character code indicating `false': */


/*     End Include Section:  EK Boolean Enumerated Type */

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


/*     Include Section:  EK Column Name Size */

/*        ekcnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of column name, in characters. */


/*     End Include Section:  EK Column Name Size */

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


/*     Include Section:  EK Column Descriptor Parameters */

/*        ekcoldsc.inc Version 6    23-AUG-1995 (NJB) */


/*     Note:  The column descriptor size parameter CDSCSZ  is */
/*     declared separately in the include section CDSIZE$INC.FOR. */

/*     Offset of column descriptors, relative to start of segment */
/*     integer address range.  This number, when added to the last */
/*     integer address preceding the segment, yields the DAS integer */
/*     base address of the first column descriptor.  Currently, this */
/*     offset is exactly the size of a segment descriptor.  The */
/*     parameter SDSCSZ, which defines the size of a segment descriptor, */
/*     is declared in the include file eksegdsc.inc. */


/*     Size of column descriptor */


/*     Indices of various pieces of column descriptors: */


/*     CLSIDX is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     TYPIDX is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     LENIDX is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     SIZIDX is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     NAMIDX is the index of the base address of the column's name. */


/*     IXTIDX is the data type of the column's index.  IXTIDX */
/*     contains a type value only if the column is indexed. For columns */
/*     that are not indexed, the location IXTIDX contains the boolean */
/*     value IFALSE. */


/*     IXPIDX is a pointer to the column's index.  IXTPDX contains a */
/*     meaningful value only if the column is indexed.  The */
/*     interpretation of the pointer depends on the data type of the */
/*     index. */


/*     NFLIDX is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     ORDIDX is the index of the column's ordinal position in the */
/*     list of columns belonging to the column's parent segment. */


/*     METIDX is the index of the column's integer metadata pointer. */
/*     This pointer is a DAS integer address. */


/*     The last position in the column descriptor is reserved.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Column Descriptor Parameters */

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


/*     Include Section:  EK Join Row Set Parameters */

/*        ekjrs.inc  Version 1    07-FEB-1995 (NJB) */


/*     Maximum number of join row sets in a join row set union: */


/*     The layout of a join row set in the EK scratch area is shown */
/*     below: */

/*        +--------------------------------------------+ */
/*        |              join row set size             |  1 element */
/*        +--------------------------------------------+ */
/*        |    number of row vectors in join row set   |  1 element */
/*        +--------------------------------------------+ */
/*        |               table count (TC)             |  1 element */
/*        +--------------------------------------------+ */
/*        |          segment vector count (SVC)        |  1 element */
/*        +--------------------------------------------+ */
/*        |               segment vector 1             |  TC elements */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |               segment vector SVC           |  TC elements */
/*        +--------------------------------------------+ */
/*        |   segment vector 1 row set base address    |  1 element */
/*        +--------------------------------------------+ */
/*        |      segment vector 1 row count (RC_1)     |  1 element */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |  segment vector SVC row set base address   |  1 element */
/*        +--------------------------------------------+ */
/*        |   segment vector SVC row count (RC_SVC)    |  1 element */
/*        +--------------------------------------------+ */
/*        | Augmented row vectors for segment vector 1 |  (TC+1)*RC_1 */
/*        +--------------------------------------------+  elements */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |Augmented row vectors for segment vector SVC|  (TC+1)*RC_SVC1 */
/*        +--------------------------------------------+  elements */


/*     The following parameters indicate positions of elements in the */
/*     join row set structure: */


/*     Base-relative index of join row set size */


/*     Index of row vector count */


/*     Index of table count */


/*     Index of segment vector count */


/*     Base address of first segment vector */



/*     End Include Section:  EK Join Row Set Parameters */

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


/*     Include Section:  EK Operator Codes */

/*        ekopcd.inc  Version 1  30-DEC-1994 (NJB) */


/*     Within the EK system, operators used in EK queries are */
/*     represented by integer codes.  The codes and their meanings are */
/*     listed below. */

/*     Relational expressions in EK queries have the form */

/*        <column name> <operator> <value> */

/*     For columns containing numeric values, the operators */

/*        EQ,  GE,  GT,  LE,  LT,  NE */

/*     may be used; these operators have the same meanings as their */
/*     Fortran counterparts.  For columns containing character values, */
/*     the list of allowed operators includes those in the above list, */
/*     and in addition includes the operators */

/*        LIKE,  UNLIKE */

/*     which are used to compare strings to a template.  In the character */
/*     case, the meanings of the parameters */

/*        GE,  GT,  LE,  LT */

/*     match those of the Fortran lexical functions */

/*        LGE, LGT, LLE, LLT */


/*     The additional unary operators */

/*        ISNULL, NOTNUL */

/*     are used to test whether a value of any type is null. */



/*     End Include Section:  EK Operator Codes */

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


/*     Include Section:  EK Query Limit Parameters */

/*        ekqlimit.inc  Version 3    16-NOV-1995 (NJB) */

/*           Parameter MAXCON increased to 1000. */

/*        ekqlimit.inc  Version 2    01-AUG-1995 (NJB) */

/*           Updated to support SELECT clause. */


/*        ekqlimit.inc  Version 1    07-FEB-1995 (NJB) */


/*     These limits apply to character string queries input to the */
/*     EK scanner.  This limits are part of the EK system's user */
/*     interface:  the values should be advertised in the EK required */
/*     reading document. */


/*     Maximum length of an input query:  MAXQRY.  This value is */
/*     currently set to twenty-five 80-character lines. */


/*     Maximum number of columns that may be listed in the */
/*     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50. */


/*     Maximum number of tables that may be listed in the `FROM */
/*     clause' of a query: MAXTAB. */


/*     Maximum number of relational expressions that may be listed */
/*     in the `constraint clause' of a query: MAXCON. */

/*     This limit applies to a query when it is represented in */
/*     `normalized form': that is, the constraints have been */
/*     expressed as a disjunction of conjunctions of relational */
/*     expressions. The number of relational expressions in a query */
/*     that has been expanded in this fashion may be greater than */
/*     the number of relations in the query as orginally written. */
/*     For example, the expression */

/*             ( ( A LT 1 ) OR ( B GT 2 ) ) */
/*        AND */
/*             ( ( C NE 3 ) OR ( D EQ 4 ) ) */

/*     which contains 4 relational expressions, expands to the */
/*     equivalent normalized constraint */

/*             (  ( A LT 1 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( A LT 1 ) AND ( D EQ 4 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( D EQ 4 )  ) */

/*     which contains eight relational expressions. */



/*     MXJOIN is the maximum number of tables that can be joined. */


/*     MXJCON is the maximum number of join constraints allowed. */


/*     Maximum number of order-by columns that may be used in the */
/*     `order-by clause' of a query: MAXORD. MAXORD = 10. */


/*     Maximum number of tokens in a query: 500. Tokens are reserved */
/*     words, column names, parentheses, and values. Literal strings */
/*     and time values count as single tokens. */


/*     Maximum number of numeric tokens in a query: */


/*     Maximum total length of character tokens in a query: */


/*     Maximum length of literal string values allowed in queries: */
/*     MAXSTR. */


/*     End Include Section:  EK Query Limit Parameters */

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


/*     Include Section:  EK Encoded Query Internal Parameters */

/*        ekquery.inc  Version 3    16-NOV-1995 (NJB) */

/*           Updated to reflect increased value of MAXCON in */
/*           ekqlimit.inc. */

/*        ekquery.inc  Version 2    03-AUG-1995 (NJB) */

/*           Updated to support representation of the SELECT clause. */


/*        ekquery.inc  Version 1    12-JAN-1995 (NJB) */


/*     An encoded EK query is an abstract data type implemented */
/*     as an integer cell, along with a double precision cell and */
/*     a character string.  The d.p. cell and string contain numeric */
/*     and string values from the query string represented by the */
/*     encoded query. */

/*     The parameters in this file are intended for use only by the */
/*     EK encoded query access routines.  Callers of EK routines should */
/*     not use these parameters. */

/*     The following parameters are indices of specified elements */
/*     in the integer portion of the encoded query. */

/*     Encoded query architecture type: */


/*     `Name resolution' consists of: */

/*        - Verifying existence of tables:  any table names listed */
/*          in the FROM clause of a query must be loaded. */

/*        - Validating table aliases used to qualify column names. */

/*        - Verifying existence of columns and obtaining data types */
/*          for columns. */

/*        - Setting data type codes for literal values in the encoded */
/*          query. */

/*        - Checking consistency of operators and operand data types. */

/*        - Making sure unqualified column names are unambiguous. */

/*        - For constraints, mapping the table names used to qualify */
/*          column names to the ordinal position in the FROM clause */
/*          of the corresponding table. */


/*     Initialization status---this flag indicates whether the encoded */
/*     query has been initialized.  Values are ITRUE or IFALSE.  See the */
/*     include file ekbool.inc for parameter values. */


/*     Parse status---this flag indicates whether the parsing operation */
/*     that produced an encoded query has been completed. Values are */
/*     ITRUE or IFALSE. */


/*     Name resolution status---this flag indicates whether names */
/*     have been resolved in an encoded query.  Values are ITRUE or */
/*     IFALSE. */


/*     Time resolution status---this flag indicates whether time values */
/*     have been resolved in an encoded query.  Time resolution */
/*     consists of converting strings representing time values to ET. */
/*     Values of the status are ITRUE or IFALSE. */


/*     Semantic check status---this flag indicates whether semantic */
/*     checking of constraints has been performed. */


/*     Number of tables specified in FROM clause: */


/*     Number of constraints in query: */


/*     A special value is used to indicate the `maximal' constraint--- */
/*     one that logically cannot be satisfied.  If the constraints */
/*     are equivalent to the maximal constraint, the location EQNCNS */
/*     is assigned the value EQMXML */


/*     Number of constraint conjunctions: */


/*     Number of order-by columns: */


/*     Number of SELECT columns: */


/*     Size of double precision buffer: */


/*     `Free' pointer into double precision buffer: */


/*     Size of character string buffer: */


/*     `Free' pointer into character string buffer: */


/*     The following four base pointers will be valid after a query */
/*     has been parsed: */

/*     Base pointer for SELECT column descriptors: */


/*     Base pointer for constraint descriptors: */


/*     Base pointer for conjunction sizes: */


/*     Base pointer for order-by column descriptors: */


/*     After the quantities named above, the integer array contains */
/*     series of descriptors for tables, constraints, and order-by */
/*     columns, as well as a list of `conjunction sizes'---that is, */
/*     the sizes of the groups of constraints that form conjunctions, */
/*     after the input query has been re-arranged as a disjunction of */
/*     conjunctions of constraints. */


/*     The offsets of specific elements within descriptors are */
/*     parameterized. The base addresses of the descriptors themselves */
/*     must be  calculated using the counts and sizes of the items */
/*     preceding them. */

/*     A diagram of the structure of the variable-size portion of the */
/*     integer array is shown below: */


/*        +-------------------------------------+ */
/*        | Fixed-size portion of encoded query | */
/*        +-------------------------------------+ */
/*        |         Encoded FROM clause         | */
/*        +-------------------------------------+ */
/*        |      Encoded constraint clause      | */
/*        +-------------------------------------+ */
/*        |          Conjunction sizes          | */
/*        +-------------------------------------+ */
/*        |       Encoded ORDER BY clause       | */
/*        +-------------------------------------+ */
/*        |        Encoded SELECT clause        | */
/*        +-------------------------------------+ */


/*     Value Descriptors */
/*     ---------------- */

/*     In order to discuss the various descriptors below, we'll make use */
/*     of sub-structures called `value descriptors'.  These descriptors */
/*     come in two flavors:  character and double precision.  For */
/*     strings, a descriptor is a set of begin and end pointers that */
/*     indicate the location of the string in the character portion of an */
/*     encoded query, along with the begin and end pointers for the */
/*     corresponding lexeme in the original query.  The pointers are set */
/*     to zero when they are not in use, for example if they refer to an */
/*     optional lexeme that did not appear in the input query. */

/*     All value descriptors start with a data type indicator; values */
/*     are from ektype.inc.  Integer and time values are referred to */
/*     by double precision descriptors. */

/*     Parameters for string value descriptor elements: */


/*     Numeric value descriptors are similar to those for string values, */
/*     the difference being that they have only one pointer to the value */
/*     they represent.  This pointer is the index of the value in the */
/*     encoded query's numeric buffer. */


/*     All value descriptors have the same size.  In order to allow */
/*     table descriptors to have the same size as value descriptors, */
/*     we include an extra element in the descriptor. */


/*     Column Descriptors */
/*     ----------------- */

/*     Each column descriptor consists of a character descriptor for the */
/*     name of the column, followed by an index, which gives the ordinal */
/*     position of the column in the logical table to which the column */
/*     belongs.  The index element is filled in during name resolution. */


/*     Table Descriptors */
/*     ----------------- */

/*     Each table descriptor consists of a character descriptor for the */
/*     name of the table, followed by an index, which gives the ordinal */
/*     position of the table in the FROM clause in the original query. */
/*     The index element is filled in during name resolution.  Aliases */
/*     and table names have identical descriptor structures. */


/*     Constraint descriptors */
/*     ------------------ */

/*     Each constraint is characterized by: */

/*        - A code indicating whether the constraint compares values */
/*          in two columns or the value in a column and a literal */
/*          value.  The values of this element are EQCOL and EQVAL. */



/*        - A descriptor for the table used to qualify the */
/*          column name on the left side of the constraint. */


/*        - A character value descriptor for the column name on the left */
/*          side of the query. */


/*        - An operator code indicating the relational operator used */
/*          in the constraint. */


/*        If the constraint compares values from two columns, the */
/*        next items are table and column name descriptors that apply to */
/*        the column named on the right side of the relational operator. */


/*        If the constraint has a literal value on the right side, the */
/*        operator code is followed by... */

/*        - a value descriptor. */


/*        - Size of a constraint descriptor: */


/*     Conjunction sizes */
/*     ----------------- */

/*     The size of each conjunction of constraints occupies a single */
/*     integer. */




/*     Order-by Column Descriptors */
/*     --------------------------- */

/*     Each order-by column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself; one additional element is used to indicate the direction */
/*     of the ordering (ascending vs descending). */


/*        - The last integer in the descriptor indicates whether the */
/*          order direction is ascending or descending. */


/*        - Size of an order-by column descriptor: */


/*     Codes indicating sense of ordering (ascending vs descending): */


/*     SELECT Column Descriptors */
/*     --------------------------- */

/*     Each SELECT column descriptor contains descriptors for */
/*     the table containing the column and for the name of the column */
/*     itself. */


/*        - Size of a SELECT column descriptor: */


/*     Miscellaneous parameters: */


/*     EQIMIN is the minimum size of the integer portion of */
/*     an encoded query.  EQIMIN depends on the parameters */

/*        MAXTAB */
/*        MAXCON */
/*        MAXORD */
/*        MAXSEL */

/*     all of which are declared in the include file ekqlimit.inc. */
/*     The functional definition of EQIMIN is: */

/*     INTEGER               EQIMIN */
/*     PARAMETER           ( EQIMIN =   EQVBAS */
/*    .                              +  MAXTAB * EQVDSZ * 2 */
/*    .                              +  MAXCON * EQCDSZ */
/*    .                              +  MAXCON */
/*    .                              +  MAXORD * EQODSZ */
/*    .                              +  MAXSEL * EQSDSZ     ) */


/*     End Include Section:  EK Encoded Query Internal Parameters */

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


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */

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


/*     Include Section:  EK Table Name Size */

/*        ektnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of table name, in characters. */


/*     End Include Section:  EK Table Name Size */

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


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */

/* $ Brief_I/O */

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     CINDEX     I   EKCII */
/*     ELMENT     I   EKGC, EKGD, EKGI */
/*     EQRYC      I   EKSRCH */
/*     EQRYD      I   EKSRCH */
/*     EQRYI      I   EKSRCH */
/*     FNAME      I   EKLEF */
/*     ROW        I   EKGC, EKGD, EKGI, EKNELT */
/*     SELIDX     I   EKGC, EKGD, EKGI, EKNELT */
/*     COLUMN    I-O  EKCIN, EKGC, EKGD, EKGI, EKNELT, EKCII */
/*     HANDLE    I-O  EKLEF, EKUEF */
/*     N         I-O  EKTNAM, EKNTAB */
/*     TABLE     I-O  EKCCNT, EKCII, EKTNAM */
/*     ATTDSC     O   EKCII, EKCIN */
/*     CCOUNT     O   EKCCNT */
/*     FOUND      O   EKCIN, EKGC, EKGD, EKGI */
/*     NELT       O   EKNELT */
/*     NMROWS     O   EKSRCH */
/*     SEMERR     O   EKSRCH */
/*     ERRMSG     O   EKSRCH */
/*     CDATA      O   EKGC */
/*     DDATA      O   EKGD */
/*     IDATA      O   EKGI */
/*     NULL       O   EKGC, EKGD, EKGI */
/*     FTSIZE     P   All */
/*     MAXCON     P   All */
/*     MXCLLD     P   All */
/*     STSIZE     P   All */
/*     MAXORD     P   All */
/*     CNAMSZ     P   All */
/*     ITSIZE     P   All */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their outputs. */

/* $ Parameters */

/*     FTSIZE         is the maximum number of EK files that may be */
/*                    loaded.  Any other DAS files loaded by the calling */
/*                    program count against this limit. */

/*     STSIZE         is the size of the segment table; this is the */
/*                    maximum number of segments that can be loaded at */
/*                    one time. */

/*     MXTBLD         is the maximum number of tables that can be loaded */
/*                    at any time.  A table can consist of multiple */
/*                    segments. */

/*     MXCLLD         is the maximum number of columns that can be loaded */
/*                    at any time.  A column may be spread across */
/*                    multiple segments; in this case, the portions of */
/*                    the column contained in each segment count against */
/*                    this limit. */

/*     ADSCSZ         is the size of column attribute descriptor. */
/*                    (Defined in ekattdsc.inc.) */

/*     LBCELL         is the SPICELIB cell lower bound. */

/*     Many other parameters are defined in the include files referenced */
/*     above.  See those files for details. */


/* $ Exceptions */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/*     See the headers of the entry points for descriptions of exceptions */
/*     specific to those routines. */

/* $ Files */

/*     This suite of routines reads binary `sequence component' EK files. */
/*     In order for a binary EK file to be accessible to this routine, */
/*     the file must be `loaded' via a call to the entry point EKLEF. */

/*     Text format EK files cannot be used by this routine; they must */
/*     first be converted by binary format by the NAIF Toolkit utility */
/*     SPACIT. */

/* $ Particulars */

/*     EKQMGR is an umbrella routine for its entry points:  all variables */
/*     used by the entry points are declared here. */

/*     EKQMGR supports loading and unloading EK files, executing queries, */
/*     and fetching the results of executed queries.  The entry points */
/*     and their functions are: */

/*        File loading and unloading: */

/*           EKLEF  ( EK, load event file   ) */
/*           EKUEF  ( EK, unload event file ) */

/*        Query execution: */

/*           EKSRCH ( EK, search for events ) */

/*        Fetching query results: */

/*           EKGC   ( EK, get event data, character        ) */
/*           EKGD   ( EK, get event data, double precision ) */
/*           EKGI   ( EK, get event data, integer          ) */

/*        Utilities: */

/*           EKNTAB ( EK, return the number of loaded tables        ) */
/*           EKTNAM ( EK, return the names of loaded tables         ) */
/*           EKCCNT ( EK, return the column count of a table        ) */
/*           EKCII  ( EK, look up column info by index              ) */
/*           EKNELT ( EK, return number of elements in column entry ) */


/*     To issue queries to the EK system, users would normally call the */
/*     high-level interface routine EKFIND.  EKFIND parses queries and */
/*     converts them to the encoded form expected by EKSRCH.  It is */
/*     possible to call EKSRCH directly, but this should not be attempted */
/*     by others than EK masters.  EKFIND is not an entry point of */
/*     EKQMGR, but instead is a separate subroutine. */

/* $ Examples */

/*     1)  Query the EK system and fetch data matching queries. */
/*         The code fragment shown here does not rely on advance */
/*         knowledge of the input query or the contents of any loaded EK */
/*         files. */

/*         To simplify the example, we assume that all data are scalar. */
/*         This assumption relieves us of the need to test the size of */
/*         column entries before fetching them.  In the event that a */
/*         column contains variable-size array entries, the entry point */
/*         EKNELT may be called to obtain the size of column entries to */
/*         be fetched.  See EKNELT for an example. */


/*            C */
/*            C     Load EK file.  Also load leapseconds file for */
/*            C     time conversion. */
/*            C */
/*                  CALL EKLEF  ( EK, HANDLE ) */
/*                  CALL FURNSH ( LEAP       ) */

/*            C */
/*            C     Prompt for query.  Parse the SELECT clause using */
/*            C     EKPSEL. */
/*            C */
/*                  CALL PROMPT ( 'Enter query > ', QUERY ) */

/*                  CALL EKPSEL ( QUERY, */
/*                                N, */
/*                                XBEGS, */
/*                                XENDS, */
/*                                XBEGS, */
/*                                XTYPES, */
/*                                XCLASS, */
/*                                TABS, */
/*                                COLS, */
/*                                ERROR, */
/*                                ERRMSG ) */


/*                  IF ( ERROR ) THEN */

/*                     WRITE (*,*) ERRMSG */

/*                  ELSE */
/*            C */
/*            C        Submit query to the EK query system. */
/*            C */
/*                     CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*                     IF ( ERROR ) THEN */

/*                        WRITE (*,*) ERRMSG */

/*                     ELSE */
/*            C */
/*            C           Fetch the rows that matched the query. */
/*            C */
/*                        DO ROW = 1, NMROWS */
/*            C */
/*            C              Fetch data from the Ith row. */
/*            C */
/*                           WRITE (*,*) ' ' */
/*                           WRITE (*,*) 'ROW = ', ROW */

/*                           DO COL = 1, N */
/*            C */
/*            C                 Fetch the data from the Jth selected */
/*            C                 column. */
/*            C */
/*                              IF ( XCLASS(COL) .EQ. 'COL' ) THEN */

/*                                 OUTSTR  =  COLS(COL) */
/*                                 CALL PREFIX ( '.',       0, OUTSTR ) */
/*                                 CALL PREFIX ( TABS(COL), 0, OUTSTR ) */
/*                                 WRITE (*,*) 'COLUMN = ', OUTSTR */

/*                              ELSE */

/*                                 B  =  XBEGS(COL) */
/*                                 E  =  XENDS(COL) */
/*                                 WRITE (*,*) 'ITEM = ', QUERY(B:E) */

/*                              END IF */

/*                              IF ( XTYPES(COL) .EQ. 'CHR' ) THEN */

/*                                 CALL EKGC ( COL,   ROW,   1, */
/*                 .                           CDATA, NULL,  FOUND ) */

/*                                 IF ( NULL ) THEN */
/*                                    WRITE (*,*) '<Null>' */
/*                                 ELSE */
/*                                    WRITE (*,*) CDATA */
/*                                 END IF */


/*                              ELSE IF ( XTYPES(COL) .EQ. 'DP' ) THEN */

/*                                 CALL EKGD ( COL,   ROW,   1, */
/*                 .                           DDATA, NULL,  FOUND ) */

/*                                 IF ( NULL ) THEN */
/*                                    WRITE (*,*) '<Null>' */
/*                                 ELSE */
/*                                    WRITE (*,*) DDATA */
/*                                 END IF */


/*                              ELSE IF ( XTYPES(COL) .EQ. 'INT' ) THEN */

/*                                 CALL EKGI ( COL,   ROW,   1, */
/*                 .                           IDATA, NULL,  FOUND ) */

/*                                 IF ( NULL ) THEN */
/*                                    WRITE (*,*) '<Null>' */
/*                                 ELSE */
/*                                    WRITE (*,*) IDATA */
/*                                 END IF */


/*                              ELSE */
/*            C */
/*            C                    The item is a time value.  Convert it */
/*            C                    to UTC for output. */
/*            C */
/*                                 CALL EKGD   ( COL,   ROW,   1, */
/*                 .                             TDATA, NULL,  FOUND ) */

/*                                 IF ( NULL ) THEN */
/*                                    WRITE (*,*) '<Null>' */
/*                                 ELSE */
/*                                    CALL ET2UTC ( TDATA, 'C', 3, UTC ) */
/*                                    WRITE (*,*) UTC */
/*                                 END IF */

/*                              END IF */

/*                           END DO */
/*            C */
/*            C              We're done with the column having index COL. */
/*            C */
/*                        END DO */
/*            C */
/*            C           We're done with the row having index ROW. */
/*            C */
/*                     END IF */
/*            C */
/*            C        We either processed the query or had an error. */
/*            C */
/*                  END IF */
/*            C */
/*            C     We either parsed the SELECT clause or had an error. */
/*            C */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.3, 10-FEB-2014 (BVS) */

/*        Added descriptions of ADSCSZ and LBCELL to the Parameters */
/*        section of the header. */

/* -    SPICELIB Version 2.0.2, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 2.0.1, 22-SEP-2004 (EDW) */

/*        Removed from the header descriptions, all occurrences of the */
/*        token used to mark the $Procedure section. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.3.0, 12-FEB-1999 (NJB) */

/*        Bug fix:  in entry point EKNELT, there was a error handling */
/*        branch that called CHKOUT where CHKIN should have been called. */
/*        This has been fixed. */

/* -    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB) */

/*        In the entry point EKSRCH, a ZZEKJSQZ call was added after */
/*        the ZZEKJOIN call.  This change reduces the scratch area usage */
/*        for intermediate results of joins.  It also prevents ZZEKJOIN */
/*        from being handed a join row set containing a segment vector */
/*        having no corresponding row vectors. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Code fixes were made in routines */

/*           EKNELT, EKGC, EKGD, EKGI */

/*        Version lines were fixed in all routines:  versions were */
/*        changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Manage EK query operations */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.3.0, 12-FEB-1999 (NJB) */

/*        Bug fix:  in entry point EKNELT, there was a error handling */
/*        branch that called CHKOUT where CHKIN should have been called. */
/*        This has been fixed. */

/* -    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB) */

/*        In the entry point EKSRCH, a ZZEKJSQZ call was added after */
/*        the ZZEKJOIN call.  This change reduces the scratch area usage */
/*        for intermediate results of joins.  It also prevents ZZEKJOIN */
/*        from being handed a join row set containing a segment vector */
/*        having no corresponding row vectors. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Code fixes were made in routines */

/*           EKNELT, EKGC, EKGD, EKGI */

/*        Version lines were fixed in all routines:  versions were */
/*        changed from "Beta" to "SPICELIB." */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Linked list functions: */

/*        Find next node */
/*        Find tail of list */
/*        Return number of free nodes */



/*     Local parameters */


/*     Maximum number of constraints allowed in a single query: */


/*     Miscellaneous parameters */


/*     Number of data types */


/*     Length of strings used for data type names. */


/*     Chunk size for buffered DAS integer reads. */


/*     Length of status strings. */


/*     Local variables */


/*     As do the CK and SPK `BSR' entry points, the EKQMGR entry points */
/*     make use of an amusing panoply of tables, linked lists, and */
/*     pointers.  Here's where they're declared and described. */


/*     The file table contains a list of handles of loaded EK files. */
/*     Entries in the table are organized as a doubly linked list. */
/*     Names of file table variables begin with the string 'FT'. */

/*        The maximum number of EK files that can be loaded is FTSIZE. */

/*        The linked list pool used to index table entries is called */
/*        FTPOOL. */

/*        FTHAN is an array containing file handles of loaded EKs. */

/*        FTHEAD is the head node of the file list. */


/*     The table list contains table names, segment table pointers, */
/*     and column table pointers for every table associated with a */
/*     loaded segment.  The segment table pointers indicate the head node */
/*     of the segment list for each table.  The column table pointers */
/*     indicate the column names and attributes associated with each */
/*     table. */

/*     The entries of the table list are organized as a doubly linked */
/*     list.  All variables in the table list have names starting with */
/*     the string 'TB'. */

/*        MXTBLD is the maximum number of tables that can be */
/*        accommodated by the table list. */

/*        TBPOOL is the doubly linked list pool used to index the */
/*        table list. */

/*        TBNAMS is an array of table names. */

/*        TBSTPT is an array containing pointers to the heads of segment */
/*        lists corresponding to segments belonging to the table. */

/*        TBNCOL is the number of columns in each table. */

/*        TBCTPT is an array of pointers to lists of column table */
/*        entries giving the names and attributes of the columns in each */
/*        table. */

/*        TBFILS is an array containing, for each table, handles of the */
/*        files that contain segments belonging to that table. */

/*        TBFLSZ is an array of sizes of handle lists for each table */
/*        entry. */

/*        TBHEAD is the head node of the table list. */




/*     The segment table contains descriptive information for each */
/*     loaded segment.  Entries in the table are indexed by a linked */
/*     list pool containing a doubly linked list for each system (or */
/*     instrument) for which segments are loaded. */

/*     Names of segment table variables begin with the string 'ST'. */

/*        The maximum number of segments that can be loaded is MAXSEG. */
/*        Currently, the value of MAXSEG is just the size of the segment */
/*        table, STSIZE. */

/*        The linked list pool used to index segment table entries is */
/*        called STPOOL. */

/*        For each loaded segment, the following information is stored: */

/*           -- The file handle of the EK containing the segment. */

/*           -- The index of the segment within the EK that contains it. */
/*              Indices start at 1 and end with the segment count for the */
/*              EK file. */

/*           -- The segment descriptor. */

/*           -- The number of rows in the segment. */

/*           -- The number of columns in the segment. */

/*           -- A pointer to a list of column descriptors.  The */
/*              column descriptor table contains a complete descriptor */
/*              for every loaded column. */




/*     The column descriptor table contains a column descriptor for */
/*     every loaded column.  This table allows segments to share the */
/*     area used for buffering descriptors, making it reasonable for */
/*     the buffer space to have room for fewer than */

/*        MXCLLD * MAXSEG */

/*     column descriptors. */

/*     The space in the table is organized as a doubly linked list. */


/*     The column attribute table contains attribute information for */
/*     every column in every loaded segment.  There is one entry per */
/*     column name; columns with the same names and different data */
/*     types may not be loaded simultaneously. */

/*     The entries of the column table are organized as a doubly linked */
/*     list.  All variables in the column table have names starting with */
/*     the string 'CT'. */

/*        CTSIZE is the maximum number of distinct column declarations */
/*        that can be accommodated by the column table. */

/*        CTPOOL is the doubly linked list pool used to index the column */
/*        table. */

/*        CTNAMS is an array containing column names. */

/*        CTCLAS is an array containing column class specifiers. */

/*        CTTYPS is an array containing column data types. */

/*        CTLENS is an array containing column string length specifiers. */

/*        CTFIXD is an array of logical flags indicating whether the */
/*        columns they correspond to have fixed size. */

/*        CTSIZS is an array of integers indicating the number of array */
/*        elements per column entry, for fixed-size columns. */

/*        CTINDX is an array of logical flags that indicate whether the */
/*        columns they correspond to are indexed. */

/*        CTNULL is an array of logical flags that indicate whether the */
/*        columns they correspond to may contain null values. */




/*     Other local variables */



/*     Saved variables */



/*     Initial values */

    /* Parameter adjustments */
    if (eqryd) {
	}
    if (eqryi) {
	}
    if (attdsc) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_eklef;
	case 2: goto L_ekuef;
	case 3: goto L_ekntab;
	case 4: goto L_ektnam;
	case 5: goto L_ekccnt;
	case 6: goto L_ekcii;
	case 7: goto L_eksrch;
	case 8: goto L_eknelt;
	case 9: goto L_ekgc;
	case 10: goto L_ekgd;
	case 11: goto L_ekgi;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKQMGR", (ftnlen)6);
    }

/*     Never come here. */

    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("EKQMGR", (ftnlen)6);
    return 0;
/* $Procedure     EKLEF  ( EK, load event file ) */

L_eklef:
/* $ Abstract */

/*     Load an EK file, making it accessible to the EK readers. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */
/*     SEARCH */

/* $ Declarations */

/*     CHARACTER*(*)         FNAME */
/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of EK file to load. */
/*     HANDLE     O   File handle of loaded EK file. */

/* $ Detailed_Input */

/*     FNAME          is the name of a binary EK file to be loaded. */

/* $ Detailed_Output */

/*     HANDLE         is the handle of the EK file.  The file is */
/*                    accessible by the EK reader routines once it */
/*                    has been loaded. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the EK file indicated by FNAME contains a column whose */
/*         name matches that of a column in an already loaded EK, but */
/*         whose declared attributes don't match those of the loaded */
/*         column of the same name, the error SPICE(BADATTRIBUTES) is */
/*         signaled.  HANDLE is is undefined in this case. */

/*     2)  Loading an EK file that is already loaded does not cause side */
/*         effects.  The handle already associated with the file will be */
/*         returned. */

/*     3)  If a file open error occurs, the problem will be diagnosed by */
/*         routines called by this routine.  HANDLE is undefined in */
/*         this case. */

/*     4)  If loading the input file would cause the maximum number of */
/*         loaded EK files to be exceeded, the error */
/*         SPICE(EKFILETABLEFULL) will be signaled.  HANDLE is */
/*         undefined in this case.  This routine will attempt to */
/*         unload the file from the DAS system. */

/*     5)  If loading the input file would cause the maximum number of */
/*         loaded DAS files to be exceeded, the error will be diagnosed */
/*         by routines called by this routine.  HANDLE is undefined in */
/*         this case.  This routine will attempt to unload the file */
/*         from the DAS system. */

/*     6)  If loading the input file would cause the maximum number of */
/*         segments allowed in loaded EK files to be exceeded, the error */
/*         SPICE(EKSEGMENTTABLEFULL) will be signaled.  HANDLE is */
/*         is undefined in this case.  This routine will attempt to */
/*         unload the file from the DAS system. */

/*     7)  If loading the input file would cause the maximum number of */
/*         columns allowed in loaded EK files to be exceeded, the error */
/*         SPICE(EKCOLDESCTABLEFULL) will be signaled.  HANDLE is */
/*         is undefined in this case.  This routine will attempt to */
/*         unload the file from the DAS system. */

/*     8)  If loading the input file would cause the maximum allowed */
/*         number of columns having distinct attributes in loaded EK */
/*         files to be exceeded, the error SPICE(EKCOLATTRTABLEFULL) will */
/*         be signaled.  HANDLE is is undefined in this case.  This */
/*         routine will attempt to unload the file from the DAS system. */

/*     9)  If loading the input file would cause the maximum number of */
/*         instrument codes allowed in loaded EK files to be exceeded, */
/*         the error SPICE(EKIDTABLEFULL) will be signaled.  HANDLE is */
/*         is undefined in this case.  This routine will attempt to */
/*         unload the file from the DAS system. */

/*     10) If the input file does not contain at least one segment, the */
/*         error SPICE(EKNOSEGMENTS) will be signaled. */

/* $ Files */

/*     See description of FNAME in $Detailed_Input. */

/* $ Particulars */

/*     This routine makes EK files known to the EK system.  It is */
/*     necessary to load EK files using this routine in order to */
/*     query the files using the EK readers. */

/* $ Examples */

/*     1)  Load three EK files.  During query execution, all files */
/*         will be searched. */

/*            DO I = 1, 3 */
/*               CALL EKLEF ( EK(I), HANDLE ) */
/*            END DO */

/*            [Perform queries] */


/*     2)  Load 25 EK files sequentially, unloading the previous file */
/*         before each new file is loaded.  Unloading files prevents */
/*         them from being searched during query execution. */

/*            DO I = 1, 25 */

/*               CALL EKLEF ( EK(I), HANDLE ) */

/*               [Perform queries] */

/*               CALL EKUEF ( HANDLE ) */

/*            END DO */

/* $ Restrictions */

/*     1)  EK files containing columns having the same name but */
/*         inconsistent declarations are not diagnosed.  Such kernels */
/*         are invalid in any case. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Previous version line was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     load EK file */
/*     load E-Kernel */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKLEF", (ftnlen)5);
    }

/*     Here's a brief overview of what follows: */

/*        -- We do some once-per-program run initializations. */

/*        -- We do some simple error checks.  We need to make sure */
/*           that DAS can load the file, and that the EK architecture is */
/*           the right kind. */

/*        -- We need to make sure that there's enough space in our */
/*           data structures to hold the information about the new */
/*           EK.  Some of these checks are simple; we do these first. */
/*           However, checking that we have enough room in the column */
/*           table is best done by simply loading the column data into */
/*           the table.  If we run out of room, we abort the load. */

/*        -- We also need to make sure that the column attributes for */
/*           any two columns with the same name in the same table are */
/*           identical.  This is easy to do if the attributes for every */
/*           column we've encountered have been loaded into the column */
/*           table. */

/*        -- We save the table name and column names and attributes for */
/*           each new table we encounter.  For each table, we maintain a */
/*           list of handles of files that contain segments in that */
/*           table. */

/*        -- We make a segment table entry for each segment we find. */

/*        -- We save the column descriptor for each column we find, */
/*           associating it with the segment table entry for the segment */
/*           containing the column.  The column descriptor entries are */
/*           linked together in the same order that the corresponding */
/*           column names appear in the parent table's column name list; */
/*           this order is not necessarily the order that the columns */
/*           have within the segment. */

/*        -- We maintain a list of handles of loaded EKs. */

/*        If we run out of room in the column table, we clean up our */
/*        mess.  This means removing the current file's contributions */
/*        to the column table, segment table, file table, and if */
/*        necessary, the table list. */


/*     On the first pass through this routine, initialize the tables, */
/*     if it hasn't been done yet. */

    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     Open the EK file for read access.  Bail out now if this doesn't */
/*     work.  This retreat will protect the various tables from */
/*     corruption. */

    ekopr_(fname, handle, fname_len);
    if (failed_()) {
	chkout_("EKLEF", (ftnlen)5);
	return 0;
    }

/*     Check to see whether the named EK has already been loaded. */
/*     If so, we've added another link to the EK, which must be */
/*     removed. */

    i__ = fthead;
    while(i__ > 0) {
	if (*handle == fthan[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("fthan", i__1, "ekqmgr_", (ftnlen)1211)]) {

/*           The last call we made to EKOPR added another link to */
/*           the EK file.  Remove this link. */

	    dascls_(handle);
	    chkout_("EKLEF", (ftnlen)5);
	    return 0;
	}
	i__ = lnknxt_(&i__, ftpool);
    }

/*     Nothing doing unless the architecture is correct.  This file */
/*     should be a paged DAS EK. */

    zzekpgch_(handle, "READ", (ftnlen)4);

/*     Before getting too involved with this new EK file, let's check it */
/*     out.  We must have enough room to accommodate it in the file */
/*     table, segment table, table list, and column table. */

/*     Make sure there's enough room in the file table. */

    if (lnknfn_(ftpool) == 0) {

/*        Sorry, there are no free file table entries left. */

	dashlu_(handle, &unit);
	ekcls_(handle);
	setmsg_("The EK file # could not be loaded; the maximum number of lo"
		"aded EKs has already been reached.", (ftnlen)93);
	errfnm_("#", &unit, (ftnlen)1);
	sigerr_("SPICE(EKFILETABLEFULL)", (ftnlen)22);
	chkout_("EKLEF", (ftnlen)5);
	return 0;
    }

/*     Find out how many segments are in the new kernel, and make */
/*     sure there's enough room in the segment table. */

    nseg = eknseg_(handle);
    if (nseg > lnknfn_(stpool)) {

/*        There are too many segments for the amount of space we've got */
/*        left. */

	dashlu_(handle, &unit);
	ekcls_(handle);
	setmsg_("The EK file # could not be loaded; the maximum number of lo"
		"aded segments has already been reached.", (ftnlen)98);
	errfnm_("#", &unit, (ftnlen)1);
	sigerr_("SPICE(EKSEGTABLEFULL)", (ftnlen)21);
	chkout_("EKLEF", (ftnlen)5);
	return 0;
    } else if (nseg < 1) {
	dashlu_(handle, &unit);
	ekcls_(handle);
	setmsg_("The EK file # contains no segments.", (ftnlen)35);
	errfnm_("#", &unit, (ftnlen)1);
	sigerr_("SPICE(EKNOSEGMENTS)", (ftnlen)19);
	chkout_("EKLEF", (ftnlen)5);
	return 0;
    }

/*     At this point, the file has insinuated itself into our confidence, */
/*     justified or not.  We'll attempt to load the segment and column */
/*     tables, and we'll update the table list if new tables are */
/*     introduced in this file. */

    seg = 1;
    s_copy(state, "LOAD_FILE_TABLE", (ftnlen)80, (ftnlen)15);
    while(s_cmp(state, "DONE", (ftnlen)80, (ftnlen)4) != 0) {
	if (s_cmp(state, "LOAD_FILE_TABLE", (ftnlen)80, (ftnlen)15) == 0) {

/*           Allocate a file table entry and link the new entry in before */
/*           the current head of the list.  Update the list head pointer. */
/*           Record the file handle in the new file table entry. */

	    lnkan_(ftpool, &new__);
	    lnkilb_(&new__, &fthead, ftpool);
	    fthead = new__;
	    fthan[(i__1 = new__ - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("fthan"
		    , i__1, "ekqmgr_", (ftnlen)1313)] = *handle;
	    s_copy(state, "SUMMARIZE_SEGMENT", (ftnlen)80, (ftnlen)17);
	} else if (s_cmp(state, "SUMMARIZE_SEGMENT", (ftnlen)80, (ftnlen)17) 
		== 0) {

/*           Get the summary information for this segment. */

	    zzeksinf_(handle, &seg, tabnam, segdsc, cnams, cdscrs, (ftnlen)64,
		     (ftnlen)32);
	    ncols = segdsc[4];

/*           Before going further, check the segment for duplicate */
/*           column names.  Bail out if we find any. */

	    ssizec_(&c__500, cnmset, (ftnlen)32);
	    movec_(cnams, &ncols, cnmset + 192, (ftnlen)32, (ftnlen)32);
	    validc_(&c__500, &ncols, cnmset, (ftnlen)32);
	    if (cardc_(cnmset, (ftnlen)32) < ncols) {
		s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
		s_copy(problm, "DUPLICATE_COLUMN_NAMES", (ftnlen)80, (ftnlen)
			22);
	    } else {
		s_copy(state, "FIND_TABLE", (ftnlen)80, (ftnlen)10);
	    }
	} else if (s_cmp(state, "FIND_TABLE", (ftnlen)80, (ftnlen)10) == 0) {

/*           Traverse the table list, checking for a match. */

	    tbcurr = tbhead;
	    presnt = FALSE_;
	    while(tbcurr > 0 && ! presnt) {
		if (s_cmp(tabnam, tbnams + (((i__1 = tbcurr - 1) < 100 && 0 <=
			 i__1 ? i__1 : s_rnge("tbnams", i__1, "ekqmgr_", (
			ftnlen)1358)) << 6), (ftnlen)64, (ftnlen)64) == 0) {
		    presnt = TRUE_;
		} else {
		    tbcurr = lnknxt_(&tbcurr, tbpool);
		}
	    }

/*           If TABNAM is the name of a table we know about, go on to */
/*           fill out the segment list entry for the current segment. */
/*           If we didn't find TABNAM, we have a new table.  Make a table */
/*           list entry for it. */

	    if (presnt) {

/*              Before going further, make sure the number of columns */
/*              in the segment matches the number of columns in the */
/*              parent table. */

		if (ncols != tbncol[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
			i__1 : s_rnge("tbncol", i__1, "ekqmgr_", (ftnlen)1378)
			]) {
		    npcol = tbncol[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
			    i__1 : s_rnge("tbncol", i__1, "ekqmgr_", (ftnlen)
			    1380)];
		    s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
		    s_copy(problm, "COLUMN_NUMBER_MISMATCH", (ftnlen)80, (
			    ftnlen)22);
		} else {

/*                 Add the current file to the list of files containing */
/*                 the current table. */

		    tbfils[(i__1 = tbcurr * 20 - 20) < 2000 && 0 <= i__1 ? 
			    i__1 : s_rnge("tbfils", i__1, "ekqmgr_", (ftnlen)
			    1389)] = *handle;
		    tbflsz[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : 
			    s_rnge("tbflsz", i__1, "ekqmgr_", (ftnlen)1390)] =
			     tbflsz[(i__2 = tbcurr - 1) < 100 && 0 <= i__2 ? 
			    i__2 : s_rnge("tbflsz", i__2, "ekqmgr_", (ftnlen)
			    1390)] + 1;
		    s_copy(state, "MAKE_SEGMENT_TABLE_ENTRY", (ftnlen)80, (
			    ftnlen)24);
		}
	    } else {

/*              This segment belongs to a new table. */

		s_copy(state, "MAKE_TABLE_LIST_ENTRY", (ftnlen)80, (ftnlen)21)
			;
	    }
	} else if (s_cmp(state, "MAKE_TABLE_LIST_ENTRY", (ftnlen)80, (ftnlen)
		21) == 0) {

/*           Allocate a table list entry, if we can. */

	    if (lnknfn_(tbpool) == 0) {

/*              Oops, we're out of room. */

		s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
		s_copy(problm, "TABLE_LIST_FULL", (ftnlen)80, (ftnlen)15);
	    } else {

/*              We have an entry; link it to the tail of the table list. */
/*              For consistency with the case in which the table entry */
/*              already exists, we'll call the table list node TBCURR. */

/*              If this is the first table in the table list, set the */
/*              table head pointer. */

		lnkan_(tbpool, &tbcurr);
		if (tbhead <= 0) {
		    tbhead = tbcurr;
		} else {
		    lnkilb_(&tbhead, &tbcurr, tbpool);
		}

/*              Fill in the table name. */

		s_copy(tbnams + (((i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
			i__1 : s_rnge("tbnams", i__1, "ekqmgr_", (ftnlen)1438)
			) << 6), tabnam, (ftnlen)64, (ftnlen)64);

/*              Since this table is new, the file list for this table */
/*              contains only the handle of the current EK. */

		tbfils[(i__1 = tbcurr * 20 - 20) < 2000 && 0 <= i__1 ? i__1 : 
			s_rnge("tbfils", i__1, "ekqmgr_", (ftnlen)1443)] = *
			handle;
		tbflsz[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"tbflsz", i__1, "ekqmgr_", (ftnlen)1444)] = 1;

/*              Initialize the column count, column table pointer, and */
/*              segment list pointer for this table. */

		tbncol[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"tbncol", i__1, "ekqmgr_", (ftnlen)1450)] = ncols;
		tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"tbctpt", i__1, "ekqmgr_", (ftnlen)1451)] = 0;
		tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"tbstpt", i__1, "ekqmgr_", (ftnlen)1452)] = 0;

/*              Go on to add a segment table entry for the current */
/*              segment. */

		s_copy(state, "MAKE_SEGMENT_TABLE_ENTRY", (ftnlen)80, (ftnlen)
			24);
	    }
	} else if (s_cmp(state, "MAKE_SEGMENT_TABLE_ENTRY", (ftnlen)80, (
		ftnlen)24) == 0) {

/*           Add the data for the current segment to the segment */
/*           table. */

/*           Allocate a segment table entry.  We've already verified */
/*           that there's enough room. */

	    lnkan_(stpool, &stnew);

/*           Link this segment table entry to the tail of the segment */
/*           list for the parent table, or, if the tail is NIL, just set */
/*           the segment list pointer to the current segment node. */

	    if (tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "tbstpt", i__1, "ekqmgr_", (ftnlen)1478)] <= 0) {
		tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
			"tbstpt", i__1, "ekqmgr_", (ftnlen)1480)] = stnew;
	    } else {
		lnkilb_(&tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 
			: s_rnge("tbstpt", i__1, "ekqmgr_", (ftnlen)1484)], &
			stnew, stpool);
	    }

/*           At this point, we can fill in all elements of the segment */
/*           table entry except for the pointers into the column table */
/*           and the column base addresses. */

	    sthan[(i__1 = stnew - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("sth"
		    "an", i__1, "ekqmgr_", (ftnlen)1493)] = *handle;
	    stsidx[(i__1 = stnew - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "stsidx", i__1, "ekqmgr_", (ftnlen)1494)] = seg;
	    stnrow[(i__1 = stnew - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "stnrow", i__1, "ekqmgr_", (ftnlen)1495)] = segdsc[5];
	    stncol[(i__1 = stnew - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "stncol", i__1, "ekqmgr_", (ftnlen)1496)] = segdsc[4];
	    stdtpt[(i__1 = stnew - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
		    "stdtpt", i__1, "ekqmgr_", (ftnlen)1497)] = 0;
	    movei_(segdsc, &c__24, &stdscs[(i__1 = stnew * 24 - 24) < 4800 && 
		    0 <= i__1 ? i__1 : s_rnge("stdscs", i__1, "ekqmgr_", (
		    ftnlen)1499)]);

/*           The next step is to set up the column attributes and */
/*           descriptors. */

	    s_copy(state, "MAKE_COLUMN_TABLE_ENTRIES", (ftnlen)80, (ftnlen)25)
		    ;
	} else if (s_cmp(state, "MAKE_COLUMN_TABLE_ENTRIES", (ftnlen)80, (
		ftnlen)25) == 0) {
	    if (presnt) {

/*              If the current table was present before loading the */
/*              current segment, we must make sure that the attributes */
/*              of the columns in this segment match those of the table */
/*              to which the segment belongs. */

/*              We must load the column descriptors for this segment */
/*              in the *same order* as those for every other segment */
/*              in the table.  This order matches that of the columns */
/*              in the column attribute list for the table. */

/*              For each column in the column list of the current table, */
/*              check the list of column names for the current segment, */
/*              looking for a match. */

		j = tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("tbctpt", i__1, "ekqmgr_", (ftnlen)1528)];
		while(j > 0 && s_cmp(state, "ABORT", (ftnlen)80, (ftnlen)5) !=
			 0) {
		    k = isrchc_(ctnams + (((i__1 = j - 1) < 500 && 0 <= i__1 ?
			     i__1 : s_rnge("ctnams", i__1, "ekqmgr_", (ftnlen)
			    1534)) << 5), &ncols, cnams, (ftnlen)32, (ftnlen)
			    32);
		    if (k > 0) {

/*                    We have a name match.  At this point, we must */
/*                    check that the column's other attributes---data */
/*                    type, size, and whether the column is */
/*                    indexed---match as well.  It's an error if they */
/*                    don't. */

			indexd = cdscrs[(i__1 = k * 11 - 6) < 5500 && 0 <= 
				i__1 ? i__1 : s_rnge("cdscrs", i__1, "ekqmgr_"
				, (ftnlen)1545)] != -1;
			nulsok = cdscrs[(i__1 = k * 11 - 4) < 5500 && 0 <= 
				i__1 ? i__1 : s_rnge("cdscrs", i__1, "ekqmgr_"
				, (ftnlen)1546)] != -1;
			attmch = cdscrs[(i__1 = k * 11 - 11) < 5500 && 0 <= 
				i__1 ? i__1 : s_rnge("cdscrs", i__1, "ekqmgr_"
				, (ftnlen)1547)] == ctclas[(i__2 = j - 1) < 
				500 && 0 <= i__2 ? i__2 : s_rnge("ctclas", 
				i__2, "ekqmgr_", (ftnlen)1547)] && cdscrs[(
				i__3 = k * 11 - 10) < 5500 && 0 <= i__3 ? 
				i__3 : s_rnge("cdscrs", i__3, "ekqmgr_", (
				ftnlen)1547)] == cttyps[(i__4 = j - 1) < 500 
				&& 0 <= i__4 ? i__4 : s_rnge("cttyps", i__4, 
				"ekqmgr_", (ftnlen)1547)] && cdscrs[(i__5 = k 
				* 11 - 9) < 5500 && 0 <= i__5 ? i__5 : s_rnge(
				"cdscrs", i__5, "ekqmgr_", (ftnlen)1547)] == 
				ctlens[(i__6 = j - 1) < 500 && 0 <= i__6 ? 
				i__6 : s_rnge("ctlens", i__6, "ekqmgr_", (
				ftnlen)1547)] && cdscrs[(i__7 = k * 11 - 8) < 
				5500 && 0 <= i__7 ? i__7 : s_rnge("cdscrs", 
				i__7, "ekqmgr_", (ftnlen)1547)] == ctsizs[(
				i__8 = j - 1) < 500 && 0 <= i__8 ? i__8 : 
				s_rnge("ctsizs", i__8, "ekqmgr_", (ftnlen)
				1547)] && indexd == ctindx[(i__9 = j - 1) < 
				500 && 0 <= i__9 ? i__9 : s_rnge("ctindx", 
				i__9, "ekqmgr_", (ftnlen)1547)] && nulsok == 
				ctnull[(i__10 = j - 1) < 500 && 0 <= i__10 ? 
				i__10 : s_rnge("ctnull", i__10, "ekqmgr_", (
				ftnlen)1547)];
			if (attmch) {

/*                       Great, the attributes match.  Actually, the */
/*                       addition of the current segment can *change* */
/*                       one attribute of the current table:  the */
/*                       maximum non-blank width associated with the */
/*                       current column, if the column has character */
/*                       type.  We'll make this change after we're */
/*                       sure we won't have to undo it. */

/*                       Store the column descriptor for this column */
/*                       in the descriptor table.  We'll need to */
/*                       allocate a descriptor table entry first. */

			    if (lnknfn_(dtpool) == 0) {

/*                          No free nodes left in the descriptor table. */

				s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
				s_copy(problm, "DESCRIPTOR_TABLE_FULL", (
					ftnlen)80, (ftnlen)21);
			    } else {

/*                          A free node is available.  Link it in */
/*                          at the tail of the descriptor list for */
/*                          the current segment. */

				lnkan_(dtpool, &dtnew);
				if (stdtpt[(i__1 = stnew - 1) < 200 && 0 <= 
					i__1 ? i__1 : s_rnge("stdtpt", i__1, 
					"ekqmgr_", (ftnlen)1585)] <= 0) {
				    stdtpt[(i__1 = stnew - 1) < 200 && 0 <= 
					    i__1 ? i__1 : s_rnge("stdtpt", 
					    i__1, "ekqmgr_", (ftnlen)1587)] = 
					    dtnew;
				} else {
				    lnkilb_(&stdtpt[(i__1 = stnew - 1) < 200 
					    && 0 <= i__1 ? i__1 : s_rnge(
					    "stdtpt", i__1, "ekqmgr_", (
					    ftnlen)1591)], &dtnew, dtpool);
				}

/*                          Fill in the descriptor. */

				movei_(&cdscrs[(i__1 = k * 11 - 11) < 5500 && 
					0 <= i__1 ? i__1 : s_rnge("cdscrs", 
					i__1, "ekqmgr_", (ftnlen)1598)], &
					c__11, &dtdscs[(i__2 = dtnew * 11 - 
					11) < 110000 && 0 <= i__2 ? i__2 : 
					s_rnge("dtdscs", i__2, "ekqmgr_", (
					ftnlen)1598)]);
			    }

/*                       We filled in a descriptor table entry, or */
/*                       else we ran out of room. */

			} else {

/*                       Seriously bad news.  Someone's tried to */
/*                       load an EK containing a column with */
/*                       attributes that conflict with those of a */
/*                       loaded column of the same name in the */
/*                       current table. */

			    s_copy(colnam, ctnams + (((i__1 = j - 1) < 500 && 
				    0 <= i__1 ? i__1 : s_rnge("ctnams", i__1, 
				    "ekqmgr_", (ftnlen)1616)) << 5), (ftnlen)
				    32, (ftnlen)32);
			    s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
			    s_copy(problm, "MISMATCHED_COLUMN_ATTRIBUTES", (
				    ftnlen)80, (ftnlen)28);
			}
		    } else {

/*                    No name match; the current column from the current */
/*                    table is not present in the segment we're looking */
/*                    at. */

			s_copy(colnam, ctnams + (((i__1 = j - 1) < 500 && 0 <=
				 i__1 ? i__1 : s_rnge("ctnams", i__1, "ekqmg"
				"r_", (ftnlen)1629)) << 5), (ftnlen)32, (
				ftnlen)32);
			s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
			s_copy(problm, "MISSING_COLUMN", (ftnlen)80, (ftnlen)
				14);
		    }

/*                 The current column matched one in the column list */
/*                 for the current table, or else we have a problem. */

/*                 Advance to the next column in the table's column list. */

		    if (s_cmp(state, "ABORT", (ftnlen)80, (ftnlen)5) != 0) {
			j = lnknxt_(&j, ctpool);
		    }
		}

/*              We've made descriptor table entries for each column in */
/*              the current segment, or else we have an error. */

	    } else {

/*              We need to set up the column attribute entries for */
/*              the new table introduced by loading this segment.  We */
/*              also need to set up descriptor table entries for the */
/*              segment.  We *don't* have to check the consistency of */
/*              the attributes of the columns. */

		k = 1;
		while(k <= ncols && s_cmp(state, "ABORT", (ftnlen)80, (ftnlen)
			5) != 0) {

/*                 Allocate a new entry in the column attribute table and */
/*                 link it to the tail of the column list for the */
/*                 current table.  If the column list is empty, update */
/*                 the list head. */

		    if (lnknfn_(ctpool) == 0) {

/*                    There's no more space to store attribute */
/*                    descriptors. */

			s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
			s_copy(problm, "ATTRIBUTE_TABLE_FULL", (ftnlen)80, (
				ftnlen)20);
		    } else {
			lnkan_(ctpool, &ctnew);
			if (tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
				i__1 : s_rnge("tbctpt", i__1, "ekqmgr_", (
				ftnlen)1680)] <= 0) {
			    tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
				    i__1 : s_rnge("tbctpt", i__1, "ekqmgr_", (
				    ftnlen)1682)] = ctnew;
			} else {
			    lnkilb_(&tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= 
				    i__1 ? i__1 : s_rnge("tbctpt", i__1, 
				    "ekqmgr_", (ftnlen)1686)], &ctnew, ctpool)
				    ;
			}

/*                    Fill in the new column attribute entry with the */
/*                    attributes for this column. */

			s_copy(ctnams + (((i__1 = ctnew - 1) < 500 && 0 <= 
				i__1 ? i__1 : s_rnge("ctnams", i__1, "ekqmgr_"
				, (ftnlen)1694)) << 5), cnams + (((i__2 = k - 
				1) < 500 && 0 <= i__2 ? i__2 : s_rnge("cnams",
				 i__2, "ekqmgr_", (ftnlen)1694)) << 5), (
				ftnlen)32, (ftnlen)32);
			ctclas[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("ctclas", i__1, "ekqmgr_", (ftnlen)
				1695)] = cdscrs[(i__2 = k * 11 - 11) < 5500 &&
				 0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1695)];
			cttyps[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("cttyps", i__1, "ekqmgr_", (ftnlen)
				1696)] = cdscrs[(i__2 = k * 11 - 10) < 5500 &&
				 0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1696)];
			ctlens[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("ctlens", i__1, "ekqmgr_", (ftnlen)
				1697)] = cdscrs[(i__2 = k * 11 - 9) < 5500 && 
				0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1697)];
			ctsizs[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("ctsizs", i__1, "ekqmgr_", (ftnlen)
				1698)] = cdscrs[(i__2 = k * 11 - 8) < 5500 && 
				0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1698)];
			ctindx[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("ctindx", i__1, "ekqmgr_", (ftnlen)
				1699)] = cdscrs[(i__2 = k * 11 - 6) < 5500 && 
				0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1699)] != -1;
			ctfixd[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("ctfixd", i__1, "ekqmgr_", (ftnlen)
				1700)] = cdscrs[(i__2 = k * 11 - 8) < 5500 && 
				0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1700)] != -1;
			ctnull[(i__1 = ctnew - 1) < 500 && 0 <= i__1 ? i__1 : 
				s_rnge("ctnull", i__1, "ekqmgr_", (ftnlen)
				1701)] = cdscrs[(i__2 = k * 11 - 4) < 5500 && 
				0 <= i__2 ? i__2 : s_rnge("cdscrs", i__2, 
				"ekqmgr_", (ftnlen)1701)] != -1;

/*                    Store the column descriptor for this column */
/*                    in the descriptor table.  We'll need to */
/*                    allocate a descriptor table entry first. */

			if (lnknfn_(dtpool) == 0) {

/*                       No free nodes left in the descriptor table. */

			    s_copy(state, "ABORT", (ftnlen)80, (ftnlen)5);
			    s_copy(problm, "DESCRIPTOR_TABLE_FULL", (ftnlen)
				    80, (ftnlen)21);
			} else {

/*                       A free node is available.  Link it in at the */
/*                       tail of the descriptor list for the current */
/*                       segment. */

			    lnkan_(dtpool, &dtnew);
			    if (stdtpt[(i__1 = stnew - 1) < 200 && 0 <= i__1 ?
				     i__1 : s_rnge("stdtpt", i__1, "ekqmgr_", 
				    (ftnlen)1723)] <= 0) {
				stdtpt[(i__1 = stnew - 1) < 200 && 0 <= i__1 ?
					 i__1 : s_rnge("stdtpt", i__1, "ekqm"
					"gr_", (ftnlen)1725)] = dtnew;
			    } else {
				lnkilb_(&stdtpt[(i__1 = stnew - 1) < 200 && 0 
					<= i__1 ? i__1 : s_rnge("stdtpt", 
					i__1, "ekqmgr_", (ftnlen)1729)], &
					dtnew, dtpool);
			    }

/*                       Fill in the descriptor. */

			    movei_(&cdscrs[(i__1 = k * 11 - 11) < 5500 && 0 <=
				     i__1 ? i__1 : s_rnge("cdscrs", i__1, 
				    "ekqmgr_", (ftnlen)1736)], &c__11, &
				    dtdscs[(i__2 = dtnew * 11 - 11) < 110000 
				    && 0 <= i__2 ? i__2 : s_rnge("dtdscs", 
				    i__2, "ekqmgr_", (ftnlen)1736)]);
			}
		    }

/*                 We created attribute and descriptor entries for the */
/*                 current column, or we encountered an error. */

		    if (s_cmp(state, "ABORT", (ftnlen)80, (ftnlen)5) != 0) {

/*                    Consider the next column. */

			++k;
		    }
		}

/*              We created attribute and descriptor entries for every */
/*              column in the current segment, or we encountered an */
/*              error. */

	    }

/*           We've processed the current segment in the new file, or */
/*           else we have an error condition. */

	    if (s_cmp(state, "ABORT", (ftnlen)80, (ftnlen)5) != 0) {

/*              We're ready to look at the next segment in the new file. */

		s_copy(state, "NEXT_SEGMENT", (ftnlen)80, (ftnlen)12);
	    }
	} else if (s_cmp(state, "NEXT_SEGMENT", (ftnlen)80, (ftnlen)12) == 0) 
		{
	    if (seg < nseg) {
		++seg;
		s_copy(state, "SUMMARIZE_SEGMENT", (ftnlen)80, (ftnlen)17);
	    } else {

/*              We're done with all of the segments. */

		s_copy(state, "DONE", (ftnlen)80, (ftnlen)4);
	    }
	} else if (s_cmp(state, "ABORT", (ftnlen)80, (ftnlen)5) == 0) {

/*           We must clean up all the data structure additions we made to */
/*           accommodate the new file. */

/*           Basically, we unload the new file.  We defer the call to */
/*           EKCLS until after we've reported the error. */

/*           The file table is first.  The file is at the head of the */
/*           list.  If the file has a successor, that file is now at the */
/*           head of the list. */

	    fthead = lnknxt_(&new__, ftpool);
	    if (fthead < 0) {

/*              There are no files left.  Clean up the whole shebang. */

		lnkini_(&c__20, ftpool);
		lnkini_(&c__200, stpool);
		lnkini_(&c__10000, dtpool);
		lnkini_(&c__500, ctpool);
		lnkini_(&c__100, tbpool);
		fthead = 0;
		tbhead = 0;
	    } else {

/*              If we arrived here, the file we're unloading is not the */
/*              only loaded file. */

/*              Free the file table entry for the file.  The entry can be */
/*              regarded as a sublist that starts and ends with the Ith */
/*              node, so we can call the `free sublist' routine to get */
/*              rid of it. */

		lnkfsl_(&new__, &new__, ftpool);

/*              It's time to clean up the table list, segment table, */
/*              column attribute table, and column descriptor table.  The */
/*              plan is to traverse the table list, and for each member */
/*              of the list, traverse the corresponding segment list, */
/*              removing from the list information about segments and */
/*              columns in the file we're unloading.  If the segment list */
/*              for any table becomes empty, we remove the entry for that */
/*              table from the table list. */

		tbcurr = tbhead;
		while(tbcurr > 0) {

/*                 See whether the current table is in the file we're */
/*                 unloading. */

		    i__ = 1;
		    while(i__ <= tbflsz[(i__1 = tbcurr - 1) < 100 && 0 <= 
			    i__1 ? i__1 : s_rnge("tbflsz", i__1, "ekqmgr_", (
			    ftnlen)1857)] && ! fnd) {
			if (tbfils[(i__1 = i__ + tbcurr * 20 - 21) < 2000 && 
				0 <= i__1 ? i__1 : s_rnge("tbfils", i__1, 
				"ekqmgr_", (ftnlen)1860)] == *handle) {

/*                       This table is affected by unloading the file. */

			    fnd = TRUE_;
			} else {

/*                       Look at the next file handle. */

			    ++i__;
			}
		    }
		    if (fnd) {

/*                    Update the information for the current table to */
/*                    reflect the unloading of the specified EK. */

/*                    Unloading the specified EK removes one handle from */
/*                    the list of file handles associated with this */
/*                    table.  Compress this handle out of the list. */

			i__2 = tbflsz[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ?
				 i__1 : s_rnge("tbflsz", i__1, "ekqmgr_", (
				ftnlen)1886)] - 1;
			for (j = i__; j <= i__2; ++j) {
			    tbfils[(i__1 = j + tbcurr * 20 - 21) < 2000 && 0 
				    <= i__1 ? i__1 : s_rnge("tbfils", i__1, 
				    "ekqmgr_", (ftnlen)1888)] = tbfils[(i__3 =
				     j + 1 + tbcurr * 20 - 21) < 2000 && 0 <= 
				    i__3 ? i__3 : s_rnge("tbfils", i__3, 
				    "ekqmgr_", (ftnlen)1888)];
			}
			tbflsz[(i__2 = tbcurr - 1) < 100 && 0 <= i__2 ? i__2 :
				 s_rnge("tbflsz", i__2, "ekqmgr_", (ftnlen)
				1892)] = tbflsz[(i__1 = tbcurr - 1) < 100 && 
				0 <= i__1 ? i__1 : s_rnge("tbflsz", i__1, 
				"ekqmgr_", (ftnlen)1892)] - 1;

/*                    Traverse the segment list for this table, looking */
/*                    for segments in the specified EK. */

			delseg = tbstpt[(i__2 = tbcurr - 1) < 100 && 0 <= 
				i__2 ? i__2 : s_rnge("tbstpt", i__2, "ekqmgr_"
				, (ftnlen)1898)];
			while(delseg > 0) {
			    if (sthan[(i__2 = delseg - 1) < 200 && 0 <= i__2 ?
				     i__2 : s_rnge("sthan", i__2, "ekqmgr_", (
				    ftnlen)1902)] == *handle) {

/*                          This segment is aboard the sinking ship.  Put */
/*                          it out of its misery. */

/*                          First, euthanize its column descriptors. */
/*                          These descriptors are linked together, so we */
/*                          can free all of them in one shot. */

				j = stdtpt[(i__2 = delseg - 1) < 200 && 0 <= 
					i__2 ? i__2 : s_rnge("stdtpt", i__2, 
					"ekqmgr_", (ftnlen)1911)];
				if (j > 0) {
				    k = lnktl_(&j, dtpool);
				    lnkfsl_(&j, &k, dtpool);
				}

/*                          Now we can delete the segment table entry */
/*                          itself.  This deletion may necessitate */
/*                          updating the segment list pointer in the */
/*                          parent table's table list entry. */

				if (delseg == tbstpt[(i__2 = tbcurr - 1) < 
					100 && 0 <= i__2 ? i__2 : s_rnge(
					"tbstpt", i__2, "ekqmgr_", (ftnlen)
					1924)]) {
				    tbstpt[(i__2 = tbcurr - 1) < 100 && 0 <= 
					    i__2 ? i__2 : s_rnge("tbstpt", 
					    i__2, "ekqmgr_", (ftnlen)1926)] = 
					    lnknxt_(&delseg, stpool);
				}
				next = lnknxt_(&delseg, stpool);
				lnkfsl_(&delseg, &delseg, stpool);

/*                          The segment we just freed may have been the */
/*                          last one belonging to this table.  We deal */
/*                          with this possibility later, below the end of */
/*                          the current loop. */

				delseg = next;
			    } else {
				delseg = lnknxt_(&delseg, stpool);
			    }
			}

/*                    We've examined all of the segments in the current */
/*                    table. */

/*                    If the segment list for the current table became */
/*                    empty as a result of our having plundered the */
/*                    segment table, delete the entry for this table from */
/*                    the table list. We do *not* need to concern */
/*                    ourselves with the possibility that this deletion */
/*                    will empty the table list, since we know we're */
/*                    not unloading the last loaded file.  However, we */
/*                    may need to update the head-of-list pointer for the */
/*                    table list. */

			if (tbstpt[(i__2 = tbcurr - 1) < 100 && 0 <= i__2 ? 
				i__2 : s_rnge("tbstpt", i__2, "ekqmgr_", (
				ftnlen)1965)] <= 0) {

/*                       There are no loaded segments left for this */
/*                       table. */

/*                       Delete the list of column attribute entries for */
/*                       the columns in this table, then delete the */
/*                       table's entry from the table list. */

/*                       The column attribute entries are linked, so we */
/*                       can free them in one shot.  Don't crash if the */
/*                       column attribute list is empty. */

			    j = tbctpt[(i__2 = tbcurr - 1) < 100 && 0 <= i__2 
				    ? i__2 : s_rnge("tbctpt", i__2, "ekqmgr_",
				     (ftnlen)1978)];
			    if (j > 0) {
				k = lnktl_(&j, ctpool);
				lnkfsl_(&j, &k, ctpool);
			    }
			    if (tbcurr == tbhead) {

/*                          The entry for this table is at the head of */
/*                          the table list.  Update the head of the list. */

				tbhead = lnknxt_(&tbcurr, tbpool);
				next = tbhead;
			    } else {
				next = lnknxt_(&tbcurr, tbpool);
			    }

/*                       Make the entry for this table go away. */

			    lnkfsl_(&tbcurr, &tbcurr, tbpool);

/*                       Look at the next table. */

			    tbcurr = next;
			} else {

/*                       We're done with the current table.  Look at the */
/*                       next one. */

			    tbcurr = lnknxt_(&tbcurr, tbpool);
			}

/*                    We've cleaned up the table entry for the current */
/*                    table, if it was necessary to do so. */

		    } else {

/*                    The current table is not affected by unloading this */
/*                    file.  Examine the next table. */

			tbcurr = lnknxt_(&tbcurr, tbpool);
		    }

/*                 We've processed the current table. */

		}
	    }

/*           We've cleaned up after the aborted partial load. */

/*           Now that the mess has been arranged, tell the user what the */
/*           problem was. */

	    dashlu_(handle, &unit);
	    if (s_cmp(problm, "TABLE_LIST_FULL", (ftnlen)80, (ftnlen)15) == 0)
		     {
		setmsg_("The EK file # could not be loaded; the maximum numb"
			"er of distinct tables has already been reached.", (
			ftnlen)98);
		errfnm_("#", &unit, (ftnlen)1);
		sigerr_("SPICE(EKTABLELISTFULL)", (ftnlen)22);
	    } else if (s_cmp(problm, "DUPLICATE_COLUMN_NAMES", (ftnlen)80, (
		    ftnlen)22) == 0) {
		setmsg_("The EK file # could not be loaded; the segment # co"
			"ntains duplicate column names in table #.", (ftnlen)
			92);
		errfnm_("#", &unit, (ftnlen)1);
		errint_("#", &seg, (ftnlen)1);
		errch_("#", tabnam, (ftnlen)1, (ftnlen)64);
		sigerr_("SPICE(EKCOLNUMMISMATCH)", (ftnlen)23);
	    } else if (s_cmp(problm, "COLUMN_NUMBER_MISMATCH", (ftnlen)80, (
		    ftnlen)22) == 0) {
		setmsg_("The EK file # could not be loaded; the number of co"
			"lumns (#) in segment # does not match the number of "
			"columns (#) in the parent table #.", (ftnlen)137);
		errfnm_("#", &unit, (ftnlen)1);
		errint_("#", &ncols, (ftnlen)1);
		errint_("#", &seg, (ftnlen)1);
		errint_("#", &npcol, (ftnlen)1);
		errch_("#", tabnam, (ftnlen)1, (ftnlen)64);
		sigerr_("SPICE(EKCOLNUMMISMATCH)", (ftnlen)23);
	    } else if (s_cmp(problm, "MISMATCHED_COLUMN_ATTRIBUTES", (ftnlen)
		    80, (ftnlen)28) == 0) {
		setmsg_("EK file # contains a column whose attributes confli"
			"ct with a loaded column.  The offending column name "
			"is #; the column is in segment #* of the file.", (
			ftnlen)149);
		errfnm_("#", &unit, (ftnlen)1);
		errch_("#", colnam, (ftnlen)1, (ftnlen)32);
		errint_("*", &seg, (ftnlen)1);
		sigerr_("SPICE(BADATTRIBUTES)", (ftnlen)20);
	    } else if (s_cmp(problm, "DESCRIPTOR_TABLE_FULL", (ftnlen)80, (
		    ftnlen)21) == 0) {
		setmsg_("The EK file # could not be loaded; themaximum allow"
			"ed number of loaded columns already been reached.", (
			ftnlen)100);
		errfnm_("#", &unit, (ftnlen)1);
		sigerr_("SPICE(COLDESCTABLEFULL)", (ftnlen)23);
	    } else if (s_cmp(problm, "ATTRIBUTE_TABLE_FULL", (ftnlen)80, (
		    ftnlen)20) == 0) {
		setmsg_("The EK file # could not be loaded; the maximum numb"
			"er of columns havingdistinct attributes has already "
			"been reached.", (ftnlen)116);
		errfnm_("#", &unit, (ftnlen)1);
		sigerr_("SPICE(EKCOLATTRTABLEFULL)", (ftnlen)25);
	    } else if (s_cmp(problm, "MISSING_COLUMN", (ftnlen)80, (ftnlen)14)
		     == 0) {
		setmsg_("The EK file # could not be loaded; the column # in "
			"already loaded table # is not present in segment # i"
			"n the EK file.", (ftnlen)117);
		errfnm_("#", &unit, (ftnlen)1);
		errch_("#", colnam, (ftnlen)1, (ftnlen)32);
		errch_("#", tabnam, (ftnlen)1, (ftnlen)64);
		errint_("#", &seg, (ftnlen)1);
		sigerr_("SPICE(EKMISSINGCOLUMN)", (ftnlen)22);
	    } else {
		setmsg_("The EK file # could not be loaded; the problem \""
			"#\" occurred while attempting to load the file.  By "
			"way, there is a bug in EKLEF if you see this message."
			, (ftnlen)152);
		errfnm_("#", &unit, (ftnlen)1);
		errch_("#", problm, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(BUG)", (ftnlen)10);
	    }
	    ekcls_(handle);
	    chkout_("EKLEF", (ftnlen)5);
	    return 0;
	}
    }

/*     At this point, we've made the file table, table list, segment */
/*     table, column descriptor table, and column attribute table updates */
/*     necessary to reflect the presence of the new file. */

    chkout_("EKLEF", (ftnlen)5);
    return 0;
/* $Procedure     EKUEF  ( EK, unload event file ) */

L_ekuef:
/* $ Abstract */

/*     Unload an EK file, making its contents inaccessible to the */
/*     EK reader routines, and clearing space in order to allow other */
/*     EK files to be loaded. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of EK file. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle returned by EKLEF. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  Unloading a file that is not loaded has no effect. */

/* $ Files */

/*     See the description of the input argument HANDLE in */
/*     $Detailed_Input. */

/* $ Particulars */

/*     This routine removes information about an EK file from the */
/*     EK system, freeing space to increase the number of other EK */
/*     files that can be loaded.  The file is also unloaded from */
/*     the DAS system and closed. */

/* $ Examples */

/*     1)  Load 25 EK files sequentially, unloading the previous file */
/*         before each new file is loaded.  Unloading files prevents */
/*         them from being searched during query execution. */

/*            DO I = 1, 25 */

/*               CALL EKLEF ( EK(I), HANDLE ) */

/*               [Perform queries] */

/*               CALL EKUEF ( HANDLE ) */

/*            END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Previous version line was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     unload EK file */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKUEF", (ftnlen)5);
    }

/*     On the first pass through this routine, initialize the tables, */
/*     if it hasn't been done yet. */

    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     Check to see whether the named EK has been loaded.  Do nothing */
/*     if not. */

    i__ = fthead;
    fnd = FALSE_;
    while(i__ > 0 && ! fnd) {
	if (*handle == fthan[(i__2 = i__ - 1) < 20 && 0 <= i__2 ? i__2 : 
		s_rnge("fthan", i__2, "ekqmgr_", (ftnlen)2340)]) {
	    fnd = TRUE_;
	} else {
	    i__ = lnknxt_(&i__, ftpool);
	}
    }
    if (! fnd) {
	chkout_("EKUEF", (ftnlen)5);
	return 0;
    }

/*     If we got to here, HANDLE points to a loaded EK file.  It's */
/*     time to wipe from the EK tables all trivial fond records */
/*     pertaining to the file in question. */

/*     The file table is first. */

    if (i__ == fthead) {

/*        The file is at the head of the list.  If the file has a */
/*        successor, that file is now at the head of the list. */

	fthead = lnknxt_(&i__, ftpool);
	if (fthead < 0) {

/*           There are no files left.  Clean up the whole shebang. */

	    lnkini_(&c__20, ftpool);
	    lnkini_(&c__200, stpool);
	    lnkini_(&c__10000, dtpool);
	    lnkini_(&c__500, ctpool);
	    lnkini_(&c__100, tbpool);
	    fthead = 0;
	    tbhead = 0;

/*           Close the EK file, to keep the DAS system's bookkeeping */
/*           up to date. */

	    ekcls_(handle);
	    chkout_("EKUEF", (ftnlen)5);
	    return 0;
	}
    }

/*     If we arrived here, the file we're unloading is not the only */
/*     loaded file. */

/*     Free the file table entry for the file.  The entry can be */
/*     regarded as a sublist that starts and ends with the Ith node, */
/*     so we can call the `free sublist' routine to get rid of it. */

    lnkfsl_(&i__, &i__, ftpool);

/*     It's time to clean up the table list, segment table, column */
/*     attribute table, and column descriptor table.  The plan is */
/*     to traverse the table list, and for each member of the list, */
/*     traverse the corresponding segment list, removing from the list */
/*     information about segments and columns in the file we're */
/*     unloading.  If the segment list for any table becomes empty, we */
/*     remove the entry for that table from the table list. */

    tbcurr = tbhead;
    while(tbcurr > 0) {

/*        See whether the current table is in the file we're unloading. */

	i__ = 1;
	while(i__ <= tbflsz[(i__2 = tbcurr - 1) < 100 && 0 <= i__2 ? i__2 : 
		s_rnge("tbflsz", i__2, "ekqmgr_", (ftnlen)2420)] && ! fnd) {
	    if (tbfils[(i__2 = i__ + tbcurr * 20 - 21) < 2000 && 0 <= i__2 ? 
		    i__2 : s_rnge("tbfils", i__2, "ekqmgr_", (ftnlen)2422)] ==
		     *handle) {

/*              This table is affected by unloading the file. */

		fnd = TRUE_;
	    } else {

/*              Look at the next file handle. */

		++i__;
	    }
	}
	if (fnd) {

/*           Update the information for the current table to reflect */
/*           the unloading of the specified EK. */

/*           Unloading the specified EK removes one handle from the */
/*           list of file handles associated with this table.  Compress */
/*           this handle out of the list. */

	    i__1 = tbflsz[(i__2 = tbcurr - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("tbflsz", i__2, "ekqmgr_", (ftnlen)2448)] - 1;
	    for (j = i__; j <= i__1; ++j) {
		tbfils[(i__2 = j + tbcurr * 20 - 21) < 2000 && 0 <= i__2 ? 
			i__2 : s_rnge("tbfils", i__2, "ekqmgr_", (ftnlen)2450)
			] = tbfils[(i__3 = j + 1 + tbcurr * 20 - 21) < 2000 &&
			 0 <= i__3 ? i__3 : s_rnge("tbfils", i__3, "ekqmgr_", 
			(ftnlen)2450)];
	    }
	    tbflsz[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "tbflsz", i__1, "ekqmgr_", (ftnlen)2454)] = tbflsz[(i__2 =
		     tbcurr - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("tbflsz", 
		    i__2, "ekqmgr_", (ftnlen)2454)] - 1;

/*           Traverse the segment list for this table, looking */
/*           for segments in the specified EK. */

	    seg = tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("tbstpt", i__1, "ekqmgr_", (ftnlen)2460)];
	    while(seg > 0) {
		if (sthan[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
			"sthan", i__1, "ekqmgr_", (ftnlen)2464)] == *handle) {

/*                 This segment is aboard the sinking ship.  Put it */
/*                 out of its misery. */

/*                 First, euthanize the segment's column descriptors. */
/*                 These descriptors are linked together, so we can free */
/*                 all of them in one shot.  Don't crash if the column */
/*                 descriptor list is empty. */

		    j = stdtpt[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : 
			    s_rnge("stdtpt", i__1, "ekqmgr_", (ftnlen)2474)];
		    if (j > 0) {
			k = lnktl_(&j, dtpool);
			lnkfsl_(&j, &k, dtpool);
		    }

/*                 Now we can delete the segment table entry itself. */
/*                 This deletion may necessitate updating the segment */
/*                 list pointer in the parent table's table list entry. */

		    if (seg == tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ?
			     i__1 : s_rnge("tbstpt", i__1, "ekqmgr_", (ftnlen)
			    2486)]) {
			tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 :
				 s_rnge("tbstpt", i__1, "ekqmgr_", (ftnlen)
				2488)] = lnknxt_(&seg, stpool);
		    }
		    next = lnknxt_(&seg, stpool);
		    lnkfsl_(&seg, &seg, stpool);
		    seg = next;
		} else {
		    seg = lnknxt_(&seg, stpool);
		}
	    }

/*           We've examined all of the segments in the current table. */

/*           If the segment list for the current table became empty */
/*           as a result of our having plundered the segment table, */
/*           delete the entry for this table from the table list.  We do */
/*           *not* need to concern ourselves with the possibility that */
/*           this deletion will empty the table list, since we know we're */
/*           not unloading the last loaded file.  However, we may need to */
/*           update the head-of-list pointer for the table list. */

	    if (tbstpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "tbstpt", i__1, "ekqmgr_", (ftnlen)2518)] <= 0) {

/*              There are no loaded segments left for this table. */

/*              Delete the list of column attribute entries for the */
/*              columns in this table, then delete the table's entry from */
/*              the table list. */

/*              The column attribute entries are linked, so we can free */
/*              them in one shot. */

		j = tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("tbctpt", i__1, "ekqmgr_", (ftnlen)2529)];
		if (j > 0) {
		    k = lnktl_(&j, ctpool);
		    lnkfsl_(&j, &k, ctpool);
		}
		if (tbcurr == tbhead) {

/*                 The entry for this table is at the head of the */
/*                 table list.  Update the head of the list. */

		    tbhead = lnknxt_(&tbcurr, tbpool);
		    next = tbhead;
		} else {
		    next = lnknxt_(&tbcurr, tbpool);
		}

/*              Make the entry for this table go away. */

		lnkfsl_(&tbcurr, &tbcurr, tbpool);

/*              The successor of the current node is the next node to */
/*              examine. */

		tbcurr = next;
	    } else {

/*              We're done with the current table.  Look at the next one. */

		tbcurr = lnknxt_(&tbcurr, tbpool);
	    }

/*           We've cleaned up the table entry for the current table, */
/*           if it was necessary to do so. */

	} else {

/*           The current table is not affected by unloading this file. */
/*           Examine the next table. */

	    tbcurr = lnknxt_(&tbcurr, tbpool);
	}

/*        We've processed the current table. */

    }

/*     Don't forget to unload the EK file from the DAS system. */

    ekcls_(handle);
    chkout_("EKUEF", (ftnlen)5);
    return 0;
/* $Procedure     EKNTAB  ( EK, return number of loaded tables ) */

L_ekntab:
/* $ Abstract */

/*     Return the number of loaded EK tables. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */

/* $ Declarations */

/*     INTEGER               N */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     N          O   Number of loaded tables. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     N              is the number of loaded tables.  The count refers */
/*                    to the number of logical tables; if multiple */
/*                    segments contain data for the same table, these */
/*                    segments collectively contribute only one table */
/*                    to the count. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     The returned count is based on the currently loaded EK files. */
/*     These files must be loaded via the entry point EKLEF. */

/* $ Particulars */

/*     This routine is a utility that provides the caller with the */
/*     number of loaded tables.  Callers of EKTNAM can use this count */
/*     as the upper bound on set of table indices when looking up table */
/*     names. */

/* $ Examples */

/*     1)  Suppose we have the following list of EK files and tables */
/*         contained in those files: */

/*            File name        Table name */
/*            ---------        ---------- */

/*            FILE_1.EK        TABLE_1 */
/*                             TABLE_2 */

/*            FILE_2.EK        TABLE_1 */
/*                             TABLE_3 */

/*            FILE_3.EK        TABLE_2 */
/*                             TABLE_3 */
/*                             TABLE_4 */


/*         Then after loading these files, the call */

/*            CALL EKNTAB ( N ) */

/*         returns the value N = 4. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Previous version line was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return number of loaded tables */

/* -& */
    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     Return the number of loaded tables. */

    *n = 100 - lnknfn_(tbpool);
    return 0;
/* $Procedure     EKTNAM  ( EK, return name of loaded table ) */

L_ektnam:
/* $ Abstract */

/*     Return the name of a specified, loaded table. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */

/* $ Declarations */

/*     INTEGER               N */
/*     CHARACTER*(*)         TABLE */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   Index of table. */
/*     TABLE      O   Name of table. */

/* $ Detailed_Input */

/*     N              is the index of the table whose name is desired. */
/*                    The value of N ranges from 1 to the number of */
/*                    loaded tables, which count may be obtained from */
/*                    EKNTAB. */

/* $ Detailed_Output */

/*     TABLE          is the name of the Nth loaded table. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called when no files are loaded, the */
/*         error SPICE(NOLOADEDFILES) is signaled. */

/*     2)  If the input N is out of range, the error SPICE(INVALDINDEX) */
/*         is signaled. */

/* $ Files */

/*     The returned name is based on the currently loaded EK files. */

/* $ Particulars */

/*     This routine is a utility that provides the caller with the */
/*     name of a specified loaded table.  The index of a table with */
/*     a given name depends on the kernels loaded and possibly on */
/*     the order in which the files have been loaded. */

/* $ Examples */

/*     1)  Dump the names of the loaded tables. */

/*         CALL EKNTAB ( N ) */

/*         DO I = 1, N */
/*            CALL EKTNAM ( I, TABLE ) */
/*            WRITE (*,*) TABLE */
/*         END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Previous version line was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return name of a loaded table */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKTNAM", (ftnlen)6);
    }
    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     There nothing to fetch if no files are loaded.  A sure */
/*     symptom of this problem is that the file list is empty. */

    if (fthead <= 0) {
	setmsg_("No E-kernels are currently loaded.", (ftnlen)34);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("EKTNAM", (ftnlen)6);
	return 0;
    }
    tbcurr = tbhead;
    fnd = FALSE_;
    i__ = 0;
    while(tbcurr > 0 && ! fnd) {
	++i__;
	if (i__ == *n) {
	    fnd = TRUE_;
	    s_copy(table, tbnams + (((i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
		    i__1 : s_rnge("tbnams", i__1, "ekqmgr_", (ftnlen)2954)) <<
		     6), table_len, (ftnlen)64);
	} else {
	    tbcurr = lnknxt_(&tbcurr, tbpool);
	}
    }
    if (! fnd) {
	setmsg_("The index # does not correspond to a loaded table.", (ftnlen)
		50);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
    }
    chkout_("EKTNAM", (ftnlen)6);
    return 0;
/* $Procedure     EKCCNT  ( EK, column count ) */

L_ekccnt:
/* $ Abstract */

/*     Return the number of distinct columns in a specified, currently */
/*     loaded table */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */
/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         TABLE */
/*     INTEGER               CCOUNT */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TABLE      I   Name of table. */
/*     CCOUNT     O   Count of distinct, currently loaded columns. */

/* $ Detailed_Input */

/*     TABLE          is the name of a currently loaded table.  Case */
/*                    is not significant in the table name. */

/* $ Detailed_Output */

/*     CCOUNT         is the number of distinct columns in TABLE. */
/*                    Columns that have the same name but belong to */
/*                    different segments that are considered to be */
/*                    portions of the same column, if the segments */
/*                    containing those columns belong to TABLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified table is not loaded, the error */
/*         SPICE(TABLENOTLOADED) is signaled. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     This routine is a utility intended for use in conjunction with */
/*     the entry point EKCII.  These routines can be used to find the */
/*     names and attributes of the columns that are currently loaded. */

/* $ Examples */

/*     1)  Dump the names and attributes of the columns in each loaded */
/*         table.  EKCCNT is used to obtain column counts. */

/*            C */
/*            C     Get the number of loaded tables. */
/*            C */
/*                  CALL EKNTAB ( NTAB ) */

/*                  DO TAB = 1, NTAB */
/*            C */
/*            C        Get the name of the current table, and look up */
/*            C        the column count for this table. */
/*            C */
/*                     CALL EKTNAM ( TAB,    TABNAM ) */
/*                     CALL EKCCNT ( TABNAM, NCOLS  ) */

/*                     WRITE (*,*) 'TABLE = ', TABNAM */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        For each column in the current table, look up the */
/*            C        column's attributes.  The attribute block */
/*            C        index parameters are defined in the include file */
/*            C        ekattdsc.inc. */
/*            C */
/*                     DO I = 1, NCOLS */

/*                        CALL EKCII ( TABNAM, I, COLNAM, ATTDSC ) */

/*                        WRITE (*,*) 'COLUMN = ', COLNAM */

/*            C */
/*            C           Write out the current column's data type. */
/*            C */
/*                        IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN */
/*                           WRITE (*,*) 'TYPE   =  CHR' */

/*                           IF ( ATTDSC(ATTLEN) .EQ. -1 ) THEN */
/*                              WRITE (*,*) 'STRING LENGTH = VARIABLE.' */
/*                           ELSE */
/*                              WRITE (*,*) 'STRING LENGTH = ', */
/*                 .                         ATTDSC(ATTLEN) */
/*                           END IF */

/*                        ELSE IF ( ATTDSC(ATTTYP) .EQ. DP ) THEN */
/*                           WRITE (*,*) 'TYPE   =  DP' */

/*                        ELSE IF ( ATTDSC(ATTTYP) .EQ. INT ) THEN */
/*                           WRITE (*,*) 'TYPE   =  INT' */

/*                        ELSE */
/*                           WRITE (*,*) 'TYPE   =  TIME' */
/*                        END IF */

/*            C */
/*            C           Write out the current column's entry size. */
/*            C */
/*                        WRITE (*,*) 'SIZE   = ', ATTDSC(ATTSIZ) */

/*            C */
/*            C           Indicate whether the current column is indexed. */
/*            C */
/*                        IF ( ATTDSC(ATTIDX) .EQ. -1 ) THEN */
/*                           WRITE (*,*) 'NOT INDEXED' */
/*                        ELSE */
/*                           WRITE (*,*) 'INDEXED' */
/*                        END IF */

/*            C */
/*            C           Indicate whether the current column allows */
/*            C           null values. */
/*            C */
/*                        IF ( ATTDSC(ATTNFL) .EQ. -1 ) THEN */
/*                           WRITE (*,*) 'NULL VALUES NOT ALLOWED' */
/*                        ELSE */
/*                           WRITE (*,*) 'NULL VALUES ALLOWED' */
/*                        END IF */

/*                     END DO */

/*                  END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Misspelling of "conjunction" was fixed. */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */


/* -& */
/* $ Index_Entries */

/*     return the number of loaded EK columns */
/*     return the count of loaded EK columns */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKCCNT", (ftnlen)6);
    }

/*     On the first pass through this routine, initialize the tables, */
/*     if it hasn't been done yet. */

    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     Find the table.  If there's no match, the number of loaded columns */
/*     is zero. */

    tbcurr = tbhead;
    fnd = FALSE_;
    while(tbcurr > 0 && ! fnd) {
	if (eqstr_(table, tbnams + (((i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
		i__1 : s_rnge("tbnams", i__1, "ekqmgr_", (ftnlen)3231)) << 6),
		 table_len, (ftnlen)64)) {
	    fnd = TRUE_;
	} else {
	    tbcurr = lnknxt_(&tbcurr, tbpool);
	}
    }
    if (! fnd) {
	*ccount = 0;
	setmsg_("The table # is not currently loaded.", (ftnlen)36);
	errch_("#", table, (ftnlen)1, table_len);
	sigerr_("SPICE(TABLENOTLOADED)", (ftnlen)21);
	chkout_("EKCCNT", (ftnlen)6);
	return 0;
    } else {

/*        Count the columns in the attribute table for the current table. */

	*ccount = 0;
	col = tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		"tbctpt", i__1, "ekqmgr_", (ftnlen)3254)];
	while(col > 0) {
	    ++(*ccount);
	    col = lnknxt_(&col, ctpool);
	}
    }
    chkout_("EKCCNT", (ftnlen)6);
    return 0;
/* $Procedure     EKCII  ( EK, column info by index ) */

L_ekcii:
/* $ Abstract */

/*     Return attribute information about a column belonging to a loaded */
/*     EK table, specifying the column by table and index. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */
/*     UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         TABLE */
/*     INTEGER               CINDEX */
/*     CHARACTER*(*)         COLUMN */
/*     INTEGER               ATTDSC ( ADSCSZ ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TABLE      I   Name of table containing column. */
/*     CINDEX     I   Index of column whose attributes are to be found. */
/*     COLUMN     O   Name of column. */
/*     ATTDSC     O   Column attribute descriptor. */

/* $ Detailed_Input */

/*     TABLE          is the name of a loaded EK table.  Case is not */
/*                    significant. */

/*     CINDEX         is the index, within TABLE's column attribute */
/*                    table, of the column whose attributes are to be */
/*                    found.  The indices of the column table entries */
/*                    range from 1 to CCOUNT, where CCOUNT is the value */
/*                    returned by the entry point EKCCNT. */

/* $ Detailed_Output */

/*     COLUMN         is the name of the specified column. */

/*     ATTDSC         is a column attribute descriptor.  ATTDSC is an */
/*                    integer array containing descriptive information */
/*                    that applies uniformly to all loaded columns */
/*                    having the name COLUMN.  The following parameter */
/*                    values occur in ATTDSC: */

/*                       IFALSE:  -1 */
/*                       ITRUE:    1 */
/*                       CHR:      1 */
/*                       DP:       2 */
/*                       INT:      3 */
/*                       TIME:     4 */

/*                    The meanings of the elements of ATTDSC are given */
/*                    below.  The indices of the elements are */
/*                    parameterized; the parameter values are defined */
/*                    in the include file ekattdsc.inc. */

/*                       ATTDSC(ATTCLS):   Column class code */

/*                       ATTDSC(ATTTYP):   Data type code---CHR, DP, INT, */
/*                                         or TIME */

/*                       ATTDSC(ATTLEN):   String length; applies to CHR */
/*                                         type.  Value is IFALSE for */
/*                                         variable-length strings. */

/*                       ATTDSC(ATTSIZ):   Column entry size; value is */
/*                                         IFALSE for variable-size */
/*                                         columns.  Here `size' refers */
/*                                         to the number of array */
/*                                         elements in a column entry. */

/*                       ATTDSC(ATTIDX):   Index flag; value is ITRUE if */
/*                                         column is indexed, IFALSE */
/*                                         otherwise. */

/*                       ATTDSC(ATTNFL):   Null flag; value is ITRUE if */
/*                                         column may contain null */
/*                                         values, IFALSE otherwise. */

/* $ Parameters */

/*     ADSCSZ         is the size of column attribute descriptor. */
/*                    (Defined in ekattdsc.inc.) */

/* $ Exceptions */

/*     1)  If the specified table is not loaded, the error */
/*         SPICE(TABLENOTLOADED) is signaled. */

/*     2)  If the input argument CINDEX is less than one or greater */
/*         than the number of columns in TABLE, the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     This routine is a utility that allows a calling routine to */
/*     determine the attributes of the currently loaded columns. */

/* $ Examples */

/*     1)  Dump the names and attributes of the columns in each loaded */
/*         table.  EKCII is used to obtain column attributes. */

/*            C */
/*            C     Get the number of loaded tables. */
/*            C */
/*                  CALL EKNTAB ( NTAB ) */

/*                  DO TAB = 1, NTAB */
/*            C */
/*            C        Get the name of the current table, and look up */
/*            C        the column count for this table. */
/*            C */
/*                     CALL EKTNAM ( TAB,    TABNAM ) */
/*                     CALL EKCCNT ( TABNAM, NCOLS  ) */

/*                     WRITE (*,*) 'TABLE = ', TABNAM */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        For each column in the current table, look up the */
/*            C        column's attributes.  The attribute block */
/*            C        index parameters are defined in the include file */
/*            C        ekattdsc.inc. */
/*            C */
/*                     DO I = 1, NCOLS */

/*                        CALL EKCII ( TABNAM, I, COLNAM, ATTDSC ) */

/*                        WRITE (*,*) 'COLUMN = ', COLNAM */

/*            C */
/*            C           Write out the current column's data type. */
/*            C */
/*                        IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN */
/*                           WRITE (*,*) 'TYPE   =  CHR' */

/*                           IF ( ATTDSC(ATTLEN) .EQ. -1 ) THEN */
/*                              WRITE (*,*) 'STRING LENGTH = VARIABLE.' */
/*                           ELSE */
/*                              WRITE (*,*) 'STRING LENGTH = ', */
/*                 .                         ATTDSC(ATTLEN) */
/*                           END IF */

/*                        ELSE IF ( ATTDSC(ATTTYP) .EQ. DP ) THEN */
/*                           WRITE (*,*) 'TYPE   =  DP' */

/*                        ELSE IF ( ATTDSC(ATTTYP) .EQ. INT ) THEN */
/*                           WRITE (*,*) 'TYPE   =  INT' */

/*                        ELSE */
/*                           WRITE (*,*) 'TYPE   =  TIME' */
/*                        END IF */

/*            C */
/*            C           Write out the current column's entry size. */
/*            C */
/*                        WRITE (*,*) 'SIZE   = ', ATTDSC(ATTSIZ) */

/*            C */
/*            C           Indicate whether the current column is indexed. */
/*            C */
/*                        IF ( ATTDSC(ATTIDX) .EQ. -1 ) THEN */
/*                           WRITE (*,*) 'NOT INDEXED' */
/*                        ELSE */
/*                           WRITE (*,*) 'INDEXED' */
/*                        END IF */

/*            C */
/*            C           Indicate whether the current column allows */
/*            C           null values. */
/*            C */
/*                        IF ( ATTDSC(ATTNFL) .EQ. -1 ) THEN */
/*                           WRITE (*,*) 'NULL VALUES NOT ALLOWED' */
/*                        ELSE */
/*                           WRITE (*,*) 'NULL VALUES ALLOWED' */
/*                        END IF */

/*                     END DO */

/*                  END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 10-FEB-2014 (BVS) */

/*        Added description of ADSCSZ to the Parameters section of the */
/*        header. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Previous version line was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return information on loaded EK column specified by index */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKCII", (ftnlen)5);
    }

/*     On the first pass through this routine, initialize the tables, */
/*     if it hasn't been done yet. */

    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     Find the table.  If there's no match, the number of loaded columns */
/*     is zero. */

    tbcurr = tbhead;
    fnd = FALSE_;
    while(tbcurr > 0 && ! fnd) {
	if (eqstr_(table, tbnams + (((i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? 
		i__1 : s_rnge("tbnams", i__1, "ekqmgr_", (ftnlen)3578)) << 6),
		 table_len, (ftnlen)64)) {
	    fnd = TRUE_;
	} else {
	    tbcurr = lnknxt_(&tbcurr, tbpool);
	}
    }
    if (! fnd) {
	setmsg_("The table # is not currently loaded.", (ftnlen)36);
	errch_("#", table, (ftnlen)1, table_len);
	sigerr_("SPICE(TABLENOTLOADED)", (ftnlen)21);
	chkout_("EKCII", (ftnlen)5);
	return 0;
    }

/*     Locate the named column in the column attribute table. */

    i__ = 0;
    col = tbctpt[(i__1 = tbcurr - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge("tbc"
	    "tpt", i__1, "ekqmgr_", (ftnlen)3602)];
    while(col > 0 && i__ < *cindex) {
	++i__;
	if (i__ == *cindex) {

/*           We've found the column.  Set the output arguments using */
/*           its attributes. */

	    s_copy(column, ctnams + (((i__1 = col - 1) < 500 && 0 <= i__1 ? 
		    i__1 : s_rnge("ctnams", i__1, "ekqmgr_", (ftnlen)3613)) <<
		     5), column_len, (ftnlen)32);
	    attdsc[0] = ctclas[(i__1 = col - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("ctclas", i__1, "ekqmgr_", (ftnlen)3615)];
	    attdsc[1] = cttyps[(i__1 = col - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("cttyps", i__1, "ekqmgr_", (ftnlen)3616)];
	    attdsc[2] = ctlens[(i__1 = col - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("ctlens", i__1, "ekqmgr_", (ftnlen)3617)];
	    attdsc[3] = ctsizs[(i__1 = col - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("ctsizs", i__1, "ekqmgr_", (ftnlen)3618)];
	    if (ctindx[(i__1 = col - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
		    "ctindx", i__1, "ekqmgr_", (ftnlen)3620)]) {
		attdsc[4] = 1;
	    } else {
		attdsc[4] = -1;
	    }
	    if (ctnull[(i__1 = col - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge(
		    "ctnull", i__1, "ekqmgr_", (ftnlen)3626)]) {
		attdsc[5] = 1;
	    } else {
		attdsc[5] = -1;
	    }
	    chkout_("EKCII", (ftnlen)5);
	    return 0;
	} else {
	    col = lnknxt_(&col, ctpool);
	}
    }

/*     We end up here if we ran out of columns before finding the */
/*     CINDEXth one, or if CINDEX was non-positive. */

    setmsg_("Column indices for table # range from # to #; requested index w"
	    "as #.", (ftnlen)68);
    errch_("#", tabnam, (ftnlen)1, (ftnlen)64);
    i__1 = max(1,i__);
    errint_("#", &i__1, (ftnlen)1);
    errint_("#", &i__, (ftnlen)1);
    errint_("#", cindex, (ftnlen)1);
    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
    chkout_("EKCII", (ftnlen)5);
    return 0;
/* $Procedure     EKSRCH  ( EK, search for events ) */

L_eksrch:
/* $ Abstract */

/*     Search for EK events matching a specified set of constraints. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     EVENT */
/*     FILES */
/*     SEARCH */

/* $ Declarations */

/*     INTEGER               EQRYI  ( LBCELL : * ) */
/*     CHARACTER*(*)         EQRYC */
/*     DOUBLE PRECISION      EQRYD  ( * ) */
/*     INTEGER               NMROWS */
/*     LOGICAL               SEMERR */
/*     CHARACTER*(*)         ERRMSG */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     EQRYI      I   Integer component of encoded query. */
/*     EQRYC      I   Character component of encoded query. */
/*     EQRYD      I   D.p. component of encoded query. */
/*     NMROWS     O   Number of rows matching query constraints. */
/*     SEMERR     O   Flag indicating whether semantic error occurred. */
/*     ERRMSG     O   Message describing semantic error, if any. */

/* $ Detailed_Input */

/*     EQRYI, */
/*     EQRYC, */
/*     EQRYD          are, respectively, the integer, character, and */
/*                    double precision portions of an encoded query. */
/*                    The query must have been parsed and must have */
/*                    its table and column names resolved.  Time values */
/*                    must have been resolved.  The query is expected */
/*                    to be semantically correct. */

/* $ Detailed_Output */

/*     NMROWS         is the number of rows matching the input query */
/*                    constraints. */

/*     SEMERR         is a logical flag indicating whether a semantic */
/*                    error was detected while attempting to respond to */
/*                    the input query. */

/*     ERRMSG         is a descriptive error message that is set if a */
/*                    semantic error is detected.  Otherwise, ERRMSG */
/*                    is returned blank. */

/*     See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     LBCELL         is the SPICELIB cell lower bound. */

/* $ Exceptions */

/*     1)  If this routine is called when no files are loaded, the */
/*         error SPICE(NOLOADEDFILES) is signaled. */

/*     2)  If the structure of the input query is invalid, this routine */
/*         may fail in mysterious ways. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     NAIF Toolkit-based applications will rarely need to call this */
/*     routine directly; the high-level routine EKFIND should normally */
/*     be used to query the EK system. */

/*     Because the structure of encoded queries is not part of the */
/*     SPICELIB user interface, we strongly recommend that users' */
/*     applications not call this routine directly. */

/* $ Examples */

/*     See the header of the umbrella subroutine EKQMGR for a */
/*     comprehensive example of the use of EKQMGR's entry points. */

/* $ Restrictions */

/*     1) This routine should normally not be called directly from */
/*        users' applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 10-FEB-2014 (BVS) */

/*        Added description of LBCELL to the Parameters section of the */
/*        header. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.2.0, 21-JUL-1998 (NJB) */

/*        ZZEKJSQZ call was added after the ZZEKJOIN call.  This change */
/*        reduces the scratch area usage for intermediate results of */
/*        joins.  It also prevents ZZEKJOIN from being handed a join */
/*        row set containing a segment vector having no corresponding */
/*        row vectors. */

/*        Removed a comment in the join loop indicating that non-join */
/*        constraints involving comparisons of column entries in the */
/*        table were being activated.  This comment was incorrect; the */
/*        constraints in question were applied earlier. */

/* -    SPICELIB Version 1.0.1, 07-JUL-1996 (NJB) */

/*        Previous version line was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     search for events in loaded EK files */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKSRCH", (ftnlen)6);
    }

/*     There nothing to search if no files are loaded.  A sure */
/*     symptom of this problem is that the file list is empty. */

    if (fthead <= 0) {
	setmsg_("No E-kernels are currently loaded.", (ftnlen)34);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("EKSRCH", (ftnlen)6);
	return 0;
    }

/*     No error to begin with. */

    *semerr = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    *nmrows = 0;
    if (first) {

/*        Initialize the file table pool, segment table pool, column */
/*        descriptor pool, column table pool, and table list pool. */

	lnkini_(&c__20, ftpool);
	lnkini_(&c__200, stpool);
	lnkini_(&c__10000, dtpool);
	lnkini_(&c__500, ctpool);
	lnkini_(&c__100, tbpool);
	fthead = 0;
	tbhead = 0;
	first = FALSE_;
    }

/*     Read some of our favorite things from the query.  We need: */

/*        - the table count */
/*        - the SELECT clause column count */
/*        - the order-by column count */
/*        - the table and alias list */

    zzekreqi_(eqryi, "NUM_TABLES", &ntab, (ftnlen)10);
    zzekreqi_(eqryi, "NUM_SELECT_COLS", &nsel, (ftnlen)15);
    zzekreqi_(eqryi, "NUM_ORDERBY_COLS", &norder, (ftnlen)16);
    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekqtab_(eqryi, eqryc, &i__, frmtab + (((i__2 = i__ - 1) < 10 && 0 <=
		 i__2 ? i__2 : s_rnge("frmtab", i__2, "ekqmgr_", (ftnlen)3902)
		) << 6), frmals + (((i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 
		: s_rnge("frmals", i__3, "ekqmgr_", (ftnlen)3902)) << 6), 
		eqryc_len, (ftnlen)64, (ftnlen)64);
    }

/*     Initialize the table vectors.  Also initialize a vector of column */
/*     list pointers. */

    ssizec_(&c__10, tabvec, (ftnlen)64);
    ssizei_(&c__10, tptvec);

/*     Fill in the FROM table vector and corresponding column pointer */
/*     vector.  It's an error if a table referenced in the FROM clause */
/*     can't be found. */

    i__1 = ntab;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find the table list entry for this table name. */

	tbcurr = tbhead;
	fnd = FALSE_;
	while(tbcurr > 0 && ! fnd) {
	    if (s_cmp(tbnams + (((i__2 = tbcurr - 1) < 100 && 0 <= i__2 ? 
		    i__2 : s_rnge("tbnams", i__2, "ekqmgr_", (ftnlen)3926)) <<
		     6), frmtab + (((i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 
		    : s_rnge("frmtab", i__3, "ekqmgr_", (ftnlen)3926)) << 6), 
		    (ftnlen)64, (ftnlen)64) == 0) {

/*              We've found the table list entry for the Ith table. */

		appndc_(frmtab + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 :
			 s_rnge("frmtab", i__2, "ekqmgr_", (ftnlen)3930)) << 
			6), tabvec, (ftnlen)64, (ftnlen)64);
		appndi_(&tbcurr, tptvec);
		fnd = TRUE_;
	    } else {
		tbcurr = lnknxt_(&tbcurr, tbpool);
	    }
	}
	if (! fnd) {
	    setmsg_("The table # is not currently loaded.", (ftnlen)36);
	    errch_("#", frmtab + (((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 :
		     s_rnge("frmtab", i__2, "ekqmgr_", (ftnlen)3941)) << 6), (
		    ftnlen)1, (ftnlen)64);
	    sigerr_("SPICE(INVALIDTABLENAME)", (ftnlen)23);
	    chkout_("EKSRCH", (ftnlen)6);
	    return 0;
	}
    }

/*     Since this is a new search, re-initialize the stack in the EK */
/*     scratch area.  Also initialize our total segment list count. */

    zzekstop_(&top);
    zzeksdec_(&top);

/*     Initialize the size of the join row set union for the current */
/*     query.  At this point, no matching rows have been found. */

    usize = 0;
    unrows = 0;

/*     Get the number of conjunctions and the sizes of the conjunctions. */

    zzekreqi_(eqryi, "NUM_CONJUNCTIONS", &nconj, (ftnlen)16);
    cleari_(&c__1000, sizes);
    i__1 = nconj;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekqcnj_(eqryi, &i__, &sizes[(i__2 = i__ - 1) < 1000 && 0 <= i__2 ? 
		i__2 : s_rnge("sizes", i__2, "ekqmgr_", (ftnlen)3970)]);
    }

/*     For each conjunction of constraints, we'll build a join row */
/*     set representing the row vectors matching those constraints. */
/*     The final result will be a join row set union representing the */
/*     row vectors satisfying at least one conjunction. */

/*     We want to build a join row set even if there are *no* */
/*     constraints.  Therefore, we always make at least one pass */
/*     through the loop below. */

    cjend = 0;
    i__1 = max(1,nconj);
    for (conj = 1; conj <= i__1; ++conj) {

/*        Our objective is to build a join row set representing the table */
/*        defined by the FROM columns and the input constraints.  To do */
/*        this, we'll first build a trivial join row set for each table; */
/*        this join row set represents the rows that satisfy constraints */
/*        on columns in that table.  Having done this, we'll produce a */
/*        final (for this conjunction) join row set that represents the */
/*        constrained join of the FROM tables.  The base address of this */
/*        join row set will be stored in the array UBASE. */

/*        We'll start out by recording the FROM table indices and column */
/*        list indices of columns listed in the constraints. */

	if (nconj == 0) {
	    cjsize = 0;
	} else {
	    cjsize = sizes[(i__2 = conj - 1) < 1000 && 0 <= i__2 ? i__2 : 
		    s_rnge("sizes", i__2, "ekqmgr_", (ftnlen)4002)];
	}
	cjbeg = cjend + 1;
	cjend += cjsize;
	i__2 = cjsize;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__14 = cjbeg + i__ - 1;
	    zzekqcon_(eqryi, eqryc, eqryd, &i__14, &cnstyp[(i__3 = i__ - 1) < 
		    1000 && 0 <= i__3 ? i__3 : s_rnge("cnstyp", i__3, "ekqmg"
		    "r_", (ftnlen)4010)], ltname, &ltbidx[(i__4 = i__ - 1) < 
		    1000 && 0 <= i__4 ? i__4 : s_rnge("ltbidx", i__4, "ekqmg"
		    "r_", (ftnlen)4010)], lcname, &lcidx[(i__5 = i__ - 1) < 
		    1000 && 0 <= i__5 ? i__5 : s_rnge("lcidx", i__5, "ekqmgr_"
		    , (ftnlen)4010)], &ops[(i__6 = i__ - 1) < 1000 && 0 <= 
		    i__6 ? i__6 : s_rnge("ops", i__6, "ekqmgr_", (ftnlen)4010)
		    ], rtname, &rtbidx[(i__7 = i__ - 1) < 1000 && 0 <= i__7 ? 
		    i__7 : s_rnge("rtbidx", i__7, "ekqmgr_", (ftnlen)4010)], 
		    rcname, &rcidx[(i__8 = i__ - 1) < 1000 && 0 <= i__8 ? 
		    i__8 : s_rnge("rcidx", i__8, "ekqmgr_", (ftnlen)4010)], &
		    dtype[(i__9 = i__ - 1) < 1000 && 0 <= i__9 ? i__9 : 
		    s_rnge("dtype", i__9, "ekqmgr_", (ftnlen)4010)], &cbegs[(
		    i__10 = i__ - 1) < 1000 && 0 <= i__10 ? i__10 : s_rnge(
		    "cbegs", i__10, "ekqmgr_", (ftnlen)4010)], &cends[(i__11 =
		     i__ - 1) < 1000 && 0 <= i__11 ? i__11 : s_rnge("cends", 
		    i__11, "ekqmgr_", (ftnlen)4010)], &dvals[(i__12 = i__ - 1)
		     < 1000 && 0 <= i__12 ? i__12 : s_rnge("dvals", i__12, 
		    "ekqmgr_", (ftnlen)4010)], &ivals[(i__13 = i__ - 1) < 
		    1000 && 0 <= i__13 ? i__13 : s_rnge("ivals", i__13, "ekq"
		    "mgr_", (ftnlen)4010)], eqryc_len, (ftnlen)64, (ftnlen)32, 
		    (ftnlen)64, (ftnlen)32);
	}
	i__2 = ntab;
	for (t = 1; t <= i__2; ++t) {

/*           We will build a trivial (one-table) join row set for the */
/*           current table. */

/*           Initialize the join row set.  Retain the base address.  We */
/*           can fill in the table count right away; the count is 1. */

	    zzekstop_(&rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4028)]);
	    for (i__ = 1; i__ <= 4; ++i__) {
		zzekspsh_(&c__1, &c__0);
	    }
	    i__5 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4034)] + 3;
	    i__6 = rbas[(i__4 = t - 1) < 10 && 0 <= i__4 ? i__4 : s_rnge(
		    "rbas", i__4, "ekqmgr_", (ftnlen)4034)] + 3;
	    zzeksupd_(&i__5, &i__6, &c__1);

/*           Count the loaded segments for the current table.  We'll */
/*           leave enough room in the join row set for each segment. */

	    tab = tptvec[(i__3 = t + 5) < 16 && 0 <= i__3 ? i__3 : s_rnge(
		    "tptvec", i__3, "ekqmgr_", (ftnlen)4040)];
	    i__ = tbstpt[(i__3 = tab - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
		    "tbstpt", i__3, "ekqmgr_", (ftnlen)4041)];
	    nsv = 0;
	    while(i__ > 0) {
		zzekspsh_(&c__1, &c__0);
		++nsv;
		i__ = lnknxt_(&i__, stpool);
	    }

/*           Save room for the row vector base addresses and counts. */

	    i__3 = nsv << 1;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		zzekspsh_(&c__1, &c__0);
	    }

/*           At this point, we can set the segment vector count in the */
/*           join row set. */

	    i__5 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4064)] + 4;
	    i__6 = rbas[(i__4 = t - 1) < 10 && 0 <= i__4 ? i__4 : s_rnge(
		    "rbas", i__4, "ekqmgr_", (ftnlen)4064)] + 4;
	    zzeksupd_(&i__5, &i__6, &nsv);

/*           Find the matching rows in the segments belonging to the */
/*           current table. */

	    seg = tbstpt[(i__3 = tab - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
		    "tbstpt", i__3, "ekqmgr_", (ftnlen)4070)];
	    nseg = 0;
	    rtotal = 0;
	    while(seg > 0) {
		++nseg;

/*              The segment vector for this segment is trivial:  it's */
/*              just the segment's index in the segment table. */

		sgvbas = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : 
			s_rnge("rbas", i__3, "ekqmgr_", (ftnlen)4081)] + 4 + (
			nseg - 1);
		i__3 = sgvbas + 1;
		i__4 = sgvbas + 1;
		zzeksupd_(&i__3, &i__4, &seg);

/*              Label as `inactive' any constraints that don't apply to */
/*              this table.  Join constraints are inactive at this stage */
/*              of the game.  Label all other constraints `active'. */
/*              We'll keep track of column and value constraints */
/*              separately. */

		i__3 = cjsize;
		for (i__ = 1; i__ <= i__3; ++i__) {

/*                 Each constraint is active to start with. */

		    activc[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? i__4 : 
			    s_rnge("activc", i__4, "ekqmgr_", (ftnlen)4096)] =
			     cnstyp[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? 
			    i__5 : s_rnge("cnstyp", i__5, "ekqmgr_", (ftnlen)
			    4096)] == 1;
		    activv[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? i__4 : 
			    s_rnge("activv", i__4, "ekqmgr_", (ftnlen)4097)] =
			     cnstyp[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? 
			    i__5 : s_rnge("cnstyp", i__5, "ekqmgr_", (ftnlen)
			    4097)] == 2;

/*                 The parent table of the LHS column must be the Tth */
/*                 table, or this constraint does not apply. */

/*                 We'll also exclude join constraints.  Note that */
/*                 constraints comparing values from two columns need not */
/*                 be join constraints:  it's possible that the column on */
/*                 the right belongs to the same FROM table as the */
/*                 column on the left. */

		    if (ltbidx[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? i__4 : 
			    s_rnge("ltbidx", i__4, "ekqmgr_", (ftnlen)4109)] 
			    != t) {
			activc[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? i__4 : 
				s_rnge("activc", i__4, "ekqmgr_", (ftnlen)
				4111)] = FALSE_;
			activv[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? i__4 : 
				s_rnge("activv", i__4, "ekqmgr_", (ftnlen)
				4112)] = FALSE_;
		    } else if (cnstyp[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? 
			    i__4 : s_rnge("cnstyp", i__4, "ekqmgr_", (ftnlen)
			    4115)] == 1) {
			if (ltbidx[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? 
				i__4 : s_rnge("ltbidx", i__4, "ekqmgr_", (
				ftnlen)4117)] != rtbidx[(i__5 = i__ - 1) < 
				1000 && 0 <= i__5 ? i__5 : s_rnge("rtbidx", 
				i__5, "ekqmgr_", (ftnlen)4117)]) {

/*                       This is a join constraint; disable it. */

			    activc[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? 
				    i__4 : s_rnge("activc", i__4, "ekqmgr_", (
				    ftnlen)4121)] = FALSE_;
			}
		    }
		}

/*              At this point, we'll have to search the segment for */
/*              matching rows.  Pick a key column for the segment.  To */
/*              do this, we'll need to pack an array with column */
/*              descriptors for each active constraint.  The */
/*              descriptor for the column on the left side of the Ith */
/*              constraint will be placed in elements LDSCRS(*,I), if */
/*              the Ith constraint is active. */

		cleari_(&c__11000, ldscrs);
		i__3 = cjsize;
		for (i__ = 1; i__ <= i__3; ++i__) {
		    if (activv[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? i__4 : 
			    s_rnge("activv", i__4, "ekqmgr_", (ftnlen)4145)]) 
			    {

/*                     Look up the column descriptor for this */
/*                     constraint. */

			j = stdtpt[(i__4 = seg - 1) < 200 && 0 <= i__4 ? i__4 
				: s_rnge("stdtpt", i__4, "ekqmgr_", (ftnlen)
				4150)];
			i__5 = lcidx[(i__4 = i__ - 1) < 1000 && 0 <= i__4 ? 
				i__4 : s_rnge("lcidx", i__4, "ekqmgr_", (
				ftnlen)4152)];
			for (k = 2; k <= i__5; ++k) {
			    j = lnknxt_(&j, dtpool);
			}
			movei_(&dtdscs[(i__5 = j * 11 - 11) < 110000 && 0 <= 
				i__5 ? i__5 : s_rnge("dtdscs", i__5, "ekqmgr_"
				, (ftnlen)4156)], &c__11, &ldscrs[(i__4 = i__ 
				* 11 - 11) < 11000 && 0 <= i__4 ? i__4 : 
				s_rnge("ldscrs", i__4, "ekqmgr_", (ftnlen)
				4156)]);
		    }
		}
		zzekkey_(&sthan[(i__3 = seg - 1) < 200 && 0 <= i__3 ? i__3 : 
			s_rnge("sthan", i__3, "ekqmgr_", (ftnlen)4163)], &
			stdscs[(i__5 = seg * 24 - 24) < 4800 && 0 <= i__5 ? 
			i__5 : s_rnge("stdscs", i__5, "ekqmgr_", (ftnlen)4163)
			], &stnrow[(i__4 = seg - 1) < 200 && 0 <= i__4 ? i__4 
			: s_rnge("stnrow", i__4, "ekqmgr_", (ftnlen)4163)], &
			cjsize, lcidx, ldscrs, ops, dtype, eqryc, cbegs, 
			cends, dvals, ivals, activv, &key, keydsc, &begidx, &
			endidx, &keyfnd, eqryc_len);

/*              ZZEKKEY has updated ACTIVV to reflect the application */
/*              of constraints that were used to determine BEGIDX and */
/*              ENDIDX. */

		if (keyfnd) {
		    indexd = TRUE_;
		} else {

/*                 A key column could not be determined from the */
/*                 active constraints.  We'll use the first column of */
/*                 the segment as the key column. */

		    indexd = FALSE_;
		    begidx = 1;
		    endidx = stnrow[(i__3 = seg - 1) < 200 && 0 <= i__3 ? 
			    i__3 : s_rnge("stnrow", i__3, "ekqmgr_", (ftnlen)
			    4191)];
		}

/*              Whether or not we have any matching rows, we'll need */
/*              to record how many we have.  Save the offset from the */
/*              join row set base for the pointer to the row vectors. */
/*              The row vector count follows this pointer. */

		ptroff = nsv + 4 + (nseg - 1 << 1) + 1;
		if (endidx >= begidx) {

/*                 Initialize the count of matching rows for this */
/*                 segment.  The current stack top is the base address */
/*                 for the row vectors; save the offset of this */
/*                 address from the join row set's base. */
/*                 Also compute the base address of the segment vector */
/*                 for the current segment. */

		    nmatch = 0;
		    zzekstop_(&rwvbas);
		    i__6 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : 
			    s_rnge("rbas", i__3, "ekqmgr_", (ftnlen)4216)] + 
			    ptroff;
		    i__7 = rbas[(i__5 = t - 1) < 10 && 0 <= i__5 ? i__5 : 
			    s_rnge("rbas", i__5, "ekqmgr_", (ftnlen)4216)] + 
			    ptroff;
		    i__8 = rwvbas - rbas[(i__4 = t - 1) < 10 && 0 <= i__4 ? 
			    i__4 : s_rnge("rbas", i__4, "ekqmgr_", (ftnlen)
			    4216)];
		    zzeksupd_(&i__6, &i__7, &i__8);

/*                 Count the active constraints.  While we're at it, */
/*                 fill in the descriptor lists LDSCRS and RDSCRS */
/*                 with, respectively, the descriptors for the columns */
/*                 on the left hand sides and right hand sides of */
/*                 these constraints. */

		    cleari_(&c__11000, ldscrs);
		    cleari_(&c__11000, rdscrs);
		    nact = 0;
		    i__3 = cjsize;
		    for (i__ = 1; i__ <= i__3; ++i__) {
			if (activc[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? 
				i__5 : s_rnge("activc", i__5, "ekqmgr_", (
				ftnlen)4232)] || activv[(i__4 = i__ - 1) < 
				1000 && 0 <= i__4 ? i__4 : s_rnge("activv", 
				i__4, "ekqmgr_", (ftnlen)4232)]) {
			    ++nact;

/*                       Look up the column descriptor for this */
/*                       constraint. */
			    j = stdtpt[(i__5 = seg - 1) < 200 && 0 <= i__5 ? 
				    i__5 : s_rnge("stdtpt", i__5, "ekqmgr_", (
				    ftnlen)4239)];
			    i__4 = lcidx[(i__5 = i__ - 1) < 1000 && 0 <= i__5 
				    ? i__5 : s_rnge("lcidx", i__5, "ekqmgr_", 
				    (ftnlen)4241)];
			    for (k = 2; k <= i__4; ++k) {
				j = lnknxt_(&j, dtpool);
			    }
			    movei_(&dtdscs[(i__4 = j * 11 - 11) < 110000 && 0 
				    <= i__4 ? i__4 : s_rnge("dtdscs", i__4, 
				    "ekqmgr_", (ftnlen)4245)], &c__11, &
				    ldscrs[(i__5 = i__ * 11 - 11) < 11000 && 
				    0 <= i__5 ? i__5 : s_rnge("ldscrs", i__5, 
				    "ekqmgr_", (ftnlen)4245)]);
			    j = stdtpt[(i__4 = seg - 1) < 200 && 0 <= i__4 ? 
				    i__4 : s_rnge("stdtpt", i__4, "ekqmgr_", (
				    ftnlen)4248)];
			    i__5 = rcidx[(i__4 = i__ - 1) < 1000 && 0 <= i__4 
				    ? i__4 : s_rnge("rcidx", i__4, "ekqmgr_", 
				    (ftnlen)4250)];
			    for (k = 2; k <= i__5; ++k) {
				j = lnknxt_(&j, dtpool);
			    }
			    movei_(&dtdscs[(i__5 = j * 11 - 11) < 110000 && 0 
				    <= i__5 ? i__5 : s_rnge("dtdscs", i__5, 
				    "ekqmgr_", (ftnlen)4254)], &c__11, &
				    rdscrs[(i__4 = i__ * 11 - 11) < 11000 && 
				    0 <= i__4 ? i__4 : s_rnge("rdscrs", i__4, 
				    "ekqmgr_", (ftnlen)4254)]);
			}
		    }
		    if (nact > 0) {

/*                    There are still active constraints left, so */
/*                    proceed linearly through the remaining rows, */
/*                    testing each one against these constraints. Add */
/*                    matching rows to the current join row set. */

			i__3 = endidx;
			for (r__ = begidx; r__ <= i__3; ++r__) {
			    if (indexd) {
				zzekixlk_(&sthan[(i__5 = seg - 1) < 200 && 0 
					<= i__5 ? i__5 : s_rnge("sthan", i__5,
					 "ekqmgr_", (ftnlen)4273)], keydsc, &
					r__, &rowidx);
			    } else {

/*                          Look up the record pointer for row R. */

				zzekrplk_(&sthan[(i__5 = seg - 1) < 200 && 0 
					<= i__5 ? i__5 : s_rnge("sthan", i__5,
					 "ekqmgr_", (ftnlen)4281)], &stdscs[(
					i__4 = seg * 24 - 24) < 4800 && 0 <= 
					i__4 ? i__4 : s_rnge("stdscs", i__4, 
					"ekqmgr_", (ftnlen)4281)], &r__, &
					rowidx);
			    }

/*                       Test the row against both value and column */
/*                       constraints.  For now, we supply an array */
/*                       of default column entry element indices. */

			    vmtch = zzekrmch_(&cjsize, activv, &sthan[(i__5 = 
				    seg - 1) < 200 && 0 <= i__5 ? i__5 : 
				    s_rnge("sthan", i__5, "ekqmgr_", (ftnlen)
				    4292)], &stdscs[(i__4 = seg * 24 - 24) < 
				    4800 && 0 <= i__4 ? i__4 : s_rnge("stdscs"
				    , i__4, "ekqmgr_", (ftnlen)4292)], ldscrs,
				     &rowidx, lelts, ops, dtype, eqryc, cbegs,
				     cends, dvals, ivals, eqryc_len);
			    cmtch = TRUE_;

/*                       Note that ZZEKVMCH expects a set of inputs */
/*                       that are not really parallel to those */
/*                       expected by ZZEKRMCH.  We feed the */
/*                       column comparison constraints to ZZEKVMCH */
/*                       one at a time. */

			    i__5 = cjsize;
			    for (j = 1; j <= i__5; ++j) {
				cmtch = cmtch && zzekvmch_(&c__1, &activc[(
					i__4 = j - 1) < 1000 && 0 <= i__4 ? 
					i__4 : s_rnge("activc", i__4, "ekqmg"
					"r_", (ftnlen)4311)], &sthan[(i__6 = 
					seg - 1) < 200 && 0 <= i__6 ? i__6 : 
					s_rnge("sthan", i__6, "ekqmgr_", (
					ftnlen)4311)], &stdscs[(i__7 = seg * 
					24 - 24) < 4800 && 0 <= i__7 ? i__7 : 
					s_rnge("stdscs", i__7, "ekqmgr_", (
					ftnlen)4311)], &ldscrs[(i__8 = j * 11 
					- 11) < 11000 && 0 <= i__8 ? i__8 : 
					s_rnge("ldscrs", i__8, "ekqmgr_", (
					ftnlen)4311)], &rowidx, &c__1, &ops[(
					i__9 = j - 1) < 1000 && 0 <= i__9 ? 
					i__9 : s_rnge("ops", i__9, "ekqmgr_", 
					(ftnlen)4311)], &sthan[(i__10 = seg - 
					1) < 200 && 0 <= i__10 ? i__10 : 
					s_rnge("sthan", i__10, "ekqmgr_", (
					ftnlen)4311)], &stdscs[(i__11 = seg * 
					24 - 24) < 4800 && 0 <= i__11 ? i__11 
					: s_rnge("stdscs", i__11, "ekqmgr_", (
					ftnlen)4311)], &rdscrs[(i__12 = j * 
					11 - 11) < 11000 && 0 <= i__12 ? 
					i__12 : s_rnge("rdscrs", i__12, "ekq"
					"mgr_", (ftnlen)4311)], &rowidx, &c__1)
					;
			    }
			    if (cmtch && vmtch) {

/*                          Push the `augmented row vector' for the */
/*                          current row onto the stack.  In this case, */
/*                          of course, the augmented row vector is */
/*                          trivial:  it consists of the row number, */
/*                          followed by the base address of the parent */
/*                          segment vector. */

				++nmatch;
				zzekspsh_(&c__1, &rowidx);
				i__4 = sgvbas - rbas[(i__5 = t - 1) < 10 && 0 
					<= i__5 ? i__5 : s_rnge("rbas", i__5, 
					"ekqmgr_", (ftnlen)4340)];
				zzekspsh_(&c__1, &i__4);
			    }
			}
		    } else {

/*                    All the rows indicated by BEGIDX and ENDIDX */
/*                    match the constraints.  This code section should */
/*                    be upgraded to transfer the row numbers in */
/*                    chunks. */

			nmatch = endidx - begidx + 1;
			i__3 = endidx;
			for (r__ = begidx; r__ <= i__3; ++r__) {
			    if (indexd) {

/*                          Look up the record pointer for row R */
/*                          from the column index. */

				zzekixlk_(&sthan[(i__5 = seg - 1) < 200 && 0 
					<= i__5 ? i__5 : s_rnge("sthan", i__5,
					 "ekqmgr_", (ftnlen)4364)], keydsc, &
					r__, &rowidx);
			    } else {

/*                          Look up the record pointer for row R. */

				zzekrplk_(&sthan[(i__5 = seg - 1) < 200 && 0 
					<= i__5 ? i__5 : s_rnge("sthan", i__5,
					 "ekqmgr_", (ftnlen)4372)], &stdscs[(
					i__4 = seg * 24 - 24) < 4800 && 0 <= 
					i__4 ? i__4 : s_rnge("stdscs", i__4, 
					"ekqmgr_", (ftnlen)4372)], &r__, &
					rowidx);
			    }
			    zzekspsh_(&c__1, &rowidx);
			    i__4 = sgvbas - rbas[(i__5 = t - 1) < 10 && 0 <= 
				    i__5 ? i__5 : s_rnge("rbas", i__5, "ekqm"
				    "gr_", (ftnlen)4380)];
			    zzekspsh_(&c__1, &i__4);
			}
		    }

/*                 Fill in the row count for this segment in the join row */
/*                 set. */

		    i__4 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : 
			    s_rnge("rbas", i__3, "ekqmgr_", (ftnlen)4390)] + 
			    ptroff + 1;
		    i__6 = rbas[(i__5 = t - 1) < 10 && 0 <= i__5 ? i__5 : 
			    s_rnge("rbas", i__5, "ekqmgr_", (ftnlen)4390)] + 
			    ptroff + 1;
		    zzeksupd_(&i__4, &i__6, &nmatch);
		}

/*              Take a look at the next segment.  Update the total count */
/*              of matching rows for this table. */

		seg = lnknxt_(&seg, stpool);
		rtotal += nmatch;
	    }

/*           Fill in the size and count information for the join row set. */

	    zzekstop_(&top);
	    rsize[(i__3 = t - 1) < 200 && 0 <= i__3 ? i__3 : s_rnge("rsize", 
		    i__3, "ekqmgr_", (ftnlen)4408)] = top - rbas[(i__5 = t - 
		    1) < 10 && 0 <= i__5 ? i__5 : s_rnge("rbas", i__5, "ekqm"
		    "gr_", (ftnlen)4408)];
	    i__6 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4410)] + 1;
	    i__7 = rbas[(i__5 = t - 1) < 10 && 0 <= i__5 ? i__5 : s_rnge(
		    "rbas", i__5, "ekqmgr_", (ftnlen)4410)] + 1;
	    zzeksupd_(&i__6, &i__7, &rsize[(i__4 = t - 1) < 200 && 0 <= i__4 ?
		     i__4 : s_rnge("rsize", i__4, "ekqmgr_", (ftnlen)4410)]);
	    i__4 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4411)] + 2;
	    i__6 = rbas[(i__5 = t - 1) < 10 && 0 <= i__5 ? i__5 : s_rnge(
		    "rbas", i__5, "ekqmgr_", (ftnlen)4411)] + 2;
	    zzeksupd_(&i__4, &i__6, &rtotal);

/*           Compress out any empty segment vectors from the join row */
/*           set. */

	    zzekjsqz_(&rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4417)]);

/*           At this point, we've filled in the entire join row set for */
/*           table T. */

	}

/*        Join the trivial join row sets, producing a final join row set */
/*        for the current conjunction.  Retain the base address of this */
/*        join row set, if it is non-empty.  Update the size of the join */
/*        row set union. */


	resbas = rbas[0];
	i__2 = ntab;
	for (t = 2; t <= i__2; ++t) {

/*           Arm the join constraints!  Turn on the constraints that */
/*           have the Tth table on the one side, and tables */
/*           1, 2, ... , T on the other. */

	    i__3 = cjsize;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		activc[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? i__5 : s_rnge(
			"activc", i__5, "ekqmgr_", (ftnlen)4442)] = FALSE_;
		if (cnstyp[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? i__5 : 
			s_rnge("cnstyp", i__5, "ekqmgr_", (ftnlen)4444)] == 1)
			 {
		    l = ltbidx[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? i__5 : 
			    s_rnge("ltbidx", i__5, "ekqmgr_", (ftnlen)4446)];
		    r__ = rtbidx[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? i__5 :
			     s_rnge("rtbidx", i__5, "ekqmgr_", (ftnlen)4447)];
		    if (l >= 1 && l <= t && r__ >= 1 && r__ <= t && l != r__ 
			    && (r__ == t || l == t)) {
			activc[(i__5 = i__ - 1) < 1000 && 0 <= i__5 ? i__5 : 
				s_rnge("activc", i__5, "ekqmgr_", (ftnlen)
				4457)] = TRUE_;
		    }
		}
	    }

/*           The base address of the first join row set is the base */
/*           address of the result of the previous join.  The first time */
/*           through, the base of the join row set for table 1 is used. */

	    if (t == 2) {
		jbase1 = rbas[0];
	    } else {
		jbase1 = resbas;
	    }
	    jbase2 = rbas[(i__3 = t - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge(
		    "rbas", i__3, "ekqmgr_", (ftnlen)4476)];
	    zzekjoin_(&jbase1, &jbase2, &cjsize, activc, ltbidx, lcidx, lelts,
		     ops, rtbidx, rcidx, relts, sthan, stdscs, stdtpt, dtpool,
		     dtdscs, &resbas, &jsize);
	    zzekjsqz_(&resbas);
	}

/*        At this point, we've found the matching rows for the current */
/*        query conjunction.  Update the size of the join row set union */
/*        corresponding to the current query.  Save the base address of */
/*        the final join row set.  Update the total number of matching */
/*        rows in the join row set union. */

	++usize;
	ubase[(i__2 = usize - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("ubase", 
		i__2, "ekqmgr_", (ftnlen)4496)] = resbas;
	i__2 = resbas + 2;
	i__3 = resbas + 2;
	zzeksrd_(&i__2, &i__3, &cjrows);
	unrows += cjrows;

/*        Remove redundant row vectors from the join row set union. */
/*        These row vectors may arise in the execution of queries whose */
/*        WHERE clauses contain multiple conjunctions. */

	zzekweed_(&usize, ubase, &unrows);

/*        Initialize the addressing function for the current join row */
/*        set union. */

	if (usize > 0) {
	    zzekvset_(&usize, ubase);
	}
    }

/*     At this point, we've formed the join row set union that */
/*     represents the set of row vectors matching the entire query. */

    *nmrows = unrows;

/*     Get the tables and columns of from the SELECT clause.  For */
/*     each qualifying table, we need the index in the FROM clause */
/*     of that table.  For each column, we need the column table */
/*     index. */

    i__1 = nsel;
    for (i__ = 1; i__ <= i__1; ++i__) {
	zzekqsel_(eqryi, eqryc, &i__, &lxbeg, &lxend, tabnam, &tabidx, colnam,
		 &k, eqryc_len, (ftnlen)64, (ftnlen)32);

/*        Locate the column's attribute information.  Retain the column's */
/*        index within the parent table's column list. */

	tab = tptvec[(i__2 = tabidx + 5) < 16 && 0 <= i__2 ? i__2 : s_rnge(
		"tptvec", i__2, "ekqmgr_", (ftnlen)4540)];
	j = tbctpt[(i__2 = tab - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("tbct"
		"pt", i__2, "ekqmgr_", (ftnlen)4541)];
	col = 0;
	fnd = FALSE_;
	while(j > 0 && ! fnd) {
	    ++col;
	    if (s_cmp(ctnams + (((i__2 = j - 1) < 500 && 0 <= i__2 ? i__2 : 
		    s_rnge("ctnams", i__2, "ekqmgr_", (ftnlen)4549)) << 5), 
		    colnam, (ftnlen)32, (ftnlen)32) == 0) {
		fnd = TRUE_;
	    } else {
		j = lnknxt_(&j, ctpool);
	    }
	}
	if (! fnd) {
	    setmsg_("# is not name of a column in FROM table #.", (ftnlen)42);
	    errch_("#", colnam, (ftnlen)1, (ftnlen)32);
	    errint_("#", &tabidx, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("EKSRCH", (ftnlen)6);
	    return 0;
	}
	selctp[(i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : s_rnge("selctp", 
		i__2, "ekqmgr_", (ftnlen)4566)] = j;
	selcol[(i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : s_rnge("selcol", 
		i__2, "ekqmgr_", (ftnlen)4567)] = col;
	seltab[(i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : s_rnge("seltab", 
		i__2, "ekqmgr_", (ftnlen)4568)] = tabidx;
    }

/*     Enable sorting of the matching row vectors, if necessary.  The */
/*     first fetch request will invoke the sort. */

    dosort = norder > 0 && *nmrows > 0;
    sorted = FALSE_;
    if (dosort) {
	i__1 = norder;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    zzekqord_(eqryi, eqryc, &i__, tabnam, &otabs[(i__2 = i__ - 1) < 
		    10 && 0 <= i__2 ? i__2 : s_rnge("otabs", i__2, "ekqmgr_", 
		    (ftnlen)4583)], colnam, &ocols[(i__3 = i__ - 1) < 10 && 0 
		    <= i__3 ? i__3 : s_rnge("ocols", i__3, "ekqmgr_", (ftnlen)
		    4583)], &sense[(i__5 = i__ - 1) < 10 && 0 <= i__5 ? i__5 :
		     s_rnge("sense", i__5, "ekqmgr_", (ftnlen)4583)], 
		    eqryc_len, (ftnlen)64, (ftnlen)32);
	}
    }
    chkout_("EKSRCH", (ftnlen)6);
    return 0;
/* $Procedure     EKNELT  ( EK, get number of elements in column entry ) */

L_eknelt:
/* $ Abstract */

/*     Return the number of elements in a specified column entry in */
/*     the current row. */

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

/*     EK */

/* $ Keywords */

/*     EK */

/* $ Declarations */

/*     INTEGER               SELIDX */
/*     INTEGER               ROW */
/*     INTEGER               NELT */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SELIDX     I   Index of parent column in SELECT clause. */
/*     ROW        I   Row containing element. */
/*     NELT       O   Number of elements in entry in current row. */

/* $ Detailed_Input */

/*     SELIDX         is the SELECT clause index of the column to */
/*                    fetch from. */

/*     ROW            is the index of the row containing the element. */
/*                    This number refers to a member of the set of rows */
/*                    matching a query.  ROW must be in the range */

/*                      1 : NMROWS */

/*                    where NMROWS is the matching row count returned */
/*                    by EKSRCH. */

/* $ Detailed_Output */

/*     NELT           is the number of elements in the column entry */
/*                    belonging to the specified column in the current */
/*                    row. */

/*                    Null entries in variable-size columns are */
/*                    considered to have size 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If this routine is called when no E-kernels have been loaded, */
/*         the error SPICE(NOLOADEDFILES) is signaled. */

/*     2)  If SELIDX is outside of the range established by the */
/*         last query passed to EKSRCH, the error SPICE(INVALIDINDEX) */
/*         will be signaled. */

/*     3)  If ROW is outside of the range established by the */
/*         last query passed to EKSRCH, the error SPICE(INVALIDINDEX) */
/*         will be signaled. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     This routine is meant to be used in conjunction with the EKQMGR */
/*     fetch entry points EKGC, EKGD, and EKGI.  This routine */
/*     allows the caller of those routines to determine appropriate */
/*     loop bounds to use to fetch each column entry in the current row. */

/* $ Examples */

/*     1)  Suppose the EK table TAB contains the following columns: */


/*            Column name   Data Type   Size */
/*            -----------   ---------   ---- */
/*            IARRAY        INT         10 */
/*            DARRAY        DP          VARIABLE */
/*            CARRAY        CHR         VARIABLE */


/*         Suppose the query */

/*            QUERY = 'SELECT IARRAY, DARRAY, CARRAY FROM TAB' */

/*         is issued to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by the fetch routines since we know */
/*         in advance how many elements are contained in each column */
/*         entry we fetch. */


/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column IARRAY in the current */
/*            C        row.  Since IARRAY was the first column selected, */
/*            C        the selection index SELIDX is set to 1. */
/*            C */
/*                     SELIDX = 1 */

/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE ( ( ELTIDX .LE. 10 ) .AND. .NOT. ISNULL ) */
/*            C */
/*            C           If the column entry is null, we'll be kicked */
/*            C           out of this loop after the first iteration. */
/*            C */
/*                        CALL EKGI ( SELIDX,         ROW,     ELTIDX, */
/*                                    IVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = IARRAY' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( IVALS(I), I = 1, 10 ) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column DARRAY in the current */
/*            C        row.  Since DARRAY contains variable-size array */
/*            C        elements, we call EKNELT to determine how many */
/*            C        elements to fetch. */
/*            C */
/*                     SELIDX = 2 */

/*                     CALL EKNELT ( SELIDX, ROW, NELT ) */

/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  NELT   ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGD ( SELIDX,         ROW,     ELTIDX, */
/*                                    DVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = DARRAY' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( DVALS(I), I = 1, NELT ) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column CARRAY in the current */
/*            C        row. */
/*            C */
/*                     SELIDX = 3 */
/*                     CALL EKNELT ( SELIDX, ROW, NELT ) */

/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  NELT   ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGC ( SELIDX,         ROW,     ELTIDX, */
/*                                    CVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = CARRAY' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( CVALS(I), I = 1, NELT ) */
/*                     END IF */

/*                  END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.2.0, 12-FEB-1999 (NJB) */

/*        Bug fix:  There was a error handling branch that called CHKOUT */
/*        where CHKIN should have been called.  This has been fixed. */

/* -    SPICELIB Version 1.1.0, 09-JUL-1996 (NJB) */

/*        Bug fix:  EKNELT now initiates a sort operation if sorted */
/*        outputs are required and EKNELT is called after query */
/*        resolution but before the fetch routines.  Also, addressing */
/*        for sorted query results has been fixed. */

/*        Misspelling of "issued" was fixed.  Previous version line was */
/*        changed from "Beta" to "SPICELIB." */


/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return the number of elements in a column entry */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 12-FEB-1999 (NJB) */

/*        Bug fix:  There was a error handling branch that called CHKOUT */
/*        where CHKIN should have been called.  This has been fixed. */

/* -    SPICELIB Version 1.1.0, 09-JUL-1996 (NJB) */

/*        Bug fix:  EKNELT now initiates a sort operation if sorted */
/*        outputs are required and EKNELT is called after query */
/*        resolution but before the fetch routines.  Also, addressing */
/*        for sorted query results has been fixed.  The fix involved */
/*        copying the sort invocation and addressing code from the */
/*        fetch routines. */

/*        Misspelling of "issued" was fixed.  Previous version line was */
/*        changed from "Beta" to "SPICELIB." */

/* -& */

/*     Use discovery check-in for speed. */

    if (return_()) {
	return 0;
    }

/*     The request doesn't make sense if no files are loaded.  A sure */
/*     symptom of this problem is that the file list is empty. */

    if (fthead <= 0) {
	chkin_("EKNELT", (ftnlen)6);
	setmsg_("No E-kernels are currently loaded.", (ftnlen)34);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("EKNELT", (ftnlen)6);
	return 0;
    }

/*     The row number must be valid, or we can't proceed. */

    if (*row < 1 || *row > unrows) {
	chkin_("EKNELT", (ftnlen)6);
	setmsg_("Row indices for query result range from 1 to #; requested r"
		"ow index was #.", (ftnlen)74);
	errint_("#", &unrows, (ftnlen)1);
	errint_("#", row, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKNELT", (ftnlen)6);
	return 0;
    }

/*     Make sure the SELECT clause column index is valid. */

    if (*selidx < 1 || *selidx > nsel) {
	chkin_("EKNELT", (ftnlen)6);
	setmsg_("The SELECT column index # is out of the valid range 1:#", (
		ftnlen)55);
	errint_("#", selidx, (ftnlen)1);
	errint_("#", &ntab, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKNELT", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, and if it needs to be done, sort the */
/*     matching row vectors. */

    if (dosort) {
	zzekjsrt_(&usize, ubase, &norder, otabs, ocols, oelts, sense, sthan, 
		stdscs, stdtpt, dtpool, dtdscs, &ordbas);
	dosort = FALSE_;
	sorted = TRUE_;
    }

/*     Look up the segment vector and row vector for the current row. */

    if (sorted) {
	i__1 = ordbas + *row;
	i__2 = ordbas + *row;
	zzeksrd_(&i__1, &i__2, &i__);
	zzekvcal_(&i__, &rwvbas, &sgvbas);
    } else {
	zzekvcal_(row, &rwvbas, &sgvbas);
    }
    i__1 = rwvbas + 1;
    i__2 = rwvbas + ntab;
    zzeksrd_(&i__1, &i__2, rowvec);
    i__1 = sgvbas + 1;
    i__2 = sgvbas + ntab;
    zzeksrd_(&i__1, &i__2, segvec);
    tabidx = seltab[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "seltab", i__1, "ekqmgr_", (ftnlen)4981)];
    rowidx = rowvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "rowvec", i__1, "ekqmgr_", (ftnlen)4982)];
    seg = segvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("segv"
	    "ec", i__1, "ekqmgr_", (ftnlen)4983)];
    col = selcol[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge("sel"
	    "col", i__1, "ekqmgr_", (ftnlen)4984)];
    colptr = stdtpt[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("std"
	    "tpt", i__1, "ekqmgr_", (ftnlen)4986)];
    i__1 = col;
    for (i__ = 2; i__ <= i__1; ++i__) {
	colptr = lnknxt_(&colptr, dtpool);
    }
    *nelt = zzekesiz_(&sthan[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : 
	    s_rnge("sthan", i__1, "ekqmgr_", (ftnlen)4992)], &stdscs[(i__2 = 
	    seg * 24 - 24) < 4800 && 0 <= i__2 ? i__2 : s_rnge("stdscs", i__2,
	     "ekqmgr_", (ftnlen)4992)], &dtdscs[(i__3 = colptr * 11 - 11) < 
	    110000 && 0 <= i__3 ? i__3 : s_rnge("dtdscs", i__3, "ekqmgr_", (
	    ftnlen)4992)], &rowidx);
    return 0;
/* $Procedure     EKGC  ( EK, get event data, character ) */

L_ekgc:
/* $ Abstract */

/*     Return an element of an entry in a column of character */
/*     type in a specified row. */

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

/*     EK */

/* $ Keywords */

/*     ASSIGNMENT */
/*     EK */

/* $ Declarations */

/*     INTEGER               SELIDX */
/*     INTEGER               ROW */
/*     INTEGER               ELMENT */
/*     CHARACTER*(*)         CDATA */
/*     LOGICAL               NULL */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SELIDX     I   Index of parent column in SELECT clause. */
/*     ROW        I   Row to fetch from. */
/*     ELMENT     I   Index of element, within column entry, to fetch. */
/*     CDATA      O   Character string element of column entry. */
/*     NULL       O   Flag indicating whether column entry was null. */
/*     FOUND      O   Flag indicating whether column was present in row. */

/* $ Detailed_Input */

/*     SELIDX         is the SELECT clause index of the column to */
/*                    fetch from. */

/*     ROW            is the output row containing the entry to fetch */
/*                    from. */

/*     ELMENT         is the index of the element of the column entry */
/*                    to fetch.  The normal range of ELMENT is from 1 to */
/*                    the size of the column's entry, but ELMENT is */
/*                    allowed to exceed the number of elements in the */
/*                    column entry; if it does, FOUND is returned .FALSE. */
/*                    This allows the caller to read data from the column */
/*                    entry in a loop without checking the number of */
/*                    available elements first. */

/*                    Null values in variable-sized columns are */
/*                    considered to have size 1. */

/* $ Detailed_Output */

/*     CDATA          is the requested element of the specified column */
/*                    entry.  If the entry is null, CDATA is undefined. */

/*                    If CDATA is too short to accommodate the requested */
/*                    column entry element, the element is truncated on */
/*                    the right to fit CDATA.  If CDATA is longer than */
/*                    the element, CDATA is returned blank-padded on */
/*                    the right. */

/*     NULL           is a logical flag indicating whether the entry */
/*                    belonging to the specified column in the specified */
/*                    row is null. */

/*     FOUND          is a logical flag indicating whether the specified */
/*                    element was found.  If the element does not exist, */
/*                    FOUND is returned .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input argument ELMENT is less than 1, FOUND is returned */
/*         .FALSE., and the error SPICE(INVALIDINDEX) is signaled. */
/*         However, ELMENT is allowed to be greater than the number of */
/*         elements in the specified column entry; this allows the caller */
/*         to read data from the column entry in a loop without checking */
/*         the number of available elements first.  If ELMENT is greater */
/*         than the number of available elements, FOUND is returned */
/*         .FALSE. */

/*     2)  If SELIDX is outside of the range established by the */
/*         last query passed to EKSRCH, the error SPICE(INVALIDINDEX) */
/*         will be signaled. */

/*     3)  If the input argument ROW is less than 1 or greater than */
/*         the number of rows matching the query, FOUND is returned */
/*        .FALSE., and the error SPICE(INVALIDINDEX) is signaled. */

/*     4)  If the specified column does not have character type, the */
/*         error SPICE(INVALIDTYPE) is signaled. */

/*     5)  If this routine is called when no E-kernels have been loaded, */
/*         the error SPICE(NOLOADEDFILES) is signaled. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     This routine allows retrieval of data from character columns. */

/*     This routine returns one element at a time in order to save the */
/*     caller from imposing a limit on the size of the column entries */
/*     that can be handled. */

/* $ Examples */

/*     1)  Suppose the EK table TAB contains the following columns: */

/*            Column name   Data Type   Size */
/*            -----------   ---------   ---- */
/*            CHR_COL_1     CHR         1 */
/*            CHR_COL_2     CHR         VARIABLE */
/*            CHR_COL_3     CHR         10 */


/*         Suppose the query */

/*            QUERY = 'SELECT CHR_COL_1 FROM TAB' */

/*         is issued to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by EKGC since we know that every */
/*         entry in column CHR_COL_1 contains one element. */

/*            C */
/*            C     Since CHR_COL_1was the first column selected, */
/*            C     the selection index SELIDX is set to 1. */
/*            C     The column is scalar, so the element index ELTIDX */
/*            C     is set to 1.  The variable NMROWS is the number of */
/*            C     matching rows returned by EKFIND. */
/*            C */

/*                  SELIDX = 1 */
/*                  ELTIDX = 1 */

/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column CHR_COL_1. */
/*            C */
/*                     CALL EKGC ( SELIDX,  ROW,     ELTIDX, */
/*                                 CVAL,    ISNULL,  FOUND   ) */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) CVAL */
/*                     END IF */

/*                  END DO */



/*     2)  Suppose the EK table TAB is as in example 1, and we issue */
/*         the query */

/*            QUERY = 'SELECT CHR_COL_1, CHR_COL_2, CHR_COL_3 FROM TAB' */

/*         to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by EKGC since we know in advance how */
/*         many elements are contained in each column entry we fetch. */


/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column CHR_COL_1.  Since */
/*            C        CHR_COL_1 was the first column selected, the */
/*            C        selection index SELIDX is set to 1. */
/*            C */
/*                     SELIDX = 1 */
/*                     ELTIDX = 1 */
/*                     CALL EKGC ( SELIDX,    ROW,     ELTIDX, */
/*                                 CVALS(1),  ISNULL,  FOUND   ) */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = CHR_COL_1' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) CVALS(1) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column CHR_COL_2 in the current */
/*            C        row.  Since CHR_COL_2 contains variable-size array */
/*            C        elements, we call EKNELT to determine how many */
/*            C        elements to fetch. */
/*            C */
/*                     SELIDX = 2 */
/*                     CALL EKNELT ( SELIDX, ROW, NELT ) */

/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  NELT   ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGC ( SELIDX,         ROW,     ELTIDX, */
/*                                    CVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*            C */
/*            C           If the column entry is null, we'll be kicked */
/*            C           out of this loop after the first iteration. */
/*            C */
/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = CHR_COL_2' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( CVALS(I), I = 1, NELT ) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column CHR_COL_3 in the current */
/*            C        row.  We need not call EKNELT since we know how */
/*            C        many elements are in each column entry. */
/*            C */
/*                     SELIDX = 3 */
/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  10    ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGC ( SELIDX,         ROW,     ELTIDX, */
/*                                    CVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = CHR_COL_3' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( CVALS(I), I = 1, 10 ) */
/*                     END IF */

/*                  END DO */


/*     3)  See the $Examples section of the umbrella routine EKQMGR */
/*         for an example in which the names and data types of the */
/*         columns from which to fetch data are not known in advance. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Redundant CHKIN call removed from SELIDX error check. */
/*        Misspelling of "issued" was fixed.  Previous version line */
/*        was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch element from character column entry */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Redundant CHKIN call removed from SELIDX error check. */
/*        Misspelling of "issued" was fixed.  Previous version line */
/*        was changed from "Beta" to "SPICELIB." */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKGC", (ftnlen)4);
    }

/*     Nothing found yet. */

    *found = FALSE_;

/*     There nothing to fetch if no files are loaded.  A sure */
/*     symptom of this problem is that the file list is empty. */

    if (fthead <= 0) {
	setmsg_("No E-kernels are currently loaded.", (ftnlen)34);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("EKGC", (ftnlen)4);
	return 0;
    }

/*     The row number must be valid, or we can't proceed. */

    if (*row < 1 || *row > unrows) {
	setmsg_("Row indices for query result range from 1 to #; requested r"
		"ow index was #.", (ftnlen)74);
	errint_("#", &unrows, (ftnlen)1);
	errint_("#", row, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGC", (ftnlen)4);
	return 0;
    }

/*     The element index must be positive. */

    if (*elment < 1) {
	setmsg_("ELMENT must be positive but was #.", (ftnlen)34);
	errint_("#", elment, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGC", (ftnlen)4);
	return 0;
    }

/*     Make sure the SELECT clause column index is valid. */

    if (*selidx < 1 || *selidx > nsel) {
	setmsg_("The SELECT column index # is out of the valid range 1:#", (
		ftnlen)55);
	errint_("#", selidx, (ftnlen)1);
	errint_("#", &ntab, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGC", (ftnlen)4);
	return 0;
    }

/*     COL is the column's index within the parent */
/*     table's column list. */

    tabidx = seltab[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "seltab", i__1, "ekqmgr_", (ftnlen)5432)];
    col = selcol[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge("sel"
	    "col", i__1, "ekqmgr_", (ftnlen)5433)];
    colptr = selctp[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "selctp", i__1, "ekqmgr_", (ftnlen)5434)];
    tab = tptvec[(i__1 = tabidx + 5) < 16 && 0 <= i__1 ? i__1 : s_rnge("tptv"
	    "ec", i__1, "ekqmgr_", (ftnlen)5435)];

/*     Make sure the column has character type. */

    if (cttyps[(i__1 = colptr - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("cttyps"
	    , i__1, "ekqmgr_", (ftnlen)5440)] != 1) {
	setmsg_("Column # has data type #.", (ftnlen)25);
	errch_("#", ctnams + (((i__1 = colptr - 1) < 500 && 0 <= i__1 ? i__1 :
		 s_rnge("ctnams", i__1, "ekqmgr_", (ftnlen)5443)) << 5), (
		ftnlen)1, (ftnlen)32);
	errch_("#", chtype + (((i__2 = cttyps[(i__1 = colptr - 1) < 500 && 0 
		<= i__1 ? i__1 : s_rnge("cttyps", i__1, "ekqmgr_", (ftnlen)
		5444)] - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("chtype", i__2, 
		"ekqmgr_", (ftnlen)5444)) << 2), (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("EKGC", (ftnlen)4);
	return 0;
    }

/*     If it hasn't been done yet, and if it needs to be done, sort the */
/*     matching row vectors. */

    if (dosort) {
	zzekjsrt_(&usize, ubase, &norder, otabs, ocols, oelts, sense, sthan, 
		stdscs, stdtpt, dtpool, dtdscs, &ordbas);
	dosort = FALSE_;
	sorted = TRUE_;
    }

/*     Look up the segment vector and row vector for the current row. */

    if (sorted) {
	i__1 = ordbas + *row;
	i__2 = ordbas + *row;
	zzeksrd_(&i__1, &i__2, &i__);
	zzekvcal_(&i__, &rwvbas, &sgvbas);
    } else {
	zzekvcal_(row, &rwvbas, &sgvbas);
    }
    i__1 = rwvbas + 1;
    i__2 = rwvbas + ntab;
    zzeksrd_(&i__1, &i__2, rowvec);
    i__1 = sgvbas + 1;
    i__2 = sgvbas + ntab;
    zzeksrd_(&i__1, &i__2, segvec);

/*     Identify the segment containing the column entry of interest. */
/*     Obtain the column descriptor for the column. */

    rowidx = rowvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "rowvec", i__1, "ekqmgr_", (ftnlen)5482)];
    seg = segvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("segv"
	    "ec", i__1, "ekqmgr_", (ftnlen)5483)];
    j = stdtpt[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("stdtpt", 
	    i__1, "ekqmgr_", (ftnlen)5485)];
    i__1 = col;
    for (i__ = 2; i__ <= i__1; ++i__) {
	j = lnknxt_(&j, dtpool);
    }

/*     Look up the element. */

    zzekrsc_(&sthan[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("sth"
	    "an", i__1, "ekqmgr_", (ftnlen)5494)], &stdscs[(i__2 = seg * 24 - 
	    24) < 4800 && 0 <= i__2 ? i__2 : s_rnge("stdscs", i__2, "ekqmgr_",
	     (ftnlen)5494)], &dtdscs[(i__3 = j * 11 - 11) < 110000 && 0 <= 
	    i__3 ? i__3 : s_rnge("dtdscs", i__3, "ekqmgr_", (ftnlen)5494)], &
	    rowidx, elment, &cvlen, cdata, null, found, cdata_len);
    chkout_("EKGC", (ftnlen)4);
    return 0;
/* $Procedure     EKGD  ( EK, get event data, double precision ) */

L_ekgd:
/* $ Abstract */

/*     Return an element of an entry in a column of double precision */
/*     or `time' type in a specified row. */

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

/*     EK */

/* $ Keywords */

/*     ASSIGNMENT */
/*     EK */

/* $ Declarations */

/*     INTEGER               SELIDX */
/*     INTEGER               ROW */
/*     INTEGER               ELMENT */
/*     DOUBLE PRECISION      DDATA */
/*     LOGICAL               NULL */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SELIDX     I   Index of parent column in SELECT clause. */
/*     ROW        I   Row to fetch from. */
/*     ELMENT     I   Index of element, within column entry, to fetch. */
/*     DDATA      O   D.p. element of column entry. */
/*     NULL       O   Flag indicating whether column entry was null. */
/*     FOUND      O   Flag indicating whether column was present in row. */

/* $ Detailed_Input */

/*     SELIDX         is the SELECT clause index of the column to */
/*                    fetch from. */

/*     ROW            is the output row containing the entry to fetch */
/*                    from. */

/*     ELMENT         is the index of the element of the column entry */
/*                    to fetch.  The normal range of ELMENT is from 1 to */
/*                    the size of the column's entry, but ELMENT is */
/*                    allowed to exceed the number of elements in the */
/*                    column entry; if it does, FOUND is returned .FALSE. */
/*                    This allows the caller to read data from the column */
/*                    entry in a loop without checking the number of */
/*                    available elements first. */

/*                    Null values in variable-sized columns are */
/*                    considered to have size 1. */

/* $ Detailed_Output */

/*     DDATA          is the requested element of the specified column */
/*                    entry.  If the entry is null, DDATA is undefined. */

/*     NULL           is a logical flag indicating whether the entry */
/*                    belonging to the specified column in the specified */
/*                    row is null. */

/*     FOUND          is a logical flag indicating whether the specified */
/*                    element was found.  If the element does not exist, */
/*                    FOUND is returned .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input argument ELMENT is less than 1, FOUND is returned */
/*         .FALSE., and the error SPICE(INVALIDINDEX) is signaled. */
/*         However, ELMENT is allowed to be greater than the number of */
/*         elements in the specified column entry; this allows the caller */
/*         to read data from the column entry in a loop without checking */
/*         the number of available elements first.  If ELMENT is greater */
/*         than the number of available elements, FOUND is returned */
/*         .FALSE. */

/*     2)  If SELIDX is outside of the range established by the */
/*         last query passed to EKSRCH, the error SPICE(INVALIDINDEX) */
/*         will be signaled. */

/*     3)  If the input argument ROW is less than 1 or greater than */
/*         the number of rows matching the query, FOUND is returned */
/*        .FALSE., and the error SPICE(INVALIDINDEX) is signaled. */

/*     4)  If the specified column does not have DP or TIME type, the */
/*         error SPICE(INVALIDTYPE) is signaled. */

/*     5)  If this routine is called when no E-kernels have been loaded, */
/*         the error SPICE(NOLOADEDFILES) is signaled. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     This routine allows retrieval of data from double precision or */
/*     `time' columns. */

/*     This routine returns one element at a time in order to save the */
/*     caller from imposing a limit on the size of the column entries */
/*     that can be handled. */

/* $ Examples */

/*     1)  Suppose the EK table TAB contains the following columns: */

/*            Column name   Data Type   Size */
/*            -----------   ---------   ---- */
/*            DP_COL_1      DP          1 */
/*            DP_COL_2      DP          VARIABLE */
/*            DP_COL_3      DP          10 */
/*            TIME          TIME        1 */


/*         Suppose the query */

/*            QUERY = 'SELECT DP_COL_1 FROM TAB' */

/*         is issued to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by EKGD since we know that every */
/*         entry in column DP_COL_1 contains one element. */

/*            C */
/*            C     Since DP_COL_1was the first column selected, */
/*            C     the selection index SELIDX is set to 1. */
/*            C     The column is scalar, so the element index ELTIDX */
/*            C     is set to 1.  The variable NMROWS is the number of */
/*            C     matching rows returned by EKFIND. */
/*            C */

/*                  SELIDX = 1 */
/*                  ELTIDX = 1 */

/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column DP_COL_1. */
/*            C */
/*                     CALL EKGD ( SELIDX,  ROW,     ELTIDX, */
/*                                 DVAL,    ISNULL,  FOUND   ) */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) DVAL */
/*                     END IF */

/*                  END DO */


/*     2)  Suppose the EK table TAB is as in example 1, and we issue */
/*         the query */

/*            QUERY = 'SELECT TIME FROM TAB' */

/*         to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         We wish to dump the time values as UTC calendar dates. */
/*         The code fragment below carries out this task.  We assume */
/*         a leapseconds kernel is loaded.  The variable UTC shown */
/*         below should be declared as a character string. */

/*                  SELIDX = 1 */
/*                  ELTIDX = 1 */

/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column TIME. */
/*            C */
/*                     CALL EKGD ( SELIDX,  ROW,     ELTIDX, */
/*                                 DVAL,    ISNULL,  FOUND   ) */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        CALL ET2UTC ( DVAL, 'C', 3, UTC ) */
/*                        WRITE (*,*) UTC */
/*                     END IF */

/*                  END DO */


/*     3)  Suppose the EK table TAB is as in example 1, and we issue */
/*         the query */

/*            QUERY = 'SELECT DP_COL_1, DP_COL_2, DP_COL_3 FROM TAB' */

/*         to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by EKGD since we know in advance how */
/*         many elements are contained in each column entry we fetch. */

/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column DP_COL_1.  Since */
/*            C        DP_COL_1was the first column selected, the */
/*            C        selection index SELIDX is set to 1. */
/*            C */
/*                     SELIDX = 1 */
/*                     ELTIDX = 1 */
/*                     CALL EKGD ( SELIDX,    ROW,     ELTIDX, */
/*                                 DVALS(1),  ISNULL,  FOUND   ) */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = DP_COL_1' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) DVALS(1) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column DP_COL_2 in the current */
/*            C        row.  Since DP_COL_2 contains variable-size array */
/*            C        elements, we call EKNELT to determine how many */
/*            C        elements to fetch. */
/*            C */
/*                     SELIDX = 2 */
/*                     CALL EKNELT ( SELIDX, ROW, NELT ) */

/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  NELT   ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGD ( SELIDX,         ROW,     ELTIDX, */
/*                                    DVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*            C */
/*            C           If the column entry is null, we'll be kicked */
/*            C           out of this loop after the first iteration. */
/*            C */
/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = DP_COL_2' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( DVALS(I), I = 1, NELT ) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column DP_COL_3 in the current */
/*            C        row.  We need not call EKNELT since we know how */
/*            C        many elements are in each column entry. */
/*            C */
/*                     SELIDX = 3 */
/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  10    ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGD ( SELIDX,         ROW,     ELTIDX, */
/*                                    DVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = DP_COL_3' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( DVALS(I), I = 1, 10 ) */
/*                     END IF */

/*                  END DO */


/*     4)  See the $Examples section of the umbrella routine EKQMGR */
/*         for an example in which the names and data types of the */
/*         columns from which to fetch data are not known in advance. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Redundant CHKIN call removed from SELIDX error check. */
/*        Misspelling of "issued" was fixed.  Previous version line */
/*        was changed from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch element from double precision column entry */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Redundant CHKIN call removed from SELIDX error check. */
/*        Misspelling of "issued" was fixed.  Previous version line */
/*        was changed from "Beta" to "SPICELIB." */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKGD", (ftnlen)4);
    }

/*     Nothing found yet. */

    *found = FALSE_;

/*     There nothing to fetch if no files are loaded.  A sure */
/*     symptom of this problem is that the file list is empty. */

    if (fthead <= 0) {
	setmsg_("No E-kernels are currently loaded.", (ftnlen)34);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("EKGD", (ftnlen)4);
	return 0;
    }

/*     The row number must be valid, or we can't proceed. */

    if (*row < 1 || *row > unrows) {
	setmsg_("Row indices for query result range from 1 to #; requested r"
		"ow index was #.", (ftnlen)74);
	errint_("#", &unrows, (ftnlen)1);
	errint_("#", row, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGD", (ftnlen)4);
	return 0;
    }

/*     The element index must be positive. */

    if (*elment < 1) {
	setmsg_("ELMENT must be positive but was #.", (ftnlen)34);
	errint_("#", elment, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGD", (ftnlen)4);
	return 0;
    }

/*     Make sure the SELECT clause column index is valid. */

    if (*selidx < 1 || *selidx > nsel) {
	setmsg_("The SELECT column index # is out of the valid range 1:#", (
		ftnlen)55);
	errint_("#", selidx, (ftnlen)1);
	errint_("#", &ntab, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGD", (ftnlen)4);
	return 0;
    }

/*     COL is the column's index within the parent */
/*     table's column list. */

    tabidx = seltab[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "seltab", i__1, "ekqmgr_", (ftnlen)5976)];
    col = selcol[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge("sel"
	    "col", i__1, "ekqmgr_", (ftnlen)5977)];
    colptr = selctp[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "selctp", i__1, "ekqmgr_", (ftnlen)5978)];
    tab = tptvec[(i__1 = tabidx + 5) < 16 && 0 <= i__1 ? i__1 : s_rnge("tptv"
	    "ec", i__1, "ekqmgr_", (ftnlen)5979)];

/*     Make sure the column has double precision or `time' type. */

    if (cttyps[(i__1 = colptr - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("cttyps"
	    , i__1, "ekqmgr_", (ftnlen)5984)] != 2 && cttyps[(i__2 = colptr - 
	    1) < 500 && 0 <= i__2 ? i__2 : s_rnge("cttyps", i__2, "ekqmgr_", (
	    ftnlen)5984)] != 4) {
	setmsg_("Column # has data type #.", (ftnlen)25);
	errch_("#", ctnams + (((i__1 = colptr - 1) < 500 && 0 <= i__1 ? i__1 :
		 s_rnge("ctnams", i__1, "ekqmgr_", (ftnlen)5988)) << 5), (
		ftnlen)1, (ftnlen)32);
	errch_("#", chtype + (((i__2 = cttyps[(i__1 = colptr - 1) < 500 && 0 
		<= i__1 ? i__1 : s_rnge("cttyps", i__1, "ekqmgr_", (ftnlen)
		5989)] - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("chtype", i__2, 
		"ekqmgr_", (ftnlen)5989)) << 2), (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("EKGD", (ftnlen)4);
	return 0;
    }

/*     If it hasn't been done yet, and if it needs to be done, sort the */
/*     matching row vectors. */

    if (dosort) {
	zzekjsrt_(&usize, ubase, &norder, otabs, ocols, oelts, sense, sthan, 
		stdscs, stdtpt, dtpool, dtdscs, &ordbas);
	dosort = FALSE_;
	sorted = TRUE_;
    }

/*     Look up the segment vector and row vector for the current row. */

    if (sorted) {
	i__1 = ordbas + *row;
	i__2 = ordbas + *row;
	zzeksrd_(&i__1, &i__2, &i__);
	zzekvcal_(&i__, &rwvbas, &sgvbas);
    } else {
	zzekvcal_(row, &rwvbas, &sgvbas);
    }
    i__1 = rwvbas + 1;
    i__2 = rwvbas + ntab;
    zzeksrd_(&i__1, &i__2, rowvec);
    i__1 = sgvbas + 1;
    i__2 = sgvbas + ntab;
    zzeksrd_(&i__1, &i__2, segvec);

/*     Identify the segment containing the column entry of interest. */
/*     Obtain the column descriptor for the column. */

    rowidx = rowvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "rowvec", i__1, "ekqmgr_", (ftnlen)6027)];
    seg = segvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("segv"
	    "ec", i__1, "ekqmgr_", (ftnlen)6028)];
    j = stdtpt[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("stdtpt", 
	    i__1, "ekqmgr_", (ftnlen)6030)];
    i__1 = col;
    for (i__ = 2; i__ <= i__1; ++i__) {
	j = lnknxt_(&j, dtpool);
    }

/*     Look up the element. */

    zzekrsd_(&sthan[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("sth"
	    "an", i__1, "ekqmgr_", (ftnlen)6039)], &stdscs[(i__2 = seg * 24 - 
	    24) < 4800 && 0 <= i__2 ? i__2 : s_rnge("stdscs", i__2, "ekqmgr_",
	     (ftnlen)6039)], &dtdscs[(i__3 = j * 11 - 11) < 110000 && 0 <= 
	    i__3 ? i__3 : s_rnge("dtdscs", i__3, "ekqmgr_", (ftnlen)6039)], &
	    rowidx, elment, ddata, null, found);
    chkout_("EKGD", (ftnlen)4);
    return 0;
/* $Procedure     EKGI  ( EK, get event data, integer ) */

L_ekgi:
/* $ Abstract */

/*     Return an element of an entry in a column of integer */
/*     type in a specified row. */

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

/*     EK */

/* $ Keywords */

/*     ASSIGNMENT */
/*     EK */

/* $ Declarations */

/*     INTEGER               SELIDX */
/*     INTEGER               ROW */
/*     INTEGER               ELMENT */
/*     INTEGER               IDATA */
/*     LOGICAL               NULL */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     SELIDX     I   Index of parent column in SELECT clause. */
/*     ROW        I   Row to fetch from. */
/*     ELMENT     I   Index of element, within column entry, to fetch. */
/*     IDATA      O   Integer element of column entry. */
/*     NULL       O   Flag indicating whether column entry was null. */
/*     FOUND      O   Flag indicating whether column was present in row. */

/* $ Detailed_Input */

/*     SELIDX         is the SELECT clause index of the column to */
/*                    fetch from. */

/*     ROW            is the output row containing the entry to fetch */
/*                    from. */

/*     ELMENT         is the index of the element of the column entry */
/*                    to fetch.  The normal range of ELMENT is from 1 to */
/*                    the size of the column's entry, but ELMENT is */
/*                    allowed to exceed the number of elements in the */
/*                    column entry; if it does, FOUND is returned .FALSE. */
/*                    This allows the caller to read data from the column */
/*                    entry in a loop without checking the number of */
/*                    available elements first. */

/*                    Null values in variable-sized columns are */
/*                    considered to have size 1. */

/* $ Detailed_Output */

/*     IDATA          is the requested element of the specified column */
/*                    entry.  If the entry is null, IDATA is undefined. */

/*     NULL           is a logical flag indicating whether the entry */
/*                    belonging to the specified column in the specified */
/*                    row is null. */

/*     FOUND          is a logical flag indicating whether the specified */
/*                    element was found.  If the element does not exist, */
/*                    FOUND is returned .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input argument ELMENT is less than 1, FOUND is returned */
/*         .FALSE., and the error SPICE(INVALIDINDEX) is signaled. */
/*         However, ELMENT is allowed to be greater than the number of */
/*         elements in the specified column entry; this allows the caller */
/*         to read data from the column entry in a loop without checking */
/*         the number of available elements first.  If ELMENT is greater */
/*         than the number of available elements, FOUND is returned */
/*         .FALSE. */

/*     2)  If SELIDX is outside of the range established by the */
/*         last query passed to EKSRCH, the error SPICE(INVALIDINDEX) */
/*         will be signaled. */

/*     3)  If the input argument ROW is less than 1 or greater than */
/*         the number of rows matching the query, FOUND is returned */
/*        .FALSE., and the error SPICE(INVALIDINDEX) is signaled. */

/*     4)  If the specified column does not have integer type, the */
/*         error SPICE(INVALIDTYPE) is signaled. */

/*     5)  If this routine is called when no E-kernels have been loaded, */
/*         the error SPICE(NOLOADEDFILES) is signaled. */

/* $ Files */

/*     See the header of EKQMGR for a description of files used */
/*     by this routine. */

/* $ Particulars */

/*     This routine allows retrieval of data from integer columns. */

/*     This routine returns one element at a time in order to save the */
/*     caller from imposing a limit on the size of the column entries */
/*     that can be handled. */

/* $ Examples */

/*     1)  Suppose the EK table TAB contains the following columns: */

/*            Column name   Data Type   Size */
/*            -----------   ---------   ---- */
/*            INT_COL_1     INT         1 */
/*            INT_COL_2     INT         VARIABLE */
/*            INT_COL_3     INT         10 */


/*         Suppose the query */

/*            QUERY = 'SELECT INT_COL_1 FROM TAB' */

/*         is issued to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by EKGI since we know that every */
/*         entry in column INT_COL_1 contains one element. */

/*            C */
/*            C     Since INT_COL_1was the first column selected, */
/*            C     the selection index SELIDX is set to 1. */
/*            C     The column is scalar, so the element index ELTIDX */
/*            C     is set to 1.  The variable NMROWS is the number of */
/*            C     matching rows returned by EKFIND. */
/*            C */

/*                  SELIDX = 1 */
/*                  ELTIDX = 1 */

/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column INT_COL_1. */
/*            C */
/*                     CALL EKGI ( SELIDX,  ROW,     ELTIDX, */
/*                                 IVAL,    ISNULL,  FOUND   ) */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) IVAL */
/*                     END IF */

/*                  END DO */



/*     2)  Suppose the EK table TAB is as in example 1, and we issue */
/*         the query */

/*            QUERY = 'SELECT INT_COL_1, INT_COL_2, INT_COL_3 FROM TAB' */

/*         to EKFIND via the call */

/*            CALL EKFIND ( QUERY, NMROWS, ERROR, ERRMSG ) */

/*         To fetch and dump column values from the rows that satisfy the */
/*         query, the loop below could be used.  Note that we don't check */
/*         the FOUND flags returned by EKGI since we know in advance how */
/*         many elements are contained in each column entry we fetch. */


/*                  DO ROW = 1, NMROWS */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'ROW  = ', ROW */
/*                     WRITE (*,*) ' ' */

/*            C */
/*            C        Fetch values from column INT_COL_1.  Since */
/*            C        INT_COL_1 was the first column selected, the */
/*            C        selection index SELIDX is set to 1. */
/*            C */
/*                     SELIDX = 1 */
/*                     ELTIDX = 1 */
/*                     CALL EKGI ( SELIDX,    ROW,     ELTIDX, */
/*                                 IVALS(1),  ISNULL,  FOUND   ) */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = INT_COL_1' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) IVALS(1) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column INT_COL_2 in the current */
/*            C        row.  Since INT_COL_2 contains variable-size array */
/*            C        elements, we call EKNELT to determine how many */
/*            C        elements to fetch. */
/*            C */
/*                     SELIDX = 2 */
/*                     CALL EKNELT ( SELIDX, ROW, NELT ) */

/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  NELT   ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGI ( SELIDX,         ROW,     ELTIDX, */
/*                                    IVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*            C */
/*            C           If the column entry is null, we'll be kicked */
/*            C           out of this loop after the first iteration. */
/*            C */
/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = INT_COL_2' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( IVALS(I), I = 1, NELT ) */
/*                     END IF */

/*            C */
/*            C        Fetch values from column INT_COL_3 in the current */
/*            C        row.  We need not call EKNELT since we know how */
/*            C        many elements are in each column entry. */
/*            C */
/*                     SELIDX = 3 */
/*                     ELTIDX = 1 */
/*                     ISNULL = .FALSE. */

/*                     DO WHILE (       ( ELTIDX .LE.  10    ) */
/*                 .              .AND. (        .NOT. ISNULL )  ) */

/*                        CALL EKGI ( SELIDX,         ROW,     ELTIDX, */
/*                                    IVALS(ELTIDX),  ISNULL,  FOUND   ) */

/*                        ELTIDX = ELTIDX + 1 */

/*                     END DO */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'COLUMN = INT_COL_3' */
/*                     WRITE (*,*) ' ' */

/*                     IF ( ISNULL ) THEN */
/*                        WRITE (*,*) '<Null>' */
/*                     ELSE */
/*                        WRITE (*,*) ( IVALS(I), I = 1, 10 ) */
/*                     END IF */

/*                  END DO */


/*     3)  See the $Examples section of the umbrella routine EKQMGR */
/*         for an example in which the names and data types of the */
/*         columns from which to fetch data are not known in advance. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-SEP-2004 (EDW) */

/*        Edited 1.1.0 Version entry to not include */
/*        the token used to mark the $Procedure section. */

/* -    SPICELIB Version 2.0.0, 16-NOV-2001 (NJB) */

/*        Bug fix:   When an already loaded kernel is opened with EKOPR, */
/*        it now has its link count reset to 1 via a call to EKCLS. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Redundant CHKIN call removed from SELIDX error check. */
/*        Misspelling of "issued" was fixed.  Previous version line */
/*        was changed from "Beta" to "SPICELIB."  Header $Procedure */
/*        line was corrected to indicate integer data type. */

/* -    SPICELIB Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch element from integer column entry */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.1, 22-SEP-2004 (EDW) */

/*        Edited 1.1.0 Version entry to not include */
/*        the token used to mark the $Procedure section. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Redundant CHKIN call removed from SELIDX error check. */
/*        Misspelling of "issued" was fixed.  Previous version line */
/*        was changed from "Beta" to "SPICELIB."  Header $Procedure */
/*        line was corrected to indicate integer data type. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKGI", (ftnlen)4);
    }

/*     Nothing found yet. */

    *found = FALSE_;

/*     There nothing to fetch if no files are loaded.  A sure */
/*     symptom of this problem is that the file list is empty. */

    if (fthead <= 0) {
	setmsg_("No E-kernels are currently loaded.", (ftnlen)34);
	sigerr_("SPICE(NOLOADEDFILES)", (ftnlen)20);
	chkout_("EKGI", (ftnlen)4);
	return 0;
    }

/*     The row number must be valid, or we can't proceed. */

    if (*row < 1 || *row > unrows) {
	setmsg_("Row indices for query result range from 1 to #; requested r"
		"ow index was #.", (ftnlen)74);
	errint_("#", &unrows, (ftnlen)1);
	errint_("#", row, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGI", (ftnlen)4);
	return 0;
    }

/*     The element index must be positive. */

    if (*elment < 1) {
	setmsg_("ELMENT must be positive but was #.", (ftnlen)34);
	errint_("#", elment, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGI", (ftnlen)4);
	return 0;
    }

/*     Make sure the SELECT clause column index is valid. */

    if (*selidx < 1 || *selidx > nsel) {
	setmsg_("The SELECT column index # is out of the valid range 1:#", (
		ftnlen)55);
	errint_("#", selidx, (ftnlen)1);
	errint_("#", &ntab, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("EKGI", (ftnlen)4);
	return 0;
    }

/*     COL is the column's index within the parent */
/*     table's column list. */

    tabidx = seltab[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "seltab", i__1, "ekqmgr_", (ftnlen)6492)];
    col = selcol[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge("sel"
	    "col", i__1, "ekqmgr_", (ftnlen)6493)];
    colptr = selctp[(i__1 = *selidx - 1) < 50 && 0 <= i__1 ? i__1 : s_rnge(
	    "selctp", i__1, "ekqmgr_", (ftnlen)6494)];
    tab = tptvec[(i__1 = tabidx + 5) < 16 && 0 <= i__1 ? i__1 : s_rnge("tptv"
	    "ec", i__1, "ekqmgr_", (ftnlen)6495)];

/*     Make sure the column has integer type. */

    if (cttyps[(i__1 = colptr - 1) < 500 && 0 <= i__1 ? i__1 : s_rnge("cttyps"
	    , i__1, "ekqmgr_", (ftnlen)6500)] != 3) {
	setmsg_("Column # has data type #.", (ftnlen)25);
	errch_("#", ctnams + (((i__1 = colptr - 1) < 500 && 0 <= i__1 ? i__1 :
		 s_rnge("ctnams", i__1, "ekqmgr_", (ftnlen)6503)) << 5), (
		ftnlen)1, (ftnlen)32);
	errch_("#", chtype + (((i__2 = cttyps[(i__1 = colptr - 1) < 500 && 0 
		<= i__1 ? i__1 : s_rnge("cttyps", i__1, "ekqmgr_", (ftnlen)
		6504)] - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("chtype", i__2, 
		"ekqmgr_", (ftnlen)6504)) << 2), (ftnlen)1, (ftnlen)4);
	sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	chkout_("EKGI", (ftnlen)4);
	return 0;
    }

/*     If it hasn't been done yet, and if it needs to be done, sort the */
/*     matching row vectors. */

    if (dosort) {
	zzekjsrt_(&usize, ubase, &norder, otabs, ocols, oelts, sense, sthan, 
		stdscs, stdtpt, dtpool, dtdscs, &ordbas);
	dosort = FALSE_;
	sorted = TRUE_;
    }

/*     Look up the segment vector and row vector for the current row. */

    if (sorted) {
	i__1 = ordbas + *row;
	i__2 = ordbas + *row;
	zzeksrd_(&i__1, &i__2, &i__);
	zzekvcal_(&i__, &rwvbas, &sgvbas);
    } else {
	zzekvcal_(row, &rwvbas, &sgvbas);
    }
    i__1 = rwvbas + 1;
    i__2 = rwvbas + ntab;
    zzeksrd_(&i__1, &i__2, rowvec);
    i__1 = sgvbas + 1;
    i__2 = sgvbas + ntab;
    zzeksrd_(&i__1, &i__2, segvec);

/*     Identify the segment containing the column entry of interest. */
/*     Obtain the column descriptor for the column. */

    rowidx = rowvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "rowvec", i__1, "ekqmgr_", (ftnlen)6542)];
    seg = segvec[(i__1 = tabidx - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("segv"
	    "ec", i__1, "ekqmgr_", (ftnlen)6543)];
    j = stdtpt[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("stdtpt", 
	    i__1, "ekqmgr_", (ftnlen)6545)];
    i__1 = col;
    for (i__ = 2; i__ <= i__1; ++i__) {
	j = lnknxt_(&j, dtpool);
    }

/*     Look up the element. */

    zzekrsi_(&sthan[(i__1 = seg - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge("sth"
	    "an", i__1, "ekqmgr_", (ftnlen)6554)], &stdscs[(i__2 = seg * 24 - 
	    24) < 4800 && 0 <= i__2 ? i__2 : s_rnge("stdscs", i__2, "ekqmgr_",
	     (ftnlen)6554)], &dtdscs[(i__3 = j * 11 - 11) < 110000 && 0 <= 
	    i__3 ? i__3 : s_rnge("dtdscs", i__3, "ekqmgr_", (ftnlen)6554)], &
	    rowidx, elment, idata, null, found);
    chkout_("EKGI", (ftnlen)4);
    return 0;
} /* ekqmgr_ */

/* Subroutine */ int ekqmgr_(integer *cindex, integer *elment, char *eqryc, 
	doublereal *eqryd, integer *eqryi, char *fname, integer *row, integer 
	*selidx, char *column, integer *handle, integer *n, char *table, 
	integer *attdsc, integer *ccount, logical *found, integer *nelt, 
	integer *nmrows, logical *semerr, char *errmsg, char *cdata, 
	doublereal *ddata, integer *idata, logical *null, ftnlen eqryc_len, 
	ftnlen fname_len, ftnlen column_len, ftnlen table_len, ftnlen 
	errmsg_len, ftnlen cdata_len)
{
    return ekqmgr_0_(0, cindex, elment, eqryc, eqryd, eqryi, fname, row, 
	    selidx, column, handle, n, table, attdsc, ccount, found, nelt, 
	    nmrows, semerr, errmsg, cdata, ddata, idata, null, eqryc_len, 
	    fname_len, column_len, table_len, errmsg_len, cdata_len);
    }

/* Subroutine */ int eklef_(char *fname, integer *handle, ftnlen fname_len)
{
    return ekqmgr_0_(1, (integer *)0, (integer *)0, (char *)0, (doublereal *)
	    0, (integer *)0, fname, (integer *)0, (integer *)0, (char *)0, 
	    handle, (integer *)0, (char *)0, (integer *)0, (integer *)0, (
	    logical *)0, (integer *)0, (integer *)0, (logical *)0, (char *)0, 
	    (char *)0, (doublereal *)0, (integer *)0, (logical *)0, (ftnint)0,
	     fname_len, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ekuef_(integer *handle)
{
    return ekqmgr_0_(2, (integer *)0, (integer *)0, (char *)0, (doublereal *)
	    0, (integer *)0, (char *)0, (integer *)0, (integer *)0, (char *)0,
	     handle, (integer *)0, (char *)0, (integer *)0, (integer *)0, (
	    logical *)0, (integer *)0, (integer *)0, (logical *)0, (char *)0, 
	    (char *)0, (doublereal *)0, (integer *)0, (logical *)0, (ftnint)0,
	     (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ekntab_(integer *n)
{
    return ekqmgr_0_(3, (integer *)0, (integer *)0, (char *)0, (doublereal *)
	    0, (integer *)0, (char *)0, (integer *)0, (integer *)0, (char *)0,
	     (integer *)0, n, (char *)0, (integer *)0, (integer *)0, (logical 
	    *)0, (integer *)0, (integer *)0, (logical *)0, (char *)0, (char *)
	    0, (doublereal *)0, (integer *)0, (logical *)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ektnam_(integer *n, char *table, ftnlen table_len)
{
    return ekqmgr_0_(4, (integer *)0, (integer *)0, (char *)0, (doublereal *)
	    0, (integer *)0, (char *)0, (integer *)0, (integer *)0, (char *)0,
	     (integer *)0, n, table, (integer *)0, (integer *)0, (logical *)0,
	     (integer *)0, (integer *)0, (logical *)0, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (logical *)0, (ftnint)0, (ftnint)0, 
	    (ftnint)0, table_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ekccnt_(char *table, integer *ccount, ftnlen table_len)
{
    return ekqmgr_0_(5, (integer *)0, (integer *)0, (char *)0, (doublereal *)
	    0, (integer *)0, (char *)0, (integer *)0, (integer *)0, (char *)0,
	     (integer *)0, (integer *)0, table, (integer *)0, ccount, (
	    logical *)0, (integer *)0, (integer *)0, (logical *)0, (char *)0, 
	    (char *)0, (doublereal *)0, (integer *)0, (logical *)0, (ftnint)0,
	     (ftnint)0, (ftnint)0, table_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ekcii_(char *table, integer *cindex, char *column, 
	integer *attdsc, ftnlen table_len, ftnlen column_len)
{
    return ekqmgr_0_(6, cindex, (integer *)0, (char *)0, (doublereal *)0, (
	    integer *)0, (char *)0, (integer *)0, (integer *)0, column, (
	    integer *)0, (integer *)0, table, attdsc, (integer *)0, (logical *
	    )0, (integer *)0, (integer *)0, (logical *)0, (char *)0, (char *)
	    0, (doublereal *)0, (integer *)0, (logical *)0, (ftnint)0, (
	    ftnint)0, column_len, table_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int eksrch_(integer *eqryi, char *eqryc, doublereal *eqryd, 
	integer *nmrows, logical *semerr, char *errmsg, ftnlen eqryc_len, 
	ftnlen errmsg_len)
{
    return ekqmgr_0_(7, (integer *)0, (integer *)0, eqryc, eqryd, eqryi, (
	    char *)0, (integer *)0, (integer *)0, (char *)0, (integer *)0, (
	    integer *)0, (char *)0, (integer *)0, (integer *)0, (logical *)0, 
	    (integer *)0, nmrows, semerr, errmsg, (char *)0, (doublereal *)0, 
	    (integer *)0, (logical *)0, eqryc_len, (ftnint)0, (ftnint)0, (
	    ftnint)0, errmsg_len, (ftnint)0);
    }

/* Subroutine */ int eknelt_(integer *selidx, integer *row, integer *nelt)
{
    return ekqmgr_0_(8, (integer *)0, (integer *)0, (char *)0, (doublereal *)
	    0, (integer *)0, (char *)0, row, selidx, (char *)0, (integer *)0, 
	    (integer *)0, (char *)0, (integer *)0, (integer *)0, (logical *)0,
	     nelt, (integer *)0, (logical *)0, (char *)0, (char *)0, (
	    doublereal *)0, (integer *)0, (logical *)0, (ftnint)0, (ftnint)0, 
	    (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ekgc_(integer *selidx, integer *row, integer *elment, 
	char *cdata, logical *null, logical *found, ftnlen cdata_len)
{
    return ekqmgr_0_(9, (integer *)0, elment, (char *)0, (doublereal *)0, (
	    integer *)0, (char *)0, row, selidx, (char *)0, (integer *)0, (
	    integer *)0, (char *)0, (integer *)0, (integer *)0, found, (
	    integer *)0, (integer *)0, (logical *)0, (char *)0, cdata, (
	    doublereal *)0, (integer *)0, null, (ftnint)0, (ftnint)0, (ftnint)
	    0, (ftnint)0, (ftnint)0, cdata_len);
    }

/* Subroutine */ int ekgd_(integer *selidx, integer *row, integer *elment, 
	doublereal *ddata, logical *null, logical *found)
{
    return ekqmgr_0_(10, (integer *)0, elment, (char *)0, (doublereal *)0, (
	    integer *)0, (char *)0, row, selidx, (char *)0, (integer *)0, (
	    integer *)0, (char *)0, (integer *)0, (integer *)0, found, (
	    integer *)0, (integer *)0, (logical *)0, (char *)0, (char *)0, 
	    ddata, (integer *)0, null, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int ekgi_(integer *selidx, integer *row, integer *elment, 
	integer *idata, logical *null, logical *found)
{
    return ekqmgr_0_(11, (integer *)0, elment, (char *)0, (doublereal *)0, (
	    integer *)0, (char *)0, row, selidx, (char *)0, (integer *)0, (
	    integer *)0, (char *)0, (integer *)0, (integer *)0, found, (
	    integer *)0, (integer *)0, (logical *)0, (char *)0, (char *)0, (
	    doublereal *)0, idata, null, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

