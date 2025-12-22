MODULE MAIN1
!***********************************************************************
!     MAIN1
!     AERMOD Model Data - Parameter, Variable and Array Declarations
!                         Global Data for All Modules
!
!***********************************************************************

   IMPLICIT NONE

! ----------------------------------------------------------------------
! --- AERMOD
!
!     Modified to accept additional building data required by
!     the PRIME building downwash modules
!
!     Changes are denoted in comment fields
!
!     Prepared by    Earth Tech, Inc
!     Prepared for   EPRI under contract WO3527-01
! ----------------------------------------------------------------------

!***********************************************************************
!     User Specified Model Parameters for Array Dimensions
!***********************************************************************

! --- Most array limits for data storage are now allocated at runtime.
   INTEGER, PARAMETER :: NWSCAT= 6, NKST= 6, NHR= 24,&
   &NPAIR= 100, NHIANN= 10,&
   &NMXPM= 10, MXPLVL=50, MXGLVL=87

   INTEGER :: NYEARS

!**   NWSCAT = Number of Wind Speed Categories
!**   NKST   = Number of Stability Categories
!**   NHR    = Number of Hours in Met Data Loop
!**   NPAIR  = Number of Pairs of TXCONC and IDCONC for TOXXFILE Output
!**   NHIANN = Number of high period/annual averages to include in the
!**                   summary page of the output file (formerly controlled
!**                   by NVAL)
!**   NMXPM  = Number of high average High-N-High 24-hour PM2.5 averages or
!**                   High-N-High 1-hour NO2 or SO2 averages to include in the
!**                   summary table for PM-2.5/NO2/SO2 processing across years
!**   MXPLVL = Maximum number of levels in the observed profile file
!**   MXGLVL = Maximum number of levels in the gridded profiles (0 - 4000 m)
!**   NYEARS = Number of Years allowed for multi-year analyses for PM2.5,
!**                   1-hour NO2 and 1-hour SO2 standards, which are averaged
!**                   over the number of years modeled; a default value of 5
!**                   years is assumed, but the user can override the default
!**                   using the ME NUMYEARS keyword; e.g., setting NYEARS = 1
!**                   on the ME NUMYEARS keyword for 1 year of site-specific
!**                   met data will significantly reduce the memory requirements
!**                   for the MAXDCONT option.

!** The following array limits are set dynamically at runtime:
!**   NSRC   = Max Number of Sources
!**   NREC   = Max Number of Receptors
!**   NGRP   = Max Number of Source Groups
!**   NAVE   = Max Number of Short Term Averaging Periods
!**   NVAL   = Max Number of High Values by Receptor (RECTABLE Keyword)
!**   NTYP   = Max Number of Output Types per Run (CONC, DEPOS, DDEP and WDEP)
!**   NMAX   = Max Number of Overall Maximum Values (MAXTABLE Keyword)
!**   NSEC   = Number of Sectors for Building Dimensions
!**   NQF    = Number of Variable Emission Rate Factors Per Source
!**   NBF    = Number of Temporally Varying Background Concentrations
!**   NO3F   = Number of Temporally Varying Ozone Concentrations
!**   NNOXF   = Number of Temporally Varying NOx Concentrations
!**   NPDMAX = Max Number of Particle Diameter Categories Per Source
!**   NVMAX  = Max Number of Vertices for AREA/AREACIRC/AREAPOLY and/or
!**            OPENPIT Sources
!**   IXM    = Max Number of X-coord (Distance) Values Per Receptor Network
!**   IYM    = Max Number of Y-coord (Direction) Values Per Receptor Network
!**   NNET   = Max Number of Cartesian and/or Polar Receptor Networks
!**   NARC   = Maximum number of Receptor Groupings ('ARCs') for EVALCART
!**   NURB   = Maximum number of Urban Areas


!***********************************************************************
!     Programmer Specified Model Parameters
!***********************************************************************


!JAT  06/10/2020:  ISSUE D53, ADDED FROM 19191; ADD 1 TO IERRN TO
!     REFLECT NEW ERROR CODE 126
!CRT  02/16/2021:  ISSUE D062 - MINSW, BIGT, ADD 2 TO IERRN TO
!     REFLECT NEW ERROR CODES AND AVOID CONFLICTS WITH PAST CODE MERGES
!      INTEGER, PARAMETER :: IFMAX=150, IKN=102, ISTRG=512, ILEN_FLD=200,
!     &                      IERRN=317
!CRCO 2/18/2021 - need to check IKN and IERRN after merge
!      INTEGER, PARAMETER :: IFMAX=150, IKN=117, ISTRG=512, ILEN_FLD=200,
!     &                      IERRN=500
   INTEGER, PARAMETER :: IFMAX=150, IKN=120, ISTRG=512, ILEN_FLD=200,&
   &IERRN=500
!**   IFMAX  = Max Number of Fields Per Runstream Record
!**   IKN    = Number of Keywords
!**   ISTRG  = Max Length of Runstream Image Record
!**   ILEN_FLD = Max Length of Runstream Input Fields.  Also used
!**              to specify length of input filenames and formats.
!**   IERRN  = Array length of Error/Warning/Informational Message arrays


!***********************************************************************
!     Model Constants Specified as Parameters
!***********************************************************************

   DOUBLE PRECISION, PARAMETER ::&
   &G = 9.80616D0,       VONKAR = 0.4D0,&
   &GOVRCP = 0.00977D0,  DCTODK = 273.16D0,&
   &BETA1  = 0.6D0,      BETA2  = 0.4D0,&
   &AT1PT2 = 1.2D0,      UMINGR = 0.01D0,&
   &GSIGV  = 0.073864D0, EFOLDH = 0.44D0,&
!**   CRT 9/11/2020, D062 User Minimum Sigma W
!**     &                    SVUMIN = 0.05D0,     SWMIN  = 0.02D0,
   &SVUMIN = 0.05D0,&
   &XVAL   = 0.0D0,      SPTGMN = 0.002D0,&
   &BSUBC  = 0.5D0,      ALPHAR = 1.4D0,&
   &LAMDAY = 2.3D0,      ASUBE  = 0.1D0,&
   &REFPOP = 2.0D+6,     DELTRUR= 12.0D0,&
   &RGAS   = 8.3145D0,&
   &AWMA_BETA_ENTRAINCOEFF = 0.35D0,&
!**  Added for Aircraft Plume Rise; UNC-IE
   &R00 = 2.0D0, ALPHAM = 0.10D0,&
   &PAA = 1.013D+5, RAA = 287.058D0,&
   &HFUEL = 4.3D+7, CPA = 1003.0D0
!    Description of the variables/constants used in ARISE
!    R00    = Aircraft Engine Exhaust Radius (m)(as Initial Plume Radius)
!    ALPHAM = Entrainment Constant (Momentum Part of ARISE)
!    PAA    = Ambient Pressure (N/m2)
!    RAA    = Gas Constant of Air (J/kg-K)
!    HFUEL  = Heating Value of Fuel (J/kg)
!    CPA    = Specific Heat of Air (J/kg-K)
!**  End Aircraft Plume Rise insert; April 2023

! --- AWMA version D20350
!     AWMA_Beta_EntrainCoeff  = entrainment coefficient for PRIME downwash;
!                  changes valuefrom m 0.60 for bet0 and betap when
!                  ALPHA option AWMAEntrain is specified  (July 2020)

   DOUBLE PRECISION :: SZCOEF    ! SZCOEF

!**   Declare SVMIN variable for minimum sigma-v, formerly specified as
!**   PARAMETER SVMIN.  Default value of 0.2D0 is initialized in SUBROUTINE
!**   MODOPT; (SVMIN was increased to 0.5D0 under the old LOWWIND1 BETA option,
!**   and increased to 0.3D0 under the old LOWWIND2 and LOWWIND3 options)
!**   optional user inputs under the CO LOW_WIND keyword.
   DOUBLE PRECISION :: SVMIN

!**   Declare WSMIN variable for minimum wind speed, formerly set to 0.2828
!**   based on SQRT(2*SVmin*SVmin), where SVmin was 0.2. The default value
!**   of WSMIN is also set to 0.2828, but may be adjusted under the LOWWIND
!**   BETA options using the CO LOW_WIND keyword.
   DOUBLE PRECISION :: WSMIN

!**   Declare FRANMAX variable for maximum meander factor, FRAN, used in
!**   the LowWind2 BETA option.  The "default" value for the LowWind2
!**   option is set at 0.95, but can be modified by the user under the
!**   optional LOW_WIND keyword on the CO pathway, within a range of
!**   0.50 to 1.0, inclusive.
   DOUBLE PRECISION :: FRANMAX

!**   3/18/2022 D127 - add FRANMIN for meander testing -Wood
   DOUBLE PRECISION :: FRANMIN

!**   CRT 9/11/2020, D062 User Minimum Sigma W
!**   Declare SWMIN variable for user-defined minimum sigma-w formerly
!**   specified as PARAMETER SWMIN.  The default value will be set to
!**   the former parameter value = 0.02 m/s, but can be modified under
!**   the optional LOW_WIND keyword on the CO pathway within a range of
!**   0.00 to 3.00 which is consistent with the default range check by
!**   AERMET when SWnn is included as an onsite variable.
   DOUBLE PRECISION :: SWMIN

!**   RCO 9/27/2020, D061 User BIGT values
!**   Declare BIGT variable for user-defined BIGT value, formerly
!**   specified as PARAMETER BIGT.  The default value will be set to
!**   the former parameter value = 12 hours, but can be modified under
!**   the optional LOW_WIND keyword on the CO pathway within a range of
!**   0.50 to 48.00. Previous LOWWIND2 option set at 12 hours, not
   DOUBLE PRECISION :: BIGT


!**   Set concentration unit conversion factors for NO2, SO2, CO, and 03
!**   for use with OZONEVAL, OZONEFIL, O3VALUES, and BACKGRND keywords;
!**   factors defined by pollutant for PPB-to-UG/M3 and PPM-to-UG/M3,
!**   based on reference temperature (25 C) and pressure (1013.25 mb).
!**   Note that factors for NO2 and SO2 are PPB/(UG/M3) and PPM/(UG/M3),
!**   and factors for CO and O3 are for (UG/M3)/PPB and (UG/M3)/PPM.
   DOUBLE PRECISION, PARAMETER ::&
   &NO2_PPB = 0.5319D0, NO2_PPM = 0.5319D-3,&
   &SO2_PPB = 0.3823D0, SO2_PPM = 0.3823D-3,&
   &CO_PPB  = 1.144D0,  CO_PPM  = 1.144D3,&
   &O3_PPB  = 1.960D0,  O3_PPM  = 1.960D3


   DOUBLE PRECISION :: PI, TWOPI, RTOFPI, SRT2PI, RTOF2,&
   &RTPIBY2, RT2BYPI,&
   &DTORAD, RTODEG,&
   &THIRD, TWOTHIRDS

!**   PI     = PI               ! Initialized in sub.VARINI as 4.0D0*DATAN(1.0D0)
!**   TWOPI  = 2.*PI            ! Initialized in sub.VARINI
!**   RTOFPI = SQRT(PI)         ! Initialized in sub.VARINI
!**   SRT2PI = SQRT(2.*PI)      ! Initialized in sub.VARINI
!**   RTOF2  = SQRT(2.)         ! Initialized in sub.VARINI
!**   RTPIBY2= SQRT(PI/2.)      ! Initialized in sub.VARINI
!**   RT2BYPI= SQRT(2./PI)      ! Initialized in sub.VARINI
!**   DTORAD = Degrees to Radians Conversion   ! Initialized in sub.VARINI as PI/180.0D0
!**   RTODEG = Radians to Degrees Conversion   ! Initialized in sub.VARINI as 180.0D0/PI
!**   THIRD  = 1.0/3.0          ! Initialized in sub.VARINI
!**   TWOTHIRDS  = 2.0/3.0      ! Initialized in sub.VARINI

!**   G      = Acceleration Due to Gravity (m/s**2)
!**   VONKAR = von Karman constant
!**   GOVRCP = Gravity divided by specific heat at constant pressure
!**   DCTODK = Degrees Celsius to kelvin conversion factor
!**   BETA1  = Coeff. in the calculation of 'direct' plume rise
!**   BETA2  = Coeff. in the calculation of buoyancy-induced dispersion
!**   AT1PT2 = The fraction of the mixed layer and above the mixed
!              layer through which a variable changes its value
!**   UMINGR = Minimum value for a gridded wind speed
!**   GSIGV  = Constant used in converting sigma_A to sigma_V
!**   EFOLDH = Constant in computation of dTHETA/dZ in stable atmosphere
!**   SVUMIN = Minimum value applied to Sigma_V / U when calculating
!**            Sigma_Y.
!**   SVMIN  = Minimum value applied to measured Sigma_V values
!**   SWMIN  = Minimum value applied to measured Sigma_W values
!**   BIGT   = Random scaling time parameter
!**   XVAL   = Gradient in the miXing layer (unstable atmosphere)
!**   SPTGMN = Minimum vert. potential temp. gradient for stable atmosphere
!**   BSUBC  = Constant used in computing sigma_Z for surface layer releases
!**   SZCOEF = Coefficient of sigma-z to define the plume half-width
!**   ALPHAR = Parameter used in January 18, 1995 indirect source MCA
!**   LAMDAY = Parameter used in January 18, 1995 indirect source MCA
!**   ASUBE  = Parameter used in January 18, 1995 indirect source MCA
!**   REFPOP = Reference population for urban option (4/1/96 MCA)
!**   DELTRUR= Surface cooling in the rural area (4/1/96 MCA)
!**   RGAS   = ideal gas law constant = 8.3145 Pa-m^3/mol-K
!**

!**   NUMSYEFF = Number of effective sigma-y's to use with the
!**              FASTALL non-DFAULT option;
!**              receptors more than NUMSYEFF*SYEFF off the plume
!**              centerline are skipped to optimize runtime
   DOUBLE PRECISION, PARAMETER :: NUMSYEFF  = 4.0D0

!**   MAXDIST = Maximum transport distance for calculation;
!**             set to 80km for FASTALL or FASTAREA options;
!**             this was formerly associated with the TOXICS
!**             option, which is now obsolete.
!**             Set to 1.0D20 for applications w/o FASTALL or FASTAREA
   DOUBLE PRECISION :: MAXDIST


!***********************************************************************
!     Common Block for Input/Output File Units (Initialized in BLOCK DATA)
!***********************************************************************

!CRT  D063 Add Platform Downwash Debug PLATFMDBUNT
!     Wood 10/10/22 added GRID_WS debug RLINEDBUNT_WS
   INTEGER :: INUNIT, IOUNIT, MFUNIT, MPUNIT, IERUNT, IERWRT,&
   &IDPUNT, IDPUN2, IRSUNT, IEVUNT, ITEVUT, IHREMI,&
   &IBGUNT(6), IO3UNT(6), INCUNT, ISUMUNT, DBGUNT, DBMUNT,&
   &AREADBUNT, GDEPDBG, PDEPDBG, PRMDBUNT, PVMDBG, OLMDBG,&
   &ARM2DBG, RDISPUNT, AWMADWDBUNT, GRSMDBG, INOXUNT(6),&
   &TTRMUNT, TTRM2TMP(3),&
   &RLINEDBUNT, URBUNT, URBUNT1, URBUNT2, BLPUNT,&
   &PLATFMDBUNT, SWDBGUNT, RLINEDBUNT_WS,&
   &ARCFTDBG,&   ! Added for Aircraft Plume Rise; UNC-IE
   &HBPUNT ! Added for HBPDEBUG

!**   These input/output file units are initialized below in a DATA statement
!**   INUNIT = Input Runstream File Unit (Initialized to 7)
!**   IOUNIT = Main Printed Output File Unit (Initialized to 8)
!**   PVMDBG = PVMRM debug option file unit (initialized to 9)
!**   OLMDBG = OLM debug option file unit (initialized to 9)
!**   ARM2DBG = ARM2 debug option file unit (initialized to 9)
!**   GRSMDBG = GRSM debug option file unit (initialized to 9)
!**   IERUNT = Temporary Error/Message File Unit (Initialized to 10)
!**   IERWRT = Permanent Detailed Error/Message File Unit (Init. to 11)
!**   IDPUNT = Main SAVEFILE Unit for Re-start Option (Init. to 12)
!**   IDPUN2 = Secondary SAVEFILE Unit for Re-start Option (Init. to 14)
!**   IRSUNT = INITFILE Unit for Re-start Option (Initialized to 15)
!**   IHREMI = Hourly Emission Parameters File Unit (Init. to 16)
!**   IEVUNT = Event File Unit for Use With EVENT Model Option (Init. to 17)
!**   ITEVUT = Temporary Event File Used to Store High Value Events for
!**            Summary Tables and for EVENTFIL Option (Initialized to 18)
!**   MFUNIT = Input Surface Met Data File Unit (Initialized to 19)
!**   INCUNT = INCLUDED File Unit (Initialized to 20)
!**   MPUNIT = Input Profile Met Data File Unit (Initialized to 21)
!**   ISUNIT = Surface Meteorology File for SCIM'd data (Initialized to 22)
!**   IPUNIT = Profile Meteorology File for SCIM'd data (Initialized to 23)
!**   DBGUNT = Debug Output File for Calculations (Init. to 24)
!**   DBMUNT = Debug Output File for Meteorology Profiles (Init. to 25)
!**   AREADBUNT = Debug Output File for AREA/LINE/OPENPIT sources (Init. to 26)
!**   PRMDBUNT  = Debug Output File for PRIME related Debug Infor (Init. to 27)
!**   ISUMUNT = Summary File under SUMMFILE Option (Init. to 28)
!**   GDEPDBG = Debug Output File for Gas Deposition Velocities (Init. to 29)
!**   PDEPDBG = Debug Output File for Particle Deposition Velocities (Init. to. 30)
!**   AWMADWDBUNT = Debug Output File for Mods to AWMA PRIME2 Subcommittee Downwash Algorithms
!**   RLINEDBUNT = Debug Output File for RLINE sources (Init. to 32)
!**   IBGUNT  = Hourly Background Data File(s) by Sector (= 2000 + sector number)
!**   IO3UNT  = Hourly Ozone Data File(s) for PVMRM/OLM/GRSM Options (= 1000 + sector number)
!**   TTRMUNT = Output file generated by invoking TTRM
!!    TTRM2TMP = Temporary output file for TTRM2  !! Added Nov. 2021, AECOM
!**   URBUNT = Output file for Urban Debug
!**   URBUNT1 = Output file for Urban Debug
!**   PLATFMDBUNT = Debug Output File for PLATFORM algorithms
!**   BLPUNT =  BLP-RISE debug file
!**   SWDBGUNT = Debug output file for sidewash source debug
!**   RLINEDBUNT_WS = Debug Output File for RLINE sourcesgridded wind speed profile (Init. to 32)
!**   ARCFTDBG = Debug Output File for Aircraft Plume Rise (Init. to 32)

!***********************************************************************
!     This is The Global Variable Definition Block for Runstream Data
!***********************************************************************

   LOGICAL BLINE, INFLD, MARK, ECHO

   CHARACTER PATH*2, PPATH*2, KEYWRD*8, PKEYWD*8, KEYWD*8, KTYPE*5,&
   &RUNST*1

   CHARACTER (LEN=ILEN_FLD) :: FIELD, INPFIL, OUTFIL, INCFIL
   CHARACTER (LEN=ISTRG)    :: RUNST1

   INTEGER ::  LOCB(IFMAX), LOCE(IFMAX), IFC, IDC1, IPNUM, IPPNUM
   DIMENSION   FIELD(IFMAX), KEYWD(IKN), RUNST(ISTRG)


!***********************************************************************
!     This is The Global Variable Definition Block for Error Handling
!***********************************************************************

   LOGICAL FATAL, ISTART, IFINIS, RECERR, ERRLST, EOF, ALLOC_ERR
   LOGICAL L_SkipMessages

   REAL    :: STORE                ! Estimate of memory storage requirement

   CHARACTER ERRMSG*50, ERRCOD*3, VERSN*6
   CHARACTER (LEN=6) :: C_METVER         ! Character string for met version
   CHARACTER (LEN=ILEN_FLD) :: MSGFIL

   DIMENSION  ERRMSG(IERRN), ERRCOD(IERRN)
   INTEGER :: ILINE, IQLINE, IBLINE, IOLINE, INOXLINE, IERROR, IFTL,&
   &IWRN, INFO, ICLM, IMSG, NFATAL, NWARN, IPAGE, IPGSUM
! --- Met data array indices for use with MAXDCONT option
   INTEGER :: IHR_NDX, IYR_NDX
   DOUBLE PRECISION :: EXPLIM

   INTEGER :: ICSTAT(50), ISSTAT(50), IRSTAT(50), IMSTAT(50),&
   &IOSTAT(50), IESTAT(50)
   INTEGER :: INCSET, IXYSET, IEVSET, IHLSET, IFGSET

! --- Include a variable to count number of header records in the surface file
   INTEGER :: NumHeaders



!***********************************************************************
!     This is The Global Variable Definition Block for COntrol Pathway
!***********************************************************************

!CRT  D063  Add Downwash Platform Debug PLATFMDBG
!CRT  D113  Add Sidewash Debug SWDBG

   LOGICAL DFAULT, CONC, DEPOS, DDEP, WDEP, RURAL, URBAN, GRDRIS,&
   &NOSTD, NOBID, CLMPRO, MSGPRO, PERIOD, ANNUAL, MONTH,&
   &FLAT, ELEV, FLATSRCS, FLGPOL, RUN, EVENTS, RSTSAV,&
   &RSTINP, DAYTAB, MXFILE, PPFILE, PLFILE, ANPOST, ANPLOT,&
   &STATOK, MULTYR, TXFILE, RKFILE, SEASONHR,&
   &MXDAILY, MXDAILY_BYYR, L_MAXDCONT,&
   &DDPLETE, WDPLETE, DRYDPLT, WETDPLT, NODRYDPLT, NOWETDPLT,&
   &FSTCMP, EVONLY, SOCONT, DETAIL, NEWMET, ARDPLETE,&
   &PM25AVE, NO2AVE, SO2AVE, L_NO_PM25AVE, L_NO_NO2AVE,&
   &L_NO_SO2AVE, NOCHKD, NOWARN,&
   &DEBUG, METEORDBG, AREADBG, PRIMEDBG, PVMRMDBG, OLMDEBUG,&
   &ARM2DEBUG, GRSMDEBUG, DEPOSDBG, AWMADWDBG, RLINEDBG,&
   &PLATFMDBG,&
   &L_WARNCHKD, SCIM, SCIMHR,&
   &FASTAREA, FASTALL, L_NonDFAULT,&
   &SCREEN, URBSTAB, PRM_FSTREC, ROMBERG,&
   &PVMRM, PSDCREDIT, OLM, L_MULTURB,&
   &L_PRESET_URBAN, L_UrbanTransition, L_URBAN_ALL,&
   &L_Urban, L_Rural,&
   &L_AWMADW,&
   &L_STRMLN_BLDG, L_RECT_BLDG, L_AWMA_Ueff, L_AWMA_UTurb,&
   &L_AWMA_ENTRAIN, L_ORDDW, L_AWMA_UTurbHX,&
   &L_ORD_Ueff, L_ORD_Turb, L_ORD_Cav,&
   &ARM2, BETA, L_ALPHA, L_PREINC, GRSM,&
   &RUNTTRM, TTRMDBG, URBDBUG, BLPDBUG, SWDBG,&
   &RUNTTRM2, TTRM2DBG,& ! Added Nov. 2021; AECOM
   &ARCFTDEBUG, ARCFT, L_PRESET_ARCFT, L_ARCFT_ALL, L_Arcft,& !Added for Aircraft Plume Rise; UNC-IE
   &HBPLUME, HBPDBG, L_HBP_ALL,& ! Added for HBP & HBPDEBUG; Jan. 2023
   &L_AREAMNDR !Added area meander flag to the alpha options Wood 6/3/22

! --- Logical variables used to track inconsistency between 'original'
!     results and results calculated during the MAXDCONT option
!     internal "post-processing" and between 'original' results and
!     results calculated during the EVENT post-processing
   LOGICAL L_EVENT_OrigConc_Warning, L_MAXDCONT_OrigConc_Warning

!CRT 3/23/2021, D061/D062 - Add logicals for SWmin and BigT low wind options
!CRT 4/11/2022, D131 - FRAN Alpha Formulation - add logical for PBal (L_PBal)
!    Wood 3/18/2022 D127 - added FRANMIN to LOW_WIND option
!CRT 8/9/2023, D176 - COARE Beta Check - add logical to indicate met is from AERMET/COARE (L_COARE)
   LOGICAL :: L_EFFSIGY,   L_VECTORWS,&
   &L_AdjUstar,  L_BULKRN,&
   &L_MMIF_Data, L_MMIF_Profile,&
   &LOW_WIND, L_UserSVmin, L_UserWSmin, L_UserFRANmax,&
   &L_UserSWmin, L_UserBigT,&
   &L_UserSZCoef,&
   &L_CCVR_Sub,  L_TEMP_Sub, L_TurbData,&
   &L_Got_SigA,  L_Got_SigW, L_PBal,&
   &L_UserFRANmin, L_COARE

   CHARACTER (LEN=ILEN_FLD) :: TITLE1, TITLE2
   CHARACTER RUNDAT*8, RUNTIM*8
   CHARACTER EVPARM*6, CHRAVE*5, CHIDEP*4, SOELEV*6, REELEV*6,&
   &TGELEV*6, OUTTYP*5, NO2_FIELD4*3, SO2_FIELD4*3,&
   &PM25_FIELD4*3

!CRT  D063  Add Downwash Platform Debug PLATFMDBGFILE
!CRT  D113  Add Sidewash Debug SWFIL

   CHARACTER (LEN=ILEN_FLD) :: SAVFIL, SAVFL2, INIFIL, EVFILE,&
   &DBGFIL, DBMFIL, DBAREAFIL, DBPVFIL,&
   &RDISPFIL,&
   &DBOLMFIL, DBPRMFIL, DBAwmaDwFIL,&
   &DBARM2FIL, DBGRSMFIL, OZONFL(6),&
   &O3FILUNITS, O3VALUNITS, O3FORM(6),&
   &OzoneUnits, URBNAM, NOXVALUNITS,&
   &NOxUnits, NOXFL(6), NOXFILUNITS,&
   &NOXFORM(6), TTRMFIL, RLINEDBGFIL,&
   &URBFIL,URBFIL1,URBFIL2,&
   &BLPFIL,&
   &PLATFMDBGFILE,&
   &SWFIL, RLINEDBGFIL_WS,&   !Added RLINE gridded wind speed debug filename Wood 10/10/22
   &DBARCFTFIL,& ! Added for Aircraft Plume Rise; UNC-IE
   &HBPFIL ! Added for HBPDEBUG; Jan. 2023

!    TTRMFIL is reserved for an unformatted POSTFILE for potential
!    post-processing using TTRM
!    end of TTRM insert
   DOUBLE PRECISION ::  O3CONC, O3BACK(6), NO2Equil, NO2Stack,&
   &EV_O3CONC(NHR), O3SECT(6), O3_Max24hr(NHR,6),&
   &ARM2_Min, ARM2_Max,&
   &RatioARM2

! --- Logical to determine if minimum ozone restriction will be enforced.
!D074 1/7/2021
   LOGICAL ::  O3MISS, L_O3Sector, L_O3Hourly, NOMINO3
   LOGICAL ::  L_O3File(6), L_O3VAL(6), L_O3VALUES(6)
   LOGICAL ::  L_AO3MISS(24)

!     CERC 11/30/20
   LOGICAL ::  NOXMISS, L_NOXSector, L_NOxHourly
   LOGICAL ::  L_NOxFile(6), L_NOXVALUE(6), L_NOX_VALS(6)
   LOGICAL ::  L_ANOXMISS(24)
   LOGICAL ::  L_CalcNoxFromNO2

   INTEGER ::  NUMO3Sects, IO3SECT
   INTEGER ::  NHIVAL, NMXVAL, NDUMP, NHIMXDLY

   INTEGER ::  NumNOxSects, INOXSECT

   DOUBLE PRECISION :: NOXBGCONC, NOXBACK(6), NOXSECT(6),&
   &EV_NOXCONC(NHR)

! --- Declare arrays for O3SECTs and ANOXSECTs by hour for EVENT processing
   INTEGER ::  AO3SECT(NHR), ANOXSECT(NHR)

   INTEGER ::  NSRC, NREC, NGRP, NAVE, NVAL, NTYP, NMAX,&
   &NSEC, NQF, NBF, NO3F, NPDMAX, NNET, IXM, IYM,&
   &NEVE, NUMEVE, IEVENT, NARC, NOLM, NURB, NPSD, NBLGRP,&
   &IO3MAX(6), IBGMAX(6), INOXMAX(6), NNOXF,&
   &NAFT                ! Added for Aircraft Plume Rise; UNC-IE
!**  NAFT = Maximum Number of Airports (Here, it is fixed as 1 in ARISE)

!     NBLGRP  = Number of buoyant line groups defined by BLPGROUP records;
!                incremented in subr.SRCSIZ; used to allocate BL arrays
!                and write output summary
!                (see also NUMBLGRPS in module buoyant_line -
!                 incremented in subr.BLPGRP)

   INTEGER ::  NUMCONT     ! Number of contributing sources for PVMRM

   INTEGER, ALLOCATABLE :: KAVE(:)

   LOGICAL, ALLOCATABLE :: EVAL(:)

   ALLOCATABLE ::  CHRAVE(:), CHIDEP(:,:), OUTTYP(:), URBNAM(:)

! --- Declare character strings to hold modeling options, including a
!     composite string that includes only options used in current run
   CHARACTER (LEN=12)  :: MODOPS(30)
   CHARACTER (LEN=250) :: MODOPS_String


!***********************************************************************
!     This is The Global Variable Definition Block for SOurce Pathway
!***********************************************************************

!     CRT, 1/18/2012: D063 add SOPLAT for PLATFORM keyword
!     NOTE: A similar variable, OSPLAT, is used to indicate the a source
!     is subject to offshore platform downwash once platform params
!     are validated.
   CHARACTER SRCID*12, SRCTYP*8, SOPCRD*1, SOGAS*1, URBSRC*1,&
   &GRPID*8, EMILBL*40, OUTLBL*40, POLLUT*8,&
   &QFLAG*8, BFLAG(6)*8, O3FLAG(6)*8, PERLBL*40, OLMID*8,&
   &URBID*8, PSDID*8, NOxFLAG(6)*8,  SOPLAT*1,&
   &AFTSRC*1, AFTID*8,& !Added for Aircraft Plume Rise; UNC-IE
   &HBPSRC*1  ! Added for HBP, JAN 2023

!**  AFTSRC = Used in SOurce Pathway to read the Aircraft Source IDs
!**  AFTID  = Used in COmmon Pathway for Airport ID (this is not used in
!**             calculation)

   CHARACTER (LEN=ILEN_FLD) :: HRFILE, BKGRND_File(6), BGFORM(6),&
   &BackUnits
!*#
   CHARACTER PREVSRCID*12
   CHARACTER PREVGRPID*8

! --- Declare logicals related to deposition options applicable to output types;
!     LDPART indicates that particle dry deposition is used
!     LWPART indicates that particle wet deposition is used
!     LDGAS  indicates that gaseous dry deposition is used
!     LWGAS  indicates that gaseous wet deposition is used
   LOGICAL LDPART, LWPART, LDGAS, LWGAS

! --- Declare logicals related to background concentration options:
!     L_BACKGRND indicates generally that background concentration options are used
!     L_BGHourly indicates generally that hourly background concentrations are used
!     L_BGSector indicates generally that sector-varying background concs are used
   LOGICAL L_BACKGRND, L_BGHourly, L_BGSector
! --- Declare logicals related to sector-varying background concentration options:
!     L_BGFile indicates that hourly background concentrations are used for given sector
!     L_BGValues indicates that non-hourly background concs are available for given sector
   LOGICAL L_BGFile(6), L_BGValues(6)

   LOGICAL, ALLOCATABLE :: GRP_BACK(:)

   DOUBLE PRECISION :: BGCONC, EV_BGCONC(NHR)
   DOUBLE PRECISION :: BGBACK(6), BGSECT(6)

   INTEGER ::  NUMBGSects, IBGSECT
   INTEGER ::  NSubBGHOUR                      ! No. of BGHOUR subs
   INTEGER ::  INDX_GRPALL                     ! Index for SrcGroup ALL for ARM/ARM2 options

! --- Declare array for BGSECTs by hour for EVENT processing
   INTEGER ::  ABGSECT(NHR)

   DOUBLE PRECISION, ALLOCATABLE ::  AXS(:), AYS(:), AZS(:), AQS(:),&
   &AHS(:), ATS(:), AVS(:), ADS(:),&
   &ASYINI(:), ASZINI(:), ANO2_RATIO(:),&
   &ADSFACT(:)

!     CRT, 1/18/2012: D063 declare PLATFORM keyword parameters
!     PLATELV = Platform base elevation, PLATHB = Platform building height
!     PLATWB = Platform building width
   DOUBLE PRECISION, ALLOCATABLE ::  PLATELV(:), PLATHB(:), PLATWB(:)

!MGS  7/29/2021 Added AC and BC as distance independent parameters used in CUBIC when
!MGS            computing plume rise adjustment
   DOUBLE PRECISION :: AC, BC

   DOUBLE PRECISION, ALLOCATABLE :: AAQS(:,:,:), AAHS(:,:,:),&
   &AATS(:,:,:), AAVS(:,:,:),&
   &AASYINI(:,:,:), AASZINI(:,:,:)

!**  Added for Aircraft Plume Rise; UNC-IE
!**---Aircraft Engine Parameters
   DOUBLE PRECISION, ALLOCATABLE ::  AMFUEL(:), ATHRUST(:), AVAA(:),&
   &AAFR(:), ABYPR(:),ASRCANGLE(:),&
   &ARPWR(:)          ! For Shaft-Based Engines
   DOUBLE PRECISION, ALLOCATABLE ::  AAMFUEL(:,:,:), AATHRUST(:,:,:),&
   &AAVAA(:,:,:), AAAFR(:,:,:),&
   &AABYPR(:,:,:),AASRCANGLE(:,:,:),&
   &AARPWR(:,:,:)     ! For Shaft-Based Engines
!**  End Aircraft Plume Rise insert; April 2023

   DOUBLE PRECISION, ALLOCATABLE ::  ADSBH(:,:), ADSBW(:,:),&
   &ADSBL(:,:), ADSXADJ(:,:), ADSYADJ(:,:)

!*--- Added for LINE source
   DOUBLE PRECISION, ALLOCATABLE ::  AWIDTH(:), AXS1(:), AYS1(:),&
   &AXS2(:), AYS2(:)

!*--- Added for AREA source meander !Wood 4/14/22
   LOGICAL :: L_APLUME

!*--- Added for BUOYANT LINE source
!     AFP = average buoyancy parameter
   DOUBLE PRECISION, ALLOCATABLE ::  AFP(:), AAFP(:,:,:)

   INTEGER, ALLOCATABLE :: INPD(:), NDXSTK(:)

   DOUBLE PRECISION, ALLOCATABLE ::  QFACT(:,:),&
   &O3VARY(:,:),&
   &BACKGRND(:,:),&
   &NOXVARY(:,:)
   DOUBLE PRECISION :: EMICON, HAFLIF, DECOEF, VAIRMS, ZRDEP, VDPHOR
   DOUBLE PRECISION, ALLOCATABLE :: EMIFAC(:), APDIAM(:,:),&
   &APHI(:,:), APDENS(:,:),&
   &AVGRAV(:,:), ATSTOP(:,:)

!*--- Variables for hourly emissions
   DOUBLE PRECISION ::  HRQS, HRTS, HRVS, HRHS, HRSY, HRSZ, HRFP
!**  Added for Aircraft Plume Rise; UNC-IE
   DOUBLE PRECISION ::  HRMFUEL, HRVAA, HRTHRUST, HRBYPR, HRAFR,&
   &HRSRCANGLE, HRRPWR
!**  End Aircraft Plume Rise insert; April 2023

   DOUBLE PRECISION, ALLOCATABLE ::  EV_HRQS(:,:), EV_HRTS(:,:),&
   &EV_HRVS(:,:), EV_HRHS(:,:),&
   &EV_HRSY(:,:), EV_HRSZ(:,:),&
   &EV_HRFP(:,:),&
   &EV_HRMFUEL(:,:),EV_HRVAA(:,:),&   ! Added for Aircraft Plume Rise; UNC-IE
   &EV_HRTHRUST(:,:),EV_HRBYPR(:,:),& ! Added for Aircraft Plume Rise; UNC-IE
   &EV_HRAFR(:,:),EV_HRSRCANGLE(:,:),&! Added for Aircraft Plume Rise; UNC-IE
   &EV_HRRPWR(:,:)                   ! Added for Aircraft Plume Rise; UNC-IE

!     AECOM 4/12/2022 D113 Added for SIDEWASH
   DOUBLE PRECISION :: CONC_P, CONC_SW, THETA_P, THETA_SW,&
   &SX1, SY1, SWZ
   DOUBLE PRECISION, ALLOCATABLE ::  ABW(:), ABL(:), ABH(:), ABA(:),&
   &SWXS(:), SWYS(:), SWXR(:),&
   &SWYR(:)
!     End SIDEWASH insert, April 2022

   INTEGER ::  FULLHRQ
!*----
!*#
   CHARACTER :: PSDSRCTYP*2
   INTEGER, ALLOCATABLE :: IGROUP(:,:), IGRP_OLM(:,:), IGRP_PSD(:,:),&
   &IURBGRP(:,:),&
   &IGRP_BLP(:,:)               ! Multiple_BuoyLines_D41_Wood

!    CRT, 1/18/2012: D063 add SOPLAT = SO PLATFORM keyword
   ALLOCATABLE ::  SRCID(:), SRCTYP(:), SOPCRD(:), SOPLAT(:),&
   &SOGAS(:), URBSRC(:),&
   &GRPID(:), QFLAG(:), EMILBL(:),&
   &OUTLBL(:),&
   &PERLBL(:), OLMID(:), PSDID(:), URBID(:),&
   &AFTSRC(:), AFTID(:),&                              ! Added for Aircraft Plume Rise; UNC-IE
   &HBPSRC(:) ! Added for HBP, JAN 2023

   LOGICAL, ALLOCATABLE :: L_OLMGRP(:), L_PSDGRP(:)
   LOGICAL, ALLOCATABLE :: L_HRLYSIG(:), L_FLATSRC(:),&
   &L_WakeMessage(:),&
   &L_TTRMSRCTYP(:)

   ALLOCATABLE :: PSDSRCTYP(:)
   DOUBLE PRECISION, ALLOCATABLE :: TTRMOUT(:,:,:)
   CHARACTER :: TTRMSRC*10
   ALLOCATABLE :: TTRMSRC(:,:)


!**   NDXSTK   Index of the gridded height immediately below stack top
!**   FULLHRQ  Date/Time Stamp for Hourly Emissions Data
!**   HRFILE   Hourly Emissions Data File Name
!*#

!***********************************************************************
!     This is The Global Variable Definition Block for the New Area
!     Source Algorithm - 7/7/93
!
!*    Added XVERT,YVERT - Jayant Hardikar, PES, 7/20/94
!***********************************************************************

!**   NVMAX and NVMAX2 are now dynamically allocated at runtime for
!**   AREAPOLY and AREACIRC sources.  If AREACIRC sources are included,
!**   then the maximum number of vertices needed for AREACIRC sources
!**   is based on the maximum number specified by the user using the
!**   optional NVERTS parameter + 4.  The default number of vertices
!**   for an AREACIRC source is still set at 20 vertices, but that
!**   value is only used to determine array storage if the the input
!**   file includes an AREACIRC source without the NVERTS parameter.
!**   If AREAPOLY sources are included, then NVMAX is set to the maximum
!**   number vertices specified for an AREAPOLY source + 8 (but not
!**   less than the number needed for AREACIRC sources).  The '+ 8'
!**   allows for the additional number of sides on an AREAPOLY source that
!**   may be needed to define the portion of the source being integrated,
!**   depending on the source/receptor/wind direction geometry.  This
!**   allows for some complexity in the shape of an AREAPOLY source, but
!**   very complex shapes may result in runtime errors (E406) and should
!**   be avoided.
!**   If no AREACIRC or AREAPOLY sources are included, but rectangular
!**   AREA sources and/or OPENPIT sources are included, then the value
!**   of NVMAX is set to 8 (4 sides plus 4).

!**   NVMAX2= NVMAX * 2

!**   Assign NVPOLY parameter for the number of additional vertices to
!**   include for AREAPOLY sources, beyond the number of vertices used
!**   to define the source.  This is needed to account for the integrated
!**   portion of the area intersecting several sides of the polygon,
!**   depending on the geometry of the source, receptor, and wind direcion.
!**   The NVPOLY parameter is assigned a value of 12, which should work
!**   in most cases.  NVPOLY is added to NVMAX in subroutine SRCSIZ in
!**   the 'aermod.f' source file.
   INTEGER, PARAMETER :: NVPOLY = 12

   INTEGER :: IVERT, NVERT, NSEGS,&
   &NVMAX, NVMAX2, NPIT, NPNT, NVOL, NLINE, NBLINE,&
   &NAREA, NPOLY, NVTEMP, NCIRC, NSWP,&
   &NPTEMP
   DOUBLE PRECISION, ALLOCATABLE :: UVERT(:), VVERT(:), VNVERT(:),&
   &WVERT(:), UASEGS(:), UBSEGS(:),&
   &XVERT(:), YVERT(:)
   DOUBLE PRECISION, ALLOCATABLE :: SPA(:,:)
   DOUBLE PRECISION, ALLOCATABLE :: AXINIT(:), AYINIT(:), AANGLE(:),&
   &AXVERT(:,:), AYVERT(:,:),&
   &RADIUS(:), AXCNTR(:), AYCNTR(:)
   INTEGER, ALLOCATABLE :: NVERTS(:)

   LOGICAL LSEG


!***********************************************************************
!     This is The Global Variable Definition Block for the New OPENPIT
!     Source Algorithm - 7/19/94
!***********************************************************************


   DOUBLE PRECISION, PARAMETER   :: ALPHA = 0.029D0
   DOUBLE PRECISION, ALLOCATABLE :: AALPHA(:), APDEFF(:), AVOLUM(:)
   DOUBLE PRECISION, ALLOCATABLE :: EFRAC(:), QPART(:)
   DOUBLE PRECISION :: PALPHA, THETA, PDEFF, PDREL, PITFRA, QEFF
   DOUBLE PRECISION :: PITLEN, PITWID, PITL, EMIHGT, XEFF, YEFF


!*    ALPHA     = Proportionality Constant for determining Escape Fraction
!*    AALPHA    = Array of Angles of Long Pit Dimension w.r.t.
!*                North for Each Source
!*    PALPHA    = Angle of Long Pit Dimension w.r.t. North for
!*                the Current Source
!*    THETA     = Wind Direction angle w.r.t Long Axis
!*                of the Pit
!*    APDEFF    = Array of Effective Pit Depths for Each Source
!*    PDEFF     = Effective Pit Depths for Current Source
!*    PDREL     = Relative Pit Depth
!*    AVOLUM    = Array of Volume of the OPENPIT Sources
!*    EFRAC     = Array of Escape Fractions
!*    QPART     = Array of Adjusted Emission Rates
!*    PITFRA    = Fractional Size of the Effective Pit Area
!*    PITLEN    = Length of the Pit
!*    PITWID    = Width of the Pit
!*    PITL      = Along-Wind Length of the Pit
!*    EMIHGT    = Height of Emissions Above Base of Pit
!*    XEFF      = X-dimension of Effective Pit
!*    YEFF      = Y-dimension of Effective Pit



!----------------------------------------------------------------------
! --- COMMON BLOCK /DRYGAS/ -- Dry deposition parameters        CALPUFF
!----------------------------------------------------------------------
!
   DOUBLE PRECISION, ALLOCATABLE :: pdiff(:),pdiffw(:),rmolwt(:),&
   &alphas(:),react(:),henry(:),&
   &rcli(:),finemass(:), scf(:)

   LOGICAL, ALLOCATABLE :: L_METHOD2(:)

   INTEGER :: ISEAS_GD(12), ILAND_GD(36), NCLOUD

   DOUBLE PRECISION :: rm, rcut, qsw, xlai, vdepg, uservd, zsubp,&
   &delta_z, FO, FSEAS2, FSEAS5, fracsat,&
   &liqcont, denom, xnu

   DOUBLE PRECISION :: Wold, Wnew, f2, EsTa

   CHARACTER (LEN = 40) ::  REFSPE

   LOGICAL  LUSERVD
!
!     REFSPE      - Reference Species (Default is SO2)
!
! --- COMMON BLOCK /DRYGAS/ Variables:
!       PDIFF(NSRC) - real    - Molecular diffusivity (m**2/s)
!                               of each pollutant.
!                               SEE NOTE #1
!      PDIFFW(NSRC) - real    - Molecular diffusivity in water (m**2/s)
!                               of each pollutant.
!                               SEE NOTE #1
!      RMOLWT(NSRC) - real    - Molecular weight of pollutant (g/mol)
!                               of each pollutant.
!      ALPHAS(NSRC) - real    - Solubility enhancement factor due
!                               to the aqueous phase reactivity of
!                               the pollutant.
!       REACT(NSRC) - real    - Reactivity factor for each
!                               pollutant.
!                RM - real    - Mesophyll resistance (s/m)
!                               SEE NOTE #2
!       HENRY(NSRC) - real    - Henry's law constant (ratio of
!                               gas to aqueous phase concentration
!                               of the pollutant).
!              RCUT - real    - Cuticle resistance (s/m).
!
!  NOTE #1: Input units of this variable are cm**2/s.  Conversion to m**2/s
!           is made internally in the SETUP phase.
!
!  NOTE #2: Input units of s/cm are converted to s/m in the SETUP phase.



!***********************************************************************
!     This is The Global Variable Definition Block for REceptor Pathway
!***********************************************************************

   LOGICAL ISTA, IEND, NEWID

   CHARACTER NETID*8, NETIDT*8, PNETID*8, NTID*8, NTTYP*8,&
   &RECTYP*2, PXSOID*12, PESOID*12, ARCID*8

   DOUBLE PRECISION, ALLOCATABLE ::  AXR(:), AYR(:), AZELEV(:),&
   &AZFLAG(:), AZHILL(:)
   INTEGER, ALLOCATABLE :: IREF(:), NDXARC(:)
   ALLOCATABLE ::  NETID(:), RECTYP(:), NTID(:),&
   &NTTYP(:), ARCID(:)
   INTEGER ::      ICOUNT, JCOUNT, IZE, IZH, IZF, IRZE, IRZH, IRZF,&
   &IRXR, IRYR, IRHZ, INNET
   DOUBLE PRECISION ::  XINT, YINT
   DOUBLE PRECISION, ALLOCATABLE ::  XCOORD(:,:), YCOORD(:,:),&
   &XORIG(:), YORIG(:)
   INTEGER, ALLOCATABLE :: NETSTA(:), NETEND(:),&
   &NUMXPT(:), NUMYPT(:)

!**  AZHILL Hill Height Associated with the Receptor
!**  HCRIT  Critical dividing streamline associated with the receptor


!***********************************************************************
!     This is The Global Variable Definition Block for MEteorology Pathway
!***********************************************************************

   CHARACTER SFNAME*40, UANAME*40, ONNAME*40, ALAT*10, ALON*10

   CHARACTER (LEN=ILEN_FLD) :: METINP, SCIM_SFCFIL, SCIM_PROFIL,&
   &PROINP
   CHARACTER (LEN=ILEN_FLD) :: METFRM, PROFRM
   CHARACTER :: MMIF_Version*29  ! CRT 1/22/2021 D077, increase from 27 to 29

   LOGICAL SCIMOUT

! JAT 01/29/21 ISSUE D070 TURBULENCE OPTIONS
!     Logical flags for ignoring turbulence options
!     variable is an array of size 9.  The indices are by keywrd:
!     1:  NOTURB (ignore sigma-theta and sigma-w for all hours)
!     2:  NOTURBST (ignore sigma-theta and sigma-w for stable hours only (OBULEN > 0))
!     3:  NOTURBCO (ignore sigma-theta and sigma-w for convective hours only (OBULEN < 0))
!     4:  NOSA (ignore sigma-theta for all hours)
!     5:  NOSW (ignore sigma-w for all hours)
!     6:  NOSAST (ignore sigma-theta for stable hours only (OBULEN > 0))
!     7:  NOSWST (ignore sigma-w for stable hours only (OBULEN > 0))
!     8:  NOSACO (ignore sigma-theta for convective hours only (OBULEN < 0))
!     9:  NOSWCO (ignore sigma-w for convective hours only (OBULEN < 0))
!     NOTE: ONLY THE FIRST 2 CAN BE USED WITH THE DEFAULT OPTION; THE OTHERS
!     ARE NON-DEFAULT. IF THOSE ARE ISSUED WITH THE DEFAULT KEYWORD, THE USER
!     WILL GET A WARNING MESSAGE AND THEY WILL BE IGNORED.
   LOGICAL TURBOPTS(9)

!**** Logical flags for met data version, L_OldMetVer is used to flag
!     an outdated met version date in the surface file header record;
!     L_NAD_ADJ_Flags is used to flag cases where surface file header shows
!     current met version date, but surface file lacks additional fields
!     introduced with version 11059 for the wind data source/adjustment.
   LOGICAL :: L_OldMetVer, L_NAD_ADJ_Flags

!**** Include logical variable to track whether current year being
!     processed is a leap year or not, in order to properly handle
!     DAYRANGE inputs based on MN/DY for multiple-year met data
   LOGICAL :: L_LeapYear

   INTEGER :: ISDATE, IEDATE, ISYR, ISMN, ISDY, ISHR, IEYR,&
   &IEMN, IEDY, IEHR, IPROC(366), IPROCL(366),&
   &ISYEAR, IUYEAR, IOYEAR,&
   &IDSURF, IDUAIR, IDSITE, ISJDAY, IEJDAY,&
   &NDAYS, INCRST,&
   &ISTRT_CENT, ISTRT_WIND,&
!        RWB/MJ - allow for SCIM option - May, 1998.
   &NREGSTART, NREGINT, IFIRSTHR, ISUNIT, IPUNIT,&
   &NSKIPTOT, IMETMSG

   INTEGER :: FULL_YYMMDD, IEDATE_YYMMDD

   DOUBLE PRECISION :: UCAT(5), ROTANG,&
   &VIRTPNT_URB(NKST), VIRTPNT_RUR(NKST), VP_FACT
   DOUBLE PRECISION :: SFX, SFY, UAX, UAY, ONX, ONY


!***********************************************************************
!     This is The Global Variable Definition Block for METEXT
!***********************************************************************

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     ADD NEW LOGICAL VARIABLES TO DENOTE THAT SIGMA-THETA AND SIGMA-W
!     WERE RESET
   LOGICAL CLMHR, MSGHR, UNSTAB, NEUTRL, STABLE,&
   &RUNERR, PFLERR, ENDMON, METHDR,&
   &HOURLY, L_DayOfWeekOpts,reset_sa,reset_sw

   LOGICAL, ALLOCATABLE :: L_MorningTrans(:), AL_MorningTrans(:,:,:),&
   &ACLMHR(:,:), AMSGHR(:,:),&
   &ASTABLE(:,:), AUNSTAB(:,:),&
   &AURBSTAB(:,:)

   INTEGER ::  KSTMSG
   INTEGER ::  IHOUR, IYEAR, IMONTH, IDAY, KURDAT, JDAY, ISEAS,&
   &KHOUR, KYEAR, KMONTH, KDAY, KURPFL, NTOTHRS,&
   &IPHOUR, IPYEAR, IPDATE, IPCODE, KST,&
   &IYR, IDAY_OF_WEEK, IDAY_OF_WEEK7, NPLVLS, NTGLVL,&
   &IFLAG(MXPLVL)
   INTEGER ::  JDAY_PREV
   INTEGER ::  FULLDATE
   DOUBLE PRECISION ::  SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH,&
   &OBULEN, SFCZ0, BOWEN, ALBEDO, UREF, WDREF,&
   &UREFHT, TA, TREFHT, ZI, AFV,&
   &BVF, BVPRIM, XLAT, TSIGN, ZIRUR,&
   &PRATE, PREC1, PREC2, TOTAL_PRECIP,&
   &UREF10, RURUSTR, RUROBULEN, RH, SFCP
!  ******************************** added code Jan. 2023 --kja
   INTEGER :: NHBP
!  ** variables for next hour convective and mechanical mixing heights
   DOUBLE PRECISION ::  ZICONVN, ZIMECHN
!  *******************************  added code end  --kja
! Added for HBP MAXDCONT and EVENT processing; Jan. 2023
   DOUBLE PRECISION, ALLOCATABLE :: AZICONVN(:,:), AZIMECHN(:,:)
! End HBP addition

   DOUBLE PRECISION, ALLOCATABLE :: URBPOP(:), URBZ0(:), ZIURB(:),&
   &URBWSTR(:), URBUSTR(:),&
   &URBOBULEN(:)

   INTEGER, ALLOCATABLE :: IKST(:,:), IAPCODE(:,:), NACLOUD(:,:)

   DOUBLE PRECISION, ALLOCATABLE :: APRATE(:,:), AQSW(:,:),ARH(:,:),&
   &ASFCP(:,:)
   DOUBLE PRECISION, ALLOCATABLE :: ASFCHF(:,:), AUREF(:,:),&
   &AUREFHT(:,:), ATA(:,:),&
   &ATREFHT(:,:), AWDREF(:,:),&
   &AUSTAR(:,:), AWSTAR(:,:),&
   &AZICONV(:,:), AZIMECH(:,:),&
   &AOBULEN(:,:), AVPTGZI(:,:),&
   &ASFCZ0(:,:), ABOWEN(:,:),&
   &AALBEDO(:,:), AWNEW(:,:),&
   &AWOLD(:,:), AESTA(:,:),&
   &AF2(:,:), APREC1(:,:),&
   &APREC2(:,:),&
   &AO3CONC(:,:), ABGCONC(:,:),&
   &ANOXBGCONC(:,:),&
   &AKST(:,:),ABLTA(:,:)

   INTEGER :: ISTRHOUR, ISTRDY, ISTRMN,&
   &IENDHOUR, IENDDY, IENDMN, NUMYRS, NREMAIN, NDX4ZI

   INTEGER, ALLOCATABLE :: ANDX4ZI(:,:)

   DOUBLE PRECISION :: PFLHT(MXPLVL), PFLWD(MXPLVL), PFLWS(MXPLVL),&
   &PFLTA(MXPLVL), PFLSA(MXPLVL), PFLSW(MXPLVL),&
   &PFLSV(MXPLVL), PFLTG(MXPLVL), PFLTGZ(MXPLVL)

   DOUBLE PRECISION, ALLOCATABLE :: APFLHT(:,:,:), APFLWD(:,:,:),&
   &APFLWS(:,:,:), APFLTA(:,:,:),&
   &APFLSA(:,:,:), APFLSW(:,:,:),&
   &APFLSV(:,:,:), APFLTG(:,:,:),&
   &APFLTGZ(:,:,:)

   INTEGER, ALLOCATABLE :: AIFLAG(:,:,:)

   INTEGER, ALLOCATABLE :: ANPLVLS(:,:), ANTGLVL(:,:)

   DOUBLE PRECISION :: GRIDHT(MXGLVL), GRIDWD(MXGLVL),&
   &GRIDWS(MXGLVL), GRIDSW(MXGLVL),&
   &GRIDSV(MXGLVL), GRIDTG(MXGLVL),&
   &GRIDPT(MXGLVL),&
!---  Add density profile for PRIME
   &GRIDRHO(MXGLVL),&
!---  Add tubulence dissipation rate (epsilon) profile for PVMRM/GRSM
   &GRIDEPS(MXGLVL),&
!     Added  RL_GRIDWS array for RLINE windspeeds when used in CRITDS - WOOD 6-28-2022
   &RL_GRIDWS(MXGLVL,3)
   DOUBLE PRECISION :: GRDSWR(MXGLVL), GRDSVR(MXGLVL),&
   &GRDTGR(MXGLVL),&
   &GRDPTR(MXGLVL)

   DOUBLE PRECISION, ALLOCATABLE :: GRDSWU(:,:), GRDSVU(:,:),&
   &GRDTGU(:,:), GRDPTU(:,:)

   DOUBLE PRECISION, ALLOCATABLE ::&
   &AGRIDHT(:,:,:), AGRIDWD(:,:,:),&
   &AGRIDWS(:,:,:), AGRIDSW(:,:,:),&
   &AGRIDSV(:,:,:), AGRIDTG(:,:,:),&
   &AGRIDPT(:,:,:),&
!---  Add density profile for PRIME
   &AGRIDRHO(:,:,:),&
!---  Add tubulence dissipation rate (epsilon) profile for PVMRM/GRSM
   &AGRIDEPS(:,:,:)
   DOUBLE PRECISION, ALLOCATABLE :: AGRDSWR(:,:,:), AGRDSVR(:,:,:),&
   &AGRDTGR(:,:,:), AGRDPTR(:,:,:),&
   &AUATZI(:,:),  ASVATZI(:,:),&
   &ASWATZI(:,:), APTATZI(:,:),&
   &AUAVG(:,:),  ASVAVG(:,:),&
   &ASWAVG(:,:), APTAVG(:,:)

   DOUBLE PRECISION, ALLOCATABLE :: AGRDSWU(:,:,:,:),&
   &AGRDSVU(:,:,:,:),&
   &AGRDTGU(:,:,:,:),&
   &AGRDPTU(:,:,:,:),&
   &AZIURB(:,:,:),&
   &AURBWSTR(:,:,:),&
   &AURBUSTR(:,:,:),&
   &AURBOBULEN(:,:,:),&
   &ARURUSTR(:,:),&
   &ARUROBULEN(:,:)

   DOUBLE PRECISION :: TG4PFL, TG4XTR,&
   &THSTAR, SVAVG, SWAVG, UAVG,&
   &SVATZI, SWATZI, UATZI,&
   &PTATZI, UATHE, SVATHE, SWATHE,&
   &UAVH3, SVAVH3, SWAVH3, SWRMAX

!**   BVF    = Brunt-Vaisala frequency
!**   BVPRIM = 0.7*BVF
!**   SFCHF  = Surface heat flux (W/sq m)
!**   USTAR  = Surface friction velocity (m/s)
!**   WSTAR  = Convective scaling velocity (m/s)
!**   VPTGZI = Vertical potential temperature gradient from ZI to ZI+500
!**            (degrees/m)
!**   ZICONV = Hourly convective mixing height estimated by AERMET (m)
!**   ZIMECH = Hourly mechanical mixing height estimated by AERMET (m)
!**   OBULEN = Monin-Obukhov length (m)
!**   SFCZ0  = Surface roughness length (m)
!**   BOWEN  = Bowen ratio = sensible heat flux/latent heat flux
!**   ALBEDO = Albedo at the earth's surface (nondimensional)
!**   UREF   = Reference height wind speed (m/s)
!**   WDREF  = Reference height wind direction (degrees from north)
!**   UREFHT = Reference height for winds (m) (first nonmissing level
!**            of wind speed AND direction above 7.0*SFCZ0)
!**   TA     = Ambient temperature at a reference height (kelvin)
!**   TREFHT = Reference height for temperature (m) (first nonmissing
!**            level of temperature)
!**   ZI     = The mixing height used by AERMOD after any manipulation
!**            and massaging (m)
!**   NPLVLS = Number of levels in the observed hourly profile data
!**   NTGLVL = Number of levels of observed potential temperature gradient
!**   IFLAG  = Top of profile flag: 1 = top level, 0 = level below top
!**   PFLHT  = Profile height above local ground level (m)
!**   PFLWD  = Profile wind direction (degrees from north)
!**   PFLWS  = Profile wind speed (m/s)
!**   PFLTA  = Profile ambient temperature (kelvins)
!**   PFLSA  = Profile sigma_A (degrees)
!**   PFLSW  = Profile sigma_W (m/s)
!**   PFLSV  = Profile sigma_V (m/s), computed from sigma_A and wind speed
!**   PFLTG  = Profile of Vertical Potential Temperature Gradient (kelvin/m)
!**   PFLTGZ = Profile of VPTG heights (midpoint of interval) (m)
!**   GRIDHT = Gridded height (m)
!**   GRIDWD = Gridded wind direction (degrees from north)
!**   GRIDWS = Gridded wind speed (m/s)
!**   RL_GRIDWS = RLine Gridded wind speed (m/s) Wind Speed Array for determining critical height Added by - WOOD 6-28-2022
!**   GRIDSW = Gridded sigma_W (m/s)
!**   GRIDSV = Gridded sigma_V (m/s)
!**   GRIDTG = Gridded vertical potential temperature gradient (deg/m)
!**   GRIDPT = Gridded potential temperature profile
!**   GRIDRHO= Gridded density profile
!**   GRIDEPS= Gridded tubulence dissipation rate (epsilon) profile for PVMRM/GRSM
!**   TG4PFL = Potential temperature gradient at 2.0 meters
!**   TG4XTR = Potential temperature gradient at 100.0 meters
!**   XLAT   = Station latitude, decimal degrees
!**   TSIGN  = Sign used for turning of wind: 1.0 for northern hemis.
!**                                          -1.0 for southern hemis.
!**   NDX4ZI = Index of gridded height immediately below ZI
!**   SVAVG  = Average sigma_V from the surface to ZI (m/s)
!**   SWAVG  = Average sigma_W from the surface to ZI (m/s)
!**   UAVG   = Average wind speed from the surface to ZI (m/s)
!**   SVATZI = sigma_V at ZI (m/s)
!**   SWATZI = sigma_W at ZI (m/s)
!**   UATZI  = Wind speed at ZI (m)
!**   PTATZI = Potential temperature at ZI (kelvin)
!**   SVATHE = Average sigma_V from the surface to HS for HS > ZI (m/s)
!**   SWATHE = Average sigma_W from the surface to HS for HS > ZI (m/s)
!**   UATHE  = Average wind speed from the surface to HS for HS > ZI (m/s)
!**   SVAVH3 = Average sigma_V from the surface to HE3 for penetrated plume
!**   SWAVH3 = Average sigma_W from the surface to HE3 for penetrated plume
!**   UAVH3  = Average wind speed from the surface to HE3 for penetrated plume
!**   SWRMAX = Residual vertical turbulence, average of measured sigma-w
!**            above ZI or 0.02 * UATZI


!***********************************************************************
!     This is The Global Variable Definition Block for Calculation
!***********************************************************************

   LOGICAL CALCS, WAKE
   LOGICAL SURFAC

   DOUBLE PRECISION PHID1, PHID2, PHIN1, PHIN2

   INTEGER :: IREC,   ISRC,   IGRP,   IAVE,   ITYP,  ISET,&
   &NUMREC, NUMSRC, NUMGRP, NUMAVE, NUMARC, NUMTYP,&
   &NUMCAP, NUMHOR,&
! ---            Include counters for all source types
   &NUMPNT, NUMVOL, NUMAREA, NUMLINE, NUMPIT,&
!                Add counter for SIDEWASH Point
   &NUMSWP,&
   &NUMFLAT, IBKGRD(6), IO3SET(6), INOXSET(6),&
   &ICYEAR, NURBSRC, NUMURB, NPD, IFVSEC,&
   &IUCAT, IOLM, NUMOLM, IPSD, NUMPSD, IURB,&
   &ITTRM,&
!! Added for TTRM2
   &CMETH,&
!! End of TTRM2 insert; Nov. 2021
!**  Added for Aircraft Plume Rise; UNC-IE
   &NAFTSRC
!**  NAFTSRC = Number of Aircraft Sources
!**  End Aircraft Plume Rise insert; April 2023

   DOUBLE PRECISION :: XS, YS, ZS, QS, HS, DS, VS, TS, SYINIT,&
   &SZINIT, XINIT, YINIT, ANGLE, XCNTR, YCNTR,&
   &DSFACT, DSBH, DSBW,&
! --- PRIME Modification -------------------------------------------
   &DSBL, XADJ, YADJ, B_SUBS, B_SUBL, RSCALE,&
! ------------------------------------------------------------------
   &D, VD, E, WDRAD, WDSIN, WDCOS, ZBASE,&
!**  Added for Aircraft Plume Rise; UNC-IE
!---- AIRCRAFT Plume Rise Modification -----------------------------
   &MFUEL, THRUST, VAA, AFR, BYPR, SRCANGLE,&
   &RPWR
!**  Aircraft Engine Parameters
!**    MFUEL    = Fuel Burn Rate (kg/s)
!**    THRUST   = Thrust (N)
!**    VAA      = Aircraft Velocity (m/s)
!**    AFR      = Air Fuel Ratio
!**    BYPR     = Engine By Pass Ratio
!**    SRCANGLE = Source Angle for Airborne Sources (degree)
!**    RPWR     = Rated Power for Shaft-based Engines (kilo-Watt)
!**  End Aircraft Plume Rise insert; April 2023

!     AECOM 4/13/2022 D113 Added for SIDEWASH
   DOUBLE PRECISION :: U_AMB,LAMBDA,MU_Y1, MU_Z1, MU_Y2, MU_Z2
   DOUBLE PRECISION :: SWQS, SWHS,& !SWBH, SWBW, SWBL, SWBA,
   &SWTHETA, BL, BW, BH, BA, SWCONC,&
   &BHS, BLS, BWS, HSS  ! Scaled building dimensions and stack height
   LOGICAL RUNSW
!     End insert for SIDEWASH
   DOUBLE PRECISION, ALLOCATABLE :: PDIAM(:), PHI(:), PDENS(:),&
   &VGRAV(:), TSTOP(:), SCHMIDT(:),&
   &VDEP(:), WQCOR(:), DQCOR(:),&
   &PSCVRT(:), WASHOUT(:), ECOLL(:),&
   &AWDSIN(:), AWDCOS(:),&
   &AAFV(:)

!     CRT, 1/20/2012: D063 add OSPLAT logical to indicate if source is
!                     subject to offshore platform downwash
!                     A similar variable, SOPLAT, is used to indicate the
!                     the PLATFORM keyword was specified on the SO card
   LOGICAL, ALLOCATABLE          :: OSPLAT(:)

!     CRT, 1/20/2012: add platform sigma-y and sigma-z
   DOUBLE PRECISION :: PLATSY, PLATSZ      ! stable or unstable injected
   DOUBLE PRECISION :: PLATSYD1, PLATSZD1  ! unstable, direct, updraft
   DOUBLE PRECISION :: PLATSYD2, PLATSZD2  ! unstable, direct, downdraft
   DOUBLE PRECISION :: PLATSYN1, PLATSZN1  ! unstable, indirect, updraft
   DOUBLE PRECISION :: PLATSYN2, PLATSZN2  ! unstable, indirect, downdraft
   DOUBLE PRECISION :: PLATSYP, PLATSZP    ! unstable, penetrated
   DOUBLE PRECISION :: DHP3PLAT

   DOUBLE PRECISION, ALLOCATABLE :: TTRMINST(:), TTRMFRAC(:),&
   &TTRMNO2(:), TTRMSUB(:),&
   &TTRMCOMPARE(:,:,:,:),&
   &TTRMFRAC_PRM(:),&
   &TTRMFRAC_AER(:)
!     variables associated with the incremental NO2 calculations using
!     the ozone response rate (TTRM) approach:
!     TTRMINSTACK = concentration of source-s[ecific in-stack converted NO2
!     TTRMFRAC    = cpncentration due to the conversion fraction of NO to NO2
!                  as a function of hourly ozone and transport time (TTRMTIME)
!     TTRMSUBC    = sub-total of in-stack and fractional NO2
!                  from either COHERENT, PRIME or PANCAKE contributions
!     end TTRM insert AECOM, Feb. 2021
   DOUBLE PRECISION :: WQCORG, GSCVRT, DQCORG, WASHOUTG, VSETL
   DOUBLE PRECISION :: XR, YR, X, Y, ZELEV, ZFLAG, ZR, ZEFF, DISTR,&
   &ZHILL, HCRIT, ZRT, XDIST
   DOUBLE PRECISION :: HE, HSP, HEFLAT, HTER, HEMWAK, HEDHH, ZB, ZM,&
   &HED1, HED2, HEN1, HEN2, HE3, HPEN, HED1M,&
   &HED2M, HEN1M, HEN2M, HE3M, HSBL, QSUBN, QSUB3,&
   &XY, XZ, SBID, FM, FB, DTDZ, DHF, DHFAER, DHP,&
   &DHP1,DHP2, DHP3, DELT, DHPB, DHPM, XF, XMAX,&
   &XFM, XFB, XRAD, WPB, DHCRIT, HTEFF, CENTER,&
   &Z4GAMMA, XTR4GAMMA,&
!**  Added for Aircraft Plume Rise; UNC-IE
!**   HPM   = Horizontal Momentum Plume Rise (m)
!**   HDISP = Horizontal Momentum Displacement for Airborne Source (m)
!**   RP0   = Plume Radius (m)
   &HPM, HDISP, RP0
!** End Aircraft Plume Rise insert; April 2023
   DOUBLE PRECISION :: HESETL, HE3SETL, HV
   DOUBLE PRECISION :: US, SVS, SWS, TGS, TYS, PTS, UP, WDIR, DA,&
   &ZLY, ZLB, RINIT, CB, CM, QTK, PPF, PSUBS, FHC,&
   &SY, SYB, SYN, SY3, SZ, SZUPR, SYAMB, SZAMB,&
   &SZAS, SZAD1, SZAD2, SZAN1, SZAN2, SYAN, SZA3,&
   &SZB, SZBD, SZBN, SZ3, SZD1, SZD2, SZN1, SZN2,&
   &SZEFF, SZSURF, SYA3, SYB3, SZB3, VSY3, VSIGY,&
   &VSIGZ, VSYN, VSZD1,VSZD2, VSZN1, VSZN2, VSZ3,&
   &SZD1M, SZD2M, SZN1M, SZN2M, SZ3M, U3, SV3,&
   &SW3, TGP, SVP, SWP    ! SVP and SWP - Added for ARISE; UNC-IE

!  ************************************  added code Jan. 2023 --kja
!  ** penetrated plume factor below mixing height - Weil's Fq term
   DOUBLE PRECISION ::  PPFN, ZIN, ZIAVG, HHTOP, HTOPDIF, HHBOT,PPWID
   DOUBLE PRECISION ::  SZ3DBG
!  ***********************************  added code end  --kja

   DOUBLE PRECISION :: FSUBY, FSUBYD, FSUBYN, FSUBY3
   DOUBLE PRECISION :: FSUBZ, FSUBZD, FSUBZN, FSUBZ3,&
   &PHEE, FOPT, CWRAP, CLIFT, XMDBG,&
   &CWRAPC, CLIFTC, FSUBYC, FSBY3C
   DOUBLE PRECISION :: UEFF, SVEFF, SWEFF, TGEFF,&
   &UEFFD, SVEFFD, SWEFFD,&
   &UEFFN, SVEFFN, SWEFFN,&
   &UEFF3, SVEFF3, SWEFF3, TGEFF3,&
   &EPSEFF, EPSEFFD, EPSEFF3,&
   &XMIXED, XFINAL, ZMIDMX,&
   &SIGVEFF, SIGVEFFD,&
   &SYEFF
   DOUBLE PRECISION :: SKEW, R, ALPHPD, BETAPD, ASUB1, ASUB2,&
   &BSUB1, BSUB2, LAMDA1, LAMDA2
   DOUBLE PRECISION :: CHIW, CHIDW, CHINW, CHI3W,&
   &CHIL, CHIDL, CHINL, CHI3L
   DOUBLE PRECISION :: GAMFACT
!     CERC 11/30/20:
   DOUBLE PRECISION :: CHI_TTRAVPLM, CHI_TTRAVPAN,&
   &CHI_TTRAVAER, CHI_TTRAVPRM
   DOUBLE PRECISION, ALLOCATABLE :: CHI_TTRAVCHM(:,:), TTRAVCHM(:),&
   &BLDFAC(:,:)
   DOUBLE PRECISION :: UCHM, PRMVAL_Src1
!     added for TTRM approach
   DOUBLE PRECISION :: TTRMTIME, TTRMTIME_PRM, K1, GAMF
!     variable for calculating the transport time of NOx based on
!     distance (downwind for coherent and prime, radial for meander) and
!     effective wind speed
!     end TTRM insert, AECOM; Feb. 2021
!     TTRMTIME_PRM added Nov. 2021 to address transport under
!     building downwash conditions
!**   AZSAVG = Average stack base elevation (m)
!**   ZBASE  = Base elevation used for potential temperature profile (m MSL)
!**   US     = Wind speed at stack height (m/s)
!**   UP     = Stack top wind speed for plume rise computations
!**   WDIR   = Stack top wind direction used for plume transport
!**   SVS    = sigma_V at stack height (m/s)
!**   SWS    = sigma_W at stack height (m/s)
!**   TGS    = Potential temperature gradient at stack top
!**   PTS    = Stack top potential temperature for plume rise
!**   xxEFF  = "effective" value for parameter xx
!**   FHC    = Function of Plume material above HCRIT
!**   PHEE   = "PHI" Term : Fraction of Plume Below Hcrit
!**   FSUBY  = Fy Term (Horizontal Gaussian term)
!**   FSUBYN = Fy Term (Horizontal Gaussian term) for the
!**            Indirect Source
!**   FSUBY3 = Fy Term (Horizontal Gaussian term) for the
!**            Penetrated Source
!**   HEDx   = Effective Source Heights for Direct Plume,
!**            x corresponding to each of the 2 distributions
!**   HENx   = Effective Source Heights for Indirect Plume,
!**            x corresponding to each of the 2 distributions
!**   HE3    = Effective Source Height for Penetrated Plume
!**   HEDxM  = Effective Source Heights for Direct Plume at Xm,
!**            x corresponding to each of the 2 distributions
!**   HENxM  = Effective Source Heights for Indirect Plume at Xm,
!**            x corresponding to each of the 2 distributions
!**   HE3M   = Effective Source Height for Penetrated Plume at Xm

!**   QSUBN  = Source Term for Indirect Source
!**   QSUB3  = Source Term for Penetrated Source
!**   SKEW   = Skewness of the Vertical Velocity
!**   R      = Lagrangian Correlation Coefficient
!**   ALPHPD = ALPHA Coefficient for the CBL PDF
!**   BETAPD = BETA  Coefficient for the CBL PDF
!**   ASUB1  =
!**   ASUB2  =
!**   BSUB1  =
!**   BSUB2  =
!**   LAMDA1 = Relative Frequencies of Updrafts
!**   LAMDA2 = Relative Frequencies of Downdrafts

!**   QSUM        = Sum of emissions for merged plumes in PVMRM option
!**   SUM_NO2RAT  = Sum of NO2/NOx ratios for merged plumes in PVMRM

   DOUBLE PRECISION :: QSUM,  SUM_NO2RAT
   DOUBLE PRECISION :: QSUM3, SUM3_NO2RAT

   DOUBLE PRECISION :: CWMAX, CWMIN, CWMAX3, CWMIN3,&
   &DWMAX, DWMIN, DWMAX3, DWMIN3
   DOUBLE PRECISION :: HMNH,  HMXH,  HMNT,   HMXT,&
   &HMNH3, HMXH3, HMNT3,  HMXT3

!***********************************************************************
!     This is The Global Variable Definition Block for EVent Pathway
!***********************************************************************

   CHARACTER EVNAME*10, EVGRP*8
   INTEGER, ALLOCATABLE ::  EVAPER(:), EVDATE(:), EVJDAY(:),&
   &IDXEV(:)

   ALLOCATABLE ::  EVNAME(:), EVGRP(:)




!***********************************************************************
!     This is The Global Variable Definition Block for OUtput Pathway
!***********************************************************************

   LOGICAL OUTPART, SUMMFILE, L_NoHeader(8), EVALFIL, TOXXFIL

   CHARACTER (LEN=ILEN_FLD) :: THRFIL, PSTFIL, PLTFIL, ANNPST,&
   &ANNPLT, THRFRM, PSTFRM, PLTFRM,&
   &TOXFIL, SEAHRS, RNKFIL, RNKFRM,&
   &EVLFIL, SUMFIL, MXDFRM,&
   &MAXDLY, MAXDLY_BYYR, MAXDCONT_FILE

! --- Variable for specifying format for file outputs (default = 'FIX')
   CHARACTER (LEN = 3) :: FILE_FORMAT

   INTEGER, ALLOCATABLE :: NHIAVE(:,:), MAXAVE(:), IMXVAL(:),&
   &IDYTAB(:), MAXFLE(:,:), IPSTFL(:,:),&
   &IPLTFL(:,:,:), IANPST(:), IANPLT(:),&
   &INHI(:), ITOXFL(:), ISEAHR(:),&
   &IMXDLY(:), IMXDLY_BYYR(:), MAXDCONT(:),&
   &IRNKFL(:), IRKVAL(:)

   DOUBLE PRECISION, ALLOCATABLE :: THRESH(:,:), TOXTHR(:),&
   &MAXD_THRESH(:)

   DOUBLE PRECISION, ALLOCATABLE :: AXR_SAV(:), AYR_SAV(:),&
   &AZELEV_SAV(:), AZFLAG_SAV(:),&
   &AZHILL_SAV(:)

   INTEGER, ALLOCATABLE :: IMXUNT(:,:), IPSUNT(:,:), IPSFRM(:,:),&
   &IPLUNT(:,:,:), IAPUNT(:),&
   &IANFRM(:), IPPUNT(:), ITXUNT(:),&
   &IRKUNT(:), IELUNT(:), IUPART(:),&
   &ISHUNT(:), IMDUNT(:), IMDUNT_BYYR(:),&
   &IMXDCUNT(:), MXD_RANK(:,:)

   ALLOCATABLE ::  THRFIL(:,:), PSTFIL(:,:), PLTFIL(:,:,:),&
   &ANNPST(:), ANNPLT(:), TOXFIL(:), SEAHRS(:),&
   &RNKFIL(:), EVLFIL(:), MAXDLY(:),&
   &MAXDLY_BYYR(:), MAXDCONT_FILE(:)

   INTEGER, ALLOCATABLE :: IDCONC(:,:)

   INTEGER :: ITAB, NXTOX, NYTOX, NHOURS, IPAIR

   DOUBLE PRECISION, ALLOCATABLE :: TXCONC(:,:)



!***********************************************************************
!     This is The Global Variable Definition Block for Working Space
!***********************************************************************

   CHARACTER WORKID*12, DUMMY*12

   INTEGER :: IMIT, INUM, IDUM, INDAVE, INDGRP, INDVAL,&
   &ISC, IOERRN, NCPP, NRPP, NGPP, NPPX, NPPY
   REAL :: FNUM
   DOUBLE PRECISION :: DNUM

   ALLOCATABLE ::          WORKID(:)
   INTEGER, ALLOCATABLE :: IWRK2(:,:)

!     Declare Temporary Work Arrays for ZELEV and ZFLAG Receptor Data
   DOUBLE PRECISION, ALLOCATABLE :: ZETMP1(:), ZETMP2(:)
   DOUBLE PRECISION, ALLOCATABLE :: ZFTMP1(:), ZFTMP2(:)
   DOUBLE PRECISION, ALLOCATABLE :: ZHTMP1(:), ZHTMP2(:)


!***********************************************************************
!     AERMOD Model Data - Array Names, Array Limits, Named Common Blocks
!                         Necessary for Model Results
!     MODIFIED - 4/17/95   Output CONC/DEPOS in same model run
!***********************************************************************


!***********************************************************************
!     This is The Global Variable Definition Block For The Maximum
!     Value, Highest Value, Average Value, Annual Average Value and
!     Model Result Arrays.  Also Included are Calm/Missing Flag Arrays.
!***********************************************************************


   CHARACTER HCLMSG, MCLMSG, HMCLM

   DOUBLE PRECISION, ALLOCATABLE ::  HRVAL(:), AVEVAL(:,:,:,:),&
   &AERVAL(:), PRMVAL(:)
   DOUBLE PRECISION, ALLOCATABLE ::  HIVALU(:,:,:,:,:),&
   &HMAX(:,:,:,:)
   INTEGER, ALLOCATABLE ::  HMLOC(:,:,:,:),&
   &HMDATE(:,:,:,:),&
   &NHIDAT(:,:,:,:,:),&
   &NHIDATMXD(:,:,:),&
   &NHIDATMXD_BYYR(:,:,:,:)

   DOUBLE PRECISION, ALLOCATABLE ::  ANNVAL(:,:,:), AMXVAL(:,:,:),&
   &SHVALS(:,:,:,:,:), MXDVAL(:,:),&
   &HIMXDLY(:,:,:),&
   &HIMXDLY_BYYR(:,:,:,:)
   INTEGER, ALLOCATABLE ::  IMXLOC(:,:,:), IMXDHR(:,:)
   INTEGER              ::  IANHRS, IANCLM, IANMSG,&
   &NSEAHR(4,24), NSEACM(4,24)
   DOUBLE PRECISION, ALLOCATABLE ::  RMXVAL(:,:,:,:)
   INTEGER, ALLOCATABLE ::  MXDATE(:,:,:,:),&
   &MXLOCA(:,:,:,:)
   INTEGER, ALLOCATABLE ::  NUMHRS(:), NUMCLM(:), NUMMSG(:)
   ALLOCATABLE ::           HCLMSG(:,:,:,:,:),&
   &MCLMSG(:,:,:,:),&
   &HMCLM(:,:,:,:)

   DOUBLE PRECISION, ALLOCATABLE ::  SUMANN(:,:,:)
   DOUBLE PRECISION, ALLOCATABLE ::  SUMHNH(:,:,:), MXPMVAL(:,:,:)
   DOUBLE PRECISION, ALLOCATABLE ::  SUMVAL_MAXD(:,:,:,:)

   INTEGER, ALLOCATABLE ::  MXPMLOC(:,:,:)

   DOUBLE PRECISION, ALLOCATABLE ::  CHI(:,:,:), HECNTR(:,:),&
   &HECNTR3(:,:), PPFACT(:,:),&
   &UEFFS(:,:), UEFF3S(:,:),&
   &EPSEF(:,:), EPSEF3(:,:),&
   &FOPTS(:,:),&
   &ABVAL(:,:), BCVAL(:,:)

   DOUBLE PRECISION, ALLOCATABLE ::  ARCMAX(:), QMAX(:), DXMAX(:),&
   &UMAX(:),&
   &SVMAX(:), SWMAX(:), SYMAX(:), SY3MX(:),&
   &U3MAX(:), HEMAX(:), ARCCL(:), SZMAX(:),&
   &CHIDMW(:), CHINMW(:), CHI3MW(:),&
   &CHIDML(:), CHINML(:), CHI3ML(:),&
   &HSBLMX(:)

   LOGICAL, ALLOCATABLE :: CHIMASK(:,:,:)

!***********************************************************************
!     This is The Global Variable Definition Block For The
!     EVENT Model Result Arrays
!***********************************************************************

   DOUBLE PRECISION, ALLOCATABLE ::  EV_AVEVAL(:), HRVALS(:,:),&
   &GRPVAL(:,:), BACKHR(:,:),&
   &GRPAVE(:), BACKAVE(:),&
   &BACKANN(:), BACKSEASHR(:,:,:)

! --- Declare allocatable array for Original Event Concentrations;
!     to be compared to GRPAVE calculated value for QA purpose
   DOUBLE PRECISION, ALLOCATABLE :: EV_OrigConc(:)

   INTEGER ::  EV_NUMHRS, EV_NUMCLM, EV_NUMMSG, ISTAHR, IENDHR


!***********************************************************************
!
!     BLOCK DATA area for initializing global data
!
!***********************************************************************
!***********************************************************************
!     Initialize Model Version Number, VERSN (Year, Julian Day), as a
!     Character Variable
!***********************************************************************

!---- VERSN is now a 6-character variable to accommodate leading qualifier
!     character, such 'D' for Draft version.
   DATA VERSN /'24142'/   ! May 21, 2024

!     Initialize C_METVER to blanks in case there is an error opening
!     the surface met file or with reading the version date, otherwise
!     C_METVER will be undefined when writing the page headers.
   DATA C_METVER /'      '/

! --- Initialize array of hourly O3 values to 40 ppb (78.4 ug/m3)
   DATA O3_Max24hr /144*78.4D0/   ! 144 = 24hr/day*6sectors

!***********************************************************************
!     Input/Output File Units and Input/Output File Names
!***********************************************************************
!CRT  D063  Add Downwash Platform Debug PLATFMDBUNT
!JAT  D137 6/7/22:  Change file units for AWMADWDBUNT, RLINEDBUNT, PLATFMDBUNT,
!               URBUNT, URBUNT1, and BLPUNT by adding 900 to original
!               value to avoid conflicts with user-defined output file units
!      DATA INUNIT/ 7/, IOUNIT/ 8/, PVMDBG/ 9/, OLMDBG/ 9/, ARM2DBG/ 9/,
!     &     GRSMDBG/ 9/, IERUNT/10/, IERWRT/11/, IDPUNT/12/,
!     &     IDPUN2/14/, IRSUNT/15/, IHREMI/16/, IEVUNT/17/, ITEVUT/18/,
!     &     MFUNIT/19/, INCUNT/20/,
!     &     MPUNIT/21/, ISUNIT/22/, IPUNIT/23/, DBGUNT/24/, DBMUNT/25/,
!     &     AREADBUNT/26/, PRMDBUNT/27/, ISUMUNT/28/, GDEPDBG/29/,
!     &     PDEPDBG/30/, AWMADWDBUNT/31/, RLINEDBUNT/32/,
!     &     PLATFMDBUNT/33/, RDISPUNT/ 3/,                       ! RDISPUNT is for RELDISP debug file for PVMRM
!     &     TTRMUNT/9937/, URBUNT/37/, URBUNT1/38/,BLPUNT/39/,
!     &     TTRM2TMP/9938, 9939, 9940/,
!     &     SWDBGUNT/8837/ ! Added for sidewash
!RCO - D168 Debug files. Add new unit for urban debug
   DATA INUNIT/ 7/, IOUNIT/ 8/, PVMDBG/ 9/, OLMDBG/ 9/, ARM2DBG/ 9/,&
   &GRSMDBG/ 9/, IERUNT/10/, IERWRT/11/, IDPUNT/12/,&
   &IDPUN2/14/, IRSUNT/15/, IHREMI/16/, IEVUNT/17/, ITEVUT/18/,&
   &MFUNIT/19/, INCUNT/20/,&
   &MPUNIT/21/, ISUNIT/22/, IPUNIT/23/, DBGUNT/24/, DBMUNT/25/,&
   &AREADBUNT/26/, PRMDBUNT/27/, ISUMUNT/28/, GDEPDBG/29/,&
   &PDEPDBG/30/, AWMADWDBUNT/931/, RLINEDBUNT/932/,&
   &PLATFMDBUNT/933/, RDISPUNT/ 3/,&                       ! RDISPUNT is for RELDISP debug file for PVMRM
   &TTRMUNT/9937/, URBUNT/937/, URBUNT1/938/,BLPUNT/939/,&
   &TTRM2TMP/9938, 9939, 9940/,URBUNT2/941/,&
   &SWDBGUNT/8837/,& ! Added for sidewash
   &RLINEDBUNT_WS/8932/,& ! Added for the gridded wind speed profile in RLINE Wood 10/10/22
   &ARCFTDBG/32/,&        ! Added for Aircraft Plume Rise; UNC-IE
   &HBPUNT/731/ ! Added for HBPDEBUG; Jan. 2023

!*#

! --- Initialize logical variables for tracking use of turbulence data
   DATA L_Got_SigA/.FALSE./, L_Got_SigW/.FALSE./

   DATA INPFIL/' '/, OUTFIL/' '/


!***********************************************************************
!     Initialize Keyword Array
!     CRT, 1/18/2012: D063 Add PLATFORM for platform downwash parameters
!                     on the SO pathway.
!***********************************************************************

   INTEGER, PRIVATE :: I
! JAT 01/29/21  ISSUE D070 TURBULENCE OPTIONS
!     ADD 9 TURBULENCE OPTION KEYWORDS
!     1:  NOTURB (ignore sigma-theta and sigma-w for all hours)
!     2:  NOTURBST (ignore sigma-theta and sigma-w for stable hours only (OBULEN > 0))
!     3:  NOTURBCO (ignore sigma-theta and sigma-w for convective hours only (OBULEN < 0))
!     4:  NOSA (ignore sigma-theta for all hours)
!     5:  NOSW (ignore sigma-w for all hours)
!     6:  NOSAST (ignore sigma-theta for stable hours only (OBULEN > 0))
!     7:  NOSWST (ignore sigma-w for stable hours only (OBULEN > 0))
!     8:  NOSACO (ignore sigma-theta for convective hours only (OBULEN < 0))
!     9:  NOSWCO (ignore sigma-w for convective hours only (OBULEN < 0))
!     NOTE: ONLY THE FIRST 2 CAN BE USED WITH THE DEFAULT OPTION; THE OTHERS
!     ARE NON-DEFAULT. IF THOSE ARE ISSUED WITH THE DEFAULT KEYWORD, THE USER
!     WILL GET A WARNING MESSAGE AND THEY WILL BE IGNORED.
   DATA (KEYWD(I),I=1,IKN) /&
   &'STARTING','FINISHED','TITLEONE','TITLETWO','MODELOPT',&
   &'AVERTIME','POLLUTID','HALFLIFE','DCAYCOEF','DEBUGOPT',&
   &'ELEVUNIT','FLAGPOLE','RUNORNOT','EVENTFIL','SAVEFILE',&
   &'INITFILE','MULTYEAR','ERRORFIL','GASDEPDF','GDSEASON',&
   &'GDLANUSE','GASDEPVD','URBANOPT','NO2EQUIL','NO2STACK',&
   &'OZONEVAL','OZONEFIL','O3VALUES','OZONUNIT','O3SECTOR',&
   &'NOXVALUE','NOX_FILE','NOX_VALS','NOX_UNIT','NOXSECTR',&
   &'LOW_WIND','ARMRATIO',&
   &'LOCATION','SRCPARAM','BUILDHGT','BUILDWID','BUILDLEN',&
   &'XBADJ   ','YBADJ   ','EMISFACT','EMISUNIT','PARTDIAM',&
   &'MASSFRAX','PARTDENS','METHOD_2','CONCUNIT','DEPOUNIT',&
   &'GASDEPOS','HOUREMIS','NO2RATIO','AREAVERT','URBANSRC',&
   &'SRCGROUP','OLMGROUP','PSDGROUP','BACKGRND','BACKUNIT',&
   &'BGSECTOR','BLPINPUT','RBARRIER','RDEPRESS','RLEMCONV',&
   &'INCLUDED','EVENTPER','EVENTLOC','GRIDCART','GRIDPOLR',&
   &'DISCCART','DISCPOLR','EVALCART','SURFFILE','PROFFILE',&
   &'PROFBASE','SURFDATA','UAIRDATA','SITEDATA','STARTEND',&
   &'DAYRANGE','SCIMBYHR','WDROTATE','WINDCATS','NUMYEARS',&
   &'RECTABLE','MAXTABLE','DAYTABLE','SUMMFILE','MAXIFILE',&
   &'POSTFILE','PLOTFILE','TOXXFILE','SEASONHR','EVENTOUT',&
   &'RANKFILE','EVALFILE','FILEFORM','MAXDAILY','MXDYBYYR',&
   &'MAXDCONT','NOHEADER','AWMADWNW','ORD_DWNW','BLPGROUP',&
   &'NOTURB  ','NOTURBST','NOTURBCO','NOSA    ','NOSW    ',&
   &'NOSAST  ','NOSWST  ','NOSACO  ','NOSWCO  ','PLATFORM',&
   &'ARCFTOPT','ARCFTSRC',& !Added for Aircraft Plume Rise; UNC-IE
   &'HBPSRCID'/ ! Added for HBP, JAN 2023

!**  ARCFTOPT  =  To identify the Aircraft Sources in COmmon Pathway
!**  ARCFTSRC  =  To identify the Aircraft Sources in SOurce Pathway

!***********************************************************************
!     Initialize Miscellaneous Variables
!***********************************************************************

   DATA IPROC /366*1/, IPROCL/366*1/, EXPLIM /-50.0D0/
   DATA UCAT /1.54D0, 3.09D0, 5.14D0, 8.23D0, 10.8D0/
   DATA MODOPS /30*'         '/

!***********************************************************************
!     Initialize distance factors used in determining when to switch
!     to point source approximation for area sources under the FASTAREA
!     option (formerly the TOXICS option).
!***********************************************************************

!     STAB. CLASS         A      B      C       D       E       F
!                        ***    ***    ***     ***     ***     ***
   DATA VIRTPNT_URB /3.5D0, 3.5D0, 5.5D0, 10.5D0, 15.5D0, 15.5D0/,&
   &VIRTPNT_RUR /3.5D0, 5.5D0, 7.5D0, 12.5D0, 15.5D0, 25.5D0/


!***********************************************************************
!     Initialize Setup Status Arrays
!***********************************************************************

!     JAT 1/29/21 D070

   DATA ICSTAT/50*0/, ISSTAT/50*0/, IRSTAT/50*0/, IMSTAT/50*0/,&
   &IOSTAT/50*0/, IESTAT/50*0/
!     JAT 1/29/21 D070 UPDATE IMSTAT cross-reference
!***********************************************************************
!     Cross-reference table of Keywords by I?STAT array index:
!
!     Array    Index  Keyword            Array    Index  Keyword
!     -----    -----  -------            -----    -----  -------
!     ICSTAT:    1 = starting            ISSTAT:    1 = starting
!                2 = titleone                       2 = location
!                3 = titletwo                       3 = srcparam
!                4 = modelopt                       4 = buildhgt
!                5 = avertime                       5 = buildwid
!                6 = pollutid                       7 = emisfact
!                7 = halflife                       8 = emisunit
!                8 = dcaycoef                       9 = partdiam
!               11 = flagpole                      10 = massfrax
!               12 = runornot                      11 = partdens
!               13 = eventfil                      15 = elevunit
!               14 = savefile                      16 = houremis
!               15 = initfile                      17 = concunit
!               16 = multyear                      18 = depounit
!               17 = errorfil                      19 = areavert
!               18 = gdseason                      20 = included
!               19 = gasdepdf                      21 = buildlen
!               20 = gdlanuse                      22 = xbadj
!               21 = gasdepvd                      23 = ybadj
!               22 = debugopt                      24 = srcgroup
!               23 = urbanopt                      26 = gasdepos
!               24 = ozoneval                      27 = method_2
!               25 = O3VALUES                      28 = urbansrc
!               26 = ozonefil                      29 = no2ratio
!               27 = ozonunit                      30 = olmgroup
!               28 = no2stack                      34 = psdgroup
!               29 = no2equil                      40 = backgrnd
!               30 = low_wind                      41 = backunit
!               31 = o3sector                      42 = bgsector
!               32 = armratio                      43 = blavginp
!               33 = awmadwnw                      44 = blpgroup
!               34 = ord_dwnw                      45 = hbpsrc
!               35 = noxsectr                      47 = platform
!               36 = noxvalue                      48 = arcftsrc       ! Added for Aircraft Plume Rise; UNC-IE
!               37 = nox_vals                      50 = finished
!               38 = nox_unit
!               39 = nox_file
!               40 = arcftopt                                          ! Added for Aircraft Plume Rise; UNC-IE
!               50 = finished
!
!     IRSTAT:    1 = starting            IESTAT:    1 = starting
!                2 = gridcart            (EVENT)    2 = eventper
!                3 = gridpolr                       3 = eventloc
!                4 = disccart                      10 = included
!                5 = discpolr                      50 = finished
!                8 = evalcart
!                9 = elevunit
!               11 = included
!               50 = finished
!
!     IMSTAT:    1 = starting
!                2 = surffile
!                3 = proffile
!                4 = surfdata
!                5 = uairdata
!                6 = startend
!                7 = dayrange
!                8 = wdrotate
!                9 = sitedata
!               10 = profbase
!               11 = windcats
!               12 = scimbyhr
!               13 = numyears
!               14 = one of turbulence options
!               50 = finished
!
!     IOSTAT:    1 = starting            IOSTAT:    1 = starting
! (non-EVENT)    2 = rectable            (EVENT)    2 = eventout
!                3 = maxtable                      13 = fileform
!                4 = daytable                      25 = finished
!                5 = maxifile
!                6 = postfile
!                7 = plotfile
!                8 = toxxfile
!                9 = seasonhr
!               10 = rankfile
!               11 = evalfile
!               12 = summfile
!               13 = fileform
!               14 = maxdaily
!               15 = mxdybyyr
!               16 = maxdcont
!               18 = noheader
!               50 = finished
!
!***********************************************************************


!***********************************************************************
!     Initialize Gridded Profile Height Array
!***********************************************************************

   DATA GRIDHT /&
   &0.0D0, 0.5D0, 1.0D0, 2.0D0, 4.0D0,  8.0D0, 14.0D0, 20.0D0,&
   &30.0D0,  40.0D0,  50.0D0,  60.0D0,  70.0D0,  80.0D0,  90.0D0,&
   &100.0D0, 120.0D0, 140.0D0, 160.0D0, 180.0D0, 200.0D0, 250.0D0,&
   &300.0D0, 350.0D0, 400.0D0, 450.0D0, 500.0D0, 550.0D0, 600.0D0,&
   &650.0D0, 700.0D0, 750.0D0, 800.0D0, 850.0D0, 900.0D0, 950.0D0,&
   &1000.0D0, 1050.0D0, 1100.0D0, 1150.0D0, 1200.0D0, 1250.0D0,&
   &1300.0D0, 1350.0D0, 1400.0D0, 1450.0D0, 1500.0D0, 1550.0D0,&
   &1600.0D0, 1650.0D0, 1700.0D0, 1750.0D0, 1800.0D0, 1850.0D0,&
   &1900.0D0, 1950.0D0, 2000.0D0, 2100.0D0, 2200.0D0, 2300.0D0,&
   &2400.0D0, 2500.0D0, 2600.0D0, 2700.0D0, 2800.0D0, 2900.0D0,&
   &3000.0D0, 3100.0D0, 3200.0D0, 3300.0D0, 3400.0D0, 3500.0D0,&
   &3600.0D0, 3700.0D0, 3800.0D0, 3900.0D0, 4000.0D0, 4100.0D0,&
   &4200.0D0, 4300.0D0, 4400.0D0, 4500.0D0, 4600.0D0, 4700.0D0,&
   &4800.0D0, 4900.0D0, 5000.0D0/


!CRT  D100, CRT, 8/6/2021
!CRT  Add CONTAINS statement to module and move error code and message
!CRT  arrays into separate subroutine (ERRWRNMSG()) where hardcoded
!CRT  array indices can be written dynamically with a incremented
!CRT  counter variable

CONTAINS


!CRT  D100, CRT, 8/6/2021: Subroutine ERRWRNMSG()
!CRT  Populate error code and message arrays for error and warning
!CRT  messages.  Replace hardcoded array indices with incremental
!CRT  counter.  Move code and message statements to keep codes in order.

   SUBROUTINE ERRWRNMSG()

      IMPLICIT NONE

      INTEGER ECD

!***********************************************************************
!     Initialize Error Code and Message Arrays
!***********************************************************************
      ECD = 0

!-----------------------------------------------------------------------
!---- 100s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ECD = ECD+1
      ERRCOD(ECD)='100'
      ERRMSG(ECD)='Invalid Pathway Specified. The Troubled Pathway is'

      ECD = ECD+1
      ERRCOD(ECD)='105'
      ERRMSG(ECD)='Invalid Keyword Specified. The Troubled Keyword is'

      ECD = ECD+1
      ERRCOD(ECD)='109'
      ERRMSG(ECD)='Too many fields specified on runstream image; MAX='

      ECD = ECD+1
      ERRCOD(ECD)='110'
      ERRMSG(ECD)='Keyword is Not Valid for This Pathway.  Keyword is'

      ECD = ECD+1
      ERRCOD(ECD)='111'
      ERRMSG(ECD)='User-specified minimum Sigma-V on LOW_WIND Keyword'

      ECD = ECD+1
      ERRCOD(ECD)='112'
      ERRMSG(ECD)='User-specified minimum WindSpeed on LOW_WIND Keywd'

      ECD = ECD+1
      ERRCOD(ECD)='113'
      ERRMSG(ECD)='User-specified maximum FRAN on the LOW_WIND Keywrd'

!CRT  4/11/2022 - This message code and message is not used?
      ECD = ECD+1
      ERRCOD(ECD)='114'
      ERRMSG(ECD)='User-specified SZCOEF value on LOW_WIND Keyword   '

      ECD = ECD+1
      ERRCOD(ECD)='115'
      ERRMSG(ECD)='STARTING or FINISHED Out of Sequence:  Pathway =  '

      ECD = ECD+1
      ERRCOD(ECD)='116'
      ERRMSG(ECD)='Vector Wind Speeds specified on MODELOPT Keyword  '

!     Wood 3/18/22 added FRANMIN to LOW_WIND option
      ECD = ECD+1
      ERRCOD(ECD)='117'
      ERRMSG(ECD)='User-specified minimum FRAN on the LOW_WIND Keywrd'

      ECD = ECD+1
      ERRCOD(ECD)='119'
      ERRMSG(ECD)='Missing FINISHED-Runstream File Incomplete: ISTAT='

      ECD = ECD+1
      ERRCOD(ECD)='120'
      ERRMSG(ECD)='Pathway is Out of Sequence:  Pathway =            '

! --- New messages for AWMADWNW and ORD_DWNW keywords on CO pathway
      ECD = ECD+1
      ERRCOD(ECD)='121'
      ERRMSG(ECD)='Duplicate option specified for Keyword            '

      ECD = ECD+1
      ERRCOD(ECD)='122'
      ERRMSG(ECD)='AWMADWNW Option requires ALPHA option on MODELOPT '

      ECD = ECD+1
      ERRCOD(ECD)='123'
      ERRMSG(ECD)='ORD_DWNW Option requires ALPHA option on MODELOPT '

      ECD = ECD+1
      ERRCOD(ECD)='124'
      ERRMSG(ECD)='UEFF conflict: AWMADWNW and ORD_DWNW keywords'

!JAT  6/10/2020:  ISSUE D53.  ADD FROM 19191
!     ADD ERROR MESSAGE FOR INCOMPLETE PATHS,
!     I.E. FINISHED KEYWORD NOT FOUND.  125 HAS ALWAYS BEEN ASSOCIATED
!     WITH MESSAGE BUT THERE NEVER HAS BEEN A MESSAGE.
!JAT  10/28/2020:  UPDATE FROM ISSUE D53 TO BE IN LINE WITH NEW
!     MESSAGES FROM D32.  CHANGE FROM 305 TO 314
      ECD = ECD+1
      ERRCOD(ECD)='125'
      ERRMSG(ECD)='ONE OR MORE PATHS HAS FINISHED KEYWORD MISSING'

!JAT  6/10/2020:  ISSUE D53, ASSIGN NEW ERROR CODE NUMBER, 126 TO THE
!CRT  2/2/2021:  Update message to include AWMAUTURBHX option
!     AWMADW STREAMLINE ERROR MESSAGE.  125 IS ACTUALLY MEANT FOR INCOMPLETE
!     PATHS, I.E. FINISHED KEYWORD NOT FOUND
!      ERRCOD(ECD)='125'
      ECD = ECD+1
      ERRCOD(ECD)='126'
      ERRMSG(ECD)='STREAMLINE requires AWMAUTURB or AWMAUTURBHX'

!CRT 9/11/2020, D062 User Minimum Sigma W
!CRT 02/16/2021: D062 User Minimum Sigma W - Update array index for to
!CRT avoid conflict with merging code
      ECD = ECD+1
      ERRCOD(ECD)='127'
      ERRMSG(ECD)='User-specified minimum Sigma-W on LOW_WIND Keyword'

!CRT 4/11/2022: D131 FRAN Alpha Formulation - Momentum Balance
      ECD = ECD+1
      ERRCOD(ECD)='128'
      ERRMSG(ECD)='User-specified PBAL Option on LOW_WIND Keyword'

!RCO 9/28/2020, D061 User BIGT
!CRT 02/16/2021: D061 User BIGT - Update array index for to
!CRT avoid conflict with merging code
      ECD = ECD+1
      ERRCOD(ECD)='129'
      ERRMSG(ECD)='User-specified BIGT on LOW_WIND Keyword'

      ECD = ECD+1
      ERRCOD(ECD)='130'
      ERRMSG(ECD)='Missing Mandatory Keyword.  The Missing Keyword is'

      ECD = ECD+1
      ERRCOD(ECD)='133'
      ERRMSG(ECD)='LOW_WIND Option requires ALPHA option on MODELOPT '

!     Wood 6/3/22 D128 added AREAMNDR as an ALPHA option to CO pathway
      ECD = ECD+1
      ERRCOD(ECD)='134'
      ERRMSG(ECD)='AREAMNDR ALPHA option selected on MODELOPT Keyword'

      ECD = ECD+1
      ERRCOD(ECD)='135'
      ERRMSG(ECD)='Nonrepeatable Keyword or Recursed INCLUDED: Keywrd'

      ECD = ECD+1
      ERRCOD(ECD)='136'
      ERRMSG(ECD)='LOW_WIND ALPHA option selected on MODELOPT Keyword'

      ECD = ECD+1
      ERRCOD(ECD)='137'
      ERRMSG(ECD)='BETA option not allowed with DFAULT on MODELOPT '

      ECD = ECD+1
      ERRCOD(ECD)='138'
      ERRMSG(ECD)='ALPHA option not allowed with DFAULT on MODELOPT '

!     Wood 6/3/22 D128 added AREAMNDR as an ALPHA option to CO pathway
      ECD = ECD+1
      ERRCOD(ECD)='139'
      ERRMSG(ECD)='AREAMNDR Option requires ALPHA option on MODELOPT '

      ECD = ECD+1
      ERRCOD(ECD)='140'
      ERRMSG(ECD)='Invalid Order of Keyword. The Troubled Keyword is '

      ECD = ECD+1
      ERRCOD(ECD)='141'
      ERRMSG(ECD)='Conflicting Options for NO2 conversion specified: '

      ECD = ECD+1
      ERRCOD(ECD)='142'
      ERRMSG(ECD)='Following Keyword Invalid Without PVMRM or OLM:   '

      ECD = ECD+1
      ERRCOD(ECD)='143'
      ERRMSG(ECD)='Following Keyword Invalid Without PVMRM Option:   '

      ECD = ECD+1
      ERRCOD(ECD)='144'
      ERRMSG(ECD)='Following Keyword Invalid Without OLM Option:     '

      ECD = ECD+1
      ERRCOD(ECD)='145'
      ERRMSG(ECD)='Following Keyword Invalid Without ARM or ARM2:    '

      ECD = ECD+1
      ERRCOD(ECD)='146'
      ERRMSG(ECD)='PSDGROUP Keyword Specified without PSDCREDIT Opt. '

      ECD = ECD+1
      ERRCOD(ECD)='147'
      ERRMSG(ECD)='Following Option is Invalid with PSDCREDIT Option:'

      ECD = ECD+1
      ERRCOD(ECD)='148'
      ERRMSG(ECD)='Both OZONEVAL and O3VALUES keywords are specified '

      ECD = ECD+1
      ERRCOD(ECD)='149'
      ERRMSG(ECD)='Conflicting options specified on MODELOPT keyword:'

      ECD = ECD+1
      ERRCOD(ECD)='150'
      ERRMSG(ECD)='Conflicting Options: MULTYEAR Option with         '

      ECD = ECD+1
      ERRCOD(ECD)='151'
      ERRMSG(ECD)='Non-DFAULT NoUrbTran option selected on MODELOPT  '

      ECD = ECD+1
      ERRCOD(ECD)='152'
      ERRMSG(ECD)='ELEVUNIT card must be first for this Pathway:     '

      ECD = ECD+1
      ERRCOD(ECD)='153'
      ERRMSG(ECD)='Conflicting Opts: MAXDCONT with Re-Start or MULTYR'

      ECD = ECD+1
      ERRCOD(ECD)='154'
      ERRMSG(ECD)='Conflicting options:  SCIM cannot be used with    '

      ECD = ECD+1
      ERRCOD(ECD)='155'
      ERRMSG(ECD)='Conflicting Decay Keyword. Inputs Ignored for     '

      ECD = ECD+1
      ERRCOD(ECD)='156'
      ERRMSG(ECD)='Option ignored - not valid with SCIM.  Option =   '

      ECD = ECD+1
      ERRCOD(ECD)='157'
      ERRMSG(ECD)='Wet SCIM Not Supported - Wet SCIM Inputs Ignored  '

      ECD = ECD+1
      ERRCOD(ECD)='158'
      ERRMSG(ECD)='EMISUNIT Keyword Used With More Than 1 Output Type'

      ECD = ECD+1
      ERRCOD(ECD)='159'
      ERRMSG(ECD)='EMISUNIT Keyword Used With the Following Keyword: '

      ECD = ECD+1
      ERRCOD(ECD)='160'
      ERRMSG(ECD)='Duplicate ORIG Secondary Keyword for GRIDPOLR:    '

      ECD = ECD+1
      ERRCOD(ECD)='161'
      ERRMSG(ECD)='MAXDCONT option already defined for source group: '

      ECD = ECD+1
      ERRCOD(ECD)='162'
      ERRMSG(ECD)='Option only applies to 1-hr NO2 or 1-hr SO2 NAAQS:'

      ECD = ECD+1
      ERRCOD(ECD)='163'
      ERRMSG(ECD)='Option only applies to 24h PM25, 1h NO2 or 1h SO2:'

      ECD = ECD+1
      ERRCOD(ECD)='164'
      ERRMSG(ECD)='NOHEADER selected for non-specified output option:'

      ECD = ECD+1
      ERRCOD(ECD)='165'
      ERRMSG(ECD)='Inconsistent temporally-varying BACKGRND options: '

      ECD = ECD+1
      ERRCOD(ECD)='166'
      ERRMSG(ECD)='SECTOR option invalid w/o BG/O3/NOx Inputs:       '

      ECD = ECD+1
      ERRCOD(ECD)='167'
      ERRMSG(ECD)='Inconsistent temporally-varying O3VALUES options: '

      ECD = ECD+1
      ERRCOD(ECD)='168'
      ERRMSG(ECD)='Hourly BACKGRND already specified for this sector:'

      ECD = ECD+1
      ERRCOD(ECD)='170'
      ERRMSG(ECD)='Invalid Secondary Keyword for Receptor Grid:      '

      ECD = ECD+1
      ERRCOD(ECD)='171'
      ERRMSG(ECD)='Sector ID specified without Sector-varying Option:'

      ECD = ECD+1
      ERRCOD(ECD)='175'
      ERRMSG(ECD)='Missing Secondary Keyword END for Receptor Grid:  '

      ECD = ECD+1
      ERRCOD(ECD)='180'
      ERRMSG(ECD)='Conflicting Secondary Keyword for Receptor Grid:  '

      ECD = ECD+1
      ERRCOD(ECD)='181'
      ERRMSG(ECD)='BULKRN Delta-T & SolarRad option for SBL was used '

      ECD = ECD+1
      ERRCOD(ECD)='182'
      ERRMSG(ECD)='MMIF-generated meteorological inputs were used    '

      ECD = ECD+1
      ERRCOD(ECD)='183'
      ERRMSG(ECD)='Non-DFAULT option for MMIF-generated data without '

      ECD = ECD+1
      ERRCOD(ECD)='184'
      ERRMSG(ECD)='PROFFILE heights > 999m; inputs could be from MMIF'

      ECD = ECD+1
      ERRCOD(ECD)='185'
      ERRMSG(ECD)='Either No Sources or No Receptors are specified!!!'

      ECD = ECD+1
      ERRCOD(ECD)='186'
      ERRMSG(ECD)='THRESH_1MIN 1-min ASOS wind speed threshold used  '

      ECD = ECD+1
      ERRCOD(ECD)='187'
      ERRMSG(ECD)='ADJ_U* Option for Stable Low Winds used in AERMET '

      ECD = ECD+1
      ERRCOD(ECD)='188'
      ERRMSG(ECD)='Non-DFAULT FLAT required for RLINE/RLINEXT source '

      ECD = ECD+1
      ERRCOD(ECD)='189'
      ERRMSG(ECD)='No Keywords for OU Path and No PERIOD/ANNUAL Aves.'

      ECD = ECD+1
      ERRCOD(ECD)='190'
      ERRMSG(ECD)='Incompatible Option Used With SAVEFILE or INITFILE'

      ECD = ECD+1
      ERRCOD(ECD)='191'
      ERRMSG(ECD)='PM25, 1h NO2 or SO2 w/o MAXIFILE incompatible with'

      ECD = ECD+1
      ERRCOD(ECD)='192'
      ERRMSG(ECD)='FASTALL option also implies use of FASTAREA option'

      ECD = ECD+1
      ERRCOD(ECD)='193'
      ERRMSG(ECD)='Units keyword specified without appropriate option'

      ECD = ECD+1
      ERRCOD(ECD)='194'
      ERRMSG(ECD)='DEBUGOPT input option is invalid or not applicable'

      ECD = ECD+1
      ERRCOD(ECD)='195'
      ERRMSG(ECD)='Incompatible Keyword used with GASDEPVD option    '

      ECD = ECD+1
      ERRCOD(ECD)='196'
      ERRMSG(ECD)='Gas deposition algorithms are non-DFAULT options  '

      ECD = ECD+1
      ERRCOD(ECD)='197'
      ERRMSG(ECD)='METHOD_2 for particulates is a non-DFAULT option  '

      ECD = ECD+1
      ERRCOD(ECD)='198'
      ERRMSG(ECD)='Non-DFAULT ALPHA Option Required for use of       '

      ECD = ECD+1
      ERRCOD(ECD)='199'
      ERRMSG(ECD)='Non-DFAULT BETA Option Required for use of        '

!-----------------------------------------------------------------------
!---- 200s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ECD = ECD+1
      ERRCOD(ECD)='200'
      ERRMSG(ECD)='Missing Parameter(s). No Options Specified For    '

      ECD = ECD+1
      ERRCOD(ECD)='201'
      ERRMSG(ECD)='Not Enough Parameters Specified For the Keyword of'

      ECD = ECD+1
      ERRCOD(ECD)='202'
      ERRMSG(ECD)='Too Many Parameters Specified For the Keyword of  '

      ECD = ECD+1
      ERRCOD(ECD)='203'
      ERRMSG(ECD)='Invalid Parameter Specified.  Troubled Parameter: '

      ECD = ECD+1
      ERRCOD(ECD)='204'
      ERRMSG(ECD)='Regulatory DFAULT Conflicts with Non-DFAULT Option'

      ECD = ECD+1
      ERRCOD(ECD)='205'
      ERRMSG(ECD)='No Option Parameter Setting.  Forced by Default to'

      ECD = ECD+1
      ERRCOD(ECD)='206'
      ERRMSG(ECD)='Regulatory DFAULT Overrides Non-DFAULT Option For '

      ECD = ECD+1
      ERRCOD(ECD)='207'
      ERRMSG(ECD)='No Parameters Specified. Default Values Will Used.'

      ECD = ECD+1
      ERRCOD(ECD)='208'
      ERRMSG(ECD)='Illegal Numerical Field Encountered in            '

      ECD = ECD+1
      ERRCOD(ECD)='209'
      ERRMSG(ECD)='Negative Value Appears For Non-negative Variable. '

      ECD = ECD+1
      ERRCOD(ECD)='210'
      ERRMSG(ECD)='Num Ranked values on RANKFILE > MAXTABLE value for'

      ECD = ECD+1
      ERRCOD(ECD)='211'
      ERRMSG(ECD)='Duplicate Averaging Period Specified for Keyword  '

      ECD = ECD+1
      ERRCOD(ECD)='212'
      ERRMSG(ECD)='END Encountered Without (X,Y) Points Properly Set '

      ECD = ECD+1
      ERRCOD(ECD)='213'
      ERRMSG(ECD)='ELEV Input Inconsistent With Option: Input Ignored'

      ECD = ECD+1
      ERRCOD(ECD)='214'
      ERRMSG(ECD)='ELEV Input Inconsistent With Option: Defaults Used'

      ECD = ECD+1
      ERRCOD(ECD)='215'
      ERRMSG(ECD)='FLAG Input Inconsistent With Option: Input Ignored'

      ECD = ECD+1
      ERRCOD(ECD)='216'
      ERRMSG(ECD)='FLAG Input Inconsistent With Option: Defaults Used'

      ECD = ECD+1
      ERRCOD(ECD)='217'
      ERRMSG(ECD)='More Than One Delimiter In A Field for Keyword    '

      ECD = ECD+1
      ERRCOD(ECD)='218'
      ERRMSG(ECD)='Number of (X,Y) Points Does Not Match Number of   '

      ECD = ECD+1
      ERRCOD(ECD)='219'
      ERRMSG(ECD)='Urban ID field is too long (>8); first 12 char:   '

      ECD = ECD+1
      ERRCOD(ECD)='220'
      ERRMSG(ECD)='Missing Origin (Use Default = 0,0) In GRIDPOLR    '

      ECD = ECD+1
      ERRCOD(ECD)='221'
      ERRMSG(ECD)='Missing Dist or Direction Setting In Polar Network'

      ECD = ECD+1
      ERRCOD(ECD)='222'
      ERRMSG(ECD)='SECTOR Value is out of order:                     '

      ECD = ECD+1
      ERRCOD(ECD)='223'
      ERRMSG(ECD)='Missing Distance or Degree Field in               '

! --- New messages '224' '225' and '226' added for undefined SrcID on
!     the SRCGROUP, OLMGROUP, or PSDGROUP keywords
      ECD = ECD+1
      ERRCOD(ECD)='224'
      ERRMSG(ECD)='SrcID specified on SRCGROUP keyword not defined:  '

      ECD = ECD+1
      ERRCOD(ECD)='225'
      ERRMSG(ECD)='SrcID specified on OLMGROUP keyword not defined:  '

      ECD = ECD+1
      ERRCOD(ECD)='226'
      ERRMSG(ECD)='SrcID specified on PSDGROUP keyword not defined:  '

      ECD = ECD+1
      ERRCOD(ECD)='227'
      ERRMSG(ECD)='SECTOR Width is out of range:                     '

      ECD = ECD+1
      ERRCOD(ECD)='228'
      ERRMSG(ECD)='Default(s) Used for Missing Parameters on Keyword '

      ECD = ECD+1
      ERRCOD(ECD)='229'
      ERRMSG(ECD)='Too Many Parameters - Inputs Ignored on Keyword   '

      ECD = ECD+1
      ERRCOD(ECD)='230'
      ERRMSG(ECD)='Source ID field is too long (>12); first 12 chars:'

      ECD = ECD+1
      ERRCOD(ECD)='231'
      ERRMSG(ECD)='Too Many Numerical Values Specified for           '

      ECD = ECD+1
      ERRCOD(ECD)='232'
      ERRMSG(ECD)='OLMGroup ID field is too long (>8); first 12 char:'

      ECD = ECD+1
      ERRCOD(ECD)='233'
      ERRMSG(ECD)='Building Dimensions Specified for Non-POINT Source'

      ECD = ECD+1
      ERRCOD(ECD)='234'
      ERRMSG(ECD)='Too Many Sectors Input for                        '

      ECD = ECD+1
      ERRCOD(ECD)='235'
      ERRMSG(ECD)='Num of SRCGRPs exceeds limit for EVT name; Set=999'

      ECD = ECD+1
      ERRCOD(ECD)='236'
      ERRMSG(ECD)='Not Enough BUILDHGTs Specified for SourceID       '

      ECD = ECD+1
      ERRCOD(ECD)='237'
      ERRMSG(ECD)='Not Enough BUILDWIDs Specified for SourceID       '

      ECD = ECD+1
      ERRCOD(ECD)='238'
      ERRMSG(ECD)='Not Enough BACKGRND Concentration Values Specified'

      ECD = ECD+1
      ERRCOD(ECD)='239'
      ERRMSG(ECD)='Not Enough QFACTs Specified for SourceID          '

      ECD = ECD+1
      ERRCOD(ECD)='240'
      ERRMSG(ECD)='Inconsistent Number of Particle Categories for    '

      ECD = ECD+1
      ERRCOD(ECD)='241'
      ERRMSG(ECD)='Not Enough BUILDLENs Specified for SourceID       '

      ECD = ECD+1
      ERRCOD(ECD)='242'
      ERRMSG(ECD)='No Particle Cat. or Gas Depos. Specified for SRCID'

      ECD = ECD+1
      ERRCOD(ECD)='243'
      ERRMSG(ECD)='Wet depos (DEPOS, WDEP, WETDPLT) incompatible with'

      ECD = ECD+1
      ERRCOD(ECD)='244'
      ERRMSG(ECD)='Source parameters are missing or incomplete for   '

      ECD = ECD+1
      ERRCOD(ECD)='245'
      ERRMSG(ECD)='SrcGroup ID field is too long (>8); first 12 char:'

      ECD = ECD+1
      ERRCOD(ECD)='246'
      ERRMSG(ECD)='Not Enough XBADJs Specified for SourceID          '

      ECD = ECD+1
      ERRCOD(ECD)='247'
      ERRMSG(ECD)='Not Enough YBADJs Specified for SourceID          '

      ECD = ECD+1
      ERRCOD(ECD)='248'
      ERRMSG(ECD)='Either BGVALs or BGFILE missing for this sector:  '

      ECD = ECD+1
      ERRCOD(ECD)='249'
      ERRMSG(ECD)='Source elevation is missing (-9999.0); SRCID =    '

      ECD = ECD+1
      ERRCOD(ECD)='250'
      ERRMSG(ECD)='Duplicate XPNT/DIST or YPNT/DIR Specified for GRID'

      ECD = ECD+1
      ERRCOD(ECD)='251'
      ERRMSG(ECD)='Deposition (DEPOS, DDEP, WDEP) incompatible with  '

      ECD = ECD+1
      ERRCOD(ECD)='252'
      ERRMSG(ECD)='Duplicate Receptor Network ID Specified.  NETID = '

      ECD = ECD+1
      ERRCOD(ECD)='253'
      ERRMSG(ECD)='PSDGROUP ID field is too long (>8); first 12 char:'

! Multiple_BuoyLines_D41_Wood
!     Messages to accomodate multiple buoyant line processing.
      ECD = ECD+1
      ERRCOD(ECD)='254'
      ERRMSG(ECD)='SrcID specified on BLPGROUP keyword not defined:  '

      ECD = ECD+1
      ERRCOD(ECD)='255'
      ERRMSG(ECD)='Non-BL source specified in a BLPGROUP'

      ECD = ECD+1
      ERRCOD(ECD)='256'
      ERRMSG(ECD)='EVALFILE Option Used Without EVALCART Receptors   '

      ECD = ECD+1
      ERRCOD(ECD)='257'
      ERRMSG(ECD)='BL SourceID in more than one BLPGROUP'


!CRT  4/5/2022 D091 Missing BLINPUT Checks - Updated Message
      ECD = ECD+1
      ERRCOD(ECD)='258'
      ERRMSG(ECD)='BL SourceID not in a BLPGROUP defined via BLPINPUT'

      ECD = ECD+1
      ERRCOD(ECD)='259'
      ERRMSG(ECD)='Receptor elevation is missing (-9999.0); IREC =   '

      ECD = ECD+1
      ERRCOD(ECD)='260'
      ERRMSG(ECD)='Number of EMISFACT/O3VALUES/BACKGRND values > max:'

      ECD = ECD+1
      ERRCOD(ECD)='261'
      ERRMSG(ECD)='Not Enough O3VALUES Ozone Concentrations Specified'

      ECD = ECD+1
      ERRCOD(ECD)='262'
      ERRMSG(ECD)='First Vertex Does Not Match LOCATION for AREAPOLY '

      ECD = ECD+1
      ERRCOD(ECD)='264'
      ERRMSG(ECD)='Too Many Vertices Specified for AREAPOLY Source   '

      ECD = ECD+1
      ERRCOD(ECD)='265'
      ERRMSG(ECD)='Not Enough Vertices Specified for AREAPOLY Source '

      ECD = ECD+1
      ERRCOD(ECD)='266'
      ERRMSG(ECD)='Invalid shape defined (area=0) for AREAPOLY source'

      ECD = ECD+1
      ERRCOD(ECD)='267'
      ERRMSG(ECD)='RLINE/RLINEXT requires Zs = 0.0 or FLAT; SRCID=   '

      ECD = ECD+1
      ERRCOD(ECD)='271'
      ERRMSG(ECD)='O3FILE w/o O3VALs; full conv for hrs with miss O3 '

      ECD = ECD+1
      ERRCOD(ECD)='272'
      ERRMSG(ECD)='Upper bound rank > Lower bound rank for MAXDCONT: '

      ECD = ECD+1
      ERRCOD(ECD)='273'
      ERRMSG(ECD)='Range of ranks for MAXDCONT THRESH Opt is limited:'

! --- Included new messages regarding special processing for 1hr NO2/SO2
!     and 24hr PM25
      ECD = ECD+1
      ERRCOD(ECD)='276'
      ERRMSG(ECD)='Special proc for 1h-NO2/SO2 24hPM25 NAAQS disabled'

      ECD = ECD+1
      ERRCOD(ECD)='277'
      ERRMSG(ECD)='Specified option not applicable for this pollutant'

      ECD = ECD+1
      ERRCOD(ECD)='278'
      ERRMSG(ECD)='Keyword only applies to RLINEXT source type:      '

      ECD = ECD+1
      ERRCOD(ECD)='279'
      ERRMSG(ECD)='Multiple URBANOPT/URBANSRC inputs not allowed for:'

      ECD = ECD+1
      ERRCOD(ECD)='280'
      ERRMSG(ECD)='Number of Output Types Specified Exceeds Max:NTYP='

!D132 Remove alpha requirement for rline/bline
!      ECD = ECD+1
!      ERRCOD(ECD)='281'
!      ERRMSG(ECD)='ALPHA required for RLINE/RLINEXT/BUOYLINE URBANSRC'

      ECD = ECD+1
      ERRCOD(ECD)='282'
      ERRMSG(ECD)='Following SRCID Included in Multiple OLMGROUPs:   '

      ECD = ECD+1
      ERRCOD(ECD)='283'
      ERRMSG(ECD)='OZONEVAL, O3VALUES or OZONEFIL Keyword Needed for '

      ECD = ECD+1
      ERRCOD(ECD)='284'
      ERRMSG(ECD)='Invalid POLLUTID Given for NO2 option; Must Use   '

      ECD = ECD+1
      ERRCOD(ECD)='285'
      ERRMSG(ECD)='BACKGROUND and BACKGRND are invalid as Source IDs '

      ECD = ECD+1
      ERRCOD(ECD)='286'
      ERRMSG(ECD)='Following SRCID Included in Multiple PSDGROUPs:   '

      ECD = ECD+1
      ERRCOD(ECD)='287'
      ERRMSG(ECD)='PSDGROUP ID Must be INCRCONS, RETRBASE or NONRBASE'

      ECD = ECD+1
      ERRCOD(ECD)='288'
      ERRMSG(ECD)='Use of "*" for repeated values not meaningful for '

      ECD = ECD+1
      ERRCOD(ECD)='289'
      ERRMSG(ECD)='Source defined as both particulate and gaseous    '

      ECD = ECD+1
      ERRCOD(ECD)='290'
      ERRMSG(ECD)='This array limit exceeded; possible coding error: '

      ECD = ECD+1
      ERRCOD(ECD)='291'
      ERRMSG(ECD)='Filename specified is too long. Maximum length =  '

      ECD = ECD+1
      ERRCOD(ECD)='292'
      ERRMSG(ECD)='Potential problem with Fortran format specifier:  '

      ECD = ECD+1
      ERRCOD(ECD)='293'
      ERRMSG(ECD)='User-specified met data format not used;  use FREE'

      ECD = ECD+1
      ERRCOD(ECD)='294'
      ERRMSG(ECD)='PERIOD and ANNUAL averages are both selected for  '

      ECD = ECD+1
      ERRCOD(ECD)='295'
      ERRMSG(ECD)='Invalid Averaging Period Specified for SCREEN Mode'

      ECD = ECD+1
      ERRCOD(ECD)='296'
      ERRMSG(ECD)='Averaging Period .NE. 1-Hr for TOXXFILE Option    '

      ECD = ECD+1
      ERRCOD(ECD)='297'
      ERRMSG(ECD)='Aver. Period must be .LE. 24 for EVENT Processing '

      ECD = ECD+1
      ERRCOD(ECD)='298'
      ERRMSG(ECD)='Results reported for source group ALL include     '

      ECD = ECD+1
      ERRCOD(ECD)='299'
      ERRMSG(ECD)='SRCGROUP ALL is missing, but is NOT required for  '

!-----------------------------------------------------------------------
!---- 300s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ECD = ECD+1
      ERRCOD(ECD)='300'
      ERRMSG(ECD)='Specified SRCID Has Not Been Defined Yet: KEYWORD='

      ECD = ECD+1
      ERRCOD(ECD)='301'
      ERRMSG(ECD)='Urban Area ID Has Not Been Defined.  URBID =      '

      ECD = ECD+1
      ERRCOD(ECD)='302'
      ERRMSG(ECD)='Following SRCID Included in Multiple Urban Areas: '

      ECD = ECD+1
      ERRCOD(ECD)='303'
      ERRMSG(ECD)='Urban ID has already been defined.  URBID =       '

!CRT 3/5/2021 D067: Delete GEP stack height warning - causes confusion
!CRT This warning was added in v.11059 when WAKEFLG was disabled to
!CRT inform user that downwash would be applied even though stack height
!CRT was at or above GEP calculated for the wind direction of the current
!CRT hour.  However, message is confusing as it implies stack height is
!CRT >= EPA formula GEP based on building ht and max projected width,
!CRT independent of wind direction.
!CRT      ECD = ECD+1
!CRT      ERRCOD(ECD)='305'
!CRT      ERRMSG(ECD)='Stack height > or = EPA formula height for SRCID: '

      ECD = ECD+1
      ERRCOD(ECD)='310'
      ERRMSG(ECD)='Attempt to Define Duplicate LOCATION Card for SRC:'

      ECD = ECD+1
      ERRCOD(ECD)='313'
      ERRMSG(ECD)='Attempt to Define Duplicate EVENTPER card for     '

      ECD = ECD+1
      ERRCOD(ECD)='314'
      ERRMSG(ECD)='Specified GRP index and SRC index is duplicated:  '

      ECD = ECD+1
      ERRCOD(ECD)='315'
      ERRMSG(ECD)='Attempt to Define Duplicate SRCPARAM Card for SRC:'

      ECD = ECD+1
      ERRCOD(ECD)='316'
      ERRMSG(ECD)='Specified SRCID is not included in any SRCGROUP:  '

      ECD = ECD+1
      ERRCOD(ECD)='317'
      ERRMSG(ECD)='Specified SRCID is not included in any PSDGROUP:  '

      ECD = ECD+1
      ERRCOD(ECD)='318'
      ERRMSG(ECD)='No Sources Defined for Urban Area.  URBID =       '

      ECD = ECD+1
      ERRCOD(ECD)='319'
      ERRMSG(ECD)='No Sources Included in Specified Source Group:    '

      ECD = ECD+1
      ERRCOD(ECD)='320'
      ERRMSG(ECD)='Input Parameter May Be Out-of-Range for Parameter '

      ECD = ECD+1
      ERRCOD(ECD)='321'
      ERRMSG(ECD)='BACKGROUND concs are NOT included in any SRCGROUP!'

      ECD = ECD+1
      ERRCOD(ECD)='322'
      ERRMSG(ECD)='Release Height Exceeds Effective Depth for OPENPIT'

      ECD = ECD+1
      ERRCOD(ECD)='323'
      ERRMSG(ECD)='BACKGRND included w/o BACKGRND keyword for SrcGrp:'

      ECD = ECD+1
      ERRCOD(ECD)='324'
      ERRMSG(ECD)='Release Height Exceeds 3000 Meters for SRCID:     '

      ECD = ECD+1
      ERRCOD(ECD)='325'
      ERRMSG(ECD)='Negative Exit Velocity (Set=1.0E-5) for SRCID:    '

      ECD = ECD+1
      ERRCOD(ECD)='330'
      ERRMSG(ECD)='Mass Fraction Parameters Do Not Sum to 1. for Src '

      ECD = ECD+1
      ERRCOD(ECD)='332'
      ERRMSG(ECD)='Mass Fraction Parameter Out-of-Range for Source   '

      ECD = ECD+1
      ERRCOD(ECD)='334'
      ERRMSG(ECD)='Particle Density Out-of-Range for Source          '

      ECD = ECD+1
      ERRCOD(ECD)='335'
      ERRMSG(ECD)='Particle Diameter Out-of-Range for Source         '

      ECD = ECD+1
      ERRCOD(ECD)='336'
      ERRMSG(ECD)='NO2RATIO Missing/Invalid for OLM/PVMRM/GRSM. Src:'

      ECD = ECD+1
      ERRCOD(ECD)='338'
      ERRMSG(ECD)='Neg Emis Rate Invalid with OLM/PVMRM/GRSM. Src: '

      ECD = ECD+1
      ERRCOD(ECD)='340'
      ERRMSG(ECD)='Possible Error in PROFBASE Input:  Value is < 0   '

      ECD = ECD+1
      ERRCOD(ECD)='341'
      ERRMSG(ECD)='Emissions in HOUREMIS file < -90; set to 0.0 for  '

      ECD = ECD+1
      ERRCOD(ECD)='342'
      ERRMSG(ECD)='Src ID Mismatch in Hourly Emissions File for ID = '

      ECD = ECD+1
      ERRCOD(ECD)='344'
      ERRMSG(ECD)='Missing HOUREMIS fields; EmisRate set = 0. KURDAT='

      ECD = ECD+1
      ERRCOD(ECD)='345'
      ERRMSG(ECD)='Problem processing the HOUREMIS file.   KURDAT =  '

      ECD = ECD+1
      ERRCOD(ECD)='346'
      ERRMSG(ECD)='Too many fields for HOUREMIS file.     KURDAT =   '

      ECD = ECD+1
      ERRCOD(ECD)='350'
      ERRMSG(ECD)='Julian Day Out Of Range at                        '

      ECD = ECD+1
      ERRCOD(ECD)='352'
      ERRMSG(ECD)='The "H6H" field is no longer required for MULTYEAR'

      ECD = ECD+1
      ERRCOD(ECD)='353'
      ERRMSG(ECD)='Urban Roughness Length (m) May Be Out-of-Range:   '

      ECD = ECD+1
      ERRCOD(ECD)='360'
      ERRMSG(ECD)='2-Digit Year Specified: Valid for Range 1950-2049 '

      ECD = ECD+1
      ERRCOD(ECD)='361'
      ERRMSG(ECD)='Multiyear PERIOD/ANNUAL values for NO2/SO2 require'

      ECD = ECD+1
      ERRCOD(ECD)='362'
      ERRMSG(ECD)='Multiyear 1h NO2/SO2 processing not applicable for'

      ECD = ECD+1
      ERRCOD(ECD)='363'
      ERRMSG(ECD)='Multiyr 24h/Ann PM25 processing not applicable for'

      ECD = ECD+1
      ERRCOD(ECD)='365'
      ERRMSG(ECD)='Year Input is Greater Than 2147                   '

      ECD = ECD+1
      ERRCOD(ECD)='370'
      ERRMSG(ECD)='Invalid Date: 2/29 In a Non-leap Year.            '

      ECD = ECD+1
      ERRCOD(ECD)='380'
      ERRMSG(ECD)='This Input Variable is Out-of-Range:              '

      ECD = ECD+1
      ERRCOD(ECD)='381'
      ERRMSG(ECD)='Latitude in Surface File Is Not Valid:            '

      ECD = ECD+1
      ERRCOD(ECD)='382'
      ERRMSG(ECD)='Error Decoding Latitude:                          '

! --- More new messages for buoyant line processing; updated for multiple
!     buoyant line sources (Multiple_BuoyLines_D41_Wood)
      ECD = ECD+1
      ERRCOD(ECD)='383'
      ERRMSG(ECD)='# buoy. lines in group not equal to # on HOUREMIS:'

      ECD = ECD+1
      ERRCOD(ECD)='384'
      ERRMSG(ECD)='Not enough fields specified for HOUREMIS; KURDAT ='

! --- New messages for buoyant line processing; # '385'updated for multiple
!     buoyant line sources (Multiple_BuoyLines_D41_Wood)
      ECD = ECD+1
      ERRCOD(ECD)='385'
      ERRMSG(ECD)='Following SRCID Included in Multiple BLPGROUPs:   '

      ECD = ECD+1
      ERRCOD(ECD)='386'
      ERRMSG(ECD)='PARTDIAM and METHOD_2 specified for same SRCID:   '

      ECD = ECD+1
      ERRCOD(ECD)='387'
      ERRMSG(ECD)='METHOD_2 option already specified for this SRCID: '

      ECD = ECD+1
      ERRCOD(ECD)='388'
      ERRMSG(ECD)='Input buoyant line sources not in correct order:  '

      ECD = ECD+1
      ERRCOD(ECD)='389'
      ERRMSG(ECD)='Rotated buoyant line sources not in correct order:'

      ECD = ECD+1
      ERRCOD(ECD)='390'
      ERRMSG(ECD)='Aspect ratio (L/W) of LINE source greater than 100'

      ECD = ECD+1
      ERRCOD(ECD)='391'
      ERRMSG(ECD)='Aspect ratio (L/W) of AREA source greater than 100'

      ECD = ECD+1
      ERRCOD(ECD)='392'
      ERRMSG(ECD)='Aspect ratio (L/W) of OPENPIT is greater than 10  '

!     If at least one BL line is declared as urban, then all must be urban
      ECD = ECD+1
      ERRCOD(ECD)='393'
      ERRMSG(ECD)='Not all lines in BL source declared urban in group'

      ECD = ECD+1
      ERRCOD(ECD)='394'
      ERRMSG(ECD)='Met data may be from outdated version of AERMET:  '

      ECD = ECD+1
      ERRCOD(ECD)='395'
      ERRMSG(ECD)='Met. Data Error; Incompatible Version of AERMET:  '

      ECD = ECD+1
      ERRCOD(ECD)='396'
      ERRMSG(ECD)='AERMET Version Out-dated or Non-standard; Version:'

      ECD = ECD+1
      ERRCOD(ECD)='397'
      ERRMSG(ECD)='SCREEN option used without use of SCREEN Met Data '

      ECD = ECD+1
      ERRCOD(ECD)='398'
      ERRMSG(ECD)='SCREEN met used without specifying SCREEN option  '

      ECD = ECD+1
      ERRCOD(ECD)='399'
      ERRMSG(ECD)='EXP format specified with no applicable file types'

!-----------------------------------------------------------------------
!---- 400s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ECD = ECD+1
      ERRCOD(ECD)='400'
      ERRMSG(ECD)='Output values exceed format limit; use OU FILEFORM'

      ECD = ECD+1
      ERRCOD(ECD)='401'
      ERRMSG(ECD)='Use of turbulence data with ADJ_U* is NonDFAULT   '

! --- New messages for v16216
      ECD = ECD+1
      ERRCOD(ECD)='402'
      ERRMSG(ECD)='Turbulence data being used with ADJ_U* w/o DFAULT '

      ECD = ECD+1
      ERRCOD(ECD)='403'
      ERRMSG(ECD)='Turbulence data is being used w/o ADJ_U* option   '

      ECD = ECD+1
      ERRCOD(ECD)='405'
      ERRMSG(ECD)='Value of PHEE Exceeds 1.0 on KURDAT =             '

      ECD = ECD+1
      ERRCOD(ECD)='406'
      ERRMSG(ECD)='Number of Vertices Exceeds Max (NVMAX) for SRCID: '

      ECD = ECD+1
      ERRCOD(ECD)='409'
      ERRMSG(ECD)='Error Allocating Storage for Setup/Result Arrays! '

      ECD = ECD+1
      ERRCOD(ECD)='410'
      ERRMSG(ECD)='Wind Direction Out-of-Range.  KURDAT =            '

! --- Included new message regarding QSUM = 0.0 runtime error in PVMRM;
!     this should never occur, but could indicate a programming error
      ECD = ECD+1
      ERRCOD(ECD)='411'
      ERRMSG(ECD)='Possible ERROR in PVMRM_CALC! QSUM=0.0 @ Rec# Date'

      ECD = ECD+1
      ERRCOD(ECD)='412'
      ERRMSG(ECD)='Possible ERROR in PVMRM_CALC! QSUM=0.0 @ Evt# Date'

      ECD = ECD+1
      ERRCOD(ECD)='413'
      ERRMSG(ECD)='Number of Threshold Events > 999999 for Ave Period'


      ECD = ECD+1
      ERRCOD(ECD)='415'
      ERRMSG(ECD)='MAXDCONT THRESH not reached within range of ranks '

      ECD = ECD+1
      ERRCOD(ECD)='420'
      ERRMSG(ECD)='Wind Speed Out-of-Range.   KURDAT =               '

!     CRT D176 COARE Beta Check - COARE used to process met
      ECD = ECD+1
      ERRCOD(ECD)='422'
      ERRMSG(ECD)='Meteorological data processed with COARE in AERMET'

!     CRT D176 COARE Beta Check - BULKRN cannot be used with COARE
      ECD = ECD+1
      ERRCOD(ECD)='423'
      ERRMSG(ECD)='BULKRN in AERMET cannot be used with COARE        '

!     D127 - Added for minimum fran (see similar message 494 for max fran)
      ECD = ECD+1
      ERRCOD(ECD)='424'
      ERRMSG(ECD)='Meander factor (FRAN) below min @ YR MN DY ISRC:  '

!CRT     D127 - Added for fran min > fran max
      ECD = ECD+1
      ERRCOD(ECD)='426'
      ERRMSG(ECD)='LOW_WIND user-specified FRANMIN > FRANMAX:  '

      ECD = ECD+1
      ERRCOD(ECD)='430'
      ERRMSG(ECD)='Ambient Temperature Data Out-of-Range.  KURDAT =  '

      ECD = ECD+1
      ERRCOD(ECD)='432'
      ERRMSG(ECD)='Friction Velocity Out-of-Range.   KURDAT =        '

      ECD = ECD+1
      ERRCOD(ECD)='435'
      ERRMSG(ECD)='Surface Roughness Length Out-of-Range.  KURDAT =  '

      ECD = ECD+1
      ERRCOD(ECD)='438'
      ERRMSG(ECD)='Convective Velocity Data Out-of-Range.  KURDAT =  '

      ECD = ECD+1
      ERRCOD(ECD)='439'
      ERRMSG(ECD)='Monin-Obukhov Length Out-of-Range.  KURDAT =      '

      ECD = ECD+1
      ERRCOD(ECD)='440'
      ERRMSG(ECD)='Calm Hour Identified in Meteorology Data File at  '

      ECD = ECD+1
      ERRCOD(ECD)='441'
      ERRMSG(ECD)='Vert Pot Temp Grad abv ZI set to min .005, KURDAT='

      ECD = ECD+1
      ERRCOD(ECD)='442'
      ERRMSG(ECD)='Vert Pot Temp Grad abv ZI exceeds 0.1 K/m, KURDAT='

!     JAT 1/29/21  D070 TURBULENCE OPTIONS
!     ADD WARNING MESSAGE AND INFORMATIONAL MESSAGES
      ECD = ECD+1
      ERRCOD(ECD)='443'
      ERRMSG(ECD)='SELECTED TURBULENCE OPTION:                       '

      ECD = ECD+1
      ERRCOD(ECD)='444'
      ERRMSG(ECD)='TURBOPT INVALID WITH DFAULT; RESET                '

      ECD = ECD+1
      ERRCOD(ECD)='445'
      ERRMSG(ECD)='Set sigma-theta to missing for                    '

      ECD = ECD+1
      ERRCOD(ECD)='446'
      ERRMSG(ECD)='Set sigma-w to missing for                        '

      ECD = ECD+1
      ERRCOD(ECD)='450'
      ERRMSG(ECD)='Record Out of Sequence in Meteorological File at: '

      ECD = ECD+1
      ERRCOD(ECD)='452'
      ERRMSG(ECD)='Missing hourly BACKGRND w/o BGSUB, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='453'
      ERRMSG(ECD)='BGSUB for missing hourly BACKGRND, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='454'
      ERRMSG(ECD)='Date/time Mismatch: BACKGRND File, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='455'
      ERRMSG(ECD)='Date/time Mismatch: Hourly Emission File, KURDAT ='

      ECD = ECD+1
      ERRCOD(ECD)='456'
      ERRMSG(ECD)='Date/time Mismatch on Surface & Profile.  KURDAT ='

      ECD = ECD+1
      ERRCOD(ECD)='457'
      ERRMSG(ECD)='Date/time Mismatch: OZONEFIL File, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='458'
      ERRMSG(ECD)='O3SUB for missing hourly O3 value, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='459'
      ERRMSG(ECD)='No Hrly O3 & No Sub; Use Full Conversion, KURDAT ='

      ECD = ECD+1
      ERRCOD(ECD)='460'
      ERRMSG(ECD)='Missing Hour Identified in Meteor. Data File at   '

      ECD = ECD+1
      ERRCOD(ECD)='465'
      ERRMSG(ECD)='Number of Profile Levels Exceeds Max:   MXPLVL =  '

      ECD = ECD+1
      ERRCOD(ECD)='470'
      ERRMSG(ECD)='Mixing Height Value is < or = 0.0.   KURDAT =     '

      ECD = ECD+1
      ERRCOD(ECD)='471'
      ERRMSG(ECD)='Met. ref hgt WS < 1.0 m/s; set to 1.0 for BL srcs '

      ECD = ECD+1
      ERRCOD(ECD)='472'
      ERRMSG(ECD)='Release hgt < 2.0m for BL source; set to 2.0m for '

!     JAT D068 1/15/21:  UDPATED DEPOSITION MESSAGE AFTER NEWER MESSAGES
      ECD = ECD+1
      ERRCOD(ECD)='473'
      ERRMSG(ECD)='Default deposition parameter(s) used for SRCID:   '

      ECD = ECD+1
      ERRCOD(ECD)='474'
      ERRMSG(ECD)='WS RefHt invalid (<0.001); Not msg or clm: KURDAT='

      ECD = ECD+1
      ERRCOD(ECD)='475'
      ERRMSG(ECD)='WS reference height is higher than 100m.  KURDAT ='

! --- More new messages for buoyant line processing; updated for multiple
!     buoyant line sources (Multiple_BuoyLines_D41_Wood)
      ECD = ECD+1
      ERRCOD(ECD)='476'
      ERRMSG(ECD)='# receptors within BL source (BLPGROUP)           '
      ECD = ECD+1
      ERRCOD(ECD)='477'
      ERRMSG(ECD)='Receptor inside BL source group for event:        '

!CRT  2/22/2021: D059 AWMA Downwash Options
!CRT  Resolve conflict with error array index and code
      ECD = ECD+1
      ERRCOD(ECD)='478'
      ERRMSG(ECD)='AWMAUTurb & AWMAUTurbHX entered; AWMAUTurbHX used '

      ECD = ECD+1
      ERRCOD(ECD)='479'
      ERRMSG(ECD)='Potential temperature gradient is out-of-range:   '

      ECD = ECD+1
      ERRCOD(ECD)='480'
      ERRMSG(ECD)='Less than 1yr for MULTYEAR, MAXDCONT or ANNUAL Ave'

      ECD = ECD+1
      ERRCOD(ECD)='481'
      ERRMSG(ECD)='Data Remaining After End of Year. Number of Hours='

      ECD = ECD+1
      ERRCOD(ECD)='482'
      ERRMSG(ECD)='Too many years modeled for 24h-PM25 1h-NO2 1h-SO2:'

      ECD = ECD+1
      ERRCOD(ECD)='483'
      ERRMSG(ECD)='User Start Date is Earlier Than Start of Met File '

      ECD = ECD+1
      ERRCOD(ECD)='484'
      ERRMSG(ECD)='Restart Date < STARTEND date or start of Met File '

      ECD = ECD+1
      ERRCOD(ECD)='485'
      ERRMSG(ECD)='MULTYR DataGap; Restart Date < STARTEND or MetFile'

      ECD = ECD+1
      ERRCOD(ECD)='486'
      ERRMSG(ECD)='MULTYR Date Overlap; STARTEND Date < Restart Date '

      ECD = ECD+1
      ERRCOD(ECD)='487'
      ERRMSG(ECD)='MULTYR Date Overlap; MetFile Start < Restart Date '

      ECD = ECD+1
      ERRCOD(ECD)='488'
      ERRMSG(ECD)='First met HR.ne.1; ST results may not be valid    '

      ECD = ECD+1
      ERRCOD(ECD)='489'
      ERRMSG(ECD)='First met HR.ne.1; EV results may not be valid for'

      ECD = ECD+1
      ERRCOD(ECD)='490'
      ERRMSG(ECD)='Problem reading SURFFILE date for EVENTS; MNDYHR ='

      ECD = ECD+1
      ERRCOD(ECD)='491'
      ERRMSG(ECD)='MAXDCONT option requires 1st Hr of met data = 01; '

      ECD = ECD+1
      ERRCOD(ECD)='492'
      ERRMSG(ECD)='SURFDATA YR .NE. 1st YR of file, adj to match file'

      ECD = ECD+1
      ERRCOD(ECD)='493'
      ERRMSG(ECD)='SURFDATA YR must match 1st YR of file for DAYRANGE'

!     see similar message 424 for fran max
      ECD = ECD+1
      ERRCOD(ECD)='494'
      ERRMSG(ECD)='Meander factor (FRAN) exceeds max @ YR MN DY ISRC:'

      ECD = ECD+1
      ERRCOD(ECD)='495'
      ERRMSG(ECD)='Surface met file does not include enough variables'

      ECD = ECD+1
      ERRCOD(ECD)='496'
      ERRMSG(ECD)='Total precipitation in SURFFILE is zero (0.0) with'

      ECD = ECD+1
      ERRCOD(ECD)='497'
      ERRMSG(ECD)='Possible code ERROR!!! EVENT mismatch for EVENTID:'

      ECD = ECD+1
      ERRCOD(ECD)='498'
      ERRMSG(ECD)='Possible code ERROR! MAXDCONT mismatch GRP/RNK/REC'

      ECD = ECD+1
      ERRCOD(ECD)='499'
      ERRMSG(ECD)='PRIME plume rise error; check stack parameters for'

!-----------------------------------------------------------------------
!---- 500s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ECD = ECD+1
      ERRCOD(ECD)='500'
      ERRMSG(ECD)='Fatal Error Occurs Opening the Data File of       '

      ECD = ECD+1
      ERRCOD(ECD)='501'
      ERRMSG(ECD)='Dup Filename! Fatal Error Opening the Data File of'

! Multiple_BuoyLines_D41_Wood
!     Messages to accomodate multiple buoyant line processing.
!CRT  4/5/2022 D091 Missing BLPINPUT Checks - Updated Message
      ECD = ECD+1
      ERRCOD(ECD)='502'
      ERRMSG(ECD)='No BLPINPUT record for BLPGROUP ID'

      ECD = ECD+1
      ERRCOD(ECD)='503'
      ERRMSG(ECD)='BLPGROUP IDs do not match BLPINPUT IDs'

      ECD = ECD+1
      ERRCOD(ECD)='504'
      ERRMSG(ECD)='BLPINPUT group ID already specified-must be unique'

      ECD = ECD+1
      ERRCOD(ECD)='505'
      ERRMSG(ECD)='BLPINPUT records out of order, BLPINPUT Groups:'

! D_32 Wood - Check for non-parallel lines within group
      ECD = ECD+1
      ERRCOD(ECD)='506'
      ERRMSG(ECD)='BUOYLINE not parallel to 1st line of its group:'

!CRT  4/5/2022 D091 Missing BLPINPUT Checks - Added Messages (507, 508, 509)
      ECD = ECD+1
      ERRCOD(ECD)='507'
      ERRMSG(ECD)='No BLPINPUT records for BUOYLINE sources'

      ECD = ECD+1
      ERRCOD(ECD)='508'
      ERRMSG(ECD)='No BUOYLINE sources for BLPINPUT record'

      ECD = ECD+1
      ERRCOD(ECD)='509'
      ERRMSG(ECD)='# BLPGROUP IDs defined by BLPINPUTs <> # allocated'

      ECD = ECD+1
      ERRCOD(ECD)='510'
      ERRMSG(ECD)='Fatal Error Occurs During Reading of the File of  '

      ECD = ECD+1
      ERRCOD(ECD)='520'
      ERRMSG(ECD)='Fatal Error Occurs During Writing to the File of  '

      ECD = ECD+1
      ERRCOD(ECD)='530'
      ERRMSG(ECD)='CAUTION! Met Station ID Mismatch with SURFFILE for'

      ECD = ECD+1
      ERRCOD(ECD)='531'
      ERRMSG(ECD)='CAUTION! Met Station ID Missing from SURFFILE for '

      ECD = ECD+1
      ERRCOD(ECD)='540'
      ERRMSG(ECD)='No RECTABLE/MAXTABLE/DAYTABLE for Average Period  '

      ECD = ECD+1
      ERRCOD(ECD)='550'
      ERRMSG(ECD)='File Unit/Name Conflict for the Output Option:    '

      ECD = ECD+1
      ERRCOD(ECD)='555'
      ERRMSG(ECD)='File Unit/Name conflict across options: GRP# AVE  '

      ECD = ECD+1
      ERRCOD(ECD)='560'
      ERRMSG(ECD)='User Specified File Unit .LE. 30 for OU Keyword:  '

      ECD = ECD+1
      ERRCOD(ECD)='565'
      ERRMSG(ECD)='Possible Conflict With Dynamically Allocated FUNIT'

      ECD = ECD+1
      ERRCOD(ECD)='570'
      ERRMSG(ECD)='Problem Reading Temporary Event File for Event:   '

      ECD = ECD+1
      ERRCOD(ECD)='580'
      ERRMSG(ECD)='End of File Reached Trying to Read the File of    '

      ECD = ECD+1
      ERRCOD(ECD)='585'
      ERRMSG(ECD)='Output data file for INITFILE option was not found'

      ECD = ECD+1
      ERRCOD(ECD)='590'
      ERRMSG(ECD)='The INITFILE filename matches a SAVEFILE filename '

      ECD = ECD+1
      ERRCOD(ECD)='592'
      ERRMSG(ECD)='MAXIFILE includes data past start of MULTYEAR run '
      ECD = ECD+1
      ERRCOD(ECD)='593'
      ERRMSG(ECD)='POSTFILE includes data past start of MULTYEAR run '

!-----------------------------------------------------------------------
!---- 600s -------------------------------------------------------------
!-----------------------------------------------------------------------

!RCO 2/25/2021 need to resolve error code numbers
!---- CERC 11/30/20 New messages for GRSM option
      ECD = ECD+1
      ERRCOD(ECD)='600'
      ERRMSG(ECD)='Keyword Invalid w/o PVMRM/OLM/GRSM/TTRM:        '

      ECD = ECD+1
      ERRCOD(ECD)='601'
      ERRMSG(ECD)='Error calculating travel time for GRSM option:  '

      ECD = ECD+1
      ERRCOD(ECD)='602'
      ERRMSG(ECD)='Following Keyword Invalid Without GRSM:         '

      ECD = ECD+1
      ERRCOD(ECD)='603'
      ERRMSG(ECD)='Not Enough NOx Concentrations Specified           '

      ECD = ECD+1
      ERRCOD(ECD)='604'
      ERRMSG(ECD)='NOXVALUE, NOX_VALS or NOX_FILE Keyword Needed for '

      ECD = ECD+1
      ERRCOD(ECD)='605'
      ERRMSG(ECD)='Both NOXVALUE and NOX_VALS keywords are specified '

      ECD = ECD+1
      ERRCOD(ECD)='606'
      ERRMSG(ECD)='Inconsistent temporally-varying NOX_VALS options: '

      ECD = ECD+1
      ERRCOD(ECD)='607'
      ERRMSG(ECD)='No Hrly NOx & No Sub; Assume zero conc,   KURDAT ='

      ECD = ECD+1
      ERRCOD(ECD)='608'
      ERRMSG(ECD)='Date/time Mismatch: NOX_FILE File, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='609'
      ERRMSG(ECD)='No Hrly NOx & No Sub; Use zero conc,      KURDAT ='

      ECD = ECD+1
      ERRCOD(ECD)='610'
      ERRMSG(ECD)='NOXSUB for missing hourly NOX val, KURDAT/Sector ='

      ECD = ECD+1
      ERRCOD(ECD)='611'
      ERRMSG(ECD)='NOXFIL w/o NOXVALs; zero bgd for hrs wth miss NOx '

      ECD = ECD+1
      ERRCOD(ECD)='612'
      ERRMSG(ECD)='No NOx bgd.  NOx bgd calc from NO2 assuming equil '

      ECD = ECD+1
      ERRCOD(ECD)='613'
      ERRMSG(ECD)='NO2 bgd value > NOx bgd value. Setting NOx = NO2. '

      ECD = ECD+1
      ERRCOD(ECD)='614'
      ERRMSG(ECD)='Src type not recognised in instant. plume calc.   '

      ECD = ECD+1
      ERRCOD(ECD)='615'
      ERRMSG(ECD)='Error allocating variables for plume size calc.   '

      ECD = ECD+1
      ERRCOD(ECD)='620'
      ERRMSG(ECD)='Both RLINE barriers on same side of SRCID: '

! --- New error code for NOMINO3
      ECD = ECD+1
      ERRCOD(ECD)='621'
      ERRMSG(ECD)='No applicable NO2 option set with '

! --- New messages related to platform downwash: 3/18/2022, CRT
      ECD = ECD+1
      ERRCOD(ECD)='631'
      ERRMSG(ECD)='PLATFORM Keyword Specified for Non-POINT Source'

      ECD = ECD+1
      ERRCOD(ECD)='632'
      ERRMSG(ECD)='Duplicate PLATFORM Keyword Specified for Source'

      ECD = ECD+1
      ERRCOD(ECD)='633'
      ERRMSG(ECD)='PRIME and PLATFORM Parameters Specified for Source'

!CRT  D036 CRT 3/8/2022: Add new error message for 'CCC' placeholder
!CRT  leftover from v.21112
      ECD = ECD+1
      ERRCOD(ECD)='640'
      ERRMSG(ECD)='Invalid source type: '

!CRT  CRT 4/20/2022 D113 - Added for Sidewash source - negative concentratoin
      ECD = ECD+1
      ERRCOD(ECD)='650'
      ERRMSG(ECD)='SWPOINT neg. conc. set to 0.0 (see debug), Hr: '

!    D128 4/5/23: Added warning message when the AREA meander keyword is used without an AREA source
      ECD = ECD+1
      ERRCOD(ECD)='668'
      ERRMSG(ECD)='AREAMNDR used without an AREA source present '

!-----------------------------------------------------------------------
!---- 700s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ECD = ECD+1
      ERRCOD(ECD)='710'
      ERRMSG(ECD)='TTRM NO2 processing not currently configured for '

!     Added error statement for RLINE barrier or depressed sources missing
!     FLAT option Wood 10/10/22
      ECD = ECD+1
      ERRCOD(ECD)='713'
      ERRMSG(ECD)='Non-DFAULT FLAT required for RBARRIER/RDEPRESSION '

      ECD = ECD+1
      ERRCOD(ECD)='720'
      ERRMSG(ECD)='Non-fatal Error Occurs Writing to the File of  '

      ECD = ECD+1
      ERRCOD(ECD)='721'
      ERRMSG(ECD)='2nd NOx-to-NO2 option not selected for TTRM2 '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ECD = ECD+1
      ERRCOD(ECD)='722'
      ERRMSG(ECD)='PVMRM NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ECD = ECD+1
      ERRCOD(ECD)='723'
      ERRMSG(ECD)='TTRM2 NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ECD = ECD+1
      ERRCOD(ECD)='724'
      ERRMSG(ECD)='GRSM NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ECD = ECD+1
      ERRCOD(ECD)='725'
      ERRMSG(ECD)='OLM NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 4/21/23 CRT
      ECD = ECD+1
      ERRCOD(ECD)='726'
      ERRMSG(ECD)='ARM2 NO2 processing not currently configured for '

!     D164 2/21/23 WSP Message when SCREEN option is used with incompatible sources
      ECD = ECD+1
      ERRCOD(ECD)='731'
      ERRMSG(ECD)='SCREEN processing not currently configured for '

!     D081 - Added for 24-hr average when less than 18 hours of data are present Wood 9/28/22
      ECD = ECD+1
      ERRCOD(ECD)='732'
      ERRMSG(ECD)='24-hr avg, < 18 hours of data, calms policy used.'

!     D081 - Added for 8-hr average when less than 6 hours of data are present CRT 4/28/2023
      ECD = ECD+1
      ERRCOD(ECD)='733'
      ERRMSG(ECD)='8-hr avg, < 6 hours of data, calms policy used.'

!     D081 - Added for 3-hr average when less than 3 hours of data are present CRT 4/28/2023
      ECD = ECD+1
      ERRCOD(ECD)='734'
      ERRMSG(ECD)='3-hr avg, < 3 hours of data, calms policy used.'

!     D157 WSP 3/28/2023 - Added for ARMRATIO limits when cahnged from DFAULT 0.5 and 0.9
!     D157 CRT 5/31/2023 - Modify message when ARMRATIO is within default range.
      ECD = ECD+1
      ERRCOD(ECD)='736'
      ERRMSG(ECD)='ARMRATIO within DFAULT range, 0.5 - 0.9 '
!     ERRMSG(ECD)='ARMRATIO in DFAULT range, 0.5 <= ARMRATIO <= 0.9 '

!     D157 CRT 5/31/2023 - Modify warning when ARMRATIO is outside of default range.
      ECD = ECD+1
      ERRCOD(ECD)='737'
      ERRMSG(ECD)='ARMRATIO outside of DFAULT range of 0.5 - 0.9 '
!     ERRMSG(ECD)='Non-Default ARMRATIO range, 0 < ARMRATIO <= 1 '

!     Added for HBP
      ECD = ECD+1
      ERRCOD(ECD)='740'
      ERRMSG(ECD)='HBP option ignored for non-POINT source: '

!     D167 Warning message when 'FLAT' is used in the source elevation
!     field on the SO LOCATION pathway WSP 3/6/23
      ECD = ECD+1
      ERRCOD(ECD)='752'
      ERRMSG(ECD)='ZHILL and ZELEV are ignored for flat source: '

!-----------------------------------------------------------------------
!---- 800s -------------------------------------------------------------
!-----------------------------------------------------------------------
!**  Added for Aircraft Plume Rise; UNC-IE

      ECD = ECD+1
      ERRCOD(ECD)='807'
      ERRMSG(ECD)='Not enough ARCFTSRC parameters for SRCID = '

      ECD = ECD+1
      ERRCOD(ECD)='821'
      ERRMSG(ECD)='Missing Mandatory Keyword for ARCFTSRC in CO PTHWY '

      ECD = ECD+1
      ERRCOD(ECD)='822'
      ERRMSG(ECD)='Missing Mandatory Keyword for Aircraft. Keyword is '

      ECD = ECD+1
      ERRCOD(ECD)='823'
      ERRMSG(ECD)='No HOUREMIS File Found, Must for Aircraft Source '

      ECD = ECD+1
      ERRCOD(ECD)='824'
      ERRMSG(ECD)='Not All Aircraft Parameters defined;set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='825'
      ERRMSG(ECD)='Too many Aircraft Parameters defined;set to 0. for '

      ECD = ECD+1
      ERRCOD(ECD)='826'
      ERRMSG(ECD)='FuelBurnRate in HOUREMIS file < 0;set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='827'
      ERRMSG(ECD)='Thrust in HOUREMIS file < 0; set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='828'
      ERRMSG(ECD)='AircraftSpeed in HOUREMIS file < 0;set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='829'
      ERRMSG(ECD)='AirFuelRatio in HOUREMIS file < 0;set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='830'
      ERRMSG(ECD)='ByPR in HOUREMIS < 0 & not as -999;set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='831'
      ERRMSG(ECD)='RPWR in HOUREMIS < 0 & not as -99999;set 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='832'
      ERRMSG(ECD)='SrcAngle in HOUREMIS != -20 to 20;set to 0.0 for '

      ECD = ECD+1
      ERRCOD(ECD)='833'
      ERRMSG(ECD)='This source does not fall into Aircraft category '

!**  End Aircraft Plume Rise insert; April 2023
   END SUBROUTINE ERRWRNMSG



END MODULE MAIN1

MODULE RLINE_DATA
!***********************************************************************
!     This is The Global Variable Definition Block for the New RLINE
!     Source Algorithm - December 2017 (Wood)
!
!     MODIFIED:   Michelle G. Snyder, Wood - 6/22/2021
!     MODIFIED:   Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!***********************************************************************

!     Indicate RLINE algorithms based on R-LINE model version 1.2.
   CHARACTER (LEN=12)  :: RLINEver = "RLINEv1_2"

   INTEGER :: NRLINES
   LOGICAL :: RLPROCESSED
   LOGICAL :: RLFIRSTHR
   LOGICAL :: RLMOVESCONV
   LOGICAL :: L_RDEPRESS
   LOGICAL :: L_RBARRIER
!     NRLINES     = number of RLINE sources
!     RLPROCESSED = flag to perform rotation on first RLINE source of each hour
!     RMFIRSTHR   = flag to compute CREATE_EXP_TABLE and RLEMCONV only once
!     RLMOVESCONV = flag to indicate if input units from MOVES
!     L_RDEPRESS  = flag to indicate key word of "RDEPRESS" in source section of input file
!     L_RBARRIER  = flag to indicate key word of "RBARRIER" in source section of input file

   TYPE AVSOURCE
      INTEGER   :: ISRCNUM
      DOUBLE PRECISION  ::  XSB, YSB, ZSB
      DOUBLE PRECISION  ::  XSE, YSE, ZSE
      DOUBLE PRECISION  ::  DCL, INIT_SIGMAZ, WIDTH, QEMIS
      DOUBLE PRECISION  ::  HTWALL, DCLWALL, DEPTH, WTOP, WBOTTOM
      DOUBLE PRECISION  ::  HTWALL2, DCLWALL2
   END TYPE
!     ISRCNUM     = stored ISRC number
!     XSB         = x-coordinate of beginning point (center line for depressed roadway)
!     YSB         = y-coordinate of beginning point (center line for depressed roadway)
!     ZSB         = z-coordinate of beginning point (center line for depressed roadway)
!     XSE         = x-coordinate of end point (center line for depressed roadway)
!     YSE         = y-coordinate of end point (center line for depressed roadway)
!     ZSE         = z-coordinate of end point (center line for depressed roadway)
!     DCL         = offset distance from center line
!     INIT_SIGMAZ = initial vertical spread
!     WIDTH      = width of roadway
!     QEMIS       = emission rate per unit length of road
!     HTWALL      = height of barrier 1
!     DCLWALL     = barrier 1 distance from center line
!     HTWALL2     = height of barrier 2
!     DCLWALL2    = barrier 2 distance from center line
!     DEPTH       = depth of depression
!     WTOP        = width of top of depression
!     WBOTTOM     = width of bottom of depression

!     Allocatable array of sources
   TYPE(AVSOURCE),    ALLOCATABLE  :: RLSOURCE(:)

!     Array to store MOVES to RLINE native units
   DOUBLE PRECISION,  ALLOCATABLE  :: RLEMISCONV(:)

!     Meteorological variables
   DOUBLE PRECISION  :: SIGMAV, DISPHT, UEFF, THETAW, RLWSTAR
   DOUBLE PRECISION  :: WSPD_ADJ, UMIN, SIGZ_Y
   DOUBLE PRECISION  :: Z0_A(3), DH_A(3), UST_A(3), LMO_A(3)
!     SIGMAV      = lateral turbulence intensity
!     DISPHT      = displacement height
!     UEFF        = effective wind speed
!     THETAW      = standard deviation of wind direction
!     RLWSTAR     = WSTAR used for sigmav calculation in RLCALC
!     WSPD_ADJ    = Ratio of ref wind speed to its computed value from MOST
!     UMIN        = Minimum wind speed, calculated in COMPUTE_MET
!     SIGZ_Y      = vertical dispersion coefficient used for SIGMAY calculation
!     Z0_A        = array of values for SFCZ0; 1 = no barrier, 2 = downwind barrier, 3 = upwind barrier
!     DH_A        = array of values for DISPHT; 1 = no barrier, 2 = downwind barrier, 3 = upwind barrier
!     UST_A       = array of values for USTAR; 1 = no barrier, 2 = downwind barrier, 3 = upwind barrier
!     LMO_A       = array of values for OBULEN; 1 = no barrier, 2 = downwind barrier, 3 = upwind barrier

!     Run option parameters
!      DOUBLE PRECISION, PARAMETER  :: ERROR_LIMIT = 5.0E-4 !D178_RLINE_RecpOrder_WSP
   DOUBLE PRECISION, PARAMETER  :: ERROR_LIMIT = 5.0D-4
   DOUBLE PRECISION  :: FAC_DISPHT !removed = 5.0D0  and PARAMETER definition (wood 6/22/21)
!     ERROR_LIMIT = RLINE error limit
!     FAC_DISPHT  = ratio of displacement height to roughness length (DISPHT=FAC_DISPTH*SFCZ0)

!     Source variables
   INTEGER           :: INDQ
   DOUBLE PRECISION  :: SIGMAY0, SIGMAZ0, HRELEASE
   DOUBLE PRECISION  :: XSBEGIN, YSBEGIN, ZSBEGIN
   DOUBLE PRECISION  :: XSEND, YSEND, ZSEND
!     INDQ        = source index
!     SIGMAY0     = initial horizontal dispersion coefficient
!     SIGMAZ0     = initial vertical dispersion coefficient
!     HRELEASE    = source height
!     XSBEGIN     = x-coordinate of beginning point of line source
!     YSBEGIN     = y-coordinate of beginning point of line source
!     XSBEGIN     = z-coordinate of beginning point of line source
!     XSEND       = x-coordinate of end point of line source
!     YSEND       = y-coordinate of end point of line source
!     ZSEND       = z-coordinate of end point of line source

!     Computation parameters
!MGS      DOUBLE PRECISION, PARAMETER  :: SM_NUM = 1.0E-8 !D178_RLINE_RecpOrder_WSP
   DOUBLE PRECISION, PARAMETER  :: SM_NUM = 1.0D-8
   DOUBLE PRECISION, PARAMETER  :: XD_MIN = 1.0D0
   INTEGER, PARAMETER           :: NP = 100
!     SM_NUM      = number for numerical calculations to avoid a zero
!     XD_MIN      = minimum distance between a source and receptor
!     NP          = number of points in wind speed table

!     Computation variables
   DOUBLE PRECISION  :: XEXP(1000), AEXP(1000), BEXP(1000), DELEXP
   DOUBLE PRECISION  :: ZWIND(NP,3), AWIND(NP,3), BWIND(NP,3)
   DOUBLE PRECISION  :: DELZ(3), LOGZMAX, LOGZMIN(3)              ! CREATE_WIND_TABLE
   INTEGER           :: I_ALPHA
   DOUBLE PRECISION  :: XRECEP, YRECEP, ZRECEP
   DOUBLE PRECISION, ALLOCATABLE  :: XRCP_ROT(:), YRCP_ROT(:)
   DOUBLE PRECISION  :: X0, Y0
   DOUBLE PRECISION  :: XR_ROT, YR_ROT
   DOUBLE PRECISION, ALLOCATABLE  :: XSB_ROT(:), YSB_ROT(:),&
   &XSE_ROT(:), YSE_ROT(:)

   INTEGER, ALLOCATABLE :: BDW_FLAG(:,:)
   LOGICAL              :: SHIFT_FLAG
   DOUBLE PRECISION     :: SZB, XPER, THETA_LINE
   DOUBLE PRECISION     :: DWU, DW_PERU, HBU
   DOUBLE PRECISION     :: DWD, DW_PERD, HBD
   INTEGER              :: NBARR
   DOUBLE PRECISION     :: UH, XSHIFT, YSHIFT
   DOUBLE PRECISION     :: ALPHA, ALPHA_U, ALPHA_D
   LOGICAL              :: FASTRLINE
   DOUBLE PRECISION     :: PSY1, PSY2, PSY3, PSY4
   DOUBLE PRECISION     :: PSZ1, PSZ2, PSZ3, PSZ4
   DOUBLE PRECISION     :: PU1, PU2, PU3, PU4

!     XEXP        = exponential lookup
!     AEXP        = exponential lookup
!     BEXP        = exponential lookup
!     DELEXP      = exponential lookup
!     I_ALPHA     = index for choosing alpha value
!     XRECEP      = dummy x receptor
!     YRECEP      = dummy y receptor
!     ZRECEP      = dummy z receptor
!     XRCP_ROT    = x-coordinate of rotated receptor
!     YRCP_ROT    = y-coordinate of rotated receptor
!     X0          = system x origin
!     Y0          = system y origin
!     XR_ROT      = x-coordinate of rotated receptor
!     YR_ROT      = y-coordinate of rotated receptor
!     XSB_ROT     = x-coordinate of rotated source beginning point
!     YSB_ROT     = y-coordinate of rotated source beginning point
!     XSE_ROT     = x-coordinate of rotated source end point
!     YSE_ROT     = y-coordinate of rotated source end point

!     BDW_FLAG    = flag to indicate if downwind barrier exists; if == 1, yes
!     SHIFT_FLAG  = flag to indicate if shift in x, z, alpha should be applied; if == TRUE, apply shift
!     SZB         = additional sigmaz due to barrier
!     XPER        = source_receptor_distance (along wind)
!     THETA_LINE  = Angle between line source and wind direction
!     DWU         = along wind distance between source and upwind barrier
!     DW_PERU     = perpendicular distance between source and upwind barrier
!     HBU         = height of upwind barrier
!     DWD         = along wind distance between source and downwind barrier
!     DW_PERD     = perpendicular distance between source and downwind barrier
!     HBD         = height of downwind barrier
!     NBARR       = number of barriers
!     UH          = wind speed at barrier height
!     XSHIFT      = amount to shift x if in the recirculation zone of upwind barrier
!     YSHIFT      = amount to shift y if in the recirculation zone of upwind barrier
!     ALPHA       = enhancement of ustar due to barrier presence
!     ALPHA_U     = alpha for upwind barrier
!     ALPHA_D     = alpha for downwind barrier
!     ZWIND       = heights for wind speed table
!     AWIND, BWIND = interpolation constants for wind speed table
!     DELZ, LOGZMAX, LOGZMIN = parameters used in wind speed table

!     FASTALL INTERPOLATION COEFFICIENTS
!     PSY1, PSY2, PSY3, PSY4  = INTERPOLATION COEFFS FOR SIGMAY
!     PSZ1, PSZ2, PSZ3, PSZ4  = INTERPOLATION COEFFS FOR SIGMAZ
!     PU1, PU2, PU3, PU4  = INTERPOLATION COEFFS FOR UEFF

!     Variables needed for DEBUG file output
   DOUBLE PRECISION  :: FRAN_SUM, SIGMAV_SUM, UEFF_SUM,&
   &VERT_SUM, HORZ_SUM, CONC_P_SUM, CONC_M_SUM,&
   &POINT_CONC_SUM

END MODULE RLINE_DATA

MODULE BUOYANT_LINE
!***********************************************************************
!     This is The Global Variable Definition Block for the New BUOYLINE
!     Source Algorithm - January 2015
!***********************************************************************


! BuoyantLine_CheckLinesParallel_D32 (Wood)
   DOUBLE PRECISION, PARAMETER :: Parallel_Tol = 5.0D0


! Multiple_BuoyLines_D41_Wood
!     Added several variables and changed a few arrays from 1-D to 2_D
!     to process multiple buoyant lines

   TYPE BLINEDATA
      INTEGER   (kind=4)   :: ISRCNUM
      CHARACTER (len=12)   :: SRCID

! Multiple_BuoyLines_D41_Wood
!        Added for processing multiple buoyan lines
      INTEGER   (kind=4)   :: IBLPGRPNUM
      CHARACTER (len=8)    :: BLPGRPNAME

!         INTEGER   (kind=4)   :: IURBSRCNUM
!         CHARACTER (len=8)    :: URBSRCNAME

      DOUBLE PRECISION     :: XBEG, YBEG, XEND, YEND                 ! untranslated, unrotated
      DOUBLE PRECISION     :: XBEG_TR1, YBEG_TR1, XEND_TR1, YEND_TR1 ! translated, rotate #1 w/TCOR
      DOUBLE PRECISION     :: ELEV, BLQS, BLHS
   END TYPE BLINEDATA

!     ISRCNUM = source number in the list of sources for the model run
!     SRCID   = source ID in the control file for this line number
!     XBEG    = x-coordinate of beginning of line (as entered by user)
!     YBEG    = y-coordinate of beginning of line (as entered by user)
!     XEND    = x-coordinate of end of line (as entered by user)
!     YEND    = y-coordinate of end of line (as entered by user)
!     XBEG_TR1 = x-coordinate of beginning of translated, rotated line with TCOR
!     YBEG_TR1 = y-coordinate of beginning of translated, rotated line with TCOR
!     XEND_TR1 = x-coordinate of end of translated, rotated line with TCOR
!     YEND_TR1 = y-coordinate of end of translated, rotated line with TCOR
!     ELEV    = elevation of line
!     BLQS    = emission rate of line source in g/s
!     BLHS    = release height

   TYPE (BLINEDATA), ALLOCATABLE :: BLINEPARMS (:)

!     Whereas the source coordinates in BLINEPARMS are for the entire
!      line, the following are for the segments of the line.
   DOUBLE PRECISION, ALLOCATABLE :: XS_SCS(:,:), YS_SCS(:)
   DOUBLE PRECISION, ALLOCATABLE :: XS_RCS(:,:), YS_RCS(:,:)

!      1st subscript: individual buoyant line number
!      2nd subscript: segment number (not needed for YS_SCS)
!      _SCS = source coordinate system, initial translation/rotation and
!             performed only once
!      _RCS = rotated coordinate system, translated/rotated for wind dir

   LOGICAL              :: L_BLSOURCE, L_BLHOURLY
!     BLPINPUT_Checks_Missing_D091_Wood: begin
!     L_BLSOURCE is set to true if any of the following are true
!       there is at least one BUOYLINE record
!       there is a BLPINPUT record
!       there is a BLPGROUP record
!     BLPINPUT_Checks_Missing_D091_Wood: end

!     L_BLHOURLY - Flag indicating if hourly emissions for buoyant line
!                  source(s) are included in the HREMIS file

!     Multiple_BuoyLines_D41_Wood
!     changed or increased array dimensions on many variables
   LOGICAL, ALLOCATABLE :: L_BLURBAN(:)
   LOGICAL, ALLOCATABLE :: BL_RFLAG(:,:)
   INTEGER, ALLOCATABLE :: NBLINGRP(:)
   INTEGER, ALLOCATABLE :: HRLYBLCOUNT(:)
   INTEGER, ALLOCATABLE :: BL_NUMURB(:)
   INTEGER              :: NBLP, NBLTOTAL, NUMBLGRPS
!     NBLP         = number of individual buoyant lines in run (used to
!                     allocate arrays)
!     NBLTOTAL     = number of individial buoyant lines in run (used in
!                     calculations)
!     NUMBLGRPS    = number of buoyant line sources/groups
!     NBLINGRP     = number of lines in each buoyant line source group
!     BL_RFLAG     = flag indicating if a receptor is inside (true) or
!                     outside (false) the extents of the buoyant line
!                     source; by BL source group since each group could
!                     be oriented differently, creating a different
!                     rectangular footprint (i.e., an exclusion zone)
!                     defined by the buoyant line source
!     BL_NUMURB    = counter for # of lines declared urban - used for QA
!     L_BLSOURCE   = indicates whether or not model run has a BL source
!     L_BLHOURLY   = indicates that at least one buoyant line source is
!                     in the HOUREMIS file
!     L_BLURBAN    = logical that is set to TRUE if at least one
!                    individual line in a BL source is declared URBSRC

!     The following are input via the source keyword BLPINPUT:

   DOUBLE PRECISION, ALLOCATABLE ::&
   &BLAVGINP_LLEN(:), BLAVGINP_BHGT(:), BLAVGINP_BWID(:),&
   &BLAVGINP_LWID(:), BLAVGINP_BSEP(:), BLAVGINP_FPRM(:)
   CHARACTER (len=8), ALLOCATABLE :: BLAVGINP_GRPID(:), BL_GRPID(:)
   INTEGER     NUMBLAVGINP, NBLAVGINPalloc
!     BLAVGINP_LLEN  = average line length (m)
!     BLAVGINP_BHGT  = average building height (m)
!     BLAVGINP_BWID  = average building width (m)
!     BLAVGINP_LWID  = average line source width (m)
!     BLAVGINP_BSEP  = average building separation (m)
!     BLAVGINP_FPRM  = average buoyancy parameter (m^4/s^3)
!     BLAVGINP_ID    = BL source input ID associated with each BLPINPUT
!                       record
!     BL_GRPID       = BL group IDs - used when processing BLPGROUP keywords
!     NUMBLAVGINP    = counter for number of BLPINPUT records processed
!     NBLAVGINPalloc = allocation counter for number of BLPINPUT records
!     The following are the local variables used in the calculations
!       MAKE THEM LOCAL IN BL_CALC AND PASS TO APPROPRIATE ROUTINES
   DOUBLE PRECISION :: BLAVGLLEN, BLAVGBHGT, BLAVGBWID,&
   &BLAVGLWID, BLAVGBSEP, BLAVGFPRM
!     BLAVGLLEN  = average line length (m)
!     BLAVGBHGT  = average building height (m)
!     BLAVGBWID  = average building width (m)
!     BLAVGLWID  = average line source width (m)
!     BLAVGBSEP  = average building separation (m)
!     BLAVGFPRM  = average buoyancy parameter (m^4/s^3)

   DOUBLE PRECISION :: BLTA
   DOUBLE PRECISION :: BL_UREF
!     BLTA       = ambient temperature read from AERMET's surface file
!                  (saved because TA is modified if there are point sources)
!     BL_UREF    = reference wind speed used in BL calculations; set to
!                  1.0 m/s if WS is less than 1.0 m/s

   DOUBLE PRECISION, dimension (7)  :: BL_XDIST = (0.0D0)
   DOUBLE PRECISION, dimension (7)  :: DH = (0.0D0)
!CRT  4/2/2022 D088 Limit on # of buoyant lines, make DEL allocatable
!      DOUBLE PRECISION, dimension (10) :: DEL
   DOUBLE PRECISION, ALLOCATABLE    :: DEL(:)
   DOUBLE PRECISION :: BL_XFB, LEFF, LD, R0, BL_XFINAL, BL_XFS
   DOUBLE PRECISION :: FPRMXNB, XMATCH, PARTCH
!     The following variables are used in a couple subroutines, therefore
!       they must be allocated for multiple buoyant lines

   DOUBLE PRECISION, ALLOCATABLE :: XOR(:), YOR(:), ANGRAD(:)

   DOUBLE PRECISION, ALLOCATABLE :: TCOR(:), SINTCOR(:), COSTCOR(:)
!     LEFF     = effective building length
!     R0       = edge radius
!     LD       = LEFF * sin (theta), where theta = angle between flow
!                vector and the orientation of the line source
!     BL_XDIST = array of intermediate distances for downswsh calculations
!     FPRMXNB  = fprime * number of lines
!     BL_XFB   = distance to full buoyancy
!     BL_XFS   = distance to final rise
!     XMATCH   =
!     AVFACT   =
!     XOR, YOR = defines the origin of the BL group based on the group's
!                first line defined on the SO LOCATION card
!     TCOR     = rotation angle to align the x-axis with the long side
!                of the buoyant line source
!     SINTCOR, COSTCOR = sine and cosine of TCOR

!     For the translated/rotated receptors
!      to be allocated with the number of receptors
   DOUBLE PRECISION, ALLOCATABLE :: XR_SCS(:,:), YR_SCS(:,:)
   DOUBLE PRECISION, ALLOCATABLE :: XR_RCS(:), YR_RCS(:)
!
!      1st SCS subscript:
!      2nd SCS subscript:
!      _SCS = source coordinate system, initial translation/rotation and
!             performed only once
!      _RCS = rotated coordinate system, translated/rotated for wind dir
!
!     The following arrays are used to save the coordinates from the
!       first tranlastion/rotation of the receptors when MAXDCONT is used
   DOUBLE PRECISION, ALLOCATABLE :: XR_SCS_SAV(:,:), YR_SCS_SAV(:,:)

!     The following array is used to save the flag indicating if a
!       receptor is in the rectangular footprint defined by a buoyant
!       line source (exclusion zone) of the buoyant line source when
!       MAXDCONT is used
   LOGICAL, ALLOCATABLE :: BL_RFLAG_SAV(:,:)

!     Arrays for total and partial concentration from lines,
!      to be allocated with the number of receptors
   DOUBLE PRECISION, ALLOCATABLE :: CHIBL(:)

END MODULE BUOYANT_LINE

!-----------------------------------------------------------------------
! --- The following MODULE subprograms replace the *.pri "INCLUDE"
!     files formerly used for global data storage for PRIME, and the
!     /PLU/-named COMMON block used in a few subroutines.
! --- R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009


MODULE PRIME_PARAMS

! --- Formerly part of PARAMS.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- PARAMETER statements                                        PRIME
!----------------------------------------------------------------------

   INTEGER, PARAMETER :: io5=7,io6=8

! --- FORTRAN I/O unit numbers:
!           IO5 - Control file                  - input  - formatted
!           IO6 - List file                     - output - formatted
!

END MODULE PRIME_PARAMS


MODULE PRIME_NUMPARM

! --- Formerly NUMPARM.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /NUMPARM/ -- Parameters used in the            PRIME
!                               numerical plume rise algorithm
!----------------------------------------------------------------------
!
   INTEGER, PARAMETER :: mxnw=5000
   INTEGER, PARAMETER :: mxent=10
   INTEGER, PARAMETER :: mxentp1=mxent+1
   INTEGER :: nstep, nent
   DOUBLE PRECISION :: gravi,rgas,zmin,ds,slast,rp,&
   &alphap(mxent),betap(mxent),xcat(mxentp1)

!
! --- GENERAL PARAMETER definitions:
!          MXNW - Maximum number of downwind distances for numerical
!                 plume rise integration (should be set equal to
!                 SLAST/DS)
!         MXENT - Maximum number of perturbed entrainment coefficients
!                 entered
!
! --- FORTRAN I/O unit numbers:
!           IO5 - Control file                  - input  - formatted
!           IO6 - List file                     - output - formatted
!
! --- NUMPARM Global Variables:
!
!         GRAVI - real    - Acceleration due to gravity (m/s**2)
!          RGAS - real    - Gas constant (m**2/s**2/deg. K)
!          ZMIN - real    - Minimum plume centerline height (m)
!            DS - real    - Step size (m) in the numerical plume
!                           rise algorithm
!         NSTEP - integer - Internal save frequency of plume rise
!                           calculations (i.e., every DS*NSTEP meters)
!                           (NOTE: this the frequency with which the
!                           results are saved internally -- not that
!                           passed back from the NUMRISE routine)
!         SLAST - real    - Termination distance (m) of the plume rise
!                           calculation
!            RP - real    - Radiation coefficient (kg/m**2/deg. K**3/s)
!   ALPHAP(mxent) - real array - Perturbed entrainment coefficients
!                                (parallel)
!    BETAP(mxent) - real array - Perturbed entrainment coefficients
!                                (normal)
!   XCAT(mxentp1) - real array - Downwind distances (m) for which each
!                                perturbed entrainment coefficient
!                                (ALPHAP, BETAP) is valid (NENT+1 values
!                                for NENT entrainment coefficients).
!            NENT - integer    - Number of perturbed entrainment
!                                coefficients entered

END MODULE PRIME_NUMPARM


MODULE PRIME_DFSN

! --- Formerly DFSN.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /DFSN/ -- Parameters used in the            PRIME
!                            PRIME turbulence and diffusion
!                            subroutines
!----------------------------------------------------------------------
!
   DOUBLE PRECISION :: afac,xbyrmax,wiz0,wiy0,wfz,wfy,&
   &dua_ua,xdecay,xdecayi
!
! --- DFSN Global Variables:
!
!          AFAC - real    - Diffusion transitions to ambient (with
!                           virtual source) when wake turbulence decays
!                           to AFAC*(ambient turbulence intensity) for
!                           PG classes 4, 5, and 6
!       XBYRMAX - real    - Upper limit on distance from upwind face
!                           of bldg to transition point for ambient
!                           diffusion
!       WIZ,WIY - real    - Base Turbulence intensities in wake
!       WFZ,WFY - real    - Scaling factors for sigmaz and sigmay
!        DUA_UA - real    - [Ua-U]/Ua in wake at downwind face of bldg
!                                U: average speed in wake
!                               Ua: ambient speed
!         DECAY - real    - Exponent for turbulence intensity change
!                           with distance from downwind face of bldg
!        DECAYI - real    - 1/DECAY
!     RURLIZ(6) - real    - Rural turbulence intensities in z
!     RURLIY(6) - real    - Rural turbulence intensities in y
!     URBNIZ(6) - real    - Urban turbulence intensities in z
!     URBNIY(6) - real    - Urban turbulence intensities in y
! --- Ambient turbulence intensities are inferred from Briggs (1973)
! --- "Diffusion estimation for small emissions", ATDL-106;

END MODULE PRIME_DFSN


MODULE PRIME_WAKEDAT

! --- Formerly WAKEDAT.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /WAKEDAT/ -- Parameters used in the            PRIME
!                               PRIME wake and streamline
!                               subroutines
!----------------------------------------------------------------------
!
   logical lrurl
   INTEGER, PARAMETER :: mxntr=50
   INTEGER :: nwak,ncav
   DOUBLE PRECISION :: Hb,Wb,xLb,Rb,HR,xLR,xLC,&
   &xbadj,ybadj,Ub,Urh,&
   &xwak(mxntr),szwak(mxntr),sywak(mxntr),&
   &drwak(mxntr),&
   &xcav(mxntr),szcav(mxntr),sycav(mxntr),&
   &fqcav,&
   &vsigy, vsigz, vsigyc, vsigzc, zint

   DOUBLE PRECISION :: third  ! constant = 1/3 used in various places
   ! initialized for PRIME in sub. WAKINI

! --- GENERAL PARAMETER definitions:
!         MXNTR - Maximum number of downwind distances for which
!                 numerical plume rise will be reported
!
! --- WAKEDAT Global Variables:
!
!            HB - real    - Building height (m)
!            WB - real    - Building width (crosswind) - (m)
!           XLB - real    - Building length (alongwind) - (m)
!            RB - real    - Scale length (m)
!            HR - real    - Maximum cavity height (m) above ground
!           XLR - real    - Length of downwind cavity (m) from
!                           downwind face of building
!           XLC - real    - Length of roof cavity (m)
!         XBADJ - real    - Distance along the wind from the stack to
!                           the origin of the building (upwind center
!                           of effective building)
!         YBADJ - real    - Distance crosswind from the stack to
!                           the origin of the building (upwind center
!                           of effective building)
!            Ub - real    - Wind speed (m/s) at the height of bldg
!           Urh - real    - Wind speed (m/s) at release height
!
!          NWAK - integer - Number of downwind distances at which
!                           wake properties are tabulated (LE mxntr)
!   XWAK(mxntr) - real    - Downwind distance (m) from source
!  SZWAK(mxntr) - real    - Sigma-z (m) at position XWAK
!  SYWAK(mxntr) - real    - Sigma-y (m) at position XWAK
!  DRWAK(mxntr) - real    - Plume growth rate at position XWAK expressed
!                           as d/dx(plume radius) for equivalent top-hat
!          NCAV - integer - Number of downwind distances at which
!                           wake properties of cavity source are
!                           tabulated (LE mxntr)
!   XCAV(mxntr) - real    - Downwind distance (m) from primary source
!  SZCAV(mxntr) - real    - Sigma-z (m) for cavity source
!  SYCAV(mxntr) - real    - Sigma-y (m) for cavity source
!         FQCAV - real    - Fraction of plume mass captured by cavity
!         ISTAB - integer - PG stability class
!         LRURL - logical - Rural dispersion when .TRUE.
!         VSIGZ - real    - Virtual source sigma (m) for sigma-z beyond wake
!         VSIGY - real    - Virtual source sigma (m) for sigma-y beyond wake
!        VSIGZC - real    - Virtual source sigma (m) for sigma-z beyond wake
!                           for cavity source
!        VSIGYC - real    - Virtual source sigma (m) for sigma-y beyond wake
!                           for cavity source

END MODULE PRIME_WAKEDAT


MODULE PRIME_AMBIENT

! --- Formerly AMBIENT.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /AMBIENT/ -- Selected met. data at one         PRIME
!                               grid cell;  used in numerical
!                               plume rise computation
!----------------------------------------------------------------------
!
   INTEGER, PARAMETER :: mxnz=100
   INTEGER, PARAMETER :: mxnzp1=mxnz+1
   INTEGER :: NZA
   DOUBLE PRECISION :: uamb(mxnz),ramb(mxnz),dedz(mxnzp1),tamb(mxnz),&
   &zfacea(mxnzp1),zgpta(mxnz),tamb0,ramb0,adia,ptgrad0

! --- GENERAL PARAMETER definitions:
!          MXNZ - Maximum number of vertical layers in
!                 the meteorological data
!
! --- COMMON BLOCK /AMBIENT/ Variables:
!
!                    NZA - integer - Number of layers
!             UAMB(mxnz) - real    - Wind speed profile (m/s) - winds
!                                    defined at cell CENTERS
!             RAMB(mxnz) - real    - Ambient air density profile
!                                    (kg/m**3) - defined at cell CENTERS
!           DEDZ(mxnzp1) - real    - Pot. temperature gradient profile
!                                    (deg. K/m) - defined at cell FACES
!             TAMB(mxnz) - real    - Temperature profile (deg .K) -
!                                    defined at cell CENTERS
!         ZFACEA(mxnzp1) - real    - Heights of layer faces (m)
!            ZGPTA(mxnz) - real    - Heights of layer centers (m)
!                  TAMB0 - real    - Surface air temperature (deg. K)
!                  RAMB0 - real    - Surface air density (kg/m**3)
!                   ADIA - real    - Dry adiabatic lapse rate (deg. K/m)
!                PTGRAD0 - real    - Minimum potential temperature lapse
!                                    rate (deg. K/m)

END MODULE PRIME_AMBIENT


MODULE PRIME_PLU

! --- Formerly COMMON /PLU/ in selected PRIME subroutines:
!
!----------------------------------------------------------------------
! --- Notation --- in (KG,M,S) units
!               S:      LENGTH ALONG PLUME CENTERLINE
!               X:      PLUME LOCATION (downwind from source)
!               Y:      PLUME LOCATION (crosswind from source)
!               Z:      PLUME HEIGHT
!               R:      PLUME RADIUS
!               U:      PLUME HORIZONTAL (ALONGWIND) VELOCITY COMPONENT
!               V:      PLUME CROSSWIND VELOCITY COMPONENT
!               W:      PLUME VERTICAL VELOCITY COMPONENT
!               USC:    VELOCITY ALONG PLUME CENTERLINE
!               PHI:    ANGLE BETWEEN PLUME TRAJECTORY AND GROUND
!               DEN:    PLUME DENSITY
!               TP:     PLUME TEMPERATURE
!----------------------------------------------------------------------

   DOUBLE PRECISION :: S,X,Y,Z,R,U,V,W,USC,PHI,DEN,TP

END MODULE PRIME_PLU

! --- Additional modules for AWMADW

!     Make effective height, calculated in PRMCALC, available to wake_u_turb
MODULE PRM2_WAKEDAT
   INTEGER, PARAMETER :: p2mxntr=50
   DOUBLE PRECISION Zeff_PRM2, u30, sv30, sw30
!         DOUBLE PRECISION XTR, ZTR, NTR
   DOUBLE PRECISION XTR_SAV(p2mxntr), ZTR_SAV(p2mxntr)
   DOUBLE PRECISION Ueff_save, SWeff_save, SVeff_save
   LOGICAL DFSN2CALL
END MODULE PRM2_WAKEDAT

!     CERC 11/30/20 Module for GRSM NO2 option
MODULE GRSMMOD

!-----GRSM global variables/parameters---------------------------------------------------
!     R1:                    Reaction rate for NO + O3 -> NO2
!     R2:                    Reaction rate for NO2 + hv -> NO + O3
!     NOXCONC_BG:            Equilibrium background NOx concentration (ppb)
!     NO2CONC_BG:            Equilibrium background NO2 concentration (ppb)
!     NOCONC_BG:             Equilibrium background NO concentration (ppb)
!     O3CONC_BG:             Equilibrium background ozone concentration (ppb)
!     nNO:                   NO enumeration constant
!     nNO2:                  NO2 enumeration connstant
!     nO3:                   Ozone enumeration constant
!     CONCTEMP(1:nPolsGRSM): Holds temporary concentrations during chemistry calculations
!     L_NightHour:           Logical indicating whether night-time hour or not
!     CFrac:                 Maximum fractional change in concentration over one time step
!     MinimumConc:           Minimum concentration value to prevent chemistry solver NaNs
!-----------------------------------------------------------------------------------------

   DOUBLE PRECISION :: R1, R2
   DOUBLE PRECISION :: NOXCONC_BG,NO2CONC_BG,NOCONC_BG,O3CONC_BG
   INTEGER, PARAMETER :: nNO=1, nNO2=2, nO3=3, nPolsGRSM=3
   DOUBLE PRECISION :: CONCTEMP(nPolsGRSM)
   LOGICAL :: L_NightHour
   DOUBLE PRECISION, PARAMETER :: CFrac = 1.0D-2
! MKP    4/23/2024 D193 fix provided by CERC
!        Prevents denormal or very small concentration values from being
!        passed to chemistry solver resulting in NaNs for certain ground
!        level releases from area, volume, and openpit source types
!        See grsm.f/DoGRSMChem
   DOUBLE PRECISION, PARAMETER::MinimumConc=1.0D-21

END MODULE GRSMMOD

