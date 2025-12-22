module main1
!***********************************************************************
!     MAIN1
!     AERMOD Model Data - Parameter, Variable and Array Declarations
!                         Global Data for All Modules
!
!***********************************************************************

   implicit none

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
   integer, parameter :: nwscat= 6, nkst= 6, nhr= 24,&
   &npair= 100, nhiann= 10,&
   &nmxpm= 10, mxplvl=50, mxglvl=87

   integer :: nyears

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
   integer, parameter :: ifmax=150, ikn=120, istrg=512, ilen_fld=200,&
   &ierrn=500
!**   IFMAX  = Max Number of Fields Per Runstream Record
!**   IKN    = Number of Keywords
!**   ISTRG  = Max Length of Runstream Image Record
!**   ILEN_FLD = Max Length of Runstream Input Fields.  Also used
!**              to specify length of input filenames and formats.
!**   IERRN  = Array length of Error/Warning/Informational Message arrays


!***********************************************************************
!     Model Constants Specified as Parameters
!***********************************************************************

   double precision, parameter ::&
   &g = 9.80616d0,       vonkar = 0.4d0,&
   &govrcp = 0.00977d0,  dctodk = 273.16d0,&
   &beta1  = 0.6d0,      beta2  = 0.4d0,&
   &at1pt2 = 1.2d0,      umingr = 0.01d0,&
   &gsigv  = 0.073864d0, efoldh = 0.44d0,&
!**   CRT 9/11/2020, D062 User Minimum Sigma W
!**     &                    SVUMIN = 0.05D0,     SWMIN  = 0.02D0,
   &svumin = 0.05d0,&
   &xval   = 0.0d0,      sptgmn = 0.002d0,&
   &bsubc  = 0.5d0,      alphar = 1.4d0,&
   &lamday = 2.3d0,      asube  = 0.1d0,&
   &refpop = 2.0d+6,     deltrur= 12.0d0,&
   &rgas   = 8.3145d0,&
   &awma_beta_entraincoeff = 0.35d0,&
!**  Added for Aircraft Plume Rise; UNC-IE
   &r00 = 2.0d0, alpham = 0.10d0,&
   &paa = 1.013d+5, raa = 287.058d0,&
   &hfuel = 4.3d+7, cpa = 1003.0d0
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

   double precision :: szcoef    ! SZCOEF

!**   Declare SVMIN variable for minimum sigma-v, formerly specified as
!**   PARAMETER SVMIN.  Default value of 0.2D0 is initialized in SUBROUTINE
!**   MODOPT; (SVMIN was increased to 0.5D0 under the old LOWWIND1 BETA option,
!**   and increased to 0.3D0 under the old LOWWIND2 and LOWWIND3 options)
!**   optional user inputs under the CO LOW_WIND keyword.
   double precision :: svmin

!**   Declare WSMIN variable for minimum wind speed, formerly set to 0.2828
!**   based on SQRT(2*SVmin*SVmin), where SVmin was 0.2. The default value
!**   of WSMIN is also set to 0.2828, but may be adjusted under the LOWWIND
!**   BETA options using the CO LOW_WIND keyword.
   double precision :: wsmin

!**   Declare FRANMAX variable for maximum meander factor, FRAN, used in
!**   the LowWind2 BETA option.  The "default" value for the LowWind2
!**   option is set at 0.95, but can be modified by the user under the
!**   optional LOW_WIND keyword on the CO pathway, within a range of
!**   0.50 to 1.0, inclusive.
   double precision :: franmax

!**   3/18/2022 D127 - add FRANMIN for meander testing -Wood
   double precision :: franmin

!**   CRT 9/11/2020, D062 User Minimum Sigma W
!**   Declare SWMIN variable for user-defined minimum sigma-w formerly
!**   specified as PARAMETER SWMIN.  The default value will be set to
!**   the former parameter value = 0.02 m/s, but can be modified under
!**   the optional LOW_WIND keyword on the CO pathway within a range of
!**   0.00 to 3.00 which is consistent with the default range check by
!**   AERMET when SWnn is included as an onsite variable.
   double precision :: swmin

!**   RCO 9/27/2020, D061 User BIGT values
!**   Declare BIGT variable for user-defined BIGT value, formerly
!**   specified as PARAMETER BIGT.  The default value will be set to
!**   the former parameter value = 12 hours, but can be modified under
!**   the optional LOW_WIND keyword on the CO pathway within a range of
!**   0.50 to 48.00. Previous LOWWIND2 option set at 12 hours, not
   double precision :: bigt


!**   Set concentration unit conversion factors for NO2, SO2, CO, and 03
!**   for use with OZONEVAL, OZONEFIL, O3VALUES, and BACKGRND keywords;
!**   factors defined by pollutant for PPB-to-UG/M3 and PPM-to-UG/M3,
!**   based on reference temperature (25 C) and pressure (1013.25 mb).
!**   Note that factors for NO2 and SO2 are PPB/(UG/M3) and PPM/(UG/M3),
!**   and factors for CO and O3 are for (UG/M3)/PPB and (UG/M3)/PPM.
   double precision, parameter ::&
   &no2_ppb = 0.5319d0, no2_ppm = 0.5319d-3,&
   &so2_ppb = 0.3823d0, so2_ppm = 0.3823d-3,&
   &co_ppb  = 1.144d0,  co_ppm  = 1.144d3,&
   &o3_ppb  = 1.960d0,  o3_ppm  = 1.960d3


   double precision :: pi, twopi, rtofpi, srt2pi, rtof2,&
   &rtpiby2, rt2bypi,&
   &dtorad, rtodeg,&
   &third, twothirds

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
   double precision, parameter :: numsyeff  = 4.0d0

!**   MAXDIST = Maximum transport distance for calculation;
!**             set to 80km for FASTALL or FASTAREA options;
!**             this was formerly associated with the TOXICS
!**             option, which is now obsolete.
!**             Set to 1.0D20 for applications w/o FASTALL or FASTAREA
   double precision :: maxdist


!***********************************************************************
!     Common Block for Input/Output File Units (Initialized in BLOCK DATA)
!***********************************************************************

!CRT  D063 Add Platform Downwash Debug PLATFMDBUNT
!     Wood 10/10/22 added GRID_WS debug RLINEDBUNT_WS
   integer :: inunit, iounit, mfunit, mpunit, ierunt, ierwrt,&
   &idpunt, idpun2, irsunt, ievunt, itevut, ihremi,&
   &ibgunt(6), io3unt(6), incunt, isumunt, dbgunt, dbmunt,&
   &areadbunt, gdepdbg, pdepdbg, prmdbunt, pvmdbg, olmdbg,&
   &arm2dbg, rdispunt, awmadwdbunt, grsmdbg, inoxunt(6),&
   &ttrmunt, ttrm2tmp(3),&
   &rlinedbunt, urbunt, urbunt1, urbunt2, blpunt,&
   &platfmdbunt, swdbgunt, rlinedbunt_ws,&
   &arcftdbg,&   ! Added for Aircraft Plume Rise; UNC-IE
   &hbpunt ! Added for HBPDEBUG

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

   logical :: bline, infld, mark, echo

   character :: path*2, ppath*2, keywrd*8, pkeywd*8, keywd*8, ktype*5,&
   &runst*1

   character (len=ilen_fld) :: field, inpfil, outfil, incfil
   character (len=istrg)    :: runst1

   integer ::  locb(ifmax), loce(ifmax), ifc, idc1, ipnum, ippnum
   dimension   field(ifmax), keywd(ikn), runst(istrg)


!***********************************************************************
!     This is The Global Variable Definition Block for Error Handling
!***********************************************************************

   logical :: fatal, istart, ifinis, recerr, errlst, eof, alloc_err
   logical :: L_SkipMessages

   real    :: store                ! Estimate of memory storage requirement

   character :: errmsg*50, errcod*3, versn*6
   character (len=6) :: c_metver         ! Character string for met version
   character (len=ilen_fld) :: msgfil

   dimension  errmsg(ierrn), errcod(ierrn)
   integer :: iline, iqline, ibline, ioline, inoxline, ierror, iftl,&
   &iwrn, info, iclm, imsg, nfatal, nwarn, ipage, ipgsum
! --- Met data array indices for use with MAXDCONT option
   integer :: ihr_ndx, iyr_ndx
   double precision :: explim

   integer :: icstat(50), isstat(50), irstat(50), imstat(50),&
   &iostat(50), iestat(50)
   integer :: incset, ixyset, ievset, ihlset, ifgset

! --- Include a variable to count number of header records in the surface file
   integer :: NumHeaders



!***********************************************************************
!     This is The Global Variable Definition Block for COntrol Pathway
!***********************************************************************

!CRT  D063  Add Downwash Platform Debug PLATFMDBG
!CRT  D113  Add Sidewash Debug SWDBG

   logical :: dfault, conc, depos, ddep, wdep, rural, urban, grdris,&
   &nostd, nobid, clmpro, msgpro, period, annual, month,&
   &flat, elev, flatsrcs, flgpol, run, events, rstsav,&
   &rstinp, daytab, mxfile, ppfile, plfile, anpost, anplot,&
   &statok, multyr, txfile, rkfile, seasonhr,&
   &mxdaily, mxdaily_byyr, l_maxdcont,&
   &ddplete, wdplete, drydplt, wetdplt, nodrydplt, nowetdplt,&
   &fstcmp, evonly, socont, detail, newmet, ardplete,&
   &pm25ave, no2ave, so2ave, l_no_pm25ave, l_no_no2ave,&
   &l_no_so2ave, nochkd, nowarn,&
   &debug, meteordbg, areadbg, primedbg, pvmrmdbg, olmdebug,&
   &arm2debug, grsmdebug, deposdbg, awmadwdbg, rlinedbg,&
   &platfmdbg,&
   &l_warnchkd, scim, scimhr,&
   &fastarea, fastall, L_NonDFAULT,&
   &screen, urbstab, prm_fstrec, romberg,&
   &pvmrm, psdcredit, olm, l_multurb,&
   &l_preset_urban, L_UrbanTransition, l_urban_all,&
   &L_Urban, L_Rural,&
   &l_awmadw,&
   &l_strmln_bldg, l_rect_bldg, L_AWMA_Ueff, L_AWMA_UTurb,&
   &l_awma_entrain, l_orddw, L_AWMA_UTurbHX,&
   &L_ORD_Ueff, L_ORD_Turb, L_ORD_Cav,&
   &arm2, beta, l_alpha, l_preinc, grsm,&
   &runttrm, ttrmdbg, urbdbug, blpdbug, swdbg,&
   &runttrm2, ttrm2dbg,& ! Added Nov. 2021; AECOM
   &arcftdebug, arcft, l_preset_arcft, l_arcft_all, L_Arcft,& !Added for Aircraft Plume Rise; UNC-IE
   &hbplume, hbpdbg, l_hbp_all,& ! Added for HBP & HBPDEBUG; Jan. 2023
   &l_areamndr !Added area meander flag to the alpha options Wood 6/3/22

! --- Logical variables used to track inconsistency between 'original'
!     results and results calculated during the MAXDCONT option
!     internal "post-processing" and between 'original' results and
!     results calculated during the EVENT post-processing
   logical :: L_EVENT_OrigConc_Warning, L_MAXDCONT_OrigConc_Warning

!CRT 3/23/2021, D061/D062 - Add logicals for SWmin and BigT low wind options
!CRT 4/11/2022, D131 - FRAN Alpha Formulation - add logical for PBal (L_PBal)
!    Wood 3/18/2022 D127 - added FRANMIN to LOW_WIND option
!CRT 8/9/2023, D176 - COARE Beta Check - add logical to indicate met is from AERMET/COARE (L_COARE)
   logical :: l_effsigy,   l_vectorws,&
   &L_AdjUstar,  l_bulkrn,&
   &L_MMIF_Data, L_MMIF_Profile,&
   &low_wind, L_UserSVmin, L_UserWSmin, L_UserFRANmax,&
   &L_UserSWmin, L_UserBigT,&
   &L_UserSZCoef,&
   &L_CCVR_Sub,  L_TEMP_Sub, L_TurbData,&
   &L_Got_SigA,  L_Got_SigW, L_PBal,&
   &L_UserFRANmin, l_coare

   character (len=ilen_fld) :: title1, title2
   character :: rundat*8, runtim*8
   character :: evparm*6, chrave*5, chidep*4, soelev*6, reelev*6,&
   &tgelev*6, outtyp*5, no2_field4*3, so2_field4*3,&
   &pm25_field4*3

!CRT  D063  Add Downwash Platform Debug PLATFMDBGFILE
!CRT  D113  Add Sidewash Debug SWFIL

   character (len=ilen_fld) :: savfil, savfl2, inifil, evfile,&
   &dbgfil, dbmfil, dbareafil, dbpvfil,&
   &rdispfil,&
   &dbolmfil, dbprmfil, DBAwmaDwFIL,&
   &dbarm2fil, dbgrsmfil, ozonfl(6),&
   &o3filunits, o3valunits, o3form(6),&
   &OzoneUnits, urbnam, noxvalunits,&
   &NOxUnits, noxfl(6), noxfilunits,&
   &noxform(6), ttrmfil, rlinedbgfil,&
   &urbfil,urbfil1,urbfil2,&
   &blpfil,&
   &platfmdbgfile,&
   &swfil, rlinedbgfil_ws,&   !Added RLINE gridded wind speed debug filename Wood 10/10/22
   &dbarcftfil,& ! Added for Aircraft Plume Rise; UNC-IE
   &hbpfil ! Added for HBPDEBUG; Jan. 2023

!    TTRMFIL is reserved for an unformatted POSTFILE for potential
!    post-processing using TTRM
!    end of TTRM insert
   double precision ::  o3conc, o3back(6), NO2Equil, NO2Stack,&
   &ev_o3conc(nhr), o3sect(6), O3_Max24hr(nhr,6),&
   &ARM2_Min, ARM2_Max,&
   &RatioARM2

! --- Logical to determine if minimum ozone restriction will be enforced.
!D074 1/7/2021
   logical ::  o3miss, L_O3Sector, L_O3Hourly, nomino3
   logical ::  L_O3File(6), l_o3val(6), l_o3values(6)
   logical ::  l_ao3miss(24)

!     CERC 11/30/20
   logical ::  noxmiss, L_NOXSector, L_NOxHourly
   logical ::  L_NOxFile(6), l_noxvalue(6), l_nox_vals(6)
   logical ::  l_anoxmiss(24)
   logical ::  L_CalcNoxFromNO2

   integer ::  NUMO3Sects, io3sect
   integer ::  nhival, nmxval, ndump, nhimxdly

   integer ::  NumNOxSects, inoxsect

   double precision :: noxbgconc, noxback(6), noxsect(6),&
   &ev_noxconc(nhr)

! --- Declare arrays for O3SECTs and ANOXSECTs by hour for EVENT processing
   integer ::  ao3sect(nhr), anoxsect(nhr)

   integer ::  nsrc, nrec, ngrp, nave, nval, ntyp, nmax,&
   &nsec, nqf, nbf, no3f, npdmax, nnet, ixm, iym,&
   &neve, numeve, ievent, narc, nolm, nurb, npsd, nblgrp,&
   &io3max(6), ibgmax(6), inoxmax(6), nnoxf,&
   &naft                ! Added for Aircraft Plume Rise; UNC-IE
!**  NAFT = Maximum Number of Airports (Here, it is fixed as 1 in ARISE)

!     NBLGRP  = Number of buoyant line groups defined by BLPGROUP records;
!                incremented in subr.SRCSIZ; used to allocate BL arrays
!                and write output summary
!                (see also NUMBLGRPS in module buoyant_line -
!                 incremented in subr.BLPGRP)

   integer ::  numcont     ! Number of contributing sources for PVMRM

   integer, allocatable :: kave(:)

   logical, allocatable :: eval(:)

   allocatable ::  chrave(:), chidep(:,:), outtyp(:), urbnam(:)

! --- Declare character strings to hold modeling options, including a
!     composite string that includes only options used in current run
   character (len=12)  :: modops(30)
   character (len=250) :: MODOPS_String


!***********************************************************************
!     This is The Global Variable Definition Block for SOurce Pathway
!***********************************************************************

!     CRT, 1/18/2012: D063 add SOPLAT for PLATFORM keyword
!     NOTE: A similar variable, OSPLAT, is used to indicate the a source
!     is subject to offshore platform downwash once platform params
!     are validated.
   character :: srcid*12, srctyp*8, sopcrd*1, sogas*1, urbsrc*1,&
   &grpid*8, emilbl*40, outlbl*40, pollut*8,&
   &qflag*8, bflag(6)*8, o3flag(6)*8, perlbl*40, olmid*8,&
   &urbid*8, psdid*8, NOxFLAG(6)*8,  soplat*1,&
   &aftsrc*1, aftid*8,& !Added for Aircraft Plume Rise; UNC-IE
   &hbpsrc*1  ! Added for HBP, JAN 2023

!**  AFTSRC = Used in SOurce Pathway to read the Aircraft Source IDs
!**  AFTID  = Used in COmmon Pathway for Airport ID (this is not used in
!**             calculation)

   character (len=ilen_fld) :: hrfile, BKGRND_File(6), bgform(6),&
   &BackUnits
!*#
   character :: prevsrcid*12
   character :: prevgrpid*8

! --- Declare logicals related to deposition options applicable to output types;
!     LDPART indicates that particle dry deposition is used
!     LWPART indicates that particle wet deposition is used
!     LDGAS  indicates that gaseous dry deposition is used
!     LWGAS  indicates that gaseous wet deposition is used
   logical :: ldpart, lwpart, ldgas, lwgas

! --- Declare logicals related to background concentration options:
!     L_BACKGRND indicates generally that background concentration options are used
!     L_BGHourly indicates generally that hourly background concentrations are used
!     L_BGSector indicates generally that sector-varying background concs are used
   logical :: l_backgrnd, L_BGHourly, L_BGSector
! --- Declare logicals related to sector-varying background concentration options:
!     L_BGFile indicates that hourly background concentrations are used for given sector
!     L_BGValues indicates that non-hourly background concs are available for given sector
   logical :: L_BGFile(6), L_BGValues(6)

   logical, allocatable :: grp_back(:)

   double precision :: bgconc, ev_bgconc(nhr)
   double precision :: bgback(6), bgsect(6)

   integer ::  NUMBGSects, ibgsect
   integer ::  NSubBGHOUR                      ! No. of BGHOUR subs
   integer ::  indx_grpall                     ! Index for SrcGroup ALL for ARM/ARM2 options

! --- Declare array for BGSECTs by hour for EVENT processing
   integer ::  abgsect(nhr)

   double precision, allocatable ::  axs(:), ays(:), azs(:), aqs(:),&
   &ahs(:), ats(:), avs(:), ads(:),&
   &asyini(:), aszini(:), ano2_ratio(:),&
   &adsfact(:)

!     CRT, 1/18/2012: D063 declare PLATFORM keyword parameters
!     PLATELV = Platform base elevation, PLATHB = Platform building height
!     PLATWB = Platform building width
   double precision, allocatable ::  platelv(:), plathb(:), platwb(:)

!MGS  7/29/2021 Added AC and BC as distance independent parameters used in CUBIC when
!MGS            computing plume rise adjustment
   double precision :: ac, bc

   double precision, allocatable :: aaqs(:,:,:), aahs(:,:,:),&
   &aats(:,:,:), aavs(:,:,:),&
   &aasyini(:,:,:), aaszini(:,:,:)

!**  Added for Aircraft Plume Rise; UNC-IE
!**---Aircraft Engine Parameters
   double precision, allocatable ::  amfuel(:), athrust(:), avaa(:),&
   &aafr(:), abypr(:),asrcangle(:),&
   &arpwr(:)          ! For Shaft-Based Engines
   double precision, allocatable ::  aamfuel(:,:,:), aathrust(:,:,:),&
   &aavaa(:,:,:), aaafr(:,:,:),&
   &aabypr(:,:,:),aasrcangle(:,:,:),&
   &aarpwr(:,:,:)     ! For Shaft-Based Engines
!**  End Aircraft Plume Rise insert; April 2023

   double precision, allocatable ::  adsbh(:,:), adsbw(:,:),&
   &adsbl(:,:), adsxadj(:,:), adsyadj(:,:)

!*--- Added for LINE source
   double precision, allocatable ::  awidth(:), axs1(:), ays1(:),&
   &axs2(:), ays2(:)

!*--- Added for AREA source meander !Wood 4/14/22
   logical :: l_aplume

!*--- Added for BUOYANT LINE source
!     AFP = average buoyancy parameter
   double precision, allocatable ::  afp(:), aafp(:,:,:)

   integer, allocatable :: inpd(:), ndxstk(:)

   double precision, allocatable ::  qfact(:,:),&
   &o3vary(:,:),&
   &backgrnd(:,:),&
   &noxvary(:,:)
   double precision :: emicon, haflif, decoef, vairms, zrdep, vdphor
   double precision, allocatable :: emifac(:), apdiam(:,:),&
   &aphi(:,:), apdens(:,:),&
   &avgrav(:,:), atstop(:,:)

!*--- Variables for hourly emissions
   double precision ::  hrqs, hrts, hrvs, hrhs, hrsy, hrsz, hrfp
!**  Added for Aircraft Plume Rise; UNC-IE
   double precision ::  hrmfuel, hrvaa, hrthrust, hrbypr, hrafr,&
   &hrsrcangle, hrrpwr
!**  End Aircraft Plume Rise insert; April 2023

   double precision, allocatable ::  ev_hrqs(:,:), ev_hrts(:,:),&
   &ev_hrvs(:,:), ev_hrhs(:,:),&
   &ev_hrsy(:,:), ev_hrsz(:,:),&
   &ev_hrfp(:,:),&
   &ev_hrmfuel(:,:),ev_hrvaa(:,:),&   ! Added for Aircraft Plume Rise; UNC-IE
   &ev_hrthrust(:,:),ev_hrbypr(:,:),& ! Added for Aircraft Plume Rise; UNC-IE
   &ev_hrafr(:,:),ev_hrsrcangle(:,:),&! Added for Aircraft Plume Rise; UNC-IE
   &ev_hrrpwr(:,:)                   ! Added for Aircraft Plume Rise; UNC-IE

!     AECOM 4/12/2022 D113 Added for SIDEWASH
   double precision :: conc_p, conc_sw, theta_p, theta_sw,&
   &sx1, sy1, swz
   double precision, allocatable ::  abw(:), abl(:), abh(:), aba(:),&
   &swxs(:), swys(:), swxr(:),&
   &swyr(:)
!     End SIDEWASH insert, April 2022

   integer ::  fullhrq
!*----
!*#
   character :: psdsrctyp*2
   integer, allocatable :: igroup(:,:), igrp_olm(:,:), igrp_psd(:,:),&
   &iurbgrp(:,:),&
   &igrp_blp(:,:)               ! Multiple_BuoyLines_D41_Wood

!    CRT, 1/18/2012: D063 add SOPLAT = SO PLATFORM keyword
   allocatable ::  srcid(:), srctyp(:), sopcrd(:), soplat(:),&
   &sogas(:), urbsrc(:),&
   &grpid(:), qflag(:), emilbl(:),&
   &outlbl(:),&
   &perlbl(:), olmid(:), psdid(:), urbid(:),&
   &aftsrc(:), aftid(:),&                              ! Added for Aircraft Plume Rise; UNC-IE
   &hbpsrc(:) ! Added for HBP, JAN 2023

   logical, allocatable :: l_olmgrp(:), l_psdgrp(:)
   logical, allocatable :: l_hrlysig(:), l_flatsrc(:),&
   &L_WakeMessage(:),&
   &l_ttrmsrctyp(:)

   allocatable :: psdsrctyp(:)
   double precision, allocatable :: ttrmout(:,:,:)
   character :: ttrmsrc*10
   allocatable :: ttrmsrc(:,:)


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
   integer, parameter :: nvpoly = 12

   integer :: ivert, nvert, nsegs,&
   &nvmax, nvmax2, npit, npnt, nvol, nline, nbline,&
   &narea, npoly, nvtemp, ncirc, nswp,&
   &nptemp
   double precision, allocatable :: uvert(:), vvert(:), vnvert(:),&
   &wvert(:), uasegs(:), ubsegs(:),&
   &xvert(:), yvert(:)
   double precision, allocatable :: spa(:,:)
   double precision, allocatable :: axinit(:), ayinit(:), aangle(:),&
   &axvert(:,:), ayvert(:,:),&
   &radius(:), axcntr(:), aycntr(:)
   integer, allocatable :: nverts(:)

   logical :: lseg


!***********************************************************************
!     This is The Global Variable Definition Block for the New OPENPIT
!     Source Algorithm - 7/19/94
!***********************************************************************


   double precision, parameter   :: alpha = 0.029d0
   double precision, allocatable :: aalpha(:), apdeff(:), avolum(:)
   double precision, allocatable :: efrac(:), qpart(:)
   double precision :: palpha, theta, pdeff, pdrel, pitfra, qeff
   double precision :: pitlen, pitwid, pitl, emihgt, xeff, yeff


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
   double precision, allocatable :: pdiff(:),pdiffw(:),rmolwt(:),&
   &alphas(:),react(:),henry(:),&
   &rcli(:),finemass(:), scf(:)

   logical, allocatable :: l_method2(:)

   integer :: iseas_gd(12), iland_gd(36), ncloud

   double precision :: rm, rcut, qsw, xlai, vdepg, uservd, zsubp,&
   &delta_z, fo, fseas2, fseas5, fracsat,&
   &liqcont, denom, xnu

   double precision :: Wold, Wnew, f2, EsTa

   character (len = 40) ::  refspe

   logical  :: luservd
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

   logical :: ista, iend, newid

   character :: netid*8, netidt*8, pnetid*8, ntid*8, nttyp*8,&
   &rectyp*2, pxsoid*12, pesoid*12, arcid*8

   double precision, allocatable ::  axr(:), ayr(:), azelev(:),&
   &azflag(:), azhill(:)
   integer, allocatable :: iref(:), ndxarc(:)
   allocatable ::  netid(:), rectyp(:), ntid(:),&
   &nttyp(:), arcid(:)
   integer ::      icount, jcount, ize, izh, izf, irze, irzh, irzf,&
   &irxr, iryr, irhz, innet
   double precision ::  xint, yint
   double precision, allocatable ::  xcoord(:,:), ycoord(:,:),&
   &xorig(:), yorig(:)
   integer, allocatable :: netsta(:), netend(:),&
   &numxpt(:), numypt(:)

!**  AZHILL Hill Height Associated with the Receptor
!**  HCRIT  Critical dividing streamline associated with the receptor


!***********************************************************************
!     This is The Global Variable Definition Block for MEteorology Pathway
!***********************************************************************

   character :: sfname*40, uaname*40, onname*40, alat*10, alon*10

   character (len=ilen_fld) :: metinp, scim_sfcfil, scim_profil,&
   &proinp
   character (len=ilen_fld) :: metfrm, profrm
   character :: MMIF_Version*29  ! CRT 1/22/2021 D077, increase from 27 to 29

   logical :: scimout

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
   logical :: turbopts(9)

!**** Logical flags for met data version, L_OldMetVer is used to flag
!     an outdated met version date in the surface file header record;
!     L_NAD_ADJ_Flags is used to flag cases where surface file header shows
!     current met version date, but surface file lacks additional fields
!     introduced with version 11059 for the wind data source/adjustment.
   logical :: L_OldMetVer, L_NAD_ADJ_Flags

!**** Include logical variable to track whether current year being
!     processed is a leap year or not, in order to properly handle
!     DAYRANGE inputs based on MN/DY for multiple-year met data
   logical :: L_LeapYear

   integer :: isdate, iedate, isyr, ismn, isdy, ishr, ieyr,&
   &iemn, iedy, iehr, iproc(366), iprocl(366),&
   &isyear, iuyear, ioyear,&
   &idsurf, iduair, idsite, isjday, iejday,&
   &ndays, incrst,&
   &istrt_cent, istrt_wind,&
!        RWB/MJ - allow for SCIM option - May, 1998.
   &nregstart, nregint, ifirsthr, isunit, ipunit,&
   &nskiptot, imetmsg

   integer :: full_yymmdd, iedate_yymmdd

   double precision :: ucat(5), rotang,&
   &virtpnt_urb(nkst), virtpnt_rur(nkst), vp_fact
   double precision :: sfx, sfy, uax, uay, onx, ony


!***********************************************************************
!     This is The Global Variable Definition Block for METEXT
!***********************************************************************

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     ADD NEW LOGICAL VARIABLES TO DENOTE THAT SIGMA-THETA AND SIGMA-W
!     WERE RESET
   logical :: clmhr, msghr, unstab, neutrl, stable,&
   &runerr, pflerr, endmon, methdr,&
   &hourly, L_DayOfWeekOpts,reset_sa,reset_sw

   logical, allocatable :: L_MorningTrans(:), AL_MorningTrans(:,:,:),&
   &aclmhr(:,:), amsghr(:,:),&
   &astable(:,:), aunstab(:,:),&
   &aurbstab(:,:)

   integer ::  kstmsg
   integer ::  ihour, iyear, imonth, iday, kurdat, jday, iseas,&
   &khour, kyear, kmonth, kday, kurpfl, ntothrs,&
   &iphour, ipyear, ipdate, ipcode, kst,&
   &iyr, iday_of_week, iday_of_week7, nplvls, ntglvl,&
   &iflag(mxplvl)
   integer ::  jday_prev
   integer ::  fulldate
   double precision ::  sfchf, ustar, wstar, vptgzi, ziconv, zimech,&
   &obulen, sfcz0, bowen, albedo, uref, wdref,&
   &urefht, ta, trefht, zi, afv,&
   &bvf, bvprim, xlat, tsign, zirur,&
   &prate, prec1, prec2, total_precip,&
   &uref10, rurustr, rurobulen, rh, sfcp
!  ******************************** added code Jan. 2023 --kja
   integer :: nhbp
!  ** variables for next hour convective and mechanical mixing heights
   double precision ::  ziconvn, zimechn
!  *******************************  added code end  --kja
! Added for HBP MAXDCONT and EVENT processing; Jan. 2023
   double precision, allocatable :: aziconvn(:,:), azimechn(:,:)
! End HBP addition

   double precision, allocatable :: urbpop(:), urbz0(:), ziurb(:),&
   &urbwstr(:), urbustr(:),&
   &urbobulen(:)

   integer, allocatable :: ikst(:,:), iapcode(:,:), nacloud(:,:)

   double precision, allocatable :: aprate(:,:), aqsw(:,:),arh(:,:),&
   &asfcp(:,:)
   double precision, allocatable :: asfchf(:,:), auref(:,:),&
   &aurefht(:,:), ata(:,:),&
   &atrefht(:,:), awdref(:,:),&
   &austar(:,:), awstar(:,:),&
   &aziconv(:,:), azimech(:,:),&
   &aobulen(:,:), avptgzi(:,:),&
   &asfcz0(:,:), abowen(:,:),&
   &aalbedo(:,:), awnew(:,:),&
   &awold(:,:), aesta(:,:),&
   &af2(:,:), aprec1(:,:),&
   &aprec2(:,:),&
   &ao3conc(:,:), abgconc(:,:),&
   &anoxbgconc(:,:),&
   &akst(:,:),ablta(:,:)

   integer :: istrhour, istrdy, istrmn,&
   &iendhour, ienddy, iendmn, numyrs, nremain, ndx4zi

   integer, allocatable :: andx4zi(:,:)

   double precision :: pflht(mxplvl), pflwd(mxplvl), pflws(mxplvl),&
   &pflta(mxplvl), pflsa(mxplvl), pflsw(mxplvl),&
   &pflsv(mxplvl), pfltg(mxplvl), pfltgz(mxplvl)

   double precision, allocatable :: apflht(:,:,:), apflwd(:,:,:),&
   &apflws(:,:,:), apflta(:,:,:),&
   &apflsa(:,:,:), apflsw(:,:,:),&
   &apflsv(:,:,:), apfltg(:,:,:),&
   &apfltgz(:,:,:)

   integer, allocatable :: aiflag(:,:,:)

   integer, allocatable :: anplvls(:,:), antglvl(:,:)

   double precision :: gridht(mxglvl), gridwd(mxglvl),&
   &gridws(mxglvl), gridsw(mxglvl),&
   &gridsv(mxglvl), gridtg(mxglvl),&
   &gridpt(mxglvl),&
!---  Add density profile for PRIME
   &gridrho(mxglvl),&
!---  Add tubulence dissipation rate (epsilon) profile for PVMRM/GRSM
   &grideps(mxglvl),&
!     Added  RL_GRIDWS array for RLINE windspeeds when used in CRITDS - WOOD 6-28-2022
   &rl_gridws(mxglvl,3)
   double precision :: grdswr(mxglvl), grdsvr(mxglvl),&
   &grdtgr(mxglvl),&
   &grdptr(mxglvl)

   double precision, allocatable :: grdswu(:,:), grdsvu(:,:),&
   &grdtgu(:,:), grdptu(:,:)

   double precision, allocatable ::&
   &agridht(:,:,:), agridwd(:,:,:),&
   &agridws(:,:,:), agridsw(:,:,:),&
   &agridsv(:,:,:), agridtg(:,:,:),&
   &agridpt(:,:,:),&
!---  Add density profile for PRIME
   &agridrho(:,:,:),&
!---  Add tubulence dissipation rate (epsilon) profile for PVMRM/GRSM
   &agrideps(:,:,:)
   double precision, allocatable :: agrdswr(:,:,:), agrdsvr(:,:,:),&
   &agrdtgr(:,:,:), agrdptr(:,:,:),&
   &auatzi(:,:),  asvatzi(:,:),&
   &aswatzi(:,:), aptatzi(:,:),&
   &auavg(:,:),  asvavg(:,:),&
   &aswavg(:,:), aptavg(:,:)

   double precision, allocatable :: agrdswu(:,:,:,:),&
   &agrdsvu(:,:,:,:),&
   &agrdtgu(:,:,:,:),&
   &agrdptu(:,:,:,:),&
   &aziurb(:,:,:),&
   &aurbwstr(:,:,:),&
   &aurbustr(:,:,:),&
   &aurbobulen(:,:,:),&
   &arurustr(:,:),&
   &arurobulen(:,:)

   double precision :: tg4pfl, tg4xtr,&
   &thstar, svavg, swavg, uavg,&
   &svatzi, swatzi, uatzi,&
   &ptatzi, uathe, svathe, swathe,&
   &uavh3, svavh3, swavh3, swrmax

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

   logical :: calcs, wake
   logical :: surfac

   double precision :: phid1, phid2, phin1, phin2

   integer :: irec,   isrc,   igrp,   iave,   ityp,  iset,&
   &numrec, numsrc, numgrp, numave, numarc, numtyp,&
   &numcap, numhor,&
! ---            Include counters for all source types
   &numpnt, numvol, numarea, numline, numpit,&
!                Add counter for SIDEWASH Point
   &numswp,&
   &numflat, ibkgrd(6), io3set(6), inoxset(6),&
   &icyear, nurbsrc, numurb, npd, ifvsec,&
   &iucat, iolm, numolm, ipsd, numpsd, iurb,&
   &ittrm,&
!! Added for TTRM2
   &cmeth,&
!! End of TTRM2 insert; Nov. 2021
!**  Added for Aircraft Plume Rise; UNC-IE
   &naftsrc
!**  NAFTSRC = Number of Aircraft Sources
!**  End Aircraft Plume Rise insert; April 2023

   double precision :: xs, ys, zs, qs, hs, ds, vs, ts, syinit,&
   &szinit, xinit, yinit, angle, xcntr, ycntr,&
   &dsfact, dsbh, dsbw,&
! --- PRIME Modification -------------------------------------------
   &dsbl, xadj, yadj, b_subs, b_subl, rscale,&
! ------------------------------------------------------------------
   &d, vd, e, wdrad, wdsin, wdcos, zbase,&
!**  Added for Aircraft Plume Rise; UNC-IE
!---- AIRCRAFT Plume Rise Modification -----------------------------
   &mfuel, thrust, vaa, afr, bypr, srcangle,&
   &rpwr
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
   double precision :: u_amb,lambda,mu_y1, mu_z1, mu_y2, mu_z2
   double precision :: swqs, swhs,& !SWBH, SWBW, SWBL, SWBA,
   &swtheta, bl, bw, bh, ba, swconc,&
   &bhs, bls, bws, hss  ! Scaled building dimensions and stack height
   logical :: runsw
!     End insert for SIDEWASH
   double precision, allocatable :: pdiam(:), phi(:), pdens(:),&
   &vgrav(:), tstop(:), schmidt(:),&
   &vdep(:), wqcor(:), dqcor(:),&
   &pscvrt(:), washout(:), ecoll(:),&
   &awdsin(:), awdcos(:),&
   &aafv(:)

!     CRT, 1/20/2012: D063 add OSPLAT logical to indicate if source is
!                     subject to offshore platform downwash
!                     A similar variable, SOPLAT, is used to indicate the
!                     the PLATFORM keyword was specified on the SO card
   logical, allocatable          :: osplat(:)

!     CRT, 1/20/2012: add platform sigma-y and sigma-z
   double precision :: platsy, platsz      ! stable or unstable injected
   double precision :: platsyd1, platszd1  ! unstable, direct, updraft
   double precision :: platsyd2, platszd2  ! unstable, direct, downdraft
   double precision :: platsyn1, platszn1  ! unstable, indirect, updraft
   double precision :: platsyn2, platszn2  ! unstable, indirect, downdraft
   double precision :: platsyp, platszp    ! unstable, penetrated
   double precision :: dhp3plat

   double precision, allocatable :: ttrminst(:), ttrmfrac(:),&
   &ttrmno2(:), ttrmsub(:),&
   &ttrmcompare(:,:,:,:),&
   &ttrmfrac_prm(:),&
   &ttrmfrac_aer(:)
!     variables associated with the incremental NO2 calculations using
!     the ozone response rate (TTRM) approach:
!     TTRMINSTACK = concentration of source-s[ecific in-stack converted NO2
!     TTRMFRAC    = cpncentration due to the conversion fraction of NO to NO2
!                  as a function of hourly ozone and transport time (TTRMTIME)
!     TTRMSUBC    = sub-total of in-stack and fractional NO2
!                  from either COHERENT, PRIME or PANCAKE contributions
!     end TTRM insert AECOM, Feb. 2021
   double precision :: wqcorg, gscvrt, dqcorg, washoutg, vsetl
   double precision :: xr, yr, x, y, zelev, zflag, zr, zeff, distr,&
   &zhill, hcrit, zrt, xdist
   double precision :: he, hsp, heflat, hter, hemwak, hedhh, zb, zm,&
   &hed1, hed2, hen1, hen2, he3, hpen, hed1m,&
   &hed2m, hen1m, hen2m, he3m, hsbl, qsubn, qsub3,&
   &xy, xz, sbid, fm, fb, dtdz, dhf, dhfaer, dhp,&
   &dhp1,dhp2, dhp3, delt, dhpb, dhpm, xf, xmax,&
   &xfm, xfb, xrad, wpb, dhcrit, hteff, center,&
   &z4gamma, xtr4gamma,&
!**  Added for Aircraft Plume Rise; UNC-IE
!**   HPM   = Horizontal Momentum Plume Rise (m)
!**   HDISP = Horizontal Momentum Displacement for Airborne Source (m)
!**   RP0   = Plume Radius (m)
   &hpm, hdisp, rp0
!** End Aircraft Plume Rise insert; April 2023
   double precision :: hesetl, he3setl, hv
   double precision :: us, svs, sws, tgs, tys, pts, up, wdir, da,&
   &zly, zlb, rinit, cb, cm, qtk, ppf, psubs, fhc,&
   &sy, syb, syn, sy3, sz, szupr, syamb, szamb,&
   &szas, szad1, szad2, szan1, szan2, syan, sza3,&
   &szb, szbd, szbn, sz3, szd1, szd2, szn1, szn2,&
   &szeff, szsurf, sya3, syb3, szb3, vsy3, vsigy,&
   &vsigz, vsyn, vszd1,vszd2, vszn1, vszn2, vsz3,&
   &szd1m, szd2m, szn1m, szn2m, sz3m, u3, sv3,&
   &sw3, tgp, svp, swp    ! SVP and SWP - Added for ARISE; UNC-IE

!  ************************************  added code Jan. 2023 --kja
!  ** penetrated plume factor below mixing height - Weil's Fq term
   double precision ::  ppfn, zin, ziavg, hhtop, htopdif, hhbot,ppwid
   double precision ::  sz3dbg
!  ***********************************  added code end  --kja

   double precision :: fsuby, fsubyd, fsubyn, fsuby3
   double precision :: fsubz, fsubzd, fsubzn, fsubz3,&
   &phee, fopt, cwrap, clift, xmdbg,&
   &cwrapc, cliftc, fsubyc, fsby3c
   double precision :: ueff, sveff, sweff, tgeff,&
   &ueffd, sveffd, sweffd,&
   &ueffn, sveffn, sweffn,&
   &ueff3, sveff3, sweff3, tgeff3,&
   &epseff, epseffd, epseff3,&
   &xmixed, xfinal, zmidmx,&
   &sigveff, sigveffd,&
   &syeff
   double precision :: skew, r, alphpd, betapd, asub1, asub2,&
   &bsub1, bsub2, lamda1, lamda2
   double precision :: chiw, chidw, chinw, chi3w,&
   &chil, chidl, chinl, chi3l
   double precision :: gamfact
!     CERC 11/30/20:
   double precision :: chi_ttravplm, chi_ttravpan,&
   &chi_ttravaer, chi_ttravprm
   double precision, allocatable :: chi_ttravchm(:,:), ttravchm(:),&
   &bldfac(:,:)
   double precision :: uchm, PRMVAL_Src1
!     added for TTRM approach
   double precision :: ttrmtime, ttrmtime_prm, k1, gamf
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

   double precision :: qsum,  sum_no2rat
   double precision :: qsum3, sum3_no2rat

   double precision :: cwmax, cwmin, cwmax3, cwmin3,&
   &dwmax, dwmin, dwmax3, dwmin3
   double precision :: hmnh,  hmxh,  hmnt,   hmxt,&
   &hmnh3, hmxh3, hmnt3,  hmxt3

!***********************************************************************
!     This is The Global Variable Definition Block for EVent Pathway
!***********************************************************************

   character :: evname*10, evgrp*8
   integer, allocatable ::  evaper(:), evdate(:), evjday(:),&
   &idxev(:)

   allocatable ::  evname(:), evgrp(:)




!***********************************************************************
!     This is The Global Variable Definition Block for OUtput Pathway
!***********************************************************************

   logical :: outpart, summfile, L_NoHeader(8), evalfil, toxxfil

   character (len=ilen_fld) :: thrfil, pstfil, pltfil, annpst,&
   &annplt, thrfrm, pstfrm, pltfrm,&
   &toxfil, seahrs, rnkfil, rnkfrm,&
   &evlfil, sumfil, mxdfrm,&
   &maxdly, maxdly_byyr, maxdcont_file

! --- Variable for specifying format for file outputs (default = 'FIX')
   character (len = 3) :: file_format

   integer, allocatable :: nhiave(:,:), maxave(:), imxval(:),&
   &idytab(:), maxfle(:,:), ipstfl(:,:),&
   &ipltfl(:,:,:), ianpst(:), ianplt(:),&
   &inhi(:), itoxfl(:), iseahr(:),&
   &imxdly(:), imxdly_byyr(:), maxdcont(:),&
   &irnkfl(:), irkval(:)

   double precision, allocatable :: thresh(:,:), toxthr(:),&
   &maxd_thresh(:)

   double precision, allocatable :: axr_sav(:), ayr_sav(:),&
   &azelev_sav(:), azflag_sav(:),&
   &azhill_sav(:)

   integer, allocatable :: imxunt(:,:), ipsunt(:,:), ipsfrm(:,:),&
   &iplunt(:,:,:), iapunt(:),&
   &ianfrm(:), ippunt(:), itxunt(:),&
   &irkunt(:), ielunt(:), iupart(:),&
   &ishunt(:), imdunt(:), imdunt_byyr(:),&
   &imxdcunt(:), mxd_rank(:,:)

   allocatable ::  thrfil(:,:), pstfil(:,:), pltfil(:,:,:),&
   &annpst(:), annplt(:), toxfil(:), seahrs(:),&
   &rnkfil(:), evlfil(:), maxdly(:),&
   &maxdly_byyr(:), maxdcont_file(:)

   integer, allocatable :: idconc(:,:)

   integer :: itab, nxtox, nytox, nhours, ipair

   double precision, allocatable :: txconc(:,:)



!***********************************************************************
!     This is The Global Variable Definition Block for Working Space
!***********************************************************************

   character :: workid*12, dummy*12

   integer :: imit, inum, idum, indave, indgrp, indval,&
   &isc, ioerrn, ncpp, nrpp, ngpp, nppx, nppy
   real :: fnum
   double precision :: dnum

   allocatable ::          workid(:)
   integer, allocatable :: iwrk2(:,:)

!     Declare Temporary Work Arrays for ZELEV and ZFLAG Receptor Data
   double precision, allocatable :: zetmp1(:), zetmp2(:)
   double precision, allocatable :: zftmp1(:), zftmp2(:)
   double precision, allocatable :: zhtmp1(:), zhtmp2(:)


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


   character :: hclmsg, mclmsg, hmclm

   double precision, allocatable ::  hrval(:), aveval(:,:,:,:),&
   &aerval(:), prmval(:)
   double precision, allocatable ::  hivalu(:,:,:,:,:),&
   &hmax(:,:,:,:)
   integer, allocatable ::  hmloc(:,:,:,:),&
   &hmdate(:,:,:,:),&
   &nhidat(:,:,:,:,:),&
   &nhidatmxd(:,:,:),&
   &nhidatmxd_byyr(:,:,:,:)

   double precision, allocatable ::  annval(:,:,:), amxval(:,:,:),&
   &shvals(:,:,:,:,:), mxdval(:,:),&
   &himxdly(:,:,:),&
   &himxdly_byyr(:,:,:,:)
   integer, allocatable ::  imxloc(:,:,:), imxdhr(:,:)
   integer              ::  ianhrs, ianclm, ianmsg,&
   &nseahr(4,24), nseacm(4,24)
   double precision, allocatable ::  rmxval(:,:,:,:)
   integer, allocatable ::  mxdate(:,:,:,:),&
   &mxloca(:,:,:,:)
   integer, allocatable ::  numhrs(:), numclm(:), nummsg(:)
   allocatable ::           hclmsg(:,:,:,:,:),&
   &mclmsg(:,:,:,:),&
   &hmclm(:,:,:,:)

   double precision, allocatable ::  sumann(:,:,:)
   double precision, allocatable ::  sumhnh(:,:,:), mxpmval(:,:,:)
   double precision, allocatable ::  sumval_maxd(:,:,:,:)

   integer, allocatable ::  mxpmloc(:,:,:)

   double precision, allocatable ::  chi(:,:,:), hecntr(:,:),&
   &hecntr3(:,:), ppfact(:,:),&
   &ueffs(:,:), ueff3s(:,:),&
   &epsef(:,:), epsef3(:,:),&
   &fopts(:,:),&
   &abval(:,:), bcval(:,:)

   double precision, allocatable ::  arcmax(:), qmax(:), dxmax(:),&
   &umax(:),&
   &svmax(:), swmax(:), symax(:), sy3mx(:),&
   &u3max(:), hemax(:), arccl(:), szmax(:),&
   &chidmw(:), chinmw(:), chi3mw(:),&
   &chidml(:), chinml(:), chi3ml(:),&
   &hsblmx(:)

   logical, allocatable :: chimask(:,:,:)

!***********************************************************************
!     This is The Global Variable Definition Block For The
!     EVENT Model Result Arrays
!***********************************************************************

   double precision, allocatable ::  ev_aveval(:), hrvals(:,:),&
   &grpval(:,:), backhr(:,:),&
   &grpave(:), backave(:),&
   &backann(:), backseashr(:,:,:)

! --- Declare allocatable array for Original Event Concentrations;
!     to be compared to GRPAVE calculated value for QA purpose
   double precision, allocatable :: EV_OrigConc(:)

   integer ::  ev_numhrs, ev_numclm, ev_nummsg, istahr, iendhr


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
   data versn /'24142'/   ! May 21, 2024

!     Initialize C_METVER to blanks in case there is an error opening
!     the surface met file or with reading the version date, otherwise
!     C_METVER will be undefined when writing the page headers.
   data c_metver /'      '/

! --- Initialize array of hourly O3 values to 40 ppb (78.4 ug/m3)
   data O3_Max24hr /144*78.4d0/   ! 144 = 24hr/day*6sectors

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
   data inunit/ 7/, iounit/ 8/, pvmdbg/ 9/, olmdbg/ 9/, arm2dbg/ 9/,&
   &grsmdbg/ 9/, ierunt/10/, ierwrt/11/, idpunt/12/,&
   &idpun2/14/, irsunt/15/, ihremi/16/, ievunt/17/, itevut/18/,&
   &mfunit/19/, incunt/20/,&
   &mpunit/21/, isunit/22/, ipunit/23/, dbgunt/24/, dbmunt/25/,&
   &areadbunt/26/, prmdbunt/27/, isumunt/28/, gdepdbg/29/,&
   &pdepdbg/30/, awmadwdbunt/931/, rlinedbunt/932/,&
   &platfmdbunt/933/, rdispunt/ 3/,&                       ! RDISPUNT is for RELDISP debug file for PVMRM
   &ttrmunt/9937/, urbunt/937/, urbunt1/938/,blpunt/939/,&
   &ttrm2tmp/9938, 9939, 9940/,urbunt2/941/,&
   &swdbgunt/8837/,& ! Added for sidewash
   &rlinedbunt_ws/8932/,& ! Added for the gridded wind speed profile in RLINE Wood 10/10/22
   &arcftdbg/32/,&        ! Added for Aircraft Plume Rise; UNC-IE
   &hbpunt/731/ ! Added for HBPDEBUG; Jan. 2023

!*#

! --- Initialize logical variables for tracking use of turbulence data
   data L_Got_SigA/.false./, L_Got_SigW/.false./

   data inpfil/' '/, outfil/' '/


!***********************************************************************
!     Initialize Keyword Array
!     CRT, 1/18/2012: D063 Add PLATFORM for platform downwash parameters
!                     on the SO pathway.
!***********************************************************************

   integer, private :: i
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
   data (keywd(i),i=1,ikn) /&
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

   data iproc /366*1/, iprocl/366*1/, explim /-50.0d0/
   data ucat /1.54d0, 3.09d0, 5.14d0, 8.23d0, 10.8d0/
   data modops /30*'         '/

!***********************************************************************
!     Initialize distance factors used in determining when to switch
!     to point source approximation for area sources under the FASTAREA
!     option (formerly the TOXICS option).
!***********************************************************************

!     STAB. CLASS         A      B      C       D       E       F
!                        ***    ***    ***     ***     ***     ***
   data virtpnt_urb /3.5d0, 3.5d0, 5.5d0, 10.5d0, 15.5d0, 15.5d0/,&
   &virtpnt_rur /3.5d0, 5.5d0, 7.5d0, 12.5d0, 15.5d0, 25.5d0/


!***********************************************************************
!     Initialize Setup Status Arrays
!***********************************************************************

!     JAT 1/29/21 D070

   data icstat/50*0/, isstat/50*0/, irstat/50*0/, imstat/50*0/,&
   &iostat/50*0/, iestat/50*0/
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

   data gridht /&
   &0.0d0, 0.5d0, 1.0d0, 2.0d0, 4.0d0,  8.0d0, 14.0d0, 20.0d0,&
   &30.0d0,  40.0d0,  50.0d0,  60.0d0,  70.0d0,  80.0d0,  90.0d0,&
   &100.0d0, 120.0d0, 140.0d0, 160.0d0, 180.0d0, 200.0d0, 250.0d0,&
   &300.0d0, 350.0d0, 400.0d0, 450.0d0, 500.0d0, 550.0d0, 600.0d0,&
   &650.0d0, 700.0d0, 750.0d0, 800.0d0, 850.0d0, 900.0d0, 950.0d0,&
   &1000.0d0, 1050.0d0, 1100.0d0, 1150.0d0, 1200.0d0, 1250.0d0,&
   &1300.0d0, 1350.0d0, 1400.0d0, 1450.0d0, 1500.0d0, 1550.0d0,&
   &1600.0d0, 1650.0d0, 1700.0d0, 1750.0d0, 1800.0d0, 1850.0d0,&
   &1900.0d0, 1950.0d0, 2000.0d0, 2100.0d0, 2200.0d0, 2300.0d0,&
   &2400.0d0, 2500.0d0, 2600.0d0, 2700.0d0, 2800.0d0, 2900.0d0,&
   &3000.0d0, 3100.0d0, 3200.0d0, 3300.0d0, 3400.0d0, 3500.0d0,&
   &3600.0d0, 3700.0d0, 3800.0d0, 3900.0d0, 4000.0d0, 4100.0d0,&
   &4200.0d0, 4300.0d0, 4400.0d0, 4500.0d0, 4600.0d0, 4700.0d0,&
   &4800.0d0, 4900.0d0, 5000.0d0/


!CRT  D100, CRT, 8/6/2021
!CRT  Add CONTAINS statement to module and move error code and message
!CRT  arrays into separate subroutine (ERRWRNMSG()) where hardcoded
!CRT  array indices can be written dynamically with a incremented
!CRT  counter variable

contains


!CRT  D100, CRT, 8/6/2021: Subroutine ERRWRNMSG()
!CRT  Populate error code and message arrays for error and warning
!CRT  messages.  Replace hardcoded array indices with incremental
!CRT  counter.  Move code and message statements to keep codes in order.

   subroutine errwrnmsg()

      implicit none

      integer :: ecd

!***********************************************************************
!     Initialize Error Code and Message Arrays
!***********************************************************************
      ecd = 0

!-----------------------------------------------------------------------
!---- 100s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ecd = ecd+1
      errcod(ecd)='100'
      errmsg(ecd)='Invalid Pathway Specified. The Troubled Pathway is'

      ecd = ecd+1
      errcod(ecd)='105'
      errmsg(ecd)='Invalid Keyword Specified. The Troubled Keyword is'

      ecd = ecd+1
      errcod(ecd)='109'
      errmsg(ecd)='Too many fields specified on runstream image; MAX='

      ecd = ecd+1
      errcod(ecd)='110'
      errmsg(ecd)='Keyword is Not Valid for This Pathway.  Keyword is'

      ecd = ecd+1
      errcod(ecd)='111'
      errmsg(ecd)='User-specified minimum Sigma-V on LOW_WIND Keyword'

      ecd = ecd+1
      errcod(ecd)='112'
      errmsg(ecd)='User-specified minimum WindSpeed on LOW_WIND Keywd'

      ecd = ecd+1
      errcod(ecd)='113'
      errmsg(ecd)='User-specified maximum FRAN on the LOW_WIND Keywrd'

!CRT  4/11/2022 - This message code and message is not used?
      ecd = ecd+1
      errcod(ecd)='114'
      errmsg(ecd)='User-specified SZCOEF value on LOW_WIND Keyword   '

      ecd = ecd+1
      errcod(ecd)='115'
      errmsg(ecd)='STARTING or FINISHED Out of Sequence:  Pathway =  '

      ecd = ecd+1
      errcod(ecd)='116'
      errmsg(ecd)='Vector Wind Speeds specified on MODELOPT Keyword  '

!     Wood 3/18/22 added FRANMIN to LOW_WIND option
      ecd = ecd+1
      errcod(ecd)='117'
      errmsg(ecd)='User-specified minimum FRAN on the LOW_WIND Keywrd'

      ecd = ecd+1
      errcod(ecd)='119'
      errmsg(ecd)='Missing FINISHED-Runstream File Incomplete: ISTAT='

      ecd = ecd+1
      errcod(ecd)='120'
      errmsg(ecd)='Pathway is Out of Sequence:  Pathway =            '

! --- New messages for AWMADWNW and ORD_DWNW keywords on CO pathway
      ecd = ecd+1
      errcod(ecd)='121'
      errmsg(ecd)='Duplicate option specified for Keyword            '

      ecd = ecd+1
      errcod(ecd)='122'
      errmsg(ecd)='AWMADWNW Option requires ALPHA option on MODELOPT '

      ecd = ecd+1
      errcod(ecd)='123'
      errmsg(ecd)='ORD_DWNW Option requires ALPHA option on MODELOPT '

      ecd = ecd+1
      errcod(ecd)='124'
      errmsg(ecd)='UEFF conflict: AWMADWNW and ORD_DWNW keywords'

!JAT  6/10/2020:  ISSUE D53.  ADD FROM 19191
!     ADD ERROR MESSAGE FOR INCOMPLETE PATHS,
!     I.E. FINISHED KEYWORD NOT FOUND.  125 HAS ALWAYS BEEN ASSOCIATED
!     WITH MESSAGE BUT THERE NEVER HAS BEEN A MESSAGE.
!JAT  10/28/2020:  UPDATE FROM ISSUE D53 TO BE IN LINE WITH NEW
!     MESSAGES FROM D32.  CHANGE FROM 305 TO 314
      ecd = ecd+1
      errcod(ecd)='125'
      errmsg(ecd)='ONE OR MORE PATHS HAS FINISHED KEYWORD MISSING'

!JAT  6/10/2020:  ISSUE D53, ASSIGN NEW ERROR CODE NUMBER, 126 TO THE
!CRT  2/2/2021:  Update message to include AWMAUTURBHX option
!     AWMADW STREAMLINE ERROR MESSAGE.  125 IS ACTUALLY MEANT FOR INCOMPLETE
!     PATHS, I.E. FINISHED KEYWORD NOT FOUND
!      ERRCOD(ECD)='125'
      ecd = ecd+1
      errcod(ecd)='126'
      errmsg(ecd)='STREAMLINE requires AWMAUTURB or AWMAUTURBHX'

!CRT 9/11/2020, D062 User Minimum Sigma W
!CRT 02/16/2021: D062 User Minimum Sigma W - Update array index for to
!CRT avoid conflict with merging code
      ecd = ecd+1
      errcod(ecd)='127'
      errmsg(ecd)='User-specified minimum Sigma-W on LOW_WIND Keyword'

!CRT 4/11/2022: D131 FRAN Alpha Formulation - Momentum Balance
      ecd = ecd+1
      errcod(ecd)='128'
      errmsg(ecd)='User-specified PBAL Option on LOW_WIND Keyword'

!RCO 9/28/2020, D061 User BIGT
!CRT 02/16/2021: D061 User BIGT - Update array index for to
!CRT avoid conflict with merging code
      ecd = ecd+1
      errcod(ecd)='129'
      errmsg(ecd)='User-specified BIGT on LOW_WIND Keyword'

      ecd = ecd+1
      errcod(ecd)='130'
      errmsg(ecd)='Missing Mandatory Keyword.  The Missing Keyword is'

      ecd = ecd+1
      errcod(ecd)='133'
      errmsg(ecd)='LOW_WIND Option requires ALPHA option on MODELOPT '

!     Wood 6/3/22 D128 added AREAMNDR as an ALPHA option to CO pathway
      ecd = ecd+1
      errcod(ecd)='134'
      errmsg(ecd)='AREAMNDR ALPHA option selected on MODELOPT Keyword'

      ecd = ecd+1
      errcod(ecd)='135'
      errmsg(ecd)='Nonrepeatable Keyword or Recursed INCLUDED: Keywrd'

      ecd = ecd+1
      errcod(ecd)='136'
      errmsg(ecd)='LOW_WIND ALPHA option selected on MODELOPT Keyword'

      ecd = ecd+1
      errcod(ecd)='137'
      errmsg(ecd)='BETA option not allowed with DFAULT on MODELOPT '

      ecd = ecd+1
      errcod(ecd)='138'
      errmsg(ecd)='ALPHA option not allowed with DFAULT on MODELOPT '

!     Wood 6/3/22 D128 added AREAMNDR as an ALPHA option to CO pathway
      ecd = ecd+1
      errcod(ecd)='139'
      errmsg(ecd)='AREAMNDR Option requires ALPHA option on MODELOPT '

      ecd = ecd+1
      errcod(ecd)='140'
      errmsg(ecd)='Invalid Order of Keyword. The Troubled Keyword is '

      ecd = ecd+1
      errcod(ecd)='141'
      errmsg(ecd)='Conflicting Options for NO2 conversion specified: '

      ecd = ecd+1
      errcod(ecd)='142'
      errmsg(ecd)='Following Keyword Invalid Without PVMRM or OLM:   '

      ecd = ecd+1
      errcod(ecd)='143'
      errmsg(ecd)='Following Keyword Invalid Without PVMRM Option:   '

      ecd = ecd+1
      errcod(ecd)='144'
      errmsg(ecd)='Following Keyword Invalid Without OLM Option:     '

      ecd = ecd+1
      errcod(ecd)='145'
      errmsg(ecd)='Following Keyword Invalid Without ARM or ARM2:    '

      ecd = ecd+1
      errcod(ecd)='146'
      errmsg(ecd)='PSDGROUP Keyword Specified without PSDCREDIT Opt. '

      ecd = ecd+1
      errcod(ecd)='147'
      errmsg(ecd)='Following Option is Invalid with PSDCREDIT Option:'

      ecd = ecd+1
      errcod(ecd)='148'
      errmsg(ecd)='Both OZONEVAL and O3VALUES keywords are specified '

      ecd = ecd+1
      errcod(ecd)='149'
      errmsg(ecd)='Conflicting options specified on MODELOPT keyword:'

      ecd = ecd+1
      errcod(ecd)='150'
      errmsg(ecd)='Conflicting Options: MULTYEAR Option with         '

      ecd = ecd+1
      errcod(ecd)='151'
      errmsg(ecd)='Non-DFAULT NoUrbTran option selected on MODELOPT  '

      ecd = ecd+1
      errcod(ecd)='152'
      errmsg(ecd)='ELEVUNIT card must be first for this Pathway:     '

      ecd = ecd+1
      errcod(ecd)='153'
      errmsg(ecd)='Conflicting Opts: MAXDCONT with Re-Start or MULTYR'

      ecd = ecd+1
      errcod(ecd)='154'
      errmsg(ecd)='Conflicting options:  SCIM cannot be used with    '

      ecd = ecd+1
      errcod(ecd)='155'
      errmsg(ecd)='Conflicting Decay Keyword. Inputs Ignored for     '

      ecd = ecd+1
      errcod(ecd)='156'
      errmsg(ecd)='Option ignored - not valid with SCIM.  Option =   '

      ecd = ecd+1
      errcod(ecd)='157'
      errmsg(ecd)='Wet SCIM Not Supported - Wet SCIM Inputs Ignored  '

      ecd = ecd+1
      errcod(ecd)='158'
      errmsg(ecd)='EMISUNIT Keyword Used With More Than 1 Output Type'

      ecd = ecd+1
      errcod(ecd)='159'
      errmsg(ecd)='EMISUNIT Keyword Used With the Following Keyword: '

      ecd = ecd+1
      errcod(ecd)='160'
      errmsg(ecd)='Duplicate ORIG Secondary Keyword for GRIDPOLR:    '

      ecd = ecd+1
      errcod(ecd)='161'
      errmsg(ecd)='MAXDCONT option already defined for source group: '

      ecd = ecd+1
      errcod(ecd)='162'
      errmsg(ecd)='Option only applies to 1-hr NO2 or 1-hr SO2 NAAQS:'

      ecd = ecd+1
      errcod(ecd)='163'
      errmsg(ecd)='Option only applies to 24h PM25, 1h NO2 or 1h SO2:'

      ecd = ecd+1
      errcod(ecd)='164'
      errmsg(ecd)='NOHEADER selected for non-specified output option:'

      ecd = ecd+1
      errcod(ecd)='165'
      errmsg(ecd)='Inconsistent temporally-varying BACKGRND options: '

      ecd = ecd+1
      errcod(ecd)='166'
      errmsg(ecd)='SECTOR option invalid w/o BG/O3/NOx Inputs:       '

      ecd = ecd+1
      errcod(ecd)='167'
      errmsg(ecd)='Inconsistent temporally-varying O3VALUES options: '

      ecd = ecd+1
      errcod(ecd)='168'
      errmsg(ecd)='Hourly BACKGRND already specified for this sector:'

      ecd = ecd+1
      errcod(ecd)='170'
      errmsg(ecd)='Invalid Secondary Keyword for Receptor Grid:      '

      ecd = ecd+1
      errcod(ecd)='171'
      errmsg(ecd)='Sector ID specified without Sector-varying Option:'

      ecd = ecd+1
      errcod(ecd)='175'
      errmsg(ecd)='Missing Secondary Keyword END for Receptor Grid:  '

      ecd = ecd+1
      errcod(ecd)='180'
      errmsg(ecd)='Conflicting Secondary Keyword for Receptor Grid:  '

      ecd = ecd+1
      errcod(ecd)='181'
      errmsg(ecd)='BULKRN Delta-T & SolarRad option for SBL was used '

      ecd = ecd+1
      errcod(ecd)='182'
      errmsg(ecd)='MMIF-generated meteorological inputs were used    '

      ecd = ecd+1
      errcod(ecd)='183'
      errmsg(ecd)='Non-DFAULT option for MMIF-generated data without '

      ecd = ecd+1
      errcod(ecd)='184'
      errmsg(ecd)='PROFFILE heights > 999m; inputs could be from MMIF'

      ecd = ecd+1
      errcod(ecd)='185'
      errmsg(ecd)='Either No Sources or No Receptors are specified!!!'

      ecd = ecd+1
      errcod(ecd)='186'
      errmsg(ecd)='THRESH_1MIN 1-min ASOS wind speed threshold used  '

      ecd = ecd+1
      errcod(ecd)='187'
      errmsg(ecd)='ADJ_U* Option for Stable Low Winds used in AERMET '

      ecd = ecd+1
      errcod(ecd)='188'
      errmsg(ecd)='Non-DFAULT FLAT required for RLINE/RLINEXT source '

      ecd = ecd+1
      errcod(ecd)='189'
      errmsg(ecd)='No Keywords for OU Path and No PERIOD/ANNUAL Aves.'

      ecd = ecd+1
      errcod(ecd)='190'
      errmsg(ecd)='Incompatible Option Used With SAVEFILE or INITFILE'

      ecd = ecd+1
      errcod(ecd)='191'
      errmsg(ecd)='PM25, 1h NO2 or SO2 w/o MAXIFILE incompatible with'

      ecd = ecd+1
      errcod(ecd)='192'
      errmsg(ecd)='FASTALL option also implies use of FASTAREA option'

      ecd = ecd+1
      errcod(ecd)='193'
      errmsg(ecd)='Units keyword specified without appropriate option'

      ecd = ecd+1
      errcod(ecd)='194'
      errmsg(ecd)='DEBUGOPT input option is invalid or not applicable'

      ecd = ecd+1
      errcod(ecd)='195'
      errmsg(ecd)='Incompatible Keyword used with GASDEPVD option    '

      ecd = ecd+1
      errcod(ecd)='196'
      errmsg(ecd)='Gas deposition algorithms are non-DFAULT options  '

      ecd = ecd+1
      errcod(ecd)='197'
      errmsg(ecd)='METHOD_2 for particulates is a non-DFAULT option  '

      ecd = ecd+1
      errcod(ecd)='198'
      errmsg(ecd)='Non-DFAULT ALPHA Option Required for use of       '

      ecd = ecd+1
      errcod(ecd)='199'
      errmsg(ecd)='Non-DFAULT BETA Option Required for use of        '

!-----------------------------------------------------------------------
!---- 200s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ecd = ecd+1
      errcod(ecd)='200'
      errmsg(ecd)='Missing Parameter(s). No Options Specified For    '

      ecd = ecd+1
      errcod(ecd)='201'
      errmsg(ecd)='Not Enough Parameters Specified For the Keyword of'

      ecd = ecd+1
      errcod(ecd)='202'
      errmsg(ecd)='Too Many Parameters Specified For the Keyword of  '

      ecd = ecd+1
      errcod(ecd)='203'
      errmsg(ecd)='Invalid Parameter Specified.  Troubled Parameter: '

      ecd = ecd+1
      errcod(ecd)='204'
      errmsg(ecd)='Regulatory DFAULT Conflicts with Non-DFAULT Option'

      ecd = ecd+1
      errcod(ecd)='205'
      errmsg(ecd)='No Option Parameter Setting.  Forced by Default to'

      ecd = ecd+1
      errcod(ecd)='206'
      errmsg(ecd)='Regulatory DFAULT Overrides Non-DFAULT Option For '

      ecd = ecd+1
      errcod(ecd)='207'
      errmsg(ecd)='No Parameters Specified. Default Values Will Used.'

      ecd = ecd+1
      errcod(ecd)='208'
      errmsg(ecd)='Illegal Numerical Field Encountered in            '

      ecd = ecd+1
      errcod(ecd)='209'
      errmsg(ecd)='Negative Value Appears For Non-negative Variable. '

      ecd = ecd+1
      errcod(ecd)='210'
      errmsg(ecd)='Num Ranked values on RANKFILE > MAXTABLE value for'

      ecd = ecd+1
      errcod(ecd)='211'
      errmsg(ecd)='Duplicate Averaging Period Specified for Keyword  '

      ecd = ecd+1
      errcod(ecd)='212'
      errmsg(ecd)='END Encountered Without (X,Y) Points Properly Set '

      ecd = ecd+1
      errcod(ecd)='213'
      errmsg(ecd)='ELEV Input Inconsistent With Option: Input Ignored'

      ecd = ecd+1
      errcod(ecd)='214'
      errmsg(ecd)='ELEV Input Inconsistent With Option: Defaults Used'

      ecd = ecd+1
      errcod(ecd)='215'
      errmsg(ecd)='FLAG Input Inconsistent With Option: Input Ignored'

      ecd = ecd+1
      errcod(ecd)='216'
      errmsg(ecd)='FLAG Input Inconsistent With Option: Defaults Used'

      ecd = ecd+1
      errcod(ecd)='217'
      errmsg(ecd)='More Than One Delimiter In A Field for Keyword    '

      ecd = ecd+1
      errcod(ecd)='218'
      errmsg(ecd)='Number of (X,Y) Points Does Not Match Number of   '

      ecd = ecd+1
      errcod(ecd)='219'
      errmsg(ecd)='Urban ID field is too long (>8); first 12 char:   '

      ecd = ecd+1
      errcod(ecd)='220'
      errmsg(ecd)='Missing Origin (Use Default = 0,0) In GRIDPOLR    '

      ecd = ecd+1
      errcod(ecd)='221'
      errmsg(ecd)='Missing Dist or Direction Setting In Polar Network'

      ecd = ecd+1
      errcod(ecd)='222'
      errmsg(ecd)='SECTOR Value is out of order:                     '

      ecd = ecd+1
      errcod(ecd)='223'
      errmsg(ecd)='Missing Distance or Degree Field in               '

! --- New messages '224' '225' and '226' added for undefined SrcID on
!     the SRCGROUP, OLMGROUP, or PSDGROUP keywords
      ecd = ecd+1
      errcod(ecd)='224'
      errmsg(ecd)='SrcID specified on SRCGROUP keyword not defined:  '

      ecd = ecd+1
      errcod(ecd)='225'
      errmsg(ecd)='SrcID specified on OLMGROUP keyword not defined:  '

      ecd = ecd+1
      errcod(ecd)='226'
      errmsg(ecd)='SrcID specified on PSDGROUP keyword not defined:  '

      ecd = ecd+1
      errcod(ecd)='227'
      errmsg(ecd)='SECTOR Width is out of range:                     '

      ecd = ecd+1
      errcod(ecd)='228'
      errmsg(ecd)='Default(s) Used for Missing Parameters on Keyword '

      ecd = ecd+1
      errcod(ecd)='229'
      errmsg(ecd)='Too Many Parameters - Inputs Ignored on Keyword   '

      ecd = ecd+1
      errcod(ecd)='230'
      errmsg(ecd)='Source ID field is too long (>12); first 12 chars:'

      ecd = ecd+1
      errcod(ecd)='231'
      errmsg(ecd)='Too Many Numerical Values Specified for           '

      ecd = ecd+1
      errcod(ecd)='232'
      errmsg(ecd)='OLMGroup ID field is too long (>8); first 12 char:'

      ecd = ecd+1
      errcod(ecd)='233'
      errmsg(ecd)='Building Dimensions Specified for Non-POINT Source'

      ecd = ecd+1
      errcod(ecd)='234'
      errmsg(ecd)='Too Many Sectors Input for                        '

      ecd = ecd+1
      errcod(ecd)='235'
      errmsg(ecd)='Num of SRCGRPs exceeds limit for EVT name; Set=999'

      ecd = ecd+1
      errcod(ecd)='236'
      errmsg(ecd)='Not Enough BUILDHGTs Specified for SourceID       '

      ecd = ecd+1
      errcod(ecd)='237'
      errmsg(ecd)='Not Enough BUILDWIDs Specified for SourceID       '

      ecd = ecd+1
      errcod(ecd)='238'
      errmsg(ecd)='Not Enough BACKGRND Concentration Values Specified'

      ecd = ecd+1
      errcod(ecd)='239'
      errmsg(ecd)='Not Enough QFACTs Specified for SourceID          '

      ecd = ecd+1
      errcod(ecd)='240'
      errmsg(ecd)='Inconsistent Number of Particle Categories for    '

      ecd = ecd+1
      errcod(ecd)='241'
      errmsg(ecd)='Not Enough BUILDLENs Specified for SourceID       '

      ecd = ecd+1
      errcod(ecd)='242'
      errmsg(ecd)='No Particle Cat. or Gas Depos. Specified for SRCID'

      ecd = ecd+1
      errcod(ecd)='243'
      errmsg(ecd)='Wet depos (DEPOS, WDEP, WETDPLT) incompatible with'

      ecd = ecd+1
      errcod(ecd)='244'
      errmsg(ecd)='Source parameters are missing or incomplete for   '

      ecd = ecd+1
      errcod(ecd)='245'
      errmsg(ecd)='SrcGroup ID field is too long (>8); first 12 char:'

      ecd = ecd+1
      errcod(ecd)='246'
      errmsg(ecd)='Not Enough XBADJs Specified for SourceID          '

      ecd = ecd+1
      errcod(ecd)='247'
      errmsg(ecd)='Not Enough YBADJs Specified for SourceID          '

      ecd = ecd+1
      errcod(ecd)='248'
      errmsg(ecd)='Either BGVALs or BGFILE missing for this sector:  '

      ecd = ecd+1
      errcod(ecd)='249'
      errmsg(ecd)='Source elevation is missing (-9999.0); SRCID =    '

      ecd = ecd+1
      errcod(ecd)='250'
      errmsg(ecd)='Duplicate XPNT/DIST or YPNT/DIR Specified for GRID'

      ecd = ecd+1
      errcod(ecd)='251'
      errmsg(ecd)='Deposition (DEPOS, DDEP, WDEP) incompatible with  '

      ecd = ecd+1
      errcod(ecd)='252'
      errmsg(ecd)='Duplicate Receptor Network ID Specified.  NETID = '

      ecd = ecd+1
      errcod(ecd)='253'
      errmsg(ecd)='PSDGROUP ID field is too long (>8); first 12 char:'

! Multiple_BuoyLines_D41_Wood
!     Messages to accomodate multiple buoyant line processing.
      ecd = ecd+1
      errcod(ecd)='254'
      errmsg(ecd)='SrcID specified on BLPGROUP keyword not defined:  '

      ecd = ecd+1
      errcod(ecd)='255'
      errmsg(ecd)='Non-BL source specified in a BLPGROUP'

      ecd = ecd+1
      errcod(ecd)='256'
      errmsg(ecd)='EVALFILE Option Used Without EVALCART Receptors   '

      ecd = ecd+1
      errcod(ecd)='257'
      errmsg(ecd)='BL SourceID in more than one BLPGROUP'


!CRT  4/5/2022 D091 Missing BLINPUT Checks - Updated Message
      ecd = ecd+1
      errcod(ecd)='258'
      errmsg(ecd)='BL SourceID not in a BLPGROUP defined via BLPINPUT'

      ecd = ecd+1
      errcod(ecd)='259'
      errmsg(ecd)='Receptor elevation is missing (-9999.0); IREC =   '

      ecd = ecd+1
      errcod(ecd)='260'
      errmsg(ecd)='Number of EMISFACT/O3VALUES/BACKGRND values > max:'

      ecd = ecd+1
      errcod(ecd)='261'
      errmsg(ecd)='Not Enough O3VALUES Ozone Concentrations Specified'

      ecd = ecd+1
      errcod(ecd)='262'
      errmsg(ecd)='First Vertex Does Not Match LOCATION for AREAPOLY '

      ecd = ecd+1
      errcod(ecd)='264'
      errmsg(ecd)='Too Many Vertices Specified for AREAPOLY Source   '

      ecd = ecd+1
      errcod(ecd)='265'
      errmsg(ecd)='Not Enough Vertices Specified for AREAPOLY Source '

      ecd = ecd+1
      errcod(ecd)='266'
      errmsg(ecd)='Invalid shape defined (area=0) for AREAPOLY source'

      ecd = ecd+1
      errcod(ecd)='267'
      errmsg(ecd)='RLINE/RLINEXT requires Zs = 0.0 or FLAT; SRCID=   '

      ecd = ecd+1
      errcod(ecd)='271'
      errmsg(ecd)='O3FILE w/o O3VALs; full conv for hrs with miss O3 '

      ecd = ecd+1
      errcod(ecd)='272'
      errmsg(ecd)='Upper bound rank > Lower bound rank for MAXDCONT: '

      ecd = ecd+1
      errcod(ecd)='273'
      errmsg(ecd)='Range of ranks for MAXDCONT THRESH Opt is limited:'

! --- Included new messages regarding special processing for 1hr NO2/SO2
!     and 24hr PM25
      ecd = ecd+1
      errcod(ecd)='276'
      errmsg(ecd)='Special proc for 1h-NO2/SO2 24hPM25 NAAQS disabled'

      ecd = ecd+1
      errcod(ecd)='277'
      errmsg(ecd)='Specified option not applicable for this pollutant'

      ecd = ecd+1
      errcod(ecd)='278'
      errmsg(ecd)='Keyword only applies to RLINEXT source type:      '

      ecd = ecd+1
      errcod(ecd)='279'
      errmsg(ecd)='Multiple URBANOPT/URBANSRC inputs not allowed for:'

      ecd = ecd+1
      errcod(ecd)='280'
      errmsg(ecd)='Number of Output Types Specified Exceeds Max:NTYP='

!D132 Remove alpha requirement for rline/bline
!      ECD = ECD+1
!      ERRCOD(ECD)='281'
!      ERRMSG(ECD)='ALPHA required for RLINE/RLINEXT/BUOYLINE URBANSRC'

      ecd = ecd+1
      errcod(ecd)='282'
      errmsg(ecd)='Following SRCID Included in Multiple OLMGROUPs:   '

      ecd = ecd+1
      errcod(ecd)='283'
      errmsg(ecd)='OZONEVAL, O3VALUES or OZONEFIL Keyword Needed for '

      ecd = ecd+1
      errcod(ecd)='284'
      errmsg(ecd)='Invalid POLLUTID Given for NO2 option; Must Use   '

      ecd = ecd+1
      errcod(ecd)='285'
      errmsg(ecd)='BACKGROUND and BACKGRND are invalid as Source IDs '

      ecd = ecd+1
      errcod(ecd)='286'
      errmsg(ecd)='Following SRCID Included in Multiple PSDGROUPs:   '

      ecd = ecd+1
      errcod(ecd)='287'
      errmsg(ecd)='PSDGROUP ID Must be INCRCONS, RETRBASE or NONRBASE'

      ecd = ecd+1
      errcod(ecd)='288'
      errmsg(ecd)='Use of "*" for repeated values not meaningful for '

      ecd = ecd+1
      errcod(ecd)='289'
      errmsg(ecd)='Source defined as both particulate and gaseous    '

      ecd = ecd+1
      errcod(ecd)='290'
      errmsg(ecd)='This array limit exceeded; possible coding error: '

      ecd = ecd+1
      errcod(ecd)='291'
      errmsg(ecd)='Filename specified is too long. Maximum length =  '

      ecd = ecd+1
      errcod(ecd)='292'
      errmsg(ecd)='Potential problem with Fortran format specifier:  '

      ecd = ecd+1
      errcod(ecd)='293'
      errmsg(ecd)='User-specified met data format not used;  use FREE'

      ecd = ecd+1
      errcod(ecd)='294'
      errmsg(ecd)='PERIOD and ANNUAL averages are both selected for  '

      ecd = ecd+1
      errcod(ecd)='295'
      errmsg(ecd)='Invalid Averaging Period Specified for SCREEN Mode'

      ecd = ecd+1
      errcod(ecd)='296'
      errmsg(ecd)='Averaging Period .NE. 1-Hr for TOXXFILE Option    '

      ecd = ecd+1
      errcod(ecd)='297'
      errmsg(ecd)='Aver. Period must be .LE. 24 for EVENT Processing '

      ecd = ecd+1
      errcod(ecd)='298'
      errmsg(ecd)='Results reported for source group ALL include     '

      ecd = ecd+1
      errcod(ecd)='299'
      errmsg(ecd)='SRCGROUP ALL is missing, but is NOT required for  '

!-----------------------------------------------------------------------
!---- 300s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ecd = ecd+1
      errcod(ecd)='300'
      errmsg(ecd)='Specified SRCID Has Not Been Defined Yet: KEYWORD='

      ecd = ecd+1
      errcod(ecd)='301'
      errmsg(ecd)='Urban Area ID Has Not Been Defined.  URBID =      '

      ecd = ecd+1
      errcod(ecd)='302'
      errmsg(ecd)='Following SRCID Included in Multiple Urban Areas: '

      ecd = ecd+1
      errcod(ecd)='303'
      errmsg(ecd)='Urban ID has already been defined.  URBID =       '

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

      ecd = ecd+1
      errcod(ecd)='310'
      errmsg(ecd)='Attempt to Define Duplicate LOCATION Card for SRC:'

      ecd = ecd+1
      errcod(ecd)='313'
      errmsg(ecd)='Attempt to Define Duplicate EVENTPER card for     '

      ecd = ecd+1
      errcod(ecd)='314'
      errmsg(ecd)='Specified GRP index and SRC index is duplicated:  '

      ecd = ecd+1
      errcod(ecd)='315'
      errmsg(ecd)='Attempt to Define Duplicate SRCPARAM Card for SRC:'

      ecd = ecd+1
      errcod(ecd)='316'
      errmsg(ecd)='Specified SRCID is not included in any SRCGROUP:  '

      ecd = ecd+1
      errcod(ecd)='317'
      errmsg(ecd)='Specified SRCID is not included in any PSDGROUP:  '

      ecd = ecd+1
      errcod(ecd)='318'
      errmsg(ecd)='No Sources Defined for Urban Area.  URBID =       '

      ecd = ecd+1
      errcod(ecd)='319'
      errmsg(ecd)='No Sources Included in Specified Source Group:    '

      ecd = ecd+1
      errcod(ecd)='320'
      errmsg(ecd)='Input Parameter May Be Out-of-Range for Parameter '

      ecd = ecd+1
      errcod(ecd)='321'
      errmsg(ecd)='BACKGROUND concs are NOT included in any SRCGROUP!'

      ecd = ecd+1
      errcod(ecd)='322'
      errmsg(ecd)='Release Height Exceeds Effective Depth for OPENPIT'

      ecd = ecd+1
      errcod(ecd)='323'
      errmsg(ecd)='BACKGRND included w/o BACKGRND keyword for SrcGrp:'

      ecd = ecd+1
      errcod(ecd)='324'
      errmsg(ecd)='Release Height Exceeds 3000 Meters for SRCID:     '

      ecd = ecd+1
      errcod(ecd)='325'
      errmsg(ecd)='Negative Exit Velocity (Set=1.0E-5) for SRCID:    '

      ecd = ecd+1
      errcod(ecd)='330'
      errmsg(ecd)='Mass Fraction Parameters Do Not Sum to 1. for Src '

      ecd = ecd+1
      errcod(ecd)='332'
      errmsg(ecd)='Mass Fraction Parameter Out-of-Range for Source   '

      ecd = ecd+1
      errcod(ecd)='334'
      errmsg(ecd)='Particle Density Out-of-Range for Source          '

      ecd = ecd+1
      errcod(ecd)='335'
      errmsg(ecd)='Particle Diameter Out-of-Range for Source         '

      ecd = ecd+1
      errcod(ecd)='336'
      errmsg(ecd)='NO2RATIO Missing/Invalid for OLM/PVMRM/GRSM. Src:'

      ecd = ecd+1
      errcod(ecd)='338'
      errmsg(ecd)='Neg Emis Rate Invalid with OLM/PVMRM/GRSM. Src: '

      ecd = ecd+1
      errcod(ecd)='340'
      errmsg(ecd)='Possible Error in PROFBASE Input:  Value is < 0   '

      ecd = ecd+1
      errcod(ecd)='341'
      errmsg(ecd)='Emissions in HOUREMIS file < -90; set to 0.0 for  '

      ecd = ecd+1
      errcod(ecd)='342'
      errmsg(ecd)='Src ID Mismatch in Hourly Emissions File for ID = '

      ecd = ecd+1
      errcod(ecd)='344'
      errmsg(ecd)='Missing HOUREMIS fields; EmisRate set = 0. KURDAT='

      ecd = ecd+1
      errcod(ecd)='345'
      errmsg(ecd)='Problem processing the HOUREMIS file.   KURDAT =  '

      ecd = ecd+1
      errcod(ecd)='346'
      errmsg(ecd)='Too many fields for HOUREMIS file.     KURDAT =   '

      ecd = ecd+1
      errcod(ecd)='350'
      errmsg(ecd)='Julian Day Out Of Range at                        '

      ecd = ecd+1
      errcod(ecd)='352'
      errmsg(ecd)='The "H6H" field is no longer required for MULTYEAR'

      ecd = ecd+1
      errcod(ecd)='353'
      errmsg(ecd)='Urban Roughness Length (m) May Be Out-of-Range:   '

      ecd = ecd+1
      errcod(ecd)='360'
      errmsg(ecd)='2-Digit Year Specified: Valid for Range 1950-2049 '

      ecd = ecd+1
      errcod(ecd)='361'
      errmsg(ecd)='Multiyear PERIOD/ANNUAL values for NO2/SO2 require'

      ecd = ecd+1
      errcod(ecd)='362'
      errmsg(ecd)='Multiyear 1h NO2/SO2 processing not applicable for'

      ecd = ecd+1
      errcod(ecd)='363'
      errmsg(ecd)='Multiyr 24h/Ann PM25 processing not applicable for'

      ecd = ecd+1
      errcod(ecd)='365'
      errmsg(ecd)='Year Input is Greater Than 2147                   '

      ecd = ecd+1
      errcod(ecd)='370'
      errmsg(ecd)='Invalid Date: 2/29 In a Non-leap Year.            '

      ecd = ecd+1
      errcod(ecd)='380'
      errmsg(ecd)='This Input Variable is Out-of-Range:              '

      ecd = ecd+1
      errcod(ecd)='381'
      errmsg(ecd)='Latitude in Surface File Is Not Valid:            '

      ecd = ecd+1
      errcod(ecd)='382'
      errmsg(ecd)='Error Decoding Latitude:                          '

! --- More new messages for buoyant line processing; updated for multiple
!     buoyant line sources (Multiple_BuoyLines_D41_Wood)
      ecd = ecd+1
      errcod(ecd)='383'
      errmsg(ecd)='# buoy. lines in group not equal to # on HOUREMIS:'

      ecd = ecd+1
      errcod(ecd)='384'
      errmsg(ecd)='Not enough fields specified for HOUREMIS; KURDAT ='

! --- New messages for buoyant line processing; # '385'updated for multiple
!     buoyant line sources (Multiple_BuoyLines_D41_Wood)
      ecd = ecd+1
      errcod(ecd)='385'
      errmsg(ecd)='Following SRCID Included in Multiple BLPGROUPs:   '

      ecd = ecd+1
      errcod(ecd)='386'
      errmsg(ecd)='PARTDIAM and METHOD_2 specified for same SRCID:   '

      ecd = ecd+1
      errcod(ecd)='387'
      errmsg(ecd)='METHOD_2 option already specified for this SRCID: '

      ecd = ecd+1
      errcod(ecd)='388'
      errmsg(ecd)='Input buoyant line sources not in correct order:  '

      ecd = ecd+1
      errcod(ecd)='389'
      errmsg(ecd)='Rotated buoyant line sources not in correct order:'

      ecd = ecd+1
      errcod(ecd)='390'
      errmsg(ecd)='Aspect ratio (L/W) of LINE source greater than 100'

      ecd = ecd+1
      errcod(ecd)='391'
      errmsg(ecd)='Aspect ratio (L/W) of AREA source greater than 100'

      ecd = ecd+1
      errcod(ecd)='392'
      errmsg(ecd)='Aspect ratio (L/W) of OPENPIT is greater than 10  '

!     If at least one BL line is declared as urban, then all must be urban
      ecd = ecd+1
      errcod(ecd)='393'
      errmsg(ecd)='Not all lines in BL source declared urban in group'

      ecd = ecd+1
      errcod(ecd)='394'
      errmsg(ecd)='Met data may be from outdated version of AERMET:  '

      ecd = ecd+1
      errcod(ecd)='395'
      errmsg(ecd)='Met. Data Error; Incompatible Version of AERMET:  '

      ecd = ecd+1
      errcod(ecd)='396'
      errmsg(ecd)='AERMET Version Out-dated or Non-standard; Version:'

      ecd = ecd+1
      errcod(ecd)='397'
      errmsg(ecd)='SCREEN option used without use of SCREEN Met Data '

      ecd = ecd+1
      errcod(ecd)='398'
      errmsg(ecd)='SCREEN met used without specifying SCREEN option  '

      ecd = ecd+1
      errcod(ecd)='399'
      errmsg(ecd)='EXP format specified with no applicable file types'

!-----------------------------------------------------------------------
!---- 400s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ecd = ecd+1
      errcod(ecd)='400'
      errmsg(ecd)='Output values exceed format limit; use OU FILEFORM'

      ecd = ecd+1
      errcod(ecd)='401'
      errmsg(ecd)='Use of turbulence data with ADJ_U* is NonDFAULT   '

! --- New messages for v16216
      ecd = ecd+1
      errcod(ecd)='402'
      errmsg(ecd)='Turbulence data being used with ADJ_U* w/o DFAULT '

      ecd = ecd+1
      errcod(ecd)='403'
      errmsg(ecd)='Turbulence data is being used w/o ADJ_U* option   '

      ecd = ecd+1
      errcod(ecd)='405'
      errmsg(ecd)='Value of PHEE Exceeds 1.0 on KURDAT =             '

      ecd = ecd+1
      errcod(ecd)='406'
      errmsg(ecd)='Number of Vertices Exceeds Max (NVMAX) for SRCID: '

      ecd = ecd+1
      errcod(ecd)='409'
      errmsg(ecd)='Error Allocating Storage for Setup/Result Arrays! '

      ecd = ecd+1
      errcod(ecd)='410'
      errmsg(ecd)='Wind Direction Out-of-Range.  KURDAT =            '

! --- Included new message regarding QSUM = 0.0 runtime error in PVMRM;
!     this should never occur, but could indicate a programming error
      ecd = ecd+1
      errcod(ecd)='411'
      errmsg(ecd)='Possible ERROR in PVMRM_CALC! QSUM=0.0 @ Rec# Date'

      ecd = ecd+1
      errcod(ecd)='412'
      errmsg(ecd)='Possible ERROR in PVMRM_CALC! QSUM=0.0 @ Evt# Date'

      ecd = ecd+1
      errcod(ecd)='413'
      errmsg(ecd)='Number of Threshold Events > 999999 for Ave Period'


      ecd = ecd+1
      errcod(ecd)='415'
      errmsg(ecd)='MAXDCONT THRESH not reached within range of ranks '

      ecd = ecd+1
      errcod(ecd)='420'
      errmsg(ecd)='Wind Speed Out-of-Range.   KURDAT =               '

!     CRT D176 COARE Beta Check - COARE used to process met
      ecd = ecd+1
      errcod(ecd)='422'
      errmsg(ecd)='Meteorological data processed with COARE in AERMET'

!     CRT D176 COARE Beta Check - BULKRN cannot be used with COARE
      ecd = ecd+1
      errcod(ecd)='423'
      errmsg(ecd)='BULKRN in AERMET cannot be used with COARE        '

!     D127 - Added for minimum fran (see similar message 494 for max fran)
      ecd = ecd+1
      errcod(ecd)='424'
      errmsg(ecd)='Meander factor (FRAN) below min @ YR MN DY ISRC:  '

!CRT     D127 - Added for fran min > fran max
      ecd = ecd+1
      errcod(ecd)='426'
      errmsg(ecd)='LOW_WIND user-specified FRANMIN > FRANMAX:  '

      ecd = ecd+1
      errcod(ecd)='430'
      errmsg(ecd)='Ambient Temperature Data Out-of-Range.  KURDAT =  '

      ecd = ecd+1
      errcod(ecd)='432'
      errmsg(ecd)='Friction Velocity Out-of-Range.   KURDAT =        '

      ecd = ecd+1
      errcod(ecd)='435'
      errmsg(ecd)='Surface Roughness Length Out-of-Range.  KURDAT =  '

      ecd = ecd+1
      errcod(ecd)='438'
      errmsg(ecd)='Convective Velocity Data Out-of-Range.  KURDAT =  '

      ecd = ecd+1
      errcod(ecd)='439'
      errmsg(ecd)='Monin-Obukhov Length Out-of-Range.  KURDAT =      '

      ecd = ecd+1
      errcod(ecd)='440'
      errmsg(ecd)='Calm Hour Identified in Meteorology Data File at  '

      ecd = ecd+1
      errcod(ecd)='441'
      errmsg(ecd)='Vert Pot Temp Grad abv ZI set to min .005, KURDAT='

      ecd = ecd+1
      errcod(ecd)='442'
      errmsg(ecd)='Vert Pot Temp Grad abv ZI exceeds 0.1 K/m, KURDAT='

!     JAT 1/29/21  D070 TURBULENCE OPTIONS
!     ADD WARNING MESSAGE AND INFORMATIONAL MESSAGES
      ecd = ecd+1
      errcod(ecd)='443'
      errmsg(ecd)='SELECTED TURBULENCE OPTION:                       '

      ecd = ecd+1
      errcod(ecd)='444'
      errmsg(ecd)='TURBOPT INVALID WITH DFAULT; RESET                '

      ecd = ecd+1
      errcod(ecd)='445'
      errmsg(ecd)='Set sigma-theta to missing for                    '

      ecd = ecd+1
      errcod(ecd)='446'
      errmsg(ecd)='Set sigma-w to missing for                        '

      ecd = ecd+1
      errcod(ecd)='450'
      errmsg(ecd)='Record Out of Sequence in Meteorological File at: '

      ecd = ecd+1
      errcod(ecd)='452'
      errmsg(ecd)='Missing hourly BACKGRND w/o BGSUB, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='453'
      errmsg(ecd)='BGSUB for missing hourly BACKGRND, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='454'
      errmsg(ecd)='Date/time Mismatch: BACKGRND File, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='455'
      errmsg(ecd)='Date/time Mismatch: Hourly Emission File, KURDAT ='

      ecd = ecd+1
      errcod(ecd)='456'
      errmsg(ecd)='Date/time Mismatch on Surface & Profile.  KURDAT ='

      ecd = ecd+1
      errcod(ecd)='457'
      errmsg(ecd)='Date/time Mismatch: OZONEFIL File, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='458'
      errmsg(ecd)='O3SUB for missing hourly O3 value, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='459'
      errmsg(ecd)='No Hrly O3 & No Sub; Use Full Conversion, KURDAT ='

      ecd = ecd+1
      errcod(ecd)='460'
      errmsg(ecd)='Missing Hour Identified in Meteor. Data File at   '

      ecd = ecd+1
      errcod(ecd)='465'
      errmsg(ecd)='Number of Profile Levels Exceeds Max:   MXPLVL =  '

      ecd = ecd+1
      errcod(ecd)='470'
      errmsg(ecd)='Mixing Height Value is < or = 0.0.   KURDAT =     '

      ecd = ecd+1
      errcod(ecd)='471'
      errmsg(ecd)='Met. ref hgt WS < 1.0 m/s; set to 1.0 for BL srcs '

      ecd = ecd+1
      errcod(ecd)='472'
      errmsg(ecd)='Release hgt < 2.0m for BL source; set to 2.0m for '

!     JAT D068 1/15/21:  UDPATED DEPOSITION MESSAGE AFTER NEWER MESSAGES
      ecd = ecd+1
      errcod(ecd)='473'
      errmsg(ecd)='Default deposition parameter(s) used for SRCID:   '

      ecd = ecd+1
      errcod(ecd)='474'
      errmsg(ecd)='WS RefHt invalid (<0.001); Not msg or clm: KURDAT='

      ecd = ecd+1
      errcod(ecd)='475'
      errmsg(ecd)='WS reference height is higher than 100m.  KURDAT ='

! --- More new messages for buoyant line processing; updated for multiple
!     buoyant line sources (Multiple_BuoyLines_D41_Wood)
      ecd = ecd+1
      errcod(ecd)='476'
      errmsg(ecd)='# receptors within BL source (BLPGROUP)           '
      ecd = ecd+1
      errcod(ecd)='477'
      errmsg(ecd)='Receptor inside BL source group for event:        '

!CRT  2/22/2021: D059 AWMA Downwash Options
!CRT  Resolve conflict with error array index and code
      ecd = ecd+1
      errcod(ecd)='478'
      errmsg(ecd)='AWMAUTurb & AWMAUTurbHX entered; AWMAUTurbHX used '

      ecd = ecd+1
      errcod(ecd)='479'
      errmsg(ecd)='Potential temperature gradient is out-of-range:   '

      ecd = ecd+1
      errcod(ecd)='480'
      errmsg(ecd)='Less than 1yr for MULTYEAR, MAXDCONT or ANNUAL Ave'

      ecd = ecd+1
      errcod(ecd)='481'
      errmsg(ecd)='Data Remaining After End of Year. Number of Hours='

      ecd = ecd+1
      errcod(ecd)='482'
      errmsg(ecd)='Too many years modeled for 24h-PM25 1h-NO2 1h-SO2:'

      ecd = ecd+1
      errcod(ecd)='483'
      errmsg(ecd)='User Start Date is Earlier Than Start of Met File '

      ecd = ecd+1
      errcod(ecd)='484'
      errmsg(ecd)='Restart Date < STARTEND date or start of Met File '

      ecd = ecd+1
      errcod(ecd)='485'
      errmsg(ecd)='MULTYR DataGap; Restart Date < STARTEND or MetFile'

      ecd = ecd+1
      errcod(ecd)='486'
      errmsg(ecd)='MULTYR Date Overlap; STARTEND Date < Restart Date '

      ecd = ecd+1
      errcod(ecd)='487'
      errmsg(ecd)='MULTYR Date Overlap; MetFile Start < Restart Date '

      ecd = ecd+1
      errcod(ecd)='488'
      errmsg(ecd)='First met HR.ne.1; ST results may not be valid    '

      ecd = ecd+1
      errcod(ecd)='489'
      errmsg(ecd)='First met HR.ne.1; EV results may not be valid for'

      ecd = ecd+1
      errcod(ecd)='490'
      errmsg(ecd)='Problem reading SURFFILE date for EVENTS; MNDYHR ='

      ecd = ecd+1
      errcod(ecd)='491'
      errmsg(ecd)='MAXDCONT option requires 1st Hr of met data = 01; '

      ecd = ecd+1
      errcod(ecd)='492'
      errmsg(ecd)='SURFDATA YR .NE. 1st YR of file, adj to match file'

      ecd = ecd+1
      errcod(ecd)='493'
      errmsg(ecd)='SURFDATA YR must match 1st YR of file for DAYRANGE'

!     see similar message 424 for fran max
      ecd = ecd+1
      errcod(ecd)='494'
      errmsg(ecd)='Meander factor (FRAN) exceeds max @ YR MN DY ISRC:'

      ecd = ecd+1
      errcod(ecd)='495'
      errmsg(ecd)='Surface met file does not include enough variables'

      ecd = ecd+1
      errcod(ecd)='496'
      errmsg(ecd)='Total precipitation in SURFFILE is zero (0.0) with'

      ecd = ecd+1
      errcod(ecd)='497'
      errmsg(ecd)='Possible code ERROR!!! EVENT mismatch for EVENTID:'

      ecd = ecd+1
      errcod(ecd)='498'
      errmsg(ecd)='Possible code ERROR! MAXDCONT mismatch GRP/RNK/REC'

      ecd = ecd+1
      errcod(ecd)='499'
      errmsg(ecd)='PRIME plume rise error; check stack parameters for'

!-----------------------------------------------------------------------
!---- 500s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ecd = ecd+1
      errcod(ecd)='500'
      errmsg(ecd)='Fatal Error Occurs Opening the Data File of       '

      ecd = ecd+1
      errcod(ecd)='501'
      errmsg(ecd)='Dup Filename! Fatal Error Opening the Data File of'

! Multiple_BuoyLines_D41_Wood
!     Messages to accomodate multiple buoyant line processing.
!CRT  4/5/2022 D091 Missing BLPINPUT Checks - Updated Message
      ecd = ecd+1
      errcod(ecd)='502'
      errmsg(ecd)='No BLPINPUT record for BLPGROUP ID'

      ecd = ecd+1
      errcod(ecd)='503'
      errmsg(ecd)='BLPGROUP IDs do not match BLPINPUT IDs'

      ecd = ecd+1
      errcod(ecd)='504'
      errmsg(ecd)='BLPINPUT group ID already specified-must be unique'

      ecd = ecd+1
      errcod(ecd)='505'
      errmsg(ecd)='BLPINPUT records out of order, BLPINPUT Groups:'

! D_32 Wood - Check for non-parallel lines within group
      ecd = ecd+1
      errcod(ecd)='506'
      errmsg(ecd)='BUOYLINE not parallel to 1st line of its group:'

!CRT  4/5/2022 D091 Missing BLPINPUT Checks - Added Messages (507, 508, 509)
      ecd = ecd+1
      errcod(ecd)='507'
      errmsg(ecd)='No BLPINPUT records for BUOYLINE sources'

      ecd = ecd+1
      errcod(ecd)='508'
      errmsg(ecd)='No BUOYLINE sources for BLPINPUT record'

      ecd = ecd+1
      errcod(ecd)='509'
      errmsg(ecd)='# BLPGROUP IDs defined by BLPINPUTs <> # allocated'

      ecd = ecd+1
      errcod(ecd)='510'
      errmsg(ecd)='Fatal Error Occurs During Reading of the File of  '

      ecd = ecd+1
      errcod(ecd)='520'
      errmsg(ecd)='Fatal Error Occurs During Writing to the File of  '

      ecd = ecd+1
      errcod(ecd)='530'
      errmsg(ecd)='CAUTION! Met Station ID Mismatch with SURFFILE for'

      ecd = ecd+1
      errcod(ecd)='531'
      errmsg(ecd)='CAUTION! Met Station ID Missing from SURFFILE for '

      ecd = ecd+1
      errcod(ecd)='540'
      errmsg(ecd)='No RECTABLE/MAXTABLE/DAYTABLE for Average Period  '

      ecd = ecd+1
      errcod(ecd)='550'
      errmsg(ecd)='File Unit/Name Conflict for the Output Option:    '

      ecd = ecd+1
      errcod(ecd)='555'
      errmsg(ecd)='File Unit/Name conflict across options: GRP# AVE  '

      ecd = ecd+1
      errcod(ecd)='560'
      errmsg(ecd)='User Specified File Unit .LE. 30 for OU Keyword:  '

      ecd = ecd+1
      errcod(ecd)='565'
      errmsg(ecd)='Possible Conflict With Dynamically Allocated FUNIT'

      ecd = ecd+1
      errcod(ecd)='570'
      errmsg(ecd)='Problem Reading Temporary Event File for Event:   '

      ecd = ecd+1
      errcod(ecd)='580'
      errmsg(ecd)='End of File Reached Trying to Read the File of    '

      ecd = ecd+1
      errcod(ecd)='585'
      errmsg(ecd)='Output data file for INITFILE option was not found'

      ecd = ecd+1
      errcod(ecd)='590'
      errmsg(ecd)='The INITFILE filename matches a SAVEFILE filename '

      ecd = ecd+1
      errcod(ecd)='592'
      errmsg(ecd)='MAXIFILE includes data past start of MULTYEAR run '
      ecd = ecd+1
      errcod(ecd)='593'
      errmsg(ecd)='POSTFILE includes data past start of MULTYEAR run '

!-----------------------------------------------------------------------
!---- 600s -------------------------------------------------------------
!-----------------------------------------------------------------------

!RCO 2/25/2021 need to resolve error code numbers
!---- CERC 11/30/20 New messages for GRSM option
      ecd = ecd+1
      errcod(ecd)='600'
      errmsg(ecd)='Keyword Invalid w/o PVMRM/OLM/GRSM/TTRM:        '

      ecd = ecd+1
      errcod(ecd)='601'
      errmsg(ecd)='Error calculating travel time for GRSM option:  '

      ecd = ecd+1
      errcod(ecd)='602'
      errmsg(ecd)='Following Keyword Invalid Without GRSM:         '

      ecd = ecd+1
      errcod(ecd)='603'
      errmsg(ecd)='Not Enough NOx Concentrations Specified           '

      ecd = ecd+1
      errcod(ecd)='604'
      errmsg(ecd)='NOXVALUE, NOX_VALS or NOX_FILE Keyword Needed for '

      ecd = ecd+1
      errcod(ecd)='605'
      errmsg(ecd)='Both NOXVALUE and NOX_VALS keywords are specified '

      ecd = ecd+1
      errcod(ecd)='606'
      errmsg(ecd)='Inconsistent temporally-varying NOX_VALS options: '

      ecd = ecd+1
      errcod(ecd)='607'
      errmsg(ecd)='No Hrly NOx & No Sub; Assume zero conc,   KURDAT ='

      ecd = ecd+1
      errcod(ecd)='608'
      errmsg(ecd)='Date/time Mismatch: NOX_FILE File, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='609'
      errmsg(ecd)='No Hrly NOx & No Sub; Use zero conc,      KURDAT ='

      ecd = ecd+1
      errcod(ecd)='610'
      errmsg(ecd)='NOXSUB for missing hourly NOX val, KURDAT/Sector ='

      ecd = ecd+1
      errcod(ecd)='611'
      errmsg(ecd)='NOXFIL w/o NOXVALs; zero bgd for hrs wth miss NOx '

      ecd = ecd+1
      errcod(ecd)='612'
      errmsg(ecd)='No NOx bgd.  NOx bgd calc from NO2 assuming equil '

      ecd = ecd+1
      errcod(ecd)='613'
      errmsg(ecd)='NO2 bgd value > NOx bgd value. Setting NOx = NO2. '

      ecd = ecd+1
      errcod(ecd)='614'
      errmsg(ecd)='Src type not recognised in instant. plume calc.   '

      ecd = ecd+1
      errcod(ecd)='615'
      errmsg(ecd)='Error allocating variables for plume size calc.   '

      ecd = ecd+1
      errcod(ecd)='620'
      errmsg(ecd)='Both RLINE barriers on same side of SRCID: '

! --- New error code for NOMINO3
      ecd = ecd+1
      errcod(ecd)='621'
      errmsg(ecd)='No applicable NO2 option set with '

! --- New messages related to platform downwash: 3/18/2022, CRT
      ecd = ecd+1
      errcod(ecd)='631'
      errmsg(ecd)='PLATFORM Keyword Specified for Non-POINT Source'

      ecd = ecd+1
      errcod(ecd)='632'
      errmsg(ecd)='Duplicate PLATFORM Keyword Specified for Source'

      ecd = ecd+1
      errcod(ecd)='633'
      errmsg(ecd)='PRIME and PLATFORM Parameters Specified for Source'

!CRT  D036 CRT 3/8/2022: Add new error message for 'CCC' placeholder
!CRT  leftover from v.21112
      ecd = ecd+1
      errcod(ecd)='640'
      errmsg(ecd)='Invalid source type: '

!CRT  CRT 4/20/2022 D113 - Added for Sidewash source - negative concentratoin
      ecd = ecd+1
      errcod(ecd)='650'
      errmsg(ecd)='SWPOINT neg. conc. set to 0.0 (see debug), Hr: '

!    D128 4/5/23: Added warning message when the AREA meander keyword is used without an AREA source
      ecd = ecd+1
      errcod(ecd)='668'
      errmsg(ecd)='AREAMNDR used without an AREA source present '

!-----------------------------------------------------------------------
!---- 700s -------------------------------------------------------------
!-----------------------------------------------------------------------

      ecd = ecd+1
      errcod(ecd)='710'
      errmsg(ecd)='TTRM NO2 processing not currently configured for '

!     Added error statement for RLINE barrier or depressed sources missing
!     FLAT option Wood 10/10/22
      ecd = ecd+1
      errcod(ecd)='713'
      errmsg(ecd)='Non-DFAULT FLAT required for RBARRIER/RDEPRESSION '

      ecd = ecd+1
      errcod(ecd)='720'
      errmsg(ecd)='Non-fatal Error Occurs Writing to the File of  '

      ecd = ecd+1
      errcod(ecd)='721'
      errmsg(ecd)='2nd NOx-to-NO2 option not selected for TTRM2 '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ecd = ecd+1
      errcod(ecd)='722'
      errmsg(ecd)='PVMRM NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ecd = ecd+1
      errcod(ecd)='723'
      errmsg(ecd)='TTRM2 NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ecd = ecd+1
      errcod(ecd)='724'
      errmsg(ecd)='GRSM NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 2/20/23 WSP
      ecd = ecd+1
      errcod(ecd)='725'
      errmsg(ecd)='OLM NO2 processing not currently configured for '
! ---   D161 Added error message for sources which NO2 options are not implemented 4/21/23 CRT
      ecd = ecd+1
      errcod(ecd)='726'
      errmsg(ecd)='ARM2 NO2 processing not currently configured for '

!     D164 2/21/23 WSP Message when SCREEN option is used with incompatible sources
      ecd = ecd+1
      errcod(ecd)='731'
      errmsg(ecd)='SCREEN processing not currently configured for '

!     D081 - Added for 24-hr average when less than 18 hours of data are present Wood 9/28/22
      ecd = ecd+1
      errcod(ecd)='732'
      errmsg(ecd)='24-hr avg, < 18 hours of data, calms policy used.'

!     D081 - Added for 8-hr average when less than 6 hours of data are present CRT 4/28/2023
      ecd = ecd+1
      errcod(ecd)='733'
      errmsg(ecd)='8-hr avg, < 6 hours of data, calms policy used.'

!     D081 - Added for 3-hr average when less than 3 hours of data are present CRT 4/28/2023
      ecd = ecd+1
      errcod(ecd)='734'
      errmsg(ecd)='3-hr avg, < 3 hours of data, calms policy used.'

!     D157 WSP 3/28/2023 - Added for ARMRATIO limits when cahnged from DFAULT 0.5 and 0.9
!     D157 CRT 5/31/2023 - Modify message when ARMRATIO is within default range.
      ecd = ecd+1
      errcod(ecd)='736'
      errmsg(ecd)='ARMRATIO within DFAULT range, 0.5 - 0.9 '
!     ERRMSG(ECD)='ARMRATIO in DFAULT range, 0.5 <= ARMRATIO <= 0.9 '

!     D157 CRT 5/31/2023 - Modify warning when ARMRATIO is outside of default range.
      ecd = ecd+1
      errcod(ecd)='737'
      errmsg(ecd)='ARMRATIO outside of DFAULT range of 0.5 - 0.9 '
!     ERRMSG(ECD)='Non-Default ARMRATIO range, 0 < ARMRATIO <= 1 '

!     Added for HBP
      ecd = ecd+1
      errcod(ecd)='740'
      errmsg(ecd)='HBP option ignored for non-POINT source: '

!     D167 Warning message when 'FLAT' is used in the source elevation
!     field on the SO LOCATION pathway WSP 3/6/23
      ecd = ecd+1
      errcod(ecd)='752'
      errmsg(ecd)='ZHILL and ZELEV are ignored for flat source: '

!-----------------------------------------------------------------------
!---- 800s -------------------------------------------------------------
!-----------------------------------------------------------------------
!**  Added for Aircraft Plume Rise; UNC-IE

      ecd = ecd+1
      errcod(ecd)='807'
      errmsg(ecd)='Not enough ARCFTSRC parameters for SRCID = '

      ecd = ecd+1
      errcod(ecd)='821'
      errmsg(ecd)='Missing Mandatory Keyword for ARCFTSRC in CO PTHWY '

      ecd = ecd+1
      errcod(ecd)='822'
      errmsg(ecd)='Missing Mandatory Keyword for Aircraft. Keyword is '

      ecd = ecd+1
      errcod(ecd)='823'
      errmsg(ecd)='No HOUREMIS File Found, Must for Aircraft Source '

      ecd = ecd+1
      errcod(ecd)='824'
      errmsg(ecd)='Not All Aircraft Parameters defined;set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='825'
      errmsg(ecd)='Too many Aircraft Parameters defined;set to 0. for '

      ecd = ecd+1
      errcod(ecd)='826'
      errmsg(ecd)='FuelBurnRate in HOUREMIS file < 0;set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='827'
      errmsg(ecd)='Thrust in HOUREMIS file < 0; set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='828'
      errmsg(ecd)='AircraftSpeed in HOUREMIS file < 0;set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='829'
      errmsg(ecd)='AirFuelRatio in HOUREMIS file < 0;set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='830'
      errmsg(ecd)='ByPR in HOUREMIS < 0 & not as -999;set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='831'
      errmsg(ecd)='RPWR in HOUREMIS < 0 & not as -99999;set 0.0 for '

      ecd = ecd+1
      errcod(ecd)='832'
      errmsg(ecd)='SrcAngle in HOUREMIS != -20 to 20;set to 0.0 for '

      ecd = ecd+1
      errcod(ecd)='833'
      errmsg(ecd)='This source does not fall into Aircraft category '

!**  End Aircraft Plume Rise insert; April 2023
   end subroutine errwrnmsg



end module main1

module rline_data
!***********************************************************************
!     This is The Global Variable Definition Block for the New RLINE
!     Source Algorithm - December 2017 (Wood)
!
!     MODIFIED:   Michelle G. Snyder, Wood - 6/22/2021
!     MODIFIED:   Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!***********************************************************************

!     Indicate RLINE algorithms based on R-LINE model version 1.2.
   character (len=12)  :: RLINEver = "RLINEv1_2"

   integer :: nrlines
   logical :: rlprocessed
   logical :: rlfirsthr
   logical :: rlmovesconv
   logical :: l_rdepress
   logical :: l_rbarrier
!     NRLINES     = number of RLINE sources
!     RLPROCESSED = flag to perform rotation on first RLINE source of each hour
!     RMFIRSTHR   = flag to compute CREATE_EXP_TABLE and RLEMCONV only once
!     RLMOVESCONV = flag to indicate if input units from MOVES
!     L_RDEPRESS  = flag to indicate key word of "RDEPRESS" in source section of input file
!     L_RBARRIER  = flag to indicate key word of "RBARRIER" in source section of input file

   type avsource
      integer   :: isrcnum
      double precision  ::  xsb, ysb, zsb
      double precision  ::  xse, yse, zse
      double precision  ::  dcl, init_sigmaz, width, qemis
      double precision  ::  htwall, dclwall, depth, wtop, wbottom
      double precision  ::  htwall2, dclwall2
   end type avsource
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
   type(avsource),    allocatable  :: rlsource(:)

!     Array to store MOVES to RLINE native units
   double precision,  allocatable  :: rlemisconv(:)

!     Meteorological variables
   double precision  :: sigmav, dispht, ueff, thetaw, rlwstar
   double precision  :: wspd_adj, umin, sigz_y
   double precision  :: z0_a(3), dh_a(3), ust_a(3), lmo_a(3)
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
   double precision, parameter  :: error_limit = 5.0d-4
   double precision  :: fac_dispht !removed = 5.0D0  and PARAMETER definition (wood 6/22/21)
!     ERROR_LIMIT = RLINE error limit
!     FAC_DISPHT  = ratio of displacement height to roughness length (DISPHT=FAC_DISPTH*SFCZ0)

!     Source variables
   integer           :: indq
   double precision  :: sigmay0, sigmaz0, hrelease
   double precision  :: xsbegin, ysbegin, zsbegin
   double precision  :: xsend, ysend, zsend
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
   double precision, parameter  :: sm_num = 1.0d-8
   double precision, parameter  :: xd_min = 1.0d0
   integer, parameter           :: np = 100
!     SM_NUM      = number for numerical calculations to avoid a zero
!     XD_MIN      = minimum distance between a source and receptor
!     NP          = number of points in wind speed table

!     Computation variables
   double precision  :: xexp(1000), aexp(1000), bexp(1000), delexp
   double precision  :: zwind(np,3), awind(np,3), bwind(np,3)
   double precision  :: delz(3), logzmax, logzmin(3)              ! CREATE_WIND_TABLE
   integer           :: i_alpha
   double precision  :: xrecep, yrecep, zrecep
   double precision, allocatable  :: xrcp_rot(:), yrcp_rot(:)
   double precision  :: x0, y0
   double precision  :: xr_rot, yr_rot
   double precision, allocatable  :: xsb_rot(:), ysb_rot(:),&
   &xse_rot(:), yse_rot(:)

   integer, allocatable :: bdw_flag(:,:)
   logical              :: shift_flag
   double precision     :: szb, xper, theta_line
   double precision     :: dwu, dw_peru, hbu
   double precision     :: dwd, dw_perd, hbd
   integer              :: nbarr
   double precision     :: uh, xshift, yshift
   double precision     :: alpha, alpha_u, alpha_d
   logical              :: fastrline
   double precision     :: psy1, psy2, psy3, psy4
   double precision     :: psz1, psz2, psz3, psz4
   double precision     :: pu1, pu2, pu3, pu4

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
   double precision  :: fran_sum, sigmav_sum, ueff_sum,&
   &vert_sum, horz_sum, conc_p_sum, conc_m_sum,&
   &point_conc_sum

end module rline_data

module buoyant_line
!***********************************************************************
!     This is The Global Variable Definition Block for the New BUOYLINE
!     Source Algorithm - January 2015
!***********************************************************************


! BuoyantLine_CheckLinesParallel_D32 (Wood)
   double precision, parameter :: Parallel_Tol = 5.0d0


! Multiple_BuoyLines_D41_Wood
!     Added several variables and changed a few arrays from 1-D to 2_D
!     to process multiple buoyant lines

   type blinedata
      integer   (kind=4)   :: isrcnum
      character (len=12)   :: srcid

! Multiple_BuoyLines_D41_Wood
!        Added for processing multiple buoyan lines
      integer   (kind=4)   :: iblpgrpnum
      character (len=8)    :: blpgrpname

!         INTEGER   (kind=4)   :: IURBSRCNUM
!         CHARACTER (len=8)    :: URBSRCNAME

      double precision     :: xbeg, ybeg, xend, yend                 ! untranslated, unrotated
      double precision     :: xbeg_tr1, ybeg_tr1, xend_tr1, yend_tr1 ! translated, rotate #1 w/TCOR
      double precision     :: elev, blqs, blhs
   end type blinedata

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

   type (blinedata), allocatable :: blineparms (:)

!     Whereas the source coordinates in BLINEPARMS are for the entire
!      line, the following are for the segments of the line.
   double precision, allocatable :: xs_scs(:,:), ys_scs(:)
   double precision, allocatable :: xs_rcs(:,:), ys_rcs(:,:)

!      1st subscript: individual buoyant line number
!      2nd subscript: segment number (not needed for YS_SCS)
!      _SCS = source coordinate system, initial translation/rotation and
!             performed only once
!      _RCS = rotated coordinate system, translated/rotated for wind dir

   logical              :: l_blsource, l_blhourly
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
   logical, allocatable :: l_blurban(:)
   logical, allocatable :: bl_rflag(:,:)
   integer, allocatable :: nblingrp(:)
   integer, allocatable :: hrlyblcount(:)
   integer, allocatable :: bl_numurb(:)
   integer              :: nblp, nbltotal, numblgrps
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

   double precision, allocatable ::&
   &blavginp_llen(:), blavginp_bhgt(:), blavginp_bwid(:),&
   &blavginp_lwid(:), blavginp_bsep(:), blavginp_fprm(:)
   character (len=8), allocatable :: blavginp_grpid(:), bl_grpid(:)
   integer     :: numblavginp, NBLAVGINPalloc
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
   double precision :: blavgllen, blavgbhgt, blavgbwid,&
   &blavglwid, blavgbsep, blavgfprm
!     BLAVGLLEN  = average line length (m)
!     BLAVGBHGT  = average building height (m)
!     BLAVGBWID  = average building width (m)
!     BLAVGLWID  = average line source width (m)
!     BLAVGBSEP  = average building separation (m)
!     BLAVGFPRM  = average buoyancy parameter (m^4/s^3)

   double precision :: blta
   double precision :: bl_uref
!     BLTA       = ambient temperature read from AERMET's surface file
!                  (saved because TA is modified if there are point sources)
!     BL_UREF    = reference wind speed used in BL calculations; set to
!                  1.0 m/s if WS is less than 1.0 m/s

   double precision, dimension (7)  :: bl_xdist = (0.0d0)
   double precision, dimension (7)  :: dh = (0.0d0)
!CRT  4/2/2022 D088 Limit on # of buoyant lines, make DEL allocatable
!      DOUBLE PRECISION, dimension (10) :: DEL
   double precision, allocatable    :: del(:)
   double precision :: bl_xfb, leff, ld, r0, bl_xfinal, bl_xfs
   double precision :: fprmxnb, xmatch, partch
!     The following variables are used in a couple subroutines, therefore
!       they must be allocated for multiple buoyant lines

   double precision, allocatable :: xor(:), yor(:), angrad(:)

   double precision, allocatable :: tcor(:), sintcor(:), costcor(:)
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
   double precision, allocatable :: xr_scs(:,:), yr_scs(:,:)
   double precision, allocatable :: xr_rcs(:), yr_rcs(:)
!
!      1st SCS subscript:
!      2nd SCS subscript:
!      _SCS = source coordinate system, initial translation/rotation and
!             performed only once
!      _RCS = rotated coordinate system, translated/rotated for wind dir
!
!     The following arrays are used to save the coordinates from the
!       first tranlastion/rotation of the receptors when MAXDCONT is used
   double precision, allocatable :: xr_scs_sav(:,:), yr_scs_sav(:,:)

!     The following array is used to save the flag indicating if a
!       receptor is in the rectangular footprint defined by a buoyant
!       line source (exclusion zone) of the buoyant line source when
!       MAXDCONT is used
   logical, allocatable :: bl_rflag_sav(:,:)

!     Arrays for total and partial concentration from lines,
!      to be allocated with the number of receptors
   double precision, allocatable :: chibl(:)

end module buoyant_line

!-----------------------------------------------------------------------
! --- The following MODULE subprograms replace the *.pri "INCLUDE"
!     files formerly used for global data storage for PRIME, and the
!     /PLU/-named COMMON block used in a few subroutines.
! --- R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009


module prime_params

! --- Formerly part of PARAMS.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- PARAMETER statements                                        PRIME
!----------------------------------------------------------------------

   integer, parameter :: io5=7,io6=8

! --- FORTRAN I/O unit numbers:
!           IO5 - Control file                  - input  - formatted
!           IO6 - List file                     - output - formatted
!

end module prime_params


module prime_numparm

! --- Formerly NUMPARM.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /NUMPARM/ -- Parameters used in the            PRIME
!                               numerical plume rise algorithm
!----------------------------------------------------------------------
!
   integer, parameter :: mxnw=5000
   integer, parameter :: mxent=10
   integer, parameter :: mxentp1=mxent+1
   integer :: nstep, nent
   double precision :: gravi,rgas,zmin,ds,slast,rp,&
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

end module prime_numparm


module prime_dfsn

! --- Formerly DFSN.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /DFSN/ -- Parameters used in the            PRIME
!                            PRIME turbulence and diffusion
!                            subroutines
!----------------------------------------------------------------------
!
   double precision :: afac,xbyrmax,wiz0,wiy0,wfz,wfy,&
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

end module prime_dfsn


module prime_wakedat

! --- Formerly WAKEDAT.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /WAKEDAT/ -- Parameters used in the            PRIME
!                               PRIME wake and streamline
!                               subroutines
!----------------------------------------------------------------------
!
   logical :: lrurl
   integer, parameter :: mxntr=50
   integer :: nwak,ncav
   double precision :: Hb,Wb,xLb,Rb,hr,xLR,xLC,&
   &xbadj,ybadj,Ub,Urh,&
   &xwak(mxntr),szwak(mxntr),sywak(mxntr),&
   &drwak(mxntr),&
   &xcav(mxntr),szcav(mxntr),sycav(mxntr),&
   &fqcav,&
   &vsigy, vsigz, vsigyc, vsigzc, zint

   double precision :: third  ! constant = 1/3 used in various places
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

end module prime_wakedat


module prime_ambient

! --- Formerly AMBIENT.PRI "INCLUDE" File:
!
!----------------------------------------------------------------------
! --- COMMON BLOCK /AMBIENT/ -- Selected met. data at one         PRIME
!                               grid cell;  used in numerical
!                               plume rise computation
!----------------------------------------------------------------------
!
   integer, parameter :: mxnz=100
   integer, parameter :: mxnzp1=mxnz+1
   integer :: nza
   double precision :: uamb(mxnz),ramb(mxnz),dedz(mxnzp1),tamb(mxnz),&
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

end module prime_ambient


module prime_plu

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

   double precision :: s,x,y,z,r,u,v,w,usc,phi,den,tp

end module prime_plu

! --- Additional modules for AWMADW

!     Make effective height, calculated in PRMCALC, available to wake_u_turb
module prm2_wakedat
   integer, parameter :: p2mxntr=50
   double precision :: Zeff_PRM2, u30, sv30, sw30
!         DOUBLE PRECISION XTR, ZTR, NTR
   double precision :: xtr_sav(p2mxntr), ztr_sav(p2mxntr)
   double precision :: Ueff_save, SWeff_save, SVeff_save
   logical :: dfsn2call
end module prm2_wakedat

!     CERC 11/30/20 Module for GRSM NO2 option
module grsmmod

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

   double precision :: r1, r2
   double precision :: noxconc_bg,no2conc_bg,noconc_bg,o3conc_bg
   integer, parameter :: nNO=1, nNO2=2, nO3=3, nPolsGRSM=3
   double precision :: conctemp(nPolsGRSM)
   logical :: L_NightHour
   double precision, parameter :: CFrac = 1.0d-2
! MKP    4/23/2024 D193 fix provided by CERC
!        Prevents denormal or very small concentration values from being
!        passed to chemistry solver resulting in NaNs for certain ground
!        level releases from area, volume, and openpit source types
!        See grsm.f/DoGRSMChem
   double precision, parameter::MinimumConc=1.0d-21

end module grsmmod

