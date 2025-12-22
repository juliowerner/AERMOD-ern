subroutine inpsum
!***********************************************************************
!                 INPSUM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Input Data Summary
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Print summary of Model Options and Met Data
!                    to optional SUMMFILE.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   integer :: i
   character :: modnam*12
! Unused: INTEGER ILEN, NOPS

!     Variable Initializations
   modnam = 'INPSUM'

!     Print Out The Model Options
   call prtopt(iounit)
   if (summfile) then
! ---    Include Model Options Summary in SUMMFILE
      call prtopt(isumunt)
   end if

! --- Print temporally varying ozone concentrations,
!     if specified on the O3VALUES keyword
   do i = 1, NUMO3Sects
      if (l_o3values(i)) then
         call prto3vals(i)
      end if
   end do

! --- CERC 11/30/20 Print temporally varying NOx concentrations,
!     if specified on the NOX_VALS keyword
   do i = 1, NUMNOxSects
      if (l_nox_vals(i)) then
         call prtnoxvals(i)
      end if
   end do

!     Print Out The Input Source Data
   call prtsrc

! --- Print Out Background Concentrations, if specified
   if (l_backgrnd) then
      do i = 1, NUMBGSects
         call prtbkg(i)
      end do
   end if

   if (.not. evonly) then
!        Print Out The Input Receptor Coordinates.
      call prtrec

!        Check For Receptors Too Close To Sources (< 1m or < 3Lb)
      call chkrec
   end if

!     Print Out The Input Met Data Summary
   call prtmet(iounit)
   if (summfile) then
! ---    Include Met Data Summary in SUMMFILE
      call prtmet(isumunt)
   end if

   return
end subroutine inpsum

subroutine prtopt(iount)
!***********************************************************************
!                 PRTOPT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Model Options and Keyword Summary
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Updated the code to add the Aircraft plume rise
!                    debug option print
!                    Gavendra Pandey; UNC-IE, NC, Chapel Hill, USA
!                    04/01/2023
!
!        MODIFIED:   To summarize information about multiple buoyant lines
!                    (Multiple_BuoyLines_D41_Wood)
!
!        MODIFIED:   To include GRSM NO2 option.
!                    CERC, 11/30/20
!
!        MODIFIED:   Added summary information for a RLINE source,
!                    including Barrier/Depressed ALPHA options.
!                    Wood, 7/20/2018
!
!        MODIFIED:   To include note regarding special processing
!                    requirements for PM.25, 1hr NO2 and 1hr SO2.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   To include DFAULT urban roughness length,
!                    provide a more complete summary of options
!                    when DFAULT option is not specified, clarify
!                    options and emissions/output units related to
!                    deposition algorithms, address EVENT vs.
!                    non-EVENT processing options, and provide a
!                    more "refined" estimate of memory storage
!                    requirements in STORE.
!                    Include output file unit calling argument to
!                    support printing summary to main 'aermod.out'
!                    file and to the optional SUMMFILE.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To remove reference to "upper bound" values for
!                    supersquat buildings, and to summarize inputs for
!                    multiple urban areas.
!                    Roger Brode, MACTEC (f/k/a PES), Inc., - 08/25/05
!
!        MODIFIED:   To Remove Summary of Keywords Table
!                    Roger Brode, PES, Inc.,  - 11/08/94
!
!        MODIFIED:   To add pathway 'TG' to process input file of Gridded
!                    Terrain data.
!                    D. Strimaitis, SRC - 11/8/93
!
!        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
!                    to allow just the wet or just the dry deposition flux
!                    to be reported.  DEPOS now reports the sum of wet and
!                    dry fluxes.  Expand keywords to include input of wet
!                    scavenging coefficients (SO path).  Add override of
!                    Intermediate Terrain so that results are for only the
!                    simple terrain or the complex terrain model.
!                    D. Strimaitis, SRC - 11/8/93
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  Model Options and Keyword Summarys
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   use rline_data, only: nrlines, l_rdepress, l_rbarrier
   use buoyant_line

   implicit none
   character :: modnam*12

   integer :: nops

   integer :: i, ilmax, iount, NumBack,&
   &NumHrlySect,   NumNonHrlySect,&
   &NumHrlyO3Sect, NumNonHrlyO3Sect,&
   &NumHrlyNOxSect, NumNonHrlyNOxSect,&
   &ilen, ilen1, ilen2
!CRT  CRT, 3/18/2022 D063 Platform Downwash, number of platform sources
   integer :: NumPFSrcs
   character (len=80) :: BFLAG_String,  BFLAG_TempString,&
   &BGSECT_String, BGSECT_TempString
   character (len=80) :: O3FLAG_String, O3FLAG_TempString,&
   &O3SECT_String, O3SECT_TempString
! Unused:      CHARACTER (LEN=80) :: AWMADW_String, ORDDW_String
   character (len=10):: debug_opts(12)      ! Updated 12 from 11 for Aircraft; UNC-IE
   character (len=80):: DEBUG_OPTS_String
   character (len=30):: chraves
   character (len=80) :: NOXFLAG_String, NOXFLAG_TempString,&
   &NOXSECT_String, NOXSECT_TempString

!     Variable Initializations
   modnam = 'PRTOPT'
   BFLAG_String = ' '
   BFLAG_TempString = ' '
   BGSECT_String = ' '
   BGSECT_TempString = ' '
   O3FLAG_String = ' '
   O3FLAG_TempString = ' '
   O3SECT_String = ' '
   O3SECT_TempString = ' '
   NOXFLAG_String = ' '
   NOXFLAG_TempString = ' '
   NOXSECT_String = ' '
   NOXSECT_TempString = ' '
   debug_opts = ''
   chraves    = ''
   ilen  = 0
   ilen1 = 0
   ilen2 = 0
   nops = 0
   ilen = 0
!     Initialize counter for number of source groups with background
   NumBack = 0
!     Initialize counters for number of sectors with hourly and non-hourly background
   NumHrlySect = 0
   NumNonHrlySect = 0
   NumHrlyO3Sect = 0
   NumHrlyNOxSect = 0
   NumNonHrlyO3Sect = 0
   NumNonHrlyNOxSect = 0

!CRT  CRT, 3/18/2022 D063 Platform Downwash, initialize then get number of platform sources
   NumPFSrcs = 0

!     Get number of platform sources
   do i = 1, size(osplat)
      if (osplat(i)) then
         NumPFSrcs = NumPFSrcs + 1
      end if
   end do

!     Summarize The Model Options
   call header(iount)
   write(iount,9041)
!     How are options specified
   write (iount, *) '** Model Options Selected:'

   if (dfault) then
      write(iount,*) '     * Model Uses Regulatory DEFAULT Options'
   else
      write(iount,*) '     * Model Allows User-Specified Options'
   end if


!     Model Setup is for...


   if (conc) then
      write(iount,*) '     * Model Is Setup For Calculation of ',&
      &'Average CONCentration Values.'
   end if
   if (depos) then
      write(iount,*) '     * Model Is Setup For Calculation of ',&
      &'Total DEPOSition Values.'
   end if
   if (ddep) then
      write(iount,*) '     * Model Is Setup For Calculation of ',&
      &'Dry DEPosition Values.'
   end if
   if (wdep) then
      write(iount,*) '     * Model Is Setup For Calculation of ',&
      &'Wet DEPosition Values.'
   end if

!     Dry Depostion Logic

   if (ldgas .and. luservd) then
      write(iount,*) '     * User-specified GAS DRY ',&
      &' DEPOSITION Velocity Provided.'
   else if (ldgas .or. lwgas) then
      write(iount,*) '     * GAS DEPOSITION Data Provided.'
   else
      write(iount,*) '     * NO GAS DEPOSITION Data Provided.'
   end if

   if (ldpart) then
      write(iount,*) '     * PARTICLE DEPOSITION Data Provided.'
   else
      write(iount,*) '     * NO PARTICLE DEPOSITION Data Provided.'
   end if

   if (ddplete) then
      write(iount,*) '     * Model Uses DRY DEPLETION. ',&
      &'DDPLETE  = ',&
      &ddplete
      if (ardplete) then
         write(iount,*)&
         &'        with AREADPLT option    AREADPLT = ',&
         &ardplete
      else if (romberg) then
         write(iount,*)&
         &'        with ROMBERG option     ROMBERG  = ',&
         &romberg
      end if
   else
      write(iount,*) '     * Model Uses NO DRY DEPLETION. ',&
      &'DDPLETE  = ',&
      &ddplete
   end if
!     Wet Depostion Logic
   if (wdplete) then
      write(iount,*) '     * Model Uses WET DEPLETION. ',&
      &'WETDPLT  = ',&
      &wdplete
   else
      write(iount,*) '     * Model Uses NO WET DEPLETION. '       ,&
      &'WETDPLT  = ',&
      &wdplete
   end if

!     Stack Tip or Not
   if (.not. dfault) then
      if (nostd) then
         write(iount,*) '     * No Stack-tip Downwash'
      else
         write(iount,*) '     * Stack-tip Downwash.'
      end if
   else
      write(iount,*) '     * Stack-tip Downwash.'
   end if

!     Terrain Options
   if (dfault) then
      write(iount,*) '     * Model Accounts for ELEVated ',&
      &'Terrain Effects.'
   else
      if (flatsrcs) then
         write(iount,*) '     * Allow FLAT/ELEV ',&
         &'Terrain Option by Source,'
         write(iount,9049) numflat, numsrc-numflat
      else if (flat) then
         write(iount,*) '     * Model Assumes Receptors on ',&
         &'FLAT Terrain.'
      else if (elev) then
         write(iount,*) '     * Model Accounts for ELEVated ',&
         &'Terrain Effects.'
      end if
   end if

!      Calms Processing
   write(iount,*) '     * Use Calms Processing ',&
   &'Routine.'

!     Missing Data Processing
   write(iount,*) '     * Use Missing Data ',&
   &'Processing Routine.'

!     Exponential Decay (SO2)
   if (dfault) then
      if (urban .and. pollut == 'SO2') then
         write(iount,*) '     * Half-life of 4 hrs for',&
         &' URBAN SO2.'
      else
         write(iount,*) '     * No Exponential Decay.'
      end if
   end if
   if (.not.dfault) then
! ---    Check for Non-DFAULT Half-life for URBAN SO2 application
      if (urban .and. pollut=='SO2') then
         if( icstat(7) == 1 .or. icstat(8) == 1 )then
! ----           User-specified HALFLIFE or DCAYCOEF
            if (dabs(decoef-4.81d-5) <= 5.0d-8) then
               write(iount,*) '     * Half-life of 4 hrs for',&
               &' URBAN SO2.'
            else
               write(iount,*) '     * Non-DFAULT Half-life for ',&
               &' URBAN SO2.'
            end if
         else if (icstat(7) == 0 .and. icstat(8) == 0) then
            write(iount,*) '     * Half-life of 4 hrs for',&
            &' URBAN SO2.'
         end if
      else if (decoef /= 0.0d0) then
         write(iount,*) '     * Non-DFAULT Exponential',&
         &' Decay.'
      else
         write(iount,*) '     * No Exponential Decay.'
      end if
   end if

!     NO2 Conversion
   if (pollut == 'NO2') then
      if (pvmrm) then
         write(iount,*)&
         &'     * Plume Volume Molar Ratio Method (PVMRM)',&
         &'Used for NO2 Conversion'
         write(iount,9090) NO2Equil
9090     format('        with an Equilibrium NO2/NOx ',&
         &'Ratio of ',f6.3)
         write(iount,9091) NO2Stack
9091     format('        and with a Default In-stack ',&
         &'Ratio of ',f6.3)
         if (psdcredit) then
            write(iount,*)'     * ALPHA Option for  ',&
            &'Calculating PSD Increment'
            write(iount,*)'     * Consumption with PSD ',&
            &'Credits Selected.'
         end if
      else if (olm) then
         write(iount,*) '     * Ozone Limiting Method (OLM) ',&
         &'Used for NO2 Conversion'
         write(iount,90901) NO2Equil
90901    format('        with an Equilibrium NO2/NOx ',&
         &'Ratio of ',f6.3,' and')
         if (numolm > 0) then
            write(iount,90911) numolm
90911       format('        with ',i3,' OLMGROUP(s)')
         else
            write(iount,90912)
90912       format('        with NO OLMGROUPs')
         end if
      else if (runttrm .or. runttrm) then
         write(iount,*) '     * Travel Time Reaction Method',&
         &'Used for NO2 Conversion'
         write(iount,90901) NO2Equil
      else if (arm2) then
         write(iount,*) '     * Ambient Ratio ',&
         &'Method Ver 2 (ARM2) Used for NO2 Conversion'
         write(iount,9990) ARM2_Min
9990     format('         with a Minimum NO2/NOx ',&
         &'Ratio of ',f6.3)
         write(iount,9991) ARM2_Max
9991     format('         and a Maximum NO2/NOx ',&
         &'Ratio of ',f6.3)
      else if (grsm) then
!            CERC 11/30/20
         write(iount,*) '     * GRS Chemistry ',&
         &'Method (GRSM) Used for NO2 Conversion'
      else
         write(iount,*) '     * Full Conversion ',&
         &'Assumed for NO2.'
      end if
   end if

!     Zo and urban Alg
   if (.not.urban) then
      write(iount,*) '     * Model Uses RURAL Dispersion Only.'
   else if (urban) then
      write(iount,9039) nurbsrc, numurb
9039  format(1x,'     * Model Uses URBAN Dispersion Algorithm ',&
      &'for the SBL for ',i5,' Source(s),'&
      &/'        for Total of ',i4,' Urban Area(s):')
      if (numurb > 1) then
         do i = 1, numurb
            write(iount,9040) urbid(i), urbpop(i), urbz0(i)
9040        format(8x,'Urban ID = ',a8,' ;  Urban Population = ',&
            &f11.1,' ;  Urban Roughness Length = ',f6.3,' m')
         end do
      else
         write(iount,90401) urbpop(1), urbz0(1)
90401    format(3x,'Urban Population = ',&
         &f11.1,' ;  Urban Roughness Length = ',f6.3,' m')
      end if

      if (maxval(urbz0)/=1.0d0 .or.&
      &minval(urbz0)/=1.0d0) then
         write(iount,*) '     * Non-DFAULT Urban ',&
         &'Roughness Length(s) Used.'
      else
         write(iount,*) '     * Urban Roughness Length ' ,&
         &'of 1.0 Meter Used.'
      end if
      if (.not. L_UrbanTransition) then
         write(iount,90402)
90402    format(1x,'     * Non-DFAULT option to ignore morning ',&
         &'transition from nighttime urban boundary',&
         &/'        layer (NoUrbTran) selected.')
      end if
   end if

!     Capped Option Used
   if ( numcap>0 .or. numhor>0 ) then
      write(iount,*) '     * Option for Capped &',&
      &' Horiz Stacks Selected With:'
      write(iount,9092) numcap, numhor
9092  format(1x,i10,' Capped Stack(s); and',i10,&
      &' Horizontal Stack(s)')
   end if

!     Other Options
   if (nochkd) then
      write(iount,*) '     * NOCHKD   - Suppresses checking',&
      &' of date sequence in meteorology files'
   else if (l_warnchkd) then
      write(iount,*) '     * WARNCHKD - Issues warning messages',&
      &' for records out of sequence'
      write(iount,*) '       in meteorology files'
   end if

   if (nowarn) then
      write(iount,*) '     * NOWARN   - Suppresses writing',&
      &' of warning messages in main print file'
   end if
   if (fastall) then
      write(iount,*) '     * FASTALL  - Use effective sigma-y to',&
      &' optimize meander for '
      write(iount,*) ' POINT and VOLUME',&
      &' sources, and hybrid approach'
      write(iount,*) 'to optimize AREA sources',&
      &' (formerly TOXICS option)'
   else if (fastarea) then
      write(iount,*) '     * FASTAREA - Use hybrid approach to',&
      &' optimize AREA sources;'
      write(iount,*) '                   also applies to LINE ',&
      &'sources (formerly TOXICS option)'
   end if
   if (scim) then
      write(iount,*) '     * SCIM  - Use Sampled',&
      &' Chronological Input Model (SCIM) option'
   end if

   if (screen) then
      write(iount,*) '     * SCREEN   - Use screening option ',&
      &'which forces calculation of centerline values'
   end if

! Added for HBP, JAN 2023
   if (hbplume) then
      write(iount,*) '     * HBP - Non-DFAULT option to optimize',&
      &' for Highly Buoyant Plumes from'
      write(iount,7909) nhbp
7909  format(1x,i10,' Identified HBP POINT-type Source(s)')
   end if
! End HBP insert

!     3/18/22 Wood D127 - added FRANMIN to LOW_WIND option
!     4/12/2022 CRT D131 - added PBAL to LOW_WIND option
!     4/12/2022 CRT - add more summary info for user-specified options
   if (low_wind) then
      write(iount,*)'     * Use LOW_WIND ALPHA option'
      if (L_UserSVmin .or. L_UserWSmin .or. L_UserFRANmax .or.&
      &L_UserSWmin .or. L_UserBigT .or. L_UserFRANmin .or.&
      &L_PBal) then
         write(iount,*)&
         &'        with the following parameters:'
         if (L_UserSVmin) then
            write(iount,'(12x,"* SVMin =   ",F8.2," m/s")') SVMin
         end if
         if (L_UserWSmin) then
            write(iount,'(12x,"* WSMin =   ",F8.2," m/s")') WSMin
         end if
         if (L_UserFRANmin) then
            write(iount,'(12x,"* FRANMin = ",F8.2)') franmin
         end if
         if (L_UserSWmin) then
            write(iount,'(12x,"* SWMin =   ",F8.2," m/s")') SWMin
         end if
         if (L_UserBigT) then
            write(iount,'(12x,"* BigT =    ",F8.2," hours")') bigt
         end if
         if (L_UserFRANmax) then
            write(iount,'(12x,"* FRANMax = ",F8.2)') franmax
         end if
         if (L_PBal) then
            write(iount,'(12x,"* PBalance")')
         end if
      end if
   end if
   if (L_AdjUstar) then
      write(iount,*) '     * ADJ_U*   - Use ADJ_U* option ',&
      &'for SBL in AERMET'
   end if

   if (L_MMIF_Data) then
      write(iount,*) '     * MMIFData - Use MMIF met data inputs'
      if( len_trim(MMIF_Version)>0 )then
         write(iount,*) '                   ',MMIF_Version
      end if
   end if

!     CRT 8/9/2023 D176 COARE processing in AERMET
   if (l_coare) then
      write(iount,*) '     * COARE - Use COARE processing in AERMET'
   end if

   if (l_bulkrn) then
      if( L_MMIF_Data )then
         write(iount,*) '     * BULKRN   - Use BULKRN Delta-T and ',&
         &'SolarRad option for SBL with MMIF'
      else
         write(iount,*) '     * BULKRN   - Use BULKRN Delta-T and ',&
         &'SolarRad option for SBL in AERMET'
      end if
   end if

   if (l_vectorws) then
      write(iount,*) '     * VECTORWS - User specifies ',&
      &'that input wind speeds are VECTOR means'
   end if
   if (L_CCVR_Sub) then
      write(iount,*) '     * CCVR_Sub - Meteorological data ',&
      &'includes CCVR substitutions'
   end if

   if (L_TEMP_Sub) then
      write(iount,*) '     * TEMP_Sub - Meteorological data ',&
      &'includes TEMP substitutions'
   end if

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     ADD TEXT TO WRITE FOR TURBULENCE OPTIONS
   if (turbopts(1)) then
      write(iount,*) '     * NOTURB - Meteorological data ',&
      &'Ignore turbulence - all hours'
   end if
   if (turbopts(2)) then
      write(iount,*) '     * NOTURBST - Meteorological data ',&
      &'Ignore turbulence - stable hours'
   end if
   if (turbopts(3)) then
      write(iount,*) '     * NOTURBCO - Meteorological data ',&
      &'Ignore turbulence - convective hours'
   end if

   if (turbopts(4)) then
      write(iount,*) '     * NOSA - Meteorological data ',&
      &'Ignore sigma-theta - all hours'
   end if

   if (turbopts(5)) then
      write(iount,*) '     * NOSW - Meteorological data ',&
      &'Ignore sigma-w - all hours'
   end if

   if (turbopts(6)) then
      write(iount,*) '     * NOSAST - Meteorological data ',&
      &'Ignore sigma-theta - stable hours'
   end if

   if (turbopts(7)) then
      write(iount,*) '     * NOSWST - Meteorological data ',&
      &'Ignore sigma-w - stable hours'
   end if

   if (turbopts(8)) then
      write(iount,*) '     * NOSACO - Meteorological data ',&
      &'Ignore sigma-theta - convective hours'
   end if

   if (turbopts(9)) then
      write(iount,*) '     * NOSWCO - Meteorological data ',&
      &'Ignore sigma-w - convective hours'
   end if

   if (l_rdepress) then
      write(iount,*) '     * RDEPRESS - Use ALPHA option ',&
      &'for RLINEXT with depression'
   end if

   if (l_rbarrier) then
      write(iount,*) '     * RBARRIER - Use ALPHA option ',&
      &'for RLINEXT with barrier'
   end if
!
   if (flgpol) then
      write(iount,*) '     * Model Accepts FLAGPOLE Receptor .',&
      &' Heights. '
   else
      write(iount,*) '     * Model Assumes No FLAGPOLE Receptor ',&
      &'Heights. '
   end if

!     Write Out Pollutant Type
   write(iount,9048) pollut

! --- Include note regarding 24-hr PM2.5, 1-hr NO2 and 1-hr SO2 processing;
!     including a note regarding 1hr NO2/SO2 and 24hr PM25 NAAQS processing
!     being disabled by user, if applicable.
   if ((pollut == 'PM25' .or. pollut == 'PM-2.5' .or.&
   &pollut == 'PM-25'.or. pollut == 'PM2.5') .and.&
   &.not.evonly) then
      if (pm25ave) then
         write(iount,99090)
      else if (l_no_pm25ave) then
         write(iount,99190)
      end if
   else if (pollut == 'NO2' .and. .not.evonly) then
      if (no2ave) then
! ---       Special processing for 1-hr NO2 NAAQS
!           is applicable
         write(iount,99091)
      else if (l_no_no2ave) then
! ---       User has disabled special processing for
!           1-hr NO2 NAAQS processing
         write(iount,99191) no2_field4(1:3)
      else if (.not.no2ave .and. .not.l_no_no2ave) then
! ---       Special processing for 1-hr NO2 NAAQS is NOT
!           applicable due to non-standard averaging period(s)
         do i = 1, numave
            if (chrave(i) /= ' 1-HR') then
               ilen = len_trim(chraves)
               chraves = chraves(1:ilen)//'  '//chrave(i)
            end if
         end do
         write(iount,99291) chraves
      end if
   else if (pollut == 'SO2' .and. .not.evonly) then
      if (so2ave) then
! ---       Special processing for 1-hr SO2 NAAQS
!           is applicable
         write(iount,99092)
      else if (l_no_so2ave) then
! ---       User has disabled special processing for
!           1-hr SO2 NAAQS processing
         write(iount,99192) so2_field4(1:3)
      else if (.not.so2ave .and. .not.l_no_so2ave) then
! ---       Special processing for 1-hr SO2 NAAQS is NOT
!           applicable due to non-standard averaging period(s)
         do i = 1, numave
            if (chrave(i) /= ' 1-HR') then
               ilen = len_trim(chraves)
               chraves = chraves(1:ilen)//'  '//chrave(i)
            end if
         end do
         write(iount,99292) chraves
      end if
   end if

!     Modeled average period(s) summary
   write(iount,9099)
   if (period) then
      if (numave > 0) then
         write(iount,9042) numave, (chrave(i),i=1,numave)
         write(iount,9043)
      else
         write(iount,9045)
      end if
   else if (annual) then
      if (numave > 0) then
         write(iount,9042) numave, (chrave(i),i=1,numave)
         write(iount,9143)
      else
         write(iount,9145)
      end if
   else
      write(iount,9042) numave, (chrave(i),i=1,numave)
   end if

!     Write Out Numbers of Sources, Groups, and Receptors for This Run
   write(iount,9099)
   if (evonly) then
      write(iount,9046) numsrc, numgrp, numeve, numpnt, numcap,&
      &numhor, numvol, numarea, numline, nrlines,&
      &numpit, numblgrps, nbltotal, numswp
   else if (.not. evonly) then
      write(iount,9044) numsrc, numgrp, numrec, numpnt, numcap,&
      &numhor, numvol, numarea, numline, nrlines,&
      &numpit, numblgrps, nbltotal, numswp
   end if

!     CRT, 3/18/2022 D063 Write number of point, pointhor, pointcap
!     sources subject to platform downwash
   if ((numpnt > 0 .or. numcap > 0 .or. numhor > 0) .and.&
   &NumPFSrcs > 0) then
      write (iount, 9059) NumPFSrcs
9059  format(1x,'**Number of Platform Point Sources:', i6)
   end if


   if (l_awmadw) then
      write (iount, 9060)
9060  format(1x, '**AWMA Downwash Options Specified:')
      if (L_AWMA_Ueff) then
         write (iount, 9061)
9061     format(10x, 'AWMAUEFF')
      end if

      if (L_AWMA_UTurb .and. .not. L_AWMA_UTurbHX) then
         write (iount, 9062)
9062     format(10x, 'AWMAUTURB')
      end if

      if (L_AWMA_UTurbHX) then
         write (iount, 9064)
9064     format(10x, 'AWMAUTURBHX')
      end if
   end if

   if (l_strmln_bldg) then
      write (iount, 9063)
9063  format(10x, 'Streamlined Buildings')
   end if

   if (L_AWMA_Entrain) then
      write (iount, 9200)
9200  format(10x, 'AWMAENTRAIN')
   end if

   if (l_orddw) then
      write (iount, 9165)
9165  format(1x,'**ORD Downwash Options Specified:')
      if (L_ORD_Ueff) then
         write (iount, 9166)
9166     format(10x, 'ORDUEFF')
      end if
      if (L_ORD_Turb) then
         write (iount, 9167)
9167     format(10x, 'ORDTURB')
      end if

      if (L_ORD_Cav) then
         write (iount, 9168)
9168     format(10x, 'ORDCAV')
      end if
   end if

! --- Indicate whether background concentrations are included in this run
   if (l_backgrnd) then
!        Determine how many source groups include background
      NumBack = 0
      do i = 1, numgrp
         if (grp_back(i)) then
            NumBack = Numback + 1
         end if
      end do

      if (L_BGSector) then
! ---       BGSECTOR Option; First summarize how many SRCGRPs and how many Sectors
         write(BGSECT_String,'(6I4)')&
         &(nint(bgsect(i)),i=1,NUMBGSects)
         write(iount,19047) NumBack, NUMBGSects,&
         &BGSECT_String(1:len_trim(BGSECT_String))
19047    format(/1x,'**This Run Includes BACKGRND Values',&
         &' for ',i6,' Source Group(s) Varying Across ',i3,&
         &' Downwind Sector(s): ',a:)

! ---       Determine number of sectors with HOURLY BACKGRND
         BGSECT_String = '' ! MKP re-initialize
         BGSECT_TempString = '' ! MKP re-initialize
         do i = 1, NUMBGsects
            if (L_BGFile(i)) then
               write(BGSECT_TempString,'(A,I4)')& ! MKP D196, write to temp string, avoid write-to-self warning
               &BGSECT_String(1:len_trim(BGSECT_String)),&
               &nint(bgsect(i))
               BGSECT_String = BGSECT_TempString ! MKP D196, save each new sector value to summary string
               NumHrlySect = NumHrlySect + 1
            end if
         end do

! ---       Summarize how many sectors have HOURLY BACKGRND
         if (NumHrlySect > 0) then
            write(iount,19048) NumHrlySect,&
            &BGSECT_String(1:len_trim(BGSECT_String))
19048       format(1x,'             HOURLY BACKGRND Values',&
            &' are Available for ',i3,&
            &' Downwind Sector(s): ',a:)
         end if

! ---       Determine number of sectors with Non-HOURLY BACKGRND
         BGSECT_String = ''     ! re-initialize
         BGSECT_TempString = '' ! re-initialize
         do i = 1, NUMBGsects
            if (L_BGValues(i)) then
               write(BGSECT_TempString,'(A,I4)')& ! MKP D196, write to temp string, avoid write-to-self warning
               &BGSECT_String(1:len_trim(BGSECT_String)),&
               &nint(bgsect(i))
               BGSECT_String = BGSECT_TempString ! MKP D196, save each new sector value to summary string
               NumNonHrlySect = NumNonHrlySect + 1
            end if
         end do

! ---       Non-HOURLY BACKGRND concentrations available
!           First reinitialize BFLAG_String and BFLAG_TempString
         BFLAG_String = ''
         BFLAG_TempString = ''
         if (NumNonHrlySect > 0) then
            write(iount,19049) NumNonHrlySect,&
            &BGSECT_String(1:len_trim(BGSECT_String))
19049       format(1x,'         Non-HOURLY BACKGRND Values',&
            &' are Available for ',i3,&
            &' Downwind Sector(s): ',a:)
         end if

! ---       Summarize Non-HOURLY BACKGRND options for missing HOURLY
         do i = 1, NUMBGsects
            if (L_BGValues(i) .and. L_BGFile(i)) then
               ilen1 = len_trim(BFLAG_String)
               ilen2 = len_trim(bflag(i))
               write(BFLAG_TempString,'(A,1x,A)')&
               &BFLAG_String(1:ilen1),bflag(i)(1:ilen2)
               BFLAG_String = BFLAG_TempString
            end if
         end do
         if (len_trim(BFLAG_String) > 0) then
            write(iount,19050)&
            &BFLAG_String(1:len_trim(BFLAG_String))
19050       format(1x,'     Missing HOURLY BACKGRND Values ',&
            &'are Filled Based on Values Varying by: ',&
            &a:)
         end if

! ---       Summarize all Non-HOURLY BACKGRND options available
         BFLAG_String = ''
         BFLAG_TempString = ''
         do i = 1, NUMBGsects
            if (L_BGValues(i)) then
               ilen1 = len_trim(BFLAG_String)
               ilen2 = len_trim(bflag(i))
               write(BFLAG_TempString,'(A,1x,A)')&
               &BFLAG_String(1:ilen1),bflag(i)(1:ilen2)
               BFLAG_String = BFLAG_TempString
            end if
         end do
         if (len_trim(BFLAG_String) > 0) then
            write(iount,19051) BFLAG_String(1:len_trim(BFLAG_String))
19051       format(1x,'         Non-HOURLY BACKGRND Values',&
            &' are Available Varying by: ',a:)
         end if

      else
! ---       No BGSECTORs

! ---       First indicate how many source groups include BACKGRND
         write(iount,29047) NumBack
29047    format(/1x,'**This Run Includes BACKGRND Values ',&
         &'for ',i6,' Source Group(s) for a Single Sector')

! ---       Next check for HOURLY or non-hourly BACKGRND values
         if (L_BGFile(1)) then
            write(iount,29048)
29048       format(1x,'             HOURLY BACKGRND Values',&
            &' are Available')
            if (len_trim(bflag(1)) > 0) then
               write(iount,19050) bflag(1)(1:len_trim(bflag(1)))
            end if
         else
            if (len_trim(bflag(1)) > 0) then
               write(iount,19051) bflag(1)(1:len_trim(bflag(1)))
            end if
         end if
      end if

   end if

   if (olm .or. pvmrm .or. grsm) then
! ---    Summarize OZONE data inputs, includeing O3SECTOR options
! ---    First summarize how many Sectors
      if (L_O3Sector) then
         write(O3SECT_String,'(6I4)')&
         &(nint(o3sect(i)),i=1,NUMO3Sects)
         write(iount,19057) NUMO3Sects,&
         &O3SECT_String(1:len_trim(O3SECT_String))
19057    format(/1x,'**This Run Includes OZONE    Values',&
         &' that Vary Across  ',i3,' Downwind Sector(s): ',a:)
! ---       Determine number of sectors with HOURLY OZONE
         O3SECT_String = ''
         O3SECT_TempString = ''
         do i = 1, NUMO3sects
            if (L_O3File(i)) then
               write(O3SECT_TempString,'(A,I4)')& ! MKP D196, write to temp string, avoid write-to-self warning
               &O3SECT_String(1:len_trim(O3SECT_String)),&
               &nint(o3sect(i))
               O3SECT_String = O3SECT_TempString ! MKP D196, save each new sector value to summary string
               NumHrlyO3Sect = NumHrlyO3Sect + 1
            end if
         end do

! ---       Summarize how many sectors have HOURLY OZONE
         if (NumHrlyO3Sect > 0) then
            write(iount,19058) NumHrlyO3Sect,&
            &O3SECT_String(1:len_trim(O3SECT_String))
19058       format(1x,'             HOURLY OZONE    Values',&
            &' are Available for ',i3,&
            &' Downwind Sector(s): ',a:)
         end if

! ---       Determine number of sectors with Non-HOURLY OZONE
         O3SECT_String = ''
         O3SECT_TempString = ''
         do i = 1, NUMO3sects
            if (io3set(i) > 0 .or. l_o3val(i)) then
               write(O3SECT_TempString,'(A,I4)')& ! MKP D196, write to temp string, avoid write-to-self warning
               &O3SECT_String(1:len_trim(O3SECT_String)),&
               &nint(o3sect(i))
               O3SECT_String = O3SECT_TempString ! MKP D196, save each new sector value to summary string
               NumNonHrlyO3Sect = NumNonHrlyO3Sect + 1
            end if
         end do

! ---       Non-HOURLY OZONE concentrations available
!           First reinitialize O3FLAG_String and O3FLAG_TempString
         O3FLAG_String = ''
         O3FLAG_TempString = ''
         if (NumNonHrlyO3Sect > 0) then
            write(iount,19059) NumNonHrlyO3Sect,&
            &O3SECT_String(1:len_trim(O3SECT_String))
19059       format(1x,'         Non-HOURLY OZONE    Values',&
            &' are Available for ',i3,&
            &' Downwind Sector(s): ',a:)
         end if

! ---       Summarize Non-HOURLY OZONE options for missing HOURLY O3
         do i = 1, NUMO3sects
            if ((io3set(i)>0 .or. l_o3val(i)) .and.&
            &L_O3File(i)) then
               if (io3set(i) > 0) then
                  ilen1 = len_trim(O3FLAG_String)
                  ilen2 = len_trim(o3flag(i))
                  write(O3FLAG_TempString,'(A,1x,A)')&
                  &O3FLAG_String(1:ilen1),o3flag(i)(1:ilen2)
               else if (l_o3val(i)) then
                  ilen1 = len_trim(O3FLAG_String)
                  ilen2 = len_trim('ANNUAL')
                  write(O3FLAG_TempString,'(A,1x,A)')&
                  &O3FLAG_String(1:ilen1),'ANNUAL'
               end if
               O3FLAG_String = O3FLAG_TempString
            end if
         end do
         if (len_trim(O3FLAG_String) > 0) then
            write(iount,19060)&
            &O3FLAG_String(1:len_trim(O3FLAG_String))
19060       format(1x,'     Missing HOURLY OZONE    Values ',&
            &'are Filled Based on Values Varying by: ', a:)
         end if

! ---       O3FLAG_String all Non-HOURLY OZONE options available
         O3FLAG_String = ''
         O3FLAG_TempString = ''
         do i = 1, NUMO3sects
            if (io3set(i) > 0 .or. l_o3val(i)) then
               if (io3set(i) > 0) then
                  ilen1 = len_trim(O3FLAG_String)
                  ilen2 = len_trim(o3flag(i))
                  write(O3FLAG_TempString,'(A,1x,A)')&
                  &O3FLAG_String(1:ilen1),o3flag(i)(1:ilen2)
               else if (l_o3val(i)) then
                  ilen1 = len_trim(O3FLAG_String)
                  ilen2 = len_trim('ANNUAL')
                  write(O3FLAG_TempString,'(A,1x,A)')&
                  &O3FLAG_String(1:ilen1),'ANNUAL'
               end if
               O3FLAG_String = O3FLAG_TempString
            end if
         end do
         if (len_trim(O3FLAG_String) > 0) then
            write(iount,19061)&
            &O3FLAG_String(1:len_trim(O3FLAG_String))
19061       format(1x,'         Non-HOURLY OZONE    Values',&
            &' are Available Varying by: ',a:)
         end if

      else
! ---       No O3SECTORs

         write(iount,29057)
29057    format(/1x,'**This Run Includes OZONE    Values ',&
         &'for a Single Sector')

! ---       Next check for HOURLY or non-hourly OZONE values
         if (L_O3File(1)) then
            NumHrlyO3Sect = NumHrlyO3Sect + 1
         end if
         if (NumHrlyO3Sect >= 1) then
            write(iount,29058)
29058       format(1x,'             HOURLY OZONE    Values ',&
            &'are Available')
            if (len_trim(o3flag(1)) > 0) then
               write(iount,19060) o3flag(1)(1:len_trim(o3flag(1)))
            end if
         else
            if (len_trim(o3flag(1)) > 0) then
               write(iount,19061) o3flag(1)(1:len_trim(o3flag(1)))
            end if
         end if
      end if
   end if

!     CERC 11/30/20
   if (grsm) then
! ---    Summarize NOx data inputs, including NOXSECTR options
! ---    First summarize how many Sectors
      if (L_NOxSector) then
         write(NOXSECT_String,'(6I4)')&
         &(nint(noxsect(i)),i=1,NUMNOxSects)
         write(iount,19062) NUMNOXSects,&
         &NOXSECT_String(1:len_trim(NOXSECT_String))
19062    format(/1x,'**This Run Includes NOx      Values',&
         &' that Vary Across  ',i3,' Downwind Sector(s): ',a:)

! ---       Determine number of sectors with HOURLY NOX
         NOXSECT_String= ' '
         NOXSECT_TempString= ' '
         do i =1,NUMNOxSects
            if(L_NOxFile(i))then
               write(NOXSECT_TempString,'(A,I4)')& ! MKP D196, write to temp string, avoid write-to-self warning
               &NOXSECT_String(1:len_trim(NOXSECT_String)),&
               &nint(noxsect(i))
               NOXSECT_String = NOXSECT_TempString ! MKP D196, save each new sector value to summary string
               NumHrlyNOXSect = NumHrlyNOxSect + 1
            end if
         end do

! ---      Summarize how many sectors have HOURLY NOX
         if(NumHrlyNOxSect > 0)then
            write(iount,19065) NumHrlyNOxSect,&
            &NOXSECT_String(1:len_trim(NOXSECT_String))
19065       format(1x,'             HOURLY NOX      Values',&
            &' are Available for ',i3,&
            &' Downwind Sector(s): ',a:)
         end if

! ---       Determine number of sectors with Non-HOURLY NOx
         NOXSECT_String = ''
         NOXSECT_TempString = ''
         do i = 1, NUMNOxSects
            if (inoxset(i) > 0 .or. l_noxvalue(i)) then
               write(NOXSECT_TempString,'(A,I4)')& ! MKP D196, write to temp string, avoid write-to-self warning
               &NOXSECT_String(1:len_trim(NOXSECT_String)),&
               &nint(noxsect(i))
               NOXSECT_String = NOXSECT_TempString ! MKP D196, save each new sector value to summary string
               NumNonHrlyNOXSect = NumNonHrlyNOXSect + 1
            end if
         end do
! ---       Non-HOURLY NOx concentrations available
!           First reinitialize NOxFLAG_String and NOxFLAG_TempString
         NOXFLAG_String = ''
         NOXFLAG_TempString = ''
         if (NumNonHrlyNOxSect > 0) then
            write(iount,19063) NumNonHrlyNOxSect,&
            &NOxSECT_String(1:len_trim(NOxSECT_String))
19063       format(1x,'         Non-HOURLY NOx      Values',&
            &' are Available for ',i3,&
            &' Downwind Sector(s): ',a:)
         end if

! ---       Summarize Non-Hourly NOX options for missing HOURLY NOx
         do i = 1, NumNOxsects
            if ((inoxset(i)>0 .or. l_noxvalue(i)) .and.&
            &L_NOxFile(i)) then
               if(inoxset(i)>0)then
                  ilen1=len_trim(NOXFLAG_String)
                  ilen2=len_trim(noxflag(i))
                  write(NOXFLAG_TempString,'(A,1x,A)')&
                  &NOXFLAG_String(1:ilen1),noxflag(i)(1:ilen2)
               else if (l_noxvalue(i))then
                  ilen1=len_trim(NOXFLAG_String)
                  ilen2=len_trim('ANNUAL')
                  write(NOXFLAG_TempString,'(A,1X,A)')&
                  &NOXFLAG_String(1:ilen1),'ANNUAL'
               end if
               NOXFLAG_String=NOXFLAG_TempString
            end if
         end do
         if(len_trim(NOXFLAG_String)>0)then
            write(iount,19066)&
            &NOXFLAG_String(1:len_trim(NOXFLAG_String))
19066       format(1x,'     Missing HOURLY NOX      Values ',&
            &'are Filled Based on Values Varying by: ', a:)
         end if

! ---       NOxFLAG_String all Non-HOURLY NOx options available
         NOxFLAG_String = ''
         NOxFLAG_TempString = ''
         do i = 1, NUMNOxSects
            if (INOxSET(i) > 0 .or. l_noxvalue(i)) then
               if (INOxSET(i) > 0) then
                  ilen1 = len_trim(NOxFLAG_String)
                  ilen2 = len_trim(NOxFLAG(i))
                  write(NOxFLAG_TempString,'(A,1x,A)')&
                  &NOxFLAG_String(1:ilen1),NOxFLAG(i)(1:ilen2)
               else if (l_noxvalue(i)) then
                  ilen1 = len_trim(NOxFLAG_String)
                  ilen2 = len_trim('ANNUAL')
                  write(NOxFLAG_TempString,'(A,1x,A)')&
                  &NOxFLAG_String(1:ilen1),'ANNUAL'
               end if
               NOxFLAG_String = NOxFLAG_TempString
            end if
         end do
         if (len_trim(NOxFLAG_String) > 0) then
            write(iount,19064)&
            &NOxFLAG_String(1:len_trim(NOxFLAG_String))
19064       format(1x,'         Non-HOURLY NOx      Values',&
            &' are Available Varying by: ',a:)
         end if
      else
! ---       No NOxSECTORs
         write(iount,29065)
29065    format(/1x,'**This Run Includes NOx      Values ',&
         &'for a Single Sector')
! ---       Next check for HOURLY or non-hourly NOX values
         if(L_NOxFile(1))then
            NumHrlyNOxSect = NumHrlyNOxSect + 1
         end if
         if(NumHrlyNOxSect >= 1)then
            write(iount,29066)
29066       format(1x,'             HOURLY NOX      Values ',&
            &'are Available')
            if (len_trim(NOxFLAG(1)) > 0) then
               write(iount,19066) noxflag(1)(1:len_trim(NOxFLAG(1)))
            end if
         else
            if (len_trim(NOxFLAG(1)) > 0) then
               write(iount,19064) noxflag(1)(1:len_trim(NOxFLAG(1)))
            end if
         end if
      end if
   end if

!     Model Run OR Not Options
   write(iount,9099)
   if (run) then
      write(iount,*) '**Model Set To Continue RUNning After the ',&
      &'Setup Testing.'
   else
      write(iount,*) '**Model Will NOT Run After the ',&
      &'Setup Testing.'
   end if

! --- Write the AERMET version date from the surface file header record
   write(iount,99093) c_metver

   write(iount,9099)
!     Model Output Options Setting Summary
   write(iount,9070)
   if (evonly) then
!        Write output option for EVENT processing
      if (socont) then
         write(iount,98071)
      else if (detail) then
         write(iount,98072)
      end if
   else
!        Write output options for non-EVENT processing
      if (period) then
!           PERIOD Averages by Receptor Are Output
         write(iount,9071)
      else if (annual) then
!           ANNUAL Averages by Receptor Are Output
         write(iount,9171)
      end if
      if (iostat(2) > 0) then
!           RECTABLE Keyword Used
         write(iount,9072)
      end if
      if (iostat(3) > 0) then
!           MAXTABLE Keyword Used
         write(iount,9073)
      end if
      if (iostat(4) > 0) then
!           DAYTABLE Keyword Used
         write(iount,9074)
      end if
      if (iostat(5) > 0) then
!           MAXIFILE Keyword Used
         write(iount,9075)
      end if
      if (iostat(6) > 0) then
!           POSTFILE Keyword Used
         write(iount,9076)
      end if
      if (iostat(7) > 0) then
!           PLOTFILE Keyword Used
         write(iount,9077)
      end if
      if (iostat(8) > 0) then
!           TOXXFILE Keyword Used
         write(iount,9078)
      end if
      if (iostat(9) > 0) then
!           SEASONHR Keyword Used
         write(iount,99071)
      end if
      if (iostat(10) > 0) then
!           RANKFILE Keyword Used
         write(iount,99072)
      end if
      if (iostat(11) > 0) then
!           EVALFILE Keyword Used
         write(iount,99073)
      end if
      if (iostat(12) > 0) then
!           SUMMFILE Keyword Used
         write(iount,99074)
      end if
      if (iostat(14) > 0) then
!           MAXDAILY Keyword Used
         write(iount,99173)
      end if
      if (iostat(15) > 0) then
!           MXDYBYYR Keyword Used
         write(iount,99273)
      end if
      if (iostat(16) > 0) then
!           MAXDCONT Keyword Used
         write(iount,99373)
      end if
   end if

! --- Check for user-specified option for exponential-format outputs
   if ( file_format == 'EXP' .and.&
   &(mxfile .or. ppfile .or. rkfile .or. anpost .or. anplot .or.&
   &seasonhr) ) then
      write(iount,9099)
      write(iount,99075)
   end if

!     Write Explanatory Note About Calm and Missing Flags
   if (clmpro .or. msgpro) then
      write(iount,9099)
      write(iount,9079) chidep(3,1)
   end if

!     Model Misc. Information
   write(iount,9099)
   write(iount,9050) zbase, decoef, rotang
! --- Write out emission and output units
   if (numtyp == 1) then
! ---    Only one output type; write out units without label
      write(iount,9055) emilbl(1), emifac(1), outlbl(1)
   else if (conc) then
! ---    More than one output type including CONC;
!        Write out CONC units first with label, followed by
!        deposition units (for DEPOS, DDEP and/or WDEP)
      write(iount,90551) emilbl(1), emifac(1), outlbl(1)
      write(iount,90552) emilbl(2), emifac(2), outlbl(2)
   else
! ---    More than one output type but no CONC;
!        Write out units for deposition without label
      write(iount,9055) emilbl(1), emifac(1), outlbl(1)
   end if

   if (luservd) then
!        Write user-specified gas dry deposition velocity (GASDEPVD)
      write(iount,9099)
      write(iount,9056)  uservd
   end if

   if (.not. evonly) then
!        Write Allocated Storage Requirements (est.)
      write(iount,9099)
      write(iount,9057) store
   end if

!     Model I/O Setting Summary
   write(iount,9099)
   ilmax = min( 96, ilen_fld )
   if (inpfil /= ' ' .or. outfil /= ' ') then
      write(iount,9080) inpfil(1:ilmax), outfil(1:ilmax)
   end if
   if (rstinp .and. .not.multyr) then
      write(iount,9081) inifil(1:ilmax)
   else if (rstinp .and. multyr) then
      write(iount,99081) inifil(1:ilmax)
   end if
! --- Summarize DEBUGOPTs selected by the user
!CRT  D063 Platform Downwash Debug PLATFMDBG
   if( debug .or. meteordbg .or. areadbg .or. primedbg .or. pvmrmdbg&
   &.or. olmdebug .or. arm2debug .or. deposdbg .or. ttrmdbg .or.&
   &grsmdebug .or. platfmdbg .or.&
   &arcftdebug )then       ! Added for Aircraft; UNC-IE

! ---       Create a character string that includes only those modeling
!           options (MODOPS) that are applicable for this model run
      DEBUG_OPTS_String = ''
      nops = 0

      if( debug )then
         debug_opts(1) = 'DEBUG'
      endif
      if( meteordbg )then
         debug_opts(2) = 'METEOR'
      endif
      if( areadbg )then
         debug_opts(3) = 'AREADBG'
      endif
      if( primedbg )then
         debug_opts(4) = 'PRIMEDBG'
      endif
      if( pvmrmdbg )then
         debug_opts(5) = 'PVMRMDBG'
      endif
      if( olmdebug )then
         debug_opts(6) = 'OLMDEBUG'
      endif
      if( arm2debug )then
         debug_opts(8) = 'ARM2DEBUG'
      endif
      if( deposdbg )then
         debug_opts(9) = 'DEPOSDBG'
      endif
      if( grsmdebug )then
         debug_opts(10) = 'GRSMDEBUG'
      endif
!CRT     3/24/2021, D076 TTRM NO2
!CRT     update for TTRM debug opt display in .out and .sum
      if( ttrmdbg )then
         debug_opts(11) = 'TTRMDEBUG'
      endif

!** Added for Aircraft Plume Rise; UNC-IE
      if( arcftdebug )then
         debug_opts(12) = 'AIRCRAFT'
      endif
!** End Aircraft Plume Rise insert; April 2023

! ---    Alpha option AWMADW debug specified
      if( awmadwdbg )then
         debug_opts(7) = 'AWMADWDBG'
      endif

!CRT     D063 Platform Downwash Debug
! ---    Alpha option PLATFORM debug specified
      if( platfmdbg )then
         debug_opts(10) = 'PLATFMDBG'
      endif

!CCRT     3/24/2021, D076 TTRM NO2
!CCRT     update for TTRM debug opt display in .out and .sum
!C ---    Loop through the 11 different options that are flagged
!         DO I = 1, 11
!            IF (LEN_TRIM(DEBUG_OPTS(I)) .GT. 0) THEN
!               NOPS = NOPS + 1
!               ILEN = 10*(NOPS-1)
!               DEBUG_OPTS_String =
!     &         DEBUG_OPTS_String(1:ILEN)//' '//DEBUG_OPTS(I)
!            END IF
!         END DO

!** Updated for Aircraft Plume Rise; UNC-IE
!     update for Aircraft debug opt display in .out and .sum
! ---    Loop through the 12 different options that are flagged
      do i = 1, 12
         if (len_trim(debug_opts(i)) > 0) then
            nops = nops + 1
            ilen = 10*(nops-1)
            DEBUG_OPTS_String =&
            &DEBUG_OPTS_String(1:ilen)//' '//debug_opts(i)
         end if
      end do
!** End Aircraft Plume Rise insert; April 2023

! ---    Write DEBUG Option String to main output unit
      write(iount,99100)&
      &DEBUG_OPTS_String(1:len_trim(DEBUG_OPTS_String))
      write(iount,*)
   endif

   if (rstsav) write(iount,9082) savfil(1:ilmax)
   if (errlst) write(iount,9083) msgfil(1:ilmax)
   if (events) write(iount,9084) evfile(1:ilmax)
   if (summfile) write(iount,9085) sumfil(1:ilmax)

   if (multyr) then
! ---    Write message regarding MULTYEAR applications
      write(iount,9099)
      write(iount,*) '**This Run is Part of a Multi-year (MULTYEAR)',&
      &' Application.'
      if (period) then
         write(iount,*) '  NOTE:  The PERIOD Results Table Reflects',&
         &' Current Period Only;'
         write(iount,*) '         The Overall Maximum PERIOD',&
         &' Results Table and'
      else if (annual) then
         write(iount,*) '  NOTE:  Both ANNUAL Average Results and'
      else
         write(iount,*) '  NOTE:'
      end if
      write(iount,*) '         Short Term Results are Cumulative',&
      &' Across All Years Processed.'
   end if

9041 format(/44x,'***     MODEL SETUP OPTIONS SUMMARY       ***'/&
   &63(' -')/)
9042 format(1x,'**Model Calculates ',i2,' Short Term Average(s)',&
   &' of:  ',9(a5,2x,:))
9043 format(1x,'    and Calculates PERIOD Averages')
9045 format(1x,'**Model Calculates PERIOD Averages Only')
9143 format(1x,'    and Calculates ANNUAL Averages')
9145 format(1x,'**Model Calculates ANNUAL Averages Only')

9044 format(1x,'**This Run Includes: ',i6,' Source(s);  ',i6,&
   &' Source Group(s); and  ',i6,' Receptor(s)',//&
   &16x,'with: ',i6,' POINT(s), including'/&
   &22x,i6,' POINTCAP(s) and ',i6,' POINTHOR(s)'/&
   &16x,' and: ',i6,' VOLUME source(s)',/,&
   &16x,' and: ',i6,' AREA type source(s)'/,&
   &16x,' and: ',i6,' LINE source(s)',/,&
   &16x,' and: ',i6,' RLINE/RLINEXT source(s)'/,&
   &16x,' and: ',i6,' OPENPIT source(s)',/,&
   &16x,' and: ',i6,' BUOYANT LINE source(s) with a total of',&
   &i6,' line(s)',/,&
   &16x,' and: ',i6,' SWPOINT source(s)',/)

9046 format(1x,'**This Run is for EVENT Processing.',&
   &/1x,'**         and Includes: ',i6,' Source(s);  ',i6,&
   &' Source Group(s); and  ',i6,' Event(s)',//&
   &16x,'with: ',i6,' POINT(s), including'/&
   &22x,i6,' POINTCAP(s) and ',i6,' POINTHOR(s)'/&
   &16x,' and: ',i6,' VOLUME source(s)',/,&
   &16x,' and: ',i6,' AREA type source(s)'/,&
   &16x,' and: ',i6,' LINE source(s)',/,&
   &16x,' and: ',i6,' RLINE/RLINEXT source(s)'/,&
   &16x,' and: ',i6,' OPENPIT source(s)',/,&
   &16x,' and: ',i6,' BUOYANT LINE source(s) with a total of',&
   &i6,' line(s)',/,&
   &16x,' and: ',i6,' SWPOINT source(s)',/)

! Unused: 99048 FORMAT(21X,'BACKGRND Concentrations Varying by Direction Across',
!     &       I6,' Sectors')
9048 format(1x,'     * The User Specified a Pollutant Type of: ',a8)
9049 format(8x,'with ',i6,' FLAT and ',i6,' ELEV Source(s).')
9050 format(1x,'**Misc. Inputs:  Base Elev. for Pot. Temp. Profile ',&
   &'(m MSL) = ',f8.2,' ;  Decay Coef. = ',g12.4,' ;',&
   &'  Rot. Angle = ',f7.1)
9055 format(18x,'Emission Units = ',a40,' ;  Emission Rate Unit ',&
   &'Factor = ',g13.5,&
   &/18x,'Output Units   = ',a40)
90551 format(2x,'Concentration:',2x,'Emission Units = ',a40,' ;  ',&
   &'Emission Rate Unit Factor = ',g13.5,&
   &/18x,'Output Units   = ',a40)
90552 format(5x,'Deposition:',2x,'Emission Units = ',a40,' ;  ',&
   &'Emission Rate Unit Factor = ',g13.5,&
   &/18x,'Output Units   = ',a40)
9056 format(18x,'User-Specified Dry Deposition Velocity for Gases ',&
   &'(m/s) = ',g13.5)
9057 format(1x,'**Approximate Storage Requirements of Model = ',f8.1,&
   &' MB of RAM.')
9070 format(1x,'**Output Options Selected:')
98071 format(10x,'Model Outputs Source Contribution Information Only ',&
   &'for EVENT Processing (SOCONT Option)')
98072 format(10x,'Model Outputs Hourly Average Values for Each Source ',&
   &'for EVENT Processing (DETAIL Option)')
9071 format(10x,'Model Outputs Tables of PERIOD Averages by Receptor')
9171 format(10x,'Model Outputs Tables of ANNUAL Averages by Receptor')
9072 format(10x,'Model Outputs Tables of Highest Short Term Values by',&
   &' Receptor (RECTABLE Keyword)')
9073 format(10x,'Model Outputs Tables of Overall Maximum Short Term',&
   &' Values (MAXTABLE Keyword)')
9074 format(10x,'Model Outputs Tables of Concurrent Short Term Values',&
   &' by Receptor for Each Day Processed (DAYTABLE Keyword)')
9075 format(10x,'Model Outputs External File(s) of Threshold',&
   &' Violations (MAXIFILE Keyword)')
9076 format(10x,'Model Outputs External File(s) of Concurrent Values',&
   &' for Postprocessing (POSTFILE Keyword)')
9077 format(10x,'Model Outputs External File(s) of High Values for',&
   &' Plotting (PLOTFILE Keyword)')
9078 format(10x,'Model Outputs External File(s) of Values for Input',&
   &' to TOXX Model (TOXXFILE Keyword)')
99071 format(10x,'Model Outputs External File(s) of Values by Season',&
   &' and Hour-of-Day (SEASONHR Keyword)')
99072 format(10x,'Model Outputs External File(s) of Ranked Values',&
   &' (RANKFILE Keyword)')
99073 format(10x,'Model Outputs External File(s) of Arc-maximum Values',&
   &' for Evaluation Purposes (EVALFILE Keyword)')
99173 format(10x,'Model Outputs External File(s) of Maximum Daily 1-hr',&
   &' Values by Day (MAXDAILY Keyword)')
99273 format(10x,'Model Outputs External File(s) of Maximum Daily 1-hr',&
   &' Values by Year (MXDYBYYR Keyword)')
99373 format(10x,'Model Outputs External File(s) of Contributions',&
   &' to Maximum Daily Values Paired in Time & Space',&
   &' (MAXDCONT Keyword)')
99074 format(10x,'Model Outputs Separate Summary File of High Ranked',&
   &' Values (SUMMFILE Keyword)')
99075 format(10x,'NOTE: Option for EXPonential format used in',&
   &' formatted output result files (FILEFORM Keyword)')
9079 format(1x,'**NOTE:  The Following Flags May Appear Following ',&
   &a4,' Values:  c for Calm Hours',&
   &/65x,'m for Missing Hours',&
   &/65x,'b for Both Calm and Missing Hours')
9080 format(1x,'**Input Runstream File:          ',a96,&
   &/1x,'**Output Print File:             ',a96/)
9081 format(1x,'**NOTE: This Run was Restarted (INITFILE Keyword):',&
   &/1x,'**File for Initializing Arrays:  ',a96)
99081 format(1x,'**NOTE: This Run was Restarted (MULTYEAR Keyword):',&
   &/1x,'**File for Initializing Arrays:  ',a96)
9082 format(1x,'**File for Saving Result Arrays: ',a96)
9083 format(1x,'**Detailed Error/Message File:   ',a96)

99100 format(1x,'**Debug Options Selected:',8x,a:)

9084 format(1x,'**File Created for Event Model:  ',a96)
9085 format(1x,'**File for Summary of Results:   ',a96)

99090 format(/1x,'**Note that special processing requirements apply',&
   &' for the 24-hour PM2.5 NAAQS - check available guidance.'/,&
   &'   Model will process user-specified ranks of high 24-hour',&
   &' values averaged across the number of years modeled, and'/,&
   &'   the multi-year average of individual ANNUAL values,',&
   &' averaged across the number of years modeled.')
99190 format(/1x,'**NOTE: Special processing requirements applicable',&
   &' for the 24-hour PM2.5 NAAQS have been disabled!!!'/,&
   &'         High ranked 24-hour values are NOT averaged across',&
   &' the number of years modeled, and'/,&
   &'         complete years of data are NOT required.')
99091 format(/1x,'**Note that special processing requirements apply',&
   &' for the 1-hour NO2 NAAQS - check available guidance.'/,&
   &'   Model will process user-specified ranks of daily maximum',&
   &' 1-hour values averaged across the number of years modeled.'/,&
   &'   For annual NO2 NAAQS modeling, the multi-year maximum of',&
   &' PERIOD values can be simulated using the MULTYEAR keyword.'/,&
   &'   Multi-year PERIOD and 1-hour values should only be done',&
   &' in a single model run using the MULTYEAR option with a'/,&
   &'   single multi-year meteorological data file using STARTEND',&
   &' keyword.')
99191 format(/1x,'**NOTE: Special processing requirements applicable',&
   &' for the 1-hour NO2 NAAQS have been disabled!!!'/,&
   &'         User has specified ',a3,' on the POLLUTID keyword.'/,&
   &'         High ranked 1-hour values are NOT averaged across',&
   &' the number of years modeled, and'/,&
   &'         complete years of data are NOT required.')
99291 format(/1x,'**NOTE: Special processing requirements applicable',&
   &' for the 1-hour NO2 NAAQS have been disabled!!!'/,&
   &'         User has specified non-standard averaging periods: ',&
   &a/,&
   &'         High ranked 1-hour values are NOT averaged across',&
   &' the number of years modeled, and'/,&
   &'         complete years of data are NOT required.')
99092 format(/1x,'**Note that special processing requirements apply',&
   &' for the 1-hour SO2 NAAQS - check available guidance.'/,&
   &'   Model will process user-specified ranks of daily maximum',&
   &' 1-hour values averaged across the number of years modeled.')
99192 format(/1x,'**NOTE: Special processing requirements applicable',&
   &' for the 1-hour SO2 NAAQS have been disabled!!!'/,&
   &'         User has specified ',a3,' on the POLLUTID keyword.'/,&
   &'         High ranked 1-hour values are NOT averaged across',&
   &' the number of years modeled, and'/,&
   &'         complete years of data are NOT required.')
99292 format(/1x,'**NOTE: Special processing requirements applicable',&
   &' for the 1-hour SO2 NAAQS have been disabled!!!'/,&
   &'         User has specified non-standard averaging periods: ',&
   &a/,&
   &'         High ranked 1-hour values are NOT averaged across',&
   &' the number of years modeled, and'/,&
   &'         complete years of data are NOT required.')
99093 format(/1x,'**The AERMET Input Meteorological Data Version Date:',&
   &1x,a6)


9099 format(1x,' ')

   return
end subroutine prtopt

subroutine prto3vals(isect)
!***********************************************************************
!                 PRTO3VALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The O3VALUES Input Data Summary
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED: Corrected code to use the O3FLAG variable which
!                  identifies the user-specified option for defining
!                  temporally-varying background ozone concentrations.
!                  The previous version erroneously referenced the
!                  BFLAG variable associated with user-specified
!                  BACKGROUND concentrations.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 12/19/2011
!
!        INPUTS:  Model Options and Keyword Summarys
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i1, i2, i3, ifr, idw
   integer :: isect
   character :: season(4)*6, months(12)*9, dayofweek(3)*8,&
   &dayofweek7(7)*8
! Unused:       INTEGER :: I, J, K, NL, INDC, INGRP
! Unused:       CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
! Unused:       CHARACTER CNPD*5, CAZS*8

!     Variable Initializations
   data season /'WINTER','SPRING','SUMMER',' FALL '/
   data months /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',&
   &'MAY      ','JUNE     ','JULY     ','AUGUST   ',&
   &'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
   data dayofweek  /'WEEKDAY ','SATURDAY','SUNDAY  '/
   data dayofweek7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',&
   &'FRIDAY  ','SATURDAY','SUNDAY  '/
   modnam = 'PRTO3VALS'


!     Print User-specified Background Ozone Concetrations
   if (o3flag(isect) == 'ANNUAL') then
      call header(iounit)
      write(iounit,9001) isect, OzoneUnits
      write(iounit,9006) o3vary(1,isect)
   else if (o3flag(isect) == 'SEASON') then
      call header(iounit)
      write(iounit,9002) isect, OzoneUnits
      write(iounit,9004) (season(i1),i1=1,4)
      write(iounit,9006) (o3vary(i1,isect),i1=1,4)
   else if (o3flag(isect) == 'MONTH') then
      call header(iounit)
      write(iounit,9007) isect, OzoneUnits
      write(iounit,9008)
      write(iounit,9013)
      write(iounit,9010) (o3vary(i1,isect),i1=1,12)
   else if (o3flag(isect) == 'HROFDY') then
      call header(iounit)
      write(iounit,9011) isect, OzoneUnits
      write(iounit,9012)
      write(iounit,9013)
      write(iounit,9014) (i1,o3vary(i1,isect),i1=1,24)
   else if (o3flag(isect) == 'SEASHR') then
      call header(iounit)
      write(iounit,9018) isect, OzoneUnits
      write(iounit,9012)
      write(iounit,9013)
      do i1 = 1, 4
         ifr = (i1-1)*24
         write(iounit,9019) season(i1)
         write(iounit,9014) (i2,o3vary(i2+ifr,isect),i2=1,24)
      end do
   else if (o3flag(isect) == 'HRDOW') then
      call header(iounit)
      write(iounit,99218) isect, OzoneUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 3
         idw = (i1-1)*24
         write(iounit,99021) dayofweek(i1)
         write(iounit,99014) (i3,o3vary(i3+idw,isect),i3=1,24)
      end do
   else if (o3flag(isect) == 'HRDOW7') then
      call header(iounit)
      write(iounit,79218) isect, OzoneUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 7
         idw = (i1-1)*24
         write(iounit,99021) dayofweek7(i1)
         write(iounit,99014) (i3,o3vary(i3+idw,isect),i3=1,24)
      end do
   else if (o3flag(isect) == 'SHRDOW') then
      call header(iounit)
      write(iounit,99018) isect, OzoneUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 3
         idw = (i1-1)*96
         do i2 = 1, 4
            ifr = (i2-1)*24
            write(iounit,99019) season(i2), dayofweek(i1)
            write(iounit,99014) (i3,o3vary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (o3flag(isect) == 'SHRDOW7') then
      call header(iounit)
      write(iounit,79018) isect, OzoneUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 7
         idw = (i1-1)*96
         do i2 = 1, 4
            ifr = (i2-1)*24
            write(iounit,99019) season(i2), dayofweek7(i1)
            write(iounit,99014) (i3,o3vary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (o3flag(isect) == 'MHRDOW') then
      do i1 = 1, 3
         call header(iounit)
         write(iounit,99118) isect, OzoneUnits
         write(iounit,99012)
         write(iounit,99013)
         idw = (i1-1)*288
         do i2 = 1, 12
            ifr = (i2-1)*24
            write(iounit,99020) months(i2), dayofweek(i1)
            write(iounit,99014) (i3,o3vary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (o3flag(isect) == 'MHRDOW7') then
      do i1 = 1, 7
         call header(iounit)
         write(iounit,79118) isect, OzoneUnits
         write(iounit,99012)
         write(iounit,99013)
         idw = (i1-1)*288
         do i2 = 1, 12
            ifr = (i2-1)*24
            write(iounit,99020) months(i2), dayofweek7(i1)
            write(iounit,99014) (i3,o3vary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   end if

9001 format(/40x,'* SECT',i1,' ANNUAL (NON-VARYING) OZONE ',&
   &'CONCENTRATION (',a5,') *'/)
9002 format(/30x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &'WHICH VARY SEASONALLY *'/)
9004 format(40x,4(a6,9x)/20x,40('- ')/)
9006 format(38x,4(e10.5,5x))
9007 format(/42x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &'WHICH VARY MONTHLY *'/)
9008 format(7x,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',&
   &'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',&
   &'DECEMBER'/)
9010 format(5x,12(e9.3,1x))
9011 format(/29x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &'WHICH VARY FOR EACH HOUR OF THE DAY *'/)
9012 format(5x,6('HOUR    O3VALS',6x))
99012 format(2x,8('HOUR   O3VALS',3x))
9013 format(1x,65('- ')/)
99013 format(1x,65('- '))
9014 format(4(5x,6(i3,3x,e10.5,4x)/))
99014 format(2(3x,8(i2,2x,e9.4,3x)/),3x,8(i2,2x,e9.4,3x))
! Unused: 9015 FORMAT(/26X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
!     &       ' WHICH VARY WITH WIND SPEED *'/)
9018 format(/23x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 format(/18x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(SHRDOW) *'/)
79018 format(/18x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(SHRDOW7) *'/)
99118 format(/19x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(MHRDOW) *'/)
79118 format(/19x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(MHRDOW7) *'/)
99218 format(/20x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 format(/20x,'* SECT',i1,' OZONE CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
9019 format(59x,'SEASON = ',a6)
99019 format(46x,'SEASON = ',a6,';  DAY OF WEEK = ',a8)
99020 format(46x,'MONTH = ',a9,';  DAY OF WEEK = ',a8)
99021 format(46x,'DAY OF WEEK = ',a8)
! Unused:  9024 FORMAT(26X,6(1X,E12.5))
! Unused: 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

   return
end subroutine prto3vals

subroutine prtnoxvals(isect)
!***********************************************************************
!                 PRTNOXVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The NOX_VALS Input Data Summary
!
!        PROGRAMMER: CERC
!
!        DATE:       November 2020
!
!        INPUTS:  Model Options and Keyword Summarys
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i1, i2, i3, ifr, idw
   integer :: isect
   character :: season(4)*6, months(12)*9, dayofweek(3)*8,&
   &dayofweek7(7)*8

!     Variable Initializations
   data season /'WINTER','SPRING','SUMMER',' FALL '/
   data months /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',&
   &'MAY      ','JUNE     ','JULY     ','AUGUST   ',&
   &'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
   data dayofweek  /'WEEKDAY ','SATURDAY','SUNDAY  '/
   data dayofweek7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',&
   &'FRIDAY  ','SATURDAY','SUNDAY  '/
   modnam = 'PRTNOXVALS'


!     Print User-specified Background NOx Concetrations
   if (noxflag(isect) == 'ANNUAL') then
      call header(iounit)
      write(iounit,9001) isect, NOxUnits
      write(iounit,9006) noxvary(1,isect)
   else if (noxflag(isect) == 'SEASON') then
      call header(iounit)
      write(iounit,9002) isect, NOxUnits
      write(iounit,9004) (season(i1),i1=1,4)
      write(iounit,9006) (noxvary(i1,isect),i1=1,4)
   else if (noxflag(isect) == 'MONTH') then
      call header(iounit)
      write(iounit,9007) isect, NOxUnits
      write(iounit,9008)
      write(iounit,9013)
      write(iounit,9010) (noxvary(i1,isect),i1=1,12)
   else if (noxflag(isect) == 'HROFDY') then
      call header(iounit)
      write(iounit,9011) isect, NOxUnits
      write(iounit,9012)
      write(iounit,9013)
      write(iounit,9014) (i1,noxvary(i1,isect),i1=1,24)
   else if (noxflag(isect) == 'SEASHR') then
      call header(iounit)
      write(iounit,9018) isect, NOxUnits
      write(iounit,9012)
      write(iounit,9013)
      do i1 = 1, 4
         ifr = (i1-1)*24
         write(iounit,9019) season(i1)
         write(iounit,9014) (i2,noxvary(i2+ifr,isect),i2=1,24)
      end do
   else if (noxflag(isect) == 'HRDOW') then
      call header(iounit)
      write(iounit,99218) isect, NOxUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 3
         idw = (i1-1)*24
         write(iounit,99021) dayofweek(i1)
         write(iounit,99014) (i3,noxvary(i3+idw,isect),i3=1,24)
      end do
   else if (noxflag(isect) == 'HRDOW7') then
      call header(iounit)
      write(iounit,79218) isect, NOxUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 7
         idw = (i1-1)*24
         write(iounit,99021) dayofweek7(i1)
         write(iounit,99014) (i3,noxvary(i3+idw,isect),i3=1,24)
      end do
   else if (noxflag(isect) == 'SHRDOW') then
      call header(iounit)
      write(iounit,99018) isect, NOxUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 3
         idw = (i1-1)*96
         do i2 = 1, 4
            ifr = (i2-1)*24
            write(iounit,99019) season(i2), dayofweek(i1)
            write(iounit,99014) (i3,&
            &noxvary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (noxflag(isect) == 'SHRDOW7') then
      call header(iounit)
      write(iounit,79018) isect, NOxUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 7
         idw = (i1-1)*96
         do i2 = 1, 4
            ifr = (i2-1)*24
            write(iounit,99019) season(i2), dayofweek7(i1)
            write(iounit,99014) (i3,&
            &noxvary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (noxflag(isect) == 'MHRDOW') then
      do i1 = 1, 3
         call header(iounit)
         write(iounit,99118) isect, NOxUnits
         write(iounit,99012)
         write(iounit,99013)
         idw = (i1-1)*288
         do i2 = 1, 12
            ifr = (i2-1)*24
            write(iounit,99020) months(i2), dayofweek(i1)
            write(iounit,99014) (i3,&
            &noxvary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (noxflag(isect) == 'MHRDOW7') then
      do i1 = 1, 7
         call header(iounit)
         write(iounit,79118) isect, NOxUnits
         write(iounit,99012)
         write(iounit,99013)
         idw = (i1-1)*288
         do i2 = 1, 12
            ifr = (i2-1)*24
            write(iounit,99020) months(i2), dayofweek7(i1)
            write(iounit,99014) (i3,&
            &noxvary(i3+ifr+idw,isect),i3=1,24)
         end do
      end do
   else if (noxflag(isect) == 'WSPEED') then
      call header(iounit)
      write(iounit,9015) isect, NOxUnits
      write(iounit,9025) (i1,i1=1,6)
      write(iounit,99013)
      write(iounit,9024) (noxvary(i1,isect),i1=1,6)
   end if
!
9001 format(/40x,'* SECT',i1,' ANNUAL (NON-VARYING) NOX ',&
   &'CONCENTRATION (',a5,') *'/)
9002 format(/30x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &'WHICH VARY SEASONALLY *'/)
9004 format(40x,4(a6,9x)/20x,40('- ')/)
9006 format(38x,4(e10.5,5x))
9007 format(/42x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &'WHICH VARY MONTHLY *'/)
9008 format(7x,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',&
   &'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',&
   &'DECEMBER'/)
9010 format(5x,12(e9.3,1x))
9011 format(/29x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &'WHICH VARY FOR EACH HOUR OF THE DAY *'/)
9012 format(5x,6('HOUR    NOXVALS',6x))
99012 format(2x,8('HOUR   NOXVALS',3x))
9013 format(1x,65('- ')/)
99013 format(1x,65('- '))
9014 format(4(5x,6(i3,3x,e10.5,4x)/))
99014 format(2(3x,8(i2,2x,e9.4,3x)/),3x,8(i2,2x,e9.4,3x))
9015 format(/34x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY WITH WIND SPEED *'/)
9018 format(/23x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 format(/18x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(SHRDOW) *'/)
79018 format(/18x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(SHRDOW7) *'/)
99118 format(/19x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(MHRDOW) *'/)
79118 format(/19x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',&
   &'(MHRDOW7) *'/)
99218 format(/20x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 format(/20x,'* SECT',i1,' NOX CONCENTRATIONS (',a5,') ',&
   &' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
9019 format(59x,'SEASON = ',a6)
99019 format(46x,'SEASON = ',a6,';  DAY OF WEEK = ',a8)
99020 format(46x,'MONTH = ',a9,';  DAY OF WEEK = ',a8)
99021 format(46x,'DAY OF WEEK = ',a8)
9024 format(26x,6(1x,e12.5))
9025 format(26x,6('   WIND SPEED')/26x,6('   CATEGORY',i2))

   return
end subroutine prtnoxvals

subroutine prtsrc
!***********************************************************************
!                 PRTSRC Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        V. Tino
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Print Out The Input Source Data Summary
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        MODIFIED:   To include the Aircraft source data
!                    for Volume and Area sources.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Modified to handle two barrier cases for RLINEXT sources.
!                    Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:   Modified to handle RLINE and RLINEXT sources.
!                    M. G. Snyder, R. Cleary,  Wood 03/04/2019
!
!        MODIFIED:   To include RLINE source data information.
!                    M. G. Snyder, R. Cleary,  Wood 07/20/2018
!
!        MODIFIED:   To include BUOYLINE source data information for
!                    all lines in buoyant line source.
!                    R. Cleary, Wood, 10/18/2017
!
!        MODIFIED:   To remove reference to "STABILITY CATEGORY" and
!                    correct format statement 9024 for 'WSPEED'
!                    EMISFACT option, inherited from ISCST3 code
!                    for 'STAR' option.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To include emission factors that vary by
!                    hour-of-day and day-of-week (HRDOW and HRDOW7).
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To modify format for outputting QFLAG from A6 to
!                    A7, and other minor adjustments to formatting.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
!
!        MODIFIED:   To incorporate flag for CAPPED and/or HORIZONTAL
!                    stack releases based on BETA-test draft option.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
!
!        MODIFIED:   To remove reference to STAR emission factors,
!                    include identification of capped stacks (POINTCAP),
!                    and emission factors that vary by month, hour-of-day
!                    and day-of-week (MHRDOW and MHRDOW7).
!                    Roger Brode, MACTEC (f/k/a PES), Inc., - 08/25/05
!
!        MODIFIED by YICHENG ZHUANG, SRC to combine version 93188 with
!                 version 93046 - 9/28/93
!
!        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/25/93
!
!*       MODIFIED BY PES (for OPENPIT Source) - 7/22/94
!
!*       MODIFIED BY PES to properly handle page breaks in summary
!*                of sources within a source group - 11/19/98
!
!*       MODIFIED BY R. Brode, PES to include additional building
!                 dimensions for PRIME downwash algorithm - 8/9/01
!
!*       MODIFIED BY R. Brode, MACTEC/PES to include identification
!                 of urban and Method 2 sources - 9/29/03
!
!        INPUTS:  Model Options and Keyword Summarys
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   use rline_data, only: rlsource, rlmovesconv
   use buoyant_line
   implicit none
   character :: modnam*12
   logical   :: bloutprocessed

   integer :: i, j, k, nl, i1, i2, i3, ifr, idw, indc, ingrp, nbl
   character :: blding*3, urb*3, iqun*12, cap*3, cqflg*7
   character :: season(4)*6, months(12)*9, dayofweek(3)*8,&
   &dayofweek7(7)*8, cnpd*5, cazs*8,&
   &aft*3                                                 !  Added for Aircraft; UNC-IE

!     Variable Initializations
   data season /'WINTER','SPRING','SUMMER',' FALL '/
   data months /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',&
   &'MAY      ','JUNE     ','JULY     ','AUGUST   ',&
   &'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
   data dayofweek  /'WEEKDAY ','SATURDAY','SUNDAY  '/
   data dayofweek7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',&
   &'FRIDAY  ','SATURDAY','SUNDAY  '/
   modnam = 'PRTSRC'

! --- Set the bouyant line summary output process flag to false to
!      indicate that average parameter summary data has not been
!      processed for this hour
   bloutprocessed = .false.

   if (isstat(8) == 0 .and. isstat(17) == 0 .and.&
   &isstat(18) == 0) then
!        Write Default Emission Rate Units
      iqun = ' (GRAMS/SEC)'
   else
      iqun = '(USER UNITS)'
   end if

!     Write Out The Point Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i)(1:5) == 'POINT') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
         blding = 'NO'
         if (nsec > 0) then
! ---          Check for building data for this source
            do j = 1, nsec
               if(adsbh(j,i)/=0.0d0 .and. adsbw(j,i)/=0.0d0&
               &.and. adsbl(j,i)/=0.0d0) then
! -----------------------------------------------------------------
                  blding = 'YES'
                  exit
               end if
            end do
         end if
         if (srctyp(i) == 'POINTCAP') then
            cap = 'CAP'
         else if (srctyp(i) == 'POINTHOR') then
            cap = 'HOR'
         else
            cap = ' NO'
         end if
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9046) iqun
         end if
         write(iounit,9047) srcid(i), cnpd, aqs(i),&
         &axs(i), ays(i), cazs, ahs(i), ats(i), avs(i),&
         &ads(i), blding, urb, cap, qflag(i)
      end if
   end do

!     Write Out The Volume Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i) == 'VOLUME') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
!**  Added for Aircraft Plume Rise; UNC-IE
         if (aftsrc(i) == 'Y') then
            aft = 'YES'
         else
            aft = 'NO'
         end if
!**  End Aircraft Plume Rise insert; April 2023
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9074) iqun
         end if
         if (qflag(i) == 'HOURLY' .and. l_hrlysig(i)) then
!              Source uses HOUREMIS option with hourly varying sigmas
            cqflg = 'HRLYSIG'
         else
            cqflg = qflag(i)
         end if
         write(iounit,9075) srcid(i), cnpd, aqs(i),&
         &axs(i), ays(i), cazs, ahs(i), asyini(i), aszini(i),&
         &urb, cqflg, aft   ! Added for Aircraft; UNC-IE
      end if
   end do

!     Write Out The Area Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i) == 'AREA') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
!**  Added for Aircraft Plume Rise; UNC-IE
         if (aftsrc(i) == 'Y') then
            aft = 'YES'
         else
            aft = 'NO'
         end if
!**  End Aircraft Plume Rise insert; April 2023
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9076) iqun
         end if
         if (qflag(i) == 'HOURLY' .and. l_hrlysig(i)) then
!              Source uses HOUREMIS option with hourly varying sigmas
            cqflg = 'HRLYSIG'
         else
            cqflg = qflag(i)
         end if
         write(iounit,9077) srcid(i), cnpd, aqs(i),&
         &axs(i), ays(i), cazs, ahs(i), axinit(i), ayinit(i),&
         &aangle(i), aszini(i), urb, cqflg, aft   ! Added for Aircraft; UNC-IE
      end if
   end do

!     Write Out The AREACIRC Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i) == 'AREACIRC') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
!**  Added for Aircraft Plume Rise; UNC-IE
         if (aftsrc(i) == 'Y') then
            aft = 'YES'
         else
            aft = 'NO'
         end if
!**  End Aircraft Plume Rise insert; April 2023
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9078) iqun
         end if
         if (qflag(i) == 'HOURLY' .and. l_hrlysig(i)) then
!              Source uses HOUREMIS option with hourly varying sigmas
            cqflg = 'HRLYSIG'
         else
            cqflg = qflag(i)
         end if
         write(iounit,9079) srcid(i), cnpd, aqs(i),&
         &axs(i), ays(i), cazs, ahs(i), radius(i),&
         &nverts(i), aszini(i), urb, cqflg, aft  ! Added for Aircraft; UNC-IE
      end if
   end do

!     Write Out The AREAPOLY Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i) == 'AREAPOLY') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
!**  Added for Aircraft Plume Rise; UNC-IE
         if (aftsrc(i) == 'Y') then
            aft = 'YES'
         else
            aft = 'NO'
         end if
!**  End Aircraft Plume Rise insert; April 2023
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9080) iqun
         end if
         if (qflag(i) == 'HOURLY' .and. l_hrlysig(i)) then
!              Source uses HOUREMIS option with hourly varying sigmas
            cqflg = 'HRLYSIG'
         else
            cqflg = qflag(i)
         end if
         write(iounit,9081) srcid(i), cnpd, aqs(i),&
         &axs(i), ays(i), cazs, ahs(i), nverts(i),&
         &aszini(i), urb, cqflg, aft    ! Added for Aircraft; UNC-IE
      end if
   end do

!     Write Out The Line Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i) == 'LINE') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,89076) iqun
         end if
         if (qflag(i) == 'HOURLY' .and. l_hrlysig(i)) then
!              Source uses HOUREMIS option with hourly varying sigmas
            cqflg = 'HRLYSIG'
         else
            cqflg = qflag(i)
         end if
         write(iounit,89077) srcid(i), cnpd, aqs(i),&
         &axs1(i), ays1(i), axs2(i), ays2(i),cazs, ahs(i),&
         &awidth(i), aszini(i), urb, cqflg
      end if
   end do

!     Write Out The RLINE Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if ((srctyp(i) == 'RLINE') .or.&
      &(srctyp(i) == 'RLINEXT')) then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            if (rlmovesconv) then
               write (iounit,9109)
            else
               call header(iounit)
               write (iounit,9110) iqun
            end if
         end if
         write(iounit,9111) srcid(i), cnpd,&
         &rlsource(i)%qemis, rlsource(i)%xsb,&
         &rlsource(i)%ysb, rlsource(i)%zsb,&
         &rlsource(i)%xse, rlsource(i)%yse,&
         &rlsource(i)%zse, cazs,&
         &rlsource(i)%dcl, rlsource(i)%init_sigmaz,&
         &rlsource(i)%width, urb, qflag(i)
      end if
   end do

!     Write Out The RLINE Source Configuration Parameters for
!     Barriers, If Any
   indc = 0
   do i = 1, numsrc
      if ((srctyp(i) == 'RLINE') .or.&
      &(srctyp(i) == 'RLINEXT')) then
         if (rlsource(i)%htwall > 0.0) then
            indc = indc + 1
            if (mod(indc-1,40) == 0) then
               write(iounit,9112)
            end if
            write(iounit,9113) srcid(i),&
            &rlsource(i)%htwall, rlsource(i)%dclwall,&
            &rlsource(i)%htwall2, rlsource(i)%dclwall2
         end if
      end if
   end do

!     Write Out The RLINE Source Configuration Parameters for
!     Depressed Roadways, If Any
   indc = 0
   do i = 1, numsrc
      if ((srctyp(i) == 'RLINE') .or.&
      &(srctyp(i) == 'RLINEXT')) then
         if (rlsource(i)%depth /= 0.0) then
            indc = indc + 1
            if (mod(indc-1,40) == 0) then
               write(iounit,9114)
            end if
            write(iounit,9115) srcid(i),&
            &rlsource(i)%depth, rlsource(i)%wtop,&
            &rlsource(i)%wbottom
         end if
      end if
   end do

!*    Write Out The OpenPit Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if (srctyp(i) == 'OPENPIT') then
         indc = indc + 1
         if (urbsrc(i) == 'Y') then
            urb = 'YES'
         else
            urb = 'NO'
         end if
         if (l_method2(i)) then
            write(cnpd,'("METH2")')
         else
            write(cnpd,'(I4,1X)') inpd(i)
         end if
         if (l_flatsrc(i)) then
            write(cazs,'(4X,"FLAT")')
         else
            write(cazs,'(F8.1)') azs(i)
         end if
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9082) iqun
         end if
         write(iounit,9083) srcid(i), cnpd, aqs(i),&
         &axs(i), ays(i), cazs, ahs(i), axinit(i), ayinit(i),&
         &aangle(i), avolum(i), urb, qflag(i)
      end if
   end do

!     Write Out The Buoyant Line Source Data, If Any
   indc = 0
   do i = 1, numsrc
      if  (srctyp(i) == 'BUOYLINE' .and.&
      &(.not. bloutprocessed)) then
!           BLOUTPROCESSED lets AERMOD know that all lines associated
!            with the buoyant line source were written on the first pass
!            through the sources since all the lines are processed
!            as one source
         do nbl = 1, nbltotal
            indc = indc + 1
            if (urbsrc(i) == 'Y') then
               urb = 'YES'
            else
               urb = 'NO'
            end if
            if (l_method2(i)) then
               write(cnpd,'("METH2")')
            else
               write(cnpd,'(I4,1X)') inpd(i)
            end if
            if (l_flatsrc(i)) then
               write(cazs,'(4X,"FLAT")')
            else
               write(cazs,'(F8.1)') blineparms(nbl)%elev
            end if
            if (mod(indc-1,40) == 0) then
               call header(iounit)
               write(iounit,9086) iqun
            end if
            write(iounit,9087) blineparms(nbl)%srcid, cnpd,&
            &blineparms(nbl)%blqs, blineparms(nbl)%xbeg,&
            &blineparms(nbl)%ybeg, blineparms(nbl)%xend,&
            &blineparms(nbl)%yend, cazs,&
            &blineparms(nbl)%blhs, urb,&
            &qflag(blineparms(nbl)%isrcnum)                       ! CRT 12/4/2017
9087        format(1x,a12,2x,a5,2x,e11.5,4f10.1,a8,f9.2,7x,a3,3x,a7)
         end do

         write(iounit,9088)
9088     format(/6x,'BL GRP',3x,'AVG BLDG',3x,'AVG BLDG',3x,&
         &'AVG BLDG',3x,&
         &'AVG LINE',3x,'AVG SEPARATION',5x,'AVG BUOYANCY',&
         &/16x,'LENGTH',5x,'HEIGHT',5x,'WIDTH',6x,'WIDTH',5x,&
         &'BETWEEN LINES',7x,'PARAMETER',1x,&
         &/15x,'(METERS)',3x,'(METERS)',3x,'(METERS)',3x,&
         &'(METERS)',6x,'(METERS)',6x,'(METER**4/SEC**3)',1x&
         &/66(' -')/)

! Multiple_BuoyLines_D41_Wood
!        Processing for multiple buoyant lines
         do k = 1,numblavginp
            write(iounit,9089) blavginp_grpid(k),&
            &blavginp_llen(k), blavginp_bhgt(k),&
            &blavginp_bwid(k), blavginp_lwid(k),&
            &blavginp_bsep(k), blavginp_fprm(k)
         end do
9089     format(6x,a8,3x,f8.1,3x,f8.1,3x,f8.1,3x,f8.1,6x,f8.1,&
         &10x,f8.1)
         bloutprocessed = .true.
      end if
   end do

   if (.not. psdcredit) then
!        Print The Source Group IDs with Source IDs
      indc = 0
      do j = 1, numgrp
         ingrp = 0
         do k = 1, numsrc
            if (igroup(k,j) == 1) then
               ingrp = ingrp + 1
               workid(ingrp) = srcid(k)
            end if
         end do
! ---       Check for BACKGRND "source" being included
!           in source group
         if (grp_back(j)) then
            ingrp = ingrp + 1
            workid(ingrp) = 'BACKGROUND'
         end if
!           Determine Number of Lines @ 8/Line
         nl = 1 + int((ingrp-1)/8)
         do k = 1, nl
            indc = indc + 1
            if (mod(indc-1,20) == 0) then
               call header(iounit)
               write(iounit,9058)
            end if
            if (k == 1 .and. k == nl) then
               write(iounit,9068) grpid(j), (workid(i),i=1,ingrp)
            else if (k == 1 .and. k /= nl) then
               write(iounit,9068) grpid(j), (workid(i),i=1,8*k)
            else if (k == nl) then
               write(iounit,9067) (workid(i),i=1+8*(k-1),ingrp)
            else
               write(iounit,9067) (workid(i),i=1+8*(k-1),8*k)
            end if
         end do
      end do
   end if

!     Print The OLM Source Group IDs with Source IDs
   indc = 0
   do j = 1, numolm
      ingrp = 0
      do k = 1, numsrc
         if (igrp_olm(k,j) == 1) then
            ingrp = ingrp + 1
            workid(ingrp) = srcid(k)
         end if
      end do
!        Determine Number of Lines @ 8/Line
      nl = 1 + int((ingrp-1)/8)
      do k = 1, nl
         indc = indc + 1
         if (mod(indc-1,20) == 0) then
            call header(iounit)
            write(iounit,9059)
         end if
         if (k == 1 .and. k == nl) then
            write(iounit,9068) olmid(j), (workid(i),i=1,ingrp)
         else if (k == 1 .and. k /= nl) then
            write(iounit,9068) olmid(j), (workid(i),i=1,8*k)
         else if (k == nl) then
            write(iounit,9067) (workid(i),i=1+8*(k-1),ingrp)
         else
            write(iounit,9067) (workid(i),i=1+8*(k-1),8*k)
         end if
      end do
   end do

!     Print The PSD Source Group IDs with Source IDs for PSDCREDIT Option
   indc = 0
   do j = 1, numpsd
      ingrp = 0
      do k = 1, numsrc
         if (igrp_psd(k,j) == 1) then
            ingrp = ingrp + 1
            workid(ingrp) = srcid(k)
         end if
      end do
!        Determine Number of Lines @ 8/Line
      nl = 1 + int((ingrp-1)/8)
      do k = 1, nl
         indc = indc + 1
         if (mod(indc-1,20) == 0) then
            call header(iounit)
            write(iounit,99059)
         end if
         if (k == 1 .and. k == nl) then
            write(iounit,9068) psdid(j), (workid(i),i=1,ingrp)
         else if (k == 1 .and. k /= nl) then
            write(iounit,9068) psdid(j), (workid(i),i=1,8*k)
         else if (k == nl) then
            write(iounit,9067) (workid(i),i=1+8*(k-1),ingrp)
         else
            write(iounit,9067) (workid(i),i=1+8*(k-1),8*k)
         end if
      end do
   end do

!     Print The URBAN Areas with applicable Source IDs
   indc = 0
   do j = 1, numurb
      ingrp = 0
      do k = 1, numsrc
         if (iurbgrp(k,j) == 1) then
            ingrp = ingrp + 1
            workid(ingrp) = srcid(k)
         end if
      end do
!        Determine Number of Lines @ 8/Line
      nl = 1 + int((ingrp-1)/8)
      do k = 1, nl
         indc = indc + 1
         if (mod(indc-1,20) == 0) then
            call header(iounit)
            write(iounit,99058)
         end if
         if (k == 1 .and. k == nl) then
            write(iounit,99068) urbid(j), urbpop(j),&
            &(workid(i),i=1,ingrp)
         else if (k == 1 .and. k /= nl) then
            write(iounit,99068) urbid(j), urbpop(j),&
            &(workid(i),i=1,8*k)
         else if (k == nl) then
            write(iounit,9067) (workid(i),i=1+8*(k-1),ingrp)
         else
            write(iounit,9067) (workid(i),i=1+8*(k-1),8*k)
         end if
      end do
   end do

!     Print out NO2_RATIO Data for OLM, PVMRM and TTRM Options
   if (olm .or. pvmrm .or. runttrm .or. grsm) then
      indc = 0
      do i = 1, numsrc, 4
         indc = indc + 1
         if (mod(indc-1,40) == 0) then
            call header(iounit)
            write(iounit,9060)
         end if
         if (i+3 <= numsrc) then
            write(iounit,9061) srcid(i), ano2_ratio(i),&
            &srcid(i+1), ano2_ratio(i+1),&
            &srcid(i+2), ano2_ratio(i+2),&
            &srcid(i+3), ano2_ratio(i+3)
         else if (i+2 <= numsrc) then
            write(iounit,9061) srcid(i), ano2_ratio(i),&
            &srcid(i+1), ano2_ratio(i+1),&
            &srcid(i+2), ano2_ratio(i+2)
         else if (i+1 <= numsrc) then
            write(iounit,9061) srcid(i), ano2_ratio(i),&
            &srcid(i+1), ano2_ratio(i+1)
         else
            write(iounit,9061) srcid(i), ano2_ratio(i)
         end if
      end do
   end if

!     Print Out Wet or Dry Deposition Information.
   indc = 0
   do i = 1, numsrc
      npd = inpd(i)
      if (npd /= 0 .and. .not.l_method2(i)) then
         indc = indc + 1
         if (mod(indc-1,3) == 0) then
            call header(iounit)
            write(iounit,9049)
         end if
         write(iounit,9050) srcid(i), srctyp(i)
         write(iounit,9051) (aphi(j,i),j=1,npd)
         write(iounit,9052) (apdiam(j,i),j=1,npd)
         write(iounit,9053) (apdens(j,i),j=1,npd)
      else if (npd /= 0 .and. l_method2(i)) then
!           Summarize inputs for Method 2 particle deposition
         indc = indc + 1
         if (mod(indc-1,3) == 0) then
            call header(iounit)
            write(iounit,9049)
         end if
         write(iounit,9050)  srcid(i), srctyp(i)
         write(iounit,99051) (finemass(i),j=1,npd)
         write(iounit,9052)  (apdiam(j,i),j=1,npd)
         write(iounit,9053)  (apdens(j,i),j=1,npd)
      else if (lwgas .or. (ldgas .and. .not.luservd)) then
!           Summarize inputs for gas deposition option
         indc = indc + 1
         if (mod(indc-1,3) == 0) then
            call header(iounit)
            write(iounit,9049)
         end if
         write(iounit,9050) srcid(i), srctyp(i)
         if (ldgas) then
            write(iounit,99090) pdiff(i)
            write(iounit,99091) pdiffw(i)
            write(iounit,99093) rcli(i)
            write(iounit,9094)  henry(i)
         end if

      end if
   end do

!     Write Out Direction Specific Bldg. Dimensions, If Present
   indc = 0
   do i = 1, numsrc
      blding = 'NO'
      if (nsec > 0) then
! ---       Check for building data for this source
         do j = 1, nsec
            if(adsbh(j,i)/=0.0d0 .and. adsbw(j,i)/=0.0d0&
            &.and. adsbl(j,i)/=0.0d0) then
! --------------------------------------------------------------
               blding = 'YES'
               exit
            end if
         end do
      end if
      if (blding == 'YES') then
         indc = indc + 1
!           Print Out Direction Specific Bldg. Dimensions
         if (mod(indc-1,4) == 0) then
            call header(iounit)
            write(iounit,9064)
         end if
         write(iounit,9062) srcid(i),&
         &(j,dabs(adsbh(j,i)),adsbw(j,i),adsbl(j,i),adsxadj(j,i),&
         &adsyadj(j,i),j=1,nsec)

! ---       Print the type of structure: rectangular or streamlined
!            when running the AWMADWNW options
         if( l_awmadw )then
            if (l_strmln_bldg) then
               write (iounit,9065) srcid(i)
            else if (l_rect_bldg) then
               write (iounit,9066) srcid(i)
            endif
         end if
! --------------------------------------------------------------------
      end if
   end do

!     Print Source Emission Rate Scalars.
   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'SEASON') then
         indc = indc + 1
         if (mod(indc-1,6) == 0) then
            call header(iounit)
            write(iounit,9002)
            write(iounit,9004) (season(i1),i1=1,4)
         end if
         write(iounit,9005) srcid(i),srctyp(i)
         write(iounit,9006) (qfact(i1,i),i1=1,4)
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'MONTH') then
         indc = indc + 1
         if (mod(indc-1,6) == 0) then
            call header(iounit)
            write(iounit,9007)
            write(iounit,9008)
            write(iounit,9013)
         end if
         write(iounit,9009) srcid(i),srctyp(i)
         write(iounit,9010) (qfact(i1,i),i1=1,12)
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'HROFDY') then
         indc = indc + 1
         if (mod(indc-1,5) == 0) then
            call header(iounit)
            write(iounit,9011)
            write(iounit,9012)
            write(iounit,9013)
         end if
         write(iounit,9009) srcid(i),srctyp(i)
         write(iounit,9014) (i1,qfact(i1,i),i1=1,24)
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'WSPEED') then
         indc = indc + 1
         if (mod(indc-1,3) == 0) then
            call header(iounit)
            write(iounit,9015)
            write(iounit,9013)
         end if
         write(iounit,9009) srcid(i),srctyp(i)
         write(iounit,9025) (j, j=1,6)
         write(iounit,9024) (qfact(i2,i),i2=1,6)
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'SEASHR') then
         indc = indc + 1
         call header(iounit)
         write(iounit,9018)
         write(iounit,9012)
         write(iounit,9013)
         write(iounit,9009) srcid(i),srctyp(i)
         do i1 = 1, 4
            ifr = (i1-1)*24
            write(iounit,9019) season(i1)
            write(iounit,9014) (i2,qfact(i2+ifr,i),i2=1,24)
         end do
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'HRDOW') then
         indc = indc + 1
         call header(iounit)
         write(iounit,99218)
         write(iounit,99009) srcid(i), srctyp(i)
         write(iounit,99012)
         write(iounit,99013)
         do i1 = 1, 3
            idw = (i1-1)*24
            write(iounit,99021) dayofweek(i1)
            write(iounit,99014) (i3,qfact(i3+idw,i),i3=1,24)
         end do
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'HRDOW7') then
         indc = indc + 1
         call header(iounit)
         write(iounit,79218)
         write(iounit,99009) srcid(i), srctyp(i)
         write(iounit,99012)
         write(iounit,99013)
         do i1 = 1, 7
            idw = (i1-1)*24
            write(iounit,99021) dayofweek7(i1)
            write(iounit,99014) (i3,qfact(i3+idw,i),i3=1,24)
         end do
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'SHRDOW') then
         indc = indc + 1
         call header(iounit)
         write(iounit,99018)
         write(iounit,99009) srcid(i), srctyp(i)
         write(iounit,99012)
         write(iounit,99013)
         do i1 = 1, 3
            idw = (i1-1)*96
            do i2 = 1, 4
               ifr = (i2-1)*24
               write(iounit,99019) season(i2), dayofweek(i1)
               write(iounit,99014) (i3,qfact(i3+ifr+idw,i),i3=1,24)
            end do
         end do
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'SHRDOW7') then
         indc = indc + 1
         call header(iounit)
         write(iounit,79018)
         write(iounit,99009) srcid(i), srctyp(i)
         write(iounit,99012)
         write(iounit,99013)
         do i1 = 1, 7
            idw = (i1-1)*96
            do i2 = 1, 4
               ifr = (i2-1)*24
               write(iounit,99019) season(i2), dayofweek7(i1)
               write(iounit,99014) (i3,qfact(i3+ifr+idw,i),i3=1,24)
            end do
         end do
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'MHRDOW') then
         do i1 = 1, 3
            call header(iounit)
            write(iounit,99118)
            write(iounit,99009) srcid(i), srctyp(i)
            write(iounit,99012)
            write(iounit,99013)
            idw = (i1-1)*288
            do i2 = 1, 12
               ifr = (i2-1)*24
               write(iounit,99020) months(i2), dayofweek(i1)
               write(iounit,99014) (i3,qfact(i3+ifr+idw,i),i3=1,24)
            end do
         end do
      end if
   end do

   indc = 0
   do i = 1, numsrc
      if (qflag(i) == 'MHRDOW7') then
         do i1 = 1, 7
            call header(iounit)
            write(iounit,79118)
            write(iounit,99009) srcid(i), srctyp(i)
            write(iounit,99012)
            write(iounit,99013)
            idw = (i1-1)*288
            do i2 = 1, 12
               ifr = (i2-1)*24
               write(iounit,99020) months(i2), dayofweek7(i1)
               write(iounit,99014) (i3,qfact(i3+ifr+idw,i),i3=1,24)
            end do
         end do
      end if
   end do

9002 format(/39x,'* SOURCE EMISSION RATE SCALARS WHICH VARY ',&
   &'SEASONALLY *'/)
9004 format(40x,4(a6,9x)/20x,40('- ')/)
9005 format(/10x,' SOURCE ID = ',a12,' ;  SOURCE TYPE = ',a8,' :')
9006 format(38x,4(e10.5,5x))
9007 format(/41x,'* SOURCE EMISSION RATE SCALARS WHICH VARY MONTHLY *',&
   &/)
9008 format(7x,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',&
   &'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',&
   &'DECEMBER'/)
9009 format(/' SOURCE ID = ',a12,' ; SOURCE TYPE = ',a8,' :')
99009 format(' SOURCE ID = ',a12,' ; SOURCE TYPE = ',a8,' :')
9010 format(5x,12(e9.3,1x))
9011 format(/28x,'* SOURCE EMISSION RATE SCALARS WHICH VARY FOR EACH',&
   &' HOUR OF THE DAY *'/)
9012 format(5x,6('HOUR    SCALAR',6x))
99012 format(2x,8('HOUR   SCALAR',3x))
9013 format(1x,65('- ')/)
99013 format(1x,65('- '))
9014 format(4(5x,6(i3,3x,e10.5,4x)/))
99014 format(2(3x,8(i2,2x,e9.4,3x)/),3x,8(i2,2x,e9.4,3x))
9015 format(/25x,'* SOURCE EMISSION RATE SCALARS WHICH VARY WITH',&
   &' WIND SPEED *'/)
9018 format(/22x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 format(/17x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW) *'/)
79018 format(/17x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7) *'/)
99118 format(/18x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW) *'/)
79118 format(/18x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7) *'/)
99218 format(/19x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 format(/19x,'* SOURCE EMISSION RATE SCALARS WHICH VARY',&
   &' DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
9019 format(59x,'SEASON = ',a6)
99019 format(46x,'SEASON = ',a6,';  DAY OF WEEK = ',a8)
99020 format(46x,'MONTH = ',a9,';  DAY OF WEEK = ',a8)
99021 format(46x,'DAY OF WEEK = ',a8)
9024 format(26x,6(1x,e12.5))
9025 format(/26x,6('   WIND SPEED')/26x,6('   CATEGORY',i2))
9046 format(//50x,'*** POINT SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',20x,'BASE     STACK   STACK',4x,&
   &'STACK     STACK    BLDG   URBAN  CAP/  EMIS RATE',/3x,&
   &'SOURCE',7x,'PART. ',a12,5x,'X',8x,'Y',6x,'ELEV.    ',&
   &'HEIGHT  TEMP.   EXIT VEL. DIAMETER',2x,'EXISTS SOURCE ',&
   &'HOR   SCALAR',/4x,' ID         CATS.              ',&
   &1x,2('(METERS) (METERS) '),'(DEG.K) ',' (M/SEC) ',1x,'(METERS)',&
   &22x,'VARY BY'/65(' -')/)
9047 format(1x,a12,2x,a5,2x,e11.5,2f10.1,a8,4f9.2,&
   &4x,a3,5x,a3,2x,a3,2x,a7)
9049 format(/48x,'*** SOURCE PARTICULATE/GAS DATA ***'//)
9050 format(//8x,'*** SOURCE ID = ',a12,'; SOURCE TYPE = ',a8,' ***')
9051 format(/8x,'MASS FRACTION ='/4(:10x,10(:f9.5,', ')/))
99051 format(/8x,'FINE PARTICLE MASS FRACTION ='/4(:10x,10(:f9.5,', ')&
   &/))
9052 format(/8x,'PARTICLE DIAMETER (MICRONS) ='/4(:10x,10(:f9.5,', ')&
   &/))
9053 format(/8x,'PARTICLE DENSITY (G/CM**3)  ='/4(:10x,10(:f9.5,', ')&
   &/))
9058 format(//43x,'*** SOURCE IDs DEFINING SOURCE GROUPS ***'//&
   &1x,'SRCGROUP ID',46x,'SOURCE IDs'/&
   &1x,'-----------',46x,'----------'/)
99058 format(//42x,'*** SOURCE IDs DEFINED AS URBAN SOURCES ***'//&
   &2x,'URBAN ID',3x,'URBAN POP',36x,'SOURCE IDs'/&
   &2x,'--------',3x,'---------',36x,'----------'/)
9059 format(//41x,'*** SOURCE IDs DEFINING OLM SOURCE GROUPS ***'/&
   &41x,'***        FOR COMBINING PLUMES           ***'//&
   &1x,'OLMGROUP ID',47x,'SOURCE IDs'/&
   &1x,'-----------',47x,'----------'/)
99059 format(/41x,'*** SOURCE IDs DEFINING PSD SOURCE GROUPS ***'/&
   &41x,'***      FOR PVMRM PSDCREDIT OPTION       ***'//&
   &1x,'PSDGROUP ID',47x,'SOURCE IDs'/)
9060 format(//39x,'*** IN-STACK NO2 RATIOS FOR ***'/&
   &39x,'*** OLM/PVMRM/GRSM OPTIONS ***'//&
   &/1x,4('SOURCE_ID',4x,'NO2_RATIO',6x)/)
9061 format(1x,4(a12,2x,f7.3,7x))
9068 format(/2x,a8,2x,8(1x,a12,','))
99068 format(/2x,a8,3x,f9.0,2x,7(1x,a12,','))
9067 format(/12x,8(1x,a12,','))

! --- PRIME --------------------------------------------------
9062 format(/' SOURCE ID: ',a12,&
   &/,2('  IFV    BH      BW      BL     XADJ    YADJ',3x)&
   &,/,18(2(2x,i3,5(f7.1,','),2x)/))
! ------------------------------------------------------------

9064 format(/42x,'*** DIRECTION SPECIFIC BUILDING DIMENSIONS ***'/)
9065 format(' BUILDING STRUCTURE TYPE: STREAMLINED FOR SRCID: ',a12,/)
9066 format(' BUILDING STRUCTURE TYPE: RECTANGULAR FOR SRCID: ',a12,/)

9074 format(//50x,'*** VOLUME SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',20x,'BASE    RELEASE    INIT.',4x,&
   &'INIT.   URBAN  EMISSION RATE   AIRCRAFT',/3x,&             ! Added for Aircraft; UNC-IE
   &'SOURCE',7x,'PART. ',a12,5x,'X',8x,'Y',6x,'ELEV.   ',&
   &'HEIGHT      SY       SZ     SOURCE  SCALAR VARY',&
   &/4x,' ID         CATS.              ',&
   &1x,3('(METERS) (METERS) '),13x,'BY'/61(' -')/)
! 9075 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,1X,F8.2,1X,
!    &       F8.2,5X,A3,3X,A7)
!**  Updated for Aircraft; UNC-IE
9075 format(1x,a12,2x,a5,2x,e11.5,2f10.1,a8,f9.2,1x,f8.2,1x,&
   &f8.2,5x,a3,3x,a7,10x,a3)

9076 format(//50x,'*** AREA SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',2x,'COORD (SW CORNER)',2x,&
   &'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4x,&
   &'INIT.   URBAN  ',&
   &'EMISSION RATE  AIRCRAFT',&                                 ! Added for Aircraft; UNC-IE
   &/3x,'SOURCE',7x,'PART. ',a11,7x,'X',8x,'Y',6x,'ELEV.    ',&
   &'HEIGHT  OF AREA   OF AREA   OF AREA     SZ     SOURCE ',&
   &' SCALAR VARY',/4x,' ID         CATS.   /METER**2)  ',&
   &1x,2('(METERS) (METERS) '),2('(METERS)',2x),' (DEG.)  (METERS)',&
   &14x,'BY'/66(' -')/)
! 9077 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,3(1X,F9.2),1X,F8.2,
!    &       5X,A3,3X,A7)
!**  Updated for Aircraft; UNC-IE
9077 format(1x,a12,2x,a5,2x,e11.5,2f10.1,a8,f9.2,3(1x,f9.2),1x,f8.2,&
   &5x,a3,3x,a7,10x,a3)
9078 format(//48x,'*** AREACIRC SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',4x,'CENTER OF AREA',3x,&
   &'BASE     RELEASE  RADIUS     NUMBER     INIT.',&
   &3x,'URBAN  EMISSION RATE  AIRCRAFT',&                     ! Added for Aircraft; UNC-IE
   &/3x,'SOURCE',7x,'PART. ',a11,7x,'X',8x,'Y',6x,'ELEV.    ',&
   &'HEIGHT   OF AREA   OF VERTS.    SZ     SOURCE  SCALAR VARY',&
   &/4x,' ID         CATS.   /METER**2)  ',&
   &1x,2('(METERS) (METERS) '),21x,'(METERS)',14x,'BY'&
   &/63(' -')/)
! 9079 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,2X,F9.2,4X,I4,4X,
!     &       F8.2,5X,A3,3X,A7)
!**  Updated for Aircraft; UNC-IE
9079 format(1x,a12,2x,a5,2x,e11.5,2f10.1,a8,f9.2,2x,f9.2,4x,i4,4x,&
   &f8.2,5x,a3,3x,a7,10x,a3)
9080 format(//48x,'*** AREAPOLY SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',3x,'LOCATION OF AREA',2x,&
   &'BASE     RELEASE  NUMBER      INIT.',3x,'URBAN  EMISSION RATE',&
   &'    AIRCRAFT',&                                          ! Added for Aircraft; UNC-IE
   &/3x,'SOURCE',7x,'PART. ',a11,7x,'X',8x,'Y',6x,'ELEV.    ',&
   &'HEIGHT  OF VERTS.     SZ     SOURCE  SCALAR VARY',&
   &/4x,' ID         CATS.   /METER**2)  ',&
   &1x,2('(METERS) (METERS) '),11x,'(METERS)              BY'&
   &/63(' -')/)
! 9081 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,4X,I4,5X,F8.2,5X,
!     & A3,3X,A7)
!**  Updated for Aircraft; UNC-IE
9081 format(1x,a12,2x,a5,2x,e11.5,2f10.1,a8,f9.2,4x,i4,5x,f8.2,5x,&
   &a3,3x,a7,10x,a3)
89076 format(//50x,'*** LINE SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',5x,'FIRST COORD',8x,'SECOND COORD',5x,&
   &'BASE    RELEASE    WIDTH    INIT.   URBAN  EMISSION RATE',&
   &/3x,'SOURCE',7x,'PART. ',a11,8x,'X',7x,'Y',11x,'X',7x,'Y',6x,&
   &'ELEV.   ',&
   &'HEIGHT    OF LINE    SZ     SOURCE SCALAR VARY',&
   &/4x,' ID         CATS.   /METER**2)  ',1x,&
   &'(METERS) (METERS) ',2x,'(METERS) (METERS) ',2('(METERS)',1x),&
   &1x,'(METERS) (METERS)',13x,'BY'/66(' -')/)
89077 format(1x,a12,2x,a5,2x,e11.5,4f10.1,a8,f9.2,1x,f9.2,&
   &1x,f8.2,5x,a3,3x,a7)
9082 format(//50x,'*** OPENPIT SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',2x,'COORD (SW CORNER)',2x,&
   &'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4x,&
   &'VOLUME',3x,'URBAN  EMISSION RATE',&
   &/3x,'SOURCE',7x,'PART. ',a11,7x,'X',8x,'Y',6x,'ELEV.    ',&
   &'HEIGHT  OF PIT    OF PIT    OF PIT     OF PIT   ',&
   &'SOURCE  SCALAR VARY',&
   &/4x,' ID         CATS.   /METER**2)  ',&
   &1x,2('(METERS) (METERS) '),2('(METERS)',2x),' (DEG.) ',3x,&
   &'(M**3)               BY'&
   &/66(' -')/)
9083 format(1x,a12,2x,a5,2x,e11.5,2f10.1,a8,f9.2,3(1x,f9.2),&
   &3x,e10.5,2x,a3,3x,a7)

9109 format(//40x,'*** RLINE/RLINEXT SOURCE DATA (with MOVES units)',&
   &'***'//10x,&
   &'NUMBER EMISSION RATE',6x,'FIRST COORD',13x,'SECOND COORD',6x,&
   &'BASE',3x,4x,'INIT.  NO.  WIDTH  URBAN  EMIS RATE',&
   &/3x,'SOURCE',7x,'PART. GRAMS/HOUR',5x,'X',7x,'Y',7x,&
   &'Z',7x,'X',7x,'Y',7x,'Z',4x,'ELEV.',1x,'DCL',3x,&
   &'SZ    OF    PER',3x,'SOURCE',2x,'SCALAR',&
   &/4x,' ID         CATS.    ',&
   &1x,('/LINK)'),4x,('(M)     (M)     (M)'),5x,&
   &('(M)     (M)     (M)   (M)   (M)   (M)'),2x,&
   &'LANES',2x,'LANE',9x,'VARY BY',&
   &/66(' -')/)

9110 format(//50x,'*** RLINE/RLINEXT SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',6x,'FIRST COORD',13x,'SECOND COORD',9x,&
   &'BASE',8x,'INIT.  WIDTH  URBAN EMIS RATE',&
   &/3x,'SOURCE',7x,'PART. ',a11,5x,'X',7x,'Y',7x,&
   &'Z',8x,'X',7x,'Y',8x,'Z',5x,'ELEV.',3x,'DCL',3x,&
   &'SZ  ',5x,3x,'SOURCE',2x,'SCALAR',&
   &/4x,' ID         CATS.    ',&
   &1x,('/METER)'),4x,('(M)     (M)     (M)'),6x,&
   &('(M)     (M)      (M)     (M)    (M)   (M)    (M)'),1x,&
   &10x,'VARY BY',&
   &/66(' -')/)

9111 format(1x,a12,2x,a5,2x,e11.5,1x,f8.1,1x,f9.1,1x,f5.1,1x,f8.1,1x,&
   &f9.1,1x,f5.1,1x,a8,1x,f5.1,1x,f5.2,1x,f5.2,2x,a3,3x,a7)

9112 format(//14x, 'BARRIER',6x,'BARRIER',7x,'BARRIER',7x,'BARRIER',&
   &/3x,'SOURCE',5x,'HTWALL',7x,'DCLWALL',7x,'HTWALL2',6x,'DCLWALL2',&
   &/4x,'ID',7x,'(METERS)',5x,'(METERS)',6x,'(METERS)',6x,'(METERS)',&
   &/66(' -')/)

9113 format(1x,a12,1x,f6.2,8x,f6.2,8x,f6.2,8x,f6.2)

9114 format(//14x, 'DEPRESSED',5x,'DEPRESSED',3x,'DEPRESSED'&
   &/3x,'SOURCE',5x,'DEPTH',12x,'WTOP',7x,'WBOTTOM'&
   &/4x,'ID',7x,'(METERS)',5x,'(METERS)',5x,'(METERS)'&
   &/66(' -')/)


9115 format(1x,a12,1x,f6.2,2(8x,f6.2))

9086 format(//50x,'*** BUOYANT LINE SOURCE DATA ***'//15x,&
   &'NUMBER EMISSION RATE',5x,'FIRST COORD',8x,'SECOND COORD',5x,&
   &'BASE    RELEASE    URBAN   EMISSION RATE',&
   &/3x,'SOURCE',7x,'PART. ',a11,')',7x,'X',7x,'Y',11x,'X',7x,'Y',6x,&
   &'ELEV.   ','HEIGHT',5x,'SOURCE',2x,'SCALAR VARY',&
   &/4x,' ID         CATS.               ',&
   &1x,('(METERS) (METERS)'),3x,('(METERS) (METERS)'),1x,&
   &('(METERS) (METERS)'),16x,'BY'&
   &/66(' -')/)

99090 format(/10x,'DIFF IN AIR (M**2/SEC)     =',2x,e9.2)
99091 format(/10x,'DIFF IN WATER (M**2/SEC)   =',2x,e9.2)
99093 format(/10x,'LEAF LIPID RESIST (SEC/M)  =',2x,e9.2)
9094 format(/10x,'HENRY`S LAW COEFFICIENT    =',2x,e9.2)

   return
end subroutine prtsrc

subroutine prtbkg(isect)
!***********************************************************************
!                 PRTBKG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Summary of BACKGRND Data
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  Model Options and Keyword Summarys
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: isect, i1, i2, i3, ifr, idw
   character :: season(4)*6, months(12)*9, dayofweek(3)*8,&
   &dayofweek7(7)*8
! Unused: CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
! Unused: CHARACTER CNPD*5, CAZS*8
! Unused: INTEGER :: I, J, K, NL, INDC, INGRP

!     Variable Initializations
   data season /'WINTER','SPRING','SUMMER',' FALL '/
   data months /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',&
   &'MAY      ','JUNE     ','JULY     ','AUGUST   ',&
   &'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
   data dayofweek  /'WEEKDAY ','SATURDAY','SUNDAY  '/
   data dayofweek7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',&
   &'FRIDAY  ','SATURDAY','SUNDAY  '/
   modnam = 'PRTBKG'

!     Print User-specified Background Concetrations
   if (bflag(isect) == 'ANNUAL') then
      call header(iounit)
      write(iounit,9001) isect, BackUnits
      write(iounit,9006) backgrnd(1,1)
   else if (bflag(isect) == 'SEASON') then
      call header(iounit)
      write(iounit,9002) isect, BackUnits
      write(iounit,9004) (season(i1),i1=1,4)
      write(iounit,9006) (backgrnd(i1,isect),i1=1,4)
   else if (bflag(isect) == 'MONTH') then
      call header(iounit)
      write(iounit,9007) isect, BackUnits
      write(iounit,9008)
      write(iounit,9013)
      write(iounit,9010) (backgrnd(i1,isect),i1=1,12)
   else if (bflag(isect) == 'HROFDY') then
      call header(iounit)
      write(iounit,9011) isect, BackUnits
      write(iounit,9012)
      write(iounit,9013)
      write(iounit,9014) (i1,backgrnd(i1,isect),i1=1,24)
   else if (bflag(isect) == 'SEASHR') then
      call header(iounit)
      write(iounit,9018) isect, BackUnits
      write(iounit,9012)
      write(iounit,9013)
      do i1 = 1, 4
         ifr = (i1-1)*24
         write(iounit,9019) season(i1)
         write(iounit,9014) (i2,backgrnd(i2+ifr,isect),i2=1,24)
      end do
   else if (bflag(isect) == 'HRDOW') then
      call header(iounit)
      write(iounit,99218) isect, BackUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 3
         idw = (i1-1)*24
         write(iounit,99021) dayofweek(i1)
         write(iounit,99014) (i3,backgrnd(i3+idw,isect),i3=1,24)
      end do
   else if (bflag(isect) == 'HRDOW7') then
      call header(iounit)
      write(iounit,79218) isect, BackUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 7
         idw = (i1-1)*24
         write(iounit,99021) dayofweek7(i1)
         write(iounit,99014) (i3,backgrnd(i3+idw,isect),i3=1,24)
      end do
   else if (bflag(isect) == 'SHRDOW') then
      call header(iounit)
      write(iounit,99018) isect, BackUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 3
         idw = (i1-1)*96
         do i2 = 1, 4
            ifr = (i2-1)*24
            write(iounit,99019) season(i2), dayofweek(i1)
            write(iounit,99014) (i3,backgrnd(i3+ifr+idw,isect),&
            &i3=1,24)
         end do
      end do
   else if (bflag(isect) == 'SHRDOW7') then
      call header(iounit)
      write(iounit,79018) isect, BackUnits
      write(iounit,99012)
      write(iounit,99013)
      do i1 = 1, 7
         idw = (i1-1)*96
         do i2 = 1, 4
            ifr = (i2-1)*24
            write(iounit,99019) season(i2), dayofweek7(i1)
            write(iounit,99014) (i3,backgrnd(i3+ifr+idw,isect),&
            &i3=1,24)
         end do
      end do
   else if (bflag(isect) == 'MHRDOW') then
      do i1 = 1, 3
         call header(iounit)
         write(iounit,99118) isect, BackUnits
         write(iounit,99012)
         write(iounit,99013)
         idw = (i1-1)*288
         do i2 = 1, 12
            ifr = (i2-1)*24
            write(iounit,99020) months(i2), dayofweek(i1)
            write(iounit,99014) (i3,backgrnd(i3+ifr+idw,isect),&
            &i3=1,24)
         end do
      end do
   else if (bflag(isect) == 'MHRDOW7') then
      do i1 = 1, 7
         call header(iounit)
         write(iounit,79118) isect, BackUnits
         write(iounit,99012)
         write(iounit,99013)
         idw = (i1-1)*288
         do i2 = 1, 12
            ifr = (i2-1)*24
            write(iounit,99020) months(i2), dayofweek7(i1)
            write(iounit,99014) (i3,backgrnd(i3+ifr+idw,isect),&
            &i3=1,24)
         end do
      end do
   end if

9001 format(/31x,'* SECT',i1,' ANNUAL (NON-VARYING) BACKGROUND ',&
   &'CONCENTRATION (',a5,') *'/)
9002 format(/31x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY ',&
   &'SEASONALLY (',a5,') *'/)
9004 format(40x,4(a6,9x)/20x,40('- ')/)
9006 format(38x,4(e10.5,5x))
9007 format(/33x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY ',&
   &'MONTHLY (',a5,') *'/)
9008 format(7x,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',&
   &'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',&
   &'DECEMBER'/)
9010 format(5x,12(e9.3,1x))
9011 format(/20x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY ',&
   &'FOR EACH HOUR OF THE DAY (',a5,') *'/)
9012 format(5x,6('HOUR    BKGRND',6x))
99012 format(2x,8('HOUR   BKGRND',3x))
9013 format(1x,65('- ')/)
99013 format(1x,65('- '))
9014 format(4(5x,6(i3,3x,e10.5,4x)/))
99014 format(2(3x,8(i2,2x,e9.4,3x)/),3x,8(i2,2x,e9.4,3x))
! Unused: 9015 FORMAT(/17X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY ',
!     &       'WITH WIND SPEED (',A5,') *'/)
9018 format(/15x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' SEASONALLY AND DIURNALLY (SEASHR) (',a5,') *'/)
99018 format(/9x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW)',&
   &' (',a5,') *'/)
79018 format(/9x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7)',&
   &' (',a5,') *'/)
99118 format(/10x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW)',&
   &' (',a5,') *'/)
79118 format(/10x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7)',&
   &' (',a5,') *'/)
99218 format(/11x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' DIURNALLY AND BY DAY OF WEEK (HRDOW)',&
   &' (',a5,') *'/)
79218 format(/11x,'* SECT',i1,' BACKGROUND CONCENTRATIONS WHICH VARY',&
   &' DIURNALLY AND BY DAY OF WEEK (HRDOW7)',&
   &' (',a5,') *'/)
9019 format(59x,'SEASON = ',a6)
99019 format(46x,'SEASON = ',a6,';  DAY OF WEEK = ',a8)
99020 format(46x,'MONTH = ',a9,';  DAY OF WEEK = ',a8)
99021 format(46x,'DAY OF WEEK = ',a8)
! Unused: 9024 FORMAT(26X,6(1X,E12.5))
! Unused: 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

   return
end subroutine prtbkg

subroutine prtrec
!***********************************************************************
!                 PRTREC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Receptor Network Values
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To remove reference to Boundary
!                    Receptors - 4/1/2004
!
!        MODIFIED:   To Adjust Format Statement 9082 for Boundary
!                    Receptors - 9/29/92
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, indz, nx, ny, indc
   double precision :: ycoval, xrms, yrms, range, radial
   character :: buf132*132

!     Variable Initializations
   modnam = 'PRTREC'
   buf132 = ' '
   indz   = 0

   do i = 1, innet
      call header(iounit)
      write(iounit,9034)
      write(iounit,9037) ntid(i), nttyp(i)
      if (nttyp(i) == 'GRIDCART') then
         write(iounit,9038)
      else
         write(iounit,9036) xorig(i), yorig(i)
         write(iounit,9039)
      end if
      write(iounit,9040) (xcoord(j,i),j=1,numxpt(i))
      if (nttyp(i) == 'GRIDCART') then
         write(iounit,9041)
      else
         write(iounit,9042)
      end if
      write(iounit,9040) (ycoord(j,i),j=1,numypt(i))
      if (elev) then
!           Print Terrain Heights for Network
!           Set Number of Columns Per Page, NCPP
         ncpp = 9
!           Set Number of Rows Per Page, NRPP
         nrpp = 40
!           Begin LOOP Through Networks
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         nppx = 1 + int((numxpt(i)-1)/ncpp)
         nppy = 1 + int((numypt(i)-1)/nrpp)
         do nx = 1, nppx
            do ny = 1, nppy
               call header(iounit)
               write(iounit,9037) ntid(i), nttyp(i)
               write(iounit,9011)
               if (nx == nppx) then
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  end if
               else
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  end if
               end if
               write(iounit,9010)
               if (ny == nppy) then
                  do k = 1+nrpp*(ny-1), numypt(i)
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(azelev(indz+j-1),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(azelev(indz+j-1),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               else
                  do k = 1+nrpp*(ny-1), nrpp*ny
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(azelev(indz+j-1),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(azelev(indz+j-1),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               end if
            end do
         end do
!           Print Hill Height Scales for Network
!           Set Number of Columns Per Page, NCPP
         ncpp = 9
!           Set Number of Rows Per Page, NRPP
         nrpp = 40
!           Begin LOOP Through Networks
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         nppx = 1 + int((numxpt(i)-1)/ncpp)
         nppy = 1 + int((numypt(i)-1)/nrpp)
         do nx = 1, nppx
            do ny = 1, nppy
               call header(iounit)
               write(iounit,9037) ntid(i), nttyp(i)
               write(iounit,9012)
               if (nx == nppx) then
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  end if
               else
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  end if
               end if
               write(iounit,9010)
               if (ny == nppy) then
                  do k = 1+nrpp*(ny-1), numypt(i)
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(azhill(indz+j-1),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(azhill(indz+j-1),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               else
                  do k = 1+nrpp*(ny-1), nrpp*ny
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(azhill(indz+j-1),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(azhill(indz+j-1),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               end if
            end do
         end do
      end if
      if (flgpol) then
!           Print The Receptor Heights Above Ground for This Network
!           Set Number of Columns Per Page, NCPP
         ncpp = 9
!           Set Number of Rows Per Page, NRPP
         nrpp = 40
!           Begin LOOP Through Networks
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         nppx = 1 + int((numxpt(i)-1)/ncpp)
         nppy = 1 + int((numypt(i)-1)/nrpp)
         do nx = 1, nppx
            do ny = 1, nppy
               call header(iounit)
               write(iounit,9037) ntid(i), nttyp(i)
               write(iounit,9035)
               if (nx == nppx) then
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  end if
               else
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  end if
               end if
               write(iounit,9010)
               if (ny == nppy) then
                  do k = 1+nrpp*(ny-1), numypt(i)
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(azflag(indz+j-1),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(azflag(indz+j-1),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               else
                  do k = 1+nrpp*(ny-1), nrpp*ny
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(azflag(indz+j-1),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(azflag(indz+j-1),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               end if
            end do
         end do
      end if
   end do

   if (irstat(4)/=0 .or. irstat(8)/=0) then
! ---    Include EVALCART receptors with DISCCART receptors.
!        Print Out The Coordinates, Height , Hill Height & Flags For
!        Discrete Cart Receptors

      indc = 0
      do i = 1, numrec
         if (rectyp(i) == 'DC') then
            indc = indc + 1
            if (mod(indc-1,90) == 0) then
               call header(iounit)
               write(iounit,9043)
            end if
            if (mod(indc,2) /= 0) then
               write(buf132(1:65),9045) axr(i),ayr(i),azelev(i),&
               &azhill(i),azflag(i)
            else
               write(buf132(66:130),9045) axr(i),ayr(i),azelev(i),&
               &azhill(i),azflag(i)
               write(iounit,9090) buf132
               write(buf132,9095)
            end if
         end if
      end do
      if (mod(indc,2) /= 0) then
         write(iounit,9090) buf132
         write(buf132,9095)
      end if
   end if

   if (irstat(5) /= 0) then
!        Print Out The Coordinates, Height & Flags For Discrete Polar Receptors
      indc = 0
      do i = 1, numrec
         if (rectyp(i) == 'DP') then
            indc = indc + 1
            xrms = axr(i) - axs(iref(i))
            yrms = ayr(i) - ays(iref(i))
            range  = dsqrt(xrms*xrms + yrms*yrms)
            radial = datan2(xrms, yrms) * rtodeg
            if(radial <= 0.0d0) radial = radial + 360.0d0
            if (mod(indc-1,90) == 0) then
               call header(iounit)
               write(iounit,9044)
            end if
            if (mod(indc,2) /= 0) then
               write(buf132(1:65),9047) srcid(iref(i)),range,radial,&
               &azelev(i),azhill(i),azflag(i)
            else
               write(buf132(66:130),9047) srcid(iref(i)),range,&
               &radial,azelev(i),azhill(i),&
               &azflag(i)
               write(iounit,9090) buf132
               write(buf132,9095)
            end if
         end if
      end do
      if (mod(indc,2) /= 0) then
         write(iounit,9090) buf132
         write(buf132,9095)
      end if
   end if

9010 format(66(' -')/)
9011 format(/48x,'* ELEVATION HEIGHTS IN METERS *'/)
9012 format(/48x,'* HILL HEIGHT SCALES IN METERS *'/)
9013 format(2x,f10.2,1x,'|',1x,9(1x,f12.2,:))
9016 format(3x,' Y-COORD  |',48x,'X-COORD (METERS)')
9017 format(3x,' (METERS) |',1x,9(1x,f12.2,:))
9018 format(3x,'DIRECTION |',48x,'DISTANCE (METERS)')
9019 format(3x,'(DEGREES) |',1x,9(1x,f12.2,:))
9035 format(/44x,'* RECEPTOR FLAGPOLE HEIGHTS IN METERS *'/)
9034 format(/40x,'*** GRIDDED RECEPTOR NETWORK SUMMARY ***')
9036 format(/42x,'*** ORIGIN FOR POLAR NETWORK ***'/,&
   &32x,'X-ORIG =',f10.2,' ;   Y-ORIG = ',f10.2,'  (METERS)')
9037 format(/34x,'*** NETWORK ID: ',a8,' ;  NETWORK TYPE: ',&
   &a8,' ***')
9038 format(/42x,'*** X-COORDINATES OF GRID ***'/&
   &52x,'(METERS)'/)
9039 format(/42x,'*** DISTANCE RANGES OF NETWORK ***'/&
   &52x,'(METERS)'/)
9040 format(100(5x,10(f10.1,',')/))
9041 format(/42x,'*** Y-COORDINATES OF GRID *** ',&
   &/52x,'(METERS)'/)
9042 format(/42x,'*** DIRECTION RADIALS OF NETWORK *** ',&
   &/52x,'(DEGREES)'/)
9043 format(/45x,'*** DISCRETE CARTESIAN RECEPTORS ***',&
   &/43x,'(X-COORD, Y-COORD, ZELEV, ZHILL, ZFLAG)',&
   &/45x,'              (METERS)'/)
9044 format(/43x,'      *** DISCRETE POLAR RECEPTORS ***',&
   &/43x,' ORIGIN:    (DIST, DIR, ZELEV, ZHILL, ZFLAG)',&
   &/43x,' SRCID:    (METERS,DEG,METERS,METERS,METERS)'/)
9045 format(4x,' (',4(f9.1,', '),f9.1,'); ')
9047 format(1x,a12,': (',f9.1,', ',3(f7.1,', '),f7.1,'); ')
9090 format(a132)
9095 format(132(' '))

   return
end subroutine prtrec

subroutine chkrec
!***********************************************************************
!                 CHKREC Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Print Out The Input Met Data Summary and Source Groups
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include check for receptors beyond MAXDIST from
!                    sources, using center of AREA/AREACIRC/AREAPOLY and
!                    OPENPIT sources.  MAXDIST is set to 80km under the
!                    FASTALL and FASTAREA options.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To account for new area source algorithm, which
!                    allows for receptors located within the area - 7/7/93
!
!        MODIFIED:   To account for OpenPit Source - PES - 7/22/94
!
!        INPUTS:  Source and Receptor Inputs
!
!        OUTPUTS: Listing of Receptors Too Close To Sources
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: inc, inout
   double precision :: xsrc, ysrc, dist, xvm(5), yvm(5)

!     Variable Initializations
   modnam = 'CHKREC'
   inc = 0
   xsrc = 0.0d0
   ysrc = 0.0d0

!     Begin Source LOOP
   do isrc = 1, numsrc

!        Set Effective Source Radius Based on Source Type
      if (srctyp(isrc)(1:5) == 'POINT') then
         xrad = 0.0d0
         xsrc = axs(isrc)
         ysrc = ays(isrc)
      else if (srctyp(isrc) == 'VOLUME') then
         xrad = 2.15d0 * asyini(isrc)
         xsrc = axs(isrc)
         ysrc = ays(isrc)
      else if (srctyp(isrc)(1:4) == 'AREA' .or.&
      &srctyp(isrc) == 'LINE') then
!           Set XRAD to -1 since no minimum distance for
!           AREA and LINE source types.  Use center coordinates
!           for comparison to MAXDIST.
         xrad = -1.0d0
         xsrc = axcntr(isrc)
         ysrc = aycntr(isrc)
      else if (srctyp(isrc) == 'OPENPIT') then
!           Set XRAD to -1 since minimum distance for
!           OPENPIT source is handled separately to
!           flag receptors within the boundary of the
!           OPENPIT source.  Use center coordinates
!           for comparison to MAXDIST.
         xrad   = -1.0d0
         xsrc   = axcntr(isrc)
         ysrc   = aycntr(isrc)
         xvm(1) = axvert(1,isrc)
         xvm(2) = axvert(2,isrc)
         xvm(3) = axvert(3,isrc)
         xvm(4) = axvert(4,isrc)
         xvm(5) = axvert(5,isrc)
         yvm(1) = ayvert(1,isrc)
         yvm(2) = ayvert(2,isrc)
         yvm(3) = ayvert(3,isrc)
         yvm(4) = ayvert(4,isrc)
         yvm(5) = ayvert(5,isrc)
      end if

!        Begin Receptor LOOP
      do irec = 1, numrec

!           Calculate DIST From Source to Receptor
         x = axr(irec) - xsrc
         y = ayr(irec) - ysrc
         dist = dsqrt (x*x + y*y) - xrad

         if (dist < 0.99d0) then
!              Receptor Is Too Close To Source
            inc = inc + 1
            if (mod((inc-1), 40) == 0) then
               call header(iounit)
               write(iounit,9002)
            end if
            write(iounit,9003) srcid(isrc), axr(irec),&
            &ayr(irec), dist

         else if (dist > maxdist) then
!              Receptor Is Too Far From Source
            inc = inc + 1
            if (mod((inc-1), 40) == 0) then
               call header(iounit)
               write(iounit,9002)
            end if
            write(iounit,9003) srcid(isrc), axr(irec),&
            &ayr(irec), dist

         else if (srctyp(isrc) == 'OPENPIT') then
!              Check for receptors within boundary of an open pit source
            xr = axr(irec)
            yr = ayr(irec)
            call pnpoly(xr,yr,xvm,yvm,4,inout)
            if (inout > 0) then
!                 Receptor is within boundary
               inc = inc + 1
               if (mod((inc-1), 40) == 0) then
                  call header(iounit)
                  write(iounit,9002)
               end if
               write(iounit,9004) srcid(isrc), axr(irec),&
               &ayr(irec)
            end if
         end if

      end do
!        End Receptor LOOP

   end do
!     End Source LOOP

9002 format(/22x,&
   &'* SOURCE-RECEPTOR COMBINATIONS FOR WHICH CALCULATIONS ',&
   &'MAY NOT BE PERFORMED *'/24x,'LESS THAN 1.0 METER;',&
! --- PRIME ---------------------------------------------------------
   &' WITHIN OPENPIT; OR BEYOND 80KM FOR FASTAREA/FASTALL',//&
! -------------------------------------------------------------------
   &/30x,'SOURCE',10x,'- - RECEPTOR LOCATION - -',9x,'DISTANCE',&
   &/30x,'  ID  ',10x,'XR (METERS)   YR (METERS)',9x,'(METERS)',&
   &/28x,31('- ')/)
9003 format(29x,a12,3x,f13.1,1x,f13.1,5x,f12.2)
9004 format(29x,a12,3x,f13.1,1x,f13.1,5x,'     OPENPIT')

   return
end subroutine chkrec

subroutine prtmet(iount)
!***********************************************************************
!                 PRTMET Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Input Met Data Summary and Source Groups
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include output file unit argument to support
!                    output to main 'aermod.out' file and to the
!                    optional SUMMFILE.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To output 4-digit start year and end year for
!                    Y2K compliance.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Model Options and Keyword Summarys
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   INPSUM
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, iount

!     Variable Initializations
   modnam = 'PRTMET'

!     Start New Page and Print The Titles
   call header(iount)

!     Print The Meteorology Data Date Array;
!     use IPROC if ISYR is not Leap Year; or
!     use IPROCL if ISYR is a Leap Year
   if( L_LeapYear )then
      write(iount,9037) (iprocl(i),i = 1, 366)
   else
      write(iount,9037) (iproc(i),i = 1, 365)
   endif

   if (isdate /= 0 .or. iedate /= 2147123124) then
!        Write Out User-specified Start and End Dates
      write(iount,9038) isyr, ismn, isdy, ishr,&
      &ieyr, iemn, iedy, iehr
   end if

   write(iount,9039)

!     Print the upper bound of the first 5 wind speed categories
   write ( iount, 9001 ) (ucat(i), i=1,5)

9001 format(//34x,'*** UPPER BOUND OF FIRST THROUGH FIFTH WIND SPEED',&
   &' CATEGORIES ***'/60x,'(METERS/SEC)'//46x,5(f7.2,','))
9037 format(/44x,'*** METEOROLOGICAL DAYS SELECTED FOR PROCESSING ***'&
   &/63x,'(1=YES; 0=NO)'//8(11x,5(10i2,2x)/))
9038 format(/23x,'METEOROLOGICAL DATA PROCESSED BETWEEN START DATE: ',&
   &i4,1x,3i3,/59x,'AND END DATE: ',i4,1x,3i3)
9039 format(/16x,'NOTE:  METEOROLOGICAL DATA ACTUALLY PROCESSED WILL',&
   &' ALSO DEPEND ON WHAT IS INCLUDED IN THE DATA FILE.'/)

   return
end subroutine prtmet

subroutine rsinit
!***********************************************************************
!                 RSINIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Initialize Results Variables for Restart
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Use local variable, IRSDATE, to read 10-digit start
!                    date from re-start file.  Include checks on restart
!                    date being ealier than user-specified start date
!                    on STARTEND keyword, with fatal error message for
!                    INITFILE restarts and warning for MULTYEAR restarts.
!                    Also check for overlapping periods for MULTYEAR
!                    applications with start date from STARTEND keyword
!                    being earlier than start date from the MULTYEAR
!                    re-start file.  This condition results in a fatal
!                    error message.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Added arrays associated with post-1997 PM10
!                    processing.
!                    R.W. Brode, PES, Inc.,  5/12/99
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        INPUTS:  None
!
!        OUTPUTS: Initialized Variables
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, l, m
!     JAT D065 8/9/21
!     JSYR, JSMN, JSDY, AND JSHR NOT USED
!      INTEGER :: JSYR, JSMN, JSDY, JSHR
   integer :: irsdate

! JAT 06/22/21 D065
! REMOVE IDYMAX AS UNUSED VARIABLE; ALSO REMOVE ITS DATA initialization
!      INTEGER IDYMAX(12)

!     Variable Initializations
!      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
   modnam = 'RSINIT'

! --- Read "start date" from restart file, IRSDATE, based on last
!     hour processed in SAVEFILE; IRSDATE is full 10-digit value
!     based on 4-digit year
   read(irsunt,err=99,end=999) irsdate, ntothrs

! --- Adjust IRSDATE by adding 1 hour since IRSDATE will be used as the
!     first hour of data to process in the restarted run for INITFILE
!     JAT D065 8/9/21
!     JSYR, JSMN, JSDY, AND JSHR NOT USED
!      JSYR = IRSDATE/1000000
!      JSMN = (IRSDATE/10000) - (IRSDATE/1000000)*100
!      JSDY = (IRSDATE/100) - (IRSDATE/10000)*100
!      JSHR = IRSDATE - (IRSDATE/100)*100


! --- Compare "start date" from restart file, IRSDATE, to "start date"
!     from STARTEND keyword (IMSTAT(6)=1), ISDATE.
!     If IRSDATE < ISDATE, then issue fatal error message for INITFILE
!     restarts and warning message for MULTYEAR restarts;
!     IF IRSDATE > ISDATE for MULTYEAR restarts, then issue fatal error
!     message since this implies overlapping data periods.
   if (.not.multyr .and. imstat(6) == 1 .and.&
   &irsdate < isdate) then
!        Re-start date is less than start date based on STARTEND keyword
      call errhdl(path,modnam,'E','484','STARTEND')
      runerr = .true.
      go to 1000
   else if (multyr .and. imstat(6) == 1 .and.&
   &irsdate < isdate) then
      call errhdl(path,modnam,'W','485','STARTEND')

   else if (multyr .and. imstat(6) == 1 .and.&
   &irsdate > isdate) then
      write(dummy,'(I10.10)') irsdate - (irsdate/100000000)*100000000
      call errhdl(path,modnam,'E','486',dummy)
      runerr = .true.
      go to 1000

   else
!        Assign IRSDATE to ISDATE as start date for data processing
      isdate = irsdate
   end if

   read(irsunt,err=99,end=999) nhival, nmxval, numrec, numgrp,&
   &numave, numtyp

   if (nhival > 0) then
      read(irsunt,err=99,end=999) (((((hivalu(i,j,k,l,m),i=1,numrec),&
      &j=1,nhival),k=1,numgrp),l=1,numave),m=1,numtyp)
      read(irsunt,err=99,end=999) (((((nhidat(i,j,k,l,m),i=1,numrec),&
      &j=1,nhival),k=1,numgrp),l=1,numave),m=1,numtyp)
      read(irsunt,err=99,end=999) (((((hclmsg(i,j,k,l,m),i=1,numrec),&
      &j=1,nhival),k=1,numgrp),l=1,numave),m=1,numtyp)

! ---    Include arrays associated with multi-year processing of high
!        ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
      if (pm25ave .or. no2ave .or. so2ave) then
         read(irsunt,err=99,end=999) numyrs
         read(irsunt,err=99,end=999) (((sumhnh(i,j,k),i=1,numrec),&
         &j=1,numgrp),k=1,nhival)
         read(irsunt,err=99,end=999) (((himxdly(i,j,k),i=1,numrec),&
         &j=1,numgrp),k=1,nhival)
         read(irsunt,err=99,end=999) (((nhidatmxd(i,j,k),i=1,numrec),&
         &j=1,numgrp),k=1,nhival)
         read(irsunt,err=99,end=999) ((((himxdly_byyr(i,j,k,l),&
         &i=1,numrec),j=1,numgrp),k=1,nhival),l=1,numyrs)
         read(irsunt,err=99,end=999) ((((nhidatmxd_byyr(i,j,k,l),&
         &i=1,numrec),j=1,numgrp),k=1,nhival),l=1,numyrs)
      end if

   end if

   if (nmxval > 0) then
      read(irsunt,err=99,end=999) ((((rmxval(i,j,k,l),i=1,nmxval),&
      &j=1,numgrp),k=1,numave),l=1,numtyp)
      read(irsunt,err=99,end=999) ((((mxdate(i,j,k,l),i=1,nmxval),&
      &j=1,numgrp),k=1,numave),l=1,numtyp)
      read(irsunt,err=99,end=999) ((((mxloca(i,j,k,l),i=1,nmxval),&
      &j=1,numgrp),k=1,numave),l=1,numtyp)
      read(irsunt,err=99,end=999) ((((mclmsg(i,j,k,l),i=1,nmxval),&
      &j=1,numgrp),k=1,numave),l=1,numtyp)
   end if

   if (seasonhr) then
!        Initialize the SEASON by HOUR-OF-DAY Arrays
      read(irsunt,err=99,end=999) (((((shvals(i,j,k,l,m),i=1,numrec),&
      &j=1,numgrp),k=1,4),l=1,24),m=1,numtyp)
      read(irsunt,err=99,end=999) ((nseahr(i,j),i=1,4),j=1,24)
      read(irsunt,err=99,end=999) ((nseacm(i,j),i=1,4),j=1,24)
   end if

   if (period) then
      read(irsunt,err=99,end=999) ianhrs, ianclm, ianmsg, numyrs
      read(irsunt,err=99,end=999) (((annval(i,j,k),i=1,numrec),&
      &j=1,numgrp),k=1,numtyp)
      if (multyr) then
!           Reinitialize the ANNVAL(NUMREC,NUMGRP,NUMTYP) Array and Annual Counters
         annval = 0.0d0
         ianhrs = 0
         ianclm = 0
         ianmsg = 0
!           Read the Maximum Annual Values
         read(irsunt,err=99,end=999) (((amxval(i,j,k),i=1,nhiann),&
         &j=1,numgrp),k=1,numtyp)
         read(irsunt,err=99,end=999) (((imxloc(i,j,k),i=1,nhiann),&
         &j=1,numgrp),k=1,numtyp)
      end if
   else if (annual) then
      read(irsunt,err=99,end=999) ianhrs, ianclm, ianmsg, numyrs
      read(irsunt,err=99,end=999) (((annval(i,j,k),i=1,numrec),&
      &j=1,numgrp),k=1,numtyp)
      read(irsunt,err=99,end=999) (((sumann(i,j,k),i=1,numrec),&
      &j=1,numgrp),k=1,numtyp)
   end if

   go to 1000

!     WRITE Error Message:  Error Reading INITFILE
99 dummy = 'INITFILE'
   call errhdl(path,modnam,'E','510',dummy)
   runerr = .true.
   go to 1000

!     WRITE Error Message:  End of File Reached for INITFILE
999 dummy = 'INITFILE'
   call errhdl(path,modnam,'E','580',dummy)
   runerr = .true.

1000 return
end subroutine rsinit

subroutine resini
!***********************************************************************
!                 RESINI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Initialize Results Variables With Zeroes
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Replaced DO-loops with array assignment statements,
!                    and checked for allocation status of allocatable
!                    arrays.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Added results arrays for post-1997 PM10 processing
!                    option.  Also replaced labeled DO loop terminators
!                    with unlabeled END DO statements.
!                    R.W. Brode, PES, Inc.,  11/19/98
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        INPUTS:  None
!
!        OUTPUTS: Initialized Variables
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'RESINI'

!     Initialize the Results Arrays
   numhrs(:) = 0
   numclm(:) = 0
   nummsg(:) = 0
   if (allocated(hrval))  hrval  = 0.0d0
   if (allocated(aveval)) aveval = 0.0d0
   if (allocated(hivalu)) hivalu = 0.0d0
   if (allocated(nhidat)) nhidat = 0
   if (allocated(hclmsg)) hclmsg = ' '
   if (allocated(hmax))   hmax   = 0.0d0
   if (allocated(hmdate)) hmdate = 0
   if (allocated(hmloc))  hmloc  = 0
   if (allocated(hmclm))  hmclm  = ' '
   if (allocated(rmxval)) rmxval = 0.0d0
   if (allocated(mxdate)) mxdate = 0
   if (allocated(mxloca)) mxloca = 0
   if (allocated(mclmsg)) mclmsg = ' '
   if (allocated(backave)) backave(:) = 0.0d0
   if (allocated(backann)) backann(:) = 0.0d0
   if (allocated(backseashr)) backseashr(:,:,:) = 0.0d0
   if (allocated(backhr))  backhr(:,:) = 0.0d0

   ianhrs = 0
   ianclm = 0
   ianmsg = 0

!     The following were added as part of implementing the SCIM option
   nskiptot = 0

!     Initialize results arrays for ANNUAL/PERIOD processing; if allocated
   if (allocated(annval))  annval = 0.0d0
   if (allocated(sumann))  sumann = 0.0d0
   if (allocated(amxval))  amxval = 0.0d0
   if (allocated(imxloc))  imxloc = 0

!     Initialize results array for PM-2.5 processing; if allocated
   if (allocated(sumhnh))  sumhnh  = 0.0d0
   if (allocated(mxpmval)) mxpmval = 0.0d0
   if (allocated(mxpmloc)) mxpmloc = 0

!     Initialize results array for SEASONHR option; check for allocated first
   if (allocated(shvals)) shvals = 0.0d0
!     Initialize results array for MAXDAILY option; check for allocated first
   if (allocated(mxdval))  mxdval  = 0.0d0
   if (allocated(himxdly)) himxdly = 0.0d0
   if (allocated(himxdly_byyr)) himxdly_byyr = 0.0d0
   if (allocated(imxdhr))  imxdhr  = 0
   if (allocated(nhidatmxd)) nhidatmxd = 0
   if (allocated(nhidatmxd_byyr)) nhidatmxd_byyr = 0

   nseahr = 0
   nseacm = 0

   return
end subroutine resini
