subroutine evcalc
!***********************************************************************
!                 EVCALC Module of AERMOD EVENT Option
!
!        PURPOSE: Controls Flow and Processing of CALCulation Modules
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED: Preparation work for multiple buoyant lines
!                  (Multiple_BuoyLines_D41_Wood)
!
!        MODIFIED: Modified to call RLCALC for RLINE and RLINEXT sources.
!                   Wood, 03/18/2019
!
!        MODIFIED:  Added code to call RLCALC for a RLINE source.
!                   Wood, 7/20/2018
!
!        MODIFIED:  Added code to call BL_CALC, buoyant line source
!                   Amec Foster Wheeler,  03/31/2016
!
!        MODIFIED:  Modified to include user-specified background
!                   concentrations through the SO BACKGRND option.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:  To set NUMREC = 1 and use PCALC, VCALC, ACALC, and
!                   OCALC subroutines.  R.W. Brode, PES, Inc. - 12/2/98
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
!
!        CALLED FROM:   EVLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   use rline_data, only: rlprocessed
   use buoyant_line, only: numblgrps, bl_grpid
   use buoyant_line, only: bl_uref, bl_rflag              !D097 Wood 9/30/22 3/29/23
   implicit none

   logical   :: blprocessed
   integer   :: kk
   character :: modnam*12

!     Variable Initializations
   modnam = 'EVCALC'
   path   = 'CN'

! --- Set the bouyant line source flag to false to indicate that no
!      lines have been processed for this hour since all lines must be
!      processed together
   blprocessed = .false.

! --- Set the RLINE source flag to false to indicate that no
!      RLINES have been processed for this hour since rotation of
!      receptors only needs to happen for first source.
   rlprocessed = .false.

! --- Set NUMREC = 1 to allow use of PCALC, VCALC, ACALC, OCALC,
!     ARM2_CALC, OLM_CALC, PVMRM_CALC, GRSM_CALC subroutines for EVENT processing
   numrec = 1

! --- Assign IGRP for this event
   igrp = idxev(ievent)

!     Assing METHDR = .TRUE. to print source&receptor-independent
!     meteorology debug information to METEOR debug output file.
   methdr = .true.

!     Begin Source LOOP
   source_loop: do isrc = 1, numsrc
! ---    Proceed with calcs if ISRC is included in IGRP, or if NO2
!        options are being used since these require full CHI array
      if (igroup(isrc,igrp) == 1 .or. arm2 .or.&
      &olm .or. pvmrm .or. grsm) then
         if (srctyp(isrc)(1:5) == 'POINT') then
!              Calculate Point Source Values                        ---   CALL PCALC
            call pcalc
         else if (srctyp(isrc) == 'VOLUME') then
!              Calculate Volume Source Values                       ---   CALL VCALC
            call vcalc
         else if (srctyp(isrc)(1:4) == 'AREA' .or.&
         &srctyp(isrc) == 'LINE') then
!              Calculate AREA/AREAPOLY/AREACIRC/LINE Source Values  ---   CALL ACALC
            call acalc
         else if ((srctyp(isrc) == 'RLINE') .or.&
         &(srctyp(isrc) == 'RLINEXT')) then
!              Calculate RLINE Source Values                        ---   CALL RLCALC
            call rlcalc
            rlprocessed = .true.
         else if (srctyp(isrc) == 'OPENPIT') then
!              Calculate OpenPit Source Values                      ---   CALL OCALC
            call ocalc
!              D097 WSP 3/29/2023 removed the lmitor of BLPROCESSED being FALSE following RLINE example
!            ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE') THEN
         else if (srctyp(isrc) == 'BUOYLINE' .and.&
         &(.not. blprocessed)) then
!              Calculate Bouyant Line Source Values                 ---   CALL BL_CALC
!              BLPROCESSED lets AERMOD know that all lines associated with
!               the buoyant line source were processed on the first pass
!               through the sources
!
!           D097 Wood 9/30/22
!           The original BLP used meteorology from the met processor CRSTER.
!           CRSTER did not allow wind speeds to be less than 1 m/s.  Set the
!           reference wind speed used in the calculations below to be a minimum
!           of 1.0 m/s.  Use the local variable BL_UREF.  Keep the reference
!           wind speed height as UREFHT.
            if (uref >= 1.0d0) then
               bl_uref = uref
            else
               bl_uref = 1.0d0
!             WRITE Message              ! Ref ht wind speed less than minimum
               write(dummy,'(''on '',I8.8)') kurdat
               call errhdl(path,modnam,'W','471',dummy)
            end if

! Multiple_BuoyLines_D41_Wood
!              Determine which BL source group to process
            if (evgrp(ievent) == 'ALL') then
!                 Process all the buoyant line source group events
               do kk = 1,numblgrps
                  call blevrecp(ievent,kk)
                  call bl_calc(kk)
               end do
               blprocessed = .true.

            else
!                 Process the individual buoyant line source group events
               do kk = 1,numblgrps
                  if (evgrp(ievent) == bl_grpid(kk)) then
                     bl_rflag = .false.                          !D097 WSP 3/29/23 set BL_RFLAG to false
                     call blevrecp(ievent,kk)
                     call bl_calc(kk)
                     exit
                  endif
               end do
               blprocessed = .true.
            end if                     ! EVGRP(IEVENT) .EQ. 'ALL'
         end if                        ! SRCTYP(ISRC) .EQ. 'BUOYLINE'
      end if                           ! IGROUP(ISRC,IGRP) .EQ. 1
   end do source_loop
!     End Source LOOP

   if (l_backgrnd .and. .not.arm2 .and.&
   &.not.olm .and. .not.pvmrm .and. .not.grsm) then
! ---    User-specified background concentrations are included;
!        add to modeled concentrations by source group unless
!        NO2 options are specified (which are handled separately)
      call ev_sumback
   end if

   return
end subroutine evcalc

subroutine ev_sumval
!***********************************************************************
!                 EV_SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Modified to include GRPAVE variable to account for
!                   user-specified background for the full averaging
!                   period based user-specified background concentrations
!                   through the SO BACKGRND option.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        INPUTS:  HRVAL - Hourly Value for (IHOUR,ISRC) Combination
!                 Averaging Period Options
!                 Source Groupings
!
!        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!                       OCALC
!                       RLCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EV_SUMVAL'

   hrvals(ihour,isrc) = hrval(1)
   ev_aveval(isrc)    = ev_aveval(isrc) + hrval(1)
! --- Update GRPAVE and GRPVAL, using IDXEV to identify
!     IGRP for this EVENT
   grpave(idxev(ievent)) = grpave(idxev(ievent)) + hrval(1)
   grpval(idxev(ievent),ihour) =&
   &grpval(idxev(ievent),ihour) + hrval(1)

   return
end subroutine ev_sumval

subroutine ev_sumback
!***********************************************************************
!                 EV_SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums Background Values to AVEVAL and ANNVAL Arrays
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:
!
!        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   double precision :: bckgrd
   character :: modnam*12
!     RCO 7/27/20 ADDED FROM 19191
!     TWO MODIFICATIONS
!     1.  Multiply BGCONC by ratio of EMIFAC/1.0E6 to account for the
!         that background concentrations are in
!         micrograms/m^3 for internal calculations
!         but modeled output may be in other units
!         such as ppb.  This division puts the
!         background in the same units as the modeled
!         concentrations before adding them.
!     2.  Only add background to the concentration output
!         type, i.e. ityp=1.  Background concentration (ug/^3)
!         should not be added to deposition (g/m^2)
!     Variable Initializations
   modnam = 'EV_SUMBACK'
   bckgrd = 0.0d0
   ityp=1
   bckgrd = ev_bgconc(ihour)
!     IGRP has been assigned for this EVENT in sub_EVCALC
   if (grp_back(idxev(ievent))) then
      grpval(idxev(ievent),ihour) = grpval(idxev(ievent),ihour) +&
      &bckgrd*emifac(ityp)/1.0d6
      backhr(idxev(ievent),ihour) = backhr(idxev(ievent),ihour) +&
      &bckgrd*emifac(ityp)/1.0d6
      grpave(idxev(ievent))  = grpave(idxev(ievent))  +&
      &bckgrd*emifac(ityp)/1.0d6
      backave(idxev(ievent)) = backave(idxev(ievent)) +&
      &bckgrd*emifac(ityp)/1.0d6
   end if

   return
end subroutine ev_sumback

subroutine evloop
!***********************************************************************
!                 EVLOOP Module of AERMOD EVENT Option
!
!        PURPOSE: Controls Main Calculation Loop Through Events
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To remove mixed-mode math in calculation of
!                    IENDHR - 4/19/93
!
!        INPUTS:  Source, Receptor and Setup Options
!
!        OUTPUTS: Update Hourly Results
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: ievyr, iastat
   double precision :: GRPAVE_Test
   logical :: fopen, l_firstcall
   logical, allocatable :: l_evt_proc(:)

!     Variable Initializations
   modnam = 'EVLOOP'
   data l_firstcall/.true./
   GRPAVE_Test = 0.0d0

! --- Allocate L_EVT_PROC array to track whether an event has
!     already been processed, and initialize as .FALSE.
   if( .not.allocated(l_evt_proc) )then
      allocate( l_evt_proc(numeve), stat=iastat)
      l_evt_proc(:) = .false.
   endif
! --- Initialize other logicals
   eof   = .false.
   fopen = .false.

!     Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
   call ev_flush

! --- Assign FULLDATE and IEDATE with hour = 00 to
!     FULL_YYMMDD, IEDATE_YYMMDD local variables,
!     since hourly events within the same day may
!     not be in sequence
   full_yymmdd = (fulldate/100) * 100
   iedate_yymmdd = (iedate/100) * 100

   day_loop: do while (full_yymmdd < iedate_yymmdd .and. .not.eof)
!        Retrieve Hourly Meteorology Data for Current Day   ---   CALL MEREAD
      call meread

! ---    Check for runtime errors from MEREAD and exit DAY_LOOP if found
      if( runerr )then
         write(*,901) jday, iyr
901      format('+','MEREAD Error For Day No. ',i4,' of ',i4)
         exit day_loop
      endif

      full_yymmdd = (fulldate/100) * 100
      fulldate = full_yymmdd + 24

!        Check for Hourly Emissions File
      inquire (unit=ihremi,opened=fopen)
      if (fopen) then
!*          Retrieve Hourly Emissions from File for Current Day---   CALL EV_HRQREAD
!*          Set ILINE = 1 if L_FIRSTCALL for determining whether
!           VOLUME and AREA source inputs include hourly sigmas
         if (l_firstcall) iline = 1
         call ev_hrqread(l_firstcall)
      end if

      if (l_backgrnd) then
!-----      Extract BACKGRND concentrations for the current day, if available
         call bgread
      end if

! ---    Check for runtime errors from BGREAD and exit DAY_LOOP if found
      if( runerr )then
         write(*,902) jday, iyr
902      format('+','BGREAD Error For Day No. ',i4,' of ',i4)
         exit day_loop
      endif

      if (pvmrm .or. olm .or. grsm) then
!-----      Extract Ozone Data for current day; L_FIRSTCALL used
!           to initialize array of O3 values used to apply minimum
!           O3 for stable hours
         call o3read
      end if

      if (grsm) then
!-----      CERC 11/30/20 Extract NOx Data for current day
         call noxread
      end if

! ---    Check for runtime errors from O3READ and exit DAY_LOOP if found
      if( runerr )then
         write(*,903) jday, iyr
903      format('+','O3READ Error For Day No. ',i4,' of ',i4)
         exit day_loop
      endif

! ---    Set L_FIRSTCALL to .F.
      l_firstcall = .false.

!        Write Out Update to the Screen for the PC Version
      write(*,909) jday, iyr
909   format('+','Now Processing Events For Day No. ',i4,' of ',i4)

      ev_loop: do ievent = 1, numeve
! ---       Loop through Events and process Events that
!           occur on this day

! ---       Calculate year of event to account for multiple
!           year data files
         ievyr = int(evdate(ievent)/1000000)
         if( l_evt_proc(ievent) )then
! ---          Event has already been processed;
!              cycle to next event
            cycle ev_loop
         else if (ievyr < iyear) then
! ---          Event year is less than current met year;
!              cycle to next event
            cycle ev_loop
         else if (ievyr > iyear) then
! ---          Event year is greater than met year;
!              cycle EV_LOOP - event will be processed later
            cycle ev_loop
         else if (evjday(ievent) == jday) then
! ---          Event occurs on this day and year;
! ---          process this event and assign logical
!              indicating that event has been processed
            l_evt_proc(ievent) = .true.

            if( l_backgrnd )then
! ---             Reinitialize BACKAVE array for this event
               backave(:) = 0.0d0
            endif
            if( evonly )then
! ---             Reinitialize GRPAVE array for this event
               grpave(:) = 0.0d0
            endif

! ---          Assign start and end hour of the event
            iendhr = evdate(ievent) -&
            &int(evdate(ievent)/100)*100
            istahr = iendhr - evaper(ievent) + 1

! ---          Loop through hours for this event
            do ihour = istahr, iendhr
! ---             Assign IHOUR to KHOUR, used for profile met data
               khour = ihour

!                 Time/Date Marker for DEBUG Output
               if (debug) then
                  write(dbgunt,*)
                  write(dbgunt,*) '--------------------------------',&
                  &'--------------------------------'
                  write(dbgunt,*) '---  IEVENT, JDAY, IHOUR =  ',&
                  &ievent, jday, ihour
                  write(dbgunt,*) '--------------------------------',&
                  &'--------------------------------'
               end if

! ---             Assign O3MISS logical for this hour
               o3miss = l_ao3miss(ihour)
! ---             Assign NOXMISS logical for this hour
               noxmiss = l_anoxmiss(ihour)
!                 Retrieve Hourly Data for Current Event ---   CALL EV_METEXT
               call ev_metext
!                 Retrieve Hourly Ozone Value
               if (pvmrm .or. olm .or. grsm) then
                  if (.not. o3miss) then
                     o3conc = ev_o3conc(ihour)
                  else
                     o3conc = -999.0d0
                  end if
               end if
!                 CERC 11/30/20 Retrieve Hourly NOx Value
               if (grsm) then
                  if (.not. noxmiss) then
                     noxbgconc = ev_noxconc(ihour)
                  else
                     noxbgconc = -999.0d0
                  end if
               end if
!*                Process Hourly Emissions from File, if needed
               if (hourly) then
!*                   Begin Source Loop
                  do isrc = 1, numsrc
                     if (qflag(isrc) == 'HOURLY') then
!*                        Retrieve Source Parameters for This Hour  ---   CALL HRQEXT
                        call hrqext(isrc)
                     end if
                  end do
!*                   End Source Loop
               end if
!*----
               if (clmhr .and. clmpro) then
!                    Check for Calm Hr & Processing and
!                    Increment Counters
                  ev_numhrs = ev_numhrs + 1
                  ev_numclm = ev_numclm + 1
               else if (msghr .and. msgpro) then
!                    Check for Missing Hour & Processing and
!                    Increment Counters
                  ev_numhrs = ev_numhrs + 1
                  ev_nummsg = ev_nummsg + 1
               else if (zi <= 0.0d0) then
!                    Write Out The Informational Message &
!                    Increment Counters
                  write(dummy,'(I8.8)') kurdat
                  call errhdl(path,modnam,'I','470',dummy)
                  ev_numhrs = ev_numhrs + 1
               else
!                    Set CALCS Flag, Increment Counters
!                    & Calculate HRVAL
                  calcs = .true.
                  ev_numhrs = ev_numhrs + 1
!                    Calculate CONC or DEPOS Values      ---   CALL EVCALC
                  call evcalc
               end if

               if (.not.clmhr .and. .not.msghr) then
! ---                Non-calm, non-missing hour; apply NO2 options as appropriate

                  if (pvmrm .and. .not.psdcredit) then
! ---                   Process Hourly Values for PVMRM Option
                     call pvmrm_calc('ALLSRCS')

                  else if (pvmrm .and. psdcredit) then
! ---                   Process Hourly Values for PVMRM Option and PSD credits
! ---                   Need to process two separate sets of sources - the
!                       increment consumption sources ('NAAQSRC') and the
!                       increment expanding sources ('ALLBASE')
                     call pvmrm_calc('NAAQSRC')
                     call pvmrm_calc('ALLBASE')

                  else if (olm) then
! ---                   Process Hourly Values for OLM Option
                     call olm_calc

                  else if (arm2) then
! ---                   Process Hourly Values for ARM2 Option
                     call arm2_calc

                  else if (grsm) then
! ---                   CERC 11/30/20 Process Hourly Values for GRSM Option
                     call grsm_calc

                  end if

               end if

            end do
!              End Hourly LOOP

!              Calculate Applicable Averages             ---   CALL AVEREV
            call averev

!              Print Out Model Results                   ---   CALL OUTPUT
            call ev_output

! ---          Compare calculated EVENT concentration (GRPAVE) to "original"
!              EVENT concentration included on EVENTPER keyword (EV_OrigConc)
            if( EV_OrigConc(ievent) > 0.0d0 ) then
! ---             Since "original" EVENT concentration is read from input file
!                 with 5 decimal places, first round the internal results to
!                 5 decimal places to avoid spurious messages if the original
!                 concentration is less than 10.0.
               if( grpave(idxev(ievent)) < 10.0d0 )then
                  GRPAVE_Test = dble(idnint(1.0d5 *&
                  &grpave(idxev(ievent))))/&
                  &1.0d5
               else
! ---                Use original value for comparison
                  GRPAVE_Test = grpave(idxev(ievent))
               end if

               if( dabs((EV_OrigConc(ievent)-GRPAVE_Test)/&
               &EV_OrigConc(ievent)) > 2.0d-6 )then
!                   WRITE Warning Message
                  call errhdl(path,modnam,'W','497',evname(ievent))
! ---               Assign logical flag indicating the EVENT consistency warning
!                   has been issued; a warning to the default output unit will be
!                   issued at the end of the run.
                  L_EVENT_OrigConc_Warning = .true.
               end if
            end if

!              Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
            call ev_flush

!              Reset CALCS Flag
            calcs = .false.

!              Reset the Counters
            ev_numhrs = 0
            ev_numclm = 0
            ev_nummsg = 0

         end if   ! IF-ENDIF block on events for this JDAY

      end do ev_loop
!        End Event LOOP

   end do day_loop
!     End Loop Through Meteorology Data

   return
end subroutine evloop

subroutine meread
!***********************************************************************
!                MEREAD Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Controls Extraction and Quality Assurance of
!                 One Day of Meteorological Data for EVENT Processing
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Modified met data arrays to include an additional
!                   array index, since these arrays are also used by
!                   the OU MAXDCONT post-processing option.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:  Modified code for processing multi-year data
!                   files to determine if header record is present
!                   between years for concatenated files.  Use presence
!                   of colon (':') as only criterion for header record.
!                   Use warning messages if UAIR and SURF IDs don't
!                   match input runstream file for multiple years since
!                   AERMOD allows mismatch for single year files.
!                   Modified check for stable or missing hours in
!                   calculation of solar irradiance (QSW) for use
!                   in deposition calculations.
!                   Modified to check first hour of met data files
!                   to determine if file starts on hour 01. If not,
!                   cycle through hour loop until loop index matches
!                   hour in data file.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:  Modified code for reading the header record of the
!                   surface file to use a character variable for the
!                   AERMET version date field, in order to allow for
!                   the future use of screening meteorology that is not
!                   directly linked to a specific version under the
!                   of the AERMET processor under the SCREEN option.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!        MODIFIED:  To assign non-array logicals STABLE and UNSTAB
!                   for use in subroutine COMPTG.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
!
!        MODIFIED:  To remove support for unformatted meteorological
!                   data files.
!                   R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:  To incorporate modifications to date processing
!                   for Y2K compliance, including use of date window
!                   variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                   of 10-digit date variable (FULLDATE) with 4-digit
!                   year for date comparisons.
!                   Also modified calls to METDAT insteaad of EV_METDAT
!                   to allow use of same routine for both normal and
!                   EVENT processing.
!                   R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Meteorology File Specifications
!
!        OUTPUTS: Arrays of Meteorological Variables for One Day
!
!        CALLED FROM:   EVLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!---- Constants used in the computation of QSW
   double precision, parameter :: c1=5.31d-13, c2=60.0d0, c3=1.12d0,&
   &stefb= 5.67d-08
   double precision :: rn, Es25, fvref

   integer :: i, ihr, ijday, idatchk, iusi, issi,&
   &jflag, level
! --- Declare integer variable to store the current date in MEREAD
   integer :: MEREAD_Date

   character (len=8)   :: cusi, cssi
   character (len=256) :: buffer

   save MEREAD_Date

!     Variable Initializations
   modnam = 'MEREAD'
   data MEREAD_Date/0/
   path   = 'MX'
   idatchk = 0

!     READ Meteorology Data Based on Format --
!     When DRY deposition is modeled, U-star, L, and z0 (surface
!     roughness length) are read in addition to the standard
!     data.
!
!     When WET deposition is modeled, ipcode (precip.
!     code) and prate (precip. rate in mm/hr) must also be added to
!     each hourly record.
!     The format statement allows for all additional data:

! --- Calculate the MMDDHH variable to check for end of the year
!     based on MEREAD_Date
   idatchk = MEREAD_Date - int(MEREAD_Date/1000000)*1000000

   if ((imonth==12 .and. iday==31 .and. ihour==24) .or.&
   &idatchk == 123124) then
!        End of year has been reached - check for presence of header
!        record at beginning of next year for multi-year data files.
      read(mfunit,'(A256)',err=998,end=1000,iostat=ioerrn) buffer

! ---    First check for ':' as indicator of header record, then extract
!        AERMET version date, C_METVER, and station IDs
      if (index(buffer,':') == 0) then
!           Record does not contain colon. Assume it must be regular
!           met data record, so backspace met file before proceeding.
         backspace mfunit
      else
!           Record contains colons. Assume it is a header record, and
!           extract AERMET version date, C_METVER, and check station
!           IDs before proceeding in order to flag potential for use
!           of different stations in multi-year data files.
         if( index(buffer,'VERSION:') /= 0 )then
!              Extract AERMET version date from embedded header record
            read(buffer(index(buffer,'VERSION:')+8:&
            &index(buffer,'VERSION:')+13),'(A6)')&
            &c_metver
         elseif( buffer(93:98) /= '      ' )then
!              The 'VERSION:' keyword is missing from header so assign columns
!              93-98 to C_METVER
            c_metver = buffer(93:98)
         else
!              AERMET version not found in header record, issue fatal error message
            call errhdl(path,modnam,'E','395','No Version')
         endif

! ---       Read Lat/Lon from header record BUFFER
         read(buffer,1900,err=99,iostat=ioerrn) alat, alon
1900     format(2a10)

! ---       Now extract UA, SF, and OS station IDs from header record
         if( index(buffer,'UA_ID:') >= 0 )then
            read(buffer(index(buffer,'UA_ID:')+7:&
            &index(buffer,'UA_ID:')+15),'(A)') cusi
         else
            cusi = '        '
         end if
         call stonum(cusi,8,fnum,imit)
         if (imit == 1) then
            iusi = nint(fnum)
         else
            iusi = 0
         end if

         if( index(buffer,'SF_ID:') >= 0 )then
            read(buffer(index(buffer,'SF_ID:')+7:&
            &index(buffer,'SF_ID:')+15),'(A)') cssi
         else
            cssi = '        '
         end if
         call stonum(cssi,8,fnum,imit)
         if (imit == 1) then
            issi = nint(fnum)
         else
            issi = 0
         end if

         if (issi /= idsurf) then
!              Write Warning Message:  SURFDATA id mismatch
            call errhdl(path,modnam,'W','530','SURFDATA')
         end if
         if (iusi /= iduair) then
!              Write Warning Message:  UAIRDATA id mismatch
            call errhdl(path,modnam,'W','530','UAIRDATA')
         end if
      end if

      go to 1001

!        Error reading 'header record' - assume that header record is
!        missing.  Backspace met file and continue processing.
998   backspace mfunit

   end if

1001 continue

   hour_loop: do ihr = 1, nhr
!
!---- READ surface scaling meteorology data based on format
!
      if( ldpart .or. lwpart .or. ldgas .or. lwgas .or. grsm )then
!        Read record from ASCII scalar parameter file using FREE format
!        with deposition variables
!
! ---    First read date variables to check for problems
         read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
         &imonth, iday, ijday, ihour

!        D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
         call cent_date(iyear,iyr)
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Determine The Current Julian Day and Calculate Current Gregorian Date
!C        First Convert Year to 4-Digit Value
!         IF (IYEAR .GE. ISTRT_WIND .and. IYEAR .LE. 99) THEN
!            IYR = ISTRT_CENT*100 + IYEAR
!         ELSE IF (IYEAR .LT. ISTRT_WIND) THEN
!            IYR = (ISTRT_CENT+1)*100 + IYEAR
!         ELSE
!C           Input IYEAR must be 4-digit:  Save to IYR and convert to 2-digit
!            IYR   = IYEAR
!            IYEAR = IYR - 100 * (IYR/100)
!         END IF

         MEREAD_Date = iyr*1000000 + imonth*10000 + iday*100 + ihour
!
         if (ihour == ihr) then
! ---       Data file hour matches loop hour; backspace and read full record
            backspace mfunit
            read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
            &imonth, iday, ijday, ihour, asfchf(ihr,1), austar(ihr,1),&
            &awstar(ihr,1),avptgzi(ihr,1),aziconv(ihr,1),azimech(ihr,1),&
            &aobulen(ihr,1), asfcz0(ihr,1),abowen(ihr,1),aalbedo(ihr,1),&
            &auref(ihr,1), awdref(ihr,1), aurefht(ihr,1), ata(ihr,1),&
            &atrefht(ihr,1), iapcode(ihr,1), aprate(ihr,1), arh(ihr,1),&
            &asfcp(ihr,1), nacloud(ihr,1)
         else if (ihour > ihr) then
! ---       Data file starts after hour 01;
!           Issue warning, backspace file and skip to Profile file
            write(dummy,'(2X,3I2)',err=99) imonth, iday, ihr
            call errhdl(path,modnam,'W','489',dummy)
            backspace mfunit
            go to 888
         else
! ---       Data file hour is less than loop hour;
!           could be problem with data file or use of 00-23 hour convention
!           Issue error message:
            write(dummy,'(2X,3I2)',err=99) imonth, iday, ihour
            call errhdl(path,modnam,'E','490',dummy)
            exit hour_loop
         end if

!        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
!        albedo and cloud cover, for use in gas deposition algorithm.
!        Include check for ABOWEN < 0 for non-standard inputs.
         if (aobulen(ihr,1)>0.0d0 .or. aobulen(ihr,1)<-99990.0d0&
         &.or. ata(ihr,1)<0.0d0 .or.&
         &aalbedo(ihr,1)==1.0d0 .or. abowen(ihr,1)<=0.0d0) then
!           Hour is stable or missing or inappropriate surface chars.
            aqsw(ihr,1) = 0.0d0
         else
            rn = (1.d0 + 1.d0/abowen(ihr,1))*asfchf(ihr,1)/0.9d0
            aqsw(ihr,1) = (rn*(1.d0+c3)-c1*ata(ihr,1)**6+&
            &stefb*ata(ihr,1)**4 -&
            &c2*0.1d0*dble(nacloud(ihr,1))) /&
            &(1.d0-aalbedo(ihr,1))
         end if
!
!        Save precipitation rates for two previous hours
         if (ihr == 1) then
            Aprec2(ihr,1) = APrate(nhr-1,1)
            Aprec1(ihr,1) = APrate(nhr,1)
         else if (ihr == 2) then
            Aprec2(ihr,1) = APrate(nhr,1)
            Aprec1(ihr,1) = APrate(ihr-1,1)
         else
            Aprec2(ihr,1) = APrate(ihr-2,1)
            Aprec1(ihr,1) = APrate(ihr-1,1)
         end if

!        Set variables for dry deposition
         if (ldpart .or. ldgas) then
            if (ata(ihr,1)<0.0d0 .or. aprate(ihr,1)<0.0d0) then
               awnew(ihr,1) = awold(ihr,1)
            else
! ...          Compute saturation vapor pressure based on CMAQ formula
               AEsTa(ihr,1) = 0.6112d0*dexp(19.83d0 -&
               &5417.4d0/ata(ihr,1))
               Es25 = 3.167d0
               AWnew(ihr,1) = Wold+APrec1(ihr,1)-&
               &0.5d0*f2*AEsTa(ihr,1)/Es25
               Wold = AWnew(ihr,1)
               Af2(ihr,1) = AWnew(ihr,1)/200.d0
               if (Af2(ihr,1)<=0.01d0) Af2(ihr,1) = 0.01d0
               if (Af2(ihr,1)>1.0d0) Af2(ihr,1) = 1.0d0
               f2 = Af2(ihr,1)
            end if
         end if

      else
!        Read record from ASCII scalar parameter file without deposition
!        parameters, using FREE format
!
! ---    Calculate the MMDDHH variable to check for end of the year
!        based on MEREAD_Date
         idatchk = MEREAD_Date - int(MEREAD_Date/1000000)*1000000
         if ((imonth==12 .and. iday==31 .and. ihour==24) .or.&
         &idatchk == 123124) then
!           End of year has been reached - check for presence of header
!           record at beginning of next year for multi-year data files.
            read(mfunit,'(A256)',err=9981,end=1000,iostat=ioerrn) buffer

! ---       First check for ':' as indicator of header record, then extract
!           AERMET version date, C_METVER, and station IDs
            if (index(buffer,':') == 0) then
!              Record does not contain colon. Assume it must be regular
!              met data record, so backspace met file before proceeding.
               backspace mfunit
            else
!              Record contains colons. Assume it is a header record, and
!              extract AERMET version date, C_METVER, and check station
!              IDs before proceeding in order to flag potential for use
!              of different stations in multi-year data files.
               if( index(buffer,'VERSION:') /= 0 )then
!                 Extract AERMET version date from embedded header record
                  read(buffer(index(buffer,'VERSION:')+8:&
                  &index(buffer,'VERSION:')+13),'(A6)')&
                  &c_metver
               elseif( buffer(93:98) /= '      ' )then
!                 The 'VERSION:' keyword is missing from header so assign columns
!                 93-98 to C_METVER
                  c_metver = buffer(93:98)
               else
!                 AERMET version not found in header record, issue fatal error message
                  call errhdl(path,modnam,'E','395','No Version')
               endif

! ---          Read Lat/Lon from header record BUFFER
               read(buffer,1900,err=99,iostat=ioerrn) alat, alon

! ---          Now extract UA, SF, and OS station IDs from header record
               if( index(buffer,'UA_ID:') >= 0 )then
                  read(buffer(index(buffer,'UA_ID:')+7:&
                  &index(buffer,'UA_ID:')+15),'(A)') cusi
               else
                  cusi = '        '
               end if
               call stonum(cusi,8,fnum,imit)
               if (imit == 1) then
                  iusi = nint(fnum)
               else
                  iusi = 0
               end if

               if( index(buffer,'SF_ID:') >= 0 )then
                  read(buffer(index(buffer,'SF_ID:')+7:&
                  &index(buffer,'SF_ID:')+15),'(A)') cssi
               else
                  cssi = '        '
               end if
               call stonum(cssi,8,fnum,imit)
               if (imit == 1) then
                  issi = nint(fnum)
               else
                  issi = 0
               end if

               if (issi /= idsurf) then
!                 Write Warning Message:  SURFDATA id mismatch
                  call errhdl(path,modnam,'W','530','SURFDATA')
               end if
               if (iusi /= iduair) then
!                 Write Warning Message:  UAIRDATA id mismatch
                  call errhdl(path,modnam,'W','530','UAIRDATA')
               end if
            end if

            go to 1002

!           Error reading 'header record' - assume that header record is
!           missing.  Backspace met file and continue processing.
9981        backspace mfunit

         end if

1002     continue

! ---    First read date variables to check for problems
         read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
         &imonth, iday, ijday, ihour

!        D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
         call cent_date(iyear,iyr)
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Determine The Current Julian Day and Calculate Current Gregorian Date
!C        First Convert Year to 4-Digit Value
!         IF (IYEAR .GE. ISTRT_WIND .and. IYEAR .LE. 99) THEN
!            IYR = ISTRT_CENT*100 + IYEAR
!         ELSE IF (IYEAR .LT. ISTRT_WIND) THEN
!            IYR = (ISTRT_CENT+1)*100 + IYEAR
!         ELSE
!C           Input IYEAR must be 4-digit:  Save to IYR and convert to 2-digit
!            IYR   = IYEAR
!            IYEAR = IYR - 100 * (IYR/100)
!         END IF

         MEREAD_Date = iyr*1000000 + imonth*10000 + iday*100 + ihour

         if (ihour == ihr) then
! ---       Data file hour matches loop hour; backspace and read full record
            backspace mfunit
            read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
            &imonth, iday, ijday, ihour, asfchf(ihr,1), austar(ihr,1),&
            &awstar(ihr,1),avptgzi(ihr,1),aziconv(ihr,1),azimech(ihr,1),&
            &aobulen(ihr,1), asfcz0(ihr,1),abowen(ihr,1),aalbedo(ihr,1),&
            &auref(ihr,1), awdref(ihr,1), aurefht(ihr,1), ata(ihr,1),&
            &atrefht(ihr,1), iapcode(ihr,1), aprate(ihr,1), arh(ihr,1),&
            &asfcp(ihr,1), nacloud(ihr,1)
         else if (ihour > ihr) then
! ---       Data file starts after hour 01;
!           Issue warning, backspace file and skip to Profile file
            write(dummy,'(2X,3I2)',err=99) imonth, iday, ihr
            call errhdl(path,modnam,'W','489',dummy)
            backspace mfunit
            go to 888
         else
! ---       Data file hour is less than loop hour;
!           could be problem with data file or use of 00-23 hour convention
!           Issue error message:
            write(dummy,'(2X,3I2)',err=99) imonth, iday, ihour
            call errhdl(path,modnam,'E','490',dummy)
            exit hour_loop
         end if
!
      end if

!     Set the stability logical variables
      if( aobulen(ihr,1) > 0.0d0 ) then
         aunstab(ihr,1) = .false.
         astable(ihr,1) = .true.
!        Also set non-array variables for use in COMPTG
         unstab = .false.
         stable = .true.
      else
         aunstab(ihr,1) = .true.
         astable(ihr,1) = .false.
!        Also set non-array variables for use in COMPTG
         unstab = .true.
         stable = .false.
      end if

! --- Assign Sector IDs by hour for sector-varying BACKGRND if needed
      if (L_Backgrnd) then
         if (awdref(ihr,1) <= 0.0d0 .or.&
         &awdref(ihr,1) > 360.0d0) then
! ---       Hour is calm or missing; set ABGSECT = 0
            abgsect(ihr) = 0
         else
! ---       Valid wind direction is available
! ---       Assign sector ID for direction-varying BACKGRND
            fvref = awdref(ihr,1) + 180.0d0
            if (fvref > 360.0d0) then
               fvref = fvref - 360.0d0
            end if
            if (L_BGSector) then
               if (fvref < bgsect(1) .or.&
               &fvref >= bgsect(NUMBGSects) ) then
                  abgsect(ihr) = NUMBGSects
               else
                  do i = 1, NUMBGSects-1
                     if (fvref >= bgsect(i) .and.&
                     &fvref < bgsect(i+1)) then
                        abgsect(ihr) = i
                        exit
                     end if
                  end do
               end if
            else
               abgsect(ihr) = 1
            end if
         end if
      end if

! --- Assign Sector IDs by hour for direction-varying background O3 if needed
      if (l_o3sector) then
         if (awdref(ihr,1) <= 0.0d0 .or.&
         &awdref(ihr,1) > 360.0d0) then
! ---       Hour is calm or missing; set AO3SECT = 0
            ao3sect(ihr) = 0
         else
! ---       Valid wind direction is available
! ---       Assign sector ID for direction-varying background O3
            fvref = awdref(ihr,1) + 180.0d0
            if (fvref > 360.0d0) then
               fvref = fvref - 360.0d0
            end if
            if (L_O3Sector) then
               if (fvref < o3sect(1) .or.&
               &fvref >= o3sect(NUMO3Sects) ) then
                  ao3sect(ihr) = NUMO3Sects
               else
                  do i = 1, NUMO3Sects-1
                     if (fvref >= o3sect(i) .and.&
                     &fvref < o3sect(i+1)) then
                        ao3sect(ihr) = i
                        exit
                     end if
                  end do
               end if
            else
               ao3sect(ihr) = 1
            end if
         end if
      else
! ---    No O3SECTORs; assign 1 to AO3SECT array
         ao3sect(ihr) = 1
      end if

! --- CERC 11/30/20 Assign Sector IDs by hour for direction-varying background NOX if needed
      if (l_noxsector) then
         if (awdref(ihr,1) <= 0.0d0 .or.&
         &awdref(ihr,1) > 360.0d0) then
! ---       Hour is calm or missing; set ANOXSECT = 0
            anoxsect(ihr) = 0
         else
! ---       Valid wind direction is available
! ---       Assign sector ID for direction-varying background NOX
            fvref = awdref(ihr,1) + 180.0d0
            if (fvref > 360.0d0) then
               fvref = fvref - 360.0d0
            end if
            if (fvref < noxsect(1) .or.&
            &fvref >= noxsect(NUMNOxSects) ) then
               anoxsect(ihr) = NUMNOxSects
            else
               do i = 1, NUMNOxSects-1
                  if (fvref >= noxsect(i) .and.&
                  &fvref < noxsect(i+1)) then
                     anoxsect(ihr) = i
                     exit
                  end if
               end do
            end if
         end if
      else
! ---    No NOXSECTORs; assign 1 to ANOXSECT array
         anoxsect(ihr) = 1
      end if

!---- Initialize the profile data to missing;
!     READ profile data based on format
!

! --- Branch here if surface data file starts after hour 01
!
888   continue

      call pflini ()
      level = 1
      jflag = 0
!     Read record from ASCII profile file using FREE format; compute
!     sigma_V from sigma_A and wind speed
! --- First read date variables to check for problems
      read( mpunit, *, end=1000, err=98, iostat=ioerrn ) kyear,&
      &kmonth, kday, khour
!
      if (khour == ihr) then
! ---    Data file hour matches loop hour; backspace and read full record
         backspace mpunit

         do while( jflag == 0 )
            read( mpunit, *, end=1000, err=98, iostat=ioerrn ) kyear,&
            &kmonth, kday, khour, pflht(level), jflag,&
            &pflwd(level), pflws(level), pflta(level),&
            &pflsa(level), pflsw(level)

!        Convert the data to the required units
            call pflcnv (level)

!        Set the number of profile levels to current index, store
!        the 'top of profile' flag, and increment level if not at top
!        Check that the level does not exceed the maximum allowable
            nplvls = level
            anplvls(ihr,1) = level
            aiflag(ihr,level,1) = jflag
            apflht(ihr,level,1) = pflht(level)
            apflwd(ihr,level,1) = pflwd(level)
            apflws(ihr,level,1) = pflws(level)
            apflta(ihr,level,1) = pflta(level)
            apflsa(ihr,level,1) = pflsa(level)
            apflsv(ihr,level,1) = pflsv(level)
            apflsw(ihr,level,1) = pflsw(level)
            if( jflag == 0 )then
               level = level + 1

               if( level > mxplvl )then
                  if( .not. pflerr )then
!                 WRITE Error Message: Number of profile levels
!                                      exceeds maximum allowable
                     write(dummy,'(I8)') mxplvl
                     call errhdl(path,modnam,'E','465',dummy)
                     pflerr = .true.
                     runerr = .true.
                  end if

!              Limit the number of levels to the maximum allowable
                  level = mxplvl
               end if

            end if

         end do

      else if (khour > ihr) then
! ---    Data file starts after hour 01;
!        Backspace file and cycle hour loop
         backspace mpunit
         cycle hour_loop

      else
! ---    Data file hour is less than loop hour;
!        could be problem with data file or use of 00-23 hour convention
!        Issue error message:
         write(dummy,'(2X,3I2)',err=99) imonth, iday, ihour
         call errhdl(path,modnam,'E','489',dummy)
         exit hour_loop
      end if

!     Compute the vertical potential temperature gradient profile
      if( .not. runerr ) then
         ntglvl = 0
         call comptg ()
         antglvl(ihr,1) = ntglvl
         do i = 1, ntglvl
            apfltg(ihr,i,1)  = pfltg(i)
            apfltgz(ihr,i,1) = pfltgz(i)
         end do
      end if

   end do hour_loop

! --- Set the date variables; but first assign IHOUR = 24
!     since only full days of met data are read in EVENT mode
   ihour = 24
   call set_dates

   go to 999

!     WRITE Error Messages:  Error Reading Met Data File

98 call errhdl(path,modnam,'E','510','PROFFILE')
   runerr = .true.
   go to 999

99 call errhdl(path,modnam,'E','510','SURFFILE')
   runerr = .true.
   go to 999

1000 eof = .true.

! --- Set the date variables; but first assign IHOUR = 24
!     since only full days of met data are read in EVENT mode
   ihour = 24
   call set_dates

999 return
end subroutine meread

subroutine ev_metext
!***********************************************************************
!                EV_METEXT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Extraction and Quality Assurance of
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        MODIFIED:   Modified met data arrays to include an additional
!                    array index, since these arrays are also used by
!                    the OU MAXDCONT post-processing option.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   To remove unused data array (NDAY).
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                    of 10-digit date variable (FULLDATE) with 4-digit
!                    year for date comparisons.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:   To add determination of season index (ISEAS).
!                    R.W. Brode, PES, Inc. - 12/2/98
!
!        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        MODIFIED:   To avoid potential math error due to negative
!                    ambient temperatures in calculating the square
!                    root of the stability parameter, RTOFS - 4/19/93
!
!        MODIFIED:
!        7/27/94     J. Paumier, PES, Inc.
!                    The variables for displacement height, ZDM and
!                    AZDM(), were removed from the input to and output
!                    from ISC-COMPDEP.  The following format statements
!                    also were affected: 9009, 9026, 9032, 9033
!
!*       7/27/94     J. Hardikar, PES, Inc.
!*                   Added code to calculate reference wind speed at 10m
!*                   to be used for OPENPIT source algorithms
!
!        INPUTS:  Meteorology File Specifications
!
!        OUTPUTS: Meteorological Variables for One Hour
!
!        CALLED FROM:   EVLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12
! Unused:      INTEGER I

!     Variable Initializations
   modnam = 'EV_METEXT'
   path   = 'MX'

!     Save Value of Last YR/MN/DY/HR and Previous Hour
   ipdate = kurdat
   iphour = ihour

!     Set Meteorological Variables for This Hour
   sfchf  = asfchf(ihour,1)
   uref   = auref(ihour,1)
   urefht = aurefht(ihour,1)
   ta     = ata(ihour,1)
!     Save the temperature for buoyant line source processing because
!      TA is 'adjusted' when point sources are processed
   if (l_blsource) then
      blta = ata(ihour,1)
   end if
   trefht = atrefht(ihour,1)
   wdref  = awdref(ihour,1)
   ustar  = austar(ihour,1)
   wstar  = awstar(ihour,1)
   ziconv = aziconv(ihour,1)
   zimech = azimech(ihour,1)
   obulen = aobulen(ihour,1)
   vptgzi = avptgzi(ihour,1)
   sfcz0  = asfcz0(ihour,1)
   bowen  = abowen(ihour,1)
   albedo = aalbedo(ihour,1)
   ipcode = iapcode(ihour,1)
   prate  = aprate(ihour,1)
   rh     = arh(ihour,1)
   sfcp   = asfcp(ihour,1)
   ncloud = nacloud(ihour,1)
   qsw    = aqsw(ihour,1)
   Wnew   = AWnew(ihour,1)
   f2     = Af2(ihour,1)
   EsTa   = AEsTa(ihour,1)
   Prec1  = APrec1(ihour,1)
   Prec2  = APrec2(ihour,1)

   nplvls = anplvls(ihour,1)

   iflag(1:nplvls) = aiflag(ihour,1:nplvls,1)
   pflht(1:nplvls) = apflht(ihour,1:nplvls,1)
   pflwd(1:nplvls) = apflwd(ihour,1:nplvls,1)
   pflws(1:nplvls) = apflws(ihour,1:nplvls,1)
   pflta(1:nplvls) = apflta(ihour,1:nplvls,1)
   pflsa(1:nplvls) = apflsa(ihour,1:nplvls,1)
   pflsv(1:nplvls) = apflsv(ihour,1:nplvls,1)
   pflsw(1:nplvls) = apflsw(ihour,1:nplvls,1)

   ntglvl = antglvl(ihour,1)

   pfltg(1:ntglvl)  = apfltg(ihour,1:ntglvl,1)
   pfltgz(1:ntglvl) = apfltgz(ihour,1:ntglvl,1)

!     Set Meteorological Variables for Current Hour
!       If a buoyant line source is processed, the PG stability is
!       required.  SET_METDATA calls LTOPG to calculate KST, so it does
!       not need to be done in this subroutine.
   call set_metdata

   return
end subroutine ev_metext

subroutine ev_hrqread(l_firstcall)
!***********************************************************************
!*                  EV_HQREAD Module of AERMOD
!*
!*         PURPOSE: To Read a 24-hour Block of Hourly Emissions Data
!*
!*        MODIFIED:     Streamlined for Aircraft sources, by adding nested
!*                      IF-statement instead of complicated IF-statements.
!*                      M.G. Snyder, WSP 6/6/2023
!*
!*         MODIFIED:    Modified to read Aircraft Engine Parameters for
!*                      VOLUME/AREA Sources only for Aircraft Source Group.
!*                      Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!*                      04/01/2023
!*
!*         PROGRAMMER:  Jayant Hardikar, Roger Brode
!*
!*         DATE:    September 15, 1993
!*
!*         INPUTS:  Variable QFLAG and Current Source Number Being Processed
!*
!*         OUTPUTS: Source Arrays
!*
!*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES
!*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
!*
!*         MODIFIED:  Added code to process hourly values for buoyant
!*                    line source. Amec Foster Wheeler - 03/31/2016
!*
!*         CALLED FROM:  EVLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: is, ihr
!     JAT 7/22/21 D065 ILSAVE NOT USED
!      INTEGER :: ILSAVE
   logical :: l_firstcall
   logical :: eof_save

!*    Variable Initializations
   modnam = 'EV_HRQREAD'
!*    Save current value of EOF from MEREAD
   eof_save = eof
!*    Reinitialize EOF = .F. for HRQREAD
   eof = .false.
!     JAT 7/22/21 D065 ILSAVE NOT USED
!      ILSAVE = ILINE

   hour_loop: do ihr = 1, nhr
      iqline = iqline + 1
      source_loop: do is = 1, numsrc
         if (qflag(is) == 'HOURLY') then

            if (l_firstcall) iline = 1
! ---          Assign ILINE = 1 for first call to HRQREAD

!MGS           Check for aircraft source type for reading/setting
!              aircraft plume rise parameters.
!MGS               CALL HRQREAD (IS) !D151 - 6/5/2023
            if((aftsrc(is) == 'Y')) then
!*               Retrieve AIRCRAFT Source Parameters for This Hour     ---   CALL AHRQREAD
               call ahrqread(is)
            else
!*               Retrieve Source Parameters for This Hour     ---   CALL HRQREAD
               call hrqread(is)
            end if
!MGS           END - Check for aircraft source type

            if (.not.eof .and. ihr == nhr) then
!*                Check for Date and Time Consistency with Met Data;
!*                If Failed, Issue Fatal Error
               if (fulldate /= fullhrq) then
!*                   WRITE Error Message - Date mismatch
                  write(dummy,'(I10.10)') fulldate
                  call errhdl(path,modnam,'E','455',dummy)
                  runerr = .true.
                  exit hour_loop
               end if
            else if (eof) then
! ---             EOF reached in HRQREAD; reassign EOF based on MEREAD
!                 Exit hour loop to avoid read error in HRQREAD
               eof = eof_save
               exit hour_loop
            end if

            ev_hrqs(is,ihr) = hrqs

            if (srctyp(is)(1:5) == 'POINT') then
               ev_hrts(is,ihr) = hrts
               ev_hrvs(is,ihr) = hrvs
            else if (srctyp(is) == 'VOLUME' .and.&
            &l_hrlysig(is)) then
               ev_hrhs(is,ihr) = hrhs
               ev_hrsy(is,ihr) = hrsy
               ev_hrsz(is,ihr) = hrsz

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/6/23
               if (aftsrc(is) == 'Y') then
                  ev_hrmfuel(is,ihr) = hrmfuel
                  ev_hrthrust(is,ihr) = hrthrust
                  ev_hrvaa(is,ihr) = hrvaa
                  ev_hrafr(is,ihr) = hrafr
                  ev_hrbypr(is,ihr) = hrbypr
                  ev_hrsrcangle(is,ihr) = hrsrcangle
                  ev_hrrpwr(is,ihr) = hrrpwr
               end if
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/6/23

            else if ((srctyp(is)(1:4) == 'AREA' .or.&
            &srctyp(is) == 'LINE') .and.&
            &l_hrlysig(is)) then
               ev_hrhs(is,ihr) = hrhs
               ev_hrsz(is,ihr) = hrsz

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/6/23
               if (aftsrc(is) == 'Y') then
                  ev_hrmfuel(is,ihr) = hrmfuel
                  ev_hrthrust(is,ihr) = hrthrust
                  ev_hrvaa(is,ihr) = hrvaa
                  ev_hrafr(is,ihr) = hrafr
                  ev_hrbypr(is,ihr) = hrbypr
                  ev_hrsrcangle(is,ihr) = hrsrcangle
                  ev_hrrpwr(is,ihr) = hrrpwr
               end if
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/6/23

            else if (srctyp(is) == 'OPENPIT') then
               ev_hrts(is,ihr) = hrts
            else if (srctyp(is) == 'BUOYLINE') then
               ev_hrfp(is,ihr)  = hrfp
            end if

         end if
      end do source_loop
   end do hour_loop

   return
end subroutine ev_hrqread

subroutine o3read
!***********************************************************************
!*                  O3READ Module of AERMOD
!*
!*         PURPOSE: To Read a 24-hour Block of Ozone Data
!*
!*         PROGRAMMER:  Roger Brode
!*
!*         DATE:    October 17, 2005
!*
!          MODIFIED:  Modified to assign EV_O3CONC(IHR) value based on
!                     the O3BACK variable from CO OZONEVAL keyword when
!                     no background hourly ozone file or CO O3VALUES inputs
!                     are available.
!                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
!
!          MODIFIED:  Modified to use background ozone values input
!                     through the CO O3VALUES option to substitute for
!                     missing hourly ozone concentrations.
!                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!*         INPUTS:
!*
!*         OUTPUTS:
!*
!*         CALLED FROM:
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: o3min, o3max24
   double precision :: o3sub(6)
   double precision :: ev_o3temp(24)
   integer :: i, ihr, io3yr, io3mn, io3dy, io3hr, io3yr2
   integer :: fullo3hr(6), fullo3yymmdd
! Unused:      INTEGER :: J

!*    Variable Initializations
   modnam  = 'O3READ'

! --- Initialize full date variable for all sectors to 0
   fullo3hr(:)  = 0
! --- Initialize O3SUB substitution values to 0
   o3sub(:)     = 0.0d0
   ev_o3temp(:) = 0.0d0
! --- Initialize O3MISS logical array to FALSE for all hours
   l_ao3miss(:) = .false.

! --- Loop through the current day
   do ihr = 1, 24

! ---    Initialize EV_O3CONC to 0.0 and other arrays to -99.
      ev_o3conc(ihr) =   0.0d0
      ev_o3temp(ihr) = -99.0d0
      o3sub(:) = -99.0d0
      o3min    = -99.0d0

! ---    Assign local IHR index to global IHOUR index; since this
!        may be used to identify temporally-varying O3 values
      ihour = ihr

! ---    Assign O3SECT array value to scalar variable
      io3sect = ao3sect(ihr)

      do i = 1, NUMO3Sects
! ---       Loop through O3SECTORs

! ---       Reinitialize O3SUB for this sector
         o3sub(i) = -99.0d0

! ---       Check for temporally-varying ozone concentrations from O3VALUES
!           keyword; used to fill in for missing hourly data.
         if (l_o3values(i)) then
            call ozonvals(i,o3sub(i))
         else if (l_o3val(i)) then
            o3sub(i) = o3back(i)
         else
            o3sub(i) = 0.0d0
         end if

         if (L_O3Hourly) then
! ---          Hourly O3 data is available; read and process the data

            if (l_o3file(i)) then
! ---             Hourly O3 file available for current sector

               if (i == io3sect) then
! ---                This is the applicable sector for this hour; read next hour of O3 data

                  if (o3form(i) == 'FREE') then
                     read(io3unt(i),*,err=99,end=999) io3yr, io3mn,&
                     &io3dy, io3hr,&
                     &ev_o3conc(io3hr)
                  else
                     read(io3unt(i),o3form(i),err=99,end=999)&
                     &io3yr, io3mn,&
                     &io3dy, io3hr,&
                     &ev_o3conc(io3hr)
                  end if
!        D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  if (io3yr <= 99) then
                     call cent_date(io3yr2,io3yr)
                  end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---                Determine 4-digit year
!                     IF (IO3YR .LE. 99) THEN
!                        IO3YR2 = IO3YR
!                        IF (IO3YR2 .GE. ISTRT_WIND .and.
!     &                                       IO3YR2 .LE. 99) THEN
!                           IO3YR  = ISTRT_CENT*100 + IO3YR2
!                        ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
!                           IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
!                        END IF
!                     END IF

! ---                Calculate full date for this hour of O3 data
                  fullo3hr(i) = io3yr*1000000 + io3mn*10000 +&
                  &io3dy*100 + io3hr

                  if (ev_o3conc(io3hr) >= 0.0d0 .and.&
                  &ev_o3conc(io3hr) < 900.0d0) then
! ---                   Valid hourly value; convert to ug/m3 if needed

                     if (o3filunits == 'PPB') then
                        ev_o3conc(io3hr) = ev_o3conc(io3hr) * o3_ppb
                     else if (o3filunits == 'PPM') then
                        ev_o3conc(io3hr) = ev_o3conc(io3hr) * o3_ppm
                     end if

                     if (.not. nomino3) then !CRCO D074 check for NOMIN03
                        if (astable(io3hr,1)) then
!                           Use min of 40 ppb (78.4ug/m3) and max
!                           from previous 24 hrs
                           o3max24 = min ( 78.40d0,&
                           &maxval(O3_Max24hr(:,ao3sect(io3hr))))
!                           Adjust minimum O3 value based on OBULEN
                           if (aobulen(io3hr,1)>0.0d0 .and.&
                           &aobulen(io3hr,1)<=50.0d0) then
                              o3min = o3max24
                           else if (aobulen(io3hr,1) > 250.0d0) then
                              o3min = 0.0d0
                           else
                              o3min = o3max24*(250.d0-aobulen(io3hr,1))/&
                              &200.d0
                           end if
                        else
                           o3min = -9.0d0
                        end if
! ---                    Save this hour's O3CONC to array of previous
!                        24 values, before applying minimum value
                        O3_Max24hr(io3hr,io3sect) = ev_o3conc(io3hr)
                        ev_o3conc(io3hr) = max(ev_o3conc(io3hr),&
                        &o3min)
                     end if !End CRCO D074 Add check for NOMIN03
                  else if (l_o3values(io3sect) .or.&
                  &l_o3val(io3sect)) then
! ---                   Hourly O3 values is missing; assign O3VALS value;
!                       these have already been converted to ug/m3
                     ev_o3conc(io3hr) = o3sub(io3sect)
! ---                   Assign 0.0 to O3_Max24hr array for this hour
                     O3_Max24hr(io3hr,io3sect) = 0.0d0
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)') fullo3hr(i),i
                        call errhdl(path,modnam,'I','458',dummy)
                     end if
                  else
! ---                   Assign L_AO3MISS logical to TRUE for this hour
                     l_ao3miss(io3hr) = .true.
! ---                   Assign 0.0 to O3_Max24hr array for this hour
                     O3_Max24hr(io3hr,io3sect) = 0.0d0
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)') fullo3hr(i),i
                        call errhdl(path,modnam,'I','459',dummy)
                     end if
                  end if

               else
! ---                This is not applicable sector for this hour; however, read
!                    O3 values to keep track of 24hr max value for this sector
                  if (o3form(i) == 'FREE') then
                     read(io3unt(i),*,err=99,end=999) io3yr, io3mn,&
                     &io3dy, io3hr,&
                     &ev_o3temp(io3hr)
                  else
                     read(io3unt(i),o3form(i),err=99,end=999)&
                     &io3yr, io3mn,&
                     &io3dy, io3hr,&
                     &ev_o3temp(io3hr)
                  end if

!                    D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  if (io3yr <= 99) then
                     call cent_date(io3yr2,io3yr)
                  end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---                Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
!C                    year for comparison with FULLDATE based on met data file
!                     IF (IO3YR .LE. 99) THEN
!                        IO3YR2 = IO3YR
!                        IF (IO3YR2 .GE. ISTRT_WIND .and.
!     &                                       IO3YR2 .LE. 99) THEN
!                           IO3YR  = ISTRT_CENT*100 + IO3YR2
!                        ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
!                           IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
!                        END IF
!                     END IF

! ---                Calculate full date for this hour of O3 data
                  fullo3hr(i) = io3yr*1000000 + io3mn*10000 +&
                  &io3dy*100 + io3hr

                  if (ev_o3temp(io3hr) >= 0.0d0 .and.&
                  &ev_o3temp(io3hr) < 900.0d0) then
! ---                   Valid hourly value; convert to ug/m3 if needed
                     if (o3filunits == 'PPB') then
                        ev_o3temp(io3hr) = ev_o3temp(io3hr) * o3_ppb
                     else if (o3filunits == 'PPM') then
                        ev_o3temp(io3hr) = ev_o3temp(io3hr) * o3_ppm
                     end if
! ---                   Save this hour's O3CONC to array of previous
!                       24 values for this sector
                     O3_Max24hr(io3hr,i) = ev_o3temp(io3hr)
                  else if (l_o3values(i) .or.&
                  &l_o3val(i)) then
! ---                   Hourly O3 value is missing; assign O3SUB value;
!                       these have already been converted to ug/m3
                     ev_o3temp(io3hr) = o3sub(i)
! ---                   Assign 0.0 to O3_Max24hr array so that subbed value will
!                       not be used in determining max value from previous 24 hrs
                     O3_Max24hr(io3hr,i) = 0.0d0
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)') fullo3hr(i),i
                        call errhdl(path,modnam,'I','458',dummy)
                     end if
                  else
! ---                   Assign 0.0 to O3_Max24hr array
                     O3_Max24hr(io3hr,i) = 0.0d0
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)') fullo3hr(i),i
                        call errhdl(path,modnam,'I','459',dummy)
                     end if
                  end if
               end if

            end if   ! IF-THEN block for reading hourly O3FILEs

         else
! ---          No hourly O3 data available; apply O3SUB based on non-hourly data
!              if this is the applicable sector
            if (i == io3sect) then
               ev_o3conc(ihr) = o3sub(i)
            end if

! ---          Calculate full date for this hour of O3 data
!              CERC 11/30/20 - commenting this out as don't think it should
!              be done if O3 is not being read in from a file, since IO3YR
!              etc have not been set.
!               FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 +
!     &                                         IO3DY*100 + IO3HR

         end if

      end do      ! END of Sector Loop

      if (l_ao3miss(ihr)) then
! ---       No O3 value available for this hour; full conversion
!           is assumed (subject to equilibrium ratio); issue an
!           informational message
         ev_o3conc(ihr) = 0.0d0
         if (.not. L_SkipMessages) then
            write(dummy,'(I10.10)') 100*(fulldate/100)+ihr
            call errhdl(path,modnam,'I','459',dummy)
         end if
      end if

   end do     ! Hour of Hour Loop

   do i = 1, NUMO3Sects
! ---    Loop through O3SECTORs
      if (fullo3hr(i) > 0) then
!*          Recalculate full date with last value of IO3HR (should be = 24) for
!*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a
!*          loop through one day of meteorological data and reflects HR 24.
!*          Check for Date and Time Consistency ; If Failed, Issue Fatal Error
!*          Ignore hour in checking for date consistency with met data
         fullo3yymmdd = (fullo3hr(i)/100) * 100
         if (full_yymmdd /= fullo3yymmdd) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fullo3hr(i), i
            call errhdl(path,modnam,'E','457',dummy)
            runerr = .true.
         end if
      end if
   end do

   go to 1000

!*    Write Error Message for Error Reading Hourly Ozone File
99 continue
   write(dummy,'(''O3FILE SECT'',I1)') dummy
   call errhdl(path,modnam,'E','510',dummy)
   runerr = .true.

999 continue

! --- End of file reached on O3 file

1000 return
end subroutine o3read

subroutine noxread
!***********************************************************************
!*                  NOXREAD Module of AERMOD
!*
!*         PURPOSE: To Read a 24-hour Block of NOx background Data
!*
!*         PROGRAMMER:  CERC
!*
!*         DATE:    November 2020
!*
!
!*         INPUTS:
!*
!*         OUTPUTS:
!*
!*         CALLED FROM:
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: noxmin
   double precision :: noxsub(6)
   double precision :: ev_noxtemp(24)
   integer :: i, ihr, inoxyr2, inoxyr, inoxmn, inoxdy, inoxhr
   integer :: fullnoxhr(6), fullnoxyymmdd
!*    Variable Initializations
   modnam  = 'NOXREAD'

! --- Initialize full date variable for all sectors to 0
   fullnoxhr(:)  = 0
! --- Initialize NOXSUB substitution values to 0
   noxsub(:)     = 0.0d0
   ev_noxtemp(:) = 0.0d0
! --- Initialise NOXMISS logical array to FALSE for all hours
   l_anoxmiss(:) = .false.

! --- Loop through the current day
   do ihr = 1, 24
! ---    Initialize EV_NOXCONC, NOXMIN to 0.0 and other arrays to -99.
      ev_noxconc(ihr) =   0.0d0
      ev_noxtemp(ihr) = -99.0d0
      noxsub(:) = -99.0d0
      noxmin    = 0.0d0
!
! ---    Assign local IHR index to global IHOUR index; since this
!        may be used to identify temporally-varying NOX values
      ihour = ihr

! ---    Assign NOXSECT array value to scalar variable
      inoxsect = anoxsect(ihr)

      do i = 1, NUMNOxSects
! ---       Loop through NOXSECTORs

! ---       Reinitialize NOXSUB for this sector
         noxsub(i) = -99.0d0

! ---       Check for temporally-varying NOX concentrations
         if (l_nox_vals(i)) then
            call varynoxvals(i,noxsub(i))
         else if (l_noxvalue(i)) then
            noxsub(i) = noxback(i)
         else
            noxsub(i) = 0.0d0
         end if

         if (L_NOxHourly) then
! ---          Hourly NOx data is available; read and process the data

            if (L_NOxFILE(i)) then
! ---             Hourly NOx file available for current sector

               if (i == inoxsect) then
! ---                This is the applicable sector for this hour; read next hour of NOx data

                  if (noxform(i) == 'FREE') then
                     read(inoxunt(i),*,err=99,end=999) inoxyr,&
                     &inoxmn, inoxdy, inoxhr,&
                     &ev_noxconc(inoxhr)
                  else
                     read(inoxunt(i),noxform(i),err=99,end=999)&
                     &inoxyr, inoxmn,&
                     &inoxdy, inoxhr,&
                     &ev_noxconc(inoxhr)
                  end if
!                    D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  if (inoxyr <= 99) then
                     call cent_date(inoxyr2,inoxyr)
                  end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---                Determine 4-digit year
!                     IF (INOXYR .LE. 99) THEN
!                        INOXYR2 = INOXYR
!                        IF (INOXYR2 .GE. ISTRT_WIND .and.
!     &                                       INOXYR2 .LE. 99) THEN
!                           INOXYR  = ISTRT_CENT*100 + INOXYR2
!                        ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
!                           INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
!                        END IF
!                     END IF

! ---                Calculate full date for this hour of NOx data
                  fullnoxhr(i) = inoxyr*1000000 + inoxmn*10000 +&
                  &inoxdy*100 + inoxhr

                  if (ev_noxconc(inoxhr) >= 0.0d0) then
! ---                   Valid hourly value; convert to ug/m3 if needed
! ---                   using NO2 factors (NOx expressed as 'NOx as NO2')
                     if (noxfilunits == 'PPB') then
                        ev_noxconc(inoxhr) = ev_noxconc(inoxhr) /&
                        &no2_ppb
                     else if (noxfilunits == 'PPM') then
                        ev_noxconc(inoxhr) = ev_noxconc(inoxhr) /&
                        &no2_ppm
                     end if
                     !Ensure non-negative
                     ev_noxconc(inoxhr)=max(ev_noxconc(inoxhr),&
                     &noxmin)
                  else if (l_nox_vals(inoxsect) .or.&
                  &l_noxvalue(inoxsect)) then
! ---                   Hourly NOx values is missing; assign NOX_VALS value;
!                       these have already been converted to ug/m3
                     ev_noxconc(inoxhr) = noxsub(inoxsect)
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)') fullnoxhr(i),&
                        &i
                        call errhdl(path,modnam,'I','610',dummy)
                     end if
                  else
! ---                   Assign L_ANOXMISS logical to TRUE for this hour
                     l_anoxmiss(inoxhr) = .true.
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)') fullnoxhr(i),&
                        &i
                        call errhdl(path,modnam,'I','609',dummy)
                     end if
                  end if

               else
! ---                This is not applicable sector for this hour; however, read
!                    NOX values to keep track of 24hr max value for this sector
                  if (noxform(i) == 'FREE') then
                     read(inoxunt(i),*,err=99,end=999) inoxyr, inoxmn,&
                     &inoxdy, inoxhr,&
                     &ev_noxtemp(inoxhr)
                  else
                     read(inoxunt(i),noxform(i),err=99,end=999)&
                     &inoxyr, inoxmn,&
                     &inoxdy, inoxhr,&
                     &ev_noxtemp(inoxhr)
                  end if
!                     D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  if (inoxyr <= 99) then
                     call cent_date(inoxyr2,inoxyr)
                  end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---                Check for use of 2-digit year in NOX_FILE file, adjust to 4-digit
!C                    year for comparison with FULLDATE based on met data file
!                     IF (INOXYR .LE. 99) THEN
!                        INOXYR2 = INOXYR
!                        IF (INOXYR2 .GE. ISTRT_WIND .and.
!     &                                       INOXYR2 .LE. 99) THEN
!                           INOXYR  = ISTRT_CENT*100 + INOXYR2
!                        ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
!                           INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
!                        END IF
!                     END IF

! ---                Calculate full date for this hour of NOx data
                  fullnoxhr(i) = inoxyr*1000000 + inoxmn*10000 +&
                  &inoxdy*100 + inoxhr

                  if (ev_noxtemp(inoxhr) >= 0.0d0) then
! ---                   Valid hourly value; convert to ug/m3 if needed
! ---                   using NO2 factors (NOx expressed as 'NOx as NO2')
                     if (noxfilunits == 'PPB') then
                        ev_noxtemp(inoxhr)=ev_noxtemp(inoxhr)/no2_ppb
                     else if (noxfilunits == 'PPM') then
                        ev_noxtemp(inoxhr)=ev_noxtemp(inoxhr)/no2_ppm
                     end if
                  else if (l_nox_vals(i) .or.&
                  &l_noxvalue(i)) then
! ---                   Hourly NOx value is missing; assign NOXSUB value;
!                       these have already been converted to ug/m3
                     ev_noxtemp(inoxhr) = noxsub(i)
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)')FULLNOxHR(i),i
                        call errhdl(path,modnam,'I','610',dummy)
                     end if
                  else
                     if (.not. L_SkipMessages) then
                        write(dummy,'(I10.10,''S'',I1)')fullnoxhr(i),i
                        call errhdl(path,modnam,'I','609',dummy)
                     end if
                  end if
               end if

            end if   ! IF-THEN block for reading hourly NOXFILEs

         else
! ---          No hourly NOx data available as yet; apply NOXSUB based on non-hourly data
!              if this is the applicable sector
            if (i == inoxsect) then
               ev_noxconc(ihr) = noxsub(i)
            end if

! ---          Calculate full date for this hour of NOx data
!              CERC 11/30/20 - commenting this out as don't think it should
!              be done if NOx is not being read in from a file, since INOXYR
!              etc have not been set.
!               FULLNOXHR(I)=INOXYR*1000000 + INOXMN*10000 +
!     &                                                INOXDY*100 +INOXHR
         end if
      end do    ! END of Sector Loop

      if(l_anoxmiss(ihr))then
! ---       No NOx value available for this hour; zero conc is assumed
         ev_noxconc(ihr)=0.0d0
         if (.not. L_SkipMessages) then
            write(dummy,'(I10.10)') 100*(fulldate/100)+ihr
            call errhdl(path,modnam,'I','607',dummy)
         end if
      end if

   end do !End of Hour loop

   do i = 1, NUMNOxSects
! ---    Loop through NOxSectors
      if (fullnoxhr(i) > 0) then
!*          Recalculate full date with last value of INOXHR (should be =24) for
!*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a
!*          loop through one day of meteorological data and reflects HR 24.
!*          Check Date and Time Consistency ; If Failed, Issue Fatal Error
!*          Ignore hour in checking for date consistency with met data
         fullnoxyymmdd = (fullnoxhr(i)/100)*100
         if (full_yymmdd /= fullnoxyymmdd) then
!*             WRITE Error Message - Date mismatch
            write(dummy, '(I10.10,''S'',I1)') fullnoxhr(i), i
            call errhdl(path,modnam,'E','608',dummy)
            runerr = .true.
         end if
      end if
   end do

   goto 1000

!*    Write Error Message for Error Reading Hourly NOx File
99 continue
   write(dummy,'(''NOXFILE SCT'',I1)') dummy
   call errhdl(path,modnam,'E','510',dummy)
   runerr = .true.

999 continue

! --- End of file reached on NOx file

1000 return
end subroutine noxread

subroutine bgread
!***********************************************************************
!*                  BGREAD Module of AERMOD
!*
!*         PURPOSE: To Read a 24-hour Block of Background Data
!*
!*         PROGRAMMER:  Roger Brode
!*
!*         DATE:    February 28, 2011
!*
!*         MODIFIED:   Modified subroutine BGREAD to move the unit conversion for
!*                     hourly background concentrations to follow the READ statements
!*                     to avoid "double counting" unit conversion for non-hourly
!*                     background since unit conversion for BGSUB has already been
!*                     applied in sub_BGVAL.
!*                     R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
!*
!*         INPUTS:
!*
!*         OUTPUTS:
!*
!*         CALLED FROM:
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i
   double precision :: bgsub(6)
   integer :: ihr, ibgyr, ibgmn, ibgdy, ibghr, ibgyr2
   integer :: fullbghr(6), fullbgyymmdd

!     Variable Initializations
   modnam  = 'BGREAD'
   fullbghr(:) = 0
   bgsub(:)    = -99.0d0

   do ihr = 1, 24

! ---    Initialize EV_BGCONC to missing
      ev_bgconc(ihr) = -99.0d0

! ---    Assign local IHR index to global IHOUR index; since this
!        may be used to identify temporally-varying BG values
      ihour = ihr

! ---    Assign BGSECT array value to scalar variable
      ibgsect = abgsect(ihr)

      do i = 1, NUMBGSects

! ---       Reinitialize BGSUB for this sector
         bgsub(i) = 0.0d0

! ---       Check for temporally-varying background to substitute for missing hours
         if (L_BGValues(i)) then
            call bgval(i,bgsub(i))
         else
            bgsub(i) = 0.0d0
         end if

         if (L_BGFile(i)) then
! ---          Hourly BACKGRND data file available

            if (i == ibgsect) then
! ---             This is the applicable sector; read hourly BGCONC

!*                Retrieve hourly background concentrations
               if (bgform(i) == 'FREE') then
                  read(ibgunt(i),*,err=99,end=999) ibgyr, ibgmn,&
                  &ibgdy, ibghr,&
                  &ev_bgconc(ibghr)
               else
                  read(ibgunt(i),bgform(i),err=99,end=999)&
                  &ibgyr, ibgmn,&
                  &ibgdy, ibghr,&
                  &ev_bgconc(ibghr)
               end if

! ---             Adjust background concentration units to UG/M3 if needed
!                 for hourly background; unit conversion for BGSUB is
!                 applied in subroutine BGVAL;
!                 conversion is based on reference temperature (25C) and
!                 pressure (1013.25 mb)
               if (ev_bgconc(ibghr) >= 0.0d0) then
                  if (pollut == 'NO2') then
                     if (BackUnits == 'PPB') then
                        ev_bgconc(ibghr) = ev_bgconc(ibghr) / no2_ppb
                     else if (BackUnits == 'PPM') then
                        ev_bgconc(ibghr) = ev_bgconc(ibghr) / no2_ppm
                     end if
                  else if (pollut == 'SO2') then
                     if (BackUnits == 'PPB') then
                        ev_bgconc(ibghr) = ev_bgconc(ibghr) / so2_ppb
                     else if (BackUnits == 'PPM') then
                        ev_bgconc(ibghr) = ev_bgconc(ibghr) / so2_ppm
                     end if
                  else if (pollut == 'CO') then
                     if (BackUnits == 'PPB') then
                        ev_bgconc(ibghr) = ev_bgconc(ibghr) * co_ppb
                     else if (BackUnits == 'PPM') then
                        ev_bgconc(ibghr) = ev_bgconc(ibghr) * co_ppm
                     end if
                  end if
               end if

!                 D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
               if (ibgyr <= 99) then
                  call cent_date(ibgyr2,ibgyr)
               end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---             Check for use of 2-digit year in background file, adjust to 4-digit
!C                 year for comparison with FULLDATE based on met data file
!                  IF (IBGYR .LE. 99) THEN
!                     IBGYR2 = IBGYR
!                     IF (IBGYR2 .GE. ISTRT_WIND .and.
!     &                                          IBGYR2 .LE. 99) THEN
!                        IBGYR  = ISTRT_CENT*100 + IBGYR2
!                     ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
!                        IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
!                     END IF
!                  END IF

! ---             Calculate full date for this hour of BACKGRND data
               fullbghr(i) = ibgyr*1000000 + ibgmn*10000 + ibgdy*100&
               &+ ibghr

            else
! ---             This is not applicable sector for this hour; read record without data

               if (bgform(i) == 'FREE') then
                  read(ibgunt(i),*,err=99,end=999) ibgyr, ibgmn,&
                  &ibgdy, ibghr
               else
                  read(ibgunt(i),bgform(i),err=99,end=999)&
                  &ibgyr, ibgmn,&
                  &ibgdy, ibghr
               end if
!                 D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
               if (ibgyr <= 99) then
                  call cent_date(ibgyr2,ibgyr)
               end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---             Check for use of 2-digit year in background file, adjust to 4-digit
!C                 year for comparison with FULLDATE based on met data file
!                  IF (IBGYR .LE. 99) THEN
!                     IBGYR2 = IBGYR
!                     IF (IBGYR2 .GE. ISTRT_WIND .and.
!     &                                          IBGYR2 .LE. 99) THEN
!                        IBGYR  = ISTRT_CENT*100 + IBGYR2
!                     ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
!                        IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
!                     END IF
!                  END IF

! ---             Calculate full date for this hour of BACKGRND data
               fullbghr(i) = ibgyr*1000000 + ibgmn*10000 + ibgdy*100&
               &+ ibghr

            end if

         end if

      end do    ! NUMBGSects Loop

      if (ev_bgconc(ihr) < 0.0d0) then
! ---       Hourly BGCONC is missing; look for substitution values
         if (ibgsect > 0) then
! ---          Valid BGSECT defined, check for hourly values for this
!              sector, and then for non-hourly values to substitute
            if (L_BGFile(ibgsect)) then
               if (L_BGValues(ibgsect)) then
!                    Hourly background value is missing but non-hourly
!                    values have been specified for substitution,
!                    which were processed in subroutine BGVAL;
                  ev_bgconc(ihr) = bgsub(ibgsect)
!                    Write informational message
                  write(dummy,'(I10.10,''S'',I1)')&
                  &100*(fulldate/100)+ihr,ibgsect
                  call errhdl(path,modnam,'I','453',dummy)
! ---                Increment counter for number of missing BGval substitutions
                  NSubBGHOUR = NSubBGHOUR + 1
               else
!                    Hourly background value is missing for this sector and no
!                    non-hourly values specified for substitution;
!                    Write Error message
                  write(dummy,'(I10.10,''s'',I1)')&
                  &100*(fulldate/100)+ihr,ibgsect
                  call errhdl(path,modnam,'E','452',dummy)
                  runerr = .true.
                  go to 1000
               end if
            else
               if (L_BGValues(ibgsect)) then
!                    Hourly background value is missing but non-hourly
!                    values have been specified for substitution,
!                    which were processed in subroutine BGVAL;
                  ev_bgconc(ihr) = bgsub(ibgsect)
               end if
            end if
         else
! ---          IBGSECT .LE. 0 due to calm/msg hr; Set EV_BGCONC to 0 and exit
            ev_bgconc(ihr) = 0.0d0
         end if
      end if

   end do    ! Hour Loop

!*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   do i = 1, NUMBGSects
      if (fullbghr(i) > 0) then
!*          Check for Date and Time Consistency based on YR/MN/DY
!*          since EVENTS are processed by day, but events within
!*          the day may not be in sequence.
!*          If Failed, Issue Fatal Error
         fullbgyymmdd = (fullbghr(i)/100) * 100
         if (full_yymmdd /= fullbgyymmdd) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fullbghr(i), i
            call errhdl(path,modnam,'E','454',dummy)
            runerr = .true.
         end if
      end if
   end do

   go to 1000

!*    Write Error Message for Error Reading Hourly BACKGRND File
99 continue
   write(dummy,'(''BGFILE SECT'',I1)') ibgsect
   call errhdl(path,modnam,'E','510',dummy)
   runerr = .true.
   go to 1000

999 continue

1000 return
end subroutine bgread
