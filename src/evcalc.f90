SUBROUTINE EVCALC
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
   USE MAIN1
   USE RLINE_DATA, ONLY: RLPROCESSED
   USE BUOYANT_LINE, ONLY: NUMBLGRPS, BL_GRPID
   USE BUOYANT_LINE, ONLY: BL_UREF, BL_RFLAG              !D097 Wood 9/30/22 3/29/23
   IMPLICIT NONE

   LOGICAL   BLPROCESSED
   INTEGER   KK
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'EVCALC'
   PATH   = 'CN'

! --- Set the bouyant line source flag to false to indicate that no
!      lines have been processed for this hour since all lines must be
!      processed together
   BLPROCESSED = .FALSE.

! --- Set the RLINE source flag to false to indicate that no
!      RLINES have been processed for this hour since rotation of
!      receptors only needs to happen for first source.
   RLPROCESSED = .FALSE.

! --- Set NUMREC = 1 to allow use of PCALC, VCALC, ACALC, OCALC,
!     ARM2_CALC, OLM_CALC, PVMRM_CALC, GRSM_CALC subroutines for EVENT processing
   NUMREC = 1

! --- Assign IGRP for this event
   IGRP = IDXEV(IEVENT)

!     Assing METHDR = .TRUE. to print source&receptor-independent
!     meteorology debug information to METEOR debug output file.
   METHDR = .TRUE.

!     Begin Source LOOP
   SOURCE_LOOP: DO ISRC = 1, NUMSRC
! ---    Proceed with calcs if ISRC is included in IGRP, or if NO2
!        options are being used since these require full CHI array
      IF (IGROUP(ISRC,IGRP) .EQ. 1 .or. ARM2 .or.&
      &OLM .or. PVMRM .or. GRSM) THEN
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
!              Calculate Point Source Values                        ---   CALL PCALC
            CALL PCALC
         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
!              Calculate Volume Source Values                       ---   CALL VCALC
            CALL VCALC
         ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .or.&
         &SRCTYP(ISRC) .EQ. 'LINE') THEN
!              Calculate AREA/AREAPOLY/AREACIRC/LINE Source Values  ---   CALL ACALC
            CALL ACALC
         ELSE IF ((SRCTYP(ISRC) .EQ. 'RLINE') .or.&
         &(SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
!              Calculate RLINE Source Values                        ---   CALL RLCALC
            CALL RLCALC
            RLPROCESSED = .TRUE.
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
!              Calculate OpenPit Source Values                      ---   CALL OCALC
            CALL OCALC
!              D097 WSP 3/29/2023 removed the lmitor of BLPROCESSED being FALSE following RLINE example
!            ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE') THEN
         ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE' .and.&
         &(.NOT. BLPROCESSED)) THEN
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
            IF (UREF .GE. 1.0D0) THEN
               BL_UREF = UREF
            ELSE
               BL_UREF = 1.0D0
!             WRITE Message              ! Ref ht wind speed less than minimum
               WRITE(DUMMY,'(''on '',I8.8)') KURDAT
               CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
            END IF

! Multiple_BuoyLines_D41_Wood
!              Determine which BL source group to process
            IF (EVGRP(IEVENT) .EQ. 'ALL') THEN
!                 Process all the buoyant line source group events
               DO KK = 1,NUMBLGRPS
                  CALL BLEVRECP(IEVENT,KK)
                  CALL BL_CALC(KK)
               END DO
               BLPROCESSED = .TRUE.

            ELSE
!                 Process the individual buoyant line source group events
               DO KK = 1,NUMBLGRPS
                  IF (EVGRP(IEVENT) .EQ. BL_GRPID(KK)) THEN
                     BL_RFLAG = .false.                          !D097 WSP 3/29/23 set BL_RFLAG to false
                     CALL BLEVRECP(IEVENT,KK)
                     CALL BL_CALC(KK)
                     EXIT
                  ENDIF
               END DO
               BLPROCESSED = .TRUE.
            END IF                     ! EVGRP(IEVENT) .EQ. 'ALL'
         END IF                        ! SRCTYP(ISRC) .EQ. 'BUOYLINE'
      END IF                           ! IGROUP(ISRC,IGRP) .EQ. 1
   END DO SOURCE_LOOP
!     End Source LOOP

   IF (L_BACKGRND .and. .NOT.ARM2 .and.&
   &.NOT.OLM .and. .NOT.PVMRM .and. .NOT.GRSM) THEN
! ---    User-specified background concentrations are included;
!        add to modeled concentrations by source group unless
!        NO2 options are specified (which are handled separately)
      CALL EV_SUMBACK
   END IF

   RETURN
END

SUBROUTINE EV_SUMVAL
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'EV_SUMVAL'

   HRVALS(IHOUR,ISRC) = HRVAL(1)
   EV_AVEVAL(ISRC)    = EV_AVEVAL(ISRC) + HRVAL(1)
! --- Update GRPAVE and GRPVAL, using IDXEV to identify
!     IGRP for this EVENT
   GRPAVE(IDXEV(IEVENT)) = GRPAVE(IDXEV(IEVENT)) + HRVAL(1)
   GRPVAL(IDXEV(IEVENT),IHOUR) =&
   &GRPVAL(IDXEV(IEVENT),IHOUR) + HRVAL(1)

   RETURN
END

SUBROUTINE EV_SUMBACK
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
   USE MAIN1
   IMPLICIT NONE
   DOUBLE PRECISION :: BCKGRD
   CHARACTER MODNAM*12
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
   MODNAM = 'EV_SUMBACK'
   BCKGRD = 0.0D0
   ITYP=1
   BCKGRD = EV_BGCONC(IHOUR)
!     IGRP has been assigned for this EVENT in sub_EVCALC
   IF (GRP_BACK(IDXEV(IEVENT))) THEN
      GRPVAL(IDXEV(IEVENT),IHOUR) = GRPVAL(IDXEV(IEVENT),IHOUR) +&
      &BCKGRD*EMIFAC(ITYP)/1.0D6
      BACKHR(IDXEV(IEVENT),IHOUR) = BACKHR(IDXEV(IEVENT),IHOUR) +&
      &BCKGRD*EMIFAC(ITYP)/1.0D6
      GRPAVE(IDXEV(IEVENT))  = GRPAVE(IDXEV(IEVENT))  +&
      &BCKGRD*EMIFAC(ITYP)/1.0D6
      BACKAVE(IDXEV(IEVENT)) = BACKAVE(IDXEV(IEVENT)) +&
      &BCKGRD*EMIFAC(ITYP)/1.0D6
   END IF

   RETURN
END

SUBROUTINE EVLOOP
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: IEVYR, IASTAT
   DOUBLE PRECISION :: GRPAVE_Test
   LOGICAL FOPEN, L_FIRSTCALL
   LOGICAL, ALLOCATABLE :: L_EVT_PROC(:)

!     Variable Initializations
   MODNAM = 'EVLOOP'
   DATA L_FIRSTCALL/.TRUE./
   GRPAVE_Test = 0.0D0

! --- Allocate L_EVT_PROC array to track whether an event has
!     already been processed, and initialize as .FALSE.
   IF( .NOT.ALLOCATED(L_EVT_PROC) )THEN
      ALLOCATE( L_EVT_PROC(NUMEVE), STAT=IASTAT)
      L_EVT_PROC(:) = .FALSE.
   ENDIF
! --- Initialize other logicals
   EOF   = .FALSE.
   FOPEN = .FALSE.

!     Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
   CALL EV_FLUSH

! --- Assign FULLDATE and IEDATE with hour = 00 to
!     FULL_YYMMDD, IEDATE_YYMMDD local variables,
!     since hourly events within the same day may
!     not be in sequence
   FULL_YYMMDD = (FULLDATE/100) * 100
   IEDATE_YYMMDD = (IEDATE/100) * 100

   DAY_LOOP: DO WHILE (FULL_YYMMDD .LT. IEDATE_YYMMDD .and. .NOT.EOF)
!        Retrieve Hourly Meteorology Data for Current Day   ---   CALL MEREAD
      CALL MEREAD

! ---    Check for runtime errors from MEREAD and exit DAY_LOOP if found
      IF( RUNERR )THEN
         WRITE(*,901) JDAY, IYR
901      FORMAT('+','MEREAD Error For Day No. ',I4,' of ',I4)
         EXIT DAY_LOOP
      ENDIF

      FULL_YYMMDD = (FULLDATE/100) * 100
      FULLDATE = FULL_YYMMDD + 24

!        Check for Hourly Emissions File
      INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
      IF (FOPEN) THEN
!*          Retrieve Hourly Emissions from File for Current Day---   CALL EV_HRQREAD
!*          Set ILINE = 1 if L_FIRSTCALL for determining whether
!           VOLUME and AREA source inputs include hourly sigmas
         IF (L_FIRSTCALL) ILINE = 1
         CALL EV_HRQREAD(L_FIRSTCALL)
      END IF

      IF (L_BACKGRND) THEN
!-----      Extract BACKGRND concentrations for the current day, if available
         CALL BGREAD
      END IF

! ---    Check for runtime errors from BGREAD and exit DAY_LOOP if found
      IF( RUNERR )THEN
         WRITE(*,902) JDAY, IYR
902      FORMAT('+','BGREAD Error For Day No. ',I4,' of ',I4)
         EXIT DAY_LOOP
      ENDIF

      IF (PVMRM .or. OLM .or. GRSM) THEN
!-----      Extract Ozone Data for current day; L_FIRSTCALL used
!           to initialize array of O3 values used to apply minimum
!           O3 for stable hours
         CALL O3READ
      END IF

      IF (GRSM) THEN
!-----      CERC 11/30/20 Extract NOx Data for current day
         CALL NOXREAD
      END IF

! ---    Check for runtime errors from O3READ and exit DAY_LOOP if found
      IF( RUNERR )THEN
         WRITE(*,903) JDAY, IYR
903      FORMAT('+','O3READ Error For Day No. ',I4,' of ',I4)
         EXIT DAY_LOOP
      ENDIF

! ---    Set L_FIRSTCALL to .F.
      L_FIRSTCALL = .FALSE.

!        Write Out Update to the Screen for the PC Version
      WRITE(*,909) JDAY, IYR
909   FORMAT('+','Now Processing Events For Day No. ',I4,' of ',I4)

      EV_LOOP: DO IEVENT = 1, NUMEVE
! ---       Loop through Events and process Events that
!           occur on this day

! ---       Calculate year of event to account for multiple
!           year data files
         IEVYR = INT(EVDATE(IEVENT)/1000000)
         IF( L_EVT_PROC(IEVENT) )THEN
! ---          Event has already been processed;
!              cycle to next event
            CYCLE EV_LOOP
         ELSE IF (IEVYR .LT. IYEAR) THEN
! ---          Event year is less than current met year;
!              cycle to next event
            CYCLE EV_LOOP
         ELSE IF (IEVYR .GT. IYEAR) THEN
! ---          Event year is greater than met year;
!              cycle EV_LOOP - event will be processed later
            CYCLE EV_LOOP
         ELSE IF (EVJDAY(IEVENT) .EQ. JDAY) THEN
! ---          Event occurs on this day and year;
! ---          process this event and assign logical
!              indicating that event has been processed
            L_EVT_PROC(IEVENT) = .TRUE.

            IF( L_BACKGRND )THEN
! ---             Reinitialize BACKAVE array for this event
               BACKAVE(:) = 0.0D0
            ENDIF
            IF( EVONLY )THEN
! ---             Reinitialize GRPAVE array for this event
               GRPAVE(:) = 0.0D0
            ENDIF

! ---          Assign start and end hour of the event
            IENDHR = EVDATE(IEVENT) -&
            &INT(EVDATE(IEVENT)/100)*100
            ISTAHR = IENDHR - EVAPER(IEVENT) + 1

! ---          Loop through hours for this event
            DO IHOUR = ISTAHR, IENDHR
! ---             Assign IHOUR to KHOUR, used for profile met data
               KHOUR = IHOUR

!                 Time/Date Marker for DEBUG Output
               IF (DEBUG) THEN
                  WRITE(DBGUNT,*)
                  WRITE(DBGUNT,*) '--------------------------------',&
                  &'--------------------------------'
                  WRITE(DBGUNT,*) '---  IEVENT, JDAY, IHOUR =  ',&
                  &IEVENT, JDAY, IHOUR
                  WRITE(DBGUNT,*) '--------------------------------',&
                  &'--------------------------------'
               END IF

! ---             Assign O3MISS logical for this hour
               O3MISS = L_AO3MISS(IHOUR)
! ---             Assign NOXMISS logical for this hour
               NOXMISS = L_ANOXMISS(IHOUR)
!                 Retrieve Hourly Data for Current Event ---   CALL EV_METEXT
               CALL EV_METEXT
!                 Retrieve Hourly Ozone Value
               IF (PVMRM .or. OLM .or. GRSM) THEN
                  IF (.NOT. O3MISS) THEN
                     O3CONC = EV_O3CONC(IHOUR)
                  ELSE
                     O3CONC = -999.0D0
                  END IF
               END IF
!                 CERC 11/30/20 Retrieve Hourly NOx Value
               IF (GRSM) THEN
                  IF (.NOT. NOXMISS) THEN
                     NOXBGCONC = EV_NOXCONC(IHOUR)
                  ELSE
                     NOXBGCONC = -999.0D0
                  END IF
               END IF
!*                Process Hourly Emissions from File, if needed
               IF (HOURLY) THEN
!*                   Begin Source Loop
                  DO ISRC = 1, NUMSRC
                     IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
!*                        Retrieve Source Parameters for This Hour  ---   CALL HRQEXT
                        CALL HRQEXT(ISRC)
                     END IF
                  END DO
!*                   End Source Loop
               END IF
!*----
               IF (CLMHR .and. CLMPRO) THEN
!                    Check for Calm Hr & Processing and
!                    Increment Counters
                  EV_NUMHRS = EV_NUMHRS + 1
                  EV_NUMCLM = EV_NUMCLM + 1
               ELSE IF (MSGHR .and. MSGPRO) THEN
!                    Check for Missing Hour & Processing and
!                    Increment Counters
                  EV_NUMHRS = EV_NUMHRS + 1
                  EV_NUMMSG = EV_NUMMSG + 1
               ELSE IF (ZI .LE. 0.0D0) THEN
!                    Write Out The Informational Message &
!                    Increment Counters
                  WRITE(DUMMY,'(I8.8)') KURDAT
                  CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
                  EV_NUMHRS = EV_NUMHRS + 1
               ELSE
!                    Set CALCS Flag, Increment Counters
!                    & Calculate HRVAL
                  CALCS = .TRUE.
                  EV_NUMHRS = EV_NUMHRS + 1
!                    Calculate CONC or DEPOS Values      ---   CALL EVCALC
                  CALL EVCALC
               END IF

               IF (.NOT.CLMHR .and. .NOT.MSGHR) THEN
! ---                Non-calm, non-missing hour; apply NO2 options as appropriate

                  IF (PVMRM .and. .NOT.PSDCREDIT) THEN
! ---                   Process Hourly Values for PVMRM Option
                     CALL PVMRM_CALC('ALLSRCS')

                  ELSE IF (PVMRM .and. PSDCREDIT) THEN
! ---                   Process Hourly Values for PVMRM Option and PSD credits
! ---                   Need to process two separate sets of sources - the
!                       increment consumption sources ('NAAQSRC') and the
!                       increment expanding sources ('ALLBASE')
                     CALL PVMRM_CALC('NAAQSRC')
                     CALL PVMRM_CALC('ALLBASE')

                  ELSE IF (OLM) THEN
! ---                   Process Hourly Values for OLM Option
                     CALL OLM_CALC

                  ELSE IF (ARM2) THEN
! ---                   Process Hourly Values for ARM2 Option
                     CALL ARM2_CALC

                  ELSE IF (GRSM) THEN
! ---                   CERC 11/30/20 Process Hourly Values for GRSM Option
                     CALL GRSM_CALC

                  END IF

               END IF

            END DO
!              End Hourly LOOP

!              Calculate Applicable Averages             ---   CALL AVEREV
            CALL AVEREV

!              Print Out Model Results                   ---   CALL OUTPUT
            CALL EV_OUTPUT

! ---          Compare calculated EVENT concentration (GRPAVE) to "original"
!              EVENT concentration included on EVENTPER keyword (EV_OrigConc)
            IF( EV_OrigConc(IEVENT) .GT. 0.0D0 ) THEN
! ---             Since "original" EVENT concentration is read from input file
!                 with 5 decimal places, first round the internal results to
!                 5 decimal places to avoid spurious messages if the original
!                 concentration is less than 10.0.
               IF( GRPAVE(IDXEV(IEVENT)) .LT. 10.0D0 )THEN
                  GRPAVE_Test = DBLE(IDNINT(1.0D5 *&
                  &GRPAVE(IDXEV(IEVENT))))/&
                  &1.0D5
               ELSE
! ---                Use original value for comparison
                  GRPAVE_Test = GRPAVE(IDXEV(IEVENT))
               END IF

               IF( DABS((EV_OrigConc(IEVENT)-GRPAVE_Test)/&
               &EV_OrigConc(IEVENT)) .GT. 2.0D-6 )THEN
!                   WRITE Warning Message
                  CALL ERRHDL(PATH,MODNAM,'W','497',EVNAME(IEVENT))
! ---               Assign logical flag indicating the EVENT consistency warning
!                   has been issued; a warning to the default output unit will be
!                   issued at the end of the run.
                  L_EVENT_OrigConc_Warning = .TRUE.
               END IF
            END IF

!              Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
            CALL EV_FLUSH

!              Reset CALCS Flag
            CALCS = .FALSE.

!              Reset the Counters
            EV_NUMHRS = 0
            EV_NUMCLM = 0
            EV_NUMMSG = 0

         END IF   ! IF-ENDIF block on events for this JDAY

      END DO EV_LOOP
!        End Event LOOP

   END DO DAY_LOOP
!     End Loop Through Meteorology Data

   RETURN
END

SUBROUTINE MEREAD
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Constants used in the computation of QSW
   DOUBLE PRECISION, PARAMETER :: C1=5.31D-13, C2=60.0D0, C3=1.12D0,&
   &STEFB= 5.67D-08
   DOUBLE PRECISION :: RN, Es25, FVREF

   INTEGER :: I, IHR, IJDAY, IDATCHK, IUSI, ISSI,&
   &JFLAG, LEVEL
! --- Declare integer variable to store the current date in MEREAD
   INTEGER :: MEREAD_Date

   CHARACTER (LEN=8)   :: CUSI, CSSI
   CHARACTER (LEN=256) :: BUFFER

   SAVE MEREAD_Date

!     Variable Initializations
   MODNAM = 'MEREAD'
   DATA MEREAD_Date/0/
   PATH   = 'MX'
   IDATCHK = 0

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
   IDATCHK = MEREAD_Date - INT(MEREAD_Date/1000000)*1000000

   IF ((IMONTH.EQ.12 .and. IDAY.EQ.31 .and. IHOUR.EQ.24) .or.&
   &IDATCHK .EQ. 123124) THEN
!        End of year has been reached - check for presence of header
!        record at beginning of next year for multi-year data files.
      READ(MFUNIT,'(A256)',ERR=998,END=1000,IOSTAT=IOERRN) BUFFER

! ---    First check for ':' as indicator of header record, then extract
!        AERMET version date, C_METVER, and station IDs
      IF (INDEX(BUFFER,':') .EQ. 0) THEN
!           Record does not contain colon. Assume it must be regular
!           met data record, so backspace met file before proceeding.
         BACKSPACE MFUNIT
      ELSE
!           Record contains colons. Assume it is a header record, and
!           extract AERMET version date, C_METVER, and check station
!           IDs before proceeding in order to flag potential for use
!           of different stations in multi-year data files.
         IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
!              Extract AERMET version date from embedded header record
            READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:&
            &INDEX(BUFFER,'VERSION:')+13),'(A6)')&
            &C_METVER
         ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
!              The 'VERSION:' keyword is missing from header so assign columns
!              93-98 to C_METVER
            C_METVER = BUFFER(93:98)
         ELSE
!              AERMET version not found in header record, issue fatal error message
            CALL ERRHDL(PATH,MODNAM,'E','395','No Version')
         ENDIF

! ---       Read Lat/Lon from header record BUFFER
         READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON
1900     FORMAT(2A10)

! ---       Now extract UA, SF, and OS station IDs from header record
         IF( INDEX(BUFFER,'UA_ID:') .GE. 0 )THEN
            READ(BUFFER(INDEX(BUFFER,'UA_ID:')+7:&
            &INDEX(BUFFER,'UA_ID:')+15),'(A)') CUSI
         ELSE
            CUSI = '        '
         END IF
         CALL STONUM(CUSI,8,FNUM,IMIT)
         IF (IMIT .EQ. 1) THEN
            IUSI = NINT(FNUM)
         ELSE
            IUSI = 0
         END IF

         IF( INDEX(BUFFER,'SF_ID:') .GE. 0 )THEN
            READ(BUFFER(INDEX(BUFFER,'SF_ID:')+7:&
            &INDEX(BUFFER,'SF_ID:')+15),'(A)') CSSI
         ELSE
            CSSI = '        '
         END IF
         CALL STONUM(CSSI,8,FNUM,IMIT)
         IF (IMIT .EQ. 1) THEN
            ISSI = NINT(FNUM)
         ELSE
            ISSI = 0
         END IF

         IF (ISSI .NE. IDSURF) THEN
!              Write Warning Message:  SURFDATA id mismatch
            CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
         END IF
         IF (IUSI .NE. IDUAIR) THEN
!              Write Warning Message:  UAIRDATA id mismatch
            CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
         END IF
      END IF

      GO TO 1001

!        Error reading 'header record' - assume that header record is
!        missing.  Backspace met file and continue processing.
998   BACKSPACE MFUNIT

   END IF

1001 CONTINUE

   HOUR_LOOP: DO IHR = 1, NHR
!
!---- READ surface scaling meteorology data based on format
!
      IF( LDPART .or. LWPART .or. LDGAS .or. LWGAS .or. GRSM )THEN
!        Read record from ASCII scalar parameter file using FREE format
!        with deposition variables
!
! ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
         &IMONTH, IDAY, IJDAY, IHOUR

!        D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
         CALL CENT_DATE(IYEAR,IYR)
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

         MEREAD_Date = IYR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR
!
         IF (IHOUR .EQ. IHR) THEN
! ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
            &IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),&
            &AWSTAR(IHR,1),AVPTGZI(IHR,1),AZICONV(IHR,1),AZIMECH(IHR,1),&
            &AOBULEN(IHR,1), ASFCZ0(IHR,1),ABOWEN(IHR,1),AALBEDO(IHR,1),&
            &AUREF(IHR,1), AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),&
            &ATREFHT(IHR,1), IAPCODE(IHR,1), APRATE(IHR,1), ARH(IHR,1),&
            &ASFCP(IHR,1), NACLOUD(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
! ---       Data file starts after hour 01;
!           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
! ---       Data file hour is less than loop hour;
!           could be problem with data file or use of 00-23 hour convention
!           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF

!        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
!        albedo and cloud cover, for use in gas deposition algorithm.
!        Include check for ABOWEN < 0 for non-standard inputs.
         IF (AOBULEN(IHR,1).GT.0.0D0 .or. AOBULEN(IHR,1).LT.-99990.0D0&
         &.or. ATA(IHR,1).LT.0.0D0 .or.&
         &AALBEDO(IHR,1).EQ.1.0D0 .or. ABOWEN(IHR,1).LE.0.0D0) THEN
!           Hour is stable or missing or inappropriate surface chars.
            AQSW(IHR,1) = 0.0D0
         ELSE
            RN = (1.D0 + 1.D0/ABOWEN(IHR,1))*ASFCHF(IHR,1)/0.9D0
            AQSW(IHR,1) = (RN*(1.D0+C3)-C1*ATA(IHR,1)**6+&
            &STEFB*ATA(IHR,1)**4 -&
            &C2*0.1D0*DBLE(NACLOUD(IHR,1))) /&
            &(1.D0-AALBEDO(IHR,1))
         END IF
!
!        Save precipitation rates for two previous hours
         IF (IHR .EQ. 1) THEN
            Aprec2(IHR,1) = APrate(NHR-1,1)
            Aprec1(IHR,1) = APrate(NHR,1)
         ELSE IF (IHR .EQ. 2) THEN
            Aprec2(IHR,1) = APrate(NHR,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         ELSE
            Aprec2(IHR,1) = APrate(IHR-2,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         END IF

!        Set variables for dry deposition
         IF (LDPART .or. LDGAS) THEN
            IF (ATA(IHR,1).LT.0.0D0 .or. APRATE(IHR,1).LT.0.0D0) THEN
               AWNEW(IHR,1) = AWOLD(IHR,1)
            ELSE
! ...          Compute saturation vapor pressure based on CMAQ formula
               AEsTa(IHR,1) = 0.6112D0*DEXP(19.83D0 -&
               &5417.4D0/ATA(IHR,1))
               Es25 = 3.167D0
               AWnew(IHR,1) = Wold+APrec1(IHR,1)-&
               &0.5D0*f2*AEsTa(IHR,1)/Es25
               Wold = AWnew(IHR,1)
               Af2(IHR,1) = AWnew(IHR,1)/200.D0
               if (Af2(IHR,1).le.0.01D0) Af2(IHR,1) = 0.01D0
               if (Af2(IHR,1).gt.1.0D0) Af2(IHR,1) = 1.0D0
               f2 = Af2(IHR,1)
            END IF
         END IF

      ELSE
!        Read record from ASCII scalar parameter file without deposition
!        parameters, using FREE format
!
! ---    Calculate the MMDDHH variable to check for end of the year
!        based on MEREAD_Date
         IDATCHK = MEREAD_Date - INT(MEREAD_Date/1000000)*1000000
         IF ((IMONTH.EQ.12 .and. IDAY.EQ.31 .and. IHOUR.EQ.24) .or.&
         &IDATCHK .EQ. 123124) THEN
!           End of year has been reached - check for presence of header
!           record at beginning of next year for multi-year data files.
            READ(MFUNIT,'(A256)',ERR=9981,END=1000,IOSTAT=IOERRN) BUFFER

! ---       First check for ':' as indicator of header record, then extract
!           AERMET version date, C_METVER, and station IDs
            IF (INDEX(BUFFER,':') .EQ. 0) THEN
!              Record does not contain colon. Assume it must be regular
!              met data record, so backspace met file before proceeding.
               BACKSPACE MFUNIT
            ELSE
!              Record contains colons. Assume it is a header record, and
!              extract AERMET version date, C_METVER, and check station
!              IDs before proceeding in order to flag potential for use
!              of different stations in multi-year data files.
               IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
!                 Extract AERMET version date from embedded header record
                  READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:&
                  &INDEX(BUFFER,'VERSION:')+13),'(A6)')&
                  &C_METVER
               ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
!                 The 'VERSION:' keyword is missing from header so assign columns
!                 93-98 to C_METVER
                  C_METVER = BUFFER(93:98)
               ELSE
!                 AERMET version not found in header record, issue fatal error message
                  CALL ERRHDL(PATH,MODNAM,'E','395','No Version')
               ENDIF

! ---          Read Lat/Lon from header record BUFFER
               READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON

! ---          Now extract UA, SF, and OS station IDs from header record
               IF( INDEX(BUFFER,'UA_ID:') .GE. 0 )THEN
                  READ(BUFFER(INDEX(BUFFER,'UA_ID:')+7:&
                  &INDEX(BUFFER,'UA_ID:')+15),'(A)') CUSI
               ELSE
                  CUSI = '        '
               END IF
               CALL STONUM(CUSI,8,FNUM,IMIT)
               IF (IMIT .EQ. 1) THEN
                  IUSI = NINT(FNUM)
               ELSE
                  IUSI = 0
               END IF

               IF( INDEX(BUFFER,'SF_ID:') .GE. 0 )THEN
                  READ(BUFFER(INDEX(BUFFER,'SF_ID:')+7:&
                  &INDEX(BUFFER,'SF_ID:')+15),'(A)') CSSI
               ELSE
                  CSSI = '        '
               END IF
               CALL STONUM(CSSI,8,FNUM,IMIT)
               IF (IMIT .EQ. 1) THEN
                  ISSI = NINT(FNUM)
               ELSE
                  ISSI = 0
               END IF

               IF (ISSI .NE. IDSURF) THEN
!                 Write Warning Message:  SURFDATA id mismatch
                  CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
               END IF
               IF (IUSI .NE. IDUAIR) THEN
!                 Write Warning Message:  UAIRDATA id mismatch
                  CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
               END IF
            END IF

            GO TO 1002

!           Error reading 'header record' - assume that header record is
!           missing.  Backspace met file and continue processing.
9981        BACKSPACE MFUNIT

         END IF

1002     CONTINUE

! ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
         &IMONTH, IDAY, IJDAY, IHOUR

!        D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
         CALL CENT_DATE(IYEAR,IYR)
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

         MEREAD_Date = IYR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR

         IF (IHOUR .EQ. IHR) THEN
! ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
            &IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),&
            &AWSTAR(IHR,1),AVPTGZI(IHR,1),AZICONV(IHR,1),AZIMECH(IHR,1),&
            &AOBULEN(IHR,1), ASFCZ0(IHR,1),ABOWEN(IHR,1),AALBEDO(IHR,1),&
            &AUREF(IHR,1), AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),&
            &ATREFHT(IHR,1), IAPCODE(IHR,1), APRATE(IHR,1), ARH(IHR,1),&
            &ASFCP(IHR,1), NACLOUD(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
! ---       Data file starts after hour 01;
!           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
! ---       Data file hour is less than loop hour;
!           could be problem with data file or use of 00-23 hour convention
!           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF
!
      END IF

!     Set the stability logical variables
      IF( AOBULEN(IHR,1) .GT. 0.0D0 ) THEN
         AUNSTAB(IHR,1) = .FALSE.
         ASTABLE(IHR,1) = .TRUE.
!        Also set non-array variables for use in COMPTG
         UNSTAB = .FALSE.
         STABLE = .TRUE.
      ELSE
         AUNSTAB(IHR,1) = .TRUE.
         ASTABLE(IHR,1) = .FALSE.
!        Also set non-array variables for use in COMPTG
         UNSTAB = .TRUE.
         STABLE = .FALSE.
      END IF

! --- Assign Sector IDs by hour for sector-varying BACKGRND if needed
      IF (L_Backgrnd) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .or.&
         &AWDREF(IHR,1) .GT. 360.0D0) THEN
! ---       Hour is calm or missing; set ABGSECT = 0
            ABGSECT(IHR) = 0
         ELSE
! ---       Valid wind direction is available
! ---       Assign sector ID for direction-varying BACKGRND
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (L_BGSector) THEN
               IF (FVREF .LT. BGSECT(1) .or.&
               &FVREF .GE. BGSECT(NUMBGSects) ) THEN
                  ABGSECT(IHR) = NUMBGSects
               ELSE
                  DO I = 1, NUMBGSects-1
                     IF (FVREF .GE. BGSECT(I) .and.&
                     &FVREF .LT. BGSECT(I+1)) THEN
                        ABGSECT(IHR) = I
                        EXIT
                     END IF
                  END DO
               END IF
            ELSE
               ABGSECT(IHR) = 1
            END IF
         END IF
      END IF

! --- Assign Sector IDs by hour for direction-varying background O3 if needed
      IF (L_O3SECTOR) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .or.&
         &AWDREF(IHR,1) .GT. 360.0D0) THEN
! ---       Hour is calm or missing; set AO3SECT = 0
            AO3SECT(IHR) = 0
         ELSE
! ---       Valid wind direction is available
! ---       Assign sector ID for direction-varying background O3
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (L_O3Sector) THEN
               IF (FVREF .LT. O3SECT(1) .or.&
               &FVREF .GE. O3SECT(NUMO3Sects) ) THEN
                  AO3SECT(IHR) = NUMO3Sects
               ELSE
                  DO I = 1, NUMO3Sects-1
                     IF (FVREF .GE. O3SECT(I) .and.&
                     &FVREF .LT. O3SECT(I+1)) THEN
                        AO3SECT(IHR) = I
                        EXIT
                     END IF
                  END DO
               END IF
            ELSE
               AO3SECT(IHR) = 1
            END IF
         END IF
      ELSE
! ---    No O3SECTORs; assign 1 to AO3SECT array
         AO3SECT(IHR) = 1
      END IF

! --- CERC 11/30/20 Assign Sector IDs by hour for direction-varying background NOX if needed
      IF (L_NOXSECTOR) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .or.&
         &AWDREF(IHR,1) .GT. 360.0D0) THEN
! ---       Hour is calm or missing; set ANOXSECT = 0
            ANOXSECT(IHR) = 0
         ELSE
! ---       Valid wind direction is available
! ---       Assign sector ID for direction-varying background NOX
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (FVREF .LT. NOXSECT(1) .or.&
            &FVREF .GE. NOXSECT(NUMNOxSects) ) THEN
               ANOXSECT(IHR) = NUMNOxSects
            ELSE
               DO I = 1, NUMNOxSects-1
                  IF (FVREF .GE. NOXSECT(I) .and.&
                  &FVREF .LT. NOXSECT(I+1)) THEN
                     ANOXSECT(IHR) = I
                     EXIT
                  END IF
               END DO
            END IF
         END IF
      ELSE
! ---    No NOXSECTORs; assign 1 to ANOXSECT array
         ANOXSECT(IHR) = 1
      END IF

!---- Initialize the profile data to missing;
!     READ profile data based on format
!

! --- Branch here if surface data file starts after hour 01
!
888   CONTINUE

      CALL PFLINI ()
      LEVEL = 1
      JFLAG = 0
!     Read record from ASCII profile file using FREE format; compute
!     sigma_V from sigma_A and wind speed
! --- First read date variables to check for problems
      READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,&
      &KMONTH, KDAY, KHOUR
!
      IF (KHOUR .EQ. IHR) THEN
! ---    Data file hour matches loop hour; backspace and read full record
         BACKSPACE MPUNIT

         DO WHILE( JFLAG .EQ. 0 )
            READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,&
            &KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,&
            &PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),&
            &PFLSA(LEVEL), PFLSW(LEVEL)

!        Convert the data to the required units
            CALL PFLCNV (LEVEL)

!        Set the number of profile levels to current index, store
!        the 'top of profile' flag, and increment level if not at top
!        Check that the level does not exceed the maximum allowable
            NPLVLS = LEVEL
            ANPLVLS(IHR,1) = LEVEL
            AIFLAG(IHR,LEVEL,1) = JFLAG
            APFLHT(IHR,LEVEL,1) = PFLHT(LEVEL)
            APFLWD(IHR,LEVEL,1) = PFLWD(LEVEL)
            APFLWS(IHR,LEVEL,1) = PFLWS(LEVEL)
            APFLTA(IHR,LEVEL,1) = PFLTA(LEVEL)
            APFLSA(IHR,LEVEL,1) = PFLSA(LEVEL)
            APFLSV(IHR,LEVEL,1) = PFLSV(LEVEL)
            APFLSW(IHR,LEVEL,1) = PFLSW(LEVEL)
            IF( JFLAG .EQ. 0 )THEN
               LEVEL = LEVEL + 1

               IF( LEVEL .GT. MXPLVL )THEN
                  IF( .NOT. PFLERR )THEN
!                 WRITE Error Message: Number of profile levels
!                                      exceeds maximum allowable
                     WRITE(DUMMY,'(I8)') MXPLVL
                     CALL ERRHDL(PATH,MODNAM,'E','465',DUMMY)
                     PFLERR = .TRUE.
                     RUNERR = .TRUE.
                  END IF

!              Limit the number of levels to the maximum allowable
                  LEVEL = MXPLVL
               END IF

            END IF

         END DO

      ELSE IF (KHOUR .GT. IHR) THEN
! ---    Data file starts after hour 01;
!        Backspace file and cycle hour loop
         BACKSPACE MPUNIT
         CYCLE HOUR_LOOP

      ELSE
! ---    Data file hour is less than loop hour;
!        could be problem with data file or use of 00-23 hour convention
!        Issue error message:
         WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
         CALL ERRHDL(PATH,MODNAM,'E','489',DUMMY)
         EXIT HOUR_LOOP
      END IF

!     Compute the vertical potential temperature gradient profile
      IF( .NOT. RUNERR ) THEN
         NTGLVL = 0
         CALL COMPTG ()
         ANTGLVL(IHR,1) = NTGLVL
         DO I = 1, NTGLVL
            APFLTG(IHR,I,1)  = PFLTG(I)
            APFLTGZ(IHR,I,1) = PFLTGZ(I)
         END DO
      END IF

   END DO HOUR_LOOP

! --- Set the date variables; but first assign IHOUR = 24
!     since only full days of met data are read in EVENT mode
   IHOUR = 24
   CALL SET_DATES

   GO TO 999

!     WRITE Error Messages:  Error Reading Met Data File

98 CALL ERRHDL(PATH,MODNAM,'E','510','PROFFILE')
   RUNERR = .TRUE.
   GO TO 999

99 CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
   RUNERR = .TRUE.
   GO TO 999

1000 EOF = .TRUE.

! --- Set the date variables; but first assign IHOUR = 24
!     since only full days of met data are read in EVENT mode
   IHOUR = 24
   CALL SET_DATES

999 RETURN
END

SUBROUTINE EV_METEXT
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
   USE MAIN1
   USE BUOYANT_LINE
   IMPLICIT NONE
   CHARACTER MODNAM*12
! Unused:      INTEGER I

!     Variable Initializations
   MODNAM = 'EV_METEXT'
   PATH   = 'MX'

!     Save Value of Last YR/MN/DY/HR and Previous Hour
   IPDATE = KURDAT
   IPHOUR = IHOUR

!     Set Meteorological Variables for This Hour
   SFCHF  = ASFCHF(IHOUR,1)
   UREF   = AUREF(IHOUR,1)
   UREFHT = AUREFHT(IHOUR,1)
   TA     = ATA(IHOUR,1)
!     Save the temperature for buoyant line source processing because
!      TA is 'adjusted' when point sources are processed
   IF (L_BLSOURCE) THEN
      BLTA = ATA(IHOUR,1)
   END IF
   TREFHT = ATREFHT(IHOUR,1)
   WDREF  = AWDREF(IHOUR,1)
   USTAR  = AUSTAR(IHOUR,1)
   WSTAR  = AWSTAR(IHOUR,1)
   ZICONV = AZICONV(IHOUR,1)
   ZIMECH = AZIMECH(IHOUR,1)
   OBULEN = AOBULEN(IHOUR,1)
   VPTGZI = AVPTGZI(IHOUR,1)
   SFCZ0  = ASFCZ0(IHOUR,1)
   BOWEN  = ABOWEN(IHOUR,1)
   ALBEDO = AALBEDO(IHOUR,1)
   IPCODE = IAPCODE(IHOUR,1)
   PRATE  = APRATE(IHOUR,1)
   RH     = ARH(IHOUR,1)
   SFCP   = ASFCP(IHOUR,1)
   NCLOUD = NACLOUD(IHOUR,1)
   QSW    = AQSW(IHOUR,1)
   Wnew   = AWnew(IHOUR,1)
   f2     = Af2(IHOUR,1)
   EsTa   = AEsTa(IHOUR,1)
   Prec1  = APrec1(IHOUR,1)
   Prec2  = APrec2(IHOUR,1)

   NPLVLS = ANPLVLS(IHOUR,1)

   IFLAG(1:NPLVLS) = AIFLAG(IHOUR,1:NPLVLS,1)
   PFLHT(1:NPLVLS) = APFLHT(IHOUR,1:NPLVLS,1)
   PFLWD(1:NPLVLS) = APFLWD(IHOUR,1:NPLVLS,1)
   PFLWS(1:NPLVLS) = APFLWS(IHOUR,1:NPLVLS,1)
   PFLTA(1:NPLVLS) = APFLTA(IHOUR,1:NPLVLS,1)
   PFLSA(1:NPLVLS) = APFLSA(IHOUR,1:NPLVLS,1)
   PFLSV(1:NPLVLS) = APFLSV(IHOUR,1:NPLVLS,1)
   PFLSW(1:NPLVLS) = APFLSW(IHOUR,1:NPLVLS,1)

   NTGLVL = ANTGLVL(IHOUR,1)

   PFLTG(1:NTGLVL)  = APFLTG(IHOUR,1:NTGLVL,1)
   PFLTGZ(1:NTGLVL) = APFLTGZ(IHOUR,1:NTGLVL,1)

!     Set Meteorological Variables for Current Hour
!       If a buoyant line source is processed, the PG stability is
!       required.  SET_METDATA calls LTOPG to calculate KST, so it does
!       not need to be done in this subroutine.
   CALL SET_METDATA

   RETURN
END

SUBROUTINE EV_HRQREAD(L_FIRSTCALL)
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: IS, IHR
!     JAT 7/22/21 D065 ILSAVE NOT USED
!      INTEGER :: ILSAVE
   LOGICAL :: L_FIRSTCALL
   LOGICAL :: EOF_SAVE

!*    Variable Initializations
   MODNAM = 'EV_HRQREAD'
!*    Save current value of EOF from MEREAD
   EOF_SAVE = EOF
!*    Reinitialize EOF = .F. for HRQREAD
   EOF = .FALSE.
!     JAT 7/22/21 D065 ILSAVE NOT USED
!      ILSAVE = ILINE

   HOUR_LOOP: DO IHR = 1, NHR
      IQLINE = IQLINE + 1
      SOURCE_LOOP: DO IS = 1, NUMSRC
         IF (QFLAG(IS) .EQ. 'HOURLY') THEN

            IF (L_FIRSTCALL) ILINE = 1
! ---          Assign ILINE = 1 for first call to HRQREAD

!MGS           Check for aircraft source type for reading/setting
!              aircraft plume rise parameters.
!MGS               CALL HRQREAD (IS) !D151 - 6/5/2023
            IF((AFTSRC(IS) .EQ. 'Y')) THEN
!*               Retrieve AIRCRAFT Source Parameters for This Hour     ---   CALL AHRQREAD
               CALL AHRQREAD(IS)
            ELSE
!*               Retrieve Source Parameters for This Hour     ---   CALL HRQREAD
               CALL HRQREAD(IS)
            END IF
!MGS           END - Check for aircraft source type

            IF (.NOT.EOF .and. IHR .EQ. NHR) THEN
!*                Check for Date and Time Consistency with Met Data;
!*                If Failed, Issue Fatal Error
               IF (FULLDATE .NE. FULLHRQ) THEN
!*                   WRITE Error Message - Date mismatch
                  WRITE(DUMMY,'(I10.10)') FULLDATE
                  CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
                  RUNERR = .TRUE.
                  EXIT HOUR_LOOP
               END IF
            ELSE IF (EOF) THEN
! ---             EOF reached in HRQREAD; reassign EOF based on MEREAD
!                 Exit hour loop to avoid read error in HRQREAD
               EOF = EOF_SAVE
               EXIT HOUR_LOOP
            END IF

            EV_HRQS(IS,IHR) = HRQS

            IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
               EV_HRTS(IS,IHR) = HRTS
               EV_HRVS(IS,IHR) = HRVS
            ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .and.&
            &L_HRLYSIG(IS)) THEN
               EV_HRHS(IS,IHR) = HRHS
               EV_HRSY(IS,IHR) = HRSY
               EV_HRSZ(IS,IHR) = HRSZ

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/6/23
               IF (AFTSRC(IS) .EQ. 'Y') THEN
                  EV_HRMFUEL(IS,IHR) = HRMFUEL
                  EV_HRTHRUST(IS,IHR) = HRTHRUST
                  EV_HRVAA(IS,IHR) = HRVAA
                  EV_HRAFR(IS,IHR) = HRAFR
                  EV_HRBYPR(IS,IHR) = HRBYPR
                  EV_HRSRCANGLE(IS,IHR) = HRSRCANGLE
                  EV_HRRPWR(IS,IHR) = HRRPWR
               END IF
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/6/23

            ELSE IF ((SRCTYP(IS)(1:4) .EQ. 'AREA' .or.&
            &SRCTYP(IS) .EQ. 'LINE') .and.&
            &L_HRLYSIG(IS)) THEN
               EV_HRHS(IS,IHR) = HRHS
               EV_HRSZ(IS,IHR) = HRSZ

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/6/23
               IF (AFTSRC(IS) .EQ. 'Y') THEN
                  EV_HRMFUEL(IS,IHR) = HRMFUEL
                  EV_HRTHRUST(IS,IHR) = HRTHRUST
                  EV_HRVAA(IS,IHR) = HRVAA
                  EV_HRAFR(IS,IHR) = HRAFR
                  EV_HRBYPR(IS,IHR) = HRBYPR
                  EV_HRSRCANGLE(IS,IHR) = HRSRCANGLE
                  EV_HRRPWR(IS,IHR) = HRRPWR
               END IF
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/6/23

            ELSE IF (SRCTYP(IS) .EQ. 'OPENPIT') THEN
               EV_HRTS(IS,IHR) = HRTS
            ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE') THEN
               EV_HRFP(IS,IHR)  = HRFP
            END IF

         END IF
      END DO SOURCE_LOOP
   END DO HOUR_LOOP

   RETURN
END

SUBROUTINE O3READ
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: O3MIN, O3MAX24
   DOUBLE PRECISION :: O3SUB(6)
   DOUBLE PRECISION :: EV_O3TEMP(24)
   INTEGER :: I, IHR, IO3YR, IO3MN, IO3DY, IO3HR, IO3YR2
   INTEGER :: FULLO3HR(6), FULLO3YYMMDD
! Unused:      INTEGER :: J

!*    Variable Initializations
   MODNAM  = 'O3READ'

! --- Initialize full date variable for all sectors to 0
   FULLO3HR(:)  = 0
! --- Initialize O3SUB substitution values to 0
   O3SUB(:)     = 0.0D0
   EV_O3TEMP(:) = 0.0D0
! --- Initialize O3MISS logical array to FALSE for all hours
   L_AO3MISS(:) = .FALSE.

! --- Loop through the current day
   DO IHR = 1, 24

! ---    Initialize EV_O3CONC to 0.0 and other arrays to -99.
      EV_O3CONC(IHR) =   0.0D0
      EV_O3TEMP(IHR) = -99.0D0
      O3SUB(:) = -99.0D0
      O3MIN    = -99.0D0

! ---    Assign local IHR index to global IHOUR index; since this
!        may be used to identify temporally-varying O3 values
      IHOUR = IHR

! ---    Assign O3SECT array value to scalar variable
      IO3SECT = AO3SECT(IHR)

      DO I = 1, NUMO3Sects
! ---       Loop through O3SECTORs

! ---       Reinitialize O3SUB for this sector
         O3SUB(I) = -99.0D0

! ---       Check for temporally-varying ozone concentrations from O3VALUES
!           keyword; used to fill in for missing hourly data.
         IF (L_O3VALUES(I)) THEN
            CALL OZONVALS(I,O3SUB(I))
         ELSE IF (L_O3VAL(I)) THEN
            O3SUB(I) = O3BACK(I)
         ELSE
            O3SUB(I) = 0.0D0
         END IF

         IF (L_O3Hourly) THEN
! ---          Hourly O3 data is available; read and process the data

            IF (L_O3FILE(I)) THEN
! ---             Hourly O3 file available for current sector

               IF (I .EQ. IO3SECT) THEN
! ---                This is the applicable sector for this hour; read next hour of O3 data

                  IF (O3FORM(I) .EQ. 'FREE') THEN
                     READ(IO3UNT(I),*,ERR=99,END=999) IO3YR, IO3MN,&
                     &IO3DY, IO3HR,&
                     &EV_O3CONC(IO3HR)
                  ELSE
                     READ(IO3UNT(I),O3FORM(I),ERR=99,END=999)&
                     &IO3YR, IO3MN,&
                     &IO3DY, IO3HR,&
                     &EV_O3CONC(IO3HR)
                  END IF
!        D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  IF (IO3YR .LE. 99) THEN
                     CALL CENT_DATE(IO3YR2,IO3YR)
                  END IF
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
                  FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 +&
                  &IO3DY*100 + IO3HR

                  IF (EV_O3CONC(IO3HR) .GE. 0.0D0 .and.&
                  &EV_O3CONC(IO3HR) .LT. 900.0D0) THEN
! ---                   Valid hourly value; convert to ug/m3 if needed

                     IF (O3FILUNITS .EQ. 'PPB') THEN
                        EV_O3CONC(IO3HR) = EV_O3CONC(IO3HR) * O3_PPB
                     ELSE IF (O3FILUNITS .EQ. 'PPM') then
                        EV_O3CONC(IO3HR) = EV_O3CONC(IO3HR) * O3_PPM
                     END IF

                     IF (.NOT. NOMINO3) THEN !CRCO D074 check for NOMIN03
                        IF (ASTABLE(IO3HR,1)) THEN
!                           Use min of 40 ppb (78.4ug/m3) and max
!                           from previous 24 hrs
                           O3MAX24 = MIN ( 78.40D0,&
                           &MAXVAL(O3_Max24hr(:,AO3SECT(IO3HR))))
!                           Adjust minimum O3 value based on OBULEN
                           IF (AOBULEN(IO3HR,1).GT.0.0D0 .and.&
                           &AOBULEN(IO3HR,1).LE.50.0D0) THEN
                              O3MIN = O3MAX24
                           ELSE IF (AOBULEN(IO3HR,1) .GT. 250.0D0) THEN
                              O3MIN = 0.0D0
                           ELSE
                              O3MIN = O3MAX24*(250.D0-AOBULEN(IO3HR,1))/&
                              &200.D0
                           END IF
                        ELSE
                           O3MIN = -9.0D0
                        END IF
! ---                    Save this hour's O3CONC to array of previous
!                        24 values, before applying minimum value
                        O3_Max24hr(IO3HR,IO3SECT) = EV_O3CONC(IO3HR)
                        EV_O3CONC(IO3HR) = MAX(EV_O3CONC(IO3HR),&
                        &O3MIN)
                     END IF !End CRCO D074 Add check for NOMIN03
                  ELSE IF (L_O3VALUES(IO3SECT) .or.&
                  &L_O3VAL(IO3SECT)) THEN
! ---                   Hourly O3 values is missing; assign O3VALS value;
!                       these have already been converted to ug/m3
                     EV_O3CONC(IO3HR) = O3SUB(IO3SECT)
! ---                   Assign 0.0 to O3_Max24hr array for this hour
                     O3_Max24hr(IO3HR,IO3SECT) = 0.0D0
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                        CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                     END IF
                  ELSE
! ---                   Assign L_AO3MISS logical to TRUE for this hour
                     L_AO3MISS(IO3HR) = .TRUE.
! ---                   Assign 0.0 to O3_Max24hr array for this hour
                     O3_Max24hr(IO3HR,IO3SECT) = 0.0D0
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                        CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                     END IF
                  END IF

               ELSE
! ---                This is not applicable sector for this hour; however, read
!                    O3 values to keep track of 24hr max value for this sector
                  IF (O3FORM(I) .EQ. 'FREE') THEN
                     READ(IO3UNT(I),*,ERR=99,END=999) IO3YR, IO3MN,&
                     &IO3DY, IO3HR,&
                     &EV_O3TEMP(IO3HR)
                  ELSE
                     READ(IO3UNT(I),O3FORM(I),ERR=99,END=999)&
                     &IO3YR, IO3MN,&
                     &IO3DY, IO3HR,&
                     &EV_O3TEMP(IO3HR)
                  END IF

!                    D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  IF (IO3YR .LE. 99) THEN
                     CALL CENT_DATE(IO3YR2,IO3YR)
                  END IF
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
                  FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 +&
                  &IO3DY*100 + IO3HR

                  IF (EV_O3TEMP(IO3HR) .GE. 0.0D0 .and.&
                  &EV_O3TEMP(IO3HR) .LT. 900.0D0) THEN
! ---                   Valid hourly value; convert to ug/m3 if needed
                     IF (O3FILUNITS .EQ. 'PPB') THEN
                        EV_O3TEMP(IO3HR) = EV_O3TEMP(IO3HR) * O3_PPB
                     ELSE IF (O3FILUNITS .EQ. 'PPM') then
                        EV_O3TEMP(IO3HR) = EV_O3TEMP(IO3HR) * O3_PPM
                     END IF
! ---                   Save this hour's O3CONC to array of previous
!                       24 values for this sector
                     O3_Max24hr(IO3HR,I) = EV_O3TEMP(IO3HR)
                  ELSE IF (L_O3VALUES(I) .or.&
                  &L_O3VAL(I)) THEN
! ---                   Hourly O3 value is missing; assign O3SUB value;
!                       these have already been converted to ug/m3
                     EV_O3TEMP(IO3HR) = O3SUB(I)
! ---                   Assign 0.0 to O3_Max24hr array so that subbed value will
!                       not be used in determining max value from previous 24 hrs
                     O3_Max24hr(IO3HR,I) = 0.0D0
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                        CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                     END IF
                  ELSE
! ---                   Assign 0.0 to O3_Max24hr array
                     O3_Max24hr(IO3HR,I) = 0.0D0
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                        CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                     END IF
                  END IF
               END IF

            END IF   ! IF-THEN block for reading hourly O3FILEs

         ELSE
! ---          No hourly O3 data available; apply O3SUB based on non-hourly data
!              if this is the applicable sector
            IF (I .EQ. IO3SECT) THEN
               EV_O3CONC(IHR) = O3SUB(I)
            END IF

! ---          Calculate full date for this hour of O3 data
!              CERC 11/30/20 - commenting this out as don't think it should
!              be done if O3 is not being read in from a file, since IO3YR
!              etc have not been set.
!               FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 +
!     &                                         IO3DY*100 + IO3HR

         END IF

      END DO      ! END of Sector Loop

      IF (L_AO3MISS(IHR)) THEN
! ---       No O3 value available for this hour; full conversion
!           is assumed (subject to equilibrium ratio); issue an
!           informational message
         EV_O3CONC(IHR) = 0.0D0
         IF (.NOT. L_SkipMessages) THEN
            WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
            CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
         END IF
      END IF

   END DO     ! Hour of Hour Loop

   DO I = 1, NUMO3Sects
! ---    Loop through O3SECTORs
      IF (FULLO3HR(I) .GT. 0) THEN
!*          Recalculate full date with last value of IO3HR (should be = 24) for
!*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a
!*          loop through one day of meteorological data and reflects HR 24.
!*          Check for Date and Time Consistency ; If Failed, Issue Fatal Error
!*          Ignore hour in checking for date consistency with met data
         FULLO3YYMMDD = (FULLO3HR(I)/100) * 100
         IF (FULL_YYMMDD .NE. FULLO3YYMMDD) THEN
!*             WRITE Error Message - Date mismatch
            WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I), I
            CALL ERRHDL(PATH,MODNAM,'E','457',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF
   END DO

   GO TO 1000

!*    Write Error Message for Error Reading Hourly Ozone File
99 CONTINUE
   WRITE(DUMMY,'(''O3FILE SECT'',I1)') DUMMY
   CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
   RUNERR = .TRUE.

999 CONTINUE

! --- End of file reached on O3 file

1000 RETURN
END

SUBROUTINE NOXREAD
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: NOXMIN
   DOUBLE PRECISION :: NOXSUB(6)
   DOUBLE PRECISION :: EV_NOXTEMP(24)
   INTEGER :: I, IHR, INOXYR2, INOXYR, INOXMN, INOXDY, INOXHR
   INTEGER :: FULLNOXHR(6), FULLNOXYYMMDD
!*    Variable Initializations
   MODNAM  = 'NOXREAD'

! --- Initialize full date variable for all sectors to 0
   FULLNOXHR(:)  = 0
! --- Initialize NOXSUB substitution values to 0
   NOXSUB(:)     = 0.0D0
   EV_NOXTEMP(:) = 0.0D0
! --- Initialise NOXMISS logical array to FALSE for all hours
   L_ANOXMISS(:) = .FALSE.

! --- Loop through the current day
   DO IHR = 1, 24
! ---    Initialize EV_NOXCONC, NOXMIN to 0.0 and other arrays to -99.
      EV_NOXCONC(IHR) =   0.0D0
      EV_NOXTEMP(IHR) = -99.0D0
      NOXSUB(:) = -99.0D0
      NOXMIN    = 0.0D0
!
! ---    Assign local IHR index to global IHOUR index; since this
!        may be used to identify temporally-varying NOX values
      IHOUR = IHR

! ---    Assign NOXSECT array value to scalar variable
      INOXSECT = ANOXSECT(IHR)

      DO I = 1, NUMNOxSects
! ---       Loop through NOXSECTORs

! ---       Reinitialize NOXSUB for this sector
         NOXSUB(I) = -99.0D0

! ---       Check for temporally-varying NOX concentrations
         IF (L_NOX_VALS(I)) THEN
            CALL VARYNOXVALS(I,NOXSUB(I))
         ELSE IF (L_NOXVALUE(I)) THEN
            NOXSUB(I) = NOXBACK(I)
         ELSE
            NOXSUB(I) = 0.0D0
         END IF

         IF (L_NOxHourly) THEN
! ---          Hourly NOx data is available; read and process the data

            IF (L_NOxFILE(I)) THEN
! ---             Hourly NOx file available for current sector

               IF (I .EQ. INOXSECT) THEN
! ---                This is the applicable sector for this hour; read next hour of NOx data

                  IF (NOXFORM(I) .EQ. 'FREE') THEN
                     READ(INOXUNT(I),*,ERR=99,END=999) INOXYR,&
                     &INOXMN, INOXDY, INOXHR,&
                     &EV_NOXCONC(INOXHR)
                  ELSE
                     READ(INOXUNT(I),NOXFORM(I),ERR=99,END=999)&
                     &INOXYR, INOXMN,&
                     &INOXDY, INOXHR,&
                     &EV_NOXCONC(INOXHR)
                  END IF
!                    D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  IF (INOXYR .LE. 99) THEN
                     CALL CENT_DATE(INOXYR2,INOXYR)
                  END IF
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
                  FULLNOXHR(I) = INOXYR*1000000 + INOXMN*10000 +&
                  &INOXDY*100 + INOXHR

                  IF (EV_NOXCONC(INOXHR) .GE. 0.0D0) THEN
! ---                   Valid hourly value; convert to ug/m3 if needed
! ---                   using NO2 factors (NOx expressed as 'NOx as NO2')
                     IF (NOXFILUNITS .EQ. 'PPB') THEN
                        EV_NOXCONC(INOXHR) = EV_NOXCONC(INOXHR) /&
                        &NO2_PPB
                     ELSE IF (NOXFILUNITS .EQ. 'PPM') then
                        EV_NOXCONC(INOXHR) = EV_NOXCONC(INOXHR) /&
                        &NO2_PPM
                     END IF
                     !Ensure non-negative
                     EV_NOXCONC(INOXHR)=MAX(EV_NOXCONC(INOXHR),&
                     &NOXMIN)
                  ELSE IF (L_NOX_VALS(INOXSECT) .or.&
                  &L_NOXVALUE(INOXSECT)) THEN
! ---                   Hourly NOx values is missing; assign NOX_VALS value;
!                       these have already been converted to ug/m3
                     EV_NOXCONC(INOXHR) = NOXSUB(INOXSECT)
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)') FULLNOXHR(I),&
                        &I
                        CALL ERRHDL(PATH,MODNAM,'I','610',DUMMY)
                     END IF
                  ELSE
! ---                   Assign L_ANOXMISS logical to TRUE for this hour
                     L_ANOXMISS(INOXHR) = .TRUE.
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)') FULLNOXHR(I),&
                        &I
                        CALL ERRHDL(PATH,MODNAM,'I','609',DUMMY)
                     END IF
                  END IF

               ELSE
! ---                This is not applicable sector for this hour; however, read
!                    NOX values to keep track of 24hr max value for this sector
                  IF (NOXFORM(I) .EQ. 'FREE') THEN
                     READ(INOXUNT(I),*,ERR=99,END=999) INOXYR, INOXMN,&
                     &INOXDY, INOXHR,&
                     &EV_NOXTEMP(INOXHR)
                  ELSE
                     READ(INOXUNT(I),NOXFORM(I),ERR=99,END=999)&
                     &INOXYR, INOXMN,&
                     &INOXDY, INOXHR,&
                     &EV_NOXTEMP(INOXHR)
                  END IF
!                     D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
                  IF (INOXYR .LE. 99) THEN
                     CALL CENT_DATE(INOXYR2,INOXYR)
                  END IF
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
                  FULLNOXHR(I) = INOXYR*1000000 + INOXMN*10000 +&
                  &INOXDY*100 + INOXHR

                  IF (EV_NOXTEMP(INOXHR) .GE. 0.0D0) THEN
! ---                   Valid hourly value; convert to ug/m3 if needed
! ---                   using NO2 factors (NOx expressed as 'NOx as NO2')
                     IF (NOXFILUNITS .EQ. 'PPB') THEN
                        EV_NOXTEMP(INOXHR)=EV_NOXTEMP(INOXHR)/NO2_PPB
                     ELSE IF (NOXFILUNITS .EQ. 'PPM') then
                        EV_NOXTEMP(INOXHR)=EV_NOXTEMP(INOXHR)/NO2_PPM
                     END IF
                  ELSE IF (L_NOX_VALS(I) .or.&
                  &L_NOXVALUE(I)) THEN
! ---                   Hourly NOx value is missing; assign NOXSUB value;
!                       these have already been converted to ug/m3
                     EV_NOXTEMP(INOXHR) = NOXSUB(I)
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)')FULLNOxHR(I),I
                        CALL ERRHDL(PATH,MODNAM,'I','610',DUMMY)
                     END IF
                  ELSE
                     IF (.NOT. L_SkipMessages) THEN
                        WRITE(DUMMY,'(I10.10,''S'',I1)')FULLNOXHR(I),I
                        CALL ERRHDL(PATH,MODNAM,'I','609',DUMMY)
                     END IF
                  END IF
               END IF

            END IF   ! IF-THEN block for reading hourly NOXFILEs

         ELSE
! ---          No hourly NOx data available as yet; apply NOXSUB based on non-hourly data
!              if this is the applicable sector
            IF (I .EQ. INOXSECT) THEN
               EV_NOXCONC(IHR) = NOXSUB(I)
            END IF

! ---          Calculate full date for this hour of NOx data
!              CERC 11/30/20 - commenting this out as don't think it should
!              be done if NOx is not being read in from a file, since INOXYR
!              etc have not been set.
!               FULLNOXHR(I)=INOXYR*1000000 + INOXMN*10000 +
!     &                                                INOXDY*100 +INOXHR
         END IF
      END DO    ! END of Sector Loop

      IF(L_ANOXMISS(IHR))THEN
! ---       No NOx value available for this hour; zero conc is assumed
         EV_NOXCONC(IHR)=0.0D0
         IF (.NOT. L_SkipMessages) THEN
            WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
            CALL ERRHDL(PATH,MODNAM,'I','607',DUMMY)
         END IF
      END IF

   END DO !End of Hour loop

   DO I = 1, NUMNOxSects
! ---    Loop through NOxSectors
      IF (FULLNOXHR(I) .GT. 0) THEN
!*          Recalculate full date with last value of INOXHR (should be =24) for
!*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a
!*          loop through one day of meteorological data and reflects HR 24.
!*          Check Date and Time Consistency ; If Failed, Issue Fatal Error
!*          Ignore hour in checking for date consistency with met data
         FULLNOXYYMMDD = (FULLNOXHR(I)/100)*100
         IF (FULL_YYMMDD .NE. FULLNOXYYMMDD) THEN
!*             WRITE Error Message - Date mismatch
            WRITE(DUMMY, '(I10.10,''S'',I1)') FULLNOXHR(I), I
            CALL ERRHDL(PATH,MODNAM,'E','608',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF
   END DO

   GOTO 1000

!*    Write Error Message for Error Reading Hourly NOx File
99 CONTINUE
   WRITE(DUMMY,'(''NOXFILE SCT'',I1)') DUMMY
   CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
   RUNERR = .TRUE.

999 CONTINUE

! --- End of file reached on NOx file

1000 RETURN
END SUBROUTINE NOXREAD

SUBROUTINE BGREAD
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I
   DOUBLE PRECISION :: BGSUB(6)
   INTEGER :: IHR, IBGYR, IBGMN, IBGDY, IBGHR, IBGYR2
   INTEGER :: FULLBGHR(6), FULLBGYYMMDD

!     Variable Initializations
   MODNAM  = 'BGREAD'
   FULLBGHR(:) = 0
   BGSUB(:)    = -99.0D0

   DO IHR = 1, 24

! ---    Initialize EV_BGCONC to missing
      EV_BGCONC(IHR) = -99.0D0

! ---    Assign local IHR index to global IHOUR index; since this
!        may be used to identify temporally-varying BG values
      IHOUR = IHR

! ---    Assign BGSECT array value to scalar variable
      IBGSECT = ABGSECT(IHR)

      DO I = 1, NUMBGSects

! ---       Reinitialize BGSUB for this sector
         BGSUB(I) = 0.0D0

! ---       Check for temporally-varying background to substitute for missing hours
         IF (L_BGValues(I)) THEN
            CALL BGVAL(I,BGSUB(I))
         ELSE
            BGSUB(I) = 0.0D0
         END IF

         IF (L_BGFile(I)) THEN
! ---          Hourly BACKGRND data file available

            IF (I .EQ. IBGSECT) THEN
! ---             This is the applicable sector; read hourly BGCONC

!*                Retrieve hourly background concentrations
               IF (BGFORM(I) .EQ. 'FREE') THEN
                  READ(IBGUNT(I),*,ERR=99,END=999) IBGYR, IBGMN,&
                  &IBGDY, IBGHR,&
                  &EV_BGCONC(IBGHR)
               ELSE
                  READ(IBGUNT(I),BGFORM(I),ERR=99,END=999)&
                  &IBGYR, IBGMN,&
                  &IBGDY, IBGHR,&
                  &EV_BGCONC(IBGHR)
               END IF

! ---             Adjust background concentration units to UG/M3 if needed
!                 for hourly background; unit conversion for BGSUB is
!                 applied in subroutine BGVAL;
!                 conversion is based on reference temperature (25C) and
!                 pressure (1013.25 mb)
               IF (EV_BGCONC(IBGHR) .GE. 0.0D0) THEN
                  IF (POLLUT .EQ. 'NO2') THEN
                     IF (BackUnits .EQ. 'PPB') THEN
                        EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / NO2_PPB
                     ELSE IF (BackUnits .EQ. 'PPM') THEN
                        EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / NO2_PPM
                     END IF
                  ELSE IF (POLLUT .EQ. 'SO2') THEN
                     IF (BackUnits .EQ. 'PPB') THEN
                        EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / SO2_PPB
                     ELSE IF (BackUnits .EQ. 'PPM') THEN
                        EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / SO2_PPM
                     END IF
                  ELSE IF (POLLUT .EQ. 'CO') THEN
                     IF (BackUnits .EQ. 'PPB') THEN
                        EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) * CO_PPB
                     ELSE IF (BackUnits .EQ. 'PPM') THEN
                        EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) * CO_PPM
                     END IF
                  END IF
               END IF

!                 D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
               IF (IBGYR .LE. 99) THEN
                  CALL CENT_DATE(IBGYR2,IBGYR)
               END IF
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
               FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100&
               &+ IBGHR

            ELSE
! ---             This is not applicable sector for this hour; read record without data

               IF (BGFORM(I) .EQ. 'FREE') THEN
                  READ(IBGUNT(I),*,ERR=99,END=999) IBGYR, IBGMN,&
                  &IBGDY, IBGHR
               ELSE
                  READ(IBGUNT(I),BGFORM(I),ERR=99,END=999)&
                  &IBGYR, IBGMN,&
                  &IBGDY, IBGHR
               END IF
!                 D001 Call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
               IF (IBGYR .LE. 99) THEN
                  CALL CENT_DATE(IBGYR2,IBGYR)
               END IF
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
               FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100&
               &+ IBGHR

            END IF

         END IF

      END DO    ! NUMBGSects Loop

      IF (EV_BGCONC(IHR) .LT. 0.0D0) THEN
! ---       Hourly BGCONC is missing; look for substitution values
         IF (IBGSECT .GT. 0) THEN
! ---          Valid BGSECT defined, check for hourly values for this
!              sector, and then for non-hourly values to substitute
            IF (L_BGFile(IBGSECT)) THEN
               IF (L_BGValues(IBGSECT)) THEN
!                    Hourly background value is missing but non-hourly
!                    values have been specified for substitution,
!                    which were processed in subroutine BGVAL;
                  EV_BGCONC(IHR) = BGSUB(IBGSECT)
!                    Write informational message
                  WRITE(DUMMY,'(I10.10,''S'',I1)')&
                  &100*(FULLDATE/100)+IHR,IBGSECT
                  CALL ERRHDL(PATH,MODNAM,'I','453',DUMMY)
! ---                Increment counter for number of missing BGval substitutions
                  NSubBGHOUR = NSubBGHOUR + 1
               ELSE
!                    Hourly background value is missing for this sector and no
!                    non-hourly values specified for substitution;
!                    Write Error message
                  WRITE(DUMMY,'(I10.10,''s'',I1)')&
                  &100*(FULLDATE/100)+IHR,IBGSECT
                  CALL ERRHDL(PATH,MODNAM,'E','452',DUMMY)
                  RUNERR = .TRUE.
                  GO TO 1000
               END IF
            ELSE
               IF (L_BGValues(IBGSECT)) THEN
!                    Hourly background value is missing but non-hourly
!                    values have been specified for substitution,
!                    which were processed in subroutine BGVAL;
                  EV_BGCONC(IHR) = BGSUB(IBGSECT)
               END IF
            END IF
         ELSE
! ---          IBGSECT .LE. 0 due to calm/msg hr; Set EV_BGCONC to 0 and exit
            EV_BGCONC(IHR) = 0.0D0
         END IF
      END IF

   END DO    ! Hour Loop

!*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   DO I = 1, NUMBGSects
      IF (FULLBGHR(I) .GT. 0) THEN
!*          Check for Date and Time Consistency based on YR/MN/DY
!*          since EVENTS are processed by day, but events within
!*          the day may not be in sequence.
!*          If Failed, Issue Fatal Error
         FULLBGYYMMDD = (FULLBGHR(I)/100) * 100
         IF (FULL_YYMMDD .NE. FULLBGYYMMDD) THEN
!*             WRITE Error Message - Date mismatch
            WRITE(DUMMY,'(I10.10,''S'',I1)') FULLBGHR(I), I
            CALL ERRHDL(PATH,MODNAM,'E','454',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF
   END DO

   GO TO 1000

!*    Write Error Message for Error Reading Hourly BACKGRND File
99 CONTINUE
   WRITE(DUMMY,'(''BGFILE SECT'',I1)') IBGSECT
   CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
   RUNERR = .TRUE.
   GO TO 1000

999 CONTINUE

1000 RETURN
END
