SUBROUTINE METEXT
!***********************************************************************
!                METEXT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Extraction and Quality Assurance of
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        Revision History:
!
!        MODIFIED:   Modified to store additional variables needed
!                    for MAXDCONT option when the URBAN option is
!                    being used.  Previous version could report
!                    erroneous source group contributions in the
!                    MAXDCONT file for any application that included
!                    urban sources.
!
!                    Modified to only increment hour index and year
!                    index if MAXDCONT option is being used. This
!                    change avoids unnecessary runtime error if the
!                    met data file includes a single hour beyond
!                    the maximum number of years specified by the
!                    NYEARS PARAMETER, which is initialized to 5 in
!                    the MAIN1 module.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/19/2011
!
!        MODIFIED:   Modified code for setting the month, day, and hour
!                    for the "end of the year", based on the first hour
!                    of the meteorological data file, to resolve potential
!                    problems for PM-2.5 and ANNUAL average applications
!                    in which the first hour of the file is not 01.
!
!                    Modified to include check for end-of-file (EOF) on
!                    first data record, and to assign file type for possible
!                    end-of-file error messages.
!
!                    Included code to check for presence of additional
!                    variables in the surface meteorological file needed
!                    for use of the deposition options when required.
!                    Also checks for minimum number of data fields for
!                    applications without deposition.
!
!                    Include checks for potential conflicts between
!                    first date of met data file being later than
!                    start date from re-start file or user-specified
!                    start date on STARTEND keyword, with fatal error
!                    message for conflict with INITFILE restarts and
!                    warning for MULTYEAR restarts.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Modified code for processing multi-year data
!                    files to determine if header record is present
!                    between years for concatenated files.  Use presence
!                    of colon (':') as only criterion for header record.
!                    Use warning messages if UAIR and SURF IDs don't
!                    match input runstream file for multiple years since
!                    AERMOD allows mismatch for single year files.
!                    Modified check for stable or missing hours in
!                    calculation of solar irradiance (QSW) for use
!                    in deposition calculations.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Modified code for reading the header record of the
!                    surface file to use a character variable for the
!                    AERMET version date field, in order to allow for
!                    the future use of screening meteorology that is not
!                    directly linked to a specific version under the
!                    of the AERMET processor under the SCREEN option.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
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
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                    of 10-digit date variable (FULLDATE) with 4-digit
!                    year for date comparisons.
!                    Also moved call to METDAT to allow use of single
!                    METDAT routine for normal and EVENT processing.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:   To correct potential problem with check for
!                    concatenated data files.
!                    R.W. Brode, PES, Inc., 9/15/2000
!
!        MODIFIED:   To incorporate additional variables for dry
!                    and wet deposition, and to remove formatted
!                    read for surface file.  Surface file is now
!                    read FREE format.
!                    R.W. Brode, PES, Inc., 9/29/2003
!
!        MODIFIED:   Moved call to SUB. METDAT ahead of call to
!                    SUB. SET_METDATA to avoid potential problem
!                    with negative precipitation for first hour
!                    of data.
!                    R.W. Brode, MACTEC, 10/26/2004
!
!        INPUTS:  Meteorology File Specifications
!
!        OUTPUTS: Meteorological Variables for One Hour
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Constants used in the computation of QSW
   DOUBLE PRECISION, PARAMETER :: C1=5.31D-13, C2=60.0D0, C3=1.12D0,&
   &STEFB= 5.67D-08
   DOUBLE PRECISION :: RN, Es25
   INTEGER :: IDYMAX(12), IJDAY, JFLAG, LEVEL
!     JAT D065 8/9/21
!     IOSI SET BUT NOT USED
!      INTEGER :: IUSI, ISSI, IOSI
   INTEGER :: IUSI, ISSI
   CHARACTER (LEN=8)   :: CUSI, CSSI, COSI
   CHARACTER (LEN=6)   :: Temp_METVER
   CHARACTER (LEN=256) :: BUFFER
   INTEGER :: I, NFLD
! Unused:      INTEGER :: IYR4
! Unused:       CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
! JAT 06/22/21 D065
! REMOVE NTURB_Warnings AS UNUSED VARIABLE
!      INTEGER :: NTURB_Warnings

!     Variable Initializations
   DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
   DATA Temp_METVER/'      '/

! --- Initialize variable to track warnings regarding use of turbulence
!     data with ADJ_U* option
! JAT 06/22/21 D065
! REMOVE NTURB_Warnings INITIALIZATION AS UNUSED VARIABLE
!      DATA NTURB_Warnings/0/

   MODNAM = 'METEXT'
   PATH   = 'MX'

!     Save Value of Last YR/MN/DY/HR and Previous Hour
   IPDATE = KURDAT
   IPYEAR = IYR
   IPHOUR = IHOUR

!     Initialize USTAR, OBULEN, SFCZ0, QSW, IPCODE, AND PRATE to ZERO for hour
   USTAR  = 0.0D0
   OBULEN = 0.0D0
   SFCZ0  = 0.0D0
!jop  ZDM    = 0.0D0
   QSW    = 0.0D0
   IPCODE = 0
   PRATE  = 0.0D0

!     JAT D070 intialize reset_sa and reset_sw
   RESET_SA=.FALSE.
   RESET_SW=.FALSE.

! --- Increment line counter for messages related to met data file
   ILINE = ILINE + 1

! --- Set 'DUMMY' variable = 'SURFFILE' for error handling
   DUMMY = 'SURFFILE'

   IF (IMONTH .EQ. 12 .AND. IDAY .EQ. 31 .AND. IHOUR .EQ. 24) THEN
!        End of year has been reached - check for presence of header
!        record at beginning of next year for multi-year data files.
      READ(MFUNIT,'(A256)',ERR=998,END=1000,IOSTAT=IOERRN) BUFFER

      IF (INDEX(BUFFER,':') .EQ. 0) THEN
!           Record does not contain colon. Assume it must be regular
!           met data record, so backspace met file before proceeding.
         BACKSPACE MFUNIT
      ELSE
!           Record contains colon. Assume it is a header record;
!           check for AERMET version date, C_METVER, and also check
!           station IDs before proceeding to flag potential
!           use of different stations in multi-year data files.
!           Convert UAIR and SURF character IDs to integers.
! ---       First extract AERMET version date, Temp_METVER
         NumHeaders = NumHeaders + 1
         IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
!              Extract AERMET version date
            READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:&
            &INDEX(BUFFER,'VERSION:')+13),'(A6)')&
            &Temp_METVER
         ELSE
!              AERMET version not found in header record, issue fatal error message
            CALL ERRHDL(PATH,MODNAM,'E','395','No Version')
         ENDIF
         IF (Temp_METVER .NE. C_METVER) THEN
!              AERMET version not found in header record, or different AERMET versions
!              were used; issue fatal error message
            CALL ERRHDL(PATH,MODNAM,'E','395',Temp_METVER)
         ENDIF

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

         IF( INDEX(BUFFER,'OS_ID:') .GE. 0 )THEN
            READ(BUFFER(INDEX(BUFFER,'OS_ID:')+7:&
            &INDEX(BUFFER,'OS_ID:')+15),'(A)') COSI
         ELSE
            COSI = '        '
         END IF
         CALL STONUM(COSI,8,FNUM,IMIT)
!           JAT D065 IOSI NOT USED OTHER THAN BEING SET HERE
!           COMMENT OUT THIS CODE
!            IF (IMIT .EQ. 1) THEN
!               IOSI = NINT(FNUM)
!            ELSE
!               IOSI = 0
!            END IF

! ----      Check for consistency between UA and SF station IDs in header record
!           with values input by user on the ME pathway
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

!
!---- READ surface scaling meteorology data based on free format
!

   IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
!        Check for deposition variables on first data record
!        CERC 11/30/20 Calculation of QSW also needed for GRSM
      IF (ILINE .EQ. 1) THEN
         NFLD  = 0
         INFLD = .FALSE.
         READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           modified 12/11/17 to account for use of MMIF straight to AERMOD
!           so user doesn't get misleading warning message
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         IF( INDEX(BUFFER,'NAD') .GT. 0 .OR.&
         &INDEX(BUFFER,'ADJ') .GT. 0 .OR.&
         &INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR.&
         &INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.&
         &INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR.&  !JAT D077
         &INDEX(BUFFER,'PROG-OS') .GT. 0 )THEN !JAT D077
            L_NAD_ADJ_Flags = .TRUE.
         ENDIF
         DO I = 1, LEN_TRIM(BUFFER)
            IF (.NOT.INFLD .AND. BUFFER(I:I).NE.' ' .AND.&
            &BUFFER(I:I).NE.',') THEN
!                 Set Mark of in a Field
               INFLD = .TRUE.
!                 Increment the Field Counter
               NFLD = NFLD + 1
            ELSE IF (INFLD .AND. (BUFFER(I:I).EQ.' ' .OR.&
            &BUFFER(I:I).EQ.',')) THEN
!                 Location is the End of a Field
!                 Set Mark of Not In a field
               INFLD = .FALSE.
            END IF
         END DO
         IF (NFLD .LT. 25 .AND. (LDPART .OR. LWPART .OR.&
         &LDGAS .OR. LWGAS .OR.&
         &GRSM) ) THEN
!              Met record does not have enough fields,
!              deposition variables may be missing
            CALL ERRHDL(PATH,MODNAM,'E','495','with DEP')
            RUNERR = .TRUE.
            EOF = .TRUE.
            GO TO 99
         ELSE
            BACKSPACE MFUNIT
         END IF

      ELSE IF (ILINE .GT. 1) THEN
         READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           JAT add MMIF to AERMOD in flag check 12/11/17
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         IF( INDEX(BUFFER,'NAD') .GT. 0 .OR.&
         &INDEX(BUFFER,'ADJ') .GT. 0 .OR.&
         &INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR.&
         &INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.&
         &INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR.& !JAT D077
         &INDEX(BUFFER,'PROG-OS') .GT. 0)THEN !JAT D077
            L_NAD_ADJ_Flags = .TRUE.
         ELSE
! ---          Wind data source/adjustment flag is missing
            L_NAD_ADJ_Flags = .FALSE.
         ENDIF
         BACKSPACE MFUNIT

      ENDIF

! ---    Read record from ASCII scalar parameter file using FREE format
!        with deposition variables
!
      READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
      &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
      &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
      &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
      &SFCP, NCLOUD

!        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
!        albedo and cloud cover, for use in gas deposition algorithm.
      IF (OBULEN.GT.0.0D0 .OR. OBULEN.LT.-99990.0D0 .OR.&
      &TA.LT.0.0D0 .OR.&
      &ALBEDO.EQ.1.0D0 .OR. BOWEN.EQ.0.0D0) THEN
!           Hour is stable or missing or inappropriate surface chars.
         QSW = 0.0D0
      ELSE
         RN  = (1.0D0 + 1.0D0/BOWEN)*SFCHF/0.9D0
         QSW = (RN*(1.0D0+C3) - C1*TA**6 + STEFB*TA**4 -&
         &C2*0.1D0*DBLE(NCLOUD))/&
         &(1.0D0-ALBEDO)
      END IF
!
!        Set variables for dry deposition
      IF (LDPART .OR. LDGAS) THEN
         IF (Ta.LT.0.0D0 .OR. PRATE.LT.0.0D0) THEN
            Wnew = Wold
         ELSE
! ...          Compute saturation vapor pressure based on CMAQ formula
            EsTa = 0.6112D0 * DEXP(19.83D0 - 5417.4D0/Ta)
            Es25 = 3.167D0
            Wnew = Wold+Prec1-0.5D0*f2*EsTa/Es25
            Wold = Wnew
            f2 = Wnew/200.D0
            if (f2.le.0.01D0) f2 = 0.01D0
            if (f2.gt.1.0D0) f2 = 1.0D0
         END IF
      END IF

   ELSE
!        Read record from ASCII scalar parameter file without deposition
!        parameters, using FREE format
!        Check for number of data fields on first data record
      IF (ILINE .EQ. 1) THEN
         NFLD  = 0
         INFLD = .FALSE.
         READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           modified 12/11/17 to account for use of MMIF straight to AERMOD
!           so user doesn't get misleading warning message
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         IF( INDEX(BUFFER,'NAD') .GT. 0 .OR.&
         &INDEX(BUFFER,'ADJ') .GT. 0 .OR.&
         &INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR.&
         &INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.&
         &INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR.& !JAT D077
         &INDEX(BUFFER,'PROG-OS') .GT. 0)THEN !JAT D077
            L_NAD_ADJ_Flags = .TRUE.
         ENDIF
         DO I = 1, LEN_TRIM(BUFFER)
            IF (.NOT.INFLD .AND. BUFFER(I:I).NE.' ' .AND.&
            &BUFFER(I:I).NE.',') THEN
!                 Set Mark of in a Field
               INFLD = .TRUE.
!                 Increment the Field Counter
               NFLD = NFLD + 1
            ELSE IF (INFLD .AND. (BUFFER(I:I).EQ.' ' .OR.&
            &BUFFER(I:I).EQ.',')) THEN
!                 Location is the End of a Field
!                 Set Mark of Not In a field
               INFLD = .FALSE.
            END IF
         END DO
         IF (NFLD .LT. 20) THEN
!              Met record does not include enough variables
            CALL ERRHDL(PATH,MODNAM,'E','495','Non-DEP ')
            RUNERR = .TRUE.
            EOF = .TRUE.
            GO TO 99
         ELSE
            BACKSPACE MFUNIT
         END IF

      ELSE IF (ILINE .GT. 1) THEN
         READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           modified 12/11/17 to account for use of MMIF straight to AERMOD
!           so user doesn't get misleading warning message
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         IF( INDEX(BUFFER,'NAD') .GT. 0 .OR.&
         &INDEX(BUFFER,'ADJ') .GT. 0 .OR.&
         &INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR.&
         &INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.&
         &INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR.& !JAT D077
         &INDEX(BUFFER,'PROG-OS') .GT. 0)THEN !JAT D077
            L_NAD_ADJ_Flags = .TRUE.
         ELSE
! ---          Wind data source/adjustment flag is missing
            L_NAD_ADJ_Flags = .FALSE.
         ENDIF

         BACKSPACE MFUNIT

      END IF
!
      READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
      &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
      &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
      &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
      &SFCP, NCLOUD
!  ****** FOR HIGHLY BUOYANT PLUME ****** added code JAN 2023--kja
! ** Get next hours mixing heights in needed
! ** Read next hour to get next mixing height for unstable conditions
      IF (HBPLUME) THEN
         IF (OBULEN .LT. 0.0D0 .AND. OBULEN .GT. -9.9D4) THEN
            READ( MFUNIT, *, END=1006, ERR=99, IOSTAT=IOERRN ) IYEAR,&
            &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
            &VPTGZI, ZICONVN, ZIMECHN, OBULEN, SFCZ0, BOWEN, ALBEDO,&
            &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
            &SFCP, NCLOUD
! ** Check for missing next hour IE. OBULEN =-99999.0
            IF(OBULEN .LT. -9.9D4) GOTO 1006
            BACKSPACE MFUNIT
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
            &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
            &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
            &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
            &SFCP, NCLOUD
            GOTO 1003
1006        ZICONVN = -999.0D0
            ZIMECHN = -999.0D0
            BACKSPACE MFUNIT
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,&
            &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
            &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
            &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
            &SFCP, NCLOUD
1003        CONTINUE
         ELSE
            ZICONVN = -999.0D0
            ZIMECHN = -999.0D0
         ENDIF
      ENDIF
!  **************************************  added code end --kja

   END IF

! --- Check for L_NAD_ADJ_Flags; if surface file header shows current
!     version date (i.e., L_OldMetVer=.F.), but the wind data
!     source/adj flags are missing (i.e., L_NAD_ADJ_Flags = .FALSE.)
!     issue warning message, but only issue warning once.
   IF( .NOT. L_OldMetVer .AND. .NOT. SCREEN .AND.&
   &IMETMSG.EQ.0 .AND. .NOT. L_NAD_ADJ_Flags )THEN
! ---    Set L_OldMetVer = .T.
      L_OldMetVer = .TRUE.
      CALL ERRHDL(PATH,MODNAM,'W','394','No NAD/ADJ')
      IMETMSG = IMETMSG + 1
   ENDIF

!     Set the stability logical variables, which are needed in COMPTG
   IF( OBULEN .GT. 0.0D0 ) THEN
      UNSTAB = .FALSE.
      STABLE = .TRUE.
   ELSE
      UNSTAB = .TRUE.
      STABLE = .FALSE.
   ENDIF

!---- Initialize the profile data to missing;
!     READ profile data based on free format
!
   CALL PFLINI ()
   LEVEL = 1
   JFLAG = 0
!     Read record from ASCII profile file using FREE format; compute
!     sigma_V from sigma_A and wind speed

! --- Set 'DUMMY' variable = 'PROFFILE' for error handling
   DUMMY = 'PROFFILE'

! --- Initialize logical variable to track for turbulence data
   L_TurbData = .FALSE.

! --- First loop through PROFFILE to determine if turbulence data
!     are present
   DO WHILE( JFLAG .EQ. 0 )
      READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,&
      &KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,&
      &PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),&
      &PFLSA(LEVEL), PFLSW(LEVEL)

!        Convert the data to the required units
      CALL PFLCNV (LEVEL)

! ---    Check for observed turbulence parameters in PROFFILE file
      IF( (PFLSA(LEVEL).GT.0.0D0 .AND. PFLSA(LEVEL).LT.99.0D0) .OR.&
      &(PFLSW(LEVEL).GT.0.0D0 .AND. PFLSW(LEVEL).LT.99.0D0) )THEN
         L_TurbData = .TRUE.
      ENDIF

!        Set the number of profile levels to current index, store
!        the 'top of profile' flag, and increment level if not at top
!        Check that the level does not exceed the maximum allowable
      NPLVLS = LEVEL
      IFLAG(LEVEL) = JFLAG

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

! ---    Check for observed turbulence parameters in PROFFILE file
      IF( PFLSA(LEVEL).GT.0.0D0 .AND. PFLSA(LEVEL).LT.99.0D0 )THEN
         L_TurbData = .TRUE.
         L_Got_SigA = .TRUE.
      ENDIF

      IF( PFLSW(LEVEL).GT.0.0D0 .AND. PFLSW(LEVEL).LT.99.0D0 )THEN
         L_TurbData = .TRUE.
         L_Got_SigW = .TRUE.
      ENDIF

   ENDDO

! --- Need to check for TURB with DFAULT
! --- and adjusted U*, IF both turbulence and
! --- adjusted u* present, error !JAT 10/24/16
!!! --- We should always include turbulence data in AERMOD header
   IF( DFAULT .AND. L_TurbData .AND. L_AdjUstar )THEN
      IF( L_Got_SigA .AND. L_Got_SigW )THEN
         DUMMY = 'SigA & SigW'
      ELSEIF( L_Got_SigA)THEN
         DUMMY = 'SigA'
      ELSEIF( L_Got_SigW )THEN
         DUMMY = 'SigW'
      ENDIF
      CALL ERRHDL(PATH,MODNAM,'E','401',DUMMY)
      RUNERR = .TRUE.
   ENDIF
! --- Always include turbulence data in AERMOD header      ! RWB
   IF( L_TurbData )THEN
      IF( L_Got_SigA .AND. L_Got_SigW )THEN
         DUMMY = 'SigA & SigW'
      ELSEIF( L_Got_SigA )THEN
         DUMMY = 'SigA'
      ELSEIF( L_Got_SigW )THEN
         DUMMY = 'SigW'
      ENDIF
! ---    Issue error message if TurbData is used with ADJ_U*
      IF( L_AdjUstar .AND. DFAULT )THEN
         CALL ERRHDL(PATH,MODNAM,'E','401',DUMMY)
         RUNERR = .TRUE.
      ENDIF
   ENDIF

!     Compute the vertical potential temperature gradient profile
   IF( .NOT. RUNERR ) THEN
      NTGLVL = 0
      CALL COMPTG ()
   ENDIF

   IF (ILINE .EQ. 1) THEN
!        Write Out Sample of the Meteorology Data
!        (Up to the First 24 Hours)                         ---   CALL METDAT
      JDAY = IJDAY      ! Assign IJDAY to global variable JDAY
      IF( .NOT. L_SkipMessages ) CALL METDAT(IOUNIT)
      IF (SUMMFILE) THEN
! ---       Include sample of meteorological data in SUMMFILE
         IF( .NOT. L_SkipMessages ) CALL METDAT(ISUMUNT)
      END IF
   END IF

!     Set Meteorological Variables for Current Hour
   CALL SET_METDATA

   IF (ILINE .EQ. 1) THEN
! ---    First hour of met data file; check for IHOUR .ne. 01 with short-term averages
      IF (IHOUR .GT. 1 .AND. (PM25AVE .OR. NO2AVE .OR. SO2AVE)) THEN
         IF (L_MAXDCONT) THEN
!              Write Error Message: MAXDCONT option requires data file to begin with hour 1
            WRITE(DUMMY,'(''First Hr= '',I2.2)') IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','491',DUMMY)
!              Assign RUNERR logical to .T.
            RUNERR = .TRUE.
         ELSE IF (NO2AVE .OR. SO2AVE) THEN
!              Write Warning Message: 1hr NO2 & SO2 modeling should begin with hour 1
            WRITE(DUMMY,'(''First Hr= '',I2.2)') IHOUR
            CALL ERRHDL(PATH,MODNAM,'W','488',DUMMY)
         ELSE IF (PM25AVE) THEN
!              Write Warning Message: Short-term averages for first calendar day may not be valid
            CALL ERRHDL(PATH,MODNAM,'W','488','for 1st Day')
         END IF
      ELSE IF (IHOUR .GT. 1 .AND. (NUMAVE.GT.1 .OR.&
      &(NUMAVE.EQ.1 .AND.&
      &KAVE(1).NE.1)) ) THEN
!           Write Warning Message: Short-term averages for first calendar day may not be valid
         CALL ERRHDL(PATH,MODNAM,'W','488','for 1st Day')
      END IF

! ---    Check for start year based on met data file matching start year based on
!        ME SURFDATA keyword; note that IYR based on met data and ISYEAR based on
!        ME SURFDATA should both be 4-digits (a warning message will be issued if
!        ME SURFDATA input is not 4-digits)

      IF (IYR .NE. ISYEAR .AND. IMSTAT(7).EQ.0) THEN
! ---       Issue warning message that year specified on SURFDATA does not match first year
!           of data file; if DAYRANGE keyword not used (IMSTAT(7)=0), adjust ISYEAR to match
!           data file (IYR))
         WRITE(DUMMY,'(''StartYR '',I4)') IYR
         CALL ERRHDL(PATH,MODNAM,'W','492',DUMMY)
         ISYEAR = IYR

      ELSE IF (IYR .NE. ISYEAR .AND. IMSTAT(7).GT.0) THEN
! ---       Issue ERROR message that year specified on SURFDATA does not match first year
!           of data file when DAYRANGE keyword is being used (IMSTAT(7)>0).
         WRITE(DUMMY,'(''StartYR '',I4)') IYR
         CALL ERRHDL(PATH,MODNAM,'E','493',DUMMY)
         ISYEAR = IYR

         RUNERR = .TRUE.

      END IF

      IF (PM25AVE .OR. NO2AVE .OR. SO2AVE .OR.&
      &ANNUAL .OR. MULTYR) THEN
! ---       If PM-2.5 averaging, NO2 1-hour averaging, ANNUAL average, or MULTYEAR,
!           then need to set the variables for the "end-of-year" check if the STARTEND
!           keyword is not used
         IF (IMSTAT(6) .EQ. 0) THEN
! ---          Determine MN, DY, and HR for end-of-the-year check.
!              Subtract one from start hour to set end hour for the year of data;
!              adjust start day and month if needed.
            ISYR = IYEAR
            ISHR = IHOUR
            ISDY = IDAY
            ISMN = IMONTH

!              D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            CALL CENT_DATE(IYEAR,ISYR)      !Convert ISYR to four digits
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C              Convert ISYR to Four Digits
!               IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
!                  ISYR = ISTRT_CENT*100 + ISYR
!               ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!                  ISYR = (ISTRT_CENT+1)*100 + ISYR
!               END IF

            CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)

            IF (IHOUR .GT. 1) THEN
               IENDHOUR = IHOUR - 1
               IENDDY   = IDAY
               IENDMN   = IMONTH
            ELSE
               IENDHOUR = 24
               IF (IDAY .GT. 1) THEN
                  IENDDY = IDAY - 1
                  IENDMN = IMONTH
               ELSE
                  IENDMN = IMONTH - 1
                  IF (IENDMN .EQ. 0) IENDMN = 12
                  IENDDY = IDYMAX(IENDMN)
               END IF
            END IF
! ---          Determine ISDATE based on first hour of data file,
!              unless this is a restarted run
            IF (.NOT.RSTINP) ISDATE = FULLDATE
         END IF
      END IF

! ---    Check for potential conflicts between "start dates" and
!        first date of met data file
      IF (.NOT.MULTYR .AND. IMSTAT(6) .EQ. 1 .AND.&
      &FULLDATE .GT. ISDATE) THEN
! ---       Write Error Message:  Met data file starts later than
!           user-specified start date (ISDATE)
         WRITE(DUMMY,'(I10.10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'E','483',DUMMY)
         RUNERR = .TRUE.
      ELSE IF (.NOT.MULTYR .AND. RSTINP .AND.&
      &FULLDATE .GT. ISDATE) THEN
! ---       Write Error Message:  Met data file starts later than
!           start date (ISDATE) for Re-started model run
         WRITE(DUMMY,'(I10.10)') ISDATE-(ISDATE/100000000)*100000000
         CALL ERRHDL(PATH,MODNAM,'E','484',DUMMY)
         RUNERR = .TRUE.
      ELSE IF (MULTYR .AND. RSTINP .AND.&
      &FULLDATE .GT. ISDATE) THEN
! ---       Write Warning Message:  Met data file starts later than
!           start date (ISDATE) for restarted MULTYEAR run, indicating
!           gap between years of met data
         WRITE(DUMMY,'(I10.10)') ISDATE-(ISDATE/100000000)*100000000
         CALL ERRHDL(PATH,MODNAM,'W','485',DUMMY)
      ELSE IF (MULTYR .AND. RSTINP .AND. IMSTAT(6).EQ.0 .AND.&
      &FULLDATE .LT. ISDATE) THEN
! ---       Write Error Message:  Met data file starts earlier than
!           start date (ISDATE) for restarted MULTYEAR run, without the
!           STARTEND keyword, indicating a date overlap between years
!           of met data
         WRITE(DUMMY,'(I10.10)') ISDATE-(ISDATE/100000000)*100000000
         CALL ERRHDL(PATH,MODNAM,'E','487',DUMMY)
         RUNERR = .TRUE.
      END IF

   END IF

! --- Increment index for hour-of-year and year to save met data for
!     MAXDCONT option
   IF (FULLDATE .GE. ISDATE .AND. L_MAXDCONT) THEN
      IHR_NDX = IHR_NDX + 1
      IYR_NDX = NUMYRS + 1

      IF (IYR_NDX .GT. NYEARS) THEN
! ---       Year index exceeds maximum array limit, set
!           by NYEARS parameter in modules.f
! ---       Write Error Message        ! Too many years
         WRITE(DUMMY,'(''NYEARS='',I4)') NYEARS
         CALL ERRHDL(PATH,MODNAM,'E','482',DUMMY)
         RUNERR = .TRUE.
         GO TO 999
      END IF

! ---    Store hourly met data to arrays for MAXDCONT option
      IF (.NOT.RSTINP .AND. L_MAXDCONT) THEN
         ASFCHF(IHR_NDX,IYR_NDX)  =  SFCHF
         AUREF(IHR_NDX,IYR_NDX)   =  UREF
         AUREFHT(IHR_NDX,IYR_NDX) =  UREFHT
         ATA(IHR_NDX,IYR_NDX)     =  TA
         ATREFHT(IHR_NDX,IYR_NDX) =  TREFHT
         AWDREF(IHR_NDX,IYR_NDX)  =  WDREF
         AUSTAR(IHR_NDX,IYR_NDX)  =  USTAR
         AWSTAR(IHR_NDX,IYR_NDX)  =  WSTAR
         AZICONV(IHR_NDX,IYR_NDX) =  ZICONV
         AZIMECH(IHR_NDX,IYR_NDX) =  ZIMECH
         AOBULEN(IHR_NDX,IYR_NDX) =  OBULEN
         AVPTGZI(IHR_NDX,IYR_NDX) =  VPTGZI
         ASFCZ0(IHR_NDX,IYR_NDX)  =  SFCZ0
         AKST(IHR_NDX,IYR_NDX)    =  KST
! Added for HBP; JAN. 2023
         IF (HBPLUME) THEN
            AZICONVN(IHR_NDX,IYR_NDX) = ZICONVN
            AZIMECHN(IHR_NDX,IYR_NDX) = ZIMECHN
         ENDIF
! End HBP Insert
         IF (LDGAS .OR. LDPART .OR. LWPART .OR. LWGAS .OR.&
         &GRSM)THEN
            ABOWEN(IHR_NDX,IYR_NDX)  =  BOWEN
            AALBEDO(IHR_NDX,IYR_NDX) =  ALBEDO
            IAPCODE(IHR_NDX,IYR_NDX) =  IPCODE
            APRATE(IHR_NDX,IYR_NDX)  =  PRATE
            ARH(IHR_NDX,IYR_NDX)     =  RH
            ASFCP(IHR_NDX,IYR_NDX)   =  SFCP
            NACLOUD(IHR_NDX,IYR_NDX) =  NCLOUD
            AQSW(IHR_NDX,IYR_NDX)    =  QSW
            AWnew(IHR_NDX,IYR_NDX)   =  Wnew
            Af2(IHR_NDX,IYR_NDX)     =  f2
            AEsTa(IHR_NDX,IYR_NDX)   =  EsTa
            APrec1(IHR_NDX,IYR_NDX)  =  Prec1
            APrec2(IHR_NDX,IYR_NDX)  =  Prec2
         END IF
         IF (L_BLSOURCE) THEN
            ABLTA(IHR_NDX,IYR_NDX)  = TA
         END IF
         ARURUSTR(IHR_NDX,IYR_NDX)   = RURUSTR
         ARUROBULEN(IHR_NDX,IYR_NDX) = RUROBULEN

         ACLMHR(IHR_NDX,IYR_NDX)   =  CLMHR
         AMSGHR(IHR_NDX,IYR_NDX)   =  MSGHR
         AUNSTAB(IHR_NDX,IYR_NDX)  =  UNSTAB
         ASTABLE(IHR_NDX,IYR_NDX)  =  STABLE
         AURBSTAB(IHR_NDX,IYR_NDX) =  URBSTAB

         ANDX4ZI(IHR_NDX,IYR_NDX) = NDX4ZI
         AUATZI(IHR_NDX,IYR_NDX)  = UATZI
         ASVATZI(IHR_NDX,IYR_NDX) = SVATZI
         ASWATZI(IHR_NDX,IYR_NDX) = SWATZI
         AUAVG(IHR_NDX,IYR_NDX)   = UAVG
         ASVAVG(IHR_NDX,IYR_NDX)  = SVAVG
         ASWAVG(IHR_NDX,IYR_NDX)  = SWAVG
         APTATZI(IHR_NDX,IYR_NDX) = PTATZI

         AGRIDWD(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDWD(1:MXGLVL)
         AGRIDWS(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDWS(1:MXGLVL)
         AGRIDSW(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDSW(1:MXGLVL)
         AGRIDSV(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDSV(1:MXGLVL)
         AGRIDTG(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDTG(1:MXGLVL)
         AGRIDPT(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDPT(1:MXGLVL)
         IF (NSEC .GT. 0) THEN
            AGRIDRHO(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDRHO(1:MXGLVL)
         END IF
         IF (PVMRM .OR. GRSM) THEN
            AGRIDEPS(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDEPS(1:MXGLVL)
         END IF
         IF (NURB .GT. 0) THEN
            AGRDSWR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDSWR(1:MXGLVL)
            AGRDSVR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDSVR(1:MXGLVL)
            AGRDTGR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDTGR(1:MXGLVL)
            AGRDPTR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDPTR(1:MXGLVL)

            DO I = 1, NURB
               AGRDSWU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDSWU(1:MXGLVL,I)
               AGRDSVU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDSVU(1:MXGLVL,I)
               AGRDTGU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDTGU(1:MXGLVL,I)
               AGRDPTU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDPTU(1:MXGLVL,I)
!RWB              Add variables for URBAN option
               AZIURB(IHR_NDX,IYR_NDX,I)     = ZIURB(I)
               AURBWSTR(IHR_NDX,IYR_NDX,I)   = URBWSTR(I)
               AURBUSTR(IHR_NDX,IYR_NDX,I)   = URBUSTR(I)
               AURBOBULEN(IHR_NDX,IYR_NDX,I) = URBOBULEN(I)
!RWB              Also include L_MorningTrans array for urban morning transition
               AL_MorningTrans(IHR_NDX,IYR_NDX,I) = L_MorningTrans(I)
            END DO
         END IF

      END IF

   END IF

   GO TO 999

!---- End-of-file and error handling for METEXT
!
!     WRITE Error Messages:  Error Reading Met Data File

98 CALL ERRHDL(PATH,MODNAM,'E','510','PROFFILE')
   RUNERR = .TRUE.
   GO TO 999

99 CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
   RUNERR = .TRUE.
   GO TO 999

1000 EOF = .TRUE.

! --- Check for EOF on first data record, ILINE=1
   IF (ILINE .EQ. 1) THEN
!        Write Error Message for EOF on first data record
      CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
      RUNERR = .TRUE.
   END IF

999 RETURN
END

SUBROUTINE SET_METDATA
!***********************************************************************
!                 SET_METDATA Module of AERMOD Model
!
!        PURPOSE: Sets the meteorological data variables for current hour
!
!        PROGRAMMER: ROGER BRODE
!
!        DATE:    May 12, 1999
!
!        MODIFIED:   To include IPROCL array for identifying which
!                    Julian days to process for Leap Years (based on
!                    the year specified on the ME SURFDATA keyword)
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 06/30/2015
!
!                    To include call to GRDEPS, for calculation of
!                    gridded turbulence dissipation rate for use in the
!                    PVMRM algorithm.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!                    To include determination of the day-of-week index
!                    (1 for Weekday [M-F], 2 for Saturday, 3 for Sunday)
!                    for use in the option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METEXT
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE

   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Declare Arrays for Use With Day/Date Calcs
! JAT 06/22/21 D065
! REMOVE ISEA_NDX AS UNUSED VARIABLE
   INTEGER :: NDAY(12) !, ISEA_NDX(12)
   INTEGER :: I, NL, NUMSW
   DOUBLE PRECISION :: SUMSW, FVREF
! Unused:       INTEGER ::  IA, IY, IM, ID,

!     Variable Initializations
   DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
! JAT 06/22/21 D065
! REMOVE ISEA_NDX INITITALIZATION AS UNUSED VARIABLE
!      DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/

   MODNAM = 'SET_METDATA'

!---- Set lower limit of 1.0 meter for mixing heights
   IF (ZICONV.GE.0.0D0 .AND. ZICONV.LT.1.0D0) ZICONV = 1.0D0
   IF (ZIMECH.GE.0.0D0 .AND. ZIMECH.LT.1.0D0) ZIMECH = 1.0D0

!     Set the date variables for this hour
   CALL SET_DATES

   IF (MONTH .AND. IHOUR .EQ. 24) THEN
!        Check for the End of the Month
      IF (IMONTH .EQ. 1 .OR. (MOD(IYR,4) .NE. 0) .OR.&
      &(MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
!           Not a Leap Year OR Month = January
         IF (JDAY .EQ. NDAY(IMONTH)) THEN
            ENDMON = .TRUE.
         END IF
      ELSE
!           Leap Year AND Month > January
         IF (JDAY .EQ. NDAY(IMONTH)+1) THEN
            ENDMON = .TRUE.
         END IF
      END IF
   END IF

!     Check Data for Calms, Missing, Out-of-Range Values    ---   CALL METCHK
   IF (.NOT. L_SkipMessages) CALL METCHK

!     Limit ZI to 4000 meters.
   IF (ZICONV .GT. 4000.D0) ZICONV = 4000.D0
   IF (ZIMECH .GT. 4000.D0) ZIMECH = 4000.D0
!     Select appropriate mixing height from convective and mechanical values
   IF (.NOT.MSGHR .AND. .NOT.CLMHR .AND. OBULEN.LT.0.0D0) THEN
      ZI = MAX ( ZICONV, ZIMECH )
   ELSE IF (.NOT.MSGHR .AND. .NOT.CLMHR) THEN
      ZI = ZIMECH
   ELSE
      ZI = -999.0D0
   END IF
!---- Set lower limit of 1.0 meter for mixing height
   IF (ZI.GE.0.0D0 .AND. ZI.LT.1.0D0) ZI = 1.0D0

! --- Assign ZI to ZIRUR for URBAN option
   IF (URBAN) ZIRUR = ZI

!     Apply ROTANG Adjustment to Wind Direction
   IF (DABS(ROTANG) .GT. 0.0000001D0) THEN
      WDREF = WDREF - ROTANG
      IF (WDREF .LE. 0.0D0) THEN
         WDREF = WDREF + 360.0D0
      END IF
      DO NL = 1, NPLVLS
         IF( PFLWD(NL) .GT. 0.0D0 )THEN
            PFLWD(NL) = PFLWD(NL) - ROTANG

            IF( PFLWD(NL) .LE. 0.0D0 )THEN
               PFLWD(NL) = PFLWD(NL) + 360.0D0
            ENDIF

         ENDIF
      END DO
   END IF

!---- Save the ambient temperature from the 'surface' file to a separate
!      variable for the bouyant line algorithms
   IF (L_BLSOURCE) THEN
      BLTA = TA
   ENDIF

!---- Initialize urban stable flag to false.
   IF(.NOT.L_SkipMessages) URBSTAB = .FALSE.

!
!---- Check the RUNERR flag - if it is FALSE, then there is sufficient
!     data to continue processing the data.
!     Also skip this IF-THEN block if L_SkipMessages is TRUE, which
!     indicates that this call is during the MAXDCONT "post-processing"
!     stage since the gridded profiles and other data have been retrieved
!     from arrays.
   IF( .NOT. RUNERR .AND. .NOT.L_SkipMessages )THEN
!
      IF( .NOT. CLMHR  .AND.  .NOT. MSGHR )THEN
!           Set the stability logical variables
         IF( OBULEN .GT. 0.0D0 )THEN
            UNSTAB = .FALSE.
            STABLE = .TRUE.
         ELSE
            UNSTAB = .TRUE.
            STABLE = .FALSE.
         ENDIF


         IF (FULLDATE.GE.ISDATE .AND.&
         &( (L_LeapYear .AND. IPROCL(JDAY).EQ.1) .OR.&
         &(.NOT.L_LeapYear .AND. IPROC( JDAY).EQ.1)) ) THEN
!
!              Initialize the gridded profile arrays
            GRIDSV = -99.0D0
            GRIDSW = -99.0D0
            GRIDWS = -99.0D0
            GRIDWD = -99.0D0
            GRIDTG = -99.0D0
            GRIDPT = -99.0D0
            IF (URBAN) THEN
               GRDSVR = -99.0D0
               GRDSVU = -99.0D0
               GRDSWR = -99.0D0
               GRDSWU = -99.0D0
               GRDTGR = -99.0D0
               GRDTGU = -99.0D0
               GRDPTR = -99.0D0
               GRDPTU = -99.0D0
            END IF

!              Compute gridded profile of epsilon for PVMRM or GRSM option
            IF (PVMRM .OR. GRSM) THEN
               GRIDEPS = -99.0D0
            END IF

!              Get the index from the array of gridded heights that
!              corresponds to the height immediately below ZI
            CALL LOCATE( GRIDHT, 1, MXGLVL, ZI, NDX4ZI )

!              Compute THETA_STAR and DTHDZ for the gridded
!              potential temperature gradient

            CALL TGINIT ()
!
!              Profile all variables here except sv and sw; defer sv
!              and sw until u at zi is known.
!
            CALL GRDWS ()
            CALL GRDWD ()
            CALL GRDPTG()
            CALL GRDPT ()

!----------    Compute density profile for PRIME
            CALL GRDDEN

!----------    Compute the parameter values at ZI; if ZI is above the
!              highest gridded profile level, use the value at the high-
!              est level
            IF( NDX4ZI .LT. MXGLVL )THEN
               CALL GINTRP( GRIDHT(NDX4ZI), GRIDWS(NDX4ZI),&
               &GRIDHT(NDX4ZI+1), GRIDWS(NDX4ZI+1),&
               &ZI, UATZI )
               CALL GINTRP( GRIDHT(NDX4ZI), GRIDPT(NDX4ZI),&
               &GRIDHT(NDX4ZI+1), GRIDPT(NDX4ZI+1),&
               &ZI, PTATZI )

            ELSE
               UATZI  = GRIDWS(MXGLVL)
               PTATZI = GRIDPT(MXGLVL)

            ENDIF
!
!              Add turbulence variables here
!
            CALL GRDSV ()

!              Obtain residual turbulence value before calling GRDSW
            NUMSW = 0
            SUMSW = 0.0D0

            DO I = 1, NPLVLS
               IF (PFLHT(I).GE.ZI .AND. PFLSW(I).GE.0.0D0) THEN
                  NUMSW = NUMSW + 1
                  SUMSW = SUMSW + PFLSW(I)
               END IF
            END DO

            IF (NUMSW .GT. 0) THEN
               SWRMAX = SUMSW / DBLE(NUMSW)
            ELSE
               SWRMAX = 0.02D0 * UATZI
            END IF

            CALL GRDSW ()

            IF( NDX4ZI .LT. MXGLVL )THEN
               CALL GINTRP( GRIDHT(NDX4ZI), GRIDSV(NDX4ZI),&
               &GRIDHT(NDX4ZI+1), GRIDSV(NDX4ZI+1),&
               &ZI, SVATZI )
               CALL GINTRP( GRIDHT(NDX4ZI), GRIDSW(NDX4ZI),&
               &GRIDHT(NDX4ZI+1), GRIDSW(NDX4ZI+1),&
               &ZI, SWATZI )
            ELSE
               SVATZI = GRIDSV(MXGLVL)
               SWATZI = GRIDSW(MXGLVL)
            END IF

!---           Compute the overbar (average) quantities for sigma_V, sigma_W,
!              and wind speed, from the surface up to ZI (formerly done in METINI)
            CALL ZIAVER (MXGLVL,GRIDHT,GRIDSV,ZI,NDX4ZI,SVAVG,SVATZI)
            CALL ZIAVER (MXGLVL,GRIDHT,GRIDSW,ZI,NDX4ZI,SWAVG,SWATZI)
            CALL ZIAVER (MXGLVL,GRIDHT,GRIDWS,ZI,NDX4ZI,UAVG,UATZI)

!              Compute gridded profile of epsilon for PVMRM/GRSM option
            IF (PVMRM .OR. GRSM) THEN
               CALL GRDEPS
            END IF

!              Compute Urban Profiles if Needed
            IF (URBAN) THEN
               CALL URBCALC
               CALL GRDURBAN
            END IF

         END IF
! JAT D070 WRITE MESSAGE THAT SIGMA-THETA OR SIGMA-W CHANGED DO NOT DO
! FOR NOTURB, NOSA, AND NOSW (UNDERSTOOD ALL HOURS ARE RESET)
         WRITE(DUMMY,'(I10.10)') FULLDATE
         IF (RESET_SA .AND. (TURBOPTS(2) .OR. TURBOPTS(3) .OR.&
         &TURBOPTS(6) .OR. TURBOPTS(8)))&
         &CALL ERRHDL(PATH,MODNAM,'I','445',DUMMY)
         IF (RESET_SW .AND. (TURBOPTS(2) .OR. TURBOPTS(3) .OR.&
         &TURBOPTS(7) .OR. TURBOPTS(9)))&
         &CALL ERRHDL(PATH,MODNAM,'I','446',DUMMY)
      ENDIF

   END IF

! --- Assign sector ID for direction-varying background O3 and NOx
!     based on flow vector from surface file, FVREF;
!     first check for valid wind direction
   IF (WDREF .LE. 0.0D0 .OR. WDREF .GT. 360.0D0) THEN
! ---    Hour is calm or missing; set IO3SECT = 0, INOXSECT = 0
      IO3SECT = 0
      INOXSECT = 0
   ELSE
! ---    Valid wind direction is available
      FVREF = WDREF + 180.0D0
      IF (FVREF .GT. 360.0D0) THEN
         FVREF = FVREF - 360.0D0
      END IF
      IF (L_O3Sector) THEN
         IF (FVREF .LT. O3SECT(1) .OR.&
         &FVREF .GE. O3SECT(NUMO3Sects) ) THEN
            IO3SECT = NUMO3Sects
         ELSE
            DO I = 1, NUMO3Sects-1
               IF (FVREF .GE. O3SECT(I) .AND.&
               &FVREF .LT. O3SECT(I+1)) THEN
                  IO3SECT = I
                  EXIT
               END IF
            END DO
         END IF
      ELSE
         IO3SECT = 1
      END IF
!        CERC 11/30/20
      IF (L_NOxSector) THEN
         IF (FVREF .LT. NOxSECT(1) .OR.&
         &FVREF .GE. NOxSECT(NUMNOxSects) ) THEN
            INOxSECT = NUMNOxSects
         ELSE
            DO I = 1, NUMNOxSects-1
               IF (FVREF .GE. NOXSECT(I) .AND.&
               &FVREF .LT. NOXSECT(I+1)) THEN
                  INOXSECT = I
                  EXIT
               END IF
            END DO
         END IF
      ELSE
         INOXSECT = 1
      END IF
   END IF

! --- Assign sector ID for direction-varying background
!     based on flow vector from surface file, FVREF;
!     first check for valid wind direction
   IF (WDREF .LE. 0.0D0 .OR. WDREF .GT. 360.0D0) THEN
! ---    Hour is calm or missing; set IBGSECT = 0
      IBGSECT = 0
   ELSE
! ---    Valid wind direction is available
      FVREF = WDREF + 180.0D0
      IF (FVREF .GT. 360.0D0) THEN
         FVREF = FVREF - 360.0D0
      END IF
      IF (L_BGSector) THEN
         IF (FVREF .LT. BGSECT(1) .OR.&
         &FVREF .GE. BGSECT(NUMBGSects) ) THEN
            IBGSECT = NUMBGSects
         ELSE
            DO I = 1, NUMBGSects-1
               IF (FVREF .GE. BGSECT(I) .AND.&
               &FVREF .LT. BGSECT(I+1)) THEN
                  IBGSECT = I
                  EXIT
               END IF
            END DO
         END IF
      ELSE
         IBGSECT = 1
      END IF
   END IF

!     Set Appropriate Wind Speed Category Index
   IF (UREF .LE. UCAT(1)) THEN
      IUCAT = 1
   ELSE IF (UREF .LE. UCAT(2)) THEN
      IUCAT = 2
   ELSE IF (UREF .LE. UCAT(3)) THEN
      IUCAT = 3
   ELSE IF (UREF .LE. UCAT(4)) THEN
      IUCAT = 4
   ELSE IF (UREF .LE. UCAT(5)) THEN
      IUCAT = 5
   ELSE
      IUCAT = 6
   END IF

   IF (FASTAREA .OR. L_BLSOURCE) THEN
!        Set Stability Category based on Golder (1972) for use with
!        FASTAREA Area Source Optimizations (formerly the TOXICS option)
      CALL LTOPG( KST )
   ELSE
! ---    Assign D stability as default (KST=4)
      KST = 4
   END IF

   IF (MSGHR .AND. .NOT. L_SkipMessages) THEN
      IF (.NOT. MSGPRO) THEN
!           Set Flag for Runtime Met. Error to Prevent Further Calculations
         RUNERR = .TRUE.
!           WRITE Error Message:  Missing Meteorological Data
         WRITE(DUMMY,'(I10.10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'E','460',DUMMY)
      ELSE IF (.NOT. L_SkipMessages) THEN
!           WRITE Informational Message:  Missing Meteorological Data
         WRITE(DUMMY,'(I10.10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'I','460',DUMMY)
      END IF
   END IF

   RETURN
END

SUBROUTINE SET_DATES
!***********************************************************************
!                 SET_DATES Module of AERMOD Model
!
!        PURPOSE: Sets the date variables for current hour
!
!        PROGRAMMER: ROGER BRODE
!
!        DATE:    May 12, 1999
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   SET_METDATA
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

! JAT 06/22/21 D065
! REMOVE NDAY AS UNUSED VARIABLE
!      INTEGER :: NDAY(12), ISEA_NDX(12)
   INTEGER :: ISEA_NDX(12)
   INTEGER :: IA, IY, IM, ID
   CHARACTER MODNAM*12
! Unused:       INTEGER :: I, NL

!     Variable Initializations
! JAT 06/22/21 D065
! REMOVE NDAY INITIALIZATION AS UNUSED VARIABLE
!      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
   DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/

   MODNAM = 'SET_DATES'

   IF (.NOT. L_SkipMessages) THEN
! ---    This call is not part of MAXDCONT processing,
!        otherwise date calculations are skipped.

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
      CALL CENT_DATE(IYEAR,IYR)
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Determine The Current Julian Day and Calculate Current Gregorian Date
!C        First Convert Year to 4-Digit Value
!         IF (IYEAR .GE. ISTRT_WIND .AND. IYEAR .LE. 99) THEN
!            IYR = ISTRT_CENT*100 + IYEAR
!         ELSE IF (IYEAR .LT. ISTRT_WIND) THEN
!            IYR = (ISTRT_CENT+1)*100 + IYEAR
!         ELSE
!C           Input IYEAR must be 4-digit:  Save to IYR and convert to 2-digit
!            IYR   = IYEAR
!            IYEAR = IYR - 100 * (IYR/100)
!         END IF

! ---    Assign L_LeapYear variable
      IF ((MOD(IYR,4) .NE. 0) .OR.&
      &(MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
!           Not a Leap Year
         L_LeapYear = .FALSE.
      ELSE
!           Leap Year
         L_LeapYear = .TRUE.
      END IF

! ---    Save previous Julian Day value
      IF (JDAY .GE. 1) THEN
         JDAY_PREV = JDAY
      ELSE
         JDAY_PREV = 0
      END IF

!        Determine Julian Day (Day of Year) Number, JDAY    ---   CALL JULIAN
      CALL JULIAN(IYR,IMONTH,IDAY,JDAY)

!        Calculate 8-digit Integer Variable for Current Date/Hour, KURDAT
!        and 10-digit Integer Variable (with 4-digit year), FULLDATE;
!        IYEAR = 2-digit year and IYR = 4-digit year
      KURDAT = IYEAR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR
      IF (IYR .GE. 2148) THEN
!           Write Error Message:  Input Year is > 2147.
         WRITE(DUMMY,'("YR= ",I4)') IYR
         CALL ERRHDL(PATH,MODNAM,'E','365',DUMMY)
         RUNERR = .TRUE.
         FULLDATE = 2147123124
      ELSE
         FULLDATE = IYR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR
      END IF

!        Check for 4-digit year input for profile data
      IF (KYEAR .GE. 100) THEN
         KYEAR = KYEAR - 100 * (KYEAR/100)
      END IF
      KURPFL = KYEAR*1000000 + KMONTH*10000 + KDAY*100 + KHOUR

   END IF

!     Determine SEASON index
   ISEAS = ISEA_NDX(IMONTH)

   IF (L_DayOfWeekOpts) THEN
! ---    An option requiring day-of-week is being used (EMISFACT/O3VALUES/BACKGRND)
!        Determine Day of Week (1 = Weekday [M-F], 2 = Saturday, 3 = Sunday).
!        Based on "Frequently Asked Questions about Calendars," Version 2.2,
!        by Claus Tondering, April 9, 2000, available on the web at URL
!        http://www.tondering.dk/claus/calendar.html
      IA = (14-IMONTH)/12
      IY = IYR - IA
      IM = IMONTH + 12*IA - 2
      ID = MOD( (IDAY + IY + IY/4 - IY/100 + IY/400 + (31*IM)/12), 7)
      IF (ID .GE. 1 .AND. ID .LE. 5) THEN
!           This is a weekday
         IDAY_OF_WEEK = 1
      ELSE IF (ID .EQ. 6) THEN
!           This is a Saturday
         IDAY_OF_WEEK = 2
      ELSE IF (ID .EQ. 0) THEN
!           This is a Sunday
         IDAY_OF_WEEK = 3
      END IF
      IF (ID .EQ. 0) THEN
!           This is a Sunday
         IDAY_OF_WEEK7 = 7
      ELSE
!           This is weekday or Saturday
         IDAY_OF_WEEK7 = ID
      END IF
   ELSE
! ---    Use 1 as default
      IDAY_OF_WEEK  = 1
      IDAY_OF_WEEK7 = 1
   END IF

   RETURN
END

SUBROUTINE METCHK
!***********************************************************************
!                 METCHK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs Various Checks and Quality Assurance of
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: JEFF WANG, ROGER BRODE
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To skip date sequence checking for EVENT processing,
!                    which is handled separately by EV_CHKDAT.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METEXT
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'METCHK'
   CLMHR  = .FALSE.
   MSGHR  = .FALSE.

   IF (.NOT.NOCHKD .AND. .NOT.EVONLY) THEN
!----    Check date for record out of sequence on the surface
!        scaling file - NOCHKD=.TRUE. means no date check   ---   CALL CHKDAT
      CALL CHKDAT
   END IF

!---- Compare date & time in the surface and profile files  ---   CALL CMPDAT
   CALL CMPDAT

!---- Check Data for Calm Winds                             ---   CALL CHKCLM
   CALL CHKCLM

   IF (.NOT. CLMHR) THEN
!----    Check data for missing data indicators             ---   CALL CHKMSG
      CALL CHKMSG
   END IF

!---- Check Data for Out-of-Range Values                    ---   CALL METQA
   CALL METQA

   RETURN
END

SUBROUTINE CHKDAT
!***********************************************************************
!                 CHKDAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks Meteorological Data for Record Out of Sequence
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Include additional checks to identify data gaps
!                    that consist of complete days and/or complete
!                    years.  A data gap of complete year(s) is treated
!                    as a non-fatal warning, since a year of met data
!                    may be skipped due to data capture problems.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   Include option to issue Warning rather than
!                    Fatal Error message for meteorological record
!                    out of sequence (WARNCHKD option on MODELOPT).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance.  Specifically, allow for
!                    transition from KURDAT=99123124 to KURDAT=00010101
!                    for new century.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Date Variable
!
!        OUTPUTS: Date Error Messages
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'CHKDAT'

! --- Check for Met Data Record Out of Sequence;
!     IPDATE and KURDAT are 8-digit date variables (YYMMDDHH) for
!     the previous hour and current hour, respectively
   IF (IPDATE .GT. 0) THEN
      IF (KURDAT .LE. IPDATE) THEN
! ---       Previous date is .LE. current date; check for date crossing
!           century mark.
         IF (KURDAT.NE.10101 .OR. IPDATE.NE.99123124) THEN
! ---          Record Out of Sequence; current date is same or earlier
!              than previous date
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         END IF
      ELSE IF (IHOUR.GT.1) THEN
! ---       Current hour > 01
         IF ((KURDAT-IPDATE) .EQ. 1) THEN
! ---          Record is in sequence - continue with processing
            CONTINUE
         ELSE IF ((KURDAT-IPDATE) .LT. 23) THEN
! ---          Gap is within the same day; issue message with date
!              of the gap and a second message with number of hours
!              within the gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            WRITE(DUMMY,'(I3,'' hour gap'')') KURDAT-IPDATE-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         ELSE
! ---          Gap extends beyond the current day; issue message with
!              date of the gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         END IF
      ELSE IF (IHOUR.EQ.1 .AND. IPHOUR.EQ.24) THEN
! ---       Current hour is 01 and previous hour is 24; look for
!           gaps between days
         IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.EQ.1 .AND.&
         &IYR.EQ.IPYEAR) THEN
! ---          No gap between days within same year; continue processing
            CONTINUE
         ELSE IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.GT.1 .AND.&
         &IYR.EQ.IPYEAR) THEN
! ---          Record Out of Sequence; gap of full day(s); issue message
!              with date of gap and second message with # of days in gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            WRITE(DUMMY,'(I3,'' day gap'')') JDAY-JDAY_PREV-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         ELSE IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.GT.1 .AND.&
         &IYR.GT.IPYEAR) THEN
! ---          Record Out of Sequence; gap between years; issue message
!              with date of gap and second message with # of days in gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            WRITE(DUMMY,'(I2,'' yr;'',I3,'' dy'')') IYR-IPYEAR,&
            &JDAY-JDAY_PREV-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         ELSE IF (JDAY.GT.1 .AND. JDAY_PREV.GE.JDAY .AND.&
         &IYR.GT.IPYEAR) THEN
! ---          Record Out of Sequence; gap between years; issue message
!              with date of gap and second message with # of days in gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            WRITE(DUMMY,'(I2,'' yr;'',I3,'' dy'')') IYR-IPYEAR-1,&
            &(365-JDAY_PREV)+JDAY-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         ELSE IF (JDAY.EQ.1 .AND. JDAY_PREV.GE.1 .AND.&
         &(IYR-IPYEAR.EQ.1) )THEN
! ---          Check for gap at end of previous year, accounting for leap years
            IF(  (MOD(IPYEAR,4) .NE. 0) .OR.&
            &(MOD(IPYEAR,100) .EQ. 0 .AND.&
            &MOD(IPYEAR,400) .NE. 0) )THEN
! ---             Previous year is not a leap year, check for previous JDAY < 365
               IF (JDAY_PREV.LT.365 ) THEN
! ---                Record Out of Sequence; gap at end of previous non-leap year;
!                    issue two warning messages, first with standard warning
!                    indicating location of gap and second warning indicating
!                    number of days in gap
                  WRITE(DUMMY,'(I8.8)') KURDAT
                  IF (.NOT. L_WARNCHKD) THEN
! ---                   Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                     RUNERR = .TRUE.
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                   Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
                  WRITE(DUMMY,'(I3,'' day gap'')') 365-JDAY_PREV
                  IF (.NOT. L_WARNCHKD) THEN
! ---                   Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                   Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
               END IF
            ELSE IF (JDAY_PREV .LT. 366) THEN
! ---             Previous year is a leap year, and previous JDAY < 366;
!                 Record Out of Sequence; gap at end of previous leap year;
!                 issue two warning messages, first with standard warning
!                 indicating location of gap and second warning indicating
!                 number of days in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
! ---                Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I3,'' day gap'')') 366-JDAY_PREV
               IF (.NOT. L_WARNCHKD) THEN
! ---                Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            END IF
         ELSE IF (IHOUR.EQ.1 .AND. JDAY.EQ.1 .AND.&
         &JDAY_PREV.GE.1 .AND.&
         &(IYR-IPYEAR).GT.1) THEN
! ---          Record Out of Sequence; gap of at least 1 complete year.
!              First check for additional gaps at the end of the previous
!              year, which would be an error (unless WARNCHKD is specified).
!
! ---          Check for gap at end of previous year, accounting for leap years
            IF(  (MOD(IPYEAR,4) .NE. 0) .OR.&
            &(MOD(IPYEAR,100) .EQ. 0 .AND.&
            &MOD(IPYEAR,400) .NE. 0) )THEN
! ---             Previous year is not a leap year, check for previous JDAY < 365
               IF (JDAY_PREV.LT.365 ) THEN
! ---                Record Out of Sequence; gap at end of previous non-leap year;
!                    issue two warning messages, first with standard warning
!                    indicating location of gap and second warning indicating
!                    number of days in gap
                  WRITE(DUMMY,'(I8.8)') KURDAT
                  IF (.NOT. L_WARNCHKD) THEN
! ---                   Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                     RUNERR = .TRUE.
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                   Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
                  WRITE(DUMMY,'(I3,'' day gap'')') 365-JDAY_PREV
                  IF (.NOT. L_WARNCHKD) THEN
! ---                   Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                   Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
               END IF
            ELSE IF (JDAY_PREV .LT. 366) THEN
! ---             Previous year is a leap year, and previous JDAY < 366;
!                 Record Out of Sequence; gap at end of previous leap year;
!                 issue two warning messages, first with standard warning
!                 indicating location of gap and second warning indicating
!                 number of days in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
! ---                Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I3,'' day gap'')') 366-JDAY_PREV
               IF (.NOT. L_WARNCHKD) THEN
! ---                Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---                Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            END IF

! ---          Now issue two warning messages regarding full year(s)
!              data gap, first with standard warning indicating location
!              of gap and second warning indicating number of years in gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            WRITE(DUMMY,'(I3,'' year gap'')') IYR-IPYEAR-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         END IF

      ELSE IF (IHOUR.EQ.1 .AND. IPHOUR.NE.24) THEN
! ---       Record Out of Sequence - gap between days; issue first
!           message with date of data gap, with a second message
!           with # hours/days in gap
         WRITE(DUMMY,'(I8.8)') KURDAT
         IF (.NOT. L_WARNCHKD) THEN
! ---          Write Error Message - Record out of sequence
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---          Write Warning Message - Record out of sequence
            CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
         END IF
         IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.GT.1 .AND.&
         &IYR.EQ.IPYEAR) THEN
! ---          Gap of at least 1 day; issue message with # of hours
!              and # of days.
            WRITE(DUMMY,'(I2,''hr & '',I3,''dy'')') 24-IPHOUR,&
            &JDAY-JDAY_PREV-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         ELSE IF (JDAY.EQ.1 .AND. JDAY_PREV.GE.1 .AND.&
         &(IYR-IPYEAR.EQ.1) )THEN
! ---          Record Out of Sequence; gap of full day(s); issue message
!              with date of gap and second message with # of days in gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            WRITE(DUMMY,'(I3,'' day gap'')') JDAY-JDAY_PREV-1
            IF (.NOT. L_WARNCHKD) THEN
! ---             Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
! ---             Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
         END IF
      END IF
   END IF

   RETURN
END

SUBROUTINE CMPDAT
!***********************************************************************
!             CMPDAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Compares the date and time from the scalar and profile
!                 files
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Date variables
!
!        OUTPUTS: Date error messages
!
!        ASSUMPTIONS:   <none>
!
!        CALLED FROM:   METCHK
!***********************************************************************

!---- Variable declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!---- Variable initializations
   MODNAM = 'CMPDAT'

!---- Check for a date mismatch between the scalar and profile files
!
   IF (KURDAT .NE. KURPFL) THEN
!        WRITE Error Message - Date mismatch
      WRITE(DUMMY,'(I8.8)') KURDAT
      CALL ERRHDL(PATH,MODNAM,'E','456',DUMMY)
      RUNERR = .TRUE.
!
   END IF

   RETURN
END
!
SUBROUTINE CHKCLM
!***********************************************************************
!                 CHKCLM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks One Hour Meteorological Data for Calm Winds
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Calm Hour Flag, CLMHR, and Message
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'CHKCLM'

!     Check Data for Calm Winds, defined as UREF = 0.0
   IF (UREF .EQ. 0.0D0) THEN
      CLMHR = .TRUE.
      IF (.NOT. L_SkipMessages) THEN
!           WRITE Informational Message: Calm Hour
         WRITE(DUMMY,'(I10.10)') FULLDATE
         CALL ERRHDL(PATH,MODNAM,'I','440',DUMMY)
      END IF
   END IF

   RETURN
END

SUBROUTINE CHKMSG
!***********************************************************************
!                 CHKMSG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks One Hour Meteorological Data for Missing Data
!
!        PROGRAMMER: JEFF WANG
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To Add Upper Bound on USTAR Range Check - 12/07/2006
!
!        MODIFIED:  To Change Wind Direction Range Check - 10/26/2004
!
!        MODIFIED:  To Change Temperature Range Check - 9/29/92
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'CHKMSG'

!---- Check Data for Missing Data Indicators
!
!     Wind speed (meters/second)
   IF( UREF .GE. 90.0D0 .OR. UREF .LT. 0.0D0 )THEN
      MSGHR = .TRUE.
!
!     Wind direction (degrees from north)
   ELSE IF( (WDREF .GT. 900.0D0)  .OR.  (WDREF .LE. -9.0D0) )THEN
      MSGHR = .TRUE.
!
!     Ambient temperature (kelvins)
   ELSE IF( (TA .GT. 900.0D0)  .OR.  (TA .LE. 0.0D0) )THEN
      MSGHR = .TRUE.
!
!     Monin-Obukhov length (meters)
   ELSE IF( OBULEN  .LT.  -99990.0D0 )THEN
      MSGHR = .TRUE.

!     Convective Mixing height (meters)
   ELSE IF( OBULEN .LT. 0.0D0 .AND.&
   &((ZICONV .GT. 90000.0D0)  .OR.  (ZICONV .LT. 0.0D0)) )THEN
      MSGHR = .TRUE.
!
!     Mechanical Mixing height (meters)
   ELSE IF( (ZIMECH .GT. 90000.0D0)  .OR.  (ZIMECH .LT. 0.0D0) )THEN
      MSGHR = .TRUE.
!
!     Surface friction velocity (meters/second)
   ELSE IF( USTAR  .LT.  0.0D0 .OR. USTAR .GE. 9.0D0 )THEN
      MSGHR = .TRUE.

!     Convective velocity scale (meters/second)
   ELSE IF( WSTAR .LT. 0.0D0 .AND.&
   &(OBULEN .LT. 0.0D0 .AND. OBULEN .GT. -99990.0D0) )THEN
      MSGHR = .TRUE.
!
   END IF

   RETURN
END

SUBROUTINE METQA
!***********************************************************************
!                 METQA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs Quality Assurance Checks for
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: JEFF WANG, ROGER BRODE
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Added range check for vertical potential temperature
!                   gradient above ZI (variable VPTGZI).  Also applies
!                   minimum value of 0.005 K/m for consistency with
!                   AERMET.
!                   R. Brode, MACTEC/PES, 10/17/2005
!
!        MODIFIED:  Included check for absolute values of Monin-Obukhov
!                   length (OBULEN) less than 1.0.  Adjustment of OBULEN
!                   is made to limit ABS(OBULEN) .GE. 1.0.  The sign of
!                   OBULEN is assigned the opposite of the sign of the
!                   heat flux if OBULEN is 0.0.  This limit on OBULEN is
!                   already applied in AERMET, so this change will
!                   only affect input data generated by other means.
!                   R. Brode, MACTEC/PES, 10/26/2004
!
!        MODIFIED:  To adjust warning limit for USTAR from 2.0 to 4.0,
!                   adjust warning limit for WSTAR from 3.0 to 4.0, and
!                   to minimize duplication of warning messages for
!                   missing hours.
!                   R. Brode, MACTEC/PES, 10/26/2004
!
!        MODIFIED:  To check for errors reading surface variables for
!                   new deposition algorithms.  R. Brode, PES, 12/6/94
!
!        MODIFIED:  To Change Temperature Range Check Lower Limit To
!                   230 K - 9/29/92
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: NL

!     Variable Initializations
   MODNAM = 'METQA'

!---- Check Data for Out-of-Range Values:

!---- Wind direction:
   IF( WDREF .EQ. 0.0D0)THEN
      WDREF = 360.0D0
   ENDIF

   DO NL = 1, NPLVLS
      IF( PFLWD(NL) .EQ. 0.0D0 )THEN
         PFLWD(NL) = 360.0D0
      END IF
   END DO

   IF( .NOT. L_SkipMessages )THEN
      IF( (WDREF.LT.  0.0D0 .AND. WDREF.GT.-9.0D0)  .OR.&
      &(WDREF.GT.360.0D0 .AND. WDREF.LT.900.0D0) )THEN
!           WRITE Warning Message: Invalid Wind Dir'n
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
      END IF

!----    Wind speed range:
      IF( UREF.LT.0.0D0 .AND. UREF.GT.-9.0D0)THEN
!           WRITE Warning Message: Invalid Wind Speed;
!           This case is already flagged as missing hour,
!           but not with standard missing data code
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
      END IF
!
      IF( UREF .GT. 30.0D0 .AND. UREF .LT. 90.0D0)THEN
!           WRITE Warning Message: Wind Speed Over 30m/s
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
      END IF

!----    Wind data reference height:
      IF( UREFHT  .GT.  100.0D0 )THEN
!
!           -----------------------------------------------
!           Height of the wind data to be used in the
!           computation is greater than 100m -  warn the user
!           -----------------------------------------------

         WRITE ( DUMMY, '(I8.8)' ) KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','475',DUMMY)

      ELSE IF( UREFHT .LT. 0.001D0 .AND.  .NOT.CLMHR .AND.&
      &.NOT.MSGHR)THEN
!
!           -----------------------------------------------
!           Height of the wind data to be used in the
!           computation is LT 0.001 for non-calm and
!           non-missing hour - this is an invalid entry;
!           issue fatal error message
!           -----------------------------------------------

         WRITE ( DUMMY, '(I8.8)' ) KURDAT
         CALL ERRHDL(PATH,MODNAM,'E','474',DUMMY)

         RUNERR = .TRUE.

      ENDIF

!----    Ambient temperature:
      IF( (TA .LT. 220.0D0 .AND. TA .GT. 0.0D0)  .OR.&
      &(TA .GT. 330.0D0 .AND. TA .LT. 900.0D0) )THEN
!           WRITE Warning Message: Ambient Temperature May be Out-of-Range
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','430',DUMMY)
      END IF

!----    Friction velocity (meters/second):
      IF( USTAR .GT. 4.0D0 )THEN
!           WRITE Warning Message: Friction velocity may be too large
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','432',DUMMY)
      END IF

   END IF

!---- Convective velocity (meters/second):
   IF( WSTAR .GT. 4.00D0 )THEN
!        WRITE Warning Message: Convective velocity may be too large
      IF( .NOT. L_SkipMessages )THEN
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','438',DUMMY)
      END IF
   ELSE IF( WSTAR .EQ. 0.0D0 )THEN
!        WRITE Warning Message: Convective velocity = 0.0, set to 0.001
      IF( .NOT. L_SkipMessages )THEN
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','438',DUMMY)
      END IF
      WSTAR = 0.001D0
   END IF

!---- Monin-Obukhov length (meters):
   IF( DABS(OBULEN) .LT. 1.00D0 )THEN
!        WRITE Warning Message: Monin-Obukhov length is too small
      IF( .NOT. L_SkipMessages )THEN
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','439',DUMMY)
      END IF
!        Set ABS(OBULEN) = 1.0D0
      IF (OBULEN .LT. 0.0D0) THEN
         OBULEN = -1.0D0
      ELSE IF (OBULEN .GT. 0.0D0) THEN
         OBULEN =  1.0D0
      ELSE
         OBULEN = -1.0D0 * DSIGN( 1.0D0, SFCHF )
      END IF
   END IF

!---- Vertical potential temperature gradient above ZI (K/m)
   IF( ZICONV .GT. 0.0D0 .AND. OBULEN .LT. 0.0D0 .AND.&
   &OBULEN .GT. -99990.0D0 .AND. VPTGZI .LT. 0.005D0 )THEN
!        WRITE Warning Message: VPTGZI less than or equal to 0.005 K/m
      IF( .NOT. L_SkipMessages )THEN
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','441',DUMMY)
      END IF
!        Adjust value to 0.005
      VPTGZI = 0.005D0
   ELSE IF( ZICONV .GT. 0.0D0 .AND. OBULEN .LT. 0.0D0 .AND.&
   &OBULEN .GT. -99990.0D0 .AND. VPTGZI .GT. 0.10D0 )THEN
!        WRITE Warning Message: VPTGZI is greater than 0.10 K/m
      IF( .NOT. L_SkipMessages )THEN
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','442',DUMMY)
      END IF
   END IF

!---- Surface roughness length (m):
   IF (SFCZ0 .LT. 0.0001D0) THEN
      IF (.NOT.MSGHR .AND. .NOT.CLMHR) THEN
!           WRITE Warning Message:  Surface roughness length out-of-range
         IF( .NOT. L_SkipMessages )THEN
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','435',DUMMY)
         END IF
      END IF
!        Set to 0.0001 to avoid divide-by-zero error
      SFCZ0 = 0.0001D0
   END IF

!---- Check for precipitation rate out of range
   IF (PRATE .LT. 0.0D0 .OR. PRATE .GT. 900.0D0) THEN
!        Assume precipitation is missing, set to 0.0
      PRATE = 0.0D0
   ELSE
! ---    Calculate total precipitation
      TOTAL_PRECIP = TOTAL_PRECIP + PRATE
   END IF

   RETURN
END

SUBROUTINE METDAT(IOUNT)
!***********************************************************************
!                 METDAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Summary Of The Meteorology Data
!
!        PROGRAMMER: JEFF WANG
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        MODIFIED:   To include output file unit argument to support
!                    output to main 'aermod.out' file and to the
!                    optional SUMMFILE.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To include met data version date from surface file.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED BY R.W. Brode, PES, to avoid print string > 132 chars.
!        (DATE:    December 29, 1997)
!
!        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        INPUTS:  Meteorology Input Data
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   METEXT, MEREAD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, ILMAX, IJDAY, IOUNT

!     Variable Initializations
   MODNAM = 'METDAT'

!---- WRITE Out Header Information
   CALL HEADER(IOUNT)
   WRITE(IOUNT,9011)
   ILMAX = MIN( 80, ILEN_FLD )
   WRITE(IOUNT,9016) METINP(1:ILMAX), C_METVER, PROINP(1:ILMAX),&
   &METFRM, PROFRM
   WRITE(IOUNT,9020) IDSURF, IDUAIR, SFNAME, UANAME,&
   &ISYEAR, IUYEAR
   WRITE(IOUNT,9024)
   IF (LDPART .OR. LDGAS .OR. LWPART .OR. LWGAS .OR. GRSM) THEN
      WRITE(IOUNT,99025)
   ELSE
      WRITE(IOUNT,9025)
   END IF

!---- Since the first record has been read, write out the data to
!     IOUNIT, then read the next record from the scalar file

   DO I = 1, 24
!----    Loop through first 24 hours of data file

!        We use the IF..ELSE structure because the global variable
!        for Julian day that is JDAY, not IJDAY. This avoids overwriting
!        JDAY read in METEXT.
      IF( I .EQ. 1 )THEN

         IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR.&
         &GRSM)THEN
            WRITE(IOUNT,99026) IYEAR, IMONTH, IDAY, IHOUR,&
            &SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,&
            &SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT,&
            &IPCODE, PRATE, RH, SFCP, NCLOUD
         ELSE
            WRITE(IOUNT,9026) IYEAR, IMONTH, IDAY, JDAY, IHOUR,&
            &SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,&
            &SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT
         END IF

      ELSE
         IF (IYEAR .GE. 100) THEN
            IYEAR = IYEAR - 100 * (IYEAR/100)
         END IF
         IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR.&
         &GRSM)THEN
            WRITE(IOUNT,99026) IYEAR, IMONTH, IDAY, IHOUR,&
            &SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,&
            &SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT,&
            &IPCODE, PRATE, RH, SFCP, NCLOUD
         ELSE
            WRITE(IOUNT,9026) IYEAR, IMONTH, IDAY, IJDAY, IHOUR,&
            &SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,&
            &SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT
         END IF

      END IF

      IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
!           Read record from ASCII scalar parameter file using FREE format
!           with deposition variables
         READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN) IYEAR,&
         &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
         &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
         &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
         &SFCP, NCLOUD
!
      ELSE
!           Read hourly records from ASCII file using FREE format
!           without deposition variables
         READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN ) IYEAR,&
         &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
         &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
         &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
         &SFCP, NCLOUD
!
      END IF
!
   END DO
!
!---- REWIND met file, skip first record (with the latitude &
!     longitude), and read first data record to reset variables
!     to the first hour in the file.
!
999 CONTINUE
   REWIND MFUNIT
   READ( MFUNIT, '(I2)' )  IDUM
!
   IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
!        Read record from ASCII scalar parameter file using FREE format
!        with deposition variables
      READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN) IYEAR,&
      &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
      &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
      &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
      &SFCP, NCLOUD

!
   ELSE
!        Read hourly records from ASCII file using FREE format
!        without deposition variables
      READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN ) IYEAR,&
      &IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,&
      &VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,&
      &UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,&
      &SFCP, NCLOUD

!
   END IF

!---- Write the first hour of profile data to IOUNIT; only 1 hour
!        is written because there can be up to 50 levels of data, which
!        could create a large amount of output.

   IF( NPLVLS .GT. 10 )THEN
      CALL HEADER(IOUNT)
   ENDIF

   WRITE (IOUNT, 9034)
   WRITE (IOUNT, 9035)
   DO I = 1,NPLVLS
      WRITE (IOUNT, 9036) KYEAR, KMONTH, KDAY, KHOUR, PFLHT(I),&
      &IFLAG(I), PFLWD(I), PFLWS(I), PFLTA(I), PFLSA(I),&
      &PFLSW(I), PFLSV(I)

   END DO
   WRITE (IOUNT,9037)

   GO TO 9999
!
!---- FORMAT statements
!
9011 FORMAT(/36X,'*** UP TO THE FIRST 24 HOURS OF ',&
   &'METEOROLOGICAL DATA ***'/)
9016 FORMAT(3X,'Surface file:   ',A80,3X,'Met Version: ',A6,&
   &/,3X,'Profile file:   ',A80,&
   &/,3X,'Surface format: ',A105,&
   &/,3X,'Profile format: ',A105 )
9020 FORMAT(3X,'Surface station no.: ',I8,18X,&
   &'Upper air station no.: ',I8/18X,'Name: ',A40,3X,&
   &'Name: ',A40/18X,'Year: ',I6,37X,'Year: ',I6)
9024 FORMAT (/' First 24 hours of scalar data')
9025 FORMAT (' YR',' MO', ' DY', ' JDY', ' HR', '     H0',&
   &'     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',&
   &'  M-O LEN', '    Z0', '  BOWEN', ' ALBEDO',&
   &'  REF WS', '   WD', '     HT', '  REF TA', '     HT',&
   &/61('- '))
99025 FORMAT (' YR',' MO', ' DY', ' HR', '     H0',&
   &'     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',&
   &'  M-O LEN', '  Z0 ', 'BOWEN', '  ALB',&
   &'  REF WS', '   WD', '   HT', '  REF TA', '  HT',&
   &' IPCOD',' PRATE','  RH',' SFCP',' CCVR'&
   &/66('- '))
9026 FORMAT ( 1X,3(I2.2,1X),I3,1X,I2.2,1X,F6.1,1X,3(F6.3,1X),&
   &2(F5.0,1X),F8.1,1X,F5.2,1X,2(F6.2,1X),F7.2,1X,F5.0,&
   &3(1X,F6.1) )
99026 FORMAT ( 1X,3(I2.2,1X),I2.2,1X,F6.1,1X,3(F6.3,1X),&
   &2(F5.0,1X),F8.1,3F5.2,1X,F7.2,1X,F5.0,&
   &1X,F4.0,1X,F6.1,1X,F4.0,I3,F7.2,F6.0,F6.0,I3)
9034 FORMAT (//,' First hour of profile data')
9035 FORMAT ( ' YR', ' MO', ' DY', ' HR', ' HEIGHT', ' F', '  WDIR',&
   &'    WSPD',' AMB_TMP', ' sigmaA', '  sigmaW',&
   &'  sigmaV' )
9036 FORMAT (1X, 4(I2.2,1X),F6.1,1X,I1,1X,F5.0,1X,F7.2,1X,F7.1,1X,&
   &F6.1,1X,F7.2,1X,F7.2)
9037 FORMAT (/ ' F indicates top of profile (=1) or below (=0)')

!---- WRITE Error Message:  Error Reading Met Data Input File
!
99 CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
   RUNERR = .TRUE.

9999 RETURN
END

SUBROUTINE METSUM
!***********************************************************************
!                 METSUM Module of AERMOD Model
!
!        PURPOSE: Print Out The Summary Of The Meteorology Data
!                 Sampled Using the SCIM Option
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    April 14, 1998
!
!        MODIFIED:  To output missing temperatures correctly in the
!                   SCIM met data file.
!                   R.W. Brode, PES, Inc., - 02/25/02
!
!        INPUTS:  Meteorology Input Data
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: PFLTEMP   ! Add PFLTEMP to
   INTEGER :: I, ILMAX

!     Variable Initializations
   MODNAM = 'METSUM'

!     WRITE Out Header Information
   IF (ILINE .EQ. IFIRSTHR) THEN
      WRITE(ISUNIT,9011)
!        Write Surface Data, including user-specified SCIMBYHR parameters
      WRITE(ISUNIT,'(A,1x,I3)')  ' Start Hr: ', IFIRSTHR
      WRITE(ISUNIT,'(A,1x,I3)')  ' Interval: ', NREGINT
      WRITE(ISUNIT,*)
      ILMAX = MIN( 80, ILEN_FLD )
      WRITE(ISUNIT,9016) METINP(1:ILMAX), METFRM
      WRITE(ISUNIT,9020) IDSURF, IDUAIR, SFNAME, UANAME,&
      &ISYEAR, IUYEAR

!        Write column headers, depending on whether deposition is included,
!        consistent with WRITE statements below.
! ---    Use FORMAT 98025 for all cases
      WRITE(ISUNIT,98025)

!        Write Profile Data
      WRITE(IPUNIT,99011)
!        Write Surface Data, including user-specified SCIMBYHR parameters
      WRITE(IPUNIT,'(A,1x,I3)') ' Start Hr: ', IFIRSTHR
      WRITE(IPUNIT,'(A,1x,I3)') ' Interval: ', NREGINT
      WRITE(IPUNIT,*)
      ILMAX = MIN( 80, ILEN_FLD )
      WRITE(IPUNIT,99016) PROINP(1:ILMAX), PROFRM
      WRITE(IPUNIT,99020) IDSURF, IDUAIR, SFNAME, UANAME,&
      &ISYEAR, IUYEAR
      WRITE(IPUNIT,99025)
   END IF

!     Write surface file record, depending on whether deposition is included
   WRITE(ISUNIT,98026) IYEAR, IMONTH, IDAY, IHOUR,&
   &SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,&
   &SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT,&
   &IPCODE, PRATE, RH, SFCP, NCLOUD

   DO I = 1,NPLVLS
      IF (PFLTA(I) .EQ. -999.0D0) THEN
         PFLTEMP = PFLTA(I)
      ELSE
         PFLTEMP = PFLTA(I)-DCTODK
      END IF
      WRITE (IPUNIT, 99026) KYEAR, KMONTH, KDAY, KHOUR, PFLHT(I),&
      &IFLAG(I), PFLWD(I), PFLWS(I), PFLTEMP,&
      &PFLSW(I), PFLSV(I)
   END DO

9011 FORMAT(/1X,'*** SUMMARY OF THE SAMPLED SURFACE ',&
   &'METEOROLOGICAL DATA USED WITH THE SCIM OPTION ***'/)
9016 FORMAT(1X,'Surface file:   ',A80,&
   &/,1X,'Surface format: ',A105)
9020 FORMAT(1X,'SURFACE STATION NO.: ',I6,20X,&
   &'UPPER AIR STATION NO.: ',I6/16X,'NAME: ',A40,3X,&
   &'NAME: ',A40/16X,'YEAR: ',I6,37X,'YEAR: ',I6/)
! Unused: 9025 FORMAT (' YR',' MO', ' DY', ' JDY', ' HR', '     H0',
!     &          '     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',
!     &          '  M-O LEN', '    Z0', '  BOWEN', ' ALBEDO',
!     &          '  REF WS', '   WD', '     HT', '  REF TA', '     HT',
!     &         /61(' -'))
98025 FORMAT (' YR',' MO', ' DY', ' HR', '     H0',&
   &'     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',&
   &'  M-O LEN', '   Z0 ', 'BOWEN', '  ALB',&
   &'  REF WS', '   WD', '   HT', '  REF TA', '  HT',&
   &' IPCOD',' PRATE','  RH',' SFCP',' CCVR'&
   &/66(' -'))
! Unused: 9026 FORMAT (1X, 3(I2.2,1X),I3,1X,I2.2,1X,F6.1,1X,3(F6.3,1X),
!     &        2(F5.0,1X),F8.1,1X,F5.3,1X,2(F6.2,1X),F7.2,1X,F5.0,
!     &        3(1X,F6.1) )
98026 FORMAT ( 1X,3(I2.2,1X),I2.2,1X,F6.1,1X,3(F6.3,1X),&
   &2(F5.0,1X),F8.1,1X,F5.3,2F5.2,1X,F7.2,1X,F5.0,&
   &1X,F4.0,1X,F6.1,1X,F4.0,I3,F7.2,F6.0,F6.0,I3,F7.3)

99011 FORMAT(/1X,'*** SUMMARY OF THE SAMPLED PROFILE ',&
   &'METEOROLOGICAL DATA USED WITH THE SCIM OPTION ***'/)
99016 FORMAT(1X,'Profile file:   ',A80,&
   &/,1X,'Profile format: ',A105)
99020 FORMAT(1X,'SURFACE STATION NO.: ',I6,20X,&
   &'UPPER AIR STATION NO.: ',I6/16X,'NAME: ',A40,3X,&
   &'NAME: ',A40/16X,'YEAR: ',I6,37X,'YEAR: ',I6/)
99025 FORMAT ( ' YR', ' MO', ' DY', ' HR', ' HEIGHT', ' F', '  WDIR',&
   &'    WSPD',' AMB_TMP', ' sigmaA', '  sigmaW' ,&
   &/29(' -'))
99026 FORMAT (1X, 4(I2.2,1X),F6.1,1X,I1,1X,F5.0,1X,F7.2,1X,F7.1,1X,&
   &F6.1,1X,F7.2)


   RETURN
END


SUBROUTINE PFLCNV( LEVEL )
!***********************************************************************
!             PFLCNV Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Converts data in profile file to the required units
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Hourly profile data
!
!        OUTPUTS: Hourly profile data converted to required units
!
!        Revisions:
!                    R. Brode, PES, Inc.                  25 Feb 2002
!                    Modify upper limit on temperature from 900 to 90
!                    so that a value of 99.0 will be treated as
!                    missing
!
!                    R. Brode, PES, Inc.                  22 Jan 1998
!                    Check for wind direction > 900. and recode
!                    as missing (-999.0).
!
!                    J. Paumier, PES, Inc                 16 Dec 1994
!                    Fixed the logic in determining if ambient
!                    temperature is missing in the conversion from
!                    Celsius to kelvin
!
!        ASSUMPTIONS:
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable Declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   LOGICAL          :: L_Turb_Warn
   INTEGER          :: LEVEL, N_Got_SigA, N_Got_SigW, N_Turb_Warn
   DOUBLE PRECISION :: SIGRAD, EPSIL, USUBV

!
!---- Variable Initializations
!
   DATA N_Got_SigA/0/, N_Got_SigW/0/, N_Turb_Warn/0/
   DATA L_Turb_Warn/.FALSE./
   MODNAM = 'PFLCNV'
!-----------------------------------------------------------------------

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     RESET PFLSA AND/OR PFLSW BASED ON TURBULENCE OPTIONS
   WRITE(DUMMY,'(I10.10)') FULLDATE

   IF (PFLSA(LEVEL) .LT. 99.0D0 .AND. (TURBOPTS(1) .OR.&
   &(TURBOPTS(2) .AND. STABLE) .OR. (TURBOPTS(3) .AND. UNSTAB)&
   &.OR. TURBOPTS(4) .OR. (TURBOPTS(6) .AND. STABLE) .OR.&
   &(TURBOPTS(8) .AND. UNSTAB))) THEN
      PFLSA(LEVEL)=99.0D0
      IF (.NOT. RESET_SA) RESET_SA=.TRUE.

   ENDIF
   IF (PFLSW(LEVEL) .LT. 99.0D0 .AND. (TURBOPTS(1) .OR.&
   &(TURBOPTS(2) .AND. STABLE) .OR. (TURBOPTS(3) .AND. UNSTAB)&
   &.OR. TURBOPTS(5) .OR. (TURBOPTS(7) .AND. STABLE) .OR.&
   &(TURBOPTS(9) .AND. UNSTAB))) THEN
      PFLSW(LEVEL)=99.0D0
      IF (.NOT. RESET_SW) RESET_SW=.TRUE.

   ENDIF

!     Change the missing value indicator for wind speed to -99.0

   IF( PFLWS(LEVEL)  .LT. 0.0D0  .OR.&
   &PFLWS(LEVEL)  .GT. 90.0D0 )THEN
      PFLWS(LEVEL) =  -99.0D0
   ENDIF

!     Change the wind direction from 0.0 to 360.0 if wind speed is nonzero

   IF(( PFLWS(LEVEL) .GT. 0.0D0 .AND. PFLWS(LEVEL) .LE. 90.0D0 ).AND.&
   &PFLWD(LEVEL) .EQ. 0.0D0 )THEN
      PFLWD(LEVEL) = 360.0D0
   ENDIF

   IF( PFLWD(LEVEL) .GT. 900.0D0 .OR. PFLWD(LEVEL) .LT. 0.0D0 )THEN
      PFLWD(LEVEL) = -999.0D0
   ENDIF

   IF( PFLWS(LEVEL) .EQ. 0.0D0  .AND.&
   &PFLWD(LEVEL) .EQ. 0.0D0 )THEN
      PFLWS(LEVEL) = -99.0D0
      PFLWD(LEVEL) = -999.0D0
   ENDIF

!     Compute sigmaV from nonmissing wind speed and sigmaTHETA
   IF( PFLWS(LEVEL) .GT. 0.0D0  .AND.&
   &PFLSA(LEVEL) .GE. 0.0D0  .AND.&
   &PFLSA(LEVEL) .LT. 99.0D0 )THEN
      SIGRAD = PFLSA(LEVEL) * DTORAD
      EPSIL = DSIN(SIGRAD) * ( 1.0D0 - GSIGV * SIGRAD )           ! GSIGV = 0.073864D0 (in MODULES.f)
      USUBV = PFLWS(LEVEL) * DSQRT( 1.0D0 - EPSIL*EPSIL )
      PFLSV(LEVEL) = SIGRAD * USUBV
! ---       Compare to minimum value PARAMETER, SVMIN
      PFLSV(LEVEL) = MAX( SVMIN, PFLSV(LEVEL) )
      L_Got_SigA = .TRUE.
      N_Got_SigA = N_Got_SigA + 1
   ELSE
      PFLSV(LEVEL) = -99.0D0
   ENDIF

! --- Check for sigmaW data
   IF( PFLSW(LEVEL) .GT. 0.0D0 .AND.&
   &PFLSW(LEVEL) .LT. 90.0D0 )THEN
      L_Got_SigW = .TRUE.
      N_Got_SigW = N_Got_SigW + 1
   ENDIF

! --- Check for turbulence data with ADJ_U
   IF( L_AdjUstar .AND. DFAULT )THEN
! ---    Issue FATAL error if observed turbulence data are
!        used with L_AdjUstar with RegDFAULT option
      IF( L_Got_SigA .AND. L_Got_SigW )THEN
         IF( N_Got_SigA .EQ. 1 .AND. N_Got_SigW .EQ. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','401','SigA & SigW')
            FATAL = .TRUE.
         ENDIF
      ELSEIF( L_Got_SigA .AND. .NOT.L_Got_SigW )THEN
         IF( N_Got_SigA .EQ. 1 ) THEN
            CALL ERRHDL(PATH,MODNAM,'E','401','SigA Data')
            FATAL = .TRUE.
         ENDIF
      ELSEIF( .NOT.L_Got_SigA .AND. L_Got_SigW )THEN
         IF( N_Got_SigW .EQ. 1 ) THEN
            CALL ERRHDL(PATH,MODNAM,'E','401','SigW Data')
            FATAL = .TRUE.
         ENDIF
      ENDIF
   ELSEIF( L_AdjUstar )THEN
! ---    Issue Warning error if observed turbulence data are used
!        with L_AdjUstar and Non-DFAULT; limit to one warning
      IF( L_Got_SigA .AND. L_Got_SigW )THEN
         IF( N_Got_SigA .EQ. 1 .AND. N_Got_SigW .EQ. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'W','401','SigA & SigW')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            MODOPS(1)  = 'NonDFAULT'
            MODOPS(26) = 'SigA&SigW'
         ENDIF
      ELSEIF( L_Got_SigA .AND. .NOT.L_Got_SigW )THEN
         IF( N_Got_SigA .EQ. 1 ) THEN
            CALL ERRHDL(PATH,MODNAM,'W','401','SigA Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            MODOPS(1)  = 'NonDFAULT'
            MODOPS(26) = 'SigA Data'
         ENDIF
      ELSEIF( .NOT.L_Got_SigA .AND. L_Got_SigW )THEN
         IF( N_Got_SigW .EQ. 1 ) THEN
            CALL ERRHDL(PATH,MODNAM,'W','401','SigW Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            MODOPS(1)  = 'NonDFAULT'
            MODOPS(26) = 'SigW Data'
         ENDIF
      ENDIF
      IF( .NOT. L_Turb_Warn .and. (L_Got_Siga .or. L_Got_SigW) )THEN
         L_Turb_Warn = .TRUE.
         N_Turb_Warn = N_Turb_Warn + 1
         IF( N_Turb_Warn .EQ. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'W','402','Option')
         ENDIF
      ENDIF
   ELSE
! ---    ADJ_U* is NOT being used, however include use of turbulence,
! ---    SigA and/or SigW in the MODOPS array even with without ADJ_U*
      IF( L_Got_SigA .AND. L_Got_SigW )THEN
         IF( N_Got_SigA .EQ. 1 .AND. N_Got_SigW .EQ. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'W','403','SigA & SigW')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            MODOPS(26) = 'SigA&SigW'
         ENDIF
      ELSEIF( L_Got_SigA .AND. .NOT.L_Got_SigW )THEN
         IF( N_Got_SigA .EQ. 1 ) THEN
            CALL ERRHDL(PATH,MODNAM,'W','403','SigA Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            MODOPS(26) = 'SigA Data'
         ENDIF
      ELSEIF( .NOT.L_Got_SigA .AND. L_Got_SigW )THEN
         IF( N_Got_SigW .EQ. 1 ) THEN
            CALL ERRHDL(PATH,MODNAM,'W','403','SigW Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            MODOPS(26) = 'SigW Data'
         ENDIF
      ENDIF

!JAT ISSUE D030: COMMENT OUT DUPLICATE CODE BELOW
!         IF SIGMA-A IS INCLUDED, THE CALCULATION
!         HAS ALREADY BEEN DONE OUTSIDE THIS IF/ELSE
! ---    Make calculation                                            ! 20190709- Poss. duplicate code - Review block below
!         IF( PFLSA(LEVEL) .GT. 0.0D0  .AND.
!     &       PFLSA(LEVEL) .LT. 99.0D0 )THEN   !! May need to reconsider upper bound for SigA
!            SIGRAD = PFLSA(LEVEL) * DTORAD
!            EPSIL = DSIN(SIGRAD) * ( 1.0D0 - GSIGV * SIGRAD )        ! GSIGV = 0.073864D0 (in MODULES.f)
!            USUBV = PFLWS(LEVEL) * DSQRT( 1.0D0 - EPSIL*EPSIL )
!            PFLSV(LEVEL) = SIGRAD * USUBV
!C ---       Compare to minimum value PARAMETER
!            PFLSV(LEVEL) = MAX( SVMIN, PFLSV(LEVEL) )
!         ENDIF                                                       ! 20190709- End review
!JAT  ISSUE D030: END COMMENT OUT
   ENDIF

!     Convert temperature from degrees Celsius to kelvins

   IF( PFLTA(LEVEL)  .GT. -90.0D0  .AND.&
   &PFLTA(LEVEL)  .LT.  90.0D0 )THEN
      PFLTA(LEVEL) = PFLTA(LEVEL) + DCTODK

   ELSE
      PFLTA(LEVEL) = -999.0D0

   ENDIF

!     Change the missing value indicator for sigmaW to -99.0

   IF( PFLSW(LEVEL)  .LT. 0.0D0  .OR.&
   &PFLSW(LEVEL)  .GT. 90.0D0 )THEN
      PFLSW(LEVEL) =  -99.0D0
   ELSE
!        Compare to minimum value PARAMETER, SWMIN = 0.02
      PFLSW(LEVEL) = MAX( SWMIN, PFLSW(LEVEL) )
   ENDIF

   RETURN
END

SUBROUTINE PFLINI ()
!***********************************************************************
!             PFLINI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Initializes the observed profile arrays to missing
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  None
!
!        OUTPUTS: Initialized observed profile arrays
!
!        ASSUMPTIONS:
!
!        CALLED FROM:  METEXT
!***********************************************************************
!
!---- Variable Declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
! Unused:      INTEGER :: I

!
!---- Variable Initializations
!
   MODNAM = 'PFLINI'
   PATH   = 'MX'

!.......................................................................
!     Initialize arrays (1:MXGLVL)
   IFLAG(:)  = 0
   PFLHT(:)  = -99.0D0
   PFLWS(:)  = -99.0D0
   PFLWD(:)  = -99.0D0
   PFLTA(:)  = -99.0D0
   PFLSA(:)  = -99.0D0
   PFLSW(:)  = -99.0D0
   PFLSV(:)  = -99.0D0
   PFLTG(:)  = -99.0D0
   PFLTGZ(:) = -99.0D0

   RETURN
END

SUBROUTINE ZIAVER ( NLVLS,HTS,PARRAY,ZI,NDXBLW,PBLAVG,VALZI )
!***********************************************************************
!             ZIAVER Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To compute the average value of the parameter between
!                the surface and the mixing height
!
!   Input:       Number of levels in the profile (NLVLS)
!                Array of gridded profile heights (HTS)
!                Parameter array (PARRAY)
!                Boundary layer height (ZI) (or stack height, if higher)
!                Index of the level gridded profile height immediately
!                   below ZI (NDXBLW)
!                Value of parameter at ZI (VALZI)
!
!   Output:      Average value of parameter in boundary layer (PBLAVG);
!
!   Called by:   METEXT
!
!   Assumptions: If the mixing height (ZI) is above the highest
!                profile height (5000 m), then we assume the profile
!                is constant (= PARRAY(NLVLS)) above ZI and compute
!                the average accordingly.
!
!
!   Programmer:  Jim Paumier PES, Inc.
!
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   Reference(s): Inhomgeneous Boundary Layer, A. Venkatram,
!                 June 25, 1993 (GOLF document #5)
!
!***********************************************************************
!
!---- Variable declarations
!
   IMPLICIT NONE

   INTEGER   NDXBLW, NLVLS, I
   DOUBLE PRECISION  HTS(NLVLS), PARRAY(NLVLS), ZI, SUM, PBLAVG,&
   &VALZI
!
!---- Data dictionary
!
!---- Data initializations
!
!.......................................................................

   SUM = 0.0D0

!---- Sum over each layer of the gridded profile (PARRAY) to the level
!     immediately below ZI

   DO I = 2, NDXBLW
      SUM = SUM + (HTS(I) - HTS(I-1)) * 0.5D0 *&
      &(PARRAY(I) + PARRAY(I-1))
   END DO

!---- Finish the summation

   IF( NDXBLW .LT. NLVLS )THEN
!------- Add the area between the level below ZI and ZI to the
!        sum and compute the average.

      SUM = SUM + (ZI - HTS(NDXBLW) ) * 0.5D0 *&
      &(VALZI + PARRAY(NDXBLW) )
      PBLAVG = SUM / ZI

   ELSE
!----    ZI is above the top level (5000 m), assume the parameter is
!        constant above that level and sum accordingly and compute
!        the average
      SUM = SUM + (ZI - HTS(NLVLS)) * PARRAY(NLVLS)
      PBLAVG = SUM / ZI
   ENDIF

   RETURN
END

SUBROUTINE GINTRP ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
!***********************************************************************
!             GINTRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     A generalized interpolation routine
!
!   Input:       Height below the required height (HTBELO)
!                Value below the required height (VBELOW)
!                Height above the required height (HTBELO)
!                Value above the required height (VBELOW)
!                Height at which a value is required (REQDHT)
!
!   Output:      Value of the parameter at the required level (VALUE)
!
!   Called by:   Utility routine called by many modules
!
!   Assumptions:
!
!   Programmer:  Jim Paumier, PES, Inc.
!
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   Reference(s):
!
!***********************************************************************
!
!---- Variable declarations
!
   IMPLICIT NONE
   DOUBLE PRECISION :: VALUE, HTBELO, VBELOW, HTABOV, VABOVE, REQDHT
!
!---- Data dictionary
!
!---- Data initializations
!
!.......................................................................
!
!---- Interpolate

   VALUE = VBELOW + ( (REQDHT - HTBELO) / (HTABOV - HTBELO) ) *&
   &(VABOVE - VBELOW)

   RETURN
END

SUBROUTINE URBCALC
!***********************************************************************
!             URBCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Parameters for Urban Stable Boundary Layer
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    June 11, 1996
!
!
!        MODIFIED:  Calculate an urban ustar by setting equivalence
!                   between convective sigma-w based on urban wtar
!                   and mechanical sigma-w based on urban ustar at
!                   a height of 7 times the urban roughness length.
!                   R.W. Brode, PES, Inc., - 06/10/02
!
!        MODIFIED:  To set the value for Z_iuo at 400m instead of
!                   500m in calculation of ZIURB, based on observed data.
!                   R.W. Brode, PES, Inc., - 04/08/02
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Urban Mixing Height, Heat Flux, and "Convective
!                 Velocity Scale"
!
!        ASSUMPTIONS:  <none>
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
!     JAT D065 8/9/21, RHO CALCULATED BUT NOT USED
!      DOUBLE PRECISION :: DELTUR, URBHF, RHO, HT7Z0
   DOUBLE PRECISION :: DELTUR, URBHF, HT7Z0, RHO
   DOUBLE PRECISION :: URBOBLSAV, URBUSTRSAV
!---- Assign the reference mixing height, REFZI, based on a reference
!     population of 2 million.
!     CP     = specific heat capacity of dry air
   DOUBLE PRECISION, PARAMETER :: REFZI = 400.0D0, CP = 1004


!---- Variable initializations
   MODNAM = 'URBCALC'

!     Save Rural values of USTAR and OBULEN
   RURUSTR   = USTAR
   RUROBULEN = OBULEN

!     Loop Through Urban Areas
   DO IURB = 1, NUMURB

!        Compute Urban-Rural Temperature Difference, DELTUR;
!RWB     DELTUR = DELTRUR * (0.1046 * LOG(URBPOP/REFPOP) + 0.9983)
!RWB     using DELTRUR = 12.0 K for the reference population
!RWB     (REFPOP) of 2.0D+6 (assigned in MODULE MAIN1).
!RWB     Use rounded values for parameters
      DELTUR = DELTRUR * (0.1D0 * DLOG(URBPOP(IURB)/REFPOP) + 1.0D0)

      IF (STABLE) THEN
! ---       Compute Urban Convective Mixing Height
         ZIURB(IURB) = REFZI * (URBPOP(IURB)/REFPOP) ** 0.25D0
         L_MorningTrans(IURB) = .FALSE.
      ELSE IF (L_UrbanTransition) THEN
! ---       Compute Urban pseudo-Convective Mixing Height for morning transition
         ZIURB(IURB) = REFZI * (URBPOP(IURB)/REFPOP) ** 0.25D0
! ---       Check for ZICONV > ZIURB; if so then disable morning transition
         IF (ZICONV .GT. ZIURB(IURB)) THEN
            L_MorningTrans(IURB) = .FALSE.
            CYCLE
         ELSE
            L_MorningTrans(IURB) = .TRUE.
         END IF
! JAT D136_URBAN_TRANS; ADD ELSE STATEMENT TO CYCLE IF THE NOURBANTRAN INVOKED AND
! HOUR IS CONVECTIVE
! THIS WILL KEEP THE URBAN DEBUG FILE FROM HAVING NaNs WHEN FIRST HOUR OF MET FILE IS
! CONVECTIVE
      ELSE
         L_MorningTrans(IURB) = .FALSE.
         CYCLE
      END IF

!     JAT D065 8/9/21, RHO CALCULATED BUT NOT USED
      RHO    = 101325.D0/(287.04D0*TA)

!        Compute Urban Heat Flux, and recalculate Monin-Obukhov length
      URBHF  = 0.03D0 * DELTUR * USTAR * RHO * CP


!        Compute Urban WSTAR
      URBWSTR(IURB) = ((G/TA/RHO/CP) * URBHF * ZIURB(IURB)) ** THIRD


!        Compute Urban USTAR; first set height for equivalence between
!        convective and mechanical sigma-w as 7 times the maximum of the
!        rural and urban surface roughness length.
      HT7Z0 = 7.0D0* MAX( URBZ0(IURB), SFCZ0 )
      URBUSTRSAV = URBWSTR(IURB) *&
      &DSQRT(1.6D0*(HT7Z0/ZIURB(IURB))**(2.0D0*THIRD))/&
      &(1.3D0*DSQRT(1.0D0-HT7Z0/MAX(ZIURB(IURB),ZIMECH)))
      URBUSTR(IURB) = MAX( USTAR, URBUSTRSAV)

!        Compute equivalent Monin-Obukhov length
      URBOBULEN(IURB) = -((RHO*CP*TA*URBUSTR(IURB)**3)/&
      &(0.4D0*G*URBHF))
      URBOBLSAV = URBOBULEN(IURB)

!RCO D095, compare compute MOL to original MOL, pick the more
!    neutral for nighttime
!CRCO D120 Check for urban transition, use most convective value
      IF (L_MorningTrans(IURB)) THEN
!During morning transition hours, RUROBULEN is already negative, so most
!convective value will be maximum of the two negative values
         URBOBULEN(IURB) = max(URBOBULEN(IURB),RUROBULEN)
      ELSE
!During stable conditions, RUROBULEN is positive, while URBOBULEN. Take the
!largest of the absolute value of the two to get the most neutral value.
!Using the positive values to replace downstream calculations in various places
!that were making the convective URBOBULEN stable/neutral.
         URBOBULEN(IURB) = max(abs(URBOBULEN(IURB)),RUROBULEN)
      END IF

!RCO D095 Added for urban debug 8/3/2021
      IF (URBDBUG) THEN
         WRITE(URBUNT,3333) IURB,IYEAR,IMONTH,IDAY,IHOUR,&
         &URBOBULEN(IURB),URBOBLSAV,RUROBULEN,&
         &URBUSTR(IURB),URBUSTRSAV,&
         &USTAR,DELTUR,ZIURB(IURB),ZIMECH,ZICONV,&
         &URBPOP(IURB),URBHF,URBWSTR(IURB),WSTAR,&
         &TA,UREF,BOWEN,&
         &STABLE,L_MorningTrans(IURB)
3333     FORMAT(1X,5(2X,I2),17(F12.2),(7X,L2),(9X,L2))
      ENDIF
! End URBDBUG insert
   END DO

   RETURN
END

SUBROUTINE GRDURBAN
!***********************************************************************
!             GRDURBAN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Computes Urban Profiles
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    June 11, 1996
!
!        MODIFIED:   Changed subroutine name from GRDURB to GRDURBAN.
!                    R. Brode, PES, 11/21/97
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Urban Profiles of sigma-v, sigma-w, Tlz, VPTG
!
!        ASSUMPTIONS:  <none>
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: I
   DOUBLE PRECISION :: ZDCRS, SV2, SVURB, SV2DCR, VAL2, ATZI, SW2,&
   &SWURB

!---- Variable initializations
   MODNAM = 'GRDURBAN'

!     Save Rural Profiles

!     Save gridded profile arrays to 'rural' arrays (1:MXGLVL)
   GRDSVR(:) = GRIDSV(:)
   GRDSWR(:) = GRIDSW(:)
   GRDTGR(:) = GRIDTG(:)
   GRDPTR(:) = GRIDPT(:)

!     Loop Through Urban Areas
   DO IURB = 1, NUMURB

      IF (.NOT.STABLE .AND. .NOT.L_MorningTrans(IURB)) CYCLE

      ZDCRS  =  AT1PT2 * ZIURB(IURB)

!        Loop Through Grid Levels
      DO I = 1, MXGLVL

         SV2 = 0.35D0 * URBWSTR(IURB)**2
!
         IF( GRIDHT(I)  .LE.  ZIURB(IURB) )THEN
            SVURB = DSQRT( SV2 )

         ELSEIF( GRIDHT(I) .GT. ZIURB(IURB) .AND.&
         &GRIDHT(I) .LE. ZDCRS )THEN
!              COMPUTE sigmaV at 1.2*ZI
            SV2DCR = MIN( SV2, 0.25D0 )
!              INTERPOLATE between value of SV2 at ZI and at 1.2*ZI
            CALL GINTRP ( ZIURB(IURB), SV2, ZDCRS, SV2DCR, GRIDHT(I),&
            &VAL2 )
            SVURB = DSQRT( VAL2 )

         ELSE   ! requested height is above 1.2*urban mixing height
            ATZI  = DSQRT( SV2 )
            SVURB = MIN( ATZI, 0.5D0 )

         ENDIF
!

         IF( GRIDHT(I)  .LE.  0.1D0*ZIURB(IURB) )THEN
            SW2 = 1.6D0 * ( GRIDHT(I)/ZIURB(IURB) )**(2.0D0*THIRD) *&
            &URBWSTR(IURB)**2
            SWURB  = DSQRT( SW2 )

         ELSEIF( GRIDHT(I).GT.0.1D0*ZIURB(IURB) .AND.&
         &GRIDHT(I).LE.ZIURB(IURB) )THEN
            SWURB = DSQRT( 0.35D0 * URBWSTR(IURB)**2 )

         ELSE   ! requested height is above urban mixing height
            SW2 = 0.35D0 * URBWSTR(IURB)**2 *&
            &DEXP(-(6.D0*(GRIDHT(I)-ZIURB(IURB))/ZIURB(IURB)))
            SWURB = DSQRT( SW2 )

         ENDIF
!
         GRDSVU(I,IURB) = DSQRT(GRIDSV(I)**2 + SVURB**2)
         GRDSWU(I,IURB) = DSQRT(GRIDSW(I)**2 + SWURB**2)


!RCO D095 Added for urban debug 8/3/2021
         IF (URBDBUG) THEN
            IF (I .LT. 17) THEN
               WRITE(URBUNT1,3333) IURB,IYEAR,IMONTH,IDAY,IHOUR,I,&
               &GRIDHT(I),&
               &GRIDSV(I),SVURB,GRDSVU(I,IURB),&
               &GRIDSW(I),SWURB,GRDSWU(I,IURB)
3333           FORMAT(1X,6(2X,I2),9(F10.2))
            ENDIF
         ENDIF
! End URBDBUG insert

         IF (GRIDHT(I) .LE. ZIURB(IURB)) THEN
            GRDTGU(I,IURB) = 1.0D-5
         ELSE
            GRDTGU(I,IURB) = GRIDTG(I)
         END IF

      END DO

   END DO

!---- Compute potential temperature profile from urban Dtheta/Dz profile
   CALL GRDPTURB

   RETURN
END

SUBROUTINE GRDPTURB
!=======================================================================
!                GRDPTURB module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of
!                potential temperature for URBAN cases
!
!   Input:       Profile of gridded potential temperature gradients
!                Temperature at the reference height
!                Profile of grid heights
!
!   Output:      Potential temperature profile at the grid heights.
!
!   Assumptions: There is at least one grid level below the reference
!                temperature height (which should be satisfied
!                because the lowest grid level is 0.5 meters)
!
!   Called by:   METEXT
!
!   Programmer:  Jim Paumier                          30 Sept 1993
!                Pacific Environmental Services
!
!   Revision history:
!        12/10/97  - R. Brode, PES, Inc.
!                    Corrected the order of array indices used for profiling
!                    potential temperature above the reference height.
!        12/16/94  - J. Paumier, PES, Inc.
!                  - CALL LOCATE to get the number of levels below the
!                    temperature reference height, replacing the original
!                    method which relied on grid heights being every 10 m
!
!-----------------------------------------------------------------------
!
!---- Variable declarations

   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: L, NBELOW, I
   DOUBLE PRECISION :: PTREF

!---- Data definitions
!
!
!---- Data initializations
   MODNAM = 'GRDPTURB'
!
!
!.......................................................................
!

!---- Determine the grid level below the temperature reference
!     height (as defined in the scalar file)               ---- CALL LOCATE

   CALL LOCATE( GRIDHT, 1, MXGLVL, TREFHT, NBELOW )

!---- Compute the potential temperature at the reference level
!     using the reference temperature (TA), the reference
!     temperature height (TREFHT), and the base elevation
!     of the measurement site specified on the ME PROFBASE
!     keyword (ZBASE)

   PTREF = TA + GOVRCP * (TREFHT + ZBASE)

!---- Loop Through Urban Areas
   DO IURB = 1, NUMURB

      IF (.NOT.STABLE .AND. .NOT.L_MorningTrans(IURB)) CYCLE

!----    Compute the potential temperature at the grid level below
!        the temperature reference height

      GRDPTU(NBELOW,IURB) = PTREF -&
      &0.5D0 * (GRDTGU(NBELOW+1,IURB) + GRDTGU(NBELOW,IURB)) *&
      &(TREFHT - GRIDHT(NBELOW))


!----    Compute Potential Temp Values for Grid Levels Below Reference Ht.
      DO L = NBELOW-1, 1, -1

         GRDPTU(L,IURB) = GRDPTU(L+1,IURB) - 0.5D0 *&
         &(GRDTGU(L+1,IURB) + GRDTGU(L,IURB)) *&
         &(GRIDHT(L+1) - GRIDHT(L))

      END DO


!----    Compute Potential Temp Values for Grid Levels Above Reference Ht.
      DO L = NBELOW+1, MXGLVL

         GRDPTU(L,IURB) = GRDPTU(L-1,IURB) + 0.5D0 *&
         &(GRDTGU(L,IURB) + GRDTGU(L-1,IURB)) *&
         &(GRIDHT(L) - GRIDHT(L-1))

      END DO

!RCO D168 Debug files. Added for temp profile for urban debug 3/17/2023
      IF (URBDBUG) THEN
         DO I = 1, MXGLVL
            WRITE(URBUNT2,3333) IURB,IYEAR,IMONTH,IDAY,IHOUR,I,NBELOW,&
            &GRIDHT(I),GRIDTG(I),GRDTGU(I,IURB),&
            &GRIDPT(I),GRDPTU(I,IURB)
3333        FORMAT(1X,6(2X,I2),1(5X,I2),1(F10.2),2(E10.3),2(F10.4))
         END DO
      ENDIF
! End URBDBUG insert


   END DO

   RETURN
END

!CRFL
!CRFL  Subroutine METDEB added to improve debug output of meteorological
!CRFL  data.
!CRFL

SUBROUTINE METDEB
!***********************************************************************
!             METDEB Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Writes debug output of this hour's meteorological
!                 data.
!
!        PROGRAMMERS:  Bob Paine (developer) and Russ Lee (implementation)
!
!        DATE:    August 18, 1994;  Revised September 26, 1994.
!
!        MODIFIED:   To include value for Theta* (THSTAR) in the METEOR
!                    debug option.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 06/30/2015
!
!        INPUTS:  Meteorological data input to model
!
!        OUTPUTS: Meteorological data output to debug file
!
!        ASSUMPTIONS:  None
!
!        CALLED FROM:  HRLOOP
!***********************************************************************

!---- Variable declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: I, NDIST

!---- Variable initializations
   MODNAM = 'METDEB'

   IF (METHDR) THEN
!----    Modified to use 4-digit year (IYR) instead of 2-digit year (IYEAR)
      WRITE (DBMUNT, 6115) IYR, IMONTH, IDAY, IHOUR, ZI, TA, USTAR,&
      &WSTAR, OBULEN, SFCZ0, THSTAR, UAVG, SVAVG, SWAVG, UATZI,&
      &SVATZI, SWATZI, VPTGZI
      WRITE (DBMUNT, 6118)
      DO I = MXGLVL, 1, -1
         WRITE (DBMUNT, 6120) I, GRIDHT(I), GRIDWD(I), GRIDWS(I),&
         &GRIDSV(I), GRIDSW(I), GRIDPT(I), GRIDTG(I)
      END DO
      WRITE (DBMUNT, 6116)
      METHDR = .FALSE.
   ENDIF

! --- Adjust for distances larger than output field
   IF( DABS(X) .GT. 999999.0D0 )THEN
      IF( X .LT. 0.0D0 )THEN
         NDIST = -999999
      ELSE
         NDIST =  999999
      ENDIF
   ELSE
      NDIST = IDNINT(X)
   ENDIF

   IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
      WRITE (DBMUNT, 6131) IREC, NDIST, UEFF, SVEFF, SWEFF
   ELSE IF(PPF .GE. 1.0D0) THEN
      WRITE (DBMUNT, 6132) IREC, NDIST, UEFF3, SVEFF3, SWEFF3
   ELSE IF(PPF .LE. 0.0D0) THEN
      WRITE (DBMUNT, 6133) IREC, NDIST, UEFFD, SVEFFD, SWEFFD,&
      &UEFFN, SVEFFN, SWEFFN
   ELSE
      WRITE (DBMUNT, 6134) IREC, NDIST, UEFFD, SVEFFD, SWEFFD,&
      &UEFFN, SVEFFN, SWEFFN, UEFF3, SVEFF3, SWEFF3
   ENDIF
!
6115 FORMAT( 1X, 80('-'),//,'  SURFACE AND PROFILE MET DATA:',/,&
   &T48, 'MONIN-     SFC',/,T17,&
   &'MIXING   TEMP                  OBUKHOV   ROUGH.',/,T17,&
   &'HEIGHT   @ HS    U*      W*    LENGTH    LENGTH    THSTAR',/,&
   &'  YR  MO DA HR',&
   &'    (M)    (K)   (M/S)   (M/S)    (M)       (M)',//,&
   &1X,I4,3I3,2X,F6.1,2X,F5.1,1X,F6.3,1X,F7.3,2X,F7.1,3X,F7.3,3X,&
   &F7.4///,&
   &' <--AVG: SFC TO ZI---> <--------VALUE AT ZI-------->',/,&
   &'   U    SIG-V  SIG-W      U    SIG-V  SIG-W   VPTG',/,&
   &' (M/S)  (M/S)  (M/S)    (M/S)  (M/S)  (M/S)   (K/M)',//,&
   &1X,F5.2,2(2X,F5.2),3X,F5.2,2X,F5.2,2X,F5.2,&
   &1X,F7.4,//)
6116 FORMAT(//,1X, '            <-STABLE/DIRECT EFF. VALUES-> ',&
   &'<-INDIRECT EFF. VALUES-> <-PENETRATED EFF. VALUES->',&
   &/,' RECEPT  DIST.    U    SIG-V  SIG-W   ',&
   &'       U    SIG-V  SIG-W          U    SIG-V  SIG-W',/)
!RJP 6117 FORMAT (1X,I5,1X,F6.0,3(2X,F5.2),1X,F7.0,3(2X,F5.2),2(1X,F7.0))
6118 FORMAT(5X,' GRID     WIND    WIND                    POT.',&
   &/,5X,'HEIGHT    DIR.    SPEED   SIG-V   SIG-W   TEMP.',&
   &'  VPTG',/,&
   &5X,' (M)     (DEG)    (M/S)   (M/S)   (M/S)    (K)  ',&
   &' (K/M)',/)
6120 FORMAT (I4,F7.1,2X,F6.1,2X,F7.2,1X,F7.2,1X,F7.2,1X,F6.2,&
   &1X,F9.6)

!RJP  Add new FORMAT statements here.

6131 FORMAT (I5,1X,I7,1X,3(2X,F5.2))
6132 FORMAT (I5,1X,I7,1X,54X,3(2X,F5.2))
6133 FORMAT (I5,1X,I7,1X,2(3(2X,F5.2),6X))
6134 FORMAT (I5,1X,I7,1X,3(3(2X,F5.2),6X))

   RETURN
END

SUBROUTINE GRDEPS
!***********************************************************************
!             GRDEPS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Computes profile of epsilon, turbulence dissipation
!                 rate, for PVMRM or GRSM option
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:       May 13, 2002
!
!        INPUTS:
!
!        OUTPUTS:
!
!        ASSUMPTIONS:
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: I
   DOUBLE PRECISION :: TSUBLR
   DOUBLE PRECISION, PARAMETER :: AR1 = 0.46D0

!---- Variable initializations
   MODNAM = 'GRDEPS'

!     Loop Through Grid Levels
   DO I = 1, MXGLVL

      TSUBLR = AR1 * ZI/GRIDSW(I)
      GRIDEPS(I) = 0.78D0 * GRIDSW(I)*GRIDSW(I)/TSUBLR

   END DO

   RETURN
END
