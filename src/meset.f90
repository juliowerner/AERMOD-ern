SUBROUTINE MECARD
!***********************************************************************
!                 MECARD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To process MEteorology Pathway Card Images
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:       March 2, 1992
!
!        MODIFIED:   To allow the user to specify the number of years of
!                    meteorological data that are being processed for a
!                    particular run.  The option is exercised with the
!                    new NUMYEARS keyword on the ME pathway. The value
!                    specified on the NUMYEARS keyword is used to allocate
!                    storage for the arrays that are used in the MAXDCONT
!                    option, and allows the user to reduce the memory storage
!                    requirements under the MAXDCONT option when less than
!                    five (5) years of met data are being used. The default
!                    number of years used for the MAXDCONT array allocation
!                    without the NUMYEARS keyword is still 5 years (formerly
!                    specified by the NYEARS PARAMETER).
!                    R. Brode, US EPA, OAQPS, AQMG, 02/29/2012
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  Pathway (ME) and Keyword
!
!        OUTPUTS: Meteorology Option Switches
!                 Meteorology Setup Status Switches
!
!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, ND, NDYS

!     Variable Initializations
   MODNAM = 'MECARD'

   IF (KEYWRD .EQ. 'STARTING') THEN
!        Set Status Switch
      IMSTAT(1) = IMSTAT(1) + 1
      IF (IMSTAT(1) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      END IF

   ELSE IF (KEYWRD .EQ. 'SURFFILE') THEN
!        Set Status Switch
      IMSTAT(2) = IMSTAT(2) + 1
      IF (IMSTAT(2) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Surface Meteorology File Information    ---   CALL SURFIL
         CALL SURFIL
      END IF

   ELSE IF (KEYWRD .EQ. 'PROFFILE') THEN
!        Set Status Switch
      IMSTAT(3) = IMSTAT(3) + 1
      IF (IMSTAT(3) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Profile Meteorology File Information    ---   CALL PROFIL
         CALL PROFIL
      END IF

   ELSE IF (KEYWRD .EQ. 'SURFDATA') THEN
!        Set Status Switch
      IMSTAT(4) = IMSTAT(4) + 1
      IF (IMSTAT(4) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Surface Data Information                ---   CALL SFDATA
         CALL SFDATA
      END IF

   ELSE IF (KEYWRD .EQ. 'UAIRDATA') THEN
!        Set Status Switch
      IMSTAT(5) = IMSTAT(5) + 1
      IF (IMSTAT(5) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Upper Air Data Information              ---   CALL UADATA
         CALL UADATA
      END IF

   ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'STARTEND') THEN
!        Set Status Switch
      IMSTAT(6) = IMSTAT(6) + 1
      IF (SCIM) THEN
!           Write out error message:  STARTEND cannot be used with SCIM option
         CALL ERRHDL(PATH,MODNAM,'E','154',KEYWRD)
      ELSE
         IF (IMSTAT(6) .NE. 1) THEN
!              WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
!              Process Start and End Dates for Reading      ---   CALL STAEND
            CALL STAEND
         END IF
      END IF

   ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'DAYRANGE') THEN
!        Set Status Switch
      IMSTAT(7) = IMSTAT(7) + 1
      IF (SCIM) THEN
!           Write out error message:  DAYRANGE cannot be used with SCIM option
         CALL ERRHDL(PATH,MODNAM,'E','154',KEYWRD)
      ELSE
!           Check for First Occurrence of DAYRANGE Card, and
!           Reinitialize IPROC and IPROCL Arrays to 0's
         IF (IMSTAT(7) .EQ. 1) THEN
            IPROC(:)  = 0
            IPROCL(:) = 0
         END IF
!           Process Days and Day Ranges for Processing      ---   CALL DAYRNG
         CALL DAYRNG
      END IF

   ELSE IF (KEYWRD .EQ. 'WDROTATE') THEN
!        Set Status Switch
      IMSTAT(8) = IMSTAT(8) + 1
      IF (IMSTAT(8) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Wind Direction Correction Option        ---   CALL WDROTA
         CALL WDROTA
      END IF

   ELSE IF (KEYWRD .EQ. 'SITEDATA') THEN
!        Set Status Switch
      IMSTAT(9) = IMSTAT(9) + 1
      IF (IMSTAT(9) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process On-site Data Information                ---   CALL ONDATA
         CALL ONDATA
      END IF

   ELSE IF (KEYWRD .EQ. 'PROFBASE') THEN
!        Set Status Switch
      IMSTAT(10) = IMSTAT(10) + 1
      IF (IMSTAT(10) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process On-site Data Information                ---   CALL PRBASE
         CALL PRBASE
      END IF

   ELSE IF (KEYWRD .EQ. 'WINDCATS') THEN
!        Set Status Switch
      IMSTAT(11) = IMSTAT(11) + 1
      IF (IMSTAT(11) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Wind Speed Categories                   ---   CALL WSCATS
         CALL WSCATS
      END IF

   ELSE IF (KEYWRD .EQ. 'SCIMBYHR' .AND. SCIM) THEN
!        Set Status Switch
      IMSTAT(12) = IMSTAT(12) + 1
      IF (IMSTAT(12) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Wind Speed Categories                   ---   CALL SCIMIT
         CALL SCIMIT
      END IF

   ELSE IF (KEYWRD .EQ. 'NUMYEARS') THEN
!        Set Status Switch
      IMSTAT(13) = IMSTAT(13) + 1
      IF (IMSTAT(13) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Number of Years for MAXDCONT arrays     ---  CALL NUMYR
         CALL NUMYR
      END IF

!     JAT 1/29/21 ISSUE D070 TURBULENCE OPTIONS
!     ADD CHECK FOR ONE OF THE TURBULENCE KEYWORDS
   ELSE IF (KEYWRD .EQ. 'NOTURB  ' .OR. KEYWRD .EQ. 'NOTURBST' .OR.&
   &KEYWRD .EQ. 'NOTURBCO' .OR. KEYWRD .EQ. 'NOSA    ' .OR.&
   &KEYWRD .EQ. 'NOSW    ' .OR. KEYWRD .EQ. 'NOSAST  ' .OR.&
   &KEYWRD .EQ. 'NOSWST  ' .OR. KEYWRD .EQ. 'NOSACO  ' .OR.&
   &KEYWRD .EQ. 'NOSWCO  ') THEN
!        Set Status Switch
      IMSTAT(14) = IMSTAT(14) + 1
      IF (IMSTAT(14) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process turbulence option     ---  CALL TURBOPTS
         CALL TURBOPT
      END IF
   ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
!        Set Status Switch
      IMSTAT(50) = IMSTAT(50) + 1
      IF (IMSTAT(50) .NE. 1) THEN
!           WRITE Error Message: Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         GO TO 999
      END IF
!        Write Error Messages for Missing Mandatory Keyword(s)
      IF (IMSTAT(1) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
      END IF
      IF (IMSTAT(2) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','SURFFILE')
      END IF
      IF (IMSTAT(3) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','PROFFILE')
      END IF
      IF (IMSTAT(4) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','SURFDATA')
      END IF
      IF (IMSTAT(5) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','UAIRDATA')
      END IF
      IF (IMSTAT(10) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','PROFBASE')
      END IF
      IF (SCIM .AND. IMSTAT(12) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','SCIMBYHR')
      END IF

!        OPEN Met Data File                                 ---   CALL MEOPEN
      IF (IMSTAT(2) .NE. 0 .AND. IMSTAT(3) .NE. 0) THEN
         CALL MEOPEN
      END IF

! ---    Assign L_LeapYear variable
      IF ((MOD(ISYEAR,4) .NE. 0) .OR.&
      &(MOD(ISYEAR,100) .EQ. 0 .AND. MOD(ISYEAR,400) .NE. 0)) THEN
!           Not a Leap Year
         L_LeapYear = .FALSE.
      ELSE
!           Leap Year
         L_LeapYear = .TRUE.
      END IF

      IF (MULTYR) THEN
!           Set the Increment for Saving Results, INCRST, Based on
!           ISYEAR, Surface Data Year, from SURFDATA Keyword
         IF ((MOD(ISYEAR,4) .NE. 0) .OR.&
         &(MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
!              Not a Leap Year
            INCRST = 365
         ELSE
!              Leap Year
            INCRST = 366
         END IF
      END IF

!        Determine Number of Hours to be Processed, NHOURS, For Use
!        With the TOXXFILE Option - 9/29/92
      IF ((MOD(ISYEAR,4) .NE. 0) .OR.&
      &(MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
!           Not a Leap Year
         ND = 365
      ELSE
!           Leap Year
         ND = 366
      END IF
      NDYS = 0
      DO I = 1, ND
! ---       Adjust for leap year vs. non-leap year
         IF (ND .EQ. 365) THEN
            IF (IPROC(I) .EQ. 1) THEN
               NDYS = NDYS + 1
            END IF
         ELSE IF (ND .EQ. 366) THEN
            IF (IPROCL(I) .EQ. 1) THEN
               NDYS = NDYS + 1
            END IF
         END IF
      END DO
      NHOURS = NDYS * 24

   ELSE
!        Write Error Message: Invalid Keyword for This Pathway
      CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
   END IF

999 RETURN
END

SUBROUTINE SURFIL
!***********************************************************************
!                 SURFIL Module of AERMOD
!
!        PURPOSE: Process Surface Meteorology Input File Options
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, James Paumier
!
!        DATE:    September 30, 1993
!
!        MODIFIED:  Remove optional field for format; FREE format
!                   is used for all met inputs
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Meteorological Data Filename and Format
!
!        ERROR HANDLING:   Checks for No Parameters;
!                          Checks for No Format (uses default);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'SURFIL'

   IF (IFC .EQ. 3) THEN
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         METINP = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  METINP Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF
   ELSE IF (IFC .EQ. 4) THEN
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         METINP = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  METINP Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF
      IF (FIELD(4) .NE. 'FREE') THEN
!           WRITE Warning Message         ! Format field no longer used
         CALL ERRHDL(PATH,MODNAM,'W','293','format')
      END IF
   ELSE IF (IFC .GT. 4) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! No Parameters Specified
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END

SUBROUTINE PROFIL
!***********************************************************************
!                 PROFIL Module of AERMOD
!
!        PURPOSE: Process Profile Meteorology Input File Options
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, James Paumier
!
!        DATE:    September 30, 1993
!
!        MODIFIED:  Remove optional field for format; FREE format
!                   is used for all met inputs
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!                   Check length of format string for PROFRM.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Meteorological Data Filename and Format
!
!        ERROR HANDLING:   Checks for No Parameters;
!                          Checks for No Format (uses default);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'PROFIL'

   IF (IFC .EQ. 3) THEN
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         PROINP = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  PROINP Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF
   ELSE IF (IFC .EQ. 4) THEN
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         PROINP = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  PROINP Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF
      IF (FIELD(4) .NE. 'FREE') THEN
!           WRITE Warning Message         ! Format field no longer used
         CALL ERRHDL(PATH,MODNAM,'W','293','format')
      END IF
   ELSE IF (IFC .GT. 4) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! No Parameters Specified
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END

SUBROUTINE SFDATA
!***********************************************************************
!                 SFDATA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Meteorology Surface Data Station Options
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT).
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Meteorological Surface Data Station Identification
!
!        ERROR HANDLING:   Checks for Too Few Parameters;
!                          Checks for Invalid Numeric Fields;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'SFDATA'

   IF (IFC .EQ. 2) THEN
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC .LT. 4) THEN
!        WRITE Error Message           ! Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC .GT. 7) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT .NE. 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      IDSURF = 0
      GO TO 199
   END IF
   IDSURF = NINT(FNUM)

199 CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT .NE. 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ISYEAR = 0
      GO TO 299
   END IF
   ISYEAR = NINT(FNUM)

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert ISYEAR to Four Digits
   IF (ISYEAR .LE. 99) THEN
      CALL CENT_DATE(ISYEAR,ISYEAR)
   END IF
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C     Check for 2-digit Input and Convert ISYEAR to Four Digits
!      IF (ISYEAR .GE. ISTRT_WIND .AND. ISYEAR .LE. 99) THEN
!         ISYEAR = ISTRT_CENT*100 + ISYEAR
!      ELSE IF (ISYEAR .LT. ISTRT_WIND) THEN
!         ISYEAR = (ISTRT_CENT+1)*100 + ISYEAR
!      END IF

299 IF (IFC .GE. 5) THEN
!        Retrieve Surface Data Station Name (Optional)
      SFNAME = FIELD(5)
   ELSE
      SFNAME = 'UNKNOWN'
   END IF

   IF (IFC .EQ. 7) THEN
!        Retrieve Coordinates for Surface Data Location (Optional)
      CALL STODBL(FIELD(6),ILEN_FLD,SFX,IMIT)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
      CALL STODBL(FIELD(7),ILEN_FLD,SFY,IMIT)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
   END IF

999 RETURN
END

SUBROUTINE UADATA
!***********************************************************************
!                 UADATA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Meteorology Upper Air Data Station Options
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT).
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Meteorological Upper Air Data Station Identification
!
!        ERROR HANDLING:   Checks for Too Few Parameters;
!                          Checks for Invalid Numeric Fields;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'UADATA'

   IF (IFC .EQ. 2) THEN
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC .LT. 4) THEN
!        WRITE Error Message           ! Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC .GT. 7) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT .NE. 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      IDUAIR = 0
      GO TO 199
   END IF
   IDUAIR = NINT(FNUM)

199 CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT .NE. 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      IUYEAR = 0
      GO TO 299
   END IF
   IUYEAR = NINT(FNUM)

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert IUYEAR to Four Digits
   IF (IUYEAR .LE. 99) THEN
      CALL CENT_DATE(IUYEAR,IUYEAR)
   END IF
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C     Convert IUYEAR to Four Digits
!      IF (IUYEAR .GE. ISTRT_WIND .AND. IUYEAR .LE. 99) THEN
!         IUYEAR = ISTRT_CENT*100 + IUYEAR
!      ELSE IF (IUYEAR .LT. ISTRT_WIND) THEN
!         IUYEAR = (ISTRT_CENT+1)*100 + IUYEAR
!      END IF

299 IF (IFC .GE. 5) THEN
!        Retrieve Surface Data Station Name (Optional)
      UANAME = FIELD(5)
   ELSE
      UANAME = 'UNKNOWN'
   END IF

   IF (IFC .EQ. 7) THEN
!        Retrieve Coordinates for Surface Data Location (Optional)
      CALL STODBL(FIELD(6),ILEN_FLD,UAX,IMIT)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
      CALL STODBL(FIELD(7),ILEN_FLD,UAY,IMIT)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
   END IF

999 RETURN
END

SUBROUTINE ONDATA
!***********************************************************************
!                 ONDATA Module of AERMOD
!
!        PURPOSE: Process On-site Meteorology Data Station Options
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, James Paumier
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: On-site Meteorological Data Station Identification
!
!        ERROR HANDLING:   Checks for Too Few Parameters;
!                          Checks for Invalid Numeric Fields;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'ONDATA'

   IF (IFC .EQ. 2) THEN
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC .LT. 4) THEN
!        WRITE Error Message           ! Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC .GT. 7) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT .NE. 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 199
   END IF
   IDSITE = NINT(FNUM)

199 CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT .NE. 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 299
   END IF
   IOYEAR = NINT(FNUM)

299 IF (IFC .GE. 5) THEN
!        Retrieve Surface Data Station Name (Optional)
      ONNAME = FIELD(5)
   ELSE
      ONNAME = 'UNKNOWN'
   END IF

   IF (IFC .EQ. 7) THEN
!        Retrieve Coordinates for Surface Data Location (Optional)
      CALL STODBL(FIELD(6),ILEN_FLD,ONX,IMIT)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
      CALL STODBL(FIELD(7),ILEN_FLD,ONY,IMIT)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
   END IF

999 RETURN
END

SUBROUTINE PRBASE
!***********************************************************************
!                 PRBASE Module of the AERMOD Model
!
!        PURPOSE: Process Inputs for Profile Base Elevation
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 9, 1998
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Profile Base Elevation (m MSL), ZBASE
!
!        ERROR HANDLING:   Checks for No Parameters;
!                          Checks for No Units (uses default of m);
!                          Checks for Invalid or Suspicious Values of ZBASE;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'PRBASE'

   IF (IFC .EQ. 3 .OR. IFC .EQ. 4) THEN
      CALL STODBL(FIELD(3),ILEN_FLD,ZBASE,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      IF (IFC .EQ. 4 .AND. FIELD(4) .EQ. 'FEET') THEN
         ZBASE = 0.3048D0 * ZBASE
      ELSE IF (IFC .EQ. 4 .AND. FIELD(4) .NE. 'METERS') THEN
!           WRITE Warning Message - Invalid ZRUNIT Parameter
         CALL ERRHDL(PATH,MODNAM,'W','203','ZRUNIT')
      END IF
      IF (ZBASE .LT. 0.0D0 .AND. IMIT .EQ. 1) THEN
!           WRITE Warning Message - Possible Error In ZBASE
         CALL ERRHDL(PATH,MODNAM,'W','340',KEYWRD)
      END IF
   ELSE IF (IFC .GT. 4) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

!     Reinitialize AZS, AZELEV, and AZHILL arrays for FLAT terrain
   IF (FLAT) THEN
      IF (.NOT. FLATSRCS) THEN
!           Assign ZBASE to source elevation for all sources
         AZS = ZBASE
      ELSE
!           Assign ZBASE to source elevation only for FLAT sources
         DO ISRC = 1, NUMSRC
            IF (L_FLATSRC(ISRC)) THEN
               AZS(ISRC) = ZBASE
            END IF
         END DO
      END IF
      IF (.NOT. FLATSRCS) THEN
!           Assign ZBASE to AZELEV and AZHILL for all receptors
         DO IREC = 1, NUMREC
            AZELEV(IREC) = ZBASE
            AZHILL(IREC) = ZBASE
         END DO
      END IF
   END IF

999 RETURN
END

SUBROUTINE STAEND
!***********************************************************************
!                 STAEND Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Start and End Dates for Meteorology File
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Modified code for setting the month, day, and hour
!                    for the "end of the year", based on the STARTEND
!                    keyword, to resolve potential problems for PM-2.5
!                    and ANNUAL average applications in which the first
!                    hour of the file is not 01.  Also improved the
!                    error handling for STARTEND date inputs that are
!                    out of range, and included check for STARTEND
!                    data range less than 1 complete year for ANNUAL
!                    averages.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                    of 10-digit variables for start date (ISDATE) and
!                    end date (IEDATE).
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Start and End Dates to Read from Meteorological File
!
!        ERROR HANDLING:   Checks for Too Few Parameters;
!                          Checks for Invalid Numeric Fields;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER IDYMAX(12)

!     Variable Initializations
   DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/

   MODNAM = 'STAEND'

   IF (IFC .EQ. 8) THEN
!        Process for YR, MD, DY
      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 198
      END IF
      ISYR = NINT(FNUM)
198   CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 298
      END IF
      ISMN = NINT(FNUM)
      IF (ISMN .LT. 1 .OR. ISMN .GT. 12) THEN
!           WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
      END IF
298   CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 398
      END IF
      ISDY = NINT(FNUM)
      IF (ISMN .GE. 1 .AND. ISMN .LE. 12) THEN
         IF (ISDY .LT. 1 .OR. ISDY .GT. IDYMAX(ISMN)) THEN
!              WRITE Error Message    ! Invalid Day
            CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
         END IF
      END IF
398   CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 498
      END IF
      IEYR = NINT(FNUM)
498   CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 598
      END IF
      IEMN = NINT(FNUM)
      IF (IEMN .LT. 1 .OR. IEMN .GT. 12) THEN
!           WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
      END IF
598   CALL STONUM(FIELD(8),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 698
      END IF
      IEDY = NINT(FNUM)
      IF (IEMN .GE. 1 .AND. IEMN .LE. 12) THEN
         IF (IEDY .LT. 1 .OR. IEDY .GT. IDYMAX(IEMN)) THEN
!              WRITE Error Message    ! Invalid Day
            CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
         END IF
      END IF
698   CONTINUE

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert ISYR to Four Digits
      IF (ISYR .LE. 99) THEN
         CALL CENT_DATE(ISYR,ISYR)
      END IF
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Convert ISYR and IEYR to Four Digits
!         IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
!            ISYR = ISTRT_CENT*100 + ISYR
!         ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!            ISYR = (ISTRT_CENT+1)*100 + ISYR
!         END IF

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert IEYR to Four Digits
      IF (IEYR .LE. 99) THEN
         CALL CENT_DATE(IEYR,IEYR)
      END IF
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!         IF (IEYR .GE. ISTRT_WIND .AND. IEYR .LE. 99) THEN
!            IEYR = ISTRT_CENT*100 + IEYR
!         ELSE IF (IEYR .LT. ISTRT_WIND) THEN
!            IEYR = (ISTRT_CENT+1)*100 + IEYR
!         END IF

!        Calculate JULIAN Day for Start and End Dates
      CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
      CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
!        Use 1 for Start Hour and 24 for End Hour
      ISHR = 1
      IEHR = 24
! ---    Calculate 10-digit start date (ISDATE) and end date (IEDATE)
!        including 4-digit year (for comparisons with FULLDATE)
      IF (ISYR .LE. 2147) THEN
         ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
      ELSE
         CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
         ISDATE = 2147123124
      END IF
      IF (IEYR .LE. 2147) THEN
         IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
      ELSE
         CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
         IEDATE = 2147123124
      END IF

   ELSE IF (IFC .EQ. 10) THEN
!        Process for YR, MD, DY, HR
      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 199
      END IF
      ISYR = NINT(FNUM)
199   CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 299
      END IF
      ISMN = NINT(FNUM)
      IF (ISMN .LT. 1 .OR. ISMN .GT. 12) THEN
!           WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
      END IF
299   CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 399
      END IF
      ISDY = NINT(FNUM)
      IF (ISMN .GE. 1 .AND. ISMN .LE. 12) THEN
         IF (ISDY .LT. 1 .OR. ISDY .GT. IDYMAX(ISMN)) THEN
!              WRITE Error Message    ! Invalid Day
            CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
         END IF
      END IF
399   CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 499
      END IF
      ISHR = NINT(FNUM)
      IF (ISHR .LT. 1 .OR. ISHR .GT. 24) THEN
!           WRITE Error Message    ! Invalid Hour
         CALL ERRHDL(PATH,MODNAM,'E','203','HOUR')
      END IF
499   CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 599
      END IF
      IEYR = NINT(FNUM)
599   CALL STONUM(FIELD(8),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 699
      END IF
      IEMN = NINT(FNUM)
      IF (IEMN .LT. 1 .OR. IEMN .GT. 12) THEN
!           WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
      END IF
699   CALL STONUM(FIELD(9),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 799
      END IF
      IEDY = NINT(FNUM)
      IF (IEMN .GE. 1 .AND. IEMN .LE. 12) THEN
         IF (IEDY .LT. 1 .OR. IEDY .GT. IDYMAX(IEMN)) THEN
!              WRITE Error Message    ! Invalid Day
            CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
         END IF
      END IF
799   CALL STONUM(FIELD(10),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 899
      END IF
      IEHR = NINT(FNUM)
      IF (IEHR .LT. 1 .OR. IEHR .GT. 24) THEN
!           WRITE Error Message    ! Invalid Hour
         CALL ERRHDL(PATH,MODNAM,'E','203','HOUR')
      END IF
899   CONTINUE

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert ISYR to Four Digits
      IF (ISYR .LE. 99) THEN
         CALL CENT_DATE(ISYR,ISYR)
      END IF
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Convert ISYR and IEYR to Four Digits
!         IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
!            ISYR = ISTRT_CENT*100 + ISYR
!         ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!            ISYR = (ISTRT_CENT+1)*100 + ISYR
!         END IF

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert IEYR to Four Digits
      IF (IEYR .LE. 99) THEN
         CALL CENT_DATE(IEYR,IEYR)
      END IF
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!         IF (IEYR .GE. ISTRT_WIND .AND. IEYR .LE. 99) THEN
!            IEYR = ISTRT_CENT*100 + IEYR
!         ELSE IF (IEYR .LT. ISTRT_WIND) THEN
!            IEYR = (ISTRT_CENT+1)*100 + IEYR
!         END IF

!        Calculate JULIAN Day for Start and End Dates
      CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
      CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
!        Calculate 10-digit start date (ISDATE) and end date (IEDATE)
      IF (ISYR .LE. 2147) THEN
         ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
      ELSE
         CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
         ISDATE = 2147123124
      END IF
      IF (IEYR .LE. 2147) THEN
         IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
      ELSE
         CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
         IEDATE = 2147123124
      END IF

   ELSE IF (IFC .GT. 8) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999

   ELSE
!        WRITE Error Message           ! Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     Determine MN, DY, and HR for end-of-the-year check.
!     Subtract one from start hour to set end hour for the year of data
   IF (ISHR .GT. 1) THEN
      IENDHOUR = ISHR - 1
      IENDDY   = ISDY
      IENDMN   = ISMN
   ELSE
      IENDHOUR = 24
      IF (ISDY .GT. 1) THEN
         IENDDY = ISDY - 1
         IENDMN = ISMN
      ELSE
         IENDMN = ISMN - 1
         IF (IENDMN .EQ. 0) IENDMN = 12
         IENDDY = IDYMAX(IENDMN)
      END IF
   END IF

!     Check for End Year .LT. Start Year
   IF (IEYR .LT. ISYR) THEN
!        WRITE Error Message    ! Invalid End Year
      CALL ERRHDL(PATH,MODNAM,'E','203','END YEAR')
      GO TO 999
   END IF

!     Check for STARTEND period less than a complete year if
!     ANNUAL average is specified
   IF (ANNUAL .OR. MULTYR .OR. L_MAXDCONT) THEN
!        First check for End Year = Start Year,
!        then for End Year = Start Year + 1, otherwise
!        if End Year - Start Year > 1 no further checks needed
      IF (IEYR .EQ. ISYR) THEN
!           End Year equals Start Year, therefore the
!           Start Month, Start Day, Start Hour must be 01/01/01, and
!           End Month, End Day, End Hour must be 12/31/24
         IF (ISMN .NE.  1 .OR. ISDY .NE.  1 .OR. ISHR .NE.  1 .OR.&
         &IEMN .NE. 12 .OR. IEDY .NE. 31 .OR. IEHR .NE. 24) THEN
!              WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
            CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
         END IF
      ELSE IF (IEYR - ISYR .EQ. 1) THEN
!           End Year is Start Year plus 1, therefore the
!           End Month, End Day, End Hour must greater than or equal to
!           Start Month, Start Day, Start Hour
         IF (IEMN .LT. IENDMN) THEN
!              WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
            CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
         ELSE IF (IEMN .EQ. IENDMN) THEN
            IF (IEDY .LT. IENDDY) THEN
!                 WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
               CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
            ELSE IF (IEDY .EQ. IENDDY) THEN
               IF (IEHR .LT. IENDHOUR) THEN
!                    WRITE Error Message ! Incomplete Year for MULTYR or ANNUAL
                  CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
               END IF
            END IF
         END IF
      END IF
   END IF

999 RETURN
END

SUBROUTINE DAYRNG
!***********************************************************************
!                 DAYRNG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process the Selection of Days and Ranges of Days
!                 for Processing from the Meteorology File
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Modified to account for leap-years vs. non-leap-years
!                    in cases where DAYRANGE inputs are defined based on
!                    the Month/Day rather than Julian days.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/10/2012
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Array of Dates to Process from Meteorological File
!
!        ERROR HANDLING:   Checks for Too Few Parameters;
!                          Checks for Invalid Numeric Fields;
!                          Checks for Improper Combinations of Fields;
!                          Checks for Dates Out of Range
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, K, IMN, IDY, IMN1, IDY1, IMN2, IDY2, JDAYB, JDAYE
   CHARACTER BEGRNG*8, ENDRNG*8, CMN1*8, CDY1*8, CMN2*8, CDY2*8
   CHARACTER BLNK08*8
   LOGICAL RMARK, GMARK

!     Variable Initializations
   DATA BLNK08/'        '/

   MODNAM = 'DAYRNG'

   IF (IFC .LT. 3) THEN
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   ELSE
      DO 40 I = 3, IFC
!           First Check For Range Marker (-) And Gregorian Day Marker (/)
!           Initialize Character Fields
         BEGRNG = BLNK08
         ENDRNG = BLNK08
         CMN1 = BLNK08
         CDY1 = BLNK08
         CMN2 = BLNK08
         CDY2 = BLNK08
         CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,&
         &BEGRNG,ENDRNG)
         CALL FSPLIT(PATH,KEYWRD,BEGRNG,8,'/',GMARK,CMN1,CDY1)
         IF (RMARK .AND. GMARK) THEN
            CALL FSPLIT(PATH,KEYWRD,ENDRNG,8,'/',GMARK,CMN2,CDY2)
         END IF

         IF (.NOT.RMARK .AND. .NOT.GMARK) THEN
!              Field Must Be a Single Julian Day
            CALL STONUM(BEGRNG,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               JDAY = NINT(FNUM)
            END IF
            IF (JDAY.GE.1 .AND. JDAY.LE.366 .AND. IMIT.EQ.1) THEN
               IPROC(JDAY)  = 1
!                 Also need IPROCL array for Leap Years
               IPROCL(JDAY) = 1
            ELSE
!                 WRITE Error Message    ! Invalid Julian Day
               CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
            END IF
            IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
!                 WRITE Warning Message  ! Julian Day Out-of-Range
               WRITE(DUMMY,'(I8)') JDAY
               CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
            END IF

         ELSE IF (RMARK .AND. .NOT.GMARK) THEN
!              Field Must Be a Julian Day Range - Extract Beg & End
            CALL STONUM(BEGRNG,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               JDAYB = NINT(FNUM)
            END IF
            CALL STONUM(ENDRNG,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               JDAYE = NINT(FNUM)
            END IF
            IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.&
            &(JDAYE .LE. 366)) THEN
               DO K = JDAYB, JDAYE
                  IPROC(K)  = 1
!                    Also need IPROCL array for Leap Years
                  IPROCL(K) = 1
               END DO
            ELSE
!                 WRITE Error Message    ! Invalid Julian Day Range
               CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
            END IF
            IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
!                 WRITE Warning Message  ! Julian Day Out-of-Range
               WRITE(DUMMY,'(I3,"-",I3)') JDAYB, JDAYE
               CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
            END IF

         ELSE IF (.NOT.RMARK .AND. GMARK) THEN
!              Field Must Be a Single Month/Day
            CALL STONUM(CMN1,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               IMN = NINT(FNUM)
            END IF
            CALL STONUM(CDY1,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               IDY = NINT(FNUM)
            END IF
! ---          Determine JULIAN Day Number; For Non-Leap Year First;
!              Note that JDAY for MN/DY inputs will be assigned based on
!              on the Year specified on the ME SURFFILE keyword. However,
!              the IPROCL array used to identify which Julian day(s) to be
!              processed for leap years will be assigned based on the MN/DY
!              input by the user.
            CALL JULIAN(ISYEAR,IMN,IDY,JDAY)

            IF ( (MOD(ISYEAR,4) .NE. 0) .OR.&
            &(MOD(ISYEAR,100) .EQ. 0 .AND.&
            &MOD(ISYEAR,400) .NE. 0) ) THEN
!                 Not a Leap Year; Get JULIAN day number for specified MN/DY
! ---             Assign specified MN/DY to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               IF (JDAY .GE. 1 .AND. JDAY .LE. 365) THEN
                  IPROC(JDAY) = 1
                  IF (IMN .GT. 2) THEN
                     IPROCL(JDAY+1) = 1
                  ELSE
                     IPROCL(JDAY) = 1
                  END IF
               ELSE
!                    WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
! ---             Check for consistency with STARTEND inputs, if provided
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
!                    WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE
! ---             Determine JULIAN Day Number; For Leap Year
! ---             Assign specified MD/DY to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               IF (JDAY .GE. 1 .AND. JDAY .LE. 366) THEN
                  IPROCL(JDAY) = 1
                  IF (IMN .GT. 2) THEN
                     IPROC(JDAY-1) = 1
                  ELSE
                     IPROC(JDAY)   = 1
                  END IF
               ELSE
!                    WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
! ---             Check for consistency with STARTEND inputs, if provided
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
!                    WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF
            END IF

         ELSE IF (RMARK .AND. GMARK) THEN
!              Field Must Be a Greg. Date Range (MN/DY-MN/DY)
            CALL STONUM(CMN1,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 41
            ELSE
               IMN1 = NINT(FNUM)
            END IF
            CALL STONUM(CDY1,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 41
            ELSE
               IDY1 = NINT(FNUM)
            END IF
41          CALL STONUM(CMN2,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               IMN2 = NINT(FNUM)
            END IF
            CALL STONUM(CDY2,8,FNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 40
            ELSE
               IDY2 = NINT(FNUM)
            END IF

! ---          Determine JULIAN Day Number; For Non-Leap Year First
            IF ( (MOD(ISYEAR,4) .NE. 0) .OR.&
            &(MOD(ISYEAR,100) .EQ. 0 .AND.&
            &MOD(ISYEAR,400) .NE. 0) ) THEN
!                 Not a Leap Year; Get JULIAN day numbers for specified
!                 Start MN/DY and End MN/DY, based on STARTEND
               CALL JULIAN(ISYEAR,IMN1,IDY1,JDAYB)
               CALL JULIAN(ISYEAR,IMN2,IDY2,JDAYE)

! ---             Assign specified MN/DY range to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.&
               &(JDAYE .LE. 365)) THEN
! ---                Assign IPROC array for use with non-leap years
                  DO K = JDAYB, JDAYE
                     IPROC(K) = 1
                  END DO
! ---                Assign IPROCL array for use with leap years
                  DO K = JDAYB, JDAYE
                     IPROCL(K) = 1
                  END DO
! ---                Assign IPROCL = 1 for last day if JDAYE = 365
                  IF (JDAYE .EQ. 365) THEN
                     IPROCL(366) = 1
                  END IF
               ELSE
!                    WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
! ---             Check for consistency with STARTEND inputs, if provided
! ---              D146 Changed conditional to reference JDAYB and JDAYE rather than JDAY WSP 10/18/22
               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
!                  IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN

!                    WRITE Warning Message  ! Julian Day Out-of-Range
!  ---               D146 Changed warning to reference JDAYE and JDAYB rather than JDAY WSP 10/18/22
                  WRITE(DUMMY,'(I3,"-",I3)') JDAYB, JDAYE
!                     WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE
! ---             Determine JULIAN Day Number; For Leap Year
!                 Get JULIAN day numbers for specified
!                 Start MN/DY and End MN/DY
               CALL JULIAN(ISYEAR,IMN1,IDY1,JDAYB)
               CALL JULIAN(ISYEAR,IMN2,IDY2,JDAYE)
! ---             Assign specified MN/DY range to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.&
               &(JDAYE .LE. 366)) THEN
                  DO K = JDAYB, JDAYE
                     IPROCL(K) = 1
                  END DO
                  DO K = JDAYB, JDAYE
                     IF (K .LE. 59 ) THEN
                        IPROC(K) = 1
                     ELSE IF (K .GT. 60) THEN
! ---                      Adjust non-leapyear Jday array for March 1 - Dec 31
                        IPROC(K-1) = 1
                     END IF
                  END DO
               ELSE
!                    WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF

               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
!                    WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I3,"-",I3)') JDAYB, JDAYE
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            END IF

         ELSE
!               WRITE Error Message    ! Invalid Field
            CALL ERRHDL(PATH,MODNAM,'E','203','DAYRANGE')
         END IF

40    CONTINUE
   END IF

   RETURN
END

SUBROUTINE WDROTA
!***********************************************************************
!                 WDROTA Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    PROCESSES INPUT FOR ROTATING WIND DIRECTION DATA
!
!     PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!     INPUTS:     Input Runstream Image Parameters
!
!     OUTPUT:     Wind Direction Rotation Angle
!
!     CALLED FROM:   MECARD
!
!     ERROR HANDLING:   Checks for No Parameters;
!                       Checks for Too Many Parameters;
!                       Checks for Invalid Numeric Field
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'WDROTA'

   ROTANG = 0.0D0

   IF (IFC .EQ. 3) THEN
      CALL STODBL(FIELD(3),ILEN_FLD,ROTANG,IMIT)
      IF (IMIT .NE. 1) THEN
!            WRITE Error Message  ! Invalid Numeric Field Encountered
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE IF (DABS(ROTANG) .GT. 180.0D0) THEN
!            WRITE Error Message       ! ROTANG Out of Range
         CALL ERRHDL(PATH,MODNAM,'E','380','ROTANG')
      END IF
   ELSE IF (IFC .GT. 3) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END

SUBROUTINE WSCATS
!***********************************************************************
!                 WSCATS Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    PROCESSES INPUT FOR WIND SPEED CATEGORIES
!
!     PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!     INPUTS:     Input Runstream Image Parameters
!
!     OUTPUT:     Array of Wind Speed Category Limits (5)
!
!     CALLED FROM:   MECARD
!
!     ERROR HANDLING:   Checks for No Parameters;
!                       Checks for Too Many Parameters;
!                       Checks for Invalid Numeric Fields;
!                       Checks for Wind Speed Category Decreasing
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, IWS

!     Variable Initializations
   MODNAM = 'WSCATS'

   IF (IFC .EQ. 7) THEN
!        Fill UCAT Array
      DO I = 3, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
         IF (IMIT .NE. 1) THEN
!              WRITE Error Message  ! Invalid Numeric Field Encountered
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (DNUM .LT. 1.0D0 .OR. DNUM .GT. 20.0D0) THEN
!              WRITE Error Message       ! UCAT Out of Range
            CALL ERRHDL(PATH,MODNAM,'E','380','UCAT')
         ELSE
            IWS = I - 2
            UCAT(IWS) = DNUM
            IF (IWS.GT.1 .AND. UCAT(IWS).LE.UCAT(IWS-1)) THEN
!                 WRITE Error Message    ! Invalid UCAT Value, LE Previous
               CALL ERRHDL(PATH,MODNAM,'E','203','UCAT')
            END IF
         END IF
      END DO
   ELSE IF (IFC .GT. 7) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END

SUBROUTINE MEOPEN
!***********************************************************************
!                 MEOPEN Module of the AERMOD Model
!
!        PURPOSE: Open The Input file for Hourly Meteorological Data,
!                 And Check Header Record
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Meteorology File Specifications
!
!        OUTPUTS: File OPEN Error Status
!
!        CALLED FROM:   SETUP
!
!        REVISION HISTORY:
!         --  Modified code for reading the header record to correct
!             problems with processing of 'SCREEN' option in the
!             AERMET version date field.
!             R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!         --  Modified code for reading the header record of the surface
!             file to include a separate test on the AERMET version date
!             field under the SCREEN option, to allow for the future use
!             of screening meteorology that is not directly linked to a
!             specific version of the AERMET processor.
!             R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!         --  Modified check of version date in header record of surface
!             file.  Fatal error occurs if version date is greater than
!             90000 OR less than current release date.
!             R. Brode, PES, 09/10/02
!         --  Modified comparisons of met station IDs between SURFFILE
!             header record and ME pathway.  IDs are initially read as
!             integers, and then as characters if an error occurs.  Also
!             changed from fatal error to a warning message if a mismatch
!             occurs, and included SITEDATA ID in the comparison.
!             R. Brode, PES, 11/10/98
!         --  Modified to check for version date associated with of AERMET
!             surface file, and compare to two reference dates.  Versions
!             prior to first date cause fatal error, while version prior to
!             second date cause warning.  R. Brode, PES, 11/21/97
!         --  Removed the comparison of the years defined in the input
!             data file with the years declared in the control file
!         --  Added OPEN for the profile file
!         --  Read and interpreted the latitude and longitude in the
!             first record of the SURFFILE
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: METVER, IOSI, ISSI, IUSI
   integer :: LEVEL, JFLAG
   LOGICAL :: FOPEN, MFOPEN, MPOPEN

!     Set Parameters for AERMET Version Dates.
!     Using data > MDATE1 or < MDATE2 causes a fatal error message.
   INTEGER, PARAMETER :: MDATE1 = 90000,  MDATE2 = 12345
!     Using data < MDATE1, > MDATE2, and .NE. MDATE3 causes warning message.
   INTEGER, PARAMETER :: MDATE3 = 14134

   CHARACTER (LEN=6)   :: ASOSTHRESH

   CHARACTER (LEN=8)   :: CUSI, CSSI, COSI
! Unused:      CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
   CHARACTER (LEN=256) :: BUFFER

!     Variable Initializations
   MODNAM = 'MEOPEN'
   FOPEN  = .FALSE.
   MFOPEN = .FALSE.
   MPOPEN = .FALSE.

! --- Initialize AERMOD version date (METVER) and ASOSTHRESH flag
   METVER = 0
   ASOSTHRESH = '      '

!     File Unit Initialized in BLOCK DATA INIT
!     File Format Set By Keyword "SURFFILE" on "ME" pathway
!     OPEN Surface Met Data File --- Formatted is the only option
!     Open with ACTION='READ' to prevent overwrite and allow multiple access
!     READ In the Station Numbers for Comparison to SETUP File

!     Open SURFFILE Met File If Not Already Open
   INQUIRE (FILE=METINP,OPENED=FOPEN)

   IF (.NOT. FOPEN) THEN
!        Open SURFFILE Met File If Not Already Open
      INQUIRE (UNIT=MFUNIT,OPENED=FOPEN)
      IF (.NOT. FOPEN) THEN
!           Open with ACTION='READ' to prevent overwrite and allow multiple access
         OPEN(UNIT=MFUNIT,FILE=METINP,STATUS='OLD',&
         &ERR=998,ACTION='READ',FORM='FORMATTED')
         MFOPEN = .TRUE.

      ELSE
         MFOPEN = .FALSE.
!           SURFFILE Met File is Already Opened With Different Filename
         CALL ERRHDL(PATH,MODNAM,'E','501','SURFFILE')
      END IF
   ELSE
      MFOPEN = .TRUE.
   END IF

   GO TO 1000

!     Write Out Error Message for File OPEN Error
998 CALL ERRHDL(PATH,MODNAM,'E','500','SURFFILE')
!     Skip READ if there is an error opening file

1000 CONTINUE

! --- Next OPEN the PROFFILE Met File
!     File Format Set By Keyword "PROFFILE" on "ME" pathway
!     Open with ACTION='READ' to prevent overwrite and allow multiple access
!     OPEN Profile Met Data File --- Formatted is the only option

! --- Initialize FOPEN to .FALSE.
   FOPEN = .FALSE.

!     Open PROFFILE Met File If Not Already Open
   INQUIRE (FILE=PROINP,OPENED=FOPEN)

   IF (.NOT. FOPEN) THEN
!        Open PROFFILE Met File If Not Already Open
      INQUIRE (UNIT=MPUNIT,OPENED=FOPEN)
      IF (.NOT. FOPEN) THEN
!           Open with ACTION='READ' to prevent overwrite and allow multiple access
         OPEN(UNIT=MPUNIT,FILE=PROINP,STATUS='OLD',&
         &ERR=999,ACTION='READ',FORM='FORMATTED')
         MPOPEN = .TRUE.

      ELSE
         MPOPEN = .FALSE.
!           PROFFILE Met File is Already Opened With Different Filename
         CALL ERRHDL(PATH,MODNAM,'E','501','PROFFILE')
      END IF
   ELSE
      MPOPEN = .TRUE.
   END IF

   GO TO 1001

!     Write Out Error Message for File OPEN Error
999 CALL ERRHDL(PATH,MODNAM,'E','500','PROFFILE')
   MPOPEN = .FALSE.

1001 CONTINUE

   IF( .NOT.MFOPEN .and. .NOT.MPOPEN )THEN
      GOTO 1003
   ELSEIF( MFOPEN )THEN
      CONTINUE
   ENDIF

! --- First read header record as character string to check for AERMET
!     options, THRESH_1MIN, ADJ_U*, CCVR_Sub, and/or TEMP_SUb
! --- Assign SURFFILE to DUMMY variable for read error message
   DUMMY = 'SURFFILE'
   IF (MFOPEN) THEN
      READ(MFUNIT,1200,ERR=99,IOSTAT=IOERRN) BUFFER
1200  FORMAT(A256)
      IF (IOERRN .NE. 0) GOTO 99 ! 16216 Added to check for empty file
   ELSE
      GO TO 1002
   END IF

! --- First extract AERMET version date, C_METVER
   IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
!        Extract AERMET version date
      READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:&
      &INDEX(BUFFER,'VERSION:')+13),'(A6)')&
      &C_METVER
   ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
!        The 'VERSION:' keyword is missing so assign columns 93-98 to C_METVER
      C_METVER = BUFFER(93:98)
   ELSE
      C_METVER = '      '
!        AERMET version not found in header record, issue fatal error message
      CALL ERRHDL(PATH,MODNAM,'E','395','No Version')
   ENDIF

! --- Next check for THRESH_1MIN indicating that wind speed threshold was
!     applied to 1-minute ASOS wind data
   IF( INDEX(BUFFER,'THRESH_1MIN') .NE. 0 )THEN
!        Extract 1-min ASOS threshold value and write out Warning message
      READ(BUFFER(INDEX(BUFFER,'=')+1:&
      &INDEX(BUFFER,'m/s')),*)&
      &ASOSTHRESH
      CALL ERRHDL(PATH,MODNAM,'W','186',ASOSTHRESH)
   ENDIF

! --- Check for use of various met data processing options and issue
!     message as appropriate
   IF( INDEX(BUFFER,'ADJ_U*') .NE. 0 )THEN
!        Check for use of ADJ_U* option in AERMET
      L_AdjUstar = .TRUE.
      CALL ERRHDL(PATH,MODNAM,'W','187',' ')
!        Assign keyword to MODOPS array
      MODOPS(23) = 'ADJ_U*'
   ENDIF

!     JAT 1/14/21 ISSUE D077; ADD PROG AS WELL AS MMIF TO ACCOMODATE NEW AERMET
!      IF( INDEX(BUFFER,'MMIF') .NE. 0 )THEN
   IF( INDEX(BUFFER,'MMIF') .NE. 0 .OR. INDEX(BUFFER,'PROG')&
   &.NE. 0 )THEN
!        Check for use of MMIF option in AERMET
      L_MMIF_Data = .TRUE.
      CALL ERRHDL(PATH,MODNAM,'W','182','  ')
!        Assign keyword to MODOPS array
      MODOPS(24) = 'MMIF_Data'
!         IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0 )THEN
      IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0 .OR.&
      &INDEX(BUFFER,'PROG VERSION') .NE. 0 )THEN
!           Extract MMIF Version info to include in output file
         IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0) THEN
            MMIF_Version = BUFFER( INDEX(BUFFER,'MMIF VERSION'):&
            &INDEX(BUFFER,'MMIF VERSION')+28 ) ! CRT 1/11/2021 D077, 26 to 28
         ELSE
            MMIF_Version = BUFFER( INDEX(BUFFER,'PROG VERSION'):&
            &INDEX(BUFFER,'PROG VERSION')+28 ) ! CRT 1/11/2021 D077, 26 to 28
         ENDIF
      ELSE
         MMIF_Version = ''
      ENDIF
   ENDIF

!     CRT 8/9/2023 ISSUE D176; Add 'COARE' flag to accomodate COARE in AERMET
   IF( INDEX(BUFFER,'COARE') .NE. 0 )THEN
!        Set COARE logical to true and MODOPS
      L_COARE = .TRUE.
      MODOPS(27) = 'COARE'
      CALL ERRHDL(PATH,MODNAM,'W','422','  ')
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
!        COARE met requires beta flag
!MGS         IF( .NOT. BETA )THEN
!MGS            CALL ERRHDL(PATH,MODNAM,'E','199','COARE Met')
!MGS         ENDIF
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
   ENDIF

   IF( INDEX(BUFFER,'BULKRN') .NE. 0 )THEN
!        Check for use of BULKRN option in AERMET and assign logical variable;
!        Note that BULKRN is NOT a BETA or non-DFAULT option, and message is
!        issued for informational purposes
      L_BULKRN = .TRUE.
!        Assign keyword to MODOPS array, unless MMIF is also being used
!        CRT 8/9/2023 ISSUE D176; Also cannot use BULKRN with COARE
!        CRT8/11/2023 ISSUE D176: Modify MODOPS to 27 for BULKRN, also used for COARE
!         IF( .NOT. L_MMIF_Data )THEN
      IF( .NOT. L_COARE )THEN
         MODOPS(27) = 'BULKRN'
      ELSE
         CALL ERRHDL(PATH,MODNAM,'E','423','  ')
      ENDIF
! ---    Check for use of MMIF data and adjust BULKRN message accordingly
      IF( L_MMIF_Data )THEN
         CALL ERRHDL(PATH,MODNAM,'W','181','with MMIF')
      ELSE
         CALL ERRHDL(PATH,MODNAM,'W','181','in AERMET')
      ENDIF
   ENDIF

   IF( INDEX(BUFFER,'CCVR_Sub') .NE. 0 )THEN
! ---    Set logical flag indicating that CCVR_Sub option was used
      L_CCVR_Sub = .TRUE.
   ELSE
      L_CCVR_Sub = .FALSE.
   ENDIF

   IF( INDEX(BUFFER,'TEMP_Sub') .NE. 0 )THEN
! ---    Set logical flag indicating that TEMP_Sub option was used
      L_TEMP_Sub = .TRUE.
   ELSE
      L_TEMP_Sub = .FALSE.
   ENDIF

! --- Read Lat/Lon from header record BUFFER
   READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON
1900 FORMAT(2A10)

! --- Now extract UA, SF, and OS station IDs from header record
   IF( INDEX(BUFFER,'UA_ID:') .GT. 0 )THEN
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

   IF( INDEX(BUFFER,'SF_ID:') .GT. 0 )THEN
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

   IF( INDEX(BUFFER,'OS_ID:') .GT. 0 )THEN
      READ(BUFFER(INDEX(BUFFER,'OS_ID:')+7:&
      &INDEX(BUFFER,'OS_ID:')+15),'(A)') COSI
   ELSE
      COSI = '        '
   END IF
   CALL STONUM(COSI,8,FNUM,IMIT)
   IF (IMIT .EQ. 1) THEN
      IOSI = NINT(FNUM)
   ELSE
      IOSI = 0
   END IF

!     Check for valid version of meteorological data.
   IF (SCREEN .AND. C_METVER .NE. 'SCREEN') THEN
!        Check for use of screening meteorology under the SCREEN option
      CALL ERRHDL(PATH,MODNAM,'W','397',C_METVER)
   ELSE IF (.NOT.SCREEN .AND. C_METVER .EQ. 'SCREEN') THEN
!        Check for use of screening meteorology without SCREEN option
      CALL ERRHDL(PATH,MODNAM,'W','398','        ')
   ELSE IF (.NOT.SCREEN) THEN
!        Read integer Julian date from character field for comparison to
!        acceptable AERMET version number.
      READ(C_METVER,'(1X,I5)',ERR=109) METVER
      IF (METVER.GT.MDATE1 .OR. METVER.LT.MDATE2) THEN
! ---       Issue fatal error for use of invalid met version date
         WRITE(DUMMY,'(2X,I5.5)') METVER
         CALL ERRHDL(PATH,MODNAM,'E','395',DUMMY)
      ELSE IF (METVER.GE.MDATE2 .AND. METVER.LT.MDATE3) THEN
! ---       Issue warning message for use of "outdated" met version date
         WRITE(DUMMY,'(2X,I5.5)') METVER
         CALL ERRHDL(PATH,MODNAM,'W','396',DUMMY)
! ---       Set logical flag for use of "old" met data
         L_OldMetVer = .TRUE.
      END IF

      GO TO 110

109   CONTINUE
! ---    Error reading METVER date as integer from C_METVER character string;
!        issue warning message for "outdated" met version date; this allows for
!        interim version "dates" for AERMET drafts, e.g., 13DFT
      CALL ERRHDL(PATH,MODNAM,'W','396',C_METVER)
! ---    Set logical flag for use of "old" met data
      L_OldMetVer = .TRUE.
   END IF

110 CONTINUE

! --- Check Station IDs in SURFFILE for agreement with ME pathway;
!     First check for blank fields in surface file header record
   IF( LEN_TRIM(CSSI) .EQ. 0 .AND. IDSURF .NE. 0 )THEN
!        Write Warning Message:  SURFDATA ID missing;
      CALL ERRHDL(PATH,MODNAM,'W','531','SURFDATA')
   ELSEIF( ISSI .NE. IDSURF )THEN
!        Write Warning Message:  SURFDATA id mismatch
      CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
   ENDIF
   IF( LEN_TRIM(CUSI) .EQ. 0 .AND. IDUAIR .NE. 0 )THEN
!        Write Warning Message:  UAIRDATA ID missing;
      CALL ERRHDL(PATH,MODNAM,'W','531','UAIRDATA')
   ELSEIF( IUSI .NE. IDUAIR )THEN
!        Write Warning Message:  UAIRDATA id mismatch
      CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
   ENDIF
   IF( IMSTAT(9) .EQ. 1 )THEN
      IF( LEN_TRIM(COSI) .EQ. 0 .AND. IDSITE .NE. 0 )THEN
!           Write Warning Message:  SITEDATA ID missing;
         CALL ERRHDL(PATH,MODNAM,'W','531','SITEDATA')
      ELSEIF( IOSI .NE. IDSITE )THEN
!           Write Warning Message:  SITEDATA id mismatch
         CALL ERRHDL(PATH,MODNAM,'W','530','SITEDATA')
      END IF
   ENDIF

!     Get the hemisphere and latitude (from the first record of the
!     scalar file
   CALL DCDLAT ()

! --- Open and read first hour of PROFFILE in order to check for
!     PROFFILE heights that may indicate use of prognostic met data

1002 CONTINUE

! --- Check for whether PROFFILE met file has been opened;
!     otherwise, skip to end
   IF( .NOT. MPOPEN ) GOTO 1003

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

   DO WHILE( JFLAG .EQ. 0 )
      READ( MPUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) KYEAR,&
      &KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,&
      &PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),&
      &PFLSA(LEVEL), PFLSW(LEVEL)

!        Convert the data to the required units
      CALL PFLCNV (LEVEL)

!        Set the number of profile levels to current index, store
!        the 'top of profile' flag, and increment level if not at top
!        Check that the level does not exceed the maximum allowable
      NPLVLS = LEVEL
      IFLAG(LEVEL) = JFLAG

! ---    Check for PFLHT > 999m, which could indicate use of
!        MMIF or other gridded met data inputs
      IF( PFLHT(LEVEL) .GT. 999.0D0 .OR. PFLHT(LEVEL) .LE. 0.0D0)THEN
         IF( .NOT. L_MMIF_Data .AND. .NOT. L_MMIF_Profile )THEN
! ---          Issue warning message for PFLHT > 999m if MMIF data
!              inputs were not specified; message(s) will only be
!              generated for a single profile
            WRITE(DUMMY,'(''LVL'',I2.2,1X,I5,''m'')') LEVEL,&
            &NINT(PFLHT(LEVEL))
            CALL ERRHDL(PATH,MODNAM,'W','184',DUMMY)
            IF( JFLAG .EQ. 1 )THEN
! ---             Top of profile has been flagged for PFLHT > 999m
!                 Set L_MMIF_Profile flag to .TRUE. to turn off
!                 additional warning messages
               L_MMIF_Profile = .TRUE.
            ENDIF
         ENDIF
      ENDIF

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

   REWIND MPUNIT

   GO TO 1003

!     Write Out Error Message for File READ Error
99 CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
   RETURN

1003 CONTINUE

END

SUBROUTINE SCIMIT
!***********************************************************************
!                 SCIMIT Module of AERMOD Model
!
!        PURPOSE: Process Sampled Chronological Input Model (SCIM) Options
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    April 14, 1998
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: SCIM parameters:  Start Hour (1-24)
!                                   Number of Hours to Skip
!                                   Optional filename to summarize
!                                      the SCIM's meteorology
!
!        ERROR HANDLING:   Checks for No Parameters;
!                          Checks for Too Many Parameters;
!                          Checks for Invalid Numeric Inputs
!
!        CALLED FROM:   MECARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, IMIT5, IMIT6

!     Variable Initializations
   MODNAM = 'SCIMIT'
   IMIT5  = 1
   IMIT6  = 1

   IF (IFC .EQ. 4 .OR. IFC .EQ. 6 .OR. IFC .EQ. 8) THEN
      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
!           Issue error message: Invalid numeric field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
!           Assign value for SCIM starting hour
         NREGSTART = NINT( FNUM )
      END IF
      IF (NREGSTART .LT. 1 .OR. NREGSTART .GT. 24) THEN
!           WRITE Error Message        ! Start Hour out of range
         CALL ERRHDL(PATH,MODNAM,'E','380','StartHr')
      END IF

      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT .NE. 1) THEN
!           Issue error message: Invalid numeric field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
!           Assign value for SCIM interval
         NREGINT = NINT( FNUM )
      END IF
      IF (NREGINT .LT. 1) THEN
!           WRITE Error Message        ! NRegInt is out of range
         CALL ERRHDL(PATH,MODNAM,'E','380','NRegInt')
      END IF

      IF (IFC .EQ. 8) THEN
! ---       Issue warning message:  Wet scimming not supported
         CALL ERRHDL(PATH,MODNAM,'W','157',KEYWRD)

! ---       Skip wet scimming inputs in fields 5 and 6

!           Assume fields 7 and 8 are optional files for summary of
!           SCIM'd met data.
         SCIMOUT = .TRUE.
!           Retrieve Sfc Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(7)-LOCB(7)) .LE. (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SCIM_SFCFIL = RUNST1(LOCB(7):LOCE(7))
            OPEN(UNIT=ISUNIT,FILE=SCIM_SFCFIL,STATUS='REPLACE')
         ELSE
!              WRITE Error Message:  SCIM_SFCFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
!           Retrieve Pfl Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(8)-LOCB(8)) .LE. (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SCIM_PROFIL = RUNST1(LOCB(8):LOCE(8))
            OPEN(UNIT=IPUNIT,FILE=SCIM_PROFIL,STATUS='REPLACE')
         ELSE
!              WRITE Error Message:  SCIM_PROFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF

      ELSE IF (IFC .EQ. 6) THEN
! ---       Check fields 5 and 6 for optional SCIM'd met data files or for
!           unsupported wet scimming inputs.

! ---       Check the fields for non-numeric characters; if any are found,
!           then assume that these are optional met data file names;
!           otherwise, assume these are unsupported wet scimming inputs
         DO I = LOCB(5), LOCE(5)
            READ(RUNST1(I:I),'(I1)',ERR=99) IMIT5
         END DO
         DO I = LOCB(6), LOCE(6)
            READ(RUNST1(I:I),'(I1)',ERR=99) IMIT6
         END DO

! ---       Neither field 5 nor 6 has non-numeric characters,
!           assume that these are unsupported wet scimming inputs.
!           Issue warning message:  Wet scimming not supported
         CALL ERRHDL(PATH,MODNAM,'W','157',KEYWRD)

! ---       Done processing, return
         RETURN

! ---       Non-numeric characters found in fields 5 and/or 6;
!           assume these are file names
99       CONTINUE

! ---       At least one field has non-numeric characters,
!           assume that these are optional SCIM'd met data file names
         SCIMOUT = .TRUE.
!           Retrieve Sfc Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SCIM_SFCFIL = RUNST1(LOCB(5):LOCE(5))
            OPEN(UNIT=ISUNIT,FILE=SCIM_SFCFIL,STATUS='REPLACE')
         ELSE
!              WRITE Error Message:  SCIM_SFCFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
!           Retrieve Pfl Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SCIM_PROFIL = RUNST1(LOCB(6):LOCE(6))
            OPEN(UNIT=IPUNIT,FILE=SCIM_PROFIL,STATUS='REPLACE')
         ELSE
!              WRITE Error Message:  SCIM_PROFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF

      END IF

   ELSE IF (IFC .GT. 8) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
   END IF

   RETURN
END

SUBROUTINE NUMYR
!***********************************************************************
!                 NUMYR Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    Processes optional keyword to specify the number of
!                 years in the input meteorological data files to
!                 adjust arrays sizes for the MAXDCONT option
!
!     PROGRAMMER: Roger Brode, U.S. EPA, OAQPS, AQMG
!
!        DATE:    February 29, 2012
!
!     INPUTS:     Input Runstream Image Parameters
!
!     OUTPUT:     Wind Direction Rotation Angle
!
!     CALLED FROM:   MECARD
!
!     ERROR HANDLING:   Checks for No Parameters;
!                       Checks for Too Many Parameters;
!                       Checks for Invalid Numeric Field
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'NUMYR'

   IF (IFC .EQ. 3) THEN
      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
      IF (IMIT .NE. 1) THEN
!           WRITE Error Message  ! Invalid Numeric Field Encountered
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         NYEARS = NINT(FNUM)
         IF ( ABS(FNUM-REAL(NYEARS)) .GT. 1.0E-5 ) THEN
!              WRITE Error Message  ! Invalid Numeric Field, should be integer
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF
   ELSE IF (IFC .GT. 3) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END

SUBROUTINE TURBOPT
!***********************************************************************
!                 TURBOPT Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    Processes optional keyword to specify how to treat
!                 turbulence in the profile file
!
!     PROGRAMMER: James Thurman, U.S. EPA, OAQPS, AQMG
!
!        DATE:    January 29, 2021
!
!     INPUTS:     Input Runstream Image Parameters
!
!     OUTPUT:     set logical variables for TURBOPTS
!
!     CALLED FROM:   MECARD
!
!     ERROR HANDLING:   Checks for correct use of DFAULT keyword;
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
!     LOOPING VARIABLE
   INTEGER I
   LOGICAL LFOUND
   CHARACTER MODNAM*12
   CHARACTER(LEN=8) KEYS(9)

!     Variable Initializations
   MODNAM = 'TURBOPT'

   DATA KEYS /'NOTURB  ','NOTURBST','NOTURBCO','NOSA    ',&
   &'NOSW    ','NOSAST  ','NOSWST  ','NOSACO  ','NOSWCO  '/


   I=1
   LFOUND=.FALSE.
!     LOOK FOR THE KEYWORD IN THE KEYS ARRAY AND SET THE APPROPRIATE VALUE OF TURBOPTS TO TRUE
   DO WHILE( I .LE. 9 .AND. .NOT. LFOUND)
      IF (KEYWRD .EQ. KEYS(I)) THEN
         LFOUND=.TRUE.
         TURBOPTS(I)=.TRUE.
      ELSE
         I=I+1
      ENDIF
   ENDDO

!     WRITE MESSAGE THAT AN OPTION CHOSEN
   CALL ERRHDL(PATH,MODNAM,'I','443',KEYWRD)

!     1:  NOTURB (ignore sigma-theta and sigma-w for all hours)
!     2:  NOTURBST (ignore sigma-theta and sigma-w for stable hours only (OBULEN > 0))
!     3:  NOTURBCO (ignore sigma-theta and sigma-w for convective hours only (OBULEN < 0))
!     4:  NOSA (ignore sigma-theta for all hours)
!     5:  NOSW (ignore sigma-w for all hours)
!     6:  NOSAST (ignore sigma-theta for stable hours only (OBULEN > 0))
!     7:  NOSWST (ignore sigma-w for stable hours only (OBULEN > 0))
!     8:  NOSACO (ignore sigma-theta for convective hours only (OBULEN < 0))
!     9:  NOSWCO (ignore sigma-w for convective hours only (OBULEN < 0))

!     ONLY NOTURB AND NOTURBST CAN BE USED WITH THE DEFAULT KEYWORD
!     IF ONE OF THE OTHERS ARE USED WITH DEFAULT, THEN ISSUE WARNING AND
!     RESET TO FALSE
   IF (I .GT. 2 .AND. DFAULT) THEN
      CALL ERRHDL(PATH,MODNAM,'W','444',KEYWRD)
      TURBOPTS(I)=.FALSE.
   ENDIF

   RETURN
END

SUBROUTINE CENT_DATE(TwoDigitYear,FourDigitYear)
!***********************************************************************
!                 CENT_DATE Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    Processes the input two digit date and calculates the
!                 four digit date assuming that the only century options
!                 are 1900 and 2000
!
!     PROGRAMMER: Wood
!
!        DATE:    9/15/22
!
!     INPUTS:     TwoDigitYear, FourDigitYear defined in the subroutine call
!                 ISTART_WIND and ISTRT_CENT from MAIN1
!
!     OUTPUT:     FourDigitYear - 4 digit year
!                 TwoDigitYear  - 2 digit year
!
!     CALLED FROM:   AERMOD, EVCALC, MESET, and METEXT
!
!     ERROR HANDLING:   Checks if the input two digit year is actaully two digits;
!                       Checks that the input four digit year is actually four digits;
!
!***********************************************************************
!     Variable Declarations
   USE MAIN1, ONLY: ISTRT_WIND, ISTRT_CENT
   IMPLICIT NONE
   INTEGER  :: TwoDigitYear, FourDigitYear

!      Assuming that TwoDigitYear is a two digit value for the year from the SFC file and ISTRT_WIND is
!      a two digit value derived from the user input four digit year (or set to 00 is the
!      user input a two digit year in the INP file

!     Check that FourDigitYear is four digits not two digits
!     IF FourDigitYear is two digits, save it as the two digit year
   IF(FourDigitYear .LE. 99 .AND. FourDigitYear .NE. 0) THEN
      TwoDigitYear = FourDigitYear
   END IF

!MGS   D181_Y2K_WSP: Corrected when the met years cross from 19xx over to 20xx (4/10/2024)
!MGS      IF (TwoDigitYear .NE. ISTRT_WIND .AND. TwoDigitYear .LE. 99) THEN
   IF (TwoDigitYear > ISTRT_WIND .AND. TwoDigitYear .LE. 99) THEN
      FourDigitYear = ISTRT_CENT*100 + TwoDigitYear

   ELSE IF (TwoDigitYear < ISTRT_WIND) THEN !CMGS Add 1 to the century (4/10/2024)
      FourDigitYear = (ISTRT_CENT+1)*100 + TwoDigitYear  !CMGS Add 1 to the century (4/10/2024)
   ELSE IF (TwoDigitYear .GT. 99) THEN
!         Input TwoDigitYear is a 4-digit:  Save to FourDigitYear and convert to 2-digit
      FourDigitYear   = TwoDigitYear
      TwoDigitYear = FourDigitYear - 100 * (FourDigitYear/100)
   END IF

   RETURN
END

SUBROUTINE LONG_DATE(EightDigit,TenDigit,TwoDigit,OutputTwoDigit)
!***********************************************************************
!                 LONG_DATE Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    Processes the 2-digit year from the user and the SFC file
!                 to determine the ten digit date and two digit date
!
!     PROGRAMMER: Wood
!
!     DATE:    9/15/22
!
!     INPUTS:     EightDigit, TenDigit, TwoDigit, OutputTwoDigit defined in the subroutine call
!                 ISTART_WIND and ISTRT_CENT from MAIN1
!
!     OUTPUT:     TenDigitYear   - 10-digit date
!                 OutputTwoDigit - 2-digit date
!
!     CALLED FROM:   CALC2, AERMOD, EVSET, and OUSET
!
!     ERROR HANDLING:   Checks that the input 2-digit year is 2-digits;
!
!***********************************************************************
!     Variable Declarations
   USE MAIN1, ONLY: ISTRT_WIND, ISTRT_CENT
   IMPLICIT NONE
   INTEGER  :: EightDigit, TenDigit, OutputTwoDigit,TwoDigit

!      Assuming that TwoDigitYear is a two digit value for the year from the SFC file and ISTRT_WIND is
!      a two digit value derived from the user input four digit year (or set to 00 is the
!      user input a two digit year in the INP file

   IF (TwoDigit .NE. ISTRT_WIND .AND. TwoDigit .LE. 99) THEN
      OutputTwoDigit = ISTRT_CENT*100 + TwoDigit
      TenDigit = ISTRT_CENT*100000000 + EightDigit
   END IF

   RETURN
END

SUBROUTINE LONG_DATE_Opt2(EightDigit,TenDigit,TwoDigit)
!***********************************************************************
!                 LONG_DATE Module of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE:    PURPOSE:    Processes the 2-digit year from the user and the
!                             SFC file to determine the ten digit date only
!
!     PROGRAMMER: Wood
!
!        DATE:    9/15/22
!
!     INPUTS:     EightDigit,TenDigit,TwoDigit defined in the subroutine call
!                 ISTART_WIND and ISTRT_CENT from MAIN1
!
!     OUTPUT:     TenDigit   - 10-digit date
!
!     CALLED FROM:   EVSET
!
!     ERROR HANDLING:   Checks that the input 2-digit year is 2-digits;
!
!***********************************************************************
!     Variable Declarations
   USE MAIN1, ONLY: ISTRT_WIND, ISTRT_CENT
   IMPLICIT NONE
   INTEGER  :: EightDigit,TenDigit,TwoDigit

!      Assuming that TwoDigitYear is a two digit value for the year from the SFC file and ISTRT_WIND is
!      a two digit value derived from the user input four digit year (or set to 00 is the
!      user input a two digit year in the INP file

   IF (TwoDigit .NE. ISTRT_WIND .AND. TwoDigit .LE. 99) THEN
      TenDigit = ISTRT_CENT*100000000 + EightDigit
   END IF

   RETURN
END
