SUBROUTINE SETUP
!***********************************************************************
!                 SETUP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Processing of Run SETUP Information
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        MODIFIED BY D. Strimaitis, SRC (for GRIDDED TERRAIN Processing)
!
!        MODIFIED:   Removed '1X' from format statements for echoing
!                    the runstream inputs to the output file.  This
!                    was needed when Fortan carriage-control was
!                    invoked, which is no longer the case based on
!                    modifications to subroutine HEADER.
!                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
!
!        MODIFIED:   To remove reference to obsolete TG pathway inherited
!                    from ISCST3 code.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Determine final settings for DRYDPLT and WETDPLT;
!                    reassign as .FALSE. if no deposition calculations
!                    are invoked.  Modify MODOPS header accordingly.
!                    R.W. Brode, MACTEC/PES, Inc. - 10/26/2004
!
!        MODIFIED:   Moved the code to insert a blank line in temporary event
!                    file after each pathway from SUB EVEFIL.
!                    R.W. Brode, PES, Inc. - November 15, 1995.
!
!        MODIFIED:   Default format for METFRM modified to eliminate the
!                    variable ZDM on input.
!                    BY:  J. Paumier, PES              DATE: 27 July 1994
!
!        DATE:    December 15, 1993
!
!        INPUTS:  Input Runstream File
!
!        OUTPUTS: Processing Option Switches
!                 Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Meteorological Data Specifications
!                 Terrain Grid Data Specifications
!                 Output Options
!
!        CALLED FROM:   MAIN
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
!     JAT D065 8/9/21 NOPS AND ILEN SET BUT NOT USED
!      INTEGER :: NOPS, ILEN
!     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!      INTEGER :: MOD_Len

   INTEGER :: I, IFSTAT
   LOGICAL NOPATH, NOKEY
   LOGICAL NOCOMMENTECHO
   CHARACTER RDFRM*20
! JAT 06/22/21 D065
! REMOVE INPFLD AS UNUSED VARIABLE
!      CHARACTER INPFLD*2, PATHWY(6)*2
   CHARACTER PATHWY(6)*2
   INTERFACE
      SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
         CHARACTER (LEN=2), INTENT(IN) :: INPFLD
         CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
         INTEGER, INTENT(IN) :: IPN
         LOGICAL, INTENT(OUT) :: NOPATH
      END SUBROUTINE EXPATH
   END INTERFACE


!     Variable Initializations
   MODNAM = 'SETUP'
   PATH  = '  '
   PPATH = '  '
!     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!      MOD_Len = 0
   EOF = .FALSE.
   NOCOMMENTECHO = .FALSE.

!     Initialize line counters: ILINE for met file; IQLINE for HOUREMIS file; IOLINE for OZONEFIL;
!     INOXLINE for NOX_FILE
   ILINE  = 0
   IQLINE = 0
   IOLINE = 0
   IBLINE = 0
   INOXLINE = 0
!     JAT D065 8/9/21 NOPS SET BUT NOT USED
!      NOPS = 0
!     JAT D065 8/9/21 ILEN SET BUT NOT USED
!      ILEN = 0
! --- Initialize counters for arrays to store met data for MAXDCONT option
   IHR_NDX = 0
   IYR_NDX = 0

!     Initialize PATHWY array
   PATHWY(1) = 'CO'
   PATHWY(2) = 'SO'
   PATHWY(3) = 'RE'
   PATHWY(4) = 'ME'
   PATHWY(5) = 'OU'
   PATHWY(6) = '**'

!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   WRITE(RDFRM,9100) ISTRG, ISTRG
9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

!     LOOP Through Input Runstream Records
   DO WHILE (.NOT. EOF)

!        Increment the Line Counter
      ILINE = ILINE + 1

!        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

!        Check for blank input record; echo the blank record to the
!        output file and the Temporary Event File and then cycle to
!        the next record; unless PATH = 'RE' for EVENT
      IF (LEN_TRIM(RUNST1) .EQ. 0) THEN
         WRITE(IOUNIT,*)
         IF (PATH .NE. 'RE' .and. PATH .NE. 'OU') THEN
!              Skip echo of blank field to Event file for RE pathway
            WRITE(ITEVUT,*)
         END IF
         CYCLE
      END IF

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      CALL LWRUPR

!        Define Fields on Card                              ---   CALL DEFINE
      CALL DEFINE

!        Get the Contents of the Fields                     ---   CALL GETFLD
      CALL GETFLD

      IF (ECHO .and.&
      &(FIELD(1).EQ.'OU' .and. FIELD(2).EQ.'FINISHED')) THEN
!           Echo Last Input Card to Output File (Use Character Substring to
!           Avoid Echoing ^Z Which May Appear at "End of File" for Some
!           Editors).  Also, Allow for Shift in the Input Runstream File of
!           Up to 3 Columns.
         IF (LOCB(1) .EQ. 1) THEN
            WRITE(IOUNIT,9200) RUNST1(1:11)
9200        FORMAT(A11)
         ELSE IF (LOCB(1) .EQ. 2) THEN
            WRITE(IOUNIT,9210) RUNST1(1:12)
9210        FORMAT(A12)
         ELSE IF (LOCB(1) .EQ. 3) THEN
            WRITE(IOUNIT,9220) RUNST1(1:13)
9220        FORMAT(A13)
         ELSE IF (LOCB(1) .EQ. 4) THEN
            WRITE(IOUNIT,9230) RUNST1(1:14)
9230        FORMAT(A14)
         END IF
      ELSE IF (ECHO) THEN
!           Echo Full Input Card to Output File
         WRITE(IOUNIT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
      END IF

!        Check for 'NO ECHO' In First Two Fields
      IF (FIELD(1) .EQ. 'NO' .and. FIELD(2) .EQ. 'ECHO') THEN
         ECHO = .FALSE.
         CYCLE
      END IF

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      CALL EXPATH(FIELD(1),PATHWY,6,NOPATH)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      IF (NOPATH) THEN
!           WRITE Error Message    ! Invalid Pathway ID
         CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
         PATH = PPATH
         CYCLE
      ELSE IF (PATH .EQ. 'RE') THEN
         NOCOMMENTECHO = .TRUE.
      ELSE IF (PATH .EQ. 'ME') THEN
         NOCOMMENTECHO = .FALSE.
      ELSE IF (PATH .EQ. 'OU') THEN
         NOCOMMENTECHO = .TRUE.
      END IF

      IF (PATH .EQ. '**') THEN
         IF (NOCOMMENTECHO) THEN
! ---          Skip echo to temporary event file and cycle
            CYCLE
         ELSE
! ---          "Echo" the comment record to the Temporary Event File
!              and then CYCLE to the next record
            WRITE(ITEVUT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
            CYCLE
         END IF
      END IF

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      CALL EXKEY(FIELD(2),NOKEY)

! Multiple_BuoyLines_D41_Wood
!        Removed logic that BLPGROUP was not operational in v19191
      IF (NOKEY) THEN
!           WRITE Error Message    ! Invalid Keyword
         CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
         PKEYWD = KEYWRD
         CYCLE
      END IF

!        Check for Proper Order of Setup Cards              ---   CALL SETORD
      CALL SETORD

!        Process Input Card Based on Pathway
      IF (PATH .EQ. 'CO') THEN
!           Process COntrol Pathway Cards                   ---   CALL COCARD
         CALL COCARD
! ---       Echo Runstream Image to Temporary Event File (Except EVENTFIL,
!           SAVEFILE, INITFILE & MULTYEAR)
         IF (KEYWRD.NE.'EVENTFIL' .and. KEYWRD.NE.'SAVEFILE' .and.&
         &KEYWRD.NE.'INITFILE' .and. KEYWRD.NE.'MULTYEAR') THEN
            WRITE(ITEVUT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
         END IF
      ELSE IF (PATH .EQ. 'SO') THEN
!           Echo Runstream Image to Temporary Event File
         WRITE(ITEVUT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
!           Process SOurce Pathway Cards                    ---   CALL SOCARD
         CALL SOCARD
      ELSE IF (PATH .EQ. 'RE') THEN
!           Process REceptor Pathway Cards                  ---   CALL RECARD
         CALL RECARD
      ELSE IF (PATH .EQ. 'ME') THEN
!           Process MEteorology Pathway Cards               ---   CALL MECARD
         CALL MECARD
!           Echo Runstream Image to Temporary Event File (Except STARTEND
!           & DAYRANGE)
         IF (KEYWRD.NE.'STARTEND' .and. KEYWRD.NE.'DAYRANGE') THEN
            WRITE(ITEVUT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
         END IF

! ---       Create a character string that includes only those modeling
!           options (MODOPS) that are applicable for this model run
         MODOPS_String = ''
!         JAT D065 8/9/21 NOPS SET BUT NOT USED
!            NOPS = 0
!     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!            MOD_LEN = 0

! ---       Loop through the 30 different options that are flagged
         DO I = 1, 30
            IF (LEN_TRIM(MODOPS(I)) .GT. 0) THEN
!                 JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!                  MOD_LEN = LEN_TRIM(MODOPS(I))
               MODOPS_String =&
               &MODOPS_String(1:LEN_TRIM(MODOPS_String))//'  '//&
               &MODOPS(I)(1:LEN_TRIM(MODOPS(I)))
            END IF
         END DO

      ELSE IF (PATH .EQ. 'OU') THEN
!           Process OUtput Pathway Cards                    ---   CALL OUCARD
         CALL OUCARD
      END IF

!        Store the Current Keyword as the Previous Keyword
      PKEYWD = KEYWRD

!        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
!        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
!        Marker That May Be Present For Some Editors.
      IF (PATH .EQ. 'OU' .and. KEYWRD .EQ. 'FINISHED') THEN
         GO TO 999
      END IF

      GO TO 11
999   EOF = .TRUE.
11    CONTINUE
   END DO

!     Reinitialize Line Number Counter to Count Meteorology Data
   ILINE = 0

!     Check That All Pathways Were Finished
   IF (ICSTAT(50).NE.1 .or. ISSTAT(50).NE.1 .or. IRSTAT(50).NE.1 .or.&
   &IMSTAT(50).NE.1 .or. IOSTAT(50).NE.1) THEN
!        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
      IFSTAT = ICSTAT(50)*10000 + ISSTAT(50)*1000 + IRSTAT(50)*100 +&
      &IMSTAT(50)*10 + IOSTAT(50)
      WRITE(DUMMY,'(I5.5)') IFSTAT
      CALL ERRHDL(PATH,MODNAM,'E','125',DUMMY)
! ---    Check for IFSTAT = 0, indicating an empty input file;
!        issue error message to 'aermod.out' file and abort
      IF (IFSTAT .EQ. 0) THEN
         WRITE(IOUNIT,9990)
9990     FORMAT(/1X,'All AERMOD input pathways missing! ',&
         &'Processing aborted!!!')
! ---       Also issue error message to "screen"
         WRITE(*,9990)
         STOP
      END IF
   END IF

! --- Check for non-DFAULT options for "optimized" area source,
!     FASTAREA, or for all source types, FASTALL; set MAXDIST = 80KM
!     if FASTALL or FASTAREA, otherwise MAXDIST = 1.0D20
   IF (FASTALL .or. FASTAREA) THEN
      MAXDIST = 8.0D04
   ELSE
      MAXDIST = 1.0D20
   END IF

   RETURN
END

SUBROUTINE LWRUPR
!***********************************************************************
!                 LWRUPR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Transfer All Characters From Lower Case To
!                 Upper Case (Using INDEX Intrinsic Function)
!                 Note that the CHAR*ISTRG RUNST1 Variable Includes
!                 the Original Case for Echoing and for Later Use
!                 To Retrieve Filenames.
!
!        PROGRAMMER: Roger Brode, Kevin Stroupe
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image (ISTRG Character Array)
!                 Number of Characters in String, PARAMETER ISTRG
!
!        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
!
!        CALLED FROM:   SETUP
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, INDCHK
   CHARACTER UPCASE*26
   CHARACTER LWCASE*26

!     Variable Initializations
   DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
   DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
   MODNAM = 'LWRUPR'
   INDCHK = 0

   DO I = 1, ISTRG
      IF (RUNST(I) .NE. ' ') THEN
         INDCHK = INDEX(LWCASE,RUNST(I))
         IF (INDCHK .NE. 0) THEN
            RUNST(I) = UPCASE(INDCHK:INDCHK)
         END IF
      END IF
   END DO

   RETURN
END

SUBROUTINE DEFINE
!***********************************************************************
!                 DEFINE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Defines Location of Fields on Runstream Input Image
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!*       Revision History:
!*
!*       MODIFIED: October 19, 2009
!*
!*                 Modified to recognize double quotes (") as
!*                 field delimiters to allow for filenames with
!*                 embedded spaces.
!
!        INPUTS:   Input Runstream Card Image
!
!        OUTPUTS:  Number of Fields on Card, IFC
!                  Beginning and Ending Columns of Fields, LOCB and LOCE
!
!        CALLED FROM:   SETUP
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   LOGICAL INQUOTE

   INTEGER :: I

!     Variable Initializations
   MODNAM = 'DEFINE'
   LOCB(3:IFMAX) = 0
   LOCE(3:IFMAX) = 0

!     Initialize the Blank Line and In-field Status Indicators
   BLINE = .TRUE.
   INFLD = .FALSE.
   INQUOTE = .FALSE.

   IF (ILINE .EQ. 1) THEN
!        Define the Starting Column for the Input File In Case File Is Shifted.
!        Allow for Shift of Up to 3 Columns
      LOCB(1) = 0
      IF (RUNST(1) .NE. ' ') THEN
         LOCB(1) = 1
      ELSE IF (RUNST(2) .NE. ' ') THEN
         LOCB(1) = 2
      ELSE IF (RUNST(3) .NE. ' ') THEN
         LOCB(1) = 3
      ELSE IF (RUNST(4) .NE. ' ') THEN
         LOCB(1) = 4
      ELSE
         LOCB(1) = 1
      END IF
      LOCE(1) = LOCB(1) + 1
      LOCB(2) = LOCB(1) + 3
      LOCE(2) = LOCB(1) + 10
   END IF

   IFC = 2

!     Check RUNST1 (full input record string) for Blank Line
   IF (LEN_TRIM(RUNST1) .GT. 0) THEN
      BLINE = .FALSE.
   ELSE
      RETURN
   END IF

!     Loop through the Data Fields
   DO I = LOCB(1)+12, ISTRG

      IF (.NOT.INFLD .and. RUNST(I).EQ.'"') THEN
!           Location is the Beginning of a Field using "'s
!           Set Mark of not Blank Line
         BLINE = .FALSE.
!           Set Mark of in a Field
         INFLD = .TRUE.
!           Set Mark of in a Quote Field
         INQUOTE = .TRUE.
!           Increment the Field Counter
         IFC = IFC + 1
!           Check for number of fields > IFMAX parameter
         IF (IFC .GT. IFMAX) THEN
!              WRITE Error Message: Too many fields specified
            WRITE(DUMMY,'(I8)') IFMAX
            CALL ERRHDL(PPATH,MODNAM,'E','109',DUMMY)
            EXIT
         END IF
!           Record the Location of Beginning of the Field
         LOCB(IFC) = I + 1
      ELSE IF (.NOT.INFLD .and. RUNST(I).NE.' ') THEN
!           Location is the Beginning of a Field
!           Set Mark of not Blank Line
         BLINE = .FALSE.
!           Set Mark of in a Field
         INFLD = .TRUE.
!           Increment the Field Counter
         IFC = IFC + 1
!           Check for number of fields > IFMAX parameter
         IF (IFC .GT. IFMAX) THEN
!              WRITE Error Message: Too many fields specified
            WRITE(DUMMY,'(I8)') IFMAX
            CALL ERRHDL(PPATH,MODNAM,'E','109',DUMMY)
            EXIT
         END IF
!           Record the Location of Beginning of the Field
         LOCB(IFC) = I
      ELSE IF (INQUOTE .and. RUNST(I).EQ.'"') THEN
!           Location is the End of a Field
!           Set Mark of Not In a field
         INFLD = .FALSE.
!           Set Mark of Not in a Quote Field
         INQUOTE = .FALSE.
!           Record the Location of Ending of the Field
         LOCE(IFC) = I - 1
      ELSE IF (.NOT.INQUOTE .and. INFLD .and. RUNST(I).EQ.' ') THEN
!           Location is the End of a Field
!           Set Mark of Not In a field
         INFLD = .FALSE.
!           Record the Location of Ending of the Field
         LOCE(IFC) = I - 1
      END IF

!        Check for End of Input String
!        (Length of ISTRG is Set as a PARAMETER in MAIN1)
      IF (INFLD .and. I.EQ.ISTRG) THEN
         LOCE(IFC) = ISTRG
      END IF

   END DO

   RETURN
END

SUBROUTINE GETFLD
!***********************************************************************
!                 GETFLD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Gets Contents of Fields on Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: Contents of Fields on Card
!
!        CALLED FROM:   SETUP
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, J
   CHARACTER WRTFRM*20

!     Variable Initializations
   MODNAM = 'GETFLD'
   FIELD(:) = ''

!     Setup WRITE format for internal write to FIELD
!     based on the ILEN_FLD PARAMETER (set in MAIN1)
   WRITE(WRTFRM,9004) ILEN_FLD
9004 FORMAT('(',I4.4,'(A1:))')

   DO I = 1, IFC
! ---    Skip processing of fields if IFC > IFMAX
      IF (I .GT. IFMAX) EXIT
      IF (LOCE(I)-LOCB(I) .LE. (ILEN_FLD - 1) ) THEN
!           Field Satisfies Limit of ILEN_FLD Characters (set in MAIN1)
         WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),LOCE(I))
      ELSE
!           Field Exceeds ILEN_FLD Character Limit
!           Truncate Field at ILEN_FLD Characters
         WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),&
         &LOCB(I)+ILEN_FLD-1)
      END IF
   END DO

   RETURN
END

SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
!***********************************************************************
!                 EXPATH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Extracts and Verifies Pathway ID from
!                 Runstream Input Card Image
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: The Extracted Pathway ID
!
!        CALLED FROM:   SETUP
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I
   CHARACTER (LEN=2), INTENT(IN) :: INPFLD
   CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
   INTEGER, INTENT(IN) :: IPN
   LOGICAL, INTENT(OUT) :: NOPATH

!     Variable Initializations
   NOPATH = .TRUE.
   MODNAM = 'EXPATH'

!     Begin The Processing
   IF (INPFLD .NE. '  ') THEN
!        Check the Read-in Pathway
      PATH = INPFLD
      DO I = 1, IPN
!           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
         IF (INPFLD .EQ. PATHWY(I)) THEN
            NOPATH = .FALSE.
            IPNUM = I
!              Exit to END
            GO TO 999
         END IF
      END DO
   ELSE
!        In Case of Blank Field Set Pathway to Previous Pathway
      NOPATH = .FALSE.
      PATH   = PPATH
      IPNUM  = IPPNUM
   END IF

999 RETURN
END

SUBROUTINE EXKEY(INPFLD,NOKEY)
!***********************************************************************
!                 EXKEY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Extracts and Verifies Keyword from
!                 Runstream Input Card Image
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: The Extracted Keyword
!
!        CALLED FROM:   SETUP
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I
   CHARACTER (LEN=8) :: INPFLD
   LOGICAL NOKEY

!     Variable Initializations
   NOKEY  = .TRUE.
   MODNAM = 'EXKEY'

!     Begin The Processing
   IF (INPFLD .NE. '        ') THEN
!        Check the Read-in Keyword
      KEYWRD = INPFLD
      DO I = 1, IKN
!           In Case of Match Set NOKEY to FALSE
         IF (INPFLD .EQ. KEYWD(I)) THEN
            NOKEY = .FALSE.
!              Exit to END
            GO TO 999
         END IF
      END DO
   ELSE IF (PKEYWD .NE. 'STARTING') THEN
!        In Case of Blank Field, Keyword Is Set to Previous Keyword
!        unless previous keyword is 'STARTING'
      NOKEY  = .FALSE.
      KEYWRD = PKEYWD
   ELSE
!        No Keyword is available; keyword field is blank and the
!        previous keyword is 'STARTING'
      NOKEY  = .TRUE.
      KEYWRD = 'blank   '
   END IF

999 RETURN
END

SUBROUTINE SETORD
!***********************************************************************
!                 SETORD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Check Run Stream Setup Images for Proper
!                 Order
!
!        MODIFIED:   To remove reference to obsolete TG pathway inherited
!                    from ISCST3 code.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To allow for skipping of TG pathway if no terrain
!                    grid is used.  Roger Brode, PES, Inc. - 11/7/94
!
!        INPUTS:  Input Runstream Card Image
!
!        OUTPUTS: Status Settings and Error Messages
!
!        CALLED FROM:   SETUP
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'SETORD'

   IF (KEYWRD .EQ. 'STARTING') THEN
      IF (ISTART .or. .NOT.IFINIS) THEN
!           WRITE Error Message: Starting Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','119',PATH)
      ELSE IF (IPNUM .NE. IPPNUM+1) THEN
!           WRITE Error Message: Pathway Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
      END IF
!        Set Starting Indicator
      ISTART = .TRUE.
!        Set Finished Indicator
      IFINIS = .FALSE.
   ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
      IF (IFINIS .or. .NOT.ISTART) THEN
!           WRITE Error Message: Finished Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','119',PATH)
      ELSE IF (ISTART .and. PATH.NE.PPATH) THEN
!           WRITE Warning Message: Pathway Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
      END IF
!        Reset Starting Indicator
      ISTART = .FALSE.
!        Set Finished Indicator
      IFINIS = .TRUE.
   ELSE IF (.NOT.ISTART .or. IFINIS) THEN
!        WRITE Error Message: Starting or Finished Out of Order
      CALL ERRHDL(PPATH,MODNAM,'E','119',PATH)
   ELSE IF (ISTART .and. PATH.NE.PPATH) THEN
!        WRITE Warning Message: Pathway Out of Order
      CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
   END IF

!     Save Current Path and Path Number as Previous Path and Number
   PPATH = PATH
   IPPNUM = IPNUM

   RETURN
END

SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
!***********************************************************************
!                 STONUM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Gets Number From A String Variable
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input String Variable
!                 Length of Character String
!
!        OUTPUTS: Numbers
!
!        CALLED FROM: (This Is A Utility Program)
!***********************************************************************
!
!     Variable Declarations
   IMPLICIT NONE

   CHARACTER STRVAR*(*), CHK, MODNAM*6, NUMS*10
   INTEGER :: I, IMUTI, LENGTH
   REAL    :: FNUM, CNUM, FDEC, FDC1, HEAD
   LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

!     Variable Initialization
   MODNAM = 'STONUM'
   NUMS = '0123456789'
   I = 1
   MEND = .FALSE.
   IN = .FALSE.
   NMARK = .FALSE.
   PMARK = .FALSE.
   DMARK = .FALSE.
   MMARK = .FALSE.
   EMARK = .FALSE.
   CNUM  = 0.0
   HEAD  = 0.0
   IMUTI = 1
   FDEC  = 1.

!     Beginning the Processing
   DO WHILE (.NOT.MEND .and. I.LE.LENGTH)
      CHK = STRVAR(I:I)
      IF (CHK .NE. ' ') THEN
         IN = .TRUE.
         IF (CHK.GE.'0' .and. CHK.LE.'9') THEN
!              CHK is a Number, Assign a Value
            IF (.NOT. DMARK) THEN
               CNUM = CNUM*10.+FLOAT(INDEX(NUMS,CHK)-1)
            ELSE
               FDEC = FDEC/10.
               FDC1 = FDEC*FLOAT(INDEX(NUMS,CHK)-1)
               CNUM = CNUM+FDC1
            END IF
         ELSE
!              Handle The E-Type Real Number
            IF (DMARK .and. .NOT.EMARK .and. CHK.EQ.'E') THEN
               EMARK = .TRUE.
               IF (.NOT.NMARK) THEN
                  HEAD = CNUM
               ELSE
                  HEAD = -CNUM
               END IF
               DMARK = .FALSE.
               NMARK = .FALSE.
               CNUM = 0.0
            ELSE IF (.NOT.PMARK .and. CHK.EQ.'+') THEN
!                 Set Positive Indicator
               PMARK = .TRUE.
            ELSE IF (.NOT.NMARK .and. CHK.EQ.'-') THEN
!                 Set Negative Indicator
               NMARK = .TRUE.
            ELSE IF (.NOT.DMARK .and. CHK.EQ.'.') THEN
!                 Set Decimal Indicator
               DMARK = .TRUE.
            ELSE IF (.NOT.MMARK .and. CHK.EQ.'*' .and.&
            &.NOT.NMARK) THEN
!                 Set Repeat Number
               MMARK = .TRUE.
               IMUTI = NINT(CNUM)
               CNUM = 0.0
            ELSE
!                 Error Occurs, Set Switch and Exit Out Of The Subroutine
               GO TO 9999
            END IF
         END IF
      ELSE IF (IN .and. CHK.EQ.' ') THEN
         MEND = .TRUE.
      END IF
      I = I + 1
   END DO

   FNUM = CNUM

!     In Case Of Negative Field, Value Set to Negative
   IF (NMARK) THEN
      FNUM = -FNUM
   END IF

!     In Case of E-Format, Check for Exponents Out of Range
   IF (EMARK .and. ABS(FNUM) .LE. 30.) THEN
      FNUM = HEAD*10.0**(FNUM)
   ELSE IF (EMARK .and. ABS(FNUM) .GT. 30.) THEN
      IF (FNUM .LT. 0.0) THEN
         FNUM = 0.0
      ELSE IF (FNUM .GT. 0.0) THEN
         FNUM = HEAD * 10.**30.
      END IF
      GO TO 9999
   END IF

   GO TO 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 IMUTI = -1

1000 RETURN
END

SUBROUTINE STODBL(STRVAR,LEN,FNUM,IMUTI)
!***********************************************************************
!                 Subroutine STODBL
!
!        PURPOSE: Gets Double Precision of Real Number
!                 From A Character String
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To Change Exponent Limit for Out-of-range
!                    Inputs - 9/29/92
!
!        INPUTS:  Input String Variable
!                 Length of Character String
!
!        OUTPUTS: Double Precision Real Numbers
!
!        CALLED FROM: (This Is A Utility Program)
!***********************************************************************
!
!     Variable Declarations
   IMPLICIT NONE

   CHARACTER STRVAR*(*), CHK, MODNAM*6, NUMS*10
   INTEGER :: IMUTI, LEN, I
   DOUBLE PRECISION :: FDEC, FDC1, HEAD
   DOUBLE PRECISION :: FNUM, CNUM
   LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

!     Variable Initialization
   MODNAM = 'STODBL'
   NUMS = '0123456789'
   I = 1
   MEND = .FALSE.
   IN = .FALSE.
   NMARK = .FALSE.
   PMARK = .FALSE.
   DMARK = .FALSE.
   MMARK = .FALSE.
   EMARK = .FALSE.
   CNUM = 0.0D0
   HEAD = 0.0D0
   FDEC = 1.0D0
   IMUTI = 1

!     Beginning the Processing
   DO WHILE (.NOT.MEND .and. I.LE.LEN)
      CHK = STRVAR(I:I)
      IF (CHK .NE. ' ') THEN
         IN = .TRUE.
         IF (CHK.GE.'0' .and. CHK.LE.'9') THEN
!              CHK is a Number, Assign a Value
            IF (.NOT. DMARK) THEN
               CNUM = CNUM*10.0D0+DBLE(INDEX(NUMS,CHK)-1)
            ELSE
               FDEC = FDEC/10.0D0
               FDC1 = FDEC*DBLE(INDEX(NUMS,CHK)-1)
               CNUM = CNUM+FDC1
            END IF
         ELSE
!              Handle The E-Type (or D-Type) Real Number
            IF ((DMARK .and. .NOT.EMARK .and. CHK.EQ.'E') .or.&
            &(DMARK .and. .NOT.EMARK .and. CHK.EQ.'D')) THEN
               EMARK = .TRUE.
               IF (.NOT.NMARK) THEN
                  HEAD = CNUM
               ELSE
                  HEAD = -CNUM
               END IF
               DMARK = .FALSE.
               NMARK = .FALSE.
               CNUM = 0.0D0
            ELSE IF (.NOT.PMARK .and. CHK.EQ.'+') THEN
!                 Set Positive Indicator
               PMARK = .TRUE.
            ELSE IF (.NOT.NMARK .and. CHK.EQ.'-') THEN
!                 Set Negative Indicator
               NMARK = .TRUE.
            ELSE IF (.NOT.DMARK .and. CHK.EQ.'.') THEN
!                 Set Decimal Indicator
               DMARK = .TRUE.
            ELSE IF (.NOT.MMARK .and. CHK.EQ.'*' .and.&
            &.NOT.NMARK) THEN
!                 Set Repeat Indicator
               MMARK = .TRUE.
               IMUTI = IDNINT(CNUM)
               CNUM = 0.0D0
            ELSE
!                 Error Occurs, Set Switch and Exit Out Of The Subroutine
               GO TO 9999
            END IF
         END IF
      ELSE IF (IN .and. CHK.EQ.' ') THEN
         MEND = .TRUE.
      END IF
      I = I + 1
   END DO

   FNUM = CNUM

!     In Case Of Negative Field, Value set to Negative
   IF (NMARK) THEN
      FNUM = -FNUM
   END IF

!     In Case of *E* Format, Check for Exponents Out of Range
   IF (EMARK .and. DABS(FNUM) .LE. 30.0D0) THEN
      FNUM = HEAD*10.0D0**(FNUM)
   ELSE IF (EMARK .and. DABS(FNUM) .GT. 30.0D0) THEN
      IF (FNUM .LT. 0.0D0) THEN
         FNUM = 0.0D0
      ELSE IF (FNUM .GT. 0.0D0) THEN
         FNUM = HEAD * 10.0D0**30.0D0
      END IF
      GO TO 9999
   END IF

   GO TO 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 IMUTI = -1

1000 RETURN
END

SUBROUTINE SINDEX(ARRIN,IDIM,ELEM,INDEXS,FOUND)
!***********************************************************************
!                 SINDEX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Search The Index of An Input Array Element
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Character Element
!
!        OUTPUTS: Index Of This Element in An Array
!
!        CALLED FROM:  (This Is An Utility Programm)
!***********************************************************************
!
!     Variable Declarations
   IMPLICIT NONE

   INTEGER :: I, IDIM, INDEXS
   CHARACTER (LEN=*) :: ARRIN(IDIM), ELEM
   CHARACTER MODNAM*6
   LOGICAL FOUND

!     Variable Initializations
   MODNAM = 'SINDEX'
   FOUND = .FALSE.
   I = 1
   INDEXS = 0

   DO WHILE (.NOT.FOUND .and. I.LE.IDIM)
      IF (ELEM .EQ. ARRIN(I)) THEN
         FOUND = .TRUE.
         INDEXS = I
      END IF
      I = I + 1
   END DO

   RETURN
END

SUBROUTINE FSPLIT(PATHIN,KEYIN,INPFLD,LENGTH,DELIM,LFLAG,&
&BEGFLD,ENDFLD)
!***********************************************************************
!                 FSPLIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: SPLIT A FIELD, BASED ON AN INPUT DELIMITER
!                 CHARACTER.  SETS A LOGICAL FLAG AND RETURNS
!                 BEGINNING AND ENDING PARTS OF FIELD.
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Pathway for Calling Routine
!                 Keyword for Calling Routine
!                 Input Field Variable
!                 Length of Input Character Field
!                 Delimiter Character
!
!        OUTPUTS: Logical Flag to Indicate Presence of Delimiter
!                 Beginning Part of Field (.LE. 12 Character)
!                 Ending Part of Field (.LE. 12 Character)
!
!        CALLED FROM: (This Is A Utility Program)
!***********************************************************************

!     Variable Declarations
   IMPLICIT NONE

   INTEGER :: I, LENGTH, IDELM
   CHARACTER CHK, INPFLD*(*), BEGFLD*(*), ENDFLD*(*),&
   &DELIM*1, MODNAM*12, PATHIN*2, KEYIN*8
   LOGICAL LFLAG, MEND, IN

!     Variable Initialization
   MODNAM = 'FSPLIT'
   I = LENGTH
   IDELM = LENGTH
   BEGFLD = ' '
   ENDFLD = ' '
   MEND  = .FALSE.
   IN    = .FALSE.
   LFLAG = .FALSE.

!     Begin the Processing
   DO WHILE (.NOT.MEND .and. I.GE.1)
      CHK = INPFLD(I:I)
      IF (CHK .NE. ' ') THEN
         IN = .TRUE.
!           Check for the Group Delimiter
         IF (.NOT.LFLAG .and. CHK.EQ.DELIM) THEN
            LFLAG = .TRUE.
            IDELM = I
            ENDFLD = INPFLD(I+1:LENGTH)
            IF (I .EQ. 1) THEN
!                 Write Error Message for Invalid Range Parameter
               CALL ERRHDL(PATHIN,MODNAM,'E','203',KEYIN)
               GO TO 999
            END IF
         ELSE IF (LFLAG .and. CHK.EQ.DELIM) THEN
!              WRITE Error Message  ! More Than One Delimiter in a Field
            CALL ERRHDL(PATHIN,MODNAM,'E','217',KEYIN)
         END IF
      ELSE IF (IN .and. CHK.EQ.' ') THEN
         MEND = .TRUE.
         IF (LFLAG) THEN
            BEGFLD = INPFLD(1:IDELM-1)
         ELSE
            BEGFLD = INPFLD
         END IF
      END IF
      I = I - 1
   END DO

   IF (.NOT. MEND) THEN
      IF (LFLAG) THEN
         BEGFLD = INPFLD(1:IDELM-1)
      ELSE
         BEGFLD = INPFLD
      END IF
   END IF

!     In Case Of No Delimiter, Set ENDFLD = BEGFLD
   IF (.NOT. LFLAG) THEN
      ENDFLD = BEGFLD
   END IF

999 RETURN
END

SUBROUTINE VARINI
!***********************************************************************
!                 VARINI Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        V. Tino
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: To Initialize Variables for Setup
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION, INT.
!                                        TERRAIN, and GRIDDED TERRAIN
!                                        Processing)
!
!        DATE:    December 15, 1993
!
!        MODIFIED:  To add the Aircraft related parameters (Aircraft Plume Rise)
!                   Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                   04/01/2023
!
!        MODIFIED:  To remove reference to obsolete TG pathway inherited
!                   from ISCST3 code.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:  To initialize DDPLETE and WDPLETE to .TRUE. for
!                   dry and wet depletion.
!                   R.W. Brode, MACTEC, Inc. (f/k/a PES, Inc.) - 10/26/04
!
!        MODIFIED:  To use two decimal places for reading temperatures
!                   from the profile file in the default format (PROFRM)
!                   R.W. Brode, PES, Inc. - 8/28/01
!
!        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  None
!
!        OUTPUTS: Initialized Variables
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE

   IMPLICIT NONE
   CHARACTER MODNAM*12

! Unused: INTEGER :: I, J, K

!     Variable Initializations
   MODNAM = 'VARINI'

! --- Initialize double precision constants based on PI
   PI      = 4.0D0*DATAN(1.0D0)
   TWOPI   = 2.0D0*PI
   RTOFPI  = DSQRT(PI)
   SRT2PI  = DSQRT(TWOPI)
   RTOF2   = DSQRT(2.0D0)
   RTPIBY2 = DSQRT(PI/2.0D0)
   RT2BYPI = DSQRT(2.0D0/PI)
   DTORAD  = PI/180.0D0
   RTODEG  = 180.0D0/PI

! --- Initialize constant for 1/3 used as exponent
   THIRD = 1.0D0/3.0D0
   TWOTHIRDS = 2.0D0/3.0D0

! --- Initialize counters to zero
   IPNUM  = 0
   IPPNUM = 0
   NDUMP  = 0
! --- Initialize counter for old met data warning message
   IMETMSG = 0

! --- Initialize pollutant ID, POLLUT*8
   POLLUT = 'undefine'

!     Initialize the Logical Control Variables
   ISTART = .FALSE.
   IFINIS = .TRUE.
   ERRLST = .FALSE.
   DFAULT = .FALSE.
   CONC   = .FALSE.
   DEPOS  = .FALSE.
!     Add logicals to output just wet or just dry deposition fluxes
   DDEP   = .FALSE.
   WDEP   = .FALSE.
   RURAL  = .FALSE.
   URBAN  = .FALSE.
   GRDRIS = .FALSE.
   NOSTD  = .FALSE.
   NOBID  = .FALSE.
   NOWARN = .FALSE.
   MSGPRO = .TRUE.
   CLMPRO = .TRUE.
   PERIOD = .FALSE.
   ANNUAL = .FALSE.
   MONTH  = .FALSE.
   FLAT   = .FALSE.
   ELEV   = .TRUE.
   FLGPOL = .FALSE.
   RUN    = .FALSE.
   EVENTS = .FALSE.
   RSTSAV = .FALSE.
   RSTINP = .FALSE.
   MULTYR = .FALSE.
   DAYTAB = .FALSE.
   MXFILE = .FALSE.
   PPFILE = .FALSE.
   PLFILE = .FALSE.
   SUMMFILE = .FALSE.
   ARCFT  = .FALSE.                                                 !  Added for Aircraft; UNC-IE
!     Add TXFILE Variable for the TOXXFILE Option, 9/29/92
   TXFILE = .FALSE.
   RKFILE = .FALSE.
   ANPOST = .FALSE.
   ANPLOT = .FALSE.
   RECERR = .FALSE.
   PFLERR = .FALSE.
   IF (ALLOCATED(L_MorningTrans)) L_MorningTrans(:) = .FALSE.
   L_UrbanTransition = .TRUE.
   ENDMON = .FALSE.
   CALCS  = .FALSE.
   SURFAC = .FALSE.
   DEBUG  = .FALSE.
   METEORDBG = .FALSE.
   AREADBG  = .FALSE.
   PRIMEDBG = .FALSE.
   OLMDEBUG = .FALSE.
   ARM2DEBUG= .FALSE.
   PVMRMDBG = .FALSE.
   GRSMDEBUG = .FALSE.
   DEPOSDBG = .FALSE.
!CRT  D063 Platform Downwash Debug
   PLATFMDBG  = .FALSE.
   WAKE   = .FALSE.
   ECHO   = .TRUE.
   SCREEN = .FALSE.
   HOURLY = .FALSE.
   ARCFTDEBUG = .FALSE.                                              ! Added for Aircraft; UNC-IE
   L_BLHOURLY = .FALSE.                                              ! Multiple_BuoyLines_D41_Wood

!     JAT 05/08/20 ADDED FROM 19191
!     Initialized AWMADWDBG to false
   AWMADWDBG=.FALSE.

!     Add logicals to identify use wet and dry removal information
   LDPART  = .FALSE.
   LWPART  = .FALSE.
   LWGAS   = .FALSE.
   LDGAS   = .FALSE.
!     Add logicals to control use of Wet & Dry plume depletion
!     Initialize dry and wet depletion to .F. for now, but if
!     deposition algorithms are used then depletion will be assumed
   DDPLETE   = .FALSE.
   WDPLETE   = .FALSE.
   ARDPLETE  = .FALSE.
!     Initialize logicals for user-specified depletion options
   WETDPLT   = .FALSE.
   DRYDPLT   = .FALSE.
!     Initialize logicals for user-specified override of depletion
   NOWETDPLT = .FALSE.
   NODRYDPLT = .FALSE.

! --- Initialize EVENT processing output options
   SOCONT = .FALSE.
   DETAIL = .FALSE.

! --- Initialize logical for use of vector mean wind speeds, L_VECTORWS
   L_VECTORWS = .FALSE.

! --- Initialize logical for use of ALPHA option for low-wind-speed
!     modifications.
!CRT 3/23/2021 D061/D062: in aermod.f
!CRT  This resets to false after being set to true in aermod.f
!CRT  not needed here
!CRT      LOW_WIND = .FALSE.

!CRT  4/12/2022 Initialize logical variables for LOW_WIND options
   L_UserSVmin = .FALSE.
   L_UserWSmin = .FALSE.
   L_UserFRANmax = .FALSE.
   L_UserSWmin = .FALSE.
   L_UserBigT = .FALSE.
   L_UserFRANmin = .FALSE.
   L_PBal = .FALSE.

!     Added for TTRM; AECOM
!     Initialize logicals for ozone response rate (TTRM)
   RUNTTRM = .FALSE.
   TTRMDBG = .FALSE.
   RUNTTRM2 = .FALSE.
   TTRM2DBG = .FALSE.
!     End TTRM insert, Nov. 2021
! Added for HBP & HBPDEBUG; Jan. 2023
!     Initialize logicals for running HBP and HBP debug file
   HBPLUME = .FALSE.
   HBPDBG = .FALSE.
! End insert for HBP & HBPDBG
   URBDBUG = .FALSE.
   BLPDBUG = .FALSE.
! --- Initialize logicals to indicate various met data options used,
!     including the 'ADJ_U*' and 'BULKRN' options in AERMET, and the
!     use of MMIF-generated met inputs directly from MMIF or processed
!     through AERMET; L_MMIF_Data indicates that MMIF-generated data
!     have been used based on information in SURFACE file header record;
!     L_MMIF_Profile is used to flag potential use of MMIF-generated
!     data based on profile heights exceeding 999m, absent information
!     in the SURFACE file header record.
!     8/9/2023, CRT - D176 COARE BETA Check (L_COARE)
   L_AdjUstar     = .FALSE.
   L_BULKRN       = .FALSE.
   L_MMIF_Data    = .FALSE.
   L_MMIF_Profile = .FALSE.
   L_TurbData     = .FALSE.
   L_COARE        = .FALSE.

!     Add logical to control user-specified deposition velocity for gases
   LUSERVD  = .FALSE.
   SCIM     = .FALSE.
   SCIMOUT  = .FALSE.
   SCIMHR   = .FALSE.
   SEASONHR = .FALSE.
   BETA     = .FALSE.

! --- Initialize logicals for processing design values based on ranked
!     values averaged across years, 24-hr PM2.5, 1-hr NO2 and 1-hr SO2
   PM25AVE = .FALSE.
   NO2AVE  = .FALSE.
   SO2AVE  = .FALSE.
   L_NO_PM25AVE = .FALSE.
   L_NO_NO2AVE  = .FALSE.
   L_NO_SO2AVE  = .FALSE.

! --- Initialize logicals for background ozone, NOx and concentrations
   L_O3File(:)   = .FALSE.
   L_O3Hourly    = .FALSE.
   L_O3VAL(:)    = .FALSE.
   L_O3VALUES(:) = .FALSE.
   L_BACKGRND    = .FALSE.
   L_BGHourly    = .FALSE.
   L_BGFile(:)   = .FALSE.
   L_BGValues(:) = .FALSE.
   L_NOXVALUE(:) = .FALSE.
   L_NOX_VALS(:) = .FALSE.
   L_NOxHourly   = .FALSE.
   L_NOxFile(:)  = .FALSE.
   L_CalcNOXFromNO2 = .FALSE.

! --- Initialize array index counter for temporally-varying background
!     concentrations (other than hourly file) to zero
   IBKGRD(:) = 0
! --- Initialize logical for day-of-week options
   L_DayOfWeekOpts  = .FALSE.

!     Add logical for MAXDAILY output file option
   MXDAILY  = .FALSE.
   L_MAXDCONT = .FALSE.
   MXDAILY_BYYR  = .FALSE.
!     Initialize variable to skip messages during MAXDCONT processing
   L_SkipMessages = .FALSE.

! --- Set logical flag for whether "New" met data are being used, based
!     on version 11059 or later of AERMET, using the wind data source
!     and adjustment flags introduced with version 11059 ('NAD' or 'ADJ')
   L_NAD_ADJ_Flags = .FALSE.
! --- Set logical flag for use of "old" met data, i.e., v06341 of AERMET,
!     which is allowed for now, with a warning message
   L_OldMetVer = .FALSE.

!     Non-DFAULT option for optimized area source, formerly controlled by
!     TOXICS option, which is now obsolete
   FASTAREA  = .FALSE.
!     Non-DFAULT option for optimized meander for POINT and VOLUME sources based
!     on effective sigma-y; also activates the FASTAREA option
   FASTALL   = .FALSE.
!     Logical flag for optimized meander for POINT and VOLUME sources based
!     on effective sigma-y under non-DFAULT FASTALL option
   L_EFFSIGY = .FALSE.

! --- Logical flag to indicate whether non-DFAULT options are specified
!     when DFAULT is not specified. This is used to set the option label
!     in the file headers.
   L_NonDFAULT = .FALSE.

! --- Initialize options for checking date sequence in meteorological data
   NOCHKD     = .FALSE.
   L_WARNCHKD = .FALSE.

! --- Initialize logical variables used to flag EVENT or MAXDCONT inconsistency
!     warnings in order to issue a warning to the default output unit at the
!     end of the run.
   L_EVENT_OrigConc_Warning    = .FALSE.
   L_MAXDCONT_OrigConc_Warning = .FALSE.

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     INITIALIZE TURBOPTS TO FALSE
   TURBOPTS=.FALSE.
!*----
!*#

! --- Initialize variable for specifying format for file outputs;
!     Value has been "preset" under subroutine PRESET in order for the
!     postfile format to be adjusted before writing header recordes in sub_OUPOST;
!     default = 'FIX' for fixed-format outputs; 'EXP' indicates user specified
!     use of exponential-format outputs.  This variable applies to
!     MAXIFILE, PLOTFILE, POSTFILE (plot-formatted), RANKFILE, and SEASONHR outputs
   IF (FILE_FORMAT .NE. 'EXP') THEN
      FILE_FORMAT = 'FIX'
   END IF

! --- Initialize REELEV character variable to 'METERS' for default elevation units
   REELEV = 'METERS'
! --- Initialize SOELEV character variable to 'METERS' for default elevation units
   SOELEV = 'METERS'

!     Initialize Decay Coefficient to 0.0 (Urban SO2 Default Set in POLLUT)
   DECOEF = 0.0D0

!     Initialize variables to hold two previous hours of precipitation
   PREC1  = 0.0D0
   PREC2  = 0.0D0
   IF (ALLOCATED(APREC1)) APREC1 = 0.0D0
   IF (ALLOCATED(APREC2)) APREC2 = 0.0D0
! --- Initialize variable for total precipitation
   TOTAL_PRECIP = 0.0D0

!     Initialize variables used to calculate canopy stomatal resistance, Rs
   Wold = 180.0D0
   f2   = Wold/200.0D0

!     Initialize defaults for Fo, FSEAS2, and FSEAS5 (may be input by user
!     on the CO GASDEPDF card).
   Fo     = 0.0D0
   FSEAS2 = 0.5D0
   FSEAS5 = 0.25D0

!     Initialize the Source Arrays
   ISRC = 0
   AXS(:) = 0.0D0
   AYS(:) = 0.0D0
   AZS(:) = 0.0D0
   AQS(:) = 0.0D0
   AHS(:) = 0.0D0
   ADS(:) = 0.0D0
   AVS(:) = 0.0D0
   ATS(:) = 0.0D0
   ASYINI(:) = 0.0D0
   ASZINI(:) = 0.0D0
!**  Added for Aircraft Plume Rise; UNC-IE
   AMFUEL(:)  = 0.0D0
   ATHRUST(:) = 0.0D0
   AVAA(:)    = 0.0D0
   AAFR(:)    = 0.0D0
   ABYPR(:)   = 0.0D0
   ARPWR(:)   = 0.0D0
   ASRCANGLE(:) = 0.0D0
!**  End Aircraft Plume Rise insert; April 2023
   IF (ALLOCATED(AXINIT)) AXINIT(:) = 0.0D0
   IF (ALLOCATED(AYINIT)) AYINIT(:) = 0.0D0
   IF (ALLOCATED(AANGLE)) AANGLE(:) = 0.0D0
   IF (ALLOCATED(RADIUS)) RADIUS(:) = 0.0D0
   IF (ALLOCATED(AXCNTR)) AXCNTR(:) = 0.0D0
   IF (ALLOCATED(AYCNTR)) AYCNTR(:) = 0.0D0
   IF (ALLOCATED(AALPHA)) AALPHA(:) = 0.0D0
   IF (ALLOCATED(APDEFF)) APDEFF(:) = 0.0D0
   IF (ALLOCATED(AVOLUM)) AVOLUM(:) = 0.0D0
   SOPCRD(:) = 'N'
   SOGAS(:)  = 'N'
   URBSRC(:) = 'N'
   AFTSRC(:) = 'N'                                                  !  Added for Aircraft; UNC-IE
   SRCID(:)  = ' '
   SRCTYP(:) = ' '
   QFLAG(:)  = ' '
   INPD(:)   = 0
   IF (ALLOCATED(NVERTS)) NVERTS(:) = 0
   IELUNT(:) = 0
   EVAL(:) = .FALSE.
   EVLFIL(:) = ' '
   L_FLATSRC(:) = .FALSE.
   L_HRLYSIG(:) = .FALSE.
   GRP_BACK(:)  = .FALSE.
   L_WakeMessage(:) = .FALSE.

! --- 1/20/2012, CRT: D063 Intialize platform parameters
   OSPLAT(:) = .FALSE.  ! Source subject to platform downwash
   PLATELV(:) = 0.0D0   ! Platform base elev. above ocean surface
   PLATHB(:) = 0.0D0    ! Plaform building height above platform base
   PLATWB(:) = 0.0D0    ! Platform building width

! --- Initialize BFLAG for BACKGRND concentrations
   BFLAG(:) = ' '

!     Add gas dry deposition parameters
   IF (ALLOCATED(PDIFF))      PDIFF(:)  = 0.0D0
   IF (ALLOCATED(PDIFFW))     PDIFFW(:) = 0.0D0
   IF (ALLOCATED(ALPHAS))     ALPHAS(:) = 0.0D0
   IF (ALLOCATED(REACT))      REACT(:)  = 0.0D0
   IF (ALLOCATED(HENRY))      HENRY(:)  = 0.0D0
   IF (ALLOCATED(RCLI))       RCLI(:)   = 0.0D0
   IF (ALLOCATED(L_METHOD2))  L_METHOD2(:) = .FALSE.

   IF (ALLOCATED(ADSBH))   ADSBH(:,:)   = 0.0D0
   IF (ALLOCATED(ADSBW))   ADSBW(:,:)   = 0.0D0
   IF (ALLOCATED(ADSBL))   ADSBL(:,:)   = 0.0D0
   IF (ALLOCATED(ADSXADJ)) ADSXADJ(:,:) = 0.0D0
   IF (ALLOCATED(ADSYADJ)) ADSYADJ(:,:) = 0.0D0
   IF (ALLOCATED(AXVERT))  AXVERT(:,:)  = 0.0D0
   IF (ALLOCATED(AYVERT))  AYVERT(:,:)  = 0.0D0

   IF (ALLOCATED(QFACT))   QFACT(:,:)   = 0.0D0

! --- Initialize NUMO3Sects, NUMBGSects and NUMNOxSects to 1
   IF (.NOT. L_O3Sector) NUMO3Sects = 1
   IF (.NOT. L_BGSector) NUMBGSects = 1
   IF (.NOT. L_NOxSector) NUMNOxSects = 1
! --- Initialize counter for number of missing BGHrVal substitutions
   NSubBGHOUR = 0

! --- Initialize O3VARY and NOXVARY arrays across all sectors to -99.0 to facilitate QA
!     checks on completeness of the inputs
   IF (ALLOCATED(O3VARY))  O3VARY(:,:) = -99.0D0
   IF (ALLOCATED(NOXVARY))  NOXVARY(:,:) = -99.0D0

   O3FLAG(:) = ''
   BFLAG(:)  = ''
   NOXFLAG(:) = ''
   IO3MAX(:) = 0
   IBGMAX(:) = 0
   INOXMAX(:) = 0

   IF (ALLOCATED(BACKGRND)) BACKGRND(:,:) = 0.0D0
   IF (ALLOCATED(BACKAVE)) BACKAVE(:) = 0.0D0
   IF (ALLOCATED(IGROUP)) IGROUP(:,:) = 0
   IF (ALLOCATED(CHI)) CHI(:,:,:) = 0.0D0
   IF(GRSM)THEN
      CHI_TTRAVPLM = 0.0D0
      CHI_TTRAVPAN = 0.0D0
      CHI_TTRAVAER = 0.0D0
      CHI_TTRAVPRM = 0.0D0
      CHI_TTRAVCHM(:,:) = 0.0D0
      BLDFAC(:,:) = 0.0D0
      TTRAVCHM(:) = 0.0D0
      PRMVAL_Src1 = 0.0D0
   END IF
   IF (OLM) THEN
      OLMID(:) = ' '
      IGRP_OLM(:,:) = 0
      L_OLMGRP(:) = .FALSE.
   END IF
   IF (PVMRM .or. OLM .or. GRSM) THEN
      ANO2_RATIO(:) = -9.0D0
      IF (PSDCREDIT) THEN
         PSDSRCTYP(:)  = '  '
         L_PSDGRP(:)   = .FALSE.
         IGRP_PSD(:,:) = 0
      END IF
      IF (PVMRM .or. GRSM) THEN
         PPFACT(:,:)  = 0.0D0
         HECNTR(:,:)  = 0.0D0
         HECNTR3(:,:) = 0.0D0
         UEFFS(:,:)   = 0.0D0
         UEFF3S(:,:)  = 0.0D0
         EPSEF(:,:)   = 0.0D0
         EPSEF3(:,:)  = 0.0D0
         FOPTS(:,:)   = 0.0D0
      END IF
   END IF

   IF (PSDCREDIT) THEN
      PSDID(:) = ' '
      ABVAL(:,:) = 0.0D0
      BCVAL(:,:) = 0.0D0
   END IF

! Added for TTRM; Nov. 2021
   IF (RUNTTRM) THEN
      TTRMSRC(:,:) = ' '
   END IF
! End of TTRM insert

!     Initialize arrays for deposition; first check if ALLOCATED
   IF (ALLOCATED(APDIAM))   APDIAM   = 0.0D0
   IF (ALLOCATED(APHI))     APHI     = 0.0D0
   IF (ALLOCATED(APDENS))   APDENS   = 0.0D0
   IF (ALLOCATED(AVGRAV))   AVGRAV   = 0.0D0
   IF (ALLOCATED(ATSTOP))   ATSTOP   = 0.0D0
   IF (ALLOCATED(EFRAC))    EFRAC    = 0.0D0
   IF (ALLOCATED(QPART))    QPART    = 0.0D0
   IF (ALLOCATED(PDIAM))    PDIAM    = 0.0D0
   IF (ALLOCATED(PHI))      PHI      = 0.0D0
   IF (ALLOCATED(PDENS))    PDENS    = 0.0D0
   IF (ALLOCATED(VGRAV))    VGRAV    = 0.0D0
   IF (ALLOCATED(TSTOP))    TSTOP    = 0.0D0
   IF (ALLOCATED(SCHMIDT))  SCHMIDT  = 0.0D0
   IF (ALLOCATED(VDEP))     VDEP     = 0.0D0
   IF (ALLOCATED(SCF))      SCF      = 0.0D0
   IF (ALLOCATED(PSCVRT))   PSCVRT   = 0.0D0
   IF (ALLOCATED(WASHOUT))  WASHOUT  = 0.0D0
   IF (ALLOCATED(ECOLL))    ECOLL    = 0.0D0
   IF (ALLOCATED(finemass)) finemass = 0.0D0

!     Initialize URBAN arrays
   IF (ALLOCATED(IURBGRP))   IURBGRP   = 0
   IF (ALLOCATED(URBID))     URBID     = ' '
   IF (ALLOCATED(URBNAM))    URBNAM    = ' '
   IF (ALLOCATED(URBPOP))    URBPOP    = 0.0D0
   IF (ALLOCATED(URBZ0))     URBZ0     = 0.0D0
   IF (ALLOCATED(ZIURB))     ZIURB     = 0.0D0
   IF (ALLOCATED(URBWSTR))   URBWSTR   = 0.0D0
   IF (ALLOCATED(URBUSTR))   URBUSTR   = 0.0D0
   IF (ALLOCATED(URBOBULEN)) URBOBULEN = 0.0D0
   IF (ALLOCATED(GRDSWU))    GRDSWU    = 0.0D0
   IF (ALLOCATED(GRDSVU))    GRDSVU    = 0.0D0
   IF (ALLOCATED(GRDTGU))    GRDTGU    = 0.0D0
   IF (ALLOCATED(GRDPTU))    GRDPTU    = 0.0D0

!**  Added for Airport ID for Aircraft Sources; UNC-IE
   IF (ALLOCATED(AFTID)) AFTID  = ' '
!**  End Aircraft Plume Rise insert; April 2023

!     Initialize source depletion factors to unity.
   DQCORG = 1.0D0
   WQCORG = 1.0D0
   IF (ALLOCATED(WQCOR)) WQCOR = 1.0D0
   IF (ALLOCATED(DQCOR)) DQCOR = 1.0D0

!     Counters for the Receptor Groups
   IREC = 0
   ISTA = .FALSE.
   IEND = .FALSE.
   IRXR = 0
   IRYR = 0
   IRZE = 0
   IRZH = 0
   IRZF = 0
   NEWID = .TRUE.
!     Initialize ITAB, NXTOX, NYTOX Variables for the TOXXFILE Option, 9/29/92
   ITAB  = -9
   NXTOX = 0
   NYTOX = 0

!     Initialize Variables Associated with the Meteorology Data
   ISJDAY = 0
   IEJDAY = 366
   ISDATE = 0
!     Initialize End Date as largest valid date for a 4-byte integer.
   IEDATE = 2147123124
   ISYR   = 0
   ISMN   = 0
   ISDY   = 0
   ISHR   = 0
   IEYR   = 9999
   IEMN   = 99
   IEDY   = 99
   IEHR   = 24
   IPDATE = 0
   IPHOUR = 0
   IMONTH = 0
   IDAY   = 0
   IHOUR  = 0
   IYEAR  = 0
   NUMYRS = 0
   NREMAIN= 0
   IHR_NDX= 0
   IYR_NDX= 0
   INCRST = 1
   SFX = 0.0D0
   SFY = 0.0D0
   UAX = 0.0D0
   UAY = 0.0D0
   ROTANG = 0.0D0
   O3MISS = .FALSE.
   O3BACK = -999.0D0
   O3CONC = -999.0D0
   NO2Equil = 0.90D0
!      NO2Stack = 0.10D0 !D040 remove default NO2Stack value WSP 2/3/23
   NO2Stack = -9.0D0  ! D040 CRT 4/10/2023 initialize to a negative value outside of acceptable range.
   IOLM = 0
   NOXMISS = .FALSE.
   NOXBACK = -999.0D0
   NOXBGCONC = -999.0D0
   ITTRM = 0

!     Initialize Met Data Filenames and Formats
   METINP = ' '
   PROINP = ' '
   METFRM = 'FREE'
   PROFRM = 'FREE'

! --- Initialize EVENT processing arrays
   IF (EVONLY) THEN
      AXR(:)    = 0.0D0
      AYR(:)    = 0.0D0
      AZELEV(:) = 0.0D0
      AZHILL(:) = 0.0D0
      AZFLAG(:) = 0.0D0
      EVAPER(:) = 0
      EVDATE(:) = 0
      EVJDAY(:) = 0
      IDXEV(:)  = 0
      EVNAME(:) = ' '
      EVGRP(:)  = ' '
      EV_OrigConc(:) = 0.0D0
      EV_HRQS(:,:) = 0.0D0
      EV_HRTS(:,:) = 0.0D0
      EV_HRVS(:,:) = 0.0D0
      EV_HRSY(:,:) = 0.0D0
      EV_HRSZ(:,:) = 0.0D0
!**  Added for Aircraft Plume Rise; UNC-IE
      EV_HRMFUEL(:,:)  = 0.0D0
      EV_HRVAA(:,:)    = 0.0D0
      EV_HRTHRUST(:,:) = 0.0D0
      EV_HRBYPR(:,:)   = 0.0D0
      EV_HRAFR(:,:)    = 0.0D0
      EV_HRRPWR(:,:)   = 0.0D0
      EV_HRSRCANGLE(:,:)=0.0D0
!**  End Aircraft Plume Rise insert; April 2023
   ELSE IF (.NOT. EVONLY) THEN
! ---    Initialize Receptor arrays
      AXR(:)    = 0.0D0
      AYR(:)    = 0.0D0
      AZELEV(:) = 0.0D0
      AZHILL(:) = 0.0D0
      AZFLAG(:) = 0.0D0
      IF (ALLOCATED(XR_SCS)) XR_SCS(:,:) = 0.0D0                     ! Multiple_BuoyLines_D41_Wood
      IF (ALLOCATED(YR_SCS)) YR_SCS(:,:) = 0.0D0                     ! Multiple_BuoyLines_D41_Wood
      IF (ALLOCATED(XR_RCS)) XR_RCS(:) = 0.0D0
      IF (ALLOCATED(YR_RCS)) YR_RCS(:) = 0.0D0
      IREF(:)   = 0
      NETID(:)  = ' '
      RECTYP(:) = ' '
      NDXARC(:) = 0
      XORIG(:)  = 0.0D0
      YORIG(:)  = 0.0D0
      NUMXPT(:) = 0
      NUMYPT(:) = 0
      NETSTA(:) = 0
      NETEND(:) = 0
      NTID(:)   = ' '
      NTTYP(:)  = ' '
      XCOORD(:,:) = 0.0D0
      YCOORD(:,:) = 0.0D0
      XINT = 0.0D0
      YINT = 0.0D0
   END IF

   KAVE(:) = 0

!     Initialize the Outputs
   TITLE1 = ' '
   TITLE2 = ' '
   IPAGE  = 0
   IPGSUM = 0
   NHIVAL = 0
   NMXVAL = 0
   THRFRM ='(1X,I3,1X,A8,1X,I8.8,2(1X,F13.5),3(1X,F7.2),1X,F13.5)'
   PSTFRM ='(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)'
   PLTFRM ='(3(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,A8,2X,I8)'
   RNKFRM ='(1X,I6,1X,F13.5,1X,I8.8,2(1X,F13.5),3(1X,F7.2),2X,A8)'
   MXDFRM ='(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,2X,I8.8,&
   &2X,A8)'
   INHI(:)   = 0
   IDYTAB(:) = 0
   MAXAVE(:) = 0
   IMXVAL(:) = 0
   ITOXFL(:) = 0
   ITXUNT(:) = 0
   IRNKFL(:) = 0
   IRKVAL(:) = 0
   IRKUNT(:) = 0
   TOXTHR(:) = 0.0D0
   TOXFIL(:) = ' '
   RNKFIL(:) = ' '
   IDCONC(:,:) = 0
   TXCONC(:,:) = 0.0D0
   NHIAVE(:,:) = 0
   MAXFLE(:,:) = 0
   IPSTFL(:,:) = 0
   IMXUNT(:,:) = 0
   IPSUNT(:,:) = 0
   IPSFRM(:,:) = 0
   THRESH(:,:) = 0.0D0
   THRFIL(:,:) = ' '
   PSTFIL(:,:) = ' '
   IPLTFL(:,:,:) = 0
   IPLUNT(:,:,:) = 0
   PLTFIL(:,:,:) = ' '
   GRPID(:)  = ' '
   IANPST(:) = 0
   IANFRM(:) = 0
   IANPLT(:) = 0
   IAPUNT(:) = 0
   IPPUNT(:) = 0
   ISHUNT(:) = 0
   ISEAHR(:) = 0
   ANNPST(:) = ' '
   ANNPLT(:) = ' '
   IMXDLY(:) = 0
   IMDUNT(:) = 0
   MAXDLY(:) = ' '
   MAXDCONT(:) = 0
   IMXDLY_BYYR(:) = 0
   IMDUNT_BYYR(:) = 0
   MAXDLY_BYYR(:) = ' '
   MAXDCONT_FILE(:) = ' '
   MXD_RANK(:,:) = 0
   MAXD_THRESH(:) = 0.0D0

!     Initialize filenames
   SAVFIL = ' '
   MSGFIL = ' '
   EVFILE = ' '
   SUMFIL = ' '

   BKGRND_File(:) = ' '
   OZONFL(:) = ' '
   NOXFL(:) = ' '

!     Initialize the Number of Error/Warning/Informational Messages, and
!     The Number of Fatal Errors.
   IERROR = 0
   NFATAL = 0
   NWARN  = 0

   RETURN
END


SUBROUTINE INCLUD
!***********************************************************************
!*                INCLUD Module of AERMOD Model
!*
!*       PURPOSE: To read an external receptor/source file using the
!*                INCLUDED keyword.
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 30, 1995
!*
!        MODIFIED:   To remove reference to obsolete TG pathway inherited
!                    from ISCST3 code.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!*
!*       INPUTS:
!*
!*       OUTPUTS:
!*
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, ITEMPL
   LOGICAL NOPATH, NOKEY
   CHARACTER RDFRM*20
! JAT 06/22/21 D065
! REMOVE INPFLD AS UNUSED VARIABLE
!      CHARACTER INPFLD*2, PATHWY(7)*2
   CHARACTER PATHWY(7)*2
   INTERFACE
      SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
         CHARACTER (LEN=2), INTENT(IN) :: INPFLD
         CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
         INTEGER, INTENT(IN) :: IPN
         LOGICAL, INTENT(OUT) :: NOPATH
      END SUBROUTINE EXPATH
   END INTERFACE

!*    Variable Initializations
   MODNAM = 'INCLUD'
   EOF = .FALSE.
   ILINE  = 0
   ITEMPL = 0

!     Initialize PATHWY array
   PATHWY(1) = 'CO'
   PATHWY(2) = 'SO'
   PATHWY(3) = 'RE'
   PATHWY(4) = 'ME'
   PATHWY(5) = 'OU'
   PATHWY(6) = '**'
   PATHWY(7) = 'EV'

!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   WRITE(RDFRM,9100) ISTRG, ISTRG
9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

   IF (IFC .EQ. 3) THEN
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         INCFIL = RUNST1(LOCB(3):LOCE(3))
         OPEN (INCUNT,FILE=INCFIL,ACTION='READ',STATUS='OLD',ERR=99)
      ELSE
!           WRITE Error Message:  INCFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF

   ELSE IF (IFC .GT. 4) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message         ! No Parameters Specified
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   GO TO 1001

!     Write Out Error Message for File OPEN Error
99 CALL ERRHDL(PATH,MODNAM,'E','500','INCFILE ')
   RETURN

1001 CONTINUE


!     LOOP Through Input Runstream Records
   DO WHILE (.NOT. EOF)

!        Increment the Line Counter.  It was Initially Set to 1, to Handle
!        the Code in Subroutine DEFINE
      ILINE = ILINE + 1

!        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      READ (INCUNT,RDFRM,END=999,ERR=888) RUNST1,&
      &(RUNST(I), I = 1, ISTRG)

!        Check for blank input record and cycle
      IF (LEN_TRIM(RUNST1) .EQ. 0) CYCLE

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      CALL LWRUPR

!        If ILINE=1, reset ILINE temporarily to avoid the
!        check for column shift in subroutine DEFINE
      IF (ILINE .EQ. 1) THEN
         ILINE  = 2
         ITEMPL = 1
      END IF

!        Define Fields on Card                              ---   CALL DEFINE
      CALL DEFINE

!        Reset ILINE if needed
      IF (ITEMPL .EQ. 1) THEN
         ILINE  = 1
         ITEMPL = 0
      END IF

!        Get the Contents of the Fields                     ---   CALL GETFLD
      CALL GETFLD

!        Check for 'NO ECHO' In First Two Fields
      IF (FIELD(1) .EQ. 'NO' .and. FIELD(2) .EQ. 'ECHO') THEN
!           Skip record with NO ECHO in INCLUDED file, but leave ECHO "on"
         CYCLE
      END IF

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      CALL EXPATH(FIELD(1),PATHWY,7,NOPATH)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      IF (NOPATH) THEN
!           WRITE Error Message    ! Invalid Pathway ID
         CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
         PATH = PPATH
         CYCLE
      ELSE IF (PATH .EQ. '**') THEN
         CYCLE
      END IF

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      CALL EXKEY(FIELD(2),NOKEY)

      IF (NOKEY) THEN
!           WRITE Error Message    ! Invalid Keyword
         CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
         PKEYWD = KEYWRD
         CYCLE
      END IF

! ---    Check for Proper Order of Setup Cards              ---   CALL SETORD
!        Only call SETORD if not STARTING or FINISHED
!        since these keywords are not allowed in INCLUDED files.
!        This also allows INCLUD to be used for standard
!        processing and EVENT processing.
      IF (KEYWRD .NE. 'STARTING' .and.&
      &KEYWRD .NE. 'FINISHED') CALL SETORD

!        First Check for Invalid Keywords (STARTING, FINISHED, INCLUDED)
      IF (KEYWRD .EQ. 'STARTING') THEN
!           Cannot Use the STARTING keyword in the INCLUDED file
         CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)

      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
!           Cannot Recurse the INCLUDED Keyword in the INCLUDED file
!           Write Error Message: Repeat INCLUDED In Same Pathway
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)

      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
!           Cannot Use the FINISHED Keyword in the INCLUDED File
         CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)

!        Process Input Card Based on Pathway
      ELSE IF (PATH .EQ. 'SO') THEN
!           Process SOurce Pathway Cards                    ---   CALL SOCARD
         CALL SOCARD
      ELSE IF (PATH .EQ. 'RE') THEN
!           Process REceptor Pathway Cards                  ---   CALL RECARD
         CALL RECARD
      ELSE IF (PATH .EQ. 'EV') THEN
!           Process EVent Pathway Cards                     ---   CALL EVCARD
         CALL EVCARD

      END IF

!        Store the Current Keyword as the Previous Keyword
      PKEYWD = KEYWRD

!        Cycle to next record
      CYCLE

999   EOF = .TRUE.
      CONTINUE

   END DO
   EOF = .FALSE.

   GO TO 1002

888 CONTINUE
! --- Error occurred reading the included file, issue error message
   CALL ERRHDL(PATH,MODNAM,'E','510','INCLUDED')
   RUNERR = .TRUE.

1002 CONTINUE

!     Close the INCLUDED File
   CLOSE (INCUNT)

   RETURN
END
