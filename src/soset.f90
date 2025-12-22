SUBROUTINE SOCARD
!***********************************************************************
!                 SOCARD Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        V. Tino
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: To process SOurce Pathway card images
!
!        PROGRAMMER:  Roger Brode, Jeff Wang
!
!        MODIFIED:   Added code for Aircraft Source Group
!                    (to identify the aircraft sources).
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Added code to process multiple buoyant lines
!                    Multiple_BuoyLines_D41_Wood
!
!        MODIFIED:   Changed RLINEBAR and RLINEDPR keywords to RBARRIER
!                    and RDEPRESS, respectively.
!                    Wood, 3/18/2019
!
!        MODIFIED:   Replaced RLSRCCFG keyword with RLINEBAR and RLINEDPR.
!                    Wood, 12/29/2018
!
!        MODIFIED:   Added code to convert MOVES output units to RLINE
!                    units (RLEMCONV) and added the RLSRCCFG keyword.
!                    Wood, 07/20/2018
!
!        MODIFIED:   Added code to save buoyant line source parameters
!                    and perform the initial rotation (BL_ROTATE1) to
!                    align the long axis of the buildings with the
!                    x-axis coordinate system; both
!                    Amec Foster Wheeler, 06/30/2015
!
!        MODIFIED:   Modified ISSTAT() indices to eliminate potential
!                    conflicts among different options.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED BY  D. Strimaitis, SRC (for WET DEPOSITION)
!
!        DATE:    November  8, 1993
!
!        MODIFIED BY  D. Strimaitis, SRC (for DRY DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        INPUTS:  Pathway (SO) and Keyword
!
!        OUTPUTS: Source Arrays
!                 Sourcer Setup Status Switches
!
!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA, ONLY: RLMOVESCONV, L_RDEPRESS, L_RBARRIER
   USE BUOYANT_LINE

   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, ILSAVE, LNUM, KK
! Unused INTEGER :: J

!     Variable Initializations
   MODNAM = 'SOCARD'

   IF (KEYWRD == 'STARTING') THEN
!        Initialize Counters and Set Status Switch
      ISRC = 0
      IGRP = 0
      NUMSRC = 0
      NUMGRP = 0
      NUMOLM = 0
      NUMPSD = 0
      NUMBLAVGINP = 0       ! # BL groups defined on BLPINPUT records - D41_Wood
      NUMBLGRPS = 0         ! # BL groups defined by # of BLPINPUT records - D41_Wood
!        JAT 01/13/21 ISSUE D079
!        ONLY INITIALIZE NBLINGRP IF ALLOCATED
!        THIS CORRECTS A BUG INTRODUCED BY ISSUE D41
      IF (ALLOCATED(NBLINGRP)) NBLINGRP = 0          ! # lines in each BL source/group - D41/Wood

!        BL_Source_Limit_D088: Wood
      IF (ALLOCATED(IGRP_BLP)) IGRP_BLP = 0          ! 'Flag' indicating which BL group a BL line is in - D088/Wood
!        BL_Source_Limit_D088: Wood

      NUMCAP = 0
      NUMHOR = 0
      NURBSRC = 0
      NAFTSRC = 0                                    ! Added for Aircraft; UNC-IE
      NUMFLAT = 0
      ISSTAT(1) = ISSTAT(1) + 1
      IF (ISSTAT(1) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         GO TO 999
      END IF

!        Initialize the buoyant line source variables
      L_BLSOURCE = .FALSE.
!        JAT 01/13/21 ISSUE D079
!        ONLY INITIALIZE L_BLURBAN AND BL_NUMURB IF ALLOCATED
!        THIS CORRECTS A BUG INTRODUCED BY ISSUE D41
      IF (ALLOCATED(L_BLURBAN)) L_BLURBAN  = .FALSE.
      IF (ALLOCATED(BL_NUMURB)) BL_NUMURB  = 0

!        Initialize the RDEPRESS and RBARRIER source logicals
      L_RDEPRESS = .FALSE.
      L_RBARRIER = .FALSE.

!        Flush The Working Array
      IWRK2(:,:) = 0
   ELSE IF (KEYWRD == 'LOCATION') THEN
!        Set Status Switch
      ISSTAT(2) = ISSTAT(2) + 1
!        Check for SRCGROUP or PSDGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Process Source Location                            ---   CALL SOLOCA
      CALL SOLOCA
   ELSE IF (KEYWRD == 'SRCPARAM') THEN
!        Set Status Switch
      ISSTAT(3) = ISSTAT(3) + 1
!        Check for SRCGROUP or PSDGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Process Source Parameters                          ---   CALL SOPARM
      CALL SOPARM

! --- PRIME ---------------------------------
   ELSE IF (KEYWRD == 'BUILDHGT' .or.&
   &KEYWRD == 'BUILDWID' .or.&
   &KEYWRD == 'BUILDLEN' .or.&
   &KEYWRD == 'XBADJ   ' .or.&
   &KEYWRD == 'YBADJ   ') THEN
! -------------------------------------------

!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Set Status Switch
      IF (KEYWRD == 'BUILDHGT') THEN
         ISSTAT(4) = ISSTAT(4) + 1
      ELSE IF (KEYWRD == 'BUILDWID') THEN
         ISSTAT(5) = ISSTAT(5) + 1

! --- PRIME -----------------------------------
      ELSE IF (KEYWRD == 'BUILDLEN') THEN
         ISSTAT(21) = ISSTAT(21) + 1
      ELSE IF (KEYWRD == 'XBADJ   ') THEN
         ISSTAT(22) = ISSTAT(22) + 1
      ELSE IF (KEYWRD == 'YBADJ   ') THEN
         ISSTAT(23) = ISSTAT(23) + 1
! ---------------------------------------------

      END IF
!        Process Direction-specific Building Dimensions     ---   CALL DSBLDG
      CALL DSBLDG

! --- PLATFORM --------------------------------
!CRT  D063 Platform Downwash
!     CRT, 1/18/2012: add keyword PLATFORM for platform downwash
!     MGS, 10/2/2020: changed ISSTAT index to 47 from 42 (taken in v19191)
   ELSE IF (KEYWRD == 'PLATFORM') THEN

      IF(.NOT. L_ALPHA) THEN
!           WRITE Error Message:  "Non-DFAULT ALPHA option required"
!           for PLATFORM
         CALL ERRHDL(PATH,MODNAM,'E','198',' PLATFORM ')
      END IF

!        Set Status Switch
      ISSTAT(47) = ISSTAT(47) + 1

!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF

!           Process the PLATFM Card                      ---   CALL PLATFM
      CALL PLATFM
! ---------------------------------------------

   ELSE IF (KEYWRD == 'EMISFACT') THEN
!        Set Status Switch
      ISSTAT(7) = ISSTAT(7) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Process Variable Emission Rate Factors             ---   CALL EMVARY
      CALL EMVARY
   ELSE IF (KEYWRD == 'EMISUNIT') THEN
!        Set Status Switch
      ISSTAT(8) = ISSTAT(8) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (ISSTAT(8) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE IF (NUMTYP == 1) THEN
!           Process Emission Rate Unit Conversion Factors   ---   CALL EMUNIT
         CALL EMUNIT
      ELSE
!           WRITE Error Message: EMISUNIT Keyword with more than 1 output type
         CALL ERRHDL(PATH,MODNAM,'E','158',' ')
      END IF
!     Accept MOVES units (g/hr/link) for conversion to RLINE units, should follow LOCATION
   ELSE IF (KEYWRD == 'RLEMCONV') THEN
!        Set Status Switch
      ISSTAT(12) = ISSTAT(12) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Check that RLEMCONV is NOT repeated
      IF (ISSTAT(12) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
!MGS         ELSE IF(.NOT. BETA) THEN
!           WRITE Error Message: "Non-DFAULT BETA option required" for RLINE source type
!MGS            CALL ERRHDL(PATH,MODNAM,'E','199','RLEMCONV')
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (end)
!        Check The Number Of The Fields
      ELSE IF (IFC > 2) THEN
!           WRITE Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
!           Set RLINE MOVES conversion switch to TRUE
         RLMOVESCONV = .TRUE.
      END IF
   ELSE IF (KEYWRD == 'PARTDIAM' .or. KEYWRD == 'MASSFRAX' .or.&
   &KEYWRD == 'PARTDENS') THEN
!        Set Status Switch
      IF (KEYWRD == 'PARTDIAM') THEN
         ISSTAT(9) = ISSTAT(9) + 1
      ELSE IF (KEYWRD == 'MASSFRAX') THEN
         ISSTAT(10) = ISSTAT(10) + 1
      ELSE IF (KEYWRD == 'PARTDENS') THEN
         ISSTAT(11) = ISSTAT(11) + 1
      END IF
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Process Particle Deposition Parameters             ---   CALL PARTDEP
      CALL PARTDEP

   ELSE IF (KEYWRD == 'ELEVUNIT') THEN
!        Set Status Switch
      ISSTAT(15) = ISSTAT(15) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (ISSTAT(15) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE IF (NUMSRC > 0) THEN
!           Write Error Message: ELEVUNIT must be first card after STARTING
         CALL ERRHDL(PATH,MODNAM,'E','152','  SO')
      ELSE
!           Process Elevation Units for Source Elevations   ---   CALL SOELUN
         CALL SOELUN
      END IF
   ELSE IF (KEYWRD == 'HOUREMIS') THEN
!*       Set Status Switch
      ISSTAT(16) = ISSTAT(16) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Set HOURLY Flag
      HOURLY = .TRUE.
!*       Process Hourly Emissions                           ---   CALL HREMIS
      CALL HREMIS
!*#

   ELSE IF (KEYWRD == 'CONCUNIT') THEN
!        Set Status Switch
      ISSTAT(17) = ISSTAT(17) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (ISSTAT(17) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      END IF
      IF (ISSTAT(8) /= 0) THEN
!           WRITE Error Message: Conflict with EMISUNIT
         CALL ERRHDL(PATH,MODNAM,'E','159',KEYWRD)
      ELSE
!           Process Emission Rate Unit Conversion Factors   ---   CALL COUNIT
         CALL COUNIT
      END IF
   ELSE IF (KEYWRD == 'DEPOUNIT') THEN
!        Set Status Switch
      ISSTAT(18) = ISSTAT(18) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (ISSTAT(18) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      END IF
      IF (ISSTAT(8) /= 0) THEN
!           WRITE Error Message: Conflict with EMISUNIT
         CALL ERRHDL(PATH,MODNAM,'E','159',KEYWRD)
      ELSE
!           Process Emission Rate Unit Conversion Factors   ---   CALL DPUNIT
         CALL DPUNIT
      END IF

   ELSE IF (KEYWRD == 'AREAVERT') THEN
!        Set Status Switch
      ISSTAT(19) = ISSTAT(19) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Process Vertices for AREAPOLY Sources              ---   CALL ARVERT
      CALL ARVERT

   ELSE IF (KEYWRD == 'INCLUDED') THEN
!        Set Status Switch
      ISSTAT(20) = ISSTAT(20) + 1
!        Save ILINE as ISAVE
      ILSAVE = ILINE
!        Process the Included Receptor File                 ---   CALL INCLUD
      CALL INCLUD
!        Retrieve ILINE From ISAVE
      ILINE = ILSAVE

   ELSE IF (KEYWRD == 'SRCGROUP') THEN
!        Set Status Switch
      ISSTAT(24) = ISSTAT(24) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT. PSDCREDIT) THEN
!           Process Source Groups                           ---   CALL SOGRP
         CALL SOGRP
      ELSE
!           Write Error Message: SRCGROUP specified with PSDCREDIT option
         CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
      END IF

   ELSE IF (KEYWRD == 'GASDEPOS') THEN
!     JAT 7/02/19 added from 18081
!     given general lack of testing on gas deposition
!     now making gas deposition an alpha option
!     note other gas deposition keywords GDSEASON, GDLANUSE
!     occur in the CO pathway and are also checked
      IF (L_ALPHA) THEN
!             Set Status Switch
         ISSTAT(26) = ISSTAT(26) + 1
!             Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (.NOT. LUSERVD) THEN
!                 Process Gas Deposition Parameters               ---   CALL GASDEP
            CALL GASDEP
         ELSE
!                 Write Error Message:  User-specified deposition velocity
            CALL ERRHDL(PATH,MODNAM,'E','195',KEYWRD)
         END IF
      ELSE !issue error
         CALL ERRHDL(PATH,MODNAM,'E','198','GASDEPOS')
      ENDIF
   ELSE IF (KEYWRD == 'METHOD_2') THEN
!     JAT 6/25/19 added from 18081
!     given changes to scavenging ratio calculations in subroutine
!     scavrat for method 2, and general lack of testing on method 2
!     now making method 2 an alpha option
      IF (L_ALPHA) THEN
!             Set Status Switch
         ISSTAT(27) = ISSTAT(27) + 1
! ---         Check for DFAULT option; METHOD_2 is considered non-DFAULT
         IF (DFAULT) THEN
!                 Write Error Message:  METHOD_2 w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','197','METHOD_2') !shouldn't happen now JAT 6/25/19 but leave in
         ELSE
!                 Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
!                 Check for SRCGROUP Card Out Of Order
            IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
            ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
            END IF
!                 Process Method 2 Deposition Parameters          ---   CALL METH_2
            CALL METH_2
         END IF
      ELSE !issue error
         CALL ERRHDL(PATH,MODNAM,'E','198','METHOD_2')
      ENDIF
   ELSE IF (KEYWRD == 'URBANSRC') THEN
!        Set Status Switch
      ISSTAT(28) = ISSTAT(28) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (URBAN) THEN
!           Process the Urban Source Card                   ---   CALL URBANS
         CALL URBANS
      ELSE
!           Write Error Message:  Urban source defined without URBANOPT card
         CALL ERRHDL(PATH,MODNAM,'E','130','URBANOPT')
      END IF
   ELSE IF (KEYWRD == 'NO2RATIO') THEN
!        Set Status Switch
      ISSTAT(29) = ISSTAT(29) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (PVMRM .or. OLM&
      &.or. RUNTTRM .or. GRSM) THEN
!           Process the NO2 Ratio Card                      ---   CALL NO2RAT
         CALL NO2RAT
      ELSE
!           Write Error Message:  NO2RATIO specified without PVMRM, OLM or GRSM
         CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
      END IF

   ELSE IF (KEYWRD == 'OLMGROUP') THEN
!        Set Status Switch
      ISSTAT(30) = ISSTAT(30) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (OLM) THEN
!           Process the OLM Group Card                      ---   CALL OLMGRP
         CALL OLMGRP
      ELSE
!           Write Error Message:  OLMGROUP specified without OLM
         CALL ERRHDL(PATH,MODNAM,'E','144',KEYWRD)
      END IF

   ELSE IF (KEYWRD == 'PSDGROUP') THEN
!        Set Status Switch
      ISSTAT(34) = ISSTAT(34) + 1
!        Check for PSDGROUP Card Out Of Order
      IF (PSDCREDIT) THEN
!           Process the PSD Group Card                      ---   CALL PSDGRP
         CALL PSDGRP
      ELSE
!           Write Error Message: PSDGROUP specified without PSDCREDIT option
         CALL ERRHDL(PATH,MODNAM,'E','146',KEYWRD)
      END IF

   ELSE IF (KEYWRD == 'BACKGRND') THEN
!        Set Status Switch
      ISSTAT(40) = ISSTAT(40) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
!        Process the BACKGRND Card                         ---   CALL BACK_GRND
      CALL BACK_GRND

   ELSE IF (KEYWRD == 'BACKUNIT') THEN
!        Set Status Switch
      ISSTAT(41) = ISSTAT(41) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (ISSTAT(41) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process the BACKUNIT Card                      ---   CALL BACK_UNIT
         CALL BACK_UNIT
      END IF

   ELSE IF (KEYWRD == 'BGSECTOR') THEN
!        Set Status Switch
      ISSTAT(42) = ISSTAT(42) + 1
      IF (ISSTAT(42) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process BGSECTOR keyword                       ---   CALL BGSECTOR
         CALL BGSECTOR
      END IF

!     Check for RLINE barrier input
   ELSE IF (KEYWRD == 'RBARRIER') THEN
!         Added FLAT restriction in addition to the ALPHA restriction Wood 10/7
      IF(L_ALPHA .and. FLAT) THEN
!           Set the RBARRIER logical to true
         L_RBARRIER = .TRUE.
!           Process RBARRIER keyword                        ---   CALL RLINEBAR_INPUTS
         CALL RLINEBAR_INPUTS
      ELSE
!           Add error message if FLAT is not included
         IF (.NOT. FLAT) THEN
! ---                ERROR MESSAGE THAT BARRIER SOURCE REQUIRES 'FLAT' MODELOPT
            CALL ERRHDL(PATH,MODNAM,'E','713','')
         END IF
!           Add if statement to only write error messge if the ALPHA option is not included
         IF (.NOT. L_ALPHA) THEN
!               WRITE Error Message:  "Non-DFAULT ALPHA option required"
!               for RBARRIER defining source configuration parameters
            CALL ERRHDL(PATH,MODNAM,'E','198',' RBARRIER ')
         END IF
      END IF
!      Original portion
!C     Check for RLINE barrier input
!      ELSE IF (KEYWRD .EQ. 'RBARRIER') THEN
!         IF(L_ALPHA) THEN
!C           Set the RBARRIER logical to true
!            L_RBARRIER = .TRUE.
!C           Process RBARRIER keyword                        ---   CALL RLINEBAR_INPUTS
!            CALL RLINEBAR_INPUTS
!         ELSE
!C           WRITE Error Message:  "Non-DFAULT ALPHA option required"
!C           for RBARRIER defining source configuration parameters
!            CALL ERRHDL(PATH,MODNAM,'E','198',' RBARRIER ')
!         END IF

!     Check for RLINE depressed roadway input
   ELSE IF (KEYWRD == 'RDEPRESS') THEN
!         Added FLAT restriction in addition to the ALPHA restriction Wood 10/7
      IF(L_ALPHA .and. FLAT) THEN
!           Set the RDEPRESS logical to true
         L_RDEPRESS = .TRUE.
!           Process RDEPRESS keyword                        ---   CALL RLINEDPR_INPUTS
         CALL RLINEDPR_INPUTS
      ELSE
!           Add error message if FLAT is not included
         IF (.NOT. FLAT) THEN
! ---          ERROR MESSAGE THAT DEPRESSED SOURCE REQUIRES 'FLAT' MODELOPT
            CALL ERRHDL(PATH,MODNAM,'E','713','')
         END IF
!           Add if statement to only write error messge if the ALPHA option is not included
         IF (.NOT. L_ALPHA) THEN
!              WRITE Error Message:  "Non-DFAULT ALPHA option required"
!              for RDEPRESS defining source configuration parameters
            CALL ERRHDL(PATH,MODNAM,'E','198',' RDEPRESS ')
         END IF
      END IF
!    Original portion
!      ELSE IF (KEYWRD .EQ. 'RDEPRESS') THEN
!         IF(L_ALPHA) THEN
!C           Set the RDEPRESS logical to true
!            L_RDEPRESS = .TRUE.
!C           Process RDEPRESS keyword                        ---   CALL RLINEDPR_INPUTS
!            CALL RLINEDPR_INPUTS
!         ELSE
!C           WRITE Error Message:  "Non-DFAULT ALPHA option required"
!C           for RDEPRESS defining source configuration parameters
!            CALL ERRHDL(PATH,MODNAM,'E','198',' RDEPRESS ')
!         END IF
!

   ELSE IF (KEYWRD == 'BLPINPUT') THEN
!        Multiple_BuoyLines_D041_Wood: start
!        Removed code that did not allow for multiple BLPINPUT records

!        Set Status Switch
      ISSTAT(43) = ISSTAT(43) + 1
!        Multiple_BuoyLines_D041_Wood: end
!        Subroutine name changed
!        Process BLPINPUT keyword                           ---   CALL BL_AVGINP
      CALL BL_AVGINP

   ELSE IF (KEYWRD == 'BLPGROUP') THEN
!        Set Status Switch
! Multiple_BuoyLines_D41_Wood
!        Added routine to process BLPGRPOUP keyword(s); removed message that
!        keyword was not active in version 19191
      ISSTAT(44) = ISSTAT(44) + 1
!         IF (ISSTAT(44) .NE. 1) THEN
!           WRITE Error Message:
!            CALL ERRHDL(PATH,MODNAM,'W','385','combined')
!         ELSE
!           Process BL_INPUTS keyword                       ---   CALL BLPGRP
      CALL BLPGRP
!         END IF
!**  Added for Aircraft Plume Rise; UNC-IE
   ELSE IF (KEYWRD == 'ARCFTSRC') THEN
!        Set Status Switch
      ISSTAT(48) = ISSTAT(48) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF

      IF(ARCFT) THEN
!            Process the Aircraft Source Card  --- CALL AIRCRAFT
         CALL AIRCRAFT
!           check for HOURLY Flag means check for hourly emission file
!           that is must for Aircraft Sources
         IF (.NOT. HOURLY)THEN
!            WRITE Error Message for Hourly Emission File with Aircraft Sources
            CALL ERRHDL(PATH,MODNAM,'E','823','HOUREMIS')
         END IF
      ELSE
!           Write Error Message; Aircraft Source Group defined without ARCFTOPT card
         CALL ERRHDL(PATH,MODNAM,'E','821','ARCFTOPT')
      END IF
!**  End Aircraft Plume Rise insert; April 2023
! Added for HBP, JAN 2023
   ELSE IF (KEYWRD == 'HBPSRCID') THEN
!        Set Status Switch
      ISSTAT(45) = ISSTAT(45) + 1
!        Check for SRCGROUP Card Out Of Order
      IF (.NOT.PSDCREDIT .and. ISSTAT(24) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      ELSE IF (PSDCREDIT .and. ISSTAT(34) /= 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
      END IF
      IF (HBPLUME) THEN
         IF (L_ALPHA) THEN
!             Process the HBP Source Card                   ---   CALL HBPSOURCE
            CALL HBPSOURCE
         ELSE
!           WRITE Error Message:  "Non-DFAULT ALPHA option required"
!           for HBPSRC defining source configuration parameters
            CALL ERRHDL(PATH,MODNAM,'E','198','HBPSRCID')
         ENDIF
      ELSE
!           Write Error Message:  HBP source defined without HBPLUME card
         CALL ERRHDL(PATH,MODNAM,'E','130','HBPLUME')
      END IF
! End HBP Insert
   ELSE IF (KEYWRD == 'FINISHED') THEN
!        Set Status Switch
      ISSTAT(50) = ISSTAT(50) + 1
      IF (ISSTAT(50) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      END IF

!        Check to Insure That SRCGROUP or PSDGROUP Was The Last Functional Keyword
      IF (PKEYWD /= 'SRCGROUP' .and. .NOT.PSDCREDIT) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      END IF
      IF (PKEYWD /= 'PSDGROUP' .and. PSDCREDIT) THEN
         CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
      END IF

!        Check for Missing Mandatory Keywords
      IF (ISSTAT(1) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
      END IF
      IF (ISSTAT(2) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','LOCATION')
      END IF
      IF (ISSTAT(3) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
      END IF
      IF (ISSTAT(24) == 0 .and. .NOT.PSDCREDIT) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','SRCGROUP')
      END IF
      IF (ISSTAT(34) == 0 .and. PSDCREDIT) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','PSDGROUP')
      END IF
      IF (ISSTAT(3) < ISSTAT(2)) THEN
!           Must Be Missing a SRCPARAM Card for One or More Sources
         CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
      END IF

! ---    Check for BACKUNIT keyword without BACKGRND keyword
      IF (ISSTAT(40) == 0 .and. ISSTAT(41) > 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','193','SO BACKGRND')
      END IF

! ---    Check for number of source = 0
      IF (NUMSRC == 0) THEN
!           WRITE Error Message:  No Sources Input
         CALL ERRHDL(PATH,MODNAM,'E','185','NUMSRC=0')

      ELSE
! ---       Check for non-DFAULT gas deposition options
         IF (DFAULT .and. ISSTAT(26) > 0) THEN
!              Write Error Message:  Gas Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPOS')
         ELSE IF (ISSTAT(26) > 0) THEN
!              Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
! ---       Set logical flags for deposition options
         IF (LUSERVD .or. (ICSTAT(18)>0 .and.&
         &ICSTAT(20)>0 .and.&
         &ISSTAT(26)>0)) THEN
! ---          Gas dry deposition inputs specified
!              GASDEPVD or GDSEASON, GDLANUSE & GASDEPOS
            LDGAS = .TRUE.
         END IF
         IF (ISSTAT(26) > 0) THEN
! ---          Gas wet deposition inputs specified
            LWGAS = .TRUE.
         END IF
         IF ((ISSTAT( 9)>0 .and. ISSTAT(10)>0 .and.&
         &ISSTAT(11)>0) .or. ISSTAT(27)>0) THEN
! ---          Particle dry and wet deposition inputs specified
!              PARTDIAM, MASSFRAX & PARTDENS or METHOD_2
            LDPART = .TRUE.
            LWPART = .TRUE.
         END IF
         IF (DRYDPLT .or. (.NOT.NODRYDPLT .and.&
         &(LDGAS .or. LDPART))) THEN
! ---          Set dry depletion unless overridden by user
            DDPLETE = .TRUE.
         END IF
         IF (WETDPLT .or. (.NOT.NOWETDPLT .and.&
         &(LWGAS .or. LWPART))) THEN
! ---          Set wet depletion unless overridden by user
            WDPLETE = .TRUE.
         END IF

! ---       Check for incompatibilities with user-specified deposition velocity
!           This condition may not be caught by check in subroutine COCARD
         IF (LUSERVD .and. WDPLETE) THEN
!              Write Error Message: Wet deposition/depletion incompatible
!              with GASDEPVD option
            CALL ERRHDL(PATH,MODNAM,'E','243','GASDEPVD')
         END IF

! ---       Set model option header for dry depletion
         IF (DDPLETE) THEN
            IF (ARDPLETE) THEN
               MODOPS(14) = 'AREADPLT'
            ELSE IF (ROMBERG) THEN
               MODOPS(14) = 'ROMBERG'
            ELSE
               MODOPS(14) = 'DRYDPLT'
            END IF
         ELSE IF (NODRYDPLT) THEN
            MODOPS(14) = 'NODRYDPLT'
         ELSE
            MODOPS(14) = '         '
         END IF
! ---       Set model option header for wet depletion
         IF (WDPLETE) THEN
            MODOPS(15) = 'WETDPLT'
         ELSE IF (NODRYDPLT) THEN
            MODOPS(15) = 'NOWETDPLT'
         ELSE
            MODOPS(15) = '         '
         END IF
! ---       Check for error with inputs for dry deposition
         IF ((DDPLETE .or. DEPOS .or. DDEP) .and.&
         &(.NOT.LDGAS .and. .NOT.LDPART)) THEN
            CALL ERRHDL('SO',MODNAM,'E','244','DRYDEP')
         END IF
! ---       Check for error with inputs for wet deposition
         IF ((WDPLETE .or. DEPOS .or. WDEP) .and.&
         &(.NOT.LWGAS .and. .NOT.LWPART)) THEN
            CALL ERRHDL('SO',MODNAM,'E','244','WETDEP')
         END IF

!           All SO records processed and 'FINISHED' keyword encontered:
!           Additional bouyant line processing continues here
!           Store all buoyant line information in the single derived type
!            array, BLINEPARMS (see module.f for definitions)
!            INTEGER   (kind=4)  :: ISRCNUM
!            CHARACTER (len=12)  :: SRCID
!            DOUBLE PRECISION    :: XBEG, YBEG
!            DOUBLE PRECISION    :: XEND, YEND
!            DOUBLE PRECISION    :: ELEV, QS, HS

! Multiple_BuoyLines_D41_Wood - begin
!           Code to process legacy input files prior to inclusion of
!           BLPGROUP keyword;  individual lines are lumped into a BLPGROUP
!           with the ID 'ALL'
         LNUM = 0
         IF (NBLTOTAL > 0) THEN
            MODOPS(25) = 'BUOYLINE'

!              BLPINPUT_Checks_Missing_D091_Wood: start
!              Due to the logic in subroutine SRCQA, AERMOD crashes if
!              there are BUOYLINE sources but no BLPINPUT record.
!              To maintain the logic in SRCQA, the logical L_BLSOURCE
!              is set to TRUE here to indicate that at least one buoyant
!              line source is present in the model run
            L_BLSOURCE = .TRUE.

!              If no BLPGROUP record was encountered, the control file
!               may be a legacy file (from original implementation of
!               BL into AERMOD).  Assume only one group/source and put
!               all BUOYLINE sources into the single group 'ALL'

!              The condition that NUMBLAVGINP be > 0 was added so
!              NUMBLGRPS is not set to 1 when BLPINPUT record is missing

            IF (NUMBLGRPS == 0 .and. NUMBLAVGINP > 0) THEN
!              BLPINPUT_Checks_Missing_D091_Wood: end

               NUMBLGRPS = 1
!                  NBLINGRP(1) = NBLTOTAL
               DO I = 1,NUMSRC
                  IF (SRCTYP(I) == 'BUOYLINE') THEN
                     IGRP_BLP(I,1) = 1
                  END IF
               END DO
               BL_GRPID(1) = 'ALL'
            END IF
! Multiple_BuoyLines_D41_Wood - end

            DO I = 1,NUMSRC
               IF (SRCTYP(I) == 'BUOYLINE' ) THEN
                  LNUM = LNUM + 1
                  BLINEPARMS(LNUM)%ISRCNUM = I
                  BLINEPARMS(LNUM)%SRCID  = SRCID(I)
                  BLINEPARMS(LNUM)%XBEG   = AXS1(I)
                  BLINEPARMS(LNUM)%YBEG   = AYS1(I)
                  BLINEPARMS(LNUM)%XEND   = AXS2(I)
                  BLINEPARMS(LNUM)%YEND   = AYS2(I)
                  BLINEPARMS(LNUM)%ELEV   = AZS(I)
                  BLINEPARMS(LNUM)%BLQS   = AQS(I)
                  BLINEPARMS(LNUM)%BLHS   = AHS(I)

! Multiple_BuoyLines_D41_Wood - begin
! ---                The following tracks lines by BL groups
!                     NUMBLGRPS = number of BLPGROUPS groups in
!                     the input control file
                  DO J = 1,NUMBLGRPS
                     IF (IGRP_BLP(I,J) == 1) THEN
                        BLINEPARMS(LNUM)%IBLPGRPNUM = J
                        BLINEPARMS(LNUM)%BLPGRPNAME = BL_GRPID(J)
                        NBLINGRP(J) = NBLINGRP(J) + 1
                        EXIT
                     END IF
                  END DO
! Multiple_BuoyLines_D41_Wood - end

!     Will eventually need to address urban sources
!                          BLINEPARMS(LNUM)%IURBSRCNUM = ??
!                          BLINEPARMS(LNUM)%URBSRCNAME = ??
               ENDIF
            END DO

!              A logical L_BLSOURCE was set to true when the keyword
!               BLPINPUT was processed to indicate that there is a
!               buoyant line source in this model run.

!---           If there is a buoyant line source, the axes need to be
!               translated and rotated.  The rotation results in the x-axis
!               parallel to and the y-axis perpendicular to the long
!               side of the lines.
!              As part of the translation/rotation, a check is made for
!               the correct order of the lines - this will depend on
!               BL source groupings.

! Multiple_BuoyLines_D41_Wood
!              Translation and initial rotation performed by source group
            DO KK = 1,NUMBLGRPS
               CALL BL_ROTATE1 (KK)
            END DO
         END IF

! ---       Quality Assure Source Parameter Inputs          ---   CALL SRCQA
         CALL SRCQA

! ---       Check for consistency of deposition logical variables
!           Check for CO GDSEASON Card if Gas Deposition is Calculated
         IF (.NOT. LUSERVD .and. LDGAS .and. ICSTAT(18) == 0) THEN
! ---          Write Error Message:  Missing Mandatory Keyword
            CALL ERRHDL('CO',MODNAM,'E','130','GDSEASON')
         END IF
! ---       Check for CO GDLANUSE Card if Gas Deposition is Calculated
         IF (.NOT. LUSERVD .and. LDGAS .and. ICSTAT(20) == 0) THEN
!              Write Error Message:  Missing Mandatory Keyword
            CALL ERRHDL('CO',MODNAM,'E','130','GDLANUSE')
         END IF
! ---       Check for SO GASDEPOS Card if Gas Depos is Calculated w/o LUSERVD
         IF (.NOT. LUSERVD .and. LDGAS .and. ISSTAT(26) == 0) THEN
!              Write Error Message:  Missing Mandatory Keyword
            CALL ERRHDL('SO',MODNAM,'E','130','GASDEPOS')
         END IF

! ---       Calculate settling velocity and related time-invariant
!           deposition data                                 ---   CALL VDP1
         IF (LDPART .or. LDGAS) THEN
            CALL VDP1
         END IF

! ---       Reassign MODOPS(1) character for use of non-DFAULT options
!           MODOPS(1) initially assigned in sub. COCARD, but source inputs
!           may have changed value of L_NonDFAULT.
         IF (DFAULT) THEN
            MODOPS(1) = 'RegDFAULT'
         ELSE IF (L_NonDFAULT) THEN
            MODOPS(1) = 'NonDFAULT'
         ELSE
            MODOPS(1) = '         '
         END IF

      END IF

   ELSE
!        Write Error Message: Invalid Keyword for This Pathway
      CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
   END IF

999 RETURN
END SUBROUTINE SOCARD

SUBROUTINE SRCQA
!***********************************************************************
!                 SRCQA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Quality Assure Source Parameter Inputs
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!        MODIFIED BY D. Strimaitis, SRC (for WET & DRY DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        MODIFIED:   To add the ARCFTSRC source group option; Aircraft.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Check the that the number of buoyant lines matches
!                    the count of QFLAGS for hourly emissions
!                    Amec Foster Wheeler, 03/30/2016
!
!        MODIFIED:   To include a check on the order of the lines in the
!                    buoyant line source; if they are not in the correct
!                    order, the algorithms would not give the correct answer
!                    Amec Foster Wheeler, 06/30/2015
!
!        MODIFIED:   Modified checks for urban area without urban
!                    sources, and changed warning to fatal error
!                    for urban areas with no urban sources.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Include check for source being defined as
!                    both particulate and gaseous emissions.
!                    To include options to vary emissions by
!                    hour-of-day, and day-of-week (HRDOW and HRDOW7).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Modified calculation of area and center coordinates
!                    for AREAPOLY sources to use DOUBLE PRECISION. This
!                    change avoids problems encountered with the
!                    Compaq Visual Fortran compiler producing erroneous
!                    results for some compiler options.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
!
!        MODIFIED:   Calculates equivalent XINIT and YINIT values for
!                    AREAPOLY sources to allow for calculation of area
!                    of source under TOXICS option.  Also includes a
!                    a more refined computation of centroid for
!                    AREAPOLY sources.
!                    R.W. Brode, MACTEC (f/k/a PES), Inc., 7/23/2004
!
!        MODIFIED:   To include an option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, 4/10/2000
!
!        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        INPUTS:  Source Parameters
!                 Source Parameters Array Limits, IWRK2(NSRC,13)
!
!        OUTPUTS: Source Parameter Error Messages
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE
   USE RLINE_DATA, only: NRLINES

   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   LOGICAL :: FOPEN
   INTEGER :: I, J, N, ITOTSRC, ITOTGRP, NumBack, LNUM, KK
   INTEGER :: ILSAVE, NERRCOUNT, LCOUNT
   DOUBLE PRECISION :: ATOT
   DOUBLE PRECISION :: SUMA, SUMX, SUMY, AREA
   DOUBLE PRECISION :: BLXBEG, BLXEND

!     Variable Initializations
   MODNAM = 'SRCQA'
   FOPEN  = .FALSE.
   NERRCOUNT = 0

!     Begin Source LOOP
   DO I = 1, NUMSRC

!        Check Source Array Limits for Too Few Values;
!        (Too Many Checked In DSFILL and EFFILL)
      IF (IWRK2(I,1) >0 .or. IWRK2(I,2) >0 .or.&
      &IWRK2(I,11)>0 .or. IWRK2(I,12)>0 .or.&
      &IWRK2(I,13)>0) THEN

         IF (IWRK2(I,1)<NSEC) THEN
!              WRITE Error Message:  Not Enough BUILDHGTs
            CALL ERRHDL(PATH,MODNAM,'E','236',SRCID(I))
         END IF
         IF (IWRK2(I,2)<NSEC) THEN
!              WRITE Error Message:  Not Enough BUILDWIDs
            CALL ERRHDL(PATH,MODNAM,'E','237',SRCID(I))
         END IF

! --- PRIME -------------------------------------------------
         IF (IWRK2(I,11)<NSEC) THEN
!              WRITE Error Message:  Not Enough BUILDLENs
            CALL ERRHDL(PATH,MODNAM,'E','241',SRCID(I))
         END IF
         IF (IWRK2(I,12)<NSEC) THEN
!              WRITE Error Message:  Not Enough XBADJs
            CALL ERRHDL(PATH,MODNAM,'E','246',SRCID(I))
         END IF
         IF (IWRK2(I,13)<NSEC) THEN
!              WRITE Error Message:  Not Enough YBADJs
            CALL ERRHDL(PATH,MODNAM,'E','247',SRCID(I))
         END IF
! -----------------------------------------------------------
      END IF

! --- PRIME-PLATFORM DOWNWASH CONFLICT ----------------------
!     CRT, 6/6/2012: D063 PRIME and PLATFORM downwash params
!     cannot be specified for the same source

      IF (OSPLAT(I) .and. IWRK2(I,1) > 0) THEN

!           WRITE Error Message:  Cannot be both PRIME and PLATFORM
         CALL ERRHDL(PATH,MODNAM,'E','633',SRCID(I))

      END IF

! -----------------------------------------------------------

      IF (QFLAG(I) /= ' ') THEN
         IF (QFLAG(I)=='SEASON' .and. IWRK2(I,4)<4) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF (QFLAG(I)=='MONTH' .and. IWRK2(I,4)<12) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='HROFDY' .and. IWRK2(I,4)<24) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF (QFLAG(I)=='WSPEED' .and. IWRK2(I,4)<6) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='SEASHR' .and. IWRK2(I,4)<96) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='HRDOW' .and. IWRK2(I,4)<72) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='HRDOW7' .and. IWRK2(I,4)<168) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='SHRDOW' .and. IWRK2(I,4)<288) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='SHRDOW7' .and. IWRK2(I,4)<672) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='MHRDOW' .and. IWRK2(I,4)<864) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I)=='MHRDOW7' .and. IWRK2(I,4)<2016) THEN
!              WRITE Error Message: Not Enough QFACTs
            CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
         ELSE IF(QFLAG(I) == 'HOURLY') THEN
!              Check for use of hourly-varying sigmas and release heights
!              for VOLUME and AREA source types;
!              Call HRQREAD subroutine for each source to set L_HRLYSIG flag;
!              First save ILINE and reset to 1 to trigger L_HRLYSIG check
            ILSAVE = ILINE
            IQLINE = IQLINE + 1
            ILINE  = 1
            KURDAT = 0
!MGS           Check for aircraft source type for reading/setting
!              aircraft plume rise parameters.
!MGS               CALL HRQREAD(I) !D151 - 6/5/2023
            IF((AFTSRC(I) == 'Y')) THEN
!*               Retrieve AIRCRAFT Source Parameters for This Hour     ---   CALL AHRQREAD
               CALL AHRQREAD(I)
            ELSE
!*               Retrieve Source Parameters for This Hour     ---   CALL HRQREAD
               CALL HRQREAD(I)
            END IF
            ILINE  = ILSAVE
         END IF
      END IF
!MGS           END - Check for aircraft source type

! Multiple_BuoyLines_D41_Wood
!        Removed check on emission flag for buoyant line source at this point
!         in the source QA

!        Check Settling and Removal Parameters
      IF (IWRK2(I,5)/=0 .or. IWRK2(I,6)/=0 .or.&
      &IWRK2(I,7)/=0) THEN
!           Set Number of Particle Diameter Categories for This Source
         INPD(I) = IWRK2(I,5)
!           Check for Consistent Number of Categories for All Parameters
         IF (IWRK2(I,5)/=IWRK2(I,6) .or.&
         &IWRK2(I,5)/=IWRK2(I,7)) THEN
!              WRITE Error Message: PartDiam Categories Don't Match
            CALL ERRHDL(PATH,MODNAM,'E','240',SRCID(I))
         END IF
!           Check for Mass Fraction Summing to 1.0 (+/- 2%)
         ATOT = 0.0D0
         N = INPD(I)
         IF (N <= NPDMAX) THEN
            DO J = 1, N
               ATOT = ATOT + APHI(J,I)
            END DO
            IF (ATOT < 0.98D0 .or. ATOT > 1.02D0) THEN
!                 WRITE Error Message: Mass Fractions Don't Sum to 1.0
               CALL ERRHDL(PATH,MODNAM,'W','330',SRCID(I))
            END IF
         ELSE
!              WRITE Error Message:  Too Many Settling/Removal Categories
!              This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         END IF
      END IF

!        Screen for Conflicts with the Deposition Options
      IF (INPD(I) == 0) THEN
!           Check for NPD=0 and no gas deposition with the DEPOS, DDEP, or WDEP
         IF ((DEPOS.or.DDEP.or.WDEP.or.DDPLETE.or.WDPLETE) .and.&
         &.NOT. LUSERVD .and. SOGAS(I)=='N') THEN
!              WRITE Error Message for Lack of Particle Deposition Parameters
            CALL ERRHDL(PATH,MODNAM,'E','242',SRCID(I))
         END IF
      ELSE IF (INPD(I)>0 .and. SOGAS(I)=='Y') THEN
!           Check for NPD>0 and gas deposition for same source
!           WRITE Error Message for source as both particle and gas
         CALL ERRHDL(PATH,MODNAM,'E','289',SRCID(I))
      END IF

!        Check Vertices and Determine Centroid for AREAPOLY Sources
      IF (SRCTYP(I) == 'AREAPOLY') THEN
         IF (IWRK2(I,10) < NVERTS(I) ) THEN
!              WRITE Error Message:  Not Enough Vertices Input For This Source?
            CALL ERRHDL(PATH,MODNAM,'E','265',SRCID(I))
         ELSE
!              Repeat First Vertex as Last Vertex to Close Polygon
            AXVERT(NVERTS(I)+1,I) = AXVERT(1,I)
            AYVERT(NVERTS(I)+1,I) = AYVERT(1,I)

!              Determine coordinates for centroid of polygon source;
!              First calculate area of polygon
            suma = 0.0D0
            do j = 1, NVERTS(I)
               suma = suma + (AXVERT(j,i)  *AYVERT(j+1,i) -&
               &AXVERT(j+1,i)*AYVERT(j,i))
            end do

            area = 0.5D0 * suma

!              Assign SQRT(DABS(area)) to AXINIT and AYINIT; equivalent values
!              of AXINIT and AYINIT will be used to calculate area of polygon
            AXINIT(I) = DSQRT( DABS(area) )
            AYINIT(I) = DSQRT( DABS(area) )

!              Now determine coordinates of centroid
            sumx = 0.0D0
            sumy = 0.0D0
            do j = 1, NVERTS(I)
               sumx = sumx +(AXVERT(j,i)+AXVERT(j+1,i)) *&
               &(AXVERT(j,i)*AYVERT(j+1,i) -&
               &AXVERT(j+1,i)*AYVERT(j,i))
               sumy = sumy +(AYVERT(j,i)+AYVERT(j+1,i)) *&
               &(AXVERT(j,i)*AYVERT(j+1,i) -&
               &AXVERT(j+1,i)*AYVERT(j,i))
            end do

            IF (DABS(area) < 0.0001D0) THEN
!                 WRITE Error Message:  Invalid shape for AREAPOLY source
               CALL ERRHDL(PATH,MODNAM,'E','266',SRCID(I))
               AXCNTR(I) = 0.0D0
               AYCNTR(I) = 0.0D0
            ELSE
!                 Calculate coordinates of centroid
               AXCNTR(I) = sumx/(6.0D0*area)
               AYCNTR(I) = sumy/(6.0D0*area)
            END IF

         END IF
      END IF

!        Check for urban sources
      IF (URBSRC(I) == 'Y') THEN
         NURBSRC = NURBSRC + 1
      END IF
!**  Added for Aircraft Plume Rise; UNC-IE
!        Check for Aircraft sources
      IF (AFTSRC(I) == 'Y') THEN
         NAFTSRC = NAFTSRC + 1
      END IF
!**  End Aircraft Plume Rise insert; April 2023
!        Check for capped and horizontal stack sources
      IF (SRCTYP(I) == 'POINTCAP') THEN
         NUMCAP = NUMCAP + 1
      ELSE IF (SRCTYP(I) == 'POINTHOR') THEN
         NUMHOR = NUMHOR + 1
      END IF
! ---    Include count for all source types:
      IF (SRCTYP(I)(1:5) == 'POINT') THEN
         NUMPNT = NUMPNT + 1
      ELSE IF (SRCTYP(I) == 'VOLUME') THEN
         NUMVOL = NUMVOL + 1
      ELSE IF (SRCTYP(I)(1:4) == 'AREA') THEN
         NUMAREA = NUMAREA + 1
      ELSE IF (SRCTYP(I) == 'LINE') THEN
         NUMLINE = NUMLINE + 1
      ELSE IF (SRCTYP(I) == 'OPENPIT') THEN
         NUMPIT = NUMPIT + 1
      ELSE IF (SRCTYP(I) == 'BUOYLINE') THEN
! Multiple_BuoyLines_D41_Wood
!           The counter for the total number of indivdual lines,
!           NBLTOTAL, as well as the number of lines in each BL
!           group, NBLINGRP array, now is determined in SOCARD

!        Added for SIDEWASH point source
      ELSE IF (SRCTYP(I) == 'SWPOINT') THEN
         NUMSWP = NUMSWP + 1
      END IF

!        Identify the index of the level immediately below the top of the
!        stack from the array of gridded heights; we are limiting the
!        number of levels to search to 29 (= 600 m).  (Changed from 21
!        by R. Brode, PES, 2/17/95)

      CALL LOCATE( GRIDHT, 1, 29, AHS(I), NDXSTK(I) )

   END DO
!     End Source LOOP

!     D128: Warning message when AREAMNDR is used without an AREA Source present
   IF (L_AREAMNDR .and. NUMAREA == 0) THEN
      CALL ERRHDL(PATH,'RLINE','W','668','AREAMNDR')
   END IF

!     Check for open HOUREMIS file; if so, rewind file
   IF (HOURLY) THEN
!        Check for Hourly Emissions File
      INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
      IF (FOPEN) THEN
         REWIND IHREMI
         IQLINE = 0
      END IF
   END IF

   IF (PSDCREDIT) THEN
!RWB     Assign default "source groups" for PSDCREDIT applications:
!RWB     first "source group" contains cumulative NAAQS calculation;
!RWB     second "source group" contains PSD increment consumtion with credits.
      NUMGRP = 2
      GRPID(1) = 'NAAQS   '
      GRPID(2) = 'PSDINC  '

!        Check for source not included in any PSDGROUP
      DO I = 1, NUMSRC
         ITOTSRC = 0
         DO J = 1, NUMPSD
            IF (IGRP_PSD(I,J) == 1) THEN
               ITOTSRC = ITOTSRC + 1
            END IF
         END DO
         IF (ITOTSRC == 0) THEN
!              Write Error Message:  Source not in PSDGROUP
            CALL ERRHDL(PATH,MODNAM,'E','317',SRCID(I))
         END IF
      END DO

   ELSE

! ---    Check for empty source groups and count how many groups
!        include BACKGROUND
      NumBack = 0
      DO J = 1, NUMGRP
         ITOTSRC = 0
         DO I = 1, NUMSRC
            IF (IGROUP(I,J) == 1) THEN
               ITOTSRC = ITOTSRC + 1
            END IF
         END DO
         IF (GRP_BACK(J)) THEN
            NumBack = Numback + 1
         END IF
         IF (ITOTSRC == 0 .and. .NOT.GRP_BACK(J)) THEN
!              Write Warning Message:  No Sources in SRCGROUP
            CALL ERRHDL(PATH,MODNAM,'W','319',GRPID(J))
         END IF
      END DO

! ---    Issue warning if BACKGROUND is specified but not included with
!        any source groups
      IF (L_BACKGRND .and. NumBack == 0) THEN
!           Write Warning Message:  BACKGROUND not in any SRCGROUP
         CALL ERRHDL(PATH,MODNAM,'W','321',' ')
      END IF

!        Check for source not included in any source group
      DO I = 1, NUMSRC
         ITOTSRC = 0
         DO J = 1, NUMGRP
            IF (IGROUP(I,J) == 1) THEN
               ITOTSRC = ITOTSRC + 1
            END IF
         END DO
         IF (ITOTSRC == 0) THEN
!              Write Warning Message:  Source not in SRCGROUP
            CALL ERRHDL(PATH,MODNAM,'W','316',SRCID(I))
         END IF
      END DO

   END IF

! Added for HBP, JAN. 2023
   IF (HBPLUME .and. NHBP==0) THEN
!        Write Error Message:  No HBP sources defined with HBP
      CALL ERRHDL(PATH,MODNAM,'E','130','HBPSRCID')
   END IF
! End HBP insert

   IF (URBAN .and. NURBSRC==0) THEN
!        Write Error Message:  No urban sources defined with URBANOPT
      CALL ERRHDL(PATH,MODNAM,'E','130','URBANSRC')
   END IF

!     Check for Urban Areas with No Sources;
!     (single urban area checked based on missing URBANSRC card)
   IF (NUMURB > 1) THEN
      DO J = 1, NUMURB
         ITOTSRC = 0
         DO I = 1, NUMSRC
            IF (IURBGRP(I,J) == 1) THEN
               ITOTSRC = ITOTSRC + 1
            END IF
         END DO
         IF (ITOTSRC == 0) THEN
!              Write Error Message:  No Sources for Urban Area
            CALL ERRHDL(PATH,MODNAM,'E','318',URBID(J))
         END IF
      END DO
   END IF

!**  Added for Aircraft Plume Rise; UNC-IE
   IF (ARCFT .and. NAFTSRC==0) THEN
!        Write Error Message:  No Aircraft sources defined with ARCFTOPT
      CALL ERRHDL(PATH,MODNAM,'E','822','ARCFTSRC')
   END IF
!**  End Aircraft Plume Rise insert, April 2023

!     Check for source in more than one Urban Area
   DO I = 1, NUMSRC
      ITOTGRP = 0
      DO J = 1, NUMURB
         IF (IURBGRP(I,J) == 1) THEN
            ITOTGRP = ITOTGRP + 1
         END IF
      END DO
      IF (ITOTGRP > 1) THEN
!           Write Error Message:  Source in more than one Urban Area
         CALL ERRHDL(PATH,MODNAM,'E','302',SRCID(I))
      END IF
   END DO

!  Multiple_BuoyLines_D41_Wood
!     QA for buoyant line sources: code moved to this point from earlier
!      in this subroutine

   IF (L_BLSOURCE) THEN
!        There is at least one buoyant line in this model run

!        A minimum release height of 2 meters has been established for
!         the lines of buoyant line sources
      DO LNUM = 1,NBLTOTAL
         IF (BLINEPARMS(LNUM)%BLHS < 2.0D0) THEN
!              Apply a minimum release ht. of 2.0 m to the buoyant line
            BLINEPARMS(LNUM)%BLHS = 2.0D0
            WRITE(DUMMY,'(''line #'',I2)') LNUM
            CALL ERRHDL(PATH,MODNAM,'W','472',DUMMY)
         END IF
      END DO


!        Be sure the x-coordinates for each buoyant line are entered
!         WEST to EAST;
!         This check is on the UNTRANSLATED, UNROTATED coordinates
!         entered on the and is not a function of BL source groups.

      DO ILINE = 1,NBLTOTAL

         BLXBEG = BLINEPARMS(ILINE)%XBEG
         BLXEND = BLINEPARMS(ILINE)%XEND
         IF(BLXBEG > BLXEND) THEN
!              Coordinates are entered in reverse order for this ILINE
            NERRCOUNT = NERRCOUNT + 1
         END IF

      END DO
!
      IF (NERRCOUNT > 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','388','BUOYLINE')
      END IF

!        BLPINPUT_Checks_Missing_D091_Wood: begin
!        There is at least one individual buoyant line but no BLPINPUT
      IF (NUMBLAVGINP == 0 .and. NBLTOTAL > 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','507','  ')
      ENDIF

!        The number of BLPGROUPS allocated (via NBLGRP) does not equal
!         the number of BL groups as defined by BLPINPUT records
!         Recall that the groups are defined by the BLPINPUT record(s),
!         which must appear before the BLPGROUP record(s)
      IF (NUMBLGRPS /= NBLGRP) THEN
         CALL ERRHDL(PATH,MODNAM,'E','509','  ')
      ENDIF
!        BLPINPUT_Checks_D091_Wood: end

!     Multiple_BuoyLines_D41_Wood: Begin

!        Check that the number of BLPGROUP IDs match the number of
!         BLPINPUT IDs; the BLPINPUT records are processed first and
!         define the group IDs so the number of BLPINPUT records
!         (NUMBLAVGINP) should equal the number in the input control file.
!         The number of BLPGROUP IDs (NUMBLGRPS) will equal the number
!         of BLPINPUT records if the IDs match; otherwise there is a
!         mismatch between the two numbers

      IF (NUMBLGRPS /= NUMBLAVGINP) THEN
         CALL ERRHDL(PATH,MODNAM,'E','503','  ')
      ENDIF

!        The next check is dependent on the number of individual lines
!         identified in each of the BLPGROUPs and the number specified
!         on the HOUREMIS record(s)
!         'Passing' this check requires that the number of lines
!         defined in a BLPGROUP (NBLINGRP()) is the same number of
!         individual lines that appear on the HOUREMIS record(s)
!         Write an error message if there is a mismatch.

!        Count the number of individual lines in each BL source
!         group in the hourly emissions file.
!         The counting is done here since we do not know which keyword,
!         HOUREMIS or BLPGROUP, is processed first.
!         If the counting were performed in subr.HREMIS and the BLPGROUPs
!         were not defined, then NUMBLGRPS would not be defined

! Multiple_BuoyLines_D41_Wood
!       Initialize array that counts the number of hourly emission
!         records by buoyant line group - do not move this inside the
!         logic that follows.  It messes up the results when the code
!         is compiled with gfortran because the array is not intialized
!         if there is no hourly emissions file and causes incorrect
!         calculations in BLCALC
      HRLYBLCOUNT(1:NUMBLGRPS) = 0

      IF (L_BLHOURLY) THEN
!           L_BLHOURLY was set in subr.HREMIS
!           Count the number of lines in each BL source group

!           Check to see if QFLAG is set for buoyant lines - increment
!           counter(s) for later use
!           DO I = 1,NUMSRC
!              IF (SRCTYP(I).EQ.'BUOYLINE' .and.
!    &              QFLAG(I).EQ.'HOURLY') THEN
!                 OUTER: DO LNUM = 1,NBLTOTAL
!                    DO KK = 1, NUMBLGRPS
!                       IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
!                          HRLYBLCOUNT(KK) = HRLYBLCOUNT(KK) + 1
!                          EXIT OUTER
!                       ENDIF
!                    END DO
!                 END DO OUTER
!              END IF
!           END DO

         DO LNUM = 1,NBLTOTAL
            IF (QFLAG(BLINEPARMS(LNUM)%ISRCNUM)=='HOURLY') THEN
               DO KK = 1, NUMBLGRPS
                  IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
                     HRLYBLCOUNT(KK) = HRLYBLCOUNT(KK) + 1
                  END IF
               END DO
            END IF
         END DO

!        Compare the number(s) obtained above for HRLYBLCOUNT to the
!        number in each source group
         DO KK = 1,NUMBLGRPS
            IF (HRLYBLCOUNT(KK) > 0 .and.&
            &NBLINGRP(KK) > 0) THEN
               IF (HRLYBLCOUNT(KK) /= NBLINGRP(KK)) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','383',BL_GRPID(KK))
               END IF
            END IF
         END DO
      END IF
!     Multiple_BuoyLines_D41_Wood: End

!        If one line of a buoyant line source group is declared urban,
!         then all lines must be declared urban.  If not, generate an
!         error message.
!         The array L_BLURBAN is initialized as FALSE in subr.SOCARD
      LCOUNT = 0
      DO KK = 1,NUMBLGRPS
         DO ILINE = 1,NBLINGRP(KK)
            LCOUNT = LCOUNT + 1
            IF (URBSRC(BLINEPARMS(LCOUNT)%ISRCNUM) == "Y") THEN
               IF (.NOT. L_BLURBAN(KK)) L_BLURBAN(KK) = .TRUE.
               BL_NUMURB(KK) = BL_NUMURB(KK) + 1
            END IF
         END DO

         IF ((L_BLURBAN(KK)) .and.&
         &(BL_NUMURB(KK) /= NBLINGRP(KK))) THEN
!               Write Error Message:  At least one but not all BL
!                lines in the BL source group are declared as urban
            CALL ERRHDL(PATH,MODNAM,'E','393',BL_GRPID(KK))
         END IF
      END DO

!        CRO 3/28/2022 D132 Remove alpha requirement for BLINE (not sure it was working in 21112 anyway)
!        Check to require ALPHA with Buoyant Line urban sources
!         DO J = 1, NUMSRC
!            IF (SRCTYP(J) .EQ. 'BUOYLINE') THEN
!               IF ((URBSRC(J) .EQ. 'Y') .and. .NOT.L_ALPHA) THEN
!                  WRITE Error Message: ALPHA required for BUOYLINE URBANSRC
!                  CALL ERRHDL(PATH,MODNAM,'E','281', SRCID(J))
!               END IF
!            END IF
!         END DO

!        Multiple_BuoyLines_D41_Wood: Begin
!        Check to require BETA with Multiple Buoyant Line sources
!         IF (NUMBLGRPS .GT. 1) THEN
!            IF (.NOT.BETA) THEN
!           WRITE Error Message: Non-DFAULT BETA option required for
!            multiple buoyant line source type
!               WRITE(DUMMY, '(''# BL Grps'',I2)') NUMBLGRPS
!               CALL ERRHDL(PATH,MODNAM,'E','199', DUMMY)
!            END IF
!         END IF

!        Check BUOYLINE source is in one and only one BLPGROUP
      IF (NUMBLGRPS > 0) THEN
!           At least one BLPGROUP record encountered
         DO I = 1, NUMSRC
            ITOTGRP = 0
            IF (SRCTYP(I) == 'BUOYLINE') THEN
               DO J = 1, NUMBLGRPS
                  IF (IGRP_BLP(I,J) == 1) THEN
                     ITOTGRP = ITOTGRP + 1
                  END IF
               END DO
               IF (ITOTGRP > 1) THEN
!                    Write Error Message:  BUOYLINE source in more than one BLPGROUP
                  CALL ERRHDL(PATH,MODNAM,'E','257',SRCID(I))
               END IF
               IF (ITOTGRP == 0) THEN
!                    Write Error Message:  BUOYLINE source not in a BLPGROUP
                  CALL ERRHDL(PATH,MODNAM,'E','258',SRCID(I))
               END IF
            END IF
         END DO
      END IF
   END IF
!     Multiple_BuoyLines_D41_Wood: End

!     Multiple_BuoyLines_D41_Wood: Start
!     Check that the order of the buoyant lines associated with the
!       BLPINPUT records matches the order of the individual sources
!       on the SO LOCATION
!       If there is only one group, no check is needed
   IF (NUMBLGRPS > 1) THEN
      DO LNUM = 2,NBLTOTAL
         IF (BLINEPARMS(LNUM)%IBLPGRPNUM -&
         &BLINEPARMS(LNUM-1)%IBLPGRPNUM < 0) THEN
            WRITE(DUMMY,'(I2,'' and'',I2)')&
            &BLINEPARMS(LNUM-1)%IBLPGRPNUM, BLINEPARMS(LNUM)%IBLPGRPNUM
            CALL ERRHDL(PATH,MODNAM,'E','505',DUMMY)
         END IF
      END DO
   END IF
!     Multiple_BuoyLines_D41_Wood: End

!     Check for source in more than one OLMGROUP
   DO I = 1, NUMSRC
      ITOTGRP = 0
      DO J = 1, NUMOLM
         IF (IGRP_OLM(I,J) == 1) THEN
            ITOTGRP = ITOTGRP + 1
         END IF
      END DO
      IF (ITOTGRP > 1) THEN
!           Write Error Message:  Source in more than one OLMGROUP
         CALL ERRHDL(PATH,MODNAM,'E','282',SRCID(I))
      END IF
   END DO


!     Check that Deposition options are all FALSE when RLINE or BUOYLINE are present
   IF(DEPOS .or. DDEP .or. WDEP) THEN
      IF (NBLTOTAL > 0) THEN
!              Write Error Message:Deposition (DEPOS, DDEP, WDEP) incompatible with
         CALL ERRHDL(PATH,MODNAM,'E','251','BUOYLINE')
      ELSE IF (NRLINES > 0) THEN
!              Write Error Message:Deposition (DEPOS, DDEP, WDEP) incompatible with
         CALL ERRHDL(PATH,MODNAM,'E','251','RLINE/RLINEXT')
      END IF
   END IF

!     Chack that SCREEN option is FALSE when RLINE, BUOYLINE, SWPOINT, AREA, and LINE sources are present
!     D164 2/21/23 WSP
!     D164 6/7/2023 CRT - MDT decided this should be a warning rather than an error. Updated 'E' to 'W'.
!                         Update to separate if statments so a warning will be issued for each source type.
   IF(SCREEN) THEN
      IF (NBLTOTAL > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','BUOYLINE')
         CALL ERRHDL(PATH,MODNAM,'W','731','BUOYLINE')
      END IF
      IF (NRLINES > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','RLINE/RLINEXT')
         CALL ERRHDL(PATH,MODNAM,'W','731','RLINE/RLINEXT')
      END IF
      IF (NSWP > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','SWPOINT')
         CALL ERRHDL(PATH,MODNAM,'W','731','SWPOINT')
      END IF
      IF (NAREA > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','AREA')
         CALL ERRHDL(PATH,MODNAM,'W','731','AREA')
      END IF
      IF (NPOLY > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','AREAPOLY')
         CALL ERRHDL(PATH,MODNAM,'W','731','AREAPOLY')
      END IF
      IF (NCIRC > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','AREACIRC')
         CALL ERRHDL(PATH,MODNAM,'W','731','AREACIRC')
      END IF
      IF (NPIT > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','OPENPIT')
         CALL ERRHDL(PATH,MODNAM,'W','731','OPENPIT')
      END IF
      IF (NLINE > 0) THEN
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','LINE')
         CALL ERRHDL(PATH,MODNAM,'W','731','LINE')
      END IF
   END IF


!     Check for source in more than one PSDGROUP                        ! jop 9/30/06
   IF (PSDCREDIT) THEN
      DO I = 1, NUMSRC
         ITOTGRP = 0
         DO J = 1, NUMPSD
            IF (IGRP_PSD(I,J) == 1) THEN
               ITOTGRP = ITOTGRP + 1
            END IF
         END DO
         IF (ITOTGRP > 1) THEN
!              Write Error Message:  Source in more than one PSDGROUP
            CALL ERRHDL(PATH,MODNAM,'E','286',SRCID(I))
         END IF
      END DO
   END IF

! --- Check for potential problems with inputs for NO2 options; first for OLM/PVMRM/GRSM/TTRM
   IF (OLM .or. PVMRM .or. RUNTTRM .or. GRSM) THEN
!        Check for negative emission rate with OLM, PVMRM or GRSM option.
!        Negative emission for credit sources cannot be used for OLM, PVMRM and
!        GRSM due to non-linear dependence of concentrations on emissions.
!RWB     Draft BETA-test option for PSDCREDIT accounts for non-linear effects
!RWB     on PSD increment calculations for PVMRM, but does not use negative
!RWB     emission rates to identify PSD credit sources.
      DO I = 1, NUMSRC
         IF (AQS(I) < 0.0D0) THEN
!              Write Error Message:  Negative emission rate for OLM/PVMRM/GRSM
            CALL ERRHDL(PATH,MODNAM,'E','338',SRCID(I))
         END IF
      END DO

!        Check for negative in-stack NO2/NOx ratio with OLM, PVMRM or GRSM option.
!        This indicates that user has not specified an in-stack ratio for
!        a particular source.  This is an error since no default ratio has
!        been determined for use with OLM, PVMRM or GRSM with the 1-hour NO2 NAAQS.
      DO I = 1, NUMSRC
         IF (ANO2_RATIO(I) < 0.0D0) THEN
!              Write Error Message:  No in-stack ratio specified for PVMRM/OLM/GRSM
            CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(I))
         END IF
      END DO

! --- Check for potential problems with inputs for NO2 options; ARM2
   ELSE IF (ARM2) THEN
!        Check for SRCGROUP ALL, which is required for the ARM2 option
      INDX_GRPALL = 0
      DO I = 1, NUMGRP
         IF (GRPID(I) == 'ALL') THEN
            INDX_GRPALL = I
            EXIT
         END IF
      END DO
      IF (INDX_GRPALL == 0) THEN
!           Issue fatal error message; Group ALL not found
         IF (ARM2) THEN
            CALL ERRHDL(PATH,MODNAM,'W','299','ARM2 Option')
         END IF
      END IF
   END IF

! --- Check for completeness of BACKGRND inputs
   IF (L_BACKGRND) THEN
! ---    Check for correct number of temporally varying background concentrations
!        for the BACKGRND keyword by sector.
      IF (L_BGSector) THEN
         DO I = 1, NUMBGSects
            IF (.NOT. L_BGFile(I)) THEN
!                 Check for no BACKGRND values for this sector w/o HOURLY BG
               IF (.NOT. L_BGValues(I)) THEN
!                    WRITE Error Message: No BACKGRND values - Missing Sector
                  WRITE(DUMMY,'(''No BG SECT'',I1,''!'')') I
                  CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
               ELSE IF (IBKGRD(I) < IBGMAX(I)) THEN
!                    WRITE Error Message: Not Enough BACKGRND values
                  IF (IBKGRD(I) < 100) THEN
                     WRITE(DUMMY,'(''BGSECT'',I1,'' N='',I2)') I,&
                     &IBKGRD(I)
                  ELSE
                     WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)') I,&
                     &IBKGRD(I)
                  END IF
                  CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
               END IF
            ELSE IF (L_BGFile(I)) THEN
!                 First check for HOURLY BG for this sector
               IF (LEN_TRIM(BKGRND_File(I)) == 0) THEN
!                    No HOURLY BG file available for this sector; check for complete BACKGRND values
                  IF (.NOT. L_BGValues(I)) THEN
!                       WRITE Error Message: No BACKGRND values for this sector
                     WRITE(DUMMY,'(''No BG SECT'',I1,''!'')') I
                     CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
                  ELSE IF (IBKGRD(I) < IBGMAX(I)) THEN
!                       WRITE Error Message: Incomplete BACKGRND values for this sector
                     IF (IBKGRD(I) < 100) THEN
                        WRITE(DUMMY,'(''BGSECT'',I1,'' N='',I2)') I,&
                        &IBKGRD(I)
                     ELSE
                        WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)') I,&
                        &IBKGRD(I)
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
                  ELSE
!                       WRITE Warning Message: BGVALs but No HOURLY BG file available for this sector
                     WRITE(DUMMY,'(''BGFILE SECT'',I1)') I
                     CALL ERRHDL(PATH,MODNAM,'W','248',DUMMY)
                  END IF
               ELSE
!                    HOURLY BG file available for this sector; check for complete BACKGRND values
                  IF (.NOT. L_BGValues(I)) THEN
!                       WRITE Warning Message: No Varying BACKGRND but HOURLY avail for this Sector
                     WRITE(DUMMY,'(''BGVALs SECT'',I1)') I
                     CALL ERRHDL(PATH,MODNAM,'W','248',DUMMY)
                  ELSE IF (IBKGRD(I) < IBGMAX(I)) THEN
!                       WRITE Error Message: Not Enough BACKGRND values
                     IF (IBKGRD(I) < 100) THEN
                        WRITE(DUMMY,'(''BGSECT'',I1,'' N='',I2)') I,&
                        &IBKGRD(I)
                     ELSE
                        WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)') I,&
                        &IBKGRD(I)
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
                  END IF
               END IF
            END IF
         END DO
      ELSE  ! .NOT. L_BGSector
! ---       No BGSECTORs; First check for No HOURLY BACKGRND values
!           Set sector array index, I = 1
         I = 1
         IF (.NOT. L_BGFile(I)) THEN
!              No HOURLY BGFILE, check for non-HOURLY BACKGRND
            IF (.NOT. L_BGValues(I)) THEN
!                 WRITE Error Message: No BACKGRND values
               WRITE(DUMMY,'(''No BGVALUES!'')')
               CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
            ELSE IF (IBKGRD(I) < IBGMAX(I)) THEN
!                 WRITE Error Message: Not Enough BACKGRND values
               WRITE(DUMMY,'(''NumVals='',I4)') IBKGRD(I)
               CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
            END IF
         ELSE IF (L_BGFile(I)) THEN
!              Check for HOURLY BGFILE for this sector
            IF (LEN_TRIM(BKGRND_File(I)) == 0) THEN
!                 No HOURLY BG file available; check for complete BACKGRND values
               IF (.NOT. L_BGValues(I)) THEN
!                    WRITE Error Message: No BACKGRND values
                  WRITE(DUMMY,'(''No BGVALUES!'')')
                  CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
               ELSE IF (IBKGRD(1) < IBGMAX(I)) THEN
!                    WRITE Error Message: Incomplete BACKGRND values for this sector
                  WRITE(DUMMY,'(''NumVals='',I4)') IBKGRD(I)
                  CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
               END IF
            ELSE
!                 HOURLY BG file available; check for complete BACKGRND values
               IF (.NOT. L_BGValues(I)) THEN
!                    WRITE Warning Message: HOURLY BG file but No BGVALs available for this sector
                  WRITE(DUMMY,'(''BGVALs Msg'')')
                  CALL ERRHDL(PATH,MODNAM,'W','248',DUMMY)
               ELSE IF (IBKGRD(I) < IBGMAX(I)) THEN
!                    WRITE Error Message: Incomplete BACKGRND values for this sector
                  WRITE(DUMMY,'(''NumVals='',I4)') IBKGRD(I)
                  CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
               END IF
            END IF
         END IF
      END IF

! ---    Check for user-specified background units; apply default if needed
      IF (ISSTAT(41) /= 1) THEN
         IF (POLLUT == 'NO2' .or. POLLUT == 'SO2') THEN
            BackUnits = 'PPB'
         ELSE IF (POLLUT == 'CO') THEN
            BackUnits = 'PPM'
         ELSE
            BackUnits = 'UG/M3'
         END IF
      END IF
   END IF

   RETURN
END SUBROUTINE SRCQA

SUBROUTINE SOELUN
!***********************************************************************
!                 SOELUN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Elevation Units Option for Sources
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 22, 1994
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Elevation Units Switch
!
!        ERROR HANDLING:   Checks for Invalid Parameters;
!                          Checks for No Parameters;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'SOELUN'

   IF (IFC == 3) THEN
      IF (FIELD(3) == 'METERS') THEN
         SOELEV = 'METERS'
      ELSE IF (FIELD(3) == 'FEET') THEN
         SOELEV = 'FEET'
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203','SO_ELEV')
      END IF
   ELSE IF (IFC > 3) THEN
!        WRITE Error Message     ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message     ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
   END IF

   RETURN
END SUBROUTINE SOELUN

SUBROUTINE SOLOCA
!***********************************************************************
!                 SOLOCA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Location Card
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        MODIFIED:    Removed the FLAT restictrion for RLINE/RLINEXT
!                     sources. The restriction still remains for barrier
!                     and depressed RLINE sources
!                     Wood, 7/5/22
!
!        MODIFIED: Modified to reduce RLINE source inputs and added RLINEXT
!                  source type to handle full set of RLINE inputs.
!                  Wood, 03/18/2019
!
!        MODIFIED: To incorporate the RLINE source
!                  Wood, 07/20/2018
!
!        MODIFIED: To incorporate the buoyant line source
!                  Amec Foster Wheeler, 06/30/2015
!
!*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
!*                    new "PIT" source type.
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Type and Location
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA, ONLY: RLSOURCE
   USE BUOYANT_LINE

   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: INDEXS
   CHARACTER (LEN=12) :: SOID
   LOGICAL :: FOUND

!     Variable Initializations
   FOUND  = .FALSE.
   MODNAM = 'SOLOCA'

!     Check The Number Of The Fields
!*--- Modified to handle LINE, BUOYLINE, and RLINE sources
   IF (FIELD(4)=='LINE' .or. FIELD(4)=='BUOYLINE'&
   &.or. FIELD(4)=='RLINE') THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 8) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 9) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
!*--- Modified to handle RLINEXT sources
   ELSE IF (FIELD(4)=='RLINEXT') THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 10) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 11) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
   ELSE
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 6) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 7) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
   END IF
!*---

!     Read In The Data Fields and Assign to Arrays
!     Check for Previous Occurrence of This SRCID
!*    First check for length of SRCID field (<=12)
   IF ((LOCE(3)-LOCB(3)) <= 11) THEN
!*       Retrieve Source ID Character Substring
      SOID = FIELD(3)
! ---    Check for "reserved" source ID for 'BACKGROUND'
      IF (SOID == 'BACKGROUND' .or. SOID == 'BACKGRND') THEN
!*          WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','285',FIELD(3)(1:12))
         GO TO 999
      END IF
   ELSE
!*       WRITE Error Message:  Source ID Field is Too Long
      CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
      GO TO 999
   END IF

   CALL SINDEX(SRCID,NSRC,SOID,INDEXS,FOUND)

   IF (.NOT. FOUND) THEN
      ISRC = ISRC + 1
      IF (ISRC <= NSRC) THEN
         SRCID(ISRC)  = FIELD(3)
         SRCTYP(ISRC) = FIELD(4)

! ---       Allow for variations in the source type string
         IF (SRCTYP(ISRC) == 'OPENPIT'  .or.&
         &SRCTYP(ISRC) == 'OPEN_PIT' .or.&
         &SRCTYP(ISRC) == 'OPEN-PIT') THEN
! ---           Assign as 'OPENPIT' for future references
            SRCTYP(ISRC) = 'OPENPIT'
         END IF

! ---       Check for acceptable source types
         IF (SRCTYP(ISRC)=='POINT' .or.&
         &SRCTYP(ISRC)=='POINTCAP' .or.&
         &SRCTYP(ISRC)=='POINTHOR' .or.&
         &SRCTYP(ISRC)=='VOLUME' .or.&
         &SRCTYP(ISRC)=='AREA' .or.&
         &SRCTYP(ISRC)=='AREAPOLY' .or.&
         &SRCTYP(ISRC)=='AREACIRC' .or.&
         &SRCTYP(ISRC)=='LINE' .or.&
         &SRCTYP(ISRC)=='RLINE' .or.&
         &SRCTYP(ISRC)=='RLINEXT' .or.&
         &SRCTYP(ISRC)=='BUOYLINE' .or.&
         &SRCTYP(ISRC)=='OPENPIT' .or.&
         &SRCTYP(ISRC)=='SWPOINT') THEN

!*---          Modified to add handling of LINE, BUOYLINE source types
            IF (SRCTYP(ISRC)=='LINE' .or.&
            &SRCTYP(ISRC)=='BUOYLINE') THEN
               CALL STODBL(FIELD(5), ILEN_FLD, AXS1(ISRC), IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               CALL STODBL(FIELD(6), ILEN_FLD, AYS1(ISRC), IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source X2
               CALL STODBL(FIELD(7), ILEN_FLD, AXS2(ISRC), IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source Y2
               CALL STODBL(FIELD(8), ILEN_FLD, AYS2(ISRC), IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!*---          Modified to add handling of RLINE source type
            ELSE IF (SRCTYP(ISRC)=='RLINE') THEN
!                 Retrieve Source XSB
               CALL STODBL(FIELD(5), ILEN_FLD, RLSOURCE(ISRC)%XSB,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source YSB
               CALL STODBL(FIELD(6), ILEN_FLD, RLSOURCE(ISRC)%YSB,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source XSE
               CALL STODBL(FIELD(7), ILEN_FLD, RLSOURCE(ISRC)%XSE,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source YSE
               CALL STODBL(FIELD(8), ILEN_FLD, RLSOURCE(ISRC)%YSE,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!*---          Modified to add handling of RLINEXT source type
            ELSE IF (SRCTYP(ISRC)=='RLINEXT') THEN
!                 Retrieve Source XSB
               CALL STODBL(FIELD(5), ILEN_FLD, RLSOURCE(ISRC)%XSB,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source YSB
               CALL STODBL(FIELD(6), ILEN_FLD, RLSOURCE(ISRC)%YSB,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source ZSB
               CALL STODBL(FIELD(7), ILEN_FLD, RLSOURCE(ISRC)%ZSB,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source XSE
               CALL STODBL(FIELD(8), ILEN_FLD, RLSOURCE(ISRC)%XSE,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source YSE
               CALL STODBL(FIELD(9), ILEN_FLD, RLSOURCE(ISRC)%YSE,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
!                 Retrieve Source ZSE
               CALL STODBL(FIELD(10), ILEN_FLD, RLSOURCE(ISRC)%ZSE,&
               &IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
            ELSE
               CALL STODBL(FIELD(5), ILEN_FLD, AXS(ISRC), IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               CALL STODBL(FIELD(6), ILEN_FLD, AYS(ISRC), IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
            END IF

! ---          Read elevation field for each source type
! ---
!           RLINE source now has terrain, treat like other sources - Wood 7/6/2022
! ---          Read elevation for LINE or BUOYLINE source
!               IF (SRCTYP(ISRC).EQ.'LINE' .or.
!     &             SRCTYP(ISRC).EQ.'BUOYLINE') THEN
            IF (SRCTYP(ISRC)=='LINE' .or.&
            &SRCTYP(ISRC)=='BUOYLINE' .or.&
            &SRCTYP(ISRC)=='RLINE') THEN
               IF (IFC == 8) THEN
!                    No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0D0
                  IF (ELEV) THEN
!                       Write Warning Message for No Source Elevation with ELEV
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               ELSE IF (IFC == 9 .and. FLATSRCS .and.&
               &FIELD(9)=='FLAT') THEN
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
                  L_FLATSRC(ISRC) = .TRUE.
                  NUMFLAT = NUMFLAT + 1
! ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
                  CALL ERRHDL(PATH,MODNAM,'W','752',SOID)
               ELSE IF (IFC == 9) THEN
!                    Retrieve Source lower elevation
                  CALL STODBL(FIELD(9), ILEN_FLD, AZS(ISRC), IMIT)
!                    Check The Numerical Field
                  IF (IMIT /= 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
! ---                Check for missing source elevations, coded as -9999.0,
!                    and convert from FEET to METERS if needed
                  IF (AZS(ISRC) < -9998.99D0) THEN
!                       WRITE Error Message:  Source elevation is missing
                     CALL ERRHDL(PATH,MODNAM,'E','249',SOID)
                  ELSE IF (SOELEV == 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048D0
                  END IF
               END IF
!              Added RLINE source to IF-statment above with LINE and BL - Wood 7/6/2022
! ---          Read elevation for RLINE source
!               ELSE IF (SRCTYP(ISRC).EQ.'RLINE') THEN
! begin        Deleted flat restriction for RLINE source Wood 6-14-22
!                 IF(.NOT.FLAT) THEN
! ---                ERROR MESSAGE THAT RLINE SOURCE REQUIRES 'FLAT' MODELOPT
!                    CALL ERRHDL(PATH,MODNAM,'E','188','')
!                 END IF
!                  IF (IFC .EQ. 8 .and. ELEV) THEN
!                    Write Warning Message for No Source Elevation with ELEV
!                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
!                     L_FLATSRC(ISRC) = .TRUE.
!                     NUMFLAT = NUMFLAT + 1
!                     AZS(ISRC) = 0.0D0
!                  ELSE IF (IFC .EQ. 9 .and. FIELD(9) .EQ. 'FLAT') THEN
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option

!WSP --- Begin: D173 deleted erronious lines. WSP 7/25/2023
!WSP                      L_FLATSRC(ISRC) = .TRUE.
!WSP                      NUMFLAT = NUMFLAT + 1
!WSP                      AZS(ISRC) = 0.0D0
!WSP C ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
!WSP                         CALL ERRHDL(PATH,MODNAM,'W','752',SOID)
!WSP                   ELSE IF (IFC .EQ. 9) THEN
!WSP --- End: D173 deleted erronious lines. WSP 7/25/2023

!                    Retrieve Source lower elevation
!                     CALL STODBL(FIELD(9), ILEN_FLD, AZS(ISRC), IMIT)
!                    Check The Numerical Field
!                     IF (IMIT .NE. 1) THEN
!                        CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
!                     END IF
!                     IF (AZS(ISRC) .NE. 0.0D0) THEN
! ---                   ERROR MESSAGE THAT RLINE SOURCE REQUIRES Zs = FLAT or = 0.0
!                        CALL ERRHDL(PATH,MODNAM,'E','267',SOID)
!                     ELSE
! ---                   Source has been identified as a terrain-following
!                       source, to be simulated with 'FLAT' terrain option
!                        L_FLATSRC(ISRC) = .TRUE.
!                        NUMFLAT = NUMFLAT + 1
!                        AZS(ISRC) = 0.0D0
!                   END IF
!                  END IF !check field of RLINE source
! end        Deleted flat restriction for RLINE source Wood 6-14-22

! ---          Read elevation for RLINEXT source
            ELSE IF (SRCTYP(ISRC)=='RLINEXT') THEN

! begin        Deleted flat restriction for RLINEXT source Wood 6-14-22
!                  IF(.NOT.FLAT) THEN
! ---                ERROR MESSAGE THAT RLINEXT SOURCE REQUIRES 'FLAT' MODELOPT
!                     CALL ERRHDL(PATH,MODNAM,'E','188','')
!                  END IF

!                  IF (IFC .EQ. 10 .and. ELEV) THEN
!                    Write Warning Message for No Source Elevation with ELEV
!                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
!                     L_FLATSRC(ISRC) = .TRUE.
!                     NUMFLAT = NUMFLAT + 1
!                     AZS(ISRC) = 0.0D0
!                  ELSE IF (IFC .EQ. 11 .and. FIELD(11) .EQ. 'FLAT') THEN
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
!                     L_FLATSRC(ISRC) = .TRUE.
!                     NUMFLAT = NUMFLAT + 1
!                     AZS(ISRC) = 0.0D0
!                  ELSE IF (IFC .EQ. 11) THEN
!                    Retrieve Source lower elevation
!                     CALL STODBL(FIELD(11), ILEN_FLD, AZS(ISRC), IMIT)
!                    Check The Numerical Field
!                     IF (IMIT .NE. 1) THEN
!                        CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
!                     END IF
!                     IF (AZS(ISRC) .NE. 0.0D0) THEN
! ---                   ERROR MESSAGE THAT RLINE SOURCE REQUIRES Zs = FLAT or = 0.0
!                        CALL ERRHDL(PATH,MODNAM,'E','267',SOID)
!                     ELSE
! ---                   Source has been identified as a terrain-following
!                       source, to be simulated with 'FLAT' terrain option
!                        L_FLATSRC(ISRC) = .TRUE.
!                        NUMFLAT = NUMFLAT + 1
!                        AZS(ISRC) = 0.0D0
!                     END IF
! end        Deleted flat restriction for RLINEXT source Wood 6-14-22

! begin           RLINEXT source now has terrain, treat like other source - Wood 7/5/2022
               IF (IFC == 10) THEN
!                    No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0D0
                  IF (ELEV) THEN
!                       Write Warning Message for No Source Elevation with ELEV
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               ELSE IF (IFC == 11 .and. FLATSRCS .and.&
               &FIELD(11)=='FLAT') THEN
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
                  L_FLATSRC(ISRC) = .TRUE.
                  NUMFLAT = NUMFLAT + 1
                  AZS(ISRC) = 0.0D0
! ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
                  CALL ERRHDL(PATH,MODNAM,'W','752',SOID)

               ELSE IF (IFC == 11) THEN
!                    Retrieve Source lower elevation
                  CALL STODBL(FIELD(11), ILEN_FLD, AZS(ISRC), IMIT)
!                    Check The Numerical Field
                  IF (IMIT /= 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
! ---                Check for missing source elevations, coded as -9999.0,
!                    and convert from FEET to METERS if needed
                  IF (AZS(ISRC) < -9998.99D0) THEN
!                       WRITE Error Message:  Source elevation is missing
                     CALL ERRHDL(PATH,MODNAM,'E','249',SOID)
                  ELSE IF (SOELEV == 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048D0
                  END IF
               END IF
! end           RLINEXT source now has terrain, treat like other source - Wood 7/5/2022
!                 END IF !check field of RLINEXT source

! ---          Read elevation for source types other than LINE, BUOYLINE, RLINE, or RLINEXT
            ELSE
               IF (IFC == 7 .and. FLATSRCS .and.&
               &FIELD(7) == 'FLAT') THEN
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
                  L_FLATSRC(ISRC) = .TRUE.
                  NUMFLAT = NUMFLAT + 1
! ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
                  CALL ERRHDL(PATH,MODNAM,'W','752',SOID)
               ELSE IF (IFC == 7) THEN
!                    Retrieve Source Elevation From Inputs
                  CALL STODBL(FIELD(7), ILEN_FLD, AZS(ISRC), IMIT)
!                    Check The Numerical Field
                  IF (IMIT /= 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
! ---                Check for missing source elevations, coded as -9999.0,
!                    and convert from FEET to METERS if needed
                  IF (AZS(ISRC) < -9998.99D0) THEN
!                       WRITE Error Message:  Source elevation is missing
                     CALL ERRHDL(PATH,MODNAM,'E','249',SOID)
                  ELSE IF (SOELEV == 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048D0
                  END IF
               ELSE
!                    No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0D0
                  IF (ELEV) THEN
!                       Write Warning Message for No Source Elevation with ELEV
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               END IF
            END IF  ! End Read elevation field for each source type

         ELSE
!              Error Message: Invalid Source Type
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCTYP')
            GO TO 999
         END IF ! End Check for acceptable source types

         ISET = ISRC
         NUMSRC = NUMSRC + 1

! Multiple_BuoyLines_D41_Wood
! ---       Determine the total number of BUOYLINE sources
!           Variable name changed from NBLINES to NBLTOTAL
         IF( SRCTYP(ISRC) == 'BUOYLINE' )THEN
            NBLTOTAL = NBLTOTAL + 1
         ENDIF

      ELSE
!           This shouldn't occur since limits are dynamically allocated
!           WRITE Error Message    ! Number of Sources Exceeds NSRC Parameter
         WRITE(DUMMY,'(''NSRC='',I7)') NSRC
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         GO TO 999
      END IF
   ELSE
!        WRITE Error Message    ! Source Location Has Already Been Identified
      CALL ERRHDL(PATH,MODNAM,'E','310',SOID)
   END IF

999 RETURN
END SUBROUTINE SOLOCA

SUBROUTINE SOPARM
!***********************************************************************
!                 SOPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameter Card
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To process parameters for the RLINE source
!                    Wood, 07/20/2018
!
!        MODIFIED:   To process parameters for the buoyant line source
!                    Amec Foster Wheeler, 06/30/2015
!
!        MODIFIED:   To allow for additional parameters on area source
!                    parameter cards for new algorithm - 7/7/93
!
!*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
!*                    new "PIT" source type.
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameters
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, ISDX
   LOGICAL :: FOUND
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   FOUND  = .FALSE.
   MODNAM = 'SOPARM'
   TEMP   = 0.0D0

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC == 3) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     Search For The Source ID Index
   CALL SINDEX(SRCID,NSRC,FIELD(3),ISDX,FOUND)

   IF (FOUND) THEN
!        Check for Previous SRCPARAM Card for This Source
      IF (SOPCRD(ISDX) == 'Y') THEN
!           WRITE Error Message: Duplicate SRCPARAM Card
         CALL ERRHDL(PATH,MODNAM,'E','315',SRCID(ISDX))
         GO TO 999
      ELSE
         SOPCRD(ISDX) = 'Y'
      END IF
!        Assign The Parameter Arrays
      DO I = 4, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,TEMP(I-3),IMIT)
!           Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
      END DO
      IF (SRCTYP(ISDX) == 'POINT' .or.&
      &SRCTYP(ISDX) == 'POINTCAP' .or.&
      &SRCTYP(ISDX) == 'POINTHOR') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 8) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 8) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL PPARM(ISDX,TEMP)
      ELSE IF (SRCTYP(ISDX) == 'VOLUME') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 7) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 7) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL VPARM(ISDX,TEMP)
      ELSE IF (SRCTYP(ISDX) == 'AREA') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 6) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 9) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL APARM(ISDX,TEMP)
      ELSE IF (SRCTYP(ISDX) == 'AREAPOLY') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 6) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 7) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL APPARM(ISDX,TEMP)
      ELSE IF (SRCTYP(ISDX) == 'AREACIRC') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 6) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 8) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL ACPARM(ISDX,TEMP)
!*       Get Source Parameters for the OPENPIT source
      ELSE IF (SRCTYP(ISDX) == 'OPENPIT') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 8) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 9) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL OPARM(ISDX,TEMP)
!---     Added to handle LINE source types
      ELSE IF (SRCTYP(ISDX) == 'LINE') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 6) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 7) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL LPARM(ISDX,TEMP)
!---     Added to handle RLINE source types
      ELSE IF (SRCTYP(ISDX) == 'RLINE') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 6) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 7) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL RLPARM(ISDX,TEMP)
!---     Get source parameters for the RLINEXT source type
      ELSE IF (SRCTYP(ISDX) == 'RLINEXT') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 7) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 7) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
!           Process RLINE parameters                        ---   CALL RLPARM
         CALL RLPARM(ISDX,TEMP)
!---     Get source parameters for BUOYANT LINE source type
      ELSE IF (SRCTYP(ISDX) == 'BUOYLINE') THEN
         IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 5) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 5) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL BLPARM(ISDX,TEMP)
!        CRT 3/25/2022 D113 Updated for sidewash
      ELSE IF (SRCTYP(ISDX) == 'SWPOINT') THEN
!           Check for ALPHA option - required
         IF (.NOT. L_ALPHA) THEN
            CALL ERRHDL(PATH,MODNAM,'E','198','SWPOINT')
            GO TO 999
         ELSE IF (IFC == 3) THEN
!              Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC < 9) THEN
!              Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC > 9) THEN
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         CALL SWPARM(ISDX,TEMP)
      END IF
   ELSE
!        WRITE Error Message: Source Location Has Not Been Identified Yet
      CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
   END IF

999 RETURN
END SUBROUTINE SOPARM

SUBROUTINE PPARM(ISDX,TEMP)
!***********************************************************************
!                 PPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for POINT Sources
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To assign default factor to adjust initial diameter
!                    of plume for capped stacks for use in the PRIME
!                    algorithm, for BETA-test draft option.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'PPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   ATS(ISDX) = TEMP(3)
   AVS(ISDX) = TEMP(4)
   ADS(ISDX) = TEMP(5)

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 600.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 600M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (ATS(ISDX) == 0.0D0) THEN
!        Set Temperature to Small Negative Value for Ambient Releases
      ATS(ISDX) = -1.0D-5
   ELSE IF (ATS(ISDX) > 2000.0D0) THEN
!        WRITE Warning Message:  Exit Temp. > 2000K
      CALL ERRHDL(PATH,MODNAM,'W','320',' TS ')
   ELSE IF ((ATS(ISDX)>0.0D0) .and. (ATS(ISDX)<200.0D0)) THEN
!*       Exit temp<200K (about -100F); Incorrect units may have been
!*       used or ATS and AVS may have been switched;
!*       WRITE Fatal Error Message
      CALL ERRHDL(PATH,MODNAM,'E','320',' TS ')
      RUNERR = .TRUE.
   END IF

   IF (AVS(ISDX) < 0.0D0) THEN
!        WRITE Warning Message:  Negative or Zero Exit Velocity
      CALL ERRHDL(PATH,MODNAM,'W','325',SRCID(ISDX))
!        Set to Small Value to Avoid Zero-divide and Underflow
      AVS(ISDX) = 1.0D-5
   ELSE IF (AVS(ISDX) < 1.0D-5) THEN
!        Set to Small Value to Avoid Zero-divide and Underflow
      AVS(ISDX) = 1.0D-5
   ELSE IF (AVS(ISDX) > 50.0D0) THEN
!        WRITE Warning Message:  Exit Velocity > 50.0 m/s
      CALL ERRHDL(PATH,MODNAM,'W','320',' VS ')
   END IF

   IF (ADS(ISDX) < 0.0D0) THEN
!        WRITE Warning Message:  Negative Stack Diameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' DS ')
   ELSE IF (ADS(ISDX) < 1.0D-5) THEN
!        Set to Small Value to Avoid Zero-divide and Underflow
      ADS(ISDX) = 1.0D-5
   ELSE IF (ADS(ISDX) > 20.0D0) THEN
!        WRITE Warning Message:  Large Stack Diameter (> 20m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' DS ')
   END IF

!     Assign default factor to adjust stack diameter for capped stack
!     releases at 2.0 (i.e. cap doubles initial diameter of plume) for
!     use in the PRIME algorithm for BETA-test draft option.
   IF (SRCTYP(ISDX) == 'POINTCAP') THEN
      ADSFACT(ISDX) = 2.0D0
   ELSE
      ADSFACT(ISDX) = 1.0D0
   END IF

   RETURN
END SUBROUTINE PPARM

SUBROUTINE VPARM(ISDX,TEMP)
!***********************************************************************
!                 VPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for VOLUME Sources
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'VPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   ASYINI(ISDX) = TEMP(3)
   ASZINI(ISDX) = TEMP(4)

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

!MGS Removed warning when HS >100m for aircraft souces (D151 - WSP 5/25/2023)
   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
!MGS      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
   ELSE IF((AHS(ISDX) > 100.0D0) .and.&
   &(.NOT. ARCFT)) THEN
!        WRITE Warning Message:  Large Release Height (> 100M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (ASYINI(ISDX) < 0.0D0) THEN
!        WRITE Warning Message:  Negative Initial Lateral Parameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' SYINIT ')
   ELSE IF (ASYINI(ISDX) < 1.0D-5) THEN
!        WRITE Warning Message:  Small Initial Lateral Parameter
      CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      ASYINI(ISDX) = 1.0D-5
   ELSE IF (ASYINI(ISDX) > 200.0D0) THEN
!        WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
   END IF

   IF (ASZINI(ISDX) < 0.0D0) THEN
!        WRITE Warning Message:  Negative Initial Vertical Parameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
   ELSE IF (ASZINI(ISDX) < 1.0D-5) THEN
!        WRITE Warning Message:  Small Initial Lateral Parameter
      CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      ASZINI(ISDX) = 1.0D-5
   ELSE IF (ASZINI(ISDX) > 200.0D0) THEN
!        WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
   END IF

   RETURN
END SUBROUTINE VPARM

SUBROUTINE APARM(ISDX,TEMP)
!***********************************************************************
!                 APARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for Rectangular AREA Sources
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To allow for additional parameters on area source
!                    parameter cards for new algorithm - 7/7/93
!
!        MODIFIED:   Corrected IF-BLOCK for error checking - 7/21/94
!
!        MODIFIED BY Roger Brode, PES (modified data structure for
!                    AXVERT and AYVERT for consistency with other
!                    2-D source arrays) - 8/15/95
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'APARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   IF (IFC == 6) THEN
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = AXINIT(ISDX)
      AANGLE(ISDX) = 0.0D0
      ASZINI(ISDX) = 0.0D0
   ELSE IF (IFC == 7) THEN
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = TEMP(4)
      AANGLE(ISDX) = 0.0D0
      ASZINI(ISDX) = 0.0D0
   ELSE IF (IFC == 8) THEN
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = TEMP(4)
      AANGLE(ISDX) = TEMP(5)
      ASZINI(ISDX) = 0.0D0
   ELSE IF (IFC == 9) THEN
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = TEMP(4)
      AANGLE(ISDX) = TEMP(5)
      ASZINI(ISDX) = TEMP(6)
   END IF

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

!MGS Removed warning when HS >100m for aircraft souces (D151 - WSP 5/25/2023)
   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
!MGS      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
   ELSE IF ((AHS(ISDX) > 100.0D0) .and.&
   &(.NOT. ARCFT)) THEN
!        WRITE Warning Message:  Large Release Height (> 100M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (AXINIT(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Area Size
      CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
   ELSE IF (AXINIT(ISDX) < 1.0D-5) THEN
!        WRITE Warning Message:  Small Source Area
      CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      AXINIT(ISDX) = 1.0D-5
   ELSE IF (AXINIT(ISDX) > 2000.0D0) THEN
!        WRITE Warning Message:  Large Source Area (> 2000m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
   END IF

   IF (AYINIT(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Area Size
      CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
   ELSE IF (AYINIT(ISDX) < 1.0D-5) THEN
!        WRITE Warning Message:  Small Source Area
      CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      AYINIT(ISDX) = 1.0D-5
   ELSE IF (AYINIT(ISDX) > 2000.0D0) THEN
!        WRITE Warning Message:  Large Source Area (> 2000m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
   END IF

   IF (DABS(AANGLE(ISDX)) > 180.0D0 ) THEN
!        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
      CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
   END IF

   IF (ASZINI(ISDX) < 0.0D0) THEN
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
   ELSE IF (ASZINI(ISDX) < 1.0D-5) THEN
!*       Set to Small Value to Avoid Zero-divide and Underflow
      ASZINI(ISDX) = 1.0D-5
   ELSE IF (ASZINI(ISDX) > 200.0D0) THEN
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
   END IF

!     Check for aspect ratio (length/width) > 100
   IF (AYINIT(ISDX)/AXINIT(ISDX) > 100.00001D0 .or.&
   &AXINIT(ISDX)/AYINIT(ISDX) > 100.00001D0) THEN
!        WRITE Warning Message: Aspect ratio > 100 for area source
      CALL ERRHDL(PATH,MODNAM,'W','391',SRCID(ISDX))
   END IF

!     Set Number of Vertices (4 for Rectangular Source)
   NVERTS(ISDX) = 4

!     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
!     Vertices Start with the "Southwest" Corner and Are Defined
!     Clockwise.  The First Vertex is Repeated as the Last Vertex.

   AXVERT(1,ISDX) = AXS(ISDX)
   AYVERT(1,ISDX) = AYS(ISDX)

   AXVERT(2,ISDX) = AXVERT(1,ISDX) +&
   &(AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
   AYVERT(2,ISDX) = AYVERT(1,ISDX) +&
   &(AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

   AXVERT(3,ISDX) = AXVERT(2,ISDX) +&
   &(AXINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))
   AYVERT(3,ISDX) = AYVERT(2,ISDX) -&
   &(AXINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))

   AXVERT(4,ISDX) = AXVERT(3,ISDX) -&
   &(AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
   AYVERT(4,ISDX) = AYVERT(3,ISDX) -&
   &(AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

   AXVERT(5,ISDX) = AXS(ISDX)
   AYVERT(5,ISDX) = AYS(ISDX)

!     Determine coordinates for center of rectangular source
   AXCNTR(ISDX) = 0.0D0
   AYCNTR(ISDX) = 0.0D0
   DO I = 1, NVERTS(ISDX)
      AXCNTR(ISDX) = AXCNTR(ISDX) + AXVERT(I,ISDX)
      AYCNTR(ISDX) = AYCNTR(ISDX) + AYVERT(I,ISDX)
   END DO
   AXCNTR(ISDX) = AXCNTR(ISDX)/DBLE(NVERTS(ISDX))
   AYCNTR(ISDX) = AYCNTR(ISDX)/DBLE(NVERTS(ISDX))

   RETURN
END SUBROUTINE APARM

SUBROUTINE APPARM(ISDX,TEMP)
!***********************************************************************
!                 APPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for AREAPOLY Sources
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    August 14, 1995
!
!        MODIFIED:   Removed NINT(int_variable).  R. Brode, PES, 11/21/97
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'APPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   NVERTS(ISDX) = IDNINT(TEMP(3))
   IF (IFC == 7) THEN
      ASZINI(ISDX) = TEMP(4)
   ELSE
      ASZINI(ISDX) = 0.0D0
   END IF

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 100.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 100M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (ASZINI(ISDX) < 0.0D0) THEN
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
   ELSE IF (ASZINI(ISDX) < 1.0D-5) THEN
!*       Set to Small Value to Avoid Zero-divide and Underflow
      ASZINI(ISDX) = 1.0D-5
   ELSE IF (ASZINI(ISDX) > 200.0D0) THEN
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
   END IF

   IF (NVERTS(ISDX) < 3) THEN
!        WRITE Error Message:  Not Enough Vertices
      CALL ERRHDL(PATH,MODNAM,'E','380',' NVERT ')
   ELSE IF (NVERTS(ISDX) > NVMAX-8) THEN
!        WRITE Error Message:  Too Many Vertices
      CALL ERRHDL(PATH,MODNAM,'W','406',SRCID(ISDX))
   END IF

   RETURN
END SUBROUTINE APPARM

SUBROUTINE ARVERT
!***********************************************************************
!                 ARVERT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Vertices for AREAPOLY Sources
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:    August 15, 1995
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Area Sources Vertices
!
!        CALLED FROM:   SOCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: K, ISDX
   DOUBLE PRECISION :: DNUMX, DNUMY
   CHARACTER (LEN=12) :: SOID
   LOGICAL :: FOUND

!     Variable Initializations
   FOUND  = .FALSE.
   MODNAM = 'ARVERT'

!     Get The Source ID, and Check for Previous Occurrence
!     of This SRCID;
!*    First check for length of SRCID field (<=12)
   IF ((LOCE(3)-LOCB(3)) <= 11) THEN
!*       Retrieve Source ID Character Substring
      SOID = FIELD(3)
   ELSE
!*       WRITE Error Message:  Source ID Field is Too Long
      CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
      GO TO 999
   END IF

!     Search For The Index
   CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
   IF (FOUND) THEN
      ISET = IWRK2(ISDX,10)
      DO K = 4, IFC-1, 2
!           Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUMX,IMIT)
!           Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         CALL STODBL(FIELD(K+1),ILEN_FLD,DNUMY,IMIT)
!           Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF

         ISET = ISET + 1
         IF (ISET == 1) THEN
!              Compare First Vertex to Source Location
            IF (DNUMX /= AXS(ISDX)) THEN
               CALL ERRHDL(PATH,MODNAM,'E','262',SRCID(ISDX))
            END IF
            IF (DNUMY /= AYS(ISDX)) THEN
               CALL ERRHDL(PATH,MODNAM,'E','262',SRCID(ISDX))
            END IF
         END IF

         IF (ISET <= NVERTS(ISDX)+1) THEN
!              Assign The Field
            AXVERT(ISET,ISDX) = DNUMX
            AYVERT(ISET,ISDX) = DNUMY
         ELSE
!              WRITE Error Message: Too Many Vertices For This Source
!              (this should not happen since arrays are allocated at runtime0
            CALL ERRHDL(PATH,MODNAM,'E','264',SRCID(ISDX))
         END IF
      END DO
      IWRK2(ISDX,10) = ISET
   ELSE
!        WRITE Error Message     ! Source Location Has Not Been Identified
      CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
   END IF

999 RETURN
END SUBROUTINE ARVERT

SUBROUTINE ACPARM(ISDX,TEMP)
!***********************************************************************
!                 ACPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for AREACIRC Sources
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 15, 1995
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'ACPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   RADIUS(ISDX) = TEMP(3)
   IF (IFC >= 7) THEN
! ---    Assign NVERTS for this source
      NVERTS(ISDX) = IDNINT(TEMP(4))
   ELSE
      NVERTS(ISDX) = 20
   END IF
   IF (IFC == 8) THEN
      ASZINI(ISDX) = TEMP(5)
   ELSE
      ASZINI(ISDX) = 0.0D0
   END IF

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 100.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 100M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (RADIUS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Radius
      CALL ERRHDL(PATH,MODNAM,'E','209',' RADIUS ')
   ELSE IF (RADIUS(ISDX) <= 0.5D0) THEN
!        WRITE Error Message:  Invalid Value for Radius
      CALL ERRHDL(PATH,MODNAM,'E','203',' RADIUS ')
   ELSE IF (RADIUS(ISDX) > 10000.0D0) THEN
!        WRITE Warning Message:  Large Radius (> 10000M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' RADIUS ')
   END IF

   IF (ASZINI(ISDX) < 0.0D0) THEN
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
   ELSE IF (ASZINI(ISDX) < 1.0D-5) THEN
!*       Set to Small Value to Avoid Zero-divide and Underflow
      ASZINI(ISDX) = 1.0D-5
   ELSE IF (ASZINI(ISDX) > 200.0D0) THEN
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
   END IF

   IF (NVERTS(ISDX) < 3) THEN
!        WRITE Error Message:  Not Enough Vertices
      CALL ERRHDL(PATH,MODNAM,'E','380',' NVERT ')
      GO TO 999
   ELSE IF (NVERTS(ISDX) > NVMAX+1) THEN
! ---    Adjust NVMAX based on user-specified NVERTS
      NVMAX = NVERTS(ISDX) + 1
   END IF

!     Setup Vertices for Circular Area
   CALL GENCIR(ISDX)

!     Set coordinates for center of circular source
   AXCNTR(ISDX) = AXS(ISDX)
   AYCNTR(ISDX) = AYS(ISDX)

999 RETURN
END SUBROUTINE ACPARM

SUBROUTINE GENCIR(ISDX)
!***********************************************************************
!                 GENCIR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generates Vertices for Circular Area Source
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:    September 15, 1995
!
!        MODIFIED:   Corrected variable type from INTEGER to REAL
!                    for NEWRAD.  R.W. Brode, EPA, 11/1/06
!
!        INPUTS:  Center of circle
!                 Radius of circle
!                 Number of vertices
!
!        OUTPUTS: Arrays of vertices
!
!        CALLED FROM:   ACPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, ISDX, NSIDES
   DOUBLE PRECISION :: ANG, ANGINC, AREA, TRIAREA, OPP, NEWRAD

!     Variable Initializations
   MODNAM = 'GENCIR'

   NSIDES = NVERTS(ISDX)
   ANGINC = 360.0D0/DBLE(NSIDES)
   ANG = 0.0D0

!     Calculate New Radius That Will Provide An Equal-Area Polygon
   AREA = PI * RADIUS(ISDX) * RADIUS(ISDX)
   TRIAREA = AREA/DBLE(NSIDES)
   OPP = DSQRT(TRIAREA * DTAN(ANGINC/(2.0D0*RTODEG)) )
   NEWRAD = OPP / (DSIN(ANGINC/(2.0D0*RTODEG)) )

!     Generate Vertices for Circular Area of NSIDES
   DO I = 1, NSIDES
      IF (I /= 1) ANG = ANG+ANGINC
      AXVERT(I,ISDX) = (NEWRAD * DSIN(ANG/RTODEG)) + AXS(ISDX)
      AYVERT(I,ISDX) = (NEWRAD * DCOS(ANG/RTODEG)) + AYS(ISDX)
   END DO

!     Repeat First Vertex as Last Vertex to Close the AREACIRC source
   AXVERT(NSIDES+1,ISDX) = AXVERT(1,ISDX)
   AYVERT(NSIDES+1,ISDX) = AYVERT(1,ISDX)

!     Assign SQRT(area) to AXINIT and AYINIT; equivalent values
!     of AXINIT and AYINIT will be used to calculate area of source
   AXINIT(ISDX) = DSQRT( area )
   AYINIT(ISDX) = DSQRT( area )

   RETURN
END SUBROUTINE GENCIR

SUBROUTINE OPARM(ISDX,TEMP)
!***********************************************************************
!                 OPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for OPENPIT Sources
!
!        PROGRAMMER: Jayant Hardikar, Roger Brode
!                    (based on APARM - Jeff Wang/Roger Brode)
!
!        DATE:       July 19, 1994
!
!        MODIFIED BY Roger Brode, PES (modified data structure for
!                    AXVERT and AYVERT for consistency with other
!                    2-D source arrays) - 8/15/95
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, ISDX
   DOUBLE PRECISION :: TEMP(IFMAX), EFFDEP

!     Variable Initializations
   MODNAM = 'OPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   AXINIT(ISDX) = TEMP(3)
   AYINIT(ISDX) = TEMP(4)
   AVOLUM(ISDX) = TEMP(5)
   AANGLE(ISDX) = 0.0D0
   IF (IFC == 9) THEN
      AANGLE(ISDX) = TEMP(6)
   END IF

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 200.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 200M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (AXINIT(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Area Size
      CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
   ELSE IF (AXINIT(ISDX) < 1.0D-5) THEN
!        WRITE Warning Message:  Small Source Area
      CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      AXINIT(ISDX) = 1.0D-5
   ELSE IF (AXINIT(ISDX) > 2000.0D0) THEN
!        WRITE Warning Message:  Large Source Area (> 2000m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
   END IF

   IF (AYINIT(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Area Size
      CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
   ELSE IF (AYINIT(ISDX) < 1.0D-5) THEN
!        WRITE Warning Message:  Small Source Area
      CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      AYINIT(ISDX) = 1.0D-5
   ELSE IF (AYINIT(ISDX) > 2000.0D0) THEN
!        WRITE Warning Message:  Large Source Area (> 2000m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
   END IF

   IF (DABS(AANGLE(ISDX)) > 180.0D0 ) THEN
!        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
      CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
   END IF

   IF (AVOLUM(ISDX) <= 0.0D0) THEN
!        WRITE Error Message: Open-Pit Volume is less than
!        or equal to zero
      CALL ERRHDL(PATH,MODNAM,'E','209',' AVOLUM ')
   END IF

!     Check for aspect ratio (length/width) > 10
   IF (AYINIT(ISDX)/AXINIT(ISDX) > 10.0D0 .or.&
   &AXINIT(ISDX)/AYINIT(ISDX) > 10.0D0) THEN
!        WRITE Warning Message: Aspect ratio > 10 for pit source
      CALL ERRHDL(PATH,MODNAM,'W','392',SRCID(ISDX))
   END IF

!     Check for Release Height > Effective Depth
   EFFDEP = AVOLUM(ISDX)/(AXINIT(ISDX)*AYINIT(ISDX))
   IF (AHS(ISDX) > EFFDEP) THEN
!        WRITE Error Message: Release Height is greater than Effective Depth
      CALL ERRHDL(PATH,MODNAM,'E','322',SRCID(ISDX))
   END IF

!     Set Number of Vertices (4 for Rectangular Source)
   NVERT = 4

!     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
!     Vertices Start with the "Southwest" Corner and Are Defined
!     Clockwise.  The First Vertex is Repeated as the Last Vertex.

   AXVERT(1,ISDX) = AXS(ISDX)
   AYVERT(1,ISDX) = AYS(ISDX)

   AXVERT(2,ISDX) = AXVERT(1,ISDX) +&
   &(AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
   AYVERT(2,ISDX) = AYVERT(1,ISDX) +&
   &(AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

   AXVERT(3,ISDX) = AXVERT(2,ISDX) +&
   &(AXINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))
   AYVERT(3,ISDX) = AYVERT(2,ISDX) -&
   &(AXINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))

   AXVERT(4,ISDX) = AXVERT(3,ISDX) -&
   &(AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
   AYVERT(4,ISDX) = AYVERT(3,ISDX) -&
   &(AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

   AXVERT(5,ISDX) = AXS(ISDX)
   AYVERT(5,ISDX) = AYS(ISDX)

!*    Determine the angle of long pit dimension with North
   IF (AYINIT(ISDX) >= AXINIT(ISDX)) THEN
      AALPHA(ISDX) = AANGLE(ISDX)
   ELSE IF (AXINIT(ISDX) > AYINIT(ISDX)) THEN
      AALPHA(ISDX) = AANGLE(ISDX) + 90.0D0
   END IF

!*    Calculate the effective pit depth
   APDEFF(ISDX) = AVOLUM(ISDX) / (AXINIT(ISDX) * AYINIT(ISDX))

!*    Calculate Initial Sigma-Z
   ASZINI(ISDX) = APDEFF(ISDX) / 4.3D0

!     Determine coordinates for center of rectangular source
   AXCNTR(ISDX) = 0.0D0
   AYCNTR(ISDX) = 0.0D0
   DO I = 1, NVERT
      AXCNTR(ISDX) = AXCNTR(ISDX) + AXVERT(I,ISDX)
      AYCNTR(ISDX) = AYCNTR(ISDX) + AYVERT(I,ISDX)
   END DO
   AXCNTR(ISDX) = AXCNTR(ISDX)/DBLE(NVERT)
   AYCNTR(ISDX) = AYCNTR(ISDX)/DBLE(NVERT)

   RETURN
END SUBROUTINE OPARM

!CRT  D063 Platform Downwash
SUBROUTINE PLATFM
!***********************************************************************
!                 PLATFM Subroutine - AERMOD
!
!        PURPOSE: Processes Platform Parameters for POINT Sources
!                 subject to platform downwash
!
!        PROGRAMMER: Clint Tillerson
!
!        DATE:    January 18, 2012
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Platform Card
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, ISDX
   LOGICAL :: FOUND
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   FOUND  = .FALSE.
   MODNAM = 'PLATFM'
   TEMP   = 0.0D0

!     Check The Number Of The Fields
   IF (IFC <= 4) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     Search For The Source ID Index
   CALL SINDEX(SRCID,NSRC,FIELD(3),ISDX,FOUND)

   IF (FOUND) THEN

!        Check for POINT, POINTHOR, POINTCAP source type
      IF (SRCTYP(ISDX) /= 'POINT' .and.&
      &SRCTYP(ISDX) /= 'POINTHOR' .and.&
      &SRCTYP(ISDX) /= 'POINTCAP') THEN
!           WRITE Error Message: PLATFORM only applies to POINT,
!           POINTHOR, and POINTCAP sources
         CALL ERRHDL(PATH,MODNAM,'E','631',SRCID(ISDX))
         GO TO 999
      END IF

!        Check for Previous PLATFORM Card for This Source
      IF (SOPLAT(ISDX) == 'Y') THEN
!           WRITE Error Message: Duplicate PLATFORM Keyword for source
         CALL ERRHDL(PATH,MODNAM,'E','632',SRCID(ISDX))
         GO TO 999
      ELSE
         SOPLAT(ISDX) = 'Y'
      END IF

!        Assign The Parameter Arrays
      DO I = 4, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,TEMP(I-3),IMIT)
!           Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
      END DO
!        Set platform parameters
      PLATELV(ISDX) = TEMP(1)    ! Platform base elevation above sea-level
      PLATHB(ISDX) = TEMP(2)     ! Platform building height above platform base elevation
      PLATWB(ISDX) = TEMP(3)     ! Platform building width

!        Set platform flag to true only if building width and height are greater the 0.0 (0.001)
      IF (PLATHB(ISDX) > 0.001D0 .and.&
      &PLATWB(ISDX) > 0.001D0) THEN
         OSPLAT(ISDX) = .TRUE.
      END IF

   ELSE
!        WRITE Error Message    ! Source Location Has Not Been Identified Yet
      CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
   END IF

999 RETURN
END SUBROUTINE PLATFM


SUBROUTINE LPARM(ISDX,TEMP)
!***********************************************************************
!                 LPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for LINE Sources
!                 Modified from APARM
!
!        PROGRAMMER: Dan Derby
!
!        DATE:    August 13, 2009
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   SAVE
   INTEGER :: I, ISDX
   DOUBLE PRECISION :: TEMP(IFMAX), XLEN, YLEN
! Unused INTEGER :: J

!     Variable Initializations
   MODNAM = 'LPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   AWIDTH(ISDX) = TEMP(3)
   IF (IFC == 7) THEN
      ASZINI(ISDX) = TEMP(4)
   ELSE
      ASZINI(ISDX) = 0.0D0
   END IF

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 100.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 100M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   END IF


   IF (ASZINI(ISDX) < 0.0D0) THEN
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
   ELSE IF (ASZINI(ISDX) < 1.0E-5) THEN
!*       Set to Small Value to Avoid Zero-divide and Underflow
      ASZINI(ISDX) = 1.0D-5
   ELSE IF (ASZINI(ISDX) > 200.0D0) THEN
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
   END IF

   IF (AWIDTH(ISDX) < 1.0D0) THEN
!        WRITE Warning Message:  Negative Width Parameter
      CALL ERRHDL(PATH,MODNAM,'E','320',' WIDTH ')
   END IF

   XLEN = AXS2(ISDX) - AXS1(ISDX)
   YLEN = AYS2(ISDX) - AYS1(ISDX)

! --- Use center of LINE source for "origin"
   AXS(ISDX) = (AXS1(ISDX) + AXS2(ISDX))*0.5D0
   AYS(ISDX) = (AYS1(ISDX) + AYS2(ISDX))*0.5D0

   AXINIT(ISDX) = AWIDTH(ISDX)
   AYINIT(ISDX) = DSQRT( XLEN**2 + YLEN**2 )
   AANGLE(ISDX) = DATAN2(XLEN,YLEN) * RTODEG

!     Check for aspect ratio (length/width) > 100
   IF (AYINIT(ISDX)/AXINIT(ISDX) > 100.00001D0 .or.&
   &AXINIT(ISDX)/AYINIT(ISDX) > 100.00001D0) THEN
!        WRITE Warning Message: Aspect ratio > 100 for line source
      CALL ERRHDL(PATH,MODNAM,'W','390',SRCID(ISDX))
   END IF

!     Set Number of Vertices (4 for Rectangular Source)
   NVERTS(ISDX) = 4

!     Set Coordinates of Vertices for Equivalent Rectangular Area.
!     Vertices Start with the "Southwest" Corner and Are Defined
!     Clockwise.  The First Vertex is Repeated as the Last Vertex.

   AXVERT(1,ISDX) = AXS1(ISDX) -&
   &(AXINIT(ISDX)/2.0D0)*DCOS(AANGLE(ISDX)*DTORAD)
   AYVERT(1,ISDX) = AYS1(ISDX) +&
   &(AXINIT(ISDX)/2.0D0)*DSIN(AANGLE(ISDX)*DTORAD)

   AXVERT(2,ISDX) = AXVERT(1,ISDX) +&
   &(AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
   AYVERT(2,ISDX) = AYVERT(1,ISDX) +&
   &(AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

   AXVERT(3,ISDX) = AXVERT(2,ISDX) +&
   &(AXINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))
   AYVERT(3,ISDX) = AYVERT(2,ISDX) -&
   &(AXINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))

   AXVERT(4,ISDX) = AXVERT(3,ISDX) -&
   &(AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
   AYVERT(4,ISDX) = AYVERT(3,ISDX) -&
   &(AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

   AXVERT(5,ISDX) = AXVERT(1,ISDX)
   AYVERT(5,ISDX) = AYVERT(1,ISDX)

!     Determine coordinates for center of rectangular source
   AXCNTR(ISDX) = 0.0D0
   AYCNTR(ISDX) = 0.0D0
   DO I = 1, NVERTS(ISDX)
      AXCNTR(ISDX) = AXCNTR(ISDX) + AXVERT(I,ISDX)
      AYCNTR(ISDX) = AYCNTR(ISDX) + AYVERT(I,ISDX)
   END DO
   AXCNTR(ISDX) = AXCNTR(ISDX)/DBLE(NVERTS(ISDX))
   AYCNTR(ISDX) = AYCNTR(ISDX)/DBLE(NVERTS(ISDX))

   RETURN
END SUBROUTINE LPARM

SUBROUTINE BLPARM(ISDX,TEMP)
!***********************************************************************
!                 BLPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for BOUYANT LINE Sources
!
!        PROGRAMMER: Jim Paumier
!                    Based on the VOLUME source code
!
!        DATE:    January 5, 2015
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'BLPARM'

   AQS(ISDX) = TEMP(1)                ! average emission release rate
   AHS(ISDX) = TEMP(2)                ! average release height

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 100.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 100M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

! --- Use center of EACH BUOYANT LINE for "origin"
   AXS(ISDX) = (AXS1(ISDX) + AXS2(ISDX))*0.5D0
   AYS(ISDX) = (AYS1(ISDX) + AYS2(ISDX))*0.5D0

   RETURN
END SUBROUTINE BLPARM

SUBROUTINE RLPARM(ISDX,TEMP)
!***********************************************************************
!                 RLPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for RLINE Sources
!
!        PROGRAMMER: M. Snyder and R. Cleary
!
!        MODIFIED:   Modified to handle two barrier cases for RLINEXT sources.
!                    Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED: Modified to handle RLINE and RLINEXT source types.
!                  Wood 03/18/2019
!
!        MODIFIED: Changes WPL * NLANES to WIDTH, Wood 12/29/2018
!
!        DATE:    December 14, 2017
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA, ONLY: RLSOURCE
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'RLPARM'

   IF (SRCTYP(ISDX) == 'RLINE') THEN
      RLSOURCE(ISDX)%QEMIS = TEMP(1)*TEMP(3)    ! emissions release rate Lnemis*Width
      RLSOURCE(ISDX)%DCL = 0.0          ! offset distance from center line
      RLSOURCE(ISDX)%ZSB = TEMP(2)      !set height of the source
      RLSOURCE(ISDX)%ZSE = TEMP(2)      !set height of the source
      IF (IFC == 7) THEN
         RLSOURCE(ISDX)%INIT_SIGMAZ = TEMP(4)  ! initial vertical spread
      ELSE
         RLSOURCE(ISDX)%INIT_SIGMAZ = 0.0  ! initial vertical spread
      END IF
      RLSOURCE(ISDX)%WIDTH = TEMP(3)        ! width of roadway
      RLSOURCE(ISDX)%HTWALL = 0.0D0         ! barrier 1 height
      RLSOURCE(ISDX)%DCLWALL = 0.0D0        ! barrier 1 distance from center line
      RLSOURCE(ISDX)%HTWALL2 = 0.0D0        ! barrier 2 height
      RLSOURCE(ISDX)%DCLWALL2 = 0.0D0       ! barrier 2 distance from center line
      RLSOURCE(ISDX)%DEPTH = 0.0D0          ! depth of depression
      RLSOURCE(ISDX)%WTOP = 0.0D0           ! width of top of depression
      RLSOURCE(ISDX)%WBOTTOM = 0.0D0        ! width of bottom of depression
   END IF


   IF (SRCTYP(ISDX) == 'RLINEXT') THEN
      RLSOURCE(ISDX)%QEMIS = TEMP(1)        ! emissions release rate
      RLSOURCE(ISDX)%DCL = TEMP(2)          ! offset distance from center line
      RLSOURCE(ISDX)%WIDTH = TEMP(3)        ! width of roadway
      RLSOURCE(ISDX)%INIT_SIGMAZ = TEMP(4)  ! initial vertical spread
      RLSOURCE(ISDX)%HTWALL = 0.0D0         ! barrier 1 height
      RLSOURCE(ISDX)%DCLWALL = 0.0D0        ! barrier 1 distance from center line
      RLSOURCE(ISDX)%HTWALL2 = 0.0D0        ! barrier 2 height
      RLSOURCE(ISDX)%DCLWALL2 = 0.0D0       ! barrier 2 distance from center line
      RLSOURCE(ISDX)%DEPTH = 0.0D0          ! depth of depression
      RLSOURCE(ISDX)%WTOP = 0.0D0           ! width of top of depression
      RLSOURCE(ISDX)%WBOTTOM = 0.0D0        ! width of bottom of depression
   END IF
!     Perform Error Checking on Source Parameters
   IF (RLSOURCE(ISDX)%QEMIS < 0.0D0) THEN
!        WRITE Error Message: Negative Emission Rate
      CALL ERRHDL(PATH,MODNAM,'E','209',' QS ')
   ELSE IF (RLSOURCE(ISDX)%QEMIS == 0.0D0) THEN
!        WRITE Warning Message: Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (RLSOURCE(ISDX)%INIT_SIGMAZ < 0.0D0) THEN
!        WRITE Error Message:  Negative Initial Vertical Spread
      CALL ERRHDL(PATH,MODNAM,'E','209',' INIT_SIGMAZ ')
   ELSE IF (RLSOURCE(ISDX)%INIT_SIGMAZ > 42.0D0) THEN
!        WRITE Warning Message:  Large Initial Vertical Parameter
      CALL ERRHDL(PATH,MODNAM,'E','320',' INIT_SIGMAZ ')
   END IF

   IF (RLSOURCE(ISDX)%WIDTH < 0.0D0) THEN
!        WRITE Error Message:  Negative Roadway Width
      CALL ERRHDL(PATH,MODNAM,'E','209',' WIDTH ')
   ELSE IF (RLSOURCE(ISDX)%WIDTH == 0.0D0) THEN
!        WRITE Error Message:  Width of Roadway Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' WIDTH ')
   ELSE IF (RLSOURCE(ISDX)%WIDTH > 30.0D0) THEN
!        WRITE Warning Message:  Large Roadwidth (> 30m)
      CALL ERRHDL(PATH,MODNAM,'W','320',' WIDTH ')
   END IF

   RETURN
END SUBROUTINE RLPARM

!     Added for sidewash
SUBROUTINE SWPARM(ISDX,TEMP)
!***********************************************************************
!                 SWPARM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Parameters for SIDEWASH Sources
!
!        PROGRAMMER: Carlos Szembek (AECOM), Roger Brode, Jeff Wang
!
!        DATE:    December 23, 2021
!C
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Parameter Card
!
!        CALLED FROM:   SOPARM
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: ISDX
   DOUBLE PRECISION :: TEMP(IFMAX)

!     Variable Initializations
   MODNAM = 'SWPARM'

   AQS(ISDX) = TEMP(1)
   AHS(ISDX) = TEMP(2)
   ABW(ISDX) = TEMP(3)
   ABL(ISDX) = TEMP(4)
   ABH(ISDX) = TEMP(5)
   ABA(ISDX) = TEMP(6)

!     Perform QA Error Checking on Source Parameters

   IF (AQS(ISDX) == 0.0D0) THEN
!        WRITE Warning Message:  Emission Rate Equals 0.0
      CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
   END IF

   IF (AHS(ISDX) < 0.0D0) THEN
!        WRITE Error Message:  Negative Release Height
      CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
   ELSE IF (AHS(ISDX) > 600.0D0) THEN
!        WRITE Warning Message:  Large Release Height (> 600M)
      CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
   ELSE IF (AHS(ISDX) > 3000.0D0) THEN
!        WRITE Error Message:  Large Release Height (> 3000M)
      CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
   END IF

   IF (ABW(ISDX) <= 0.0D0) THEN
!        Set Building width 1.0 m for Zero or Negative Values
      ABW(ISDX) = 1.0
   END IF

   IF (ABL(ISDX) <= 0.0D0) THEN
!        Set Building width 1.0 m for Zero or Negative Values
      ABL(ISDX) = 1.0
   END IF

   IF (ABH(ISDX) <= 0.0D0) THEN
!        Set Building hieght 1.0 m for Zero or Negative Values
      ABH(ISDX) = 1.0
   END IF

   IF (ABA(ISDX) < 0.0D0) THEN
!        Set Building width 1.0 m for Zero or Negative Values
      ABA(ISDX) = ABA(ISDX) + 360.
   ELSE IF (ABA(ISDX) > 360.) THEN
      ABA(ISDX) = ABA(ISDX) - 360.
   END IF

   RETURN
END SUBROUTINE SWPARM


SUBROUTINE DSBLDG
!***********************************************************************
!                 DSBLDG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Direction-specific Building Directions
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Direction Specific Building Directions
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'DSBLDG'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC == 3) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

!     Verify The Effective Srcid
   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
         IF (SRCTYP(ISDX)(1:5) == 'POINT') THEN
!              Fill Array
            CALL DSFILL(ISDX)
         ELSE
!              WRITE Warning Message: Building Inputs for Non-POINT Source
            CALL ERRHDL(PATH,MODNAM,'W','233',SRCID(ISDX))
         END IF
      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP .and. SRCTYP(I)(1:5)=='POINT') THEN
            ISDX = I
!              Fill DS Array
            CALL DSFILL(ISDX)
         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE DSBLDG

SUBROUTINE DSFILL(ISDX)
!***********************************************************************
!                 DSFILL Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        V. Tino
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Fill Direction-specific Building Dimension Arrays
!
!        PROGRAMMER:  Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Direction Specific Building Directions
!
!        CALLED FROM:   DSBLDG
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: J, K, ISDX

!     Variable Initializations
   MODNAM = 'DSFILL'

   IF (KEYWRD == 'BUILDHGT') THEN
      ISET = IWRK2(ISDX,1)
      DO K = 4, IFC
!           Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT == -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
!              Assign The Field
            IF (ISET <= NSEC) THEN
               ADSBH(ISET,ISDX) = DNUM
               IF (DNUM < 0.0D0) THEN
!                    WRITE Error Message:  Negative Value for ADSBH
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
!                 WRITE Error Message    ! Too Many Sectors Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO
      IWRK2(ISDX,1) = ISET
   ELSE IF (KEYWRD == 'BUILDWID') THEN
      ISET = IWRK2(ISDX,2)
      DO K = 4, IFC
!           Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT == -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
!              Assign The Field
            IF (ISET <= NSEC) THEN
               ADSBW(ISET,ISDX) = DNUM
               IF (DNUM < 0.0D0) THEN
!                    WRITE Error Message:  Negative Value for ADSBW
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
!                 WRITE Error Message    ! Too Many Sectors Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO
      IWRK2(ISDX,2) = ISET

! --- PRIME --------------------------------------------
! --- Fill building length information
   ELSE IF (KEYWRD == 'BUILDLEN') THEN
      ISET = IWRK2(ISDX,11)
      DO K = 4, IFC
!           Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT == -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
!              Assign The Field
            IF (ISET <= NSEC) THEN
               ADSBL(ISET,ISDX) = DNUM
               IF (DNUM < 0.0D0) THEN
!                    WRITE Error Message:  Negative value for ADSBL
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
!                 WRITE Error Message    ! Too Many Sectors Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO
      IWRK2(ISDX,11) = ISET

! --- Fill building XBADJ information
   ELSE IF (KEYWRD == 'XBADJ   ') THEN
      ISET = IWRK2(ISDX,12)
      DO K = 4, IFC
!           Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT == -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
!              Assign The Field
            IF (ISET <= NSEC) THEN
               ADSXADJ(ISET,ISDX) = DNUM
            ELSE
!                 WRITE Error Message    ! Too Many Sectors Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO
      IWRK2(ISDX,12) = ISET

! --- Fill building YBADJ information
   ELSE IF (KEYWRD == 'YBADJ   ') THEN
      ISET = IWRK2(ISDX,13)
      DO K = 4, IFC
!           Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT == -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
!              Assign The Field
            IF (ISET <= NSEC) THEN
               ADSYADJ(ISET,ISDX) = DNUM
            ELSE
!                 WRITE Error Message    ! Too Many Sectors Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO
      IWRK2(ISDX,13) = ISET
! --------------------------------------------------------

   END IF

   RETURN
END SUBROUTINE DSFILL

SUBROUTINE EMVARY
!***********************************************************************
!                 EMVARY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Variable Emission Rate Factors
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include options to vary emissions by
!                    hour-of-day, and day-of-week (HRDOW and HRDOW7).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To include options to vary emissions by month,
!                    hour-of-day, and day-of-week (MHRDOW and MHRDOW7).
!                    R.W. Brode, MACTEC (f/k/a PES), Inc., 06/22/05
!
!        MODIFIED:   To replace 'STAR' option with 'WSPEED'.
!                    R.W. Brode, PES, 02/25/02
!
!        MODIFIED:   To include an option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, 4/10/2000
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IH, IL, ISDX, IQMAX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'EMVARY'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC == 3) THEN
!        Error Message: No Numerical Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

!     Verify The Effective Srcid
   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
         QFLAG(ISDX) = FIELD(4)
         IF (QFLAG(ISDX) == 'SEASON') THEN
            IQMAX = 4
         ELSE IF (QFLAG(ISDX) == 'MONTH') THEN
            IQMAX = 12
         ELSE IF (QFLAG(ISDX) == 'HROFDY') THEN
            IQMAX = 24
         ELSE IF (QFLAG(ISDX) == 'WSPEED') THEN
            IQMAX = 6
         ELSE IF (QFLAG(ISDX) == 'SEASHR') THEN
            IQMAX = 96
         ELSE IF (QFLAG(ISDX) == 'HRDOW') THEN
            IQMAX = 72
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (QFLAG(ISDX) == 'HRDOW7') THEN
            IQMAX = 168
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (QFLAG(ISDX) == 'SHRDOW') THEN
            IQMAX = 288
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (QFLAG(ISDX) == 'SHRDOW7') THEN
            IQMAX = 672
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (QFLAG(ISDX) == 'MHRDOW') THEN
            IQMAX = 864
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (QFLAG(ISDX) == 'MHRDOW7') THEN
            IQMAX = 2016
            L_DayOfWeekOpts = .TRUE.
         ELSE
!              WRITE Error Message    ! Invalid QFLAG Field Entered
            CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
         END IF
         IF (IQMAX <= NQF) THEN
            CALL EFFILL(ISDX,IQMAX)
         ELSE
!              WRITE Error Message     ! NQF Parameter Not Large Enough
            WRITE(DUMMY,'(''NQF ='',I6)') NQF
            CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
         END IF
      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP) THEN
            ISDX = I
            QFLAG(ISDX) = FIELD(4)
            IF (QFLAG(ISDX) == 'SEASON') THEN
               IQMAX = 4
            ELSE IF (QFLAG(ISDX) == 'MONTH') THEN
               IQMAX = 12
            ELSE IF (QFLAG(ISDX) == 'HROFDY') THEN
               IQMAX = 24
            ELSE IF (QFLAG(ISDX) == 'WSPEED') THEN
               IQMAX = 6
            ELSE IF (QFLAG(ISDX) == 'SEASHR') THEN
               IQMAX = 96
            ELSE IF (QFLAG(ISDX) == 'HRDOW') THEN
               IQMAX = 72
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) == 'HRDOW7') THEN
               IQMAX = 168
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) == 'SHRDOW') THEN
               IQMAX = 288
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) == 'SHRDOW7') THEN
               IQMAX = 672
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) == 'MHRDOW') THEN
               IQMAX = 864
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) == 'MHRDOW7') THEN
               IQMAX = 2016
               L_DayOfWeekOpts = .TRUE.
            ELSE
!                 WRITE Error Message    ! Invalid QFLAG Field Entered
               CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
            END IF
            IF (IQMAX <= NQF) THEN
               CALL EFFILL(ISDX,IQMAX)
            ELSE
!                 WRITE Error Message    ! NQF Parameter Not Large Enough
               WRITE(DUMMY,'(''NQF ='',I6)') NQF
               CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
            END IF
         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE EMVARY

SUBROUTINE EFFILL(ISDX,IQMAX)
!***********************************************************************
!                 EFFILL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Fill Variable Emission Rate Array
!
!        PROGRAMMER:  Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Direction Specific Building Directions
!
!        CALLED FROM:   EMVARY
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: J, K, ISDX, IQMAX

!     Variable Initializations
   MODNAM = 'EFFILL'

   ISET = IWRK2(ISDX,4)

   DO K = 5, IFC
!        Change Fields To Numbers
      CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         CYCLE
      END IF
      DO J = 1, IMIT
         ISET = ISET + 1
!           Assign The Field
         IF (ISET <= IQMAX) THEN
            QFACT(ISET,ISDX) = DNUM
            IF (DNUM < 0.0D0) THEN
!                 WRITE Error Message:  Negative Value for QFACT
               CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
            END IF
         ELSE
!              WRITE Error Message    ! Too Many QFACT Values Input
            IF (ISDX <= 999) THEN
               WRITE(DUMMY,'(''QFACT Src'',I3.3)') ISDX
            ELSE IF (ISDX <= 99999) THEN
               WRITE(DUMMY,'(''QF Src'',I5.5)') ISDX
            ELSE
               WRITE(DUMMY,'(''QF Src>99999'')')
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','231',DUMMY)
         END IF
      END DO
   END DO

   IWRK2(ISDX,4) = ISET

   RETURN
END SUBROUTINE EFFILL

SUBROUTINE EMUNIT
!***********************************************************************
!                 EMUNIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Emission Rate Unit Conversion Factors
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Emission Rate Unit Conversion Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'EMUNIT'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 5) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Fetch Each Field
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

   EMIFAC(1) = DNUM
   EMILBL(1) = FIELD(4)
   OUTLBL(1) = FIELD(5)
   IF (.NOT.CONC .and. ANNUAL) THEN
      PERLBL(1) = RUNST1(LOCB(5):LOCE(5))//'/YR'
   ELSE
      PERLBL(1) = FIELD(5)
   END IF

999 RETURN
END SUBROUTINE EMUNIT

SUBROUTINE COUNIT
!***********************************************************************
!                 COUNIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Emission Rate Unit Conversion Factors
!                 for CONCentration Values
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Emission Rate Unit Conversion Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'COUNIT'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 5) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Fetch Each Field
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

   EMIFAC(1) = DNUM
   EMILBL(1) = FIELD(4)
   OUTLBL(1) = FIELD(5)
   PERLBL(1) = FIELD(5)

999 RETURN
END SUBROUTINE COUNIT

SUBROUTINE DPUNIT
!***********************************************************************
!                 DPUNIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Emission Rate Unit Conversion Factors
!                 for Deposition Values
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Emission Rate Unit Conversion Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I

!     Variable Initializations
   MODNAM = 'DPUNIT'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 5) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Fetch Each Field
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

   IF (.NOT. CONC) THEN
      DO I = 1, NTYP
         EMIFAC(I) = DNUM
         EMILBL(I) = FIELD(4)
         OUTLBL(I) = FIELD(5)
         IF (ANNUAL) THEN
            PERLBL(I) = RUNST1(LOCB(5):LOCE(5))//'/YR'
         ELSE
            PERLBL(I) = FIELD(5)
         END IF
      END DO
   ELSE
      DO I = 2, NTYP
         EMIFAC(I) = DNUM
         EMILBL(I) = FIELD(4)
         OUTLBL(I) = FIELD(5)
         IF (ANNUAL) THEN
            PERLBL(I) = RUNST1(LOCB(5):LOCE(5))//'/YR'
         ELSE
            PERLBL(I) = FIELD(5)
         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE DPUNIT

SUBROUTINE PARTDEP
!***********************************************************************
!                 PARTDEP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        ADAPTED from  DRYDEP Module of the AMS/EPA Regulatory Model - AERMOD
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        PURPOSE: Processes Inputs for Wet & Dry PARTicle DEPosition
!
!        DRYDEP ADAPTED BY: D. Strimaitis, SRC (for Wet & Dry Deposition)
!        DATE:    November 8, 1993
!
!        DRYDEP MODIFIED BY: D. Strimaitis, SRC (for Dry Deposition)
!        (DATE:    February 15, 1993)
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Input For Setting and Removal Variables
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'PARTDEP'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC == 3) THEN
!        Error Message: No Numerical Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     Process The Appropriate Settling & Removal Parameter
   IF (KEYWRD == 'PARTDIAM') THEN
!        Process Particle Diameter Categories (PDIAM)       ---   CALL INPPDM
      CALL INPPDM
   ELSE IF (KEYWRD == 'MASSFRAX') THEN
!        Process Mass Fractions (PHI)                       ---   CALL INPPHI
      CALL INPPHI
   ELSE IF (KEYWRD == 'PARTDENS') THEN
!        Process Particle Density (PDENS)                   ---   CALL INPPDN
      CALL INPPDN
   END IF

999 RETURN
END SUBROUTINE PARTDEP

SUBROUTINE INPPDM
!***********************************************************************
!                 INPPDM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Particle Diameter Categories
!
!        PROGRAMMER: D. Strimaitis, SRC
!
!        ADAPTED FROM "INPVSN"
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    February 15, 1993
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Particle Diameter Categories
!
!        CALLED FROM:   PARTDEP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, K, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK
! Unused INTEGER :: J

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'INPPDM'

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND .and. .NOT.L_METHOD2(ISDX)) THEN
         ISET = IWRK2(ISDX,5)
         DO K = 4, IFC
!              Change It To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT == -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            ELSE IF (IMIT > 1) THEN
! ---             Use of '*' for repeat input values is not meaningful
!                 for particle diameters; Issue Error message
               CALL ERRHDL(PATH,MODNAM,'E','288',KEYWRD)
               CYCLE
            ELSE IF (DNUM <= 0.001D0 .or. DNUM > 1000.0D0) THEN
!                 WRITE Error Message: Particle Diameter Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','335',SRCID(ISDX))
            END IF
            ISET = ISET + 1
            IF (ISET <= NPDMAX) THEN
!                 Assign The Field
               APDIAM(ISET,ISDX) = DNUM
            ELSE
!                 WRITE Error Message: Too Many PartDiam Categories
!                 This shouldn't occur since limits are dynamically allocated
               WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
               CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            END IF
         END DO
         IWRK2(ISDX,5) = ISET

      ELSE IF (FOUND .and. L_METHOD2(ISDX)) THEN
! ---       Write Error Message     ! Source ID identified as Method 2
         CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(ISDX))

      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      SOURCE_LOOP: DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP .and. .NOT.L_METHOD2(I)) THEN
            ISET = IWRK2(I,5)
            DO K = 4, IFC
!                 Get Numbers
               CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!                 Check The Numerical Field
               IF (IMIT == -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  CYCLE SOURCE_LOOP
               ELSE IF (IMIT > 1) THEN
! ---                Use of '*' for repeat input values is not meaningful
!                    for particle diameters; Issue Error message
                  CALL ERRHDL(PATH,MODNAM,'E','288',KEYWRD)
                  CYCLE SOURCE_LOOP
               ELSE IF (DNUM <= 0.001D0 .or. DNUM>1000.0D0) THEN
!                    WRITE Error Message: Particle Diameter Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','335',SRCID(I))
               END IF
               ISET = ISET + 1
               IF (ISET <= NPDMAX) THEN
                  APDIAM(ISET,I) = DNUM
               ELSE
!                    WRITE Error Message: Too Many PartDiam Categories
!                    This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               END IF
            END DO
            IWRK2(I,5) = ISET

         ELSE IF (INGRP .and. L_METHOD2(I)) THEN
! ---          Write Error Message     ! Source ID identified as Method 2
            CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(I))

         END IF
      END DO SOURCE_LOOP
   END IF

999 RETURN
END SUBROUTINE INPPDM

SUBROUTINE INPPHI
!***********************************************************************
!                 INPPHI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Mass Fraction (PHI) Input Values
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!        MODIFIED BY: D. Strimaitis, SRC
!
!        DATE:    February 15, 1993
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Mass Fraction Input Values
!
!        CALLED FROM:   PARTDEP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'INPPHI'

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
         ISET = IWRK2(ISDX,6)
         DO K = 4, IFC
!              Change It To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT == -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            IF (DNUM < 0.0D0 .or. DNUM > 1.0D0) THEN
!                 WRITE Error Message: Mass Fraction Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
               IF (ISET <= NPDMAX) THEN
!                    Assign The Field
                  APHI(ISET,ISDX) = DNUM
               ELSE
!                    WRITE Error Message: Too Many PartDiam Categories
!                    This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               END IF
            END DO
         END DO
         IWRK2(ISDX,6) = ISET
      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      SOURCE_LOOP: DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP) THEN
            ISET = IWRK2(I,6)
            DO K = 4, IFC
!                 Get Numbers
               CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!                 Check The Numerical Field
               IF (IMIT == -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  CYCLE SOURCE_LOOP
               END IF
               IF (DNUM < 0.0D0 .or. DNUM > 1.0D0) THEN
!                    WRITE Error Message: Mass Fraction Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(I))
               END IF
               DO J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET <= NPDMAX) THEN
                     APHI(ISET,I) = DNUM
                  ELSE
!                       WRITE Error Message: Too Many PartDiam Categories
!                       This shouldn't occur since limits are dynamically allocated
                     WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  END IF
               END DO
            END DO
            IWRK2(I,6) = ISET
         END IF
      END DO SOURCE_LOOP
   END IF

999 RETURN
END SUBROUTINE INPPHI

SUBROUTINE INPPDN
!***********************************************************************
!                 INPPDN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Particle Density Input Values
!
!        PROGRAMMER:  D. Strimaitis, SRC
!
!        ADAPTED FROM "INPGAM"
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    February 15, 1993
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Particle Density Input Values
!
!        CALLED FROM:   PARTDEP
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'INPPDN'

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
         ISET = IWRK2(ISDX,7)
         DO K = 4, IFC
!              Change It To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT == -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            IF (DNUM <= 0.0D0) THEN
!                 WRITE Error Message: Particle Density Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(ISDX))
            ELSE IF (DNUM <= 0.1D0) THEN
!                 WRITE Warning Message: Particle Density may be Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'W','334',SRCID(ISDX))
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
               IF (ISET <= NPDMAX) THEN
!                    Assign The Field
                  APDENS(ISET,ISDX) = DNUM
               ELSE
!                    WRITE Error Message: Too Many PartDiam Categories
!                    This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               END IF
            END DO
         END DO
         IWRK2(ISDX,7) = ISET
      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      SOURCE_LOOP: DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP) THEN
            ISET = IWRK2(I,7)
            DO K = 4, IFC
!                 Get Numbers
               CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!                 Check The Numerical Field
               IF (IMIT /= 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  CYCLE SOURCE_LOOP
               END IF
               IF (DNUM <= 0.0D0) THEN
!                    WRITE Error Message: Particle Density Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(I))
               ELSE IF (DNUM <= 0.1D0) THEN
!                    WRITE Warning Message: Particle Density may be Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'W','334',SRCID(I))
               END IF
               DO J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET <= NPDMAX) THEN
                     APDENS(ISET,I) = DNUM
                  ELSE
!                       WRITE Error Message: Too Many PartDiam Categories
!                       This shouldn't occur since limits are dynamically allocated
                     WRITE(DUMMY,'(''NPD= '',I7)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  END IF
               END DO
            END DO
            IWRK2(I,7) = ISET
         END IF
      END DO SOURCE_LOOP
   END IF

999 RETURN
END SUBROUTINE INPPDN

SUBROUTINE GASDEP
!***********************************************************************
!                 GASDEP Module of AERMOD Model
!
!        PURPOSE: Processes Deposition Parameters for Gases
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:    May 16, 1996
!
!        MODIFIED:   Apply range check on input parameters.
!                    R.W. Brode, MACTEC (f/k/a PES), Inc., 10/26/2004
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Dry Deposition Parameters for Gases
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK, DLOOKUP

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'GASDEP'
   DLOOKUP = .FALSE.

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 7) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 7) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
         SOGAS(ISDX) = 'Y'
         DLOOKUP=.FALSE.
!           Read Dry Deposition Parameters
!           Change Them To Numbers

!           First Get Gas Diffusivity (cm^2/s)
         CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
            DNUM = 5.5D-02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'HGII') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 4.5D-02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'TCDD') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 5.196D-02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'BAP') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 5.13D-02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'SO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.112D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'NO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.361D-01
            DLOOKUP=.TRUE.
         ELSE IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (DNUM <= 0.0D0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','380','PDIFF')
         END IF
!           Assign The Field
         PDIFF(ISDX) = DNUM

!PES ---    Next Get Diffusivity in Water (cm^2/s)
         CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
            DNUM = 6.4D-06
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'HGII') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 5.2D-06
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'TCDD') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 4.392D-06
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'BAP') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 4.44D-06
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'SO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.83D-05
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'NO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.4D-05
            DLOOKUP=.TRUE.
         ELSE IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (DNUM <= 0.0D0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','380','PDIFFW')
         END IF
!           Assign The Field
         PDIFFW(ISDX) = DNUM

!           Now Get Lipid Cuticle Resistence for Individual Leaves (RCLI)
         CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
            DNUM = 1.0D05
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'HGII') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.0D05
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'TCDD') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 9.67D0
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'BAP') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 4.41D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'SO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 7.32D02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'NO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.2D04
            DLOOKUP=.TRUE.
         ELSE IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (DNUM <= 0.0D0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','380','RCLI')
         END IF
!           Assign The Field
         RCLI(ISDX) = DNUM

!           Get the Henry's Law Constant
         CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
            DNUM = 7.19D02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'HGII') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 7.2D-05
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'TCDD') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.46D0
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'BAP') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 4.6D-02
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'SO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 7.2D01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'NO2') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 8.444D03
            DLOOKUP=.TRUE.
         ELSE IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (DNUM <= 0.0D0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','380','HENRY')
         END IF
!           Assign The Field
         HENRY(ISDX) = DNUM

!           WRITE Warning Message:  Default deposition parameter used
         IF (DLOOKUP .EQV. .TRUE.) THEN
            CALL ERRHDL(PATH,MODNAM,'W','473',SRCID(ISDX))
         END IF

      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP) THEN
            ISDX = I
            SOGAS(ISDX) = 'Y'
!              Read Dry Deposition Parameters
!              Change Them To Numbers

!              First Get Gas Diffusivity
            CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
               DNUM = 5.5D-02
            ELSE IF ((POLLUT == 'HGII') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 4.5D-02
            ELSE IF ((POLLUT == 'TCDD') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 5.196D-02
            ELSE IF ((POLLUT == 'BAP') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 5.13D-02
            ELSE IF ((POLLUT == 'SO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.112D-01
            ELSE IF ((POLLUT == 'NO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.361D-01
            ELSE IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM <= 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','PDIFF')
            END IF
!              Assign The Field
            PDIFF(ISDX) = DNUM

!PES ---       Next Get Diffusivity in Water (cm^2/s)
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
               DNUM = 6.4D-06
            ELSE IF ((POLLUT == 'HGII') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 5.2D-06
            ELSE IF ((POLLUT == 'TCDD') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 4.392D-06
            ELSE IF ((POLLUT == 'BAP') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 4.44D-06
            ELSE IF ((POLLUT == 'SO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.83D-05
            ELSE IF ((POLLUT == 'NO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.4D-05
            ELSE IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM <= 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','PDIFFW')
            END IF
!              Assign The Field
            PDIFFW(ISDX) = DNUM

!              Now Get Lipid Cuticle Resistence for Individual Leaves (RCLI)
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
               DNUM = 1.0D05
            ELSE IF ((POLLUT == 'HGII') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.0D05
            ELSE IF ((POLLUT == 'TCDD') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 9.67D0
            ELSE IF ((POLLUT == 'BAP') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 4.41D-01
            ELSE IF ((POLLUT == 'SO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 7.32D02
            ELSE IF ((POLLUT == 'NO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.2D04
            ELSE IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM <= 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','RCLI')
            END IF
!              Assign The Field
            RCLI(ISDX) = DNUM

!              Get the Henry's Law Constant
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            IF ((POLLUT == 'HG0') .and. (DNUM == 0.0D0)) THEN
               DNUM = 7.19D02
            ELSE IF ((POLLUT == 'HGII') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 7.2D-05
            ELSE IF ((POLLUT == 'TCDD') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.46D0
            ELSE IF ((POLLUT == 'BAP') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 4.6D-02
            ELSE IF ((POLLUT == 'SO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 7.2D01
            ELSE IF ((POLLUT == 'NO2') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 8.444D03
            ELSE IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM <= 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','HENRY')
            END IF
!              Assign The Field
            HENRY(ISDX) = DNUM

         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE GASDEP

SUBROUTINE METH_2
!***********************************************************************
!                 METH_2 Module of AERMOD Model
!
!        PURPOSE: Processes Method 2 Dry Deposition Parameters for Particles
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:    June 1, 2001
!
!        MODIFIED:   To check for out-of-range inputs for fine mass
!                    fraction (finemass)
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Dry Deposition Parameters for Particles using Method 2
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK, DLOOKUP

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'METH_2'
   DLOOKUP = .FALSE.

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 5) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND .and. .NOT.L_METHOD2(ISDX) .and.&
      &IWRK2(ISDX,5)==0) THEN
         L_METHOD2(ISDX) = .TRUE.

         DLOOKUP=.FALSE.
!           Read Dry Deposition Parameters
!           Change Them To Numbers
!           First Get Mass Fraction of Fine Particles (.lt. 2.5 microns)
         CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         IF ((POLLUT == 'AR') .and. (DNUM == 0.0D0)) THEN
            DNUM = 7.5D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'CD') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 7.0D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'PB') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 7.5D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'HG') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 8.0D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'POC') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 9.0D-01
            DLOOKUP=.TRUE.
         ELSE IF (DNUM < 0.0D0 .or. DNUM > 1.0D0) THEN
!              WRITE Error Message: Mass Fraction Out-of-Range
            CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
         END IF

!           Assign The Field
         FINEMASS(ISDX) = DNUM

!           Now Get Mass Mean Diameter
         CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         ELSE IF ((POLLUT == 'AR') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 5.0D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'CD') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 6.0D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'PB') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 5.0D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'HG') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 4.0D-01
            DLOOKUP=.TRUE.
         ELSE IF ((POLLUT == 'POC') .and.&
         &(DNUM == 0.0D0)) THEN
            DNUM = 1.0D-01
            DLOOKUP=.TRUE.
         END IF
!           Assign The Field
         APDIAM(1,ISDX) = DNUM

!           Set mass fraction and particle density
         APHI(1,ISDX)   = 1.0D0
         APDENS(1,ISDX) = 1.0D0

!           Set number of particle size categories to 1
         INPD(ISDX) = 1

!           WRITE Warning Message:  Default deposition parameter used
         IF (DLOOKUP .EQV. .TRUE.) THEN
            CALL ERRHDL(PATH,MODNAM,'W','473',SRCID(ISDX))
         END IF

      ELSE IF (FOUND .and. L_METHOD2(ISDX)) THEN
! ---       Write Error Message     ! Source ID identified twice
         CALL ERRHDL(PATH,MODNAM,'E','387',SRCID(ISDX))

      ELSE IF (FOUND .and. IWRK2(ISDX,5) > 0) THEN
! ---       Write Error Message     ! Source ID identified as Method 1
         CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(ISDX))

      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP) THEN
            ISDX = I
            IF (.NOT.L_METHOD2(ISDX) .and. IWRK2(ISDX,5) == 0) THEN
               L_METHOD2(ISDX) = .TRUE.
            ELSE IF (L_METHOD2(ISDX)) THEN
! ---             Write Error Message     ! Source ID identified twice
               CALL ERRHDL(PATH,MODNAM,'E','387',SRCID(ISDX))
               CYCLE
            ELSE IF (IWRK2(ISDX,5) > 0) THEN
! ---             Write Error Message     ! Source ID identified as Method 1
               CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(ISDX))
               CYCLE
            END IF
!              Read Dry Deposition Parameters
!              Change Them To Numbers
!              First Get Mass Fraction of Fine Particles (.lt. 2.5 microns)
            CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            IF (DNUM < 0.0D0 .or. DNUM > 1.0D0) THEN
!                 WRITE Error Message: Mass Fraction Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
            ELSE IF ((POLLUT == 'AR') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 7.5D-01
            ELSE IF ((POLLUT == 'CD') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 7.0D-01
            ELSE IF ((POLLUT == 'PB') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 7.5D-01
            ELSE IF ((POLLUT == 'HG') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 8.0D-01
            ELSE IF ((POLLUT == 'POC') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 9.0D-01
            END IF

!              Assign The Field
!              MW D068 10/30/2020 moved assignment of FINEMASS outside the IF statement above, consistant with the other assignments of APDIAM and FINEMASS in this section
            FINEMASS(ISDX) = DNUM


!              Now Get Mass Mean Diameter
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            ELSE IF ((POLLUT == 'AR') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 5.0D-01
            ELSE IF ((POLLUT == 'CD') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 6.0D-01
            ELSE IF ((POLLUT == 'PB') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 5.0D-01
            ELSE IF ((POLLUT == 'HG') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 4.0D-01
            ELSE IF ((POLLUT == 'POC') .and.&
            &(DNUM == 0.0D0)) THEN
               DNUM = 1.0D-01
            END IF
!              Assign The Field
            APDIAM(1,ISDX) = DNUM

!              Set mass fraction and particle density
            APHI(1,ISDX)   = 1.0D0
            APDENS(1,ISDX) = 1.0D0

!              Set number of particle size categories to 1
            INPD(ISDX) = 1

         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE METH_2

SUBROUTINE SOGRP
!***********************************************************************
!                 SOGRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Source Group Inputs for Pass One
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Source Group Input For Pass One
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, K, IH, IL
   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
   LOGICAL :: CONT, INGRP, RMARK, FOUND

!     Variable Initializations
   CONT   = .FALSE.
   INGRP  = .FALSE.
   FOUND  = .FALSE.
   MODNAM = 'SOGRP'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC <= 3 .and. FIELD(3) /= 'ALL') THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     READ in the Group ID and Check for Continuation Card
!*    First check for length of GRPID field (<=8)
   IF ((LOCE(3)-LOCB(3)) <= 7) THEN
!*       Retrieve Temporary Group ID Character Substring
      TEMPID = FIELD(3)
      IF (NUMGRP == 0) THEN
! ---       This is the first GRPID defined; set CONT = .F.
         CONT = .FALSE.
      ELSE
! ---       This is not the first GRPID defined; check on whether
!           this record is a continuation for the same GRPID
         DO I = 1, NUMGRP
            IF (TEMPID == GRPID(I)) THEN
               CONT = .TRUE.
               EXIT
            END IF
         END DO
      END IF
   ELSE
!*       WRITE Error Message:  Group ID Field is Too Long (>8)
      CALL ERRHDL(PATH,MODNAM,'E','245',FIELD(3)(1:12))
      GO TO 999
   END IF

!     Increment Counters and Assign Group ID If Not a Continuation Card
   IF (.NOT. CONT) THEN
! ---    This is not a continuation record so this is a new GRPID
      IGRP = IGRP + 1
      IF (IGRP > NGRP) THEN
!           WRITE Error Message    ! Too Many Source Groups Specified
!           This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NGRP='',I7)') NGRP
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
!           Exit to END
         GO TO 999
      END IF
      NUMGRP = NUMGRP + 1
      GRPID(IGRP) = TEMPID
   END IF

!     Set Up The Source Group Array and Check for inclusion of BACKGROUND
   IF (GRPID(IGRP) == 'ALL' .and. .NOT.CONT) THEN
! ---    SRCGROUP ALL has been specified; assign all sources to this group
      DO I = 1, NUMSRC
         IGROUP(I,IGRP) = 1
      END DO
! ---    Include BACKGRND concentrations as specified by the user;
!        Note the BACKGRND is NOT automatically included in source group ALL
      IF (L_BACKGRND) THEN
         IF (FIELD(4) == 'NOBACKGROUND' .or.&
         &FIELD(4) == 'NOBACKGRND') THEN
! ---          User specified to NOT include BACKGRND concentrations in group ALL
            GRP_BACK(IGRP) = .FALSE.
!              Write message indicating that BACKGRND is NOT included in group ALL
            CALL ERRHDL(PATH,MODNAM,'W','298','NOBACKGROUND')
         ELSE IF (FIELD(4) == 'BACKGROUND' .or.&
         &FIELD(4) == 'BACKGRND') THEN
! ---          Include BACKGRND concentrations in group ALL
            GRP_BACK(IGRP) = .TRUE.
!              Write message indicating that BACKGRND IS included in group ALL
            CALL ERRHDL(PATH,MODNAM,'W','298','BACKGROUND  ')
         ELSE IF (LEN_TRIM(FIELD(4)) == 0) THEN
! ---          No user-specified option, BACKGRND is NOT included in group ALL
            GRP_BACK(IGRP) = .FALSE.
!              Write message indicating that BACKGRND is NOT included in group ALL
            CALL ERRHDL(PATH,MODNAM,'W','298','NOBACKGROUND')
         END IF
      ELSE
! ---       No BACKGRND concentrations provided, set logical to FALSE
         GRP_BACK(IGRP) = .FALSE.
!           Check for BACKGRND or BACKGROUND with SrcGroup ALL, without
!           background data provided
         IF (IFC > 3) THEN
            IF (FIELD(4) == 'BACKGROUND' .or.&
            &FIELD(4) == 'BACKGRND') THEN
! ---             Write error message: BACKGRND specified for group ALL w/o BACKGRND keyword
               CALL ERRHDL(PATH,MODNAM,'E','323',GRPID(IGRP))
            ELSE
! ---             Write error message: Additional field included with SRCGROUP ALL
               CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
            END IF
         END IF
      END IF
   ELSE
!        Loop Through Fields
      DO I = 4, IFC
! ---       First check for inclusion of BACKGROUND in SrcGroups
         IF (L_BACKGRND) THEN
            IF (FIELD(I) == 'BACKGROUND' .or.&
            &FIELD(I) == 'BACKGRND') THEN
               GRP_BACK(IGRP) = .TRUE.
! ---             Cycle to next input field
               CYCLE
            END IF
         ELSE IF (FIELD(I) == 'BACKGROUND' .or.&
         &FIELD(I) == 'BACKGRND') THEN
! ---          Write error message: BACKGRND specified w/o BACKGRND keyword
            CALL ERRHDL(PATH,MODNAM,'E','323',GRPID(IGRP))
! ---          Cycle to next input field
            CYCLE
         END IF

! ---       Check for whether individual source IDs included on the SrcGroup
!           keyword have been defined.
         IF (INDEX(FIELD(I),'-') == 0) THEN
! ---          This field is specifying an individual SrcID;
!              assign to SrcGrp as appropriate
            FOUND = .FALSE.
            DO K = 1, NUMSRC
               IF (FIELD(I) == SRCID(K)) THEN
                  FOUND = .TRUE.
! ---                Check for SRCID already assigned to current group
                  IF (IGROUP(K,IGRP) > 0) THEN
                     WRITE(DUMMY,'(''G'',I4.4,'' S'',I5.5)')&
                     &MIN(IGRP,9999), MIN(K,99999)

                     CALL ERRHDL(PATH,MODNAM,'W','314',DUMMY)
                  END IF
                  IGROUP(K,IGRP) = 1
                  EXIT
               END IF
            END DO
            IF (.NOT. FOUND) THEN
!                 WRITE Error Message:  Specified Source ID not defined
               CALL ERRHDL(PATH,MODNAM,'E','224',FIELD(I)(1:12))
            END IF
         ELSE
! ---          Input field includes a range of SrcIDs to include in SrcGroup
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,&
            &LOWID,HIGID)
!              First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1<LID1) .or. (IH<IL) .or.&
            &(HID2<LID2)) THEN
! ---             WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
!                 Cycle to next input field
               CYCLE
            END IF
! ---          Loop through sources and assign to SrcGroup as appropriate
!              based on Source Range
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGROUP(K,IGRP) = 1
               END IF
            END DO
         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE SOGRP

SUBROUTINE ASNGRP(INID,LOWID,HIGID,INGRP)
!***********************************************************************
!                 ASNGRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Find Whether A Source ID is In The Specific Group
!
!        PROGRAMMER: Roger Brode, Jeff Wang, Kevin Stroupe
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Field Parameters
!
!        OUTPUTS: Indicator for Source ID in The Group
!
!        CALLED FROM: (This is An Utility Program)
!***********************************************************************
!
!     Variable Declarations
   CHARACTER (LEN=12) :: LOWID, HIGID, INID, IID1, LID1, HID1,&
   &IID2, LID2, HID2
!     JAT D065 8/9/21 PATH SET BUT NOT USED
!      CHARACTER PATH*2, MODNAM*12
   CHARACTER :: MODNAM*12
   INTEGER :: IN, IL, IH
   LOGICAL :: INGRP

!     Variable Initializations
   MODNAM = 'ASNGRP'
!     JAT D065 8/9/21 PATH SET BUT NOT USED
!      PATH   = 'SO'
   INGRP  = .FALSE.

!     Extract The Character Field And Numerical Field
   CALL SETIDG(INID,IID1,IN,IID2)
   CALL SETIDG(LOWID,LID1,IL,LID2)
   CALL SETIDG(HIGID,HID1,IH,HID2)

!     Do Comparisons of Character and Numeric Fields, All Must Satisfy Ranges
   IF ((IID1>=LID1 .and. IID1<=HID1) .and.&
   &(IN>=IL .and. IN<=IH) .and.&
   &(IID2>=LID2 .and. IID2<=HID2)) THEN
      INGRP = .TRUE.
   END IF

   RETURN
END SUBROUTINE ASNGRP

SUBROUTINE SETIDG(INID,IDCHR1,IDNUM,IDCHR2)
!***********************************************************************
!                 SETIDG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Find A Source ID's Character Part and
!                 Numerical Part
!
!        PROGRAMMER: Jeff Wang, Roger Brode, Kevin Stroupe
!
!        DATE:    March 2, 1992
!
!        REVISION HISTORY:
!
!                 Modified internal read from NUMID to use 'I8'
!                 format to avoid runtime errors with some compilers.
!                 R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!                 Modified conversion of numeric portion to use internal
!                 read rather than using call to STONUM in order to
!                 avoid precision problems for 8-digit integer IDs.
!                 R. Brode, PES, 8/9/01
!
!        INPUTS:  Input Field Parameters
!
!        OUTPUTS: An Initial Character String, a Number, and
!                 a Second Character String
!
!        CALLED FROM: (This is An Utility Program)
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, II, ISTR, IDNUM
   CHARACTER (LEN=12) :: INID, IDCHR1, IDCHR2
   CHARACTER (LEN= 1) :: CHKI
   CHARACTER (LEN=ILEN_FLD) :: NUMID
   LOGICAL :: HIT

!     Variable Initializations
   MODNAM = 'SETIDG'
   I  = 12
   II = 0
   NUMID  = ' '
   IDCHR1 = ' '
   IDCHR2 = ' '
   IDNUM  = 0
   HIT    = .FALSE.

!     Find The Length of the Input Field, II (<= 12)
   DO WHILE (.NOT.HIT .and. I>=1)
      CHKI = INID(I:I)
      IF (CHKI /= ' ') THEN
         II = I
         HIT = .TRUE.
      END IF
      I = I - 1
   END DO

!     Divide the Input Id into 3 parts (char1, int, and char2)
   I = 1
   ISTR = I
   CHKI = INID(I:I)
!     Get first character part
   DO WHILE (CHKI < '0' .or. CHKI > '9')
      IDCHR1 = INID(ISTR:I)
      I = I + 1
      IF (I > II) THEN
         GO TO 20
      ELSE
         CHKI = INID(I:I)
      END IF
   END DO

!     Get integer part
   ISTR = I
   DO WHILE (CHKI >= '0' .and. CHKI <= '9')
      NUMID = INID(ISTR:I)
      I = I + 1
      IF (I > II) THEN
         GO TO 20
      ELSE
         CHKI = INID(I:I)
      END IF
   END DO

!     Get second character part
   ISTR = I
   DO WHILE (I <= II)
      IDCHR2 = INID(ISTR:I)
      I = I + 1
      IF (I > II) THEN
         GO TO 20
      ELSE
         CHKI = INID(I:I)
      END IF
   END DO

20 CONTINUE

!     Convert Numeric Part to Integer Variable
   READ(NUMID,'(I8)') IDNUM

   RETURN
END SUBROUTINE SETIDG

!----------------------------------------------------------------------
subroutine vdp1
!----------------------------------------------------------------------
!
! --- ISC2ST     Version:  1.0     Level:  930215                  VDP1
!                J. Scire, SRC
!
! --- PURPOSE:  Setup routine for PARTICLE dry deposition.
!               Completes particle common block /SOURC4/.  Performs
!               initialization and time-invariant calculations.
!
! --- MODIFIED: To require non-zero values for all gas deposition
!               parameters.
!               R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/2004
!
! --- MODIFIED: To include calculation of SCF and AVGRAV for
!               Method 2 sources, remove assignment of ZRDEP,
!               and to remove unused calculations.
!               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
!
! --- MODIFIED: To save SCF values in array for use in SUB. VDP for
!               new TOXICS deposition option
!               R. W. Brode, PES, Inc., 02/11/03
!
! --- MODIFIED: Set deposition reference height, ZRDEP, to 1.0 meter.
!               R. W. Brode, PES, Inc., 12/29/97
!
! --- INPUTS:
!     Common block /SOURC4/ variables:
!              INPD - integer    - Number of particle size categories
!            APDIAM - real array - Mean diameter (microns) of each
!                                  particle size category
!              APHI - real array - Mass fraction in each size category
!            APDENS - real       - Particle density (g/cm**3)
!
! --- OUTPUT:
!     Common block /SOURC4/ variables:
!            AVGRAV - real array - Gravitational settling velocity (m/s)
!            ATSTOP - real array - Stopping time (s)
!            VAIRMS - real       - Viscosity of air (m**2/s)
!            VDPHOR - real       - Phoretic effects term (m/s)
!
! --- VDP1 called by:  SOCARD
! --- VDP1 calls:      none
!----------------------------------------------------------------------
!
   USE MAIN1

   DOUBLE PRECISION, PARAMETER :: a1=1.257D0, a2=0.4D0, a3=0.55D0,&
   &xmfp=6.5D-6,&
   &vcon=1.81D-4, vair=0.15D0,&
   &rhoair=1.2D-3
! unused: xk=1.38D-16
! unused: gcgs=981.0D0, tair=293.15D0
   DOUBLE PRECISION :: DIAMCM
   INTEGER          :: I, J, N
!
! ***
   if(DEBUG)then
      write(DBGUNT,*)
      write(DBGUNT,*)'SUBR. VDP1 -- INPUTS'
      write(DBGUNT,*)
      do i=1,numsrc
         write(DBGUNT,*)'SOURCE          = ',i
         write(DBGUNT,*)'INPD            = ',inpd(i)
         if (inpd(i) > 0) then
            write(DBGUNT,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
            write(DBGUNT,*)'APHI            = ',(aphi(n,i),n=1,inpd(i))
            write(DBGUNT,*)'APDENS(g/cm**3) = ',(apdens(n,i),n=1,inpd(i))
         end if
         write(DBGUNT,*)
      end do
   endif
! ***
!
! --- Convert viscosity of air (at 20 deg C) from cm**2/s to m**2/s
   vairms=1.0D-4*vair
!
! --- Define phoretic effects term (m/s)
   vdphor=0.0001D0
!
!
! --  LOOP over sources
   do j=1,numsrc
!
      if(inpd(j) <= 0 .and. .not.luservd) then
!
         if (pdiff(j)/=0.0D0 .and. pdiffw(j)/=0.0D0&
         &.and. rcli(j)/=0.0D0 .and. henry(j)/=0.0D0) then
! ---          GAS DEPOSITION
!
! ---          Convert Pollutant diffusivity (cm**2/s to m**2/s)
            pdiff(j) =pdiff(j)*1.0D-4
            pdiffw(j)=pdiffw(j)*1.0D-4
!
! ---          Convert rcli resistance from s/cm to s/m
            rcli(j)=rcli(j)*1.0D2
!
! ***
            if(debug)then
               write(DBGUNT,*)
               write(DBGUNT,*)'SUBR. VDP1 -- OUTPUT for GASES'
               write(DBGUNT,*)'PDIFF (m**2/s)  = ',&
               &(pdiff(n),n=1,numsrc)
               write(DBGUNT,*)'PDIFFW(m**2/s)  = ',&
               &(pdiffw(n),n=1,numsrc)
               write(DBGUNT,*)'RCLI(s/m)       = ',&
               &(rcli(n),n=1,numsrc)
               write(DBGUNT,*)'ZRDEP (m)       = ',zrdep
            endif
! ***
         endif

      else if (inpd(j) > 0) then
!
! ---       PARTICLE DEPOSITION
!
! ---       LOOP over "INPD" size intervals if non-zero
!
         do i=1,inpd(j)
! ---          Slip correction factor
            diamcm=1.0D-4*apdiam(i,j)
            scf(i)=1.0D0+2.0D0*xmfp*&
            &(a1+a2*DEXP(-a3*diamcm/xmfp))/diamcm
!
! ---          Gravitational settling velocity (m/s)
! ---          (rhoair is approx. density of air -- 1.2e-3 g/cm**3)
!rwb           Use PARAMETER G = 9.80616 m/s^2 instead of gcgs = 981 cm/s^2
!rwb           avgrav(i,j)=0.01*(apdens(i,j)-rhoair)*gcgs*diamcm**2
! ---          Set lower limit of 0.0 on density difference
            avgrav(i,j)= MAX(0.0D0,(apdens(i,j)-rhoair))*G*diamcm**2&
            &*scf(i)/(18.0D0*vcon)
!
! ---          Stopping times
!rwb           Use PARAMETER G = 9.80616 m/s^2 instead of gcgs = 981 cm/s^2
!rwb           atstop(i,j)=avgrav(i,j)/(0.01*gcgs)
            atstop(i,j)=avgrav(i,j)/G
         end do
! ***
         if(DEBUG)then
            write(DBGUNT,*)
            write(DBGUNT,*)'SUBR. VDP1 -- OUTPUT for PARTICLES'
            write(DBGUNT,*)
            do i=1,numsrc
               write(DBGUNT,*)'SOURCE          = ',i
               write(DBGUNT,*)'AVGRAV (m/s)    = ',&
               &(avgrav(n,i),n=1,inpd(i))
               write(DBGUNT,*)'ATSTOP (s)      = ',&
               &(atstop(n,i),n=1,inpd(i))
               write(DBGUNT,*)'VAIRMS (m**2/s) = ',vairms
               write(DBGUNT,*)'ZRDEP (m)       = ',zrdep
               write(DBGUNT,*)'VDPHOR (m/s)    = ',vdphor
               write(DBGUNT,*)
            end do
         endif
! ***
!
      endif
   end do
!     end LOOP over source

   return
end subroutine vdp1

SUBROUTINE HREMIS
!***********************************************************************
!                 HREMIS Module of AERMOD
!
!        PURPOSE: To process Hourly Emissions Data
!
!        PROGRAMMER: Jayant Hardikar, Roger Brode
!
!        DATE:    September 15, 1993
!
!        INPUTS:  Pathway (SO) and Keyword (HOURLY)
!
!        OUTPUTS: Source QFLAG Array
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE, ONLY: L_BLHOURLY   ! Multiple_BuoyLines_D41_Wood
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, K, IH, IL

   LOGICAL :: FOPEN, INGRP
   LOGICAL :: RMARK

   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID

!     Variable Initializations
   MODNAM = 'HREMIS'

   FOPEN  = .FALSE.

   IF (IFC >= 4) THEN
!        Retrieve Hourly Emissions Data Filename as Character Substring to
!        Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         HRFILE = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  HRFILE Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         GO TO 999
      END IF

!        Open Hourly Emissions Data File If Not Already Open
      INQUIRE (FILE=HRFILE,OPENED=FOPEN)

      IF (.NOT. FOPEN) THEN
!           Open Hourly Emissions Data File If Not Already Open
!           Open with ACTION='READ' to prevent overwrite and allow multiple access
         INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
         IF (.NOT. FOPEN) THEN
            OPEN(UNIT=IHREMI,ERR=998,FILE=HRFILE,IOSTAT=IOERRN,&
            &ACTION='READ',STATUS='OLD')
         END IF
      END IF

   ELSE
!        WRITE Error Message         ! Not Enough Parameters Specified
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

   TEMPID = FIELD(4)

!     Set Up The Source Group Array
   IF (TEMPID == 'ALL') THEN
      DO K = 1, NUMSRC
         QFLAG(K) = 'HOURLY'

!        Multiple_BuoyLines_D41_Wood: Begin
!           Set the flag indicating that there is one
!            (or more) buoyant line sources in the hourly emissions file
         IF (SRCTYP(K) == 'BUOYLINE' .and. .NOT. L_BLHOURLY) THEN
            L_BLHOURLY = .TRUE.
         END IF
!        Multiple_BuoyLines_D41_Wood: End

      END DO
   ELSE
!        Loop Through Fields
      DO I = 4, IFC
         CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,&
         &LOWID,HIGID)
!           First Check Range for Upper Value < Lower Value
         CALL SETIDG(LOWID,LID1,IL,LID2)
         CALL SETIDG(HIGID,HID1,IH,HID2)
         IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            CYCLE
         END IF
         DO K = 1, NUMSRC
            CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
            IF (INGRP) THEN
               QFLAG(K) = 'HOURLY'
!              Multiple_BuoyLines_D41_Wood: Begin
               IF ((SRCTYP(K) == 'BUOYLINE' .and.&
               &.NOT. L_BLHOURLY)) THEN
                  L_BLHOURLY = .TRUE.
               END IF
!              Multiple_BuoyLines_D41_Wood: End
            END IF
         END DO
      END DO
   END IF

   GO TO 999

!     Process Error Messages
998 CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)

999 RETURN
END SUBROUTINE HREMIS

SUBROUTINE URBANS
!***********************************************************************
!                 URBANS Module of AERMOD Model
!
!        PURPOSE: Processes Urban Source Card
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    June 11, 1996
!
!        MODIFIED:   Modified to allow for the use of URBANSRC ALL on
!                    the SO pathway to indicate that all sources are
!                    to be treated as URBAN sources. This option assumes
!                    that only one (1) urban area has been defined using
!                    the CO URBANOPT keyword.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Array of flags for Urban Sources
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
   INTEGER :: I, IL, IH, K, ISTR
   LOGICAL :: INGRP, RMARK, FOUND

!     Variable Initializations
   MODNAM = 'URBANS'
   FOUND = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   END IF

!     Check for 'URBANSRC ALL' option identified in PRESET
   IF (L_URBAN_ALL) THEN
      IF (IFC == 3 .and. FIELD(3) == 'ALL') THEN
         URBSRC(:) = 'Y'
         IURBGRP(:,:) = 1
         GO TO 999
      ELSE
!           WRITE Error Message:  URBANSRC ALL
         CALL ERRHDL(PATH,MODNAM,'E','279','URBANSRC ALL')
         GO TO 999
      END IF

   ELSE IF (L_MULTURB) THEN
!        Multiple Urban Areas
!        READ in the Group ID and Check for Continuation Card
      TEMPID = FIELD(3)
      DO I = 1, NUMURB
         IF (TEMPID == URBID(I)) THEN
            FOUND = .TRUE.
            IURB = I
         END IF
      END DO
      IF (.NOT. FOUND) THEN
!           WRITE Error Message:  Urban ID not defined
         CALL ERRHDL(PATH,MODNAM,'E','301',TEMPID)
         GO TO 999
      END IF

!        Specify field index to start for Source IDs
      ISTR = 4

   ELSE
!        Single Urban Area - No URBAN ID
      IURB = 1

!        Specify field index to start for Source IDs
      ISTR = 3

   END IF

!     Loop Through Fields
   DO I = ISTR, IFC
      IF (INDEX(FIELD(I),'-') == 0) THEN
         FOUND = .FALSE.
         DO K = 1, NUMSRC
            IF (SRCID(K) == FIELD(I)) THEN
               FOUND = .TRUE.
               URBSRC(K) = 'Y'
               IURBGRP(K,IURB) = 1
            END IF
         END DO
         IF (.NOT.FOUND) THEN
!              WRITE Error Message:  SRCID not found
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
            CYCLE
         END IF

      ELSE

         CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,LOWID,&
         &HIGID)
!           First Check Range for Upper Value < Lower Value
         CALL SETIDG(LOWID,LID1,IL,LID2)
         CALL SETIDG(HIGID,HID1,IH,HID2)
         IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            CYCLE
         END IF
         DO K = 1, NUMSRC
            CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
            IF (INGRP) THEN
               URBSRC(K) = 'Y'
               IURBGRP(K,IURB) = 1
            END IF
         END DO

      END IF
   END DO

!     CRO 3/28/2022 D132 Remove alpha requirement for RLINE (wasn't working in 21112 anyway)
!     Added check to require ALPHA with RLINEXT or RLINE urban sources.
!      DO K = 1, NUMSRC
!        IF((SRCTYP(K) .EQ. 'RLINE') .or.
!     &           (SRCTYP(K) .EQ. 'RLINEXT')) THEN
!          IF((URBSRC(K) .EQ. 'Y') .and. .NOT.L_ALPHA) THEN
!              WRITE Error Message: ALPHA required for RLINE/RLINEXT URBANSRC
!               CALL ERRHDL(PATH,MODNAM,'E','281', SRCID(K))
!          END IF
!        END IF
!     END DO

999 RETURN
END SUBROUTINE URBANS

SUBROUTINE HBPSOURCE
!***********************************************************************
!                 HBPSOURCE Module of AERMOD Model
!
!        PURPOSE: Processes HBP Sources in Source Card
!
!        PROGRAMMER: Carlos Szembek (AECOM)
!
!        DATE:    January 18, 2023
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Array of flags for HBP Sources
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
   INTEGER :: I, IL, IH, K, ISTR
   LOGICAL :: INGRP, RMARK, FOUND

!     Variable Initializations
   MODNAM = 'HBPSOURCE'
   FOUND = .FALSE.


!     Check The Number Of The Fields
   IF (IFC < 3) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   END IF

!     Check for 'HBPSRCID ALL' option identified in PRESET
   IF (L_HBP_ALL) THEN
      IF (IFC == 3 .and. FIELD(3) == 'ALL') THEN
         DO K = 1, NUMSRC
            IF (SRCTYP(K)(1:5) == 'POINT') THEN
               HBPSRC(K) = 'Y'
               NHBP = NHBP + 1
            ELSE
               HBPSRC(K) = 'N'
!                    WRITE Informational Message:
!                    HBP is only relevant to POINT-type sources
               CALL ERRHDL(PATH,MODNAM,'I','740',SRCID(K))
            ENDIF
         ENDDO
         GO TO 999
      END IF
   ELSE
!        Specify field index to start for Source IDs
      ISTR = 3
   END IF

!     Loop Through Fields
   DO I = ISTR, IFC
      IF (INDEX(FIELD(I),'-') == 0) THEN
         FOUND = .FALSE.
         DO K = 1, NUMSRC
            IF (SRCID(K) == FIELD(I)) THEN
               IF (SRCTYP(K)(1:5) == 'POINT') THEN
                  FOUND = .TRUE.
                  HBPSRC(K) = 'Y'
                  NHBP = NHBP + 1
               ELSE
                  FOUND = .TRUE.
                  HBPSRC(K) = 'N'
!                        WRITE Informational Message:
!                        HBP is only relevant to POINT-type sources
                  CALL ERRHDL(PATH,MODNAM,'I','740',SRCID(K))
               ENDIF
            END IF
         END DO
         IF (.NOT.FOUND) THEN
!              WRITE Error Message:  SRCID not found
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
            CYCLE
         END IF

      ELSE

         CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,LOWID,&
         &HIGID)
!           First Check Range for Upper Value < Lower Value
         CALL SETIDG(LOWID,LID1,IL,LID2)
         CALL SETIDG(HIGID,HID1,IH,HID2)
         IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            CYCLE
         END IF
         DO K = 1, NUMSRC
            CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
            IF (INGRP) THEN
               HBPSRC(K) = 'Y'
            END IF
         END DO

      END IF
   END DO

999 RETURN
END SUBROUTINE HBPSOURCE


SUBROUTINE NO2RAT
!***********************************************************************
!                 NO2RAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes In-stack NO2/NOX Ratios by Source for
!                 OLM, PVMRM and GRSM Options
!
!        PROGRAMMER: Roger W. Brode, PES, Inc.
!
!        DATE:    May 6, 2002
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Array of in-stack NO2/NOX ratios
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IH, IL, ISDX
   CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
   CHARACTER (LEN=ILEN_FLD) :: SOID
   LOGICAL :: FOUND, INGRP, RMARK

!     Variable Initializations
   FOUND  = .FALSE.
   INGRP  = .FALSE.
   MODNAM = 'NO2RAT'

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 4) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 4) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Get The Source ID(s)
   SOID = FIELD(3)
   CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

   IF (LID == HID) THEN
!        Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
!           Read NO2/NOX Ratio and Convert to Real
         CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!           Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IF (DNUM < 0.0D0 .or. DNUM > 1.0D0) THEN
!              WRITE Error Message: NO2_Ratio Out-of-Range
            CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(ISDX))
         END IF
!           Assign The Field
         ANO2_RATIO(ISDX) = DNUM
      ELSE
!           WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF
   ELSE
!        First Check Range for Upper Value < Lower Value
      CALL SETIDG(LID,LID1,IL,LID2)
      CALL SETIDG(HID,HID1,IH,HID2)
      IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
         GO TO 999
      END IF
      DO I = 1, NUMSRC
!           See Whether It's In The Group
         CALL ASNGRP(SRCID(I),LID,HID,INGRP)
         IF (INGRP) THEN
!              Read NO2/NOX Ratio and Convert to Real
            CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!              Check The Numerical Field
            IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            IF (DNUM < 0.0D0 .or. DNUM > 1.0D0) THEN
!                 WRITE Error Message: NO2_Ratio Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(I))
            END IF
!              Assign The Field
            ANO2_RATIO(I) = DNUM
         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE NO2RAT

SUBROUTINE OLMGRP
!***********************************************************************
!                 OLMGRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes OLM Source Group Inputs
!
!        PROGRAMMER: Roger W. Brode, PES, Inc.
!
!        DATE:    May 6, 2002
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: OLM Source Group Inputs
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, K, IH, IL
   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2
   CHARACTER (LEN= 8) :: TEMPID
   LOGICAL :: CONT, INGRP, RMARK, FOUND
! Unused INTEGER :: J

!     Variable Initializations
   CONT   = .FALSE.
   INGRP  = .FALSE.
   FOUND  = .FALSE.
   MODNAM = 'OLMGRP'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC <= 3 .and. FIELD(3) /= 'ALL') THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     READ in the OLMGROUP ID and Check for Continuation Card
!*    First check for length of OLMID field (<=8)
   IF ((LOCE(3)-LOCB(3)) <= 7) THEN
!*       Retrieve Temporary Group ID Character Substring
      TEMPID = FIELD(3)
      IF (NUMOLM == 0) THEN
         CONT = .FALSE.
      ELSE
! ---       This is not the first OLMGroup defined; check on whether
!           this record is a continuation for the same OLMGroup
         DO I = 1, NUMOLM
            IF (TEMPID == OLMID(I)) THEN
               CONT = .TRUE.
               EXIT
            END IF
         END DO
      END IF
   ELSE
!*       WRITE Error Message:  OLMGroup ID Field is Too Long
      CALL ERRHDL(PATH,MODNAM,'E','232',FIELD(3)(1:12))
      GO TO 999
   END IF

!     Increment Counters and Assign Group ID If Not a Continuation Card
   IF (.NOT. CONT) THEN
! ---    This is not a continuation record so this is a new GRPID
      IOLM = IOLM + 1
      IF (IOLM > NOLM) THEN
!           WRITE Error Message    ! Too Many OLM Groups Specified
!           This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NOLM='',I7)') NOLM
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
!           Exit to END
         GO TO 999
      END IF
      NUMOLM = NUMOLM + 1
      OLMID(IOLM) = TEMPID
   END IF

!     Set Up The OLM Group Array
   IF (OLMID(IOLM) == 'ALL' .and. .NOT.CONT) THEN
! ---    Check for whether all sources have been defined yet,
!        based on NSRC determined during PRESET vs. NUMSRC
!        determined during normal SETUP.
      IF (NUMSRC < NSRC) THEN
!           Issue fatal error message; OLMGROUP ALL out of order
         CALL ERRHDL(PATH,MODNAM,'E','140','OLMGROUP ALL')
         GO TO 999
      ELSE
! ---       OLMGROUP ALL has been specified; assign all sources to this OLMgroup
         DO I = 1, NUMSRC
            IGRP_OLM(I,IOLM) = 1
            L_OLMGRP(I) = .TRUE.
         END DO
      END IF
   ELSE
!        Loop Through Fields
      DO I = 4, IFC
! ---       First check for whether individual source IDs included on the
!           OLMGROUP keyword have been defined.
         IF (INDEX(FIELD(I),'-') == 0) THEN
! ---          This field is specifying an individual SrcID;
!              assign to OLMGrp as appropriate
            FOUND = .FALSE.
            DO K = 1, NUMSRC
               IF (FIELD(I) == SRCID(K)) THEN
                  FOUND = .TRUE.
! ---                Assign logical indicating that this SRCID is in an OLMGRP
                  L_OLMGRP(K) = .TRUE.
! ---                Assign flag indicating which OLMGRP this SRCID is included in
                  IGRP_OLM(K,IOLM) = 1
                  EXIT
               END IF
            END DO
            IF (.NOT. FOUND) THEN
!                 WRITE Error Message:  Specified Source ID not defined
               CALL ERRHDL(PATH,MODNAM,'E','225',FIELD(I)(1:12))
            END IF
         ELSE
! ---          Input field includes a range of SrcIDs to include in OLMGrp
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,&
            &LOWID,HIGID)
!              First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1<LID1) .or. (IH<IL) .or.&
            &(HID2<LID2)) THEN
!                 WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               CYCLE
            END IF
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
! ---                Assign logical indicating that this SRCID is in an OLMGRP
                  L_OLMGRP(K) = .TRUE.
! ---                Assign flag indicating which OLMGRP this SRCID is included in
                  IGRP_OLM(K,IOLM) = 1
               END IF
            END DO
         END IF
      END DO
   END IF

999 RETURN
END SUBROUTINE OLMGRP

SUBROUTINE PSDGRP
!***********************************************************************
!                 PSDGRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes PSD Source Group Inputs
!
!        PROGRAMMER: Jim Paumier, MACTEC FPI
!        Based on code for SOGRP and OLMGRP by: Roger W. Brode, PES, Inc.
!
!        DATE:    September 30, 2006
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: PSD Source Group Inputs
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, K, IH, IL
   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
   LOGICAL :: CONT, INGRP, RMARK, FOUND

!     Variable Initializations
   CONT   = .FALSE.
   INGRP  = .FALSE.
   FOUND  = .FALSE.
   MODNAM = 'PSDGRP'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
!        Note:  The ALL parameter is not valid for the PSDCREDIT option
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 4) THEN
!        Error Message: Not Enough Parameters
!        Note:  The ALL parameter is not valid for the PSDCREDIT option
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     READ in the PSDGROUP ID and Check for Continuation Card
!*    First check for length of PSDID field (<=8)
   IF ((LOCE(3)-LOCB(3)) <= 7) THEN
!*       Retrieve Temporary Group ID Character Substring
      TEMPID = FIELD(3)
      IF (NUMPSD == 0) THEN
! ---       This is the first PSDID defined; set CONT = .F.
         CONT = .FALSE.
      ELSE
! ---       This is not the first PSDID defined; check on whether
!           this record is a continuation for the same PSDID
         DO I = 1, NUMPSD
            IF (TEMPID == PSDID(I)) THEN
               CONT = .TRUE.
               EXIT
            END IF
         END DO
      END IF
   ELSE
!*       WRITE Error Message:  PSDGROUP ID Field is Too Long
      CALL ERRHDL(PATH,MODNAM,'E','253',FIELD(3)(1:12))
      GO TO 999
   END IF

!     Increment Counters and Assign Group ID If Not a Continuation Card
   IF (.NOT. CONT) THEN
! ---    This is not a continuation record so this is a new PSDID
      IPSD = IPSD + 1
      IF (IPSD > NPSD) THEN
!           WRITE Error Message    ! Too Many PSD Groups Specified
!           This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NPSD='',I7)') NPSD
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
!           Exit to END
         GO TO 999
      END IF
      NUMPSD = NUMPSD + 1
      PSDID(IPSD) = TEMPID
   END IF

!     Set Up The Source Group Array and the PSD source type
!       Only INCRCONS (increment consuming), RETRBASE (retired baseline),
!       and NONRBASE (non-retired basleine) are allowable; any other
!       PSD group name, including ALL, is not allowed
   IF( PSDID(IPSD) == 'INCRCONS' .or.&
   &PSDID(IPSD) == 'RETRBASE' .or.&
   &PSDID(IPSD) == 'NONRBASE' )THEN

!        Valid PSD Source Group; Loop Through Fields
      DO I = 4, IFC
! ---       First check for whether individual source IDs included on the
!           PSDGROUP keyword have been defined (based on no '-' in FIELD).
         IF (INDEX(FIELD(I),'-') == 0) THEN
            FOUND = .FALSE.
            DO K = 1, NUMSRC
               IF (FIELD(I) == SRCID(K)) THEN
                  FOUND = .TRUE.
                  IGRP_PSD(K,IPSD) = 1
! ---                Assign PSDSRCTYP flag to indicate the type of source
                  IF( PSDID(IPSD) == 'INCRCONS' )THEN
                     PSDSRCTYP(K) = 'IC'
                  ELSE IF( PSDID(IPSD) == 'NONRBASE' )THEN
                     PSDSRCTYP(K) = 'NB'
                  ELSE IF( PSDID(IPSD) == 'RETRBASE' )THEN
                     PSDSRCTYP(K) = 'RB'
                  END IF
               END IF
            END DO
            IF (.NOT. FOUND) THEN
!                 WRITE Error Message:  Specified Source ID not defined
               CALL ERRHDL(PATH,MODNAM,'E','226',FIELD(I)(1:12))
            END IF
         ELSE
! ---          Input field includes a range of SrcIDs to include in PSDGrp
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,&
            &LOWID,HIGID)
!              First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1<LID1) .or. (IH<IL) .or.&
            &(HID2<LID2)) THEN
!                 WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','PSDRANGE')
!                 Cycle to next input field
               CYCLE
            END IF
! ---          Loop through sources and assign to PSDGroup as appropriate
!              based on Source Range
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGRP_PSD(K,IPSD) = 1
                  L_PSDGRP(K) = .TRUE.
! ---                Assign PSDSRCTYP flag to indicate the type of source
                  IF( PSDID(IPSD) == 'INCRCONS' )THEN
                     PSDSRCTYP(K) = 'IC'
                  ELSE IF( PSDID(IPSD) == 'NONRBASE' )THEN
                     PSDSRCTYP(K) = 'NB'
                  ELSE IF( PSDID(IPSD) == 'RETRBASE' )THEN
                     PSDSRCTYP(K) = 'RB'
                  END IF
               END IF
            END DO
         END IF
      END DO
   ELSE
!        Error Message: Not a valid PSD source group
      CALL ERRHDL(PATH,MODNAM,'E','287',KEYWRD)
      GO TO 999
   END IF

999 RETURN
END SUBROUTINE PSDGRP

! Multiple_BuoyLines_D41_Wood
SUBROUTINE BLPGRP
!***********************************************************************
!                 BLPGRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes BLP Source Group Inputs
!
!        PROGRAMMER: Wood, Inc
!
!        DATE:    May 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: BLP Source Group Inputs
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

! JAT 06/22/21 D065
! REMOVE K AS UNUSED VARIABLE
!      INTEGER :: I, K, IH, IL, IGRPNUM, JJ, KK
   INTEGER :: I, IH, IL, JJ, KK
   INTEGER, SAVE :: IGRPNUM
   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
   LOGICAL :: CONT, INGRP, RMARK, FOUND, BLPAVGGRP_FOUND

!     Variable Initializations
! --- Flag indicating if the BLPGROUP record is a continuation record
   CONT   = .FALSE.
! --- Flag indicating whether a source in a range of sources is in a BLPGROUP
   INGRP  = .FALSE.
! --- Flag indicating if the SOURCE ID has been defined (SO LOCATION)
   FOUND  = .FALSE.
! --- Flag indicating if the BLPGROUP ID was defined on a BLPINPUT record
   BLPAVGGRP_FOUND = .FALSE.

   MODNAM = 'BLPGRP'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC <= 3 .and. FIELD(3) /= 'ALL') THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF

!     READ in the BLPGROUP ID and Check for Continuation Card
!*    First check for length of BLPID field (<=8)
   IF ((LOCE(3)-LOCB(3)) <= 7) THEN
!*       Retrieve Temporary Group ID Character Substring
      TEMPID = FIELD(3)
!         BL_Source_Limit_D088: Wood
!         Code deleted:
!         In original implementation of BLPGROUP, but is redundant
!          NUMBLGRPS will be 0 on first entry to the subroutine
!          (initialized in SOCARD) and the DO loop below will not execute
!          when NUMBLGRPS is 0
!          CONT is set to FALSE upon entering subroutine

!         IF (NUMBLGRPS .EQ. 0) THEN
! ---       This is the first BLPGROUP record encountered
!            CONT = .FALSE.
!         ELSE
!           BL_Source_Limit_D088: Wood
! ---       Check if this record is a continuation for the same BLPGROUP
      DO I = 1, NUMBLGRPS
         IF (TEMPID == BL_GRPID(I)) THEN
            BLPAVGGRP_FOUND = .TRUE.                 ! BL_Source_Limit_D088: Wood
            CONT = .TRUE.
            EXIT
         END IF
      END DO
!         END IF   !NUMBLGRPS .EQ. 0
   ELSE
!*       WRITE Error Message:  BLPGROUP ID Field is Too Long
      CALL ERRHDL(PATH,MODNAM,'E','245',FIELD(3)(1:12))
      GO TO 999
   END IF

!     Increment Counters and Assign Group ID If Not a Continuation Card
   IF (.NOT. CONT) THEN
! ---    This is not a continuation record so this is a new BLPGROUP ID

! ---    Determine if this BLPGROUP ID has been defined by a BLPINPUT record
!        TEMPID = FIELD(3)(at line 6501 above) = BLPGrpID
!         Note: BLPINPUT records with a BL group ID define the number of
!               expected BLGROUP records; therefore, if there are more
!               BLGROUP records than BLPINPUT records, then an error
!               message wll be written that indicates that one or more
!               individual buoyant lines are not in a BLPGROUP, even
!               though according to the input file, the lines are in a group.
!               This error message will likely be accompanied by another
!               error message that there is no BLPINPUT record for BLPGROUP ID

      DO KK = 1,NUMBLAVGINP
         IF (TEMPID == BLAVGINP_GRPID(KK)) THEN
            IGRPNUM = KK
            BLPAVGGRP_FOUND = .TRUE.
            NUMBLGRPS = NUMBLGRPS + 1
            BL_GRPID(IGRPNUM) = TEMPID
            EXIT
         END IF
      END DO

!        IF (IOLM .GT. NOLM) THEN
!           WRITE Error Message    ! Too Many OLM Groups Specified
!           This shouldn't occur since limits are dynamically allocated
!           WRITE(DUMMY,'(''NOLM='',I7)') NOLM
!           CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
!           Exit to END
!           GO TO 999
!        END IF
!        NUMOLM = NUMOLM + 1      no NUMBLP equivalent in this routine
   END IF

   IF (BLPAVGGRP_FOUND) THEN
! ---    The BLP group ID, as defined on a BLPINPUT record, has been found

!        Set Up The BLP Group Array
      IF (BL_GRPID(IGRPNUM) == 'ALL' .and. .NOT.CONT) THEN
! ---       BLPGROUP set to ALL
!           Assign all sources to this BLPGROUP
         DO KK = 1, NUMSRC
            IF (SRCTYP(KK) == 'BUOYLINE') THEN
               IGRP_BLP(KK,IGRPNUM) = 1
!                 L_BLPGRP(kk) = .TRUE.
            ENDIF
         END DO
      ELSE
!           BLPGROUP ID is not 'ALL' - loop through fields
         DO JJ = 4, IFC
! ---          First check for whether individual source IDs included on
!              the BLPGROUP keyword have been defined.
            IF (INDEX(FIELD(JJ),'-') == 0) THEN
! ---             This field is specifying an individual SrcID;
!                 assign to BLPGROUP as appropriate
               FOUND = .FALSE.
               DO KK = 1, NUMSRC
                  IF (FIELD(JJ) == SRCID(KK)) THEN
                     IF (SRCTYP(KK) == 'BUOYLINE') THEN
                        FOUND = .TRUE.
! ---                      Assign flag indicating which BLPGROUP this SRCID is included in
                        IGRP_BLP(KK,IGRPNUM) = 1
! ---                      Assign logical indicating that this SRCID is in a BLPGROUP
!                           L_BLPGRP(kk) = .TRUE.
                        EXIT
                     ELSE                 ! non-BL line in a BLPGROUP
!                          WRITE Error Message:  Specified Source ID is not a BL
                        CALL ERRHDL(PATH,MODNAM,'E','255',&
                        &FIELD(JJ)(1:12))
                     END IF
                  END IF
               END DO
               IF (.NOT. FOUND) THEN
!                    WRITE Error Message:  Specified Source ID not defined
                  CALL ERRHDL(PATH,MODNAM,'E','254',FIELD(JJ)(1:12))
               END IF
            ELSE
! ---             Input field includes a range of SrcIDs on BLPGROUP
               CALL FSPLIT(PATH,KEYWRD,FIELD(JJ),ILEN_FLD,'-',&
               &RMARK,LOWID,HIGID)
!                 First Check Range for Upper Value < Lower Value
               CALL SETIDG(LOWID,LID1,IL,LID2)
               CALL SETIDG(HIGID,HID1,IH,HID2)
               IF ((HID1<LID1) .or. (IH<IL) .or.&
               &(HID2<LID2)) THEN
!                    WRITE Error Message:  Invalid Range,  Upper < Lower
                  CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
                  CYCLE
               END IF

               DO KK = 1, NUMSRC
                  IF (SRCTYP(KK) == 'BUOYLINE') THEN
                     CALL ASNGRP(SRCID(KK),LOWID,HIGID,INGRP)
                     IF (INGRP) THEN
! ---                      Assign flag indicating which BLPGROUP this SRCID
!                          is included in
                        IGRP_BLP(KK,IGRPNUM) = 1
! ---                      Assign logical indicating that this SRCID is
!                          in a BLPGROUP
!                           L_BLPGRP(kk) = .TRUE.
                     END IF
                  END IF      ! SRCTYP = BUOYLINE
               END DO         ! loop over sources
            END IF            ! search for hyphen in BLPGROUP
         END DO               ! loop through fields on BLPGROUP record
      END IF                  ! BLPGROUP id ('ALL' or a group name)
   ELSE
! ---    The BLP group ID, as defined on a BLPINPUT record, has NOT been found
!          or there are no BLPINPUT records
      WRITE(DUMMY,'(A12)') TEMPID
      CALL ERRHDL(PATH,MODNAM,'E','502',DUMMY)
   END IF

999 RETURN
END SUBROUTINE BLPGRP

SUBROUTINE BACK_GRND
!***********************************************************************
!                 BACK_GRND Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes User-specified BACKGROUND concentrations,
!                 based on same options for temporal variability
!                 as the EMISFACT keyword for source emissions
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED: Include checks for potential problem with Fortran
!                  format specifier.  Should include from 1 to 4
!                  integers for date variables, and one real for
!                  background data variable.  Warning message is issued
!                  if too many or too few integers/reals are specified.
!                  An error message may also be issued when reading
!                  the background file depending on the compiler options.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 04/13/2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J
   INTEGER :: NumInt, NumReal
   LOGICAL :: FOPEN
! Unused: CHARACTER (LEN=12) :: LID, LID1, LID2, HID, HID1, HID2
! Unused: CHARACTER (LEN=ILEN_FLD) :: SOID
! Unused: INTEGER :: IH, IL, ISDX
! Unused: LOGICAL INGRP, RMARK

!     Variable Initializations
   MODNAM = 'BACK_GRND'

! --- Assign logical variable indicating that background concentrations
!     are specified
   L_BACKGRND = .TRUE.
   FOPEN  = .FALSE.
   NumInt  = 0
   NumReal = 0

! --- Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_BGSector) THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC == 3) THEN
!           Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 4) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF
! ---    Check for SECT ID in field 3 in case BGSECTOR keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without BGSECTOR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified BACKGRND option and
!        assign the option to BFLAG variable
      IBGSECT = 1
      I = 3

! ---    Check for inconsistent non-HOURLY BACKGRND options for the same sector
      IF (IBGMAX(IBGSECT) >= 1 .and.&
      &BFLAG(IBGSECT) /= FIELD(I) .and.&
      &FIELD(I) /= 'HOURLY') THEN
!           Issue error message for inconsistent options
         CALL ERRHDL(PATH,MODNAM,'E','165',FIELD(I))
      ELSE
! ---       Assign BACKGRND option to BFLAG for non-HOURLY data and assign
!           logical variables; L_BGFile(BGSect) for hourly values by sector
!           and L_BGHourly for hourly values in general
         IF (FIELD(I) /= 'HOURLY') THEN
            BFLAG(IBGSECT) = FIELD(I)
            L_BGValues(IBGSECT) = .TRUE.
         ELSE IF ( L_BGFile(IBGSECT) ) THEN
! ---          HOURLY BG File already specified for this sector
!              Issue error message for inconsistent options
            WRITE(DUMMY,'(''BGSECT'',I1)') IBGSECT
            CALL ERRHDL(PATH,MODNAM,'E','168',DUMMY)
         ELSE
!              Assign logical variables
            L_BGHourly        = .TRUE.
            L_BGFile(IBGSECT) = .TRUE.
         END IF
      END IF

   ELSE
! ---    Process BGSECTOR options
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC == 4) THEN
         IF (FIELD(3)(1:4) /= 'SECT') THEN
!              Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','BGSECTOR ID')
            GO TO 999
         ELSE
!              Error Message: No Numerical Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
      ELSE IF (IFC < 5) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF
! ---    Determine user-specified sector
      IF (FIELD(3) == 'SECT1') THEN
         IBGSECT = 1
      ELSE IF (FIELD(3) == 'SECT2' .and. NUMBGSects >= 2) THEN
         IBGSECT = 2
      ELSE IF (FIELD(3) == 'SECT3' .and. NUMBGSects >= 3) THEN
         IBGSECT = 3
      ELSE IF (FIELD(3) == 'SECT4' .and. NUMBGSects >= 4) THEN
         IBGSECT = 4
      ELSE IF (FIELD(3) == 'SECT5' .and. NUMBGSects >= 5) THEN
         IBGSECT = 5
      ELSE IF (FIELD(3) == 'SECT6' .and. NUMBGSects == 6) THEN
         IBGSECT = 6
      ELSE
!           Error Message: Invalid sector field
         CALL ERRHDL(PATH,MODNAM,'E','203','BGSECTOR ID')
         GO TO 999
      END IF
! ---    Set field index for the user-specified BACKGRND option and
!        assign the option to BFLAG variable
      I = 4

! ---    Check for inconsistent non-HOURLY BACKGRND options for the same sector
      IF (IBGMAX(IBGSECT) >= 1 .and.&
      &BFLAG(IBGSECT) /= FIELD(I) .and.&
      &FIELD(I) /= 'HOURLY') THEN
!           Issue error message for inconsistent options
         IF (LEN_TRIM(FIELD(I)) > 6) THEN
            WRITE(DUMMY,'(''SEC'',I1,1X,A:)') IBGSECT,&
            &FIELD(I)(1:LEN_TRIM(FIELD(I)))
            CALL ERRHDL(PATH,MODNAM,'E','165',DUMMY)
         ELSE
            WRITE(DUMMY,'(''SECT'',I1,1X,A:)') IBGSECT,&
            &FIELD(I)(1:LEN_TRIM(FIELD(I)))
            CALL ERRHDL(PATH,MODNAM,'E','165',DUMMY)
         END IF
      ELSE
! ---       Assign BACKGRND option to BFLAG for non-HOURLY data and assign
!           logical variables; L_BGFile(BGSect) for hourly values by sector
!           and L_BGHourly for availability of hourly values in general
         IF (FIELD(I) /= 'HOURLY') THEN
            BFLAG(IBGSECT) = FIELD(I)
            L_BGValues(IBGSECT) = .TRUE.
         ELSE IF ( L_BGFile(IBGSECT) ) THEN
! ---          HOURLY BG File already specified for this sector
!              Issue error message for inconsistent options
            WRITE(DUMMY,'(''BGSECT'',I1)') IBGSECT
            CALL ERRHDL(PATH,MODNAM,'E','168',DUMMY)
         ELSE
            L_BGHourly        = .TRUE.
            L_BGFile(IBGSECT) = .TRUE.
         END IF
      END IF
   END IF

! --- First check for hourly background file
   IF (L_BGFile(IBGSECT) .and. FIELD(I) == 'HOURLY') THEN
! ---    Hourly background concentration option selected;

      IF ((LOCE(I+1)-LOCB(I+1)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         BKGRND_File(IBGSECT) = RUNST1(LOCB(I+1):LOCE(I+1))
      ELSE
!           WRITE Error Message:  BKGRND_File Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         GO TO 999
      END IF

! ---    Assign file unit for this BKG file and Open The BACKGRND Input File
!        Open with ACTION='READ' to prevent overwrite and multiple access
      IBGUNT(IBGSECT) = 2000 + IBGSECT

! ---    Open Hourly Emissions Data File If Not Already Open,
!        first check whether file is open by filename
      INQUIRE (FILE=BKGRND_File(IBGSECT),OPENED=FOPEN)

      IF (.NOT. FOPEN) THEN
! ---       Open Hourly BACKGRND Data File If Not Already Open
!           Open with ACTION='READ' to prevent overwrite and allow multiple access;
!           First check for whether file is open by file unit
         INQUIRE (UNIT=IBGUNT(IBGSECT),OPENED=FOPEN)
         IF (.NOT. FOPEN) THEN
            OPEN(UNIT=IBGUNT(IBGSECT),ERR=998,&
            &FILE=BKGRND_File(IBGSECT),IOSTAT=IOERRN,&
            &ACTION='READ',STATUS='OLD')
         ELSE
!              Hourly BACKGRND File is Already Opened With Different Filename
            CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
            GO TO 999
         END IF
      ELSE
!           File with same name as Hourly BACKGRND file is already opened,
!           possible conflict with another file type
         CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
         GO TO 999
      END IF

! ---    Check for optional hourly BACKGRND file format
      IF (IFC == I+2) THEN
!           Check for Format String > ILEN_FLD PARAMETER
         IF ((LOCE(I+2)-LOCB(I+2)) <= (ILEN_FLD - 1)) THEN
!              Retrieve Met Format as Char. Substring
            BGFORM(IBGSECT) = RUNST1(LOCB(I+2):LOCE(I+2))

! ---          First check for user input of "FREE" for the formaat,
!              using FIELD array which has been converted to upper case
            IF (FIELD(I+2) == 'FREE') THEN
               BGFORM(IBGSECT) = 'FREE'
            ELSE
! ---             Check for correct format specifiers for BACKGRND file;
!                 should be 4 integers for date variables and 1 real for
!                 background concentration; allow for 1 to 4 integers since
!                 format statement may include 4I2, and also allow for
!                 either F, E, or D format for the data variable.
               DO J = 1, LEN_TRIM(BGFORM(IBGSECT))
                  IF (BGFORM(IBGSECT)(J:J)=='I' .or.&
                  &BGFORM(IBGSECT)(J:J)=='i') THEN
                     NumInt  = NumInt  + 1
                  ELSE IF (BGFORM(IBGSECT)(J:J)=='F' .or.&
                  &BGFORM(IBGSECT)(J:J)=='f') THEN
                     NumReal = NumReal + 1
                  ELSE IF (BGFORM(IBGSECT)(J:J)=='E' .or.&
                  &BGFORM(IBGSECT)(J:J)=='e') THEN
                     NumReal = NumReal + 1
                  ELSE IF (BGFORM(IBGSECT)(J:J)=='D' .or.&
                  &BGFORM(IBGSECT)(J:J)=='d') THEN
                     NumReal = NumReal + 1
                  ELSE IF (BGFORM(IBGSECT)(J:J)=="'" .or.&
                  &BGFORM(IBGSECT)(J:J)=='"' .or.&
                  &BGFORM(IBGSECT)(J:J)=='-' .or.&
                  &BGFORM(IBGSECT)(J:J)==';') THEN
!                       Check for invalid characters in BGFORM and issue
!                       Warning Message for dash, semicolon, single or
!                       double quotes embedded within the FORMAT.
                     WRITE(DUMMY,'(''InvalidChr '',A:)')&
                     &BGFORM(IBGSECT)(J:J)
                     CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
                  END IF
               END DO
               IF (NumInt<1 .or. NumInt>4) THEN
!                    WRITE Warning Message:  Potential problem with BGFORM
                  WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
               IF (NumReal/=1) THEN
!                    WRITE Warning Message:  Potential problem with BGFORM
                  WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
            END IF
         ELSE
!              WRITE Error Message:  BGFORM Field is Too Long
            WRITE(DUMMY,'(''LEN='',I6)') LOCE(I+2)-LOCB(I+2)
            CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
         END IF
      ELSE IF (IFC == I+1) THEN
! ---       Use 'free' format as the default
         BGFORM(IBGSECT) = 'FREE'
      ELSE IF (IFC > I+2) THEN
! ---       Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

   ELSE
! ---    Using BACKGRND option varying by SEASON, HROFDY, etc.

! ---    Assign number of background values based on BFLAG option
      IF (BFLAG(IBGSECT) == 'ANNUAL') THEN
         IBGMAX(IBGSECT) = 1
      ELSE IF (BFLAG(IBGSECT) == 'SEASON') THEN
         IBGMAX(IBGSECT) = 4
      ELSE IF (BFLAG(IBGSECT) == 'MONTH') THEN
         IBGMAX(IBGSECT) = 12
      ELSE IF (BFLAG(IBGSECT) == 'HROFDY') THEN
         IBGMAX(IBGSECT) = 24
      ELSE IF (BFLAG(IBGSECT) == 'WSPEED') THEN
         IBGMAX(IBGSECT) = 6
      ELSE IF (BFLAG(IBGSECT) == 'SEASHR') THEN
         IBGMAX(IBGSECT) = 96
      ELSE IF (BFLAG(IBGSECT) == 'HRDOW') THEN
         IBGMAX(IBGSECT) = 72
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (BFLAG(IBGSECT) == 'HRDOW7') THEN
         IBGMAX(IBGSECT) = 168
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (BFLAG(IBGSECT) == 'SHRDOW') THEN
         IBGMAX(IBGSECT) = 288
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (BFLAG(IBGSECT) == 'SHRDOW7') THEN
         IBGMAX(IBGSECT) = 672
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (BFLAG(IBGSECT) == 'MHRDOW') THEN
         IBGMAX(IBGSECT) = 864
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (BFLAG(IBGSECT) == 'MHRDOW7') THEN
         IBGMAX(IBGSECT) = 2016
         L_DayOfWeekOpts = .TRUE.
      ELSE
!           WRITE Error Message    ! Invalid BFLAG Field Entered
         CALL ERRHDL(PATH,MODNAM,'E','203','BFLAG')
         GO TO 999
      END IF

! ---    Call BGFILL to fill BACKGRND value arrays
      CALL BGFILL

   END IF

   GO TO 999

!     Process Error Messages; error opening file, include file type and sector
998 CONTINUE
   IF (.NOT. L_BGSector) THEN
      CALL ERRHDL(PATH,MODNAM,'E','500','BG File')
   ELSE
      WRITE(DUMMY,'("BGFILE SECT",I1)') IBGSECT
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
   END IF

999 RETURN
END SUBROUTINE BACK_GRND

SUBROUTINE BGFILL
!***********************************************************************
!                 BGFILL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Fill Variable Background Concentration Array
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Direction Specific Building Directions
!
!        CALLED FROM:   BACK_GRND
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K

!     Variable Initializations
   MODNAM = 'BGFILL'

! --- Initialize counter for number of BACKGRND values for this sector
   ISET = IBKGRD(IBGSECT)

! --- Assign field number for start of data values based on whether
!     sector-varying values are used
   IF (L_BGSector) THEN
      I = 5
   ELSE
      I = 4
   END IF

   DO K = I, IFC
!        Change Fields To Numbers
      CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         CYCLE
      END IF
      DO J = 1, IMIT
         ISET = ISET + 1
!           Assign The Field
         IF (ISET <= IBGMAX(IBGSECT)) THEN
            BACKGRND(ISET,IBGSECT) = DNUM
            IF (DNUM < 0.0D0) THEN
!                 WRITE Error Message:  Negative Value for BACKGRND
               CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
            END IF
         ELSE
!              WRITE Error Message    ! Too Many BACKGRND Values Input
            IF (L_BGSector) THEN
               WRITE(DUMMY,'(''BCKGRD SECT'',I1)') IBGSECT
            ELSE
               WRITE(DUMMY,'(''BACKGRND'')')
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','231',DUMMY)
            GO TO 99
         END IF
      END DO
   END DO

99 CONTINUE

! --- Save counter on number of values input so far for this sector
   IBKGRD(IBGSECT) = ISET

   RETURN
END SUBROUTINE BGFILL

SUBROUTINE BACK_UNIT
!***********************************************************************
!                 BACK_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user-specified units for BACKGROUND
!                 concentrations,
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'BACK_UNIT'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 3) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Check for units of background values

   IF (FIELD(3)=='UG/M3') THEN
      BackUnits = FIELD(3)
   ELSE IF (FIELD(3)=='PPM' .or. FIELD(3)=='PPB') THEN
      IF (POLLUT == 'NO2' .or. POLLUT == 'SO2'&
      &.or. POLLUT == 'CO') THEN
!RCO D14 AERMOD will only convert background from ppb or ppm for
!    SO2, NO2, or CO so do not let these units be specified
!    unless using these pollutants.
         BackUnits = FIELD(3)
      ELSE
!           Write Error Message:  Invalid units for background values
         CALL ERRHDL(PATH,MODNAM,'E','203','BackUnits')
      END IF
   END IF

999 RETURN
END SUBROUTINE BACK_UNIT

SUBROUTINE BGSECTOR
!***********************************************************************
!                 BGSECTOR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user-specified WD sectors for background
!                 concentrations of pollutant being modeled, based on
!                 the BGSECTOR keyword
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:       September 10, 2013
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I
   LOGICAL :: L_BadData

!     Variable Initializations
   MODNAM = 'BGSECTOR'
   L_BadData = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 4) THEN
!        Error Message: Too Few Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 8) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

! --- Set L_BGSector logical variable
   L_BGSector = .TRUE.

   DO I = 3, IFC
!        Loop through fields for starting directions for each BGSECTOR
      CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         WRITE(DUMMY,'("BGSECT",I1)') I-2
         CALL ERRHDL(PATH,MODNAM,'E','208',DUMMY)
!           Assign logical variable for bad data, but cycle through full record
         L_BadData = .TRUE.
         CYCLE
      END IF
      BGSECT(I-2) = DNUM
      IF (BGSECT(I-2) < 0.0D0 .or. BGSECT(I-2) > 360.0D0) THEN
!           Sector value out-of-range
         IF (BGSECT(I-2) > 9999.0D0) THEN
            WRITE(DUMMY,'("BGSECT>9999.")')
         ELSE IF (BGSECT(I-2) < -999.0D0) THEN
            WRITE(DUMMY,'("BGSECT<-999.")')
         ELSE
            WRITE(DUMMY,'("BGSECT=",F5.0)') BGSECT(I-2)
         END IF
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      END IF
   END DO

! --- Check for presence of bad sector data
   IF (L_BadData) GO TO 999

! --- Assign variable for number of user-specified BACKGRND sectors
   NUMBGSects = IFC-2

! --- Check BGSECTs for proper order and minimum sector widths
   DO I = 1, NUMBGSects-1
      IF (BGSECT(I+1) < BGSECT(I) ) THEN
!           Sector value out-of-order
         WRITE(DUMMY,'("BGSECT",I1," < #",I1)') I+1, I
         CALL ERRHDL(PATH,MODNAM,'E','222',DUMMY)
      ELSE IF (BGSECT(I+1) < BGSECT(I)+30.0D0 ) THEN
!           Sector width < 30 degrees
         WRITE(DUMMY,'("BGSECT",I1," < 30")') I+1
         CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
      ELSE IF (BGSECT(I+1) < BGSECT(I)+60.0D0 ) THEN
!           Sector width < 60 degrees
         WRITE(DUMMY,'("BGSECT",I1," < 60")') I+1
         CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
      END IF
   END DO
! --- Now check for width of last sector
   IF ( (BGSECT(1)+360.0D0)-BGSECT(NUMBGSects) < 30.0D0) THEN
!        Sector width < 30 degrees
      WRITE(DUMMY,'("BGSECT",I1," < 30")') NUMBGSects
      CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
   ELSE IF ( (BGSECT(1)+360.0D0)-BGSECT(NUMBGSects) < 60.0D0) THEN
!        Sector width < 60 degrees
      WRITE(DUMMY,'("BGSECT",I1," < 60")') NUMBGSects
      CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
   END IF

999 RETURN
END SUBROUTINE BGSECTOR

SUBROUTINE BL_AVGINP
!***********************************************************************
!              BL_INPUTS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Buoyant Line Source Average Plume Rise
!                 Parameters
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:     June 30, 2015
!
!        MODIFIED: May 2020
!          - updated to allow processing of multiple buoyant lines
!          - changed the name of the subroutine
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Buoyant Line Source Parameters
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE

   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   CHARACTER (LEN=12) :: TEMPID                    ! Multiple_BuoyLines_D41_Wood
! JAT 06/22/21 D065
! REMOVE I AS UNUSED VARIABLE
!      INTEGER :: I, ISDX, KK                       ! Multiple_BuoyLines_D41_Wood
   INTEGER :: ISDX, KK
   LOGICAL :: FOUND                             ! Multiple_BuoyLines_D41_Wood

!     Variable Initializations
   MODNAM = 'BL_AVGINP'                         ! Multiple_BuoyLines_D41_Wood

! --- Check the number of fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 8) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 9) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

! Multiple_BuoyLines_D41_Wood
!     Modified to check if BLPINPUT record has (9 fields) or does not have
!      (8 fileds) a BL group ID; parameter names changed to account for
!      multiple buoyant line sources (groups)
!
   IF (IFC == 8) THEN
! ---    BL group ID is not included on BLPINPUT card - assumption is
!        that there is only one BL source in this model run and all the
!        individual lines belong to that source

! ---    Check to see if either a BLPINPUT record with group ID = 'ALL'
!         or without a group ID has been processed (resulting in
!         NUMBLAVGINP >= 1);
!         if true, this is an error since all lines are considered
!         part of a single BL source when there are only 8 fields on a
!         BLPINOUT record
      IF (NUMBLAVGINP >= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE
!           BLPINPUT_Check_D091_Wood: start
!           If there is no BUOYLINE source type in the input file, then
!           array BLAVGINP_GRPID is not allocated.
         IF (NBLP == 0) THEN
!              There are no BOUYLINE sources but there is a BLPINPUT record;
!              issue an error message and skip the remainder of the subroutine
            CALL ERRHDL(PATH,MODNAM,'E','508','  ')
            GO TO 999
         ELSE
! ---          NUMBLAVGINP = 0 - this is the first BLPINPUT record processed
!              and there is no group ID - set it to ALL;
            NUMBLAVGINP = 1
            BLAVGINP_GRPID(NUMBLAVGINP) = 'ALL'
         END IF
!           BLPINPUT_Check_D091_Wood: end
      END IF

!        Fetch Each Field
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 3 = average line length
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_LLEN(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field  - Field 4 = average bldg height
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_BHGT(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 5 = average bldg width
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_BWID(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 6 = average line width
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_LWID(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 7 = average bldg separation
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_BSEP(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 8 = average line source buoyancy
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_FPRM(NUMBLAVGINP) = DNUM

   ELSE IF (IFC == 9) THEN
! ---    A BLPINPUT ID is required to be in the 3rd field on the
!          BLPINPUT record; store it in a temporary variable
      TEMPID = FIELD(3)

! ---    Check to see if this BLPINPUT ID has been defined - issue an
!        error messsage if it has and stop processing the record;
!        otherwise add the name to the BL group ID array
      IF (NUMBLAVGINP > 0) THEN
         DO KK = 1,NUMBLAVGINP
            CALL SINDEX(BLAVGINP_GRPID,NUMBLAVGINP,&
            &TEMPID,ISDX,FOUND)
            IF (FOUND) THEN
!                 This group ID has been seen before - all BL group IDs
!                 must be unique for this keyword
               CALL ERRHDL(PATH,MODNAM,'E','504',TEMPID)
               GO TO 999
            ELSE
!                 Add group ID to the BL group ID array
               NUMBLAVGINP = NUMBLAVGINP + 1
               BLAVGINP_GRPID(NUMBLAVGINP) = TEMPID
            END IF
            EXIT
         END DO
      ELSE
!           First BLPINPOUT record and there is a BL group ID in field 3
         NUMBLAVGINP = NUMBLAVGINP + 1
         BLAVGINP_GRPID(NUMBLAVGINP) = TEMPID
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 4 = average line length
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_LLEN(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field  - Field 5 = average bldg height
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_BHGT(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 6 = average bldg width
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_BWID(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 7 = average line width
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_LWID(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 8 = average bldg separation
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_BSEP(NUMBLAVGINP) = DNUM

      CALL STODBL(FIELD(9),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field - Field 9 = average line source buoyancy
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      BLAVGINP_FPRM(NUMBLAVGINP) = DNUM
   END IF

!     Set the logical to true indicating that the model run has a buoyant
!      line source
   L_BLSOURCE = .TRUE.

999 RETURN
END SUBROUTINE BL_AVGINP

SUBROUTINE BL_ROTATE1 (KK)
!***********************************************************************
!              B_ROTATE1 Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Stores Source Coordinates in Derived Type Array;
!                 Translates and Rotates Source for Buoyant Line Source
!                 Calculations
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    June 30, 2015
!

!        MODIFIED: Wood_D32 - added check to determine if individual
!                  lines in a single source group are (nearly) parallel -
!                  within "Parallel_Tol" degrees, defined as a parameter
!                  in module BUOYANT_LINE, of each other
!

!        MODIFIED: Updated to handle initial rotation of multiple
!                  buoyant line sources
!                  (Wood E&IS, for 20xyz release)
!
!        INPUTS:  Endpoints of lines defeind on SO LOCATION cards
!                 KK = Buoyant line source group number
!
!        OUTPUTS: Rotated Source and Receptor Coordinates
!
!        CALLED FROM:   SOCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE

   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   LOGICAL :: BL_YCOORD_CHK

   INTEGER :: IL, ILP, KK, IPLSTART
   DOUBLE PRECISION :: DDX, DDY
   DOUBLE PRECISION :: XOR2, YOR2, DDX2, DDY2, ANGRAD2, ANGDIFF

   DOUBLE PRECISION :: XB1, XE1, YB1, YE1, YLBSAV, YLESAV

!     Variable Initializations
   MODNAM = 'BL_ROTATE1'
   BL_YCOORD_CHK = .FALSE.

! Multiple_BuoyLines_D41_Wood
!     Process buoyant lines by group (KK, a calling argument) to determine
!      required parameters to rotate source and later, the receptors, by group
!      and if the lines in the group are ordered properly;
!      some scalars converted to 1-D arrays (e.g., XOR, YOR)


   DO_IL: DO IL = 1,NBLTOTAL
      IF (BLINEPARMS(IL)%IBLPGRPNUM == KK) THEN
         IPLSTART = IL + 1                              ! Wood_D32: for lines parallel check

!           Compute parameters for initial rotation of sources and receptors
!            Variables with an array index means it is used in other routines
         XOR(KK) = BLINEPARMS(IL)%XBEG
         YOR(KK) = BLINEPARMS(IL)%YBEG
         DDX     = BLINEPARMS(IL)%XEND - XOR(KK)
         DDY     = BLINEPARMS(IL)%YEND - YOR(KK)


         ANGRAD(KK)  = DATAN2(DDY,DDX)                ! rotation angle for the lines
         TCOR(KK)    = 90.0D0 + ANGRAD(KK) * RTODEG   ! TCOR will be used in BL_ROTATE2
         SINTCOR(KK) = DSIN(ANGRAD(KK))               ! also used later
         COSTCOR(KK) = DCOS(ANGRAD(KK))               ! also used later


!           If the lines are oriented exactly north-south (90 degrees),
!            the cosine is very very small (E-17), which causes AERMOD to
!            generate results that do not match well with BLP.  Set COSTCOR
!            to E-05 (approx. equal to 0.5 degrees)
         IF (DABS(COSTCOR(KK)) < 1.0D-10) COSTCOR(KK) = 1.0D-05

         EXIT DO_IL
      END IF
   END DO DO_IL


! --- Perform the translation and rotation for BL group KK (a calling argument)
   DO IL = 1,NBLTOTAL
      IF (BLINEPARMS(IL)%IBLPGRPNUM == KK) THEN
         XB1 = BLINEPARMS(IL)%XBEG - XOR(KK)
         XE1 = BLINEPARMS(IL)%XEND - XOR(KK)
         YB1 = BLINEPARMS(IL)%YBEG - YOR(KK)
         YE1 = BLINEPARMS(IL)%YEND - YOR(KK)
         YB1 = -XB1*SINTCOR(KK) + YB1*COSTCOR(KK)
         BLINEPARMS(IL)%YBEG_TR1 = YB1

         XB1 = (XB1+YB1*SINTCOR(KK))/COSTCOR(KK)
         BLINEPARMS(IL)%XBEG_TR1 = XB1

         YE1 = -XE1*SINTCOR(KK) + YE1*COSTCOR(KK)
         YS_SCS(IL) = YE1
         BLINEPARMS(IL)%YEND_TR1 = YE1

         XE1 = (XE1+YE1*SINTCOR(KK))/COSTCOR(KK)
         BLINEPARMS(IL)%XEND_TR1 = XE1
      END IF
   END DO

   DO IL = 1, NBLTOTAL
!        Verify line source coordinates for BL source group KK (a calling
!          argument) are input correctly wrt to the y-axis
      IF (BLINEPARMS(IL)%IBLPGRPNUM == KK) THEN
         IF( .NOT. BL_YCOORD_CHK) THEN
            YLBSAV = BLINEPARMS(IL)%YBEG_TR1
            YLESAV = BLINEPARMS(IL)%YEND_TR1
            BL_YCOORD_CHK = .TRUE.

         ELSE
            IF (BLINEPARMS(IL)%YBEG_TR1 > YLBSAV .and.&
            &BLINEPARMS(IL)%YEND_TR1 > YLESAV) THEN
!                 The pair of lines are in the correct 'order' -
!                 save the coordinates and check the next pair
               YLBSAV = BLINEPARMS(IL)%YBEG_TR1
               YLESAV = BLINEPARMS(IL)%YEND_TR1
            ELSE
               CALL ERRHDL(PATH,MODNAM,'E','389',&
               &BLINEPARMS(IL)%SRCID)
            ENDIF
         ENDIF
      END IF

   END DO

! BuoyantLine_CheckLinesParallel_D32 (Wood)
!     Confirm that the individual lines within a buoyant line group/source
!       are within 5 degrees of the first line in the group.
!     If not issue a warning but continue processing
!     IPLSTART was determined above when determining the parameters for
!       first rotation

   DO ILP = IPLSTART, NBLTOTAL
      IF (BLINEPARMS(ILP)%IBLPGRPNUM == KK) THEN
         ANGRAD2 = 0.0D0
         XOR2 = BLINEPARMS(ILP)%XBEG
         YOR2 = BLINEPARMS(ILP)%YBEG

         DDX2 = BLINEPARMS(ILP)%XEND - XOR2
         DDY2 = BLINEPARMS(ILP)%YEND - YOR2
         ANGRAD2 = DATAN2(DDY2,DDX2)
         ANGDIFF = DABS(ANGRAD(KK)-ANGRAD2) *&
         &(180.0D0/(4.0D0*DATAN(1.0D0)))
         IF (ANGDIFF > Parallel_Tol) THEN
            CALL ERRHDL(PATH,MODNAM,'W','506',BLINEPARMS(ILP)%SRCID)
         END IF
      END IF

   END DO

   RETURN
END SUBROUTINE BL_ROTATE1

SUBROUTINE RLINEBAR_INPUTS
!***********************************************************************
!        RLINEBAR_INPUTS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process RLINE SouRCe CoNFiGuRation (RLINE BARrier) params
!
!        PROGRAMMER: M. Snyder
!
!        MODIFIED:   Modified to handle two barrier cases for RLINEXT sources.
!                    Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED: Duplicated and changed name. RLSRCCFG was broken into
!                  two keywords, RLINEBAR and RLINEDPR, so two subroutines.
!                  Wood, 12/29/2018
!                  Modified to read optional second barrier.
!                  D. Heist, Jan. 2021
!
!        DATE:    March 21, 2018
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: RLSOURCE parameters are populated
!
!        CALLED FROM:   SOCARD
!***********************************************************************
!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA

   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER  :: INDEXS
   CHARACTER (LEN=12)  :: SOID
   LOGICAL  :: FOUND

!     Variable Initializations
   MODNAM = 'RLBAR_INP'
   FOUND  = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC == 6) THEN
!        Error Message: Invalid Parameter Specified
      CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 7) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Retrieve Source ID Character Substring
   SOID = FIELD(3)

!     Match the ID to the SRCID array to get ISRC
   CALL SINDEX(SRCID, NSRC, SOID, INDEXS, FOUND)

   IF(FOUND) THEN
!     Check that it is a RLINEXT source!
      IF (SRCTYP(INDEXS) == 'RLINEXT') THEN
!        Fetch and check each of the numerical fields
         CALL STODBL(FIELD(4), ILEN_FLD, RLSOURCE(INDEXS)%HTWALL, IMIT)
!        Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STODBL(FIELD(5), ILEN_FLD, RLSOURCE(INDEXS)%DCLWALL, IMIT)
!        Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
!        Fetch and check numerical fields for second barrier
         IF (IFC == 7) THEN
            CALL STODBL(FIELD(6), ILEN_FLD, RLSOURCE(INDEXS)%HTWALL2,&
            &IMIT)
!          Check The Numerical Field
            IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            END IF
            CALL STODBL(FIELD(7), ILEN_FLD, RLSOURCE(INDEXS)%DCLWALL2,&
            &IMIT)
!          Check The Numerical Field
            IF (IMIT /= 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            END IF
         END IF
      ELSE
!        WRITE Error Message: "Keyword not available for RLINE type:"
         CALL ERRHDL(PATH,MODNAM,'E','278',KEYWRD)
      END IF
   ELSE
!        WRITE Error Message: Source Location Has Not Been Identified
      CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
   END IF

!     ERROR handling for barrier parameters
   IF ((RLSOURCE(INDEXS)%HTWALL< 0.0D0) .or.&
   &(RLSOURCE(INDEXS)%HTWALL2< 0.0D0)) THEN
!        WRITE Error Message:  Negative barrier height: HTWALL
      CALL ERRHDL(PATH,MODNAM,'E','209',' HTWALL ')
   ENDIF

999 RETURN
END SUBROUTINE RLINEBAR_INPUTS

SUBROUTINE RLINEDPR_INPUTS
!***********************************************************************
!        RLINEDPR_INPUTS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process RLINE SouRCe CoNFiGuRation (RLINEDPR) params
!
!        PROGRAMMER: M. Snyder
!
!        MODIFIED: Duplicated and changed name. RLSRCCFG was broken into
!                  two keywords, RLINEBAR and RLINEDPR, so two subroutines.
!                  Wood, 12/29/2018
!
!        DATE:    March 21, 2018
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: RLSOURCE parameters are populated
!
!        CALLED FROM:   SOCARD
!***********************************************************************
!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA

   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER  :: INDEXS
   CHARACTER (LEN=12)  :: SOID
   LOGICAL  :: FOUND

!     Variable Initializations
   MODNAM = 'RLDPR_INP'
   FOUND  = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 6) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 6) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Retrieve Source ID Character Substring
   SOID = FIELD(3)

!     Match the ID to the SRCID array to get ISRC
   CALL SINDEX(SRCID, NSRC, SOID, INDEXS, FOUND)

   IF(FOUND) THEN
      IF(SRCTYP(INDEXS) == 'RLINEXT') THEN
!          Fetch and check each of the numerical fields
         CALL STODBL(FIELD(4),ILEN_FLD,RLSOURCE(INDEXS)%DEPTH,IMIT)
!          Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STODBL(FIELD(5),ILEN_FLD,RLSOURCE(INDEXS)%WTOP,IMIT)
!          Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STODBL(FIELD(6),ILEN_FLD,RLSOURCE(INDEXS)%WBOTTOM,IMIT)
!          Check The Numerical Field
         IF (IMIT /= 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE
!          WRITE Error Message: "Keyword only available for RLINEXT source type:"
         CALL ERRHDL(PATH,MODNAM,'E','278',KEYWRD)
      END IF
   ELSE
!        WRITE Error Message: Source Location Has Not Been Identified
      CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
   END IF

!     ERROR handling for barrier and depressed roadway parameters
   IF (RLSOURCE(INDEXS)%DEPTH> 0.0D0) THEN
!        WRITE Error Message:  Depth of depression is positive (should be negative)
      CALL ERRHDL(PATH,MODNAM,'E','320',' DEPTH ')
   ENDIF

   IF (RLSOURCE(INDEXS)%WTOP< 0.0D0) THEN
!        WRITE Error Message:  WTOP is negative (should be positive)
      CALL ERRHDL(PATH,MODNAM,'E','320',' WTOP ')
   ENDIF

   IF (RLSOURCE(INDEXS)%WBOTTOM < 0.0D0 .or.&
   &RLSOURCE(INDEXS)%WBOTTOM > RLSOURCE(INDEXS)%WTOP) THEN
!        WRITE Error Message:  WBOTTOM is negative or is greater than WTOP
      CALL ERRHDL(PATH,MODNAM,'E','320',' WBOTTOM ')
   ENDIF


999 RETURN
END SUBROUTINE RLINEDPR_INPUTS

SUBROUTINE AIRCRAFT
!***********************************************************************
!                AIRCRAFT  Module of AERMOD Model
!
!        PURPOSE: Processes Aircraft  Source Card
!
!        PROGRAMMER: Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Array of flags for Aircraft Sources
!
!        CALLED FROM: SOCARD
!***********************************************************************
!

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
   INTEGER :: I, IL, IH, K, ISTR
   LOGICAL :: INGRP, RMARK, FOUND

!     Variable Initializations
   MODNAM = 'AIRCRAFT'
   FOUND = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   END IF

!     Check for 'ARCFTSRC ALL' option identified in PRESET
   IF (L_ARCFT_ALL) THEN
      IF (IFC == 3 .and. FIELD(3) == 'ALL') THEN
         AFTSRC(:) = 'Y'
         GO TO 999
      ELSE
!           WRITE Error Message:  ARCFTSRC ALL
         CALL ERRHDL(PATH,MODNAM,'E','279','ARCFTSRC ALL')
         GO TO 999
      END IF

   ELSE
!        Specify field index to start for Source IDs
      ISTR = 3

   END IF

!     Loop Through Fields
   DO I = ISTR, IFC
      IF (INDEX(FIELD(I),'-') == 0) THEN
         FOUND = .FALSE.
         DO K = 1, NUMSRC
            IF (SRCID(K) == FIELD(I)) THEN
               FOUND = .TRUE.
               AFTSRC(K) = 'Y'
            END IF
         END DO
         IF (.NOT.FOUND) THEN
!              WRITE Error Message:  SRCID not found
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
            CYCLE
         END IF
      ELSE

         CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,LOWID,&
         &HIGID)
!           First Check Range for Upper Value < Lower Value
         CALL SETIDG(LOWID,LID1,IL,LID2)
         CALL SETIDG(HIGID,HID1,IH,HID2)
         IF ((HID1<LID1) .or. (IH<IL) .or. (HID2<LID2)) THEN
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            CYCLE
         END IF
         DO K = 1, NUMSRC
            CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
            IF (INGRP) THEN
               AFTSRC(K) = 'Y'
            END IF
         END DO
      END IF
   END DO

999 RETURN
END SUBROUTINE AIRCRAFT
