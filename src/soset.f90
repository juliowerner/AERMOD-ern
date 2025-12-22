subroutine socard
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
   use main1
   use rline_data, only: rlmovesconv, l_rdepress, l_rbarrier
   use buoyant_line

   implicit none
   character :: modnam*12

   integer :: i, j, ilsave, lnum, kk
! Unused INTEGER :: J

!     Variable Initializations
   modnam = 'SOCARD'

   if (keywrd == 'STARTING') then
!        Initialize Counters and Set Status Switch
      isrc = 0
      igrp = 0
      numsrc = 0
      numgrp = 0
      numolm = 0
      numpsd = 0
      numblavginp = 0       ! # BL groups defined on BLPINPUT records - D41_Wood
      numblgrps = 0         ! # BL groups defined by # of BLPINPUT records - D41_Wood
!        JAT 01/13/21 ISSUE D079
!        ONLY INITIALIZE NBLINGRP IF ALLOCATED
!        THIS CORRECTS A BUG INTRODUCED BY ISSUE D41
      if (allocated(nblingrp)) nblingrp = 0          ! # lines in each BL source/group - D41/Wood

!        BL_Source_Limit_D088: Wood
      if (allocated(igrp_blp)) igrp_blp = 0          ! 'Flag' indicating which BL group a BL line is in - D088/Wood
!        BL_Source_Limit_D088: Wood

      numcap = 0
      numhor = 0
      nurbsrc = 0
      naftsrc = 0                                    ! Added for Aircraft; UNC-IE
      numflat = 0
      isstat(1) = isstat(1) + 1
      if (isstat(1) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
         go to 999
      end if

!        Initialize the buoyant line source variables
      l_blsource = .false.
!        JAT 01/13/21 ISSUE D079
!        ONLY INITIALIZE L_BLURBAN AND BL_NUMURB IF ALLOCATED
!        THIS CORRECTS A BUG INTRODUCED BY ISSUE D41
      if (allocated(l_blurban)) l_blurban  = .false.
      if (allocated(bl_numurb)) bl_numurb  = 0

!        Initialize the RDEPRESS and RBARRIER source logicals
      l_rdepress = .false.
      l_rbarrier = .false.

!        Flush The Working Array
      iwrk2(:,:) = 0
   else if (keywrd == 'LOCATION') then
!        Set Status Switch
      isstat(2) = isstat(2) + 1
!        Check for SRCGROUP or PSDGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Process Source Location                            ---   CALL SOLOCA
      call soloca
   else if (keywrd == 'SRCPARAM') then
!        Set Status Switch
      isstat(3) = isstat(3) + 1
!        Check for SRCGROUP or PSDGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Process Source Parameters                          ---   CALL SOPARM
      call soparm

! --- PRIME ---------------------------------
   else if (keywrd == 'BUILDHGT' .or.&
   &keywrd == 'BUILDWID' .or.&
   &keywrd == 'BUILDLEN' .or.&
   &keywrd == 'XBADJ   ' .or.&
   &keywrd == 'YBADJ   ') then
! -------------------------------------------

!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Set Status Switch
      if (keywrd == 'BUILDHGT') then
         isstat(4) = isstat(4) + 1
      else if (keywrd == 'BUILDWID') then
         isstat(5) = isstat(5) + 1

! --- PRIME -----------------------------------
      else if (keywrd == 'BUILDLEN') then
         isstat(21) = isstat(21) + 1
      else if (keywrd == 'XBADJ   ') then
         isstat(22) = isstat(22) + 1
      else if (keywrd == 'YBADJ   ') then
         isstat(23) = isstat(23) + 1
! ---------------------------------------------

      end if
!        Process Direction-specific Building Dimensions     ---   CALL DSBLDG
      call dsbldg

! --- PLATFORM --------------------------------
!CRT  D063 Platform Downwash
!     CRT, 1/18/2012: add keyword PLATFORM for platform downwash
!     MGS, 10/2/2020: changed ISSTAT index to 47 from 42 (taken in v19191)
   else if (keywrd == 'PLATFORM') then

      if(.not. l_alpha) then
!           WRITE Error Message:  "Non-DFAULT ALPHA option required"
!           for PLATFORM
         call errhdl(path,modnam,'E','198',' PLATFORM ')
      end if

!        Set Status Switch
      isstat(47) = isstat(47) + 1

!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if

!           Process the PLATFM Card                      ---   CALL PLATFM
      call platfm
! ---------------------------------------------

   else if (keywrd == 'EMISFACT') then
!        Set Status Switch
      isstat(7) = isstat(7) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Process Variable Emission Rate Factors             ---   CALL EMVARY
      call emvary
   else if (keywrd == 'EMISUNIT') then
!        Set Status Switch
      isstat(8) = isstat(8) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (isstat(8) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else if (numtyp == 1) then
!           Process Emission Rate Unit Conversion Factors   ---   CALL EMUNIT
         call emunit
      else
!           WRITE Error Message: EMISUNIT Keyword with more than 1 output type
         call errhdl(path,modnam,'E','158',' ')
      end if
!     Accept MOVES units (g/hr/link) for conversion to RLINE units, should follow LOCATION
   else if (keywrd == 'RLEMCONV') then
!        Set Status Switch
      isstat(12) = isstat(12) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Check that RLEMCONV is NOT repeated
      if (isstat(12) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
!MGS         ELSE IF(.NOT. BETA) THEN
!           WRITE Error Message: "Non-DFAULT BETA option required" for RLINE source type
!MGS            CALL ERRHDL(PATH,MODNAM,'E','199','RLEMCONV')
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (end)
!        Check The Number Of The Fields
      else if (ifc > 2) then
!           WRITE Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
      else
!           Set RLINE MOVES conversion switch to TRUE
         rlmovesconv = .true.
      end if
   else if (keywrd == 'PARTDIAM' .or. keywrd == 'MASSFRAX' .or.&
   &keywrd == 'PARTDENS') then
!        Set Status Switch
      if (keywrd == 'PARTDIAM') then
         isstat(9) = isstat(9) + 1
      else if (keywrd == 'MASSFRAX') then
         isstat(10) = isstat(10) + 1
      else if (keywrd == 'PARTDENS') then
         isstat(11) = isstat(11) + 1
      end if
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Process Particle Deposition Parameters             ---   CALL PARTDEP
      call partdep

   else if (keywrd == 'ELEVUNIT') then
!        Set Status Switch
      isstat(15) = isstat(15) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (isstat(15) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else if (numsrc > 0) then
!           Write Error Message: ELEVUNIT must be first card after STARTING
         call errhdl(path,modnam,'E','152','  SO')
      else
!           Process Elevation Units for Source Elevations   ---   CALL SOELUN
         call soelun
      end if
   else if (keywrd == 'HOUREMIS') then
!*       Set Status Switch
      isstat(16) = isstat(16) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Set HOURLY Flag
      hourly = .true.
!*       Process Hourly Emissions                           ---   CALL HREMIS
      call hremis
!*#

   else if (keywrd == 'CONCUNIT') then
!        Set Status Switch
      isstat(17) = isstat(17) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (isstat(17) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
      if (isstat(8) /= 0) then
!           WRITE Error Message: Conflict with EMISUNIT
         call errhdl(path,modnam,'E','159',keywrd)
      else
!           Process Emission Rate Unit Conversion Factors   ---   CALL COUNIT
         call counit
      end if
   else if (keywrd == 'DEPOUNIT') then
!        Set Status Switch
      isstat(18) = isstat(18) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (isstat(18) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
      if (isstat(8) /= 0) then
!           WRITE Error Message: Conflict with EMISUNIT
         call errhdl(path,modnam,'E','159',keywrd)
      else
!           Process Emission Rate Unit Conversion Factors   ---   CALL DPUNIT
         call dpunit
      end if

   else if (keywrd == 'AREAVERT') then
!        Set Status Switch
      isstat(19) = isstat(19) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Process Vertices for AREAPOLY Sources              ---   CALL ARVERT
      call arvert

   else if (keywrd == 'INCLUDED') then
!        Set Status Switch
      isstat(20) = isstat(20) + 1
!        Save ILINE as ISAVE
      ilsave = iline
!        Process the Included Receptor File                 ---   CALL INCLUD
      call includ
!        Retrieve ILINE From ISAVE
      iline = ilsave

   else if (keywrd == 'SRCGROUP') then
!        Set Status Switch
      isstat(24) = isstat(24) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not. psdcredit) then
!           Process Source Groups                           ---   CALL SOGRP
         call sogrp
      else
!           Write Error Message: SRCGROUP specified with PSDCREDIT option
         call errhdl(path,modnam,'E','105',keywrd)
      end if

   else if (keywrd == 'GASDEPOS') then
!     JAT 7/02/19 added from 18081
!     given general lack of testing on gas deposition
!     now making gas deposition an alpha option
!     note other gas deposition keywords GDSEASON, GDLANUSE
!     occur in the CO pathway and are also checked
      if (l_alpha) then
!             Set Status Switch
         isstat(26) = isstat(26) + 1
!             Check for SRCGROUP Card Out Of Order
         if (.not.psdcredit .and. isstat(24) /= 0) then
            call errhdl(path,modnam,'E','140','SRCGROUP')
         else if (psdcredit .and. isstat(34) /= 0) then
            call errhdl(path,modnam,'E','140','PSDGROUP')
         end if
         if (.not. luservd) then
!                 Process Gas Deposition Parameters               ---   CALL GASDEP
            call gasdep
         else
!                 Write Error Message:  User-specified deposition velocity
            call errhdl(path,modnam,'E','195',keywrd)
         end if
      else !issue error
         call errhdl(path,modnam,'E','198','GASDEPOS')
      endif
   else if (keywrd == 'METHOD_2') then
!     JAT 6/25/19 added from 18081
!     given changes to scavenging ratio calculations in subroutine
!     scavrat for method 2, and general lack of testing on method 2
!     now making method 2 an alpha option
      if (l_alpha) then
!             Set Status Switch
         isstat(27) = isstat(27) + 1
! ---         Check for DFAULT option; METHOD_2 is considered non-DFAULT
         if (dfault) then
!                 Write Error Message:  METHOD_2 w/ DFAULT Option
            call errhdl(path,modnam,'E','197','METHOD_2') !shouldn't happen now JAT 6/25/19 but leave in
         else
!                 Set flag for use of non-DEFAULT option
            L_NonDFAULT = .true.
!                 Check for SRCGROUP Card Out Of Order
            if (.not.psdcredit .and. isstat(24) /= 0) then
               call errhdl(path,modnam,'E','140','SRCGROUP')
            else if (psdcredit .and. isstat(34) /= 0) then
               call errhdl(path,modnam,'E','140','PSDGROUP')
            end if
!                 Process Method 2 Deposition Parameters          ---   CALL METH_2
            call meth_2
         end if
      else !issue error
         call errhdl(path,modnam,'E','198','METHOD_2')
      endif
   else if (keywrd == 'URBANSRC') then
!        Set Status Switch
      isstat(28) = isstat(28) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (urban) then
!           Process the Urban Source Card                   ---   CALL URBANS
         call urbans
      else
!           Write Error Message:  Urban source defined without URBANOPT card
         call errhdl(path,modnam,'E','130','URBANOPT')
      end if
   else if (keywrd == 'NO2RATIO') then
!        Set Status Switch
      isstat(29) = isstat(29) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (pvmrm .or. olm&
      &.or. runttrm .or. grsm) then
!           Process the NO2 Ratio Card                      ---   CALL NO2RAT
         call no2rat
      else
!           Write Error Message:  NO2RATIO specified without PVMRM, OLM or GRSM
         call errhdl(path,modnam,'E','600',keywrd)
      end if

   else if (keywrd == 'OLMGROUP') then
!        Set Status Switch
      isstat(30) = isstat(30) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (olm) then
!           Process the OLM Group Card                      ---   CALL OLMGRP
         call olmgrp
      else
!           Write Error Message:  OLMGROUP specified without OLM
         call errhdl(path,modnam,'E','144',keywrd)
      end if

   else if (keywrd == 'PSDGROUP') then
!        Set Status Switch
      isstat(34) = isstat(34) + 1
!        Check for PSDGROUP Card Out Of Order
      if (psdcredit) then
!           Process the PSD Group Card                      ---   CALL PSDGRP
         call psdgrp
      else
!           Write Error Message: PSDGROUP specified without PSDCREDIT option
         call errhdl(path,modnam,'E','146',keywrd)
      end if

   else if (keywrd == 'BACKGRND') then
!        Set Status Switch
      isstat(40) = isstat(40) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
!        Process the BACKGRND Card                         ---   CALL BACK_GRND
      call back_grnd

   else if (keywrd == 'BACKUNIT') then
!        Set Status Switch
      isstat(41) = isstat(41) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (isstat(41) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process the BACKUNIT Card                      ---   CALL BACK_UNIT
         call back_unit
      end if

   else if (keywrd == 'BGSECTOR') then
!        Set Status Switch
      isstat(42) = isstat(42) + 1
      if (isstat(42) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process BGSECTOR keyword                       ---   CALL BGSECTOR
         call bgsector
      end if

!     Check for RLINE barrier input
   else if (keywrd == 'RBARRIER') then
!         Added FLAT restriction in addition to the ALPHA restriction Wood 10/7
      if(l_alpha .and. flat) then
!           Set the RBARRIER logical to true
         l_rbarrier = .true.
!           Process RBARRIER keyword                        ---   CALL RLINEBAR_INPUTS
         call rlinebar_inputs
      else
!           Add error message if FLAT is not included
         if (.not. flat) then
! ---                ERROR MESSAGE THAT BARRIER SOURCE REQUIRES 'FLAT' MODELOPT
            call errhdl(path,modnam,'E','713','')
         end if
!           Add if statement to only write error messge if the ALPHA option is not included
         if (.not. l_alpha) then
!               WRITE Error Message:  "Non-DFAULT ALPHA option required"
!               for RBARRIER defining source configuration parameters
            call errhdl(path,modnam,'E','198',' RBARRIER ')
         end if
      end if
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
   else if (keywrd == 'RDEPRESS') then
!         Added FLAT restriction in addition to the ALPHA restriction Wood 10/7
      if(l_alpha .and. flat) then
!           Set the RDEPRESS logical to true
         l_rdepress = .true.
!           Process RDEPRESS keyword                        ---   CALL RLINEDPR_INPUTS
         call rlinedpr_inputs
      else
!           Add error message if FLAT is not included
         if (.not. flat) then
! ---          ERROR MESSAGE THAT DEPRESSED SOURCE REQUIRES 'FLAT' MODELOPT
            call errhdl(path,modnam,'E','713','')
         end if
!           Add if statement to only write error messge if the ALPHA option is not included
         if (.not. l_alpha) then
!              WRITE Error Message:  "Non-DFAULT ALPHA option required"
!              for RDEPRESS defining source configuration parameters
            call errhdl(path,modnam,'E','198',' RDEPRESS ')
         end if
      end if
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

   else if (keywrd == 'BLPINPUT') then
!        Multiple_BuoyLines_D041_Wood: start
!        Removed code that did not allow for multiple BLPINPUT records

!        Set Status Switch
      isstat(43) = isstat(43) + 1
!        Multiple_BuoyLines_D041_Wood: end
!        Subroutine name changed
!        Process BLPINPUT keyword                           ---   CALL BL_AVGINP
      call bl_avginp

   else if (keywrd == 'BLPGROUP') then
!        Set Status Switch
! Multiple_BuoyLines_D41_Wood
!        Added routine to process BLPGRPOUP keyword(s); removed message that
!        keyword was not active in version 19191
      isstat(44) = isstat(44) + 1
!         IF (ISSTAT(44) .NE. 1) THEN
!           WRITE Error Message:
!            CALL ERRHDL(PATH,MODNAM,'W','385','combined')
!         ELSE
!           Process BL_INPUTS keyword                       ---   CALL BLPGRP
      call blpgrp
!         END IF
!**  Added for Aircraft Plume Rise; UNC-IE
   else if (keywrd == 'ARCFTSRC') then
!        Set Status Switch
      isstat(48) = isstat(48) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if

      if(arcft) then
!            Process the Aircraft Source Card  --- CALL AIRCRAFT
         call aircraft
!           check for HOURLY Flag means check for hourly emission file
!           that is must for Aircraft Sources
         if (.not. hourly)then
!            WRITE Error Message for Hourly Emission File with Aircraft Sources
            call errhdl(path,modnam,'E','823','HOUREMIS')
         end if
      else
!           Write Error Message; Aircraft Source Group defined without ARCFTOPT card
         call errhdl(path,modnam,'E','821','ARCFTOPT')
      end if
!**  End Aircraft Plume Rise insert; April 2023
! Added for HBP, JAN 2023
   else if (keywrd == 'HBPSRCID') then
!        Set Status Switch
      isstat(45) = isstat(45) + 1
!        Check for SRCGROUP Card Out Of Order
      if (.not.psdcredit .and. isstat(24) /= 0) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      else if (psdcredit .and. isstat(34) /= 0) then
         call errhdl(path,modnam,'E','140','PSDGROUP')
      end if
      if (hbplume) then
         if (l_alpha) then
!             Process the HBP Source Card                   ---   CALL HBPSOURCE
            call hbpsource
         else
!           WRITE Error Message:  "Non-DFAULT ALPHA option required"
!           for HBPSRC defining source configuration parameters
            call errhdl(path,modnam,'E','198','HBPSRCID')
         endif
      else
!           Write Error Message:  HBP source defined without HBPLUME card
         call errhdl(path,modnam,'E','130','HBPLUME')
      end if
! End HBP Insert
   else if (keywrd == 'FINISHED') then
!        Set Status Switch
      isstat(50) = isstat(50) + 1
      if (isstat(50) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if

!        Check to Insure That SRCGROUP or PSDGROUP Was The Last Functional Keyword
      if (pkeywd /= 'SRCGROUP' .and. .not.psdcredit) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      end if
      if (pkeywd /= 'PSDGROUP' .and. psdcredit) then
         call errhdl(path,modnam,'E','140','SRCGROUP')
      end if

!        Check for Missing Mandatory Keywords
      if (isstat(1) == 0) then
         call errhdl(path,modnam,'E','130','STARTING')
      end if
      if (isstat(2) == 0) then
         call errhdl(path,modnam,'E','130','LOCATION')
      end if
      if (isstat(3) == 0) then
         call errhdl(path,modnam,'E','130','SRCPARAM')
      end if
      if (isstat(24) == 0 .and. .not.psdcredit) then
         call errhdl(path,modnam,'E','130','SRCGROUP')
      end if
      if (isstat(34) == 0 .and. psdcredit) then
         call errhdl(path,modnam,'E','130','PSDGROUP')
      end if
      if (isstat(3) < isstat(2)) then
!           Must Be Missing a SRCPARAM Card for One or More Sources
         call errhdl(path,modnam,'E','130','SRCPARAM')
      end if

! ---    Check for BACKUNIT keyword without BACKGRND keyword
      if (isstat(40) == 0 .and. isstat(41) > 0) then
         call errhdl(path,modnam,'E','193','SO BACKGRND')
      end if

! ---    Check for number of source = 0
      if (numsrc == 0) then
!           WRITE Error Message:  No Sources Input
         call errhdl(path,modnam,'E','185','NUMSRC=0')

      else
! ---       Check for non-DFAULT gas deposition options
         if (dfault .and. isstat(26) > 0) then
!              Write Error Message:  Gas Deposition Option w/ DFAULT Option
            call errhdl(path,modnam,'E','196','GASDEPOS')
         else if (isstat(26) > 0) then
!              Set flag for use of non-DEFAULT option
            L_NonDFAULT = .true.
         end if
! ---       Set logical flags for deposition options
         if (luservd .or. (icstat(18)>0 .and.&
         &icstat(20)>0 .and.&
         &isstat(26)>0)) then
! ---          Gas dry deposition inputs specified
!              GASDEPVD or GDSEASON, GDLANUSE & GASDEPOS
            ldgas = .true.
         end if
         if (isstat(26) > 0) then
! ---          Gas wet deposition inputs specified
            lwgas = .true.
         end if
         if ((isstat( 9)>0 .and. isstat(10)>0 .and.&
         &isstat(11)>0) .or. isstat(27)>0) then
! ---          Particle dry and wet deposition inputs specified
!              PARTDIAM, MASSFRAX & PARTDENS or METHOD_2
            ldpart = .true.
            lwpart = .true.
         end if
         if (drydplt .or. (.not.nodrydplt .and.&
         &(ldgas .or. ldpart))) then
! ---          Set dry depletion unless overridden by user
            ddplete = .true.
         end if
         if (wetdplt .or. (.not.nowetdplt .and.&
         &(lwgas .or. lwpart))) then
! ---          Set wet depletion unless overridden by user
            wdplete = .true.
         end if

! ---       Check for incompatibilities with user-specified deposition velocity
!           This condition may not be caught by check in subroutine COCARD
         if (luservd .and. wdplete) then
!              Write Error Message: Wet deposition/depletion incompatible
!              with GASDEPVD option
            call errhdl(path,modnam,'E','243','GASDEPVD')
         end if

! ---       Set model option header for dry depletion
         if (ddplete) then
            if (ardplete) then
               modops(14) = 'AREADPLT'
            else if (romberg) then
               modops(14) = 'ROMBERG'
            else
               modops(14) = 'DRYDPLT'
            end if
         else if (nodrydplt) then
            modops(14) = 'NODRYDPLT'
         else
            modops(14) = '         '
         end if
! ---       Set model option header for wet depletion
         if (wdplete) then
            modops(15) = 'WETDPLT'
         else if (nodrydplt) then
            modops(15) = 'NOWETDPLT'
         else
            modops(15) = '         '
         end if
! ---       Check for error with inputs for dry deposition
         if ((ddplete .or. depos .or. ddep) .and.&
         &(.not.ldgas .and. .not.ldpart)) then
            call errhdl('SO',modnam,'E','244','DRYDEP')
         end if
! ---       Check for error with inputs for wet deposition
         if ((wdplete .or. depos .or. wdep) .and.&
         &(.not.lwgas .and. .not.lwpart)) then
            call errhdl('SO',modnam,'E','244','WETDEP')
         end if

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
         lnum = 0
         if (nbltotal > 0) then
            modops(25) = 'BUOYLINE'

!              BLPINPUT_Checks_Missing_D091_Wood: start
!              Due to the logic in subroutine SRCQA, AERMOD crashes if
!              there are BUOYLINE sources but no BLPINPUT record.
!              To maintain the logic in SRCQA, the logical L_BLSOURCE
!              is set to TRUE here to indicate that at least one buoyant
!              line source is present in the model run
            l_blsource = .true.

!              If no BLPGROUP record was encountered, the control file
!               may be a legacy file (from original implementation of
!               BL into AERMOD).  Assume only one group/source and put
!               all BUOYLINE sources into the single group 'ALL'

!              The condition that NUMBLAVGINP be > 0 was added so
!              NUMBLGRPS is not set to 1 when BLPINPUT record is missing

            if (numblgrps == 0 .and. numblavginp > 0) then
!              BLPINPUT_Checks_Missing_D091_Wood: end

               numblgrps = 1
!                  NBLINGRP(1) = NBLTOTAL
               do i = 1,numsrc
                  if (srctyp(i) == 'BUOYLINE') then
                     igrp_blp(i,1) = 1
                  end if
               end do
               bl_grpid(1) = 'ALL'
            end if
! Multiple_BuoyLines_D41_Wood - end

            do i = 1,numsrc
               if (srctyp(i) == 'BUOYLINE' ) then
                  lnum = lnum + 1
                  blineparms(lnum)%isrcnum = i
                  blineparms(lnum)%srcid  = srcid(i)
                  blineparms(lnum)%xbeg   = axs1(i)
                  blineparms(lnum)%ybeg   = ays1(i)
                  blineparms(lnum)%xend   = axs2(i)
                  blineparms(lnum)%yend   = ays2(i)
                  blineparms(lnum)%elev   = azs(i)
                  blineparms(lnum)%blqs   = aqs(i)
                  blineparms(lnum)%blhs   = ahs(i)

! Multiple_BuoyLines_D41_Wood - begin
! ---                The following tracks lines by BL groups
!                     NUMBLGRPS = number of BLPGROUPS groups in
!                     the input control file
                  do j = 1,numblgrps
                     if (igrp_blp(i,j) == 1) then
                        blineparms(lnum)%iblpgrpnum = j
                        blineparms(lnum)%blpgrpname = bl_grpid(j)
                        nblingrp(j) = nblingrp(j) + 1
                        exit
                     end if
                  end do
! Multiple_BuoyLines_D41_Wood - end

!     Will eventually need to address urban sources
!                          BLINEPARMS(LNUM)%IURBSRCNUM = ??
!                          BLINEPARMS(LNUM)%URBSRCNAME = ??
               endif
            end do

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
            do kk = 1,numblgrps
               call bl_rotate1 (kk)
            end do
         end if

! ---       Quality Assure Source Parameter Inputs          ---   CALL SRCQA
         call srcqa

! ---       Check for consistency of deposition logical variables
!           Check for CO GDSEASON Card if Gas Deposition is Calculated
         if (.not. luservd .and. ldgas .and. icstat(18) == 0) then
! ---          Write Error Message:  Missing Mandatory Keyword
            call errhdl('CO',modnam,'E','130','GDSEASON')
         end if
! ---       Check for CO GDLANUSE Card if Gas Deposition is Calculated
         if (.not. luservd .and. ldgas .and. icstat(20) == 0) then
!              Write Error Message:  Missing Mandatory Keyword
            call errhdl('CO',modnam,'E','130','GDLANUSE')
         end if
! ---       Check for SO GASDEPOS Card if Gas Depos is Calculated w/o LUSERVD
         if (.not. luservd .and. ldgas .and. isstat(26) == 0) then
!              Write Error Message:  Missing Mandatory Keyword
            call errhdl('SO',modnam,'E','130','GASDEPOS')
         end if

! ---       Calculate settling velocity and related time-invariant
!           deposition data                                 ---   CALL VDP1
         if (ldpart .or. ldgas) then
            call vdp1
         end if

! ---       Reassign MODOPS(1) character for use of non-DFAULT options
!           MODOPS(1) initially assigned in sub. COCARD, but source inputs
!           may have changed value of L_NonDFAULT.
         if (dfault) then
            modops(1) = 'RegDFAULT'
         else if (L_NonDFAULT) then
            modops(1) = 'NonDFAULT'
         else
            modops(1) = '         '
         end if

      end if

   else
!        Write Error Message: Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

999 return
end subroutine socard

subroutine srcqa
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
   use main1
   use buoyant_line
   use rline_data, only: nrlines

   implicit none
   character :: modnam*12

   logical :: fopen
   integer :: i, j, n, itotsrc, itotgrp, NumBack, lnum, kk
   integer :: ilsave, nerrcount, lcount
   double precision :: atot
   double precision :: suma, sumx, sumy, area
   double precision :: blxbeg, blxend

!     Variable Initializations
   modnam = 'SRCQA'
   fopen  = .false.
   nerrcount = 0

!     Begin Source LOOP
   do i = 1, numsrc

!        Check Source Array Limits for Too Few Values;
!        (Too Many Checked In DSFILL and EFFILL)
      if (iwrk2(i,1) >0 .or. iwrk2(i,2) >0 .or.&
      &iwrk2(i,11)>0 .or. iwrk2(i,12)>0 .or.&
      &iwrk2(i,13)>0) then

         if (iwrk2(i,1)<nsec) then
!              WRITE Error Message:  Not Enough BUILDHGTs
            call errhdl(path,modnam,'E','236',srcid(i))
         end if
         if (iwrk2(i,2)<nsec) then
!              WRITE Error Message:  Not Enough BUILDWIDs
            call errhdl(path,modnam,'E','237',srcid(i))
         end if

! --- PRIME -------------------------------------------------
         if (iwrk2(i,11)<nsec) then
!              WRITE Error Message:  Not Enough BUILDLENs
            call errhdl(path,modnam,'E','241',srcid(i))
         end if
         if (iwrk2(i,12)<nsec) then
!              WRITE Error Message:  Not Enough XBADJs
            call errhdl(path,modnam,'E','246',srcid(i))
         end if
         if (iwrk2(i,13)<nsec) then
!              WRITE Error Message:  Not Enough YBADJs
            call errhdl(path,modnam,'E','247',srcid(i))
         end if
! -----------------------------------------------------------
      end if

! --- PRIME-PLATFORM DOWNWASH CONFLICT ----------------------
!     CRT, 6/6/2012: D063 PRIME and PLATFORM downwash params
!     cannot be specified for the same source

      if (osplat(i) .and. iwrk2(i,1) > 0) then

!           WRITE Error Message:  Cannot be both PRIME and PLATFORM
         call errhdl(path,modnam,'E','633',srcid(i))

      end if

! -----------------------------------------------------------

      if (qflag(i) /= ' ') then
         if (qflag(i)=='SEASON' .and. iwrk2(i,4)<4) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if (qflag(i)=='MONTH' .and. iwrk2(i,4)<12) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='HROFDY' .and. iwrk2(i,4)<24) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if (qflag(i)=='WSPEED' .and. iwrk2(i,4)<6) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='SEASHR' .and. iwrk2(i,4)<96) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='HRDOW' .and. iwrk2(i,4)<72) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='HRDOW7' .and. iwrk2(i,4)<168) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='SHRDOW' .and. iwrk2(i,4)<288) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='SHRDOW7' .and. iwrk2(i,4)<672) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='MHRDOW' .and. iwrk2(i,4)<864) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i)=='MHRDOW7' .and. iwrk2(i,4)<2016) then
!              WRITE Error Message: Not Enough QFACTs
            call errhdl(path,modnam,'E','239',srcid(i))
         else if(qflag(i) == 'HOURLY') then
!              Check for use of hourly-varying sigmas and release heights
!              for VOLUME and AREA source types;
!              Call HRQREAD subroutine for each source to set L_HRLYSIG flag;
!              First save ILINE and reset to 1 to trigger L_HRLYSIG check
            ilsave = iline
            iqline = iqline + 1
            iline  = 1
            kurdat = 0
!MGS           Check for aircraft source type for reading/setting
!              aircraft plume rise parameters.
!MGS               CALL HRQREAD(I) !D151 - 6/5/2023
            if((aftsrc(i) == 'Y')) then
!*               Retrieve AIRCRAFT Source Parameters for This Hour     ---   CALL AHRQREAD
               call ahrqread(i)
            else
!*               Retrieve Source Parameters for This Hour     ---   CALL HRQREAD
               call hrqread(i)
            end if
            iline  = ilsave
         end if
      end if
!MGS           END - Check for aircraft source type

! Multiple_BuoyLines_D41_Wood
!        Removed check on emission flag for buoyant line source at this point
!         in the source QA

!        Check Settling and Removal Parameters
      if (iwrk2(i,5)/=0 .or. iwrk2(i,6)/=0 .or.&
      &iwrk2(i,7)/=0) then
!           Set Number of Particle Diameter Categories for This Source
         inpd(i) = iwrk2(i,5)
!           Check for Consistent Number of Categories for All Parameters
         if (iwrk2(i,5)/=iwrk2(i,6) .or.&
         &iwrk2(i,5)/=iwrk2(i,7)) then
!              WRITE Error Message: PartDiam Categories Don't Match
            call errhdl(path,modnam,'E','240',srcid(i))
         end if
!           Check for Mass Fraction Summing to 1.0 (+/- 2%)
         atot = 0.0d0
         n = inpd(i)
         if (n <= npdmax) then
            do j = 1, n
               atot = atot + aphi(j,i)
            end do
            if (atot < 0.98d0 .or. atot > 1.02d0) then
!                 WRITE Error Message: Mass Fractions Don't Sum to 1.0
               call errhdl(path,modnam,'W','330',srcid(i))
            end if
         else
!              WRITE Error Message:  Too Many Settling/Removal Categories
!              This shouldn't occur since limits are dynamically allocated
            write(dummy,'(''NPD= '',I7)') npdmax
            call errhdl(path,modnam,'E','290',dummy)
         end if
      end if

!        Screen for Conflicts with the Deposition Options
      if (inpd(i) == 0) then
!           Check for NPD=0 and no gas deposition with the DEPOS, DDEP, or WDEP
         if ((depos.or.ddep.or.wdep.or.ddplete.or.wdplete) .and.&
         &.not. luservd .and. sogas(i)=='N') then
!              WRITE Error Message for Lack of Particle Deposition Parameters
            call errhdl(path,modnam,'E','242',srcid(i))
         end if
      else if (inpd(i)>0 .and. sogas(i)=='Y') then
!           Check for NPD>0 and gas deposition for same source
!           WRITE Error Message for source as both particle and gas
         call errhdl(path,modnam,'E','289',srcid(i))
      end if

!        Check Vertices and Determine Centroid for AREAPOLY Sources
      if (srctyp(i) == 'AREAPOLY') then
         if (iwrk2(i,10) < nverts(i) ) then
!              WRITE Error Message:  Not Enough Vertices Input For This Source?
            call errhdl(path,modnam,'E','265',srcid(i))
         else
!              Repeat First Vertex as Last Vertex to Close Polygon
            axvert(nverts(i)+1,i) = axvert(1,i)
            ayvert(nverts(i)+1,i) = ayvert(1,i)

!              Determine coordinates for centroid of polygon source;
!              First calculate area of polygon
            suma = 0.0d0
            do j = 1, nverts(i)
               suma = suma + (axvert(j,i)  *ayvert(j+1,i) -&
               &axvert(j+1,i)*ayvert(j,i))
            end do

            area = 0.5d0 * suma

!              Assign SQRT(DABS(area)) to AXINIT and AYINIT; equivalent values
!              of AXINIT and AYINIT will be used to calculate area of polygon
            axinit(i) = dsqrt( dabs(area) )
            ayinit(i) = dsqrt( dabs(area) )

!              Now determine coordinates of centroid
            sumx = 0.0d0
            sumy = 0.0d0
            do j = 1, nverts(i)
               sumx = sumx +(axvert(j,i)+axvert(j+1,i)) *&
               &(axvert(j,i)*ayvert(j+1,i) -&
               &axvert(j+1,i)*ayvert(j,i))
               sumy = sumy +(ayvert(j,i)+ayvert(j+1,i)) *&
               &(axvert(j,i)*ayvert(j+1,i) -&
               &axvert(j+1,i)*ayvert(j,i))
            end do

            if (dabs(area) < 0.0001d0) then
!                 WRITE Error Message:  Invalid shape for AREAPOLY source
               call errhdl(path,modnam,'E','266',srcid(i))
               axcntr(i) = 0.0d0
               aycntr(i) = 0.0d0
            else
!                 Calculate coordinates of centroid
               axcntr(i) = sumx/(6.0d0*area)
               aycntr(i) = sumy/(6.0d0*area)
            end if

         end if
      end if

!        Check for urban sources
      if (urbsrc(i) == 'Y') then
         nurbsrc = nurbsrc + 1
      end if
!**  Added for Aircraft Plume Rise; UNC-IE
!        Check for Aircraft sources
      if (aftsrc(i) == 'Y') then
         naftsrc = naftsrc + 1
      end if
!**  End Aircraft Plume Rise insert; April 2023
!        Check for capped and horizontal stack sources
      if (srctyp(i) == 'POINTCAP') then
         numcap = numcap + 1
      else if (srctyp(i) == 'POINTHOR') then
         numhor = numhor + 1
      end if
! ---    Include count for all source types:
      if (srctyp(i)(1:5) == 'POINT') then
         numpnt = numpnt + 1
      else if (srctyp(i) == 'VOLUME') then
         numvol = numvol + 1
      else if (srctyp(i)(1:4) == 'AREA') then
         numarea = numarea + 1
      else if (srctyp(i) == 'LINE') then
         numline = numline + 1
      else if (srctyp(i) == 'OPENPIT') then
         numpit = numpit + 1
      else if (srctyp(i) == 'BUOYLINE') then
! Multiple_BuoyLines_D41_Wood
!           The counter for the total number of indivdual lines,
!           NBLTOTAL, as well as the number of lines in each BL
!           group, NBLINGRP array, now is determined in SOCARD

!        Added for SIDEWASH point source
      else if (srctyp(i) == 'SWPOINT') then
         numswp = numswp + 1
      end if

!        Identify the index of the level immediately below the top of the
!        stack from the array of gridded heights; we are limiting the
!        number of levels to search to 29 (= 600 m).  (Changed from 21
!        by R. Brode, PES, 2/17/95)

      call locate( gridht, 1, 29, ahs(i), ndxstk(i) )

   end do
!     End Source LOOP

!     D128: Warning message when AREAMNDR is used without an AREA Source present
   if (l_areamndr .and. numarea == 0) then
      call errhdl(path,'RLINE','W','668','AREAMNDR')
   end if

!     Check for open HOUREMIS file; if so, rewind file
   if (hourly) then
!        Check for Hourly Emissions File
      inquire (unit=ihremi,opened=fopen)
      if (fopen) then
         rewind ihremi
         iqline = 0
      end if
   end if

   if (psdcredit) then
!RWB     Assign default "source groups" for PSDCREDIT applications:
!RWB     first "source group" contains cumulative NAAQS calculation;
!RWB     second "source group" contains PSD increment consumtion with credits.
      numgrp = 2
      grpid(1) = 'NAAQS   '
      grpid(2) = 'PSDINC  '

!        Check for source not included in any PSDGROUP
      do i = 1, numsrc
         itotsrc = 0
         do j = 1, numpsd
            if (igrp_psd(i,j) == 1) then
               itotsrc = itotsrc + 1
            end if
         end do
         if (itotsrc == 0) then
!              Write Error Message:  Source not in PSDGROUP
            call errhdl(path,modnam,'E','317',srcid(i))
         end if
      end do

   else

! ---    Check for empty source groups and count how many groups
!        include BACKGROUND
      NumBack = 0
      do j = 1, numgrp
         itotsrc = 0
         do i = 1, numsrc
            if (igroup(i,j) == 1) then
               itotsrc = itotsrc + 1
            end if
         end do
         if (grp_back(j)) then
            NumBack = Numback + 1
         end if
         if (itotsrc == 0 .and. .not.grp_back(j)) then
!              Write Warning Message:  No Sources in SRCGROUP
            call errhdl(path,modnam,'W','319',grpid(j))
         end if
      end do

! ---    Issue warning if BACKGROUND is specified but not included with
!        any source groups
      if (l_backgrnd .and. NumBack == 0) then
!           Write Warning Message:  BACKGROUND not in any SRCGROUP
         call errhdl(path,modnam,'W','321',' ')
      end if

!        Check for source not included in any source group
      do i = 1, numsrc
         itotsrc = 0
         do j = 1, numgrp
            if (igroup(i,j) == 1) then
               itotsrc = itotsrc + 1
            end if
         end do
         if (itotsrc == 0) then
!              Write Warning Message:  Source not in SRCGROUP
            call errhdl(path,modnam,'W','316',srcid(i))
         end if
      end do

   end if

! Added for HBP, JAN. 2023
   if (hbplume .and. nhbp==0) then
!        Write Error Message:  No HBP sources defined with HBP
      call errhdl(path,modnam,'E','130','HBPSRCID')
   end if
! End HBP insert

   if (urban .and. nurbsrc==0) then
!        Write Error Message:  No urban sources defined with URBANOPT
      call errhdl(path,modnam,'E','130','URBANSRC')
   end if

!     Check for Urban Areas with No Sources;
!     (single urban area checked based on missing URBANSRC card)
   if (numurb > 1) then
      do j = 1, numurb
         itotsrc = 0
         do i = 1, numsrc
            if (iurbgrp(i,j) == 1) then
               itotsrc = itotsrc + 1
            end if
         end do
         if (itotsrc == 0) then
!              Write Error Message:  No Sources for Urban Area
            call errhdl(path,modnam,'E','318',urbid(j))
         end if
      end do
   end if

!**  Added for Aircraft Plume Rise; UNC-IE
   if (arcft .and. naftsrc==0) then
!        Write Error Message:  No Aircraft sources defined with ARCFTOPT
      call errhdl(path,modnam,'E','822','ARCFTSRC')
   end if
!**  End Aircraft Plume Rise insert, April 2023

!     Check for source in more than one Urban Area
   do i = 1, numsrc
      itotgrp = 0
      do j = 1, numurb
         if (iurbgrp(i,j) == 1) then
            itotgrp = itotgrp + 1
         end if
      end do
      if (itotgrp > 1) then
!           Write Error Message:  Source in more than one Urban Area
         call errhdl(path,modnam,'E','302',srcid(i))
      end if
   end do

!  Multiple_BuoyLines_D41_Wood
!     QA for buoyant line sources: code moved to this point from earlier
!      in this subroutine

   if (l_blsource) then
!        There is at least one buoyant line in this model run

!        A minimum release height of 2 meters has been established for
!         the lines of buoyant line sources
      do lnum = 1,nbltotal
         if (blineparms(lnum)%blhs < 2.0d0) then
!              Apply a minimum release ht. of 2.0 m to the buoyant line
            blineparms(lnum)%blhs = 2.0d0
            write(dummy,'(''line #'',I2)') lnum
            call errhdl(path,modnam,'W','472',dummy)
         end if
      end do


!        Be sure the x-coordinates for each buoyant line are entered
!         WEST to EAST;
!         This check is on the UNTRANSLATED, UNROTATED coordinates
!         entered on the and is not a function of BL source groups.

      do iline = 1,nbltotal

         blxbeg = blineparms(iline)%xbeg
         blxend = blineparms(iline)%xend
         if(blxbeg > blxend) then
!              Coordinates are entered in reverse order for this ILINE
            nerrcount = nerrcount + 1
         end if

      end do
!
      if (nerrcount > 0) then
         call errhdl(path,modnam,'E','388','BUOYLINE')
      end if

!        BLPINPUT_Checks_Missing_D091_Wood: begin
!        There is at least one individual buoyant line but no BLPINPUT
      if (numblavginp == 0 .and. nbltotal > 0) then
         call errhdl(path,modnam,'E','507','  ')
      endif

!        The number of BLPGROUPS allocated (via NBLGRP) does not equal
!         the number of BL groups as defined by BLPINPUT records
!         Recall that the groups are defined by the BLPINPUT record(s),
!         which must appear before the BLPGROUP record(s)
      if (numblgrps /= nblgrp) then
         call errhdl(path,modnam,'E','509','  ')
      endif
!        BLPINPUT_Checks_D091_Wood: end

!     Multiple_BuoyLines_D41_Wood: Begin

!        Check that the number of BLPGROUP IDs match the number of
!         BLPINPUT IDs; the BLPINPUT records are processed first and
!         define the group IDs so the number of BLPINPUT records
!         (NUMBLAVGINP) should equal the number in the input control file.
!         The number of BLPGROUP IDs (NUMBLGRPS) will equal the number
!         of BLPINPUT records if the IDs match; otherwise there is a
!         mismatch between the two numbers

      if (numblgrps /= numblavginp) then
         call errhdl(path,modnam,'E','503','  ')
      endif

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
      hrlyblcount(1:numblgrps) = 0

      if (l_blhourly) then
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

         do lnum = 1,nbltotal
            if (qflag(blineparms(lnum)%isrcnum)=='HOURLY') then
               do kk = 1, numblgrps
                  if (blineparms(lnum)%iblpgrpnum == kk) then
                     hrlyblcount(kk) = hrlyblcount(kk) + 1
                  end if
               end do
            end if
         end do

!        Compare the number(s) obtained above for HRLYBLCOUNT to the
!        number in each source group
         do kk = 1,numblgrps
            if (hrlyblcount(kk) > 0 .and.&
            &nblingrp(kk) > 0) then
               if (hrlyblcount(kk) /= nblingrp(kk)) then
                  call errhdl(path,modnam,'E','383',bl_grpid(kk))
               end if
            end if
         end do
      end if
!     Multiple_BuoyLines_D41_Wood: End

!        If one line of a buoyant line source group is declared urban,
!         then all lines must be declared urban.  If not, generate an
!         error message.
!         The array L_BLURBAN is initialized as FALSE in subr.SOCARD
      lcount = 0
      do kk = 1,numblgrps
         do iline = 1,nblingrp(kk)
            lcount = lcount + 1
            if (urbsrc(blineparms(lcount)%isrcnum) == "Y") then
               if (.not. l_blurban(kk)) l_blurban(kk) = .true.
               bl_numurb(kk) = bl_numurb(kk) + 1
            end if
         end do

         if ((l_blurban(kk)) .and.&
         &(bl_numurb(kk) /= nblingrp(kk))) then
!               Write Error Message:  At least one but not all BL
!                lines in the BL source group are declared as urban
            call errhdl(path,modnam,'E','393',bl_grpid(kk))
         end if
      end do

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
      if (numblgrps > 0) then
!           At least one BLPGROUP record encountered
         do i = 1, numsrc
            itotgrp = 0
            if (srctyp(i) == 'BUOYLINE') then
               do j = 1, numblgrps
                  if (igrp_blp(i,j) == 1) then
                     itotgrp = itotgrp + 1
                  end if
               end do
               if (itotgrp > 1) then
!                    Write Error Message:  BUOYLINE source in more than one BLPGROUP
                  call errhdl(path,modnam,'E','257',srcid(i))
               end if
               if (itotgrp == 0) then
!                    Write Error Message:  BUOYLINE source not in a BLPGROUP
                  call errhdl(path,modnam,'E','258',srcid(i))
               end if
            end if
         end do
      end if
   end if
!     Multiple_BuoyLines_D41_Wood: End

!     Multiple_BuoyLines_D41_Wood: Start
!     Check that the order of the buoyant lines associated with the
!       BLPINPUT records matches the order of the individual sources
!       on the SO LOCATION
!       If there is only one group, no check is needed
   if (numblgrps > 1) then
      do lnum = 2,nbltotal
         if (blineparms(lnum)%iblpgrpnum -&
         &blineparms(lnum-1)%iblpgrpnum < 0) then
            write(dummy,'(I2,'' and'',I2)')&
            &blineparms(lnum-1)%iblpgrpnum, blineparms(lnum)%iblpgrpnum
            call errhdl(path,modnam,'E','505',dummy)
         end if
      end do
   end if
!     Multiple_BuoyLines_D41_Wood: End

!     Check for source in more than one OLMGROUP
   do i = 1, numsrc
      itotgrp = 0
      do j = 1, numolm
         if (igrp_olm(i,j) == 1) then
            itotgrp = itotgrp + 1
         end if
      end do
      if (itotgrp > 1) then
!           Write Error Message:  Source in more than one OLMGROUP
         call errhdl(path,modnam,'E','282',srcid(i))
      end if
   end do


!     Check that Deposition options are all FALSE when RLINE or BUOYLINE are present
   if(depos .or. ddep .or. wdep) then
      if (nbltotal > 0) then
!              Write Error Message:Deposition (DEPOS, DDEP, WDEP) incompatible with
         call errhdl(path,modnam,'E','251','BUOYLINE')
      else if (nrlines > 0) then
!              Write Error Message:Deposition (DEPOS, DDEP, WDEP) incompatible with
         call errhdl(path,modnam,'E','251','RLINE/RLINEXT')
      end if
   end if

!     Chack that SCREEN option is FALSE when RLINE, BUOYLINE, SWPOINT, AREA, and LINE sources are present
!     D164 2/21/23 WSP
!     D164 6/7/2023 CRT - MDT decided this should be a warning rather than an error. Updated 'E' to 'W'.
!                         Update to separate if statments so a warning will be issued for each source type.
   if(screen) then
      if (nbltotal > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','BUOYLINE')
         call errhdl(path,modnam,'W','731','BUOYLINE')
      end if
      if (nrlines > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','RLINE/RLINEXT')
         call errhdl(path,modnam,'W','731','RLINE/RLINEXT')
      end if
      if (nswp > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','SWPOINT')
         call errhdl(path,modnam,'W','731','SWPOINT')
      end if
      if (narea > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','AREA')
         call errhdl(path,modnam,'W','731','AREA')
      end if
      if (npoly > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','AREAPOLY')
         call errhdl(path,modnam,'W','731','AREAPOLY')
      end if
      if (ncirc > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','AREACIRC')
         call errhdl(path,modnam,'W','731','AREACIRC')
      end if
      if (npit > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','OPENPIT')
         call errhdl(path,modnam,'W','731','OPENPIT')
      end if
      if (nline > 0) then
!              Write Error Message:SCREEN incompatible with
!              Write Warning Message:SCREEN incompatible with
!            CALL ERRHDL(PATH,MODNAM,'E','731','LINE')
         call errhdl(path,modnam,'W','731','LINE')
      end if
   end if


!     Check for source in more than one PSDGROUP                        ! jop 9/30/06
   if (psdcredit) then
      do i = 1, numsrc
         itotgrp = 0
         do j = 1, numpsd
            if (igrp_psd(i,j) == 1) then
               itotgrp = itotgrp + 1
            end if
         end do
         if (itotgrp > 1) then
!              Write Error Message:  Source in more than one PSDGROUP
            call errhdl(path,modnam,'E','286',srcid(i))
         end if
      end do
   end if

! --- Check for potential problems with inputs for NO2 options; first for OLM/PVMRM/GRSM/TTRM
   if (olm .or. pvmrm .or. runttrm .or. grsm) then
!        Check for negative emission rate with OLM, PVMRM or GRSM option.
!        Negative emission for credit sources cannot be used for OLM, PVMRM and
!        GRSM due to non-linear dependence of concentrations on emissions.
!RWB     Draft BETA-test option for PSDCREDIT accounts for non-linear effects
!RWB     on PSD increment calculations for PVMRM, but does not use negative
!RWB     emission rates to identify PSD credit sources.
      do i = 1, numsrc
         if (aqs(i) < 0.0d0) then
!              Write Error Message:  Negative emission rate for OLM/PVMRM/GRSM
            call errhdl(path,modnam,'E','338',srcid(i))
         end if
      end do

!        Check for negative in-stack NO2/NOx ratio with OLM, PVMRM or GRSM option.
!        This indicates that user has not specified an in-stack ratio for
!        a particular source.  This is an error since no default ratio has
!        been determined for use with OLM, PVMRM or GRSM with the 1-hour NO2 NAAQS.
      do i = 1, numsrc
         if (ano2_ratio(i) < 0.0d0) then
!              Write Error Message:  No in-stack ratio specified for PVMRM/OLM/GRSM
            call errhdl(path,modnam,'E','336',srcid(i))
         end if
      end do

! --- Check for potential problems with inputs for NO2 options; ARM2
   else if (arm2) then
!        Check for SRCGROUP ALL, which is required for the ARM2 option
      indx_grpall = 0
      do i = 1, numgrp
         if (grpid(i) == 'ALL') then
            indx_grpall = i
            exit
         end if
      end do
      if (indx_grpall == 0) then
!           Issue fatal error message; Group ALL not found
         if (arm2) then
            call errhdl(path,modnam,'W','299','ARM2 Option')
         end if
      end if
   end if

! --- Check for completeness of BACKGRND inputs
   if (l_backgrnd) then
! ---    Check for correct number of temporally varying background concentrations
!        for the BACKGRND keyword by sector.
      if (L_BGSector) then
         do i = 1, NUMBGSects
            if (.not. L_BGFile(i)) then
!                 Check for no BACKGRND values for this sector w/o HOURLY BG
               if (.not. L_BGValues(i)) then
!                    WRITE Error Message: No BACKGRND values - Missing Sector
                  write(dummy,'(''No BG SECT'',I1,''!'')') i
                  call errhdl(path,modnam,'E','238',dummy)
               else if (ibkgrd(i) < ibgmax(i)) then
!                    WRITE Error Message: Not Enough BACKGRND values
                  if (ibkgrd(i) < 100) then
                     write(dummy,'(''BGSECT'',I1,'' N='',I2)') i,&
                     &ibkgrd(i)
                  else
                     write(dummy,'(''SECT'',I1,'' N='',I4)') i,&
                     &ibkgrd(i)
                  end if
                  call errhdl(path,modnam,'E','238',dummy)
               end if
            else if (L_BGFile(i)) then
!                 First check for HOURLY BG for this sector
               if (len_trim(BKGRND_File(i)) == 0) then
!                    No HOURLY BG file available for this sector; check for complete BACKGRND values
                  if (.not. L_BGValues(i)) then
!                       WRITE Error Message: No BACKGRND values for this sector
                     write(dummy,'(''No BG SECT'',I1,''!'')') i
                     call errhdl(path,modnam,'E','238',dummy)
                  else if (ibkgrd(i) < ibgmax(i)) then
!                       WRITE Error Message: Incomplete BACKGRND values for this sector
                     if (ibkgrd(i) < 100) then
                        write(dummy,'(''BGSECT'',I1,'' N='',I2)') i,&
                        &ibkgrd(i)
                     else
                        write(dummy,'(''SECT'',I1,'' N='',I4)') i,&
                        &ibkgrd(i)
                     end if
                     call errhdl(path,modnam,'E','238',dummy)
                  else
!                       WRITE Warning Message: BGVALs but No HOURLY BG file available for this sector
                     write(dummy,'(''BGFILE SECT'',I1)') i
                     call errhdl(path,modnam,'W','248',dummy)
                  end if
               else
!                    HOURLY BG file available for this sector; check for complete BACKGRND values
                  if (.not. L_BGValues(i)) then
!                       WRITE Warning Message: No Varying BACKGRND but HOURLY avail for this Sector
                     write(dummy,'(''BGVALs SECT'',I1)') i
                     call errhdl(path,modnam,'W','248',dummy)
                  else if (ibkgrd(i) < ibgmax(i)) then
!                       WRITE Error Message: Not Enough BACKGRND values
                     if (ibkgrd(i) < 100) then
                        write(dummy,'(''BGSECT'',I1,'' N='',I2)') i,&
                        &ibkgrd(i)
                     else
                        write(dummy,'(''SECT'',I1,'' N='',I4)') i,&
                        &ibkgrd(i)
                     end if
                     call errhdl(path,modnam,'E','238',dummy)
                  end if
               end if
            end if
         end do
      else  ! .NOT. L_BGSector
! ---       No BGSECTORs; First check for No HOURLY BACKGRND values
!           Set sector array index, I = 1
         i = 1
         if (.not. L_BGFile(i)) then
!              No HOURLY BGFILE, check for non-HOURLY BACKGRND
            if (.not. L_BGValues(i)) then
!                 WRITE Error Message: No BACKGRND values
               write(dummy,'(''No BGVALUES!'')')
               call errhdl(path,modnam,'E','238',dummy)
            else if (ibkgrd(i) < ibgmax(i)) then
!                 WRITE Error Message: Not Enough BACKGRND values
               write(dummy,'(''NumVals='',I4)') ibkgrd(i)
               call errhdl(path,modnam,'E','238',dummy)
            end if
         else if (L_BGFile(i)) then
!              Check for HOURLY BGFILE for this sector
            if (len_trim(BKGRND_File(i)) == 0) then
!                 No HOURLY BG file available; check for complete BACKGRND values
               if (.not. L_BGValues(i)) then
!                    WRITE Error Message: No BACKGRND values
                  write(dummy,'(''No BGVALUES!'')')
                  call errhdl(path,modnam,'E','238',dummy)
               else if (ibkgrd(1) < ibgmax(i)) then
!                    WRITE Error Message: Incomplete BACKGRND values for this sector
                  write(dummy,'(''NumVals='',I4)') ibkgrd(i)
                  call errhdl(path,modnam,'E','238',dummy)
               end if
            else
!                 HOURLY BG file available; check for complete BACKGRND values
               if (.not. L_BGValues(i)) then
!                    WRITE Warning Message: HOURLY BG file but No BGVALs available for this sector
                  write(dummy,'(''BGVALs Msg'')')
                  call errhdl(path,modnam,'W','248',dummy)
               else if (ibkgrd(i) < ibgmax(i)) then
!                    WRITE Error Message: Incomplete BACKGRND values for this sector
                  write(dummy,'(''NumVals='',I4)') ibkgrd(i)
                  call errhdl(path,modnam,'E','238',dummy)
               end if
            end if
         end if
      end if

! ---    Check for user-specified background units; apply default if needed
      if (isstat(41) /= 1) then
         if (pollut == 'NO2' .or. pollut == 'SO2') then
            BackUnits = 'PPB'
         else if (pollut == 'CO') then
            BackUnits = 'PPM'
         else
            BackUnits = 'UG/M3'
         end if
      end if
   end if

   return
end subroutine srcqa

subroutine soelun
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SOELUN'

   if (ifc == 3) then
      if (field(3) == 'METERS') then
         soelev = 'METERS'
      else if (field(3) == 'FEET') then
         soelev = 'FEET'
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203','SO_ELEV')
      end if
   else if (ifc > 3) then
!        WRITE Error Message     ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message     ! No Parameters
      call errhdl(path,modnam,'E','200','ElevUnit')
   end if

   return
end subroutine soelun

subroutine soloca
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
   use main1
   use rline_data, only: rlsource
   use buoyant_line

   implicit none
   character :: modnam*12

   integer :: indexs
   character (len=12) :: soid
   logical :: found

!     Variable Initializations
   found  = .false.
   modnam = 'SOLOCA'

!     Check The Number Of The Fields
!*--- Modified to handle LINE, BUOYLINE, and RLINE sources
   if (field(4)=='LINE' .or. field(4)=='BUOYLINE'&
   &.or. field(4)=='RLINE') then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 8) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 9) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
!*--- Modified to handle RLINEXT sources
   else if (field(4)=='RLINEXT') then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 10) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 11) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
   else
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 6) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 7) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
   end if
!*---

!     Read In The Data Fields and Assign to Arrays
!     Check for Previous Occurrence of This SRCID
!*    First check for length of SRCID field (<=12)
   if ((loce(3)-locb(3)) <= 11) then
!*       Retrieve Source ID Character Substring
      soid = field(3)
! ---    Check for "reserved" source ID for 'BACKGROUND'
      if (soid == 'BACKGROUND' .or. soid == 'BACKGRND') then
!*          WRITE Error Message:  Source ID Field is Too Long
         call errhdl(path,modnam,'E','285',field(3)(1:12))
         go to 999
      end if
   else
!*       WRITE Error Message:  Source ID Field is Too Long
      call errhdl(path,modnam,'E','230',field(3)(1:12))
      go to 999
   end if

   call sindex(srcid,nsrc,soid,indexs,found)

   if (.not. found) then
      isrc = isrc + 1
      if (isrc <= nsrc) then
         srcid(isrc)  = field(3)
         srctyp(isrc) = field(4)

! ---       Allow for variations in the source type string
         if (srctyp(isrc) == 'OPENPIT'  .or.&
         &srctyp(isrc) == 'OPEN_PIT' .or.&
         &srctyp(isrc) == 'OPEN-PIT') then
! ---           Assign as 'OPENPIT' for future references
            srctyp(isrc) = 'OPENPIT'
         end if

! ---       Check for acceptable source types
         if (srctyp(isrc)=='POINT' .or.&
         &srctyp(isrc)=='POINTCAP' .or.&
         &srctyp(isrc)=='POINTHOR' .or.&
         &srctyp(isrc)=='VOLUME' .or.&
         &srctyp(isrc)=='AREA' .or.&
         &srctyp(isrc)=='AREAPOLY' .or.&
         &srctyp(isrc)=='AREACIRC' .or.&
         &srctyp(isrc)=='LINE' .or.&
         &srctyp(isrc)=='RLINE' .or.&
         &srctyp(isrc)=='RLINEXT' .or.&
         &srctyp(isrc)=='BUOYLINE' .or.&
         &srctyp(isrc)=='OPENPIT' .or.&
         &srctyp(isrc)=='SWPOINT') then

!*---          Modified to add handling of LINE, BUOYLINE source types
            if (srctyp(isrc)=='LINE' .or.&
            &srctyp(isrc)=='BUOYLINE') then
               call stodbl(field(5), ilen_fld, axs1(isrc), imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
               call stodbl(field(6), ilen_fld, ays1(isrc), imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source X2
               call stodbl(field(7), ilen_fld, axs2(isrc), imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source Y2
               call stodbl(field(8), ilen_fld, ays2(isrc), imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!*---          Modified to add handling of RLINE source type
            else if (srctyp(isrc)=='RLINE') then
!                 Retrieve Source XSB
               call stodbl(field(5), ilen_fld, rlsource(isrc)%xsb,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source YSB
               call stodbl(field(6), ilen_fld, rlsource(isrc)%ysb,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source XSE
               call stodbl(field(7), ilen_fld, rlsource(isrc)%xse,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source YSE
               call stodbl(field(8), ilen_fld, rlsource(isrc)%yse,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!*---          Modified to add handling of RLINEXT source type
            else if (srctyp(isrc)=='RLINEXT') then
!                 Retrieve Source XSB
               call stodbl(field(5), ilen_fld, rlsource(isrc)%xsb,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source YSB
               call stodbl(field(6), ilen_fld, rlsource(isrc)%ysb,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source ZSB
               call stodbl(field(7), ilen_fld, rlsource(isrc)%zsb,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source XSE
               call stodbl(field(8), ilen_fld, rlsource(isrc)%xse,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source YSE
               call stodbl(field(9), ilen_fld, rlsource(isrc)%yse,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
!                 Retrieve Source ZSE
               call stodbl(field(10), ilen_fld, rlsource(isrc)%zse,&
               &imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
            else
               call stodbl(field(5), ilen_fld, axs(isrc), imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
               call stodbl(field(6), ilen_fld, ays(isrc), imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               end if
            end if

! ---          Read elevation field for each source type
! ---
!           RLINE source now has terrain, treat like other sources - Wood 7/6/2022
! ---          Read elevation for LINE or BUOYLINE source
!               IF (SRCTYP(ISRC).EQ.'LINE' .or.
!     &             SRCTYP(ISRC).EQ.'BUOYLINE') THEN
            if (srctyp(isrc)=='LINE' .or.&
            &srctyp(isrc)=='BUOYLINE' .or.&
            &srctyp(isrc)=='RLINE') then
               if (ifc == 8) then
!                    No Source Elevation Field - Default to 0.0
                  azs(isrc) = 0.0d0
                  if (elev) then
!                       Write Warning Message for No Source Elevation with ELEV
                     call errhdl(path,modnam,'W','205','ZS = 0.0')
                  end if
               else if (ifc == 9 .and. flatsrcs .and.&
               &field(9)=='FLAT') then
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
                  l_flatsrc(isrc) = .true.
                  numflat = numflat + 1
! ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
                  call errhdl(path,modnam,'W','752',soid)
               else if (ifc == 9) then
!                    Retrieve Source lower elevation
                  call stodbl(field(9), ilen_fld, azs(isrc), imit)
!                    Check The Numerical Field
                  if (imit /= 1) then
                     call errhdl(path,modnam,'E','208',keywrd)
                  end if
! ---                Check for missing source elevations, coded as -9999.0,
!                    and convert from FEET to METERS if needed
                  if (azs(isrc) < -9998.99d0) then
!                       WRITE Error Message:  Source elevation is missing
                     call errhdl(path,modnam,'E','249',soid)
                  else if (soelev == 'FEET') then
                     azs(isrc) = azs(isrc) * 0.3048d0
                  end if
               end if
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
            else if (srctyp(isrc)=='RLINEXT') then

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
               if (ifc == 10) then
!                    No Source Elevation Field - Default to 0.0
                  azs(isrc) = 0.0d0
                  if (elev) then
!                       Write Warning Message for No Source Elevation with ELEV
                     call errhdl(path,modnam,'W','205','ZS = 0.0')
                  end if
               else if (ifc == 11 .and. flatsrcs .and.&
               &field(11)=='FLAT') then
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
                  l_flatsrc(isrc) = .true.
                  numflat = numflat + 1
                  azs(isrc) = 0.0d0
! ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
                  call errhdl(path,modnam,'W','752',soid)

               else if (ifc == 11) then
!                    Retrieve Source lower elevation
                  call stodbl(field(11), ilen_fld, azs(isrc), imit)
!                    Check The Numerical Field
                  if (imit /= 1) then
                     call errhdl(path,modnam,'E','208',keywrd)
                  end if
! ---                Check for missing source elevations, coded as -9999.0,
!                    and convert from FEET to METERS if needed
                  if (azs(isrc) < -9998.99d0) then
!                       WRITE Error Message:  Source elevation is missing
                     call errhdl(path,modnam,'E','249',soid)
                  else if (soelev == 'FEET') then
                     azs(isrc) = azs(isrc) * 0.3048d0
                  end if
               end if
! end           RLINEXT source now has terrain, treat like other source - Wood 7/5/2022
!                 END IF !check field of RLINEXT source

! ---          Read elevation for source types other than LINE, BUOYLINE, RLINE, or RLINEXT
            else
               if (ifc == 7 .and. flatsrcs .and.&
               &field(7) == 'FLAT') then
! ---                Source has been identified as a terrain-following
!                    source, to be simulated with 'FLAT' terrain option
                  l_flatsrc(isrc) = .true.
                  numflat = numflat + 1
! ---                D167 Warning message about ZELEV and ZHILL inputs being ignored WSP 3/26/23
                  call errhdl(path,modnam,'W','752',soid)
               else if (ifc == 7) then
!                    Retrieve Source Elevation From Inputs
                  call stodbl(field(7), ilen_fld, azs(isrc), imit)
!                    Check The Numerical Field
                  if (imit /= 1) then
                     call errhdl(path,modnam,'E','208',keywrd)
                  end if
! ---                Check for missing source elevations, coded as -9999.0,
!                    and convert from FEET to METERS if needed
                  if (azs(isrc) < -9998.99d0) then
!                       WRITE Error Message:  Source elevation is missing
                     call errhdl(path,modnam,'E','249',soid)
                  else if (soelev == 'FEET') then
                     azs(isrc) = azs(isrc) * 0.3048d0
                  end if
               else
!                    No Source Elevation Field - Default to 0.0
                  azs(isrc) = 0.0d0
                  if (elev) then
!                       Write Warning Message for No Source Elevation with ELEV
                     call errhdl(path,modnam,'W','205','ZS = 0.0')
                  end if
               end if
            end if  ! End Read elevation field for each source type

         else
!              Error Message: Invalid Source Type
            call errhdl(path,modnam,'E','203','SRCTYP')
            go to 999
         end if ! End Check for acceptable source types

         iset = isrc
         numsrc = numsrc + 1

! Multiple_BuoyLines_D41_Wood
! ---       Determine the total number of BUOYLINE sources
!           Variable name changed from NBLINES to NBLTOTAL
         if( srctyp(isrc) == 'BUOYLINE' )then
            nbltotal = nbltotal + 1
         endif

      else
!           This shouldn't occur since limits are dynamically allocated
!           WRITE Error Message    ! Number of Sources Exceeds NSRC Parameter
         write(dummy,'(''NSRC='',I7)') nsrc
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      end if
   else
!        WRITE Error Message    ! Source Location Has Already Been Identified
      call errhdl(path,modnam,'E','310',soid)
   end if

999 return
end subroutine soloca

subroutine soparm
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, isdx
   logical :: found
   double precision :: temp(ifmax)

!     Variable Initializations
   found  = .false.
   modnam = 'SOPARM'
   temp   = 0.0d0

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     Search For The Source ID Index
   call sindex(srcid,nsrc,field(3),isdx,found)

   if (found) then
!        Check for Previous SRCPARAM Card for This Source
      if (sopcrd(isdx) == 'Y') then
!           WRITE Error Message: Duplicate SRCPARAM Card
         call errhdl(path,modnam,'E','315',srcid(isdx))
         go to 999
      else
         sopcrd(isdx) = 'Y'
      end if
!        Assign The Parameter Arrays
      do i = 4, ifc
         call stodbl(field(i),ilen_fld,temp(i-3),imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
      end do
      if (srctyp(isdx) == 'POINT' .or.&
      &srctyp(isdx) == 'POINTCAP' .or.&
      &srctyp(isdx) == 'POINTHOR') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 8) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 8) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call pparm(isdx,temp)
      else if (srctyp(isdx) == 'VOLUME') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 7) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 7) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call vparm(isdx,temp)
      else if (srctyp(isdx) == 'AREA') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 6) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 9) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call aparm(isdx,temp)
      else if (srctyp(isdx) == 'AREAPOLY') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 6) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 7) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call apparm(isdx,temp)
      else if (srctyp(isdx) == 'AREACIRC') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 6) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 8) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call acparm(isdx,temp)
!*       Get Source Parameters for the OPENPIT source
      else if (srctyp(isdx) == 'OPENPIT') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 8) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 9) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call oparm(isdx,temp)
!---     Added to handle LINE source types
      else if (srctyp(isdx) == 'LINE') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 6) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 7) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call lparm(isdx,temp)
!---     Added to handle RLINE source types
      else if (srctyp(isdx) == 'RLINE') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 6) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 7) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call rlparm(isdx,temp)
!---     Get source parameters for the RLINEXT source type
      else if (srctyp(isdx) == 'RLINEXT') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 7) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 7) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
!           Process RLINE parameters                        ---   CALL RLPARM
         call rlparm(isdx,temp)
!---     Get source parameters for BUOYANT LINE source type
      else if (srctyp(isdx) == 'BUOYLINE') then
         if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 5) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 5) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call blparm(isdx,temp)
!        CRT 3/25/2022 D113 Updated for sidewash
      else if (srctyp(isdx) == 'SWPOINT') then
!           Check for ALPHA option - required
         if (.not. l_alpha) then
            call errhdl(path,modnam,'E','198','SWPOINT')
            go to 999
         else if (ifc == 3) then
!              Error Message: No Parameters
            call errhdl(path,modnam,'E','200',keywrd)
            go to 999
         else if (ifc < 9) then
!              Error Message: Not Enough Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         else if (ifc > 9) then
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
         call swparm(isdx,temp)
      end if
   else
!        WRITE Error Message: Source Location Has Not Been Identified Yet
      call errhdl(path,modnam,'E','300',keywrd)
   end if

999 return
end subroutine soparm

subroutine pparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'PPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   ats(isdx) = temp(3)
   avs(isdx) = temp(4)
   ads(isdx) = temp(5)

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 600.0d0) then
!        WRITE Warning Message:  Large Release Height (> 600M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (ats(isdx) == 0.0d0) then
!        Set Temperature to Small Negative Value for Ambient Releases
      ats(isdx) = -1.0d-5
   else if (ats(isdx) > 2000.0d0) then
!        WRITE Warning Message:  Exit Temp. > 2000K
      call errhdl(path,modnam,'W','320',' TS ')
   else if ((ats(isdx)>0.0d0) .and. (ats(isdx)<200.0d0)) then
!*       Exit temp<200K (about -100F); Incorrect units may have been
!*       used or ATS and AVS may have been switched;
!*       WRITE Fatal Error Message
      call errhdl(path,modnam,'E','320',' TS ')
      runerr = .true.
   end if

   if (avs(isdx) < 0.0d0) then
!        WRITE Warning Message:  Negative or Zero Exit Velocity
      call errhdl(path,modnam,'W','325',srcid(isdx))
!        Set to Small Value to Avoid Zero-divide and Underflow
      avs(isdx) = 1.0d-5
   else if (avs(isdx) < 1.0d-5) then
!        Set to Small Value to Avoid Zero-divide and Underflow
      avs(isdx) = 1.0d-5
   else if (avs(isdx) > 50.0d0) then
!        WRITE Warning Message:  Exit Velocity > 50.0 m/s
      call errhdl(path,modnam,'W','320',' VS ')
   end if

   if (ads(isdx) < 0.0d0) then
!        WRITE Warning Message:  Negative Stack Diameter
      call errhdl(path,modnam,'E','209',' DS ')
   else if (ads(isdx) < 1.0d-5) then
!        Set to Small Value to Avoid Zero-divide and Underflow
      ads(isdx) = 1.0d-5
   else if (ads(isdx) > 20.0d0) then
!        WRITE Warning Message:  Large Stack Diameter (> 20m)
      call errhdl(path,modnam,'W','320',' DS ')
   end if

!     Assign default factor to adjust stack diameter for capped stack
!     releases at 2.0 (i.e. cap doubles initial diameter of plume) for
!     use in the PRIME algorithm for BETA-test draft option.
   if (srctyp(isdx) == 'POINTCAP') then
      adsfact(isdx) = 2.0d0
   else
      adsfact(isdx) = 1.0d0
   end if

   return
end subroutine pparm

subroutine vparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'VPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   asyini(isdx) = temp(3)
   aszini(isdx) = temp(4)

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

!MGS Removed warning when HS >100m for aircraft souces (D151 - WSP 5/25/2023)
   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
!MGS      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
   else if((ahs(isdx) > 100.0d0) .and.&
   &(.not. arcft)) then
!        WRITE Warning Message:  Large Release Height (> 100M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (asyini(isdx) < 0.0d0) then
!        WRITE Warning Message:  Negative Initial Lateral Parameter
      call errhdl(path,modnam,'E','209',' SYINIT ')
   else if (asyini(isdx) < 1.0d-5) then
!        WRITE Warning Message:  Small Initial Lateral Parameter
      call errhdl(path,modnam,'W','320',' SYINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      asyini(isdx) = 1.0d-5
   else if (asyini(isdx) > 200.0d0) then
!        WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
      call errhdl(path,modnam,'W','320',' SYINIT ')
   end if

   if (aszini(isdx) < 0.0d0) then
!        WRITE Warning Message:  Negative Initial Vertical Parameter
      call errhdl(path,modnam,'E','209',' SZINIT ')
   else if (aszini(isdx) < 1.0d-5) then
!        WRITE Warning Message:  Small Initial Lateral Parameter
      call errhdl(path,modnam,'W','320',' SZINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      aszini(isdx) = 1.0d-5
   else if (aszini(isdx) > 200.0d0) then
!        WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      call errhdl(path,modnam,'W','320',' SZINIT ')
   end if

   return
end subroutine vparm

subroutine aparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'APARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   if (ifc == 6) then
      axinit(isdx) = temp(3)
      ayinit(isdx) = axinit(isdx)
      aangle(isdx) = 0.0d0
      aszini(isdx) = 0.0d0
   else if (ifc == 7) then
      axinit(isdx) = temp(3)
      ayinit(isdx) = temp(4)
      aangle(isdx) = 0.0d0
      aszini(isdx) = 0.0d0
   else if (ifc == 8) then
      axinit(isdx) = temp(3)
      ayinit(isdx) = temp(4)
      aangle(isdx) = temp(5)
      aszini(isdx) = 0.0d0
   else if (ifc == 9) then
      axinit(isdx) = temp(3)
      ayinit(isdx) = temp(4)
      aangle(isdx) = temp(5)
      aszini(isdx) = temp(6)
   end if

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

!MGS Removed warning when HS >100m for aircraft souces (D151 - WSP 5/25/2023)
   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
!MGS      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
   else if ((ahs(isdx) > 100.0d0) .and.&
   &(.not. arcft)) then
!        WRITE Warning Message:  Large Release Height (> 100M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (axinit(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Area Size
      call errhdl(path,modnam,'E','209',' XINIT ')
   else if (axinit(isdx) < 1.0d-5) then
!        WRITE Warning Message:  Small Source Area
      call errhdl(path,modnam,'W','320',' XINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      axinit(isdx) = 1.0d-5
   else if (axinit(isdx) > 2000.0d0) then
!        WRITE Warning Message:  Large Source Area (> 2000m)
      call errhdl(path,modnam,'W','320',' XINIT ')
   end if

   if (ayinit(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Area Size
      call errhdl(path,modnam,'E','209',' YINIT ')
   else if (ayinit(isdx) < 1.0d-5) then
!        WRITE Warning Message:  Small Source Area
      call errhdl(path,modnam,'W','320',' YINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      ayinit(isdx) = 1.0d-5
   else if (ayinit(isdx) > 2000.0d0) then
!        WRITE Warning Message:  Large Source Area (> 2000m)
      call errhdl(path,modnam,'W','320',' YINIT ')
   end if

   if (dabs(aangle(isdx)) > 180.0d0 ) then
!        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
      call errhdl(path,modnam,'W','320',' ANGLE ')
   end if

   if (aszini(isdx) < 0.0d0) then
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      call errhdl(path,modnam,'E','209',' SZINIT ')
   else if (aszini(isdx) < 1.0d-5) then
!*       Set to Small Value to Avoid Zero-divide and Underflow
      aszini(isdx) = 1.0d-5
   else if (aszini(isdx) > 200.0d0) then
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      call errhdl(path,modnam,'W','320',' SZINIT ')
   end if

!     Check for aspect ratio (length/width) > 100
   if (ayinit(isdx)/axinit(isdx) > 100.00001d0 .or.&
   &axinit(isdx)/ayinit(isdx) > 100.00001d0) then
!        WRITE Warning Message: Aspect ratio > 100 for area source
      call errhdl(path,modnam,'W','391',srcid(isdx))
   end if

!     Set Number of Vertices (4 for Rectangular Source)
   nverts(isdx) = 4

!     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
!     Vertices Start with the "Southwest" Corner and Are Defined
!     Clockwise.  The First Vertex is Repeated as the Last Vertex.

   axvert(1,isdx) = axs(isdx)
   ayvert(1,isdx) = ays(isdx)

   axvert(2,isdx) = axvert(1,isdx) +&
   &(ayinit(isdx)*dsin(aangle(isdx)*dtorad))
   ayvert(2,isdx) = ayvert(1,isdx) +&
   &(ayinit(isdx)*dcos(aangle(isdx)*dtorad))

   axvert(3,isdx) = axvert(2,isdx) +&
   &(axinit(isdx)*dcos(aangle(isdx)*dtorad))
   ayvert(3,isdx) = ayvert(2,isdx) -&
   &(axinit(isdx)*dsin(aangle(isdx)*dtorad))

   axvert(4,isdx) = axvert(3,isdx) -&
   &(ayinit(isdx)*dsin(aangle(isdx)*dtorad))
   ayvert(4,isdx) = ayvert(3,isdx) -&
   &(ayinit(isdx)*dcos(aangle(isdx)*dtorad))

   axvert(5,isdx) = axs(isdx)
   ayvert(5,isdx) = ays(isdx)

!     Determine coordinates for center of rectangular source
   axcntr(isdx) = 0.0d0
   aycntr(isdx) = 0.0d0
   do i = 1, nverts(isdx)
      axcntr(isdx) = axcntr(isdx) + axvert(i,isdx)
      aycntr(isdx) = aycntr(isdx) + ayvert(i,isdx)
   end do
   axcntr(isdx) = axcntr(isdx)/dble(nverts(isdx))
   aycntr(isdx) = aycntr(isdx)/dble(nverts(isdx))

   return
end subroutine aparm

subroutine apparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'APPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   nverts(isdx) = idnint(temp(3))
   if (ifc == 7) then
      aszini(isdx) = temp(4)
   else
      aszini(isdx) = 0.0d0
   end if

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 100.0d0) then
!        WRITE Warning Message:  Large Release Height (> 100M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (aszini(isdx) < 0.0d0) then
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      call errhdl(path,modnam,'E','209',' SZINIT ')
   else if (aszini(isdx) < 1.0d-5) then
!*       Set to Small Value to Avoid Zero-divide and Underflow
      aszini(isdx) = 1.0d-5
   else if (aszini(isdx) > 200.0d0) then
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      call errhdl(path,modnam,'W','320',' SZINIT ')
   end if

   if (nverts(isdx) < 3) then
!        WRITE Error Message:  Not Enough Vertices
      call errhdl(path,modnam,'E','380',' NVERT ')
   else if (nverts(isdx) > nvmax-8) then
!        WRITE Error Message:  Too Many Vertices
      call errhdl(path,modnam,'W','406',srcid(isdx))
   end if

   return
end subroutine apparm

subroutine arvert
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
   use main1
   implicit none
   character :: modnam*12

   integer :: k, isdx
   double precision :: dnumx, dnumy
   character (len=12) :: soid
   logical :: found

!     Variable Initializations
   found  = .false.
   modnam = 'ARVERT'

!     Get The Source ID, and Check for Previous Occurrence
!     of This SRCID;
!*    First check for length of SRCID field (<=12)
   if ((loce(3)-locb(3)) <= 11) then
!*       Retrieve Source ID Character Substring
      soid = field(3)
   else
!*       WRITE Error Message:  Source ID Field is Too Long
      call errhdl(path,modnam,'E','230',field(3)(1:12))
      go to 999
   end if

!     Search For The Index
   call sindex(srcid,nsrc,soid,isdx,found)
   if (found) then
      iset = iwrk2(isdx,10)
      do k = 4, ifc-1, 2
!           Change Fields To Numbers
         call stodbl(field(k),ilen_fld,dnumx,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
         call stodbl(field(k+1),ilen_fld,dnumy,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if

         iset = iset + 1
         if (iset == 1) then
!              Compare First Vertex to Source Location
            if (dnumx /= axs(isdx)) then
               call errhdl(path,modnam,'E','262',srcid(isdx))
            end if
            if (dnumy /= ays(isdx)) then
               call errhdl(path,modnam,'E','262',srcid(isdx))
            end if
         end if

         if (iset <= nverts(isdx)+1) then
!              Assign The Field
            axvert(iset,isdx) = dnumx
            ayvert(iset,isdx) = dnumy
         else
!              WRITE Error Message: Too Many Vertices For This Source
!              (this should not happen since arrays are allocated at runtime0
            call errhdl(path,modnam,'E','264',srcid(isdx))
         end if
      end do
      iwrk2(isdx,10) = iset
   else
!        WRITE Error Message     ! Source Location Has Not Been Identified
      call errhdl(path,modnam,'E','300',keywrd)
   end if

999 return
end subroutine arvert

subroutine acparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'ACPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   radius(isdx) = temp(3)
   if (ifc >= 7) then
! ---    Assign NVERTS for this source
      nverts(isdx) = idnint(temp(4))
   else
      nverts(isdx) = 20
   end if
   if (ifc == 8) then
      aszini(isdx) = temp(5)
   else
      aszini(isdx) = 0.0d0
   end if

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 100.0d0) then
!        WRITE Warning Message:  Large Release Height (> 100M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (radius(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Radius
      call errhdl(path,modnam,'E','209',' RADIUS ')
   else if (radius(isdx) <= 0.5d0) then
!        WRITE Error Message:  Invalid Value for Radius
      call errhdl(path,modnam,'E','203',' RADIUS ')
   else if (radius(isdx) > 10000.0d0) then
!        WRITE Warning Message:  Large Radius (> 10000M)
      call errhdl(path,modnam,'W','320',' RADIUS ')
   end if

   if (aszini(isdx) < 0.0d0) then
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      call errhdl(path,modnam,'E','209',' SZINIT ')
   else if (aszini(isdx) < 1.0d-5) then
!*       Set to Small Value to Avoid Zero-divide and Underflow
      aszini(isdx) = 1.0d-5
   else if (aszini(isdx) > 200.0d0) then
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      call errhdl(path,modnam,'W','320',' SZINIT ')
   end if

   if (nverts(isdx) < 3) then
!        WRITE Error Message:  Not Enough Vertices
      call errhdl(path,modnam,'E','380',' NVERT ')
      go to 999
   else if (nverts(isdx) > nvmax+1) then
! ---    Adjust NVMAX based on user-specified NVERTS
      nvmax = nverts(isdx) + 1
   end if

!     Setup Vertices for Circular Area
   call gencir(isdx)

!     Set coordinates for center of circular source
   axcntr(isdx) = axs(isdx)
   aycntr(isdx) = ays(isdx)

999 return
end subroutine acparm

subroutine gencir(isdx)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, isdx, nsides
   double precision :: ang, anginc, area, triarea, opp, newrad

!     Variable Initializations
   modnam = 'GENCIR'

   nsides = nverts(isdx)
   anginc = 360.0d0/dble(nsides)
   ang = 0.0d0

!     Calculate New Radius That Will Provide An Equal-Area Polygon
   area = pi * radius(isdx) * radius(isdx)
   triarea = area/dble(nsides)
   opp = dsqrt(triarea * dtan(anginc/(2.0d0*rtodeg)) )
   newrad = opp / (dsin(anginc/(2.0d0*rtodeg)) )

!     Generate Vertices for Circular Area of NSIDES
   do i = 1, nsides
      if (i /= 1) ang = ang+anginc
      axvert(i,isdx) = (newrad * dsin(ang/rtodeg)) + axs(isdx)
      ayvert(i,isdx) = (newrad * dcos(ang/rtodeg)) + ays(isdx)
   end do

!     Repeat First Vertex as Last Vertex to Close the AREACIRC source
   axvert(nsides+1,isdx) = axvert(1,isdx)
   ayvert(nsides+1,isdx) = ayvert(1,isdx)

!     Assign SQRT(area) to AXINIT and AYINIT; equivalent values
!     of AXINIT and AYINIT will be used to calculate area of source
   axinit(isdx) = dsqrt( area )
   ayinit(isdx) = dsqrt( area )

   return
end subroutine gencir

subroutine oparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, isdx
   double precision :: temp(ifmax), effdep

!     Variable Initializations
   modnam = 'OPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   axinit(isdx) = temp(3)
   ayinit(isdx) = temp(4)
   avolum(isdx) = temp(5)
   aangle(isdx) = 0.0d0
   if (ifc == 9) then
      aangle(isdx) = temp(6)
   end if

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 200.0d0) then
!        WRITE Warning Message:  Large Release Height (> 200M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (axinit(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Area Size
      call errhdl(path,modnam,'E','209',' XINIT ')
   else if (axinit(isdx) < 1.0d-5) then
!        WRITE Warning Message:  Small Source Area
      call errhdl(path,modnam,'W','320',' XINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      axinit(isdx) = 1.0d-5
   else if (axinit(isdx) > 2000.0d0) then
!        WRITE Warning Message:  Large Source Area (> 2000m)
      call errhdl(path,modnam,'W','320',' XINIT ')
   end if

   if (ayinit(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Area Size
      call errhdl(path,modnam,'E','209',' YINIT ')
   else if (ayinit(isdx) < 1.0d-5) then
!        WRITE Warning Message:  Small Source Area
      call errhdl(path,modnam,'W','320',' YINIT ')
!        Set to Small Value to Avoid Zero-divide and Underflow
      ayinit(isdx) = 1.0d-5
   else if (ayinit(isdx) > 2000.0d0) then
!        WRITE Warning Message:  Large Source Area (> 2000m)
      call errhdl(path,modnam,'W','320',' YINIT ')
   end if

   if (dabs(aangle(isdx)) > 180.0d0 ) then
!        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
      call errhdl(path,modnam,'W','320',' ANGLE ')
   end if

   if (avolum(isdx) <= 0.0d0) then
!        WRITE Error Message: Open-Pit Volume is less than
!        or equal to zero
      call errhdl(path,modnam,'E','209',' AVOLUM ')
   end if

!     Check for aspect ratio (length/width) > 10
   if (ayinit(isdx)/axinit(isdx) > 10.0d0 .or.&
   &axinit(isdx)/ayinit(isdx) > 10.0d0) then
!        WRITE Warning Message: Aspect ratio > 10 for pit source
      call errhdl(path,modnam,'W','392',srcid(isdx))
   end if

!     Check for Release Height > Effective Depth
   effdep = avolum(isdx)/(axinit(isdx)*ayinit(isdx))
   if (ahs(isdx) > effdep) then
!        WRITE Error Message: Release Height is greater than Effective Depth
      call errhdl(path,modnam,'E','322',srcid(isdx))
   end if

!     Set Number of Vertices (4 for Rectangular Source)
   nvert = 4

!     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
!     Vertices Start with the "Southwest" Corner and Are Defined
!     Clockwise.  The First Vertex is Repeated as the Last Vertex.

   axvert(1,isdx) = axs(isdx)
   ayvert(1,isdx) = ays(isdx)

   axvert(2,isdx) = axvert(1,isdx) +&
   &(ayinit(isdx)*dsin(aangle(isdx)*dtorad))
   ayvert(2,isdx) = ayvert(1,isdx) +&
   &(ayinit(isdx)*dcos(aangle(isdx)*dtorad))

   axvert(3,isdx) = axvert(2,isdx) +&
   &(axinit(isdx)*dcos(aangle(isdx)*dtorad))
   ayvert(3,isdx) = ayvert(2,isdx) -&
   &(axinit(isdx)*dsin(aangle(isdx)*dtorad))

   axvert(4,isdx) = axvert(3,isdx) -&
   &(ayinit(isdx)*dsin(aangle(isdx)*dtorad))
   ayvert(4,isdx) = ayvert(3,isdx) -&
   &(ayinit(isdx)*dcos(aangle(isdx)*dtorad))

   axvert(5,isdx) = axs(isdx)
   ayvert(5,isdx) = ays(isdx)

!*    Determine the angle of long pit dimension with North
   if (ayinit(isdx) >= axinit(isdx)) then
      aalpha(isdx) = aangle(isdx)
   else if (axinit(isdx) > ayinit(isdx)) then
      aalpha(isdx) = aangle(isdx) + 90.0d0
   end if

!*    Calculate the effective pit depth
   apdeff(isdx) = avolum(isdx) / (axinit(isdx) * ayinit(isdx))

!*    Calculate Initial Sigma-Z
   aszini(isdx) = apdeff(isdx) / 4.3d0

!     Determine coordinates for center of rectangular source
   axcntr(isdx) = 0.0d0
   aycntr(isdx) = 0.0d0
   do i = 1, nvert
      axcntr(isdx) = axcntr(isdx) + axvert(i,isdx)
      aycntr(isdx) = aycntr(isdx) + ayvert(i,isdx)
   end do
   axcntr(isdx) = axcntr(isdx)/dble(nvert)
   aycntr(isdx) = aycntr(isdx)/dble(nvert)

   return
end subroutine oparm

!CRT  D063 Platform Downwash
subroutine platfm
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, isdx
   logical :: found
   double precision :: temp(ifmax)

!     Variable Initializations
   found  = .false.
   modnam = 'PLATFM'
   temp   = 0.0d0

!     Check The Number Of The Fields
   if (ifc <= 4) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     Search For The Source ID Index
   call sindex(srcid,nsrc,field(3),isdx,found)

   if (found) then

!        Check for POINT, POINTHOR, POINTCAP source type
      if (srctyp(isdx) /= 'POINT' .and.&
      &srctyp(isdx) /= 'POINTHOR' .and.&
      &srctyp(isdx) /= 'POINTCAP') then
!           WRITE Error Message: PLATFORM only applies to POINT,
!           POINTHOR, and POINTCAP sources
         call errhdl(path,modnam,'E','631',srcid(isdx))
         go to 999
      end if

!        Check for Previous PLATFORM Card for This Source
      if (soplat(isdx) == 'Y') then
!           WRITE Error Message: Duplicate PLATFORM Keyword for source
         call errhdl(path,modnam,'E','632',srcid(isdx))
         go to 999
      else
         soplat(isdx) = 'Y'
      end if

!        Assign The Parameter Arrays
      do i = 4, ifc
         call stodbl(field(i),ilen_fld,temp(i-3),imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
      end do
!        Set platform parameters
      platelv(isdx) = temp(1)    ! Platform base elevation above sea-level
      plathb(isdx) = temp(2)     ! Platform building height above platform base elevation
      platwb(isdx) = temp(3)     ! Platform building width

!        Set platform flag to true only if building width and height are greater the 0.0 (0.001)
      if (plathb(isdx) > 0.001d0 .and.&
      &platwb(isdx) > 0.001d0) then
         osplat(isdx) = .true.
      end if

   else
!        WRITE Error Message    ! Source Location Has Not Been Identified Yet
      call errhdl(path,modnam,'E','300',keywrd)
   end if

999 return
end subroutine platfm


subroutine lparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   save
   integer :: i, isdx
   double precision :: temp(ifmax), xlen, ylen
! Unused INTEGER :: J

!     Variable Initializations
   modnam = 'LPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   awidth(isdx) = temp(3)
   if (ifc == 7) then
      aszini(isdx) = temp(4)
   else
      aszini(isdx) = 0.0d0
   end if

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 100.0d0) then
!        WRITE Warning Message:  Large Release Height (> 100M)
      call errhdl(path,modnam,'W','320',' HS ')
   end if


   if (aszini(isdx) < 0.0d0) then
!*       WRITE Warning Message:  Negative Initial Vertical Parameter
      call errhdl(path,modnam,'E','209',' SZINIT ')
   else if (aszini(isdx) < 1.0e-5) then
!*       Set to Small Value to Avoid Zero-divide and Underflow
      aszini(isdx) = 1.0d-5
   else if (aszini(isdx) > 200.0d0) then
!*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
      call errhdl(path,modnam,'W','320',' SZINIT ')
   end if

   if (awidth(isdx) < 1.0d0) then
!        WRITE Warning Message:  Negative Width Parameter
      call errhdl(path,modnam,'E','320',' WIDTH ')
   end if

   xlen = axs2(isdx) - axs1(isdx)
   ylen = ays2(isdx) - ays1(isdx)

! --- Use center of LINE source for "origin"
   axs(isdx) = (axs1(isdx) + axs2(isdx))*0.5d0
   ays(isdx) = (ays1(isdx) + ays2(isdx))*0.5d0

   axinit(isdx) = awidth(isdx)
   ayinit(isdx) = dsqrt( xlen**2 + ylen**2 )
   aangle(isdx) = datan2(xlen,ylen) * rtodeg

!     Check for aspect ratio (length/width) > 100
   if (ayinit(isdx)/axinit(isdx) > 100.00001d0 .or.&
   &axinit(isdx)/ayinit(isdx) > 100.00001d0) then
!        WRITE Warning Message: Aspect ratio > 100 for line source
      call errhdl(path,modnam,'W','390',srcid(isdx))
   end if

!     Set Number of Vertices (4 for Rectangular Source)
   nverts(isdx) = 4

!     Set Coordinates of Vertices for Equivalent Rectangular Area.
!     Vertices Start with the "Southwest" Corner and Are Defined
!     Clockwise.  The First Vertex is Repeated as the Last Vertex.

   axvert(1,isdx) = axs1(isdx) -&
   &(axinit(isdx)/2.0d0)*dcos(aangle(isdx)*dtorad)
   ayvert(1,isdx) = ays1(isdx) +&
   &(axinit(isdx)/2.0d0)*dsin(aangle(isdx)*dtorad)

   axvert(2,isdx) = axvert(1,isdx) +&
   &(ayinit(isdx)*dsin(aangle(isdx)*dtorad))
   ayvert(2,isdx) = ayvert(1,isdx) +&
   &(ayinit(isdx)*dcos(aangle(isdx)*dtorad))

   axvert(3,isdx) = axvert(2,isdx) +&
   &(axinit(isdx)*dcos(aangle(isdx)*dtorad))
   ayvert(3,isdx) = ayvert(2,isdx) -&
   &(axinit(isdx)*dsin(aangle(isdx)*dtorad))

   axvert(4,isdx) = axvert(3,isdx) -&
   &(ayinit(isdx)*dsin(aangle(isdx)*dtorad))
   ayvert(4,isdx) = ayvert(3,isdx) -&
   &(ayinit(isdx)*dcos(aangle(isdx)*dtorad))

   axvert(5,isdx) = axvert(1,isdx)
   ayvert(5,isdx) = ayvert(1,isdx)

!     Determine coordinates for center of rectangular source
   axcntr(isdx) = 0.0d0
   aycntr(isdx) = 0.0d0
   do i = 1, nverts(isdx)
      axcntr(isdx) = axcntr(isdx) + axvert(i,isdx)
      aycntr(isdx) = aycntr(isdx) + ayvert(i,isdx)
   end do
   axcntr(isdx) = axcntr(isdx)/dble(nverts(isdx))
   aycntr(isdx) = aycntr(isdx)/dble(nverts(isdx))

   return
end subroutine lparm

subroutine blparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'BLPARM'

   aqs(isdx) = temp(1)                ! average emission release rate
   ahs(isdx) = temp(2)                ! average release height

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 100.0d0) then
!        WRITE Warning Message:  Large Release Height (> 100M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

! --- Use center of EACH BUOYANT LINE for "origin"
   axs(isdx) = (axs1(isdx) + axs2(isdx))*0.5d0
   ays(isdx) = (ays1(isdx) + ays2(isdx))*0.5d0

   return
end subroutine blparm

subroutine rlparm(isdx,temp)
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
   use main1
   use rline_data, only: rlsource
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'RLPARM'

   if (srctyp(isdx) == 'RLINE') then
      rlsource(isdx)%qemis = temp(1)*temp(3)    ! emissions release rate Lnemis*Width
      rlsource(isdx)%dcl = 0.0          ! offset distance from center line
      rlsource(isdx)%zsb = temp(2)      !set height of the source
      rlsource(isdx)%zse = temp(2)      !set height of the source
      if (ifc == 7) then
         rlsource(isdx)%init_sigmaz = temp(4)  ! initial vertical spread
      else
         rlsource(isdx)%init_sigmaz = 0.0  ! initial vertical spread
      end if
      rlsource(isdx)%width = temp(3)        ! width of roadway
      rlsource(isdx)%htwall = 0.0d0         ! barrier 1 height
      rlsource(isdx)%dclwall = 0.0d0        ! barrier 1 distance from center line
      rlsource(isdx)%htwall2 = 0.0d0        ! barrier 2 height
      rlsource(isdx)%dclwall2 = 0.0d0       ! barrier 2 distance from center line
      rlsource(isdx)%depth = 0.0d0          ! depth of depression
      rlsource(isdx)%wtop = 0.0d0           ! width of top of depression
      rlsource(isdx)%wbottom = 0.0d0        ! width of bottom of depression
   end if


   if (srctyp(isdx) == 'RLINEXT') then
      rlsource(isdx)%qemis = temp(1)        ! emissions release rate
      rlsource(isdx)%dcl = temp(2)          ! offset distance from center line
      rlsource(isdx)%width = temp(3)        ! width of roadway
      rlsource(isdx)%init_sigmaz = temp(4)  ! initial vertical spread
      rlsource(isdx)%htwall = 0.0d0         ! barrier 1 height
      rlsource(isdx)%dclwall = 0.0d0        ! barrier 1 distance from center line
      rlsource(isdx)%htwall2 = 0.0d0        ! barrier 2 height
      rlsource(isdx)%dclwall2 = 0.0d0       ! barrier 2 distance from center line
      rlsource(isdx)%depth = 0.0d0          ! depth of depression
      rlsource(isdx)%wtop = 0.0d0           ! width of top of depression
      rlsource(isdx)%wbottom = 0.0d0        ! width of bottom of depression
   end if
!     Perform Error Checking on Source Parameters
   if (rlsource(isdx)%qemis < 0.0d0) then
!        WRITE Error Message: Negative Emission Rate
      call errhdl(path,modnam,'E','209',' QS ')
   else if (rlsource(isdx)%qemis == 0.0d0) then
!        WRITE Warning Message: Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (rlsource(isdx)%init_sigmaz < 0.0d0) then
!        WRITE Error Message:  Negative Initial Vertical Spread
      call errhdl(path,modnam,'E','209',' INIT_SIGMAZ ')
   else if (rlsource(isdx)%init_sigmaz > 42.0d0) then
!        WRITE Warning Message:  Large Initial Vertical Parameter
      call errhdl(path,modnam,'E','320',' INIT_SIGMAZ ')
   end if

   if (rlsource(isdx)%width < 0.0d0) then
!        WRITE Error Message:  Negative Roadway Width
      call errhdl(path,modnam,'E','209',' WIDTH ')
   else if (rlsource(isdx)%width == 0.0d0) then
!        WRITE Error Message:  Width of Roadway Equals 0.0
      call errhdl(path,modnam,'W','320',' WIDTH ')
   else if (rlsource(isdx)%width > 30.0d0) then
!        WRITE Warning Message:  Large Roadwidth (> 30m)
      call errhdl(path,modnam,'W','320',' WIDTH ')
   end if

   return
end subroutine rlparm

!     Added for sidewash
subroutine swparm(isdx,temp)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: temp(ifmax)

!     Variable Initializations
   modnam = 'SWPARM'

   aqs(isdx) = temp(1)
   ahs(isdx) = temp(2)
   abw(isdx) = temp(3)
   abl(isdx) = temp(4)
   abh(isdx) = temp(5)
   aba(isdx) = temp(6)

!     Perform QA Error Checking on Source Parameters

   if (aqs(isdx) == 0.0d0) then
!        WRITE Warning Message:  Emission Rate Equals 0.0
      call errhdl(path,modnam,'W','320',' QS ')
   end if

   if (ahs(isdx) < 0.0d0) then
!        WRITE Error Message:  Negative Release Height
      call errhdl(path,modnam,'E','209',' HS ')
   else if (ahs(isdx) > 600.0d0) then
!        WRITE Warning Message:  Large Release Height (> 600M)
      call errhdl(path,modnam,'W','320',' HS ')
   else if (ahs(isdx) > 3000.0d0) then
!        WRITE Error Message:  Large Release Height (> 3000M)
      call errhdl(path,modnam,'E','324',srcid(isdx))
   end if

   if (abw(isdx) <= 0.0d0) then
!        Set Building width 1.0 m for Zero or Negative Values
      abw(isdx) = 1.0
   end if

   if (abl(isdx) <= 0.0d0) then
!        Set Building width 1.0 m for Zero or Negative Values
      abl(isdx) = 1.0
   end if

   if (abh(isdx) <= 0.0d0) then
!        Set Building hieght 1.0 m for Zero or Negative Values
      abh(isdx) = 1.0
   end if

   if (aba(isdx) < 0.0d0) then
!        Set Building width 1.0 m for Zero or Negative Values
      aba(isdx) = aba(isdx) + 360.
   else if (aba(isdx) > 360.) then
      aba(isdx) = aba(isdx) - 360.
   end if

   return
end subroutine swparm


subroutine dsbldg
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'DSBLDG'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

!     Verify The Effective Srcid
   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found) then
         if (srctyp(isdx)(1:5) == 'POINT') then
!              Fill Array
            call dsfill(isdx)
         else
!              WRITE Warning Message: Building Inputs for Non-POINT Source
            call errhdl(path,modnam,'W','233',srcid(isdx))
         end if
      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp .and. srctyp(i)(1:5)=='POINT') then
            isdx = i
!              Fill DS Array
            call dsfill(isdx)
         end if
      end do
   end if

999 return
end subroutine dsbldg

subroutine dsfill(isdx)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: j, k, isdx

!     Variable Initializations
   modnam = 'DSFILL'

   if (keywrd == 'BUILDHGT') then
      iset = iwrk2(isdx,1)
      do k = 4, ifc
!           Change Fields To Numbers
         call stodbl(field(k),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
         do j = 1, imit
            iset = iset + 1
!              Assign The Field
            if (iset <= nsec) then
               adsbh(iset,isdx) = dnum
               if (dnum < 0.0d0) then
!                    WRITE Error Message:  Negative Value for ADSBH
                  call errhdl(path,modnam,'E','209',keywrd)
               end if
            else
!                 WRITE Error Message    ! Too Many Sectors Input
               call errhdl(path,modnam,'E','234',keywrd)
            end if
         end do
      end do
      iwrk2(isdx,1) = iset
   else if (keywrd == 'BUILDWID') then
      iset = iwrk2(isdx,2)
      do k = 4, ifc
!           Change Fields To Numbers
         call stodbl(field(k),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
         do j = 1, imit
            iset = iset + 1
!              Assign The Field
            if (iset <= nsec) then
               adsbw(iset,isdx) = dnum
               if (dnum < 0.0d0) then
!                    WRITE Error Message:  Negative Value for ADSBW
                  call errhdl(path,modnam,'E','209',keywrd)
               end if
            else
!                 WRITE Error Message    ! Too Many Sectors Input
               call errhdl(path,modnam,'E','234',keywrd)
            end if
         end do
      end do
      iwrk2(isdx,2) = iset

! --- PRIME --------------------------------------------
! --- Fill building length information
   else if (keywrd == 'BUILDLEN') then
      iset = iwrk2(isdx,11)
      do k = 4, ifc
!           Change Fields To Numbers
         call stodbl(field(k),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
         do j = 1, imit
            iset = iset + 1
!              Assign The Field
            if (iset <= nsec) then
               adsbl(iset,isdx) = dnum
               if (dnum < 0.0d0) then
!                    WRITE Error Message:  Negative value for ADSBL
                  call errhdl(path,modnam,'E','209',keywrd)
               end if
            else
!                 WRITE Error Message    ! Too Many Sectors Input
               call errhdl(path,modnam,'E','234',keywrd)
            end if
         end do
      end do
      iwrk2(isdx,11) = iset

! --- Fill building XBADJ information
   else if (keywrd == 'XBADJ   ') then
      iset = iwrk2(isdx,12)
      do k = 4, ifc
!           Change Fields To Numbers
         call stodbl(field(k),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
         do j = 1, imit
            iset = iset + 1
!              Assign The Field
            if (iset <= nsec) then
               adsxadj(iset,isdx) = dnum
            else
!                 WRITE Error Message    ! Too Many Sectors Input
               call errhdl(path,modnam,'E','234',keywrd)
            end if
         end do
      end do
      iwrk2(isdx,12) = iset

! --- Fill building YBADJ information
   else if (keywrd == 'YBADJ   ') then
      iset = iwrk2(isdx,13)
      do k = 4, ifc
!           Change Fields To Numbers
         call stodbl(field(k),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            call errhdl(path,modnam,'E','208',keywrd)
            cycle
         end if
         do j = 1, imit
            iset = iset + 1
!              Assign The Field
            if (iset <= nsec) then
               adsyadj(iset,isdx) = dnum
            else
!                 WRITE Error Message    ! Too Many Sectors Input
               call errhdl(path,modnam,'E','234',keywrd)
            end if
         end do
      end do
      iwrk2(isdx,13) = iset
! --------------------------------------------------------

   end if

   return
end subroutine dsfill

subroutine emvary
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ih, il, isdx, iqmax
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'EMVARY'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Error Message: No Numerical Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

!     Verify The Effective Srcid
   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found) then
         qflag(isdx) = field(4)
         if (qflag(isdx) == 'SEASON') then
            iqmax = 4
         else if (qflag(isdx) == 'MONTH') then
            iqmax = 12
         else if (qflag(isdx) == 'HROFDY') then
            iqmax = 24
         else if (qflag(isdx) == 'WSPEED') then
            iqmax = 6
         else if (qflag(isdx) == 'SEASHR') then
            iqmax = 96
         else if (qflag(isdx) == 'HRDOW') then
            iqmax = 72
            L_DayOfWeekOpts = .true.
         else if (qflag(isdx) == 'HRDOW7') then
            iqmax = 168
            L_DayOfWeekOpts = .true.
         else if (qflag(isdx) == 'SHRDOW') then
            iqmax = 288
            L_DayOfWeekOpts = .true.
         else if (qflag(isdx) == 'SHRDOW7') then
            iqmax = 672
            L_DayOfWeekOpts = .true.
         else if (qflag(isdx) == 'MHRDOW') then
            iqmax = 864
            L_DayOfWeekOpts = .true.
         else if (qflag(isdx) == 'MHRDOW7') then
            iqmax = 2016
            L_DayOfWeekOpts = .true.
         else
!              WRITE Error Message    ! Invalid QFLAG Field Entered
            call errhdl(path,modnam,'E','203','QFLAG')
         end if
         if (iqmax <= nqf) then
            call effill(isdx,iqmax)
         else
!              WRITE Error Message     ! NQF Parameter Not Large Enough
            write(dummy,'(''NQF ='',I6)') nqf
            call errhdl(path,modnam,'E','260',dummy)
         end if
      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp) then
            isdx = i
            qflag(isdx) = field(4)
            if (qflag(isdx) == 'SEASON') then
               iqmax = 4
            else if (qflag(isdx) == 'MONTH') then
               iqmax = 12
            else if (qflag(isdx) == 'HROFDY') then
               iqmax = 24
            else if (qflag(isdx) == 'WSPEED') then
               iqmax = 6
            else if (qflag(isdx) == 'SEASHR') then
               iqmax = 96
            else if (qflag(isdx) == 'HRDOW') then
               iqmax = 72
               L_DayOfWeekOpts = .true.
            else if (qflag(isdx) == 'HRDOW7') then
               iqmax = 168
               L_DayOfWeekOpts = .true.
            else if (qflag(isdx) == 'SHRDOW') then
               iqmax = 288
               L_DayOfWeekOpts = .true.
            else if (qflag(isdx) == 'SHRDOW7') then
               iqmax = 672
               L_DayOfWeekOpts = .true.
            else if (qflag(isdx) == 'MHRDOW') then
               iqmax = 864
               L_DayOfWeekOpts = .true.
            else if (qflag(isdx) == 'MHRDOW7') then
               iqmax = 2016
               L_DayOfWeekOpts = .true.
            else
!                 WRITE Error Message    ! Invalid QFLAG Field Entered
               call errhdl(path,modnam,'E','203','QFLAG')
            end if
            if (iqmax <= nqf) then
               call effill(isdx,iqmax)
            else
!                 WRITE Error Message    ! NQF Parameter Not Large Enough
               write(dummy,'(''NQF ='',I6)') nqf
               call errhdl(path,modnam,'E','260',dummy)
            end if
         end if
      end do
   end if

999 return
end subroutine emvary

subroutine effill(isdx,iqmax)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: j, k, isdx, iqmax

!     Variable Initializations
   modnam = 'EFFILL'

   iset = iwrk2(isdx,4)

   do k = 5, ifc
!        Change Fields To Numbers
      call stodbl(field(k),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         cycle
      end if
      do j = 1, imit
         iset = iset + 1
!           Assign The Field
         if (iset <= iqmax) then
            qfact(iset,isdx) = dnum
            if (dnum < 0.0d0) then
!                 WRITE Error Message:  Negative Value for QFACT
               call errhdl(path,modnam,'E','209',keywrd)
            end if
         else
!              WRITE Error Message    ! Too Many QFACT Values Input
            if (isdx <= 999) then
               write(dummy,'(''QFACT Src'',I3.3)') isdx
            else if (isdx <= 99999) then
               write(dummy,'(''QF Src'',I5.5)') isdx
            else
               write(dummy,'(''QF Src>99999'')')
            end if
            call errhdl(path,modnam,'E','231',dummy)
         end if
      end do
   end do

   iwrk2(isdx,4) = iset

   return
end subroutine effill

subroutine emunit
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EMUNIT'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Fetch Each Field
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

   emifac(1) = dnum
   emilbl(1) = field(4)
   outlbl(1) = field(5)
   if (.not.conc .and. annual) then
      perlbl(1) = runst1(locb(5):loce(5))//'/YR'
   else
      perlbl(1) = field(5)
   end if

999 return
end subroutine emunit

subroutine counit
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'COUNIT'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Fetch Each Field
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

   emifac(1) = dnum
   emilbl(1) = field(4)
   outlbl(1) = field(5)
   perlbl(1) = field(5)

999 return
end subroutine counit

subroutine dpunit
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i

!     Variable Initializations
   modnam = 'DPUNIT'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Fetch Each Field
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

   if (.not. conc) then
      do i = 1, ntyp
         emifac(i) = dnum
         emilbl(i) = field(4)
         outlbl(i) = field(5)
         if (annual) then
            perlbl(i) = runst1(locb(5):loce(5))//'/YR'
         else
            perlbl(i) = field(5)
         end if
      end do
   else
      do i = 2, ntyp
         emifac(i) = dnum
         emilbl(i) = field(4)
         outlbl(i) = field(5)
         if (annual) then
            perlbl(i) = runst1(locb(5):loce(5))//'/YR'
         else
            perlbl(i) = field(5)
         end if
      end do
   end if

999 return
end subroutine dpunit

subroutine partdep
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'PARTDEP'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Error Message: No Numerical Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     Process The Appropriate Settling & Removal Parameter
   if (keywrd == 'PARTDIAM') then
!        Process Particle Diameter Categories (PDIAM)       ---   CALL INPPDM
      call inppdm
   else if (keywrd == 'MASSFRAX') then
!        Process Mass Fractions (PHI)                       ---   CALL INPPHI
      call inpphi
   else if (keywrd == 'PARTDENS') then
!        Process Particle Density (PDENS)                   ---   CALL INPPDN
      call inppdn
   end if

999 return
end subroutine partdep

subroutine inppdm
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark
! Unused INTEGER :: J

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'INPPDM'

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found .and. .not.l_method2(isdx)) then
         iset = iwrk2(isdx,5)
         do k = 4, ifc
!              Change It To Numbers
            call stodbl(field(k),ilen_fld,dnum,imit)
!              Check The Numerical Field
            if (imit == -1) then
               call errhdl(path,modnam,'E','208',keywrd)
               cycle
            else if (imit > 1) then
! ---             Use of '*' for repeat input values is not meaningful
!                 for particle diameters; Issue Error message
               call errhdl(path,modnam,'E','288',keywrd)
               cycle
            else if (dnum <= 0.001d0 .or. dnum > 1000.0d0) then
!                 WRITE Error Message: Particle Diameter Out-of-Range
               call errhdl(path,modnam,'E','335',srcid(isdx))
            end if
            iset = iset + 1
            if (iset <= npdmax) then
!                 Assign The Field
               apdiam(iset,isdx) = dnum
            else
!                 WRITE Error Message: Too Many PartDiam Categories
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''NPD= '',I7)') npdmax
               call errhdl(path,modnam,'E','290',dummy)
            end if
         end do
         iwrk2(isdx,5) = iset

      else if (found .and. l_method2(isdx)) then
! ---       Write Error Message     ! Source ID identified as Method 2
         call errhdl(path,modnam,'E','386',srcid(isdx))

      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      source_loop: do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp .and. .not.l_method2(i)) then
            iset = iwrk2(i,5)
            do k = 4, ifc
!                 Get Numbers
               call stodbl(field(k),ilen_fld,dnum,imit)
!                 Check The Numerical Field
               if (imit == -1) then
                  call errhdl(path,modnam,'E','208',keywrd)
                  cycle source_loop
               else if (imit > 1) then
! ---                Use of '*' for repeat input values is not meaningful
!                    for particle diameters; Issue Error message
                  call errhdl(path,modnam,'E','288',keywrd)
                  cycle source_loop
               else if (dnum <= 0.001d0 .or. dnum>1000.0d0) then
!                    WRITE Error Message: Particle Diameter Out-of-Range
                  call errhdl(path,modnam,'E','335',srcid(i))
               end if
               iset = iset + 1
               if (iset <= npdmax) then
                  apdiam(iset,i) = dnum
               else
!                    WRITE Error Message: Too Many PartDiam Categories
!                    This shouldn't occur since limits are dynamically allocated
                  write(dummy,'(''NPD= '',I7)') npdmax
                  call errhdl(path,modnam,'E','290',dummy)
               end if
            end do
            iwrk2(i,5) = iset

         else if (ingrp .and. l_method2(i)) then
! ---          Write Error Message     ! Source ID identified as Method 2
            call errhdl(path,modnam,'E','386',srcid(i))

         end if
      end do source_loop
   end if

999 return
end subroutine inppdm

subroutine inpphi
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'INPPHI'

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found) then
         iset = iwrk2(isdx,6)
         do k = 4, ifc
!              Change It To Numbers
            call stodbl(field(k),ilen_fld,dnum,imit)
!              Check The Numerical Field
            if (imit == -1) then
               call errhdl(path,modnam,'E','208',keywrd)
               cycle
            end if
            if (dnum < 0.0d0 .or. dnum > 1.0d0) then
!                 WRITE Error Message: Mass Fraction Out-of-Range
               call errhdl(path,modnam,'E','332',srcid(isdx))
            end if
            do j = 1, imit
               iset = iset + 1
               if (iset <= npdmax) then
!                    Assign The Field
                  aphi(iset,isdx) = dnum
               else
!                    WRITE Error Message: Too Many PartDiam Categories
!                    This shouldn't occur since limits are dynamically allocated
                  write(dummy,'(''NPD= '',I7)') npdmax
                  call errhdl(path,modnam,'E','290',dummy)
               end if
            end do
         end do
         iwrk2(isdx,6) = iset
      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      source_loop: do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp) then
            iset = iwrk2(i,6)
            do k = 4, ifc
!                 Get Numbers
               call stodbl(field(k),ilen_fld,dnum,imit)
!                 Check The Numerical Field
               if (imit == -1) then
                  call errhdl(path,modnam,'E','208',keywrd)
                  cycle source_loop
               end if
               if (dnum < 0.0d0 .or. dnum > 1.0d0) then
!                    WRITE Error Message: Mass Fraction Out-of-Range
                  call errhdl(path,modnam,'E','332',srcid(i))
               end if
               do j = 1, imit
                  iset = iset + 1
                  if (iset <= npdmax) then
                     aphi(iset,i) = dnum
                  else
!                       WRITE Error Message: Too Many PartDiam Categories
!                       This shouldn't occur since limits are dynamically allocated
                     write(dummy,'(''NPD= '',I7)') npdmax
                     call errhdl(path,modnam,'E','290',dummy)
                  end if
               end do
            end do
            iwrk2(i,6) = iset
         end if
      end do source_loop
   end if

999 return
end subroutine inpphi

subroutine inppdn
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'INPPDN'

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found) then
         iset = iwrk2(isdx,7)
         do k = 4, ifc
!              Change It To Numbers
            call stodbl(field(k),ilen_fld,dnum,imit)
!              Check The Numerical Field
            if (imit == -1) then
               call errhdl(path,modnam,'E','208',keywrd)
               cycle
            end if
            if (dnum <= 0.0d0) then
!                 WRITE Error Message: Particle Density Out-of-Range
               call errhdl(path,modnam,'E','334',srcid(isdx))
            else if (dnum <= 0.1d0) then
!                 WRITE Warning Message: Particle Density may be Out-of-Range
               call errhdl(path,modnam,'W','334',srcid(isdx))
            end if
            do j = 1, imit
               iset = iset + 1
               if (iset <= npdmax) then
!                    Assign The Field
                  apdens(iset,isdx) = dnum
               else
!                    WRITE Error Message: Too Many PartDiam Categories
!                    This shouldn't occur since limits are dynamically allocated
                  write(dummy,'(''NPD= '',I7)') npdmax
                  call errhdl(path,modnam,'E','290',dummy)
               end if
            end do
         end do
         iwrk2(isdx,7) = iset
      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      source_loop: do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp) then
            iset = iwrk2(i,7)
            do k = 4, ifc
!                 Get Numbers
               call stodbl(field(k),ilen_fld,dnum,imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
                  cycle source_loop
               end if
               if (dnum <= 0.0d0) then
!                    WRITE Error Message: Particle Density Out-of-Range
                  call errhdl(path,modnam,'E','334',srcid(i))
               else if (dnum <= 0.1d0) then
!                    WRITE Warning Message: Particle Density may be Out-of-Range
                  call errhdl(path,modnam,'W','334',srcid(i))
               end if
               do j = 1, imit
                  iset = iset + 1
                  if (iset <= npdmax) then
                     apdens(iset,i) = dnum
                  else
!                       WRITE Error Message: Too Many PartDiam Categories
!                       This shouldn't occur since limits are dynamically allocated
                     write(dummy,'(''NPD= '',I7)') npdmax
                     call errhdl(path,modnam,'E','290',dummy)
                  end if
               end do
            end do
            iwrk2(i,7) = iset
         end if
      end do source_loop
   end if

999 return
end subroutine inppdn

subroutine gasdep
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark, dlookup

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'GASDEP'
   dlookup = .false.

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 7) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found) then
         sogas(isdx) = 'Y'
         dlookup=.false.
!           Read Dry Deposition Parameters
!           Change Them To Numbers

!           First Get Gas Diffusivity (cm^2/s)
         call stodbl(field(4),ilen_fld,dnum,imit)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
            dnum = 5.5d-02
            dlookup=.true.
         else if ((pollut == 'HGII') .and.&
         &(dnum == 0.0d0)) then
            dnum = 4.5d-02
            dlookup=.true.
         else if ((pollut == 'TCDD') .and.&
         &(dnum == 0.0d0)) then
            dnum = 5.196d-02
            dlookup=.true.
         else if ((pollut == 'BAP') .and.&
         &(dnum == 0.0d0)) then
            dnum = 5.13d-02
            dlookup=.true.
         else if ((pollut == 'SO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.112d-01
            dlookup=.true.
         else if ((pollut == 'NO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.361d-01
            dlookup=.true.
         else if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else if (dnum <= 0.0d0) then
            call errhdl(path,modnam,'E','380','PDIFF')
         end if
!           Assign The Field
         pdiff(isdx) = dnum

!PES ---    Next Get Diffusivity in Water (cm^2/s)
         call stodbl(field(5),ilen_fld,dnum,imit)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
            dnum = 6.4d-06
            dlookup=.true.
         else if ((pollut == 'HGII') .and.&
         &(dnum == 0.0d0)) then
            dnum = 5.2d-06
            dlookup=.true.
         else if ((pollut == 'TCDD') .and.&
         &(dnum == 0.0d0)) then
            dnum = 4.392d-06
            dlookup=.true.
         else if ((pollut == 'BAP') .and.&
         &(dnum == 0.0d0)) then
            dnum = 4.44d-06
            dlookup=.true.
         else if ((pollut == 'SO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.83d-05
            dlookup=.true.
         else if ((pollut == 'NO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.4d-05
            dlookup=.true.
         else if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else if (dnum <= 0.0d0) then
            call errhdl(path,modnam,'E','380','PDIFFW')
         end if
!           Assign The Field
         pdiffw(isdx) = dnum

!           Now Get Lipid Cuticle Resistence for Individual Leaves (RCLI)
         call stodbl(field(6),ilen_fld,dnum,imit)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
            dnum = 1.0d05
            dlookup=.true.
         else if ((pollut == 'HGII') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.0d05
            dlookup=.true.
         else if ((pollut == 'TCDD') .and.&
         &(dnum == 0.0d0)) then
            dnum = 9.67d0
            dlookup=.true.
         else if ((pollut == 'BAP') .and.&
         &(dnum == 0.0d0)) then
            dnum = 4.41d-01
            dlookup=.true.
         else if ((pollut == 'SO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 7.32d02
            dlookup=.true.
         else if ((pollut == 'NO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.2d04
            dlookup=.true.
         else if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else if (dnum <= 0.0d0) then
            call errhdl(path,modnam,'E','380','RCLI')
         end if
!           Assign The Field
         rcli(isdx) = dnum

!           Get the Henry's Law Constant
         call stodbl(field(7),ilen_fld,dnum,imit)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
            dnum = 7.19d02
            dlookup=.true.
         else if ((pollut == 'HGII') .and.&
         &(dnum == 0.0d0)) then
            dnum = 7.2d-05
            dlookup=.true.
         else if ((pollut == 'TCDD') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.46d0
            dlookup=.true.
         else if ((pollut == 'BAP') .and.&
         &(dnum == 0.0d0)) then
            dnum = 4.6d-02
            dlookup=.true.
         else if ((pollut == 'SO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 7.2d01
            dlookup=.true.
         else if ((pollut == 'NO2') .and.&
         &(dnum == 0.0d0)) then
            dnum = 8.444d03
            dlookup=.true.
         else if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else if (dnum <= 0.0d0) then
            call errhdl(path,modnam,'E','380','HENRY')
         end if
!           Assign The Field
         henry(isdx) = dnum

!           WRITE Warning Message:  Default deposition parameter used
         if (dlookup .eqv. .true.) then
            call errhdl(path,modnam,'W','473',srcid(isdx))
         end if

      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp) then
            isdx = i
            sogas(isdx) = 'Y'
!              Read Dry Deposition Parameters
!              Change Them To Numbers

!              First Get Gas Diffusivity
            call stodbl(field(4),ilen_fld,dnum,imit)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
               dnum = 5.5d-02
            else if ((pollut == 'HGII') .and.&
            &(dnum == 0.0d0)) then
               dnum = 4.5d-02
            else if ((pollut == 'TCDD') .and.&
            &(dnum == 0.0d0)) then
               dnum = 5.196d-02
            else if ((pollut == 'BAP') .and.&
            &(dnum == 0.0d0)) then
               dnum = 5.13d-02
            else if ((pollut == 'SO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.112d-01
            else if ((pollut == 'NO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.361d-01
            else if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
            else if (dnum <= 0.0d0) then
               call errhdl(path,modnam,'E','380','PDIFF')
            end if
!              Assign The Field
            pdiff(isdx) = dnum

!PES ---       Next Get Diffusivity in Water (cm^2/s)
            call stodbl(field(5),ilen_fld,dnum,imit)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
               dnum = 6.4d-06
            else if ((pollut == 'HGII') .and.&
            &(dnum == 0.0d0)) then
               dnum = 5.2d-06
            else if ((pollut == 'TCDD') .and.&
            &(dnum == 0.0d0)) then
               dnum = 4.392d-06
            else if ((pollut == 'BAP') .and.&
            &(dnum == 0.0d0)) then
               dnum = 4.44d-06
            else if ((pollut == 'SO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.83d-05
            else if ((pollut == 'NO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.4d-05
            else if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
            else if (dnum <= 0.0d0) then
               call errhdl(path,modnam,'E','380','PDIFFW')
            end if
!              Assign The Field
            pdiffw(isdx) = dnum

!              Now Get Lipid Cuticle Resistence for Individual Leaves (RCLI)
            call stodbl(field(6),ilen_fld,dnum,imit)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
               dnum = 1.0d05
            else if ((pollut == 'HGII') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.0d05
            else if ((pollut == 'TCDD') .and.&
            &(dnum == 0.0d0)) then
               dnum = 9.67d0
            else if ((pollut == 'BAP') .and.&
            &(dnum == 0.0d0)) then
               dnum = 4.41d-01
            else if ((pollut == 'SO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 7.32d02
            else if ((pollut == 'NO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.2d04
            else if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
            else if (dnum <= 0.0d0) then
               call errhdl(path,modnam,'E','380','RCLI')
            end if
!              Assign The Field
            rcli(isdx) = dnum

!              Get the Henry's Law Constant
            call stodbl(field(7),ilen_fld,dnum,imit)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            if ((pollut == 'HG0') .and. (dnum == 0.0d0)) then
               dnum = 7.19d02
            else if ((pollut == 'HGII') .and.&
            &(dnum == 0.0d0)) then
               dnum = 7.2d-05
            else if ((pollut == 'TCDD') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.46d0
            else if ((pollut == 'BAP') .and.&
            &(dnum == 0.0d0)) then
               dnum = 4.6d-02
            else if ((pollut == 'SO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 7.2d01
            else if ((pollut == 'NO2') .and.&
            &(dnum == 0.0d0)) then
               dnum = 8.444d03
            else if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
            else if (dnum <= 0.0d0) then
               call errhdl(path,modnam,'E','380','HENRY')
            end if
!              Assign The Field
            henry(isdx) = dnum

         end if
      end do
   end if

999 return
end subroutine gasdep

subroutine meth_2
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark, dlookup

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'METH_2'
   dlookup = .false.

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found .and. .not.l_method2(isdx) .and.&
      &iwrk2(isdx,5)==0) then
         l_method2(isdx) = .true.

         dlookup=.false.
!           Read Dry Deposition Parameters
!           Change Them To Numbers
!           First Get Mass Fraction of Fine Particles (.lt. 2.5 microns)
         call stodbl(field(4),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            go to 999
         end if
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         if ((pollut == 'AR') .and. (dnum == 0.0d0)) then
            dnum = 7.5d-01
            dlookup=.true.
         else if ((pollut == 'CD') .and.&
         &(dnum == 0.0d0)) then
            dnum = 7.0d-01
            dlookup=.true.
         else if ((pollut == 'PB') .and.&
         &(dnum == 0.0d0)) then
            dnum = 7.5d-01
            dlookup=.true.
         else if ((pollut == 'HG') .and.&
         &(dnum == 0.0d0)) then
            dnum = 8.0d-01
            dlookup=.true.
         else if ((pollut == 'POC') .and.&
         &(dnum == 0.0d0)) then
            dnum = 9.0d-01
            dlookup=.true.
         else if (dnum < 0.0d0 .or. dnum > 1.0d0) then
!              WRITE Error Message: Mass Fraction Out-of-Range
            call errhdl(path,modnam,'E','332',srcid(isdx))
         end if

!           Assign The Field
         finemass(isdx) = dnum

!           Now Get Mass Mean Diameter
         call stodbl(field(5),ilen_fld,dnum,imit)
!           Check The Numerical Field
!           MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            go to 999
         else if ((pollut == 'AR') .and.&
         &(dnum == 0.0d0)) then
            dnum = 5.0d-01
            dlookup=.true.
         else if ((pollut == 'CD') .and.&
         &(dnum == 0.0d0)) then
            dnum = 6.0d-01
            dlookup=.true.
         else if ((pollut == 'PB') .and.&
         &(dnum == 0.0d0)) then
            dnum = 5.0d-01
            dlookup=.true.
         else if ((pollut == 'HG') .and.&
         &(dnum == 0.0d0)) then
            dnum = 4.0d-01
            dlookup=.true.
         else if ((pollut == 'POC') .and.&
         &(dnum == 0.0d0)) then
            dnum = 1.0d-01
            dlookup=.true.
         end if
!           Assign The Field
         apdiam(1,isdx) = dnum

!           Set mass fraction and particle density
         aphi(1,isdx)   = 1.0d0
         apdens(1,isdx) = 1.0d0

!           Set number of particle size categories to 1
         inpd(isdx) = 1

!           WRITE Warning Message:  Default deposition parameter used
         if (dlookup .eqv. .true.) then
            call errhdl(path,modnam,'W','473',srcid(isdx))
         end if

      else if (found .and. l_method2(isdx)) then
! ---       Write Error Message     ! Source ID identified twice
         call errhdl(path,modnam,'E','387',srcid(isdx))

      else if (found .and. iwrk2(isdx,5) > 0) then
! ---       Write Error Message     ! Source ID identified as Method 1
         call errhdl(path,modnam,'E','386',srcid(isdx))

      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp) then
            isdx = i
            if (.not.l_method2(isdx) .and. iwrk2(isdx,5) == 0) then
               l_method2(isdx) = .true.
            else if (l_method2(isdx)) then
! ---             Write Error Message     ! Source ID identified twice
               call errhdl(path,modnam,'E','387',srcid(isdx))
               cycle
            else if (iwrk2(isdx,5) > 0) then
! ---             Write Error Message     ! Source ID identified as Method 1
               call errhdl(path,modnam,'E','386',srcid(isdx))
               cycle
            end if
!              Read Dry Deposition Parameters
!              Change Them To Numbers
!              First Get Mass Fraction of Fine Particles (.lt. 2.5 microns)
            call stodbl(field(4),ilen_fld,dnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 999
            end if
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            if (dnum < 0.0d0 .or. dnum > 1.0d0) then
!                 WRITE Error Message: Mass Fraction Out-of-Range
               call errhdl(path,modnam,'E','332',srcid(isdx))
            else if ((pollut == 'AR') .and.&
            &(dnum == 0.0d0)) then
               dnum = 7.5d-01
            else if ((pollut == 'CD') .and.&
            &(dnum == 0.0d0)) then
               dnum = 7.0d-01
            else if ((pollut == 'PB') .and.&
            &(dnum == 0.0d0)) then
               dnum = 7.5d-01
            else if ((pollut == 'HG') .and.&
            &(dnum == 0.0d0)) then
               dnum = 8.0d-01
            else if ((pollut == 'POC') .and.&
            &(dnum == 0.0d0)) then
               dnum = 9.0d-01
            end if

!              Assign The Field
!              MW D068 10/30/2020 moved assignment of FINEMASS outside the IF statement above, consistant with the other assignments of APDIAM and FINEMASS in this section
            finemass(isdx) = dnum


!              Now Get Mass Mean Diameter
            call stodbl(field(5),ilen_fld,dnum,imit)
!              Check The Numerical Field
!              MW D068 10/30/2020 check variable name and use preassigned value if field is 0 and polluntant is in lookup list
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 999
            else if ((pollut == 'AR') .and.&
            &(dnum == 0.0d0)) then
               dnum = 5.0d-01
            else if ((pollut == 'CD') .and.&
            &(dnum == 0.0d0)) then
               dnum = 6.0d-01
            else if ((pollut == 'PB') .and.&
            &(dnum == 0.0d0)) then
               dnum = 5.0d-01
            else if ((pollut == 'HG') .and.&
            &(dnum == 0.0d0)) then
               dnum = 4.0d-01
            else if ((pollut == 'POC') .and.&
            &(dnum == 0.0d0)) then
               dnum = 1.0d-01
            end if
!              Assign The Field
            apdiam(1,isdx) = dnum

!              Set mass fraction and particle density
            aphi(1,isdx)   = 1.0d0
            apdens(1,isdx) = 1.0d0

!              Set number of particle size categories to 1
            inpd(isdx) = 1

         end if
      end do
   end if

999 return
end subroutine meth_2

subroutine sogrp
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k, ih, il
   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid
   logical :: cont, ingrp, rmark, found

!     Variable Initializations
   cont   = .false.
   ingrp  = .false.
   found  = .false.
   modnam = 'SOGRP'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc <= 3 .and. field(3) /= 'ALL') then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     READ in the Group ID and Check for Continuation Card
!*    First check for length of GRPID field (<=8)
   if ((loce(3)-locb(3)) <= 7) then
!*       Retrieve Temporary Group ID Character Substring
      tempid = field(3)
      if (numgrp == 0) then
! ---       This is the first GRPID defined; set CONT = .F.
         cont = .false.
      else
! ---       This is not the first GRPID defined; check on whether
!           this record is a continuation for the same GRPID
         do i = 1, numgrp
            if (tempid == grpid(i)) then
               cont = .true.
               exit
            end if
         end do
      end if
   else
!*       WRITE Error Message:  Group ID Field is Too Long (>8)
      call errhdl(path,modnam,'E','245',field(3)(1:12))
      go to 999
   end if

!     Increment Counters and Assign Group ID If Not a Continuation Card
   if (.not. cont) then
! ---    This is not a continuation record so this is a new GRPID
      igrp = igrp + 1
      if (igrp > ngrp) then
!           WRITE Error Message    ! Too Many Source Groups Specified
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NGRP='',I7)') ngrp
         call errhdl(path,modnam,'E','290',dummy)
!           Exit to END
         go to 999
      end if
      numgrp = numgrp + 1
      grpid(igrp) = tempid
   end if

!     Set Up The Source Group Array and Check for inclusion of BACKGROUND
   if (grpid(igrp) == 'ALL' .and. .not.cont) then
! ---    SRCGROUP ALL has been specified; assign all sources to this group
      do i = 1, numsrc
         igroup(i,igrp) = 1
      end do
! ---    Include BACKGRND concentrations as specified by the user;
!        Note the BACKGRND is NOT automatically included in source group ALL
      if (l_backgrnd) then
         if (field(4) == 'NOBACKGROUND' .or.&
         &field(4) == 'NOBACKGRND') then
! ---          User specified to NOT include BACKGRND concentrations in group ALL
            grp_back(igrp) = .false.
!              Write message indicating that BACKGRND is NOT included in group ALL
            call errhdl(path,modnam,'W','298','NOBACKGROUND')
         else if (field(4) == 'BACKGROUND' .or.&
         &field(4) == 'BACKGRND') then
! ---          Include BACKGRND concentrations in group ALL
            grp_back(igrp) = .true.
!              Write message indicating that BACKGRND IS included in group ALL
            call errhdl(path,modnam,'W','298','BACKGROUND  ')
         else if (len_trim(field(4)) == 0) then
! ---          No user-specified option, BACKGRND is NOT included in group ALL
            grp_back(igrp) = .false.
!              Write message indicating that BACKGRND is NOT included in group ALL
            call errhdl(path,modnam,'W','298','NOBACKGROUND')
         end if
      else
! ---       No BACKGRND concentrations provided, set logical to FALSE
         grp_back(igrp) = .false.
!           Check for BACKGRND or BACKGROUND with SrcGroup ALL, without
!           background data provided
         if (ifc > 3) then
            if (field(4) == 'BACKGROUND' .or.&
            &field(4) == 'BACKGRND') then
! ---             Write error message: BACKGRND specified for group ALL w/o BACKGRND keyword
               call errhdl(path,modnam,'E','323',grpid(igrp))
            else
! ---             Write error message: Additional field included with SRCGROUP ALL
               call errhdl(path,modnam,'E','203',keywrd)
            end if
         end if
      end if
   else
!        Loop Through Fields
      do i = 4, ifc
! ---       First check for inclusion of BACKGROUND in SrcGroups
         if (l_backgrnd) then
            if (field(i) == 'BACKGROUND' .or.&
            &field(i) == 'BACKGRND') then
               grp_back(igrp) = .true.
! ---             Cycle to next input field
               cycle
            end if
         else if (field(i) == 'BACKGROUND' .or.&
         &field(i) == 'BACKGRND') then
! ---          Write error message: BACKGRND specified w/o BACKGRND keyword
            call errhdl(path,modnam,'E','323',grpid(igrp))
! ---          Cycle to next input field
            cycle
         end if

! ---       Check for whether individual source IDs included on the SrcGroup
!           keyword have been defined.
         if (index(field(i),'-') == 0) then
! ---          This field is specifying an individual SrcID;
!              assign to SrcGrp as appropriate
            found = .false.
            do k = 1, numsrc
               if (field(i) == srcid(k)) then
                  found = .true.
! ---                Check for SRCID already assigned to current group
                  if (igroup(k,igrp) > 0) then
                     write(dummy,'(''G'',I4.4,'' S'',I5.5)')&
                     &min(igrp,9999), min(k,99999)

                     call errhdl(path,modnam,'W','314',dummy)
                  end if
                  igroup(k,igrp) = 1
                  exit
               end if
            end do
            if (.not. found) then
!                 WRITE Error Message:  Specified Source ID not defined
               call errhdl(path,modnam,'E','224',field(i)(1:12))
            end if
         else
! ---          Input field includes a range of SrcIDs to include in SrcGroup
            call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,&
            &lowid,higid)
!              First Check Range for Upper Value < Lower Value
            call setidg(lowid,lid1,il,lid2)
            call setidg(higid,hid1,ih,hid2)
            if ((hid1<lid1) .or. (ih<il) .or.&
            &(hid2<lid2)) then
! ---             WRITE Error Message:  Invalid Range,  Upper < Lower
               call errhdl(path,modnam,'E','203','SRCRANGE')
!                 Cycle to next input field
               cycle
            end if
! ---          Loop through sources and assign to SrcGroup as appropriate
!              based on Source Range
            do k = 1, numsrc
               call asngrp(srcid(k),lowid,higid,ingrp)
               if (ingrp) then
                  igroup(k,igrp) = 1
               end if
            end do
         end if
      end do
   end if

999 return
end subroutine sogrp

subroutine asngrp(inid,lowid,higid,ingrp)
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
   character (len=12) :: lowid, higid, inid, iid1, lid1, hid1,&
   &iid2, lid2, hid2
!     JAT D065 8/9/21 PATH SET BUT NOT USED
!      CHARACTER PATH*2, MODNAM*12
   character :: modnam*12
   integer :: in, il, ih
   logical :: ingrp

!     Variable Initializations
   modnam = 'ASNGRP'
!     JAT D065 8/9/21 PATH SET BUT NOT USED
!      PATH   = 'SO'
   ingrp  = .false.

!     Extract The Character Field And Numerical Field
   call setidg(inid,iid1,in,iid2)
   call setidg(lowid,lid1,il,lid2)
   call setidg(higid,hid1,ih,hid2)

!     Do Comparisons of Character and Numeric Fields, All Must Satisfy Ranges
   if ((iid1>=lid1 .and. iid1<=hid1) .and.&
   &(in>=il .and. in<=ih) .and.&
   &(iid2>=lid2 .and. iid2<=hid2)) then
      ingrp = .true.
   end if

   return
end subroutine asngrp

subroutine setidg(inid,idchr1,idnum,idchr2)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ii, istr, idnum
   character (len=12) :: inid, idchr1, idchr2
   character (len= 1) :: chki
   character (len=ilen_fld) :: numid
   logical :: hit

!     Variable Initializations
   modnam = 'SETIDG'
   i  = 12
   ii = 0
   numid  = ' '
   idchr1 = ' '
   idchr2 = ' '
   idnum  = 0
   hit    = .false.

!     Find The Length of the Input Field, II (<= 12)
   do while (.not.hit .and. i>=1)
      chki = inid(i:i)
      if (chki /= ' ') then
         ii = i
         hit = .true.
      end if
      i = i - 1
   end do

!     Divide the Input Id into 3 parts (char1, int, and char2)
   i = 1
   istr = i
   chki = inid(i:i)
!     Get first character part
   do while (chki < '0' .or. chki > '9')
      idchr1 = inid(istr:i)
      i = i + 1
      if (i > ii) then
         go to 20
      else
         chki = inid(i:i)
      end if
   end do

!     Get integer part
   istr = i
   do while (chki >= '0' .and. chki <= '9')
      numid = inid(istr:i)
      i = i + 1
      if (i > ii) then
         go to 20
      else
         chki = inid(i:i)
      end if
   end do

!     Get second character part
   istr = i
   do while (i <= ii)
      idchr2 = inid(istr:i)
      i = i + 1
      if (i > ii) then
         go to 20
      else
         chki = inid(i:i)
      end if
   end do

20 continue

!     Convert Numeric Part to Integer Variable
   read(numid,'(I8)') idnum

   return
end subroutine setidg

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
   use main1

   double precision, parameter :: a1=1.257d0, a2=0.4d0, a3=0.55d0,&
   &xmfp=6.5d-6,&
   &vcon=1.81d-4, vair=0.15d0,&
   &rhoair=1.2d-3
! unused: xk=1.38D-16
! unused: gcgs=981.0D0, tair=293.15D0
   double precision :: diamcm
   integer          :: i, j, n
!
! ***
   if(debug)then
      write(dbgunt,*)
      write(dbgunt,*)'SUBR. VDP1 -- INPUTS'
      write(dbgunt,*)
      do i=1,numsrc
         write(dbgunt,*)'SOURCE          = ',i
         write(dbgunt,*)'INPD            = ',inpd(i)
         if (inpd(i) > 0) then
            write(dbgunt,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
            write(dbgunt,*)'APHI            = ',(aphi(n,i),n=1,inpd(i))
            write(dbgunt,*)'APDENS(g/cm**3) = ',(apdens(n,i),n=1,inpd(i))
         end if
         write(dbgunt,*)
      end do
   endif
! ***
!
! --- Convert viscosity of air (at 20 deg C) from cm**2/s to m**2/s
   vairms=1.0d-4*vair
!
! --- Define phoretic effects term (m/s)
   vdphor=0.0001d0
!
!
! --  LOOP over sources
   do j=1,numsrc
!
      if(inpd(j) <= 0 .and. .not.luservd) then
!
         if (pdiff(j)/=0.0d0 .and. pdiffw(j)/=0.0d0&
         &.and. rcli(j)/=0.0d0 .and. henry(j)/=0.0d0) then
! ---          GAS DEPOSITION
!
! ---          Convert Pollutant diffusivity (cm**2/s to m**2/s)
            pdiff(j) =pdiff(j)*1.0d-4
            pdiffw(j)=pdiffw(j)*1.0d-4
!
! ---          Convert rcli resistance from s/cm to s/m
            rcli(j)=rcli(j)*1.0d2
!
! ***
            if(debug)then
               write(dbgunt,*)
               write(dbgunt,*)'SUBR. VDP1 -- OUTPUT for GASES'
               write(dbgunt,*)'PDIFF (m**2/s)  = ',&
               &(pdiff(n),n=1,numsrc)
               write(dbgunt,*)'PDIFFW(m**2/s)  = ',&
               &(pdiffw(n),n=1,numsrc)
               write(dbgunt,*)'RCLI(s/m)       = ',&
               &(rcli(n),n=1,numsrc)
               write(dbgunt,*)'ZRDEP (m)       = ',zrdep
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
            diamcm=1.0d-4*apdiam(i,j)
            scf(i)=1.0d0+2.0d0*xmfp*&
            &(a1+a2*dexp(-a3*diamcm/xmfp))/diamcm
!
! ---          Gravitational settling velocity (m/s)
! ---          (rhoair is approx. density of air -- 1.2e-3 g/cm**3)
!rwb           Use PARAMETER G = 9.80616 m/s^2 instead of gcgs = 981 cm/s^2
!rwb           avgrav(i,j)=0.01*(apdens(i,j)-rhoair)*gcgs*diamcm**2
! ---          Set lower limit of 0.0 on density difference
            avgrav(i,j)= max(0.0d0,(apdens(i,j)-rhoair))*g*diamcm**2&
            &*scf(i)/(18.0d0*vcon)
!
! ---          Stopping times
!rwb           Use PARAMETER G = 9.80616 m/s^2 instead of gcgs = 981 cm/s^2
!rwb           atstop(i,j)=avgrav(i,j)/(0.01*gcgs)
            atstop(i,j)=avgrav(i,j)/g
         end do
! ***
         if(debug)then
            write(dbgunt,*)
            write(dbgunt,*)'SUBR. VDP1 -- OUTPUT for PARTICLES'
            write(dbgunt,*)
            do i=1,numsrc
               write(dbgunt,*)'SOURCE          = ',i
               write(dbgunt,*)'AVGRAV (m/s)    = ',&
               &(avgrav(n,i),n=1,inpd(i))
               write(dbgunt,*)'ATSTOP (s)      = ',&
               &(atstop(n,i),n=1,inpd(i))
               write(dbgunt,*)'VAIRMS (m**2/s) = ',vairms
               write(dbgunt,*)'ZRDEP (m)       = ',zrdep
               write(dbgunt,*)'VDPHOR (m/s)    = ',vdphor
               write(dbgunt,*)
            end do
         endif
! ***
!
      endif
   end do
!     end LOOP over source

   return
end subroutine vdp1

subroutine hremis
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
   use main1
   use buoyant_line, only: l_blhourly   ! Multiple_BuoyLines_D41_Wood
   implicit none
   character :: modnam*12

   integer :: i, k, ih, il

   logical :: fopen, ingrp
   logical :: rmark

   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid

!     Variable Initializations
   modnam = 'HREMIS'

   fopen  = .false.

   if (ifc >= 4) then
!        Retrieve Hourly Emissions Data Filename as Character Substring to
!        Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         hrfile = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  HRFILE Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         go to 999
      end if

!        Open Hourly Emissions Data File If Not Already Open
      inquire (file=hrfile,opened=fopen)

      if (.not. fopen) then
!           Open Hourly Emissions Data File If Not Already Open
!           Open with ACTION='READ' to prevent overwrite and allow multiple access
         inquire (unit=ihremi,opened=fopen)
         if (.not. fopen) then
            open(unit=ihremi,err=998,file=hrfile,iostat=ioerrn,&
            &action='READ',status='OLD')
         end if
      end if

   else
!        WRITE Error Message         ! Not Enough Parameters Specified
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

   tempid = field(4)

!     Set Up The Source Group Array
   if (tempid == 'ALL') then
      do k = 1, numsrc
         qflag(k) = 'HOURLY'

!        Multiple_BuoyLines_D41_Wood: Begin
!           Set the flag indicating that there is one
!            (or more) buoyant line sources in the hourly emissions file
         if (srctyp(k) == 'BUOYLINE' .and. .not. l_blhourly) then
            l_blhourly = .true.
         end if
!        Multiple_BuoyLines_D41_Wood: End

      end do
   else
!        Loop Through Fields
      do i = 4, ifc
         call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,&
         &lowid,higid)
!           First Check Range for Upper Value < Lower Value
         call setidg(lowid,lid1,il,lid2)
         call setidg(higid,hid1,ih,hid2)
         if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            call errhdl(path,modnam,'E','203','SRCRANGE')
            cycle
         end if
         do k = 1, numsrc
            call asngrp(srcid(k),lowid,higid,ingrp)
            if (ingrp) then
               qflag(k) = 'HOURLY'
!              Multiple_BuoyLines_D41_Wood: Begin
               if ((srctyp(k) == 'BUOYLINE' .and.&
               &.not. l_blhourly)) then
                  l_blhourly = .true.
               end if
!              Multiple_BuoyLines_D41_Wood: End
            end if
         end do
      end do
   end if

   go to 999

!     Process Error Messages
998 call errhdl(path,modnam,'E','500',keywrd)

999 return
end subroutine hremis

subroutine urbans
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
   use main1
   implicit none
   character :: modnam*12

   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid
   integer :: i, il, ih, k, istr
   logical :: ingrp, rmark, found

!     Variable Initializations
   modnam = 'URBANS'
   found = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   end if

!     Check for 'URBANSRC ALL' option identified in PRESET
   if (l_urban_all) then
      if (ifc == 3 .and. field(3) == 'ALL') then
         urbsrc(:) = 'Y'
         iurbgrp(:,:) = 1
         go to 999
      else
!           WRITE Error Message:  URBANSRC ALL
         call errhdl(path,modnam,'E','279','URBANSRC ALL')
         go to 999
      end if

   else if (l_multurb) then
!        Multiple Urban Areas
!        READ in the Group ID and Check for Continuation Card
      tempid = field(3)
      do i = 1, numurb
         if (tempid == urbid(i)) then
            found = .true.
            iurb = i
         end if
      end do
      if (.not. found) then
!           WRITE Error Message:  Urban ID not defined
         call errhdl(path,modnam,'E','301',tempid)
         go to 999
      end if

!        Specify field index to start for Source IDs
      istr = 4

   else
!        Single Urban Area - No URBAN ID
      iurb = 1

!        Specify field index to start for Source IDs
      istr = 3

   end if

!     Loop Through Fields
   do i = istr, ifc
      if (index(field(i),'-') == 0) then
         found = .false.
         do k = 1, numsrc
            if (srcid(k) == field(i)) then
               found = .true.
               urbsrc(k) = 'Y'
               iurbgrp(k,iurb) = 1
            end if
         end do
         if (.not.found) then
!              WRITE Error Message:  SRCID not found
            call errhdl(path,modnam,'E','300',keywrd)
            cycle
         end if

      else

         call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,lowid,&
         &higid)
!           First Check Range for Upper Value < Lower Value
         call setidg(lowid,lid1,il,lid2)
         call setidg(higid,hid1,ih,hid2)
         if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            call errhdl(path,modnam,'E','203','SRCRANGE')
            cycle
         end if
         do k = 1, numsrc
            call asngrp(srcid(k),lowid,higid,ingrp)
            if (ingrp) then
               urbsrc(k) = 'Y'
               iurbgrp(k,iurb) = 1
            end if
         end do

      end if
   end do

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

999 return
end subroutine urbans

subroutine hbpsource
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
   use main1
   implicit none
   character :: modnam*12

   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid
   integer :: i, il, ih, k, istr
   logical :: ingrp, rmark, found

!     Variable Initializations
   modnam = 'HBPSOURCE'
   found = .false.


!     Check The Number Of The Fields
   if (ifc < 3) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   end if

!     Check for 'HBPSRCID ALL' option identified in PRESET
   if (l_hbp_all) then
      if (ifc == 3 .and. field(3) == 'ALL') then
         do k = 1, numsrc
            if (srctyp(k)(1:5) == 'POINT') then
               hbpsrc(k) = 'Y'
               nhbp = nhbp + 1
            else
               hbpsrc(k) = 'N'
!                    WRITE Informational Message:
!                    HBP is only relevant to POINT-type sources
               call errhdl(path,modnam,'I','740',srcid(k))
            endif
         enddo
         go to 999
      end if
   else
!        Specify field index to start for Source IDs
      istr = 3
   end if

!     Loop Through Fields
   do i = istr, ifc
      if (index(field(i),'-') == 0) then
         found = .false.
         do k = 1, numsrc
            if (srcid(k) == field(i)) then
               if (srctyp(k)(1:5) == 'POINT') then
                  found = .true.
                  hbpsrc(k) = 'Y'
                  nhbp = nhbp + 1
               else
                  found = .true.
                  hbpsrc(k) = 'N'
!                        WRITE Informational Message:
!                        HBP is only relevant to POINT-type sources
                  call errhdl(path,modnam,'I','740',srcid(k))
               endif
            end if
         end do
         if (.not.found) then
!              WRITE Error Message:  SRCID not found
            call errhdl(path,modnam,'E','300',keywrd)
            cycle
         end if

      else

         call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,lowid,&
         &higid)
!           First Check Range for Upper Value < Lower Value
         call setidg(lowid,lid1,il,lid2)
         call setidg(higid,hid1,ih,hid2)
         if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            call errhdl(path,modnam,'E','203','SRCRANGE')
            cycle
         end if
         do k = 1, numsrc
            call asngrp(srcid(k),lowid,higid,ingrp)
            if (ingrp) then
               hbpsrc(k) = 'Y'
            end if
         end do

      end if
   end do

999 return
end subroutine hbpsource


subroutine no2rat
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ih, il, isdx
   character (len=12) :: lid, hid, lid1, lid2, hid1, hid2
   character (len=ilen_fld) :: soid
   logical :: found, ingrp, rmark

!     Variable Initializations
   found  = .false.
   ingrp  = .false.
   modnam = 'NO2RAT'

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 4) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Get The Source ID(s)
   soid = field(3)
   call fsplit(path,keywrd,soid,ilen_fld,'-',rmark,lid,hid)

   if (lid == hid) then
!        Search For The Index
      call sindex(srcid,nsrc,soid,isdx,found)
      if (found) then
!           Read NO2/NOX Ratio and Convert to Real
         call stodbl(field(4),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            go to 999
         end if
         if (dnum < 0.0d0 .or. dnum > 1.0d0) then
!              WRITE Error Message: NO2_Ratio Out-of-Range
            call errhdl(path,modnam,'E','336',srcid(isdx))
         end if
!           Assign The Field
         ano2_ratio(isdx) = dnum
      else
!           WRITE Error Message     ! Source Location Has Not Been Identified
         call errhdl(path,modnam,'E','300',keywrd)
      end if
   else
!        First Check Range for Upper Value < Lower Value
      call setidg(lid,lid1,il,lid2)
      call setidg(hid,hid1,ih,hid2)
      if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!           WRITE Error Message:  Invalid Range,  Upper < Lower
         call errhdl(path,modnam,'E','203','SRCRANGE')
         go to 999
      end if
      do i = 1, numsrc
!           See Whether It's In The Group
         call asngrp(srcid(i),lid,hid,ingrp)
         if (ingrp) then
!              Read NO2/NOX Ratio and Convert to Real
            call stodbl(field(4),ilen_fld,dnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 999
            end if
            if (dnum < 0.0d0 .or. dnum > 1.0d0) then
!                 WRITE Error Message: NO2_Ratio Out-of-Range
               call errhdl(path,modnam,'E','336',srcid(i))
            end if
!              Assign The Field
            ano2_ratio(i) = dnum
         end if
      end do
   end if

999 return
end subroutine no2rat

subroutine olmgrp
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k, ih, il
   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2
   character (len= 8) :: tempid
   logical :: cont, ingrp, rmark, found
! Unused INTEGER :: J

!     Variable Initializations
   cont   = .false.
   ingrp  = .false.
   found  = .false.
   modnam = 'OLMGRP'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc <= 3 .and. field(3) /= 'ALL') then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     READ in the OLMGROUP ID and Check for Continuation Card
!*    First check for length of OLMID field (<=8)
   if ((loce(3)-locb(3)) <= 7) then
!*       Retrieve Temporary Group ID Character Substring
      tempid = field(3)
      if (numolm == 0) then
         cont = .false.
      else
! ---       This is not the first OLMGroup defined; check on whether
!           this record is a continuation for the same OLMGroup
         do i = 1, numolm
            if (tempid == olmid(i)) then
               cont = .true.
               exit
            end if
         end do
      end if
   else
!*       WRITE Error Message:  OLMGroup ID Field is Too Long
      call errhdl(path,modnam,'E','232',field(3)(1:12))
      go to 999
   end if

!     Increment Counters and Assign Group ID If Not a Continuation Card
   if (.not. cont) then
! ---    This is not a continuation record so this is a new GRPID
      iolm = iolm + 1
      if (iolm > nolm) then
!           WRITE Error Message    ! Too Many OLM Groups Specified
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NOLM='',I7)') nolm
         call errhdl(path,modnam,'E','290',dummy)
!           Exit to END
         go to 999
      end if
      numolm = numolm + 1
      olmid(iolm) = tempid
   end if

!     Set Up The OLM Group Array
   if (olmid(iolm) == 'ALL' .and. .not.cont) then
! ---    Check for whether all sources have been defined yet,
!        based on NSRC determined during PRESET vs. NUMSRC
!        determined during normal SETUP.
      if (numsrc < nsrc) then
!           Issue fatal error message; OLMGROUP ALL out of order
         call errhdl(path,modnam,'E','140','OLMGROUP ALL')
         go to 999
      else
! ---       OLMGROUP ALL has been specified; assign all sources to this OLMgroup
         do i = 1, numsrc
            igrp_olm(i,iolm) = 1
            l_olmgrp(i) = .true.
         end do
      end if
   else
!        Loop Through Fields
      do i = 4, ifc
! ---       First check for whether individual source IDs included on the
!           OLMGROUP keyword have been defined.
         if (index(field(i),'-') == 0) then
! ---          This field is specifying an individual SrcID;
!              assign to OLMGrp as appropriate
            found = .false.
            do k = 1, numsrc
               if (field(i) == srcid(k)) then
                  found = .true.
! ---                Assign logical indicating that this SRCID is in an OLMGRP
                  l_olmgrp(k) = .true.
! ---                Assign flag indicating which OLMGRP this SRCID is included in
                  igrp_olm(k,iolm) = 1
                  exit
               end if
            end do
            if (.not. found) then
!                 WRITE Error Message:  Specified Source ID not defined
               call errhdl(path,modnam,'E','225',field(i)(1:12))
            end if
         else
! ---          Input field includes a range of SrcIDs to include in OLMGrp
            call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,&
            &lowid,higid)
!              First Check Range for Upper Value < Lower Value
            call setidg(lowid,lid1,il,lid2)
            call setidg(higid,hid1,ih,hid2)
            if ((hid1<lid1) .or. (ih<il) .or.&
            &(hid2<lid2)) then
!                 WRITE Error Message:  Invalid Range,  Upper < Lower
               call errhdl(path,modnam,'E','203','SRCRANGE')
               cycle
            end if
            do k = 1, numsrc
               call asngrp(srcid(k),lowid,higid,ingrp)
               if (ingrp) then
! ---                Assign logical indicating that this SRCID is in an OLMGRP
                  l_olmgrp(k) = .true.
! ---                Assign flag indicating which OLMGRP this SRCID is included in
                  igrp_olm(k,iolm) = 1
               end if
            end do
         end if
      end do
   end if

999 return
end subroutine olmgrp

subroutine psdgrp
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k, ih, il
   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid
   logical :: cont, ingrp, rmark, found

!     Variable Initializations
   cont   = .false.
   ingrp  = .false.
   found  = .false.
   modnam = 'PSDGRP'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
!        Note:  The ALL parameter is not valid for the PSDCREDIT option
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        Error Message: Not Enough Parameters
!        Note:  The ALL parameter is not valid for the PSDCREDIT option
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     READ in the PSDGROUP ID and Check for Continuation Card
!*    First check for length of PSDID field (<=8)
   if ((loce(3)-locb(3)) <= 7) then
!*       Retrieve Temporary Group ID Character Substring
      tempid = field(3)
      if (numpsd == 0) then
! ---       This is the first PSDID defined; set CONT = .F.
         cont = .false.
      else
! ---       This is not the first PSDID defined; check on whether
!           this record is a continuation for the same PSDID
         do i = 1, numpsd
            if (tempid == psdid(i)) then
               cont = .true.
               exit
            end if
         end do
      end if
   else
!*       WRITE Error Message:  PSDGROUP ID Field is Too Long
      call errhdl(path,modnam,'E','253',field(3)(1:12))
      go to 999
   end if

!     Increment Counters and Assign Group ID If Not a Continuation Card
   if (.not. cont) then
! ---    This is not a continuation record so this is a new PSDID
      ipsd = ipsd + 1
      if (ipsd > npsd) then
!           WRITE Error Message    ! Too Many PSD Groups Specified
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NPSD='',I7)') npsd
         call errhdl(path,modnam,'E','290',dummy)
!           Exit to END
         go to 999
      end if
      numpsd = numpsd + 1
      psdid(ipsd) = tempid
   end if

!     Set Up The Source Group Array and the PSD source type
!       Only INCRCONS (increment consuming), RETRBASE (retired baseline),
!       and NONRBASE (non-retired basleine) are allowable; any other
!       PSD group name, including ALL, is not allowed
   if( psdid(ipsd) == 'INCRCONS' .or.&
   &psdid(ipsd) == 'RETRBASE' .or.&
   &psdid(ipsd) == 'NONRBASE' )then

!        Valid PSD Source Group; Loop Through Fields
      do i = 4, ifc
! ---       First check for whether individual source IDs included on the
!           PSDGROUP keyword have been defined (based on no '-' in FIELD).
         if (index(field(i),'-') == 0) then
            found = .false.
            do k = 1, numsrc
               if (field(i) == srcid(k)) then
                  found = .true.
                  igrp_psd(k,ipsd) = 1
! ---                Assign PSDSRCTYP flag to indicate the type of source
                  if( psdid(ipsd) == 'INCRCONS' )then
                     psdsrctyp(k) = 'IC'
                  else if( psdid(ipsd) == 'NONRBASE' )then
                     psdsrctyp(k) = 'NB'
                  else if( psdid(ipsd) == 'RETRBASE' )then
                     psdsrctyp(k) = 'RB'
                  end if
               end if
            end do
            if (.not. found) then
!                 WRITE Error Message:  Specified Source ID not defined
               call errhdl(path,modnam,'E','226',field(i)(1:12))
            end if
         else
! ---          Input field includes a range of SrcIDs to include in PSDGrp
            call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,&
            &lowid,higid)
!              First Check Range for Upper Value < Lower Value
            call setidg(lowid,lid1,il,lid2)
            call setidg(higid,hid1,ih,hid2)
            if ((hid1<lid1) .or. (ih<il) .or.&
            &(hid2<lid2)) then
!                 WRITE Error Message:  Invalid Range,  Upper < Lower
               call errhdl(path,modnam,'E','203','PSDRANGE')
!                 Cycle to next input field
               cycle
            end if
! ---          Loop through sources and assign to PSDGroup as appropriate
!              based on Source Range
            do k = 1, numsrc
               call asngrp(srcid(k),lowid,higid,ingrp)
               if (ingrp) then
                  igrp_psd(k,ipsd) = 1
                  l_psdgrp(k) = .true.
! ---                Assign PSDSRCTYP flag to indicate the type of source
                  if( psdid(ipsd) == 'INCRCONS' )then
                     psdsrctyp(k) = 'IC'
                  else if( psdid(ipsd) == 'NONRBASE' )then
                     psdsrctyp(k) = 'NB'
                  else if( psdid(ipsd) == 'RETRBASE' )then
                     psdsrctyp(k) = 'RB'
                  end if
               end if
            end do
         end if
      end do
   else
!        Error Message: Not a valid PSD source group
      call errhdl(path,modnam,'E','287',keywrd)
      go to 999
   end if

999 return
end subroutine psdgrp

! Multiple_BuoyLines_D41_Wood
subroutine blpgrp
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
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

! JAT 06/22/21 D065
! REMOVE K AS UNUSED VARIABLE
!      INTEGER :: I, K, IH, IL, IGRPNUM, JJ, KK
   integer :: i, ih, il, jj, kk
   integer, save :: igrpnum
   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid
   logical :: cont, ingrp, rmark, found, blpavggrp_found

!     Variable Initializations
! --- Flag indicating if the BLPGROUP record is a continuation record
   cont   = .false.
! --- Flag indicating whether a source in a range of sources is in a BLPGROUP
   ingrp  = .false.
! --- Flag indicating if the SOURCE ID has been defined (SO LOCATION)
   found  = .false.
! --- Flag indicating if the BLPGROUP ID was defined on a BLPINPUT record
   blpavggrp_found = .false.

   modnam = 'BLPGRP'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc <= 3 .and. field(3) /= 'ALL') then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     READ in the BLPGROUP ID and Check for Continuation Card
!*    First check for length of BLPID field (<=8)
   if ((loce(3)-locb(3)) <= 7) then
!*       Retrieve Temporary Group ID Character Substring
      tempid = field(3)
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
      do i = 1, numblgrps
         if (tempid == bl_grpid(i)) then
            blpavggrp_found = .true.                 ! BL_Source_Limit_D088: Wood
            cont = .true.
            exit
         end if
      end do
!         END IF   !NUMBLGRPS .EQ. 0
   else
!*       WRITE Error Message:  BLPGROUP ID Field is Too Long
      call errhdl(path,modnam,'E','245',field(3)(1:12))
      go to 999
   end if

!     Increment Counters and Assign Group ID If Not a Continuation Card
   if (.not. cont) then
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

      do kk = 1,numblavginp
         if (tempid == blavginp_grpid(kk)) then
            igrpnum = kk
            blpavggrp_found = .true.
            numblgrps = numblgrps + 1
            bl_grpid(igrpnum) = tempid
            exit
         end if
      end do

!        IF (IOLM .GT. NOLM) THEN
!           WRITE Error Message    ! Too Many OLM Groups Specified
!           This shouldn't occur since limits are dynamically allocated
!           WRITE(DUMMY,'(''NOLM='',I7)') NOLM
!           CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
!           Exit to END
!           GO TO 999
!        END IF
!        NUMOLM = NUMOLM + 1      no NUMBLP equivalent in this routine
   end if

   if (blpavggrp_found) then
! ---    The BLP group ID, as defined on a BLPINPUT record, has been found

!        Set Up The BLP Group Array
      if (bl_grpid(igrpnum) == 'ALL' .and. .not.cont) then
! ---       BLPGROUP set to ALL
!           Assign all sources to this BLPGROUP
         do kk = 1, numsrc
            if (srctyp(kk) == 'BUOYLINE') then
               igrp_blp(kk,igrpnum) = 1
!                 L_BLPGRP(kk) = .TRUE.
            endif
         end do
      else
!           BLPGROUP ID is not 'ALL' - loop through fields
         do jj = 4, ifc
! ---          First check for whether individual source IDs included on
!              the BLPGROUP keyword have been defined.
            if (index(field(jj),'-') == 0) then
! ---             This field is specifying an individual SrcID;
!                 assign to BLPGROUP as appropriate
               found = .false.
               do kk = 1, numsrc
                  if (field(jj) == srcid(kk)) then
                     if (srctyp(kk) == 'BUOYLINE') then
                        found = .true.
! ---                      Assign flag indicating which BLPGROUP this SRCID is included in
                        igrp_blp(kk,igrpnum) = 1
! ---                      Assign logical indicating that this SRCID is in a BLPGROUP
!                           L_BLPGRP(kk) = .TRUE.
                        exit
                     else                 ! non-BL line in a BLPGROUP
!                          WRITE Error Message:  Specified Source ID is not a BL
                        call errhdl(path,modnam,'E','255',&
                        &field(jj)(1:12))
                     end if
                  end if
               end do
               if (.not. found) then
!                    WRITE Error Message:  Specified Source ID not defined
                  call errhdl(path,modnam,'E','254',field(jj)(1:12))
               end if
            else
! ---             Input field includes a range of SrcIDs on BLPGROUP
               call fsplit(path,keywrd,field(jj),ilen_fld,'-',&
               &rmark,lowid,higid)
!                 First Check Range for Upper Value < Lower Value
               call setidg(lowid,lid1,il,lid2)
               call setidg(higid,hid1,ih,hid2)
               if ((hid1<lid1) .or. (ih<il) .or.&
               &(hid2<lid2)) then
!                    WRITE Error Message:  Invalid Range,  Upper < Lower
                  call errhdl(path,modnam,'E','203','SRCRANGE')
                  cycle
               end if

               do kk = 1, numsrc
                  if (srctyp(kk) == 'BUOYLINE') then
                     call asngrp(srcid(kk),lowid,higid,ingrp)
                     if (ingrp) then
! ---                      Assign flag indicating which BLPGROUP this SRCID
!                          is included in
                        igrp_blp(kk,igrpnum) = 1
! ---                      Assign logical indicating that this SRCID is
!                          in a BLPGROUP
!                           L_BLPGRP(kk) = .TRUE.
                     end if
                  end if      ! SRCTYP = BUOYLINE
               end do         ! loop over sources
            end if            ! search for hyphen in BLPGROUP
         end do               ! loop through fields on BLPGROUP record
      end if                  ! BLPGROUP id ('ALL' or a group name)
   else
! ---    The BLP group ID, as defined on a BLPINPUT record, has NOT been found
!          or there are no BLPINPUT records
      write(dummy,'(A12)') tempid
      call errhdl(path,modnam,'E','502',dummy)
   end if

999 return
end subroutine blpgrp

subroutine back_grnd
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j
   integer :: NumInt, NumReal
   logical :: fopen
! Unused: CHARACTER (LEN=12) :: LID, LID1, LID2, HID, HID1, HID2
! Unused: CHARACTER (LEN=ILEN_FLD) :: SOID
! Unused: INTEGER :: IH, IL, ISDX
! Unused: LOGICAL INGRP, RMARK

!     Variable Initializations
   modnam = 'BACK_GRND'

! --- Assign logical variable indicating that background concentrations
!     are specified
   l_backgrnd = .true.
   fopen  = .false.
   NumInt  = 0
   NumReal = 0

! --- Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_BGSector) then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc == 3) then
!           Error Message: No Numerical Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc < 4) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      end if
! ---    Check for SECT ID in field 3 in case BGSECTOR keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without BGSECTOR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified BACKGRND option and
!        assign the option to BFLAG variable
      ibgsect = 1
      i = 3

! ---    Check for inconsistent non-HOURLY BACKGRND options for the same sector
      if (ibgmax(ibgsect) >= 1 .and.&
      &bflag(ibgsect) /= field(i) .and.&
      &field(i) /= 'HOURLY') then
!           Issue error message for inconsistent options
         call errhdl(path,modnam,'E','165',field(i))
      else
! ---       Assign BACKGRND option to BFLAG for non-HOURLY data and assign
!           logical variables; L_BGFile(BGSect) for hourly values by sector
!           and L_BGHourly for hourly values in general
         if (field(i) /= 'HOURLY') then
            bflag(ibgsect) = field(i)
            L_BGValues(ibgsect) = .true.
         else if ( L_BGFile(ibgsect) ) then
! ---          HOURLY BG File already specified for this sector
!              Issue error message for inconsistent options
            write(dummy,'(''BGSECT'',I1)') ibgsect
            call errhdl(path,modnam,'E','168',dummy)
         else
!              Assign logical variables
            L_BGHourly        = .true.
            L_BGFile(ibgsect) = .true.
         end if
      end if

   else
! ---    Process BGSECTOR options
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc == 4) then
         if (field(3)(1:4) /= 'SECT') then
!              Error Message: Invalid sector field
            call errhdl(path,modnam,'E','203','BGSECTOR ID')
            go to 999
         else
!              Error Message: No Numerical Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         end if
      else if (ifc < 5) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      end if
! ---    Determine user-specified sector
      if (field(3) == 'SECT1') then
         ibgsect = 1
      else if (field(3) == 'SECT2' .and. NUMBGSects >= 2) then
         ibgsect = 2
      else if (field(3) == 'SECT3' .and. NUMBGSects >= 3) then
         ibgsect = 3
      else if (field(3) == 'SECT4' .and. NUMBGSects >= 4) then
         ibgsect = 4
      else if (field(3) == 'SECT5' .and. NUMBGSects >= 5) then
         ibgsect = 5
      else if (field(3) == 'SECT6' .and. NUMBGSects == 6) then
         ibgsect = 6
      else
!           Error Message: Invalid sector field
         call errhdl(path,modnam,'E','203','BGSECTOR ID')
         go to 999
      end if
! ---    Set field index for the user-specified BACKGRND option and
!        assign the option to BFLAG variable
      i = 4

! ---    Check for inconsistent non-HOURLY BACKGRND options for the same sector
      if (ibgmax(ibgsect) >= 1 .and.&
      &bflag(ibgsect) /= field(i) .and.&
      &field(i) /= 'HOURLY') then
!           Issue error message for inconsistent options
         if (len_trim(field(i)) > 6) then
            write(dummy,'(''SEC'',I1,1X,A:)') ibgsect,&
            &field(i)(1:len_trim(field(i)))
            call errhdl(path,modnam,'E','165',dummy)
         else
            write(dummy,'(''SECT'',I1,1X,A:)') ibgsect,&
            &field(i)(1:len_trim(field(i)))
            call errhdl(path,modnam,'E','165',dummy)
         end if
      else
! ---       Assign BACKGRND option to BFLAG for non-HOURLY data and assign
!           logical variables; L_BGFile(BGSect) for hourly values by sector
!           and L_BGHourly for availability of hourly values in general
         if (field(i) /= 'HOURLY') then
            bflag(ibgsect) = field(i)
            L_BGValues(ibgsect) = .true.
         else if ( L_BGFile(ibgsect) ) then
! ---          HOURLY BG File already specified for this sector
!              Issue error message for inconsistent options
            write(dummy,'(''BGSECT'',I1)') ibgsect
            call errhdl(path,modnam,'E','168',dummy)
         else
            L_BGHourly        = .true.
            L_BGFile(ibgsect) = .true.
         end if
      end if
   end if

! --- First check for hourly background file
   if (L_BGFile(ibgsect) .and. field(i) == 'HOURLY') then
! ---    Hourly background concentration option selected;

      if ((loce(i+1)-locb(i+1)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         BKGRND_File(ibgsect) = runst1(locb(i+1):loce(i+1))
      else
!           WRITE Error Message:  BKGRND_File Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         go to 999
      end if

! ---    Assign file unit for this BKG file and Open The BACKGRND Input File
!        Open with ACTION='READ' to prevent overwrite and multiple access
      ibgunt(ibgsect) = 2000 + ibgsect

! ---    Open Hourly Emissions Data File If Not Already Open,
!        first check whether file is open by filename
      inquire (file=BKGRND_File(ibgsect),opened=fopen)

      if (.not. fopen) then
! ---       Open Hourly BACKGRND Data File If Not Already Open
!           Open with ACTION='READ' to prevent overwrite and allow multiple access;
!           First check for whether file is open by file unit
         inquire (unit=ibgunt(ibgsect),opened=fopen)
         if (.not. fopen) then
            open(unit=ibgunt(ibgsect),err=998,&
            &file=BKGRND_File(ibgsect),iostat=ioerrn,&
            &action='READ',status='OLD')
         else
!              Hourly BACKGRND File is Already Opened With Different Filename
            call errhdl(path,modnam,'E','501',keywrd)
            go to 999
         end if
      else
!           File with same name as Hourly BACKGRND file is already opened,
!           possible conflict with another file type
         call errhdl(path,modnam,'E','501',keywrd)
         go to 999
      end if

! ---    Check for optional hourly BACKGRND file format
      if (ifc == i+2) then
!           Check for Format String > ILEN_FLD PARAMETER
         if ((loce(i+2)-locb(i+2)) <= (ilen_fld - 1)) then
!              Retrieve Met Format as Char. Substring
            bgform(ibgsect) = runst1(locb(i+2):loce(i+2))

! ---          First check for user input of "FREE" for the formaat,
!              using FIELD array which has been converted to upper case
            if (field(i+2) == 'FREE') then
               bgform(ibgsect) = 'FREE'
            else
! ---             Check for correct format specifiers for BACKGRND file;
!                 should be 4 integers for date variables and 1 real for
!                 background concentration; allow for 1 to 4 integers since
!                 format statement may include 4I2, and also allow for
!                 either F, E, or D format for the data variable.
               do j = 1, len_trim(bgform(ibgsect))
                  if (bgform(ibgsect)(j:j)=='I' .or.&
                  &bgform(ibgsect)(j:j)=='i') then
                     NumInt  = NumInt  + 1
                  else if (bgform(ibgsect)(j:j)=='F' .or.&
                  &bgform(ibgsect)(j:j)=='f') then
                     NumReal = NumReal + 1
                  else if (bgform(ibgsect)(j:j)=='E' .or.&
                  &bgform(ibgsect)(j:j)=='e') then
                     NumReal = NumReal + 1
                  else if (bgform(ibgsect)(j:j)=='D' .or.&
                  &bgform(ibgsect)(j:j)=='d') then
                     NumReal = NumReal + 1
                  else if (bgform(ibgsect)(j:j)=="'" .or.&
                  &bgform(ibgsect)(j:j)=='"' .or.&
                  &bgform(ibgsect)(j:j)=='-' .or.&
                  &bgform(ibgsect)(j:j)==';') then
!                       Check for invalid characters in BGFORM and issue
!                       Warning Message for dash, semicolon, single or
!                       double quotes embedded within the FORMAT.
                     write(dummy,'(''InvalidChr '',A:)')&
                     &bgform(ibgsect)(j:j)
                     call errhdl(path,modnam,'W','292',dummy)
                  end if
               end do
               if (NumInt<1 .or. NumInt>4) then
!                    WRITE Warning Message:  Potential problem with BGFORM
                  write(dummy,'(''NumInts= '',I3)') NumInt
                  call errhdl(path,modnam,'W','292',dummy)
               end if
               if (NumReal/=1) then
!                    WRITE Warning Message:  Potential problem with BGFORM
                  write(dummy,'(''NumReal= '',I3)') NumReal
                  call errhdl(path,modnam,'W','292',dummy)
               end if
            end if
         else
!              WRITE Error Message:  BGFORM Field is Too Long
            write(dummy,'(''LEN='',I6)') loce(i+2)-locb(i+2)
            call errhdl(path,modnam,'E','292',dummy)
         end if
      else if (ifc == i+1) then
! ---       Use 'free' format as the default
         bgform(ibgsect) = 'FREE'
      else if (ifc > i+2) then
! ---       Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if

   else
! ---    Using BACKGRND option varying by SEASON, HROFDY, etc.

! ---    Assign number of background values based on BFLAG option
      if (bflag(ibgsect) == 'ANNUAL') then
         ibgmax(ibgsect) = 1
      else if (bflag(ibgsect) == 'SEASON') then
         ibgmax(ibgsect) = 4
      else if (bflag(ibgsect) == 'MONTH') then
         ibgmax(ibgsect) = 12
      else if (bflag(ibgsect) == 'HROFDY') then
         ibgmax(ibgsect) = 24
      else if (bflag(ibgsect) == 'WSPEED') then
         ibgmax(ibgsect) = 6
      else if (bflag(ibgsect) == 'SEASHR') then
         ibgmax(ibgsect) = 96
      else if (bflag(ibgsect) == 'HRDOW') then
         ibgmax(ibgsect) = 72
         L_DayOfWeekOpts = .true.
      else if (bflag(ibgsect) == 'HRDOW7') then
         ibgmax(ibgsect) = 168
         L_DayOfWeekOpts = .true.
      else if (bflag(ibgsect) == 'SHRDOW') then
         ibgmax(ibgsect) = 288
         L_DayOfWeekOpts = .true.
      else if (bflag(ibgsect) == 'SHRDOW7') then
         ibgmax(ibgsect) = 672
         L_DayOfWeekOpts = .true.
      else if (bflag(ibgsect) == 'MHRDOW') then
         ibgmax(ibgsect) = 864
         L_DayOfWeekOpts = .true.
      else if (bflag(ibgsect) == 'MHRDOW7') then
         ibgmax(ibgsect) = 2016
         L_DayOfWeekOpts = .true.
      else
!           WRITE Error Message    ! Invalid BFLAG Field Entered
         call errhdl(path,modnam,'E','203','BFLAG')
         go to 999
      end if

! ---    Call BGFILL to fill BACKGRND value arrays
      call bgfill

   end if

   go to 999

!     Process Error Messages; error opening file, include file type and sector
998 continue
   if (.not. L_BGSector) then
      call errhdl(path,modnam,'E','500','BG File')
   else
      write(dummy,'("BGFILE SECT",I1)') ibgsect
      call errhdl(path,modnam,'E','500',dummy)
   end if

999 return
end subroutine back_grnd

subroutine bgfill
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k

!     Variable Initializations
   modnam = 'BGFILL'

! --- Initialize counter for number of BACKGRND values for this sector
   iset = ibkgrd(ibgsect)

! --- Assign field number for start of data values based on whether
!     sector-varying values are used
   if (L_BGSector) then
      i = 5
   else
      i = 4
   end if

   do k = i, ifc
!        Change Fields To Numbers
      call stodbl(field(k),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         cycle
      end if
      do j = 1, imit
         iset = iset + 1
!           Assign The Field
         if (iset <= ibgmax(ibgsect)) then
            backgrnd(iset,ibgsect) = dnum
            if (dnum < 0.0d0) then
!                 WRITE Error Message:  Negative Value for BACKGRND
               call errhdl(path,modnam,'E','209',keywrd)
            end if
         else
!              WRITE Error Message    ! Too Many BACKGRND Values Input
            if (L_BGSector) then
               write(dummy,'(''BCKGRD SECT'',I1)') ibgsect
            else
               write(dummy,'(''BACKGRND'')')
            end if
            call errhdl(path,modnam,'E','231',dummy)
            go to 99
         end if
      end do
   end do

99 continue

! --- Save counter on number of values input so far for this sector
   ibkgrd(ibgsect) = iset

   return
end subroutine bgfill

subroutine back_unit
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'BACK_UNIT'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc > 3) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Check for units of background values

   if (field(3)=='UG/M3') then
      BackUnits = field(3)
   else if (field(3)=='PPM' .or. field(3)=='PPB') then
      if (pollut == 'NO2' .or. pollut == 'SO2'&
      &.or. pollut == 'CO') then
!RCO D14 AERMOD will only convert background from ppb or ppm for
!    SO2, NO2, or CO so do not let these units be specified
!    unless using these pollutants.
         BackUnits = field(3)
      else
!           Write Error Message:  Invalid units for background values
         call errhdl(path,modnam,'E','203','BackUnits')
      end if
   end if

999 return
end subroutine back_unit

subroutine bgsector
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
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   logical :: L_BadData

!     Variable Initializations
   modnam = 'BGSECTOR'
   L_BadData = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        Error Message: Too Few Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 8) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

! --- Set L_BGSector logical variable
   L_BGSector = .true.

   do i = 3, ifc
!        Loop through fields for starting directions for each BGSECTOR
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         write(dummy,'("BGSECT",I1)') i-2
         call errhdl(path,modnam,'E','208',dummy)
!           Assign logical variable for bad data, but cycle through full record
         L_BadData = .true.
         cycle
      end if
      bgsect(i-2) = dnum
      if (bgsect(i-2) < 0.0d0 .or. bgsect(i-2) > 360.0d0) then
!           Sector value out-of-range
         if (bgsect(i-2) > 9999.0d0) then
            write(dummy,'("BGSECT>9999.")')
         else if (bgsect(i-2) < -999.0d0) then
            write(dummy,'("BGSECT<-999.")')
         else
            write(dummy,'("BGSECT=",F5.0)') bgsect(i-2)
         end if
         call errhdl(path,modnam,'E','380',dummy)
      end if
   end do

! --- Check for presence of bad sector data
   if (L_BadData) go to 999

! --- Assign variable for number of user-specified BACKGRND sectors
   NUMBGSects = ifc-2

! --- Check BGSECTs for proper order and minimum sector widths
   do i = 1, NUMBGSects-1
      if (bgsect(i+1) < bgsect(i) ) then
!           Sector value out-of-order
         write(dummy,'("BGSECT",I1," < #",I1)') i+1, i
         call errhdl(path,modnam,'E','222',dummy)
      else if (bgsect(i+1) < bgsect(i)+30.0d0 ) then
!           Sector width < 30 degrees
         write(dummy,'("BGSECT",I1," < 30")') i+1
         call errhdl(path,modnam,'E','227',dummy)
      else if (bgsect(i+1) < bgsect(i)+60.0d0 ) then
!           Sector width < 60 degrees
         write(dummy,'("BGSECT",I1," < 60")') i+1
         call errhdl(path,modnam,'W','227',dummy)
      end if
   end do
! --- Now check for width of last sector
   if ( (bgsect(1)+360.0d0)-bgsect(NUMBGSects) < 30.0d0) then
!        Sector width < 30 degrees
      write(dummy,'("BGSECT",I1," < 30")') NUMBGSects
      call errhdl(path,modnam,'E','227',dummy)
   else if ( (bgsect(1)+360.0d0)-bgsect(NUMBGSects) < 60.0d0) then
!        Sector width < 60 degrees
      write(dummy,'("BGSECT",I1," < 60")') NUMBGSects
      call errhdl(path,modnam,'W','227',dummy)
   end if

999 return
end subroutine bgsector

subroutine bl_avginp
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
   use main1
   use buoyant_line

   implicit none
   character :: modnam*12
   character (len=12) :: tempid                    ! Multiple_BuoyLines_D41_Wood
! JAT 06/22/21 D065
! REMOVE I AS UNUSED VARIABLE
!      INTEGER :: I, ISDX, KK                       ! Multiple_BuoyLines_D41_Wood
   integer :: isdx, kk
   logical :: found                             ! Multiple_BuoyLines_D41_Wood

!     Variable Initializations
   modnam = 'BL_AVGINP'                         ! Multiple_BuoyLines_D41_Wood

! --- Check the number of fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 8) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 9) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

! Multiple_BuoyLines_D41_Wood
!     Modified to check if BLPINPUT record has (9 fields) or does not have
!      (8 fileds) a BL group ID; parameter names changed to account for
!      multiple buoyant line sources (groups)
!
   if (ifc == 8) then
! ---    BL group ID is not included on BLPINPUT card - assumption is
!        that there is only one BL source in this model run and all the
!        individual lines belong to that source

! ---    Check to see if either a BLPINPUT record with group ID = 'ALL'
!         or without a group ID has been processed (resulting in
!         NUMBLAVGINP >= 1);
!         if true, this is an error since all lines are considered
!         part of a single BL source when there are only 8 fields on a
!         BLPINOUT record
      if (numblavginp >= 1) then
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else
!           BLPINPUT_Check_D091_Wood: start
!           If there is no BUOYLINE source type in the input file, then
!           array BLAVGINP_GRPID is not allocated.
         if (nblp == 0) then
!              There are no BOUYLINE sources but there is a BLPINPUT record;
!              issue an error message and skip the remainder of the subroutine
            call errhdl(path,modnam,'E','508','  ')
            go to 999
         else
! ---          NUMBLAVGINP = 0 - this is the first BLPINPUT record processed
!              and there is no group ID - set it to ALL;
            numblavginp = 1
            blavginp_grpid(numblavginp) = 'ALL'
         end if
!           BLPINPUT_Check_D091_Wood: end
      end if

!        Fetch Each Field
      call stodbl(field(3),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 3 = average line length
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_llen(numblavginp) = dnum

      call stodbl(field(4),ilen_fld,dnum,imit)
!        Check The Numerical Field  - Field 4 = average bldg height
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_bhgt(numblavginp) = dnum

      call stodbl(field(5),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 5 = average bldg width
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_bwid(numblavginp) = dnum

      call stodbl(field(6),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 6 = average line width
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_lwid(numblavginp) = dnum

      call stodbl(field(7),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 7 = average bldg separation
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_bsep(numblavginp) = dnum

      call stodbl(field(8),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 8 = average line source buoyancy
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_fprm(numblavginp) = dnum

   else if (ifc == 9) then
! ---    A BLPINPUT ID is required to be in the 3rd field on the
!          BLPINPUT record; store it in a temporary variable
      tempid = field(3)

! ---    Check to see if this BLPINPUT ID has been defined - issue an
!        error messsage if it has and stop processing the record;
!        otherwise add the name to the BL group ID array
      if (numblavginp > 0) then
         do kk = 1,numblavginp
            call sindex(blavginp_grpid,numblavginp,&
            &tempid,isdx,found)
            if (found) then
!                 This group ID has been seen before - all BL group IDs
!                 must be unique for this keyword
               call errhdl(path,modnam,'E','504',tempid)
               go to 999
            else
!                 Add group ID to the BL group ID array
               numblavginp = numblavginp + 1
               blavginp_grpid(numblavginp) = tempid
            end if
            exit
         end do
      else
!           First BLPINPOUT record and there is a BL group ID in field 3
         numblavginp = numblavginp + 1
         blavginp_grpid(numblavginp) = tempid
      end if

      call stodbl(field(4),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 4 = average line length
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_llen(numblavginp) = dnum

      call stodbl(field(5),ilen_fld,dnum,imit)
!        Check The Numerical Field  - Field 5 = average bldg height
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_bhgt(numblavginp) = dnum

      call stodbl(field(6),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 6 = average bldg width
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_bwid(numblavginp) = dnum

      call stodbl(field(7),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 7 = average line width
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_lwid(numblavginp) = dnum

      call stodbl(field(8),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 8 = average bldg separation
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_bsep(numblavginp) = dnum

      call stodbl(field(9),ilen_fld,dnum,imit)
!        Check The Numerical Field - Field 9 = average line source buoyancy
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      blavginp_fprm(numblavginp) = dnum
   end if

!     Set the logical to true indicating that the model run has a buoyant
!      line source
   l_blsource = .true.

999 return
end subroutine bl_avginp

subroutine bl_rotate1 (kk)
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
   use main1
   use buoyant_line

   implicit none
   character :: modnam*12

   logical :: bl_ycoord_chk

   integer :: il, ilp, kk, iplstart
   double precision :: ddx, ddy
   double precision :: xor2, yor2, ddx2, ddy2, angrad2, angdiff

   double precision :: xb1, xe1, yb1, ye1, ylbsav, ylesav

!     Variable Initializations
   modnam = 'BL_ROTATE1'
   bl_ycoord_chk = .false.

! Multiple_BuoyLines_D41_Wood
!     Process buoyant lines by group (KK, a calling argument) to determine
!      required parameters to rotate source and later, the receptors, by group
!      and if the lines in the group are ordered properly;
!      some scalars converted to 1-D arrays (e.g., XOR, YOR)


   do_il: do il = 1,nbltotal
      if (blineparms(il)%iblpgrpnum == kk) then
         iplstart = il + 1                              ! Wood_D32: for lines parallel check

!           Compute parameters for initial rotation of sources and receptors
!            Variables with an array index means it is used in other routines
         xor(kk) = blineparms(il)%xbeg
         yor(kk) = blineparms(il)%ybeg
         ddx     = blineparms(il)%xend - xor(kk)
         ddy     = blineparms(il)%yend - yor(kk)


         angrad(kk)  = datan2(ddy,ddx)                ! rotation angle for the lines
         tcor(kk)    = 90.0d0 + angrad(kk) * rtodeg   ! TCOR will be used in BL_ROTATE2
         sintcor(kk) = dsin(angrad(kk))               ! also used later
         costcor(kk) = dcos(angrad(kk))               ! also used later


!           If the lines are oriented exactly north-south (90 degrees),
!            the cosine is very very small (E-17), which causes AERMOD to
!            generate results that do not match well with BLP.  Set COSTCOR
!            to E-05 (approx. equal to 0.5 degrees)
         if (dabs(costcor(kk)) < 1.0d-10) costcor(kk) = 1.0d-05

         exit do_il
      end if
   end do do_il


! --- Perform the translation and rotation for BL group KK (a calling argument)
   do il = 1,nbltotal
      if (blineparms(il)%iblpgrpnum == kk) then
         xb1 = blineparms(il)%xbeg - xor(kk)
         xe1 = blineparms(il)%xend - xor(kk)
         yb1 = blineparms(il)%ybeg - yor(kk)
         ye1 = blineparms(il)%yend - yor(kk)
         yb1 = -xb1*sintcor(kk) + yb1*costcor(kk)
         blineparms(il)%ybeg_tr1 = yb1

         xb1 = (xb1+yb1*sintcor(kk))/costcor(kk)
         blineparms(il)%xbeg_tr1 = xb1

         ye1 = -xe1*sintcor(kk) + ye1*costcor(kk)
         ys_scs(il) = ye1
         blineparms(il)%yend_tr1 = ye1

         xe1 = (xe1+ye1*sintcor(kk))/costcor(kk)
         blineparms(il)%xend_tr1 = xe1
      end if
   end do

   do il = 1, nbltotal
!        Verify line source coordinates for BL source group KK (a calling
!          argument) are input correctly wrt to the y-axis
      if (blineparms(il)%iblpgrpnum == kk) then
         if( .not. bl_ycoord_chk) then
            ylbsav = blineparms(il)%ybeg_tr1
            ylesav = blineparms(il)%yend_tr1
            bl_ycoord_chk = .true.

         else
            if (blineparms(il)%ybeg_tr1 > ylbsav .and.&
            &blineparms(il)%yend_tr1 > ylesav) then
!                 The pair of lines are in the correct 'order' -
!                 save the coordinates and check the next pair
               ylbsav = blineparms(il)%ybeg_tr1
               ylesav = blineparms(il)%yend_tr1
            else
               call errhdl(path,modnam,'E','389',&
               &blineparms(il)%srcid)
            endif
         endif
      end if

   end do

! BuoyantLine_CheckLinesParallel_D32 (Wood)
!     Confirm that the individual lines within a buoyant line group/source
!       are within 5 degrees of the first line in the group.
!     If not issue a warning but continue processing
!     IPLSTART was determined above when determining the parameters for
!       first rotation

   do ilp = iplstart, nbltotal
      if (blineparms(ilp)%iblpgrpnum == kk) then
         angrad2 = 0.0d0
         xor2 = blineparms(ilp)%xbeg
         yor2 = blineparms(ilp)%ybeg

         ddx2 = blineparms(ilp)%xend - xor2
         ddy2 = blineparms(ilp)%yend - yor2
         angrad2 = datan2(ddy2,ddx2)
         angdiff = dabs(angrad(kk)-angrad2) *&
         &(180.0d0/(4.0d0*datan(1.0d0)))
         if (angdiff > Parallel_Tol) then
            call errhdl(path,modnam,'W','506',blineparms(ilp)%srcid)
         end if
      end if

   end do

   return
end subroutine bl_rotate1

subroutine rlinebar_inputs
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
   use main1
   use rline_data

   implicit none
   character :: modnam*12

   integer  :: indexs
   character (len=12)  :: soid
   logical  :: found

!     Variable Initializations
   modnam = 'RLBAR_INP'
   found  = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc == 6) then
!        Error Message: Invalid Parameter Specified
      call errhdl(path,modnam,'E','203',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source ID Character Substring
   soid = field(3)

!     Match the ID to the SRCID array to get ISRC
   call sindex(srcid, nsrc, soid, indexs, found)

   if(found) then
!     Check that it is a RLINEXT source!
      if (srctyp(indexs) == 'RLINEXT') then
!        Fetch and check each of the numerical fields
         call stodbl(field(4), ilen_fld, rlsource(indexs)%htwall, imit)
!        Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         end if
         call stodbl(field(5), ilen_fld, rlsource(indexs)%dclwall, imit)
!        Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         end if
!        Fetch and check numerical fields for second barrier
         if (ifc == 7) then
            call stodbl(field(6), ilen_fld, rlsource(indexs)%htwall2,&
            &imit)
!          Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
            end if
            call stodbl(field(7), ilen_fld, rlsource(indexs)%dclwall2,&
            &imit)
!          Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
            end if
         end if
      else
!        WRITE Error Message: "Keyword not available for RLINE type:"
         call errhdl(path,modnam,'E','278',keywrd)
      end if
   else
!        WRITE Error Message: Source Location Has Not Been Identified
      call errhdl(path,modnam,'E','300',keywrd)
   end if

!     ERROR handling for barrier parameters
   if ((rlsource(indexs)%htwall< 0.0d0) .or.&
   &(rlsource(indexs)%htwall2< 0.0d0)) then
!        WRITE Error Message:  Negative barrier height: HTWALL
      call errhdl(path,modnam,'E','209',' HTWALL ')
   endif

999 return
end subroutine rlinebar_inputs

subroutine rlinedpr_inputs
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
   use main1
   use rline_data

   implicit none
   character :: modnam*12

   integer  :: indexs
   character (len=12)  :: soid
   logical  :: found

!     Variable Initializations
   modnam = 'RLDPR_INP'
   found  = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 6) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 6) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source ID Character Substring
   soid = field(3)

!     Match the ID to the SRCID array to get ISRC
   call sindex(srcid, nsrc, soid, indexs, found)

   if(found) then
      if(srctyp(indexs) == 'RLINEXT') then
!          Fetch and check each of the numerical fields
         call stodbl(field(4),ilen_fld,rlsource(indexs)%depth,imit)
!          Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         end if
         call stodbl(field(5),ilen_fld,rlsource(indexs)%wtop,imit)
!          Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         end if
         call stodbl(field(6),ilen_fld,rlsource(indexs)%wbottom,imit)
!          Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         end if
      else
!          WRITE Error Message: "Keyword only available for RLINEXT source type:"
         call errhdl(path,modnam,'E','278',keywrd)
      end if
   else
!        WRITE Error Message: Source Location Has Not Been Identified
      call errhdl(path,modnam,'E','300',keywrd)
   end if

!     ERROR handling for barrier and depressed roadway parameters
   if (rlsource(indexs)%depth> 0.0d0) then
!        WRITE Error Message:  Depth of depression is positive (should be negative)
      call errhdl(path,modnam,'E','320',' DEPTH ')
   endif

   if (rlsource(indexs)%wtop< 0.0d0) then
!        WRITE Error Message:  WTOP is negative (should be positive)
      call errhdl(path,modnam,'E','320',' WTOP ')
   endif

   if (rlsource(indexs)%wbottom < 0.0d0 .or.&
   &rlsource(indexs)%wbottom > rlsource(indexs)%wtop) then
!        WRITE Error Message:  WBOTTOM is negative or is greater than WTOP
      call errhdl(path,modnam,'E','320',' WBOTTOM ')
   endif


999 return
end subroutine rlinedpr_inputs

subroutine aircraft
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
   use main1
   implicit none
   character :: modnam*12

   character (len=12) :: lowid, higid, lid1, lid2, hid1, hid2, tempid
   integer :: i, il, ih, k, istr
   logical :: ingrp, rmark, found

!     Variable Initializations
   modnam = 'AIRCRAFT'
   found = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   end if

!     Check for 'ARCFTSRC ALL' option identified in PRESET
   if (l_arcft_all) then
      if (ifc == 3 .and. field(3) == 'ALL') then
         aftsrc(:) = 'Y'
         go to 999
      else
!           WRITE Error Message:  ARCFTSRC ALL
         call errhdl(path,modnam,'E','279','ARCFTSRC ALL')
         go to 999
      end if

   else
!        Specify field index to start for Source IDs
      istr = 3

   end if

!     Loop Through Fields
   do i = istr, ifc
      if (index(field(i),'-') == 0) then
         found = .false.
         do k = 1, numsrc
            if (srcid(k) == field(i)) then
               found = .true.
               aftsrc(k) = 'Y'
            end if
         end do
         if (.not.found) then
!              WRITE Error Message:  SRCID not found
            call errhdl(path,modnam,'E','300',keywrd)
            cycle
         end if
      else

         call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,lowid,&
         &higid)
!           First Check Range for Upper Value < Lower Value
         call setidg(lowid,lid1,il,lid2)
         call setidg(higid,hid1,ih,hid2)
         if ((hid1<lid1) .or. (ih<il) .or. (hid2<lid2)) then
!              WRITE Error Message:  Invalid Range,  Upper < Lower
            call errhdl(path,modnam,'E','203','SRCRANGE')
            cycle
         end if
         do k = 1, numsrc
            call asngrp(srcid(k),lowid,higid,ingrp)
            if (ingrp) then
               aftsrc(k) = 'Y'
            end if
         end do
      end if
   end do

999 return
end subroutine aircraft
