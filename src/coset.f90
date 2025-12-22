SUBROUTINE COCARD
!***********************************************************************
!                 COCARD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To process COntrol Pathway card images
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To add the Aircraft Option (ARCFTOPT)
!                    (to calculate the plume rise for Aircraft sources).
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   To add various NOx background options (NOXSECTR,
!                    NOXVALUE, NOX_VALS, NOX_UNIT, NOX_FILE) for use
!                    with the GRSM NO2 option
!                    CERC, 11/30/20
!
!        MODIFIED:   Added the PSDCREDIT option for PVMRM; in this release
!                    specifying PSDCREDIT also requires specifying PVMRM;
!                    specifying PSDCREDIT and OLM is not a valid combination
!                    J Paumier, MACTEC -  09/30/2006
!
!        MODIFIED:   Added undocumentd NODRYDPLT and NOWETDPLT options to
!                    MODOPS header.  Also moved code to write header of
!                    DEBUG output file to AERMOD.FOR to follow SETUP,
!                    to accommodate final setting for DRYDPLT and WETDPLT.
!                    R. W. Brode, PES - 10/26/2004
!
!        MODIFIED:   To allow 24-hour or ANNUAL averages to be modeled
!                    separately for post-1997 PM10 processing.
!                    R. W. Brode, PES - 12/2/98
!
!        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
!                    to allow just the wet or just the dry deposition flux
!                    to be reported.  DEPOS now reports the sum of wet and
!                    dry fluxes.
!                    D. Strimaitis, SRC - 11/8/93
!
!        MODIFIED:   To add DEPLETE parameter for plume depletion option
!                    and to allow flagpole receptors with DEPOS option.
!                    D. Strimaitis, SRC - 2/15/93
!
!        INPUTS:  Pathway (CO) and Keyword
!
!        OUTPUTS: Processing Option Switches
!                 Option Setup Status Switches
!
!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I

!     Variable Initializations
   MODNAM = 'COCARD'

   IF (KEYWRD == 'STARTING') THEN
      IURB = 0
!        Set Status Switch
      ISTART = .TRUE.
      ICSTAT(1) = ICSTAT(1) + 1
      IF (ICSTAT(1) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'TITLEONE') THEN
!        Set Status Switch
      ICSTAT(2) = ICSTAT(2) + 1
      IF (ICSTAT(2) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Titles                                  ---   CALL TITLES
         CALL TITLES
      END IF
   ELSE IF (KEYWRD == 'TITLETWO') THEN
!        Set Status Switch
      ICSTAT(3) = ICSTAT(3) + 1
      IF (ICSTAT(3) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Titles                                  ---   CALL TITLES
         CALL TITLES
      END IF
   ELSE IF (KEYWRD == 'MODELOPT') THEN
!        Set Status Switch
      ICSTAT(4) = ICSTAT(4) + 1
      IF (ICSTAT(4) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Modeling Options                        ---   CALL MODOPT
         CALL MODOPT
      END IF
   ELSE IF (KEYWRD == 'AVERTIME') THEN
!        Set Status Switch
      ICSTAT(5) = ICSTAT(5) + 1
      IF (ICSTAT(5) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Averaging Time Options                  ---   CALL AVETIM
         CALL AVETIM
      END IF
   ELSE IF (KEYWRD == 'POLLUTID') THEN
!        Set Status Switch
      ICSTAT(6) = ICSTAT(6) + 1
      IF (ICSTAT(6) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Pollutant ID Option                     ---   CALL POLLID
         CALL POLLID
      END IF
   ELSE IF (KEYWRD == 'HALFLIFE' .or.&
   &KEYWRD == 'DCAYCOEF') THEN
      IF (KEYWRD == 'HALFLIFE') THEN
!           Check for Previous DCAYCOEF Keyword in Runstream File
         IF (ICSTAT(8) /= 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
            GO TO 999
         ELSE
!              Set Status Switch and Check for Duplicate Keyword
            ICSTAT(7) = ICSTAT(7) + 1
            IF (ICSTAT(7) /= 1) THEN
!                 WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
               GO TO 999
            END IF
         END IF
      ELSE IF (KEYWRD == 'DCAYCOEF') THEN
!           Check for Previous HALFLIFE Keyword in Runstream File
         IF (ICSTAT(7) /= 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
            GO TO 999
         ELSE
!              Set Status Switch and Check for Duplicate Keyword
            ICSTAT(8) = ICSTAT(8) + 1
            IF (ICSTAT(8) /= 1) THEN
!                 WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
               GO TO 999
            END IF
         END IF
      END IF
!        Check for Keyword Out of Order
      IF (ICSTAT(4) /= 1) THEN
!           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
         CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
      ELSE IF (ICSTAT(6) /= 1) THEN
!           WRITE Error Message: Keyword Out of Order (Must Follow POLLUTID)
         CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
      END IF
!        Process Exponential Decay Option                   ---   CALL EDECAY
      CALL EDECAY
   ELSE IF (KEYWRD == 'FLAGPOLE') THEN
!        Set Status Switch
      ICSTAT(11) = ICSTAT(11) + 1
      IF (ICSTAT(11) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
         CALL FLAGDF
      END IF
   ELSE IF (KEYWRD == 'RUNORNOT') THEN
!        Set Status Switch
      ICSTAT(12) = ICSTAT(12) + 1
      IF (ICSTAT(12) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Option to Run Model or Not              ---   CALL RUNNOT
         CALL RUNNOT
      END IF
   ELSE IF (.NOT.EVONLY .and. KEYWRD == 'EVENTFIL') THEN
!        Set Status Switch
      ICSTAT(13) = ICSTAT(13) + 1
      IF (ICSTAT(13) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (PSDCREDIT) THEN
!              WRITE Warning Message:  PSDCREDIT option cannot be used with EVENT option
            CALL ERRHDL(PATH,MODNAM,'W','147',KEYWRD)
         END IF
!           Process EVENT File Option                       ---   CALL EVNTFL
         CALL EVNTFL
      END IF
   ELSE IF (.NOT.EVONLY .and. KEYWRD == 'SAVEFILE') THEN
!        Set Status Switch
      ICSTAT(14) = ICSTAT(14) + 1
      IF (ICSTAT(14) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Model Re-start Save File Option         ---   CALL SAVEFL
         CALL SAVEFL
      END IF
   ELSE IF (.NOT.EVONLY .and. KEYWRD == 'INITFILE') THEN
!        Set Status Switch
      ICSTAT(15) = ICSTAT(15) + 1
      IF (ICSTAT(15) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Re-start Initialization File Option     ---   CALL INITFL
         CALL INITFL
      END IF
   ELSE IF (.NOT.EVONLY .and. KEYWRD == 'MULTYEAR') THEN
!        Set Status Switch
      ICSTAT(16) = ICSTAT(16) + 1
      IF (ICSTAT(16) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Multiple-Year Run Option                ---   CALL MYEAR
         CALL MYEAR
      END IF
   ELSE IF (KEYWRD == 'ERRORFIL') THEN
!        Set Status Switch
      ICSTAT(17) = ICSTAT(17) + 1
      IF (ICSTAT(17) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Error File Option                       ---   CALL ERRFIL
         CALL ERRFIL
      END IF
   ELSE IF (KEYWRD == 'GDSEASON') THEN
!        Set Status Switch
      ICSTAT(18) = ICSTAT(18) + 1
      IF (ICSTAT(18) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Seasons for GASDEP Option              ---   CALL GDSEAS
         CALL GDSEAS
      END IF
   ELSE IF (KEYWRD == 'GASDEPDF') THEN
!        Set Status Switch
      ICSTAT(19) = ICSTAT(19) + 1
      IF (ICSTAT(19) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process GASDEP Defaults Option                  ---   CALL GDDEF
         CALL GDDEF
      END IF
   ELSE IF (KEYWRD == 'GDLANUSE') THEN
!        Set Status Switch
      ICSTAT(20) = ICSTAT(20) + 1
      IF (ICSTAT(20) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Error File Option                       ---   CALL GDLAND
         CALL GDLAND
      END IF
   ELSE IF (KEYWRD == 'GASDEPVD') THEN
!        Set Status Switch
      ICSTAT(21) = ICSTAT(21) + 1
      IF (ICSTAT(21) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           User Specified Deposition Velocity Option       ---   CALL GVSUBD
         CALL GVSUBD
      END IF
   ELSE IF (KEYWRD == 'DEBUGOPT') THEN
!        Set Status Switch
      ICSTAT(22) = ICSTAT(22) + 1
      IF (ICSTAT(22) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
!           Process Error File Option                       ---   CALL DEBOPT
         CALL DEBOPT
      END IF
   ELSE IF (KEYWRD == 'URBANOPT') THEN
!        Set Status Switch
      ICSTAT(23) = ICSTAT(23) + 1
!        Check for Keyword Out of Order
      IF (ICSTAT(4) /= 1) THEN
!           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
         CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
      END IF
!        Process Urban Option                               ---   CALL URBOPT
      CALL URBOPT
   ELSE IF (KEYWRD == 'OZONEVAL') THEN
!        Set Status Switch
      ICSTAT(24) = ICSTAT(24) + 1
      IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
!           Process O3 Value Option                         ---   CALL O3VAL
         CALL O3VAL
      ELSE
!RCO 3/1/2021 Check to see if we want to use 142 or 600 error code. CERC used 600
!           Write Error Message:  OZONEVAL specified w\o PVMRM, OLM, TTRM, GRSM
         CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'O3VALUES') THEN
!        Set Status Switch
      ICSTAT(25) = ICSTAT(25) + 1
      IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
!           Process O3 Value Option                         ---   CALL O3VALS
         CALL O3VALS
      ELSE
!           Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
         CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'OZONEFIL') THEN
!        Set Status Switch
      ICSTAT(26) = ICSTAT(26) + 1
      IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
!           Process O3 File Option                          ---   CALL O3FILE
         CALL O3FILE
      ELSE
!           Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
         CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'OZONUNIT') THEN
!        Set Status Switch
      ICSTAT(27) = ICSTAT(27) + 1
      IF (ICSTAT(27) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
!              Process the OZONUNIT Card                    ---   CALL OZON_UNIT
            CALL OZON_UNIT
         ELSE
!              Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
            CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'NO2STACK') THEN
!        Set Status Switch
      ICSTAT(28) = ICSTAT(28) + 1
      IF (ICSTAT(28) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
!              Process NO2Stack Option                      ---   CALL NO2STK
            CALL NO2STK
         ELSE
!              Write Error Message: NO2STACK specified w/o PVMRM, OLM, TTRM, GRSM
            CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'NO2EQUIL') THEN
!        Set Status Switch
      ICSTAT(29) = ICSTAT(29) + 1
      IF (ICSTAT(29) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
! Add equilibrium limit for GRSM also?
         IF (PVMRM .or. OLM .or. RUNTTRM) THEN
!              Process NO2Equil Option                      ---   CALL NO2EQ
            CALL NO2EQ
         ELSE
!              Write Error Message:  NO2EQUIL specified without PVMRM or OLM or TTRM
            CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'LOW_WIND') THEN
!        Set Status Switch
      ICSTAT(30) = ICSTAT(30) + 1
      IF (ICSTAT(30) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE IF (L_ALPHA) THEN
!            Process LOW_WIND keyword                       ---   CALL LOW_WND
         CALL LOW_WND
      ELSE
!           WRITE Error Message: LOW_WIND option requires ALPHA option
         CALL ERRHDL(PATH,MODNAM,'E','133',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'O3SECTOR') THEN
!        Set Status Switch
      ICSTAT(31) = ICSTAT(31) + 1
      IF (ICSTAT(31) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
!              Process O3SECTOR keyword                     ---   CALL O3SECTOR
            CALL O3SECTOR
         ELSE
!              Write Error Message: O3SECTOR specified w/o PVMRM, OLM, TTRM, GRSM
            CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'ARMRATIO') THEN
!        Set Status Switch
      ICSTAT(32) = ICSTAT(32) + 1
      IF (ICSTAT(32) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (ARM2) THEN
!              Process ARM2_Ratios Option                   ---   CALL ARM2_Ratios
            CALL ARM2_Ratios
         ELSE
!              Write Error Message:  ARMRATIO specified without ARM2
            CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'AWMADWNW') THEN
!        Set Status Switch
      ICSTAT(33) = ICSTAT(33) + 1
      IF (ICSTAT(33) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE IF (L_ALPHA) THEN
!           Process AWMADWNW keyword                         ---   CALL AWMA_DOWNWASH
         CALL AWMA_DOWNWASH
      ELSE
!           WRITE Error Message: AWMADWNW option requires ALPHA option
         CALL ERRHDL(PATH,MODNAM,'E','122',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'ORD_DWNW') THEN
!        Set Status Switch
      ICSTAT(34) = ICSTAT(34) + 1
      IF (ICSTAT(34) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE IF (L_ALPHA) THEN
!           Process ORD_DWNW keyword                       ---   CALL ORD_DOWNWASH
         CALL ORD_DOWNWASH
      ELSE
!           WRITE Error Message: ORD_DWNW option requires ALPHA option
         CALL ERRHDL(PATH,MODNAM,'E','123',KEYWRD)
      END IF
!     CERC 11/30/20:
   ELSE IF (KEYWRD == 'NOXSECTR') THEN
!        Set Status Switch
      ICSTAT(35) = ICSTAT(35) + 1
      IF (ICSTAT(35) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (GRSM) THEN
!              Process NOXSECTR keyword                    ---   CALL NOXSECTOR
            CALL NOXSECTOR
         ELSE
!              Write Error Message:  NOXSECTR specified without GRSM
            CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'NOXVALUE') THEN
!        Set Status Switch
      ICSTAT(36) = ICSTAT(36) + 1
      IF (GRSM) THEN
!           Process NOX Value Option                    ---   CALL NOXVAL
         CALL NOXVAL
      ELSE
!           Write Error Message:  NOXVALUE specified without GRSM
         CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'NOX_VALS') THEN
!        Set Status Switch
      ICSTAT(37) = ICSTAT(37) + 1
      IF (GRSM) THEN
!           Process NOx Value Option                    ---   CALL NOXVALS
         CALL NOXVALS
      ELSE
!           Write Error Message:  NOX_VALS specified without GRSM
         CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
      END IF
   ELSE IF (KEYWRD == 'NOX_UNIT') THEN
!        Set Status Switch
      ICSTAT(38) = ICSTAT(38) + 1
      IF (ICSTAT(38) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
      ELSE
         IF (GRSM) THEN
!              Process the NOX_UNIT Card                    ---   CALL NOX_UNIT
            CALL NOX_UNIT
         ELSE
!              Write Error Message:  NOX_UNIT specified without GRSM
            CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
         END IF
      END IF
   ELSE IF (KEYWRD == 'NOX_FILE') THEN
!        Set Status Switch
      ICSTAT(39) = ICSTAT(39) + 1
      IF (GRSM) THEN
!           Process NOX File Option                    ---   CALL NOXFILE
         CALL NOXFILE
      ELSE
!           Write Error Message:  NOX_FILE specified without GRSM
         CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
      END IF

!**  Added for Aircraft Plume Rise; UNC-IE
   ELSE IF (KEYWRD == 'ARCFTOPT') THEN
!        Set Status Switch
      ICSTAT(40) = ICSTAT(40) + 1
!        Check for Keyword Out of Order
      IF(ICSTAT(4) /= 1) THEN
!           WRITE Error Message: Keyword Out of Order (Must Follow
!           MODELOPT)
         CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
      END IF
      IF (IFC == 3) THEN
!            Assign the airport name (optional)
         AFTID(1) = FIELD(3)
      END IF
!     Assign Logical for Aircraft Option
      ARCFT  = .TRUE.
!**  End Aircraft Plume Rise insert; April 2023

   ELSE IF (KEYWRD == 'FINISHED') THEN
!        Set Status Switch
      IFINIS = .TRUE.
!        Set Status Switch
      ICSTAT(50) = ICSTAT(50) + 1
      IF (ICSTAT(50) /= 1) THEN
!           WRITE Error Message: Repeat Non-repeatable Keyword
         CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         GO TO 999
      END IF

!        Check for Missing Mandatory Keywords
      IF (ICSTAT(1) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
      END IF
      IF (ICSTAT(2) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','TITLEONE')
      END IF
      IF (ICSTAT(4) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','MODELOPT')
      END IF
      IF (ICSTAT(5) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','AVERTIME')
      END IF
      IF (ICSTAT(6) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','POLLUTID')
      END IF
      IF (ICSTAT(12) == 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','RUNORNOT')
      END IF

      IF (OLM .or. PVMRM .or. RUNTTRM .or. GRSM) THEN
! ---       Check for background Ozone options for OLM/PVMRM/GRSM
         IF (ICSTAT(24)==0 .and. ICSTAT(25)==0 .and.&
         &ICSTAT(26)==0) THEN
!              Write Error Message:  Ozone value or data file needed
            IF (OLM) THEN
               DUMMY = 'OLM Option  '
            ELSE IF (PVMRM) THEN
               DUMMY = 'PVMRM Option'
            ELSE IF (GRSM) THEN
               DUMMY = 'GRSM Option'
            ELSE IF (RUNTTRM) THEN
               DUMMY = 'TTRM Option  '
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','283',DUMMY)
         ELSE IF (ICSTAT(24)>0 .and. ICSTAT(25)>0) THEN
! ---          Both OZONEVAL and O3VALUES keywords have been specified;
!              issue error message if needed; first check for O3SECTOR option
            IF (.NOT. L_O3Sector) THEN
!                 No O3SECTORs; issue error message
               IF (OLM) THEN
                  DUMMY = 'for OLM   '
               ELSE IF (PVMRM) THEN
                  DUMMY = 'for PVMRM '
               ELSE IF (GRSM) THEN
                  DUMMY = 'for GRSM '
               ELSE IF (RUNTTRM) THEN
                  DUMMY = 'for TTRM   '
               END IF
!                 Issue error message for conflicting O3 options
               CALL ERRHDL(PATH,MODNAM,'E','148',DUMMY)
            ELSE
! ---             Loop through O3SECTORs for both OZONEVAL & O3VALUES
               DO I = 1, NUMO3sects
                  IF (L_O3VAL(I) .and. L_O3VALUES(I)) THEN
!                       Issue error message; OZONEVAL & O3VALUES for this sector
                     IF (OLM) THEN
                        WRITE(DUMMY,'(''  OLM SECT'',I1)') I
                     ELSE IF (PVMRM) THEN
                        WRITE(DUMMY,'(''PVMRM SECT'',I1)') I
                     ELSE IF (GRSM) THEN
                        WRITE(DUMMY,'(''GRSM SECT'',I1)') I
                     ELSE IF (RUNTTRM) THEN
                        WRITE(DUMMY,'(''  TTRM SECT'',I1)') I
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','148',DUMMY)
                  END IF
               END DO
            END IF
         END IF
!           CERC 11/30/20
         IF (GRSM) THEN
! ---         Check for background NOx options for GRSM
            IF (ICSTAT(36)==0 .and. ICSTAT(37)==0 .and.&
            &ICSTAT(39)==0 ) THEN
!               No NOx background has been specified so it will be calculated from NO2 equilibrium
! If the background NOX is missing, is it appropriate to compute from the equilibrium?
! Or use a fill value (default) to make sure it's conservative.
               L_CalcNOXFromNO2 = .TRUE.
!               Write Wng Message:  NOx calculated from NO2
               DUMMY = 'for GRSM'
               CALL ERRHDL(PATH,MODNAM,'W','612',DUMMY)
            ELSE IF (ICSTAT(36)>0 .and. ICSTAT(37)>0) THEN
! ---           Both NOXVALUE and NOX_VALS keywords have been specified;
!               issue error message if needed; first check for NOXSECTOR option
               IF (.NOT. L_NOxSector) THEN
!                 No NOXSECTORs; issue error message for conflicting NOx options
                  CALL ERRHDL(PATH,MODNAM,'E','605','for GRSM ')
               ELSE
! ---             Loop through NOxSECTORs for both NOXVALUE & NOX_VALS
                  DO I = 1, NUMNOxsects
                     IF (L_NOXVALUE(I) .and. L_NOX_VALS(I)) THEN
!                       Issue error message; NOXVALUE & NOX_VALS for this sector
                        WRITE(DUMMY,'(''GRSM SECT'',I1)') I
                        CALL ERRHDL(PATH,MODNAM,'E','605',DUMMY)
                     END IF
                  END DO
               END IF
            END IF
         END IF
         IF ((PVMRM .or. OLM .or. RUNTTRM .or. GRSM)&
         &.and. ICSTAT(28)==0) THEN
!              No NO2STACK card specified for PVMRM, OLM, TTRM or GRSM options.
!              Reinitialize ANO2_RATIO array to -9.0 to track whether
!              NO2/NOx ratios are applied on the SO Pathway with
!              NO2RATIO card for each source.
            ANO2_RATIO(:) = -9.0D0
         END IF
! ---       Check for OZONUNIT keyword without O3VALUES keyword
         IF (ICSTAT(25) == 0 .and. ICSTAT(27) > 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','193','CO O3VALUES')
         END IF
! ---       CERC 11/30/20 Check for NOXUNIT keyword without NOX_VALS keyword
         IF (ICSTAT(37) == 0 .and. ICSTAT(38) > 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','193','CO NOX_VALS')
         END IF
      END IF

! ---    Check for ARM2 inputs
      IF (ARM2) THEN
         IF (ICSTAT(32) == 0) THEN
! ---          No ARMRATIO keyword specified; apply default ratios for ARM2
            ARM2_Min = 0.50D0
            ARM2_Max = 0.90D0
         END IF
      END IF

!        OPEN Restart Save and Initialization Files
      IF (RSTSAV) THEN
         DUMMY = 'SAVEFILE'
         OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',&
         &IOSTAT=IOERRN,STATUS='REPLACE')
!           Close SAVEFILE since it is re-opened in RSDUMP
         CLOSE (IDPUNT)
         IF (SAVFL2 /= SAVFIL) THEN
            OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',&
            &IOSTAT=IOERRN,STATUS='REPLACE')
!              Close SAVEFILE since it is re-opened in RSDUMP
            CLOSE (IDPUN2)
         END IF
      END IF
      IF (RSTINP) THEN
         IF (RSTSAV) THEN
! ---          First check for filename conflicts with SAVEFILEs
            IF (INIFIL == SAVFIL .or. INIFIL == SAVFL2) THEN
! ---             The INITFILE name matches a SAVEFILE name;
!                 issue error message
               CALL ERRHDL(PATH,MODNAM,'E','590','       ')
            ELSE
! ---             No filename conflict, open INITFILE
               DUMMY = 'INITFILE'
               OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,&
               &FORM='UNFORMATTED',IOSTAT=IOERRN,STATUS='OLD')
            END IF
         ELSE
! ---          No SAVEFILEs, so open INITFILE
            DUMMY = 'INITFILE'
            OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,FORM='UNFORMATTED',&
            &IOSTAT=IOERRN,STATUS='OLD')
         END IF
      END IF

!        Check Averaging Periods Selected for SCREEN Mode Option
      IF (SCREEN) THEN
         IF (NUMAVE > 1) THEN
!              WRITE Error Message:  Too Many Averaging Periods Selected
            CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
         ELSE IF (KAVE(1) /= 1) THEN
!              WRITE Error Message:  Invalid Averaging Period Selected
            CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
         END IF
         IF (PERIOD) THEN
!              WRITE Error Message:  Too Many Averaging Periods Selected
            CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
         END IF
      END IF

! ---    Check for non-DFAULT gas deposition options
! JAT 7/2/19 CHECK FOR ALPHA AND GAS DEPOSITION PARAMETERS
!         IF GAS DEPOSITION PARAMETERS AND NO ALPHA OPTION
!     ISSUE ERROR.  THERE IS ALSO A CHECK FOR GASDEPOS AND
!     ALPHA IN SOCARD
      IF (DFAULT .and. ICSTAT(18) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         CALL ERRHDL(PATH,MODNAM,'E','196','GDSEASON')
      ELSEIF (.NOT. L_ALPHA .and. ICSTAT(18) > 0) THEN !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         CALL ERRHDL(PATH,MODNAM,'E','198','GDSEASON')
      ELSE IF (ICSTAT(18) > 0) THEN
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .TRUE.
      END IF
      IF (DFAULT .and. ICSTAT(19) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPDF')
      ELSEIF (.NOT. L_ALPHA .and. ICSTAT(19) > 0) THEN !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         CALL ERRHDL(PATH,MODNAM,'E','198','GASDEPDF')
      ELSE IF (ICSTAT(19) > 0) THEN
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .TRUE.
      END IF
      IF (DFAULT .and. ICSTAT(20) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         CALL ERRHDL(PATH,MODNAM,'E','196','GDLANUSE')
      ELSEIF (.NOT. L_ALPHA .and. ICSTAT(20) > 0) THEN !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         CALL ERRHDL(PATH,MODNAM,'E','198','GDLANUSE')
      ELSE IF (ICSTAT(20) > 0) THEN
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .TRUE.
      END IF
      IF (DFAULT .and. ICSTAT(21) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPVD')
      ELSEIF (.NOT. L_ALPHA .and. ICSTAT(21) > 0) THEN !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         CALL ERRHDL(PATH,MODNAM,'E','198','GASDEPVD')
      ELSE IF (ICSTAT(21) > 0) THEN
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .TRUE.
      END IF

! ---    Check for incompatibilities with user-specified deposition velocity
      IF (LUSERVD .and. (DEPOS .or. WDEP .or. WDPLETE)) THEN
!           Write Error Message: Wet deposition output incompatible with GASDEPVD option
         CALL ERRHDL(PATH,MODNAM,'E','243','GASDEPVD')
      END IF

! ---    Check for incompatible gas deposition inputs with GASDEPVD option for
!        user-specified gas dry deposition velocity
      IF (LUSERVD .and. ICSTAT(18) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
         CALL ERRHDL(PATH,MODNAM,'E','195','GDSEASON')
      END IF
      IF (LUSERVD .and. ICSTAT(19) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
         CALL ERRHDL(PATH,MODNAM,'E','195','GASDEPDF')
      END IF
      IF (LUSERVD .and. ICSTAT(20) > 0) THEN
!           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
         CALL ERRHDL(PATH,MODNAM,'E','195','GDLANUSE')
      END IF

!        Generate MODOPS Character Array to Summarize Modeling Options
      IF (DFAULT) THEN
         MODOPS(1) = 'RegDFAULT'
      ELSE IF (L_NonDFAULT) THEN
         MODOPS(1) = 'NonDFAULT'
      ELSE
         MODOPS(1) = '         '
      END IF

      IF (CONC) THEN
         MODOPS(2) = 'CONC'
      END IF
      IF (DEPOS) THEN
         MODOPS(3) = 'DEPOS'
      END IF
      IF (DDEP) THEN
         MODOPS(4) = 'DDEP'
      END IF
      IF (WDEP) THEN
         MODOPS(5) = 'WDEP'
      END IF

      IF (FLATSRCS) THEN
         MODOPS(6) = 'FLAT and'
         MODOPS(7) = 'ELEV'
      ELSE IF (FLAT) THEN
         MODOPS(6) = 'FLAT'
      ELSE
         MODOPS(7) = 'ELEV'
      END IF

      IF (FLGPOL) MODOPS(8)  = 'FLGPOL'
      IF (NOSTD)  MODOPS(9)  = 'NOSTD'
      IF (NOCHKD) THEN
         MODOPS(10) = 'NOCHKD'
      ELSE IF (L_WARNCHKD) THEN
         MODOPS(10) = 'WARNCHKD'
      END IF

      IF (FASTALL) THEN
         MODOPS(11) = 'FASTALL'
      ELSE IF (FASTAREA) THEN
         MODOPS(11) = 'FASTAREA'
      ELSE IF (NOWARN) THEN
         MODOPS(11) = 'NOWARN'
      END IF

      IF (SCREEN) MODOPS(12) = 'SCREEN'
      IF (MULTYR) MODOPS(13) = 'MULTYR'

      IF (ARDPLETE) THEN
         MODOPS(14) = 'AREADPLT'
      ELSE IF (ROMBERG) THEN
         MODOPS(14) = 'ROMBERG'
      ELSE IF (DDPLETE) THEN
         MODOPS(14) = 'DRYDPLT'
      ELSE IF (.NOT.DDPLETE) THEN
         MODOPS(14) = 'NODRYDPLT'
      END IF

      IF (WDPLETE) THEN
         MODOPS(15) = 'WETDPLT'
      ELSE IF (.NOT.WDPLETE) THEN
         MODOPS(15) = 'NOWETDPLT'
      END IF

      IF (SCIM) MODOPS(16) = 'SCIM'

      IF (RUNTTRM2) THEN
         IF (PVMRM) THEN
            MODOPS(17) = 'TTRM2_PVMRM'
         ELSE IF (OLM) THEN
            MODOPS(17) = 'TTRM2_OLM'
         ELSE IF (ARM2) THEN
            MODOPS(17) = 'TTRM2_ARM2'
         END IF
      ELSE IF (PVMRM) THEN
         MODOPS(17) = 'PVMRM'
      ELSE IF (OLM) THEN
         MODOPS(17) = 'OLM'
      ELSE IF (ARM2) THEN
         MODOPS(17) = 'ARM2'
      ELSE IF (GRSM) THEN
         MODOPS(17) = 'GRSM'
      ELSE IF (RUNTTRM) THEN
         MODOPS(17) = 'TTRM'
      END IF

      IF (PSDCREDIT) THEN
         MODOPS(18) = 'PSDCREDIT'
      ENDIF

! ---    Add labels for non-DFAULT ALPHA and BETA Options
      IF (.NOT. DFAULT .and. L_ALPHA) THEN
         MODOPS(19) = 'ALPHA'
      ELSE IF (.NOT. DFAULT .and. BETA) THEN
         MODOPS(19) = 'BETA'
      ELSE IF (DFAULT .and. BETA) THEN
         CALL ERRHDL(PATH,MODNAM,'E','204','BETA')
      ELSE IF (DFAULT .and. L_ALPHA) THEN
         CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA')
      END IF

! ---    Add MODOPS field for RURAL, URBAN, or RUR&URB
      IF (NURBSRC == 0) THEN
! ---       No URBAN sources
         MODOPS(20) = 'RURAL'
      ELSEIF (NURBSRC == NSRC) THEN
! ---       All sources are URBAN
         MODOPS(20) = 'URBAN'
      ELSE
! ---       Urban and Rural sources
         MODOPS(20) = 'Urb&Rur'
      END IF

!---     Add label for NoUrbTran Non-regulatory Option
      IF (.NOT. L_UrbanTransition) THEN
         MODOPS(21) = 'NoUrbTran'
      END IF

      IF (L_VectorWS) THEN
         MODOPS(23) = 'VectorWS'
      END IF

!CRCO 1/7/21 D074 Add NoMinO3 to summary of model options
!Question - MODOPS seems to have double setting of some entries. In
!meset MODOPS(23) is also set (along with 24). Then in metext 26 & 26 are
!set. So setting to 28 here, as it seems to be the largest of the array not set
!(current array size is 30). Delete this extra comment during review.
      IF (NOMINO3) THEN
         MODOPS(28) = 'NoMinO3'
      END IF

!**  Added for Aircraft Plume Rise; UNC-IE
! ---    Add MODOPS field for Aircraft
      IF (NAFTSRC == NSRC .or. NAFTSRC > 0.0D0) THEN
! ---       Aircraft sources
         MODOPS(24) = 'AIRCRAFT'
!             Check for ALPHA and DFAULT conflicts
!WSP begin --- D151 ALPHA requirement MGS 5/18/2023
!WSP              IF (.NOT. BETA) THEN
!WSPC               WRITE Error Message     ! BETA Option Required for ARCFT
!WSP                CALL ERRHDL(PATH,MODNAM,'E','199','ARCFTOPT')
!WSP              END IF
!WSP          CHECK ALPHA ON MODELOPT
         IF (.NOT. L_ALPHA) THEN
!               WRITE Error Message     ! ALPHA Option Required for ARCFT
            CALL ERRHDL(PATH,MODNAM,'E','198','ARCFTOPT')
         END IF
!WSP end  --- D151 ALPHA requirement MGS 5/18/2023
         IF (DFAULT) THEN
!               WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204','ARCFTOPT')
         END IF
      END IF
!**  End Aircraft Plume Rise insert; April 2023

! Added for HBP, JAN 2023
      IF (HBPLUME) THEN
         MODOPS(29) = 'HBP'
      END IF
! End HBP insert


! ---    Note that the non-DFAULT/BETA options of using met data processed
!        with the ADJ_U* option in AERMET or using MMIF-generated met inputs
!        are identified based on flags included in the surface file header.

      IF (SCIM .and. NUMAVE>0) THEN
!           Write Error Message:  Cannot use SCIM with short term averages
         CALL ERRHDL(PATH,MODNAM,'E','154','ST AVES')
      END IF
      IF (SCIM .and. PERIOD) THEN
!           Write Error Message:  Cannot use SCIM with PERIOD average
         CALL ERRHDL(PATH,MODNAM,'E','154','PERIOD')
      END IF
      IF (SCIM .and. DEPOS) THEN
!           Write Warning Message:  Ignore DEPOS when using SCIM
         DEPOS = .FALSE.
         NUMTYP = NUMTYP - 1
         CALL ERRHDL(PATH,MODNAM,'W','156',' DEPOS ')
      END IF

!        Adjust output label for ANNUAL average deposition fluxes
      IF (ANNUAL) THEN
         DO ITYP = 1, NUMTYP
            IF (.NOT.CONC .or. ITYP>1) THEN
               PERLBL(ITYP) = 'GRAMS/M**2/YR'
            END IF
         END DO
      END IF

! ---    Check for completeness of O3 data inputs
      IF (L_O3Sector) THEN
! ---       Check for temporally varying O3VALUES concentrations, OZONEVAL, and/or O3FILEs by sector.
         DO I = 1, NUMO3Sects
            IF (L_O3File(I)) THEN
!                 O3FILE for this sector; check for O3VALUES
               IF (L_O3VALUES(I)) THEN
!                    O3VALUES values available for this sector; check completeness
                  IF (IO3SET(I) < IO3MAX(I)) THEN
!                       WRITE Error Message: Not Enough O3VALUES values
                     IF (IO3SET(I) < 100) THEN
                        WRITE(DUMMY,'(''O3SECT'',I1,'' N='',I2)')&
                        &I, IO3SET(I)
                     ELSE
                        WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')&
                        &I, IO3SET(I)
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                  END IF
               ELSE IF (.NOT. L_O3VALUES(I).and.&
               &.NOT. L_O3VAL(I)) THEN
! ---                WRITE Warning Message: HOURLY O3FILE but no O3VALs avail for this Sector;
!                    full conversion assumed for missing hourly data (subject to equilibrium
!                    ratio)
                  WRITE(DUMMY,'(''O3SECT'',I1)') I
                  CALL ERRHDL(PATH,MODNAM,'W','271',DUMMY)
               END IF
            ELSE IF (.NOT. L_O3File(I)) THEN
!                 No HOURLY O3 file available for this sector; check for O3VALUES
               IF (L_O3VALUES(I)) THEN
!                    O3VALUES values available for this sector; check completeness
                  IF (IO3SET(I) < IO3MAX(I)) THEN
!                       WRITE Error Message: Not Enough O3VALUES values
                     IF (IO3SET(I) < 100) THEN
                        WRITE(DUMMY,'(''O3SECT'',I1,'' N='',I2)')&
                        &I, IO3SET(I)
                     ELSE
                        WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')&
                        &I, IO3SET(I)
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                  END IF
               ELSE IF (.NOT. L_O3VALUES(I).and.&
               &.NOT. L_O3VAL(I)) THEN
!                    WRITE Error Message: No O3VALUES values and no O3FILE - Missing Sector
                  WRITE(DUMMY,'(''O3SECT'',I1,'' Msg'')') I
                  CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
               END IF
            END IF
         END DO
      ELSE ! .NOT. L_O3Sector
! ---       No O3SECTORs, check for temporally varying O3VALUES concentrations and/or O3FILE
!           Set sector index (I) to 1
         I = 1
         IF (L_O3File(I)) THEN
!              O3FILE for this sector; check for O3VALUES
            IF (L_O3VALUES(I)) THEN
!                 O3VALUES values available for this sector; check completeness
               IF (IO3SET(I) < IO3MAX(I)) THEN
!                    WRITE Error Message: Not Enough O3VALUES values
                  WRITE(DUMMY,'(''NumVals='',I4)') IO3SET(I)
                  CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
               END IF
            ELSE IF (.NOT. L_O3VALUES(I) .and.&
            &.NOT. L_O3VAL(I)) THEN
! ---             WRITE Warning Message: HOURLY O3FILE but no O3VALs avail for this Sector;
!                 full conversion assumed for missing hourly data (subject to equilibrium
!                 ratio)
               CALL ERRHDL(PATH,MODNAM,'W','271',' ')
            END IF
         ELSE IF (.NOT. L_O3File(I)) THEN
            IF (L_O3VALUES(I)) THEN
!                 No O3FILE, but O3VALUES values available for this sector; check completeness
               IF (IO3SET(I) < IO3MAX(I)) THEN
!                    WRITE Error Message: Not Enough O3VALUES values
                  WRITE(DUMMY,'(''NumVals='',I4)') IO3SET(I)
                  CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
               END IF
            END IF
         END IF

      END IF

! ---    CERC 11/30/20 Check for completeness of NOx data inputs
      IF(L_NOxSector)THEN
! ---      Check for temporally varying NOXVALUE, NOX_VALS, NOX_FILE by sector.
         DO I = 1, NUMNOxSects
            IF (L_NOxFile(I)) THEN
!                 NOX_FILE for this sector; check for NOX_VALS
               IF (L_NOX_VALS(I)) THEN
!                    NOX_VALS values available for this sector; check completeness
                  IF (INOXSET(I) < INOXMAX(I)) THEN
!                       WRITE Error Message: Not Enough NOX_VALS values
                     IF (INOXSET(I) < 100) THEN
                        WRITE(DUMMY,'(''NOXSCT'',I1,'' N='',I2)')&
                        &I, INOXSET(I)
                     ELSE
                        WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')&
                        &I, INOXSET(I)
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                  END IF
               ELSE IF (.NOT. L_NOX_VALS(I).and.&
               &.NOT. L_NOXVALUE(I)) THEN
! ---                WRITE Warning Message: HOURLY NOX_FILE but no NOX_VALS avail for this Sector;
!                    give zero concentration
                  WRITE(DUMMY,'(''NOXSECT'',I1)') I
                  CALL ERRHDL(PATH,MODNAM,'W','611',DUMMY)
               END IF
            ELSE IF (.NOT. L_NOxFile(I)) THEN
!                 No HOURLY NOx file available for this sector; check for NOX_VALS
               IF (L_NOX_VALS(I)) THEN
!                    NOX_VALS avaialable for this sector; check completeness
                  IF(INOXSET(I) < INOXMAX(I))THEN
!                       WRITE Error Message: Not enough NOX_VALS values
                     IF (INOXSET(I) < 100) THEN
                        WRITE(DUMMY, '(''NOXSCT'',I1,'' N='',I2)')&
                        &I, INOXSET(I)
                     ELSE
                        WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')&
                        &I, INOXSET(I)
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                  END IF
               ELSE IF (.NOT. L_NOX_VALS(I).and.&
               &.NOT. L_NOXVALUE(I)) THEN
!                    WRITE Error Message: No NOX_VALS values and no NOXFILE - Missing Sector
                  WRITE(DUMMY,'(''NOXSECT'',I1,'' Msg'')') I
                  CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
               END IF
            END IF
         END DO
      ELSE ! .NOT. L_NOxSector
! ---      NO NOxSectors, check for temporally-varying NOX_VALS concentrations
!          Set sector index (I) to 1
         I = 1
         IF (L_NOxFile(I)) THEN
!              NOX_FILE for this sector; check for NOX_VALS
            IF (L_NOX_VALS(I)) THEN
!                 NOX_VALS values available for this sector; check completeness
               IF (INOXSET(I) < INOXMAX(I)) THEN
!                    WRITE Error Message: Not Enough NOX_VALS values
                  WRITE(DUMMY,'(''NumVals='',I4)') INOXSET(I)
                  CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
               END IF
            ELSE IF (.NOT. L_NOX_VALS(I) .and.&
            &.NOT. L_NOXVALUE(I)) THEN
! ---             WRITE Warning Message: HOURLY NOX_FILE but no NOXVALs avail for this Sector;
!                 give zero concentration
               CALL ERRHDL(PATH,MODNAM,'W','611',' ')
            END IF
         ELSE IF (.NOT. L_NOXFile(I)) THEN
            IF(L_NOX_VALS(I))THEN
!                 No NOXFILE but NOX_VALS values available for this sector; check completeness
               IF (INOXSET(I) < INOXMAX(I)) THEN
!                    WRITE Error Message: Not Enough NOX_VALS values
                  WRITE(DUMMY,'(''NumVals'',I4)') INOXSET(I)
                  CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
               END IF
            END IF
         END IF
      END IF

! ---    Check for user-specified ozone units; apply default if needed
      IF (ICSTAT(27) /= 1) THEN
         OzoneUnits = 'PPB'
      END IF

! ---    CERC 11/30/20 Check for user-specified NOx units; apply default if needed
      IF (ICSTAT(38) /= 1) THEN
         NOxUnits = 'PPB'
      END IF

! ---    Check for PM25 processing
      IF ((POLLUT == 'PM25'  .or. POLLUT == 'PM-2.5' .or.&
      &POLLUT == 'PM-25' .or. POLLUT == 'PM2.5')) THEN
         IF(.NOT.L_NO_PM25AVE .and. .NOT.NOCHKD .and.&
         &.NOT.L_WARNCHKD   .and. .NOT.EVONLY) THEN
! ---          Set logical flag for PM25 processing, averaged across years
            PM25AVE = .TRUE.
! ---          Now check for appropriate averaging periods for PM2.5
            IF (NUMAVE>1 .or. (NUMAVE==1 .and.&
            &KAVE(1)/=24)) THEN
! ---             Write Error Message: Short Term average must be 24-hr only
               DO I = 1, NUMAVE
                  IF (KAVE(I) /= 24) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'E','363',DUMMY)
                  END IF
               END DO
               PM25AVE = .FALSE.
            END IF
            IF (PERIOD) THEN
! ---             Write Error Message: Long term average must be ANNUAL
               CALL ERRHDL(PATH,MODNAM,'E','363','PERIOD Ave')
               PM25AVE = .FALSE.
            END IF
         ELSE IF (.NOT.SCREEN .and. .NOT.EVONLY) THEN
! ---          Set to false for NOCHKD or WARNCHKD options, without the SCREEN or
!              EVONLY options, and issue warning message
            IF (NOCHKD) THEN
               DUMMY = 'NOCHKD'
            ELSE IF (L_WARNCHKD) THEN
               DUMMY = 'WARNCHKD'
            END IF
            CALL ERRHDL(PATH,MODNAM,'W','363',DUMMY)
            PM25AVE = .FALSE.
         ELSE IF (SCREEN .or. EVONLY .or. L_NO_PM25AVE) THEN
! ---          Set PM25AVE to .FALSE. for SCREEN or EVONLY options or with
!              L_NO_PM25AVE option, without any warnings
            PM25AVE = .FALSE.
         END IF
      END IF

! ---    Check for NO2 1-hour NAAQS processing (NO2AVE = .T.)
      IF (POLLUT == 'NO2' .and. .NOT.L_NO_NO2AVE .and.&
      &.NOT.NOCHKD .and. .NOT.L_WARNCHKD  .and.&
      &.NOT.SCREEN .and. .NOT.EVONLY) THEN
! ---       No options precluding NO2 1-hr NAAQS processing are specified;
!           next check for averaging periods to determine if multi-year
!           processing of maximum daily 1-hour averages is being done
         IF (NUMAVE==1 .and. KAVE(1)==1 .and. .NOT.PERIOD&
         &.and. .NOT.ANNUAL) THEN
! ---          Set logical flag for 1-hr NO2 processing, averaged across years,
!              without PERIOD or ANNUAL averages
            NO2AVE = .TRUE.
         ELSE IF (NUMAVE==1 .and. KAVE(1)==1 .and. PERIOD&
         &.and. MULTYR) THEN
! ---          Set logical flag for 1-hr NO2 processing, averaged across years,
!              using MULTYEAR option to address PERIOD averages
            NO2AVE = .TRUE.
         ELSE IF (NUMAVE==1 .and. KAVE(1)==1 .and.&
         &(PERIOD .or. ANNUAL) .and.&
         &.NOT.MULTYR) THEN
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for NO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified.
            CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
! ---          Allow processing to continue, but long-term results
!              may be wrong.
            NO2AVE = .TRUE.
         ELSE IF (NUMAVE>1 .or.&
         &(NUMAVE==1 .and. KAVE(1)/=1) .and.&
         &(PERIOD .or. ANNUAL) .and.&
         &.NOT.MULTYR) THEN
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for NO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified.  If NUMAVE > 1 then a non-standard short-term
!              average is being selected, and short-term averages other than
!              1-hour cannot be processed with the 1-hr NO2 NAAQS.
            CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
! ---          Write Warning Message: Non-standard short term average for NO2,
!              and disable special processing for 1-hour NAAQS.
            DO I = 1, NUMAVE
               IF (KAVE(I) /= 1) THEN
                  WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                  CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               END IF
            END DO
            NO2AVE = .FALSE.
         ELSE IF (NUMAVE>1 .or.&
         &(NUMAVE==1 .and. KAVE(1)/=1) ) THEN
! ---          Write Warning Message: Non-standard short term average for NO2
! ---          Write Warning Message: Short Term average should be 1-hr only for NO2
            DO I = 1, NUMAVE
               IF (KAVE(I) /= 1) THEN
                  WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                  CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               END IF
            END DO
            NO2AVE = .FALSE.
         ELSE
! ---          Period or Annual average only, set NO2AVE = .F. but allow
!              processing
            NO2AVE = .FALSE.
         END IF
      ELSE IF (POLLUT == 'NO2' .and. .NOT.SCREEN .and.&
      &.NOT.EVONLY) THEN
! ---       Set NO2AVE to false for NOCHKD, WARNCHKD and L_NO_NO2AVE options, without
!           the SCREEN or EVONLY options; issue warning messages for NOCHKD or WARNCHKD
!           (message has already been issued for L_NO_NO2AVE), and disable special
!           processing for 1-hour NAAQS.
         IF (NUMAVE==1 .and. KAVE(1)==1 .and.&
         &((.NOT.PERIOD .and. .NOT.ANNUAL) .or.&
         &(PERIOD .and. MULTYR) .or.&
         &((PERIOD .or. ANNUAL) .and. .NOT.MULTYR)) )THEN
            IF (NOCHKD) THEN
               DUMMY = 'NOCHKD'
               CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
            ELSE IF (L_WARNCHKD) THEN
               DUMMY = 'WARNCHKD'
               CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
            END IF
            NO2AVE = .FALSE.
         END IF
      ELSE IF (POLLUT == 'NO2' .and. (SCREEN .or. EVONLY)) THEN
! ---       Set NO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
         NO2AVE = .FALSE.
      END IF

! ---    Check for SO2 1-hour NAAQS processing (SO2AVE = .T.)
      IF (POLLUT == 'SO2' .and. .NOT.NOCHKD .and. .NOT.L_NO_SO2AVE&
      &.and. .NOT.L_WARNCHKD .and. .NOT.EVONLY) THEN
! ---       No options precluding SO2 1-hr NAAQS processing are specified;
!           next check for averaging periods to determine if multi-year
!           processing of maximum daily 1-hour averages is being done
         IF (NUMAVE==1 .and. KAVE(1)==1 .and. .NOT.PERIOD&
         &.and. .NOT.ANNUAL) THEN
! ---          Set logical flag for 1-hr SO2 processing, averaged across years
            SO2AVE = .TRUE.
         ELSE IF (NUMAVE==1 .and. KAVE(1)==1 .and. PERIOD&
         &.and. MULTYR) THEN
! ---          Set logical flag for 1-hr SO2 processing, averaged across years,
!              using MULTYEAR option to address PERIOD averages
            SO2AVE = .TRUE.
         ELSE IF (NUMAVE==1 .and. KAVE(1)==1 .and.&
         &(PERIOD .or. ANNUAL) .and.&
         &.NOT.MULTYR) THEN
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for SO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified.
            CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
! ---          Allow processing to continue, but long-term results
!              may be wrong.
            SO2AVE = .TRUE.
         ELSE IF (NUMAVE>1 .and. MINVAL(KAVE)==1 .and.&
         &(PERIOD .or. ANNUAL) .and.&
         &.NOT.MULTYR) THEN
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for SO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified. Also, short-term averages other than
!              1-hour cannot be processed with the 1-hr SO2 NAAQS.
            CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
! ---          Write Warning Message: Non-standard short term average for SO2,
!              and disable special processing for 1-hour NAAQS.
            DO I = 1, NUMAVE
               IF (KAVE(I) /= 1) THEN
                  WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                  CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               END IF
            END DO
! ---          Allow processing to continue, but without special processing
!              of 1-hr values averaged across years
            SO2AVE = .FALSE.
         ELSE IF (NUMAVE>1 .and. MINVAL(KAVE)==1) THEN
! ---          Write Warning Message: Non-standard short term average for SO2,
!              and disable special processing for 1-hour NAAQS.
            DO I = 1, NUMAVE
               IF (KAVE(I) /= 1) THEN
                  WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                  CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               END IF
            END DO
! ---          Allow processing to continue, but without special processing
!              of 1-hr values averaged across years
            SO2AVE = .FALSE.
         ELSE
! ---          Period or Annual average only, set SO2AVE = .F. but allow
!              processing
            SO2AVE = .FALSE.
         END IF
      ELSE IF (POLLUT == 'SO2' .and. .NOT.SCREEN .and.&
      &.NOT.EVONLY) THEN
! ---       Set SO2AVE to false for NOCHKD or WARNCHKD options, without the
!           SCREEN or EVONLY options, issue warning message, and disable
!           special processing for 1-hour NAAQS.
         IF (NUMAVE==1 .and. KAVE(1)==1 .and.&
         &((.NOT.PERIOD .and. .NOT.ANNUAL) .or.&
         &(PERIOD .and. MULTYR) .or.&
         &((PERIOD .or. ANNUAL) .and. .NOT.MULTYR)) )THEN
            IF (NOCHKD) THEN
               DUMMY = 'NOCHKD'
            ELSE IF (L_WARNCHKD) THEN
               DUMMY = 'WARNCHKD'
            END IF
            CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
            SO2AVE = .FALSE.
         END IF
      ELSE IF (POLLUT == 'SO2' .and. (SCREEN .or. EVONLY)) THEN
! ---       Set SO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
         SO2AVE = .FALSE.
      END IF

! ---    Check for pollutant ID = 'NO2' for PVMRM, OLM, ARM2 and GRSM options
      IF ((PVMRM .or. OLM .or. ARM2 .or. GRSM) .and.&
      &POLLUT /= 'NO2') THEN
!           Write Error Message:  Pollutant ID doesn't match option
         CALL ERRHDL(PATH,MODNAM,'E','284',' NO2 ')
      END IF

! ---    Check for PM25, NO2, or SO2 processing based on ranked values
!        averaged across years, and adjust PLOTFILE format accordingly
      IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
         PLTFRM = '(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,&
         &               10(F13.5,2X,I8.8,2X:))'
      END IF

! ---    AWMA enhancements to PRIME
! ---    AWMADWNW and ORD_DWNW and have different ways to define
!          the height at which to compute the effective wind speed.
!          One or the other can be specified, or neither, but BOTH CANNOT
!          be specified.  Check for that here.  A similar check is performed
!          in ORD_DOWNWASH

      IF (L_AWMA_UEFF .and. L_ORD_UEFF) THEN
! ---       Write error message
         CALL ERRHDL(PATH,MODNAM,'E','124','  ')
      END IF

! ---    Check for conflicts between AWMA and ORD Ueff options (which
!         is an error) as well as AWMAUTurb and AWMAUturbHX (which only
!         issues a warning)
      IF (L_AWMA_UTurb .and. L_AWMA_UTurbHX) THEN
         CALL ERRHDL(PATH,MODNAM,'W','478',' ')
      END IF

      GO TO 1000

!        WRITE Error Message for Error Opening File
99    CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
      IF (DUMMY == 'SAVEFILE') THEN
!           Reset Logical Flag for SAVEFILE Option Due to Error Opening File
         RSTSAV = .FALSE.
      ELSE IF (DUMMY == 'INITFILE') THEN
!           Reset Logical Flag for INITFILE Option Due to Error Opening File
         RSTINP = .FALSE.
      END IF

1000  CONTINUE

   ELSE
!        Write Error Message: Invalid Keyword for This Pathway
      CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
   END IF

999 RETURN
END SUBROUTINE COCARD

SUBROUTINE TITLES
!***********************************************************************
!                 TITLES Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Title Information From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Title Strings for Model Outputs
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'TITLES'

   IF (KEYWRD == 'TITLEONE') THEN
      TITLE1 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),&
      &(LOCE(2)+2+ILEN_FLD-1)))
      IF (TITLE1 == ' ') THEN
!*          Write Error Message: Missing Parameter Title
         CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
      END IF

   ELSE IF (KEYWRD == 'TITLETWO') THEN
      TITLE2 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),&
      &(LOCE(2)+2+ILEN_FLD-1)))
      IF (TITLE2 == ' ') THEN
!*          Write Warning Message
         CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
      END IF

   END IF

   RETURN
END SUBROUTINE TITLES


SUBROUTINE MODOPT
!***********************************************************************
!                 MODOPT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Modeling Options From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Added MODELOPT keyword RLINEFDH.
!                    Default FAC_DISPHT == 5.0D0, if RLINEFDH (& ALPHA)is
!                    used FAC_DISPHT == 0.0D0 to match AERMOD's surface
!                    wind profile.
!                    Michelle G. Snyder, Wood - 6/22/2021
!
!        MODIFIED:   To add GRSM NO2 option
!                    CERC, 11/30/20
!
!        MODIFIED:   To incorporate additional options and adjust
!                    the handling of options that conflict with the
!                    regulatory DFAULT option.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
!
!        MODIFIED:   To incorporate undocumented options to turn off
!                    depletion, which is now the default.
!                    R. W. Brode, MACTEC/PES - 10/26/2004
!
!        MODIFIED:   To allow for calculating CONC/DEPOS/DDEP/WDEP in
!                    a single model run.
!                    R. W. Brode, PES - 4/17/95
!
!        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
!                    to allow just the wet or just the dry deposition flux
!                    to be reported.  DEPOS now reports the sum of wet and
!                    dry fluxes.
!                    D. Strimaitis, SRC - 11/8/93
!
!        MODIFIED:   To add DEPLETE parameter for plume depletion option
!                    D. Strimaitis, SRC - 2/15/93
!
!        MODIFIED:   To Output Warning Message '206' For Overriding
!                    Non-DEFAULT Option - 9/29/92
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Modeling Option Logical Switch Settings
!
!        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
!                          Checks for Invalid Option Keywords;
!                          Checks for Conflicting or Missing Option Keywords
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA, ONLY: FAC_DISPHT !needed for RLINEFDH MODELOPT (Wood 6/22/2021)
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I
   CHARACTER :: KOPT*9

!     Variable Initializations - Initialize All Logical Switches to FALSE
   MODNAM = 'MODOPT'

!     Check for Too Few or Too Many Parameters
   IF (IFC < 3) THEN
!        WRITE Error Message     ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   ELSE IF (IFC > 14) THEN
!        WRITE Warning Message   ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'W','202',KEYWRD)
   END IF

! --- Assign DEFAULT value of 0.2D0 to SVMIN, minimum
!     sigma-v value (formerly assigned as a PARAMETER);
!CRT  value of SVMIN may be increased to 0.5D0 using
!CRT  the ALPHA option and the CO LOW_WIND keyword:
   SVMIN = 0.2D0

! --- Assign DEFAULT value for minimum wind speed of
!     0.2828 m/s, equivalent to SQRT(2*SVMIN*SVMIN)
!     used in previous versions.
!CRT  User can change the value of WSMIN using the ALPHA model
!CRT  option and the CO LOW_WIND keyword.
   WSMIN = 0.2828D0

!CRT  Assign DEFAULT value of 1.0 for the maximum meander
!CRT  factor, FRANMAX. The value of FRANMAX was adjusted
!CRT  to 0.95 under the former LowWind2 BETA option, and the
!CRT  user could modify the value used under the LowWind2
!CRT  option using the CO LOW_WIND keyword, within a
!CRT  range from 0.50 to 1.0, inclusive.
!CRT  1/29/2018 LowWind2 has been removed as a BETA option. The user
!CRT  can now modify the FRANMAX value is using the ALPHA
!CRT  option and the CO LOW_WIND keyword
   FRANMAX = 1.0D0

!**   3/18/2022 add FRANMIN for meander testing
!**   range from 0 to 1. Default value is 0 -Wood
   FRANMIN = 0.0D0

!CRT  CRT 9/11/2020, D062 User Minimum Sigma W
!CRT  Assign DEFAULT value for minimum sigma w of 0.02 m/s
!CRT  (formerly assigned as a PARAMETER;
!CRT  Value can now be assigned by the user using the the ALPHA
!CRT  model option and the CO LOW_WIND keyword.
   SWMIN = 0.02D0

!RCO  RCO 9/27/2020, D061 User BIGT value
!RCO  Assign DEFAULT value for BIGT value of 24 hours
!RCO  (formerly a PARAMETER in MEANDR routine in calc2)
!RCO  Value can now be assigned by the user using the the ALPHA
!RCO  model option and the CO LOW_WIND keyword.
   BIGT = 24.0D0

!MGS Added MODELOPT keyword RLINEFDH, which will change this value
!MGS (from RLINE_v1_2) to =0.0. Wood 6/22/2021
   FAC_DISPHT  = 5.0D0


! --- Assign DEFAULT value of 2.15 for the SZCOEF parameter
!     used in the IBL calculations.
   SZCOEF = 2.15D0

!     First Check for Presence of DFAULT Switch
   DO I = 3, IFC
      KOPT = FIELD(I)
      IF (KOPT == 'DFAULT' .or. KOPT == 'DEFAULT') THEN
         DFAULT      = .TRUE.
         ELEV        = .TRUE.
         FLAT        = .FALSE.
         FLATSRCS    = .FALSE.
         MSGPRO      = .TRUE.
         NOSTD       = .FALSE.
         NOCHKD      = .FALSE.
         SCREEN      = .FALSE.
         SCIM        = .FALSE.
         PSDCREDIT   = .FALSE.
         BETA        = .FALSE.
         L_ALPHA     = .FALSE.
         FASTAREA    = .FALSE.
         FASTALL     = .FALSE.
         L_EFFSIGY   = .FALSE.
         LOW_WIND    = .FALSE.
         L_NonDFAULT = .FALSE.
         L_UrbanTransition = .TRUE.
         GRSM       = .FALSE.
         NOMINO3    = .FALSE.
         L_AREAMNDR   = .FALSE.  !Added area meander flag to the alpha options D128 Wood 6/3/22
         EXIT
      END IF
   END DO

!     Next check for presence of both FLAT and ELEV if NOT.DFAULT
   IF (.NOT. DFAULT) THEN
!        First look for FLAT
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT == 'FLAT') THEN
            FLAT = .TRUE.
            ELEV = .FALSE.
            EXIT
         END IF
      END DO
!        If FLAT, next look for ELEV, indicating both FLAT and
!        ELEV sources in the same run (FLATSRCS)
      IF (FLAT) THEN
         DO I = 3, IFC
            KOPT = FIELD(I)
            IF (KOPT == 'ELEV') THEN
               ELEV     = .TRUE.
               FLATSRCS = .TRUE.
               EXIT
            END IF
         END DO
      END IF
   ELSE
!        Look for FLAT with DFAULT
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT == 'FLAT') THEN
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
         END IF
      END DO
   END IF

!     Next Check for Presence of BETA Switch
   DO I = 3, IFC
      KOPT = FIELD(I)
      IF (KOPT == 'BETA' .and. .NOT.DFAULT) THEN
         BETA = .TRUE.
      ELSE IF (KOPT == 'ALPHA' .and. .NOT.DFAULT) THEN
         L_ALPHA = .TRUE.
      END IF
   END DO

   NUMTYP = 0
!     Loop Through Fields Again Setting All Swithes
   DO I = 3, IFC
      KOPT = FIELD(I)
      IF (KOPT == 'DFAULT' .or. KOPT == 'DEFAULT') THEN
         DFAULT = .TRUE.
      ELSE IF (KOPT == 'CONC') THEN
         IF (.NOT. CONC) THEN
            CONC   = .TRUE.
            NUMTYP = NUMTYP + 1
         END IF
      ELSE IF (KOPT == 'DEPOS') THEN
         IF (.NOT. DEPOS) THEN
            DEPOS  = .TRUE.
            NUMTYP = NUMTYP + 1
         END IF
      ELSE IF (KOPT == 'DDEP') THEN
         IF (.NOT. DDEP) THEN
            DDEP   = .TRUE.
            NUMTYP = NUMTYP + 1
         END IF
      ELSE IF (KOPT == 'WDEP') THEN
         IF (.NOT. WDEP) THEN
            WDEP   = .TRUE.
            NUMTYP = NUMTYP + 1
         END IF
      ELSE IF (KOPT == 'FLAT' .or. KOPT == 'ELEV') THEN
         CYCLE
      ELSE IF (KOPT == 'DRYDPLT' .and. .NOT.NODRYDPLT) THEN
         DDPLETE = .TRUE.
         DRYDPLT = .TRUE.
      ELSE IF (KOPT == 'DRYDPLT' .and. NODRYDPLT) THEN
! ---       Write Error Message        ! Conflicting options specified
         CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
      ELSE IF (KOPT == 'NODRYDPLT' .and. .NOT.DRYDPLT) THEN
!           Dry depletion is now standard - include "option" to override it
         DDPLETE = .FALSE.
!           Set separate logical for user-specified option to ensure that
!           it is reflected in the page header
         NODRYDPLT = .TRUE.
      ELSE IF (KOPT == 'NODRYDPLT' .and. DRYDPLT) THEN
! ---       Write Error Message        ! Conflicting options specified
         CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
      ELSE IF (KOPT == 'ROMBERG') THEN
         ROMBERG = .TRUE.
         DDPLETE = .TRUE.
      ELSE IF (KOPT == 'AREADPLT') THEN
         IF (.NOT. DFAULT) THEN
            ARDPLETE = .TRUE.
            DDPLETE  = .TRUE.
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
      ELSE IF (KOPT == 'WETDPLT' .and. .NOT.NOWETDPLT) THEN
         WDPLETE = .TRUE.
         WETDPLT = .TRUE.
      ELSE IF (KOPT == 'WETDPLT' .and. NOWETDPLT) THEN
! ---       Write Error Message        ! Conflicting options specified
         CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
      ELSE IF (KOPT == 'NOWETDPLT' .and. .NOT.WETDPLT) THEN
!           Wet depletion is now standard - include "option" to override it
         WDPLETE = .FALSE.
!           Set separate logical for user-specified option to ensure that
!           it is reflected in the page header
         NOWETDPLT = .TRUE.
      ELSE IF (KOPT == 'NOWETDPLT' .and. WETDPLT) THEN
! ---       Write Error Message        ! Conflicting options specified
         CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
      ELSE IF (KOPT == 'NOSTD') THEN
         IF (.NOT. DFAULT) THEN
            NOSTD = .TRUE.
         ELSE
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            NOSTD = .FALSE.
         END IF
      ELSE IF (KOPT == 'NOWARN') THEN
         NOWARN = .TRUE.
      ELSE IF (KOPT == 'NOCHKD') THEN
         IF (.NOT. DFAULT .and. .NOT.L_WARNCHKD) THEN
            NOCHKD = .TRUE.
         ELSE IF (.NOT.DFAULT .and. L_WARNCHKD) THEN
! ---          Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            NOCHKD = .FALSE.
         END IF
      ELSE IF (KOPT == 'WARNCHKD') THEN
         IF (.NOT. NOCHKD) THEN
            L_WARNCHKD = .TRUE.
         ELSE
! ---          Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         END IF
      ELSE IF (KOPT == 'SCREEN') THEN
         IF (.NOT. DFAULT) THEN
            SCREEN = .TRUE.
!              Set NOCHKD option on for SCREEN mode
            NOCHKD = .TRUE.
         ELSE
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            SCREEN = .FALSE.
         END IF
      ELSE IF (KOPT == 'SCIM') THEN
         IF (.NOT. DFAULT) THEN
            SCIM = .TRUE.
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            SCIM = .FALSE.
         END IF
      ELSE IF (KOPT == 'TOXICS') THEN
! ---       WRITE Warning Message        ! TOXICS option is obsolete
!           If this run includes area or openpit sources, refer to
!           FASTAREA option and set the logical flag
         IF (NAREA > 0 .or. NLINE > 0 .or. NPIT > 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','198','FASTAREA')
         ELSE
            CALL ERRHDL(PATH,MODNAM,'W','198','        ')
         END IF
         IF (.NOT. DFAULT) THEN
            IF (NAREA > 0 .or. NLINE > 0 .or. NPIT > 0) THEN
! ---             Assign FASTAREA option to TRUE for consistency with
!                 area source optimizations under obsolete TOXICS option
               FASTAREA = .TRUE.
            END IF
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
! Added for Highly Buoyant Plume (HBP) option; JAN 2023
      ELSE IF (KOPT == 'HBP') THEN
         HBPLUME = .TRUE.
         IF (.NOT. L_ALPHA) THEN
!              WRITE Error Message     ! ALPHA Option Required for TTRM2
            CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
!! End TTRM2 insert
         END IF
         IF (DFAULT) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
! End HBP insert
      ELSE IF (KOPT == 'PVMRM') THEN
         PVMRM = .TRUE.
      ELSE IF (KOPT == 'OLM') THEN
         OLM = .TRUE.
      ELSE IF (KOPT == 'TTRM') THEN
         RUNTTRM = .TRUE.
         IF (.NOT. L_ALPHA) THEN
!              WRITE Error Message     ! ALPHA Option Required for TTRM
            CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
         ENDIF
!! Added Nov. 2021
      ELSE IF (KOPT == 'TTRM2') THEN
         RUNTTRM2 = .TRUE.
         RUNTTRM = .TRUE.
         IF (.NOT. L_ALPHA) THEN
!              WRITE Error Message     ! ALPHA Option Required for TTRM2
            CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
!! End TTRM2 insert
         END IF
         IF (DFAULT) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
      ELSE IF (KOPT == 'ARM2') THEN
         ARM2 = .TRUE.
         IF( DFAULT )THEN
            ARM2_Min = 0.50D0
            ARM2_Max = 0.90D0
         ENDIF
      ELSE IF (KOPT == 'GRSM') THEN
!          CERC 11/30/20
!CRT 4/1/2022 GRSM in version 21112 updated from ALPHA to BETA
         GRSM = .TRUE.

!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
!MGS            IF (.NOT. BETA) THEN
!              WRITE Error Message     ! BETA Option Required for GRSM
!MGS               CALL ERRHDL(PATH,MODNAM,'E','199',KOPT)
!MGS            END IF
!MGS            IF (DFAULT) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
!MGS               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
!MGS            END IF
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (end)

!CRT 1/29/2018 Move PSDCREDIT from BETA to ALPHA option
      ELSE IF (KOPT == 'PSDCREDIT') THEN                           ! jop 093006
         IF (L_ALPHA .and. .NOT. DFAULT) THEN
            PSDCREDIT = .TRUE.
         ELSE IF (.NOT. L_ALPHA .and. .NOT. DFAULT) THEN
!              WRITE Error Message     ! ALPHA Option Required for PSDCREDIT
            CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
!CRT 1/29/2018 Remove restriction that user can only specify ALPHA or BETA
      ELSE IF (KOPT == 'ALPHA') THEN
!CRT            IF (BETA) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
!CRT               CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA & BETA')
!CRT            ELSE IF (.NOT. DFAULT) THEN
         IF (.NOT. DFAULT) THEN
            L_ALPHA = .TRUE.
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
!CRT 1/29/2018 Remove restriction that user can only specify ALPHA or BETA
      ELSE IF (KOPT == 'BETA') THEN
!CRT            IF (L_ALPHA) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
!CRT               CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA & BETA')
!CRT            ELSE IF (.NOT. DFAULT) THEN
         IF (.NOT. DFAULT) THEN
            BETA = .TRUE.
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF

      ELSE IF (KOPT == 'FASTAREA') THEN
         IF (FASTALL) THEN
!              Issue warning message since FASTALL implies FASTAREA
            CALL ERRHDL(PATH,MODNAM,'W','192','        ')
         END IF
         IF (.NOT. DFAULT) THEN
!              Set logical flag for FASTAREA for optimized area source;
!              equivalent to optimizations associated with obsolete TOXICS option
            FASTAREA = .TRUE.
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
      ELSE IF (KOPT == 'FASTALL') THEN
         IF (FASTAREA) THEN
!              Issue warning message since FASTALL implies FASTAREA
            CALL ERRHDL(PATH,MODNAM,'W','192','        ')
         END IF
         IF (.NOT. DFAULT) THEN
! ---          Set logical flag for FASTAREA for optimized area source;
!              equivalent to optimizations associated with obsolete TOXICS option.
            FASTAREA = .TRUE.
! ---          Also set L_EFFSIGY option flag for optimized meander option for
!              point and volume sources.
            FASTALL   = .TRUE.
            L_EFFSIGY = .TRUE.
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF
      ELSE IF (KOPT == 'NOURBTRAN') THEN
         IF (.NOT. DFAULT) THEN
! ---          Non-regulatory option to ignore transition from nighttime urban
!              enhanced boundary layer to daytime convective boundary
! ---          Set logical switch to account for urban transition to .F. and
!              issue warning message
            L_UrbanTransition = .FALSE.
!              WRITE Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','151','Keyword     ')
         ELSE
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF

! -----  Add option for use of vector-mean wind speeds, VECTORWS
      ELSE IF (KOPT == 'VECTORWS') THEN
         L_VECTORWS = .TRUE.
! ---       Write Warning Message
         CALL ERRHDL(PATH,MODNAM,'W','116',KOPT)

! -----  Add ALPHA option for low-wind-speed modifications, LOWWIND
      ELSE IF (KOPT == 'LOW_WIND') THEN
         IF (L_ALPHA) THEN
! ---          Assign LOW_WIND = .TRUE.
            LOW_WIND = .TRUE.
            CALL ERRHDL(PATH,MODNAM,'W','136','NonDFAULT')
            DFAULT = .FALSE.
         ELSE
            LOW_WIND = .FALSE.
            CALL ERRHDL(PATH,MODNAM,'E','133','NonDFAULT')
         END IF

! -----  Wood 6/3/22 D128 Add ALPHA option for area meander , AREAMNDR
      ELSE IF (KOPT == 'AREAMNDR') THEN
         IF (L_ALPHA) THEN
! ---          Assign AREAMNDR = .TRUE.
            L_AREAMNDR = .TRUE.
            CALL ERRHDL(PATH,MODNAM,'W','134','NonDFAULT')
            DFAULT = .FALSE.
         ELSE
            L_AREAMNDR = .FALSE.
            CALL ERRHDL(PATH,MODNAM,'E','139','NonDFAULT')
         END IF

         IF (.NOT.DFAULT) THEN
!              WRITE Error Message     ! ALPHA option required
            CALL ERRHDL(PATH,MODNAM,'W','198',KOPT)
         ELSE
!              WRITE Error Message     ! DFAULT option specified;
!                                        Issue ERROR message
            CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
         END IF


! ---       Assign minimum wind speed, WSMIN
! WSMIN is already set above, approx. line 1241. Currently no "default"
! value for the LOW_WIND option specifically (which would be set here).
         WSMIN = 0.2828D0

! ---       Add option for RLINE to use zero displacement height in wind profile
!            Michelle G. Snyder (Wood 6/22/2021)
      ELSE IF (KOPT == 'RLINEFDH') THEN
         IF (L_ALPHA) THEN !Require ALPHA on MODELOPT
            FAC_DISPHT = 0.0D0
         ELSE
            CALL ERRHDL(PATH,MODNAM,'E','198','RLINEFDH')
         END IF

!RCO 1/7/21 D074 Add NOMINO3 option
      ELSE IF (KOPT == 'NOMINO3') THEN
         NOMINO3 = .TRUE.
         IF (.NOT.OLM .and. .NOT.PVMRM .and. .NOT.GRSM&
         &.and. .NOT.RUNTTRM) THEN
!              WRITE Warning Message  ! NOMINO3 set without NO2 technique
!              D154 Changed the warning message to an error is NOMINO3 is set without NO2 technique 1/31/23 WSP
!               CALL ERRHDL(PATH,MODNAM,'W','621',KOPT)
            CALL ERRHDL(PATH,MODNAM,'E','621',KOPT)
         END IF

      ELSE !Not a vaild model option
!           WRITE Error Message     ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',KOPT)


      END IF

   END DO

! --- Check for conflicting NO2 options:
   IF (OLM .and. PVMRM) THEN
!        WRITE Error Message       ! Can't specify OLM & PVMRM
      CALL ERRHDL(PATH,MODNAM,'E','141','OLM & PVMRM')
   ELSE IF (OLM .and. ARM2) THEN
!        WRITE Error Message       ! Can't specify OLM & ARM2
      CALL ERRHDL(PATH,MODNAM,'E','141','OLM & ARM2')
   ELSE IF (OLM .and. GRSM) THEN
!        WRITE Error Message       ! Can't specify OLM & GRSM
      CALL ERRHDL(PATH,MODNAM,'E','141','OLM & GRSM')
   ELSE IF (PVMRM .and. ARM2) THEN
!        WRITE Error Message       ! Can't specify PVMRM & ARM2
      CALL ERRHDL(PATH,MODNAM,'E','141','PVMRM & ARM2')
   ELSE IF (PVMRM .and. GRSM) THEN
!        WRITE Error Message       ! Can't specify PVMRM & GRSM
      CALL ERRHDL(PATH,MODNAM,'E','141','PVMRM & GRSM')
   ELSE IF (ARM2 .and. GRSM) THEN
!        WRITE Error Message       ! Can't specify ARM2 & GRSM
      CALL ERRHDL(PATH,MODNAM,'E','141','ARM2 & GRSM')
   ELSE IF ((.NOT. RUNTTRM2) .and. (RUNTTRM .and. ARM2)) THEN
!        WRITE Error Message       ! Can't specify TTRM & ARM2
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & ARM2')
   ELSE IF ((.NOT. RUNTTRM2) .and. (RUNTTRM .and. OLM)) THEN
!        WRITE Error Message       ! Can't specify TTRM & OLM
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & OLM')
   ELSE IF ((.NOT. RUNTTRM2) .and. (RUNTTRM .and. PVMRM)) THEN
!        WRITE Error Message       ! Can't specify TTRM & PVMRM
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & PVMRM')
   ELSE IF (RUNTTRM .and. GRSM) THEN
!        WRITE Error Message       ! Can't specify TTRM & GRSM
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & GRSM')
!! Added Nov. 2021
   ELSE IF (RUNTTRM2 .and. GRSM ) THEN
!        WRITE Error Message       ! Can't specify TTRM & ARM2
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & GRSM')
   ELSE IF (RUNTTRM2 .and. OLM .and. ARM2) THEN
!        WRITE Error Message       ! Can't specify TTRM & OLM
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & >2 NO2 Options')
   ELSE IF (RUNTTRM2 .and. PVMRM .and. ARM2) THEN
!        WRITE Error Message       ! Can't specify TTRM & PVMRM
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & >2 NO2 Options')
   ELSE IF (RUNTTRM2 .and. OLM .and. PVMRM) THEN
!        WRITE Error Message       ! Can't specify TTRM & GRSM
      CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & >2 NO2 Options')
!! End Nov. 2021 TTRM insert
   END IF

   IF (PSDCREDIT .and. .NOT.PVMRM) THEN
!        WRITE Error Message       ! Can't specify PSDCREDIT without PVMRM
      CALL ERRHDL(PATH,MODNAM,'E','143','PSDCREDIT')
   END IF

   IF (PSDCREDIT .and. EVONLY) THEN
!        WRITE Error Message       ! Can't use PSDCREDIT with EVONLY Processing
      CALL ERRHDL(PATH,MODNAM,'E','147',' EVENTS ')
   END IF

! --- Check for Non-DFAULT options used without DFAULT; this will be used to
!     set the option label in the file headers.
!_OLD      IF (.NOT. DFAULT .and.
!_OLD     &    (FLAT .or. FLATSRCS .or. ARDPLETE .or. NOSTD .or. NOCHKD .or.
!_OLD     &     SCREEN .or. SCIM .or. PVMRM .or. PSDCREDIT .or.
!_OLD     &      OLM .or. ARM2 .or. BETA .or. FASTAREA .or. FASTALL .or.
!_OLD     &     L_LowWind1 .or. L_LowWind2 .or. L_LowWind3
!_OLD     &                                .or. .NOT.L_UrbanTransition)) THEN
!_OLD
!_OLD         L_NonDFAULT = .TRUE.
!_OLD      END IF
!CRT  1/29/2018: Added L_ALPHA to conditions for non-default options
!CRT  4/11/2022: D131 FRAN Alpha - Add L_PBal to non-default options
   IF (.NOT. DFAULT .and.&
   &(FLAT .or. FLATSRCS .or. ARDPLETE .or. NOSTD .or. NOCHKD .or.&
   &SCREEN .or. SCIM .or. PSDCREDIT .or. BETA .or. FASTAREA .or.&
   &FASTALL .or. LOW_WIND .or. L_ALPHA .or. GRSM .or. L_PBal&
   &.or. .NOT.L_UrbanTransition)) THEN

      L_NonDFAULT = .TRUE.
   END IF

!     Setup Label Array for Concentration and Depositions
   IF (NUMTYP > NTYP) THEN
!        WRITE Error Message: Number of output types exceeds maximum
      WRITE(DUMMY,'(I4)') NTYP
      CALL ERRHDL(PATH,MODNAM,'E','280',DUMMY)
   ELSE IF (NUMTYP == 0) THEN
!        WRITE Warning Message: No Output Types Selected, Assume CONC Only
      CALL ERRHDL(PATH,MODNAM,'W','205','CONC')
      NUMTYP = 1
      ITYP   = 1
      CONC   = .TRUE.
      CHIDEP(1,ITYP) = 'AVER'
      CHIDEP(2,ITYP) = 'AGE '
      CHIDEP(3,ITYP) = 'CONC'
      CHIDEP(4,ITYP) = 'ENTR'
      CHIDEP(5,ITYP) = 'ATIO'
      CHIDEP(6,ITYP) = 'N   '
      EMIFAC(ITYP) = 1.0D06
      EMILBL(ITYP) = 'GRAMS/SEC'
      OUTLBL(ITYP) = 'MICROGRAMS/M**3'
      PERLBL(ITYP) = 'MICROGRAMS/M**3'
      OUTTYP(ITYP) = 'CONC'
   ELSE IF (CONC) THEN
      ITYP = 1
      CHIDEP(1,ITYP) = 'AVER'
      CHIDEP(2,ITYP) = 'AGE '
      CHIDEP(3,ITYP) = 'CONC'
      CHIDEP(4,ITYP) = 'ENTR'
      CHIDEP(5,ITYP) = 'ATIO'
      CHIDEP(6,ITYP) = 'N   '
      EMIFAC(ITYP) = 1.0D06
      EMILBL(ITYP) = 'GRAMS/SEC'
      OUTLBL(ITYP) = 'MICROGRAMS/M**3'
      PERLBL(ITYP) = 'MICROGRAMS/M**3'
      OUTTYP(ITYP) = 'CONC'
      IF (DEPOS) THEN
         ITYP = 2
         CHIDEP(1,ITYP) = '  TO'
         CHIDEP(2,ITYP) = 'TAL '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DEPOS'
         IF (DDEP) THEN
            ITYP = 3
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DDEP'
            IF (WDEP) THEN
               ITYP = 4
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 3
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (DDEP) THEN
         ITYP = 2
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'DRY '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DDEP'
         IF (WDEP) THEN
            ITYP = 3
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (WDEP) THEN
         ITYP = 2
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'WDEP'
      END IF
   ELSE IF (DEPOS) THEN
      ITYP = 1
      CHIDEP(1,ITYP) = '  TO'
      CHIDEP(2,ITYP) = 'TAL '
      CHIDEP(3,ITYP) = 'DEPO'
      CHIDEP(4,ITYP) = 'SITI'
      CHIDEP(5,ITYP) = 'ON  '
      CHIDEP(6,ITYP) = '    '
      EMIFAC(ITYP) = 3600.0D0
      EMILBL(ITYP) = 'GRAMS/SEC'
      OUTLBL(ITYP) = 'GRAMS/M**2'
      PERLBL(ITYP) = 'GRAMS/M**2'
      OUTTYP(ITYP) = 'DEPOS'
      IF (DDEP) THEN
         ITYP = 2
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'DRY '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DDEP'
         IF (WDEP) THEN
            ITYP = 3
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (WDEP) THEN
         ITYP = 2
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'WDEP'
      END IF
   ELSE IF (DDEP) THEN
      ITYP = 1
      CHIDEP(1,ITYP) = '    '
      CHIDEP(2,ITYP) = 'DRY '
      CHIDEP(3,ITYP) = 'DEPO'
      CHIDEP(4,ITYP) = 'SITI'
      CHIDEP(5,ITYP) = 'ON  '
      CHIDEP(6,ITYP) = '    '
      EMIFAC(ITYP) = 3600.0D0
      EMILBL(ITYP) = 'GRAMS/SEC'
      OUTLBL(ITYP) = 'GRAMS/M**2'
      PERLBL(ITYP) = 'GRAMS/M**2'
      OUTTYP(ITYP) = 'DDEP'
      IF (WDEP) THEN
         ITYP = 2
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'WDEP'
      END IF
   ELSE IF (WDEP) THEN
      ITYP = 1
      CHIDEP(1,ITYP) = '    '
      CHIDEP(2,ITYP) = 'WET '
      CHIDEP(3,ITYP) = 'DEPO'
      CHIDEP(4,ITYP) = 'SITI'
      CHIDEP(5,ITYP) = 'ON  '
      CHIDEP(6,ITYP) = '    '
      EMIFAC(ITYP) = 3600.0D0
      EMILBL(ITYP) = 'GRAMS/SEC'
      OUTLBL(ITYP) = 'GRAMS/M**2'
      PERLBL(ITYP) = 'GRAMS/M**2'
      OUTTYP(ITYP) = 'WDEP'
   END IF

   EMICON = 1.0D+06

! --- Modify PLTFRM, PSTFRM and MXDFRM if needed for more than one output type
!     and for EXP format (note that FILE_FORMAT is set during PRESET).

   IF (NUMTYP > 1 .and. FILE_FORMAT == 'FIX') THEN
      IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
         WRITE(PLTFRM,1009) NUMTYP+2
1009     FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,',&
         &'A8,2X,10(F13.5,2X,I8.8,2X:))')
      ELSE
         WRITE(PLTFRM,1019) NUMTYP+2
1019     FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,',&
         &'A8,2X,I8)')
      END IF
      WRITE(PSTFRM,1029) NUMTYP+2
1029  FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,',&
      &'A8)')
      WRITE(MXDFRM,1039) NUMTYP+2
1039  FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,',&
      &'2X,I8.8,2X,A8)')
   ELSE IF (NUMTYP > 1 .and. FILE_FORMAT == 'EXP') THEN
      IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
         WRITE(PLTFRM,2009) NUMTYP
2009     FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,',&
         &'A8,2X,A5,5X,A8,2X,10(E13.6,2X,I8.8,2X:))')
      ELSE
         WRITE(PLTFRM,2019) NUMTYP
2019     FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),3X,A5,2X,',&
         &'A8,2X,A5,5X,A8,2X,I8)')
      END IF
      WRITE(PSTFRM,2029) NUMTYP
2029  FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',&
      &'2X,I8.8,2X,A8)')
      WRITE(MXDFRM,2039) NUMTYP
2039  FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',&
      &'2X,I4,2X,I3,2X,I8.8,2X,A8)')
   END IF

   RETURN
END SUBROUTINE MODOPT


SUBROUTINE AVETIM
!***********************************************************************
!                 AVETIM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Averaging Time Options From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Averaging Period Array and PERIOD Logical Switch
!
!        ERROR HANDLING:   Checks for Too Many Short Term Averages (>4);
!                          Checks for Invalid Averaging Periods, MOD(24,X) NE 0;
!                          Checks for Duplicate Short Term Averaging Periods
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K
   REAL    :: AVENUM
   CHARACTER (LEN = 8) :: KOPT

!     Variable Initializations
   MODNAM = 'AVETIM'

!     Check for No Parameters
   IF (IFC < 3) THEN
!        WRITE Error Message     ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

!     First Check for Presence of PERIOD or ANNUAL Switch
   DO 10 I = 3, IFC
      KOPT = FIELD(I)
      IF (KOPT == 'PERIOD') THEN
         PERIOD = .TRUE.
      ELSE IF (KOPT == 'ANNUAL') THEN
         ANNUAL = .TRUE.
      END IF
10 CONTINUE

! --- Check for Both PERIOD and ANNUAL
   IF (PERIOD .and. ANNUAL) THEN
!        Write Error Message; both PERIOD and ANNUAL specified
      CALL ERRHDL(PATH,MODNAM,'E','294',KEYWRD)
   ELSE IF (PERIOD .or. ANNUAL) THEN
!        Check for Too Many Averaging Periods
      IF (IFC > NAVE+3) THEN
!           WRITE Error Message: Too Many Period Or Time Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      END IF
   ELSE
      IF (IFC > NAVE+2) THEN
!           WRITE Error Message: Too Many Period Or Time Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      END IF
   END IF

!     Loop Through Fields Again, Filling KAVE Array for Short Term Averages
   J = 0
   DO 20 I = 3, IFC
      KOPT = FIELD(I)
      IF (KOPT /= 'PERIOD' .and. KOPT /= 'ANNUAL') THEN
         IF (KOPT /= 'MONTH') THEN
            CALL STONUM(KOPT,8,AVENUM,IMIT)
            IF (IMIT /= 1) THEN
!                 Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            END IF
!              Check for Valid Averaging Period
            IF ((MOD(24,NINT(AVENUM))==0 .and.&
            &IMIT==1)) THEN
               J = J + 1
               IF (J <= NAVE) THEN
                  KAVE(J) = NINT(AVENUM)
                  WRITE(CHRAVE(J),'(I2,"-HR")') KAVE(J)
                  NUMAVE = J
!                    Check for Duplicate Averaging Periods
                  DO 15 K = J-1, 1, -1
                     IF (KAVE(J) == KAVE(K)) THEN
!                          WRITE Error Message    ! Duplicate Averaging Period
                        CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                     END IF
15                CONTINUE
               ELSE
!                    WRITE Error Message   ! Too Many Short Term Averaging Periods
!                    This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NAVE='',I7)') NAVE
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               END IF
            ELSE
!                 WRITE Error Message      ! Invalid Averaging Period
               CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
            END IF
         ELSE
            J = J + 1
            IF (J <= NAVE) THEN
               KAVE(J) = 720
               MONTH = .TRUE.
               CHRAVE(J) = 'MONTH'
               NUMAVE = J
!                 Check for Duplicate Averaging Periods
               DO K = J-1, 1, -1
                  IF (KAVE(J) == KAVE(K)) THEN
!                       WRITE Error Message    ! Duplicate Averaging Period
                     CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                  END IF
               END DO
            ELSE
!                 WRITE Error Message   ! Too Many Short Term Averaging Periods
!                 This shouldn't occur since limits are dynamically allocated
               WRITE(DUMMY,'(''NAVE='',I7)') NAVE
               CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            END IF
         END IF
      END IF
20 CONTINUE

   RETURN
END SUBROUTINE AVETIM

SUBROUTINE POLLID
!***********************************************************************
!                 POLLID Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Pollutant Identification Option
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Pollutant Identification Option
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'POLLID'

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC == 3) THEN
!        Assign user input to POLLUT variable
      POLLUT = FIELD(3)
   ELSE IF (IFC == 4) THEN
! ---    Check for POLLUT IDs associated with "special" processing;
!        NO2 or SO2 or PM25 (including all PM25 variants)
      IF (FIELD(3) == 'NO2'  .or. FIELD(3) == 'SO2' .or.&
      &FIELD(3) == 'PM25' .or. FIELD(3) == 'PM-2.5' .or.&
      &FIELD(3) == 'PM-25'.or. FIELD(3) == 'PM2.5') THEN
! ---       This POLLID allows optional input in field 4; assign POLLUT
         POLLUT = FIELD(3)
      ELSE
! ---       User-specified POLLID doesn't allow for field 4; assign
!           POLLUT but issue an error message
         POLLUT = FIELD(3)
         IF (FIELD(4) == 'H1H' .or. FIELD(4) == 'H2H' .or.&
         &FIELD(4) == 'INC') THEN
!              Error Message: 'H1H', 'H2H', and 'INC' processing not
!              applicable to this POLLUT
            WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),&
            &FIELD(4)(1:3)
            CALL ERRHDL(PATH,MODNAM,'E','277',DUMMY)
! ---          Save FIELD(4) to include in summary of input options
            NO2_FIELD4 = FIELD(4)(1:3)
            GO TO 999
         ELSE
!              Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
      END IF
!        Now check for options to disable "special" processing for
!        these pollutants
      IF (FIELD(4) == 'H1H' .or. FIELD(4) == 'H2H' .or.&
      &FIELD(4) == 'INC') THEN
         IF (POLLUT == 'NO2') THEN
            L_NO_NO2AVE = .TRUE.
            NO2_FIELD4 = FIELD(4)(1:3)
!              Issue Warning Message: user disabled special processing
            WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),&
            &FIELD(4)(1:3)
            CALL ERRHDL(PATH,MODNAM,'W','276',DUMMY)
         ELSE IF (POLLUT == 'SO2') THEN
            L_NO_SO2AVE = .TRUE.
            SO2_FIELD4 = FIELD(4)(1:3)
            WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),&
            &FIELD(4)(1:3)
!              Issue Warning Message: user disabled special processing
            CALL ERRHDL(PATH,MODNAM,'W','276',DUMMY)
         ELSE IF (POLLUT == 'PM25' .or. POLLUT == 'PM-2.5' .or.&
         &POLLUT == 'PM-25'.or. POLLUT == 'PM2.5') THEN
            L_NO_PM25AVE = .TRUE.
            PM25_FIELD4 = FIELD(4)(1:3)
            WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),&
            &FIELD(4)(1:3)
!              Issue Warning Message: user disabled special processing
            CALL ERRHDL(PATH,MODNAM,'W','276',DUMMY)
         ELSE
!              Error Message: 'H1H', 'H2H', and 'INC' processing not
!              applicable to this POLLUT
            WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),&
            &FIELD(4)(1:3)
            CALL ERRHDL(PATH,MODNAM,'E','277',DUMMY)
            GO TO 999
         END IF
      END IF
   ELSE
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   POLLUT = FIELD(3)

999 RETURN
END SUBROUTINE POLLID

SUBROUTINE EDECAY
!***********************************************************************
!                 EDECAY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Exponential Decay Options
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Exponental Decay Options
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'EDECAY'

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

!     Start To Get Decay Coef.
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

   IF (KEYWRD == 'HALFLIFE') THEN
      HAFLIF = DNUM
!        Calculate Decay Coef. by Halflife
      DECOEF = 0.693D0/HAFLIF
   ELSE IF (KEYWRD == 'DCAYCOEF') THEN
      DECOEF = DNUM
   END IF

! --- Check for Urban Regulatory Default for SO2; use L_PRESET_URBAN rather then
!     URBAN to allow flexibility in order of keywords
   IF (DFAULT .and. L_PRESET_URBAN .and. POLLUT=='SO2') THEN
      IF (DECOEF /= 4.81D-5) THEN
!           WRITE Warning Message: Attempt to Override Regulatory Default
         CALL ERRHDL(PATH,MODNAM,'W','206','DCAYCOEF')
      END IF
      DECOEF = 4.81D-5
   ELSE IF (DFAULT) THEN
      IF (DECOEF /= 0.0D0) THEN
!           WRITE Warning Message: Attempt to Override Regulatory Default
         CALL ERRHDL(PATH,MODNAM,'W','206','DCAYCOEF')
      END IF
      DECOEF = 0.0D0
   ELSE IF (.NOT. DFAULT .and. DECOEF /= 0.0D0) THEN
!        Set flag for use of non-DEFAULT option
      L_NonDFAULT = .TRUE.
   END IF

999 RETURN
END SUBROUTINE EDECAY

SUBROUTINE RUNNOT
!***********************************************************************
!                 RUNNOT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Model RUN Logical Switch
!
!        ERROR HANDLING:   Checks for Invalid Parameters;
!                          Checks for No Parameters;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'RUNNOT'

   IF (IFC == 3) THEN
      IF (FIELD(3) == 'RUN') THEN
         RUN = .TRUE.
      ELSE IF (FIELD(3) == 'NOT') THEN
         RUN = .FALSE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
      END IF
   ELSE IF (IFC > 3) THEN
!        WRITE Error Message     ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message     ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END SUBROUTINE RUNNOT

SUBROUTINE FLAGDF
!***********************************************************************
!                 FLAGDF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Default Flagpole Receptor Height Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Default Flagpole Receptor Heights
!
!        ERROR HANDLING:   Checks for Invalid Parameters;
!                          Checks for No Parameters;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION :: ZFLG
! Unused:      INTEGER :: I

!     Variable Initializations
   MODNAM = 'FLAGDF'
   FLGPOL = .TRUE.

   IF (IFC == 3) THEN
      CALL STODBL(FIELD(3),ILEN_FLD,ZFLG,IMIT)
      IF (IMIT /= 1) THEN
!           Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
      IF (ZFLG >= 0.0D0 .and. IMIT == 1) THEN
         AZFLAG(:) = ZFLG
      ELSE IF (ZFLG < 0.0D0 .and. IMIT == 1) THEN
!            WRITE Error Message: Invalid Data. Negative value specified
         CALL ERRHDL(PATH,MODNAM,'E','209','ZFLAG')
      ELSE
!            WRITE Error Message: Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
      END IF
   ELSE IF (IFC > 3) THEN
!        WRITE Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'W','205','ZFLAG=0.')
   END IF

   RETURN
END SUBROUTINE FLAGDF

SUBROUTINE EVNTFL
!***********************************************************************
!                 EVNTFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process EVENT File Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: EVENT File Logical Switch and EVENT Filename
!
!        ERROR HANDLING:   Checks for No Parametes;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'EVNTFL'

   IF (IFC == 3) THEN
      EVENTS = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         EVFILE = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  EVFILE Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
      EVPARM = 'DETAIL'
   ELSE IF (IFC == 4) THEN
      EVENTS  = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         EVFILE = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  EVFILE Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
      EVPARM = FIELD(4)
   ELSE IF (IFC > 4) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Warning Message         ! No Parameters - Use Default Name
      CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      EVENTS = .TRUE.
      EVFILE = 'EVENTS.INP'
      EVPARM = 'DETAIL'
   END IF

!     Check for Invalid EVPARM
   IF (EVPARM /= 'SOCONT' .and. EVPARM /= 'DETAIL') THEN
!        WRITE Warning Message         ! Invalid Parameter - Use Default
      CALL ERRHDL(PATH,MODNAM,'W','203','EVPARM')
   END IF

!     Open The EVENT Input File
   OPEN(UNIT=IEVUNT,FILE=EVFILE,STATUS='REPLACE',&
   &FORM='FORMATTED')

   RETURN
END SUBROUTINE EVNTFL

SUBROUTINE SAVEFL
!***********************************************************************
!                 SAVEFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process RESTART File Save Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
!
!        ERROR HANDLING:   Checks for No Parametes (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'SAVEFL'

   IF (MULTYR) THEN
!        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
      CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
   ELSE IF (IFC == 3) THEN
      RSTSAV = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  SAVFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
      SAVFL2 = SAVFIL
      INCRST = 1
   ELSE IF (IFC == 4) THEN
      RSTSAV = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  SAVFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
      SAVFL2 = SAVFIL
      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
      INCRST = NINT(FNUM)
      IF (IMIT /= 1) THEN
!           Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
   ELSE IF (IFC == 5) THEN
      RSTSAV = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  SAVFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
      INCRST = NINT(FNUM)
      IF (IMIT /= 1) THEN
!           Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(5)-LOCB(5)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         SAVFL2 = RUNST1(LOCB(5):LOCE(5))
      ELSE
!           WRITE Error Message:  SAVFL2 Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
   ELSE IF (IFC > 5) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Warning Message          ! No Parameters - Use Default Name
      CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      RSTSAV = .TRUE.
      SAVFIL = 'SAVE.FIL'
      SAVFL2 = SAVFIL
      INCRST = 1
   END IF

   RETURN
END SUBROUTINE SAVEFL

SUBROUTINE INITFL
!***********************************************************************
!                 INITFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process RESTART Initialization Input File Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To change default filename to SAVE.FIL to match
!                    default name for SAVEFILE card.
!                    R.W. Brode, PES, Inc. - 6/20/95
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: RSTINP Logical Switch and Re-start Input Filename
!
!        ERROR HANDLING:   Checks for No Parametes (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'INITFL'

   IF (MULTYR) THEN
!        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
      CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
   ELSE IF (IFC == 3) THEN
      RSTINP = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         INIFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  INIFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
   ELSE IF (IFC > 3) THEN
!        WRITE Error Message           ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!        WRITE Warning Message          ! No Parameters - Use Default Name
      CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      RSTINP = .TRUE.
      INIFIL = 'SAVE.FIL'
   END IF

   RETURN
END SUBROUTINE INITFL

SUBROUTINE ERRFIL
!***********************************************************************
!                 ERRFIL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Error Message File Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
!
!        ERROR HANDLING:   Checks for No Parametes (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'ERRFIL'

   IF (IFC == 3) THEN
      ERRLST = .TRUE.
!        Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         MSGFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE
!           WRITE Error Message:  MSGFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         RETURN
      END IF
   ELSE IF (IFC > 3) THEN
!*       WRITE Error Message                ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   ELSE
!*       WRITE Warning Message              ! No Parameters - Use Default Name
      CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
      ERRLST = .TRUE.
      MSGFIL = 'ERRORS.LST'
   END IF
!*#

   RETURN
END SUBROUTINE ERRFIL

SUBROUTINE DEBOPT
!***********************************************************************
!                 DEBOPT Module of AERMOD
!
!        PURPOSE: Process Debug Output File Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        MODIFIED    Modified to allow user to specify debug output for
!                    the Aircraft option.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Added DEBUG option for RLINE source.
!                    Wood, 12/07/2021 Michelle G. Snyder & Laura Kent
!
!        MODIFIED    Modified to allow user to specify debug output
!                    for the GRSM NO2 option.
!                    CERC, 11/30/20
!
!        MODIFIED:   Modified to allow user to specify debug output
!                    for the PRIME downwash algorithm and for the
!                    OLM, ARM, or ARM2 options for modeling NO2.
!                    Portions of the MODEL debug outputs that were
!                    included in the main 'aermod.out' and in the
!                    'model.dbg' file will now be included in a
!                    separate PRIME debug file.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/29/2014
!
!        MODIFIED:   Modified to allow user to specify debug output
!                    only for PVMRM or deposition options on the
!                    DEBUGOPT keyword, avoiding large ouput files
!                    under the MODEL debug option. Debug output for
!                    PVMRM and/or deposition options will still be
!                    generated if the MODEL debug option is selected.
!                    See AERMOD User's Guide Addendum for details
!                    on the DEBUGOPT keyword.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Debug File Logical Switches and Filenames
!
!        ERROR HANDLING:   Checks for Too Few Parameters (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   USE RLINE_DATA, ONLY: NRLINES
!CRCO D095 - this has been added to bring NBLP into the logic. Should
! we move NBLP out of BUOYANT_LINE and into MAIN1 instead in modules.f?
   USE BUOYANT_LINE
   IMPLICIT NONE
   CHARACTER :: MODNAM*12, KOPT*8
!CRT  D063 Add Platform Downwash variable IPLATFM and initialize below
!CRT  D113 Add Sidewash source variable ISW and initialize below
   INTEGER :: I, IMOD, IMET, IAREA, IPRM, IPVM, IOLMD, IARM2, IDEP,&
   &IGRSM, NOPTS, MAXFields, IPRM2, ITTRMD, ITTRM2,&
   &IURBD, IBLP, IPLATFM, IRLINE, ISW,&
   &IARCFT,&                          ! Added for Aircraft; UNC-IE
   &IHBP ! Added for HBPDEBUG; JAN 2023

!     Variable Initializations
   MODNAM = 'DEBOPT'
!     Initialize counters for number of debug options and field number
!     associated with debugopts
   IMOD  = 0
   IMET  = 0
   IAREA = 0
   IPRM  = 0
   IPVM  = 0
   IOLMD = 0
   IARM2  = 0
   IDEP  = 0
   IGRSM = 0
   IPRM2 = 0
   IPLATFM = 0
   NOPTS = 0
   MAXFields = 0
   ITTRMD = 0
   ITTRM2 = 0
   IRLINE = 0
   IURBD = 0
   IBLP = 0
   ISW = 0
   IARCFT = 0                                 ! Added for Aircraft; UNC-IE
   IHBP = 0 ! Added for HBP, JAN 2023

!     Check for Too Few or Too Many Parameters
   IF (IFC < 3) THEN
!        WRITE Error Message     ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   ELSE IF (IFC > 13) THEN
!        WRITE Warning Message   ! Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
   END IF

! --- First Check for Presence of Debug Switches;
!     also save position to interpret optional
!     filenames
   DO I = 3, IFC
      KOPT = FIELD(I)
      IF (KOPT == 'MODEL') THEN
         DEBUG = .TRUE.
         NOPTS = NOPTS + 1
         IMOD = I
      ELSE IF (KOPT == 'METEOR') THEN
         METEORDBG = .TRUE.
         NOPTS = NOPTS + 1
         IMET = I
      ELSE IF (KOPT == 'AREA') THEN
! ---       Check to see if AREADBG option has already been assigned .T.;
!           user may have entered both AREA and LINE
         IF (.NOT. AREADBG) THEN
! ---          AREADBG option not already = .T.; assign all variables
            AREADBG = .TRUE.
            NOPTS = NOPTS + 1
            IAREA = I
         ELSE
! ---          AREADBG already assigned = .T.; user may have entered
!              both AREA and LINE options; issue ERROR message
            CALL ERRHDL(PATH,MODNAM,'E','194','AREADEBUG')
            AREADBG = .FALSE.
         END IF
      ELSE IF (KOPT == 'LINE') THEN
! ---       Check to see if AREADBG option has already been assigned .T.;
!           user may have entered both AREA and LINE
         IF (.NOT. AREADBG) THEN
! ---          AREADBG option not already = .T.; assign all variables
            AREADBG = .TRUE.
            NOPTS = NOPTS + 1
            IAREA = I
         ELSE
! ---          AREADBG already assigned = .T.; user may have entered
!              both AREA and LINE options; issue ERROR message
            CALL ERRHDL(PATH,MODNAM,'E','194','LINEDEBUG')
            AREADBG = .FALSE.
         END IF
      ELSE IF (KOPT == 'PRIME') THEN
         PRIMEDBG = .TRUE.
         NOPTS = NOPTS + 1
         IPRM = I
      ELSE IF (KOPT == 'PVMRM') THEN
         PVMRMDBG = .TRUE.
         NOPTS = NOPTS + 1
         IPVM = I
      ELSE IF (KOPT == 'OLM') THEN
         OLMDEBUG = .TRUE.
         NOPTS = NOPTS + 1
         IOLMD = I
      ELSE IF (KOPT == 'ARM2') THEN
         ARM2DEBUG = .TRUE.
         NOPTS = NOPTS + 1
         IARM2 = I
      ELSE IF (KOPT == 'GRSM') THEN
         GRSMDEBUG = .TRUE.
         NOPTS = NOPTS + 1
         IGRSM = I
      ELSE IF (KOPT == 'DEPOS') THEN
         DEPOSDBG = .TRUE.
         NOPTS = NOPTS + 1
         IDEP = I
!CRT     D063 Platform Downwash Debug
      ELSE IF (KOPT == 'PLATFORM') THEN
         PLATFMDBG = .TRUE.
         NOPTS = NOPTS + 1
         IPLATFM = I
      ELSE IF (KOPT == 'AWMADW') THEN
         AWMADWDBG   = .TRUE.
         NOPTS = NOPTS + 1
         IPRM2 = I
      ELSE IF (KOPT == 'RLINE') THEN
         RLINEDBG   = .TRUE.
         NOPTS = NOPTS + 1
         IRLINE = I
      ELSE IF (KOPT == 'TTRM') THEN
         TTRMDBG = .TRUE.
         NOPTS = NOPTS + 1
         ITTRMD = I
      ELSE IF (KOPT == 'TTRM2') THEN
         TTRM2DBG = .TRUE.
         NOPTS = NOPTS + 1
         ITTRM2 = I
      ELSE IF (KOPT == 'URBANDB') THEN
         URBDBUG = .TRUE.
         NOPTS = NOPTS + 1
         IURBD = I
      ELSE IF (KOPT == 'BLPDBUG') THEN
         BLPDBUG = .TRUE.
         NOPTS = NOPTS + 1
         IBLP = I
      ELSE IF (KOPT == 'SWPOINT') THEN
         SWDBG = .TRUE.
         NOPTS = NOPTS + 1
         ISW = I

!** Added for Aircraft Plume Rise; UNC-IE
      ELSE IF (KOPT == 'AIRCRAFT') THEN
         ARCFTDEBUG = .TRUE.
         NOPTS = NOPTS + 1
         IARCFT = I
!** End Aircraft Plume Rise insert; April 2023

! Added for HBP JAN 2023
      ELSE IF (KOPT == 'HBPDBG') THEN
         HBPDBG = .TRUE.
         NOPTS = NOPTS + 1
         IHBP = I
! End HBP add

      END IF
   END DO

! --- Determine maximum number of fields allowed based on number of
!     options specified, assuming that user has specified filename
!     for each option (except for DEPOS).
   IF (NOPTS > 0) THEN
      IF (.NOT.DEPOSDBG) THEN
         MAXFields = 2 + NOPTS*2
      ELSE
         MAXFields = 2 + (NOPTS-1)*2 + 1
      END IF
   ELSE
!        No recognizable debug options specified, issue fatal error
      WRITE(DUMMY,'(A:)') FIELD(3)(1:MIN(12,LEN_TRIM(FIELD(3))))
      CALL ERRHDL(PATH,MODNAM,'E','203',DUMMY)
      GO TO 999
   END IF

! --- Check for debug options without associated model option being used
   IF (PVMRMDBG .and. .NOT. PVMRM) THEN
!        Write Error Message:  PVMRM debug without PVMRM option
      CALL ERRHDL(PATH,MODNAM,'E','194','PVMRMDBG')
   END IF
   IF (OLMDEBUG .and. .NOT.OLM) THEN
!        Write Error Message:  OLM debug without OLM option
      CALL ERRHDL(PATH,MODNAM,'E','194','OLMDEBUG')
   END IF
   IF (ARM2DEBUG .and. .NOT.ARM2) THEN
!        Write Error Message:  ARM2 debug without ARM2 option
      CALL ERRHDL(PATH,MODNAM,'E','194','ARM2DEBUG')
   END IF
   IF (GRSMDEBUG .and. .NOT.GRSM) THEN
!        Write Error Message:  GRSM debug without GRSM option
      CALL ERRHDL(PATH,MODNAM,'E','194','GRSMDEBUG')
   END IF
   IF (TTRMDBG .and. .NOT. RUNTTRM) THEN
!        Write Error Message:  TTRM debug without TTRM option
      CALL ERRHDL(PATH,MODNAM,'E','194','TTRMDEBUG')
   END IF
   IF (TTRM2DBG .and. .NOT. RUNTTRM2) THEN
!        Write Error Message:  TTRM2 debug without TTRM2 option
      CALL ERRHDL(PATH,MODNAM,'E','194','TTRM2DEBUG')
   END IF
! Added for HBP, added Jan 2023
   IF (HBPDBG .and. .NOT. HBPLUME) THEN
!        Write Error Message:  HBPDBG debug without HBP option
      CALL ERRHDL(PATH,MODNAM,'E','194','HBPDBG')
   ENDIF
! End HBP add
   IF (DEPOSDBG .and. .NOT.DEPOS .and. .NOT.DDEP .and.&
   &.NOT.WDEP) THEN
!        Write Error Message:  DEPOS debug without deposition options
      CALL ERRHDL(PATH,MODNAM,'E','194','DEPOSDBG')
   END IF
   IF (AREADBG .and. NAREA==0 .and. NCIRC==0 .and. NLINE==0&
   &.and. NPIT==0) THEN
!        Write Error Message:  AREA/LINE debug without any applicable
!        sources
      IF (FIELD(IAREA) == 'AREA') THEN
         CALL ERRHDL(PATH,MODNAM,'E','194','AREADEBUG')
      ELSE IF (FIELD(IAREA) == 'LINE') THEN
         CALL ERRHDL(PATH,MODNAM,'E','194','LINEDEBUG')
      END IF
   END IF
!CRCO D095 Added for urban debug 8/3/2021
   IF (NURB == 0 .and. URBDBUG) THEN
!        Write Error Message:  URBDBUG debug without applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','URBANDB')
   END IF
!C END URBDBUG insert
   IF (PRIMEDBG .and. NSEC==0) THEN
!        Write Error Message:  PRIME debug without any applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','PRIMEDBG')
   END IF

   IF (AWMADWDBG .and. NSEC==0) THEN
!        Write Error Message:  AWMADW debug without any applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','AWMADWDBG')
   END IF
!CRCO D095 added for BLP debug 12/9/2021
   IF (NBLP == 0 .and. BLPDBUG) THEN
!        Write Error Message:  BLPDBUG debug without applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','BLPDBUG')
   END IF

   IF (RLINEDBG .and. NRLINES==0) THEN
!        Write Error Message:  RLINE debug without any applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','RLINEDBG')
   END IF

!CRT     D063 Platform Downwash Debug
   IF (PLATFMDBG .and. NPNT==0) THEN
!        Write Error Message:  PLATFORM debug without any applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','PLATFMDBG')
   END IF

!     CRT 3/25/02022 D113 Added for sidewash debug
   IF (SWDBG .and. NSWP == 0) THEN
!        Write Error Message:  SWDBG debug without any applicable sources
      CALL ERRHDL(PATH,MODNAM,'E','194','SWDBG')
   END IF

!** Added for Aircraft Plume Rise; UNC-IE
!MGS Changed conditional to be consistant with debug conditionals above.
!MSG (D151 - WSP 6/1/2023)
!MGS      IF (ARCFTDEBUG .and. .NOT. NAFTSRC .GT. 0.0D0) THEN
   IF (ARCFTDEBUG .and. NAFTSRC == 0) THEN
!        Write Error Message:  ARCFT debug without Aircraft option
      CALL ERRHDL(PATH,MODNAM,'E','194','ARCFTDEBUG')
   END IF
!** End Aircraft Plume Rise insert; April 2023

! --- Check for user-specified filenames, which should immediately
!     follow the keyword option in the input file
   IF (DEBUG) THEN
      IF (IFC >= IMOD+1 .and.&
      &FIELD(IMOD+1) /= 'METEOR' .and.&
      &FIELD(IMOD+1) /= 'AREA' .and.&
      &FIELD(IMOD+1) /= 'LINE' .and.&
      &FIELD(IMOD+1) /= 'RLINE' .and.&
      &FIELD(IMOD+1) /= 'PRIME' .and.&
      &FIELD(IMOD+1) /= 'PVMRM' .and.&
      &FIELD(IMOD+1) /= 'OLM' .and.&
      &FIELD(IMOD+1) /= 'ARM2' .and.&
      &FIELD(IMOD+1) /= 'GRSM' .and.&
      &FIELD(IMOD+1) /= 'DEPOS'.and.&
      &FIELD(IMOD+1) /= 'AWMADW' .and.&
      &FIELD(IMOD+1) /= 'TTRM' .and.&
      &FIELD(IMOD+1) /= 'TTRM2' .and.&
      &FIELD(IMOD+1) /= 'PLATFORM' .and.&
      &FIELD(IMOD+1) /= 'URBANDB' .and.&
      &FIELD(IMOD+1) /= 'BLPDBUG' .and.&
      &FIELD(IMOD+1) /= 'SWPOINT' .and.&
      &FIELD(IMOD+1) /= 'AIRCRAFT' .and.&
      &FIELD(IMOD+1) /= 'HBPDBG' ) THEN

! ---       Assign user-specified filename for the MODEL debug option
         DBGFIL = RUNST1(LOCB(IMOD+1):LOCE(IMOD+1))
      ELSE
! ---       Assign default MODEL debug filename
         DBGFIL = 'MODEL.DBG'
      END IF
   END IF

   IF (METEORDBG) THEN
      IF (IFC >= IMET+1 .and.&
      &FIELD(IMET+1) /= 'MODEL' .and.&
      &FIELD(IMET+1) /= 'AREA' .and.&
      &FIELD(IMET+1) /= 'LINE' .and.&
      &FIELD(IMET+1) /= 'RLINE' .and.&
      &FIELD(IMET+1) /= 'PRIME' .and.&
      &FIELD(IMET+1) /= 'PVMRM' .and.&
      &FIELD(IMET+1) /= 'OLM' .and.&
      &FIELD(IMET+1) /= 'ARM2' .and.&
      &FIELD(IMET+1) /= 'GRSM' .and.&
      &FIELD(IMET+1) /= 'DEPOS' .and.&
      &FIELD(IMET+1) /= 'AWMADW' .and.&
      &FIELD(IMET+1) /= 'TTRM' .and.&
      &FIELD(IMET+1) /= 'TTRM2' .and.&
      &FIELD(IMET+1) /= 'PLATFORM' .and.&
      &FIELD(IMET+1) /= 'URBANDB' .and.&
      &FIELD(IMET+1) /= 'BLPDBUG' .and.&
      &FIELD(IMET+1) /= 'SWPOINT' .and.&
      &FIELD(IMET+1) /= 'AIRCRAFT' .and.&
      &FIELD(IMET+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the METEOR debug option
         DBMFIL = RUNST1(LOCB(IMET+1):LOCE(IMET+1))
      ELSE
! ---       Assign default METEOR debug filename
         DBMFIL = 'METEOR.DBG'
      END IF
   END IF

   IF (AREADBG) THEN
      IF (IFC >= IAREA+1 .and.&
      &FIELD(IAREA+1) /= 'MODEL' .and.&
      &FIELD(IAREA+1) /= 'METEOR' .and.&
      &FIELD(IAREA+1) /= 'RLINE' .and.&
      &FIELD(IAREA+1) /= 'PRIME' .and.&
      &FIELD(IAREA+1) /= 'PVMRM' .and.&
      &FIELD(IAREA+1) /= 'OLM' .and.&
      &FIELD(IAREA+1) /= 'ARM2' .and.&
      &FIELD(IAREA+1) /= 'GRSM' .and.&
      &FIELD(IAREA+1) /= 'DEPOS' .and.&
      &FIELD(IAREA+1) /= 'AWMADW' .and.&
      &FIELD(IAREA+1) /= 'TTRM' .and.&
      &FIELD(IAREA+1) /= 'TTRM2' .and.&
      &FIELD(IAREA+1) /= 'PLATFORM' .and.&
      &FIELD(IAREA+1) /= 'URBANDB' .and.&
      &FIELD(IAREA+1) /= 'BLPDBUG' .and.&
      &FIELD(IAREA+1) /= 'SWPOINT'.and.&
      &FIELD(IAREA+1) /= 'AIRCRAFT' .and.&
      &FIELD(IAREA+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the AREA debug option
         DBAREAFIL = RUNST1(LOCB(IAREA+1):LOCE(IAREA+1))
      ELSE
! ---       Assign default AREA debug filename
         DBAREAFIL = 'AREA.DBG'
      END IF
   END IF

   IF (PRIMEDBG) THEN
      IF (IFC >= IPRM+1 .and.&
      &FIELD(IPRM+1) /= 'MODEL' .and.&
      &FIELD(IPRM+1) /= 'METEOR' .and.&
      &FIELD(IPRM+1) /= 'AREA' .and.&
      &FIELD(IPRM+1) /= 'LINE' .and.&
      &FIELD(IPRM+1) /= 'RLINE' .and.&
      &FIELD(IPRM+1) /= 'PVMRM' .and.&
      &FIELD(IPRM+1) /= 'OLM' .and.&
      &FIELD(IPRM+1) /= 'ARM2' .and.&
      &FIELD(IPRM+1) /= 'GRSM' .and.&
      &FIELD(IPRM+1) /= 'DEPOS' .and.&
      &FIELD(IPRM+1) /= 'AWMADW' .and.&
      &FIELD(IPRM+1) /= 'TTRM' .and.&
      &FIELD(IPRM+1) /= 'TTRM2' .and.&
      &FIELD(IPRM+1) /= 'PLATFORM' .and.&
      &FIELD(IPRM+1) /= 'URBANDB' .and.&
      &FIELD(IPRM+1) /= 'BLPDBUG' .and.&
      &FIELD(IPRM+1) /= 'SWPOINT' .and.&
      &FIELD(IPRM+1) /= 'AIRCRAFT' .and.&
      &FIELD(IPRM+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the PRIME debug option
         DBPRMFIL = RUNST1(LOCB(IPRM+1):LOCE(IPRM+1))
      ELSE
! ---       Assign default PRIME debug filename
         DBPRMFIL = 'PRIME.DBG'
      END IF
   END IF

   IF (PVMRMDBG) THEN
      IF (IFC >= IPVM+1 .and.&
      &FIELD(IPVM+1) /= 'MODEL' .and.&
      &FIELD(IPVM+1) /= 'METEOR' .and.&
      &FIELD(IPVM+1) /= 'AREA' .and.&
      &FIELD(IPVM+1) /= 'LINE' .and.&
      &FIELD(IPVM+1) /= 'RLINE' .and.&
      &FIELD(IPVM+1) /= 'PRIME' .and.&
      &FIELD(IPVM+1) /= 'OLM' .and.&
      &FIELD(IPVM+1) /= 'ARM2' .and.&
      &FIELD(IPVM+1) /= 'GRSM' .and.&
      &FIELD(IPVM+1) /= 'DEPOS' .and.&
      &FIELD(IPVM+1) /= 'AWMADW' .and.&
      &FIELD(IPVM+1) /= 'TTRM' .and.&
      &FIELD(IPVM+1) /= 'TTRM2'.and.&
      &FIELD(IPVM+1) /= 'PLATFORM' .and.&
      &FIELD(IPVM+1) /= 'URBANDB' .and.&
      &FIELD(IPVM+1) /= 'BLPDBUG' .and.&
      &FIELD(IPVM+1) /= 'SWPOINT'.and.&
      &FIELD(IPVM+1) /= 'AIRCRAFT' .and.&
      &FIELD(IPVM+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the PVMRM debug option
         DBPVFIL = RUNST1(LOCB(IPVM+1):LOCE(IPVM+1))
      ELSE
! ---       Assign default PVMRM debug filename
         IF (PVMRM) THEN
            DBPVFIL = 'PVMRM.DBG'
         END IF
      END IF
! ---    Assign default filename for RELDISP debug file for PVMRM option
      RDISPFIL = 'RelDisp.dbg'
   END IF

   IF (OLMDEBUG) THEN
      IF (IFC >= IOLMD+1 .and.&
      &FIELD(IOLMD+1) /= 'MODEL' .and.&
      &FIELD(IOLMD+1) /= 'METEOR' .and.&
      &FIELD(IOLMD+1) /= 'AREA' .and.&
      &FIELD(IOLMD+1) /= 'LINE' .and.&
      &FIELD(IOLMD+1) /= 'RLINE' .and.&
      &FIELD(IOLMD+1) /= 'PRIME' .and.&
      &FIELD(IOLMD+1) /= 'PVMRM' .and.&
      &FIELD(IOLMD+1) /= 'ARM2' .and.&
      &FIELD(IOLMD+1) /= 'GRSM' .and.&
      &FIELD(IOLMD+1) /= 'DEPOS' .and.&
      &FIELD(IOLMD+1) /= 'AWMADW' .and.&
      &FIELD(IOLMD+1) /= 'TTRM' .and.&
      &FIELD(IOLMD+1) /= 'TTRM2' .and.&
      &FIELD(IOLMD+1) /= 'PLATFORM' .and.&
      &FIELD(IOLMD+1) /= 'URBANDB' .and.&
      &FIELD(IOLMD+1) /= 'BLPDBUG' .and.&
      &FIELD(IOLMD+1) /= 'SWPOINT' .and.&
      &FIELD(IOLMD+1) /= 'AIRCRAFT' .and.&
      &FIELD(IOLMD+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the OLM debug option
         DBOLMFIL = RUNST1(LOCB(IOLMD+1):LOCE(IOLMD+1))
      ELSE
! ---       Assign default OLM debug filename
         DBOLMFIL = 'OLM.DBG'
      END IF
   END IF

   IF (ARM2DEBUG) THEN
      IF (IFC >= IARM2+1 .and.&
      &FIELD(IARM2+1) /= 'MODEL' .and.&
      &FIELD(IARM2+1) /= 'METEOR' .and.&
      &FIELD(IARM2+1) /= 'AREA' .and.&
      &FIELD(IARM2+1) /= 'LINE' .and.&
      &FIELD(IARM2+1) /= 'RLINE' .and.&
      &FIELD(IARM2+1) /= 'PRIME' .and.&
      &FIELD(IARM2+1) /= 'PVMRM' .and.&
      &FIELD(IARM2+1) /= 'OLM' .and.&
      &FIELD(IARM2+1) /= 'GRSM' .and.&
      &FIELD(IARM2+1) /= 'DEPOS' .and.&
      &FIELD(IARM2+1) /= 'AWMADW'.and.&
      &FIELD(IARM2+1) /= 'TTRM' .and.&
      &FIELD(IARM2+1) /= 'TTRM2' .and.&
      &FIELD(IARM2+1) /= 'PLATFORM' .and.&
      &FIELD(IARM2+1) /= 'URBANDB' .and.&
      &FIELD(IARM2+1) /= 'BLPDBUG' .and.&
      &FIELD(IARM2+1) /= 'SWPOINT' .and.&
      &FIELD(IARM2+1) /= 'AIRCRAFT' .and.&
      &FIELD(IARM2+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the ARM2 debug option
         DBARM2FIL = RUNST1(LOCB(IARM2+1):LOCE(IARM2+1))
      ELSE
! ---       Assign default ARM2 debug filename
         DBARM2FIL = 'ARM2.DBG'
      END IF
   END IF

!     CERC 11/30/20 Code for determining GRSM debug file name
   IF (GRSMDEBUG) THEN
      IF (IFC >= IGRSM+1 .and.&
      &FIELD(IGRSM+1) /= 'MODEL' .and.&
      &FIELD(IGRSM+1) /= 'METEOR' .and.&
      &FIELD(IGRSM+1) /= 'AREA' .and.&
      &FIELD(IGRSM+1) /= 'LINE' .and.&
      &FIELD(IGRSM+1) /= 'RLINE' .and.&
      &FIELD(IGRSM+1) /= 'PRIME' .and.&
      &FIELD(IGRSM+1) /= 'OLM' .and.&
      &FIELD(IGRSM+1) /= 'PVMRM' .and.&
      &FIELD(IGRSM+1) /= 'ARM2' .and.&
      &FIELD(IGRSM+1) /= 'DEPOS' .and.&
      &FIELD(IGRSM+1) /= 'AWMADW' .and.&
      &FIELD(IGRSM+1) /= 'TTRM' .and.&
      &FIELD(IGRSM+1) /= 'TTRM2' .and.&
      &FIELD(IGRSM+1) /= 'PLATFORM' .and.&
      &FIELD(IGRSM+1) /= 'URBANDB' .and.&
      &FIELD(IGRSM+1) /= 'BLPDBUG' .and.&
      &FIELD(IGRSM+1) /= 'SWPOINT' .and.&
      &FIELD(IGRSM+1) /= 'AIRCRAFT' .and.&
      &FIELD(IGRSM+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the GRSM debug option
         DBGRSMFIL = RUNST1(LOCB(IGRSM+1):LOCE(IGRSM+1))
      ELSE
! ---       Assign default GRSM debug filename
         DBGRSMFIL = 'GRSM.DBG'
      END IF
   END IF

   IF (AWMADWDBG) THEN
      IF (IFC >= IPRM2+1 .and.&
      &FIELD(IPRM2+1) /= 'MODEL' .and.&
      &FIELD(IPRM2+1) /= 'METEOR' .and.&
      &FIELD(IPRM2+1) /= 'AREA' .and.&
      &FIELD(IPRM2+1) /= 'LINE' .and.&
      &FIELD(IPRM2+1) /= 'RLINE' .and.&
      &FIELD(IPRM2+1) /= 'PRIME' .and.&
      &FIELD(IPRM2+1) /= 'PVMRM' .and.&
      &FIELD(IPRM2+1) /= 'OLM' .and.&
      &FIELD(IPRM2+1) /= 'ARM2' .and.&
      &FIELD(IPRM2+1) /= 'PLATFORM' .and.&
      &FIELD(IPRM2+1) /= 'GRSM' .and.&
      &FIELD(IPRM2+1) /= 'TTRM' .and.&
      &FIELD(IPRM2+1) /= 'TTRM2' .and.&
      &FIELD(IPRM2+1) /= 'DEPOS' .and.&
      &FIELD(IPRM2+1) /= 'URBANDB' .and.&
      &FIELD(IPRM2+1) /= 'BLPDBUG' .and.&
      &FIELD(IPRM2+1) /= 'SWPOINT' .and.&
      &FIELD(IPRM2+1) /= 'AIRCRAFT' .and.&
      &FIELD(IPRM2+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the PRIME debug option
         DBAwmaDwFIL = RUNST1(LOCB(IPRM2+1):LOCE(IPRM2+1))
      ELSE
! ---       Assign default AWMADW debug filename
         DBAwmaDwFIL = 'AWMADW.DBG'
      END IF
   END IF

   IF (RLINEDBG) THEN
      IF (IFC >= IRLINE +1 .and.&
      &FIELD(IRLINE+1) /= 'MODEL' .and.&
      &FIELD(IRLINE+1) /= 'METEOR' .and.&
      &FIELD(IRLINE+1) /= 'AREA' .and.&
      &FIELD(IRLINE+1) /= 'LINE' .and.&
      &FIELD(IRLINE+1) /= 'PRIME' .and.&
      &FIELD(IRLINE+1) /= 'PVMRM' .and.&
      &FIELD(IRLINE+1) /= 'OLM' .and.&
      &FIELD(IRLINE+1) /= 'ARM2' .and.&
      &FIELD(IRLINE+1) /= 'GRSM' .and.&
      &FIELD(IRLINE+1) /= 'PLATFORM' .and.&
      &FIELD(IRLINE+1) /= 'AWMADW' .and.&
      &FIELD(IRLINE+1) /= 'TTRM' .and.&
      &FIELD(IRLINE+1) /= 'TTRM2' .and.&
      &FIELD(IRLINE+1) /= 'URBANDB'.and.&
      &FIELD(IRLINE+1) /= 'DEPOS' .and.&
      &FIELD(IRLINE+1) /= 'BLPDBUG' .and.&
      &FIELD(IRLINE+1) /= 'SWPOINT' .and.&
      &FIELD(IRLINE+1) /= 'AIRCRAFT' .and.&
      &FIELD(IRLINE+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the RLINE debug option
         RLINEDBGFIL = RUNST1(LOCB(IRLINE+1):LOCE(IRLINE+1))
! ---    Add filename for the RLINE gridded wind speed debug file Wood 10/10/22
         RLINEDBGFIL_WS = RUNST1(LOCB(IRLINE+1):LOCE(IRLINE+1)-4)//&
         &"_GRIDWS"//&
         &RUNST1(LOCE(IRLINE+1)-3:LOCE(IRLINE+1))
      ELSE
! ---       Assign default RLINE debug filename
         RLINEDBGFIL = 'RLINE.DBG'
! ---    Add filename for the RLINE gridded wind speed debug file Wood 10/10/22
! ---       Assign default RLINE Grid WS debug filename
         RLINEDBGFIL_WS = 'RLINE_GRIDWS.DBG'
      END IF
   END IF

!CRT  D063 Platform Downwash Debug
   IF (PLATFMDBG) THEN
      IF (IFC >= IPLATFM+1 .and.&
      &FIELD(IPLATFM+1) /= 'MODEL' .and.&
      &FIELD(IPLATFM+1) /= 'METEOR' .and.&
      &FIELD(IPLATFM+1) /= 'AREA' .and.&
      &FIELD(IPLATFM+1) /= 'LINE' .and.&
      &FIELD(IPLATFM+1) /= 'RLINE' .and.&
      &FIELD(IPLATFM+1) /= 'PRIME' .and.&
      &FIELD(IPLATFM+1) /= 'PVMRM' .and.&
      &FIELD(IPLATFM+1) /= 'OLM' .and.&
      &FIELD(IPLATFM+1) /= 'ARM2' .and.&
      &FIELD(IPLATFM+1) /= 'DEPOS' .and.&
      &FIELD(IPLATFM+1) /= 'GRSM' .and.&
      &FIELD(IPLATFM+1) /= 'TTRM' .and.&
      &FIELD(IPLATFM+1) /= 'TTRM2' .and.&
      &FIELD(IPLATFM+1) /= 'URBANDB'.and.&
      &FIELD(IPLATFM+1) /= 'AWMADW' .and.&
      &FIELD(IPLATFM+1) /= 'SWPOINT'.and.&
      &FIELD(IPLATFM+1) /= 'AIRCRAFT'.and.&
      &FIELD(IPLATFM+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the PRIME debug option
         PLATFMDBGFILE = RUNST1(LOCB(IPLATFM+1):LOCE(IPLATFM+1))
      ELSE
! ---       Assign default PLATFORM debug filename
         PLATFMDBGFILE = 'PLATFORM.DBG'
      END IF
   END IF

! --- Now check for DEPOS option; since DEPOS debug filenames are
!     hardwired, issue warning if user appears to have specified
!     a filename
   IF (DEPOSDBG) THEN
!         JAT 05/08/2020 added from version 19191
!         wet deposition parameters are written to debug file
!         regardless if MODEL debug is chosen.  if model debug
!         not chosen, file is fort.24.  change to DEPOS.DBG
!         if dbgfil not named or next field is not MODEL
      IF (TRIM(ADJUSTL(DBGFIL)) == '' .or. FIELD(IDEP+1) /=&
      &'MODEL') DBGFIL='DEPOS.DBG'
      IF (IFC >= IDEP+1 .and.&
      &FIELD(IDEP+1) /= 'MODEL' .and.&
      &FIELD(IDEP+1) /= 'METEOR' .and.&
      &FIELD(IDEP+1) /= 'AREA' .and.&
      &FIELD(IDEP+1) /= 'LINE' .and.&
      &FIELD(IDEP+1) /= 'PRIME' .and.&
      &FIELD(IDEP+1) /= 'PVMRM' .and.&
      &FIELD(IDEP+1) /= 'ARM2' .and.&
      &FIELD(IDEP+1) /= 'OLM' .and.&
      &FIELD(IDEP+1) /= 'PLATFORM' .and.&
      &FIELD(IDEP+1) /= 'GRSM' .and.&
      &FIELD(IDEP+1) /= 'TTRM' .and.&
      &FIELD(IDEP+1) /= 'AWMADW' .and.&
      &FIELD(IDEP+1) /= 'TTRM2' .and.&
      &FIELD(IDEP+1) /= 'RLINE' .and.&
      &FIELD(IDEP+1) /= 'URBANDB' .and.&
      &FIELD(IDEP+1) /= 'BLPDBUG' .and.&
      &FIELD(IDEP+1) /= 'SWPOINT' .and.&
      &FIELD(IDEP+1) /= 'AIRCRAFT' .and.&
      &FIELD(IDEP+1) /= 'HBPDBG') THEN

! ---       Write warning message regarding DEPOS debug filenames
         CALL ERRHDL(PATH,MODNAM,'W','203','DEPOSDBG')
      END IF
   END IF

!     Added for TTRM; AECOM
   IF (TTRMDBG) THEN
      IF (IFC >= ITTRMD+1 .and.&
      &FIELD(ITTRMD+1) /= 'METEOR' .and.&
      &FIELD(ITTRMD+1) /= 'MODEL' .and.&
      &FIELD(ITTRMD+1) /= 'PRIME' .and.&
      &FIELD(ITTRMD+1) /= 'PVMRM' .and.&
      &FIELD(ITTRMD+1) /= 'OLM' .and.&
      &FIELD(ITTRMD+1) /= 'ARM2' .and.&
      &FIELD(ITTRMD+1) /= 'GRSM' .and.&
      &FIELD(ITTRMD+1) /= 'AREA' .and.&
      &FIELD(ITTRMD+1) /= 'LINE' .and.&
      &FIELD(ITTRMD+1) /= 'AWMADW' .and.&
      &FIELD(ITTRMD+1) /= 'RLINE' .and.&
      &FIELD(ITTRMD+1) /= 'PLATFORM' .and.&
      &FIELD(ITTRMD+1) /= 'DEPOS' .and.&
      &FIELD(ITTRMD+1) /= 'TTRM2' .and.&
      &FIELD(ITTRMD+1) /= 'DEPOS'.and.&
      &FIELD(ITTRMD+1) /= 'URBANDB'.and.&
      &FIELD(ITTRMD+1) /= 'BLPDBUG' .and.&
      &FIELD(ITTRMD+1) /= 'SWPOINT' .and.&
      &FIELD(ITTRMD+1) /= 'AIRCRAFT' .and.&
      &FIELD(ITTRMD+1) /= 'HBPDBG') THEN

!      added for TTRM; AECOM
! ---       Assign user-specified filename for the TTRM debug option
         TTRMFIL = RUNST1(LOCB(ITTRMD+1):LOCE(ITTRMD+1))
      ELSE
! ---       Assign default Ozone Reaction Rate debug filename
         TTRMFIL = 'TTRM_DEBUG.DBG'
      END IF
   END IF
!     End TTRM insert; Feb. 2021

!     Open TTRM2 debug files; the filenames are hard-wired
   IF (RUNTTRM2 .and. TTRM2DBG) THEN
      OPEN(UNIT=TTRM2TMP(1),FILE='AFTER_TTRM.DBG',&
      &ERR=779,STATUS='REPLACE')
!       Assign 2nd TTRM2 debug file based on selected method
      IF (ARM2) THEN
         OPEN(UNIT=TTRM2TMP(2),FILE='AFTER_ARM2.DBG',ERR=780,&
         &STATUS='REPLACE')
      ELSEIF (OLM) THEN
         OPEN(UNIT=TTRM2TMP(2),FILE='AFTER_OLM.DBG',ERR=780,&
         &STATUS='REPLACE')
      ELSE
         OPEN(UNIT=TTRM2TMP(2),FILE='AFTER_PVMRM.DBG',ERR=780,&
         &STATUS='REPLACE')
      ENDIF
      OPEN(UNIT=TTRM2TMP(3),FILE='TTRM2_MERGE.DBG',ERR=781,&
      &STATUS='REPLACE')
!       Write headers for TTRM2 debug files
      DO I = 1, 3
         WRITE(TTRM2TMP(I),7005) VERSN, TITLE1(1:68),&
         &RUNDAT, RUNTIM
         WRITE(TTRM2TMP(I),7020)
      ENDDO
      GO TO 7799
7005  FORMAT('* AERMOD (',A6,'): ',A68,5X,A8,4X,A8)

7020  FORMAT('*        X             Y      AVERAGE CONC    ZELEV    ',&
      &'ZHILL    ZFLAG    AVE     GRP       DATE     SRC ID   ',&
      &/,&
      &'* ____________  ____________  ____________   ______   _',&
      &'_____   ______  ______  ________  ________  ________  ')
!        WRITE Error Message for Error Opening File
779   WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(1)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
780   WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(2)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
781   WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(3)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
7799  CONTINUE
   ENDIF


!RCO D095 Added for urban debug 8/3/2021
   IF (URBDBUG) THEN
      IF (IFC >= IURBD+1 .and.&
      &FIELD(IURBD+1) /= 'METEOR' .and.&
      &FIELD(IURBD+1) /= 'MODEL' .and.&
      &FIELD(IURBD+1) /= 'AREA' .and.&
      &FIELD(IURBD+1) /= 'LINE' .and.&
      &FIELD(IURBD+1) /= 'PRIME' .and.&
      &FIELD(IURBD+1) /= 'PVMRM' .and.&
      &FIELD(IURBD+1) /= 'OLM' .and.&
      &FIELD(IURBD+1) /= 'ARM2' .and.&
      &FIELD(IURBD+1) /= 'GRSM' .and.&
      &FIELD(IURBD+1) /= 'PLATFORM' .and.&
      &FIELD(IURBD+1) /= 'DEPOS'.and.&
      &FIELD(IURBD+1) /= 'AWMADW' .and.&
      &FIELD(IURBD+1) /= 'TTRM' .and.&
      &FIELD(IURBD+1) /= 'TTRM2' .and.&
      &FIELD(IURBD+1) /= 'RLINE' .and.&
      &FIELD(IURBD+1) /= 'BLPDBUG' .and.&
      &FIELD(IURBD+1) /= 'SWPOINT' .and.&
      &FIELD(IURBD+1) /= 'AIRCRAFT' .and.&
      &FIELD(IURBD+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the URBDBUG debug option
         URBFIL = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1))
         URBFIL1 = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1)) // "1"
!RCO - D168 Debug files. Add output file for second urban debug
         URBFIL2 = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1)) // "2"
      ELSE
! ---       Assign default URBDBUG debug filename
         URBFIL = 'URBDBUG.DBG'
         URBFIL1 = 'URBDBUG1.DBG'
         URBFIL2 = 'URBDBUG2.DBG'
      END IF
   END IF
! End URBDBUG insert

!     CRCO D095 BLP Debug
   IF (BLPDBUG) THEN
      IF (IFC >= IBLP+1 .and.&
      &FIELD(IBLP+1) /= 'METEOR' .and.&
      &FIELD(IBLP+1) /= 'MODEL' .and.&
      &FIELD(IBLP+1) /= 'AREA' .and.&
      &FIELD(IBLP+1) /= 'LINE' .and.&
      &FIELD(IBLP+1) /= 'PRIME' .and.&
      &FIELD(IBLP+1) /= 'PVMRM' .and.&
      &FIELD(IBLP+1) /= 'OLM' .and.&
      &FIELD(IBLP+1) /= 'ARM2' .and.&
      &FIELD(IBLP+1) /= 'GRSM' .and.&
      &FIELD(IBLP+1) /= 'PLATFORM' .and.&
      &FIELD(IBLP+1) /= 'DEPOS'.and.&
      &FIELD(IBLP+1) /= 'AWMADW' .and.&
      &FIELD(IBLP+1) /= 'TTRM' .and.&
      &FIELD(IBLP+1) /= 'TTRM2' .and.&
      &FIELD(IBLP+1) /= 'RLINE' .and.&
      &FIELD(IBLP+1) /= 'URBANDB' .and.&
      &FIELD(IBLP+1) /= 'SWPOINT' .and.&
      &FIELD(IBLP+1) /= 'AIRCRAFT' .and.&
      &FIELD(IBLP+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the DISTANCE debug option
         BLPFIL = RUNST1(LOCB(IBLP+1):LOCE(IBLP+1))
      ELSE
! ---       Assign default BLPDBUG debug filename
         BLPFIL = 'BLPDBUG.DBG'
      END IF
   END IF
!     End BLPDBUG insert

!     CRT, 3/25/2022 D113 Updated for SIDEWASH
   IF (SWDBG) THEN
      IF (IFC >= ISW+1 .and.&
      &FIELD(ISW+1) /= 'METEOR' .and.&
      &FIELD(ISW+1) /= 'MODEL' .and.&
      &FIELD(ISW+1) /= 'PRIME' .and.&
      &FIELD(ISW+1) /= 'PVMRM' .and.&
      &FIELD(ISW+1) /= 'OLM' .and.&
      &FIELD(ISW+1) /= 'ARM2' .and.&
      &FIELD(ISW+1) /= 'GRSM' .and.&
      &FIELD(ISW+1) /= 'AREA' .and.&
      &FIELD(ISW+1) /= 'LINE' .and.&
      &FIELD(ISW+1) /= 'AWMADW' .and.&
      &FIELD(ISW+1) /= 'PLATFORM' .and.&
      &FIELD(ISW+1) /= 'DEPOS' .and.&
      &FIELD(ISW+1) /= 'TTRM' .and.&
      &FIELD(ISW+1) /= 'TTRM2' .and.&
      &FIELD(ISW+1) /= 'RLINE' .and.&
      &FIELD(ISW+1) /= 'URBANDB'.and.&
      &FIELD(ISW+1) /= 'AIRCRAFT' .and.&
      &FIELD(ISW+1) /= 'HBPDBG') THEN

! ---       Assign user-specified filename for the SIDEWASH debug option
         SWFIL = RUNST1(LOCB(ISW+1):LOCE(ISW+1))
      ELSE
! ---       Assign default Sidewash source debug filename
         SWFIL = 'SWPOINT.DBG'
      END IF
   END IF

!**   Added for Aircraft Plume Rise; UNC-IE
   IF (ARCFTDEBUG) THEN
      IF (IFC >= IARCFT+1 .and.&
      &FIELD(IARCFT+1) /= 'METEOR' .and.&
      &FIELD(IARCFT+1) /= 'MODEL' .and.&
      &FIELD(IARCFT+1) /= 'AREA' .and.&
      &FIELD(IARCFT+1) /= 'LINE' .and.&
      &FIELD(IARCFT+1) /= 'PRIME' .and.&
      &FIELD(IARCFT+1) /= 'PVMRM' .and.&
      &FIELD(IARCFT+1) /= 'OLM' .and.&
      &FIELD(IARCFT+1) /= 'ARM2' .and.&
      &FIELD(IARCFT+1) /= 'GRSM' .and.&
      &FIELD(IARCFT+1) /= 'PLATFORM' .and.&
      &FIELD(IARCFT+1) /= 'DEPOS'.and.&
      &FIELD(IARCFT+1) /= 'AWMADW' .and.&
      &FIELD(IARCFT+1) /= 'TTRM'.and.&
      &FIELD(IARCFT+1) /= 'RLINE' .and.&
      &FIELD(IARCFT+1) /= 'URBANDB'.and.&
      &FIELD(IARCFT+1) /= 'BLPDBUG' .and.&
      &FIELD(IARCFT+1) /= 'HBPDBG') THEN
! ---       Assign user-specified filename for the AIRCRAFT debug option
         DBARCFTFIL = RUNST1(LOCB(IARCFT+1):LOCE(IARCFT+1))
      ELSE
! ---       Assign default AIRCRAFT debug filename
         DBARCFTFIL = 'AIRCRAFT.DBG'
      END IF
   END IF
!**  End Aircraft Plume Rise insert; April 2023

! Added for HBP DEBUG, JAN 2023
   IF (HBPDBG) THEN
      IF (IFC >= IHBP+1 .and.&
      &FIELD(IHBP+1) /= 'METEOR' .and.&
      &FIELD(IHBP+1) /= 'MODEL' .and.&
      &FIELD(IHBP+1) /= 'AREA' .and.&
      &FIELD(IHBP+1) /= 'LINE' .and.&
      &FIELD(IHBP+1) /= 'PRIME' .and.&
      &FIELD(IHBP+1) /= 'PVMRM' .and.&
      &FIELD(IHBP+1) /= 'OLM' .and.&
      &FIELD(IHBP+1) /= 'ARM2' .and.&
      &FIELD(IHBP+1) /= 'GRSM' .and.&
      &FIELD(IHBP+1) /= 'PLATFORM' .and.&
      &FIELD(IHBP+1) /= 'DEPOS'.and.&
      &FIELD(IHBP+1) /= 'AWMADW' .and.&
      &FIELD(IHBP+1) /= 'TTRM'.and.&
      &FIELD(IHBP+1) /= 'TTRM2' .and.&
      &FIELD(IHBP+1) /= 'RLINE' .and.&
      &FIELD(IHBP+1) /= 'URBANDB' .and.&
      &FIELD(IHBP+1) /= 'SWPOINT' .and.&
      &FIELD(IHBP+1) /= 'BLPDBUG' .and.&
      &FIELD(IHBP+1) /= 'URBANDB'.and.&
      &FIELD(IHBP+1) /= 'AIRCRAFT') THEN
! ---       Assign user-specified filename for the HBPBUG debug option
         HBPFIL = RUNST1(LOCB(IHBP+1):LOCE(IHBP+1))

      ELSE
! ---       Assign default HBPDBG debug filename
         HBPFIL = 'HBP_DEBUG.DBG'
      END IF
   END IF
! End HBP DEBUG, JAN 2023

! --- Open MODEL, METEOR, AREA, PRIME and AWMADW debug files, if selected;
!     note that PVMRM, OLM, ARM2, GRSM and DEPOS debug files are opened
!     elsewhere
! Unused:  200 FORMAT ( ' OPTIONS: ', A /)
!     JAT 05/08/2020 ADD CODE TO OPEN IF DEBUG OR DEPOSDBG
!     BECAUSE IT USES THE DEBUGFIL AS WELL
!      IF (DEBUG) THEN
   IF (DEBUG .or. DEPOSDBG) THEN
!        Open debug output file
      DUMMY = 'DebugFile'
      OPEN (UNIT=DBGUNT,FILE=DBGFIL,ERR=91,STATUS='REPLACE')
   END IF

   GOTO 101

!     WRITE Error Message:  Error Opening File
91 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

101 CONTINUE

   IF (METEORDBG) THEN
!        Open debug meteorology output file
      DUMMY = 'DbgMetFile'
      OPEN (UNIT=DBMUNT,FILE=DBMFIL,ERR=92,STATUS='REPLACE')
   END IF

   GOTO 102

!     WRITE Error Message:  Error Opening File
92 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

102 CONTINUE

   IF (AREADBG) THEN
!        Open debug AREA output file
      DUMMY = 'AreaDbgFile'
      OPEN (UNIT=AREADBUNT,FILE=DBAREAFIL,ERR=93,STATUS='REPLACE')
   END IF

   GOTO 103

!     WRITE Error Message:  Error Opening File
93 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

103 CONTINUE

   IF (PRIMEDBG) THEN
!        Open debug PRIME output file
      DUMMY = 'PrimeDbgFile'
      OPEN (UNIT=PRMDBUNT,FILE=DBPRMFIL,ERR=94,STATUS='REPLACE')
   END IF

   GOTO 104

!     WRITE Error Message:  Error Opening File
94 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

104 CONTINUE

   IF (AWMADWDBG) THEN
!        Open debug AWMADW output file
      DUMMY = 'AwmaDwDbgFile'
      OPEN (UNIT=AwmaDwDBUNT,FILE=DBAwmaDwFIL,ERR=95,&
      &STATUS='REPLACE')
   END IF

   GOTO 105

!     WRITE Error Message:  Error Opening File
95 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

105 CONTINUE


   IF (TTRMDBG) THEN
!        Open TTRM output file
      DUMMY = 'TTRMFIL'
      OPEN (UNIT=TTRMUNT,FILE=TTRMFIL,ERR=96,STATUS='REPLACE')
      WRITE(TTRMUNT,'(''TTRM Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
   END IF

   GOTO 106


!     WRITE Error Message:  Error Opening File
96 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

106 CONTINUE

   IF (RLINEDBG) THEN
!        Open RLINE DEBUG output file
      DUMMY = 'RLINEFIL'
      OPEN (UNIT=RLINEDBUNT,FILE=RLINEDBGFIL,ERR=97,&
      &STATUS='REPLACE')
!       Write inital header Wood 10/10/22
      WRITE(RLINEDBUNT,'(''RLINE Debug File'',1x,a8,5x,a8)')&
      &rundat, runtim
!        Open second RLINE DEBUG output file for gridded wind speed Wood 10/10/22
      DUMMY = 'RLINEFIL_WS'
      OPEN (UNIT=RLINEDBUNT_WS,FILE=RLINEDBGFIL_WS,ERR=97,&
      &STATUS='REPLACE')
      WRITE(RLINEDBUNT_WS,'(''RLINE Gridded Wind Speed Debug File'',&
      &                          51x,a8,5x,a8)') rundat, runtim
      WRITE(RLINEDBUNT_WS,2234)
2234  FORMAT (3X,'YR',2X,'MO',2X,'DY',2X,'HR',3X,'HT_m',5x,'WS_m_s')
   END IF
   GOTO 107

!     WRITE Error Message:  Error Opening File
97 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

107 CONTINUE

!RCO D095 Added for urban debug 8/3/2021
! -- Open URBDBUG debug file, if selected;
   IF (URBDBUG) THEN
!        Open URBDBUG output file
      DUMMY = 'URBFIL'
      OPEN (UNIT=URBUNT,FILE=URBFIL,ERR=197,STATUS='REPLACE')
      WRITE(URBUNT,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim

      DUMMY = 'URBFIL1'
      OPEN (UNIT=URBUNT1,FILE=URBFIL1,ERR=197,STATUS='REPLACE')
      WRITE(URBUNT1,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
!RCO - D168 Debug files. Add output file for second urban debug
      DUMMY = 'URBFIL2'
      OPEN (UNIT=URBUNT2,FILE=URBFIL2,ERR=197,STATUS='REPLACE')
      WRITE(URBUNT2,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
   END IF

   GOTO 108
!     WRITE Error Message:  Error Opening File
197 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
108 CONTINUE
!   end of URBDBUG insert


!CRCO D095 BLP debug file
   IF (BLPDBUG) THEN
!        Open BLPDBUG output file
      DUMMY = 'BLPFIL'
      OPEN (UNIT=BLPUNT,FILE=BLPFIL,ERR=198,STATUS='REPLACE')
!        D140 removed exisiting BLP header to match the formatting for the other debug files Wood 9/29/22
!         WRITE(BLPUNT,'(''BLPDBUG Debug File'',51x,a8,/70x,a8)')
!     &                                           rundat, runtim
   END IF

   GOTO 109
!     WRITE Error Message:  Error Opening File
198 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
109 CONTINUE
!   end of BLPDBUG insert

!CRT 3/22/2021: File is checked and opened in aermod.f
!CRT comment out this code - leave for reference if needed later
!CRT      IF (GRSMDEBUG) THEN
!CRT!        Open GRSM output file
!CRT         DUMMY = 'GRSMFIL'
!CRT         OPEN (UNIT=GRSMDBG,FILE=DBGRSMFIL,ERR=97,STATUS='REPLACE')
!CRT         WRITE(GRSMDBG,'(''GRSM Debug File'',51x,a8,/70x,a8)')
!CRT     &                                           rundat, runtim
!CRT      END IF
!CRT
!CRT      GOTO 107
!CRT
!CRTC     WRITE Error Message:  Error Opening File
!CRT 97   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
!CRT
! 110  CONTINUE

!CRT  D063 Platform Downwash Debug
   IF (PLATFMDBG) THEN
!        Open debug PLATFORM output file
      DUMMY = 'PLATFMDBGFILE'
      OPEN (UNIT=PLATFMDBUNT,FILE=PLATFMDBGFILE,ERR=98,&
      &STATUS='REPLACE')
   END IF

   GOTO 110

!     WRITE Error Message:  Error Opening File
98 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

110 CONTINUE

!CRT  D113 Sidewash Debug, 3/25/2022
   IF (SWDBG) THEN
!        Open debug Sidewash output file
      DUMMY = 'SWFIL'
      OPEN (UNIT=SWDBGUNT,FILE=SWFIL,ERR=99,&
      &STATUS='REPLACE')
   END IF

   GOTO 111

!     WRITE Error Message:  Error Opening File
99 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

111 CONTINUE

!** Added for Aircraft Plume Rise; UNC-IE
   IF (ARCFTDEBUG) THEN
!       Open Aircraft output file
      DUMMY = 'ARCFTFIL'
      OPEN (UNIT=ARCFTDBG,FILE=DBARCFTFIL,ERR=97,STATUS='REPLACE')
!      Write the standard header information with AERMOD/AERMET
!      versions, 1st title, and rundat/runtim to the debug output file
      WRITE(ARCFTDBG,7028) VERSN, TITLE1(1:68), RUNDAT, RUNTIM
7028  FORMAT('*** AERMOD - VERSION ',A6,' ***',52X,'*** ',A28,&
      &' ***',2X,A8,/115X,' ***',2X,A8/)

      WRITE(ARCFTDBG,'(''*********************************************&
      &   Aircraft Plume Rise - Debug File  *****************************&
      &******************'')')

   END IF
   GOTO 112
!     WRITE Error Message:  Error Opening File
100 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

112 CONTINUE
!** End Aircraft Plume Rise insert; April 2023

! Added for HBP debug, JAN 2023
   IF (HBPDBG) THEN
!        Open HBP output file
      DUMMY = 'HBPFIL'
      OPEN (UNIT=HBPUNT,FILE=HBPFIL,ERR=797,STATUS='REPLACE')
      WRITE(HBPUNT,'(''HBP Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
   END IF

   GOTO 7107

!     WRITE Error Message:  Error Opening File
797 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

7107 CONTINUE
! End HBP insert

   IF (IFC > MAXFields) THEN
!        Maximum number of fields exceeded, issue warning message,
!        including up to 12 characters from last field
      WRITE(DUMMY,'(A:)') FIELD(IFC)(1:MIN(12,LEN_TRIM(FIELD(IFC))))
      CALL ERRHDL(PATH,MODNAM,'E','203',DUMMY)
   END IF

   GO TO 999

!     WRITE Error Message:  Error Opening File
! Unused:  99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

999 RETURN
END SUBROUTINE DEBOPT

SUBROUTINE MYEAR
!***********************************************************************
!                 MYEAR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process RESTART File Save Option
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Treat the 'H6H' field as optional, with a warning
!                   that it is no longer required.
!                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
!
!        ERROR HANDLING:   Checks for No Parametes (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'MYEAR'

   IF (RSTSAV) THEN
      CALL ERRHDL(PATH,MODNAM,'E','150','SAVEFILE')

   ELSE IF (RSTINP) THEN
      CALL ERRHDL(PATH,MODNAM,'E','150','INITFILE')

   ELSE IF (.NOT. (POLLUT == 'PM10' .or. POLLUT == 'PM-10' .or.&
   &POLLUT == 'NO2'  .or. POLLUT == 'SO2'   .or.&
   &POLLUT == 'LEAD' .or. POLLUT == 'OTHER' .or.&
   &POLLUT == 'PM25' .or. POLLUT == 'PM-2.5'.or.&
   &POLLUT == 'PM-25'.or. POLLUT == 'PM2.5') )THEN
!        WRITE Error Message:  Conflicting Options MULTYEAR For Wrong POLLUT
      CALL ERRHDL(PATH,MODNAM,'E','150',POLLUT)

   ELSE IF (IFC >= 4 .and. FIELD(3) == 'H6H') THEN
! ---    Write Warning Message:  The 'H6H' field is no longer required
!        for the MULTYEAR keyword
      CALL ERRHDL(PATH,MODNAM,'W','352','Keyword ')
      IF (IFC == 4) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         IF ((LOCE(4)-LOCB(4)) <= (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(4):LOCE(4))
         ELSE
!              WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC == 5) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         IF ((LOCE(4)-LOCB(4)) <= (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(4):LOCE(4))
         ELSE
!              WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
         RSTINP = .TRUE.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         IF ((LOCE(5)-LOCB(5)) <= (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            INIFIL = RUNST1(LOCB(5):LOCE(5))
         ELSE
!              WRITE Error Message:  INIFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC > 5) THEN
!           WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      END IF
   ELSE IF (IFC >= 3 .and. FIELD(3) /= 'H6H') THEN
! ---    Process input parameters without the 'H6H' keyword
      IF (IFC == 3) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         IF ((LOCE(3)-LOCB(4)) <= (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
!              WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC == 4) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         IF ((LOCE(3)-LOCB(3)) <= (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
!              WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
         RSTINP = .TRUE.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         IF ((LOCE(4)-LOCB(4)) <= (ILEN_FLD - 1) ) THEN
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            INIFIL = RUNST1(LOCB(4):LOCE(4))
         ELSE
!              WRITE Error Message:  INIFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC > 4) THEN
!           WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      END IF
   ELSE IF (IFC == 3 .and. FIELD(3) == 'H6H') THEN
!        WRITE Error Message           ! Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
   ELSE IF (IFC < 3) THEN
!        WRITE Error Message           ! No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
   END IF

   RETURN
END SUBROUTINE MYEAR

SUBROUTINE GDDEF
!***********************************************************************
!                 GDDEF Module of AERMOD Model
!
!        PURPOSE: Processes Dry Deposition Default Parameters for Gases
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:    May 16, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Dry Deposition Reference Parameters for Gases
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'GDDEF'

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 5) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 6) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Read Gas Dry Deposition Parameters
!     Change Them To Numbers
!     First Get Reactivity Value (fo)
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF
!     Assign The Field
   Fo = DNUM

!     Now Get Fraction of Maximum Green LAI for Seasonal Category 2
   CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF
!     Assign The Field
   FSEAS2 = DNUM

!     Now Get Fraction of Maximum Green LAI for Seasonal Category 5
   CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF
!     Assign The Field
   FSEAS5 = DNUM

   IF (IFC == 6) THEN
!        Get the Reference Species (Optional)
      REFSPE = FIELD(6)
   ELSE
      REFSPE = '      '
   END IF

999 RETURN
END SUBROUTINE GDDEF

SUBROUTINE GDSEAS
!***********************************************************************
!                 GDSEAS Module of AERMOD Model
!
!        PURPOSE: Define Seasons for Gas Dry Deposition (per Wesely)
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:    May 18, 2001
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Dry Deposition Reference Parameters for Gases
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I, J, ISEA_NDX

!     Variable Initializations
   MODNAM = 'GDSEAS'

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 3) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 14) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   ISET = 0
   DO I = 3, IFC
!        Change Fields To Numbers
      CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         CYCLE
      END IF
      DO J = 1, IMIT
         ISET = ISET + 1
!           Assign The Field
         IF (ISET <= 12) THEN
            ISEA_NDX = NINT(FNUM)
            IF (ISEA_NDX >= 1 .and. ISEA_NDX <= 5) THEN
               ISEAS_GD(ISET) = ISEA_NDX
            ELSE
!                 WRITE Error Message    ! Season Index out-of-range
               CALL ERRHDL(PATH,MODNAM,'E','380',KEYWRD)
            END IF
         ELSE
!              WRITE Error Message    ! Too Many Months Input
            CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
         END IF
      END DO
   END DO

999 RETURN
END SUBROUTINE GDSEAS

SUBROUTINE GVSUBD
!***********************************************************************
!                 GVSUBD Module of AERMOD Model
!
!        PURPOSE: Processes Dry Deposition Reference Parameters for Gases
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:    September 3, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: User-specified Dry Deposition Velocity for Gases
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'GVSUBD'

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 3) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 3) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Read User-specified Dry Deposition Velocity
!     Change Them To Numbers
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF
!     Assign The Field
   USERVD = DNUM

!     Perform range/validity check
   IF (USERVD < 0.0D0) THEN
!        Write Error Message:  Negative deposition velocity
      CALL ERRHDL(PATH,MODNAM,'E','209',' USERVD ')
   ELSE IF (USERVD == 0.0D0) THEN
!        Write Error Message:  Deposition velocity = 0.0
      CALL ERRHDL(PATH,MODNAM,'E','380','USERVD=0')
   ELSE IF (USERVD > 0.05D0) THEN
!        Write Warning Message:  Large deposition velocity
      CALL ERRHDL(PATH,MODNAM,'W','320',' USERVD ')
   END IF

!     Set Logical Variable for User-specified Deposition Velocity
   LUSERVD = .TRUE.

999 RETURN
END SUBROUTINE GVSUBD

SUBROUTINE GDLAND
!***********************************************************************
!                 GDLAND Module of AERMOD Model
!
!        PURPOSE: Define Land Use Categories by Direction for
!                 Gas Dry Deposition (per Wesely, et al, 2001)
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:    December 30, 2002
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Dry Deposition Reference Parameters for Gases
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I, J, ILAND_NDX

!     Variable Initializations
   MODNAM = 'GDLAND'

!     Check the Number of Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 3) THEN
!        Error Message: Not Enough Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 38) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   ISET = 0
   DO I = 3, IFC
!        Change Fields To Numbers
      CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         CYCLE
      END IF
      DO J = 1, IMIT
         ISET = ISET + 1
!           Assign The Field
         IF (ISET <= 36) THEN
            ILAND_NDX = NINT(FNUM)
            IF (ILAND_NDX >= 1 .and. ILAND_NDX <= 9) THEN
               ILAND_GD(ISET) = ILAND_NDX
            ELSE
!                 WRITE Error Message    ! Land Use Index out-of-range
               CALL ERRHDL(PATH,MODNAM,'E','380',KEYWRD)
            END IF
         ELSE
!              WRITE Error Message    ! Too Many Directions Input
            CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
         END IF
      END DO
   END DO

999 RETURN
END SUBROUTINE GDLAND

SUBROUTINE URBOPT
!***********************************************************************
!                 URBOPT Module of AERMOD Model
!
!        PURPOSE: Process Urban Option Inputs
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    June 11, 1996
!
!        MODIFIED:   Adjusted the limit for issuing a warning for urban
!                    population out-of-range from 10,000 to 21,206, which
!                    corresponds to a population density of 750/sq-km for
!                    an area within a 3km radius, consistent with the
!                    Appendix W criterion for urban/rural determination
!                    based on the population density.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 02/28/2011
!
!        MODIFIED:   To incorporate handling of non-'default' values of
!                    the optional urban roughness length other than 1m
!                    as non-DFAULT.
!                    To prohibit use of urban roughness length .ne. 1.0m
!                    for regulatory DFAULT applications.  Modified limits
!                    on urban roughness length to generate warning messages.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
!
!        MODIFIED:   To allow for multiple urban areas in a single
!                    model run, and adjust range for issuing warning
!                    regarding optional user-specified urban rounghness.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/06
!
!        MODIFIED:   To include optional parameter for urban roughness
!                    length.  Defaults to 1.0 meter if no value input.
!                    R.W. Brode, PES, Inc. - 09/10/02
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: URBPOP  [R]  Urban population
!                 URBNAM  [C]  Name of urban area (optional)
!                 URBZ0   [R]  Urban roughness lenght, m (optional)
!                                defaults to 1.0 meter
!
!        ERROR HANDLING:   Checks for Invalid Parameters;
!                          Checks for No Parameters;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   INTEGER :: I
   CHARACTER :: MODNAM*12, TEMPID*8

!     Variable Initializations
   MODNAM = 'URBOPT'

!     Determine Whether There Are Too Few Or Too Many Parameter Fields
   IF ((.NOT. L_MULTURB .and. IFC < 3) .or.&
   &(L_MULTURB .and. IFC < 4)) THEN
!        WRITE Error Message: Missing Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF ((.NOT. L_MULTURB .and. IFC > 5) .or.&
   &(L_MULTURB .and. IFC > 6)) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   IF (.NOT. L_URBAN_ALL .and. L_MULTURB) THEN
!        READ in the Urban ID for multiple urban areas
      IF ((LOCE(3)-LOCB(3)) <= 7) THEN
!*          Retrieve Source ID Character Substring
         TEMPID = FIELD(3)
      ELSE
!*          WRITE Error Message:  Urban ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','219',FIELD(ISC)(1:12))
         RECERR = .TRUE.
         GO TO 999
      END IF
      DO I = 1, NUMURB
         IF (TEMPID == URBID(I)) THEN
!              WRITE Error Message:  Urban ID already defined
            CALL ERRHDL(PATH,MODNAM,'E','303',TEMPID)
!              Exit to END
            GO TO 999
         END IF
      END DO

!        New Urban ID Defined, Increment Counters
      IURB = IURB + 1
      IF (IURB > NURB) THEN
!           WRITE Error Message    ! Too Many Urban Areas Specified
!           This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NURB='',I7)') NURB
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
!           Exit to END
         GO TO 999
      END IF
      NUMURB = NUMURB + 1
      URBID(IURB) = TEMPID

      IF (IFC >= 4) THEN
         CALL STODBL(FIELD(4),ILEN_FLD,URBPOP(IURB),IMIT)
         IF (IMIT /= 1) THEN
!              Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208','URB-POP')
         ELSE IF (URBPOP(IURB) < 100.0D0) THEN
! ---          Urban population below about 90 will cause math error
! ---          Write Error Message:Invalid Value Specified
            CALL ERRHDL(PATH,MODNAM,'E','203','URB-POP')
         ELSE IF (URBPOP(IURB) < 21206.0D0) THEN
! ---          Flag urban population below 21,206 as potentially out-of-range;
!              this value corresponds with a population density of 750/sq-km
!              across an area of 3km in radius, a criterion cited for urban
!              classification in Section 7.2.3(d) of Appendix W.
            CALL ERRHDL(PATH,MODNAM,'W','320','URB-POP')
         END IF
      END IF

      IF (IFC >= 5) THEN
!           Assign name of urban area (optional)
         URBNAM(IURB) = FIELD(5)
      END IF

      IF (IFC == 6) THEN
!           Assign value of urban roughness length (optional)
         CALL STODBL(FIELD(6),ILEN_FLD,URBZ0(IURB),IMIT)
         IF (IMIT /= 1) THEN
!              Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208','URBAN_Z0')
         ELSE
            IF (DFAULT .and. URBZ0(IURB) /= 1.0D0) THEN
!                 Write Warning Message: Non-default urban roughness length
               CALL ERRHDL(PATH,MODNAM,'W','206','URBAN_Z0')
               URBZ0(IURB) = 1.0D0
            ELSE IF (.NOT. DFAULT .and. URBZ0(IURB) /= 1.0D0) THEN
!                 Set flag for use of non-DEFAULT option
               L_NonDFAULT = .TRUE.
            END IF
            IF (URBZ0(IURB) < 0.80D0) THEN
!                 Write Warning Message: Urban roughness out of range
               WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
               CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
            ELSE IF (URBZ0(IURB) > 1.50D0 .and.&
            &URBZ0(IURB) < 5.0D0) THEN
!                 Write Warning Message: Urban roughness out of range
               WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
               CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
            ELSE IF (URBZ0(IURB) >= 5.0D0) THEN
!                 Write Error Message: Urban roughness out of range
               CALL ERRHDL(PATH,MODNAM,'E','380','URBAN Z0')
            END IF
         END IF
      ELSE
         URBZ0(IURB) = 1.0D0
      END IF

   ELSE IF (L_URBAN_ALL .and. L_MULTURB) THEN
!        Write Error Message: URBANSRC ALL option with
!        multiple URBAN areas
      CALL ERRHDL(PATH,MODNAM,'E','279','URBANSRC ALL')

   ELSE
!        Single Urban Area - Process Inputs without URBAN ID

      IURB = 1

      IF (IFC >= 3) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,URBPOP(IURB),IMIT)
         IF (IMIT /= 1) THEN
!              Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208','URB-POP')
         ELSE IF (URBPOP(IURB) < 100.0D0) THEN
! ---          Urban population below about 90 will cause math error
! ---          Write Error Message:Invalid Value Specified
            CALL ERRHDL(PATH,MODNAM,'E','203','URB-POP')
         ELSE IF (URBPOP(IURB) < 21206.0D0) THEN
! ---          Flag urban population below 21,206 as potentially out-of-range;
!              this value corresponds with a population density of 750/sq-km
!              across an area of 3km in radius, a criterion cited for urban
!              classification in Section 7.2.3(d) of Appendix W.
            CALL ERRHDL(PATH,MODNAM,'W','320','URB-POP')
         END IF
      END IF

      IF (IFC >= 4) THEN
!           Assign name of urban area (optional)
         URBNAM(IURB) = FIELD(4)
      END IF

      IF (IFC == 5) THEN
!           Assign value of urban roughness length (optional)
         CALL STODBL(FIELD(5),ILEN_FLD,URBZ0(IURB),IMIT)
         IF (IMIT /= 1) THEN
!              Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208','URBAN_Z0')
         ELSE
            IF (DFAULT .and. URBZ0(IURB) /= 1.0D0) THEN
!                 Write Warning Message: Non-default urban roughness length
               CALL ERRHDL(PATH,MODNAM,'W','206','URBAN_Z0')
               URBZ0(IURB) = 1.0D0
            ELSE IF (.NOT. DFAULT .and. URBZ0(IURB) /= 1.0D0) THEN
!                 Set flag for use of non-DEFAULT option
               L_NonDFAULT = .TRUE.
            END IF
            IF (URBZ0(IURB) < 0.80D0) THEN
!                 Write Warning Message: Urban roughness out of range
               WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
               CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
            ELSE IF (URBZ0(IURB) > 1.50D0 .and.&
            &URBZ0(IURB) < 5.0D0) THEN
!                 Write Warning Message: Urban roughness out of range
               WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
               CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
            ELSE IF (URBZ0(IURB) >= 5.0D0) THEN
!                 Write Error Message: Urban roughness out of range
               CALL ERRHDL(PATH,MODNAM,'E','380','URBAN Z0')
            END IF
         END IF
      ELSE
         URBZ0(IURB) = 1.0D0
      END IF

      NUMURB = 1

   END IF

!     Assign Logical for Urban Option
   URBAN  = .TRUE.

999 RETURN
END SUBROUTINE URBOPT

SUBROUTINE O3VAL
!***********************************************************************
!                 O3VAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Non-temporally-varying Ozone Value Option,
!                 CO OZONEVAL
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:    May 3, 2002
!
!        MODIFIED: To allow for sector-varying values
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, XX/YY/2013
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

   INTEGER :: I

   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'O3VAL'

! --- Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_O3Sector) THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 4) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
! ---    Check for SECT ID in field 3 in case O3SECTOR keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without O3SECTOR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified O3VALUES option and
!        assign the option to O3FLAG variable
      IO3SECT = 1
      I = 3
      L_O3VAL(IO3SECT) = .TRUE.

   ELSE
! ---    Process inputs based on O3SECTOR option
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC == 4) THEN
         IF (FIELD(3)(1:4) /= 'SECT') THEN
!              Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
            GO TO 999
         ELSE
!              Error Message: No Numerical Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
      ELSE IF (IFC > 5) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
! ---    Determine user-specified sector
      IF (FIELD(3) == 'SECT1') THEN
         IO3SECT = 1
      ELSE IF (FIELD(3) == 'SECT2') THEN
         IO3SECT = 2
      ELSE IF (FIELD(3) == 'SECT3') THEN
         IO3SECT = 3
      ELSE IF (FIELD(3) == 'SECT4') THEN
         IO3SECT = 4
      ELSE IF (FIELD(3) == 'SECT5') THEN
         IO3SECT = 5
      ELSE IF (FIELD(3) == 'SECT6') THEN
         IO3SECT = 6
      ELSE
!           Error Message: Invalid sector definition
         CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
         GO TO 999
      END IF

! ---    Set field index for the user-specified Ozone Value
      I = 4
      L_O3VAL(IO3SECT) = .TRUE.

   END IF

!     Get Ozone Value, O3BACK, for applicable sector
   CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

!     Assign value to O3BACK variable for this sector
   O3BACK(IO3SECT) = DNUM

!     Check for units of ozone value
   IF (IFC == I+1) THEN
      IF (FIELD(I+1)=='PPM' .or. FIELD(I+1)=='PPB' .or.&
      &FIELD(I+1)=='UG/M3') THEN
         O3VALUNITS = FIELD(I+1)
      ELSE
!           Write Error Message:  Invalid units for ozone value
         CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
      END IF
   END IF

   IF (O3VALUNITS == 'PPB') THEN
      O3BACK(IO3SECT) = O3BACK(IO3SECT) * O3_PPB
   ELSE IF (O3VALUNITS == 'PPM') then
      O3BACK(IO3SECT) = O3BACK(IO3SECT) * O3_PPM
   END IF

!     Check range of value
   IF (O3BACK(IO3SECT) <= 0.0D0 .or.&
   &O3BACK(IO3SECT) > 500.0D0)THEN
      CALL ERRHDL(PATH,MODNAM,'W','320',' O3BACK ')
   END IF

999 RETURN
END SUBROUTINE O3VAL

SUBROUTINE NOXVAL
!***********************************************************************
!                 NOXVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Non-temporally-varying NOx Value Option,
!                 CO NOXVALUE
!
!        PROGRAMMER: CERC
!
!        DATE:    November 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

   INTEGER :: I

   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'NOXVAL'

! --- Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_NOXSector) THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 4) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
! ---    Check for SECT ID in field 3 in case NOXSECTR keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without NOXSECTR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified NOXVALUES option and
!        assign the option to NOXFLAG variable
      INOXSECT = 1
      I = 3
      L_NOXVALUE(INOXSECT) = .TRUE.
!
   ELSE
! ---    Process inputs based on NOXSECTR option
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC == 4) THEN
         IF (FIELD(3)(1:4) /= 'SECT') THEN
!              Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTOR ID')
            GO TO 999
         ELSE
!              Error Message: No Numerical Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
      ELSE IF (IFC > 5) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
! ---    Determine user-specified sector
      IF (FIELD(3) == 'SECT1') THEN
         INOXSECT = 1
      ELSE IF (FIELD(3) == 'SECT2') THEN
         INOXSECT = 2
      ELSE IF (FIELD(3) == 'SECT3') THEN
         INOXSECT = 3
      ELSE IF (FIELD(3) == 'SECT4') THEN
         INOXSECT = 4
      ELSE IF (FIELD(3) == 'SECT5') THEN
         INOXSECT = 5
      ELSE IF (FIELD(3) == 'SECT6') THEN
         INOXSECT = 6
      ELSE
!           Error Message: Invalid sector definition
         CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTOR ID')
         GO TO 999
      END IF

! ---    Set field index for the user-specified NOX Value
      I = 4
      L_NOXVALUE(INOXSECT) = .TRUE.
   END IF

!     Get NOX Value, NOXBACK, for applicable sector
   CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

!     Assign value to NOXBACK variable for this sector
   NOXBACK(INOXSECT) = DNUM

!     Check for units of NOx value
   IF (IFC == I+1) THEN
      IF (FIELD(I+1)=='PPM' .or. FIELD(I+1)=='PPB' .or.&
      &FIELD(I+1)=='UG/M3') THEN
         NOXVALUNITS = FIELD(I+1)
      ELSE
!           Write Error Message:  Invalid units for NOX value
         CALL ERRHDL(PATH,MODNAM,'E','203',' NOXUNITS')
      END IF
   END IF

   !Convert to UG/M3 using NO2 factors (NOx expressed as 'NOx as NO2')
   IF (NOXVALUNITS == 'PPB') THEN
      NOXBACK(INOXSECT) = NOXBACK(INOXSECT) / NO2_PPB
   ELSE IF (NOXVALUNITS == 'PPM') then
      NOXBACK(INOXSECT) = NOXBACK(INOXSECT) / NO2_PPM
   END IF

!     Check range of value
   IF (NOXBACK(INOXSECT) <= 0.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'W','320',' NOXBACK ')
   END IF

999 RETURN
END SUBROUTINE NOXVAL

SUBROUTINE O3FILE
!***********************************************************************
!                 O3FILE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Ozone Data File Option for OZONEFIL keyword
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    May 3, 2002
!
!        MODIFIED: Include checks for potential problem with Fortran
!                  format specifier.  Should include from 1 to 4
!                  integers for date variables, and one real for
!                  ozone data variable.  Warning message is issued
!                  if too many or too few integers/reals are specified.
!                  An error message may also be issued when reading
!                  the ozone file depending on the compiler options.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 04/13/2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        ERROR HANDLING:   Checks for No Parametes (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, NumInt, NumReal
   LOGICAL :: FOPEN

!     Variable Initializations
   MODNAM = 'O3FILE'
   NumInt  = 0
   NumReal = 0
   FOPEN   = .FALSE.

! --- Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_O3Sector) THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 3) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 5) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
! ---    Check for SECT ID in field 3 in case O3FILE keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without O3SECTOR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for start of the user-specified options
      IO3SECT = 1
      I = 3
   ELSE
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 4) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 6) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
      IF (FIELD(3) == 'SECT1') THEN
         IO3SECT = 1
      ELSE IF (FIELD(3) == 'SECT2' .and. NUMO3Sects >= 2) THEN
         IO3SECT = 2
      ELSE IF (FIELD(3) == 'SECT3' .and. NUMO3Sects >= 3) THEN
         IO3SECT = 3
      ELSE IF (FIELD(3) == 'SECT4' .and. NUMO3Sects >= 4) THEN
         IO3SECT = 4
      ELSE IF (FIELD(3) == 'SECT5' .and. NUMO3Sects >= 5) THEN
         IO3SECT = 5
      ELSE IF (FIELD(3) == 'SECT6' .and. NUMO3Sects == 6) THEN
         IO3SECT = 6
      ELSE
!           Error Message: Invalid sector field
         CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
         GO TO 999
      END IF
! ---    Assign set field index for start of the user-specified options,
!        accounting for sector IDs
      I = 4
   END IF

!     Set logical flags for hourly ozone file(s)
   L_O3Hourly        = .TRUE.
   L_O3File(IO3SECT) = .TRUE.

!     Retrieve Ozone Data Filename as Character Substring to Maintain Case
   IF ((LOCE(I)-LOCB(I)) <= (ILEN_FLD - 1) ) THEN
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      OZONFL(IO3SECT) = RUNST1(LOCB(I):LOCE(I))
   ELSE
!        WRITE Error Message:  OZONFL Field is Too Long
!        Write error message and return
      WRITE(DUMMY,'(I8)') ILEN_FLD
      CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      GO TO 999
   END IF

!     Assign file unit for this O3 file and Open The Ozone Input File
!     Open with ACTION='READ' to prevent overwrite and multiple access
   IO3UNT(IO3SECT) = 1000 + IO3SECT

!     Open hourly Ozone File If Not Already Open
   INQUIRE (FILE=OZONFL(IO3SECT),OPENED=FOPEN)

   IF (.NOT. FOPEN) THEN
!        Open Hourly Ozone Data File If Not Already Open
!        Open with ACTION='READ' to prevent overwrite and allow multiple access
      INQUIRE (UNIT=IO3UNT(IO3SECT),OPENED=FOPEN)
      IF (.NOT. FOPEN) THEN
         OPEN(UNIT=IO3UNT(IO3SECT),FILE=OZONFL(IO3SECT),STATUS='OLD',&
         &ERR=998,ACTION='READ',FORM='FORMATTED')

      ELSE
!           Hourly Ozone File is Already Opened With Different Filename
         CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
         GO TO 999
      END IF
   ELSE
!        Hourly Ozone File is Already Opened With Different Filename
      CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
      GO TO 999
   END IF

!     Check for optional units of ozone value
   IF (I == 3 .and. IFC >= 4) THEN
      IF (FIELD(4)=='PPM' .or. FIELD(4)=='PPB' .or.&
      &FIELD(4)=='UG/M3') THEN
         O3FILUNITS = FIELD(4)
      ELSE
!           Write Error Message:  Invalid units for ozone value
         CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
      END IF
   ELSE IF (I == 4 .and. IFC >= 5) THEN
      IF (FIELD(5)=='PPM' .or. FIELD(5)=='PPB' .or.&
      &FIELD(5)=='UG/M3') THEN
         O3FILUNITS = FIELD(5)
      ELSE
!           Write Error Message:  Invalid units for ozone value
         CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
      END IF
   ELSE
      O3FILUNITS = 'UG/M3'
   END IF

   IF (IFC == I+2) THEN
!        Check for Format String > ILEN_FLD PARAMETER
      IF ((LOCE(I+2)-LOCB(I+2)) <= (ILEN_FLD - 1)) THEN

! ---       First check for user input of "FREE" for the formaat,
!           using FIELD array which has been converted to upper case
         IF (FIELD(I+2) == 'FREE') THEN
            O3FORM(IO3SECT) = 'FREE'
         ELSE
!              Retrieve Met Format as Char. Substring
            O3FORM(IO3SECT) = RUNST1(LOCB(I+2):LOCE(I+2))
! ---          Check for correct format specifiers for Ozone file;
!              should be 4 integers for date variables and 1 real for
!              ozone concentration; allow for 1 to 4 integers since
!              format statement may include 4I2, and also allow for
!              either F, E, or D format for the data variable.
            DO I = 1, LEN_TRIM(O3FORM(IO3SECT))
               IF (O3FORM(IO3SECT)(I:I)=='I' .or.&
               &O3FORM(IO3SECT)(I:I)=='i') THEN
                  NumInt  = NumInt  + 1
               ELSE IF (O3FORM(IO3SECT)(I:I)=='F' .or.&
               &O3FORM(IO3SECT)(I:I)=='f') THEN
                  NumReal = NumReal + 1
               ELSE IF (O3FORM(IO3SECT)(I:I)=='E' .or.&
               &O3FORM(IO3SECT)(I:I)=='e') THEN
                  NumReal = NumReal + 1
               ELSE IF (O3FORM(IO3SECT)(I:I)=='D' .or.&
               &O3FORM(IO3SECT)(I:I)=='d') THEN
                  NumReal = NumReal + 1
               END IF
            END DO
            IF (NumInt<1 .or. NumInt>4) THEN
!                 WRITE Warning Message:  Potential problem with O3FORM
               WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
               CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
            END IF
            IF (NumReal/=1) THEN
!                 WRITE Warning Message:  Potential problem with O3FORM
               WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
               CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
            END IF
         END IF
      ELSE
!           WRITE Error Message:  O3FORM Field is Too Long
         WRITE(DUMMY,'(''LEN='',I6)') LOCE(5)-LOCB(5)
         CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
      END IF
   ELSE
! ---    Use 'free' format as the default
      O3FORM(IO3SECT) = 'FREE'
   END IF

   GO TO 999

!     Process Error Messages; error opening file, include file type and sector
998 CONTINUE
   WRITE(DUMMY,'("O3FILE SECT",I1)') IO3SECT
   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

999 RETURN
END SUBROUTINE O3FILE

SUBROUTINE NOXFILE
!***********************************************************************
!                 NOXFILE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process NOx Data File Option for NOX_FILE keyword
!
!        PROGRAMMER: CERC
!
!        DATE:     November 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        ERROR HANDLING:   Checks for No Parametes (uses default name);
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   COCARD
!***********************************************************************
!
!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, NumInt, NumReal
   LOGICAL :: FOPEN

!     Variable Initializations
   MODNAM = 'NOXFILE'
   NumInt  = 0
   NumReal = 0
   FOPEN   = .FALSE.

! --- Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_NOxSector) THEN
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 3) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 5) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
! ---    Check for SECT ID in field 3 in case NOXFILE keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without NOXSECTR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for start of the user-specified options
      INOXSECT = 1
      I = 3
   ELSE
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC < 4) THEN
!           Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC > 6) THEN
!           Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF
      IF (FIELD(3) == 'SECT1') THEN
         INOXSECT = 1
      ELSE IF (FIELD(3) == 'SECT2' .and. NUMNOxSects >= 2) THEN
         INOXSECT = 2
      ELSE IF (FIELD(3) == 'SECT3' .and. NUMNOxSects >= 3) THEN
         INOXSECT = 3
      ELSE IF (FIELD(3) == 'SECT4' .and. NUMNOxSects >= 4) THEN
         INOXSECT = 4
      ELSE IF (FIELD(3) == 'SECT5' .and. NUMNOxSects >= 5) THEN
         INOXSECT = 5
      ELSE IF (FIELD(3) == 'SECT6' .and. NUMNOxSects == 6) THEN
         INOXSECT = 6
      ELSE
!           Error Message: Invalid sector field
         CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTR ID')
         GO TO 999
      END IF
! ---    Assign set field index for start of the user-specified options,
!        accounting for sector IDs
      I = 4
   END IF

!     Set logical flags for hourly NOx file(s)
   L_NOxHourly        = .TRUE.
   L_NOxFile(INOXSECT) = .TRUE.

!     Retrieve NOx Data Filename as Character Substring to Maintain Case
   IF ((LOCE(I)-LOCB(I)) <= (ILEN_FLD - 1) ) THEN
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      NOXFL(INOXSECT) = RUNST1(LOCB(I):LOCE(I))
   ELSE
!        WRITE Error Message:  NOXFL Field is Too Long
!        Write error message and return
      WRITE(DUMMY,'(I8)') ILEN_FLD
      CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      GO TO 999
   END IF

!     Assign file unit for this NOx file and Open The NOx Input File
!     Open with ACTION='READ' to prevent overwrite and multiple access
   INOXUNT(INOXSECT) = 3000 + INOXSECT

!     Open hourly NOx File If Not Already Open
   INQUIRE (FILE=NOXFL(INOXSECT),OPENED=FOPEN)

   IF (.NOT. FOPEN) THEN
!        Open Hourly NOx Data File If Not Already Open
!        Open with ACTION='READ' to prevent overwrite and allow multiple access
      INQUIRE (UNIT=INOXUNT(INOXSECT),OPENED=FOPEN)
      IF (.NOT. FOPEN) THEN
         OPEN(UNIT=INOXUNT(INOXSECT),FILE=NOXFL(INOXSECT),&
         &STATUS='OLD',ERR=998,ACTION='READ',FORM='FORMATTED')

      ELSE
!           Hourly NOx File is Already Opened With Different Filename
         CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
         GO TO 999
      END IF
   ELSE
!        Hourly NOx File is Already Opened With Different Filename
      CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
      GO TO 999
   END IF

!     Check for optional units of NOx value
   IF (I == 3 .and. IFC >= 4) THEN
      IF (FIELD(4)=='PPM' .or. FIELD(4)=='PPB' .or.&
      &FIELD(4)=='UG/M3') THEN
         NOXFILUNITS = FIELD(4)
      ELSE
!           Write Error Message:  Invalid units for NOx value
         CALL ERRHDL(PATH,MODNAM,'E','203','NOXUNITS')
      END IF
   ELSE IF (I == 4 .and. IFC >= 5) THEN
      IF (FIELD(5)=='PPM' .or. FIELD(5)=='PPB' .or.&
      &FIELD(5)=='UG/M3') THEN
         NOXFILUNITS = FIELD(5)
      ELSE
!           Write Error Message:  Invalid units for NOx value
         CALL ERRHDL(PATH,MODNAM,'E','203','NOXUNITS')
      END IF
   ELSE
      NOXFILUNITS = 'UG/M3'
   END IF

   IF (IFC == I+2) THEN
!        Check for Format String > ILEN_FLD PARAMETER
      IF ((LOCE(I+2)-LOCB(I+2)) <= (ILEN_FLD - 1)) THEN

! ---       First check for user input of "FREE" for the format,
!           using FIELD array which has been converted to upper case
         IF (FIELD(I+2) == 'FREE') THEN
            NOXFORM(INOXSECT) = 'FREE'
         ELSE
!              Retrieve Format as Char. Substring
            NOXFORM(INOXSECT) = RUNST1(LOCB(I+2):LOCE(I+2))
! ---          Check for correct format specifiers for NOx file;
!              should be 4 integers for date variables and 1 real for
!              NOx concentration; allow for 1 to 4 integers since
!              format statement may include 4I2, and also allow for
!              either F, E, or D format for the data variable.
            DO I = 1, LEN_TRIM(NOXFORM(INOXSECT))
               IF (NOXFORM(INOXSECT)(I:I)=='I' .or.&
               &NOXFORM(INOXSECT)(I:I)=='i') THEN
                  NumInt  = NumInt  + 1
               ELSE IF (NOXFORM(INOXSECT)(I:I)=='F' .or.&
               &NOXFORM(INOXSECT)(I:I)=='f') THEN
                  NumReal = NumReal + 1
               ELSE IF (NOXFORM(INOXSECT)(I:I)=='E' .or.&
               &NOXFORM(INOXSECT)(I:I)=='e') THEN
                  NumReal = NumReal + 1
               ELSE IF (NOXFORM(INOXSECT)(I:I)=='D' .or.&
               &NOXFORM(INOXSECT)(I:I)=='d') THEN
                  NumReal = NumReal + 1
               END IF
            END DO
            IF (NumInt<1 .or. NumInt>4) THEN
!                 WRITE Warning Message:  Potential problem with NOXFORM
               WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
               CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
            END IF
            IF (NumReal/=1) THEN
!                 WRITE Warning Message:  Potential problem with NOXFORM
               WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
               CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
            END IF
         END IF
      ELSE
!           WRITE Error Message:  NOXFORM Field is Too Long
         WRITE(DUMMY,'(''LEN='',I6)') LOCE(5)-LOCB(5)
         CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
      END IF
   ELSE
! ---    Use 'free' format as the default
      NOXFORM(INOXSECT) = 'FREE'
   END IF

   GO TO 999

!     Process Error Messages; error opening file, include file type and sector
998 CONTINUE
   WRITE(DUMMY,'("NOXFILE SCT",I1)') INOXSECT
   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

999 RETURN
END SUBROUTINE NOXFILE

SUBROUTINE NO2EQ
!***********************************************************************
!                 NO2EQ Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes NO2 Equilibrium Value for PVMRM based on
!                 the NO2EQUIL keyword
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:    May 3, 2004
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'NO2EQ'

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

!     Start To Get Ozone Value
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

!     Assign value to NO2Equil variable
   NO2Equil = DNUM

!     Check range of value
   IF (NO2Equil < 0.10D0 .or. NO2Equil > 1.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'E','380','NO2Equil')
   END IF

999 RETURN
END SUBROUTINE NO2EQ


SUBROUTINE NO2STK
!***********************************************************************
!                 NO2STK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes NO2 Default In-stack Ratio Value for PVMRM
!                 based on the NO2STACK keyword
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:    September 7, 2005
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   INTEGER :: I
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'NO2STK'

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

!     Start To Get Ozone Value
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   END IF

!     Assign value to NO2Stack variable
   NO2Stack = DNUM

!     Check range of value
   IF (NO2Stack < 0.0D0 .or. NO2Stack > 1.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'E','380','NO2Stack')
      GO TO 999
   END IF

   DO I = 1, NSRC
      ANO2_RATIO(I) = NO2Stack
   END DO

999 RETURN
END SUBROUTINE NO2STK

SUBROUTINE ARM2_Ratios
!***********************************************************************
!                 ARM2_Ratios Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes minimum and maximum NO2/NOx ratios for ARM2 option
!                 under the keyword.
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:    Nov. 25, 2013
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'ARM2_Ratios'

!     Check The Number Of The Fields
   IF (ARM2 .and. IFC < 4) THEN
!        Error Message: Too Few Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
   END IF


! --- Get the minimum ARM2 ratio (ARM2_Min)
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   ELSE
!        Assign value to ARM2_Min
      ARM2_Min = DNUM
   END IF

!     Check range of value for ARM2_Min
!     D157 WSP 3/28/23 Changed ARMRATIO restictions to match user's guide
!     DFAULT ARMRATIO is 0.5 <= ARMRATIO <= 0.9
!     NONDFAULT ARMRATIO is 0.0 < ARMRATIO <= 1.0
   IF (ARM2_Min < 0.50D0 .and. ARM2_Min > 0.0D0) THEN
      IF( DFAULT )THEN
         CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Min')
      ELSE
         CALL ERRHDL(PATH,MODNAM,'W','737',' ARM2Min')
      END IF
   ELSE IF (ARM2_Min > 0.90D0 .and. ARM2_Min <= 1.0D0) THEN
      IF( DFAULT )THEN
         CALL ERRHDL(PATH,MODNAM,'E','380', ' ARM2Min')
      ELSE
         CALL ERRHDL(PATH,MODNAM,'W','737',' ARM2Min')
      END IF
   ELSE IF (ARM2_Min <= 0.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Min')
   ELSE IF (ARM2_Min > 1.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Min')
!     D157 CRT 5/31/2023 remove warning message if ARMRATIO is within default
!      ELSE IF (ARM2_Min .GT. 0.50D0) THEN
!         CALL ERRHDL(PATH,MODNAM,'W','736',' ARM2Min')
   END IF

!C     Check range of value for ARM2_Min
!      IF (ARM2_Min .LE. 0.0D0) THEN
!         WRITE(DUMMY,'(''ARM2Min <= 0'')')
!         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!      ELSE IF (ARM2_Min .GT. 1.0D0) THEN
!         WRITE(DUMMY,'(''ARM2Min > 1'')')
!         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!      ELSE IF (ARM2_Min .LT. 0.50D0) THEN
!         IF( DFAULT )THEN
!            WRITE(DUMMY,'(''ARM2Min <0.5'')')
!            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!         ELSE
!            WRITE(DUMMY,'(''ARM2Min <0.5'')')
!            CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
!         END IF
!      ELSE IF (ARM2_Min .GT. 0.50D0) THEN
!         WRITE(DUMMY,'(''ARM2Min >0.5'')')
!         CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
!      END IF

! --- Get the maximum ARM2 ratio (ARM2_Max)
   CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      GO TO 999
   ELSE
!        Assign value to ARM2_Max
      ARM2_Max = DNUM
   END IF

!     Check range of value for ARM2_Max
!     D157 WSP 3/28/23 Changed ARMRATIO restictions to match user's guide
!     DFAULT ARMRATIO is 0.5 <= ARMRATIO <= 0.9
!     NONDFAULT ARMRATIO is 0.0 < ARMRATIO <= 1.0
   IF (ARM2_Max < 0.50D0 .and. ARM2_Max > 0.0D0) THEN
      IF( DFAULT )THEN
         CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Max')
      ELSE
         CALL ERRHDL(PATH,MODNAM,'W','737',' ARM2Max')
      END IF
   ELSE IF (ARM2_Max > 0.90D0 .and. ARM2_Max <= 1.0D0) THEN
      IF( DFAULT )THEN
         CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Max')
      ELSE
         CALL ERRHDL(PATH,MODNAM,'W','737',' ARM2Max')
      END IF
   ELSE IF (ARM2_Max <= 0.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Max')
   ELSE IF (ARM2_Max > 1.0D0) THEN
      CALL ERRHDL(PATH,MODNAM,'E','380',' ARM2Max')
!     D157 CRT 5/31/2023 remove warning message if ARMRATIO is within default
!      ELSE IF (ARM2_Max .LT. 0.90D0) THEN
!         CALL ERRHDL(PATH,MODNAM,'W','736',' ARM2Max')
   END IF
!     D157 Check that ARMmax > ARMmin
   IF (ARM2_Max < ARM2_Min) THEN
      WRITE(DUMMY,'(''ARM2 Max<Min'')')
      CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
   END IF
!C     Check range of value for ARM2_Max
!      IF (ARM2_Max .LE. 0.0D0) THEN
!         WRITE(DUMMY,'(''ARM2Max <= 0'')')
!         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!      ELSE IF (ARM2_Max .GT. 1.0D0) THEN
!         WRITE(DUMMY,'(''ARM2Max > 1'')')
!         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!      ELSE IF (ARM2_Max .LT. ARM2_Min) THEN
!         WRITE(DUMMY,'(''ARM2 Max<Min'')')
!         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!      ELSE IF (ARM2_Max .LT. 0.90D0) THEN
!         IF( DFAULT )THEN
!            WRITE(DUMMY,'(''ARM2Max <0.9'')')
!            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!         ELSE
!            WRITE(DUMMY,'(''ARM2Max <0.9'')')
!            CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
!         END IF
!      END IF

999 RETURN
END SUBROUTINE ARM2_Ratios

SUBROUTINE O3VALS
!***********************************************************************
!                 O3VALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes User-specified Ozone concentrations, using
!                 the O3VALUES keyword, based on same options for
!                 temporal variability as the EMISFACT keyword for
!                 source emissions
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED: Corrected the test for number of parameters to
!                  be .GE. 4 to allow for the ANNUAL option.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 12/19/2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I
! Unused:      INTEGER :: IH, IL, ISDX, NNN
! Unused:      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
! Unused:      CHARACTER (LEN=ILEN_FLD) :: SOID
! Unused:      LOGICAL FOUND, INGRP, RMARK

!     Variable Initializations
   MODNAM = 'O3VALS'

!     Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_O3Sector) THEN
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
! ---    Check for SECT ID in field 3 in case O3SECTOR keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without O3SECTOR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified O3VALUES option and
!        assign the option to O3FLAG variable
      IO3SECT = 1
      I = 3
      L_O3VALUES(IO3SECT) = .TRUE.
      IF (IO3MAX(IO3SECT) >= 1 .and.&
      &O3FLAG(IO3SECT) /= FIELD(I)) THEN
         CALL ERRHDL(PATH,MODNAM,'E','167',FIELD(I))
      ELSE
         O3FLAG(IO3SECT) = FIELD(I)
      END IF
   ELSE
! ---    Process inputs based on O3SECTOR option
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC == 4) THEN
         IF (FIELD(3)(1:4) /= 'SECT') THEN
!              Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
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
         IO3SECT = 1
      ELSE IF (FIELD(3) == 'SECT2' .and. NUMO3Sects >= 2) THEN
         IO3SECT = 2
      ELSE IF (FIELD(3) == 'SECT3' .and. NUMO3Sects >= 3) THEN
         IO3SECT = 3
      ELSE IF (FIELD(3) == 'SECT4' .and. NUMO3Sects >= 4) THEN
         IO3SECT = 4
      ELSE IF (FIELD(3) == 'SECT5' .and. NUMO3Sects >= 5) THEN
         IO3SECT = 5
      ELSE IF (FIELD(3) == 'SECT6' .and. NUMO3Sects == 6) THEN
         IO3SECT = 6
      ELSE
!           Error Message: Invalid sector field
         CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
         GO TO 999
      END IF
! ---    Set field index for the user-specified O3VALUES option and
!        assign the option to O3FLAG variable
      I = 4
      L_O3VALUES(IO3SECT) = .TRUE.
      IF (IO3MAX(IO3SECT) >= 1 .and.&
      &O3FLAG(IO3SECT) /= FIELD(I)) THEN
         IF (LEN_TRIM(FIELD(I)) > 6) THEN
            WRITE(DUMMY,'(''SEC'',I1,1X,A)') IO3SECT,&
            &FIELD(I)(1:LEN_TRIM(FIELD(I)))
            CALL ERRHDL(PATH,MODNAM,'E','167',DUMMY)
         ELSE
            WRITE(DUMMY,'(''SECT'',I1,1X,A)') IO3SECT,&
            &FIELD(I)(1:LEN_TRIM(FIELD(I)))
            CALL ERRHDL(PATH,MODNAM,'E','167',DUMMY)
         END IF
      ELSE
         O3FLAG(IO3SECT) = FIELD(I)
      END IF
   END IF

! --- Assign number of ozone values based on O3FLAG option
   IF (O3FLAG(IO3SECT) == 'ANNUAL') THEN
      IO3MAX(IO3SECT) = 1
   ELSE IF (O3FLAG(IO3SECT) == 'SEASON') THEN
      IO3MAX(IO3SECT) = 4
   ELSE IF (O3FLAG(IO3SECT) == 'MONTH') THEN
      IO3MAX(IO3SECT) = 12
   ELSE IF (O3FLAG(IO3SECT) == 'HROFDY') THEN
      IO3MAX(IO3SECT) = 24
   ELSE IF (O3FLAG(IO3SECT) == 'WSPEED') THEN
      IO3MAX(IO3SECT) = 6
   ELSE IF (O3FLAG(IO3SECT) == 'SEASHR') THEN
      IO3MAX(IO3SECT) = 96
   ELSE IF (O3FLAG(IO3SECT) == 'HRDOW') THEN
      IO3MAX(IO3SECT) = 72
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (O3FLAG(IO3SECT) == 'HRDOW7') THEN
      IO3MAX(IO3SECT) = 168
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (O3FLAG(IO3SECT) == 'SHRDOW') THEN
      IO3MAX(IO3SECT) = 288
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (O3FLAG(IO3SECT) == 'SHRDOW7') THEN
      IO3MAX(IO3SECT) = 672
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (O3FLAG(IO3SECT) == 'MHRDOW') THEN
      IO3MAX(IO3SECT) = 864
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (O3FLAG(IO3SECT) == 'MHRDOW7') THEN
      IO3MAX(IO3SECT) = 2016
      L_DayOfWeekOpts = .TRUE.
   ELSE
!        WRITE Error Message    ! Invalid O3FLAG Field Entered
      CALL ERRHDL(PATH,MODNAM,'E','203','O3FLAG')
      GO TO 999
   END IF

! --- Call subroutine O3FILL to fill the temporally-varying O3 data
   CALL O3FILL

999 RETURN
END SUBROUTINE O3VALS

SUBROUTINE NOXVALS
!***********************************************************************
!                 NOXVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes User-specified NOx concentrations, using
!                 the NOX_VALS keyword, based on same options for
!                 temporal variability as the EMISFACT keyword for
!                 source emissions
!
!        PROGRAMMER:  CERC
!
!        DATE:       November 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I

!     Variable Initializations
   MODNAM = 'NOXVALS'

!     Check The Number Of The Fields, accounting for sector-varying values
   IF (.NOT.L_NOXSector) THEN
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
! ---    Check for SECT ID in field 3 in case NOXSECTR keyword was omitted
      IF (FIELD(3)(1:4) == 'SECT') THEN
!           Error Message: SECT ID without NOXSECTOR keyword
         CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
         GO TO 999
      END IF
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified NOX_VALS option and
!        assign the option to NOXFLAG variable
      INOXSECT = 1
      I = 3
      L_NOX_VALS(INOXSECT) = .TRUE.
      IF (INOXMAX(INOXSECT) >= 1 .and.&
      &NOXFLAG(INOXSECT) /= FIELD(I)) THEN
         CALL ERRHDL(PATH,MODNAM,'E','606',FIELD(I))
      ELSE
         NOXFLAG(INOXSECT) = FIELD(I)
      END IF
   ELSE
! ---    Process inputs based on NOXSECTOR option
      IF (IFC <= 2) THEN
!           Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC == 4) THEN
         IF (FIELD(3)(1:4) /= 'SECT') THEN
!              Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTR ID')
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
         INOXSECT = 1
      ELSE IF (FIELD(3) == 'SECT2' .and. NUMNOxSects >= 2) THEN
         INOXSECT = 2
      ELSE IF (FIELD(3) == 'SECT3' .and. NUMNOxSects >= 3) THEN
         INOXSECT = 3
      ELSE IF (FIELD(3) == 'SECT4' .and. NUMNOxSects >= 4) THEN
         INOXSECT = 4
      ELSE IF (FIELD(3) == 'SECT5' .and. NUMNOxSects >= 5) THEN
         INOXSECT = 5
      ELSE IF (FIELD(3) == 'SECT6' .and. NUMNOxSects == 6) THEN
         INOXSECT = 6
      ELSE
!           Error Message: Invalid sector field
         CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTR ID')
         GO TO 999
      END IF
! ---    Set field index for the user-specified NOX_VALS option and
!        assign the option to NOXFLAG variable
      I = 4
      L_NOX_VALS(INOXSECT) = .TRUE.
      IF (INOXMAX(INOXSECT) >= 1 .and.&
      &NOXFLAG(INOXSECT) /= FIELD(I)) THEN
         IF (LEN_TRIM(FIELD(I)) > 6) THEN
            WRITE(DUMMY,'(''SEC'',I1,1X,A)') INOXSECT,&
            &FIELD(I)(1:LEN_TRIM(FIELD(I)))
            CALL ERRHDL(PATH,MODNAM,'E','606',DUMMY)
         ELSE
            WRITE(DUMMY,'(''SECT'',I1,1X,A)') INOXSECT,&
            &FIELD(I)(1:LEN_TRIM(FIELD(I)))
            CALL ERRHDL(PATH,MODNAM,'E','606',DUMMY)
         END IF
      ELSE
         NOXFLAG(INOXSECT) = FIELD(I)
      END IF
   END IF

! --- Assign number of NOx values based on NOXFLAG option
   IF (NOXFLAG(INOXSECT) == 'ANNUAL') THEN
      INOXMAX(INOXSECT) = 1
   ELSE IF (NOXFLAG(INOXSECT) == 'SEASON') THEN
      INOXMAX(INOXSECT) = 4
   ELSE IF (NOXFLAG(INOXSECT) == 'MONTH') THEN
      INOXMAX(INOXSECT) = 12
   ELSE IF (NOXFLAG(INOXSECT) == 'HROFDY') THEN
      INOXMAX(INOXSECT) = 24
   ELSE IF (NOXFLAG(INOXSECT) == 'WSPEED') THEN
      INOXMAX(INOXSECT) = 6
   ELSE IF (NOXFLAG(INOXSECT) == 'SEASHR') THEN
      INOXMAX(INOXSECT) = 96
   ELSE IF (NOXFLAG(INOXSECT) == 'HRDOW') THEN
      INOXMAX(INOXSECT) = 72
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (NOXFLAG(INOXSECT) == 'HRDOW7') THEN
      INOXMAX(INOXSECT) = 168
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (NOXFLAG(INOXSECT) == 'SHRDOW') THEN
      INOXMAX(INOXSECT) = 288
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (NOXFLAG(INOXSECT) == 'SHRDOW7') THEN
      INOXMAX(INOXSECT) = 672
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (NOXFLAG(INOXSECT) == 'MHRDOW') THEN
      INOXMAX(INOXSECT) = 864
      L_DayOfWeekOpts = .TRUE.
   ELSE IF (NOXFLAG(INOXSECT) == 'MHRDOW7') THEN
      INOXMAX(INOXSECT) = 2016
      L_DayOfWeekOpts = .TRUE.
   ELSE
!        WRITE Error Message    ! Invalid NOXFLAG Field Entered
      CALL ERRHDL(PATH,MODNAM,'E','203','NOXFLG')
      GO TO 999
   END IF

! --- Call subroutine NOXFILL to fill the temporally-varying NOX data
   CALL NOXFILL

999 RETURN
END SUBROUTINE NOXVALS

SUBROUTINE O3FILL
!***********************************************************************
!                 O3FILL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Fill Variable Ozone Concentration Array
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Direction Specific Building Directions
!
!        CALLED FROM:   O3VALS
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K

! --- Variable Initializations
   MODNAM = 'O3FILL'

! --- Initialize counter for number of O3VALUES for this sector
   ISET = IO3SET(IO3SECT)

! --- Assign field number for start of data values based on whether
!     sector-varying values are used
   IF (L_O3Sector) THEN
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
         IF (ISET <= IO3MAX(IO3SECT)) THEN
            O3VARY(ISET,IO3SECT) = DNUM
            IF (DNUM < 0.0D0) THEN
!                 WRITE Error Message:  Negative Value for O3VALUES
               CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
            END IF
         ELSE
!              WRITE Error Message    ! Too Many O3VALUES Input
            IF (L_O3Sector) THEN
               WRITE(DUMMY,'(''O3VALs SECT'',I1)') IO3SECT
            ELSE
               WRITE(DUMMY,'(''O3VALUES'')')
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','231',DUMMY)
            GO TO 99
         END IF
      END DO
   END DO

99 CONTINUE

! --- Save counter on number of values input so far for this sector
   IO3SET(IO3SECT) = ISET

   RETURN
END SUBROUTINE O3FILL

SUBROUTINE NOXFILL
!***********************************************************************
!                 NOXFILL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Fill Variable NOx Concentration Array
!
!        PROGRAMMER:  CERC
!
!        DATE:       November 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Direction Specific Building Directions
!
!        CALLED FROM:   NOXVALS
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K

! --- Variable Initializations
   MODNAM = 'NOXFILL'

! --- Initialize counter for number of NOX_VALS for this sector
   ISET = INOXSET(INOXSECT)

! --- Assign field number for start of data values based on whether
!     sector-varying values are used
   IF (L_NOxSector) THEN
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
         IF (ISET <= INOXMAX(INOXSECT)) THEN
            NOXVARY(ISET,INOXSECT) = DNUM
            IF (DNUM < 0.0D0) THEN
!                 WRITE Error Message:  Negative Value for NOX_VALS
               CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
            END IF
         ELSE
!              WRITE Error Message    ! Too Many NOX_VALS Input
            IF (L_NOxSector) THEN
               WRITE(DUMMY,'(''NOXVAL SECT'',I1)') INOXSECT
            ELSE
               WRITE(DUMMY,'(''NOX_VALS'')')
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','231',DUMMY)
            GO TO 99
         END IF
      END DO
   END DO

99 CONTINUE

! --- Save counter on number of values input so far for this sector
   INOXSET(INOXSECT) = ISET

   RETURN
END SUBROUTINE NOXFILL

SUBROUTINE OZON_UNIT
!***********************************************************************
!                 OZON_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user-specified units for O3VALUES keyword
!                 ozone concentrations, based on the OZONUNIT keyword
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'OZON_UNIT'

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
   IF (FIELD(3)=='PPM' .or. FIELD(3)=='PPB' .or.&
   &FIELD(3)=='UG/M3') THEN
      OzoneUnits = FIELD(3)
   ELSE
!        Write Error Message:  Invalid units for O3VALUES
      CALL ERRHDL(PATH,MODNAM,'E','203','OzoneUnits')
   END IF

999 RETURN
END SUBROUTINE OZON_UNIT

SUBROUTINE NOX_UNIT
!***********************************************************************
!                 NOX_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user-specified units for NOX_VALS keyword
!                 NOx concentrations, based on the NOX_VALS keyword
!
!        PROGRAMMER:  CERC
!
!        DATE:       November 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'NOX_UNIT'

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
   IF (FIELD(3)=='PPM' .or. FIELD(3)=='PPB' .or.&
   &FIELD(3)=='UG/M3') THEN
      NOxUnits = FIELD(3)
   ELSE
!        Write Error Message:  Invalid units for NOX_VALS
      CALL ERRHDL(PATH,MODNAM,'E','203','NOxUnits')
   END IF

999 RETURN
END SUBROUTINE NOX_UNIT

SUBROUTINE O3SECTOR
!***********************************************************************
!                 O3SECTOR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user-specified WD sectors for background
!                 ozone concentrations, based on the O3SECTOR keyword
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:       September 10, 2013
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I
   LOGICAL :: L_BadData

!     Variable Initializations
   MODNAM = 'O3SECTOR'
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

! --- Set L_O3Sector logical variable
   L_O3Sector = .TRUE.

   DO I = 3, IFC
!        Loop through fields for starting directions for each O3SECTOR
      CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         WRITE(DUMMY,'("O3SECT",I1)') I-2
         CALL ERRHDL(PATH,MODNAM,'E','208',DUMMY)
!           Assign logical variable for bad data, but cycle through full record
         L_BadData = .TRUE.
         CYCLE
      END IF
      O3SECT(I-2) = DNUM
      IF (O3SECT(I-2) < 0.0D0 .or. O3SECT(I-2) > 360.0D0) THEN
!           Sector value out-of-range
         IF (O3SECT(I-2) > 9999.0D0) THEN
            WRITE(DUMMY,'("O3SECT>9999.")')
         ELSE IF (O3SECT(I-2) < -999.0D0) THEN
            WRITE(DUMMY,'("O3SECT<-999.")')
         ELSE
            WRITE(DUMMY,'("O3SECT=",F5.0)') O3SECT(I-2)
         END IF
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      END IF
   END DO

! --- Check for presence of bad sector data
   IF (L_BadData) GO TO 999

! --- Assign variable for number of user-specified background O3 sectors
   NUMO3Sects = IFC-2

! --- Check O3SECTs for proper order and minimum sector widths
   DO I = 1, NUMO3Sects-1
      IF (O3SECT(I+1) < O3SECT(I) ) THEN
!           Sector value out-of-order
         WRITE(DUMMY,'("O3SECT",I1," < #",I1)') I+1, I
         CALL ERRHDL(PATH,MODNAM,'E','222',DUMMY)
      ELSE IF (O3SECT(I+1) < O3SECT(I)+30.0D0 ) THEN
!           Sector width < 30 degrees
         WRITE(DUMMY,'("O3SECT",I1," < 30")') I+1
         CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
      ELSE IF (O3SECT(I+1) < O3SECT(I)+60.0D0 ) THEN
!           Sector width < 60 degrees
         WRITE(DUMMY,'("O3SECT",I1," < 60")') I+1
         CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
      END IF
   END DO
! --- Now check for width of last sector
   IF ( (O3SECT(1)+360.0D0)-O3SECT(NUMO3Sects) < 30.0D0) THEN
!        Sector width < 30 degrees
      WRITE(DUMMY,'("O3SECT",I1," < 30")') NUMO3Sects
      CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
   ELSE IF ( (O3SECT(1)+360.0D0)-O3SECT(NUMO3Sects) < 60.0D0) THEN
!        Sector width < 60 degrees
      WRITE(DUMMY,'("O3SECT",I1," < 60")') NUMO3Sects
      CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
   END IF

999 RETURN
END SUBROUTINE O3SECTOR

SUBROUTINE NOXSECTOR
!***********************************************************************
!                 NOXSECTOR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user-specified WD sectors for background
!                 NOX concentrations, based on the NOXSECTR keyword
!
!        PROGRAMMER:  CERC
!
!        DATE:       November 2020
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Variable Emmission Rate Factors
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I
   LOGICAL :: L_BadData

!     Variable Initializations
   MODNAM = 'NOXSECTOR'
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

! --- Set L_NOXSector logical variable
   L_NOxSector = .TRUE.

   DO I = 3, IFC
!        Loop through fields for starting directions for each NOXSECTOR
      CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT == -1) THEN
         WRITE(DUMMY,'("NOXSECT",I1)') I-2
         CALL ERRHDL(PATH,MODNAM,'E','208',DUMMY)
!           Assign logical variable for bad data, but cycle through full record
         L_BadData = .TRUE.
         CYCLE
      END IF
      NOXSECT(I-2) = DNUM
      IF(NOXSECT(I-2) < 0.0D0 .or. NOXSECT(I-2) > 360.0D0)THEN
!           Sector value out-of-range
         IF (NOXSECT(I-2) > 999.0D0) THEN
            WRITE(DUMMY,'("NOXSECT>999.")')
         ELSE IF (NOXSECT(I-2) < -99.0D0) THEN
            WRITE(DUMMY,'("NOXSECT<-99.")')
         ELSE
            WRITE(DUMMY,'("NOXSECT=",F4.0)') NOXSECT(I-2)
         END IF
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      END IF
   END DO

! --- Check for presence of bad sector data
   IF (L_BadData) GO TO 999

! --- Assign variable for number of user-specified background NOX sectors
   NUMNOxSects = IFC-2

! --- Check NOxSECTs for proper order and minimum sector widths
   DO I = 1, NUMNOXSects-1
      IF (NOXSECT(I+1) < NOXSECT(I) ) THEN
!           Sector value out-of-order
         WRITE(DUMMY,'("NOXSCT",I1," < #",I1)') I+1, I
         CALL ERRHDL(PATH,MODNAM,'E','222',DUMMY)
      ELSE IF (NOXSECT(I+1) < NOXSECT(I)+30.0D0 ) THEN
!           Sector width < 30 degrees
         WRITE(DUMMY,'("NOXSCT",I1," < 30")') I+1
         CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
      ELSE IF (NOXSECT(I+1) < NOXSECT(I)+60.0D0 ) THEN
!           Sector width < 60 degrees
         WRITE(DUMMY,'("NOXSCT",I1," < 60")') I+1
         CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
      END IF
   END DO
! --- Now check for width of last sector
   IF ( (NOXSECT(1)+360.0D0)-NOXSECT(NUMNOxSects) < 30.0D0) THEN
!        Sector width < 30 degrees
      WRITE(DUMMY,'("NOXSCT",I1," < 30")') NUMNOxSects
      CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
   ELSE IF ((NOXSECT(1)+360.0D0)-NOXSECT(NUMNOxSects)<60.0D0) THEN
!        Sector width < 60 degrees
      WRITE(DUMMY,'("NOXSCT",I1," < 60")') NUMNOxSects
      CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
   END IF

999 RETURN
END SUBROUTINE NOXSECTOR

SUBROUTINE LOW_WND
!***********************************************************************
!                 LOW_WND Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes user inputs for LowWind option parameters
!                 under the LOW_WIND keyword
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:    December 10, 2012
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS:
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   LOGICAL  :: L_Error
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'LOW_WIND'
   L_Error = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC < 3) THEN
!        Error Message: Too Few Parameters
      CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      GO TO 999
!     Wood 3/18/2022 D127 Increased number of parameters by one to allow for FRANMIN
!CRT  4/11/2022 D131 Increased number of parameters by one to allow for PBAL FRAN option
   ELSE IF (IFC > 9) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

!     Get Minimum Sigma_V value, SVMIN
   CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
!     Check The Numerical Field
   IF (IMIT /= 1) THEN
      CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      L_Error = .TRUE.
   ELSE
      L_UserSVmin = .TRUE.
      SVMIN = DNUM
   END IF

!     Check for acceptable range for SVMIN
   IF (.NOT. L_Error) THEN
      IF (SVMIN < 0.01D0 .or. SVMIN > 1.001D0) THEN
         WRITE(DUMMY,'("SVMIN=",F5.2)') SVMIN
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         L_Error = .TRUE.
      ELSE
!           Issue warning message with new SVMIN
         WRITE(DUMMY,'(F6.4)') SVMIN
         CALL ERRHDL(PATH,MODNAM,'W','111',DUMMY)
      END IF
   END IF

   L_Error = .FALSE.

   IF (IFC >= 4) THEN
!        Get Minimum wind speed value, WSMIN
      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         L_Error = .TRUE.
      ELSE
         L_UserWSmin = .TRUE.
         WSMIN = DNUM
      END IF
!        Check for acceptable range for WSMIN
      IF (.NOT. L_Error) THEN
         IF (WSMIN < 0.01D0 .or. WSMIN > 1.001D0) THEN
            WRITE(DUMMY,'("WSMIN=",F5.2)') WSMIN
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         ELSE
!              Issue warning message with new WSMIN
            WRITE(DUMMY,'(F6.4)') WSMIN
            CALL ERRHDL(PATH,MODNAM,'W','112',DUMMY)
         END IF
      END IF
   END IF

   L_Error = .FALSE.

   IF (IFC >= 5) THEN
!        Get maximum meander factor, FRANMAX
      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         L_Error = .TRUE.
      ELSE
         FRANMAX = DNUM
         L_UserFRANmax = .TRUE.
      END IF
!        Check for acceptable range for FRANMAX
      IF (.NOT. L_Error) THEN
         IF (FRANMAX < 0.0D0 .or. FRANMAX > 1.0D0) THEN
            WRITE(DUMMY,'("FRANMAX=",F4.2)') FRANMAX
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         ELSE
!              Issue warning message with new FRANMAX
            WRITE(DUMMY,'(F6.4)') FRANMAX
            CALL ERRHDL(PATH,MODNAM,'W','113',DUMMY)
         END IF
      END IF
   END IF

! CRT 9/11/2020, D062 User Minimum Sigma W
   IF (IFC >= 6) THEN
!        Get minimum sigma w, SWMIN
      CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         L_Error = .TRUE.
      ELSE
         L_UserSWmin = .TRUE.
         SWMIN = DNUM
      END IF
!        Check for acceptable range for SWMIN
      IF (.NOT. L_Error) THEN
         IF (SWMIN < 0.0D0 .or. SWMIN > 3.0D0) THEN
            WRITE(DUMMY,'("SWMIN=",F4.2)') SWMIN
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         ELSE
!              Issue warning message with new SWMIN
            WRITE(DUMMY,'(F6.4)') SWMIN
            CALL ERRHDL(PATH,MODNAM,'W','127',DUMMY)
         END IF
      END IF
   END IF


! RCO 9/28/2020, D061 User BIGT
   IF (IFC >= 7) THEN
!        Get BIGT
      CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         L_Error = .TRUE.
      ELSE
         L_UserBigT = .TRUE.
         BIGT = DNUM
      END IF
!        Check for acceptable range for BIGT
      IF (.NOT. L_Error) THEN
         IF (BIGT < 0.5D0 .or. BIGT > 48.0D0) THEN
            WRITE(DUMMY,'("BIGT=",F4.2)') BIGT
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         ELSE
!              Issue warning message with new BIGT
            WRITE(DUMMY,'(F6.2)') BIGT
            CALL ERRHDL(PATH,MODNAM,'W','129',DUMMY)
         END IF
      END IF
   END IF

!     Wood 3/18/22 D127 Added FRANMIN
   IF (IFC >= 8) THEN
!        Get maximum meander factor, FRANMIN
      CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
!        Check The Numerical Field
      IF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         L_Error = .TRUE.
      ELSE
         L_UserFRANmin = .TRUE.
         FRANMIN = DNUM
      END IF
!        Check for acceptable range for FRANMIN and less than FRANMAX
      IF (.NOT. L_Error) THEN
!           FRANMIN cannot be < 0 or > 100
         IF (FRANMIN < 0.0D0 .or. FRANMIN > 1.0D0) THEN
            WRITE(DUMMY,'("FRANMIN=",F4.2)') FRANMIN
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
!           FRANMIN cannot be > FRANMAX
         ELSE IF (FRANMIN > FRANMAX) THEN
            WRITE(DUMMY,'(F4.2," > ",F4.2)') FRANMIN, FRANMAX
            CALL ERRHDL(PATH,MODNAM,'E','426',DUMMY)
!              Issue warning message with new FRANMIN
            WRITE(DUMMY,'(F6.4)') FRANMIN
            CALL ERRHDL(PATH,MODNAM,'W','117',DUMMY)
         END IF
      END IF
   END IF

!CRT  4/11/2022, D131 FRAN Alpha Formulation - Momentum Balance (PBal)
   IF (IFC >= 9) THEN
!        Get PBal - momentum balance FRAN option
      IF (FIELD(9) == 'PBAL' .or.&
      &FIELD(9) == 'PBALANCE') THEN
         L_PBal = .TRUE.
!           Issue warning message with new BIGT
         CALL ERRHDL(PATH,MODNAM,'W','128','')
      ELSE
         L_PBal = .FALSE.

! ---       Write Error Message:Illegal Parameter Field
         Dummy = FIELD(9)
         CALL ERRHDL(PATH,MODNAM,'E','203',Dummy)
         L_Error = .TRUE.
      END IF
   END IF


999 RETURN
END SUBROUTINE LOW_WND

SUBROUTINE AWMA_DOWNWASH
!***********************************************************************
!                PRIME_2 Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Informs AERMOD to use the AWMA downwash algorithms:
!
!        OPTIONS:
!           STREAMLINE, STREAMLINED - structure type
!           AWMAUEFF - height to calculate main plume concentration
!           AWMAUTURB - alternate calculations for wake_u, wake_turb
!
!        PROGRAMMER: Wood
!
!        DATE:    August 15, 2018
!
!        MODIFIED:  Added the AWMA option AWMAENTRAIN to change the
!                   entrainment constants beta0 and betap from 0.6 to
!                   0.35. Requires inclusion of ALPHA MODELOPT (July 2020)
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Logical indicating type of structure
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'AWMA_DOWNWASH'

! --- Initialize logical variables
!     By default, all buildings are assumed to be rectangular
!       (L_RECT_BLDG = .TRUE.); if streamlined buildings is specified
!       (with the parameter 'STREAMLINE' or 'STREAMLINED'),
!       L_RECT_BLDG will be set to .FALSE.

!     L_AWMADW indicates that the AWMADW keyword is being used in the
!       control file

   L_AWMADW       = .FALSE.
   L_RECT_BLDG    = .TRUE.        ! Default is rectangular structures
   L_STRMLN_BLDG  = .FALSE.
   L_AWMA_Ueff  = .FALSE.
   L_AWMA_UTurb = .FALSE.
   L_AWMA_Entrain = .FALSE.
   L_AWMA_UTurbHX = .FALSE.

!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
!CRT  CRT 2/2/2021: D059 Update max number of fields allowed for new options
!CRT  Number increased from 5 to 7 for two newest options (AWMAUTurbHX and AWMAEntrain).
!CRT  If both AWMAUTurb and AWMAUTurbHX are specified, warning message is output and
!CRT  AWMAUTurbHX is used (overrides AWMAUTurb).
!CRT      ELSE IF (IFC .GT. 5) THEN
   ELSE IF (IFC > 7) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   L_AWMADW = .TRUE.

   IF (IFC == 3) THEN
! ---    Only one parameter specified
      IF (FIELD(3) == 'STREAMLINE' .or.&
      &FIELD(3) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(3) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF
   END IF

   IF (IFC == 4) THEN
! ---    Two parameters specified
      IF (FIELD(3) == 'STREAMLINE' .or.&
      &FIELD(3) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(3) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF

      IF (FIELD(4) == 'STREAMLINE' .or.&
      &FIELD(4) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(4) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
      END IF

! ---    Check for duplicate parameters
      IF (FIELD(3) == FIELD(4)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF
   END IF

   IF (IFC == 5) THEN
!        Three parameters specified
      IF (FIELD(3) == 'STREAMLINE' .or.&
      &FIELD(3) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(3) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF

      IF (FIELD(4) == 'STREAMLINE' .or.&
      &FIELD(4) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(4) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
      END IF

      IF (FIELD(5) == 'STREAMLINE' .or.&
      &FIELD(5) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(5) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

! ---    Check for duplicate parameters
      IF (FIELD(3) == FIELD(4)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF
   ENDIF

!CRT  2/2/2021: D059 - AWMA downwash alpha options AWMAEntrain and AWMAUTurbHX
!RLP  Addition of options requires checking for 5 possible
!CRT  Number of fields increased from 5 to 7 for two newest options
!CRT  (AWMAUTurbHX and AWMAEntrain). If both AWMAUTurb and AWMAUTurbHX are
!CRT  specified, warning message is output and AWMAUTurbHX is used.
   IF (IFC == 6) THEN

!        Four parameters specified
      IF (FIELD(3) == 'STREAMLINE' .or.&
      &FIELD(3) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(3) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF

      IF (FIELD(4) == 'STREAMLINE' .or.&
      &FIELD(4) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(4) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
      END IF

      IF (FIELD(5) == 'STREAMLINE' .or.&
      &FIELD(5) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(5) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

      IF (FIELD(6) == 'STREAMLINE' .or.&
      &FIELD(6) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(6) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(6) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(6) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(6) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

! ---    Check for duplicate parameters
!        Additional checks added with the addition of the AWMAEntrain
!         downwash option
      IF (FIELD(3) == FIELD(4)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(6)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(6)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(5) == FIELD(6)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

   ENDIF

!CRT  2/2/2021: D059 - AWMA downwash alpha options AWMAEntrain and AWMAUTurbHX
!RLP  Addition of options requires checking for 5 possible
!CRT  Number of fields increased from 5 to 7 for two newest options
!CRT  (AWMAUTurbHX and AWMAEntrain). If both AWMAUTurb and AWMAUTurbHX are
!CRT  specified, warning message is output and AWMAUTurbHX is used.
   IF (IFC == 7) THEN
!        Five, i.e. all, parameters specified
      IF (FIELD(3) == 'STREAMLINE' .or.&
      &FIELD(3) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(3) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(3) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF

      IF (FIELD(4) == 'STREAMLINE' .or.&
      &FIELD(4) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(4) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(4) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
      END IF

      IF (FIELD(5) == 'STREAMLINE' .or.&
      &FIELD(5) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(5) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(5) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

      IF (FIELD(6) == 'STREAMLINE' .or.&
      &FIELD(6) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(6) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(6) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(6) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(6) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

      IF (FIELD(7) == 'STREAMLINE' .or.&
      &FIELD(7) == 'STREAMLINED') THEN
! ---       Process all structures as streamlined buildings
         L_STRMLN_BLDG = .TRUE.
         L_RECT_BLDG = .FALSE.
      ELSE IF (FIELD(7) == 'AWMAUEFF') THEN
         L_AWMA_Ueff = .TRUE.
      ELSE IF (FIELD(7) == 'AWMAUTURB') THEN
         L_AWMA_UTurb = .TRUE.
      ELSE IF (FIELD(7) == 'AWMAENTRAIN') THEN
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .TRUE.
      ELSE IF (FIELD(7) == 'AWMAUTURBHX') THEN
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .TRUE.
      ELSE
!           WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

! ---    Check for duplicate parameters
!        Additional checks added with the addition of the AWMAUturbHX
!         downwash option
      IF (FIELD(3) == FIELD(4)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(6)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(7)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(6)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(7)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(5) == FIELD(6)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(5) == FIELD(7)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(6) == FIELD(7)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

   ENDIF

!CRT  6/14/2019: STREAMLINE option requires AWMAUTURB
!CRT  Issue error if STREAMLINE flag is TRUE and
!CRT  AWMAUTURB flag is FALSE.
!JAT  6/20/2020:  ISSUE D53 ADDED FROM 19191
!     GIVE ERROR CODE NEW NUMBER, 126 TO NOT
!     CONFLICT WITH 125 (PATHS NOT FINISHED)
!CRT  2/2/2021: STREAMLINE should also work with AWMAUTurbHX option
!CRT  Update conditional statement to include L_AWMA_UTurbHX
   IF (L_STRMLN_BLDG .and.&
   &(.NOT. L_AWMA_UTurb .and. .NOT. L_AWMA_UTurbHX)) THEN
!        WRITE Error Message    ! AWMADWUTurb option required
!         CALL ERRHDL(PATH,MODNAM,'E','125',KEYWRD)
      CALL ERRHDL(PATH,MODNAM,'E','126',KEYWRD)
   END IF

999 RETURN
END SUBROUTINE AWMA_DOWNWASH

SUBROUTINE ORD_DOWNWASH
!***********************************************************************
!              ORD_DOWNWASH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Informs AERMOD to use the PRIME algorithms with
!                 ORD modifications
!
!        OPTIONS:
!           ORDCAV -  improved cavity calculations
!           ORDUEFF - height to calculate main plume concentration
!           ORDTURB - Limit on vertical turbulence intensity
!
!        PROGRAMMER: Wood
!
!        DATE:    August 15, 2018
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Logical indicating type of structure
!
!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'PRIME_ORD'

   L_ORDDW    = .FALSE.
   L_ORD_Cav  = .FALSE.
   L_ORD_Ueff = .FALSE.
   L_ORD_Turb = .FALSE.


!     Check The Number Of The Fields
   IF (IFC <= 2) THEN
!        Error Message: No Parameters
      CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      GO TO 999
   ELSE IF (IFC > 5) THEN
!        Error Message: Too Many Parameters
      CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      GO TO 999
   END IF

   L_ORDDW = .TRUE.

! --- Decode the parameters to use in the calculations
   IF (IFC == 3) THEN
!        Only one parameter specified
      IF (FIELD(3) == 'ORDCAV') THEN
         L_ORD_Cav  = .TRUE.
      ELSE IF (FIELD(3) == 'ORDUEFF') THEN
         L_ORD_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'ORDTURB') THEN
         L_ORD_Turb = .TRUE.
      ELSE
! ---       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF
   END IF

   IF (IFC == 4) THEN
!        Two parameters specified - check each
      IF (FIELD(3) == 'ORDCAV') THEN
         L_ORD_Cav  = .TRUE.
      ELSE IF (FIELD(3) == 'ORDUEFF') THEN
         L_ORD_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'ORDTURB') THEN
         L_ORD_Turb = .TRUE.
      ELSE
! ---       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF

      IF (FIELD(4) == 'ORDCAV') THEN
         L_ORD_Cav  = .TRUE.
      ELSE IF (FIELD(4) == 'ORDUEFF') THEN
         L_ORD_Ueff = .TRUE.
      ELSE IF (FIELD(4) == 'ORDTURB') THEN
         L_ORD_Turb = .TRUE.
      ELSE
! ---       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
      END IF

! ---    Check for duplicate parameters
      IF (FIELD(3) == FIELD(4)) THEN
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

   END IF

   IF (IFC == 5) THEN
!        All three parameters specified
      IF (FIELD(3) == 'ORDCAV') THEN
         L_ORD_Cav  = .TRUE.
      ELSE IF (FIELD(3) == 'ORDUEFF') THEN
         L_ORD_Ueff = .TRUE.
      ELSE IF (FIELD(3) == 'ORDTURB') THEN
         L_ORD_Turb = .TRUE.
      ELSE
! ---       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
      END IF

      IF (FIELD(4) == 'ORDCAV') THEN
         L_ORD_Cav  = .TRUE.
      ELSE IF (FIELD(4) == 'ORDUEFF') THEN
         L_ORD_Ueff = .TRUE.
      ELSE IF (FIELD(4) == 'ORDTURB') THEN
         L_ORD_Turb = .TRUE.
      ELSE
! ---       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
      END IF

      IF (FIELD(5) == 'ORDCAV') THEN
         L_ORD_Cav  = .TRUE.
      ELSE IF (FIELD(5) == 'ORDUEFF') THEN
         L_ORD_Ueff = .TRUE.
      ELSE IF (FIELD(5) == 'ORDTURB') THEN
         L_ORD_Turb = .TRUE.
      ELSE
! ---       WRITE Error Message  ! Invalid Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
      END IF

! ---    Check for duplicate parameters
      IF (FIELD(3) == FIELD(4)) THEN
!           WRITE Error Message    ! Duplicate Option
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(4) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

      IF (FIELD(3) == FIELD(5)) THEN
!           WRITE Error Message    ! Duplicate Option
         CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
      END IF

   END IF

999 RETURN
END SUBROUTINE ORD_DOWNWASH
