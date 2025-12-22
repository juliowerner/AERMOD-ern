subroutine cocard
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
   use main1
   implicit none
   character :: modnam*12
   integer :: i

!     Variable Initializations
   modnam = 'COCARD'

   if (keywrd == 'STARTING') then
      iurb = 0
!        Set Status Switch
      istart = .true.
      icstat(1) = icstat(1) + 1
      if (icstat(1) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'TITLEONE') then
!        Set Status Switch
      icstat(2) = icstat(2) + 1
      if (icstat(2) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Titles                                  ---   CALL TITLES
         call titles
      end if
   else if (keywrd == 'TITLETWO') then
!        Set Status Switch
      icstat(3) = icstat(3) + 1
      if (icstat(3) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Titles                                  ---   CALL TITLES
         call titles
      end if
   else if (keywrd == 'MODELOPT') then
!        Set Status Switch
      icstat(4) = icstat(4) + 1
      if (icstat(4) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Modeling Options                        ---   CALL MODOPT
         call modopt
      end if
   else if (keywrd == 'AVERTIME') then
!        Set Status Switch
      icstat(5) = icstat(5) + 1
      if (icstat(5) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Averaging Time Options                  ---   CALL AVETIM
         call avetim
      end if
   else if (keywrd == 'POLLUTID') then
!        Set Status Switch
      icstat(6) = icstat(6) + 1
      if (icstat(6) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Pollutant ID Option                     ---   CALL POLLID
         call pollid
      end if
   else if (keywrd == 'HALFLIFE' .or.&
   &keywrd == 'DCAYCOEF') then
      if (keywrd == 'HALFLIFE') then
!           Check for Previous DCAYCOEF Keyword in Runstream File
         if (icstat(8) /= 0) then
            call errhdl(path,modnam,'W','155',keywrd)
            go to 999
         else
!              Set Status Switch and Check for Duplicate Keyword
            icstat(7) = icstat(7) + 1
            if (icstat(7) /= 1) then
!                 WRITE Error Message: Repeat Non-repeatable Keyword
               call errhdl(path,modnam,'E','135',keywrd)
               go to 999
            end if
         end if
      else if (keywrd == 'DCAYCOEF') then
!           Check for Previous HALFLIFE Keyword in Runstream File
         if (icstat(7) /= 0) then
            call errhdl(path,modnam,'W','155',keywrd)
            go to 999
         else
!              Set Status Switch and Check for Duplicate Keyword
            icstat(8) = icstat(8) + 1
            if (icstat(8) /= 1) then
!                 WRITE Error Message: Repeat Non-repeatable Keyword
               call errhdl(path,modnam,'E','135',keywrd)
               go to 999
            end if
         end if
      end if
!        Check for Keyword Out of Order
      if (icstat(4) /= 1) then
!           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
         call errhdl(path,modnam,'E','140',keywrd)
      else if (icstat(6) /= 1) then
!           WRITE Error Message: Keyword Out of Order (Must Follow POLLUTID)
         call errhdl(path,modnam,'E','140',keywrd)
      end if
!        Process Exponential Decay Option                   ---   CALL EDECAY
      call edecay
   else if (keywrd == 'FLAGPOLE') then
!        Set Status Switch
      icstat(11) = icstat(11) + 1
      if (icstat(11) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
         call flagdf
      end if
   else if (keywrd == 'RUNORNOT') then
!        Set Status Switch
      icstat(12) = icstat(12) + 1
      if (icstat(12) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Option to Run Model or Not              ---   CALL RUNNOT
         call runnot
      end if
   else if (.not.evonly .and. keywrd == 'EVENTFIL') then
!        Set Status Switch
      icstat(13) = icstat(13) + 1
      if (icstat(13) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (psdcredit) then
!              WRITE Warning Message:  PSDCREDIT option cannot be used with EVENT option
            call errhdl(path,modnam,'W','147',keywrd)
         end if
!           Process EVENT File Option                       ---   CALL EVNTFL
         call evntfl
      end if
   else if (.not.evonly .and. keywrd == 'SAVEFILE') then
!        Set Status Switch
      icstat(14) = icstat(14) + 1
      if (icstat(14) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Model Re-start Save File Option         ---   CALL SAVEFL
         call savefl
      end if
   else if (.not.evonly .and. keywrd == 'INITFILE') then
!        Set Status Switch
      icstat(15) = icstat(15) + 1
      if (icstat(15) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Re-start Initialization File Option     ---   CALL INITFL
         call initfl
      end if
   else if (.not.evonly .and. keywrd == 'MULTYEAR') then
!        Set Status Switch
      icstat(16) = icstat(16) + 1
      if (icstat(16) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Multiple-Year Run Option                ---   CALL MYEAR
         call myear
      end if
   else if (keywrd == 'ERRORFIL') then
!        Set Status Switch
      icstat(17) = icstat(17) + 1
      if (icstat(17) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Error File Option                       ---   CALL ERRFIL
         call errfil
      end if
   else if (keywrd == 'GDSEASON') then
!        Set Status Switch
      icstat(18) = icstat(18) + 1
      if (icstat(18) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Seasons for GASDEP Option              ---   CALL GDSEAS
         call gdseas
      end if
   else if (keywrd == 'GASDEPDF') then
!        Set Status Switch
      icstat(19) = icstat(19) + 1
      if (icstat(19) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process GASDEP Defaults Option                  ---   CALL GDDEF
         call gddef
      end if
   else if (keywrd == 'GDLANUSE') then
!        Set Status Switch
      icstat(20) = icstat(20) + 1
      if (icstat(20) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Error File Option                       ---   CALL GDLAND
         call gdland
      end if
   else if (keywrd == 'GASDEPVD') then
!        Set Status Switch
      icstat(21) = icstat(21) + 1
      if (icstat(21) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           User Specified Deposition Velocity Option       ---   CALL GVSUBD
         call gvsubd
      end if
   else if (keywrd == 'DEBUGOPT') then
!        Set Status Switch
      icstat(22) = icstat(22) + 1
      if (icstat(22) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Error File Option                       ---   CALL DEBOPT
         call debopt
      end if
   else if (keywrd == 'URBANOPT') then
!        Set Status Switch
      icstat(23) = icstat(23) + 1
!        Check for Keyword Out of Order
      if (icstat(4) /= 1) then
!           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
         call errhdl(path,modnam,'E','140',keywrd)
      end if
!        Process Urban Option                               ---   CALL URBOPT
      call urbopt
   else if (keywrd == 'OZONEVAL') then
!        Set Status Switch
      icstat(24) = icstat(24) + 1
      if (pvmrm .or. olm .or. runttrm .or. grsm) then
!           Process O3 Value Option                         ---   CALL O3VAL
         call o3val
      else
!RCO 3/1/2021 Check to see if we want to use 142 or 600 error code. CERC used 600
!           Write Error Message:  OZONEVAL specified w\o PVMRM, OLM, TTRM, GRSM
         call errhdl(path,modnam,'E','600',keywrd)
      end if
   else if (keywrd == 'O3VALUES') then
!        Set Status Switch
      icstat(25) = icstat(25) + 1
      if (pvmrm .or. olm .or. runttrm .or. grsm) then
!           Process O3 Value Option                         ---   CALL O3VALS
         call o3vals
      else
!           Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
         call errhdl(path,modnam,'E','600',keywrd)
      end if
   else if (keywrd == 'OZONEFIL') then
!        Set Status Switch
      icstat(26) = icstat(26) + 1
      if (pvmrm .or. olm .or. runttrm .or. grsm) then
!           Process O3 File Option                          ---   CALL O3FILE
         call o3file
      else
!           Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
         call errhdl(path,modnam,'E','600',keywrd)
      end if
   else if (keywrd == 'OZONUNIT') then
!        Set Status Switch
      icstat(27) = icstat(27) + 1
      if (icstat(27) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (pvmrm .or. olm .or. runttrm .or. grsm) then
!              Process the OZONUNIT Card                    ---   CALL OZON_UNIT
            call ozon_unit
         else
!              Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
            call errhdl(path,modnam,'E','600',keywrd)
         end if
      end if
   else if (keywrd == 'NO2STACK') then
!        Set Status Switch
      icstat(28) = icstat(28) + 1
      if (icstat(28) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (pvmrm .or. olm .or. runttrm .or. grsm) then
!              Process NO2Stack Option                      ---   CALL NO2STK
            call no2stk
         else
!              Write Error Message: NO2STACK specified w/o PVMRM, OLM, TTRM, GRSM
            call errhdl(path,modnam,'E','600',keywrd)
         end if
      end if
   else if (keywrd == 'NO2EQUIL') then
!        Set Status Switch
      icstat(29) = icstat(29) + 1
      if (icstat(29) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
! Add equilibrium limit for GRSM also?
         if (pvmrm .or. olm .or. runttrm) then
!              Process NO2Equil Option                      ---   CALL NO2EQ
            call no2eq
         else
!              Write Error Message:  NO2EQUIL specified without PVMRM or OLM or TTRM
            call errhdl(path,modnam,'E','142',keywrd)
         end if
      end if
   else if (keywrd == 'LOW_WIND') then
!        Set Status Switch
      icstat(30) = icstat(30) + 1
      if (icstat(30) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else if (l_alpha) then
!            Process LOW_WIND keyword                       ---   CALL LOW_WND
         call low_wnd
      else
!           WRITE Error Message: LOW_WIND option requires ALPHA option
         call errhdl(path,modnam,'E','133',keywrd)
      end if
   else if (keywrd == 'O3SECTOR') then
!        Set Status Switch
      icstat(31) = icstat(31) + 1
      if (icstat(31) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (pvmrm .or. olm .or. runttrm .or. grsm) then
!              Process O3SECTOR keyword                     ---   CALL O3SECTOR
            call o3sector
         else
!              Write Error Message: O3SECTOR specified w/o PVMRM, OLM, TTRM, GRSM
            call errhdl(path,modnam,'E','600',keywrd)
         end if
      end if
   else if (keywrd == 'ARMRATIO') then
!        Set Status Switch
      icstat(32) = icstat(32) + 1
      if (icstat(32) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (arm2) then
!              Process ARM2_Ratios Option                   ---   CALL ARM2_Ratios
            call ARM2_Ratios
         else
!              Write Error Message:  ARMRATIO specified without ARM2
            call errhdl(path,modnam,'E','145',keywrd)
         end if
      end if
   else if (keywrd == 'AWMADWNW') then
!        Set Status Switch
      icstat(33) = icstat(33) + 1
      if (icstat(33) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else if (l_alpha) then
!           Process AWMADWNW keyword                         ---   CALL AWMA_DOWNWASH
         call awma_downwash
      else
!           WRITE Error Message: AWMADWNW option requires ALPHA option
         call errhdl(path,modnam,'E','122',keywrd)
      end if
   else if (keywrd == 'ORD_DWNW') then
!        Set Status Switch
      icstat(34) = icstat(34) + 1
      if (icstat(34) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else if (l_alpha) then
!           Process ORD_DWNW keyword                       ---   CALL ORD_DOWNWASH
         call ord_downwash
      else
!           WRITE Error Message: ORD_DWNW option requires ALPHA option
         call errhdl(path,modnam,'E','123',keywrd)
      end if
!     CERC 11/30/20:
   else if (keywrd == 'NOXSECTR') then
!        Set Status Switch
      icstat(35) = icstat(35) + 1
      if (icstat(35) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (grsm) then
!              Process NOXSECTR keyword                    ---   CALL NOXSECTOR
            call noxsector
         else
!              Write Error Message:  NOXSECTR specified without GRSM
            call errhdl(path,modnam,'E','602',keywrd)
         end if
      end if
   else if (keywrd == 'NOXVALUE') then
!        Set Status Switch
      icstat(36) = icstat(36) + 1
      if (grsm) then
!           Process NOX Value Option                    ---   CALL NOXVAL
         call noxval
      else
!           Write Error Message:  NOXVALUE specified without GRSM
         call errhdl(path,modnam,'E','602',keywrd)
      end if
   else if (keywrd == 'NOX_VALS') then
!        Set Status Switch
      icstat(37) = icstat(37) + 1
      if (grsm) then
!           Process NOx Value Option                    ---   CALL NOXVALS
         call noxvals
      else
!           Write Error Message:  NOX_VALS specified without GRSM
         call errhdl(path,modnam,'E','602',keywrd)
      end if
   else if (keywrd == 'NOX_UNIT') then
!        Set Status Switch
      icstat(38) = icstat(38) + 1
      if (icstat(38) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
         if (grsm) then
!              Process the NOX_UNIT Card                    ---   CALL NOX_UNIT
            call nox_unit
         else
!              Write Error Message:  NOX_UNIT specified without GRSM
            call errhdl(path,modnam,'E','602',keywrd)
         end if
      end if
   else if (keywrd == 'NOX_FILE') then
!        Set Status Switch
      icstat(39) = icstat(39) + 1
      if (grsm) then
!           Process NOX File Option                    ---   CALL NOXFILE
         call noxfile
      else
!           Write Error Message:  NOX_FILE specified without GRSM
         call errhdl(path,modnam,'E','602',keywrd)
      end if

!**  Added for Aircraft Plume Rise; UNC-IE
   else if (keywrd == 'ARCFTOPT') then
!        Set Status Switch
      icstat(40) = icstat(40) + 1
!        Check for Keyword Out of Order
      if(icstat(4) /= 1) then
!           WRITE Error Message: Keyword Out of Order (Must Follow
!           MODELOPT)
         call errhdl(path,modnam,'E','140',keywrd)
      end if
      if (ifc == 3) then
!            Assign the airport name (optional)
         aftid(1) = field(3)
      end if
!     Assign Logical for Aircraft Option
      arcft  = .true.
!**  End Aircraft Plume Rise insert; April 2023

   else if (keywrd == 'FINISHED') then
!        Set Status Switch
      ifinis = .true.
!        Set Status Switch
      icstat(50) = icstat(50) + 1
      if (icstat(50) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
         go to 999
      end if

!        Check for Missing Mandatory Keywords
      if (icstat(1) == 0) then
         call errhdl(path,modnam,'E','130','STARTING')
      end if
      if (icstat(2) == 0) then
         call errhdl(path,modnam,'E','130','TITLEONE')
      end if
      if (icstat(4) == 0) then
         call errhdl(path,modnam,'E','130','MODELOPT')
      end if
      if (icstat(5) == 0) then
         call errhdl(path,modnam,'E','130','AVERTIME')
      end if
      if (icstat(6) == 0) then
         call errhdl(path,modnam,'E','130','POLLUTID')
      end if
      if (icstat(12) == 0) then
         call errhdl(path,modnam,'E','130','RUNORNOT')
      end if

      if (olm .or. pvmrm .or. runttrm .or. grsm) then
! ---       Check for background Ozone options for OLM/PVMRM/GRSM
         if (icstat(24)==0 .and. icstat(25)==0 .and.&
         &icstat(26)==0) then
!              Write Error Message:  Ozone value or data file needed
            if (olm) then
               dummy = 'OLM Option  '
            else if (pvmrm) then
               dummy = 'PVMRM Option'
            else if (grsm) then
               dummy = 'GRSM Option'
            else if (runttrm) then
               dummy = 'TTRM Option  '
            end if
            call errhdl(path,modnam,'E','283',dummy)
         else if (icstat(24)>0 .and. icstat(25)>0) then
! ---          Both OZONEVAL and O3VALUES keywords have been specified;
!              issue error message if needed; first check for O3SECTOR option
            if (.not. L_O3Sector) then
!                 No O3SECTORs; issue error message
               if (olm) then
                  dummy = 'for OLM   '
               else if (pvmrm) then
                  dummy = 'for PVMRM '
               else if (grsm) then
                  dummy = 'for GRSM '
               else if (runttrm) then
                  dummy = 'for TTRM   '
               end if
!                 Issue error message for conflicting O3 options
               call errhdl(path,modnam,'E','148',dummy)
            else
! ---             Loop through O3SECTORs for both OZONEVAL & O3VALUES
               do i = 1, NUMO3sects
                  if (l_o3val(i) .and. l_o3values(i)) then
!                       Issue error message; OZONEVAL & O3VALUES for this sector
                     if (olm) then
                        write(dummy,'(''  OLM SECT'',I1)') i
                     else if (pvmrm) then
                        write(dummy,'(''PVMRM SECT'',I1)') i
                     else if (grsm) then
                        write(dummy,'(''GRSM SECT'',I1)') i
                     else if (runttrm) then
                        write(dummy,'(''  TTRM SECT'',I1)') i
                     end if
                     call errhdl(path,modnam,'E','148',dummy)
                  end if
               end do
            end if
         end if
!           CERC 11/30/20
         if (grsm) then
! ---         Check for background NOx options for GRSM
            if (icstat(36)==0 .and. icstat(37)==0 .and.&
            &icstat(39)==0 ) then
!               No NOx background has been specified so it will be calculated from NO2 equilibrium
! If the background NOX is missing, is it appropriate to compute from the equilibrium?
! Or use a fill value (default) to make sure it's conservative.
               L_CalcNOXFromNO2 = .true.
!               Write Wng Message:  NOx calculated from NO2
               dummy = 'for GRSM'
               call errhdl(path,modnam,'W','612',dummy)
            else if (icstat(36)>0 .and. icstat(37)>0) then
! ---           Both NOXVALUE and NOX_VALS keywords have been specified;
!               issue error message if needed; first check for NOXSECTOR option
               if (.not. L_NOxSector) then
!                 No NOXSECTORs; issue error message for conflicting NOx options
                  call errhdl(path,modnam,'E','605','for GRSM ')
               else
! ---             Loop through NOxSECTORs for both NOXVALUE & NOX_VALS
                  do i = 1, NUMNOxsects
                     if (l_noxvalue(i) .and. l_nox_vals(i)) then
!                       Issue error message; NOXVALUE & NOX_VALS for this sector
                        write(dummy,'(''GRSM SECT'',I1)') i
                        call errhdl(path,modnam,'E','605',dummy)
                     end if
                  end do
               end if
            end if
         end if
         if ((pvmrm .or. olm .or. runttrm .or. grsm)&
         &.and. icstat(28)==0) then
!              No NO2STACK card specified for PVMRM, OLM, TTRM or GRSM options.
!              Reinitialize ANO2_RATIO array to -9.0 to track whether
!              NO2/NOx ratios are applied on the SO Pathway with
!              NO2RATIO card for each source.
            ano2_ratio(:) = -9.0d0
         end if
! ---       Check for OZONUNIT keyword without O3VALUES keyword
         if (icstat(25) == 0 .and. icstat(27) > 0) then
            call errhdl(path,modnam,'E','193','CO O3VALUES')
         end if
! ---       CERC 11/30/20 Check for NOXUNIT keyword without NOX_VALS keyword
         if (icstat(37) == 0 .and. icstat(38) > 0) then
            call errhdl(path,modnam,'E','193','CO NOX_VALS')
         end if
      end if

! ---    Check for ARM2 inputs
      if (arm2) then
         if (icstat(32) == 0) then
! ---          No ARMRATIO keyword specified; apply default ratios for ARM2
            ARM2_Min = 0.50d0
            ARM2_Max = 0.90d0
         end if
      end if

!        OPEN Restart Save and Initialization Files
      if (rstsav) then
         dummy = 'SAVEFILE'
         open(unit=idpunt,err=99,file=savfil,form='UNFORMATTED',&
         &iostat=ioerrn,status='REPLACE')
!           Close SAVEFILE since it is re-opened in RSDUMP
         close (idpunt)
         if (savfl2 /= savfil) then
            open(unit=idpun2,err=99,file=savfl2,form='UNFORMATTED',&
            &iostat=ioerrn,status='REPLACE')
!              Close SAVEFILE since it is re-opened in RSDUMP
            close (idpun2)
         end if
      end if
      if (rstinp) then
         if (rstsav) then
! ---          First check for filename conflicts with SAVEFILEs
            if (inifil == savfil .or. inifil == savfl2) then
! ---             The INITFILE name matches a SAVEFILE name;
!                 issue error message
               call errhdl(path,modnam,'E','590','       ')
            else
! ---             No filename conflict, open INITFILE
               dummy = 'INITFILE'
               open(unit=irsunt,err=99,file=inifil,&
               &form='UNFORMATTED',iostat=ioerrn,status='OLD')
            end if
         else
! ---          No SAVEFILEs, so open INITFILE
            dummy = 'INITFILE'
            open(unit=irsunt,err=99,file=inifil,form='UNFORMATTED',&
            &iostat=ioerrn,status='OLD')
         end if
      end if

!        Check Averaging Periods Selected for SCREEN Mode Option
      if (screen) then
         if (numave > 1) then
!              WRITE Error Message:  Too Many Averaging Periods Selected
            call errhdl(path,modnam,'E','295',' 1h Only')
         else if (kave(1) /= 1) then
!              WRITE Error Message:  Invalid Averaging Period Selected
            call errhdl(path,modnam,'E','295',' 1h Only')
         end if
         if (period) then
!              WRITE Error Message:  Too Many Averaging Periods Selected
            call errhdl(path,modnam,'E','295',' 1h Only')
         end if
      end if

! ---    Check for non-DFAULT gas deposition options
! JAT 7/2/19 CHECK FOR ALPHA AND GAS DEPOSITION PARAMETERS
!         IF GAS DEPOSITION PARAMETERS AND NO ALPHA OPTION
!     ISSUE ERROR.  THERE IS ALSO A CHECK FOR GASDEPOS AND
!     ALPHA IN SOCARD
      if (dfault .and. icstat(18) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         call errhdl(path,modnam,'E','196','GDSEASON')
      elseif (.not. l_alpha .and. icstat(18) > 0) then !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         call errhdl(path,modnam,'E','198','GDSEASON')
      else if (icstat(18) > 0) then
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .true.
      end if
      if (dfault .and. icstat(19) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         call errhdl(path,modnam,'E','196','GASDEPDF')
      elseif (.not. l_alpha .and. icstat(19) > 0) then !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         call errhdl(path,modnam,'E','198','GASDEPDF')
      else if (icstat(19) > 0) then
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .true.
      end if
      if (dfault .and. icstat(20) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         call errhdl(path,modnam,'E','196','GDLANUSE')
      elseif (.not. l_alpha .and. icstat(20) > 0) then !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         call errhdl(path,modnam,'E','198','GDLANUSE')
      else if (icstat(20) > 0) then
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .true.
      end if
      if (dfault .and. icstat(21) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
         call errhdl(path,modnam,'E','196','GASDEPVD')
      elseif (.not. l_alpha .and. icstat(21) > 0) then !JAT ADDED 7/2/19
!           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
         call errhdl(path,modnam,'E','198','GASDEPVD')
      else if (icstat(21) > 0) then
!           Set flag for use of non-DEFAULT option
         L_NonDFAULT = .true.
      end if

! ---    Check for incompatibilities with user-specified deposition velocity
      if (luservd .and. (depos .or. wdep .or. wdplete)) then
!           Write Error Message: Wet deposition output incompatible with GASDEPVD option
         call errhdl(path,modnam,'E','243','GASDEPVD')
      end if

! ---    Check for incompatible gas deposition inputs with GASDEPVD option for
!        user-specified gas dry deposition velocity
      if (luservd .and. icstat(18) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
         call errhdl(path,modnam,'E','195','GDSEASON')
      end if
      if (luservd .and. icstat(19) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
         call errhdl(path,modnam,'E','195','GASDEPDF')
      end if
      if (luservd .and. icstat(20) > 0) then
!           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
         call errhdl(path,modnam,'E','195','GDLANUSE')
      end if

!        Generate MODOPS Character Array to Summarize Modeling Options
      if (dfault) then
         modops(1) = 'RegDFAULT'
      else if (L_NonDFAULT) then
         modops(1) = 'NonDFAULT'
      else
         modops(1) = '         '
      end if

      if (conc) then
         modops(2) = 'CONC'
      end if
      if (depos) then
         modops(3) = 'DEPOS'
      end if
      if (ddep) then
         modops(4) = 'DDEP'
      end if
      if (wdep) then
         modops(5) = 'WDEP'
      end if

      if (flatsrcs) then
         modops(6) = 'FLAT and'
         modops(7) = 'ELEV'
      else if (flat) then
         modops(6) = 'FLAT'
      else
         modops(7) = 'ELEV'
      end if

      if (flgpol) modops(8)  = 'FLGPOL'
      if (nostd)  modops(9)  = 'NOSTD'
      if (nochkd) then
         modops(10) = 'NOCHKD'
      else if (l_warnchkd) then
         modops(10) = 'WARNCHKD'
      end if

      if (fastall) then
         modops(11) = 'FASTALL'
      else if (fastarea) then
         modops(11) = 'FASTAREA'
      else if (nowarn) then
         modops(11) = 'NOWARN'
      end if

      if (screen) modops(12) = 'SCREEN'
      if (multyr) modops(13) = 'MULTYR'

      if (ardplete) then
         modops(14) = 'AREADPLT'
      else if (romberg) then
         modops(14) = 'ROMBERG'
      else if (ddplete) then
         modops(14) = 'DRYDPLT'
      else if (.not.ddplete) then
         modops(14) = 'NODRYDPLT'
      end if

      if (wdplete) then
         modops(15) = 'WETDPLT'
      else if (.not.wdplete) then
         modops(15) = 'NOWETDPLT'
      end if

      if (scim) modops(16) = 'SCIM'

      if (runttrm2) then
         if (pvmrm) then
            modops(17) = 'TTRM2_PVMRM'
         else if (olm) then
            modops(17) = 'TTRM2_OLM'
         else if (arm2) then
            modops(17) = 'TTRM2_ARM2'
         end if
      else if (pvmrm) then
         modops(17) = 'PVMRM'
      else if (olm) then
         modops(17) = 'OLM'
      else if (arm2) then
         modops(17) = 'ARM2'
      else if (grsm) then
         modops(17) = 'GRSM'
      else if (runttrm) then
         modops(17) = 'TTRM'
      end if

      if (psdcredit) then
         modops(18) = 'PSDCREDIT'
      endif

! ---    Add labels for non-DFAULT ALPHA and BETA Options
      if (.not. dfault .and. l_alpha) then
         modops(19) = 'ALPHA'
      else if (.not. dfault .and. beta) then
         modops(19) = 'BETA'
      else if (dfault .and. beta) then
         call errhdl(path,modnam,'E','204','BETA')
      else if (dfault .and. l_alpha) then
         call errhdl(path,modnam,'E','204','ALPHA')
      end if

! ---    Add MODOPS field for RURAL, URBAN, or RUR&URB
      if (nurbsrc == 0) then
! ---       No URBAN sources
         modops(20) = 'RURAL'
      elseif (nurbsrc == nsrc) then
! ---       All sources are URBAN
         modops(20) = 'URBAN'
      else
! ---       Urban and Rural sources
         modops(20) = 'Urb&Rur'
      end if

!---     Add label for NoUrbTran Non-regulatory Option
      if (.not. L_UrbanTransition) then
         modops(21) = 'NoUrbTran'
      end if

      if (L_VectorWS) then
         modops(23) = 'VectorWS'
      end if

!CRCO 1/7/21 D074 Add NoMinO3 to summary of model options
!Question - MODOPS seems to have double setting of some entries. In
!meset MODOPS(23) is also set (along with 24). Then in metext 26 & 26 are
!set. So setting to 28 here, as it seems to be the largest of the array not set
!(current array size is 30). Delete this extra comment during review.
      if (nomino3) then
         modops(28) = 'NoMinO3'
      end if

!**  Added for Aircraft Plume Rise; UNC-IE
! ---    Add MODOPS field for Aircraft
      if (naftsrc == nsrc .or. naftsrc > 0.0d0) then
! ---       Aircraft sources
         modops(24) = 'AIRCRAFT'
!             Check for ALPHA and DFAULT conflicts
!WSP begin --- D151 ALPHA requirement MGS 5/18/2023
!WSP              IF (.NOT. BETA) THEN
!WSPC               WRITE Error Message     ! BETA Option Required for ARCFT
!WSP                CALL ERRHDL(PATH,MODNAM,'E','199','ARCFTOPT')
!WSP              END IF
!WSP          CHECK ALPHA ON MODELOPT
         if (.not. l_alpha) then
!               WRITE Error Message     ! ALPHA Option Required for ARCFT
            call errhdl(path,modnam,'E','198','ARCFTOPT')
         end if
!WSP end  --- D151 ALPHA requirement MGS 5/18/2023
         if (dfault) then
!               WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204','ARCFTOPT')
         end if
      end if
!**  End Aircraft Plume Rise insert; April 2023

! Added for HBP, JAN 2023
      if (hbplume) then
         modops(29) = 'HBP'
      end if
! End HBP insert


! ---    Note that the non-DFAULT/BETA options of using met data processed
!        with the ADJ_U* option in AERMET or using MMIF-generated met inputs
!        are identified based on flags included in the surface file header.

      if (scim .and. numave>0) then
!           Write Error Message:  Cannot use SCIM with short term averages
         call errhdl(path,modnam,'E','154','ST AVES')
      end if
      if (scim .and. period) then
!           Write Error Message:  Cannot use SCIM with PERIOD average
         call errhdl(path,modnam,'E','154','PERIOD')
      end if
      if (scim .and. depos) then
!           Write Warning Message:  Ignore DEPOS when using SCIM
         depos = .false.
         numtyp = numtyp - 1
         call errhdl(path,modnam,'W','156',' DEPOS ')
      end if

!        Adjust output label for ANNUAL average deposition fluxes
      if (annual) then
         do ityp = 1, numtyp
            if (.not.conc .or. ityp>1) then
               perlbl(ityp) = 'GRAMS/M**2/YR'
            end if
         end do
      end if

! ---    Check for completeness of O3 data inputs
      if (L_O3Sector) then
! ---       Check for temporally varying O3VALUES concentrations, OZONEVAL, and/or O3FILEs by sector.
         do i = 1, NUMO3Sects
            if (L_O3File(i)) then
!                 O3FILE for this sector; check for O3VALUES
               if (l_o3values(i)) then
!                    O3VALUES values available for this sector; check completeness
                  if (io3set(i) < io3max(i)) then
!                       WRITE Error Message: Not Enough O3VALUES values
                     if (io3set(i) < 100) then
                        write(dummy,'(''O3SECT'',I1,'' N='',I2)')&
                        &i, io3set(i)
                     else
                        write(dummy,'(''SECT'',I1,'' N='',I4)')&
                        &i, io3set(i)
                     end if
                     call errhdl(path,modnam,'E','261',dummy)
                  end if
               else if (.not. l_o3values(i).and.&
               &.not. l_o3val(i)) then
! ---                WRITE Warning Message: HOURLY O3FILE but no O3VALs avail for this Sector;
!                    full conversion assumed for missing hourly data (subject to equilibrium
!                    ratio)
                  write(dummy,'(''O3SECT'',I1)') i
                  call errhdl(path,modnam,'W','271',dummy)
               end if
            else if (.not. L_O3File(i)) then
!                 No HOURLY O3 file available for this sector; check for O3VALUES
               if (l_o3values(i)) then
!                    O3VALUES values available for this sector; check completeness
                  if (io3set(i) < io3max(i)) then
!                       WRITE Error Message: Not Enough O3VALUES values
                     if (io3set(i) < 100) then
                        write(dummy,'(''O3SECT'',I1,'' N='',I2)')&
                        &i, io3set(i)
                     else
                        write(dummy,'(''SECT'',I1,'' N='',I4)')&
                        &i, io3set(i)
                     end if
                     call errhdl(path,modnam,'E','261',dummy)
                  end if
               else if (.not. l_o3values(i).and.&
               &.not. l_o3val(i)) then
!                    WRITE Error Message: No O3VALUES values and no O3FILE - Missing Sector
                  write(dummy,'(''O3SECT'',I1,'' Msg'')') i
                  call errhdl(path,modnam,'E','261',dummy)
               end if
            end if
         end do
      else ! .NOT. L_O3Sector
! ---       No O3SECTORs, check for temporally varying O3VALUES concentrations and/or O3FILE
!           Set sector index (I) to 1
         i = 1
         if (L_O3File(i)) then
!              O3FILE for this sector; check for O3VALUES
            if (l_o3values(i)) then
!                 O3VALUES values available for this sector; check completeness
               if (io3set(i) < io3max(i)) then
!                    WRITE Error Message: Not Enough O3VALUES values
                  write(dummy,'(''NumVals='',I4)') io3set(i)
                  call errhdl(path,modnam,'E','261',dummy)
               end if
            else if (.not. l_o3values(i) .and.&
            &.not. l_o3val(i)) then
! ---             WRITE Warning Message: HOURLY O3FILE but no O3VALs avail for this Sector;
!                 full conversion assumed for missing hourly data (subject to equilibrium
!                 ratio)
               call errhdl(path,modnam,'W','271',' ')
            end if
         else if (.not. L_O3File(i)) then
            if (l_o3values(i)) then
!                 No O3FILE, but O3VALUES values available for this sector; check completeness
               if (io3set(i) < io3max(i)) then
!                    WRITE Error Message: Not Enough O3VALUES values
                  write(dummy,'(''NumVals='',I4)') io3set(i)
                  call errhdl(path,modnam,'E','261',dummy)
               end if
            end if
         end if

      end if

! ---    CERC 11/30/20 Check for completeness of NOx data inputs
      if(L_NOxSector)then
! ---      Check for temporally varying NOXVALUE, NOX_VALS, NOX_FILE by sector.
         do i = 1, NUMNOxSects
            if (L_NOxFile(i)) then
!                 NOX_FILE for this sector; check for NOX_VALS
               if (l_nox_vals(i)) then
!                    NOX_VALS values available for this sector; check completeness
                  if (inoxset(i) < inoxmax(i)) then
!                       WRITE Error Message: Not Enough NOX_VALS values
                     if (inoxset(i) < 100) then
                        write(dummy,'(''NOXSCT'',I1,'' N='',I2)')&
                        &i, inoxset(i)
                     else
                        write(dummy,'(''SECT'',I1,'' N='',I4)')&
                        &i, inoxset(i)
                     end if
                     call errhdl(path,modnam,'E','603',dummy)
                  end if
               else if (.not. l_nox_vals(i).and.&
               &.not. l_noxvalue(i)) then
! ---                WRITE Warning Message: HOURLY NOX_FILE but no NOX_VALS avail for this Sector;
!                    give zero concentration
                  write(dummy,'(''NOXSECT'',I1)') i
                  call errhdl(path,modnam,'W','611',dummy)
               end if
            else if (.not. L_NOxFile(i)) then
!                 No HOURLY NOx file available for this sector; check for NOX_VALS
               if (l_nox_vals(i)) then
!                    NOX_VALS avaialable for this sector; check completeness
                  if(inoxset(i) < inoxmax(i))then
!                       WRITE Error Message: Not enough NOX_VALS values
                     if (inoxset(i) < 100) then
                        write(dummy, '(''NOXSCT'',I1,'' N='',I2)')&
                        &i, inoxset(i)
                     else
                        write(dummy,'(''SECT'',I1,'' N='',I4)')&
                        &i, inoxset(i)
                     end if
                     call errhdl(path,modnam,'E','603',dummy)
                  end if
               else if (.not. l_nox_vals(i).and.&
               &.not. l_noxvalue(i)) then
!                    WRITE Error Message: No NOX_VALS values and no NOXFILE - Missing Sector
                  write(dummy,'(''NOXSECT'',I1,'' Msg'')') i
                  call errhdl(path,modnam,'E','603',dummy)
               end if
            end if
         end do
      else ! .NOT. L_NOxSector
! ---      NO NOxSectors, check for temporally-varying NOX_VALS concentrations
!          Set sector index (I) to 1
         i = 1
         if (L_NOxFile(i)) then
!              NOX_FILE for this sector; check for NOX_VALS
            if (l_nox_vals(i)) then
!                 NOX_VALS values available for this sector; check completeness
               if (inoxset(i) < inoxmax(i)) then
!                    WRITE Error Message: Not Enough NOX_VALS values
                  write(dummy,'(''NumVals='',I4)') inoxset(i)
                  call errhdl(path,modnam,'E','603',dummy)
               end if
            else if (.not. l_nox_vals(i) .and.&
            &.not. l_noxvalue(i)) then
! ---             WRITE Warning Message: HOURLY NOX_FILE but no NOXVALs avail for this Sector;
!                 give zero concentration
               call errhdl(path,modnam,'W','611',' ')
            end if
         else if (.not. L_NOXFile(i)) then
            if(l_nox_vals(i))then
!                 No NOXFILE but NOX_VALS values available for this sector; check completeness
               if (inoxset(i) < inoxmax(i)) then
!                    WRITE Error Message: Not Enough NOX_VALS values
                  write(dummy,'(''NumVals'',I4)') inoxset(i)
                  call errhdl(path,modnam,'E','603',dummy)
               end if
            end if
         end if
      end if

! ---    Check for user-specified ozone units; apply default if needed
      if (icstat(27) /= 1) then
         OzoneUnits = 'PPB'
      end if

! ---    CERC 11/30/20 Check for user-specified NOx units; apply default if needed
      if (icstat(38) /= 1) then
         NOxUnits = 'PPB'
      end if

! ---    Check for PM25 processing
      if ((pollut == 'PM25'  .or. pollut == 'PM-2.5' .or.&
      &pollut == 'PM-25' .or. pollut == 'PM2.5')) then
         if(.not.l_no_pm25ave .and. .not.nochkd .and.&
         &.not.l_warnchkd   .and. .not.evonly) then
! ---          Set logical flag for PM25 processing, averaged across years
            pm25ave = .true.
! ---          Now check for appropriate averaging periods for PM2.5
            if (numave>1 .or. (numave==1 .and.&
            &kave(1)/=24)) then
! ---             Write Error Message: Short Term average must be 24-hr only
               do i = 1, numave
                  if (kave(i) /= 24) then
                     write(dummy,'(I3,''-hr Ave'')') kave(i)
                     call errhdl(path,modnam,'E','363',dummy)
                  end if
               end do
               pm25ave = .false.
            end if
            if (period) then
! ---             Write Error Message: Long term average must be ANNUAL
               call errhdl(path,modnam,'E','363','PERIOD Ave')
               pm25ave = .false.
            end if
         else if (.not.screen .and. .not.evonly) then
! ---          Set to false for NOCHKD or WARNCHKD options, without the SCREEN or
!              EVONLY options, and issue warning message
            if (nochkd) then
               dummy = 'NOCHKD'
            else if (l_warnchkd) then
               dummy = 'WARNCHKD'
            end if
            call errhdl(path,modnam,'W','363',dummy)
            pm25ave = .false.
         else if (screen .or. evonly .or. l_no_pm25ave) then
! ---          Set PM25AVE to .FALSE. for SCREEN or EVONLY options or with
!              L_NO_PM25AVE option, without any warnings
            pm25ave = .false.
         end if
      end if

! ---    Check for NO2 1-hour NAAQS processing (NO2AVE = .T.)
      if (pollut == 'NO2' .and. .not.l_no_no2ave .and.&
      &.not.nochkd .and. .not.l_warnchkd  .and.&
      &.not.screen .and. .not.evonly) then
! ---       No options precluding NO2 1-hr NAAQS processing are specified;
!           next check for averaging periods to determine if multi-year
!           processing of maximum daily 1-hour averages is being done
         if (numave==1 .and. kave(1)==1 .and. .not.period&
         &.and. .not.annual) then
! ---          Set logical flag for 1-hr NO2 processing, averaged across years,
!              without PERIOD or ANNUAL averages
            no2ave = .true.
         else if (numave==1 .and. kave(1)==1 .and. period&
         &.and. multyr) then
! ---          Set logical flag for 1-hr NO2 processing, averaged across years,
!              using MULTYEAR option to address PERIOD averages
            no2ave = .true.
         else if (numave==1 .and. kave(1)==1 .and.&
         &(period .or. annual) .and.&
         &.not.multyr) then
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for NO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified.
            call errhdl(path,modnam,'W','361','MULTYEAR Opt')
! ---          Allow processing to continue, but long-term results
!              may be wrong.
            no2ave = .true.
         else if (numave>1 .or.&
         &(numave==1 .and. kave(1)/=1) .and.&
         &(period .or. annual) .and.&
         &.not.multyr) then
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for NO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified.  If NUMAVE > 1 then a non-standard short-term
!              average is being selected, and short-term averages other than
!              1-hour cannot be processed with the 1-hr NO2 NAAQS.
            call errhdl(path,modnam,'W','361','MULTYEAR Opt')
! ---          Write Warning Message: Non-standard short term average for NO2,
!              and disable special processing for 1-hour NAAQS.
            do i = 1, numave
               if (kave(i) /= 1) then
                  write(dummy,'(I3,''-hr Ave'')') kave(i)
                  call errhdl(path,modnam,'W','362',dummy)
               end if
            end do
            no2ave = .false.
         else if (numave>1 .or.&
         &(numave==1 .and. kave(1)/=1) ) then
! ---          Write Warning Message: Non-standard short term average for NO2
! ---          Write Warning Message: Short Term average should be 1-hr only for NO2
            do i = 1, numave
               if (kave(i) /= 1) then
                  write(dummy,'(I3,''-hr Ave'')') kave(i)
                  call errhdl(path,modnam,'W','362',dummy)
               end if
            end do
            no2ave = .false.
         else
! ---          Period or Annual average only, set NO2AVE = .F. but allow
!              processing
            no2ave = .false.
         end if
      else if (pollut == 'NO2' .and. .not.screen .and.&
      &.not.evonly) then
! ---       Set NO2AVE to false for NOCHKD, WARNCHKD and L_NO_NO2AVE options, without
!           the SCREEN or EVONLY options; issue warning messages for NOCHKD or WARNCHKD
!           (message has already been issued for L_NO_NO2AVE), and disable special
!           processing for 1-hour NAAQS.
         if (numave==1 .and. kave(1)==1 .and.&
         &((.not.period .and. .not.annual) .or.&
         &(period .and. multyr) .or.&
         &((period .or. annual) .and. .not.multyr)) )then
            if (nochkd) then
               dummy = 'NOCHKD'
               call errhdl(path,modnam,'W','362',dummy)
            else if (l_warnchkd) then
               dummy = 'WARNCHKD'
               call errhdl(path,modnam,'W','362',dummy)
            end if
            no2ave = .false.
         end if
      else if (pollut == 'NO2' .and. (screen .or. evonly)) then
! ---       Set NO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
         no2ave = .false.
      end if

! ---    Check for SO2 1-hour NAAQS processing (SO2AVE = .T.)
      if (pollut == 'SO2' .and. .not.nochkd .and. .not.l_no_so2ave&
      &.and. .not.l_warnchkd .and. .not.evonly) then
! ---       No options precluding SO2 1-hr NAAQS processing are specified;
!           next check for averaging periods to determine if multi-year
!           processing of maximum daily 1-hour averages is being done
         if (numave==1 .and. kave(1)==1 .and. .not.period&
         &.and. .not.annual) then
! ---          Set logical flag for 1-hr SO2 processing, averaged across years
            so2ave = .true.
         else if (numave==1 .and. kave(1)==1 .and. period&
         &.and. multyr) then
! ---          Set logical flag for 1-hr SO2 processing, averaged across years,
!              using MULTYEAR option to address PERIOD averages
            so2ave = .true.
         else if (numave==1 .and. kave(1)==1 .and.&
         &(period .or. annual) .and.&
         &.not.multyr) then
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for SO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified.
            call errhdl(path,modnam,'W','361','MULTYEAR Opt')
! ---          Allow processing to continue, but long-term results
!              may be wrong.
            so2ave = .true.
         else if (numave>1 .and. minval(kave)==1 .and.&
         &(period .or. annual) .and.&
         &.not.multyr) then
! ---          Write Warning Message: PERIOD averages should not be
!              processed with 1-hour averages for SO2 unless only 1 year
!              of met data is being processed or if the MULTYEAR option
!              is specified. Also, short-term averages other than
!              1-hour cannot be processed with the 1-hr SO2 NAAQS.
            call errhdl(path,modnam,'W','361','MULTYEAR Opt')
! ---          Write Warning Message: Non-standard short term average for SO2,
!              and disable special processing for 1-hour NAAQS.
            do i = 1, numave
               if (kave(i) /= 1) then
                  write(dummy,'(I3,''-hr Ave'')') kave(i)
                  call errhdl(path,modnam,'W','362',dummy)
               end if
            end do
! ---          Allow processing to continue, but without special processing
!              of 1-hr values averaged across years
            so2ave = .false.
         else if (numave>1 .and. minval(kave)==1) then
! ---          Write Warning Message: Non-standard short term average for SO2,
!              and disable special processing for 1-hour NAAQS.
            do i = 1, numave
               if (kave(i) /= 1) then
                  write(dummy,'(I3,''-hr Ave'')') kave(i)
                  call errhdl(path,modnam,'W','362',dummy)
               end if
            end do
! ---          Allow processing to continue, but without special processing
!              of 1-hr values averaged across years
            so2ave = .false.
         else
! ---          Period or Annual average only, set SO2AVE = .F. but allow
!              processing
            so2ave = .false.
         end if
      else if (pollut == 'SO2' .and. .not.screen .and.&
      &.not.evonly) then
! ---       Set SO2AVE to false for NOCHKD or WARNCHKD options, without the
!           SCREEN or EVONLY options, issue warning message, and disable
!           special processing for 1-hour NAAQS.
         if (numave==1 .and. kave(1)==1 .and.&
         &((.not.period .and. .not.annual) .or.&
         &(period .and. multyr) .or.&
         &((period .or. annual) .and. .not.multyr)) )then
            if (nochkd) then
               dummy = 'NOCHKD'
            else if (l_warnchkd) then
               dummy = 'WARNCHKD'
            end if
            call errhdl(path,modnam,'W','362',dummy)
            so2ave = .false.
         end if
      else if (pollut == 'SO2' .and. (screen .or. evonly)) then
! ---       Set SO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
         so2ave = .false.
      end if

! ---    Check for pollutant ID = 'NO2' for PVMRM, OLM, ARM2 and GRSM options
      if ((pvmrm .or. olm .or. arm2 .or. grsm) .and.&
      &pollut /= 'NO2') then
!           Write Error Message:  Pollutant ID doesn't match option
         call errhdl(path,modnam,'E','284',' NO2 ')
      end if

! ---    Check for PM25, NO2, or SO2 processing based on ranked values
!        averaged across years, and adjust PLOTFILE format accordingly
      if (pm25ave .or. no2ave .or. so2ave) then
         pltfrm = '(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,&
         &               10(F13.5,2X,I8.8,2X:))'
      end if

! ---    AWMA enhancements to PRIME
! ---    AWMADWNW and ORD_DWNW and have different ways to define
!          the height at which to compute the effective wind speed.
!          One or the other can be specified, or neither, but BOTH CANNOT
!          be specified.  Check for that here.  A similar check is performed
!          in ORD_DOWNWASH

      if (l_awma_ueff .and. l_ord_ueff) then
! ---       Write error message
         call errhdl(path,modnam,'E','124','  ')
      end if

! ---    Check for conflicts between AWMA and ORD Ueff options (which
!         is an error) as well as AWMAUTurb and AWMAUturbHX (which only
!         issues a warning)
      if (L_AWMA_UTurb .and. L_AWMA_UTurbHX) then
         call errhdl(path,modnam,'W','478',' ')
      end if

      go to 1000

!        WRITE Error Message for Error Opening File
99    call errhdl(path,modnam,'E','500',dummy)
      if (dummy == 'SAVEFILE') then
!           Reset Logical Flag for SAVEFILE Option Due to Error Opening File
         rstsav = .false.
      else if (dummy == 'INITFILE') then
!           Reset Logical Flag for INITFILE Option Due to Error Opening File
         rstinp = .false.
      end if

1000  continue

   else
!        Write Error Message: Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

999 return
end subroutine cocard

subroutine titles
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'TITLES'

   if (keywrd == 'TITLEONE') then
      title1 = runst1(loce(2)+2:min(len_trim(runst1),&
      &(loce(2)+2+ilen_fld-1)))
      if (title1 == ' ') then
!*          Write Error Message: Missing Parameter Title
         call errhdl(path,modnam,'W','200',keywrd)
      end if

   else if (keywrd == 'TITLETWO') then
      title2 = runst1(loce(2)+2:min(len_trim(runst1),&
      &(loce(2)+2+ilen_fld-1)))
      if (title2 == ' ') then
!*          Write Warning Message
         call errhdl(path,modnam,'W','200',keywrd)
      end if

   end if

   return
end subroutine titles


subroutine modopt
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
   use main1
   use rline_data, only: fac_dispht !needed for RLINEFDH MODELOPT (Wood 6/22/2021)
   implicit none
   character :: modnam*12

   integer :: i
   character :: kopt*9

!     Variable Initializations - Initialize All Logical Switches to FALSE
   modnam = 'MODOPT'

!     Check for Too Few or Too Many Parameters
   if (ifc < 3) then
!        WRITE Error Message     ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   else if (ifc > 14) then
!        WRITE Warning Message   ! Too Many Parameters
      call errhdl(path,modnam,'W','202',keywrd)
   end if

! --- Assign DEFAULT value of 0.2D0 to SVMIN, minimum
!     sigma-v value (formerly assigned as a PARAMETER);
!CRT  value of SVMIN may be increased to 0.5D0 using
!CRT  the ALPHA option and the CO LOW_WIND keyword:
   svmin = 0.2d0

! --- Assign DEFAULT value for minimum wind speed of
!     0.2828 m/s, equivalent to SQRT(2*SVMIN*SVMIN)
!     used in previous versions.
!CRT  User can change the value of WSMIN using the ALPHA model
!CRT  option and the CO LOW_WIND keyword.
   wsmin = 0.2828d0

!CRT  Assign DEFAULT value of 1.0 for the maximum meander
!CRT  factor, FRANMAX. The value of FRANMAX was adjusted
!CRT  to 0.95 under the former LowWind2 BETA option, and the
!CRT  user could modify the value used under the LowWind2
!CRT  option using the CO LOW_WIND keyword, within a
!CRT  range from 0.50 to 1.0, inclusive.
!CRT  1/29/2018 LowWind2 has been removed as a BETA option. The user
!CRT  can now modify the FRANMAX value is using the ALPHA
!CRT  option and the CO LOW_WIND keyword
   franmax = 1.0d0

!**   3/18/2022 add FRANMIN for meander testing
!**   range from 0 to 1. Default value is 0 -Wood
   franmin = 0.0d0

!CRT  CRT 9/11/2020, D062 User Minimum Sigma W
!CRT  Assign DEFAULT value for minimum sigma w of 0.02 m/s
!CRT  (formerly assigned as a PARAMETER;
!CRT  Value can now be assigned by the user using the the ALPHA
!CRT  model option and the CO LOW_WIND keyword.
   swmin = 0.02d0

!RCO  RCO 9/27/2020, D061 User BIGT value
!RCO  Assign DEFAULT value for BIGT value of 24 hours
!RCO  (formerly a PARAMETER in MEANDR routine in calc2)
!RCO  Value can now be assigned by the user using the the ALPHA
!RCO  model option and the CO LOW_WIND keyword.
   bigt = 24.0d0

!MGS Added MODELOPT keyword RLINEFDH, which will change this value
!MGS (from RLINE_v1_2) to =0.0. Wood 6/22/2021
   fac_dispht  = 5.0d0


! --- Assign DEFAULT value of 2.15 for the SZCOEF parameter
!     used in the IBL calculations.
   szcoef = 2.15d0

!     First Check for Presence of DFAULT Switch
   do i = 3, ifc
      kopt = field(i)
      if (kopt == 'DFAULT' .or. kopt == 'DEFAULT') then
         dfault      = .true.
         elev        = .true.
         flat        = .false.
         flatsrcs    = .false.
         msgpro      = .true.
         nostd       = .false.
         nochkd      = .false.
         screen      = .false.
         scim        = .false.
         psdcredit   = .false.
         beta        = .false.
         l_alpha     = .false.
         fastarea    = .false.
         fastall     = .false.
         l_effsigy   = .false.
         low_wind    = .false.
         L_NonDFAULT = .false.
         L_UrbanTransition = .true.
         grsm       = .false.
         nomino3    = .false.
         l_areamndr   = .false.  !Added area meander flag to the alpha options D128 Wood 6/3/22
         exit
      end if
   end do

!     Next check for presence of both FLAT and ELEV if NOT.DFAULT
   if (.not. dfault) then
!        First look for FLAT
      do i = 3, ifc
         kopt = field(i)
         if (kopt == 'FLAT') then
            flat = .true.
            elev = .false.
            exit
         end if
      end do
!        If FLAT, next look for ELEV, indicating both FLAT and
!        ELEV sources in the same run (FLATSRCS)
      if (flat) then
         do i = 3, ifc
            kopt = field(i)
            if (kopt == 'ELEV') then
               elev     = .true.
               flatsrcs = .true.
               exit
            end if
         end do
      end if
   else
!        Look for FLAT with DFAULT
      do i = 3, ifc
         kopt = field(i)
         if (kopt == 'FLAT') then
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            call errhdl(path,modnam,'W','206',kopt)
         end if
      end do
   end if

!     Next Check for Presence of BETA Switch
   do i = 3, ifc
      kopt = field(i)
      if (kopt == 'BETA' .and. .not.dfault) then
         beta = .true.
      else if (kopt == 'ALPHA' .and. .not.dfault) then
         l_alpha = .true.
      end if
   end do

   numtyp = 0
!     Loop Through Fields Again Setting All Swithes
   do i = 3, ifc
      kopt = field(i)
      if (kopt == 'DFAULT' .or. kopt == 'DEFAULT') then
         dfault = .true.
      else if (kopt == 'CONC') then
         if (.not. conc) then
            conc   = .true.
            numtyp = numtyp + 1
         end if
      else if (kopt == 'DEPOS') then
         if (.not. depos) then
            depos  = .true.
            numtyp = numtyp + 1
         end if
      else if (kopt == 'DDEP') then
         if (.not. ddep) then
            ddep   = .true.
            numtyp = numtyp + 1
         end if
      else if (kopt == 'WDEP') then
         if (.not. wdep) then
            wdep   = .true.
            numtyp = numtyp + 1
         end if
      else if (kopt == 'FLAT' .or. kopt == 'ELEV') then
         cycle
      else if (kopt == 'DRYDPLT' .and. .not.nodrydplt) then
         ddplete = .true.
         drydplt = .true.
      else if (kopt == 'DRYDPLT' .and. nodrydplt) then
! ---       Write Error Message        ! Conflicting options specified
         call errhdl(path,modnam,'E','149',kopt)
      else if (kopt == 'NODRYDPLT' .and. .not.drydplt) then
!           Dry depletion is now standard - include "option" to override it
         ddplete = .false.
!           Set separate logical for user-specified option to ensure that
!           it is reflected in the page header
         nodrydplt = .true.
      else if (kopt == 'NODRYDPLT' .and. drydplt) then
! ---       Write Error Message        ! Conflicting options specified
         call errhdl(path,modnam,'E','149',kopt)
      else if (kopt == 'ROMBERG') then
         romberg = .true.
         ddplete = .true.
      else if (kopt == 'AREADPLT') then
         if (.not. dfault) then
            ardplete = .true.
            ddplete  = .true.
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
      else if (kopt == 'WETDPLT' .and. .not.nowetdplt) then
         wdplete = .true.
         wetdplt = .true.
      else if (kopt == 'WETDPLT' .and. nowetdplt) then
! ---       Write Error Message        ! Conflicting options specified
         call errhdl(path,modnam,'E','149',kopt)
      else if (kopt == 'NOWETDPLT' .and. .not.wetdplt) then
!           Wet depletion is now standard - include "option" to override it
         wdplete = .false.
!           Set separate logical for user-specified option to ensure that
!           it is reflected in the page header
         nowetdplt = .true.
      else if (kopt == 'NOWETDPLT' .and. wetdplt) then
! ---       Write Error Message        ! Conflicting options specified
         call errhdl(path,modnam,'E','149',kopt)
      else if (kopt == 'NOSTD') then
         if (.not. dfault) then
            nostd = .true.
         else
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            call errhdl(path,modnam,'W','206',kopt)
            nostd = .false.
         end if
      else if (kopt == 'NOWARN') then
         nowarn = .true.
      else if (kopt == 'NOCHKD') then
         if (.not. dfault .and. .not.l_warnchkd) then
            nochkd = .true.
         else if (.not.dfault .and. l_warnchkd) then
! ---          Write Error Message        ! Conflicting options specified
            call errhdl(path,modnam,'E','149',kopt)
         else
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            call errhdl(path,modnam,'W','206',kopt)
            nochkd = .false.
         end if
      else if (kopt == 'WARNCHKD') then
         if (.not. nochkd) then
            l_warnchkd = .true.
         else
! ---          Write Error Message        ! Conflicting options specified
            call errhdl(path,modnam,'E','149',kopt)
         end if
      else if (kopt == 'SCREEN') then
         if (.not. dfault) then
            screen = .true.
!              Set NOCHKD option on for SCREEN mode
            nochkd = .true.
         else
!              WRITE Warning Message     ! Non-DEFAULT Option Overridden
            call errhdl(path,modnam,'W','206',kopt)
            screen = .false.
         end if
      else if (kopt == 'SCIM') then
         if (.not. dfault) then
            scim = .true.
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
            scim = .false.
         end if
      else if (kopt == 'TOXICS') then
! ---       WRITE Warning Message        ! TOXICS option is obsolete
!           If this run includes area or openpit sources, refer to
!           FASTAREA option and set the logical flag
         if (narea > 0 .or. nline > 0 .or. npit > 0) then
            call errhdl(path,modnam,'W','198','FASTAREA')
         else
            call errhdl(path,modnam,'W','198','        ')
         end if
         if (.not. dfault) then
            if (narea > 0 .or. nline > 0 .or. npit > 0) then
! ---             Assign FASTAREA option to TRUE for consistency with
!                 area source optimizations under obsolete TOXICS option
               fastarea = .true.
            end if
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
! Added for Highly Buoyant Plume (HBP) option; JAN 2023
      else if (kopt == 'HBP') then
         hbplume = .true.
         if (.not. l_alpha) then
!              WRITE Error Message     ! ALPHA Option Required for TTRM2
            call errhdl(path,modnam,'E','198',kopt)
!! End TTRM2 insert
         end if
         if (dfault) then
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
! End HBP insert
      else if (kopt == 'PVMRM') then
         pvmrm = .true.
      else if (kopt == 'OLM') then
         olm = .true.
      else if (kopt == 'TTRM') then
         runttrm = .true.
         if (.not. l_alpha) then
!              WRITE Error Message     ! ALPHA Option Required for TTRM
            call errhdl(path,modnam,'E','198',kopt)
         endif
!! Added Nov. 2021
      else if (kopt == 'TTRM2') then
         runttrm2 = .true.
         runttrm = .true.
         if (.not. l_alpha) then
!              WRITE Error Message     ! ALPHA Option Required for TTRM2
            call errhdl(path,modnam,'E','198',kopt)
!! End TTRM2 insert
         end if
         if (dfault) then
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
      else if (kopt == 'ARM2') then
         arm2 = .true.
         if( dfault )then
            ARM2_Min = 0.50d0
            ARM2_Max = 0.90d0
         endif
      else if (kopt == 'GRSM') then
!          CERC 11/30/20
!CRT 4/1/2022 GRSM in version 21112 updated from ALPHA to BETA
         grsm = .true.

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
      else if (kopt == 'PSDCREDIT') then                           ! jop 093006
         if (l_alpha .and. .not. dfault) then
            psdcredit = .true.
         else if (.not. l_alpha .and. .not. dfault) then
!              WRITE Error Message     ! ALPHA Option Required for PSDCREDIT
            call errhdl(path,modnam,'E','198',kopt)
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
!CRT 1/29/2018 Remove restriction that user can only specify ALPHA or BETA
      else if (kopt == 'ALPHA') then
!CRT            IF (BETA) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
!CRT               CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA & BETA')
!CRT            ELSE IF (.NOT. DFAULT) THEN
         if (.not. dfault) then
            l_alpha = .true.
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
!CRT 1/29/2018 Remove restriction that user can only specify ALPHA or BETA
      else if (kopt == 'BETA') then
!CRT            IF (L_ALPHA) THEN
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
!CRT               CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA & BETA')
!CRT            ELSE IF (.NOT. DFAULT) THEN
         if (.not. dfault) then
            beta = .true.
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if

      else if (kopt == 'FASTAREA') then
         if (fastall) then
!              Issue warning message since FASTALL implies FASTAREA
            call errhdl(path,modnam,'W','192','        ')
         end if
         if (.not. dfault) then
!              Set logical flag for FASTAREA for optimized area source;
!              equivalent to optimizations associated with obsolete TOXICS option
            fastarea = .true.
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
      else if (kopt == 'FASTALL') then
         if (fastarea) then
!              Issue warning message since FASTALL implies FASTAREA
            call errhdl(path,modnam,'W','192','        ')
         end if
         if (.not. dfault) then
! ---          Set logical flag for FASTAREA for optimized area source;
!              equivalent to optimizations associated with obsolete TOXICS option.
            fastarea = .true.
! ---          Also set L_EFFSIGY option flag for optimized meander option for
!              point and volume sources.
            fastall   = .true.
            l_effsigy = .true.
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if
      else if (kopt == 'NOURBTRAN') then
         if (.not. dfault) then
! ---          Non-regulatory option to ignore transition from nighttime urban
!              enhanced boundary layer to daytime convective boundary
! ---          Set logical switch to account for urban transition to .F. and
!              issue warning message
            L_UrbanTransition = .false.
!              WRITE Warning Message
            call errhdl(path,modnam,'W','151','Keyword     ')
         else
!              WRITE Error Message     ! Non-DEFAULT Option Conflict
            call errhdl(path,modnam,'E','204',kopt)
         end if

! -----  Add option for use of vector-mean wind speeds, VECTORWS
      else if (kopt == 'VECTORWS') then
         l_vectorws = .true.
! ---       Write Warning Message
         call errhdl(path,modnam,'W','116',kopt)

! -----  Add ALPHA option for low-wind-speed modifications, LOWWIND
      else if (kopt == 'LOW_WIND') then
         if (l_alpha) then
! ---          Assign LOW_WIND = .TRUE.
            low_wind = .true.
            call errhdl(path,modnam,'W','136','NonDFAULT')
            dfault = .false.
         else
            low_wind = .false.
            call errhdl(path,modnam,'E','133','NonDFAULT')
         end if

! -----  Wood 6/3/22 D128 Add ALPHA option for area meander , AREAMNDR
      else if (kopt == 'AREAMNDR') then
         if (l_alpha) then
! ---          Assign AREAMNDR = .TRUE.
            l_areamndr = .true.
            call errhdl(path,modnam,'W','134','NonDFAULT')
            dfault = .false.
         else
            l_areamndr = .false.
            call errhdl(path,modnam,'E','139','NonDFAULT')
         end if

         if (.not.dfault) then
!              WRITE Error Message     ! ALPHA option required
            call errhdl(path,modnam,'W','198',kopt)
         else
!              WRITE Error Message     ! DFAULT option specified;
!                                        Issue ERROR message
            call errhdl(path,modnam,'E','204',kopt)
         end if


! ---       Assign minimum wind speed, WSMIN
! WSMIN is already set above, approx. line 1241. Currently no "default"
! value for the LOW_WIND option specifically (which would be set here).
         wsmin = 0.2828d0

! ---       Add option for RLINE to use zero displacement height in wind profile
!            Michelle G. Snyder (Wood 6/22/2021)
      else if (kopt == 'RLINEFDH') then
         if (l_alpha) then !Require ALPHA on MODELOPT
            fac_dispht = 0.0d0
         else
            call errhdl(path,modnam,'E','198','RLINEFDH')
         end if

!RCO 1/7/21 D074 Add NOMINO3 option
      else if (kopt == 'NOMINO3') then
         nomino3 = .true.
         if (.not.olm .and. .not.pvmrm .and. .not.grsm&
         &.and. .not.runttrm) then
!              WRITE Warning Message  ! NOMINO3 set without NO2 technique
!              D154 Changed the warning message to an error is NOMINO3 is set without NO2 technique 1/31/23 WSP
!               CALL ERRHDL(PATH,MODNAM,'W','621',KOPT)
            call errhdl(path,modnam,'E','621',kopt)
         end if

      else !Not a vaild model option
!           WRITE Error Message     ! Invalid Parameter
         call errhdl(path,modnam,'E','203',kopt)


      end if

   end do

! --- Check for conflicting NO2 options:
   if (olm .and. pvmrm) then
!        WRITE Error Message       ! Can't specify OLM & PVMRM
      call errhdl(path,modnam,'E','141','OLM & PVMRM')
   else if (olm .and. arm2) then
!        WRITE Error Message       ! Can't specify OLM & ARM2
      call errhdl(path,modnam,'E','141','OLM & ARM2')
   else if (olm .and. grsm) then
!        WRITE Error Message       ! Can't specify OLM & GRSM
      call errhdl(path,modnam,'E','141','OLM & GRSM')
   else if (pvmrm .and. arm2) then
!        WRITE Error Message       ! Can't specify PVMRM & ARM2
      call errhdl(path,modnam,'E','141','PVMRM & ARM2')
   else if (pvmrm .and. grsm) then
!        WRITE Error Message       ! Can't specify PVMRM & GRSM
      call errhdl(path,modnam,'E','141','PVMRM & GRSM')
   else if (arm2 .and. grsm) then
!        WRITE Error Message       ! Can't specify ARM2 & GRSM
      call errhdl(path,modnam,'E','141','ARM2 & GRSM')
   else if ((.not. runttrm2) .and. (runttrm .and. arm2)) then
!        WRITE Error Message       ! Can't specify TTRM & ARM2
      call errhdl(path,modnam,'E','141','TTRM & ARM2')
   else if ((.not. runttrm2) .and. (runttrm .and. olm)) then
!        WRITE Error Message       ! Can't specify TTRM & OLM
      call errhdl(path,modnam,'E','141','TTRM & OLM')
   else if ((.not. runttrm2) .and. (runttrm .and. pvmrm)) then
!        WRITE Error Message       ! Can't specify TTRM & PVMRM
      call errhdl(path,modnam,'E','141','TTRM & PVMRM')
   else if (runttrm .and. grsm) then
!        WRITE Error Message       ! Can't specify TTRM & GRSM
      call errhdl(path,modnam,'E','141','TTRM & GRSM')
!! Added Nov. 2021
   else if (runttrm2 .and. grsm ) then
!        WRITE Error Message       ! Can't specify TTRM & ARM2
      call errhdl(path,modnam,'E','141','TTRM2 & GRSM')
   else if (runttrm2 .and. olm .and. arm2) then
!        WRITE Error Message       ! Can't specify TTRM & OLM
      call errhdl(path,modnam,'E','141','TTRM2 & >2 NO2 Options')
   else if (runttrm2 .and. pvmrm .and. arm2) then
!        WRITE Error Message       ! Can't specify TTRM & PVMRM
      call errhdl(path,modnam,'E','141','TTRM2 & >2 NO2 Options')
   else if (runttrm2 .and. olm .and. pvmrm) then
!        WRITE Error Message       ! Can't specify TTRM & GRSM
      call errhdl(path,modnam,'E','141','TTRM2 & >2 NO2 Options')
!! End Nov. 2021 TTRM insert
   end if

   if (psdcredit .and. .not.pvmrm) then
!        WRITE Error Message       ! Can't specify PSDCREDIT without PVMRM
      call errhdl(path,modnam,'E','143','PSDCREDIT')
   end if

   if (psdcredit .and. evonly) then
!        WRITE Error Message       ! Can't use PSDCREDIT with EVONLY Processing
      call errhdl(path,modnam,'E','147',' EVENTS ')
   end if

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
   if (.not. dfault .and.&
   &(flat .or. flatsrcs .or. ardplete .or. nostd .or. nochkd .or.&
   &screen .or. scim .or. psdcredit .or. beta .or. fastarea .or.&
   &fastall .or. low_wind .or. l_alpha .or. grsm .or. L_PBal&
   &.or. .not.L_UrbanTransition)) then

      L_NonDFAULT = .true.
   end if

!     Setup Label Array for Concentration and Depositions
   if (numtyp > ntyp) then
!        WRITE Error Message: Number of output types exceeds maximum
      write(dummy,'(I4)') ntyp
      call errhdl(path,modnam,'E','280',dummy)
   else if (numtyp == 0) then
!        WRITE Warning Message: No Output Types Selected, Assume CONC Only
      call errhdl(path,modnam,'W','205','CONC')
      numtyp = 1
      ityp   = 1
      conc   = .true.
      chidep(1,ityp) = 'AVER'
      chidep(2,ityp) = 'AGE '
      chidep(3,ityp) = 'CONC'
      chidep(4,ityp) = 'ENTR'
      chidep(5,ityp) = 'ATIO'
      chidep(6,ityp) = 'N   '
      emifac(ityp) = 1.0d06
      emilbl(ityp) = 'GRAMS/SEC'
      outlbl(ityp) = 'MICROGRAMS/M**3'
      perlbl(ityp) = 'MICROGRAMS/M**3'
      outtyp(ityp) = 'CONC'
   else if (conc) then
      ityp = 1
      chidep(1,ityp) = 'AVER'
      chidep(2,ityp) = 'AGE '
      chidep(3,ityp) = 'CONC'
      chidep(4,ityp) = 'ENTR'
      chidep(5,ityp) = 'ATIO'
      chidep(6,ityp) = 'N   '
      emifac(ityp) = 1.0d06
      emilbl(ityp) = 'GRAMS/SEC'
      outlbl(ityp) = 'MICROGRAMS/M**3'
      perlbl(ityp) = 'MICROGRAMS/M**3'
      outtyp(ityp) = 'CONC'
      if (depos) then
         ityp = 2
         chidep(1,ityp) = '  TO'
         chidep(2,ityp) = 'TAL '
         chidep(3,ityp) = 'DEPO'
         chidep(4,ityp) = 'SITI'
         chidep(5,ityp) = 'ON  '
         chidep(6,ityp) = '    '
         emifac(ityp) = 3600.0d0
         emilbl(ityp) = 'GRAMS/SEC'
         outlbl(ityp) = 'GRAMS/M**2'
         perlbl(ityp) = 'GRAMS/M**2'
         outtyp(ityp) = 'DEPOS'
         if (ddep) then
            ityp = 3
            chidep(1,ityp) = '    '
            chidep(2,ityp) = 'DRY '
            chidep(3,ityp) = 'DEPO'
            chidep(4,ityp) = 'SITI'
            chidep(5,ityp) = 'ON  '
            chidep(6,ityp) = '    '
            emifac(ityp) = 3600.0d0
            emilbl(ityp) = 'GRAMS/SEC'
            outlbl(ityp) = 'GRAMS/M**2'
            perlbl(ityp) = 'GRAMS/M**2'
            outtyp(ityp) = 'DDEP'
            if (wdep) then
               ityp = 4
               chidep(1,ityp) = '    '
               chidep(2,ityp) = 'WET '
               chidep(3,ityp) = 'DEPO'
               chidep(4,ityp) = 'SITI'
               chidep(5,ityp) = 'ON  '
               chidep(6,ityp) = '    '
               emifac(ityp) = 3600.0d0
               emilbl(ityp) = 'GRAMS/SEC'
               outlbl(ityp) = 'GRAMS/M**2'
               perlbl(ityp) = 'GRAMS/M**2'
               outtyp(ityp) = 'WDEP'
            end if
         else if (wdep) then
            ityp = 3
            chidep(1,ityp) = '    '
            chidep(2,ityp) = 'WET '
            chidep(3,ityp) = 'DEPO'
            chidep(4,ityp) = 'SITI'
            chidep(5,ityp) = 'ON  '
            chidep(6,ityp) = '    '
            emifac(ityp) = 3600.0d0
            emilbl(ityp) = 'GRAMS/SEC'
            outlbl(ityp) = 'GRAMS/M**2'
            perlbl(ityp) = 'GRAMS/M**2'
            outtyp(ityp) = 'WDEP'
         end if
      else if (ddep) then
         ityp = 2
         chidep(1,ityp) = '    '
         chidep(2,ityp) = 'DRY '
         chidep(3,ityp) = 'DEPO'
         chidep(4,ityp) = 'SITI'
         chidep(5,ityp) = 'ON  '
         chidep(6,ityp) = '    '
         emifac(ityp) = 3600.0d0
         emilbl(ityp) = 'GRAMS/SEC'
         outlbl(ityp) = 'GRAMS/M**2'
         perlbl(ityp) = 'GRAMS/M**2'
         outtyp(ityp) = 'DDEP'
         if (wdep) then
            ityp = 3
            chidep(1,ityp) = '    '
            chidep(2,ityp) = 'WET '
            chidep(3,ityp) = 'DEPO'
            chidep(4,ityp) = 'SITI'
            chidep(5,ityp) = 'ON  '
            chidep(6,ityp) = '    '
            emifac(ityp) = 3600.0d0
            emilbl(ityp) = 'GRAMS/SEC'
            outlbl(ityp) = 'GRAMS/M**2'
            perlbl(ityp) = 'GRAMS/M**2'
            outtyp(ityp) = 'WDEP'
         end if
      else if (wdep) then
         ityp = 2
         chidep(1,ityp) = '    '
         chidep(2,ityp) = 'WET '
         chidep(3,ityp) = 'DEPO'
         chidep(4,ityp) = 'SITI'
         chidep(5,ityp) = 'ON  '
         chidep(6,ityp) = '    '
         emifac(ityp) = 3600.0d0
         emilbl(ityp) = 'GRAMS/SEC'
         outlbl(ityp) = 'GRAMS/M**2'
         perlbl(ityp) = 'GRAMS/M**2'
         outtyp(ityp) = 'WDEP'
      end if
   else if (depos) then
      ityp = 1
      chidep(1,ityp) = '  TO'
      chidep(2,ityp) = 'TAL '
      chidep(3,ityp) = 'DEPO'
      chidep(4,ityp) = 'SITI'
      chidep(5,ityp) = 'ON  '
      chidep(6,ityp) = '    '
      emifac(ityp) = 3600.0d0
      emilbl(ityp) = 'GRAMS/SEC'
      outlbl(ityp) = 'GRAMS/M**2'
      perlbl(ityp) = 'GRAMS/M**2'
      outtyp(ityp) = 'DEPOS'
      if (ddep) then
         ityp = 2
         chidep(1,ityp) = '    '
         chidep(2,ityp) = 'DRY '
         chidep(3,ityp) = 'DEPO'
         chidep(4,ityp) = 'SITI'
         chidep(5,ityp) = 'ON  '
         chidep(6,ityp) = '    '
         emifac(ityp) = 3600.0d0
         emilbl(ityp) = 'GRAMS/SEC'
         outlbl(ityp) = 'GRAMS/M**2'
         perlbl(ityp) = 'GRAMS/M**2'
         outtyp(ityp) = 'DDEP'
         if (wdep) then
            ityp = 3
            chidep(1,ityp) = '    '
            chidep(2,ityp) = 'WET '
            chidep(3,ityp) = 'DEPO'
            chidep(4,ityp) = 'SITI'
            chidep(5,ityp) = 'ON  '
            chidep(6,ityp) = '    '
            emifac(ityp) = 3600.0d0
            emilbl(ityp) = 'GRAMS/SEC'
            outlbl(ityp) = 'GRAMS/M**2'
            perlbl(ityp) = 'GRAMS/M**2'
            outtyp(ityp) = 'WDEP'
         end if
      else if (wdep) then
         ityp = 2
         chidep(1,ityp) = '    '
         chidep(2,ityp) = 'WET '
         chidep(3,ityp) = 'DEPO'
         chidep(4,ityp) = 'SITI'
         chidep(5,ityp) = 'ON  '
         chidep(6,ityp) = '    '
         emifac(ityp) = 3600.0d0
         emilbl(ityp) = 'GRAMS/SEC'
         outlbl(ityp) = 'GRAMS/M**2'
         perlbl(ityp) = 'GRAMS/M**2'
         outtyp(ityp) = 'WDEP'
      end if
   else if (ddep) then
      ityp = 1
      chidep(1,ityp) = '    '
      chidep(2,ityp) = 'DRY '
      chidep(3,ityp) = 'DEPO'
      chidep(4,ityp) = 'SITI'
      chidep(5,ityp) = 'ON  '
      chidep(6,ityp) = '    '
      emifac(ityp) = 3600.0d0
      emilbl(ityp) = 'GRAMS/SEC'
      outlbl(ityp) = 'GRAMS/M**2'
      perlbl(ityp) = 'GRAMS/M**2'
      outtyp(ityp) = 'DDEP'
      if (wdep) then
         ityp = 2
         chidep(1,ityp) = '    '
         chidep(2,ityp) = 'WET '
         chidep(3,ityp) = 'DEPO'
         chidep(4,ityp) = 'SITI'
         chidep(5,ityp) = 'ON  '
         chidep(6,ityp) = '    '
         emifac(ityp) = 3600.0d0
         emilbl(ityp) = 'GRAMS/SEC'
         outlbl(ityp) = 'GRAMS/M**2'
         perlbl(ityp) = 'GRAMS/M**2'
         outtyp(ityp) = 'WDEP'
      end if
   else if (wdep) then
      ityp = 1
      chidep(1,ityp) = '    '
      chidep(2,ityp) = 'WET '
      chidep(3,ityp) = 'DEPO'
      chidep(4,ityp) = 'SITI'
      chidep(5,ityp) = 'ON  '
      chidep(6,ityp) = '    '
      emifac(ityp) = 3600.0d0
      emilbl(ityp) = 'GRAMS/SEC'
      outlbl(ityp) = 'GRAMS/M**2'
      perlbl(ityp) = 'GRAMS/M**2'
      outtyp(ityp) = 'WDEP'
   end if

   emicon = 1.0d+06

! --- Modify PLTFRM, PSTFRM and MXDFRM if needed for more than one output type
!     and for EXP format (note that FILE_FORMAT is set during PRESET).

   if (numtyp > 1 .and. file_format == 'FIX') then
      if (pm25ave .or. no2ave .or. so2ave) then
         write(pltfrm,1009) numtyp+2
1009     format('(',i1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,',&
         &'A8,2X,10(F13.5,2X,I8.8,2X:))')
      else
         write(pltfrm,1019) numtyp+2
1019     format('(',i1,'(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,',&
         &'A8,2X,I8)')
      end if
      write(pstfrm,1029) numtyp+2
1029  format('(',i1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,',&
      &'A8)')
      write(mxdfrm,1039) numtyp+2
1039  format('(',i1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,',&
      &'2X,I8.8,2X,A8)')
   else if (numtyp > 1 .and. file_format == 'EXP') then
      if (pm25ave .or. no2ave .or. so2ave) then
         write(pltfrm,2009) numtyp
2009     format('(2(1X,F13.5),',i1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,',&
         &'A8,2X,A5,5X,A8,2X,10(E13.6,2X,I8.8,2X:))')
      else
         write(pltfrm,2019) numtyp
2019     format('(2(1X,F13.5),',i1,'(1X,E13.6),3(1X,F8.2),3X,A5,2X,',&
         &'A8,2X,A5,5X,A8,2X,I8)')
      end if
      write(pstfrm,2029) numtyp
2029  format('(2(1X,F13.5),',i1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',&
      &'2X,I8.8,2X,A8)')
      write(mxdfrm,2039) numtyp
2039  format('(2(1X,F13.5),',i1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',&
      &'2X,I4,2X,I3,2X,I8.8,2X,A8)')
   end if

   return
end subroutine modopt


subroutine avetim
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k
   real    :: avenum
   character (len = 8) :: kopt

!     Variable Initializations
   modnam = 'AVETIM'

!     Check for No Parameters
   if (ifc < 3) then
!        WRITE Error Message     ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

!     First Check for Presence of PERIOD or ANNUAL Switch
   do 10 i = 3, ifc
      kopt = field(i)
      if (kopt == 'PERIOD') then
         period = .true.
      else if (kopt == 'ANNUAL') then
         annual = .true.
      end if
10 continue

! --- Check for Both PERIOD and ANNUAL
   if (period .and. annual) then
!        Write Error Message; both PERIOD and ANNUAL specified
      call errhdl(path,modnam,'E','294',keywrd)
   else if (period .or. annual) then
!        Check for Too Many Averaging Periods
      if (ifc > nave+3) then
!           WRITE Error Message: Too Many Period Or Time Fields
         call errhdl(path,modnam,'E','202',keywrd)
      end if
   else
      if (ifc > nave+2) then
!           WRITE Error Message: Too Many Period Or Time Fields
         call errhdl(path,modnam,'E','202',keywrd)
      end if
   end if

!     Loop Through Fields Again, Filling KAVE Array for Short Term Averages
   j = 0
   do 20 i = 3, ifc
      kopt = field(i)
      if (kopt /= 'PERIOD' .and. kopt /= 'ANNUAL') then
         if (kopt /= 'MONTH') then
            call stonum(kopt,8,avenum,imit)
            if (imit /= 1) then
!                 Write Error Message:Invalid Numerical Field
               call errhdl(path,modnam,'E','208',keywrd)
            end if
!              Check for Valid Averaging Period
            if ((mod(24,nint(avenum))==0 .and.&
            &imit==1)) then
               j = j + 1
               if (j <= nave) then
                  kave(j) = nint(avenum)
                  write(chrave(j),'(I2,"-HR")') kave(j)
                  numave = j
!                    Check for Duplicate Averaging Periods
                  do 15 k = j-1, 1, -1
                     if (kave(j) == kave(k)) then
!                          WRITE Error Message    ! Duplicate Averaging Period
                        call errhdl(path,modnam,'E','211',keywrd)
                     end if
15                continue
               else
!                    WRITE Error Message   ! Too Many Short Term Averaging Periods
!                    This shouldn't occur since limits are dynamically allocated
                  write(dummy,'(''NAVE='',I7)') nave
                  call errhdl(path,modnam,'E','290',dummy)
               end if
            else
!                 WRITE Error Message      ! Invalid Averaging Period
               call errhdl(path,modnam,'E','203','AVEPER')
            end if
         else
            j = j + 1
            if (j <= nave) then
               kave(j) = 720
               month = .true.
               chrave(j) = 'MONTH'
               numave = j
!                 Check for Duplicate Averaging Periods
               do k = j-1, 1, -1
                  if (kave(j) == kave(k)) then
!                       WRITE Error Message    ! Duplicate Averaging Period
                     call errhdl(path,modnam,'E','211',keywrd)
                  end if
               end do
            else
!                 WRITE Error Message   ! Too Many Short Term Averaging Periods
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''NAVE='',I7)') nave
               call errhdl(path,modnam,'E','290',dummy)
            end if
         end if
      end if
20 continue

   return
end subroutine avetim

subroutine pollid
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'POLLID'

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Assign user input to POLLUT variable
      pollut = field(3)
   else if (ifc == 4) then
! ---    Check for POLLUT IDs associated with "special" processing;
!        NO2 or SO2 or PM25 (including all PM25 variants)
      if (field(3) == 'NO2'  .or. field(3) == 'SO2' .or.&
      &field(3) == 'PM25' .or. field(3) == 'PM-2.5' .or.&
      &field(3) == 'PM-25'.or. field(3) == 'PM2.5') then
! ---       This POLLID allows optional input in field 4; assign POLLUT
         pollut = field(3)
      else
! ---       User-specified POLLID doesn't allow for field 4; assign
!           POLLUT but issue an error message
         pollut = field(3)
         if (field(4) == 'H1H' .or. field(4) == 'H2H' .or.&
         &field(4) == 'INC') then
!              Error Message: 'H1H', 'H2H', and 'INC' processing not
!              applicable to this POLLUT
            write(dummy,'(A,1X,A3)') pollut(1:len_trim(pollut)),&
            &field(4)(1:3)
            call errhdl(path,modnam,'E','277',dummy)
! ---          Save FIELD(4) to include in summary of input options
            no2_field4 = field(4)(1:3)
            go to 999
         else
!              Error Message: Too Many Parameters
            call errhdl(path,modnam,'E','202',keywrd)
            go to 999
         end if
      end if
!        Now check for options to disable "special" processing for
!        these pollutants
      if (field(4) == 'H1H' .or. field(4) == 'H2H' .or.&
      &field(4) == 'INC') then
         if (pollut == 'NO2') then
            l_no_no2ave = .true.
            no2_field4 = field(4)(1:3)
!              Issue Warning Message: user disabled special processing
            write(dummy,'(A,1X,A3)') pollut(1:len_trim(pollut)),&
            &field(4)(1:3)
            call errhdl(path,modnam,'W','276',dummy)
         else if (pollut == 'SO2') then
            l_no_so2ave = .true.
            so2_field4 = field(4)(1:3)
            write(dummy,'(A,1X,A3)') pollut(1:len_trim(pollut)),&
            &field(4)(1:3)
!              Issue Warning Message: user disabled special processing
            call errhdl(path,modnam,'W','276',dummy)
         else if (pollut == 'PM25' .or. pollut == 'PM-2.5' .or.&
         &pollut == 'PM-25'.or. pollut == 'PM2.5') then
            l_no_pm25ave = .true.
            pm25_field4 = field(4)(1:3)
            write(dummy,'(A,1X,A3)') pollut(1:len_trim(pollut)),&
            &field(4)(1:3)
!              Issue Warning Message: user disabled special processing
            call errhdl(path,modnam,'W','276',dummy)
         else
!              Error Message: 'H1H', 'H2H', and 'INC' processing not
!              applicable to this POLLUT
            write(dummy,'(A,1X,A3)') pollut(1:len_trim(pollut)),&
            &field(4)(1:3)
            call errhdl(path,modnam,'E','277',dummy)
            go to 999
         end if
      end if
   else
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   pollut = field(3)

999 return
end subroutine pollid

subroutine edecay
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EDECAY'

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

!     Start To Get Decay Coef.
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

   if (keywrd == 'HALFLIFE') then
      haflif = dnum
!        Calculate Decay Coef. by Halflife
      decoef = 0.693d0/haflif
   else if (keywrd == 'DCAYCOEF') then
      decoef = dnum
   end if

! --- Check for Urban Regulatory Default for SO2; use L_PRESET_URBAN rather then
!     URBAN to allow flexibility in order of keywords
   if (dfault .and. l_preset_urban .and. pollut=='SO2') then
      if (decoef /= 4.81d-5) then
!           WRITE Warning Message: Attempt to Override Regulatory Default
         call errhdl(path,modnam,'W','206','DCAYCOEF')
      end if
      decoef = 4.81d-5
   else if (dfault) then
      if (decoef /= 0.0d0) then
!           WRITE Warning Message: Attempt to Override Regulatory Default
         call errhdl(path,modnam,'W','206','DCAYCOEF')
      end if
      decoef = 0.0d0
   else if (.not. dfault .and. decoef /= 0.0d0) then
!        Set flag for use of non-DEFAULT option
      L_NonDFAULT = .true.
   end if

999 return
end subroutine edecay

subroutine runnot
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'RUNNOT'

   if (ifc == 3) then
      if (field(3) == 'RUN') then
         run = .true.
      else if (field(3) == 'NOT') then
         run = .false.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',keywrd)
      end if
   else if (ifc > 3) then
!        WRITE Error Message     ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message     ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine runnot

subroutine flagdf
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: zflg
! Unused:      INTEGER :: I

!     Variable Initializations
   modnam = 'FLAGDF'
   flgpol = .true.

   if (ifc == 3) then
      call stodbl(field(3),ilen_fld,zflg,imit)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
      end if
      if (zflg >= 0.0d0 .and. imit == 1) then
         azflag(:) = zflg
      else if (zflg < 0.0d0 .and. imit == 1) then
!            WRITE Error Message: Invalid Data. Negative value specified
         call errhdl(path,modnam,'E','209','ZFLAG')
      else
!            WRITE Error Message: Invalid Parameter
         call errhdl(path,modnam,'E','203',keywrd)
      end if
   else if (ifc > 3) then
!        WRITE Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message: No Parameters
      call errhdl(path,modnam,'W','205','ZFLAG=0.')
   end if

   return
end subroutine flagdf

subroutine evntfl
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EVNTFL'

   if (ifc == 3) then
      events = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         evfile = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  EVFILE Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
      evparm = 'DETAIL'
   else if (ifc == 4) then
      events  = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         evfile = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  EVFILE Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
      evparm = field(4)
   else if (ifc > 4) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Warning Message         ! No Parameters - Use Default Name
      call errhdl(path,modnam,'W','207',keywrd)
      events = .true.
      evfile = 'EVENTS.INP'
      evparm = 'DETAIL'
   end if

!     Check for Invalid EVPARM
   if (evparm /= 'SOCONT' .and. evparm /= 'DETAIL') then
!        WRITE Warning Message         ! Invalid Parameter - Use Default
      call errhdl(path,modnam,'W','203','EVPARM')
   end if

!     Open The EVENT Input File
   open(unit=ievunt,file=evfile,status='REPLACE',&
   &form='FORMATTED')

   return
end subroutine evntfl

subroutine savefl
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SAVEFL'

   if (multyr) then
!        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
      call errhdl(path,modnam,'E','150',keywrd)
   else if (ifc == 3) then
      rstsav = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         savfil = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  SAVFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
      savfl2 = savfil
      incrst = 1
   else if (ifc == 4) then
      rstsav = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         savfil = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  SAVFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
      savfl2 = savfil
      call stonum(field(4),ilen_fld,fnum,imit)
      incrst = nint(fnum)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
      end if
   else if (ifc == 5) then
      rstsav = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         savfil = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  SAVFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
      call stonum(field(4),ilen_fld,fnum,imit)
      incrst = nint(fnum)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
      end if
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(5)-locb(5)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         savfl2 = runst1(locb(5):loce(5))
      else
!           WRITE Error Message:  SAVFL2 Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
   else if (ifc > 5) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Warning Message          ! No Parameters - Use Default Name
      call errhdl(path,modnam,'W','207',keywrd)
      rstsav = .true.
      savfil = 'SAVE.FIL'
      savfl2 = savfil
      incrst = 1
   end if

   return
end subroutine savefl

subroutine initfl
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'INITFL'

   if (multyr) then
!        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
      call errhdl(path,modnam,'E','150',keywrd)
   else if (ifc == 3) then
      rstinp = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         inifil = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  INIFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
   else if (ifc > 3) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Warning Message          ! No Parameters - Use Default Name
      call errhdl(path,modnam,'W','207',keywrd)
      rstinp = .true.
      inifil = 'SAVE.FIL'
   end if

   return
end subroutine initfl

subroutine errfil
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'ERRFIL'

   if (ifc == 3) then
      errlst = .true.
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         msgfil = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  MSGFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if
   else if (ifc > 3) then
!*       WRITE Error Message                ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!*       WRITE Warning Message              ! No Parameters - Use Default Name
      call errhdl(path,modnam,'W','207',keywrd)
      errlst = .true.
      msgfil = 'ERRORS.LST'
   end if
!*#

   return
end subroutine errfil

subroutine debopt
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
   use main1
   use rline_data, only: nrlines
!CRCO D095 - this has been added to bring NBLP into the logic. Should
! we move NBLP out of BUOYANT_LINE and into MAIN1 instead in modules.f?
   use buoyant_line
   implicit none
   character :: modnam*12, kopt*8
!CRT  D063 Add Platform Downwash variable IPLATFM and initialize below
!CRT  D113 Add Sidewash source variable ISW and initialize below
   integer :: i, imod, imet, iarea, iprm, ipvm, iolmd, iarm2, idep,&
   &igrsm, nopts, MAXFields, iprm2, ittrmd, ittrm2,&
   &iurbd, iblp, iplatfm, irline, isw,&
   &iarcft,&                          ! Added for Aircraft; UNC-IE
   &ihbp ! Added for HBPDEBUG; JAN 2023

!     Variable Initializations
   modnam = 'DEBOPT'
!     Initialize counters for number of debug options and field number
!     associated with debugopts
   imod  = 0
   imet  = 0
   iarea = 0
   iprm  = 0
   ipvm  = 0
   iolmd = 0
   iarm2  = 0
   idep  = 0
   igrsm = 0
   iprm2 = 0
   iplatfm = 0
   nopts = 0
   MAXFields = 0
   ittrmd = 0
   ittrm2 = 0
   irline = 0
   iurbd = 0
   iblp = 0
   isw = 0
   iarcft = 0                                 ! Added for Aircraft; UNC-IE
   ihbp = 0 ! Added for HBP, JAN 2023

!     Check for Too Few or Too Many Parameters
   if (ifc < 3) then
!        WRITE Error Message     ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   else if (ifc > 13) then
!        WRITE Warning Message   ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   end if

! --- First Check for Presence of Debug Switches;
!     also save position to interpret optional
!     filenames
   do i = 3, ifc
      kopt = field(i)
      if (kopt == 'MODEL') then
         debug = .true.
         nopts = nopts + 1
         imod = i
      else if (kopt == 'METEOR') then
         meteordbg = .true.
         nopts = nopts + 1
         imet = i
      else if (kopt == 'AREA') then
! ---       Check to see if AREADBG option has already been assigned .T.;
!           user may have entered both AREA and LINE
         if (.not. areadbg) then
! ---          AREADBG option not already = .T.; assign all variables
            areadbg = .true.
            nopts = nopts + 1
            iarea = i
         else
! ---          AREADBG already assigned = .T.; user may have entered
!              both AREA and LINE options; issue ERROR message
            call errhdl(path,modnam,'E','194','AREADEBUG')
            areadbg = .false.
         end if
      else if (kopt == 'LINE') then
! ---       Check to see if AREADBG option has already been assigned .T.;
!           user may have entered both AREA and LINE
         if (.not. areadbg) then
! ---          AREADBG option not already = .T.; assign all variables
            areadbg = .true.
            nopts = nopts + 1
            iarea = i
         else
! ---          AREADBG already assigned = .T.; user may have entered
!              both AREA and LINE options; issue ERROR message
            call errhdl(path,modnam,'E','194','LINEDEBUG')
            areadbg = .false.
         end if
      else if (kopt == 'PRIME') then
         primedbg = .true.
         nopts = nopts + 1
         iprm = i
      else if (kopt == 'PVMRM') then
         pvmrmdbg = .true.
         nopts = nopts + 1
         ipvm = i
      else if (kopt == 'OLM') then
         olmdebug = .true.
         nopts = nopts + 1
         iolmd = i
      else if (kopt == 'ARM2') then
         arm2debug = .true.
         nopts = nopts + 1
         iarm2 = i
      else if (kopt == 'GRSM') then
         grsmdebug = .true.
         nopts = nopts + 1
         igrsm = i
      else if (kopt == 'DEPOS') then
         deposdbg = .true.
         nopts = nopts + 1
         idep = i
!CRT     D063 Platform Downwash Debug
      else if (kopt == 'PLATFORM') then
         platfmdbg = .true.
         nopts = nopts + 1
         iplatfm = i
      else if (kopt == 'AWMADW') then
         awmadwdbg   = .true.
         nopts = nopts + 1
         iprm2 = i
      else if (kopt == 'RLINE') then
         rlinedbg   = .true.
         nopts = nopts + 1
         irline = i
      else if (kopt == 'TTRM') then
         ttrmdbg = .true.
         nopts = nopts + 1
         ittrmd = i
      else if (kopt == 'TTRM2') then
         ttrm2dbg = .true.
         nopts = nopts + 1
         ittrm2 = i
      else if (kopt == 'URBANDB') then
         urbdbug = .true.
         nopts = nopts + 1
         iurbd = i
      else if (kopt == 'BLPDBUG') then
         blpdbug = .true.
         nopts = nopts + 1
         iblp = i
      else if (kopt == 'SWPOINT') then
         swdbg = .true.
         nopts = nopts + 1
         isw = i

!** Added for Aircraft Plume Rise; UNC-IE
      else if (kopt == 'AIRCRAFT') then
         arcftdebug = .true.
         nopts = nopts + 1
         iarcft = i
!** End Aircraft Plume Rise insert; April 2023

! Added for HBP JAN 2023
      else if (kopt == 'HBPDBG') then
         hbpdbg = .true.
         nopts = nopts + 1
         ihbp = i
! End HBP add

      end if
   end do

! --- Determine maximum number of fields allowed based on number of
!     options specified, assuming that user has specified filename
!     for each option (except for DEPOS).
   if (nopts > 0) then
      if (.not.deposdbg) then
         MAXFields = 2 + nopts*2
      else
         MAXFields = 2 + (nopts-1)*2 + 1
      end if
   else
!        No recognizable debug options specified, issue fatal error
      write(dummy,'(A:)') field(3)(1:min(12,len_trim(field(3))))
      call errhdl(path,modnam,'E','203',dummy)
      go to 999
   end if

! --- Check for debug options without associated model option being used
   if (pvmrmdbg .and. .not. pvmrm) then
!        Write Error Message:  PVMRM debug without PVMRM option
      call errhdl(path,modnam,'E','194','PVMRMDBG')
   end if
   if (olmdebug .and. .not.olm) then
!        Write Error Message:  OLM debug without OLM option
      call errhdl(path,modnam,'E','194','OLMDEBUG')
   end if
   if (arm2debug .and. .not.arm2) then
!        Write Error Message:  ARM2 debug without ARM2 option
      call errhdl(path,modnam,'E','194','ARM2DEBUG')
   end if
   if (grsmdebug .and. .not.grsm) then
!        Write Error Message:  GRSM debug without GRSM option
      call errhdl(path,modnam,'E','194','GRSMDEBUG')
   end if
   if (ttrmdbg .and. .not. runttrm) then
!        Write Error Message:  TTRM debug without TTRM option
      call errhdl(path,modnam,'E','194','TTRMDEBUG')
   end if
   if (ttrm2dbg .and. .not. runttrm2) then
!        Write Error Message:  TTRM2 debug without TTRM2 option
      call errhdl(path,modnam,'E','194','TTRM2DEBUG')
   end if
! Added for HBP, added Jan 2023
   if (hbpdbg .and. .not. hbplume) then
!        Write Error Message:  HBPDBG debug without HBP option
      call errhdl(path,modnam,'E','194','HBPDBG')
   endif
! End HBP add
   if (deposdbg .and. .not.depos .and. .not.ddep .and.&
   &.not.wdep) then
!        Write Error Message:  DEPOS debug without deposition options
      call errhdl(path,modnam,'E','194','DEPOSDBG')
   end if
   if (areadbg .and. narea==0 .and. ncirc==0 .and. nline==0&
   &.and. npit==0) then
!        Write Error Message:  AREA/LINE debug without any applicable
!        sources
      if (field(iarea) == 'AREA') then
         call errhdl(path,modnam,'E','194','AREADEBUG')
      else if (field(iarea) == 'LINE') then
         call errhdl(path,modnam,'E','194','LINEDEBUG')
      end if
   end if
!CRCO D095 Added for urban debug 8/3/2021
   if (nurb == 0 .and. urbdbug) then
!        Write Error Message:  URBDBUG debug without applicable sources
      call errhdl(path,modnam,'E','194','URBANDB')
   end if
!C END URBDBUG insert
   if (primedbg .and. nsec==0) then
!        Write Error Message:  PRIME debug without any applicable sources
      call errhdl(path,modnam,'E','194','PRIMEDBG')
   end if

   if (awmadwdbg .and. nsec==0) then
!        Write Error Message:  AWMADW debug without any applicable sources
      call errhdl(path,modnam,'E','194','AWMADWDBG')
   end if
!CRCO D095 added for BLP debug 12/9/2021
   if (nblp == 0 .and. blpdbug) then
!        Write Error Message:  BLPDBUG debug without applicable sources
      call errhdl(path,modnam,'E','194','BLPDBUG')
   end if

   if (rlinedbg .and. nrlines==0) then
!        Write Error Message:  RLINE debug without any applicable sources
      call errhdl(path,modnam,'E','194','RLINEDBG')
   end if

!CRT     D063 Platform Downwash Debug
   if (platfmdbg .and. npnt==0) then
!        Write Error Message:  PLATFORM debug without any applicable sources
      call errhdl(path,modnam,'E','194','PLATFMDBG')
   end if

!     CRT 3/25/02022 D113 Added for sidewash debug
   if (swdbg .and. nswp == 0) then
!        Write Error Message:  SWDBG debug without any applicable sources
      call errhdl(path,modnam,'E','194','SWDBG')
   end if

!** Added for Aircraft Plume Rise; UNC-IE
!MGS Changed conditional to be consistant with debug conditionals above.
!MSG (D151 - WSP 6/1/2023)
!MGS      IF (ARCFTDEBUG .and. .NOT. NAFTSRC .GT. 0.0D0) THEN
   if (arcftdebug .and. naftsrc == 0) then
!        Write Error Message:  ARCFT debug without Aircraft option
      call errhdl(path,modnam,'E','194','ARCFTDEBUG')
   end if
!** End Aircraft Plume Rise insert; April 2023

! --- Check for user-specified filenames, which should immediately
!     follow the keyword option in the input file
   if (debug) then
      if (ifc >= imod+1 .and.&
      &field(imod+1) /= 'METEOR' .and.&
      &field(imod+1) /= 'AREA' .and.&
      &field(imod+1) /= 'LINE' .and.&
      &field(imod+1) /= 'RLINE' .and.&
      &field(imod+1) /= 'PRIME' .and.&
      &field(imod+1) /= 'PVMRM' .and.&
      &field(imod+1) /= 'OLM' .and.&
      &field(imod+1) /= 'ARM2' .and.&
      &field(imod+1) /= 'GRSM' .and.&
      &field(imod+1) /= 'DEPOS'.and.&
      &field(imod+1) /= 'AWMADW' .and.&
      &field(imod+1) /= 'TTRM' .and.&
      &field(imod+1) /= 'TTRM2' .and.&
      &field(imod+1) /= 'PLATFORM' .and.&
      &field(imod+1) /= 'URBANDB' .and.&
      &field(imod+1) /= 'BLPDBUG' .and.&
      &field(imod+1) /= 'SWPOINT' .and.&
      &field(imod+1) /= 'AIRCRAFT' .and.&
      &field(imod+1) /= 'HBPDBG' ) then

! ---       Assign user-specified filename for the MODEL debug option
         dbgfil = runst1(locb(imod+1):loce(imod+1))
      else
! ---       Assign default MODEL debug filename
         dbgfil = 'MODEL.DBG'
      end if
   end if

   if (meteordbg) then
      if (ifc >= imet+1 .and.&
      &field(imet+1) /= 'MODEL' .and.&
      &field(imet+1) /= 'AREA' .and.&
      &field(imet+1) /= 'LINE' .and.&
      &field(imet+1) /= 'RLINE' .and.&
      &field(imet+1) /= 'PRIME' .and.&
      &field(imet+1) /= 'PVMRM' .and.&
      &field(imet+1) /= 'OLM' .and.&
      &field(imet+1) /= 'ARM2' .and.&
      &field(imet+1) /= 'GRSM' .and.&
      &field(imet+1) /= 'DEPOS' .and.&
      &field(imet+1) /= 'AWMADW' .and.&
      &field(imet+1) /= 'TTRM' .and.&
      &field(imet+1) /= 'TTRM2' .and.&
      &field(imet+1) /= 'PLATFORM' .and.&
      &field(imet+1) /= 'URBANDB' .and.&
      &field(imet+1) /= 'BLPDBUG' .and.&
      &field(imet+1) /= 'SWPOINT' .and.&
      &field(imet+1) /= 'AIRCRAFT' .and.&
      &field(imet+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the METEOR debug option
         dbmfil = runst1(locb(imet+1):loce(imet+1))
      else
! ---       Assign default METEOR debug filename
         dbmfil = 'METEOR.DBG'
      end if
   end if

   if (areadbg) then
      if (ifc >= iarea+1 .and.&
      &field(iarea+1) /= 'MODEL' .and.&
      &field(iarea+1) /= 'METEOR' .and.&
      &field(iarea+1) /= 'RLINE' .and.&
      &field(iarea+1) /= 'PRIME' .and.&
      &field(iarea+1) /= 'PVMRM' .and.&
      &field(iarea+1) /= 'OLM' .and.&
      &field(iarea+1) /= 'ARM2' .and.&
      &field(iarea+1) /= 'GRSM' .and.&
      &field(iarea+1) /= 'DEPOS' .and.&
      &field(iarea+1) /= 'AWMADW' .and.&
      &field(iarea+1) /= 'TTRM' .and.&
      &field(iarea+1) /= 'TTRM2' .and.&
      &field(iarea+1) /= 'PLATFORM' .and.&
      &field(iarea+1) /= 'URBANDB' .and.&
      &field(iarea+1) /= 'BLPDBUG' .and.&
      &field(iarea+1) /= 'SWPOINT'.and.&
      &field(iarea+1) /= 'AIRCRAFT' .and.&
      &field(iarea+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the AREA debug option
         dbareafil = runst1(locb(iarea+1):loce(iarea+1))
      else
! ---       Assign default AREA debug filename
         dbareafil = 'AREA.DBG'
      end if
   end if

   if (primedbg) then
      if (ifc >= iprm+1 .and.&
      &field(iprm+1) /= 'MODEL' .and.&
      &field(iprm+1) /= 'METEOR' .and.&
      &field(iprm+1) /= 'AREA' .and.&
      &field(iprm+1) /= 'LINE' .and.&
      &field(iprm+1) /= 'RLINE' .and.&
      &field(iprm+1) /= 'PVMRM' .and.&
      &field(iprm+1) /= 'OLM' .and.&
      &field(iprm+1) /= 'ARM2' .and.&
      &field(iprm+1) /= 'GRSM' .and.&
      &field(iprm+1) /= 'DEPOS' .and.&
      &field(iprm+1) /= 'AWMADW' .and.&
      &field(iprm+1) /= 'TTRM' .and.&
      &field(iprm+1) /= 'TTRM2' .and.&
      &field(iprm+1) /= 'PLATFORM' .and.&
      &field(iprm+1) /= 'URBANDB' .and.&
      &field(iprm+1) /= 'BLPDBUG' .and.&
      &field(iprm+1) /= 'SWPOINT' .and.&
      &field(iprm+1) /= 'AIRCRAFT' .and.&
      &field(iprm+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the PRIME debug option
         dbprmfil = runst1(locb(iprm+1):loce(iprm+1))
      else
! ---       Assign default PRIME debug filename
         dbprmfil = 'PRIME.DBG'
      end if
   end if

   if (pvmrmdbg) then
      if (ifc >= ipvm+1 .and.&
      &field(ipvm+1) /= 'MODEL' .and.&
      &field(ipvm+1) /= 'METEOR' .and.&
      &field(ipvm+1) /= 'AREA' .and.&
      &field(ipvm+1) /= 'LINE' .and.&
      &field(ipvm+1) /= 'RLINE' .and.&
      &field(ipvm+1) /= 'PRIME' .and.&
      &field(ipvm+1) /= 'OLM' .and.&
      &field(ipvm+1) /= 'ARM2' .and.&
      &field(ipvm+1) /= 'GRSM' .and.&
      &field(ipvm+1) /= 'DEPOS' .and.&
      &field(ipvm+1) /= 'AWMADW' .and.&
      &field(ipvm+1) /= 'TTRM' .and.&
      &field(ipvm+1) /= 'TTRM2'.and.&
      &field(ipvm+1) /= 'PLATFORM' .and.&
      &field(ipvm+1) /= 'URBANDB' .and.&
      &field(ipvm+1) /= 'BLPDBUG' .and.&
      &field(ipvm+1) /= 'SWPOINT'.and.&
      &field(ipvm+1) /= 'AIRCRAFT' .and.&
      &field(ipvm+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the PVMRM debug option
         dbpvfil = runst1(locb(ipvm+1):loce(ipvm+1))
      else
! ---       Assign default PVMRM debug filename
         if (pvmrm) then
            dbpvfil = 'PVMRM.DBG'
         end if
      end if
! ---    Assign default filename for RELDISP debug file for PVMRM option
      rdispfil = 'RelDisp.dbg'
   end if

   if (olmdebug) then
      if (ifc >= iolmd+1 .and.&
      &field(iolmd+1) /= 'MODEL' .and.&
      &field(iolmd+1) /= 'METEOR' .and.&
      &field(iolmd+1) /= 'AREA' .and.&
      &field(iolmd+1) /= 'LINE' .and.&
      &field(iolmd+1) /= 'RLINE' .and.&
      &field(iolmd+1) /= 'PRIME' .and.&
      &field(iolmd+1) /= 'PVMRM' .and.&
      &field(iolmd+1) /= 'ARM2' .and.&
      &field(iolmd+1) /= 'GRSM' .and.&
      &field(iolmd+1) /= 'DEPOS' .and.&
      &field(iolmd+1) /= 'AWMADW' .and.&
      &field(iolmd+1) /= 'TTRM' .and.&
      &field(iolmd+1) /= 'TTRM2' .and.&
      &field(iolmd+1) /= 'PLATFORM' .and.&
      &field(iolmd+1) /= 'URBANDB' .and.&
      &field(iolmd+1) /= 'BLPDBUG' .and.&
      &field(iolmd+1) /= 'SWPOINT' .and.&
      &field(iolmd+1) /= 'AIRCRAFT' .and.&
      &field(iolmd+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the OLM debug option
         dbolmfil = runst1(locb(iolmd+1):loce(iolmd+1))
      else
! ---       Assign default OLM debug filename
         dbolmfil = 'OLM.DBG'
      end if
   end if

   if (arm2debug) then
      if (ifc >= iarm2+1 .and.&
      &field(iarm2+1) /= 'MODEL' .and.&
      &field(iarm2+1) /= 'METEOR' .and.&
      &field(iarm2+1) /= 'AREA' .and.&
      &field(iarm2+1) /= 'LINE' .and.&
      &field(iarm2+1) /= 'RLINE' .and.&
      &field(iarm2+1) /= 'PRIME' .and.&
      &field(iarm2+1) /= 'PVMRM' .and.&
      &field(iarm2+1) /= 'OLM' .and.&
      &field(iarm2+1) /= 'GRSM' .and.&
      &field(iarm2+1) /= 'DEPOS' .and.&
      &field(iarm2+1) /= 'AWMADW'.and.&
      &field(iarm2+1) /= 'TTRM' .and.&
      &field(iarm2+1) /= 'TTRM2' .and.&
      &field(iarm2+1) /= 'PLATFORM' .and.&
      &field(iarm2+1) /= 'URBANDB' .and.&
      &field(iarm2+1) /= 'BLPDBUG' .and.&
      &field(iarm2+1) /= 'SWPOINT' .and.&
      &field(iarm2+1) /= 'AIRCRAFT' .and.&
      &field(iarm2+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the ARM2 debug option
         dbarm2fil = runst1(locb(iarm2+1):loce(iarm2+1))
      else
! ---       Assign default ARM2 debug filename
         dbarm2fil = 'ARM2.DBG'
      end if
   end if

!     CERC 11/30/20 Code for determining GRSM debug file name
   if (grsmdebug) then
      if (ifc >= igrsm+1 .and.&
      &field(igrsm+1) /= 'MODEL' .and.&
      &field(igrsm+1) /= 'METEOR' .and.&
      &field(igrsm+1) /= 'AREA' .and.&
      &field(igrsm+1) /= 'LINE' .and.&
      &field(igrsm+1) /= 'RLINE' .and.&
      &field(igrsm+1) /= 'PRIME' .and.&
      &field(igrsm+1) /= 'OLM' .and.&
      &field(igrsm+1) /= 'PVMRM' .and.&
      &field(igrsm+1) /= 'ARM2' .and.&
      &field(igrsm+1) /= 'DEPOS' .and.&
      &field(igrsm+1) /= 'AWMADW' .and.&
      &field(igrsm+1) /= 'TTRM' .and.&
      &field(igrsm+1) /= 'TTRM2' .and.&
      &field(igrsm+1) /= 'PLATFORM' .and.&
      &field(igrsm+1) /= 'URBANDB' .and.&
      &field(igrsm+1) /= 'BLPDBUG' .and.&
      &field(igrsm+1) /= 'SWPOINT' .and.&
      &field(igrsm+1) /= 'AIRCRAFT' .and.&
      &field(igrsm+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the GRSM debug option
         dbgrsmfil = runst1(locb(igrsm+1):loce(igrsm+1))
      else
! ---       Assign default GRSM debug filename
         dbgrsmfil = 'GRSM.DBG'
      end if
   end if

   if (awmadwdbg) then
      if (ifc >= iprm2+1 .and.&
      &field(iprm2+1) /= 'MODEL' .and.&
      &field(iprm2+1) /= 'METEOR' .and.&
      &field(iprm2+1) /= 'AREA' .and.&
      &field(iprm2+1) /= 'LINE' .and.&
      &field(iprm2+1) /= 'RLINE' .and.&
      &field(iprm2+1) /= 'PRIME' .and.&
      &field(iprm2+1) /= 'PVMRM' .and.&
      &field(iprm2+1) /= 'OLM' .and.&
      &field(iprm2+1) /= 'ARM2' .and.&
      &field(iprm2+1) /= 'PLATFORM' .and.&
      &field(iprm2+1) /= 'GRSM' .and.&
      &field(iprm2+1) /= 'TTRM' .and.&
      &field(iprm2+1) /= 'TTRM2' .and.&
      &field(iprm2+1) /= 'DEPOS' .and.&
      &field(iprm2+1) /= 'URBANDB' .and.&
      &field(iprm2+1) /= 'BLPDBUG' .and.&
      &field(iprm2+1) /= 'SWPOINT' .and.&
      &field(iprm2+1) /= 'AIRCRAFT' .and.&
      &field(iprm2+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the PRIME debug option
         DBAwmaDwFIL = runst1(locb(iprm2+1):loce(iprm2+1))
      else
! ---       Assign default AWMADW debug filename
         DBAwmaDwFIL = 'AWMADW.DBG'
      end if
   end if

   if (rlinedbg) then
      if (ifc >= irline +1 .and.&
      &field(irline+1) /= 'MODEL' .and.&
      &field(irline+1) /= 'METEOR' .and.&
      &field(irline+1) /= 'AREA' .and.&
      &field(irline+1) /= 'LINE' .and.&
      &field(irline+1) /= 'PRIME' .and.&
      &field(irline+1) /= 'PVMRM' .and.&
      &field(irline+1) /= 'OLM' .and.&
      &field(irline+1) /= 'ARM2' .and.&
      &field(irline+1) /= 'GRSM' .and.&
      &field(irline+1) /= 'PLATFORM' .and.&
      &field(irline+1) /= 'AWMADW' .and.&
      &field(irline+1) /= 'TTRM' .and.&
      &field(irline+1) /= 'TTRM2' .and.&
      &field(irline+1) /= 'URBANDB'.and.&
      &field(irline+1) /= 'DEPOS' .and.&
      &field(irline+1) /= 'BLPDBUG' .and.&
      &field(irline+1) /= 'SWPOINT' .and.&
      &field(irline+1) /= 'AIRCRAFT' .and.&
      &field(irline+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the RLINE debug option
         rlinedbgfil = runst1(locb(irline+1):loce(irline+1))
! ---    Add filename for the RLINE gridded wind speed debug file Wood 10/10/22
         rlinedbgfil_ws = runst1(locb(irline+1):loce(irline+1)-4)//&
         &"_GRIDWS"//&
         &runst1(loce(irline+1)-3:loce(irline+1))
      else
! ---       Assign default RLINE debug filename
         rlinedbgfil = 'RLINE.DBG'
! ---    Add filename for the RLINE gridded wind speed debug file Wood 10/10/22
! ---       Assign default RLINE Grid WS debug filename
         rlinedbgfil_ws = 'RLINE_GRIDWS.DBG'
      end if
   end if

!CRT  D063 Platform Downwash Debug
   if (platfmdbg) then
      if (ifc >= iplatfm+1 .and.&
      &field(iplatfm+1) /= 'MODEL' .and.&
      &field(iplatfm+1) /= 'METEOR' .and.&
      &field(iplatfm+1) /= 'AREA' .and.&
      &field(iplatfm+1) /= 'LINE' .and.&
      &field(iplatfm+1) /= 'RLINE' .and.&
      &field(iplatfm+1) /= 'PRIME' .and.&
      &field(iplatfm+1) /= 'PVMRM' .and.&
      &field(iplatfm+1) /= 'OLM' .and.&
      &field(iplatfm+1) /= 'ARM2' .and.&
      &field(iplatfm+1) /= 'DEPOS' .and.&
      &field(iplatfm+1) /= 'GRSM' .and.&
      &field(iplatfm+1) /= 'TTRM' .and.&
      &field(iplatfm+1) /= 'TTRM2' .and.&
      &field(iplatfm+1) /= 'URBANDB'.and.&
      &field(iplatfm+1) /= 'AWMADW' .and.&
      &field(iplatfm+1) /= 'SWPOINT'.and.&
      &field(iplatfm+1) /= 'AIRCRAFT'.and.&
      &field(iplatfm+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the PRIME debug option
         platfmdbgfile = runst1(locb(iplatfm+1):loce(iplatfm+1))
      else
! ---       Assign default PLATFORM debug filename
         platfmdbgfile = 'PLATFORM.DBG'
      end if
   end if

! --- Now check for DEPOS option; since DEPOS debug filenames are
!     hardwired, issue warning if user appears to have specified
!     a filename
   if (deposdbg) then
!         JAT 05/08/2020 added from version 19191
!         wet deposition parameters are written to debug file
!         regardless if MODEL debug is chosen.  if model debug
!         not chosen, file is fort.24.  change to DEPOS.DBG
!         if dbgfil not named or next field is not MODEL
      if (trim(adjustl(dbgfil)) == '' .or. field(idep+1) /=&
      &'MODEL') dbgfil='DEPOS.DBG'
      if (ifc >= idep+1 .and.&
      &field(idep+1) /= 'MODEL' .and.&
      &field(idep+1) /= 'METEOR' .and.&
      &field(idep+1) /= 'AREA' .and.&
      &field(idep+1) /= 'LINE' .and.&
      &field(idep+1) /= 'PRIME' .and.&
      &field(idep+1) /= 'PVMRM' .and.&
      &field(idep+1) /= 'ARM2' .and.&
      &field(idep+1) /= 'OLM' .and.&
      &field(idep+1) /= 'PLATFORM' .and.&
      &field(idep+1) /= 'GRSM' .and.&
      &field(idep+1) /= 'TTRM' .and.&
      &field(idep+1) /= 'AWMADW' .and.&
      &field(idep+1) /= 'TTRM2' .and.&
      &field(idep+1) /= 'RLINE' .and.&
      &field(idep+1) /= 'URBANDB' .and.&
      &field(idep+1) /= 'BLPDBUG' .and.&
      &field(idep+1) /= 'SWPOINT' .and.&
      &field(idep+1) /= 'AIRCRAFT' .and.&
      &field(idep+1) /= 'HBPDBG') then

! ---       Write warning message regarding DEPOS debug filenames
         call errhdl(path,modnam,'W','203','DEPOSDBG')
      end if
   end if

!     Added for TTRM; AECOM
   if (ttrmdbg) then
      if (ifc >= ittrmd+1 .and.&
      &field(ittrmd+1) /= 'METEOR' .and.&
      &field(ittrmd+1) /= 'MODEL' .and.&
      &field(ittrmd+1) /= 'PRIME' .and.&
      &field(ittrmd+1) /= 'PVMRM' .and.&
      &field(ittrmd+1) /= 'OLM' .and.&
      &field(ittrmd+1) /= 'ARM2' .and.&
      &field(ittrmd+1) /= 'GRSM' .and.&
      &field(ittrmd+1) /= 'AREA' .and.&
      &field(ittrmd+1) /= 'LINE' .and.&
      &field(ittrmd+1) /= 'AWMADW' .and.&
      &field(ittrmd+1) /= 'RLINE' .and.&
      &field(ittrmd+1) /= 'PLATFORM' .and.&
      &field(ittrmd+1) /= 'DEPOS' .and.&
      &field(ittrmd+1) /= 'TTRM2' .and.&
      &field(ittrmd+1) /= 'DEPOS'.and.&
      &field(ittrmd+1) /= 'URBANDB'.and.&
      &field(ittrmd+1) /= 'BLPDBUG' .and.&
      &field(ittrmd+1) /= 'SWPOINT' .and.&
      &field(ittrmd+1) /= 'AIRCRAFT' .and.&
      &field(ittrmd+1) /= 'HBPDBG') then

!      added for TTRM; AECOM
! ---       Assign user-specified filename for the TTRM debug option
         ttrmfil = runst1(locb(ittrmd+1):loce(ittrmd+1))
      else
! ---       Assign default Ozone Reaction Rate debug filename
         ttrmfil = 'TTRM_DEBUG.DBG'
      end if
   end if
!     End TTRM insert; Feb. 2021

!     Open TTRM2 debug files; the filenames are hard-wired
   if (runttrm2 .and. ttrm2dbg) then
      open(unit=ttrm2tmp(1),file='AFTER_TTRM.DBG',&
      &err=779,status='REPLACE')
!       Assign 2nd TTRM2 debug file based on selected method
      if (arm2) then
         open(unit=ttrm2tmp(2),file='AFTER_ARM2.DBG',err=780,&
         &status='REPLACE')
      elseif (olm) then
         open(unit=ttrm2tmp(2),file='AFTER_OLM.DBG',err=780,&
         &status='REPLACE')
      else
         open(unit=ttrm2tmp(2),file='AFTER_PVMRM.DBG',err=780,&
         &status='REPLACE')
      endif
      open(unit=ttrm2tmp(3),file='TTRM2_MERGE.DBG',err=781,&
      &status='REPLACE')
!       Write headers for TTRM2 debug files
      do i = 1, 3
         write(ttrm2tmp(i),7005) versn, title1(1:68),&
         &rundat, runtim
         write(ttrm2tmp(i),7020)
      enddo
      go to 7799
7005  format('* AERMOD (',a6,'): ',a68,5x,a8,4x,a8)

7020  format('*        X             Y      AVERAGE CONC    ZELEV    ',&
      &'ZHILL    ZFLAG    AVE     GRP       DATE     SRC ID   ',&
      &/,&
      &'* ____________  ____________  ____________   ______   _',&
      &'_____   ______  ______  ________  ________  ________  ')
!        WRITE Error Message for Error Opening File
779   write(dummy,'("PSTFL",I4.4)') ttrm2tmp(1)
      call errhdl(path,modnam,'E','500',dummy)
780   write(dummy,'("PSTFL",I4.4)') ttrm2tmp(2)
      call errhdl(path,modnam,'E','500',dummy)
781   write(dummy,'("PSTFL",I4.4)') ttrm2tmp(3)
      call errhdl(path,modnam,'E','500',dummy)
7799  continue
   endif


!RCO D095 Added for urban debug 8/3/2021
   if (urbdbug) then
      if (ifc >= iurbd+1 .and.&
      &field(iurbd+1) /= 'METEOR' .and.&
      &field(iurbd+1) /= 'MODEL' .and.&
      &field(iurbd+1) /= 'AREA' .and.&
      &field(iurbd+1) /= 'LINE' .and.&
      &field(iurbd+1) /= 'PRIME' .and.&
      &field(iurbd+1) /= 'PVMRM' .and.&
      &field(iurbd+1) /= 'OLM' .and.&
      &field(iurbd+1) /= 'ARM2' .and.&
      &field(iurbd+1) /= 'GRSM' .and.&
      &field(iurbd+1) /= 'PLATFORM' .and.&
      &field(iurbd+1) /= 'DEPOS'.and.&
      &field(iurbd+1) /= 'AWMADW' .and.&
      &field(iurbd+1) /= 'TTRM' .and.&
      &field(iurbd+1) /= 'TTRM2' .and.&
      &field(iurbd+1) /= 'RLINE' .and.&
      &field(iurbd+1) /= 'BLPDBUG' .and.&
      &field(iurbd+1) /= 'SWPOINT' .and.&
      &field(iurbd+1) /= 'AIRCRAFT' .and.&
      &field(iurbd+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the URBDBUG debug option
         urbfil = runst1(locb(iurbd+1):loce(iurbd+1))
         urbfil1 = runst1(locb(iurbd+1):loce(iurbd+1)) // "1"
!RCO - D168 Debug files. Add output file for second urban debug
         urbfil2 = runst1(locb(iurbd+1):loce(iurbd+1)) // "2"
      else
! ---       Assign default URBDBUG debug filename
         urbfil = 'URBDBUG.DBG'
         urbfil1 = 'URBDBUG1.DBG'
         urbfil2 = 'URBDBUG2.DBG'
      end if
   end if
! End URBDBUG insert

!     CRCO D095 BLP Debug
   if (blpdbug) then
      if (ifc >= iblp+1 .and.&
      &field(iblp+1) /= 'METEOR' .and.&
      &field(iblp+1) /= 'MODEL' .and.&
      &field(iblp+1) /= 'AREA' .and.&
      &field(iblp+1) /= 'LINE' .and.&
      &field(iblp+1) /= 'PRIME' .and.&
      &field(iblp+1) /= 'PVMRM' .and.&
      &field(iblp+1) /= 'OLM' .and.&
      &field(iblp+1) /= 'ARM2' .and.&
      &field(iblp+1) /= 'GRSM' .and.&
      &field(iblp+1) /= 'PLATFORM' .and.&
      &field(iblp+1) /= 'DEPOS'.and.&
      &field(iblp+1) /= 'AWMADW' .and.&
      &field(iblp+1) /= 'TTRM' .and.&
      &field(iblp+1) /= 'TTRM2' .and.&
      &field(iblp+1) /= 'RLINE' .and.&
      &field(iblp+1) /= 'URBANDB' .and.&
      &field(iblp+1) /= 'SWPOINT' .and.&
      &field(iblp+1) /= 'AIRCRAFT' .and.&
      &field(iblp+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the DISTANCE debug option
         blpfil = runst1(locb(iblp+1):loce(iblp+1))
      else
! ---       Assign default BLPDBUG debug filename
         blpfil = 'BLPDBUG.DBG'
      end if
   end if
!     End BLPDBUG insert

!     CRT, 3/25/2022 D113 Updated for SIDEWASH
   if (swdbg) then
      if (ifc >= isw+1 .and.&
      &field(isw+1) /= 'METEOR' .and.&
      &field(isw+1) /= 'MODEL' .and.&
      &field(isw+1) /= 'PRIME' .and.&
      &field(isw+1) /= 'PVMRM' .and.&
      &field(isw+1) /= 'OLM' .and.&
      &field(isw+1) /= 'ARM2' .and.&
      &field(isw+1) /= 'GRSM' .and.&
      &field(isw+1) /= 'AREA' .and.&
      &field(isw+1) /= 'LINE' .and.&
      &field(isw+1) /= 'AWMADW' .and.&
      &field(isw+1) /= 'PLATFORM' .and.&
      &field(isw+1) /= 'DEPOS' .and.&
      &field(isw+1) /= 'TTRM' .and.&
      &field(isw+1) /= 'TTRM2' .and.&
      &field(isw+1) /= 'RLINE' .and.&
      &field(isw+1) /= 'URBANDB'.and.&
      &field(isw+1) /= 'AIRCRAFT' .and.&
      &field(isw+1) /= 'HBPDBG') then

! ---       Assign user-specified filename for the SIDEWASH debug option
         swfil = runst1(locb(isw+1):loce(isw+1))
      else
! ---       Assign default Sidewash source debug filename
         swfil = 'SWPOINT.DBG'
      end if
   end if

!**   Added for Aircraft Plume Rise; UNC-IE
   if (arcftdebug) then
      if (ifc >= iarcft+1 .and.&
      &field(iarcft+1) /= 'METEOR' .and.&
      &field(iarcft+1) /= 'MODEL' .and.&
      &field(iarcft+1) /= 'AREA' .and.&
      &field(iarcft+1) /= 'LINE' .and.&
      &field(iarcft+1) /= 'PRIME' .and.&
      &field(iarcft+1) /= 'PVMRM' .and.&
      &field(iarcft+1) /= 'OLM' .and.&
      &field(iarcft+1) /= 'ARM2' .and.&
      &field(iarcft+1) /= 'GRSM' .and.&
      &field(iarcft+1) /= 'PLATFORM' .and.&
      &field(iarcft+1) /= 'DEPOS'.and.&
      &field(iarcft+1) /= 'AWMADW' .and.&
      &field(iarcft+1) /= 'TTRM'.and.&
      &field(iarcft+1) /= 'RLINE' .and.&
      &field(iarcft+1) /= 'URBANDB'.and.&
      &field(iarcft+1) /= 'BLPDBUG' .and.&
      &field(iarcft+1) /= 'HBPDBG') then
! ---       Assign user-specified filename for the AIRCRAFT debug option
         dbarcftfil = runst1(locb(iarcft+1):loce(iarcft+1))
      else
! ---       Assign default AIRCRAFT debug filename
         dbarcftfil = 'AIRCRAFT.DBG'
      end if
   end if
!**  End Aircraft Plume Rise insert; April 2023

! Added for HBP DEBUG, JAN 2023
   if (hbpdbg) then
      if (ifc >= ihbp+1 .and.&
      &field(ihbp+1) /= 'METEOR' .and.&
      &field(ihbp+1) /= 'MODEL' .and.&
      &field(ihbp+1) /= 'AREA' .and.&
      &field(ihbp+1) /= 'LINE' .and.&
      &field(ihbp+1) /= 'PRIME' .and.&
      &field(ihbp+1) /= 'PVMRM' .and.&
      &field(ihbp+1) /= 'OLM' .and.&
      &field(ihbp+1) /= 'ARM2' .and.&
      &field(ihbp+1) /= 'GRSM' .and.&
      &field(ihbp+1) /= 'PLATFORM' .and.&
      &field(ihbp+1) /= 'DEPOS'.and.&
      &field(ihbp+1) /= 'AWMADW' .and.&
      &field(ihbp+1) /= 'TTRM'.and.&
      &field(ihbp+1) /= 'TTRM2' .and.&
      &field(ihbp+1) /= 'RLINE' .and.&
      &field(ihbp+1) /= 'URBANDB' .and.&
      &field(ihbp+1) /= 'SWPOINT' .and.&
      &field(ihbp+1) /= 'BLPDBUG' .and.&
      &field(ihbp+1) /= 'URBANDB'.and.&
      &field(ihbp+1) /= 'AIRCRAFT') then
! ---       Assign user-specified filename for the HBPBUG debug option
         hbpfil = runst1(locb(ihbp+1):loce(ihbp+1))

      else
! ---       Assign default HBPDBG debug filename
         hbpfil = 'HBP_DEBUG.DBG'
      end if
   end if
! End HBP DEBUG, JAN 2023

! --- Open MODEL, METEOR, AREA, PRIME and AWMADW debug files, if selected;
!     note that PVMRM, OLM, ARM2, GRSM and DEPOS debug files are opened
!     elsewhere
! Unused:  200 FORMAT ( ' OPTIONS: ', A /)
!     JAT 05/08/2020 ADD CODE TO OPEN IF DEBUG OR DEPOSDBG
!     BECAUSE IT USES THE DEBUGFIL AS WELL
!      IF (DEBUG) THEN
   if (debug .or. deposdbg) then
!        Open debug output file
      dummy = 'DebugFile'
      open (unit=dbgunt,file=dbgfil,err=91,status='REPLACE')
   end if

   goto 101

!     WRITE Error Message:  Error Opening File
91 call errhdl(path,modnam,'E','500',dummy)

101 continue

   if (meteordbg) then
!        Open debug meteorology output file
      dummy = 'DbgMetFile'
      open (unit=dbmunt,file=dbmfil,err=92,status='REPLACE')
   end if

   goto 102

!     WRITE Error Message:  Error Opening File
92 call errhdl(path,modnam,'E','500',dummy)

102 continue

   if (areadbg) then
!        Open debug AREA output file
      dummy = 'AreaDbgFile'
      open (unit=areadbunt,file=dbareafil,err=93,status='REPLACE')
   end if

   goto 103

!     WRITE Error Message:  Error Opening File
93 call errhdl(path,modnam,'E','500',dummy)

103 continue

   if (primedbg) then
!        Open debug PRIME output file
      dummy = 'PrimeDbgFile'
      open (unit=prmdbunt,file=dbprmfil,err=94,status='REPLACE')
   end if

   goto 104

!     WRITE Error Message:  Error Opening File
94 call errhdl(path,modnam,'E','500',dummy)

104 continue

   if (awmadwdbg) then
!        Open debug AWMADW output file
      dummy = 'AwmaDwDbgFile'
      open (unit=AwmaDwDBUNT,file=DBAwmaDwFIL,err=95,&
      &status='REPLACE')
   end if

   goto 105

!     WRITE Error Message:  Error Opening File
95 call errhdl(path,modnam,'E','500',dummy)

105 continue


   if (ttrmdbg) then
!        Open TTRM output file
      dummy = 'TTRMFIL'
      open (unit=ttrmunt,file=ttrmfil,err=96,status='REPLACE')
      write(ttrmunt,'(''TTRM Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
   end if

   goto 106


!     WRITE Error Message:  Error Opening File
96 call errhdl(path,modnam,'E','500',dummy)

106 continue

   if (rlinedbg) then
!        Open RLINE DEBUG output file
      dummy = 'RLINEFIL'
      open (unit=rlinedbunt,file=rlinedbgfil,err=97,&
      &status='REPLACE')
!       Write inital header Wood 10/10/22
      write(rlinedbunt,'(''RLINE Debug File'',1x,a8,5x,a8)')&
      &rundat, runtim
!        Open second RLINE DEBUG output file for gridded wind speed Wood 10/10/22
      dummy = 'RLINEFIL_WS'
      open (unit=rlinedbunt_ws,file=rlinedbgfil_ws,err=97,&
      &status='REPLACE')
      write(rlinedbunt_ws,'(''RLINE Gridded Wind Speed Debug File'',&
      &                          51x,a8,5x,a8)') rundat, runtim
      write(rlinedbunt_ws,2234)
2234  format (3x,'YR',2x,'MO',2x,'DY',2x,'HR',3x,'HT_m',5x,'WS_m_s')
   end if
   goto 107

!     WRITE Error Message:  Error Opening File
97 call errhdl(path,modnam,'E','500',dummy)

107 continue

!RCO D095 Added for urban debug 8/3/2021
! -- Open URBDBUG debug file, if selected;
   if (urbdbug) then
!        Open URBDBUG output file
      dummy = 'URBFIL'
      open (unit=urbunt,file=urbfil,err=197,status='REPLACE')
      write(urbunt,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim

      dummy = 'URBFIL1'
      open (unit=urbunt1,file=urbfil1,err=197,status='REPLACE')
      write(urbunt1,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
!RCO - D168 Debug files. Add output file for second urban debug
      dummy = 'URBFIL2'
      open (unit=urbunt2,file=urbfil2,err=197,status='REPLACE')
      write(urbunt2,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
   end if

   goto 108
!     WRITE Error Message:  Error Opening File
197 call errhdl(path,modnam,'E','500',dummy)
108 continue
!   end of URBDBUG insert


!CRCO D095 BLP debug file
   if (blpdbug) then
!        Open BLPDBUG output file
      dummy = 'BLPFIL'
      open (unit=blpunt,file=blpfil,err=198,status='REPLACE')
!        D140 removed exisiting BLP header to match the formatting for the other debug files Wood 9/29/22
!         WRITE(BLPUNT,'(''BLPDBUG Debug File'',51x,a8,/70x,a8)')
!     &                                           rundat, runtim
   end if

   goto 109
!     WRITE Error Message:  Error Opening File
198 call errhdl(path,modnam,'E','500',dummy)
109 continue
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
   if (platfmdbg) then
!        Open debug PLATFORM output file
      dummy = 'PLATFMDBGFILE'
      open (unit=platfmdbunt,file=platfmdbgfile,err=98,&
      &status='REPLACE')
   end if

   goto 110

!     WRITE Error Message:  Error Opening File
98 call errhdl(path,modnam,'E','500',dummy)

110 continue

!CRT  D113 Sidewash Debug, 3/25/2022
   if (swdbg) then
!        Open debug Sidewash output file
      dummy = 'SWFIL'
      open (unit=swdbgunt,file=swfil,err=99,&
      &status='REPLACE')
   end if

   goto 111

!     WRITE Error Message:  Error Opening File
99 call errhdl(path,modnam,'E','500',dummy)

111 continue

!** Added for Aircraft Plume Rise; UNC-IE
   if (arcftdebug) then
!       Open Aircraft output file
      dummy = 'ARCFTFIL'
      open (unit=arcftdbg,file=dbarcftfil,err=97,status='REPLACE')
!      Write the standard header information with AERMOD/AERMET
!      versions, 1st title, and rundat/runtim to the debug output file
      write(arcftdbg,7028) versn, title1(1:68), rundat, runtim
7028  format('*** AERMOD - VERSION ',a6,' ***',52x,'*** ',a28,&
      &' ***',2x,a8,/115x,' ***',2x,a8/)

      write(arcftdbg,'(''*********************************************&
      &   Aircraft Plume Rise - Debug File  *****************************&
      &******************'')')

   end if
   goto 112
!     WRITE Error Message:  Error Opening File
100 call errhdl(path,modnam,'E','500',dummy)

112 continue
!** End Aircraft Plume Rise insert; April 2023

! Added for HBP debug, JAN 2023
   if (hbpdbg) then
!        Open HBP output file
      dummy = 'HBPFIL'
      open (unit=hbpunt,file=hbpfil,err=797,status='REPLACE')
      write(hbpunt,'(''HBP Debug File'',51x,a8,/70x,a8)')&
      &rundat, runtim
   end if

   goto 7107

!     WRITE Error Message:  Error Opening File
797 call errhdl(path,modnam,'E','500',dummy)

7107 continue
! End HBP insert

   if (ifc > MAXFields) then
!        Maximum number of fields exceeded, issue warning message,
!        including up to 12 characters from last field
      write(dummy,'(A:)') field(ifc)(1:min(12,len_trim(field(ifc))))
      call errhdl(path,modnam,'E','203',dummy)
   end if

   go to 999

!     WRITE Error Message:  Error Opening File
! Unused:  99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

999 return
end subroutine debopt

subroutine myear
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'MYEAR'

   if (rstsav) then
      call errhdl(path,modnam,'E','150','SAVEFILE')

   else if (rstinp) then
      call errhdl(path,modnam,'E','150','INITFILE')

   else if (.not. (pollut == 'PM10' .or. pollut == 'PM-10' .or.&
   &pollut == 'NO2'  .or. pollut == 'SO2'   .or.&
   &pollut == 'LEAD' .or. pollut == 'OTHER' .or.&
   &pollut == 'PM25' .or. pollut == 'PM-2.5'.or.&
   &pollut == 'PM-25'.or. pollut == 'PM2.5') )then
!        WRITE Error Message:  Conflicting Options MULTYEAR For Wrong POLLUT
      call errhdl(path,modnam,'E','150',pollut)

   else if (ifc >= 4 .and. field(3) == 'H6H') then
! ---    Write Warning Message:  The 'H6H' field is no longer required
!        for the MULTYEAR keyword
      call errhdl(path,modnam,'W','352','Keyword ')
      if (ifc == 4) then
         multyr = .true.
         rstsav = .true.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            savfil = runst1(locb(4):loce(4))
         else
!              WRITE Error Message:  SAVFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
            return
         end if
         savfl2 = savfil
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      else if (ifc == 5) then
         multyr = .true.
         rstsav = .true.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            savfil = runst1(locb(4):loce(4))
         else
!              WRITE Error Message:  SAVFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
            return
         end if
         savfl2 = savfil
         rstinp = .true.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         if ((loce(5)-locb(5)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            inifil = runst1(locb(5):loce(5))
         else
!              WRITE Error Message:  INIFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
            return
         end if
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      else if (ifc > 5) then
!           WRITE Error Message           ! Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
      end if
   else if (ifc >= 3 .and. field(3) /= 'H6H') then
! ---    Process input parameters without the 'H6H' keyword
      if (ifc == 3) then
         multyr = .true.
         rstsav = .true.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         if ((loce(3)-locb(4)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            savfil = runst1(locb(3):loce(3))
         else
!              WRITE Error Message:  SAVFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
            return
         end if
         savfl2 = savfil
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      else if (ifc == 4) then
         multyr = .true.
         rstsav = .true.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            savfil = runst1(locb(3):loce(3))
         else
!              WRITE Error Message:  SAVFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
            return
         end if
         savfl2 = savfil
         rstinp = .true.
!           Use Character Substring to Retrieve Filenames to Maintain Case
         if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            inifil = runst1(locb(4):loce(4))
         else
!              WRITE Error Message:  INIFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
            return
         end if
! ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
      else if (ifc > 4) then
!           WRITE Error Message           ! Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
      end if
   else if (ifc == 3 .and. field(3) == 'H6H') then
!        WRITE Error Message           ! Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
   else if (ifc < 3) then
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine myear

subroutine gddef
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'GDDEF'

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 6) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Read Gas Dry Deposition Parameters
!     Change Them To Numbers
!     First Get Reactivity Value (fo)
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
!     Assign The Field
   Fo = dnum

!     Now Get Fraction of Maximum Green LAI for Seasonal Category 2
   call stodbl(field(4),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
!     Assign The Field
   fseas2 = dnum

!     Now Get Fraction of Maximum Green LAI for Seasonal Category 5
   call stodbl(field(5),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
!     Assign The Field
   fseas5 = dnum

   if (ifc == 6) then
!        Get the Reference Species (Optional)
      refspe = field(6)
   else
      refspe = '      '
   end if

999 return
end subroutine gddef

subroutine gdseas
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
   use main1
   implicit none
   character :: modnam*12
   integer :: i, j, isea_ndx

!     Variable Initializations
   modnam = 'GDSEAS'

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 14) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   iset = 0
   do i = 3, ifc
!        Change Fields To Numbers
      call stonum(field(i),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         cycle
      end if
      do j = 1, imit
         iset = iset + 1
!           Assign The Field
         if (iset <= 12) then
            isea_ndx = nint(fnum)
            if (isea_ndx >= 1 .and. isea_ndx <= 5) then
               iseas_gd(iset) = isea_ndx
            else
!                 WRITE Error Message    ! Season Index out-of-range
               call errhdl(path,modnam,'E','380',keywrd)
            end if
         else
!              WRITE Error Message    ! Too Many Months Input
            call errhdl(path,modnam,'E','234',keywrd)
         end if
      end do
   end do

999 return
end subroutine gdseas

subroutine gvsubd
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'GVSUBD'

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 3) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Read User-specified Dry Deposition Velocity
!     Change Them To Numbers
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
!     Assign The Field
   uservd = dnum

!     Perform range/validity check
   if (uservd < 0.0d0) then
!        Write Error Message:  Negative deposition velocity
      call errhdl(path,modnam,'E','209',' USERVD ')
   else if (uservd == 0.0d0) then
!        Write Error Message:  Deposition velocity = 0.0
      call errhdl(path,modnam,'E','380','USERVD=0')
   else if (uservd > 0.05d0) then
!        Write Warning Message:  Large deposition velocity
      call errhdl(path,modnam,'W','320',' USERVD ')
   end if

!     Set Logical Variable for User-specified Deposition Velocity
   luservd = .true.

999 return
end subroutine gvsubd

subroutine gdland
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
   use main1
   implicit none
   character :: modnam*12
   integer :: i, j, iland_ndx

!     Variable Initializations
   modnam = 'GDLAND'

!     Check the Number of Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 38) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   iset = 0
   do i = 3, ifc
!        Change Fields To Numbers
      call stonum(field(i),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         cycle
      end if
      do j = 1, imit
         iset = iset + 1
!           Assign The Field
         if (iset <= 36) then
            iland_ndx = nint(fnum)
            if (iland_ndx >= 1 .and. iland_ndx <= 9) then
               iland_gd(iset) = iland_ndx
            else
!                 WRITE Error Message    ! Land Use Index out-of-range
               call errhdl(path,modnam,'E','380',keywrd)
            end if
         else
!              WRITE Error Message    ! Too Many Directions Input
            call errhdl(path,modnam,'E','234',keywrd)
         end if
      end do
   end do

999 return
end subroutine gdland

subroutine urbopt
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
   use main1
   implicit none
   integer :: i
   character :: modnam*12, tempid*8

!     Variable Initializations
   modnam = 'URBOPT'

!     Determine Whether There Are Too Few Or Too Many Parameter Fields
   if ((.not. l_multurb .and. ifc < 3) .or.&
   &(l_multurb .and. ifc < 4)) then
!        WRITE Error Message: Missing Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if ((.not. l_multurb .and. ifc > 5) .or.&
   &(l_multurb .and. ifc > 6)) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   if (.not. l_urban_all .and. l_multurb) then
!        READ in the Urban ID for multiple urban areas
      if ((loce(3)-locb(3)) <= 7) then
!*          Retrieve Source ID Character Substring
         tempid = field(3)
      else
!*          WRITE Error Message:  Urban ID Field is Too Long
         call errhdl(path,modnam,'E','219',field(isc)(1:12))
         recerr = .true.
         go to 999
      end if
      do i = 1, numurb
         if (tempid == urbid(i)) then
!              WRITE Error Message:  Urban ID already defined
            call errhdl(path,modnam,'E','303',tempid)
!              Exit to END
            go to 999
         end if
      end do

!        New Urban ID Defined, Increment Counters
      iurb = iurb + 1
      if (iurb > nurb) then
!           WRITE Error Message    ! Too Many Urban Areas Specified
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NURB='',I7)') nurb
         call errhdl(path,modnam,'E','290',dummy)
!           Exit to END
         go to 999
      end if
      numurb = numurb + 1
      urbid(iurb) = tempid

      if (ifc >= 4) then
         call stodbl(field(4),ilen_fld,urbpop(iurb),imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field
            call errhdl(path,modnam,'E','208','URB-POP')
         else if (urbpop(iurb) < 100.0d0) then
! ---          Urban population below about 90 will cause math error
! ---          Write Error Message:Invalid Value Specified
            call errhdl(path,modnam,'E','203','URB-POP')
         else if (urbpop(iurb) < 21206.0d0) then
! ---          Flag urban population below 21,206 as potentially out-of-range;
!              this value corresponds with a population density of 750/sq-km
!              across an area of 3km in radius, a criterion cited for urban
!              classification in Section 7.2.3(d) of Appendix W.
            call errhdl(path,modnam,'W','320','URB-POP')
         end if
      end if

      if (ifc >= 5) then
!           Assign name of urban area (optional)
         urbnam(iurb) = field(5)
      end if

      if (ifc == 6) then
!           Assign value of urban roughness length (optional)
         call stodbl(field(6),ilen_fld,urbz0(iurb),imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field
            call errhdl(path,modnam,'E','208','URBAN_Z0')
         else
            if (dfault .and. urbz0(iurb) /= 1.0d0) then
!                 Write Warning Message: Non-default urban roughness length
               call errhdl(path,modnam,'W','206','URBAN_Z0')
               urbz0(iurb) = 1.0d0
            else if (.not. dfault .and. urbz0(iurb) /= 1.0d0) then
!                 Set flag for use of non-DEFAULT option
               L_NonDFAULT = .true.
            end if
            if (urbz0(iurb) < 0.80d0) then
!                 Write Warning Message: Urban roughness out of range
               write(dummy,'(F8.2)') urbz0(iurb)
               call errhdl(path,modnam,'W','353',dummy)
            else if (urbz0(iurb) > 1.50d0 .and.&
            &urbz0(iurb) < 5.0d0) then
!                 Write Warning Message: Urban roughness out of range
               write(dummy,'(F8.2)') urbz0(iurb)
               call errhdl(path,modnam,'W','353',dummy)
            else if (urbz0(iurb) >= 5.0d0) then
!                 Write Error Message: Urban roughness out of range
               call errhdl(path,modnam,'E','380','URBAN Z0')
            end if
         end if
      else
         urbz0(iurb) = 1.0d0
      end if

   else if (l_urban_all .and. l_multurb) then
!        Write Error Message: URBANSRC ALL option with
!        multiple URBAN areas
      call errhdl(path,modnam,'E','279','URBANSRC ALL')

   else
!        Single Urban Area - Process Inputs without URBAN ID

      iurb = 1

      if (ifc >= 3) then
         call stodbl(field(3),ilen_fld,urbpop(iurb),imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field
            call errhdl(path,modnam,'E','208','URB-POP')
         else if (urbpop(iurb) < 100.0d0) then
! ---          Urban population below about 90 will cause math error
! ---          Write Error Message:Invalid Value Specified
            call errhdl(path,modnam,'E','203','URB-POP')
         else if (urbpop(iurb) < 21206.0d0) then
! ---          Flag urban population below 21,206 as potentially out-of-range;
!              this value corresponds with a population density of 750/sq-km
!              across an area of 3km in radius, a criterion cited for urban
!              classification in Section 7.2.3(d) of Appendix W.
            call errhdl(path,modnam,'W','320','URB-POP')
         end if
      end if

      if (ifc >= 4) then
!           Assign name of urban area (optional)
         urbnam(iurb) = field(4)
      end if

      if (ifc == 5) then
!           Assign value of urban roughness length (optional)
         call stodbl(field(5),ilen_fld,urbz0(iurb),imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field
            call errhdl(path,modnam,'E','208','URBAN_Z0')
         else
            if (dfault .and. urbz0(iurb) /= 1.0d0) then
!                 Write Warning Message: Non-default urban roughness length
               call errhdl(path,modnam,'W','206','URBAN_Z0')
               urbz0(iurb) = 1.0d0
            else if (.not. dfault .and. urbz0(iurb) /= 1.0d0) then
!                 Set flag for use of non-DEFAULT option
               L_NonDFAULT = .true.
            end if
            if (urbz0(iurb) < 0.80d0) then
!                 Write Warning Message: Urban roughness out of range
               write(dummy,'(F8.2)') urbz0(iurb)
               call errhdl(path,modnam,'W','353',dummy)
            else if (urbz0(iurb) > 1.50d0 .and.&
            &urbz0(iurb) < 5.0d0) then
!                 Write Warning Message: Urban roughness out of range
               write(dummy,'(F8.2)') urbz0(iurb)
               call errhdl(path,modnam,'W','353',dummy)
            else if (urbz0(iurb) >= 5.0d0) then
!                 Write Error Message: Urban roughness out of range
               call errhdl(path,modnam,'E','380','URBAN Z0')
            end if
         end if
      else
         urbz0(iurb) = 1.0d0
      end if

      numurb = 1

   end if

!     Assign Logical for Urban Option
   urban  = .true.

999 return
end subroutine urbopt

subroutine o3val
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
   use main1
   implicit none

   integer :: i

   character :: modnam*12

!     Variable Initializations
   modnam = 'O3VAL'

! --- Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_O3Sector) then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc > 4) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
! ---    Check for SECT ID in field 3 in case O3SECTOR keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without O3SECTOR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified O3VALUES option and
!        assign the option to O3FLAG variable
      io3sect = 1
      i = 3
      l_o3val(io3sect) = .true.

   else
! ---    Process inputs based on O3SECTOR option
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc == 4) then
         if (field(3)(1:4) /= 'SECT') then
!              Error Message: Invalid sector field
            call errhdl(path,modnam,'E','203','O3SECTOR ID')
            go to 999
         else
!              Error Message: No Numerical Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         end if
      else if (ifc > 5) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
! ---    Determine user-specified sector
      if (field(3) == 'SECT1') then
         io3sect = 1
      else if (field(3) == 'SECT2') then
         io3sect = 2
      else if (field(3) == 'SECT3') then
         io3sect = 3
      else if (field(3) == 'SECT4') then
         io3sect = 4
      else if (field(3) == 'SECT5') then
         io3sect = 5
      else if (field(3) == 'SECT6') then
         io3sect = 6
      else
!           Error Message: Invalid sector definition
         call errhdl(path,modnam,'E','203','O3SECTOR ID')
         go to 999
      end if

! ---    Set field index for the user-specified Ozone Value
      i = 4
      l_o3val(io3sect) = .true.

   end if

!     Get Ozone Value, O3BACK, for applicable sector
   call stodbl(field(i),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

!     Assign value to O3BACK variable for this sector
   o3back(io3sect) = dnum

!     Check for units of ozone value
   if (ifc == i+1) then
      if (field(i+1)=='PPM' .or. field(i+1)=='PPB' .or.&
      &field(i+1)=='UG/M3') then
         o3valunits = field(i+1)
      else
!           Write Error Message:  Invalid units for ozone value
         call errhdl(path,modnam,'E','203',' O3UNITS')
      end if
   end if

   if (o3valunits == 'PPB') then
      o3back(io3sect) = o3back(io3sect) * o3_ppb
   else if (o3valunits == 'PPM') then
      o3back(io3sect) = o3back(io3sect) * o3_ppm
   end if

!     Check range of value
   if (o3back(io3sect) <= 0.0d0 .or.&
   &o3back(io3sect) > 500.0d0)then
      call errhdl(path,modnam,'W','320',' O3BACK ')
   end if

999 return
end subroutine o3val

subroutine noxval
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
   use main1
   implicit none

   integer :: i

   character :: modnam*12

!     Variable Initializations
   modnam = 'NOXVAL'

! --- Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_NOXSector) then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc > 4) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
! ---    Check for SECT ID in field 3 in case NOXSECTR keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without NOXSECTR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified NOXVALUES option and
!        assign the option to NOXFLAG variable
      inoxsect = 1
      i = 3
      l_noxvalue(inoxsect) = .true.
!
   else
! ---    Process inputs based on NOXSECTR option
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc == 4) then
         if (field(3)(1:4) /= 'SECT') then
!              Error Message: Invalid sector field
            call errhdl(path,modnam,'E','203','NOXSECTOR ID')
            go to 999
         else
!              Error Message: No Numerical Parameters
            call errhdl(path,modnam,'E','201',keywrd)
            go to 999
         end if
      else if (ifc > 5) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
! ---    Determine user-specified sector
      if (field(3) == 'SECT1') then
         inoxsect = 1
      else if (field(3) == 'SECT2') then
         inoxsect = 2
      else if (field(3) == 'SECT3') then
         inoxsect = 3
      else if (field(3) == 'SECT4') then
         inoxsect = 4
      else if (field(3) == 'SECT5') then
         inoxsect = 5
      else if (field(3) == 'SECT6') then
         inoxsect = 6
      else
!           Error Message: Invalid sector definition
         call errhdl(path,modnam,'E','203','NOXSECTOR ID')
         go to 999
      end if

! ---    Set field index for the user-specified NOX Value
      i = 4
      l_noxvalue(inoxsect) = .true.
   end if

!     Get NOX Value, NOXBACK, for applicable sector
   call stodbl(field(i),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

!     Assign value to NOXBACK variable for this sector
   noxback(inoxsect) = dnum

!     Check for units of NOx value
   if (ifc == i+1) then
      if (field(i+1)=='PPM' .or. field(i+1)=='PPB' .or.&
      &field(i+1)=='UG/M3') then
         noxvalunits = field(i+1)
      else
!           Write Error Message:  Invalid units for NOX value
         call errhdl(path,modnam,'E','203',' NOXUNITS')
      end if
   end if

   !Convert to UG/M3 using NO2 factors (NOx expressed as 'NOx as NO2')
   if (noxvalunits == 'PPB') then
      noxback(inoxsect) = noxback(inoxsect) / no2_ppb
   else if (noxvalunits == 'PPM') then
      noxback(inoxsect) = noxback(inoxsect) / no2_ppm
   end if

!     Check range of value
   if (noxback(inoxsect) <= 0.0d0) then
      call errhdl(path,modnam,'W','320',' NOXBACK ')
   end if

999 return
end subroutine noxval

subroutine o3file
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, NumInt, NumReal
   logical :: fopen

!     Variable Initializations
   modnam = 'O3FILE'
   NumInt  = 0
   NumReal = 0
   fopen   = .false.

! --- Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_O3Sector) then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 3) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 5) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
! ---    Check for SECT ID in field 3 in case O3FILE keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without O3SECTOR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for start of the user-specified options
      io3sect = 1
      i = 3
   else
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 4) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 6) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
      if (field(3) == 'SECT1') then
         io3sect = 1
      else if (field(3) == 'SECT2' .and. NUMO3Sects >= 2) then
         io3sect = 2
      else if (field(3) == 'SECT3' .and. NUMO3Sects >= 3) then
         io3sect = 3
      else if (field(3) == 'SECT4' .and. NUMO3Sects >= 4) then
         io3sect = 4
      else if (field(3) == 'SECT5' .and. NUMO3Sects >= 5) then
         io3sect = 5
      else if (field(3) == 'SECT6' .and. NUMO3Sects == 6) then
         io3sect = 6
      else
!           Error Message: Invalid sector field
         call errhdl(path,modnam,'E','203','O3SECTOR ID')
         go to 999
      end if
! ---    Assign set field index for start of the user-specified options,
!        accounting for sector IDs
      i = 4
   end if

!     Set logical flags for hourly ozone file(s)
   L_O3Hourly        = .true.
   L_O3File(io3sect) = .true.

!     Retrieve Ozone Data Filename as Character Substring to Maintain Case
   if ((loce(i)-locb(i)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      ozonfl(io3sect) = runst1(locb(i):loce(i))
   else
!        WRITE Error Message:  OZONFL Field is Too Long
!        Write error message and return
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
      go to 999
   end if

!     Assign file unit for this O3 file and Open The Ozone Input File
!     Open with ACTION='READ' to prevent overwrite and multiple access
   io3unt(io3sect) = 1000 + io3sect

!     Open hourly Ozone File If Not Already Open
   inquire (file=ozonfl(io3sect),opened=fopen)

   if (.not. fopen) then
!        Open Hourly Ozone Data File If Not Already Open
!        Open with ACTION='READ' to prevent overwrite and allow multiple access
      inquire (unit=io3unt(io3sect),opened=fopen)
      if (.not. fopen) then
         open(unit=io3unt(io3sect),file=ozonfl(io3sect),status='OLD',&
         &err=998,action='READ',form='FORMATTED')

      else
!           Hourly Ozone File is Already Opened With Different Filename
         call errhdl(path,modnam,'E','501',keywrd)
         go to 999
      end if
   else
!        Hourly Ozone File is Already Opened With Different Filename
      call errhdl(path,modnam,'E','501',keywrd)
      go to 999
   end if

!     Check for optional units of ozone value
   if (i == 3 .and. ifc >= 4) then
      if (field(4)=='PPM' .or. field(4)=='PPB' .or.&
      &field(4)=='UG/M3') then
         o3filunits = field(4)
      else
!           Write Error Message:  Invalid units for ozone value
         call errhdl(path,modnam,'E','203',' O3UNITS')
      end if
   else if (i == 4 .and. ifc >= 5) then
      if (field(5)=='PPM' .or. field(5)=='PPB' .or.&
      &field(5)=='UG/M3') then
         o3filunits = field(5)
      else
!           Write Error Message:  Invalid units for ozone value
         call errhdl(path,modnam,'E','203',' O3UNITS')
      end if
   else
      o3filunits = 'UG/M3'
   end if

   if (ifc == i+2) then
!        Check for Format String > ILEN_FLD PARAMETER
      if ((loce(i+2)-locb(i+2)) <= (ilen_fld - 1)) then

! ---       First check for user input of "FREE" for the formaat,
!           using FIELD array which has been converted to upper case
         if (field(i+2) == 'FREE') then
            o3form(io3sect) = 'FREE'
         else
!              Retrieve Met Format as Char. Substring
            o3form(io3sect) = runst1(locb(i+2):loce(i+2))
! ---          Check for correct format specifiers for Ozone file;
!              should be 4 integers for date variables and 1 real for
!              ozone concentration; allow for 1 to 4 integers since
!              format statement may include 4I2, and also allow for
!              either F, E, or D format for the data variable.
            do i = 1, len_trim(o3form(io3sect))
               if (o3form(io3sect)(i:i)=='I' .or.&
               &o3form(io3sect)(i:i)=='i') then
                  NumInt  = NumInt  + 1
               else if (o3form(io3sect)(i:i)=='F' .or.&
               &o3form(io3sect)(i:i)=='f') then
                  NumReal = NumReal + 1
               else if (o3form(io3sect)(i:i)=='E' .or.&
               &o3form(io3sect)(i:i)=='e') then
                  NumReal = NumReal + 1
               else if (o3form(io3sect)(i:i)=='D' .or.&
               &o3form(io3sect)(i:i)=='d') then
                  NumReal = NumReal + 1
               end if
            end do
            if (NumInt<1 .or. NumInt>4) then
!                 WRITE Warning Message:  Potential problem with O3FORM
               write(dummy,'(''NumInts= '',I3)') NumInt
               call errhdl(path,modnam,'W','292',dummy)
            end if
            if (NumReal/=1) then
!                 WRITE Warning Message:  Potential problem with O3FORM
               write(dummy,'(''NumReal= '',I3)') NumReal
               call errhdl(path,modnam,'W','292',dummy)
            end if
         end if
      else
!           WRITE Error Message:  O3FORM Field is Too Long
         write(dummy,'(''LEN='',I6)') loce(5)-locb(5)
         call errhdl(path,modnam,'E','292',dummy)
      end if
   else
! ---    Use 'free' format as the default
      o3form(io3sect) = 'FREE'
   end if

   go to 999

!     Process Error Messages; error opening file, include file type and sector
998 continue
   write(dummy,'("O3FILE SECT",I1)') io3sect
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine o3file

subroutine noxfile
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, NumInt, NumReal
   logical :: fopen

!     Variable Initializations
   modnam = 'NOXFILE'
   NumInt  = 0
   NumReal = 0
   fopen   = .false.

! --- Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_NOxSector) then
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 3) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 5) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
! ---    Check for SECT ID in field 3 in case NOXFILE keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without NOXSECTR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for start of the user-specified options
      inoxsect = 1
      i = 3
   else
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc < 4) then
!           Error Message: Not Enough Parameters
         call errhdl(path,modnam,'E','201',keywrd)
         go to 999
      else if (ifc > 6) then
!           Error Message: Too Many Parameters
         call errhdl(path,modnam,'E','202',keywrd)
         go to 999
      end if
      if (field(3) == 'SECT1') then
         inoxsect = 1
      else if (field(3) == 'SECT2' .and. NUMNOxSects >= 2) then
         inoxsect = 2
      else if (field(3) == 'SECT3' .and. NUMNOxSects >= 3) then
         inoxsect = 3
      else if (field(3) == 'SECT4' .and. NUMNOxSects >= 4) then
         inoxsect = 4
      else if (field(3) == 'SECT5' .and. NUMNOxSects >= 5) then
         inoxsect = 5
      else if (field(3) == 'SECT6' .and. NUMNOxSects == 6) then
         inoxsect = 6
      else
!           Error Message: Invalid sector field
         call errhdl(path,modnam,'E','203','NOXSECTR ID')
         go to 999
      end if
! ---    Assign set field index for start of the user-specified options,
!        accounting for sector IDs
      i = 4
   end if

!     Set logical flags for hourly NOx file(s)
   L_NOxHourly        = .true.
   L_NOxFile(inoxsect) = .true.

!     Retrieve NOx Data Filename as Character Substring to Maintain Case
   if ((loce(i)-locb(i)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      noxfl(inoxsect) = runst1(locb(i):loce(i))
   else
!        WRITE Error Message:  NOXFL Field is Too Long
!        Write error message and return
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
      go to 999
   end if

!     Assign file unit for this NOx file and Open The NOx Input File
!     Open with ACTION='READ' to prevent overwrite and multiple access
   inoxunt(inoxsect) = 3000 + inoxsect

!     Open hourly NOx File If Not Already Open
   inquire (file=noxfl(inoxsect),opened=fopen)

   if (.not. fopen) then
!        Open Hourly NOx Data File If Not Already Open
!        Open with ACTION='READ' to prevent overwrite and allow multiple access
      inquire (unit=inoxunt(inoxsect),opened=fopen)
      if (.not. fopen) then
         open(unit=inoxunt(inoxsect),file=noxfl(inoxsect),&
         &status='OLD',err=998,action='READ',form='FORMATTED')

      else
!           Hourly NOx File is Already Opened With Different Filename
         call errhdl(path,modnam,'E','501',keywrd)
         go to 999
      end if
   else
!        Hourly NOx File is Already Opened With Different Filename
      call errhdl(path,modnam,'E','501',keywrd)
      go to 999
   end if

!     Check for optional units of NOx value
   if (i == 3 .and. ifc >= 4) then
      if (field(4)=='PPM' .or. field(4)=='PPB' .or.&
      &field(4)=='UG/M3') then
         noxfilunits = field(4)
      else
!           Write Error Message:  Invalid units for NOx value
         call errhdl(path,modnam,'E','203','NOXUNITS')
      end if
   else if (i == 4 .and. ifc >= 5) then
      if (field(5)=='PPM' .or. field(5)=='PPB' .or.&
      &field(5)=='UG/M3') then
         noxfilunits = field(5)
      else
!           Write Error Message:  Invalid units for NOx value
         call errhdl(path,modnam,'E','203','NOXUNITS')
      end if
   else
      noxfilunits = 'UG/M3'
   end if

   if (ifc == i+2) then
!        Check for Format String > ILEN_FLD PARAMETER
      if ((loce(i+2)-locb(i+2)) <= (ilen_fld - 1)) then

! ---       First check for user input of "FREE" for the format,
!           using FIELD array which has been converted to upper case
         if (field(i+2) == 'FREE') then
            noxform(inoxsect) = 'FREE'
         else
!              Retrieve Format as Char. Substring
            noxform(inoxsect) = runst1(locb(i+2):loce(i+2))
! ---          Check for correct format specifiers for NOx file;
!              should be 4 integers for date variables and 1 real for
!              NOx concentration; allow for 1 to 4 integers since
!              format statement may include 4I2, and also allow for
!              either F, E, or D format for the data variable.
            do i = 1, len_trim(noxform(inoxsect))
               if (noxform(inoxsect)(i:i)=='I' .or.&
               &noxform(inoxsect)(i:i)=='i') then
                  NumInt  = NumInt  + 1
               else if (noxform(inoxsect)(i:i)=='F' .or.&
               &noxform(inoxsect)(i:i)=='f') then
                  NumReal = NumReal + 1
               else if (noxform(inoxsect)(i:i)=='E' .or.&
               &noxform(inoxsect)(i:i)=='e') then
                  NumReal = NumReal + 1
               else if (noxform(inoxsect)(i:i)=='D' .or.&
               &noxform(inoxsect)(i:i)=='d') then
                  NumReal = NumReal + 1
               end if
            end do
            if (NumInt<1 .or. NumInt>4) then
!                 WRITE Warning Message:  Potential problem with NOXFORM
               write(dummy,'(''NumInts= '',I3)') NumInt
               call errhdl(path,modnam,'W','292',dummy)
            end if
            if (NumReal/=1) then
!                 WRITE Warning Message:  Potential problem with NOXFORM
               write(dummy,'(''NumReal= '',I3)') NumReal
               call errhdl(path,modnam,'W','292',dummy)
            end if
         end if
      else
!           WRITE Error Message:  NOXFORM Field is Too Long
         write(dummy,'(''LEN='',I6)') loce(5)-locb(5)
         call errhdl(path,modnam,'E','292',dummy)
      end if
   else
! ---    Use 'free' format as the default
      noxform(inoxsect) = 'FREE'
   end if

   go to 999

!     Process Error Messages; error opening file, include file type and sector
998 continue
   write(dummy,'("NOXFILE SCT",I1)') inoxsect
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine noxfile

subroutine no2eq
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'NO2EQ'

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

!     Start To Get Ozone Value
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

!     Assign value to NO2Equil variable
   NO2Equil = dnum

!     Check range of value
   if (NO2Equil < 0.10d0 .or. NO2Equil > 1.0d0) then
      call errhdl(path,modnam,'E','380','NO2Equil')
   end if

999 return
end subroutine no2eq


subroutine no2stk
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
   use main1
   implicit none
   integer :: i
   character :: modnam*12

!     Variable Initializations
   modnam = 'NO2STK'

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

!     Start To Get Ozone Value
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if

!     Assign value to NO2Stack variable
   NO2Stack = dnum

!     Check range of value
   if (NO2Stack < 0.0d0 .or. NO2Stack > 1.0d0) then
      call errhdl(path,modnam,'E','380','NO2Stack')
      go to 999
   end if

   do i = 1, nsrc
      ano2_ratio(i) = NO2Stack
   end do

999 return
end subroutine no2stk

subroutine ARM2_Ratios
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'ARM2_Ratios'

!     Check The Number Of The Fields
   if (arm2 .and. ifc < 4) then
!        Error Message: Too Few Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if


! --- Get the minimum ARM2 ratio (ARM2_Min)
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   else
!        Assign value to ARM2_Min
      ARM2_Min = dnum
   end if

!     Check range of value for ARM2_Min
!     D157 WSP 3/28/23 Changed ARMRATIO restictions to match user's guide
!     DFAULT ARMRATIO is 0.5 <= ARMRATIO <= 0.9
!     NONDFAULT ARMRATIO is 0.0 < ARMRATIO <= 1.0
   if (ARM2_Min < 0.50d0 .and. ARM2_Min > 0.0d0) then
      if( dfault )then
         call errhdl(path,modnam,'E','380',' ARM2Min')
      else
         call errhdl(path,modnam,'W','737',' ARM2Min')
      end if
   else if (ARM2_Min > 0.90d0 .and. ARM2_Min <= 1.0d0) then
      if( dfault )then
         call errhdl(path,modnam,'E','380', ' ARM2Min')
      else
         call errhdl(path,modnam,'W','737',' ARM2Min')
      end if
   else if (ARM2_Min <= 0.0d0) then
      call errhdl(path,modnam,'E','380',' ARM2Min')
   else if (ARM2_Min > 1.0d0) then
      call errhdl(path,modnam,'E','380',' ARM2Min')
!     D157 CRT 5/31/2023 remove warning message if ARMRATIO is within default
!      ELSE IF (ARM2_Min .GT. 0.50D0) THEN
!         CALL ERRHDL(PATH,MODNAM,'W','736',' ARM2Min')
   end if

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
   call stodbl(field(4),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   else
!        Assign value to ARM2_Max
      ARM2_Max = dnum
   end if

!     Check range of value for ARM2_Max
!     D157 WSP 3/28/23 Changed ARMRATIO restictions to match user's guide
!     DFAULT ARMRATIO is 0.5 <= ARMRATIO <= 0.9
!     NONDFAULT ARMRATIO is 0.0 < ARMRATIO <= 1.0
   if (ARM2_Max < 0.50d0 .and. ARM2_Max > 0.0d0) then
      if( dfault )then
         call errhdl(path,modnam,'E','380',' ARM2Max')
      else
         call errhdl(path,modnam,'W','737',' ARM2Max')
      end if
   else if (ARM2_Max > 0.90d0 .and. ARM2_Max <= 1.0d0) then
      if( dfault )then
         call errhdl(path,modnam,'E','380',' ARM2Max')
      else
         call errhdl(path,modnam,'W','737',' ARM2Max')
      end if
   else if (ARM2_Max <= 0.0d0) then
      call errhdl(path,modnam,'E','380',' ARM2Max')
   else if (ARM2_Max > 1.0d0) then
      call errhdl(path,modnam,'E','380',' ARM2Max')
!     D157 CRT 5/31/2023 remove warning message if ARMRATIO is within default
!      ELSE IF (ARM2_Max .LT. 0.90D0) THEN
!         CALL ERRHDL(PATH,MODNAM,'W','736',' ARM2Max')
   end if
!     D157 Check that ARMmax > ARMmin
   if (ARM2_Max < ARM2_Min) then
      write(dummy,'(''ARM2 Max<Min'')')
      call errhdl(path,modnam,'E','380',dummy)
   end if
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

999 return
end subroutine ARM2_Ratios

subroutine o3vals
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i
! Unused:      INTEGER :: IH, IL, ISDX, NNN
! Unused:      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
! Unused:      CHARACTER (LEN=ILEN_FLD) :: SOID
! Unused:      LOGICAL FOUND, INGRP, RMARK

!     Variable Initializations
   modnam = 'O3VALS'

!     Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_O3Sector) then
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
! ---    Check for SECT ID in field 3 in case O3SECTOR keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without O3SECTOR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified O3VALUES option and
!        assign the option to O3FLAG variable
      io3sect = 1
      i = 3
      l_o3values(io3sect) = .true.
      if (io3max(io3sect) >= 1 .and.&
      &o3flag(io3sect) /= field(i)) then
         call errhdl(path,modnam,'E','167',field(i))
      else
         o3flag(io3sect) = field(i)
      end if
   else
! ---    Process inputs based on O3SECTOR option
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc == 4) then
         if (field(3)(1:4) /= 'SECT') then
!              Error Message: Invalid sector field
            call errhdl(path,modnam,'E','203','O3SECTOR ID')
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
         io3sect = 1
      else if (field(3) == 'SECT2' .and. NUMO3Sects >= 2) then
         io3sect = 2
      else if (field(3) == 'SECT3' .and. NUMO3Sects >= 3) then
         io3sect = 3
      else if (field(3) == 'SECT4' .and. NUMO3Sects >= 4) then
         io3sect = 4
      else if (field(3) == 'SECT5' .and. NUMO3Sects >= 5) then
         io3sect = 5
      else if (field(3) == 'SECT6' .and. NUMO3Sects == 6) then
         io3sect = 6
      else
!           Error Message: Invalid sector field
         call errhdl(path,modnam,'E','203','O3SECTOR ID')
         go to 999
      end if
! ---    Set field index for the user-specified O3VALUES option and
!        assign the option to O3FLAG variable
      i = 4
      l_o3values(io3sect) = .true.
      if (io3max(io3sect) >= 1 .and.&
      &o3flag(io3sect) /= field(i)) then
         if (len_trim(field(i)) > 6) then
            write(dummy,'(''SEC'',I1,1X,A)') io3sect,&
            &field(i)(1:len_trim(field(i)))
            call errhdl(path,modnam,'E','167',dummy)
         else
            write(dummy,'(''SECT'',I1,1X,A)') io3sect,&
            &field(i)(1:len_trim(field(i)))
            call errhdl(path,modnam,'E','167',dummy)
         end if
      else
         o3flag(io3sect) = field(i)
      end if
   end if

! --- Assign number of ozone values based on O3FLAG option
   if (o3flag(io3sect) == 'ANNUAL') then
      io3max(io3sect) = 1
   else if (o3flag(io3sect) == 'SEASON') then
      io3max(io3sect) = 4
   else if (o3flag(io3sect) == 'MONTH') then
      io3max(io3sect) = 12
   else if (o3flag(io3sect) == 'HROFDY') then
      io3max(io3sect) = 24
   else if (o3flag(io3sect) == 'WSPEED') then
      io3max(io3sect) = 6
   else if (o3flag(io3sect) == 'SEASHR') then
      io3max(io3sect) = 96
   else if (o3flag(io3sect) == 'HRDOW') then
      io3max(io3sect) = 72
      L_DayOfWeekOpts = .true.
   else if (o3flag(io3sect) == 'HRDOW7') then
      io3max(io3sect) = 168
      L_DayOfWeekOpts = .true.
   else if (o3flag(io3sect) == 'SHRDOW') then
      io3max(io3sect) = 288
      L_DayOfWeekOpts = .true.
   else if (o3flag(io3sect) == 'SHRDOW7') then
      io3max(io3sect) = 672
      L_DayOfWeekOpts = .true.
   else if (o3flag(io3sect) == 'MHRDOW') then
      io3max(io3sect) = 864
      L_DayOfWeekOpts = .true.
   else if (o3flag(io3sect) == 'MHRDOW7') then
      io3max(io3sect) = 2016
      L_DayOfWeekOpts = .true.
   else
!        WRITE Error Message    ! Invalid O3FLAG Field Entered
      call errhdl(path,modnam,'E','203','O3FLAG')
      go to 999
   end if

! --- Call subroutine O3FILL to fill the temporally-varying O3 data
   call o3fill

999 return
end subroutine o3vals

subroutine noxvals
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i

!     Variable Initializations
   modnam = 'NOXVALS'

!     Check The Number Of The Fields, accounting for sector-varying values
   if (.not.L_NOXSector) then
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
! ---    Check for SECT ID in field 3 in case NOXSECTR keyword was omitted
      if (field(3)(1:4) == 'SECT') then
!           Error Message: SECT ID without NOXSECTOR keyword
         call errhdl(path,modnam,'E','171',keywrd)
         go to 999
      end if
! ---    Assign sector ID to 1 since sector-varying values not being used;
!        also set field index for the user-specified NOX_VALS option and
!        assign the option to NOXFLAG variable
      inoxsect = 1
      i = 3
      l_nox_vals(inoxsect) = .true.
      if (inoxmax(inoxsect) >= 1 .and.&
      &noxflag(inoxsect) /= field(i)) then
         call errhdl(path,modnam,'E','606',field(i))
      else
         noxflag(inoxsect) = field(i)
      end if
   else
! ---    Process inputs based on NOXSECTOR option
      if (ifc <= 2) then
!           Error Message: No Parameters
         call errhdl(path,modnam,'E','200',keywrd)
         go to 999
      else if (ifc == 4) then
         if (field(3)(1:4) /= 'SECT') then
!              Error Message: Invalid sector field
            call errhdl(path,modnam,'E','203','NOXSECTR ID')
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
         inoxsect = 1
      else if (field(3) == 'SECT2' .and. NUMNOxSects >= 2) then
         inoxsect = 2
      else if (field(3) == 'SECT3' .and. NUMNOxSects >= 3) then
         inoxsect = 3
      else if (field(3) == 'SECT4' .and. NUMNOxSects >= 4) then
         inoxsect = 4
      else if (field(3) == 'SECT5' .and. NUMNOxSects >= 5) then
         inoxsect = 5
      else if (field(3) == 'SECT6' .and. NUMNOxSects == 6) then
         inoxsect = 6
      else
!           Error Message: Invalid sector field
         call errhdl(path,modnam,'E','203','NOXSECTR ID')
         go to 999
      end if
! ---    Set field index for the user-specified NOX_VALS option and
!        assign the option to NOXFLAG variable
      i = 4
      l_nox_vals(inoxsect) = .true.
      if (inoxmax(inoxsect) >= 1 .and.&
      &noxflag(inoxsect) /= field(i)) then
         if (len_trim(field(i)) > 6) then
            write(dummy,'(''SEC'',I1,1X,A)') inoxsect,&
            &field(i)(1:len_trim(field(i)))
            call errhdl(path,modnam,'E','606',dummy)
         else
            write(dummy,'(''SECT'',I1,1X,A)') inoxsect,&
            &field(i)(1:len_trim(field(i)))
            call errhdl(path,modnam,'E','606',dummy)
         end if
      else
         noxflag(inoxsect) = field(i)
      end if
   end if

! --- Assign number of NOx values based on NOXFLAG option
   if (noxflag(inoxsect) == 'ANNUAL') then
      inoxmax(inoxsect) = 1
   else if (noxflag(inoxsect) == 'SEASON') then
      inoxmax(inoxsect) = 4
   else if (noxflag(inoxsect) == 'MONTH') then
      inoxmax(inoxsect) = 12
   else if (noxflag(inoxsect) == 'HROFDY') then
      inoxmax(inoxsect) = 24
   else if (noxflag(inoxsect) == 'WSPEED') then
      inoxmax(inoxsect) = 6
   else if (noxflag(inoxsect) == 'SEASHR') then
      inoxmax(inoxsect) = 96
   else if (noxflag(inoxsect) == 'HRDOW') then
      inoxmax(inoxsect) = 72
      L_DayOfWeekOpts = .true.
   else if (noxflag(inoxsect) == 'HRDOW7') then
      inoxmax(inoxsect) = 168
      L_DayOfWeekOpts = .true.
   else if (noxflag(inoxsect) == 'SHRDOW') then
      inoxmax(inoxsect) = 288
      L_DayOfWeekOpts = .true.
   else if (noxflag(inoxsect) == 'SHRDOW7') then
      inoxmax(inoxsect) = 672
      L_DayOfWeekOpts = .true.
   else if (noxflag(inoxsect) == 'MHRDOW') then
      inoxmax(inoxsect) = 864
      L_DayOfWeekOpts = .true.
   else if (noxflag(inoxsect) == 'MHRDOW7') then
      inoxmax(inoxsect) = 2016
      L_DayOfWeekOpts = .true.
   else
!        WRITE Error Message    ! Invalid NOXFLAG Field Entered
      call errhdl(path,modnam,'E','203','NOXFLG')
      go to 999
   end if

! --- Call subroutine NOXFILL to fill the temporally-varying NOX data
   call noxfill

999 return
end subroutine noxvals

subroutine o3fill
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k

! --- Variable Initializations
   modnam = 'O3FILL'

! --- Initialize counter for number of O3VALUES for this sector
   iset = io3set(io3sect)

! --- Assign field number for start of data values based on whether
!     sector-varying values are used
   if (L_O3Sector) then
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
         if (iset <= io3max(io3sect)) then
            o3vary(iset,io3sect) = dnum
            if (dnum < 0.0d0) then
!                 WRITE Error Message:  Negative Value for O3VALUES
               call errhdl(path,modnam,'E','209',keywrd)
            end if
         else
!              WRITE Error Message    ! Too Many O3VALUES Input
            if (L_O3Sector) then
               write(dummy,'(''O3VALs SECT'',I1)') io3sect
            else
               write(dummy,'(''O3VALUES'')')
            end if
            call errhdl(path,modnam,'E','231',dummy)
            go to 99
         end if
      end do
   end do

99 continue

! --- Save counter on number of values input so far for this sector
   io3set(io3sect) = iset

   return
end subroutine o3fill

subroutine noxfill
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k

! --- Variable Initializations
   modnam = 'NOXFILL'

! --- Initialize counter for number of NOX_VALS for this sector
   iset = inoxset(inoxsect)

! --- Assign field number for start of data values based on whether
!     sector-varying values are used
   if (L_NOxSector) then
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
         if (iset <= inoxmax(inoxsect)) then
            noxvary(iset,inoxsect) = dnum
            if (dnum < 0.0d0) then
!                 WRITE Error Message:  Negative Value for NOX_VALS
               call errhdl(path,modnam,'E','209',keywrd)
            end if
         else
!              WRITE Error Message    ! Too Many NOX_VALS Input
            if (L_NOxSector) then
               write(dummy,'(''NOXVAL SECT'',I1)') inoxsect
            else
               write(dummy,'(''NOX_VALS'')')
            end if
            call errhdl(path,modnam,'E','231',dummy)
            go to 99
         end if
      end do
   end do

99 continue

! --- Save counter on number of values input so far for this sector
   inoxset(inoxsect) = iset

   return
end subroutine noxfill

subroutine ozon_unit
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'OZON_UNIT'

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
   if (field(3)=='PPM' .or. field(3)=='PPB' .or.&
   &field(3)=='UG/M3') then
      OzoneUnits = field(3)
   else
!        Write Error Message:  Invalid units for O3VALUES
      call errhdl(path,modnam,'E','203','OzoneUnits')
   end if

999 return
end subroutine ozon_unit

subroutine nox_unit
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'NOX_UNIT'

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
   if (field(3)=='PPM' .or. field(3)=='PPB' .or.&
   &field(3)=='UG/M3') then
      NOxUnits = field(3)
   else
!        Write Error Message:  Invalid units for NOX_VALS
      call errhdl(path,modnam,'E','203','NOxUnits')
   end if

999 return
end subroutine nox_unit

subroutine o3sector
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
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   logical :: L_BadData

!     Variable Initializations
   modnam = 'O3SECTOR'
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

! --- Set L_O3Sector logical variable
   L_O3Sector = .true.

   do i = 3, ifc
!        Loop through fields for starting directions for each O3SECTOR
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         write(dummy,'("O3SECT",I1)') i-2
         call errhdl(path,modnam,'E','208',dummy)
!           Assign logical variable for bad data, but cycle through full record
         L_BadData = .true.
         cycle
      end if
      o3sect(i-2) = dnum
      if (o3sect(i-2) < 0.0d0 .or. o3sect(i-2) > 360.0d0) then
!           Sector value out-of-range
         if (o3sect(i-2) > 9999.0d0) then
            write(dummy,'("O3SECT>9999.")')
         else if (o3sect(i-2) < -999.0d0) then
            write(dummy,'("O3SECT<-999.")')
         else
            write(dummy,'("O3SECT=",F5.0)') o3sect(i-2)
         end if
         call errhdl(path,modnam,'E','380',dummy)
      end if
   end do

! --- Check for presence of bad sector data
   if (L_BadData) go to 999

! --- Assign variable for number of user-specified background O3 sectors
   NUMO3Sects = ifc-2

! --- Check O3SECTs for proper order and minimum sector widths
   do i = 1, NUMO3Sects-1
      if (o3sect(i+1) < o3sect(i) ) then
!           Sector value out-of-order
         write(dummy,'("O3SECT",I1," < #",I1)') i+1, i
         call errhdl(path,modnam,'E','222',dummy)
      else if (o3sect(i+1) < o3sect(i)+30.0d0 ) then
!           Sector width < 30 degrees
         write(dummy,'("O3SECT",I1," < 30")') i+1
         call errhdl(path,modnam,'E','227',dummy)
      else if (o3sect(i+1) < o3sect(i)+60.0d0 ) then
!           Sector width < 60 degrees
         write(dummy,'("O3SECT",I1," < 60")') i+1
         call errhdl(path,modnam,'W','227',dummy)
      end if
   end do
! --- Now check for width of last sector
   if ( (o3sect(1)+360.0d0)-o3sect(NUMO3Sects) < 30.0d0) then
!        Sector width < 30 degrees
      write(dummy,'("O3SECT",I1," < 30")') NUMO3Sects
      call errhdl(path,modnam,'E','227',dummy)
   else if ( (o3sect(1)+360.0d0)-o3sect(NUMO3Sects) < 60.0d0) then
!        Sector width < 60 degrees
      write(dummy,'("O3SECT",I1," < 60")') NUMO3Sects
      call errhdl(path,modnam,'W','227',dummy)
   end if

999 return
end subroutine o3sector

subroutine noxsector
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
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   logical :: L_BadData

!     Variable Initializations
   modnam = 'NOXSECTOR'
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

! --- Set L_NOXSector logical variable
   L_NOxSector = .true.

   do i = 3, ifc
!        Loop through fields for starting directions for each NOXSECTOR
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         write(dummy,'("NOXSECT",I1)') i-2
         call errhdl(path,modnam,'E','208',dummy)
!           Assign logical variable for bad data, but cycle through full record
         L_BadData = .true.
         cycle
      end if
      noxsect(i-2) = dnum
      if(noxsect(i-2) < 0.0d0 .or. noxsect(i-2) > 360.0d0)then
!           Sector value out-of-range
         if (noxsect(i-2) > 999.0d0) then
            write(dummy,'("NOXSECT>999.")')
         else if (noxsect(i-2) < -99.0d0) then
            write(dummy,'("NOXSECT<-99.")')
         else
            write(dummy,'("NOXSECT=",F4.0)') noxsect(i-2)
         end if
         call errhdl(path,modnam,'E','380',dummy)
      end if
   end do

! --- Check for presence of bad sector data
   if (L_BadData) go to 999

! --- Assign variable for number of user-specified background NOX sectors
   NUMNOxSects = ifc-2

! --- Check NOxSECTs for proper order and minimum sector widths
   do i = 1, NUMNOXSects-1
      if (noxsect(i+1) < noxsect(i) ) then
!           Sector value out-of-order
         write(dummy,'("NOXSCT",I1," < #",I1)') i+1, i
         call errhdl(path,modnam,'E','222',dummy)
      else if (noxsect(i+1) < noxsect(i)+30.0d0 ) then
!           Sector width < 30 degrees
         write(dummy,'("NOXSCT",I1," < 30")') i+1
         call errhdl(path,modnam,'E','227',dummy)
      else if (noxsect(i+1) < noxsect(i)+60.0d0 ) then
!           Sector width < 60 degrees
         write(dummy,'("NOXSCT",I1," < 60")') i+1
         call errhdl(path,modnam,'W','227',dummy)
      end if
   end do
! --- Now check for width of last sector
   if ( (noxsect(1)+360.0d0)-noxsect(NUMNOxSects) < 30.0d0) then
!        Sector width < 30 degrees
      write(dummy,'("NOXSCT",I1," < 30")') NUMNOxSects
      call errhdl(path,modnam,'E','227',dummy)
   else if ((noxsect(1)+360.0d0)-noxsect(NUMNOxSects)<60.0d0) then
!        Sector width < 60 degrees
      write(dummy,'("NOXSCT",I1," < 60")') NUMNOxSects
      call errhdl(path,modnam,'W','227',dummy)
   end if

999 return
end subroutine noxsector

subroutine low_wnd
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
   use main1
   implicit none
   logical  :: L_Error
   character :: modnam*12

!     Variable Initializations
   modnam = 'LOW_WIND'
   L_Error = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Too Few Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
!     Wood 3/18/2022 D127 Increased number of parameters by one to allow for FRANMIN
!CRT  4/11/2022 D131 Increased number of parameters by one to allow for PBAL FRAN option
   else if (ifc > 9) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Get Minimum Sigma_V value, SVMIN
   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      L_Error = .true.
   else
      L_UserSVmin = .true.
      svmin = dnum
   end if

!     Check for acceptable range for SVMIN
   if (.not. L_Error) then
      if (svmin < 0.01d0 .or. svmin > 1.001d0) then
         write(dummy,'("SVMIN=",F5.2)') svmin
         call errhdl(path,modnam,'E','380',dummy)
         L_Error = .true.
      else
!           Issue warning message with new SVMIN
         write(dummy,'(F6.4)') svmin
         call errhdl(path,modnam,'W','111',dummy)
      end if
   end if

   L_Error = .false.

   if (ifc >= 4) then
!        Get Minimum wind speed value, WSMIN
      call stodbl(field(4),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         L_Error = .true.
      else
         L_UserWSmin = .true.
         wsmin = dnum
      end if
!        Check for acceptable range for WSMIN
      if (.not. L_Error) then
         if (wsmin < 0.01d0 .or. wsmin > 1.001d0) then
            write(dummy,'("WSMIN=",F5.2)') wsmin
            call errhdl(path,modnam,'E','380',dummy)
         else
!              Issue warning message with new WSMIN
            write(dummy,'(F6.4)') wsmin
            call errhdl(path,modnam,'W','112',dummy)
         end if
      end if
   end if

   L_Error = .false.

   if (ifc >= 5) then
!        Get maximum meander factor, FRANMAX
      call stodbl(field(5),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         L_Error = .true.
      else
         franmax = dnum
         L_UserFRANmax = .true.
      end if
!        Check for acceptable range for FRANMAX
      if (.not. L_Error) then
         if (franmax < 0.0d0 .or. franmax > 1.0d0) then
            write(dummy,'("FRANMAX=",F4.2)') franmax
            call errhdl(path,modnam,'E','380',dummy)
         else
!              Issue warning message with new FRANMAX
            write(dummy,'(F6.4)') franmax
            call errhdl(path,modnam,'W','113',dummy)
         end if
      end if
   end if

! CRT 9/11/2020, D062 User Minimum Sigma W
   if (ifc >= 6) then
!        Get minimum sigma w, SWMIN
      call stodbl(field(6),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         L_Error = .true.
      else
         L_UserSWmin = .true.
         swmin = dnum
      end if
!        Check for acceptable range for SWMIN
      if (.not. L_Error) then
         if (swmin < 0.0d0 .or. swmin > 3.0d0) then
            write(dummy,'("SWMIN=",F4.2)') swmin
            call errhdl(path,modnam,'E','380',dummy)
         else
!              Issue warning message with new SWMIN
            write(dummy,'(F6.4)') swmin
            call errhdl(path,modnam,'W','127',dummy)
         end if
      end if
   end if


! RCO 9/28/2020, D061 User BIGT
   if (ifc >= 7) then
!        Get BIGT
      call stodbl(field(7),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         L_Error = .true.
      else
         L_UserBigT = .true.
         bigt = dnum
      end if
!        Check for acceptable range for BIGT
      if (.not. L_Error) then
         if (bigt < 0.5d0 .or. bigt > 48.0d0) then
            write(dummy,'("BIGT=",F4.2)') bigt
            call errhdl(path,modnam,'E','380',dummy)
         else
!              Issue warning message with new BIGT
            write(dummy,'(F6.2)') bigt
            call errhdl(path,modnam,'W','129',dummy)
         end if
      end if
   end if

!     Wood 3/18/22 D127 Added FRANMIN
   if (ifc >= 8) then
!        Get maximum meander factor, FRANMIN
      call stodbl(field(8),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         L_Error = .true.
      else
         L_UserFRANmin = .true.
         franmin = dnum
      end if
!        Check for acceptable range for FRANMIN and less than FRANMAX
      if (.not. L_Error) then
!           FRANMIN cannot be < 0 or > 100
         if (franmin < 0.0d0 .or. franmin > 1.0d0) then
            write(dummy,'("FRANMIN=",F4.2)') franmin
            call errhdl(path,modnam,'E','380',dummy)
!           FRANMIN cannot be > FRANMAX
         else if (franmin > franmax) then
            write(dummy,'(F4.2," > ",F4.2)') franmin, franmax
            call errhdl(path,modnam,'E','426',dummy)
!              Issue warning message with new FRANMIN
            write(dummy,'(F6.4)') franmin
            call errhdl(path,modnam,'W','117',dummy)
         end if
      end if
   end if

!CRT  4/11/2022, D131 FRAN Alpha Formulation - Momentum Balance (PBal)
   if (ifc >= 9) then
!        Get PBal - momentum balance FRAN option
      if (field(9) == 'PBAL' .or.&
      &field(9) == 'PBALANCE') then
         L_PBal = .true.
!           Issue warning message with new BIGT
         call errhdl(path,modnam,'W','128','')
      else
         L_PBal = .false.

! ---       Write Error Message:Illegal Parameter Field
         Dummy = field(9)
         call errhdl(path,modnam,'E','203',Dummy)
         L_Error = .true.
      end if
   end if


999 return
end subroutine low_wnd

subroutine awma_downwash
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'AWMA_DOWNWASH'

! --- Initialize logical variables
!     By default, all buildings are assumed to be rectangular
!       (L_RECT_BLDG = .TRUE.); if streamlined buildings is specified
!       (with the parameter 'STREAMLINE' or 'STREAMLINED'),
!       L_RECT_BLDG will be set to .FALSE.

!     L_AWMADW indicates that the AWMADW keyword is being used in the
!       control file

   l_awmadw       = .false.
   l_rect_bldg    = .true.        ! Default is rectangular structures
   l_strmln_bldg  = .false.
   L_AWMA_Ueff  = .false.
   L_AWMA_UTurb = .false.
   L_AWMA_Entrain = .false.
   L_AWMA_UTurbHX = .false.

!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
!CRT  CRT 2/2/2021: D059 Update max number of fields allowed for new options
!CRT  Number increased from 5 to 7 for two newest options (AWMAUTurbHX and AWMAEntrain).
!CRT  If both AWMAUTurb and AWMAUTurbHX are specified, warning message is output and
!CRT  AWMAUTurbHX is used (overrides AWMAUTurb).
!CRT      ELSE IF (IFC .GT. 5) THEN
   else if (ifc > 7) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   l_awmadw = .true.

   if (ifc == 3) then
! ---    Only one parameter specified
      if (field(3) == 'STREAMLINE' .or.&
      &field(3) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(3) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(3) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(3) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(3) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if
   end if

   if (ifc == 4) then
! ---    Two parameters specified
      if (field(3) == 'STREAMLINE' .or.&
      &field(3) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(3) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(3) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(3) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(3) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if

      if (field(4) == 'STREAMLINE' .or.&
      &field(4) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(4) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(4) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(4) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(4) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(4))
      end if

! ---    Check for duplicate parameters
      if (field(3) == field(4)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if
   end if

   if (ifc == 5) then
!        Three parameters specified
      if (field(3) == 'STREAMLINE' .or.&
      &field(3) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(3) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(3) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(3) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(3) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if

      if (field(4) == 'STREAMLINE' .or.&
      &field(4) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(4) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(4) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(4) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(4) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(4))
      end if

      if (field(5) == 'STREAMLINE' .or.&
      &field(5) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(5) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(5) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(5) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(5) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

! ---    Check for duplicate parameters
      if (field(3) == field(4)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(5)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(5)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if
   endif

!CRT  2/2/2021: D059 - AWMA downwash alpha options AWMAEntrain and AWMAUTurbHX
!RLP  Addition of options requires checking for 5 possible
!CRT  Number of fields increased from 5 to 7 for two newest options
!CRT  (AWMAUTurbHX and AWMAEntrain). If both AWMAUTurb and AWMAUTurbHX are
!CRT  specified, warning message is output and AWMAUTurbHX is used.
   if (ifc == 6) then

!        Four parameters specified
      if (field(3) == 'STREAMLINE' .or.&
      &field(3) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(3) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(3) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(3) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(3) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if

      if (field(4) == 'STREAMLINE' .or.&
      &field(4) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(4) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(4) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(4) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(4) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(4))
      end if

      if (field(5) == 'STREAMLINE' .or.&
      &field(5) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(5) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(5) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(5) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(5) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

      if (field(6) == 'STREAMLINE' .or.&
      &field(6) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(6) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(6) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(6) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(6) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

! ---    Check for duplicate parameters
!        Additional checks added with the addition of the AWMAEntrain
!         downwash option
      if (field(3) == field(4)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(5)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(6)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(5)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(6)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(5) == field(6)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

   endif

!CRT  2/2/2021: D059 - AWMA downwash alpha options AWMAEntrain and AWMAUTurbHX
!RLP  Addition of options requires checking for 5 possible
!CRT  Number of fields increased from 5 to 7 for two newest options
!CRT  (AWMAUTurbHX and AWMAEntrain). If both AWMAUTurb and AWMAUTurbHX are
!CRT  specified, warning message is output and AWMAUTurbHX is used.
   if (ifc == 7) then
!        Five, i.e. all, parameters specified
      if (field(3) == 'STREAMLINE' .or.&
      &field(3) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(3) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(3) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(3) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(3) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if

      if (field(4) == 'STREAMLINE' .or.&
      &field(4) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(4) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(4) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(4) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(4) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(4))
      end if

      if (field(5) == 'STREAMLINE' .or.&
      &field(5) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(5) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(5) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(5) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(5) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

      if (field(6) == 'STREAMLINE' .or.&
      &field(6) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(6) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(6) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(6) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(6) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

      if (field(7) == 'STREAMLINE' .or.&
      &field(7) == 'STREAMLINED') then
! ---       Process all structures as streamlined buildings
         l_strmln_bldg = .true.
         l_rect_bldg = .false.
      else if (field(7) == 'AWMAUEFF') then
         L_AWMA_Ueff = .true.
      else if (field(7) == 'AWMAUTURB') then
         L_AWMA_UTurb = .true.
      else if (field(7) == 'AWMAENTRAIN') then
!RLP        Change beta0 and betap from 0.60 t0 0.35
         L_AWMA_Entrain = .true.
      else if (field(7) == 'AWMAUTURBHX') then
!RLP        In WAKE_TURB, get plume rise from trajectory arrays
         L_AWMA_UTurbHX = .true.
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

! ---    Check for duplicate parameters
!        Additional checks added with the addition of the AWMAUturbHX
!         downwash option
      if (field(3) == field(4)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(5)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(6)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(7)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(5)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(6)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(7)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(5) == field(6)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(5) == field(7)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(6) == field(7)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

   endif

!CRT  6/14/2019: STREAMLINE option requires AWMAUTURB
!CRT  Issue error if STREAMLINE flag is TRUE and
!CRT  AWMAUTURB flag is FALSE.
!JAT  6/20/2020:  ISSUE D53 ADDED FROM 19191
!     GIVE ERROR CODE NEW NUMBER, 126 TO NOT
!     CONFLICT WITH 125 (PATHS NOT FINISHED)
!CRT  2/2/2021: STREAMLINE should also work with AWMAUTurbHX option
!CRT  Update conditional statement to include L_AWMA_UTurbHX
   if (l_strmln_bldg .and.&
   &(.not. L_AWMA_UTurb .and. .not. L_AWMA_UTurbHX)) then
!        WRITE Error Message    ! AWMADWUTurb option required
!         CALL ERRHDL(PATH,MODNAM,'E','125',KEYWRD)
      call errhdl(path,modnam,'E','126',keywrd)
   end if

999 return
end subroutine awma_downwash

subroutine ord_downwash
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'PRIME_ORD'

   l_orddw    = .false.
   L_ORD_Cav  = .false.
   L_ORD_Ueff = .false.
   L_ORD_Turb = .false.


!     Check The Number Of The Fields
   if (ifc <= 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   l_orddw = .true.

! --- Decode the parameters to use in the calculations
   if (ifc == 3) then
!        Only one parameter specified
      if (field(3) == 'ORDCAV') then
         L_ORD_Cav  = .true.
      else if (field(3) == 'ORDUEFF') then
         L_ORD_Ueff = .true.
      else if (field(3) == 'ORDTURB') then
         L_ORD_Turb = .true.
      else
! ---       WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if
   end if

   if (ifc == 4) then
!        Two parameters specified - check each
      if (field(3) == 'ORDCAV') then
         L_ORD_Cav  = .true.
      else if (field(3) == 'ORDUEFF') then
         L_ORD_Ueff = .true.
      else if (field(3) == 'ORDTURB') then
         L_ORD_Turb = .true.
      else
! ---       WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if

      if (field(4) == 'ORDCAV') then
         L_ORD_Cav  = .true.
      else if (field(4) == 'ORDUEFF') then
         L_ORD_Ueff = .true.
      else if (field(4) == 'ORDTURB') then
         L_ORD_Turb = .true.
      else
! ---       WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(4))
      end if

! ---    Check for duplicate parameters
      if (field(3) == field(4)) then
!           WRITE Error Message    ! Duplicate Option on KEYWORD
         call errhdl(path,modnam,'E','121',keywrd)
      end if

   end if

   if (ifc == 5) then
!        All three parameters specified
      if (field(3) == 'ORDCAV') then
         L_ORD_Cav  = .true.
      else if (field(3) == 'ORDUEFF') then
         L_ORD_Ueff = .true.
      else if (field(3) == 'ORDTURB') then
         L_ORD_Turb = .true.
      else
! ---       WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(3))
      end if

      if (field(4) == 'ORDCAV') then
         L_ORD_Cav  = .true.
      else if (field(4) == 'ORDUEFF') then
         L_ORD_Ueff = .true.
      else if (field(4) == 'ORDTURB') then
         L_ORD_Turb = .true.
      else
! ---       WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(4))
      end if

      if (field(5) == 'ORDCAV') then
         L_ORD_Cav  = .true.
      else if (field(5) == 'ORDUEFF') then
         L_ORD_Ueff = .true.
      else if (field(5) == 'ORDTURB') then
         L_ORD_Turb = .true.
      else
! ---       WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203',field(5))
      end if

! ---    Check for duplicate parameters
      if (field(3) == field(4)) then
!           WRITE Error Message    ! Duplicate Option
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(4) == field(5)) then
!           WRITE Error Message    ! Duplicate Option
         call errhdl(path,modnam,'E','121',keywrd)
      end if

      if (field(3) == field(5)) then
!           WRITE Error Message    ! Duplicate Option
         call errhdl(path,modnam,'E','121',keywrd)
      end if

   end if

999 return
end subroutine ord_downwash
