subroutine mecard
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, nd, ndys

!     Variable Initializations
   modnam = 'MECARD'

   if (keywrd == 'STARTING') then
!        Set Status Switch
      imstat(1) = imstat(1) + 1
      if (imstat(1) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if

   else if (keywrd == 'SURFFILE') then
!        Set Status Switch
      imstat(2) = imstat(2) + 1
      if (imstat(2) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Surface Meteorology File Information    ---   CALL SURFIL
         call surfil
      end if

   else if (keywrd == 'PROFFILE') then
!        Set Status Switch
      imstat(3) = imstat(3) + 1
      if (imstat(3) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Profile Meteorology File Information    ---   CALL PROFIL
         call profil
      end if

   else if (keywrd == 'SURFDATA') then
!        Set Status Switch
      imstat(4) = imstat(4) + 1
      if (imstat(4) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Surface Data Information                ---   CALL SFDATA
         call sfdata
      end if

   else if (keywrd == 'UAIRDATA') then
!        Set Status Switch
      imstat(5) = imstat(5) + 1
      if (imstat(5) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Upper Air Data Information              ---   CALL UADATA
         call uadata
      end if

   else if (.not.evonly .and. keywrd == 'STARTEND') then
!        Set Status Switch
      imstat(6) = imstat(6) + 1
      if (scim) then
!           Write out error message:  STARTEND cannot be used with SCIM option
         call errhdl(path,modnam,'E','154',keywrd)
      else
         if (imstat(6) /= 1) then
!              WRITE Error Message: Non-repeatable Keyword
            call errhdl(path,modnam,'E','135',keywrd)
         else
!              Process Start and End Dates for Reading      ---   CALL STAEND
            call staend
         end if
      end if

   else if (.not.evonly .and. keywrd == 'DAYRANGE') then
!        Set Status Switch
      imstat(7) = imstat(7) + 1
      if (scim) then
!           Write out error message:  DAYRANGE cannot be used with SCIM option
         call errhdl(path,modnam,'E','154',keywrd)
      else
!           Check for First Occurrence of DAYRANGE Card, and
!           Reinitialize IPROC and IPROCL Arrays to 0's
         if (imstat(7) == 1) then
            iproc(:)  = 0
            iprocl(:) = 0
         end if
!           Process Days and Day Ranges for Processing      ---   CALL DAYRNG
         call dayrng
      end if

   else if (keywrd == 'WDROTATE') then
!        Set Status Switch
      imstat(8) = imstat(8) + 1
      if (imstat(8) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Wind Direction Correction Option        ---   CALL WDROTA
         call wdrota
      end if

   else if (keywrd == 'SITEDATA') then
!        Set Status Switch
      imstat(9) = imstat(9) + 1
      if (imstat(9) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process On-site Data Information                ---   CALL ONDATA
         call ondata
      end if

   else if (keywrd == 'PROFBASE') then
!        Set Status Switch
      imstat(10) = imstat(10) + 1
      if (imstat(10) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process On-site Data Information                ---   CALL PRBASE
         call prbase
      end if

   else if (keywrd == 'WINDCATS') then
!        Set Status Switch
      imstat(11) = imstat(11) + 1
      if (imstat(11) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Wind Speed Categories                   ---   CALL WSCATS
         call wscats
      end if

   else if (keywrd == 'SCIMBYHR' .and. scim) then
!        Set Status Switch
      imstat(12) = imstat(12) + 1
      if (imstat(12) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Wind Speed Categories                   ---   CALL SCIMIT
         call scimit
      end if

   else if (keywrd == 'NUMYEARS') then
!        Set Status Switch
      imstat(13) = imstat(13) + 1
      if (imstat(13) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process Number of Years for MAXDCONT arrays     ---  CALL NUMYR
         call numyr
      end if

!     JAT 1/29/21 ISSUE D070 TURBULENCE OPTIONS
!     ADD CHECK FOR ONE OF THE TURBULENCE KEYWORDS
   else if (keywrd == 'NOTURB  ' .or. keywrd == 'NOTURBST' .or.&
   &keywrd == 'NOTURBCO' .or. keywrd == 'NOSA    ' .or.&
   &keywrd == 'NOSW    ' .or. keywrd == 'NOSAST  ' .or.&
   &keywrd == 'NOSWST  ' .or. keywrd == 'NOSACO  ' .or.&
   &keywrd == 'NOSWCO  ') then
!        Set Status Switch
      imstat(14) = imstat(14) + 1
      if (imstat(14) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else
!           Process turbulence option     ---  CALL TURBOPTS
         call turbopt
      end if
   else if (keywrd == 'FINISHED') then
!        Set Status Switch
      imstat(50) = imstat(50) + 1
      if (imstat(50) /= 1) then
!           WRITE Error Message: Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
         go to 999
      end if
!        Write Error Messages for Missing Mandatory Keyword(s)
      if (imstat(1) == 0) then
         call errhdl(path,modnam,'E','130','STARTING')
      end if
      if (imstat(2) == 0) then
         call errhdl(path,modnam,'E','130','SURFFILE')
      end if
      if (imstat(3) == 0) then
         call errhdl(path,modnam,'E','130','PROFFILE')
      end if
      if (imstat(4) == 0) then
         call errhdl(path,modnam,'E','130','SURFDATA')
      end if
      if (imstat(5) == 0) then
         call errhdl(path,modnam,'E','130','UAIRDATA')
      end if
      if (imstat(10) == 0) then
         call errhdl(path,modnam,'E','130','PROFBASE')
      end if
      if (scim .and. imstat(12) == 0) then
         call errhdl(path,modnam,'E','130','SCIMBYHR')
      end if

!        OPEN Met Data File                                 ---   CALL MEOPEN
      if (imstat(2) /= 0 .and. imstat(3) /= 0) then
         call meopen
      end if

! ---    Assign L_LeapYear variable
      if ((mod(isyear,4) /= 0) .or.&
      &(mod(isyear,100) == 0 .and. mod(isyear,400) /= 0)) then
!           Not a Leap Year
         L_LeapYear = .false.
      else
!           Leap Year
         L_LeapYear = .true.
      end if

      if (multyr) then
!           Set the Increment for Saving Results, INCRST, Based on
!           ISYEAR, Surface Data Year, from SURFDATA Keyword
         if ((mod(isyear,4) /= 0) .or.&
         &(mod(isyear,100)==0 .and. mod(isyear,400)/=0)) then
!              Not a Leap Year
            incrst = 365
         else
!              Leap Year
            incrst = 366
         end if
      end if

!        Determine Number of Hours to be Processed, NHOURS, For Use
!        With the TOXXFILE Option - 9/29/92
      if ((mod(isyear,4) /= 0) .or.&
      &(mod(isyear,100)==0 .and. mod(isyear,400)/=0)) then
!           Not a Leap Year
         nd = 365
      else
!           Leap Year
         nd = 366
      end if
      ndys = 0
      do i = 1, nd
! ---       Adjust for leap year vs. non-leap year
         if (nd == 365) then
            if (iproc(i) == 1) then
               ndys = ndys + 1
            end if
         else if (nd == 366) then
            if (iprocl(i) == 1) then
               ndys = ndys + 1
            end if
         end if
      end do
      nhours = ndys * 24

   else
!        Write Error Message: Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

999 return
end subroutine mecard

subroutine surfil
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SURFIL'

   if (ifc == 3) then
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         metinp = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  METINP Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
      end if
   else if (ifc == 4) then
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         metinp = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  METINP Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
      end if
      if (field(4) /= 'FREE') then
!           WRITE Warning Message         ! Format field no longer used
         call errhdl(path,modnam,'W','293','format')
      end if
   else if (ifc > 4) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! No Parameters Specified
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine surfil

subroutine profil
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'PROFIL'

   if (ifc == 3) then
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         proinp = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  PROINP Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
      end if
   else if (ifc == 4) then
!        Retrieve Met Data Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         proinp = runst1(locb(3):loce(3))
      else
!           WRITE Error Message:  PROINP Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
      end if
      if (field(4) /= 'FREE') then
!           WRITE Warning Message         ! Format field no longer used
         call errhdl(path,modnam,'W','293','format')
      end if
   else if (ifc > 4) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! No Parameters Specified
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine profil

subroutine sfdata
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SFDATA'

   if (ifc == 2) then
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        WRITE Error Message           ! Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   call stonum(field(3),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      idsurf = 0
      go to 199
   end if
   idsurf = nint(fnum)

199 call stonum(field(4),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      isyear = 0
      go to 299
   end if
   isyear = nint(fnum)

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert ISYEAR to Four Digits
   if (isyear <= 99) then
      call cent_date(isyear,isyear)
   end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C     Check for 2-digit Input and Convert ISYEAR to Four Digits
!      IF (ISYEAR .GE. ISTRT_WIND .and. ISYEAR .LE. 99) THEN
!         ISYEAR = ISTRT_CENT*100 + ISYEAR
!      ELSE IF (ISYEAR .LT. ISTRT_WIND) THEN
!         ISYEAR = (ISTRT_CENT+1)*100 + ISYEAR
!      END IF

299 if (ifc >= 5) then
!        Retrieve Surface Data Station Name (Optional)
      sfname = field(5)
   else
      sfname = 'UNKNOWN'
   end if

   if (ifc == 7) then
!        Retrieve Coordinates for Surface Data Location (Optional)
      call stodbl(field(6),ilen_fld,sfx,imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      end if
      call stodbl(field(7),ilen_fld,sfy,imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      end if
   end if

999 return
end subroutine sfdata

subroutine uadata
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'UADATA'

   if (ifc == 2) then
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        WRITE Error Message           ! Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   call stonum(field(3),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      iduair = 0
      go to 199
   end if
   iduair = nint(fnum)

199 call stonum(field(4),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      iuyear = 0
      go to 299
   end if
   iuyear = nint(fnum)

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert IUYEAR to Four Digits
   if (iuyear <= 99) then
      call cent_date(iuyear,iuyear)
   end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C     Convert IUYEAR to Four Digits
!      IF (IUYEAR .GE. ISTRT_WIND .and. IUYEAR .LE. 99) THEN
!         IUYEAR = ISTRT_CENT*100 + IUYEAR
!      ELSE IF (IUYEAR .LT. ISTRT_WIND) THEN
!         IUYEAR = (ISTRT_CENT+1)*100 + IUYEAR
!      END IF

299 if (ifc >= 5) then
!        Retrieve Surface Data Station Name (Optional)
      uaname = field(5)
   else
      uaname = 'UNKNOWN'
   end if

   if (ifc == 7) then
!        Retrieve Coordinates for Surface Data Location (Optional)
      call stodbl(field(6),ilen_fld,uax,imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      end if
      call stodbl(field(7),ilen_fld,uay,imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      end if
   end if

999 return
end subroutine uadata

subroutine ondata
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'ONDATA'

   if (ifc == 2) then
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        WRITE Error Message           ! Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   call stonum(field(3),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 199
   end if
   idsite = nint(fnum)

199 call stonum(field(4),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 299
   end if
   ioyear = nint(fnum)

299 if (ifc >= 5) then
!        Retrieve Surface Data Station Name (Optional)
      onname = field(5)
   else
      onname = 'UNKNOWN'
   end if

   if (ifc == 7) then
!        Retrieve Coordinates for Surface Data Location (Optional)
      call stodbl(field(6),ilen_fld,onx,imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      end if
      call stodbl(field(7),ilen_fld,ony,imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      end if
   end if

999 return
end subroutine ondata

subroutine prbase
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'PRBASE'

   if (ifc == 3 .or. ifc == 4) then
      call stodbl(field(3),ilen_fld,zbase,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      if (ifc == 4 .and. field(4) == 'FEET') then
         zbase = 0.3048d0 * zbase
      else if (ifc == 4 .and. field(4) /= 'METERS') then
!           WRITE Warning Message - Invalid ZRUNIT Parameter
         call errhdl(path,modnam,'W','203','ZRUNIT')
      end if
      if (zbase < 0.0d0 .and. imit == 1) then
!           WRITE Warning Message - Possible Error In ZBASE
         call errhdl(path,modnam,'W','340',keywrd)
      end if
   else if (ifc > 4) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

!     Reinitialize AZS, AZELEV, and AZHILL arrays for FLAT terrain
   if (flat) then
      if (.not. flatsrcs) then
!           Assign ZBASE to source elevation for all sources
         azs = zbase
      else
!           Assign ZBASE to source elevation only for FLAT sources
         do isrc = 1, numsrc
            if (l_flatsrc(isrc)) then
               azs(isrc) = zbase
            end if
         end do
      end if
      if (.not. flatsrcs) then
!           Assign ZBASE to AZELEV and AZHILL for all receptors
         do irec = 1, numrec
            azelev(irec) = zbase
            azhill(irec) = zbase
         end do
      end if
   end if

999 return
end subroutine prbase

subroutine staend
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
   use main1
   implicit none
   character :: modnam*12

   integer :: idymax(12)

!     Variable Initializations
   data idymax/31,29,31,30,31,30,31,31,30,31,30,31/

   modnam = 'STAEND'

   if (ifc == 8) then
!        Process for YR, MD, DY
      call stonum(field(3),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 198
      end if
      isyr = nint(fnum)
198   call stonum(field(4),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 298
      end if
      ismn = nint(fnum)
      if (ismn < 1 .or. ismn > 12) then
!           WRITE Error Message    ! Invalid Month
         call errhdl(path,modnam,'E','203','MONTH')
      end if
298   call stonum(field(5),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 398
      end if
      isdy = nint(fnum)
      if (ismn >= 1 .and. ismn <= 12) then
         if (isdy < 1 .or. isdy > idymax(ismn)) then
!              WRITE Error Message    ! Invalid Day
            call errhdl(path,modnam,'E','203','DAY')
         end if
      end if
398   call stonum(field(6),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 498
      end if
      ieyr = nint(fnum)
498   call stonum(field(7),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 598
      end if
      iemn = nint(fnum)
      if (iemn < 1 .or. iemn > 12) then
!           WRITE Error Message    ! Invalid Month
         call errhdl(path,modnam,'E','203','MONTH')
      end if
598   call stonum(field(8),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 698
      end if
      iedy = nint(fnum)
      if (iemn >= 1 .and. iemn <= 12) then
         if (iedy < 1 .or. iedy > idymax(iemn)) then
!              WRITE Error Message    ! Invalid Day
            call errhdl(path,modnam,'E','203','DAY')
         end if
      end if
698   continue

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert ISYR to Four Digits
      if (isyr <= 99) then
         call cent_date(isyr,isyr)
      end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Convert ISYR and IEYR to Four Digits
!         IF (ISYR .GE. ISTRT_WIND .and. ISYR .LE. 99) THEN
!            ISYR = ISTRT_CENT*100 + ISYR
!         ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!            ISYR = (ISTRT_CENT+1)*100 + ISYR
!         END IF

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert IEYR to Four Digits
      if (ieyr <= 99) then
         call cent_date(ieyr,ieyr)
      end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!         IF (IEYR .GE. ISTRT_WIND .and. IEYR .LE. 99) THEN
!            IEYR = ISTRT_CENT*100 + IEYR
!         ELSE IF (IEYR .LT. ISTRT_WIND) THEN
!            IEYR = (ISTRT_CENT+1)*100 + IEYR
!         END IF

!        Calculate JULIAN Day for Start and End Dates
      call julian (isyr,ismn,isdy,isjday)
      call julian (ieyr,iemn,iedy,iejday)
!        Use 1 for Start Hour and 24 for End Hour
      ishr = 1
      iehr = 24
! ---    Calculate 10-digit start date (ISDATE) and end date (IEDATE)
!        including 4-digit year (for comparisons with FULLDATE)
      if (isyr <= 2147) then
         isdate = isyr*1000000 + ismn*10000 + isdy*100 + ishr
      else
         call errhdl(path,modnam,'E','365',keywrd)
         isdate = 2147123124
      end if
      if (ieyr <= 2147) then
         iedate = ieyr*1000000 + iemn*10000 + iedy*100 + iehr
      else
         call errhdl(path,modnam,'E','365',keywrd)
         iedate = 2147123124
      end if

   else if (ifc == 10) then
!        Process for YR, MD, DY, HR
      call stonum(field(3),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 199
      end if
      isyr = nint(fnum)
199   call stonum(field(4),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 299
      end if
      ismn = nint(fnum)
      if (ismn < 1 .or. ismn > 12) then
!           WRITE Error Message    ! Invalid Month
         call errhdl(path,modnam,'E','203','MONTH')
      end if
299   call stonum(field(5),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 399
      end if
      isdy = nint(fnum)
      if (ismn >= 1 .and. ismn <= 12) then
         if (isdy < 1 .or. isdy > idymax(ismn)) then
!              WRITE Error Message    ! Invalid Day
            call errhdl(path,modnam,'E','203','DAY')
         end if
      end if
399   call stonum(field(6),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 499
      end if
      ishr = nint(fnum)
      if (ishr < 1 .or. ishr > 24) then
!           WRITE Error Message    ! Invalid Hour
         call errhdl(path,modnam,'E','203','HOUR')
      end if
499   call stonum(field(7),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 599
      end if
      ieyr = nint(fnum)
599   call stonum(field(8),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 699
      end if
      iemn = nint(fnum)
      if (iemn < 1 .or. iemn > 12) then
!           WRITE Error Message    ! Invalid Month
         call errhdl(path,modnam,'E','203','MONTH')
      end if
699   call stonum(field(9),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 799
      end if
      iedy = nint(fnum)
      if (iemn >= 1 .and. iemn <= 12) then
         if (iedy < 1 .or. iedy > idymax(iemn)) then
!              WRITE Error Message    ! Invalid Day
            call errhdl(path,modnam,'E','203','DAY')
         end if
      end if
799   call stonum(field(10),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         go to 899
      end if
      iehr = nint(fnum)
      if (iehr < 1 .or. iehr > 24) then
!           WRITE Error Message    ! Invalid Hour
         call errhdl(path,modnam,'E','203','HOUR')
      end if
899   continue

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert ISYR to Four Digits
      if (isyr <= 99) then
         call cent_date(isyr,isyr)
      end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Convert ISYR and IEYR to Four Digits
!         IF (ISYR .GE. ISTRT_WIND .and. ISYR .LE. 99) THEN
!            ISYR = ISTRT_CENT*100 + ISYR
!         ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!            ISYR = (ISTRT_CENT+1)*100 + ISYR
!         END IF

!     D001 IF ISYEAR is 2-digits then call CENT_DATE to determine the Current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value
!     Check for 2-digit Input and Convert IEYR to Four Digits
      if (ieyr <= 99) then
         call cent_date(ieyr,ieyr)
      end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!         IF (IEYR .GE. ISTRT_WIND .and. IEYR .LE. 99) THEN
!            IEYR = ISTRT_CENT*100 + IEYR
!         ELSE IF (IEYR .LT. ISTRT_WIND) THEN
!            IEYR = (ISTRT_CENT+1)*100 + IEYR
!         END IF

!        Calculate JULIAN Day for Start and End Dates
      call julian (isyr,ismn,isdy,isjday)
      call julian (ieyr,iemn,iedy,iejday)
!        Calculate 10-digit start date (ISDATE) and end date (IEDATE)
      if (isyr <= 2147) then
         isdate = isyr*1000000 + ismn*10000 + isdy*100 + ishr
      else
         call errhdl(path,modnam,'E','365',keywrd)
         isdate = 2147123124
      end if
      if (ieyr <= 2147) then
         iedate = ieyr*1000000 + iemn*10000 + iedy*100 + iehr
      else
         call errhdl(path,modnam,'E','365',keywrd)
         iedate = 2147123124
      end if

   else if (ifc > 8) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999

   else
!        WRITE Error Message           ! Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   end if

!     Determine MN, DY, and HR for end-of-the-year check.
!     Subtract one from start hour to set end hour for the year of data
   if (ishr > 1) then
      iendhour = ishr - 1
      ienddy   = isdy
      iendmn   = ismn
   else
      iendhour = 24
      if (isdy > 1) then
         ienddy = isdy - 1
         iendmn = ismn
      else
         iendmn = ismn - 1
         if (iendmn == 0) iendmn = 12
         ienddy = idymax(iendmn)
      end if
   end if

!     Check for End Year .LT. Start Year
   if (ieyr < isyr) then
!        WRITE Error Message    ! Invalid End Year
      call errhdl(path,modnam,'E','203','END YEAR')
      go to 999
   end if

!     Check for STARTEND period less than a complete year if
!     ANNUAL average is specified
   if (annual .or. multyr .or. l_maxdcont) then
!        First check for End Year = Start Year,
!        then for End Year = Start Year + 1, otherwise
!        if End Year - Start Year > 1 no further checks needed
      if (ieyr == isyr) then
!           End Year equals Start Year, therefore the
!           Start Month, Start Day, Start Hour must be 01/01/01, and
!           End Month, End Day, End Hour must be 12/31/24
         if (ismn /=  1 .or. isdy /=  1 .or. ishr /=  1 .or.&
         &iemn /= 12 .or. iedy /= 31 .or. iehr /= 24) then
!              WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
            call errhdl(path,modnam,'E','480',keywrd)
         end if
      else if (ieyr - isyr == 1) then
!           End Year is Start Year plus 1, therefore the
!           End Month, End Day, End Hour must greater than or equal to
!           Start Month, Start Day, Start Hour
         if (iemn < iendmn) then
!              WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
            call errhdl(path,modnam,'E','480',keywrd)
         else if (iemn == iendmn) then
            if (iedy < ienddy) then
!                 WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
               call errhdl(path,modnam,'E','480',keywrd)
            else if (iedy == ienddy) then
               if (iehr < iendhour) then
!                    WRITE Error Message ! Incomplete Year for MULTYR or ANNUAL
                  call errhdl(path,modnam,'E','480',keywrd)
               end if
            end if
         end if
      end if
   end if

999 return
end subroutine staend

subroutine dayrng
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k, imn, idy, imn1, idy1, imn2, idy2, jdayb, jdaye
   character :: begrng*8, endrng*8, cmn1*8, cdy1*8, cmn2*8, cdy2*8
   character :: blnk08*8
   logical :: rmark, gmark

!     Variable Initializations
   data blnk08/'        '/

   modnam = 'DAYRNG'

   if (ifc < 3) then
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   else
      do 40 i = 3, ifc
!           First Check For Range Marker (-) And Gregorian Day Marker (/)
!           Initialize Character Fields
         begrng = blnk08
         endrng = blnk08
         cmn1 = blnk08
         cdy1 = blnk08
         cmn2 = blnk08
         cdy2 = blnk08
         call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,&
         &begrng,endrng)
         call fsplit(path,keywrd,begrng,8,'/',gmark,cmn1,cdy1)
         if (rmark .and. gmark) then
            call fsplit(path,keywrd,endrng,8,'/',gmark,cmn2,cdy2)
         end if

         if (.not.rmark .and. .not.gmark) then
!              Field Must Be a Single Julian Day
            call stonum(begrng,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               jday = nint(fnum)
            end if
            if (jday>=1 .and. jday<=366 .and. imit==1) then
               iproc(jday)  = 1
!                 Also need IPROCL array for Leap Years
               iprocl(jday) = 1
            else
!                 WRITE Error Message    ! Invalid Julian Day
               call errhdl(path,modnam,'E','203','Juli Day')
            end if
            if (jday<isjday .or. jday>iejday) then
!                 WRITE Warning Message  ! Julian Day Out-of-Range
               write(dummy,'(I8)') jday
               call errhdl(path,modnam,'W','350',dummy)
            end if

         else if (rmark .and. .not.gmark) then
!              Field Must Be a Julian Day Range - Extract Beg & End
            call stonum(begrng,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               jdayb = nint(fnum)
            end if
            call stonum(endrng,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               jdaye = nint(fnum)
            end if
            if ((jdayb <= jdaye) .and. (jdayb >= 1) .and.&
            &(jdaye <= 366)) then
               do k = jdayb, jdaye
                  iproc(k)  = 1
!                    Also need IPROCL array for Leap Years
                  iprocl(k) = 1
               end do
            else
!                 WRITE Error Message    ! Invalid Julian Day Range
               call errhdl(path,modnam,'E','203','Juli Day')
            end if
            if (jdayb<isjday .or. jdaye>iejday) then
!                 WRITE Warning Message  ! Julian Day Out-of-Range
               write(dummy,'(I3,"-",I3)') jdayb, jdaye
               call errhdl(path,modnam,'W','350',dummy)
            end if

         else if (.not.rmark .and. gmark) then
!              Field Must Be a Single Month/Day
            call stonum(cmn1,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               imn = nint(fnum)
            end if
            call stonum(cdy1,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               idy = nint(fnum)
            end if
! ---          Determine JULIAN Day Number; For Non-Leap Year First;
!              Note that JDAY for MN/DY inputs will be assigned based on
!              on the Year specified on the ME SURFFILE keyword. However,
!              the IPROCL array used to identify which Julian day(s) to be
!              processed for leap years will be assigned based on the MN/DY
!              input by the user.
            call julian(isyear,imn,idy,jday)

            if ( (mod(isyear,4) /= 0) .or.&
            &(mod(isyear,100) == 0 .and.&
            &mod(isyear,400) /= 0) ) then
!                 Not a Leap Year; Get JULIAN day number for specified MN/DY
! ---             Assign specified MN/DY to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               if (jday >= 1 .and. jday <= 365) then
                  iproc(jday) = 1
                  if (imn > 2) then
                     iprocl(jday+1) = 1
                  else
                     iprocl(jday) = 1
                  end if
               else
!                    WRITE Error Message    ! Invalid Julian Day
                  call errhdl(path,modnam,'E','203','Juli Day')
               end if
! ---             Check for consistency with STARTEND inputs, if provided
               if (jday<isjday .or. jday>iejday) then
!                    WRITE Warning Message  ! Julian Day Out-of-Range
                  write(dummy,'(I8)') jday
                  call errhdl(path,modnam,'W','350',dummy)
               end if

            else
! ---             Determine JULIAN Day Number; For Leap Year
! ---             Assign specified MD/DY to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               if (jday >= 1 .and. jday <= 366) then
                  iprocl(jday) = 1
                  if (imn > 2) then
                     iproc(jday-1) = 1
                  else
                     iproc(jday)   = 1
                  end if
               else
!                    WRITE Error Message    ! Invalid Julian Day
                  call errhdl(path,modnam,'E','203','Juli Day')
               end if
! ---             Check for consistency with STARTEND inputs, if provided
               if (jday<isjday .or. jday>iejday) then
!                    WRITE Warning Message  ! Julian Day Out-of-Range
                  write(dummy,'(I8)') jday
                  call errhdl(path,modnam,'W','350',dummy)
               end if
            end if

         else if (rmark .and. gmark) then
!              Field Must Be a Greg. Date Range (MN/DY-MN/DY)
            call stonum(cmn1,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 41
            else
               imn1 = nint(fnum)
            end if
            call stonum(cdy1,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 41
            else
               idy1 = nint(fnum)
            end if
41          call stonum(cmn2,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               imn2 = nint(fnum)
            end if
            call stonum(cdy2,8,fnum,imit)
!              Check The Numerical Field
            if (imit /= 1) then
               call errhdl(path,modnam,'E','208',keywrd)
               go to 40
            else
               idy2 = nint(fnum)
            end if

! ---          Determine JULIAN Day Number; For Non-Leap Year First
            if ( (mod(isyear,4) /= 0) .or.&
            &(mod(isyear,100) == 0 .and.&
            &mod(isyear,400) /= 0) ) then
!                 Not a Leap Year; Get JULIAN day numbers for specified
!                 Start MN/DY and End MN/DY, based on STARTEND
               call julian(isyear,imn1,idy1,jdayb)
               call julian(isyear,imn2,idy2,jdaye)

! ---             Assign specified MN/DY range to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               if ((jdayb <= jdaye) .and. (jdayb >= 1) .and.&
               &(jdaye <= 365)) then
! ---                Assign IPROC array for use with non-leap years
                  do k = jdayb, jdaye
                     iproc(k) = 1
                  end do
! ---                Assign IPROCL array for use with leap years
                  do k = jdayb, jdaye
                     iprocl(k) = 1
                  end do
! ---                Assign IPROCL = 1 for last day if JDAYE = 365
                  if (jdaye == 365) then
                     iprocl(366) = 1
                  end if
               else
!                    WRITE Error Message    ! Invalid Julian Day
                  call errhdl(path,modnam,'E','203','Juli Day')
               end if
! ---             Check for consistency with STARTEND inputs, if provided
! ---              D146 Changed conditional to reference JDAYB and JDAYE rather than JDAY WSP 10/18/22
               if (jdayb<isjday .or. jdaye>iejday) then
!                  IF (JDAY.LT.ISJDAY .or. JDAY.GT.IEJDAY) THEN

!                    WRITE Warning Message  ! Julian Day Out-of-Range
!  ---               D146 Changed warning to reference JDAYE and JDAYB rather than JDAY WSP 10/18/22
                  write(dummy,'(I3,"-",I3)') jdayb, jdaye
!                     WRITE(DUMMY,'(I8)') JDAY
                  call errhdl(path,modnam,'W','350',dummy)
               end if

            else
! ---             Determine JULIAN Day Number; For Leap Year
!                 Get JULIAN day numbers for specified
!                 Start MN/DY and End MN/DY
               call julian(isyear,imn1,idy1,jdayb)
               call julian(isyear,imn2,idy2,jdaye)
! ---             Assign specified MN/DY range to IPROC array (used for non-leap years)
!                 and IPROCL array (used for leap years)
               if ((jdayb <= jdaye) .and. (jdayb >= 1) .and.&
               &(jdaye <= 366)) then
                  do k = jdayb, jdaye
                     iprocl(k) = 1
                  end do
                  do k = jdayb, jdaye
                     if (k <= 59 ) then
                        iproc(k) = 1
                     else if (k > 60) then
! ---                      Adjust non-leapyear Jday array for March 1 - Dec 31
                        iproc(k-1) = 1
                     end if
                  end do
               else
!                    WRITE Error Message    ! Invalid Julian Day
                  call errhdl(path,modnam,'E','203','Juli Day')
               end if

               if (jdayb<isjday .or. jdaye>iejday) then
!                    WRITE Warning Message  ! Julian Day Out-of-Range
                  write(dummy,'(I3,"-",I3)') jdayb, jdaye
                  call errhdl(path,modnam,'W','350',dummy)
               end if

            end if

         else
!               WRITE Error Message    ! Invalid Field
            call errhdl(path,modnam,'E','203','DAYRANGE')
         end if

40    continue
   end if

   return
end subroutine dayrng

subroutine wdrota
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'WDROTA'

   rotang = 0.0d0

   if (ifc == 3) then
      call stodbl(field(3),ilen_fld,rotang,imit)
      if (imit /= 1) then
!            WRITE Error Message  ! Invalid Numeric Field Encountered
         call errhdl(path,modnam,'E','208',keywrd)
      else if (dabs(rotang) > 180.0d0) then
!            WRITE Error Message       ! ROTANG Out of Range
         call errhdl(path,modnam,'E','380','ROTANG')
      end if
   else if (ifc > 3) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine wdrota

subroutine wscats
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, iws

!     Variable Initializations
   modnam = 'WSCATS'

   if (ifc == 7) then
!        Fill UCAT Array
      do i = 3, ifc
         call stodbl(field(i),ilen_fld,dnum,imit)
         if (imit /= 1) then
!              WRITE Error Message  ! Invalid Numeric Field Encountered
            call errhdl(path,modnam,'E','208',keywrd)
         else if (dnum < 1.0d0 .or. dnum > 20.0d0) then
!              WRITE Error Message       ! UCAT Out of Range
            call errhdl(path,modnam,'E','380','UCAT')
         else
            iws = i - 2
            ucat(iws) = dnum
            if (iws>1 .and. ucat(iws)<=ucat(iws-1)) then
!                 WRITE Error Message    ! Invalid UCAT Value, LE Previous
               call errhdl(path,modnam,'E','203','UCAT')
            end if
         end if
      end do
   else if (ifc > 7) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine wscats

subroutine meopen
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
   use main1
   implicit none
   character :: modnam*12
   integer :: metver, iosi, issi, iusi
   integer :: level, jflag
   logical :: fopen, mfopen, mpopen

!     Set Parameters for AERMET Version Dates.
!     Using data > MDATE1 or < MDATE2 causes a fatal error message.
   integer, parameter :: mdate1 = 90000,  mdate2 = 12345
!     Using data < MDATE1, > MDATE2, and .NE. MDATE3 causes warning message.
   integer, parameter :: mdate3 = 14134

   character (len=6)   :: asosthresh

   character (len=8)   :: cusi, cssi, cosi
! Unused:      CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
   character (len=256) :: buffer

!     Variable Initializations
   modnam = 'MEOPEN'
   fopen  = .false.
   mfopen = .false.
   mpopen = .false.

! --- Initialize AERMOD version date (METVER) and ASOSTHRESH flag
   metver = 0
   asosthresh = '      '

!     File Unit Initialized in BLOCK DATA INIT
!     File Format Set By Keyword "SURFFILE" on "ME" pathway
!     OPEN Surface Met Data File --- Formatted is the only option
!     Open with ACTION='READ' to prevent overwrite and allow multiple access
!     READ In the Station Numbers for Comparison to SETUP File

!     Open SURFFILE Met File If Not Already Open
   inquire (file=metinp,opened=fopen)

   if (.not. fopen) then
!        Open SURFFILE Met File If Not Already Open
      inquire (unit=mfunit,opened=fopen)
      if (.not. fopen) then
!           Open with ACTION='READ' to prevent overwrite and allow multiple access
         open(unit=mfunit,file=metinp,status='OLD',&
         &err=998,action='READ',form='FORMATTED')
         mfopen = .true.

      else
         mfopen = .false.
!           SURFFILE Met File is Already Opened With Different Filename
         call errhdl(path,modnam,'E','501','SURFFILE')
      end if
   else
      mfopen = .true.
   end if

   go to 1000

!     Write Out Error Message for File OPEN Error
998 call errhdl(path,modnam,'E','500','SURFFILE')
!     Skip READ if there is an error opening file

1000 continue

! --- Next OPEN the PROFFILE Met File
!     File Format Set By Keyword "PROFFILE" on "ME" pathway
!     Open with ACTION='READ' to prevent overwrite and allow multiple access
!     OPEN Profile Met Data File --- Formatted is the only option

! --- Initialize FOPEN to .FALSE.
   fopen = .false.

!     Open PROFFILE Met File If Not Already Open
   inquire (file=proinp,opened=fopen)

   if (.not. fopen) then
!        Open PROFFILE Met File If Not Already Open
      inquire (unit=mpunit,opened=fopen)
      if (.not. fopen) then
!           Open with ACTION='READ' to prevent overwrite and allow multiple access
         open(unit=mpunit,file=proinp,status='OLD',&
         &err=999,action='READ',form='FORMATTED')
         mpopen = .true.

      else
         mpopen = .false.
!           PROFFILE Met File is Already Opened With Different Filename
         call errhdl(path,modnam,'E','501','PROFFILE')
      end if
   else
      mpopen = .true.
   end if

   go to 1001

!     Write Out Error Message for File OPEN Error
999 call errhdl(path,modnam,'E','500','PROFFILE')
   mpopen = .false.

1001 continue

   if( .not.mfopen .and. .not.mpopen )then
      goto 1003
   elseif( mfopen )then
      continue
   endif

! --- First read header record as character string to check for AERMET
!     options, THRESH_1MIN, ADJ_U*, CCVR_Sub, and/or TEMP_SUb
! --- Assign SURFFILE to DUMMY variable for read error message
   dummy = 'SURFFILE'
   if (mfopen) then
      read(mfunit,1200,err=99,iostat=ioerrn) buffer
1200  format(a256)
      if (ioerrn /= 0) goto 99 ! 16216 Added to check for empty file
   else
      go to 1002
   end if

! --- First extract AERMET version date, C_METVER
   if( index(buffer,'VERSION:') /= 0 )then
!        Extract AERMET version date
      read(buffer(index(buffer,'VERSION:')+8:&
      &index(buffer,'VERSION:')+13),'(A6)')&
      &c_metver
   elseif( buffer(93:98) /= '      ' )then
!        The 'VERSION:' keyword is missing so assign columns 93-98 to C_METVER
      c_metver = buffer(93:98)
   else
      c_metver = '      '
!        AERMET version not found in header record, issue fatal error message
      call errhdl(path,modnam,'E','395','No Version')
   endif

! --- Next check for THRESH_1MIN indicating that wind speed threshold was
!     applied to 1-minute ASOS wind data
   if( index(buffer,'THRESH_1MIN') /= 0 )then
!        Extract 1-min ASOS threshold value and write out Warning message
      read(buffer(index(buffer,'=')+1:&
      &index(buffer,'m/s')),*)&
      &asosthresh
      call errhdl(path,modnam,'W','186',asosthresh)
   endif

! --- Check for use of various met data processing options and issue
!     message as appropriate
   if( index(buffer,'ADJ_U*') /= 0 )then
!        Check for use of ADJ_U* option in AERMET
      L_AdjUstar = .true.
      call errhdl(path,modnam,'W','187',' ')
!        Assign keyword to MODOPS array
      modops(23) = 'ADJ_U*'
   endif

!     JAT 1/14/21 ISSUE D077; ADD PROG AS WELL AS MMIF TO ACCOMODATE NEW AERMET
!      IF( INDEX(BUFFER,'MMIF') .NE. 0 )THEN
   if( index(buffer,'MMIF') /= 0 .or. index(buffer,'PROG')&
   &.ne. 0 )then
!        Check for use of MMIF option in AERMET
      L_MMIF_Data = .true.
      call errhdl(path,modnam,'W','182','  ')
!        Assign keyword to MODOPS array
      modops(24) = 'MMIF_Data'
!         IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0 )THEN
      if( index(buffer,'MMIF VERSION') /= 0 .or.&
      &index(buffer,'PROG VERSION') /= 0 )then
!           Extract MMIF Version info to include in output file
         if( index(buffer,'MMIF VERSION') /= 0) then
            MMIF_Version = buffer( index(buffer,'MMIF VERSION'):&
            &index(buffer,'MMIF VERSION')+28 ) ! CRT 1/11/2021 D077, 26 to 28
         else
            MMIF_Version = buffer( index(buffer,'PROG VERSION'):&
            &index(buffer,'PROG VERSION')+28 ) ! CRT 1/11/2021 D077, 26 to 28
         endif
      else
         MMIF_Version = ''
      endif
   endif

!     CRT 8/9/2023 ISSUE D176; Add 'COARE' flag to accomodate COARE in AERMET
   if( index(buffer,'COARE') /= 0 )then
!        Set COARE logical to true and MODOPS
      l_coare = .true.
      modops(27) = 'COARE'
      call errhdl(path,modnam,'W','422','  ')
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
!        COARE met requires beta flag
!MGS         IF( .NOT. BETA )THEN
!MGS            CALL ERRHDL(PATH,MODNAM,'E','199','COARE Met')
!MGS         ENDIF
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
   endif

   if( index(buffer,'BULKRN') /= 0 )then
!        Check for use of BULKRN option in AERMET and assign logical variable;
!        Note that BULKRN is NOT a BETA or non-DFAULT option, and message is
!        issued for informational purposes
      l_bulkrn = .true.
!        Assign keyword to MODOPS array, unless MMIF is also being used
!        CRT 8/9/2023 ISSUE D176; Also cannot use BULKRN with COARE
!        CRT8/11/2023 ISSUE D176: Modify MODOPS to 27 for BULKRN, also used for COARE
!         IF( .NOT. L_MMIF_Data )THEN
      if( .not. l_coare )then
         modops(27) = 'BULKRN'
      else
         call errhdl(path,modnam,'E','423','  ')
      endif
! ---    Check for use of MMIF data and adjust BULKRN message accordingly
      if( L_MMIF_Data )then
         call errhdl(path,modnam,'W','181','with MMIF')
      else
         call errhdl(path,modnam,'W','181','in AERMET')
      endif
   endif

   if( index(buffer,'CCVR_Sub') /= 0 )then
! ---    Set logical flag indicating that CCVR_Sub option was used
      L_CCVR_Sub = .true.
   else
      L_CCVR_Sub = .false.
   endif

   if( index(buffer,'TEMP_Sub') /= 0 )then
! ---    Set logical flag indicating that TEMP_Sub option was used
      L_TEMP_Sub = .true.
   else
      L_TEMP_Sub = .false.
   endif

! --- Read Lat/Lon from header record BUFFER
   read(buffer,1900,err=99,iostat=ioerrn) alat, alon
1900 format(2a10)

! --- Now extract UA, SF, and OS station IDs from header record
   if( index(buffer,'UA_ID:') > 0 )then
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

   if( index(buffer,'SF_ID:') > 0 )then
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

   if( index(buffer,'OS_ID:') > 0 )then
      read(buffer(index(buffer,'OS_ID:')+7:&
      &index(buffer,'OS_ID:')+15),'(A)') cosi
   else
      cosi = '        '
   end if
   call stonum(cosi,8,fnum,imit)
   if (imit == 1) then
      iosi = nint(fnum)
   else
      iosi = 0
   end if

!     Check for valid version of meteorological data.
   if (screen .and. c_metver /= 'SCREEN') then
!        Check for use of screening meteorology under the SCREEN option
      call errhdl(path,modnam,'W','397',c_metver)
   else if (.not.screen .and. c_metver == 'SCREEN') then
!        Check for use of screening meteorology without SCREEN option
      call errhdl(path,modnam,'W','398','        ')
   else if (.not.screen) then
!        Read integer Julian date from character field for comparison to
!        acceptable AERMET version number.
      read(c_metver,'(1X,I5)',err=109) metver
      if (metver>mdate1 .or. metver<mdate2) then
! ---       Issue fatal error for use of invalid met version date
         write(dummy,'(2X,I5.5)') metver
         call errhdl(path,modnam,'E','395',dummy)
      else if (metver>=mdate2 .and. metver<mdate3) then
! ---       Issue warning message for use of "outdated" met version date
         write(dummy,'(2X,I5.5)') metver
         call errhdl(path,modnam,'W','396',dummy)
! ---       Set logical flag for use of "old" met data
         L_OldMetVer = .true.
      end if

      go to 110

109   continue
! ---    Error reading METVER date as integer from C_METVER character string;
!        issue warning message for "outdated" met version date; this allows for
!        interim version "dates" for AERMET drafts, e.g., 13DFT
      call errhdl(path,modnam,'W','396',c_metver)
! ---    Set logical flag for use of "old" met data
      L_OldMetVer = .true.
   end if

110 continue

! --- Check Station IDs in SURFFILE for agreement with ME pathway;
!     First check for blank fields in surface file header record
   if( len_trim(cssi) == 0 .and. idsurf /= 0 )then
!        Write Warning Message:  SURFDATA ID missing;
      call errhdl(path,modnam,'W','531','SURFDATA')
   elseif( issi /= idsurf )then
!        Write Warning Message:  SURFDATA id mismatch
      call errhdl(path,modnam,'W','530','SURFDATA')
   endif
   if( len_trim(cusi) == 0 .and. iduair /= 0 )then
!        Write Warning Message:  UAIRDATA ID missing;
      call errhdl(path,modnam,'W','531','UAIRDATA')
   elseif( iusi /= iduair )then
!        Write Warning Message:  UAIRDATA id mismatch
      call errhdl(path,modnam,'W','530','UAIRDATA')
   endif
   if( imstat(9) == 1 )then
      if( len_trim(cosi) == 0 .and. idsite /= 0 )then
!           Write Warning Message:  SITEDATA ID missing;
         call errhdl(path,modnam,'W','531','SITEDATA')
      elseif( iosi /= idsite )then
!           Write Warning Message:  SITEDATA id mismatch
         call errhdl(path,modnam,'W','530','SITEDATA')
      end if
   endif

!     Get the hemisphere and latitude (from the first record of the
!     scalar file
   call dcdlat ()

! --- Open and read first hour of PROFFILE in order to check for
!     PROFFILE heights that may indicate use of prognostic met data

1002 continue

! --- Check for whether PROFFILE met file has been opened;
!     otherwise, skip to end
   if( .not. mpopen ) goto 1003

!---- Initialize the profile data to missing;
!     READ profile data based on free format
!
   call pflini ()
   level = 1
   jflag = 0
!     Read record from ASCII profile file using FREE format; compute
!     sigma_V from sigma_A and wind speed

! --- Set 'DUMMY' variable = 'PROFFILE' for error handling
   dummy = 'PROFFILE'

   do while( jflag == 0 )
      read( mpunit, *, end=1000, err=99, iostat=ioerrn ) kyear,&
      &kmonth, kday, khour, pflht(level), jflag,&
      &pflwd(level), pflws(level), pflta(level),&
      &pflsa(level), pflsw(level)

!        Convert the data to the required units
      call pflcnv (level)

!        Set the number of profile levels to current index, store
!        the 'top of profile' flag, and increment level if not at top
!        Check that the level does not exceed the maximum allowable
      nplvls = level
      iflag(level) = jflag

! ---    Check for PFLHT > 999m, which could indicate use of
!        MMIF or other gridded met data inputs
      if( pflht(level) > 999.0d0 .or. pflht(level) <= 0.0d0)then
         if( .not. L_MMIF_Data .and. .not. L_MMIF_Profile )then
! ---          Issue warning message for PFLHT > 999m if MMIF data
!              inputs were not specified; message(s) will only be
!              generated for a single profile
            write(dummy,'(''LVL'',I2.2,1X,I5,''m'')') level,&
            &nint(pflht(level))
            call errhdl(path,modnam,'W','184',dummy)
            if( jflag == 1 )then
! ---             Top of profile has been flagged for PFLHT > 999m
!                 Set L_MMIF_Profile flag to .TRUE. to turn off
!                 additional warning messages
               L_MMIF_Profile = .true.
            endif
         endif
      endif

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

   rewind mpunit

   go to 1003

!     Write Out Error Message for File READ Error
99 call errhdl(path,modnam,'E','510',dummy)
   return

1003 continue

end subroutine meopen

subroutine scimit
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, imit5, imit6

!     Variable Initializations
   modnam = 'SCIMIT'
   imit5  = 1
   imit6  = 1

   if (ifc == 4 .or. ifc == 6 .or. ifc == 8) then
      call stonum(field(3),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
!           Issue error message: Invalid numeric field
         call errhdl(path,modnam,'E','208',keywrd)
      else
!           Assign value for SCIM starting hour
         nregstart = nint( fnum )
      end if
      if (nregstart < 1 .or. nregstart > 24) then
!           WRITE Error Message        ! Start Hour out of range
         call errhdl(path,modnam,'E','380','StartHr')
      end if

      call stonum(field(4),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
!           Issue error message: Invalid numeric field
         call errhdl(path,modnam,'E','208',keywrd)
      else
!           Assign value for SCIM interval
         nregint = nint( fnum )
      end if
      if (nregint < 1) then
!           WRITE Error Message        ! NRegInt is out of range
         call errhdl(path,modnam,'E','380','NRegInt')
      end if

      if (ifc == 8) then
! ---       Issue warning message:  Wet scimming not supported
         call errhdl(path,modnam,'W','157',keywrd)

! ---       Skip wet scimming inputs in fields 5 and 6

!           Assume fields 7 and 8 are optional files for summary of
!           SCIM'd met data.
         scimout = .true.
!           Retrieve Sfc Met Data Filename as Character Substring to Maintain Case
         if ((loce(7)-locb(7)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            scim_sfcfil = runst1(locb(7):loce(7))
            open(unit=isunit,file=scim_sfcfil,status='REPLACE')
         else
!              WRITE Error Message:  SCIM_SFCFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
         end if
!           Retrieve Pfl Met Data Filename as Character Substring to Maintain Case
         if ((loce(8)-locb(8)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            scim_profil = runst1(locb(8):loce(8))
            open(unit=ipunit,file=scim_profil,status='REPLACE')
         else
!              WRITE Error Message:  SCIM_PROFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
         end if

      else if (ifc == 6) then
! ---       Check fields 5 and 6 for optional SCIM'd met data files or for
!           unsupported wet scimming inputs.

! ---       Check the fields for non-numeric characters; if any are found,
!           then assume that these are optional met data file names;
!           otherwise, assume these are unsupported wet scimming inputs
         do i = locb(5), loce(5)
            read(runst1(i:i),'(I1)',err=99) imit5
         end do
         do i = locb(6), loce(6)
            read(runst1(i:i),'(I1)',err=99) imit6
         end do

! ---       Neither field 5 nor 6 has non-numeric characters,
!           assume that these are unsupported wet scimming inputs.
!           Issue warning message:  Wet scimming not supported
         call errhdl(path,modnam,'W','157',keywrd)

! ---       Done processing, return
         return

! ---       Non-numeric characters found in fields 5 and/or 6;
!           assume these are file names
99       continue

! ---       At least one field has non-numeric characters,
!           assume that these are optional SCIM'd met data file names
         scimout = .true.
!           Retrieve Sfc Met Data Filename as Character Substring to Maintain Case
         if ((loce(5)-locb(5)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            scim_sfcfil = runst1(locb(5):loce(5))
            open(unit=isunit,file=scim_sfcfil,status='REPLACE')
         else
!              WRITE Error Message:  SCIM_SFCFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
         end if
!           Retrieve Pfl Met Data Filename as Character Substring to Maintain Case
         if ((loce(6)-locb(6)) <= (ilen_fld - 1) ) then
!              Retrieve Filename as Character Substring to Maintain Original Case
!              Also Check for Filename Larger Than ILEN_FLD Characters
            scim_profil = runst1(locb(6):loce(6))
            open(unit=ipunit,file=scim_profil,status='REPLACE')
         else
!              WRITE Error Message:  SCIM_PROFIL Field is Too Long
            write(dummy,'(I8)') ilen_fld
            call errhdl(path,modnam,'E','291',dummy)
         end if

      end if

   else if (ifc > 8) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
   end if

   return
end subroutine scimit

subroutine numyr
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'NUMYR'

   if (ifc == 3) then
      call stonum(field(3),ilen_fld,fnum,imit)
      if (imit /= 1) then
!           WRITE Error Message  ! Invalid Numeric Field Encountered
         call errhdl(path,modnam,'E','208',keywrd)
      else
         nyears = nint(fnum)
         if ( abs(fnum-real(nyears)) > 1.0e-5 ) then
!              WRITE Error Message  ! Invalid Numeric Field, should be integer
            call errhdl(path,modnam,'E','208',keywrd)
         end if
      end if
   else if (ifc > 3) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message           ! No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   return
end subroutine numyr

subroutine turbopt
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
   use main1
   implicit none
!     LOOPING VARIABLE
   integer :: i
   logical :: lfound
   character :: modnam*12
   character(len=8) :: keys(9)

!     Variable Initializations
   modnam = 'TURBOPT'

   data keys /'NOTURB  ','NOTURBST','NOTURBCO','NOSA    ',&
   &'NOSW    ','NOSAST  ','NOSWST  ','NOSACO  ','NOSWCO  '/


   i=1
   lfound=.false.
!     LOOK FOR THE KEYWORD IN THE KEYS ARRAY AND SET THE APPROPRIATE VALUE OF TURBOPTS TO TRUE
   do while( i <= 9 .and. .not. lfound)
      if (keywrd == keys(i)) then
         lfound=.true.
         turbopts(i)=.true.
      else
         i=i+1
      endif
   enddo

!     WRITE MESSAGE THAT AN OPTION CHOSEN
   call errhdl(path,modnam,'I','443',keywrd)

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
   if (i > 2 .and. dfault) then
      call errhdl(path,modnam,'W','444',keywrd)
      turbopts(i)=.false.
   endif

   return
end subroutine turbopt

subroutine cent_date(TwoDigitYear,FourDigitYear)
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
   use main1, only: istrt_wind, istrt_cent
   implicit none
   integer  :: TwoDigitYear, FourDigitYear

!      Assuming that TwoDigitYear is a two digit value for the year from the SFC file and ISTRT_WIND is
!      a two digit value derived from the user input four digit year (or set to 00 is the
!      user input a two digit year in the INP file

!     Check that FourDigitYear is four digits not two digits
!     IF FourDigitYear is two digits, save it as the two digit year
   if(FourDigitYear <= 99 .and. FourDigitYear /= 0) then
      TwoDigitYear = FourDigitYear
   end if

!MGS   D181_Y2K_WSP: Corrected when the met years cross from 19xx over to 20xx (4/10/2024)
!MGS      IF (TwoDigitYear .NE. ISTRT_WIND .and. TwoDigitYear .LE. 99) THEN
   if (TwoDigitYear > istrt_wind .and. TwoDigitYear <= 99) then
      FourDigitYear = istrt_cent*100 + TwoDigitYear

   else if (TwoDigitYear < istrt_wind) then !CMGS Add 1 to the century (4/10/2024)
      FourDigitYear = (istrt_cent+1)*100 + TwoDigitYear  !CMGS Add 1 to the century (4/10/2024)
   else if (TwoDigitYear > 99) then
!         Input TwoDigitYear is a 4-digit:  Save to FourDigitYear and convert to 2-digit
      FourDigitYear   = TwoDigitYear
      TwoDigitYear = FourDigitYear - 100 * (FourDigitYear/100)
   end if

   return
end subroutine cent_date

subroutine long_date(EightDigit,TenDigit,TwoDigit,OutputTwoDigit)
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
   use main1, only: istrt_wind, istrt_cent
   implicit none
   integer  :: EightDigit, TenDigit, OutputTwoDigit,TwoDigit

!      Assuming that TwoDigitYear is a two digit value for the year from the SFC file and ISTRT_WIND is
!      a two digit value derived from the user input four digit year (or set to 00 is the
!      user input a two digit year in the INP file

   if (TwoDigit /= istrt_wind .and. TwoDigit <= 99) then
      OutputTwoDigit = istrt_cent*100 + TwoDigit
      TenDigit = istrt_cent*100000000 + EightDigit
   end if

   return
end subroutine long_date

subroutine LONG_DATE_Opt2(EightDigit,TenDigit,TwoDigit)
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
   use main1, only: istrt_wind, istrt_cent
   implicit none
   integer  :: EightDigit,TenDigit,TwoDigit

!      Assuming that TwoDigitYear is a two digit value for the year from the SFC file and ISTRT_WIND is
!      a two digit value derived from the user input four digit year (or set to 00 is the
!      user input a two digit year in the INP file

   if (TwoDigit /= istrt_wind .and. TwoDigit <= 99) then
      TenDigit = istrt_cent*100000000 + EightDigit
   end if

   return
end subroutine LONG_DATE_Opt2
