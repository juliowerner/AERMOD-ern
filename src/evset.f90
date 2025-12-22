subroutine evcard
!***********************************************************************
!                 EVCARD Module of AERMOD Model
!
!        PURPOSE: To process EVent Pathway card images
!
!        PROGRAMMER:  Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Removed code to add 24 hours to IEDATE (end date
!                    of all events) since the EVDATE already represents
!                    the last hour of the event. This avoids a potential
!                    fatal error for out-of-sequence dates between the
!                    met data and ozone or background data files if the
!                    event occurs on the last day of the data period.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                    of 10-digit variables for start date (ISDATE) and
!                    end date (IEDATE).
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:   To remove reassignment of ISYEAR.
!                    R.W. Brode, PES, 4/2/99
!
!        MODIFIED:   To remove mixed-mode math in calculation of
!                    ISDATE and IEDATE - 4/19/93
!
!        INPUTS:  Pathway (EV) and Keyword
!
!        OUTPUTS: Pass Two Event Setup
!
!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ilsave, itempdate, itempyear

!     Variable Initializations
   modnam = 'EVCARD'

   if (keywrd == 'STARTING') then
!        Set Status Switch
      iestat(1) = iestat(1)+1
      ievent = 1
      if (iestat(1) /= 1) then
!           Error Message: Repeat Starting In Same Pathway
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'EVENTPER') then
!        Set Status Switch
      iestat(2) = iestat(2)+1
!        Check for First Occurrence of EVENTPER Card, and
!        Reinitialize IPROC Array
      if (iestat(2) == 1) then
         do i = 1, 366
            iproc(i) = 0
         end do
      end if
!        Process Average Period, Date and Source Group      ---   CALL EVPER
      call evper
   else if (keywrd == 'EVENTLOC') then
!        Set Status Switch
      iestat(3) = iestat(3)+1
!        Process Discrete Receptor Location                 ---   CALL EVLOC
      call evloc
   else if (keywrd == 'INCLUDED') then
!        Set Status Switch
      iestat(10) = iestat(10) + 1
!        Save ILINE as ISAVE
      ilsave = iline
!        Process the Included Receptor File                 ---   CALL INCLUD
      call includ
!        Retrieve ILINE From ISAVE
      iline = ilsave
   else if (keywrd == 'FINISHED') then
!        Check for missing EVENTLOC cards
      if (iestat(2) > iestat(3)) then
!           Write Error Message:  Missing EVENTLOC
         call errhdl(path,modnam,'E','130','EVENTLOC')
      end if
      numeve = ievent - 1
!        Set Status Switch
      iestat(50) = iestat(50)+1
      if (iestat(50) /= 1) then
!           Error Message: Repeat Finished In Same Pathway
         call errhdl(path,modnam,'E','135',keywrd)
         go to 999
      end if

      if (numeve >= 1) then
! ---       At least one event has been defined;
!           Get start date, ISDATE, and end date, IEDATE
         isdate = evdate(1)
         iedate = evdate(1)
         isyr = isdate/1000000
         ieyr = iedate/1000000

!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
         call long_date(isdate,isdate,isyr,isyr) !Determine the longform date
! ---        D001 remove original calculation of year Wood 9/15/22
!C           Convert 8-digit EVDATE to 10-digit ISDATE and IEDATE
!            IF (ISYR .GE. ISTRT_WIND .and. ISYR .LE. 99) THEN
!               ISYR   = ISTRT_CENT*100 + ISYR
!               ISDATE = ISTRT_CENT*100000000 + ISDATE
!            ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!               ISYR   = (ISTRT_CENT+1)*100 + ISYR
!               ISDATE = (ISTRT_CENT+1)*100000000 + ISDATE
!            END IF
!

!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
         call long_date(iedate,iedate,ieyr,ieyr) !Determine the longform date
! ---        D001 remove original calculation of year Wood 9/15/22
!            IF (IEYR .GE. ISTRT_WIND .and. IEYR .LE. 99) THEN
!               IEYR   = ISTRT_CENT*100 + IEYR
!               IEDATE = ISTRT_CENT*100000000 + IEDATE
!            ELSE IF (IEYR .LT. ISTRT_WIND) THEN
!               IEYR   = (ISTRT_CENT+1)*100 + IEYR
!               IEDATE = (ISTRT_CENT+1)*100000000 + IEDATE
!            END IF

!           Loop through events to find start date and end date
         do i = 1, numeve

            itempdate = evdate(i)
            itempyear = itempdate/1000000
!            D001 Call LONG_DATE_Opt2 and determine the longform date without updating the short date Wood 9/15/22
            call LONG_DATE_Opt2(itempdate,itempdate,itempyear) !Determine the longform date
! ---        D001 remove original calculation of year Wood 9/15/22
!               IF (ITEMPYEAR.GE.ISTRT_WIND .and. ITEMPYEAR.LE.99) THEN
!                  ITEMPDATE = ISTRT_CENT*100000000 + ITEMPDATE
!               ELSE IF (ITEMPYEAR .LT. ISTRT_WIND) THEN
!                  ITEMPDATE = (ISTRT_CENT+1)*100000000 + ITEMPDATE
!               END IF

            if (itempdate < isdate) isdate = itempdate
            if (itempdate > iedate) iedate = itempdate
         end do
!           Set start hour to 00 since EVDATE is for the last hour of event
         isdate = (isdate/100)*100
         isyr = isdate/1000000
         ieyr = iedate/1000000
         ismn = (isdate/10000) - (isdate/1000000)*100
         iemn = (iedate/10000) - (iedate/1000000)*100
         isdy = (isdate/100) - (isdate/10000)*100
         iedy = (iedate/100) - (iedate/10000)*100
      end if

!        Write Out The Error Message: Mandatory Keyword Missing
      if (iestat(1) == 0) then
         call errhdl(path,modnam,'E','130','STARTING')
      end if
      if (iestat(2) == 0) then
         call errhdl(path,modnam,'E','130','EVENTPER')
      end if
      if (iestat(3) == 0) then
         call errhdl(path,modnam,'E','130','EVENTLOC')
      end if

   else
!        Write Error Message: Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

999 return
end subroutine evcard

subroutine evper
!***********************************************************************
!                 EVPER Module of AERMOD Model
!
!        PURPOSE: Processes Date, Average Period And Source Group data
!                 for EVENT
!
!        PROGRAMMER: Jeff Wang, Roger Brode
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
!        OUTPUTS: Event Name, Group ID, Average Period, Date
!
!        CALLED FROM:   EVCARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: isdx, imn, idy, ievyr2, ievyr4
   character :: usevn*10
   logical :: found

!     Variable Initializations
   modnam = 'EVPER'
   found = .false.

   if (ievent > neve) then
!        WRITE Error Message    ! Too Many Events Specified
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''NEVE='',I7)') neve
      call errhdl(path,modnam,'E','290',dummy)
      go to 999
   end if

!     Check Whether There Are Enough Parameter Fields
   if (ifc == 2) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 7) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Parameters
!        Note That FIELD(7) Is Used To Hold Concentration
!        Value for Events Generated From initial run, and
!        these concentrations are also used during Event
!        processing to check for consistency of results.
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     READ EVNAME, AVEPER, GRPID, DATE

!     Get The Event Name
   usevn = field(3)
!     Check for Previous EVNAME
   call sindex(evname,neve,usevn,isdx,found)
   if (.not.found) then
      evname(ievent) = usevn
   else
!        Error Message: Duplicate EVNAME
      call errhdl(path,modnam,'E','313',evname(isdx))
      go to 999
   end if

!     Get Averaging Period For The Event
   call stonum(field(4),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   else
      evaper(ievent) = nint(fnum)
   end if

!     Check for Valid Averaging Period
   do iave = 1, numave
      if (evaper(ievent) == kave(iave)) then
         found = .true.
         if (evaper(ievent) > 24) then
!              Write Error Message for Invalid Averaging Period, Must be <=24
            call errhdl(path,modnam,'E','297',evname(ievent))
         end if
      end if
   end do
   if (.not. found) then
!        Error Message: Averaging Period Does Not Match
      call errhdl(path,modnam,'E','203','AVEPER')
   end if

!     Take The Group ID
   evgrp(ievent) = field(5)

!     Retrieve The Index of The Group Array
   found = .false.
   call sindex(grpid,ngrp,evgrp(ievent),isdx,found)
   if (.not. found) then
!        Error Message: Group ID Does Not Match
      call errhdl(path,modnam,'E','203','GROUPID')
   else
      idxev(ievent) = isdx
   end if

!     Get The Date Of The Event -
!     First Convert Character String to Double Precision Real
   call stodbl(field(6),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   else
!        Note - EVDATE is an Integer Array
      evdate(ievent) = idnint(dnum)
!        Extract 2-digit year from event date
      ievyr2 = idnint(dnum/1000000.d0)

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
      call cent_date(ievyr2,ievyr4)
! ---    D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Convert to 4-digit year
!         IF (IEVYR2 .GE. ISTRT_WIND .and. IEVYR2 .LE. 99) THEN
!            IEVYR4 = ISTRT_CENT*100 + IEVYR2
!         ELSE IF (IEVYR2 .LT. ISTRT_WIND) THEN
!            IEVYR4 = (ISTRT_CENT+1)*100 + IEVYR2
!         END IF

      imn = idnint(dnum/10000.d0) - idnint(dnum/1000000.d0)*100
      idy = idnint(dnum/100.d0) - idnint(dnum/10000.d0)*100
      call julian(ievyr4,imn,idy,jday)
      if (jday >= 1 .and. jday <= 366) then
         iproc(jday) = 1
         evjday(ievent) = jday
      else
!           WRITE Error Message    ! Invalid Julian Day
         call errhdl(path,modnam,'E','203','Juli Day')
         go to 999
      end if
   end if

!     Get The Original Modeled Concentration Of The Event -
!     First Convert Character String to Double Precision Real
   call stodbl(field(7),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   else
!        Save Original Modeled Concentration for QA Purposes
      EV_OrigConc(ievent) = dnum
   end if

   ievent = ievent + 1

999 return
end subroutine evper

subroutine evloc
!***********************************************************************
!                 EVLOC Module of AERMOD Model
!
!        PURPOSE: Processes Receptor Location Inputs for Events
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Event Name, AXR, AYR, AZELEV, AZFLAG of the Event
!
!        CALLED FROM:   EVCARD
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

   integer :: isdx
   double precision :: setaxr, setayr
   character :: usevn*10, idnam1*4, idnam2*4
   logical :: found

!     Variable Initializations
   modnam = 'EVLOC'

!     Check Whether There Are Enough Parameter Fields
   if (ifc == 2) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 8) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 10) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     READ Event Name, XCOOR,YCOOR,ELEV,FLAG And Assign to Different Array
   usevn = field(3)
!     Check for Previous EVNAME
   call sindex(evname,neve,usevn,isdx,found)
   if (.not.found) then
!        Error Message: EVNAME Does Not Match
      call errhdl(path,modnam,'E','203','EVNAME')
      go to 999
   end if

   idnam1 = field(4)

   call stodbl(field(5),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
!        Assign value of 0.0, but continue checking all fields
      setaxr = 0.0d0
   else
      setaxr = dnum
   end if

   idnam2 = field(6)

   call stodbl(field(7),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
!        Assign value of 0.0, but continue checking all fields
      setayr = 0.0d0
   else
      setayr = dnum
   end if

   if (ifc >= 8) then
      call stodbl(field(8),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      else
         azelev(isdx) = dnum
      end if
      call stodbl(field(9),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      else
         azhill(isdx) = dnum
      end if
   else
      azelev(isdx) = 0.0d0
      azhill(isdx) = 0.0d0
   end if

   if (ifc == 10) then
      call stodbl(field(10),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
      else
         azflag(isdx) = dnum
      end if
   else
      azflag(isdx) = 0.0d0
   end if

   if (idnam1=='XR=' .and. idnam2=='YR=') then
      axr(isdx) = setaxr
      ayr(isdx) = setayr
   else if (idnam1=='RNG=' .and. idnam2=='DIR=') then
      axr(isdx) = setaxr*dsin(setayr*dtorad)
      ayr(isdx) = setaxr*dcos(setayr*dtorad)
   else
!        Write Error Message: Illegal Parameter
      call errhdl(path,modnam,'E','203','REC-TYPE')
   end if

999 return
end subroutine evloc

subroutine ev_setup
!***********************************************************************
!                 EV_SETUP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Processing of Run SETUP Information
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!        MODIFIED BY D. Strimaitis, SRC (for GRIDDED TERRAIN Processing)
!
!        MODIFIED:   To remove reference to obsolete TG pathway inherited
!                    from ISCST3 code.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Moved the code to insert a blank line in temporary event
!                    file after each pathway from SUB EVEFIL.
!                    R.W. Brode, PES, Inc. - November 15, 1995.
!
!        MODIFIED:  Default format for METFRM modified to eliminate the
!                   variable ZDM on input.
!                   BY:  J. Paumier, PES              DATE: 27 July 1994
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
   use main1
   implicit none
   character :: modnam*12

   integer :: nops, ilen

   integer :: i, ifstat
   logical :: nopath, nokey
   character :: rdfrm*20
! JAT 06/22/21 D065 REMOVE INPFLD AS UNUSED VARIABLE
!      CHARACTER INPFLD*2, PATHWY(6)*2
   character :: pathwy(6)*2
   interface
      subroutine expath(inpfld,pathwy,ipn,nopath)
         character (len=2), intent(in) :: inpfld
         character (len=2), intent(in), dimension(:) :: pathwy
         integer, intent(in) :: ipn
         logical, intent(out) :: nopath
      end subroutine expath
   end interface


!     Variable Initializations
   modnam = 'EV_SETUP'
   eof = .false.
   iline  = 0
   iqline = 0

!     Initialize PATHWY array
   pathwy(1) = 'CO'
   pathwy(2) = 'SO'
   pathwy(3) = 'ME'
   pathwy(4) = 'EV'
   pathwy(5) = 'OU'
   pathwy(6) = '**'

!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(rdfrm,9100) istrg, istrg
9100 format('(A',i4.4,',T1,',i4.4,'A1)')

!     LOOP Through Input Runstream Records
   do while (.not. eof)

!        Increment the Line Counter
      iline = iline + 1

!        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      read (inunit,rdfrm,end=999) runst1, (runst(i), i = 1, istrg)

!        Check for blank input record and cycle, but echo to output file
      if (len_trim(runst1) == 0) then
         write(iounit,*)
         cycle
      end if

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      call lwrupr

!        Define Fields on Card                              ---   CALL DEFINE
      call define

!        Get the Contents of the Fields                     ---   CALL GETFLD
      call getfld

      if (echo .and.&
      &(field(1)=='OU' .and. field(2)=='FINISHED')) then
!           Echo Last Input Card to Output File (Use Character Substring to
!           Avoid Echoing ^Z Which May Appear at "End of File" for Some
!           Editors).  Also, Allow for Shift in the Input Runstream File of
!           Up to 3 Columns.
         if (locb(1) == 1) then
            write(iounit,9200) runst1(1:11)
9200        format(a11)
         else if (locb(1) == 2) then
            write(iounit,9210) runst1(1:12)
9210        format(a12)
         else if (locb(1) == 3) then
            write(iounit,9220) runst1(1:13)
9220        format(a13)
         else if (locb(1) == 4) then
            write(iounit,9230) runst1(1:14)
9230        format(a14)
         end if
      else if (echo) then
!           Echo Full Input Card to Output File
         write(iounit,'(a:)') runst1(1:len_trim(runst1))
      end if

!        Check for 'NO ECHO' In First Two Fields
      if (field(1) == 'NO' .and. field(2) == 'ECHO') then
         echo = .false.
         cycle
      end if

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      call expath(field(1),pathwy,6,nopath)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      if (nopath) then
!           WRITE Error Message    ! Invalid Pathway ID
         call errhdl(ppath,modnam,'E','100',path)
         path = ppath
         cycle
      else if (path == '**') then
! ---       CYCLE to next runstream record
         cycle
      end if

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      call exkey(field(2),nokey)

      if (nokey) then
!           WRITE Error Message    ! Invalid Keyword
         call errhdl(path,modnam,'E','105',keywrd)
         pkeywd = keywrd
         cycle
      end if

!        Check for Proper Order of Setup Cards              ---   CALL SETORD
      call ev_setord

!        Process Input Card Based on Pathway
      if (path == 'CO') then
!           Process COntrol Pathway Cards                   ---   CALL COCARD
         call cocard
      else if (path == 'SO') then
!           Process SOurce Pathway Cards                    ---   CALL SOCARD
         call socard
      else if (path == 'ME') then
!           Process MEteorology Pathway Cards               ---   CALL MECARD
         call mecard
      else if (path == 'EV') then
!           Process EVent Pathway Cards                     ---   CALL EVCARD
         call evcard

! ---       Create a character string that includes only those modeling
!           options (MODOPS) that are applicable for this model run
         MODOPS_String = ''
         nops = 0

         do i = 1, 30
            if (len_trim(modops(i)) > 0) then
               nops = nops + 1
               ilen = 10*(nops-1)
               MODOPS_String = MODOPS_String(1:ilen)//' '//modops(i)
            end if
         end do

      else if (path == 'OU') then
!           Process OUtput Pathway Cards                    ---   CALL OUCARD
         call ev_oucard
!           Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
!           to Statement 999 in Order to Avoid Reading a ^Z "End of File"
!           Marker That May Be Present For Some Editors.
         if (path == 'OU' .and. keywrd == 'FINISHED') then
            go to 999
         end if
      end if

!        Store the Current Keyword as the Previous Keyword
      pkeywd = keywrd

      go to 11
999   eof = .true.
11    continue
   end do

!     Reinitialize Line Number Counter to Count Meteorology Data
   iline = 0

!     Check That All Pathways Were Finished
   if (icstat(50)/=1 .or. isstat(50)/=1 .or. imstat(50)/=1 .or.&
   &iestat(50)/=1 .or. iostat(50)/=1) then
!        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
      ifstat = icstat(50)*10000 + isstat(50)*1000 + imstat(50)*100 +&
      &iestat(50)*10 + iostat(50)
      write(dummy,'(I5.5)') ifstat
      call errhdl(path,modnam,'E','125',dummy)
      if (ifstat == 0) then
         write(iounit,9990)
9990     format(/1x,'All AERMOD input pathways missing! ',&
         &'Processing aborted!!!')
         stop
      end if
   end if

! --- Check for non-DFAULT options for "optimized" area source,
!     FASTAREA, or for all source types, FASTALL; set MAXDIST = 80KM
!     if FASTALL or FASTAREA, otherwise MAXDIST = 1.0D20
   if (fastall .or. fastarea) then
      maxdist = 8.0d04
   else
      maxdist = 1.0d20
   end if

   return
end subroutine ev_setup

subroutine ev_setord
!***********************************************************************
!                 EV_SETORD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Check Run Stream Setup Images for Proper
!                 Order
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EV_SETORD'

   if (keywrd == 'STARTING') then
      if (istart .or. .not.ifinis) then
!           WRITE Error Message: Starting Out of Order
         call errhdl(ppath,modnam,'E','119',path)
      else if (ipnum /= ippnum+1) then
!           WRITE Error Message: Pathway Out of Order
         call errhdl(ppath,modnam,'E','120',path)
      end if
!        Set Starting Indicator
      istart = .true.
!        Set Finished Indicator
      ifinis = .false.
   else if (keywrd == 'FINISHED') then
      if (ifinis .or. .not.istart) then
!           WRITE Error Message: Finished Out of Order
         call errhdl(ppath,modnam,'E','119',path)
      else if (istart .and. path/=ppath) then
!           WRITE Warning Message: Pathway Out of Order
         call errhdl(ppath,modnam,'E','120',path)
      end if
!        Reset Starting Indicator
      istart = .false.
!        Set Finished Indicator
      ifinis = .true.
   else if (.not.istart .or. ifinis) then
!        WRITE Error Message: Starting or Finished Out of Order
      call errhdl(ppath,modnam,'E','119',path)
   else if (istart .and. path/=ppath) then
!        WRITE Warning Message: Pathway Out of Order
      call errhdl(ppath,modnam,'E','120',path)
   end if

!     Save Current Path and Path Number as Previous Path and Number
   ppath = path
   ippnum = ipnum

   return
end subroutine ev_setord

subroutine ev_oucard
!***********************************************************************
!                 EV_OUCARD Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: To process OUtput Pathway card images
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Pathway (OU) and Keyword
!
!        OUTPUTS: Output Option Switches
!                 Output Setup Status Switches
!
!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EV_OUCARD'

   if (keywrd == 'STARTING') then
!        Set Status Switch
      iostat(1) = iostat(1) + 1
      if (iostat(1) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'EVENTOUT') then
!        Process EVENT Output File Option                ---   CALL OEVENT
      call oevent
!        Set Status Switch
      iostat(2) = iostat(2) + 1
      if (iostat(2) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'FILEFORM') then
!        Process FILEFORM Output Option                  ---   CALL FILEFORM
      call fileform
!        Set Status Switch
      iostat(13) = iostat(13) + 1
      if (iostat(13) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'FINISHED') then
!        Set Status Switch
      iostat(50) = iostat(50) + 1
!        Check If Missing Mandatory Keyword
      if (iostat(1) == 0) then
         call errhdl(path,modnam,'E','130','STARTING')
      end if
      if (iostat(2) == 0) then
         call errhdl(path,modnam,'E','130','EVENTOUT')
      end if
      if (iostat(50) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else
!        Write Error Message:  Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

   return
end subroutine ev_oucard

subroutine oevent
!***********************************************************************
!                 OEVENT Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: To Process EVENT File Output Selections
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Parameters
!
!        OUTPUTS: Output Option Switches
!
!        CALLED FROM:   OUCARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   character :: option*6

!     Variable Initializations
   modnam = 'OEVENT'

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc > 3) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Assign Variable of EVENTOUT
   option = field(3)
   if (option == 'SOCONT') then
      socont = .true.
   else if (option == 'DETAIL') then
      detail = .true.
   else
!        WRITE Error Message:  Invalid Parameter Field
      call errhdl(path,modnam,'E','203',keywrd)
   end if

999 return
end subroutine oevent

subroutine blevrecp (iev,kk)
!***********************************************************************
!                 BLEVRECP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs a translation and initial rotation of the
!                 event receptor when buoyant line sources are processed
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    March 31, 2016
!
!        MODIFIED: Multiple_BuoyLines_D41_Wood: December 1, 2019
!                  To process multiple buoyant line sources

!        INPUTS:  Receptor locations, translation and rotation parameters
!                 IEV =
!                 KK = Bouyant line source group number
!
!        OUTPUTS: Translated and rotated receptors with flag set
!                 indicating if a receptor is inside or outside the
!                 bounding buoyant line source rectangle
!
!        CALLED FROM:   EVCALC
!***********************************************************************
! ---    If there is a buoyant line source, the event receptor(s) are
!          translated and rotated so they are oriented the same as the
!          translation and rotation for the buoyant line source - use
!          the buoyant line source endpoints to determine translation
!          and rotation
!          (NOTE: this is an intial rotation based on the orientation of
!           buoyant line source relative to the x-y axes (with +y
!           pointing north); the receptors will be rotated again each
!           hour based on the wind direction)
!           XOR, YOR, COSTCOR, and SINTCOR are calculated in BL_ROTATE1
!           for the sources and are in the BUOYANT_LINE module
!        An assumption here is that the SOurce pathway is processed
!           before the REceptor pathway.
!
!        BL_RFLAG indicates if a receptor is inside (.true.) or outside
!         (.false.) of the rectangle defined by the minimum and maximum
!         extents of the translation/first rotation of the buoyant line
!
!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
! JAT 06/22/21 D065 REMOVE NVRECT AS UNUSED VARIABLE
! JAT 7/22/21 D065 REMOVE NRECIN
!      INTEGER :: IEV, KK, NRECIN, LNUM, NVRECT
   integer :: iev, kk, lnum
   double precision :: ex,ey, xlmin, xlmax, ylmin, ylmax
!      DOUBLE PRECISION :: BLREC_X(4), BLREC_Y(4)
   character :: modnam*12
! Unused: INTEGER :: ILSAVE

!     Variable Initializations
   modnam = 'BLEVRECEP'
! JAT 7/22/21 D065 REMOVE NRECIN
!      NRECIN = 0

   if (nbltotal>= 1) then
!        Translate event receptor
      xr_scs(1,kk) = axr(iev) - xor(kk)
      yr_scs(1,kk) = ayr(iev) - yor(kk)
      ex = xr_scs(1,kk)
      ey = yr_scs(1,kk)
!        Rotate event receptor
      ey = -ex*sintcor(kk) + ey*costcor(kk)
      yr_scs(1,kk) = ey
      ex = (ex + ey*sintcor(kk))/costcor(kk)
      xr_scs(1,kk) = ex

! ---    Determine the min, max extents of the bouyant line source
!         after translation and first rotation
!         These are used to determine if a receptor is inside the
!         'footprint' of the buoyant lines and excluded from the
!         calculations by setting a flag for each receptor
! ---    Note that the code to determine the bounding rectangle is
!         executed each time the event receptor is translated and rotated;
!         better placement of the code would eliminate this redundancy

! Multiple_BuoyLines_D41_Wood
!     Determine the exclusion zone by BL group
      do lnum = 1,nbltotal
         if (blineparms(lnum)%iblpgrpnum == kk) then
            ylmax = ys_scs(lnum)
            xlmax = blineparms(lnum)%xend_tr1
            xlmin = blineparms(lnum)%xbeg_tr1
            exit
         end if
      end do

      do lnum = nbltotal, 1, -1
         if (blineparms(lnum)%iblpgrpnum == kk) then
            ylmin = ys_scs(lnum)
         end if
      end do

      do lnum = 1,nbltotal
         if (blineparms(lnum)%iblpgrpnum == kk) then
            xlmin = min(xlmin,blineparms(lnum)%xbeg_tr1)
            xlmax = max(xlmax,blineparms(lnum)%xend_tr1)
            ylmin = min(ylmin,ys_scs(lnum))
            ylmax = max(ylmax,ys_scs(lnum))
         end if
      end do

! Multiple_BuoyLines_D41_Wood
!        NOT NEEDED - commented out for now
!        Define the vertices of the rectangle
!         NVRECT = 4
!         BLREC_X(1) = XLMIN
!         BLREC_X(2) = XLMAX
!         BLREC_X(3) = XLMAX
!         BLREC_X(4) = XLMIN
!         BLREC_Y(1) = YLMIN
!         BLREC_Y(2) = YLMIN
!         BLREC_Y(3) = YLMAX
!         BLREC_Y(4) = YLMAX

! ---    Check to be certain the event receptor is not inside the
!         bounding rectangle defined by the extents of the buoyant line
!         source; write a message if the receptor is inside the rectangle;
!         this should not be the case if the event was generated by
!         an AERMOD run

! Multiple_BuoyLines_D41_Wood
!        Second dimension added for multiple buoyant line processing
      if( yr_scs(1,kk) <= (ylmax + 0.1d0) .and.&
      &yr_scs(1,kk) >= (ylmin - 0.1d0) .and.&
      &xr_scs(1,kk) <= (xlmax + 0.1d0) .and.&
      &xr_scs(1,kk) >= (xlmin - 0.1d0)) then
         bl_rflag(1,kk) = .true.
         write(dummy,'(A8," ",I3)') trim(bl_grpid(kk)),iev
         call errhdl(path,modnam,'W','477',dummy)
      end if
   end if
   return
end subroutine blevrecp
