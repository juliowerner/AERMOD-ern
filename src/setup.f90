subroutine setup
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
   use main1
   implicit none
   character :: modnam*12
!     JAT D065 8/9/21 NOPS AND ILEN SET BUT NOT USED
!      INTEGER :: NOPS, ILEN
!     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!      INTEGER :: MOD_Len

   integer :: i, ifstat
   logical :: nopath, nokey
   logical :: nocommentecho
   character :: rdfrm*20
! JAT 06/22/21 D065
! REMOVE INPFLD AS UNUSED VARIABLE
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
   modnam = 'SETUP'
   path  = '  '
   ppath = '  '
!     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!      MOD_Len = 0
   eof = .false.
   nocommentecho = .false.

!     Initialize line counters: ILINE for met file; IQLINE for HOUREMIS file; IOLINE for OZONEFIL;
!     INOXLINE for NOX_FILE
   iline  = 0
   iqline = 0
   ioline = 0
   ibline = 0
   inoxline = 0
!     JAT D065 8/9/21 NOPS SET BUT NOT USED
!      NOPS = 0
!     JAT D065 8/9/21 ILEN SET BUT NOT USED
!      ILEN = 0
! --- Initialize counters for arrays to store met data for MAXDCONT option
   ihr_ndx = 0
   iyr_ndx = 0

!     Initialize PATHWY array
   pathwy(1) = 'CO'
   pathwy(2) = 'SO'
   pathwy(3) = 'RE'
   pathwy(4) = 'ME'
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

!        Check for blank input record; echo the blank record to the
!        output file and the Temporary Event File and then cycle to
!        the next record; unless PATH = 'RE' for EVENT
      if (len_trim(runst1) == 0) then
         write(iounit,*)
         if (path /= 'RE' .and. path /= 'OU') then
!              Skip echo of blank field to Event file for RE pathway
            write(itevut,*)
         end if
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
      else if (path == 'RE') then
         nocommentecho = .true.
      else if (path == 'ME') then
         nocommentecho = .false.
      else if (path == 'OU') then
         nocommentecho = .true.
      end if

      if (path == '**') then
         if (nocommentecho) then
! ---          Skip echo to temporary event file and cycle
            cycle
         else
! ---          "Echo" the comment record to the Temporary Event File
!              and then CYCLE to the next record
            write(itevut,'(a:)') runst1(1:len_trim(runst1))
            cycle
         end if
      end if

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      call exkey(field(2),nokey)

! Multiple_BuoyLines_D41_Wood
!        Removed logic that BLPGROUP was not operational in v19191
      if (nokey) then
!           WRITE Error Message    ! Invalid Keyword
         call errhdl(path,modnam,'E','105',keywrd)
         pkeywd = keywrd
         cycle
      end if

!        Check for Proper Order of Setup Cards              ---   CALL SETORD
      call setord

!        Process Input Card Based on Pathway
      if (path == 'CO') then
!           Process COntrol Pathway Cards                   ---   CALL COCARD
         call cocard
! ---       Echo Runstream Image to Temporary Event File (Except EVENTFIL,
!           SAVEFILE, INITFILE & MULTYEAR)
         if (keywrd/='EVENTFIL' .and. keywrd/='SAVEFILE' .and.&
         &keywrd/='INITFILE' .and. keywrd/='MULTYEAR') then
            write(itevut,'(a:)') runst1(1:len_trim(runst1))
         end if
      else if (path == 'SO') then
!           Echo Runstream Image to Temporary Event File
         write(itevut,'(a)') runst1(1:len_trim(runst1))
!           Process SOurce Pathway Cards                    ---   CALL SOCARD
         call socard
      else if (path == 'RE') then
!           Process REceptor Pathway Cards                  ---   CALL RECARD
         call recard
      else if (path == 'ME') then
!           Process MEteorology Pathway Cards               ---   CALL MECARD
         call mecard
!           Echo Runstream Image to Temporary Event File (Except STARTEND
!           & DAYRANGE)
         if (keywrd/='STARTEND' .and. keywrd/='DAYRANGE') then
            write(itevut,'(a:)') runst1(1:len_trim(runst1))
         end if

! ---       Create a character string that includes only those modeling
!           options (MODOPS) that are applicable for this model run
         MODOPS_String = ''
!         JAT D065 8/9/21 NOPS SET BUT NOT USED
!            NOPS = 0
!     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!            MOD_LEN = 0

! ---       Loop through the 30 different options that are flagged
         do i = 1, 30
            if (len_trim(modops(i)) > 0) then
!                 JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
!                  MOD_LEN = LEN_TRIM(MODOPS(I))
               MODOPS_String =&
               &MODOPS_String(1:len_trim(MODOPS_String))//'  '//&
               &modops(i)(1:len_trim(modops(i)))
            end if
         end do

      else if (path == 'OU') then
!           Process OUtput Pathway Cards                    ---   CALL OUCARD
         call oucard
      end if

!        Store the Current Keyword as the Previous Keyword
      pkeywd = keywrd

!        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
!        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
!        Marker That May Be Present For Some Editors.
      if (path == 'OU' .and. keywrd == 'FINISHED') then
         go to 999
      end if

      go to 11
999   eof = .true.
11    continue
   end do

!     Reinitialize Line Number Counter to Count Meteorology Data
   iline = 0

!     Check That All Pathways Were Finished
   if (icstat(50)/=1 .or. isstat(50)/=1 .or. irstat(50)/=1 .or.&
   &imstat(50)/=1 .or. iostat(50)/=1) then
!        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
      ifstat = icstat(50)*10000 + isstat(50)*1000 + irstat(50)*100 +&
      &imstat(50)*10 + iostat(50)
      write(dummy,'(I5.5)') ifstat
      call errhdl(path,modnam,'E','125',dummy)
! ---    Check for IFSTAT = 0, indicating an empty input file;
!        issue error message to 'aermod.out' file and abort
      if (ifstat == 0) then
         write(iounit,9990)
9990     format(/1x,'All AERMOD input pathways missing! ',&
         &'Processing aborted!!!')
! ---       Also issue error message to "screen"
         write(*,9990)
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
end subroutine setup

subroutine lwrupr
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, indchk
   character :: upcase*26
   character :: lwcase*26

!     Variable Initializations
   data upcase/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
   data lwcase/'abcdefghijklmnopqrstuvwxyz'/
   modnam = 'LWRUPR'
   indchk = 0

   do i = 1, istrg
      if (runst(i) /= ' ') then
         indchk = index(lwcase,runst(i))
         if (indchk /= 0) then
            runst(i) = upcase(indchk:indchk)
         end if
      end if
   end do

   return
end subroutine lwrupr

subroutine define
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
   use main1
   implicit none
   character :: modnam*12

   logical :: inquote

   integer :: i

!     Variable Initializations
   modnam = 'DEFINE'
   locb(3:ifmax) = 0
   loce(3:ifmax) = 0

!     Initialize the Blank Line and In-field Status Indicators
   bline = .true.
   infld = .false.
   inquote = .false.

   if (iline == 1) then
!        Define the Starting Column for the Input File In Case File Is Shifted.
!        Allow for Shift of Up to 3 Columns
      locb(1) = 0
      if (runst(1) /= ' ') then
         locb(1) = 1
      else if (runst(2) /= ' ') then
         locb(1) = 2
      else if (runst(3) /= ' ') then
         locb(1) = 3
      else if (runst(4) /= ' ') then
         locb(1) = 4
      else
         locb(1) = 1
      end if
      loce(1) = locb(1) + 1
      locb(2) = locb(1) + 3
      loce(2) = locb(1) + 10
   end if

   ifc = 2

!     Check RUNST1 (full input record string) for Blank Line
   if (len_trim(runst1) > 0) then
      bline = .false.
   else
      return
   end if

!     Loop through the Data Fields
   do i = locb(1)+12, istrg

      if (.not.infld .and. runst(i)=='"') then
!           Location is the Beginning of a Field using "'s
!           Set Mark of not Blank Line
         bline = .false.
!           Set Mark of in a Field
         infld = .true.
!           Set Mark of in a Quote Field
         inquote = .true.
!           Increment the Field Counter
         ifc = ifc + 1
!           Check for number of fields > IFMAX parameter
         if (ifc > ifmax) then
!              WRITE Error Message: Too many fields specified
            write(dummy,'(I8)') ifmax
            call errhdl(ppath,modnam,'E','109',dummy)
            exit
         end if
!           Record the Location of Beginning of the Field
         locb(ifc) = i + 1
      else if (.not.infld .and. runst(i)/=' ') then
!           Location is the Beginning of a Field
!           Set Mark of not Blank Line
         bline = .false.
!           Set Mark of in a Field
         infld = .true.
!           Increment the Field Counter
         ifc = ifc + 1
!           Check for number of fields > IFMAX parameter
         if (ifc > ifmax) then
!              WRITE Error Message: Too many fields specified
            write(dummy,'(I8)') ifmax
            call errhdl(ppath,modnam,'E','109',dummy)
            exit
         end if
!           Record the Location of Beginning of the Field
         locb(ifc) = i
      else if (inquote .and. runst(i)=='"') then
!           Location is the End of a Field
!           Set Mark of Not In a field
         infld = .false.
!           Set Mark of Not in a Quote Field
         inquote = .false.
!           Record the Location of Ending of the Field
         loce(ifc) = i - 1
      else if (.not.inquote .and. infld .and. runst(i)==' ') then
!           Location is the End of a Field
!           Set Mark of Not In a field
         infld = .false.
!           Record the Location of Ending of the Field
         loce(ifc) = i - 1
      end if

!        Check for End of Input String
!        (Length of ISTRG is Set as a PARAMETER in MAIN1)
      if (infld .and. i==istrg) then
         loce(ifc) = istrg
      end if

   end do

   return
end subroutine define

subroutine getfld
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j
   character :: wrtfrm*20

!     Variable Initializations
   modnam = 'GETFLD'
   field(:) = ''

!     Setup WRITE format for internal write to FIELD
!     based on the ILEN_FLD PARAMETER (set in MAIN1)
   write(wrtfrm,9004) ilen_fld
9004 format('(',i4.4,'(A1:))')

   do i = 1, ifc
! ---    Skip processing of fields if IFC > IFMAX
      if (i > ifmax) exit
      if (loce(i)-locb(i) <= (ilen_fld - 1) ) then
!           Field Satisfies Limit of ILEN_FLD Characters (set in MAIN1)
         write(field(i),wrtfrm) (runst(j),j=locb(i),loce(i))
      else
!           Field Exceeds ILEN_FLD Character Limit
!           Truncate Field at ILEN_FLD Characters
         write(field(i),wrtfrm) (runst(j),j=locb(i),&
         &locb(i)+ilen_fld-1)
      end if
   end do

   return
end subroutine getfld

subroutine expath(inpfld,pathwy,ipn,nopath)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i
   character (len=2), intent(in) :: inpfld
   character (len=2), intent(in), dimension(:) :: pathwy
   integer, intent(in) :: ipn
   logical, intent(out) :: nopath

!     Variable Initializations
   nopath = .true.
   modnam = 'EXPATH'

!     Begin The Processing
   if (inpfld /= '  ') then
!        Check the Read-in Pathway
      path = inpfld
      do i = 1, ipn
!           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
         if (inpfld == pathwy(i)) then
            nopath = .false.
            ipnum = i
!              Exit to END
            go to 999
         end if
      end do
   else
!        In Case of Blank Field Set Pathway to Previous Pathway
      nopath = .false.
      path   = ppath
      ipnum  = ippnum
   end if

999 return
end subroutine expath

subroutine exkey(inpfld,nokey)
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i
   character (len=8) :: inpfld
   logical :: nokey

!     Variable Initializations
   nokey  = .true.
   modnam = 'EXKEY'

!     Begin The Processing
   if (inpfld /= '        ') then
!        Check the Read-in Keyword
      keywrd = inpfld
      do i = 1, ikn
!           In Case of Match Set NOKEY to FALSE
         if (inpfld == keywd(i)) then
            nokey = .false.
!              Exit to END
            go to 999
         end if
      end do
   else if (pkeywd /= 'STARTING') then
!        In Case of Blank Field, Keyword Is Set to Previous Keyword
!        unless previous keyword is 'STARTING'
      nokey  = .false.
      keywrd = pkeywd
   else
!        No Keyword is available; keyword field is blank and the
!        previous keyword is 'STARTING'
      nokey  = .true.
      keywrd = 'blank   '
   end if

999 return
end subroutine exkey

subroutine setord
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
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SETORD'

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
end subroutine setord

subroutine stonum(strvar,length,fnum,imuti)
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
   implicit none

   character :: strvar*(*), chk, modnam*6, nums*10
   integer :: i, imuti, length
   real    :: fnum, cnum, fdec, fdc1, head
   logical :: mend, in, nmark, pmark, dmark, mmark, emark

!     Variable Initialization
   modnam = 'STONUM'
   nums = '0123456789'
   i = 1
   mend = .false.
   in = .false.
   nmark = .false.
   pmark = .false.
   dmark = .false.
   mmark = .false.
   emark = .false.
   cnum  = 0.0
   head  = 0.0
   imuti = 1
   fdec  = 1.

!     Beginning the Processing
   do while (.not.mend .and. i<=length)
      chk = strvar(i:i)
      if (chk /= ' ') then
         in = .true.
         if (chk>='0' .and. chk<='9') then
!              CHK is a Number, Assign a Value
            if (.not. dmark) then
               cnum = cnum*10.+float(index(nums,chk)-1)
            else
               fdec = fdec/10.
               fdc1 = fdec*float(index(nums,chk)-1)
               cnum = cnum+fdc1
            end if
         else
!              Handle The E-Type Real Number
            if (dmark .and. .not.emark .and. chk=='E') then
               emark = .true.
               if (.not.nmark) then
                  head = cnum
               else
                  head = -cnum
               end if
               dmark = .false.
               nmark = .false.
               cnum = 0.0
            else if (.not.pmark .and. chk=='+') then
!                 Set Positive Indicator
               pmark = .true.
            else if (.not.nmark .and. chk=='-') then
!                 Set Negative Indicator
               nmark = .true.
            else if (.not.dmark .and. chk=='.') then
!                 Set Decimal Indicator
               dmark = .true.
            else if (.not.mmark .and. chk=='*' .and.&
            &.not.nmark) then
!                 Set Repeat Number
               mmark = .true.
               imuti = nint(cnum)
               cnum = 0.0
            else
!                 Error Occurs, Set Switch and Exit Out Of The Subroutine
               go to 9999
            end if
         end if
      else if (in .and. chk==' ') then
         mend = .true.
      end if
      i = i + 1
   end do

   fnum = cnum

!     In Case Of Negative Field, Value Set to Negative
   if (nmark) then
      fnum = -fnum
   end if

!     In Case of E-Format, Check for Exponents Out of Range
   if (emark .and. abs(fnum) <= 30.) then
      fnum = head*10.0**(fnum)
   else if (emark .and. abs(fnum) > 30.) then
      if (fnum < 0.0) then
         fnum = 0.0
      else if (fnum > 0.0) then
         fnum = head * 10.**30.
      end if
      go to 9999
   end if

   go to 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 imuti = -1

1000 return
end subroutine stonum

subroutine stodbl(strvar,len,fnum,imuti)
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
   implicit none

   character :: strvar*(*), chk, modnam*6, nums*10
   integer :: imuti, len, i
   double precision :: fdec, fdc1, head
   double precision :: fnum, cnum
   logical :: mend, in, nmark, pmark, dmark, mmark, emark

!     Variable Initialization
   modnam = 'STODBL'
   nums = '0123456789'
   i = 1
   mend = .false.
   in = .false.
   nmark = .false.
   pmark = .false.
   dmark = .false.
   mmark = .false.
   emark = .false.
   cnum = 0.0d0
   head = 0.0d0
   fdec = 1.0d0
   imuti = 1

!     Beginning the Processing
   do while (.not.mend .and. i<=len)
      chk = strvar(i:i)
      if (chk /= ' ') then
         in = .true.
         if (chk>='0' .and. chk<='9') then
!              CHK is a Number, Assign a Value
            if (.not. dmark) then
               cnum = cnum*10.0d0+dble(index(nums,chk)-1)
            else
               fdec = fdec/10.0d0
               fdc1 = fdec*dble(index(nums,chk)-1)
               cnum = cnum+fdc1
            end if
         else
!              Handle The E-Type (or D-Type) Real Number
            if ((dmark .and. .not.emark .and. chk=='E') .or.&
            &(dmark .and. .not.emark .and. chk=='D')) then
               emark = .true.
               if (.not.nmark) then
                  head = cnum
               else
                  head = -cnum
               end if
               dmark = .false.
               nmark = .false.
               cnum = 0.0d0
            else if (.not.pmark .and. chk=='+') then
!                 Set Positive Indicator
               pmark = .true.
            else if (.not.nmark .and. chk=='-') then
!                 Set Negative Indicator
               nmark = .true.
            else if (.not.dmark .and. chk=='.') then
!                 Set Decimal Indicator
               dmark = .true.
            else if (.not.mmark .and. chk=='*' .and.&
            &.not.nmark) then
!                 Set Repeat Indicator
               mmark = .true.
               imuti = idnint(cnum)
               cnum = 0.0d0
            else
!                 Error Occurs, Set Switch and Exit Out Of The Subroutine
               go to 9999
            end if
         end if
      else if (in .and. chk==' ') then
         mend = .true.
      end if
      i = i + 1
   end do

   fnum = cnum

!     In Case Of Negative Field, Value set to Negative
   if (nmark) then
      fnum = -fnum
   end if

!     In Case of *E* Format, Check for Exponents Out of Range
   if (emark .and. dabs(fnum) <= 30.0d0) then
      fnum = head*10.0d0**(fnum)
   else if (emark .and. dabs(fnum) > 30.0d0) then
      if (fnum < 0.0d0) then
         fnum = 0.0d0
      else if (fnum > 0.0d0) then
         fnum = head * 10.0d0**30.0d0
      end if
      go to 9999
   end if

   go to 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 imuti = -1

1000 return
end subroutine stodbl

subroutine sindex(arrin,idim,elem,indexs,found)
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
   implicit none

   integer :: i, idim, indexs
   character (len=*) :: arrin(idim), elem
   character :: modnam*6
   logical :: found

!     Variable Initializations
   modnam = 'SINDEX'
   found = .false.
   i = 1
   indexs = 0

   do while (.not.found .and. i<=idim)
      if (elem == arrin(i)) then
         found = .true.
         indexs = i
      end if
      i = i + 1
   end do

   return
end subroutine sindex

subroutine fsplit(pathin,keyin,inpfld,length,delim,lflag,&
&begfld,endfld)
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
   implicit none

   integer :: i, length, idelm
   character :: chk, inpfld*(*), begfld*(*), endfld*(*),&
   &delim*1, modnam*12, pathin*2, keyin*8
   logical :: lflag, mend, in

!     Variable Initialization
   modnam = 'FSPLIT'
   i = length
   idelm = length
   begfld = ' '
   endfld = ' '
   mend  = .false.
   in    = .false.
   lflag = .false.

!     Begin the Processing
   do while (.not.mend .and. i>=1)
      chk = inpfld(i:i)
      if (chk /= ' ') then
         in = .true.
!           Check for the Group Delimiter
         if (.not.lflag .and. chk==delim) then
            lflag = .true.
            idelm = i
            endfld = inpfld(i+1:length)
            if (i == 1) then
!                 Write Error Message for Invalid Range Parameter
               call errhdl(pathin,modnam,'E','203',keyin)
               go to 999
            end if
         else if (lflag .and. chk==delim) then
!              WRITE Error Message  ! More Than One Delimiter in a Field
            call errhdl(pathin,modnam,'E','217',keyin)
         end if
      else if (in .and. chk==' ') then
         mend = .true.
         if (lflag) then
            begfld = inpfld(1:idelm-1)
         else
            begfld = inpfld
         end if
      end if
      i = i - 1
   end do

   if (.not. mend) then
      if (lflag) then
         begfld = inpfld(1:idelm-1)
      else
         begfld = inpfld
      end if
   end if

!     In Case Of No Delimiter, Set ENDFLD = BEGFLD
   if (.not. lflag) then
      endfld = begfld
   end if

999 return
end subroutine fsplit

subroutine varini
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
   use main1
   use buoyant_line

   implicit none
   character :: modnam*12

! Unused: INTEGER :: I, J, K

!     Variable Initializations
   modnam = 'VARINI'

! --- Initialize double precision constants based on PI
   pi      = 4.0d0*datan(1.0d0)
   twopi   = 2.0d0*pi
   rtofpi  = dsqrt(pi)
   srt2pi  = dsqrt(twopi)
   rtof2   = dsqrt(2.0d0)
   rtpiby2 = dsqrt(pi/2.0d0)
   rt2bypi = dsqrt(2.0d0/pi)
   dtorad  = pi/180.0d0
   rtodeg  = 180.0d0/pi

! --- Initialize constant for 1/3 used as exponent
   third = 1.0d0/3.0d0
   twothirds = 2.0d0/3.0d0

! --- Initialize counters to zero
   ipnum  = 0
   ippnum = 0
   ndump  = 0
! --- Initialize counter for old met data warning message
   imetmsg = 0

! --- Initialize pollutant ID, POLLUT*8
   pollut = 'undefine'

!     Initialize the Logical Control Variables
   istart = .false.
   ifinis = .true.
   errlst = .false.
   dfault = .false.
   conc   = .false.
   depos  = .false.
!     Add logicals to output just wet or just dry deposition fluxes
   ddep   = .false.
   wdep   = .false.
   rural  = .false.
   urban  = .false.
   grdris = .false.
   nostd  = .false.
   nobid  = .false.
   nowarn = .false.
   msgpro = .true.
   clmpro = .true.
   period = .false.
   annual = .false.
   month  = .false.
   flat   = .false.
   elev   = .true.
   flgpol = .false.
   run    = .false.
   events = .false.
   rstsav = .false.
   rstinp = .false.
   multyr = .false.
   daytab = .false.
   mxfile = .false.
   ppfile = .false.
   plfile = .false.
   summfile = .false.
   arcft  = .false.                                                 !  Added for Aircraft; UNC-IE
!     Add TXFILE Variable for the TOXXFILE Option, 9/29/92
   txfile = .false.
   rkfile = .false.
   anpost = .false.
   anplot = .false.
   recerr = .false.
   pflerr = .false.
   if (allocated(L_MorningTrans)) L_MorningTrans(:) = .false.
   L_UrbanTransition = .true.
   endmon = .false.
   calcs  = .false.
   surfac = .false.
   debug  = .false.
   meteordbg = .false.
   areadbg  = .false.
   primedbg = .false.
   olmdebug = .false.
   arm2debug= .false.
   pvmrmdbg = .false.
   grsmdebug = .false.
   deposdbg = .false.
!CRT  D063 Platform Downwash Debug
   platfmdbg  = .false.
   wake   = .false.
   echo   = .true.
   screen = .false.
   hourly = .false.
   arcftdebug = .false.                                              ! Added for Aircraft; UNC-IE
   l_blhourly = .false.                                              ! Multiple_BuoyLines_D41_Wood

!     JAT 05/08/20 ADDED FROM 19191
!     Initialized AWMADWDBG to false
   awmadwdbg=.false.

!     Add logicals to identify use wet and dry removal information
   ldpart  = .false.
   lwpart  = .false.
   lwgas   = .false.
   ldgas   = .false.
!     Add logicals to control use of Wet & Dry plume depletion
!     Initialize dry and wet depletion to .F. for now, but if
!     deposition algorithms are used then depletion will be assumed
   ddplete   = .false.
   wdplete   = .false.
   ardplete  = .false.
!     Initialize logicals for user-specified depletion options
   wetdplt   = .false.
   drydplt   = .false.
!     Initialize logicals for user-specified override of depletion
   nowetdplt = .false.
   nodrydplt = .false.

! --- Initialize EVENT processing output options
   socont = .false.
   detail = .false.

! --- Initialize logical for use of vector mean wind speeds, L_VECTORWS
   l_vectorws = .false.

! --- Initialize logical for use of ALPHA option for low-wind-speed
!     modifications.
!CRT 3/23/2021 D061/D062: in aermod.f
!CRT  This resets to false after being set to true in aermod.f
!CRT  not needed here
!CRT      LOW_WIND = .FALSE.

!CRT  4/12/2022 Initialize logical variables for LOW_WIND options
   L_UserSVmin = .false.
   L_UserWSmin = .false.
   L_UserFRANmax = .false.
   L_UserSWmin = .false.
   L_UserBigT = .false.
   L_UserFRANmin = .false.
   L_PBal = .false.

!     Added for TTRM; AECOM
!     Initialize logicals for ozone response rate (TTRM)
   runttrm = .false.
   ttrmdbg = .false.
   runttrm2 = .false.
   ttrm2dbg = .false.
!     End TTRM insert, Nov. 2021
! Added for HBP & HBPDEBUG; Jan. 2023
!     Initialize logicals for running HBP and HBP debug file
   hbplume = .false.
   hbpdbg = .false.
! End insert for HBP & HBPDBG
   urbdbug = .false.
   blpdbug = .false.
! --- Initialize logicals to indicate various met data options used,
!     including the 'ADJ_U*' and 'BULKRN' options in AERMET, and the
!     use of MMIF-generated met inputs directly from MMIF or processed
!     through AERMET; L_MMIF_Data indicates that MMIF-generated data
!     have been used based on information in SURFACE file header record;
!     L_MMIF_Profile is used to flag potential use of MMIF-generated
!     data based on profile heights exceeding 999m, absent information
!     in the SURFACE file header record.
!     8/9/2023, CRT - D176 COARE BETA Check (L_COARE)
   L_AdjUstar     = .false.
   l_bulkrn       = .false.
   L_MMIF_Data    = .false.
   L_MMIF_Profile = .false.
   L_TurbData     = .false.
   l_coare        = .false.

!     Add logical to control user-specified deposition velocity for gases
   luservd  = .false.
   scim     = .false.
   scimout  = .false.
   scimhr   = .false.
   seasonhr = .false.
   beta     = .false.

! --- Initialize logicals for processing design values based on ranked
!     values averaged across years, 24-hr PM2.5, 1-hr NO2 and 1-hr SO2
   pm25ave = .false.
   no2ave  = .false.
   so2ave  = .false.
   l_no_pm25ave = .false.
   l_no_no2ave  = .false.
   l_no_so2ave  = .false.

! --- Initialize logicals for background ozone, NOx and concentrations
   L_O3File(:)   = .false.
   L_O3Hourly    = .false.
   l_o3val(:)    = .false.
   l_o3values(:) = .false.
   l_backgrnd    = .false.
   L_BGHourly    = .false.
   L_BGFile(:)   = .false.
   L_BGValues(:) = .false.
   l_noxvalue(:) = .false.
   l_nox_vals(:) = .false.
   L_NOxHourly   = .false.
   L_NOxFile(:)  = .false.
   L_CalcNOXFromNO2 = .false.

! --- Initialize array index counter for temporally-varying background
!     concentrations (other than hourly file) to zero
   ibkgrd(:) = 0
! --- Initialize logical for day-of-week options
   L_DayOfWeekOpts  = .false.

!     Add logical for MAXDAILY output file option
   mxdaily  = .false.
   l_maxdcont = .false.
   mxdaily_byyr  = .false.
!     Initialize variable to skip messages during MAXDCONT processing
   L_SkipMessages = .false.

! --- Set logical flag for whether "New" met data are being used, based
!     on version 11059 or later of AERMET, using the wind data source
!     and adjustment flags introduced with version 11059 ('NAD' or 'ADJ')
   L_NAD_ADJ_Flags = .false.
! --- Set logical flag for use of "old" met data, i.e., v06341 of AERMET,
!     which is allowed for now, with a warning message
   L_OldMetVer = .false.

!     Non-DFAULT option for optimized area source, formerly controlled by
!     TOXICS option, which is now obsolete
   fastarea  = .false.
!     Non-DFAULT option for optimized meander for POINT and VOLUME sources based
!     on effective sigma-y; also activates the FASTAREA option
   fastall   = .false.
!     Logical flag for optimized meander for POINT and VOLUME sources based
!     on effective sigma-y under non-DFAULT FASTALL option
   l_effsigy = .false.

! --- Logical flag to indicate whether non-DFAULT options are specified
!     when DFAULT is not specified. This is used to set the option label
!     in the file headers.
   L_NonDFAULT = .false.

! --- Initialize options for checking date sequence in meteorological data
   nochkd     = .false.
   l_warnchkd = .false.

! --- Initialize logical variables used to flag EVENT or MAXDCONT inconsistency
!     warnings in order to issue a warning to the default output unit at the
!     end of the run.
   L_EVENT_OrigConc_Warning    = .false.
   L_MAXDCONT_OrigConc_Warning = .false.

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     INITIALIZE TURBOPTS TO FALSE
   turbopts=.false.
!*----
!*#

! --- Initialize variable for specifying format for file outputs;
!     Value has been "preset" under subroutine PRESET in order for the
!     postfile format to be adjusted before writing header recordes in sub_OUPOST;
!     default = 'FIX' for fixed-format outputs; 'EXP' indicates user specified
!     use of exponential-format outputs.  This variable applies to
!     MAXIFILE, PLOTFILE, POSTFILE (plot-formatted), RANKFILE, and SEASONHR outputs
   if (file_format /= 'EXP') then
      file_format = 'FIX'
   end if

! --- Initialize REELEV character variable to 'METERS' for default elevation units
   reelev = 'METERS'
! --- Initialize SOELEV character variable to 'METERS' for default elevation units
   soelev = 'METERS'

!     Initialize Decay Coefficient to 0.0 (Urban SO2 Default Set in POLLUT)
   decoef = 0.0d0

!     Initialize variables to hold two previous hours of precipitation
   prec1  = 0.0d0
   prec2  = 0.0d0
   if (allocated(aprec1)) aprec1 = 0.0d0
   if (allocated(aprec2)) aprec2 = 0.0d0
! --- Initialize variable for total precipitation
   total_precip = 0.0d0

!     Initialize variables used to calculate canopy stomatal resistance, Rs
   Wold = 180.0d0
   f2   = Wold/200.0d0

!     Initialize defaults for Fo, FSEAS2, and FSEAS5 (may be input by user
!     on the CO GASDEPDF card).
   Fo     = 0.0d0
   fseas2 = 0.5d0
   fseas5 = 0.25d0

!     Initialize the Source Arrays
   isrc = 0
   axs(:) = 0.0d0
   ays(:) = 0.0d0
   azs(:) = 0.0d0
   aqs(:) = 0.0d0
   ahs(:) = 0.0d0
   ads(:) = 0.0d0
   avs(:) = 0.0d0
   ats(:) = 0.0d0
   asyini(:) = 0.0d0
   aszini(:) = 0.0d0
!**  Added for Aircraft Plume Rise; UNC-IE
   amfuel(:)  = 0.0d0
   athrust(:) = 0.0d0
   avaa(:)    = 0.0d0
   aafr(:)    = 0.0d0
   abypr(:)   = 0.0d0
   arpwr(:)   = 0.0d0
   asrcangle(:) = 0.0d0
!**  End Aircraft Plume Rise insert; April 2023
   if (allocated(axinit)) axinit(:) = 0.0d0
   if (allocated(ayinit)) ayinit(:) = 0.0d0
   if (allocated(aangle)) aangle(:) = 0.0d0
   if (allocated(radius)) radius(:) = 0.0d0
   if (allocated(axcntr)) axcntr(:) = 0.0d0
   if (allocated(aycntr)) aycntr(:) = 0.0d0
   if (allocated(aalpha)) aalpha(:) = 0.0d0
   if (allocated(apdeff)) apdeff(:) = 0.0d0
   if (allocated(avolum)) avolum(:) = 0.0d0
   sopcrd(:) = 'N'
   sogas(:)  = 'N'
   urbsrc(:) = 'N'
   aftsrc(:) = 'N'                                                  !  Added for Aircraft; UNC-IE
   srcid(:)  = ' '
   srctyp(:) = ' '
   qflag(:)  = ' '
   inpd(:)   = 0
   if (allocated(nverts)) nverts(:) = 0
   ielunt(:) = 0
   eval(:) = .false.
   evlfil(:) = ' '
   l_flatsrc(:) = .false.
   l_hrlysig(:) = .false.
   grp_back(:)  = .false.
   L_WakeMessage(:) = .false.

! --- 1/20/2012, CRT: D063 Intialize platform parameters
   osplat(:) = .false.  ! Source subject to platform downwash
   platelv(:) = 0.0d0   ! Platform base elev. above ocean surface
   plathb(:) = 0.0d0    ! Plaform building height above platform base
   platwb(:) = 0.0d0    ! Platform building width

! --- Initialize BFLAG for BACKGRND concentrations
   bflag(:) = ' '

!     Add gas dry deposition parameters
   if (allocated(pdiff))      pdiff(:)  = 0.0d0
   if (allocated(pdiffw))     pdiffw(:) = 0.0d0
   if (allocated(alphas))     alphas(:) = 0.0d0
   if (allocated(react))      react(:)  = 0.0d0
   if (allocated(henry))      henry(:)  = 0.0d0
   if (allocated(rcli))       rcli(:)   = 0.0d0
   if (allocated(l_method2))  l_method2(:) = .false.

   if (allocated(adsbh))   adsbh(:,:)   = 0.0d0
   if (allocated(adsbw))   adsbw(:,:)   = 0.0d0
   if (allocated(adsbl))   adsbl(:,:)   = 0.0d0
   if (allocated(adsxadj)) adsxadj(:,:) = 0.0d0
   if (allocated(adsyadj)) adsyadj(:,:) = 0.0d0
   if (allocated(axvert))  axvert(:,:)  = 0.0d0
   if (allocated(ayvert))  ayvert(:,:)  = 0.0d0

   if (allocated(qfact))   qfact(:,:)   = 0.0d0

! --- Initialize NUMO3Sects, NUMBGSects and NUMNOxSects to 1
   if (.not. L_O3Sector) NUMO3Sects = 1
   if (.not. L_BGSector) NUMBGSects = 1
   if (.not. L_NOxSector) NUMNOxSects = 1
! --- Initialize counter for number of missing BGHrVal substitutions
   NSubBGHOUR = 0

! --- Initialize O3VARY and NOXVARY arrays across all sectors to -99.0 to facilitate QA
!     checks on completeness of the inputs
   if (allocated(o3vary))  o3vary(:,:) = -99.0d0
   if (allocated(noxvary))  noxvary(:,:) = -99.0d0

   o3flag(:) = ''
   bflag(:)  = ''
   noxflag(:) = ''
   io3max(:) = 0
   ibgmax(:) = 0
   inoxmax(:) = 0

   if (allocated(backgrnd)) backgrnd(:,:) = 0.0d0
   if (allocated(backave)) backave(:) = 0.0d0
   if (allocated(igroup)) igroup(:,:) = 0
   if (allocated(chi)) chi(:,:,:) = 0.0d0
   if(grsm)then
      chi_ttravplm = 0.0d0
      chi_ttravpan = 0.0d0
      chi_ttravaer = 0.0d0
      chi_ttravprm = 0.0d0
      chi_ttravchm(:,:) = 0.0d0
      bldfac(:,:) = 0.0d0
      ttravchm(:) = 0.0d0
      PRMVAL_Src1 = 0.0d0
   end if
   if (olm) then
      olmid(:) = ' '
      igrp_olm(:,:) = 0
      l_olmgrp(:) = .false.
   end if
   if (pvmrm .or. olm .or. grsm) then
      ano2_ratio(:) = -9.0d0
      if (psdcredit) then
         psdsrctyp(:)  = '  '
         l_psdgrp(:)   = .false.
         igrp_psd(:,:) = 0
      end if
      if (pvmrm .or. grsm) then
         ppfact(:,:)  = 0.0d0
         hecntr(:,:)  = 0.0d0
         hecntr3(:,:) = 0.0d0
         ueffs(:,:)   = 0.0d0
         ueff3s(:,:)  = 0.0d0
         epsef(:,:)   = 0.0d0
         epsef3(:,:)  = 0.0d0
         fopts(:,:)   = 0.0d0
      end if
   end if

   if (psdcredit) then
      psdid(:) = ' '
      abval(:,:) = 0.0d0
      bcval(:,:) = 0.0d0
   end if

! Added for TTRM; Nov. 2021
   if (runttrm) then
      ttrmsrc(:,:) = ' '
   end if
! End of TTRM insert

!     Initialize arrays for deposition; first check if ALLOCATED
   if (allocated(apdiam))   apdiam   = 0.0d0
   if (allocated(aphi))     aphi     = 0.0d0
   if (allocated(apdens))   apdens   = 0.0d0
   if (allocated(avgrav))   avgrav   = 0.0d0
   if (allocated(atstop))   atstop   = 0.0d0
   if (allocated(efrac))    efrac    = 0.0d0
   if (allocated(qpart))    qpart    = 0.0d0
   if (allocated(pdiam))    pdiam    = 0.0d0
   if (allocated(phi))      phi      = 0.0d0
   if (allocated(pdens))    pdens    = 0.0d0
   if (allocated(vgrav))    vgrav    = 0.0d0
   if (allocated(tstop))    tstop    = 0.0d0
   if (allocated(schmidt))  schmidt  = 0.0d0
   if (allocated(vdep))     vdep     = 0.0d0
   if (allocated(scf))      scf      = 0.0d0
   if (allocated(pscvrt))   pscvrt   = 0.0d0
   if (allocated(washout))  washout  = 0.0d0
   if (allocated(ecoll))    ecoll    = 0.0d0
   if (allocated(finemass)) finemass = 0.0d0

!     Initialize URBAN arrays
   if (allocated(iurbgrp))   iurbgrp   = 0
   if (allocated(urbid))     urbid     = ' '
   if (allocated(urbnam))    urbnam    = ' '
   if (allocated(urbpop))    urbpop    = 0.0d0
   if (allocated(urbz0))     urbz0     = 0.0d0
   if (allocated(ziurb))     ziurb     = 0.0d0
   if (allocated(urbwstr))   urbwstr   = 0.0d0
   if (allocated(urbustr))   urbustr   = 0.0d0
   if (allocated(urbobulen)) urbobulen = 0.0d0
   if (allocated(grdswu))    grdswu    = 0.0d0
   if (allocated(grdsvu))    grdsvu    = 0.0d0
   if (allocated(grdtgu))    grdtgu    = 0.0d0
   if (allocated(grdptu))    grdptu    = 0.0d0

!**  Added for Airport ID for Aircraft Sources; UNC-IE
   if (allocated(aftid)) aftid  = ' '
!**  End Aircraft Plume Rise insert; April 2023

!     Initialize source depletion factors to unity.
   dqcorg = 1.0d0
   wqcorg = 1.0d0
   if (allocated(wqcor)) wqcor = 1.0d0
   if (allocated(dqcor)) dqcor = 1.0d0

!     Counters for the Receptor Groups
   irec = 0
   ista = .false.
   iend = .false.
   irxr = 0
   iryr = 0
   irze = 0
   irzh = 0
   irzf = 0
   newid = .true.
!     Initialize ITAB, NXTOX, NYTOX Variables for the TOXXFILE Option, 9/29/92
   itab  = -9
   nxtox = 0
   nytox = 0

!     Initialize Variables Associated with the Meteorology Data
   isjday = 0
   iejday = 366
   isdate = 0
!     Initialize End Date as largest valid date for a 4-byte integer.
   iedate = 2147123124
   isyr   = 0
   ismn   = 0
   isdy   = 0
   ishr   = 0
   ieyr   = 9999
   iemn   = 99
   iedy   = 99
   iehr   = 24
   ipdate = 0
   iphour = 0
   imonth = 0
   iday   = 0
   ihour  = 0
   iyear  = 0
   numyrs = 0
   nremain= 0
   ihr_ndx= 0
   iyr_ndx= 0
   incrst = 1
   sfx = 0.0d0
   sfy = 0.0d0
   uax = 0.0d0
   uay = 0.0d0
   rotang = 0.0d0
   o3miss = .false.
   o3back = -999.0d0
   o3conc = -999.0d0
   NO2Equil = 0.90d0
!      NO2Stack = 0.10D0 !D040 remove default NO2Stack value WSP 2/3/23
   NO2Stack = -9.0d0  ! D040 CRT 4/10/2023 initialize to a negative value outside of acceptable range.
   iolm = 0
   noxmiss = .false.
   noxback = -999.0d0
   noxbgconc = -999.0d0
   ittrm = 0

!     Initialize Met Data Filenames and Formats
   metinp = ' '
   proinp = ' '
   metfrm = 'FREE'
   profrm = 'FREE'

! --- Initialize EVENT processing arrays
   if (evonly) then
      axr(:)    = 0.0d0
      ayr(:)    = 0.0d0
      azelev(:) = 0.0d0
      azhill(:) = 0.0d0
      azflag(:) = 0.0d0
      evaper(:) = 0
      evdate(:) = 0
      evjday(:) = 0
      idxev(:)  = 0
      evname(:) = ' '
      evgrp(:)  = ' '
      EV_OrigConc(:) = 0.0d0
      ev_hrqs(:,:) = 0.0d0
      ev_hrts(:,:) = 0.0d0
      ev_hrvs(:,:) = 0.0d0
      ev_hrsy(:,:) = 0.0d0
      ev_hrsz(:,:) = 0.0d0
!**  Added for Aircraft Plume Rise; UNC-IE
      ev_hrmfuel(:,:)  = 0.0d0
      ev_hrvaa(:,:)    = 0.0d0
      ev_hrthrust(:,:) = 0.0d0
      ev_hrbypr(:,:)   = 0.0d0
      ev_hrafr(:,:)    = 0.0d0
      ev_hrrpwr(:,:)   = 0.0d0
      ev_hrsrcangle(:,:)=0.0d0
!**  End Aircraft Plume Rise insert; April 2023
   else if (.not. evonly) then
! ---    Initialize Receptor arrays
      axr(:)    = 0.0d0
      ayr(:)    = 0.0d0
      azelev(:) = 0.0d0
      azhill(:) = 0.0d0
      azflag(:) = 0.0d0
      if (allocated(xr_scs)) xr_scs(:,:) = 0.0d0                     ! Multiple_BuoyLines_D41_Wood
      if (allocated(yr_scs)) yr_scs(:,:) = 0.0d0                     ! Multiple_BuoyLines_D41_Wood
      if (allocated(xr_rcs)) xr_rcs(:) = 0.0d0
      if (allocated(yr_rcs)) yr_rcs(:) = 0.0d0
      iref(:)   = 0
      netid(:)  = ' '
      rectyp(:) = ' '
      ndxarc(:) = 0
      xorig(:)  = 0.0d0
      yorig(:)  = 0.0d0
      numxpt(:) = 0
      numypt(:) = 0
      netsta(:) = 0
      netend(:) = 0
      ntid(:)   = ' '
      nttyp(:)  = ' '
      xcoord(:,:) = 0.0d0
      ycoord(:,:) = 0.0d0
      xint = 0.0d0
      yint = 0.0d0
   end if

   kave(:) = 0

!     Initialize the Outputs
   title1 = ' '
   title2 = ' '
   ipage  = 0
   ipgsum = 0
   nhival = 0
   nmxval = 0
   thrfrm ='(1X,I3,1X,A8,1X,I8.8,2(1X,F13.5),3(1X,F7.2),1X,F13.5)'
   pstfrm ='(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)'
   pltfrm ='(3(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,A8,2X,I8)'
   rnkfrm ='(1X,I6,1X,F13.5,1X,I8.8,2(1X,F13.5),3(1X,F7.2),2X,A8)'
   mxdfrm ='(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,2X,I8.8,&
   &2X,A8)'
   inhi(:)   = 0
   idytab(:) = 0
   maxave(:) = 0
   imxval(:) = 0
   itoxfl(:) = 0
   itxunt(:) = 0
   irnkfl(:) = 0
   irkval(:) = 0
   irkunt(:) = 0
   toxthr(:) = 0.0d0
   toxfil(:) = ' '
   rnkfil(:) = ' '
   idconc(:,:) = 0
   txconc(:,:) = 0.0d0
   nhiave(:,:) = 0
   maxfle(:,:) = 0
   ipstfl(:,:) = 0
   imxunt(:,:) = 0
   ipsunt(:,:) = 0
   ipsfrm(:,:) = 0
   thresh(:,:) = 0.0d0
   thrfil(:,:) = ' '
   pstfil(:,:) = ' '
   ipltfl(:,:,:) = 0
   iplunt(:,:,:) = 0
   pltfil(:,:,:) = ' '
   grpid(:)  = ' '
   ianpst(:) = 0
   ianfrm(:) = 0
   ianplt(:) = 0
   iapunt(:) = 0
   ippunt(:) = 0
   ishunt(:) = 0
   iseahr(:) = 0
   annpst(:) = ' '
   annplt(:) = ' '
   imxdly(:) = 0
   imdunt(:) = 0
   maxdly(:) = ' '
   maxdcont(:) = 0
   imxdly_byyr(:) = 0
   imdunt_byyr(:) = 0
   maxdly_byyr(:) = ' '
   maxdcont_file(:) = ' '
   mxd_rank(:,:) = 0
   maxd_thresh(:) = 0.0d0

!     Initialize filenames
   savfil = ' '
   msgfil = ' '
   evfile = ' '
   sumfil = ' '

   BKGRND_File(:) = ' '
   ozonfl(:) = ' '
   noxfl(:) = ' '

!     Initialize the Number of Error/Warning/Informational Messages, and
!     The Number of Fatal Errors.
   ierror = 0
   nfatal = 0
   nwarn  = 0

   return
end subroutine varini


subroutine includ
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
   use main1
   implicit none
   character :: modnam*12

   integer :: i, itempl
   logical :: nopath, nokey
   character :: rdfrm*20
! JAT 06/22/21 D065
! REMOVE INPFLD AS UNUSED VARIABLE
!      CHARACTER INPFLD*2, PATHWY(7)*2
   character :: pathwy(7)*2
   interface
      subroutine expath(inpfld,pathwy,ipn,nopath)
         character (len=2), intent(in) :: inpfld
         character (len=2), intent(in), dimension(:) :: pathwy
         integer, intent(in) :: ipn
         logical, intent(out) :: nopath
      end subroutine expath
   end interface

!*    Variable Initializations
   modnam = 'INCLUD'
   eof = .false.
   iline  = 0
   itempl = 0

!     Initialize PATHWY array
   pathwy(1) = 'CO'
   pathwy(2) = 'SO'
   pathwy(3) = 'RE'
   pathwy(4) = 'ME'
   pathwy(5) = 'OU'
   pathwy(6) = '**'
   pathwy(7) = 'EV'

!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(rdfrm,9100) istrg, istrg
9100 format('(A',i4.4,',T1,',i4.4,'A1)')

   if (ifc == 3) then
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         incfil = runst1(locb(3):loce(3))
         open (incunt,file=incfil,action='READ',status='OLD',err=99)
      else
!           WRITE Error Message:  INCFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if

   else if (ifc > 4) then
!        WRITE Error Message           ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message         ! No Parameters Specified
      call errhdl(path,modnam,'E','200',keywrd)
   end if

   go to 1001

!     Write Out Error Message for File OPEN Error
99 call errhdl(path,modnam,'E','500','INCFILE ')
   return

1001 continue


!     LOOP Through Input Runstream Records
   do while (.not. eof)

!        Increment the Line Counter.  It was Initially Set to 1, to Handle
!        the Code in Subroutine DEFINE
      iline = iline + 1

!        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      read (incunt,rdfrm,end=999,err=888) runst1,&
      &(runst(i), i = 1, istrg)

!        Check for blank input record and cycle
      if (len_trim(runst1) == 0) cycle

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      call lwrupr

!        If ILINE=1, reset ILINE temporarily to avoid the
!        check for column shift in subroutine DEFINE
      if (iline == 1) then
         iline  = 2
         itempl = 1
      end if

!        Define Fields on Card                              ---   CALL DEFINE
      call define

!        Reset ILINE if needed
      if (itempl == 1) then
         iline  = 1
         itempl = 0
      end if

!        Get the Contents of the Fields                     ---   CALL GETFLD
      call getfld

!        Check for 'NO ECHO' In First Two Fields
      if (field(1) == 'NO' .and. field(2) == 'ECHO') then
!           Skip record with NO ECHO in INCLUDED file, but leave ECHO "on"
         cycle
      end if

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      call expath(field(1),pathwy,7,nopath)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      if (nopath) then
!           WRITE Error Message    ! Invalid Pathway ID
         call errhdl(ppath,modnam,'E','100',path)
         path = ppath
         cycle
      else if (path == '**') then
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

! ---    Check for Proper Order of Setup Cards              ---   CALL SETORD
!        Only call SETORD if not STARTING or FINISHED
!        since these keywords are not allowed in INCLUDED files.
!        This also allows INCLUD to be used for standard
!        processing and EVENT processing.
      if (keywrd /= 'STARTING' .and.&
      &keywrd /= 'FINISHED') call setord

!        First Check for Invalid Keywords (STARTING, FINISHED, INCLUDED)
      if (keywrd == 'STARTING') then
!           Cannot Use the STARTING keyword in the INCLUDED file
         call errhdl(path,modnam,'E','140',keywrd)

      else if (keywrd == 'INCLUDED') then
!           Cannot Recurse the INCLUDED Keyword in the INCLUDED file
!           Write Error Message: Repeat INCLUDED In Same Pathway
         call errhdl(path,modnam,'E','135',keywrd)

      else if (keywrd == 'FINISHED') then
!           Cannot Use the FINISHED Keyword in the INCLUDED File
         call errhdl(path,modnam,'E','140',keywrd)

!        Process Input Card Based on Pathway
      else if (path == 'SO') then
!           Process SOurce Pathway Cards                    ---   CALL SOCARD
         call socard
      else if (path == 'RE') then
!           Process REceptor Pathway Cards                  ---   CALL RECARD
         call recard
      else if (path == 'EV') then
!           Process EVent Pathway Cards                     ---   CALL EVCARD
         call evcard

      end if

!        Store the Current Keyword as the Previous Keyword
      pkeywd = keywrd

!        Cycle to next record
      cycle

999   eof = .true.
      continue

   end do
   eof = .false.

   go to 1002

888 continue
! --- Error occurred reading the included file, issue error message
   call errhdl(path,modnam,'E','510','INCLUDED')
   runerr = .true.

1002 continue

!     Close the INCLUDED File
   close (incunt)

   return
end subroutine includ
