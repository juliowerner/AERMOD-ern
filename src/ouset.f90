subroutine oucard
!***********************************************************************
!                 OUCARD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To process OUtput Pathway card images
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Added new MAXDAILY, MXDYBYYR, MAXDCONT, and NOHEADER
!                   keywords for new output options.
!                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/28/2011
!
!        MODIFIED:  Added new SUMMFILE keyword for optional summary
!                   file of high ranked values and new FILEFORM
!                   keyword for specifying use of exponential-formatted
!                   output for results files.
!                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        MODIFIED:  To add subroutine call for TOXXFILE option - 9/29/92
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
   modnam = 'OUCARD'

   if (keywrd == 'STARTING') then
!        Set Status Switch
      iostat(1) = iostat(1) + 1
      if (iostat(1) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'RECTABLE') then
!        Process High Value Output Option                   ---   CALL OUHIGH
      call ouhigh
!        Set Status Switch
      iostat(2) = iostat(2) + 1
   else if (keywrd == 'MAXTABLE') then
!        Process Maximum 50 Table Option                    ---   CALL OUMXVL
      call oumxvl
!        Set Status Switch
      iostat(3) = iostat(3) + 1
   else if (keywrd == 'DAYTABLE') then
!        Process Daily Value Table Option                   ---   CALL OUDALY
      call oudaly
!        Set Status Switch
      iostat(4) = iostat(4) + 1
      if (iostat(4) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'MAXIFILE') then
!        Process Maximum Value (Threshold) File Option      ---   CALL OUMXFL
      call oumxfl
!        Set Status Switch
      iostat(5) = iostat(5) + 1
   else if (keywrd == 'POSTFILE') then
!        Process Postprocessing File Output Option          ---   CALL OUPOST
      call oupost
!        Set Status Switch
      iostat(6) = iostat(6) + 1
   else if (keywrd == 'PLOTFILE') then
!        Process Plotting File Output Option                ---   CALL OUPLOT
      call ouplot
!        Set Status Switch
      iostat(7) = iostat(7) + 1
   else if (keywrd == 'TOXXFILE') then
!        Process TOXXFILE Output Option                     ---   CALL OUTOXX
      call outoxx
!        Set Status Switch
      iostat(8) = iostat(8) + 1
   else if (keywrd == 'SEASONHR') then
      if (.not. scim) then
!           Process Season by Hour-of-Day Output Option     ---   CALL OUSEAS
         call ouseas
!           Set Status Switch
         iostat(9) = iostat(9) + 1
      else
!           Write Error Message: Conflicting Options SCIM and SEASONHR
         call errhdl(path,modnam,'E','154',keywrd)
      end if
   else if (keywrd == 'RANKFILE') then
!        Process RANKFILE Output Option                     ---   CALL OURANK
      call ourank
!        Set Status Switch
      iostat(10) = iostat(10) + 1
   else if (keywrd == 'EVALFILE') then
!        Process EVALFILE Output Option                     ---   CALL OUEVAL
      call oueval
!        Set Status Switch
      iostat(11) = iostat(11) + 1
   else if (keywrd == 'SUMMFILE') then
!        Process SUMMFILE Output Option                     ---   CALL OUSUMM
      call ousumm
!        Set Status Switch
      iostat(12) = iostat(12) + 1
      if (iostat(12) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'FILEFORM') then
!        Process FILEFORM Output Option                     ---   CALL FILEFORM
      call fileform
!        Set Status Switch
      iostat(13) = iostat(13) + 1
      if (iostat(13) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'MAXDAILY') then
      if (no2ave .or. so2ave .or. pm25ave) then
         do iave = 1, numave
            if (kave(iave) == 1 .or. kave(iave) == 24) then
!                 Process Maximum Daily 1-hr File Output Option ---   CALL OUMAXDLY
               call oumaxdly
!                 Set Status Switch
               iostat(14) = iostat(14) + 1
               exit
            end if
         end do
      else
! ---       MAXDAILY option is only applicable to NO2AVE or SO2AVE 1-hr processing
         call errhdl(path,modnam,'E','162',keywrd)
         go to 999
      end if
      if (iostat(14) < 1) then
!           WRITE Error Message: MAXDAILY Option without 1-hr averages
         call errhdl(path,modnam,'E','162',keywrd)
      end if
   else if (keywrd == 'MXDYBYYR') then
      if (no2ave .or. so2ave .or. pm25ave) then
         do iave = 1, numave
            if (kave(iave) == 1 .or. kave(iave) == 24) then
!                 Process Maximum Daily 1-hr File Output Option ---   CALL OUMXDLY_BYYR
               call oumxdly_byyr
!                 Set Status Switch
               iostat(15) = iostat(15) + 1
               exit
            end if
         end do
      else
! ---       MXDYBYYR option is only applicable to NO2AVE or SO2AVE 1-hr processing
         call errhdl(path,modnam,'E','162',keywrd)
         go to 999
      end if
      if (iostat(15) < 1) then
!           WRITE Error Message: MXDYBYYR Option without 1-hr averages
         call errhdl(path,modnam,'E','162',keywrd)
      end if
   else if (keywrd == 'MAXDCONT') then
      if (no2ave .or. so2ave .or. pm25ave) then
         do iave = 1, numave
            if (kave(iave) == 1 .or. kave(iave) == 24) then
!                 Process Maximum Daily 1-hr File Output Option ---   CALL OUMAXD_CONT
               call oumaxd_cont
!                 Set Status Switch
               iostat(16) = iostat(16) + 1
               exit
            end if
         end do
      else
! ---       MAXDCONT option is only applicable to PM25 24-hr or NO2AVE or SO2AVE 1-hr processing
         call errhdl(path,modnam,'E','163',keywrd)
         go to 999
      end if
      if (iostat(16) < 1) then
!           WRITE Error Message: MAXDCONT Option without 1-hr or 24-hr averages
         call errhdl(path,modnam,'E','163',keywrd)
      end if
   else if (keywrd == 'NOHEADER') then
!        Process NOHEADER Output Option                     ---   CALL NOHEADER
      call noheader
!        Set Status Switch
      iostat(18) = iostat(18) + 1
      if (iostat(18) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
   else if (keywrd == 'FINISHED') then
!        Set Status Switch
      iostat(50) = iostat(50) + 1
      if (iostat(50) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      end if
!        Check The Consistency of The Output Options
      call outqa
   else
!        Write Error Message: Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

999 continue

   return
end subroutine oucard

subroutine outqa
!***********************************************************************
!                 OUTQA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To process OUtput Pathway card images QA Check
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Included logical variable PLFILE for PLOTFILEs
!                   in the check for output file options that use
!                   the FILEFORM keyword specifying whether
!                   fixed-format or exp-format is used. Previous
!                   version erroneously issued warning message '399'
!                   if PLOTFILE was the only relevant output option
!                   used with the OU FILEFORM keyword.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/19/2011
!
!                   Corrected write statement for message '540'
!                   to accommodate MONTH average option, labeled
!                   as 720-hr option.  Also modified to include
!                   error checking for file name and file unit
!                   conflicts across output file options.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!                   To check for EVALFILE option without EVALCART.
!                   R.W. Brode, MACTEC/PES - 10/26/04
!
!                   To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  Pathway (OU) and Keyword
!
!        OUTPUTS: Output Messages
!
!        CALLED FROM: OUCARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, l, m, ival, idcst1, NumNoHeader
!     JAT D065 8/9/21, FOUND SET BUT NEVER USED
!      LOGICAL OUTOPT, FOUND
   logical :: outopt
   character :: keymsg*8, msg1*3

!     Variable Initializations
   modnam = 'OUTQA'
   msg1   = '-HR'
   outopt = .false.
!     JAT D065 8/9/21, FOUND SET BUT NEVER USED
!      FOUND  = .FALSE.

!     Check If Missing Mandatory Keyword
   if (iostat(1) == 0) then
      call errhdl(path,modnam,'E','130','STARTING')
   end if

!     Check For Lack of Any Output Option Cards
   do i = 2, 12
      if (iostat(i) > 0) then
         outopt = .true.
      end if
   end do
   if (.not.outopt .and. .not.period .and. .not.annual) then
!        WRITE Error Message - No Output Keywords and No PERIOD Averages
      call errhdl(path,modnam,'E','189','  ')
   end if

   do iave = 1, numave
      idcst1 = 0
      do ival = 1, nval
         if (nhiave(ival,iave) == 1) then
            idcst1 = 1
         end if
      end do
      if (idcst1==0 .and. maxave(iave)==0 .and.&
      &idytab(iave)==0) then
         write(keymsg,'(I3.3,A3)') kave(iave), msg1
         call errhdl(path,modnam,'W','540',keymsg)
      end if
   end do

!     Check for DAYTABLE Option With SAVEFILE or INITFILE Options
   if (daytab .and. (rstsav .or. rstinp)) then
!        WRITE Warning Message: DAYTABLE Results Overwritten on Re-start
      call errhdl(path,modnam,'W','190','DAYTABLE')
   end if
!     Check for TOXXFILE Option With SAVEFILE or INITFILE Options
   if (txfile .and. (rstsav .or. rstinp)) then
!        WRITE Error Message: Incompatible Options
      call errhdl(path,modnam,'E','190','TOXXFILE')
   end if
!     Check for EVALFILE Option With SAVEFILE or INITFILE Options
   if (rstsav .or. rstinp) then
      do i = 1, numsrc
         if (eval(i)) then
!              WRITE Warning Message: EVALFILE results overwritten on Re-start
            call errhdl(path,modnam,'W','190','EVALFILE')
            exit
         end if
      end do
   end if

!     Check for PM-2.5, NO2, or SO2 processing with EVENTFIL and no MAXIFILE
   if ((pm25ave .or. no2ave .or. so2ave) .and. events .and.&
   &.not.mxfile) then
!        Write Warning Message:  EVENTFIL option not compatible
!        with PM-2.5, NO2, or SO2 processing without MAXIFILE option
      call errhdl(path,modnam,'W','191','EVENTFIL Opt')
      events = .false.
   end if

!     Check for EVALFILE Option without EVALCART Inputs
   if (iostat(11) > 0 .and. numarc == 0) then
      call errhdl(path,modnam,'E','256','NUMARC=0')
   end if

! --- Check for FILEFORM keyword without applicable output file options
   if (iostat(13) > 0 .and. file_format == 'EXP') then
      if (.not.mxfile .and. .not.ppfile .and. .not.plfile .and.&
      &.not.rkfile .and.&
      &.not.anpost .and. .not.anplot .and. .not.seasonhr .and.&
      &.not.mxdaily .and. .not.mxdaily_byyr .and.&
      &.not.l_maxdcont) then
!           Write Warning Message: FILEFORM keyword ignored
         call errhdl(path,modnam,'W','399','FILEFORM')
      end if
   end if

! --- Check for NOHEADER for non-selected output file option
   NumNoHeader = 0
   do i = 1, 8
      if (L_NoHeader(i)) then
         NumNoHeader = NumNoHeader + 1
      end if
   end do
   if (NumNoHeader > 0 .and. NumNoHeader < 8) then
! ---    NOHEADER option selected for at least one output
!        type, but not all; check for output type on the
!        NOHEADER keyword that was not specified by user
      if (L_NoHeader(1) .and. iostat(5) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','MAXIFILE')
      end if
      if (L_NoHeader(2) .and. iostat(6) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','POSTFILE')
      end if
      if (L_NoHeader(3) .and. iostat(7) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','PLOTFILE')
      end if
      if (L_NoHeader(4) .and. iostat(9) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','SEASONHR')
      end if
      if (L_NoHeader(5) .and. iostat(10) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','RANKFILE')
      end if
      if (L_NoHeader(6) .and. iostat(14) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','MAXDAILY')
      end if
      if (L_NoHeader(7) .and. iostat(15) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','MXDYBYYR')
      end if
      if (L_NoHeader(8) .and. iostat(16) < 1) then
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','164','MAXDCONT')
      end if
   end if

!---- Check for filename & fileunit conflicts across different types
!     of output files:
   do i = 1, nhival
      do j = 1, numgrp
         do k = 1, numave
            do l = 1, numgrp
               do m = 1, numave
                  if (pltfil(i,j,k) == pstfil(l,m) .and.&
                  &iplunt(i,j,k) /= ipsunt(l,m)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) /= pstfil(l,m) .and.&
                  &iplunt(i,j,k) == ipsunt(l,m)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) == thrfil(l,m) .and.&
                  &iplunt(i,j,k) /= imxunt(l,m)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) /= thrfil(l,m) .and.&
                  &iplunt(i,j,k) == imxunt(l,m)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) == annpst(l) .and.&
                  &iplunt(i,j,k) /= iapunt(l)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) /= annpst(l) .and.&
                  &iplunt(i,j,k) == iapunt(l)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) == annplt(l) .and.&
                  &iplunt(i,j,k) /= ippunt(l)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  else if (pltfil(i,j,k) /= annplt(l) .and.&
                  &iplunt(i,j,k) == ippunt(l)) then
!              Write Error Message: Conflicting Inputs
                     if (l > 999) then
                        write(dummy,'("999+ ",I3)') kave(m)
                     else
                        write(dummy,'(I4,1X,I3)') l, kave(m)
                     end if
                     call errhdl(path,modnam,'E','555',dummy)
                     cycle
                  end if
               end do
            end do
         end do
      end do
   end do

   return
end subroutine outqa

subroutine ouhigh
!***********************************************************************
!                 OUHIGH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To process High Value By Receptor Table
!                 Output Selections
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To allow user-specified ranks up to the
!                    999th-highest values.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
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

   integer :: i, j, iloch(nave), iprdt, iprdt1, iprdt2, isprd, ieprd,&
   &highst(nval)
   character :: lprd*8, hprd*8, nchr1(10)*8, nchr2(10)*5
   logical :: found, rmark

!     Variable Initializations
   data (nchr1(i),i=1,10) /'FIRST','SECOND','THIRD','FOURTH',&
   &'FIFTH','SIXTH','SEVENTH','EIGHTH',&
   &'NINTH','TENTH'/
   data (nchr2(i),i=1,10) /'1ST','2ND','3RD','4TH','5TH',&
   &'6TH','7TH','8TH','9TH','10TH'/
   modnam = 'OUHIGH'
   found  = .false.

   do i = 1, nval
      highst(i) = 0
   end do

   do i = 1, nave
      iloch(i) = 0
   end do

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > nval+3) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'ALLAVE') then
!        Go For All Averaging Periods
      do i = 1, numave
         inhi(i) = 1
         iloch(i) = 1
      end do
      found = .true.
   else if (field(3) == 'MONTH' .and. month) then
!        Set Value of IPRDT = 720 for MONTHly Averages
      iprdt = 720
!        Search The Period to find out the Location
      do i = 1, numave
         if (iprdt == kave(i)) then
            found = .true.
            inhi(i) = 1
            iloch(i) = 1
         end if
      end do
   else
      call fsplit(path,keywrd,field(3),ilen_fld,'-',rmark,lprd,hprd)
!        Single Time Period
      if (hprd == lprd) then
         call stonum(hprd,8,fnum,imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field and assign value of 0
            call errhdl(path,modnam,'E','208',keywrd)
            iprdt1 = 0
            go to 115
         else
            iprdt1 = nint(fnum)
         end if
!           Search The Period to find out the Location
         do i = 1, numave
            if (iprdt1 == kave(i)) then
               found = .true.
               inhi(i) = 1
               iloch(i) = 1
            end if
         end do
      else
!           Find The Lower Boundary
         call stonum(lprd,8,fnum,imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field and assign value of 0
            call errhdl(path,modnam,'E','208',keywrd)
            iprdt1 = 0
            go to 114
         else
            iprdt1 = nint(fnum)
         end if
!           Find The Upper Boundary
114      call stonum(hprd,8,fnum,imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field
            call errhdl(path,modnam,'E','208',keywrd)
            go to 115
         end if
         iprdt2 = nint(fnum)
!           Search The Period to find out the Location
         do i = 1, numave
            if (kave(i)>=iprdt1 .and.&
            &kave(i)<=iprdt2) then
               found = .true.
               inhi(i) = 1
               iloch(i) = 1
            end if
         end do
!           Multi Time Period
      end if
   end if

115 continue

!     Check Averaging Period Against KAVE Array,
   if (.not. found) then
!        Error Message:E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Begin LOOP Through Fields
   do i = 4, ifc
!        Retrieve The High Value
      call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,lprd,hprd)
!        Fit To The Status Array
      isprd = 0
      ieprd = 0
! ---    First check for simple numeric value
      call stonum(lprd,ilen_fld,fnum,imit)
      if (imit == 1) then
         isprd = int(fnum)
      end if
      call stonum(hprd,ilen_fld,fnum,imit)
      if (imit == 1) then
         ieprd = int(fnum)
      end if
      if (isprd > 999 .or. ieprd > 999) then
!           Write Error Message:Illegal Parameter Field
         call errhdl(path,modnam,'E','203','HIVALU')
         cycle
      end if
      if (isprd == 0 .or. ieprd == 0) then
!           Check for acceptable character string if non-numeric
         do j = 1, 10
            if (lprd==nchr1(j) .or.&
            &lprd==nchr2(j)) isprd = j
            if (hprd==nchr1(j) .or.&
            &hprd==nchr2(j)) ieprd = j
         end do
      end if
      if (isprd==0 .or. ieprd==0) then
!           Write Error Message:Illegal Parameter Field
         call errhdl(path,modnam,'E','203','HIVALU')
         cycle
      end if
      if (isprd>nval .or. ieprd>nval) then
!           Write Error Message: High Value Requested Exceeds NVAL
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NVAL='',I7)') nval
         call errhdl(path,modnam,'E','290',dummy)
         cycle
      end if
      do j = isprd,ieprd
         highst(j) = 1
      end do
!     End LOOP Through Fields
   end do

!     Set Array Switch to Indicate Which High Values to Report
!     And Set the Maximum Number of High Values, NHIVAL
   do i = 1, numave
      do j = 1, nval
         if (highst(j)==1 .and. iloch(i)==1) then
            nhiave(j,i) = 1
            if (j > nhival) then
               nhival = j
            end if
         end if
      end do
   end do

999 return
end subroutine ouhigh

subroutine oumxvl
!***********************************************************************
!                 OUMXVL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Maximum (Overall) Value Table
!                 Output Selections for MAXTABLE keyword
!
!        PROGRAMMER: Jeff Wang, Roger Brode
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

   integer :: i, j, iprdt
   logical :: found

!     Variable Initializations
   modnam = 'OUMXVL'
   found = .false.

!     Check for Appropriate Number of Fields
   if (ifc == 2) then
!        Error Message: No Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc == 3) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 4) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'ALLAVE') then
!        Go For All Averaging Periods
      do i = 1, numave
         maxave(i) = 1
      end do
      found = .true.
   else
      if (field(3) == 'MONTH' .and. month) then
!           Set Value of IPRDT = 720 for MONTHly Averages
         iprdt = 720
      else
         call stonum(field(3),ilen_fld,fnum,imit)
         if (imit /= 1) then
!              Write Error Message:Invalid Numerical Field
            call errhdl(path,modnam,'E','208',keywrd)
            go to 999
         end if
         iprdt = nint(fnum)
      end if
!        Check Averaging Period Against KAVE Array
      j = 1
      do while (.not.found .and. j<=numave)
         if (iprdt == kave(j)) then
            found = .true.
            indave = j
            maxave(j) = 1
         end if
         j = j + 1
      end do
   end if
   if (.not. found) then
!        Error Message: E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Set Number of Maximum Values to Sort
   call stonum(field(4),ilen_fld,fnum,imit)
   if (imit /= 1) then
!        Write Error Message:Invalid Numerical Field
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
   inum = nint(fnum)

   if (field(3) == 'ALLAVE') then
!        Go For All Averaging Periods
      do i = 1, numave
         imxval(i) = inum
      end do
   else
      imxval(indave) = inum
   end if

999 return
end subroutine oumxvl

subroutine oudaly
!***********************************************************************
!                 OUDALY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Daily Concurrent Value Table
!                 Output Selections
!
!        PROGRAMMER: Roger Brode
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

   integer :: i, j, iprdt
   logical :: found

!     Variable Initializations
   modnam = 'OUDALY'
   found  = .false.

!     Check for Appropriate Number of Fields
   if (ifc == 2) then
!        Error Message: No AvePer And High Value
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc > numave+2) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period(s)
   if (field(3) == 'ALLAVE') then
!        Go For All Averaging Periods
      do i = 1, numave
         idytab(i) = 1
      end do
!        Set Logical Switch Indicating That Daily Value Tables Are Generated
      daytab = .true.
   else
      do i = 3, ifc
         if (field(i) == 'MONTH' .and. month) then
!              Set Value of IPRDT = 720 for MONTHly Averages
            iprdt = 720
         else
            found = .false.
            call stonum(field(i),ilen_fld,fnum,imit)
            if (imit /= 1) then
!                 Write Error Message:Invalid Numerical Field
               call errhdl(path,modnam,'E','208',keywrd)
               go to 999
            end if
            iprdt = nint(fnum)
         end if
!           Check Averaging Period Against KAVE Array
         j = 1
         do while (.not.found .and. j<=numave)
            if (iprdt == kave(j)) then
               found = .true.
               idytab(j) = 1
            end if
            j = j + 1
         end do
         if (.not. found) then
!              Error Message:E203 KAVE Not Match With Pre-Defined One
            call errhdl(path,modnam,'E','203','AVEPER')
            go to 999
         end if
      end do
!        Set Logical Switch Indicating That Daily Value Tables Are Generated
      daytab = .true.
   end if

999 return
end subroutine oudaly

subroutine oumxfl
!***********************************************************************
!                 OUMXFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Threshold Value Output Selections
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Modified to correct problems with MAXIFILE outputs
!                   for re-started model runs using the SAVEFILE/INITFILE
!                   option.  Previous versions used the 8-digit date
!                   from the MAXIFILE files to compare with the "full"
!                   12-digit date read from the INITFILE.  Also added
!                   error handling for missing MAXIFILE files with
!                   INITFILE option, and for MAXIFILE files with data
!                   past the start date for the MULTYEAR option.
!                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
!        MODIFIED:  To skip writing of header records if FATAL error
!                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
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

   integer :: i, j, iprdt, idat, idat8
   integer :: irsyr, irsdate
   character :: inpgrp*8, buf90*90
   logical :: found, l_exists

!     Variable Initializations
   modnam = 'OUMXFL'
   l_exists = .false.
   buf90  = ' '
   idat   = 0
   idat8  = 0

!     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
   mxfile = .true.

!     Check If Enough Field
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 6) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'MONTH' .and. month) then
!        Set Value of IPRDT = 720 for MONTHly Averages
      iprdt = 720
   else
      call stonum(field(3),ilen_fld,fnum,imit)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      iprdt = nint(fnum)
   end if

!     Check Averaging Period Against KAVE Array
   found = .false.
   indave = 0
   j = 1
   do while (.not.found .and. j<=numave)
      if (iprdt == kave(j)) then
         found = .true.
         indave = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(4)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Set Switch and Check for Previous MAXIFILE Card
!     for This Averaging Period & Group ID
   maxfle(indgrp,indave) = maxfle(indgrp,indave) + 1
   if (maxfle(indgrp,indave) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

!     Retrieve Threshold
   call stodbl(field(5),ilen_fld,dnum,imit)
!     Check for Valid Threshold Value
   if (imit /= 1) then
!        Write Error Message:Invalid Numerical Field
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
   thresh(indgrp,indave) = dnum

   if ((loce(6)-locb(6)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      thrfil(indgrp,indave) = runst1(locb(6):loce(6))
   else
!        WRITE Error Message:  THRFIL Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 7) then
      call stonum(field(7),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         imxunt(indgrp,indave) = nint(fnum)
      else
         imxunt(indgrp,indave) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (100's)
      imxunt(indgrp,indave) = 100 + indgrp*10 + indave
      if (indgrp >= 10 .or. indave >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do j = 1, numave
      do i = 1, numgrp
         if (i /= indgrp .or. j /= indave) then
            if (thrfil(indgrp,indave) == thrfil(i,j) .and.&
            &imxunt(indgrp,indave) == imxunt(i,j)) then
               found = .true.
            else if (thrfil(indgrp,indave) == thrfil(i,j) .and.&
            &imxunt(indgrp,indave) /= imxunt(i,j)) then
!                 Write Error Message: Conflicting Inputs
               call errhdl(path,modnam,'E','550',keywrd)
               go to 999
            else if (thrfil(indgrp,indave) /= thrfil(i,j) .and.&
            &imxunt(indgrp,indave) == imxunt(i,j)) then
!                 Write Error Message: Conflicting Inputs
               call errhdl(path,modnam,'E','550',keywrd)
               go to 999
            end if
         end if
      end do
   end do

   if (.not. found) then
!        First Time File is Identified; check for re-start option (RSTINP)
!        before OPENing File
      if (rstinp) then
!           Results Arrays Are To Be Initialized From Re-start File.
!           Check for existence of file first, then
!           Read Start Date From File and Rewind.
         inquire (file=thrfil(indgrp,indave),exist=l_exists)
         dummy = 'MAXIFILE'
         if (l_exists) then
            open(imxunt(indgrp,indave),err=99,&
            &file=thrfil(indgrp,indave),&
            &iostat=ioerrn,status='OLD')
         else
!              File does not exist; restart option will not work
            call errhdl(path,modnam,'E','585',dummy)
            return
         end if
!           Results Arrays Are To Be Initialized From Re-start File.
!           Read Start Date From File, IRSDATE, and Rewind.
         dummy = 'INITFILE'
         read(irsunt,err=919,end=919) irsdate
         rewind irsunt
!           Now Position MAXIFILE To End of File, But Not Past IRSDATE.
         dummy = 'MAXIFILE'
         eof = .false.
         do while (.not. eof)
            read(imxunt(indgrp,indave),'(A90:)',err=919,&
            &end=199) buf90
            if (buf90(1:1) /= '*') then
!                 Record Is Not Part of Header - Read Date For This Record
               read(buf90(15:22),'(I8)',err=919) idat8
!                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
               irsyr = idat8/1000000
!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
               call long_date(idat8,idat,irsyr,irsyr) !Determine the longform date
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!                  IF (IRSYR .GE. ISTRT_WIND .and. IRSYR .LE. 99) THEN
!                     IRSYR = ISTRT_CENT*100 + IRSYR
!                     IDAT  = ISTRT_CENT*100000000 + IDAT8
!                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
!                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
!                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
!                  END IF

               if (idat > irsdate) then
!                    Date of MAXIFILE Event Is Greater Than Start Date
!                    From Save File.  Treat As End of File to Exit Loop.
                  if (multyr) then
! ---                   If this is a MULTYEAR run, then MAXIFILE date should not
!                       be greater than IRSDATE; issue error message
                     write(dummy,'(I8.8)') idat8
                     call errhdl(path,modnam,'E','592',dummy)
                  end if
                  go to 199
               end if
            end if
            go to 11
199         eof = .true.
11          continue
         end do
         eof = .false.
!           End of file or IRSDATE has been passed; backspace file
         backspace imxunt(indgrp,indave)
!           Skip Header Records
         go to 999
      else
! ---       This is not a restarted run; just open the file
         open(imxunt(indgrp,indave),err=99,&
         &file=thrfil(indgrp,indave),&
         &iostat=ioerrn,status='REPLACE')
      end if
   else if (found .and. rstinp) then
!        This file is already open, and this run is a
!        Re-start from an earlier run
!        Skip Header Records
      go to 999
   end if

!     Write Header to File.  Skip Header if FATAL.
   if (run .and. .not.fatal .and. .not.L_NoHeader(1)) then
      write(imxunt(indgrp,indave),9005) versn, title1(1:68),&
      &rundat
9005  format('* AERMOD (',a6,'): ',a68,5x,a8)
      write(imxunt(indgrp,indave),9007) c_metver, runtim,&
      &MODOPS_String(1:len_trim(MODOPS_String))
9007  format('* AERMET (',a6,'):',t93,a8,&
      &/'* MODELING OPTIONS USED: ',a:)
      write(imxunt(indgrp,indave),9010) chrave(indave),&
      &thresh(indgrp,indave), grpid(indgrp),&
      &thrfrm(1:len_trim(thrfrm))
9010  format('*',9x,'MAXI-FILE FOR ',a5,' VALUES ',&
      &'>= A THRESHOLD OF ',g12.4,&
      &/'*',9x,'FOR SOURCE GROUP: ',a8,&
      &/'*',9x,'FORMAT: ',a:)
      write(imxunt(indgrp,indave),9020) chidep(1,1), chidep(2,1),&
      &chidep(3,1)
9020  format('*AVE',3x,'GRP',5x,'DATE',11x,'X',13x,'Y',8x,'ZELEV',&
      &3x,'ZHILL',3x,'ZFLAG',2x,3a4,&
      &/'*___',2(1x,'________'),2(2x,'____________'),&
      &3(2x,'______'),'  ____________')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("MAXFL",I3.3)') imxunt(indgrp,indave)
   call errhdl(path,modnam,'E','500',dummy)

   go to 999

!     WRITE Error Message for Error Reading File
919 call errhdl(path,modnam,'E','510',dummy)

999 continue

   return
end subroutine oumxfl

subroutine oupost
!***********************************************************************
!                 OUPOST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Post-processor File Output Selections
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Modified to correct problems with POSTFILE outputs
!                   for re-started model runs using the SAVEFILE/INITFILE
!                   option.  Previous versions used the 8-digit date
!                   from the POSTFILE files to compare with the "full"
!                   12-digit date read from the INITFILE and also read
!                   the 8-digit date from the wrong columns for POSTFILEs.
!                   Also added error handling for missing POSTFILE files
!                   with INITFILE option, and for POSTFILE files with data
!                   past the start date for the MULTYEAR option.
!                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        MODIFIED:  Increased length of HDRFRM variable to avoid
!                   potential problems with portability of code.
!                   R. Brode, MACTEC/PES, 10/17/2005
!
!        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
!
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
!
!        MODIFIED:  To skip writing of header records if FATAL error
!                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
!
!        MODIFIED:  To change buffer length for PLOT format to 132, and
!                   change read statement to allow for multiple output
!                   types (CONC/DEPOS/etc.).   R.W. Brode, PES, Inc., 6/20/95
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

   integer :: i, j, iprdt, idat, idat8, istr, istp
   integer :: irsyr, irsdate
   character :: inpgrp*8, hdrfrm*400
   character (len = ilen_fld) :: buffer
   logical :: found, l_exists

!     Variable Initializations
   modnam = 'OUPOST'
   l_exists = .true.
   buffer = ' '
   hdrfrm = ' '
   idat   = 0
   idat8  = 0

!     Set Logical Switch Indicating That Postprocessor File(s) Are Generated
   ppfile = .true.

!     Create Header Format for Columns
   write(hdrfrm,9020) numtyp, numtyp+2

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 6) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'PERIOD' .and. period) then
!        Post File is for PERIOD Averages                   ---   CALL PERPST
      call perpst
!        Exit to End
      go to 999
   else if (field(3) == 'ANNUAL' .and. annual) then
!        Post File is for PERIOD Averages                   ---   CALL PERPST
      call perpst
!        Exit to End
      go to 999
   else if (field(3)=='PERIOD' .or. field(3)=='ANNUAL') then
!        Period Post File Selected But No PERIOD Averages Calculated
!        WRITE Error Message: Invalid Averaging Period Selected for POSTFILE
      call errhdl(path,modnam,'E','203',' AVEPER ')
      go to 999
   else if (field(3) == 'MONTH' .and. month) then
!        Set Value of IPRDT = 720 for MONTHly Averages
      iprdt = 720
   else
      call stonum(field(3),ilen_fld,fnum,imit)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      iprdt = nint(fnum)
   end if

!     Check Averaging Period Against KAVE Array
   found = .false.
   indave = 0
   j = 1
   do while (.not.found .and. j<=numave)
      if (iprdt == kave(j)) then
         found = .true.
         indave = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(4)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Retrieve Format Secondary Keyword
   if (field(5) == 'UNFORM') then
      ipsfrm(indgrp,indave) = 0
   else if (field(5) == 'PLOT') then
      ipsfrm(indgrp,indave) = 1
   else
!        Error Message: Invalid Format Specified for POSTFILE
      call errhdl(path,modnam,'E','203','FORMAT')
      go to 999
   end if

!     Set Switch and Check for Previous POSTFILE Card
!     for This Averaging Period & Group ID
   ipstfl(indgrp,indave) = ipstfl(indgrp,indave) + 1
   if (ipstfl(indgrp,indave) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

   if ((loce(6)-locb(6)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      pstfil(indgrp,indave) = runst1(locb(6):loce(6))
   else
!        WRITE Error Message:  PSTFIL Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 7) then
      call stonum(field(7),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         ipsunt(indgrp,indave) = nint(fnum)
      else
         ipsunt(indgrp,indave) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (200's)
      ipsunt(indgrp,indave) = 200 + indgrp*10 + indave
      if (indgrp >= 10 .or. indave >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do j = 1, numave
      do i = 1, numgrp
         if (i /= indgrp .or. j /= indave) then
            if (pstfil(indgrp,indave) == pstfil(i,j) .and.&
            &ipsunt(indgrp,indave) == ipsunt(i,j)) then
               found = .true.
            else if (pstfil(indgrp,indave) == pstfil(i,j) .and.&
            &ipsunt(indgrp,indave) /= ipsunt(i,j)) then
!                 Write Error Message: Conflicting Inputs
               call errhdl(path,modnam,'E','550',keywrd)
               go to 999
            else if (pstfil(indgrp,indave) /= pstfil(i,j) .and.&
            &ipsunt(indgrp,indave) == ipsunt(i,j)) then
!                 Write Error Message: Conflicting Inputs
               call errhdl(path,modnam,'E','550',keywrd)
               go to 999
            end if
         end if
      end do
   end do

!     Check Against POSTFILEs for PERIOD Averages
   do i = 1, numgrp
      if (pstfil(indgrp,indave) == annpst(i) .and.&
      &ipsunt(indgrp,indave) == iapunt(i)) then
         found = .true.
      else if (pstfil(indgrp,indave) == annpst(i) .and.&
      &ipsunt(indgrp,indave) /= iapunt(i)) then
!           Write Error Message: Conflicting Inputs
         call errhdl(path,modnam,'E','550',keywrd)
         go to 999
      else if (pstfil(indgrp,indave) /= annpst(i) .and.&
      &ipsunt(indgrp,indave) == iapunt(i)) then
!           Write Error Message: Conflicting Inputs
         call errhdl(path,modnam,'E','550',keywrd)
         go to 999
      end if
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      if (field(5) == 'UNFORM') then
!           First Time File is Identified; check for re-start option (RSTINP)
!           before OPENing File
         if (rstinp) then
!              Results Arrays Are To Be Initialized From Re-start File.
!              Check for existence of file first, then
!              Read Start Date From File and Rewind.
            inquire (file=pstfil(indgrp,indave),exist=l_exists)
            dummy = 'POSTFILE'
            if (l_exists) then
               open(ipsunt(indgrp,indave),err=99,&
               &file=pstfil(indgrp,indave),iostat=ioerrn,&
               &form='UNFORMATTED',status='OLD')
            else
!                 File does not exist; restart option will not work
               call errhdl(path,modnam,'E','585',dummy)
               return
            end if
!              Results Arrays Are To Be Initialized From Re-start File.
!              Read Start Date From File, IRSDATE, and Rewind.
            dummy = 'INITFILE'
            read(irsunt,err=919,end=919) irsdate
            rewind irsunt
!              Now Position POSTFILE To End of File, But Not Past IRSDATE.
            dummy = 'POSTFILE'
            eof = .false.
            do while (.not. eof)
               read(ipsunt(indgrp,indave),err=919,end=199) idat8
!                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
               irsyr = idat8/1000000
!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
               call long_date(idat8,idat,irsyr,irsyr) !Determine the longform date
! ---        D001 remove original calculation of year Wood 9/15/22
!                  IF (IRSYR .GE. ISTRT_WIND .and. IRSYR .LE. 99) THEN
!                     IRSYR = ISTRT_CENT*100 + IRSYR
!                     IDAT  = ISTRT_CENT*100000000 + IDAT8
!                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
!                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
!                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
!                  END IF

               if (idat > irsdate) then
!                    Date of POSTFILE Record Is Greater Than Start Date
!                    From Save File.  Treat As End of File to Exit Loop.
                  if (multyr) then
! ---                   If this is a MULTYEAR run, then POSTFILE date should not
!                       be greater than IRSDATE; issue error message
                     write(dummy,'(I8.8)') idat8
                     call errhdl(path,modnam,'E','593',dummy)
                  end if
                  go to 199
               end if
               go to 11
199            eof = .true.
11             continue
            end do
            eof = .false.
!              End of file or IRSDATE has been passed; backspace file
            backspace ipsunt(indgrp,indave)
!              Skip Header Records
            go to 999
         else
! ---          This is not a restarted run; just open the file
            open(ipsunt(indgrp,indave),err=99,&
            &file=pstfil(indgrp,indave),iostat=ioerrn,&
            &form='UNFORMATTED',status='REPLACE')
         end if
      else if (field(5) == 'PLOT') then
!           First Time File is Identified; check for re-start option (RSTINP)
!           before OPENing File
         if (rstinp) then
!              Results Arrays Are To Be Initialized From Re-start File.
!              Check for existence of file first, then
!              Read Start Date From File and Rewind.
            inquire (file=pstfil(indgrp,indave),exist=l_exists)
            dummy = 'POSTFILE'
            if (l_exists) then
               open(ipsunt(indgrp,indave),err=99,&
               &file=pstfil(indgrp,indave),&
               &iostat=ioerrn,form='FORMATTED',status='OLD')
            else
!                 File does not exist; restart option will not work
               call errhdl(path,modnam,'E','585',dummy)
               return
            end if
!              Results Arrays Are To Be Initialized From Re-start File.
!              Read Start Date From File, IRSDATE, and Rewind.
            dummy = 'INITFILE'
            read(irsunt,err=919,end=919) irsdate
            rewind irsunt
!              Now Position POSTFILE To End of File, But Not Past IRSDATE.
            dummy = 'POSTFILE'
            eof = .false.
            do while (.not. eof)
               read(ipsunt(indgrp,indave),'(A:)',&
               &err=919,end=299) buffer
               if (buffer(1:1) /= '*') then
!                    Record Is Not Part of Header - Read Date For This Record
!                    First calculate start & end of date string based on NUMTYP
                  istr = 90 + 14*(numtyp-1)
                  istp = istr + 7
                  read(buffer(istr:istp),'(I8)',err=919) idat8
!                    Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
                  irsyr = idat8/1000000
!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
                  call long_date(idat8,idat,irsyr,irsyr) !Determine the longform date
! ---        D001 remove original calculation of year Wood 9/15/22
!                     IF (IRSYR .GE. ISTRT_WIND .and. IRSYR .LE. 99) THEN
!                        IRSYR = ISTRT_CENT*100 + IRSYR
!                        IDAT  = ISTRT_CENT*100000000 + IDAT8
!                     ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
!                        IRSYR = (ISTRT_CENT+1)*100 + IRSYR
!                        IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
!                     END IF

               else
!                    Header record - cycle to next record
                  cycle
               end if
               if (idat > irsdate) then
!                    Date of POSTFILE Record Is Greater Than Start Date
!                    From Save File.  Treat As End of File to Exit Loop.
                  if (multyr) then
! ---                   If this is a MULTYEAR run, then POSTFILE date should not
!                       be greater than IRSDATE; issue error message
                     write(dummy,'(I8.8)') idat8
                     call errhdl(path,modnam,'E','593',dummy)
                  end if
                  go to 299
               end if
               go to 21
299            eof = .true.
21             continue
            end do
            eof = .false.
!              End of file or IRSDATE has been passed; backspace file
            backspace ipsunt(indgrp,indave)
!              Skip Header Records
            go to 999
         else
! ---          This is not a restarted run; just open the file
            open(ipsunt(indgrp,indave),err=99,&
            &file=pstfil(indgrp,indave),&
            &iostat=ioerrn,form='FORMATTED',status='REPLACE')
         end if
      end if
   else if (found .and. rstinp) then
!        This file is already open, and this run is a
!        Re-start from an earlier run
!        Skip Header Records
      go to 999
   end if

!     Write Header to File for Formatted Plot Files.  Skip Header if FATAL.
   if (run .and. .not.fatal .and. field(5)=='PLOT' .and.&
   &.not.L_NoHeader(2)) then
      write(ipsunt(indgrp,indave),9005) versn, title1(1:68),&
      &rundat
9005  format('* AERMOD (',a6,'): ',a68,5x,a8)
      write(ipsunt(indgrp,indave),9007) c_metver, title2(1:68),&
      &runtim,&
      &MODOPS_String(1:len_trim(MODOPS_String))
9007  format('* AERMET (',a6,'): ',a68,t93,a8,&
      &/'* MODELING OPTIONS USED: ',a:)
      write(ipsunt(indgrp,indave),9010) chrave(indave),grpid(indgrp),&
      &numrec, pstfrm
9010  format('*',9x,'POST/PLOT FILE OF CONCURRENT ',a5,' VALUES',&
      &' FOR SOURCE GROUP: ',a8,&
      &/'*',9x,'FOR A TOTAL OF ',i5,' RECEPTORS.',&
      &/'*',9x,'FORMAT: ',a:)
      write(ipsunt(indgrp,indave),hdrfrm) (chidep(1,ityp),&
      &chidep(2,ityp), chidep(3,ityp), ityp=1,numtyp)
9020  format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
      &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''DATE'',5X,&
      &  ''NET ID'',/,''*'',',i1,'(1X,''____________ ''),1X,&
      & 3('' ______  ''),''______  ________  ________  ________'')')

   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("PSTFL",I3.3)') ipsunt(indgrp,indave)
   call errhdl(path,modnam,'E','500',dummy)

   go to 999

!     WRITE Error Message for Error Reading File
919 call errhdl(path,modnam,'E','510',dummy)

999 continue

   return
end subroutine oupost

subroutine ouplot
!***********************************************************************
!                 OUPLOT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Plot File Output Selections
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
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

   integer :: i, j, k, iprdt
   character :: nchr1(10)*8, nchr2(10)*5, inpgrp*8
   logical :: found

!     Variable Initializations
   data (nchr1(i),i=1,10) /'FIRST','SECOND','THIRD','FOURTH',&
   &'FIFTH','SIXTH','SEVENTH','EIGHTH',&
   &'NINTH','TENTH'/
   data (nchr2(i),i=1,10) /'1ST','2ND','3RD','4TH','5TH',&
   &'6TH','7TH','8TH','9TH','10TH'/
   modnam = 'OUPLOT'

!     Set Logical Switch Indicating That Plot File(s) Are Generated
   plfile = .true.

!     Check If Enough Field
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'PERIOD' .and. period) then
!        Plot File is for PERIOD Averages                   ---   CALL PERPLT
      call perplt
!        Exit to End
      go to 999
   else if (field(3) == 'ANNUAL' .and. annual) then
!        Plot File is for PERIOD Averages                   ---   CALL PERPLT
      call perplt
!        Exit to End
      go to 999
   else if (field(3)=='PERIOD' .or. field(3)=='ANNUAL') then
!        Period Plot File Selected But No PERIOD Averages Calculated
!        WRITE Error Message: Invalid Averaging Period Selected for PLOTFILE
      call errhdl(path,modnam,'E','203',' AVEPER ')
      go to 999
   else if (field(3) == 'MONTH' .and. month) then
!        Set Value of IPRDT = 720 for MONTHly Averages
      iprdt = 720
   else
      call stonum(field(3),ilen_fld,fnum,imit)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      iprdt = nint(fnum)
   end if

!     Check Short Term Averaging Period Against KAVE Array
   found = .false.
   indave = 0
   j = 1
   do while (.not.found .and. j<=numave)
      if (iprdt == kave(j)) then
         found = .true.
         indave = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(4)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Retrieve High Value
   found = .false.
   indval = 0
!     First check for numeric value
   call stonum(field(5),ilen_fld,fnum,imit)
   if (imit == 1) then
      found = .true.
      indval = int(fnum)
   else
      do i = 1, 10
         if (field(5)==nchr1(i) .or. field(5)==nchr2(i)) then
            found = .true.
            indval = i
         end if
      end do
   end if
   if (.not. found) then
!        Error Message:E203 INDVAL Not Match With Options
      call errhdl(path,modnam,'E','203','HIVALU')
      go to 999
   else if (indval > nhival) then
!        Error Message:E203 INDVAL Not Match With Options
      call errhdl(path,modnam,'E','203','HIVALU')
      go to 999
   end if

!     Check High Value Specified Against Previous Options
   if (nhiave(indval,indave) /= 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','203','HIVALU')
      go to 999
   end if

!     Set Switch and Check for Previous PLOTFILE Card
!     for This Averaging Period & Group ID
   ipltfl(indval,indgrp,indave) = ipltfl(indval,indgrp,indave) + 1
   if (ipltfl(indval,indgrp,indave) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

   if ((loce(6)-locb(6)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      pltfil(indval,indgrp,indave) = runst1(locb(6):loce(6))
   else
!        WRITE Error Message:  PLTFIL Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 7) then
      call stonum(field(7),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         iplunt(indval,indgrp,indave) = nint(fnum)
      else
         iplunt(indval,indgrp,indave) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (> 400)
      iplunt(indval,indgrp,indave) = (indval+3)*100+indgrp*10+indave
      if (indgrp >= 10 .or. indave >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do k = 1, numave
      do j = 1, numgrp
         do i = 1, nhival
            if (i/=indval .or. j/=indgrp .or. k/=indave) then
               if (pltfil(indval,indgrp,indave) == pltfil(i,j,k) .and.&
               &iplunt(indval,indgrp,indave) == iplunt(i,j,k)) then
                  found = .true.
               elseif (pltfil(indval,indgrp,indave)==pltfil(i,j,k).and.&
               &iplunt(indval,indgrp,indave)/=iplunt(i,j,k))then
!               Write Error Message: Conflicting Inputs
                  call errhdl(path,modnam,'E','550',keywrd)
                  go to 999
               elseif (pltfil(indval,indgrp,indave)/=pltfil(i,j,k).and.&
               &iplunt(indval,indgrp,indave)==iplunt(i,j,k))then
!               Write Error Message: Conflicting Inputs
                  call errhdl(path,modnam,'E','550',keywrd)
                  go to 999
               end if
            end if
         end do
      end do
   end do

!     Check Against PLOTFILEs for PERIOD Averages
   do i = 1, numgrp
      if (pltfil(indval,indgrp,indave) == annplt(i) .and.&
      &iplunt(indval,indgrp,indave) == ippunt(i)) then
         found = .true.
      else if (pltfil(indval,indgrp,indave) == annplt(i) .and.&
      &iplunt(indval,indgrp,indave) /= ippunt(i)) then
!          Write Error Message: Conflicting Inputs
         call errhdl(path,modnam,'E','550',keywrd)
         go to 999
      else if (pltfil(indval,indgrp,indave) /= annplt(i) .and.&
      &iplunt(indval,indgrp,indave) == ippunt(i)) then
!          Write Error Message: Conflicting Inputs
         call errhdl(path,modnam,'E','550',keywrd)
         go to 999
      end if
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      open(iplunt(indval,indgrp,indave),err=99,&
      &file=pltfil(indval,indgrp,indave),&
      &iostat=ioerrn,status='REPLACE')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("PLTFL",I3.3)') iplunt(indval,indgrp,indave)
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine ouplot

subroutine perpst
!***********************************************************************
!                 PERPST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Postprocessor File Output Selection for PERIOD
!                 Averages
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
!
!        INPUTS:  Input Runstream Parameters
!
!        OUTPUTS: Output Option Switches
!
!        CALLED FROM:   OUPLOT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j
   character :: inpgrp*8
   logical :: found

!     Variable Initializations
   modnam = 'PERPST'

!     Set Logical Switch Indicating That Post File(s) Are Generated
   anpost = .true.

!     Check If Too Many Fields
   if (ifc > 7) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(4)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Set Switch and Check for Previous POSTFILE Card
!     for This Averaging Period & Group ID
   ianpst(indgrp) = ianpst(indgrp) + 1
   if (ianpst(indgrp) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

!     Retrieve Format Secondary Keyword
   if (field(5) == 'UNFORM') then
      ianfrm(indgrp) = 0
   else if (field(5) == 'PLOT') then
      ianfrm(indgrp) = 1
   else
!        Error Message: Invalid Format Specified for POSTFILE
      call errhdl(path,modnam,'E','203','FORMAT')
      go to 999
   end if

   if ((loce(6)-locb(6)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      annpst(indgrp) = runst1(locb(6):loce(6))
   else
!        WRITE Error Message:  ANNPST Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 7) then
      call stonum(field(7),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         iapunt(indgrp) = nint(fnum)
      else
         iapunt(indgrp) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (300's)
      iapunt(indgrp) = 300 + indgrp*10 - 5
      if (indgrp >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numgrp
      if (i /= indgrp) then
         if (annpst(indgrp) == annpst(i) .and.&
         &iapunt(indgrp) == iapunt(i)) then
            found = .true.
         else if (annpst(indgrp) == annpst(i) .and.&
         &iapunt(indgrp) /= iapunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (annpst(indgrp) /= annpst(i) .and.&
         &iapunt(indgrp) == iapunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

!     Check Against POSTFILEs for Short Term Averages
   do j = 1, numave
      do i = 1, numgrp
         if (annpst(indgrp) == pstfil(i,j) .and.&
         &iapunt(indgrp) == ipsunt(i,j)) then
            found = .true.
         else if (annpst(indgrp) == pstfil(i,j) .and.&
         &iapunt(indgrp) /= ipsunt(i,j)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (annpst(indgrp) /= pstfil(i,j) .and.&
         &iapunt(indgrp) == ipsunt(i,j)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end do
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      if (field(5) == 'UNFORM') then
         open(iapunt(indgrp),err=99,file=annpst(indgrp),&
         &iostat=ioerrn,form='UNFORMATTED',status='REPLACE')
      else if (field(5) == 'PLOT') then
         open(iapunt(indgrp),err=99,file=annpst(indgrp),&
         &iostat=ioerrn,form='FORMATTED',status='REPLACE')
      end if
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("PSTFL",I3.3)') iapunt(indgrp)
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine perpst

subroutine perplt
!***********************************************************************
!                 PERPLT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Plot File Output Selection for PERIOD
!                 Averages
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To check correct field for long filenames.
!                   R. Brode, PES, 12/1/97
!
!        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
!
!        INPUTS:  Input Runstream Parameters
!
!        OUTPUTS: Output Option Switches
!
!        CALLED FROM:   OUPLOT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k
   character :: inpgrp*8
   logical :: found

!     Variable Initializations
   modnam = 'PERPLT'

!     Set Logical Switch Indicating That Plot File(s) Are Generated
   anplot = .true.

!     Check If Too Many Fields
   if (ifc > 6) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(4)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Set Switch and Check for Previous PLOTFILE Card
!     for This Averaging Period & Group ID
   ianplt(indgrp) = ianplt(indgrp) + 1
   if (ianplt(indgrp) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

   if ((loce(5)-locb(5)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      annplt(indgrp) = runst1(locb(5):loce(5))
   else
!        WRITE Error Message:  ANNPLT Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 6) then
      call stonum(field(6),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         ippunt(indgrp) = nint(fnum)
      else
         ippunt(indgrp) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (300's)
      ippunt(indgrp) = 300 + indgrp*10
      if (indgrp >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numgrp
      if (i /= indgrp) then
         if (annplt(indgrp) == annplt(i) .and.&
         &ippunt(indgrp) == ippunt(i)) then
            found = .true.
         else if (annplt(indgrp) == annplt(i) .and.&
         &ippunt(indgrp) /= ippunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (annplt(indgrp) /= annplt(i) .and.&
         &ippunt(indgrp) == ippunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

!     Check Against PLOTFILEs for Short Term Averages
   do k = 1, numave
      do j = 1, numgrp
         do i = 1, nhival
            if (annplt(indgrp) == pltfil(i,j,k) .and.&
            &ippunt(indgrp) == iplunt(i,j,k)) then
               found = .true.
            else if (annplt(indgrp) == pltfil(i,j,k) .and.&
            &ippunt(indgrp) /= iplunt(i,j,k)) then
!                 Write Error Message: Conflicting Inputs
               call errhdl(path,modnam,'E','550',keywrd)
               go to 999
            else if (annplt(indgrp) /= pltfil(i,j,k) .and.&
            &ippunt(indgrp) == iplunt(i,j,k)) then
!                 Write Error Message: Conflicting Inputs
               call errhdl(path,modnam,'E','550',keywrd)
               go to 999
            end if
         end do
      end do
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      open(ippunt(indgrp),err=99,file=annplt(indgrp),&
      &iostat=ioerrn,status='REPLACE')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("PLTFL",I3.3)') ippunt(indgrp)
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine perplt

subroutine outoxx
!***********************************************************************
!                 OUTOXX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process TOXXFILE Output Selections
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 29, 1992
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

   integer :: i, j, iprdt, nidum, nrdum, numper, rdum
   character :: buf12*12
   logical :: found

!     Variable Initializations
   modnam = 'OUTOXX'
   buf12  = '            '
   idum  = 0
   rdum  = 0
   nidum = 3
   nrdum = 9

!     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
   txfile = .true.

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 6) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'MONTH' .and. month) then
!        Set Value of IPRDT = 720 for MONTHly Averages
      iprdt = 720
   else
      call stonum(field(3),ilen_fld,fnum,imit)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      iprdt = nint(fnum)
   end if

!     Check Averaging Period Against KAVE Array
   found = .false.
   indave = 0
   j = 1
   do while (.not.found .and. j<=numave)
      if (iprdt == kave(j)) then
         found = .true.
         indave = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Check for Averaging Period Other Than 1-HOUR, and Write Warning
   if (iprdt /= 1) then
      write(dummy,'(2X,I4,2X)') iprdt
      call errhdl(path,modnam,'W','296',dummy)
   end if

!     Set Switch and Check for Previous TOXXFILE Card
!     for This Averaging Period
   itoxfl(indave) = itoxfl(indave) + 1
   if (itoxfl(indave) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

!     Retrieve Threshold
   call stodbl(field(4),ilen_fld,dnum,imit)
!     Check for Valid Threshold Value
   if (imit /= 1) then
!        Write Error Message:Invalid Numerical Field
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
   toxthr(indave) = dnum

   if ((loce(5)-locb(5)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      toxfil(indave) = runst1(locb(5):loce(5))
   else
!        WRITE Error Message:  TOXFIL Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 6) then
      call stonum(field(6),ilen_fld,fnum,imit)
!        Check for Valid Threshold Value
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         itxunt(indave) = nint(fnum)
      else
         itxunt(indave) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (300's)
      itxunt(indave) = 300 + indave
      if (indave >= 5) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numave
      if (i /= indave) then
         if (toxfil(indave) == toxfil(i) .and.&
         &itxunt(indave) == itxunt(i)) then
            found = .true.
         else if (toxfil(indave) == toxfil(i) .and.&
         &itxunt(indave) /= itxunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (toxfil(indave) /= toxfil(i) .and.&
         &itxunt(indave) == itxunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      open(itxunt(indave),err=99,file=toxfil(indave),&
      &form='UNFORMATTED',iostat=ioerrn,status='REPLACE')
   end if

!     Write Header to File
   if (run) then
      numper = nhours/iprdt
!        Write Header Records (BUF12 is used to fill out 80-character title)
      write(itxunt(indave)) title1(1:68), buf12
      write(itxunt(indave)) isyear, numgrp, numrec, numper, itab,&
      &nxtox, nytox, (idum,i=1,nidum)
      write(itxunt(indave)) toxthr(indave), (rdum,i=1,nrdum)
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("TOXFL",I3.3)') itxunt(indave)
   call errhdl(path,modnam,'E','500',dummy)

999 continue

   return
end subroutine outoxx

subroutine ouseas
!***********************************************************************
!                 OUSEAS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Season by Hour Output Selection
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    June 5, 1997
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

   integer :: i, j
   character :: inpgrp*8
   logical :: found

!     Variable Initializations
   modnam = 'OUSEAS'

!     Set Logical Switch Indicating That Plot File(s) Are Generated
   seasonhr = .true.

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(3)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Set Switch and Check for Previous SEASONHR Card
!     for This Group ID
   iseahr(indgrp) = iseahr(indgrp) + 1
   if (iseahr(indgrp) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

   if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      seahrs(indgrp) = runst1(locb(4):loce(4))
   else
!        WRITE Error Message:  SEAHRS Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 5) then
      call stonum(field(5),ilen_fld,fnum,imit)
!        Check for Valid Threshold Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         ishunt(indgrp) = nint(fnum)
      else
         ishunt(indgrp) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (300's)
      ishunt(indgrp) = 302 + indgrp*10
      if (indgrp >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numgrp
      if (i /= indgrp) then
         if (seahrs(indgrp) == seahrs(i) .and.&
         &ishunt(indgrp) == ishunt(i)) then
            found = .true.
         else if (seahrs(indgrp) == seahrs(i) .and.&
         &ishunt(indgrp) /= ishunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (seahrs(indgrp) /= seahrs(i) .and.&
         &ishunt(indgrp) == ishunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      open(ishunt(indgrp),err=99,file=seahrs(indgrp),&
      &iostat=ioerrn,status='REPLACE')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("SEAHR",I3.3)') ishunt(indgrp)
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine ouseas

subroutine ourank
!***********************************************************************
!                 OURANK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process RANKFILE Output Selections
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
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

   integer :: i, j, iprdt
   logical :: found

!     Variable Initializations
   modnam = 'OURANK'

!     Set Logical Switch Indicating That Ranked Value File(s) Are Generated
   rkfile = .true.

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 5) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 6) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Averaging Period
   if (field(3) == 'MONTH' .and. month) then
!        Set Value of IPRDT = 720 for MONTHly Averages
      iprdt = 720
   else
      call stonum(field(3),ilen_fld,fnum,imit)
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      iprdt = nint(fnum)
   end if

!     Check Averaging Period Against KAVE Array
   found = .false.
   indave = 0
   j = 1
   do while (.not.found .and. j<=numave)
      if (iprdt == kave(j)) then
         found = .true.
         indave = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message:E203 AVEPER Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if

!     Check for MAXTABLE Option for this INDAVE; Then Set Switch and
!     Check for Previous RANKFILE Card for This Averaging Period
   if (irnkfl(indave) == 0) then
      irnkfl(indave) = irnkfl(indave) + 1
   else
!        Error Message:E203 AVEPER Not Match With MAXTABLE Options
      call errhdl(path,modnam,'E','203','AVEPER')
      go to 999
   end if
   if (irnkfl(indave) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

!     Retrieve Rank Number (number of values to output)
   call stonum(field(4),ilen_fld,fnum,imit)
!     Check for Valid Threshold Value
   if (imit /= 1) then
!        Write Error Message:Invalid Numerical Field
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
   inum = nint(fnum)
   irkval(indave) = inum

   if ((loce(5)-locb(5)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      rnkfil(indave) = runst1(locb(5):loce(5))
   else
!        WRITE Error Message:  RNKFIL Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 6) then
      call stonum(field(6),ilen_fld,fnum,imit)
!        Check for Valid Threshold Value
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         irkunt(indave) = nint(fnum)
      else
         irkunt(indave) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (100's)
      irkunt(indave) = 100 + indave
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numave
      if (i /= indave) then
         if (rnkfil(indave) == rnkfil(i) .and.&
         &irkunt(indave) == irkunt(i)) then
            found = .true.
         else if (rnkfil(indave) == rnkfil(i) .and.&
         &irkunt(indave) /= irkunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (rnkfil(indave) /= rnkfil(i) .and.&
         &irkunt(indave) == irkunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      open(irkunt(indave),err=99,file=rnkfil(indave),&
      &form='FORMATTED',iostat=ioerrn,status='REPLACE')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("RNKFL",I3.3)') irkunt(indave)
   call errhdl(path,modnam,'E','500',dummy)

999 continue

   return
end subroutine ourank

subroutine oueval
!***********************************************************************
!                 OUEVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process EVALFILE Output Selections
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
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

   integer :: i, j, indsrc
   character :: inpsrc*8
   logical :: found

!     Variable Initializations
   modnam  = 'OUEVAL'
   indsrc  = 0
   evalfil = .true.

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      return
   else if (ifc < 4) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      return
   else if (ifc > 5) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      return
   end if

!     Retrieve Source Group ID
   inpsrc = field(3)
!     Check Source ID
   found = .false.
   indsrc = 0
   j = 1
   do while (.not.found .and. j<=numsrc)
      if (inpsrc == srcid(j)) then
         found = .true.
         indsrc = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 SRCID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','SRCID')
      return
   else
!        Set Logical Switch Indicating That EVALFILE is Generated for this Source
      eval(indsrc) = .true.
   end if

   if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      evlfil(indsrc) = runst1(locb(4):loce(4))
   else
!        WRITE Error Message:  EVLFIL Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 5) then
      call stonum(field(5),ilen_fld,fnum,imit)
!        Check for Valid Threshold Value
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         return
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         return
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         ielunt(indsrc) = nint(fnum)
      else
         ielunt(indsrc) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit
      ielunt(indsrc) = 400 + indsrc*5
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numsrc
      if (i /= indsrc) then
         if (evlfil(indsrc) == evlfil(i) .and.&
         &ielunt(indsrc) == ielunt(i)) then
            found = .true.
         else if (evlfil(indsrc) == evlfil(i) .and.&
         &ielunt(indsrc) /= ielunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            exit
         else if (evlfil(indsrc) /= evlfil(i) .and.&
         &ielunt(indsrc) == ielunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            exit
         end if
      end if
   end do

   if (.not. found) then
!        First Time File is Identified - OPEN File
      open(ielunt(indsrc),err=99,file=evlfil(indsrc),&
      &form='FORMATTED',iostat=ioerrn,status='REPLACE')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("EVLFL",I3.3)') ielunt(indsrc)
   call errhdl(path,modnam,'E','500',dummy)

999 continue

   return
end subroutine oueval

subroutine ousumm
!***********************************************************************
!                 OUSUMM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process SUMMFILE Output Selections
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    October 19, 2009
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

!     Variable Initializations
   modnam = 'OUSUMM'

!     Set Logical Switch Indicating That SUMMFILE is Generated
   summfile = .true.

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 3) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      sumfil = runst1(locb(3):loce(3))
!        Output SUMMFILE name is too long
   else
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
      go to 999
   end if

!     Open SUMMFILE
   open(isumunt,err=99,file=sumfil,form='FORMATTED',&
   &iostat=ioerrn,status='REPLACE')

   go to 999

!     WRITE Error Message for Error Opening File
99 call errhdl(path,modnam,'E','500','SUMMFILE')

999 return
end subroutine ousumm

subroutine fileform
!***********************************************************************
!                 FILEFORM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process MXFORM Output Selections
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    October 19, 2009
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

!     Variable Initializations
   modnam = 'FILEFORM'

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 3) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   if (field(3)(1:3) == 'FIX') then
!        Output file formats will use FIXed format
      file_format = 'FIX'
   else if (field(3)(1:3) == 'EXP') then
!        Output file formats will use EXPonential format
      file_format = 'EXP'

      if (numtyp == 1) then
! ---       Mofify affected formats to use EXPonential format;
!           NOTE: Modifications to PLTFRM, PSTFRM and MXDFRM for multiple
!           output types (CONC, DEPOS, etc.) are made in SUBROUTINE MOPOPS.
!           SEASONHR format is assigned in subroutine SHOUT.
         thrfrm =&
         &'(1X,I3,1X,A8,1X,I8.8,2(1X,F13.5),3(1X,F7.2),1X,E13.6)'
         pstfrm =&
         &'(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)'
         if (pm25ave .or. no2ave .or. so2ave) then
            pltfrm =&
            &'(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,&
            &10(E13.6,2X,I8.8,2X:))'
         else
            pltfrm =&
            &'(2(1X,F13.5),1X,E13.6,3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,A8,2X,I8)'
         end if
         rnkfrm =&
         &'(1X,I6,1X,E13.6,1X,I8.8,2(1X,F13.5),3(1X,F7.2),2X,A8)'
         mxdfrm =&
         &'(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,2X,&
         &I8.8,2X,A8)'
      end if

   else
!        Write Error Message:  Invalid format specified
      call errhdl(path,modnam,'E','203',' FORMAT ')
   end if

999 return
end subroutine fileform

subroutine oumaxdly
!***********************************************************************
!                OUMAXDLY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Maximum Daily 1-hour Output Selection
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
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

   integer :: i, j, idat, idat8, istr, istp
   integer :: irsyr, irsdate
   character :: inpgrp*8, hdrfrm*400
   character (len = ilen_fld) :: buffer
   logical :: found, l_exists
! Unused: INTEGER :: IPRDT

!     Variable Initializations
   modnam = 'OUMAXDLY'
   l_exists = .true.
   buffer = ' '
   hdrfrm = ' '
   idat   = 0
   idat8  = 0

!     Set Logical Switch Indicating That MAXDAILY File(s) Are Generated
   mxdaily = .true.

!     Create Header Format for Columns
   write(hdrfrm,9020) numtyp, numtyp+2

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(3)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Set Switch and Check for Previous MAXDAILY Card
!     for This Group ID
   imxdly(indgrp) = imxdly(indgrp) + 1
   if (imxdly(indgrp) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

   if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      maxdly(indgrp) = runst1(locb(4):loce(4))
   else
!        WRITE Error Message:  MAXDLY Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 5) then
      call stonum(field(5),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         imdunt(indgrp) = nint(fnum)
      else
         imdunt(indgrp) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (500's)
      imdunt(indgrp) = 501 + (indgrp-1)*2
      if (indgrp >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numgrp
      if (i /= indgrp) then
         if (maxdly(indgrp) == maxdly(i) .and.&
         &imdunt(indgrp) == imdunt(i)) then
            found = .true.
         else if (maxdly(indgrp) == maxdly(i) .and.&
         &imdunt(indgrp) /= imdunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (maxdly(indgrp) /= maxdly(i) .and.&
         &imdunt(indgrp) == imdunt(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

   if (.not. found) then
!        First Time File is Identified; check for re-start option (RSTINP)
!        before OPENing File
      if (rstinp) then
!           Results Arrays Are To Be Initialized From Re-start File.
!           Check for existence of file first, then
!           Read Start Date From File and Rewind.
         inquire (file=maxdly(indgrp),exist=l_exists)
         dummy = 'MAXDAILY'
         if (l_exists) then
            open(imdunt(indgrp),err=99, file=maxdly(indgrp),&
            &iostat=ioerrn,form='FORMATTED',status='OLD')
         else
!              File does not exist; restart option will not work
            call errhdl(path,modnam,'E','585',dummy)
            return
         end if
!           Results Arrays Are To Be Initialized From Re-start File.
!           Read Start Date From File, IRSDATE, and Rewind.
         dummy = 'INITFILE'
         read(irsunt,err=919,end=919) irsdate
         rewind irsunt
!           Now Position MAXDAILY To End of File, But Not Past IRSDATE.
         dummy = 'MAXDAILY'
         eof = .false.

         do while (.not. eof)
            read(imdunt(indgrp),'(A:)', err=919,end=299) buffer
            if (buffer(1:1) /= '*') then
!                 Record Is Not Part of Header - Read Date For This Record
!                 First calculate start & end of date string based on NUMTYP
               istr = 101
               istp = istr + 7
               read(buffer(istr:istp),'(I8)',err=919) idat8
!                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
               idat8 = 100*(idat8/100)
               irsyr = idat8/1000000

!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
               call long_date(idat8,idat,irsyr,irsyr) !Determine the longform date
! ---        D001 remove original calculation of year Wood 9/15/22
!                  IF (IRSYR .GE. ISTRT_WIND .and. IRSYR .LE. 99) THEN
!                     IRSYR = ISTRT_CENT*100 + IRSYR
!                     IDAT  = ISTRT_CENT*100000000 + IDAT8
!                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
!                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
!                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
!                  END IF
            else
!                 Header record - cycle to next record
               cycle
            end if
            if (idat > irsdate) then
!                 Date of MAXDAILY Record Is Greater Than Start Date
!                 From Save File.  Treat As End of File to Exit Loop.
               if (multyr) then
! ---                If this is a MULTYEAR run, then MAXDAILY date should not
!                    be greater than IRSDATE; issue error message
                  write(dummy,'(I8.8)') idat8
                  call errhdl(path,modnam,'E','593',dummy)
               end if
               go to 299
            end if
            go to 21
299         eof = .true.
21          continue
         end do
         eof = .false.
!           End of file or IRSDATE has been passed; backspace file
         backspace imdunt(indgrp)
!           Skip Header Records
         go to 999
      else
! ---       This is not a restarted run; just open the file
         open(imdunt(indgrp),err=99, file=maxdly(indgrp),&
         &iostat=ioerrn,form='FORMATTED',status='REPLACE')
      end if
   else if (found .and. rstinp) then
!        This file is already open, and this run is a
!        Re-start from an earlier run
!        Skip Header Records
      go to 999
   end if

!     Write Header to File.  Skip Header if FATAL.
   if (run .and. .not.fatal .and. .not.L_NoHeader(6)) then
      write(imdunt(indgrp),9005) versn, title1(1:68), rundat
9005  format('* AERMOD (',a6,'): ',a68,5x,a8)
      write(imdunt(indgrp),9007) c_metver, runtim,&
      &MODOPS_String(1:len_trim(MODOPS_String))
9007  format('* AERMET (',a6,'):',t93,a8,&
      &/'* MODELING OPTIONS USED: ',a:)
      write(imdunt(indgrp),9010) grpid(indgrp), numrec,&
      &mxdfrm(1:len_trim(mxdfrm))
9010  format('*',9x,'MAXDAILY FILE OF DAILY MAXIMUM 1-HR VALUES',&
      &' BY DAY FOR SOURCE GROUP: ',a8,&
      &/'*',9x,'FOR A TOTAL OF ',i5,' RECEPTORS.',&
      &/'*',9x,'FORMAT: ',a:)
      write(imdunt(indgrp),hdrfrm) (chidep(1,ityp),&
      &chidep(2,ityp), chidep(3,ityp), ityp=1,numtyp)
9020  format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
      &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',5X,''JDAY'',&
      &  3X,''HR'',4X,''DATE'',5X,''NET ID'',&
      &  /,''*'',',i1,'(1X,''____________ ''),1X,&
      & 3('' ______  ''),''______  ________  ____  ___  ________'',&
      &  ''  ________'')')

   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("MXDLY",I3.3)') imdunt(indgrp)
   call errhdl(path,modnam,'E','500',dummy)

   go to 999

!     WRITE Error Message for Error Reading File
919 call errhdl(path,modnam,'E','510',dummy)

999 return
end subroutine oumaxdly

subroutine oumxdly_byyr
!***********************************************************************
!                OUMAXDLY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Maximum Daily 1-hour By Year Output Selection
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
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

   integer :: i, j, idat, idat8, istr, istp
   integer :: irsyr, irsdate
   character :: inpgrp*8, hdrfrm*400
   character (len = ilen_fld) :: buffer
   logical :: found, l_exists
! Unused: INTEGER :: IPRDT

!     Variable Initializations
   modnam = 'OUMXDLY_BYYR'
   l_exists = .true.
   buffer = ' '
   hdrfrm = ' '
   idat   = 0
   idat8  = 0

!     Set Logical Switch Indicating That MXDYBYYR File(s) Are Generated
   mxdaily_byyr = .true.

!     Create Header Format for Columns
   write(hdrfrm,9020) numtyp, numtyp+2

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 4) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 5) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(3)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   end if

!     Set Switch and Check for Previous MXDYBYYR Card
!     for This Group ID
   imxdly_byyr(indgrp) = imxdly_byyr(indgrp) + 1
   if (imxdly_byyr(indgrp) > 1) then
!        WRITE Error Message
      call errhdl(path,modnam,'E','211',keywrd)
      go to 999
   end if

   if ((loce(4)-locb(4)) <= (ilen_fld - 1) ) then
!        Retrieve Filename as Character Substring to Maintain Original Case
!        Also Check for Filename Larger Than ILEN_FLD Characters
      maxdly_byyr(indgrp) = runst1(locb(4):loce(4))
   else
!        WRITE Error Message:  MAXDLY_BYYR Field is Too Long
      write(dummy,'(I8)') ilen_fld
      call errhdl(path,modnam,'E','291',dummy)
   end if

!     Retrieve File Unit If Input, or Assign File Unit and OPEN File
   if (ifc == 5) then
      call stonum(field(5),ilen_fld,fnum,imit)
!        Check for Valid File Unit Value
      if (imit /= 1) then
!           Write Error Message: Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
!        Check for Conflict With System Files
      if (nint(fnum) <= 30) then
!           WRITE Error Message:  Invalid File Unit Specified
         call errhdl(path,modnam,'E','560',keywrd)
         go to 999
      else if (nint(fnum) > 100) then
!           WRITE Warning Message:  Suspect File Unit Specified
!           Unit May Conflict With Dynamically Allocated File Units
         call errhdl(path,modnam,'W','565',keywrd)
         imdunt_byyr(indgrp) = nint(fnum)
      else
         imdunt_byyr(indgrp) = nint(fnum)
      end if
   else
!        Dynamically Allocate File Unit (600's)
      imdunt_byyr(indgrp) = 601 + (indgrp-1)*2
      if (indgrp >= 10) then
!           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
         call errhdl(path,modnam,'W','565',keywrd)
      end if
   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numgrp
      if (i /= indgrp) then
         if (maxdly_byyr(indgrp) == maxdly_byyr(i) .and.&
         &imdunt_byyr(indgrp) == imdunt_byyr(i)) then
            found = .true.
         else if (maxdly_byyr(indgrp) == maxdly_byyr(i) .and.&
         &imdunt_byyr(indgrp) /= imdunt_byyr(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         else if (maxdly_byyr(indgrp) /= maxdly_byyr(i) .and.&
         &imdunt_byyr(indgrp) == imdunt_byyr(i)) then
!             Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

   if (.not. found) then
!        First Time File is Identified; check for re-start option (RSTINP)
!        before OPENing File
      if (rstinp) then
!           Results Arrays Are To Be Initialized From Re-start File.
!           Check for existence of file first, then
!           Read Start Date From File and Rewind.
         inquire (file=maxdly_byyr(indgrp),exist=l_exists)
         dummy = 'MXDYBYYR'
         if (l_exists) then
            open(imdunt_byyr(indgrp),err=99,&
            &file=maxdly_byyr(indgrp),&
            &iostat=ioerrn,form='FORMATTED',status='OLD')
         else
!              File does not exist; restart option will not work
            call errhdl(path,modnam,'E','585',dummy)
            return
         end if
!           Results Arrays Are To Be Initialized From Re-start File.
!           Read Start Date From File, IRSDATE, and Rewind.
         dummy = 'INITFILE'
         read(irsunt,err=919,end=919) irsdate
         rewind irsunt
!           Now Position MXDYBYYR To End of File, But Not Past IRSDATE.
         dummy = 'MXDYBYYR'
         eof = .false.

         do while (.not. eof)
            read(imdunt_byyr(indgrp),'(A:)', err=919,end=299) buffer
            if (buffer(1:1) /= '*') then
!                 Record Is Not Part of Header - Read Date For This Record
!                 First calculate start & end of date string based on NUMTYP
               istr = 101
               istp = istr + 7
               read(buffer(istr:istp),'(I8)',err=919) idat8
!                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
               irsyr = idat8/1000000
!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
               call long_date(idat8,idat,irsyr,irsyr) !Determine the longform date
! ---  D001 remove original calculation of year Wood 9/15/22
!                  IF (IRSYR .GE. ISTRT_WIND .and. IRSYR .LE. 99) THEN
!                     IRSYR = ISTRT_CENT*100 + IRSYR
!                     IDAT  = ISTRT_CENT*100000000 + IDAT8
!                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
!                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
!                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
!                  END IF
            else
!                 Header record - cycle to next record
               cycle
            end if
            if (idat > irsdate) then
!                 Date of MXDYBYYR Record Is Greater Than Start Date
!                 From Save File.  Treat As End of File to Exit Loop.
               if (multyr) then
! ---                If this is a MULTYEAR run, then MXDYBYYR date should not
!                    be greater than IRSDATE; issue error message
                  write(dummy,'(I8.8)') idat8
                  call errhdl(path,modnam,'E','593',dummy)
               end if
               go to 299
            end if
            go to 21
299         eof = .true.
21          continue
         end do
         eof = .false.
!           End of file or IRSDATE has been passed; backspace file
         backspace imdunt_byyr(indgrp)
!           Skip Header Records
         go to 999
      else
! ---       This is not a restarted run; just open the file
         open(imdunt_byyr(indgrp),err=99, file=maxdly_byyr(indgrp),&
         &iostat=ioerrn,form='FORMATTED',status='REPLACE')
      end if
   else if (found .and. rstinp) then
!        This file is already open, and this run is a
!        Re-start from an earlier run
!        Skip Header Records
      go to 999
   end if

!     Write Header to File.  Skip Header if FATAL.
   if (run .and. .not.fatal .and. .not.L_NoHeader(7)) then
      write(imdunt_byyr(indgrp),9005) versn,title1(1:68), rundat
9005  format('* AERMOD (',a6,'): ',a68,5x,a8)
      write(imdunt_byyr(indgrp),9007) c_metver, runtim,&
      &MODOPS_String(1:len_trim(MODOPS_String))
9007  format('* AERMET (',a6,'):',t93,a8,&
      &/'* MODELING OPTIONS USED: ',a:)
      write(imdunt_byyr(indgrp),9010) grpid(indgrp), numrec,&
      &mxdfrm(1:len_trim(mxdfrm))
9010  format('*',9x,'MXDYBYYR FILE OF RANKED DAILY MAXIMUM 1-HR ',&
      &' VALUES BY YEAR FOR SOURCE GROUP: ',a8,&
      &/'*',9x,'FOR A TOTAL OF ',i5,' RECEPTORS.',&
      &/'*',9x,'FORMAT: ',a:)
      write(imdunt_byyr(indgrp),hdrfrm) (chidep(1,ityp),&
      &chidep(2,ityp), chidep(3,ityp), ityp=1,numtyp)
9020  format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
      &  4X,''ZHILL'',4X,''ZFLAG'',4X,''RANK'',4X,''GRP'',5X,''JDAY'',&
      &  3X,''HR'',4X,''DATE'',5X,''NET ID'',&
      &  /,''*'',',i1,'(1X,''____________ ''),1X,&
      & 3('' ______  ''),''______  ________  ____  ___  ________'',&
      &  ''  ________'')')

   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("MXDLY",I3.3)') imdunt_byyr(indgrp)
   call errhdl(path,modnam,'E','500',dummy)

   go to 999

!     WRITE Error Message for Error Reading File
919 call errhdl(path,modnam,'E','510',dummy)

999 return
end subroutine oumxdly_byyr

subroutine oumaxd_cont
!***********************************************************************
!                OUMAXD_CONT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process Option for Source Group Contributions to
!                 Ranked Maximum Daily 1-hour Results
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED:   Modified to include checks on the use of a limited
!                    range of ranks (specified on the OU RECTABLE keyword)
!                    with the THRESH option on the OU MAXDCONT keyword.
!                    A fatal error message is generated if the range of ranks
!                    specified is less than or equal to the design value rank for
!                    the specified pollutant plus 4, i.e., a fatal error will be
!                    generated if the range of ranks is less than or equal to 8
!                    for 1-hr SO2, or less than or equal to 12 for 1-hr NO2 or
!                    24-hr PM2.5. A non-fatal warning message is also generated
!                    if the range of ranks is less than or equal to the design
!                    value rank plus 20, i.e., if the range of ranks is less than
!                    or equal to 24 for 1-hr SO2, or less than or equal to 28 for
!                    1-hr NO2 or 24-hr PM2.5.
!                    R. Brode, US EPA, OAQPS, AQMG, 02/29/2012
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

   integer :: i, j
   character :: inpgrp*8
!     JAT D065, 8/9/21 BUFFER SET BUT NOT USED
!      CHARACTER (LEN = ILEN_FLD) :: BUFFER
   logical :: found

!     Variable Initializations
   modnam = 'OUMAXD_CONT'

! --- Set logical flag indicated that source group contributions to
!     maximum daily 1-hour values will be performed (as internal
!     post-processing)
   l_maxdcont = .true.

! --- Check for use of re-start option (CO INITFILE/SAVEFILE keywords),
!     or MULTYEAR option, which are not compatible with MAXDCONT option
   if (multyr) then
!        Error Message: Incompatible options
      call errhdl(path,modnam,'E','153','MULTYEAR')
      go to 999
   else if (rstsav) then
!        Error Message: Incompatible options
      call errhdl(path,modnam,'E','153','SAVEFILE')
      go to 999
   else if (rstinp) then
!        Error Message: Incompatible options
      call errhdl(path,modnam,'E','153','INITFILE')
      go to 999
   end if
!     JAT D065, 8/9/21 BUFFER SET BUT NOT USED
!      BUFFER = ' '

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 6) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 8) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Retrieve Source Group ID
   inpgrp = field(3)
!     Check Source Group ID
   found = .false.
   indgrp = 0
   j = 1
   do while (.not.found .and. j<=numgrp)
      if (inpgrp == grpid(j)) then
         found = .true.
         indgrp = j
      end if
      j = j + 1
   end do
   if (.not. found) then
!        Error Message: E203 GRPID Not Match With Pre-Defined One
      call errhdl(path,modnam,'E','203','GRPID')
      go to 999
   else if (maxdcont(indgrp) == 1) then
! ---    MAXDCONT file already specified for this source group
      call errhdl(path,modnam,'E','161',grpid(indgrp))
      go to 999
   else
! ---    Set flag for MAXDCONT option for this source group
      maxdcont(indgrp) = 1
   end if

!     Retrieve "upper" bound for rank
   call stonum(field(4),ilen_fld,fnum,imit)
!     Check for Valid Rank Value
   if (imit /= 1) then
!        Write Error Message:Invalid Numerical Field
      call errhdl(path,modnam,'E','208',keywrd)
      go to 999
   end if
   mxd_rank(indgrp,1) = nint(fnum)
   if (mxd_rank(indgrp,1) > nhival) then
!        Write Error Message:Rank exceeds # of high values;
!        this shouldn't occur since limits are dynamically allocated
      write(dummy,'(''NVAL='',I7)') nhival
      call errhdl(path,modnam,'E','290',dummy)
      go to 999
   end if

! --- Check for whether user specified a threshold for the
!     MAXDCONT file, rather than upper bound on rank

   if (field(5) == 'THRESH') then
! ---    Retrieve lower threshold value for MAXDCONT results

      call stodbl(field(6),ilen_fld,dnum,imit)
!        Check for Valid Threshold Value
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      maxd_thresh(indgrp) = dnum

! ---    Assign maximum value to "lower" bound of ranks
      mxd_rank(indgrp,2) = nhival

      if ((loce(7)-locb(7)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         maxdcont_file(indgrp) = runst1(locb(7):loce(7))
      else
!           WRITE Error Message:  MAXDCONT_FILE Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
      end if

!        Retrieve File Unit If Input, or Assign File Unit and OPEN File
      if (ifc == 8) then
         call stonum(field(8),ilen_fld,fnum,imit)
!           Check for Valid File Unit Value
         if (imit /= 1) then
!              Write Error Message: Invalid Numerical Field
            call errhdl(path,modnam,'E','208',keywrd)
            go to 999
         end if
!           Check for Conflict With System Files
         if (nint(fnum) <= 30) then
!              WRITE Error Message:  Invalid File Unit Specified
            call errhdl(path,modnam,'E','560',keywrd)
            go to 999
         else if (nint(fnum) > 100) then
!              WRITE Warning Message:  Suspect File Unit Specified
!              Unit May Conflict With Dynamically Allocated File Units
            call errhdl(path,modnam,'W','565',keywrd)
            imxdcunt(indgrp) = nint(fnum)
         else
            imxdcunt(indgrp) = nint(fnum)
         end if
      else
!           Dynamically Allocate File Unit (700's)
         imxdcunt(indgrp) = 701 + (indgrp)*2
         if (indgrp >= 10) then
!              WRITE Warning Message: Dynamic Unit Allocation May Have Conflict
            call errhdl(path,modnam,'W','565',keywrd)
         end if
      end if

! ---    Check for use of MAXDCONT THRESH option with limited range of
!        ranks on RECTABLE keyword
      if ( (so2ave .and. nhival<= 8) .or.&
      &(no2ave .and. nhival<=12) .or.&
      &(pm25ave .and. nhival<=12) ) then
! ---       NHIVAL is less than or equal to the design value rank + 4
!           for the THRESH option; issue fatal ERROR message
         write(dummy,'(''Max Rank ='',I2)') nhival
         call errhdl(path,modnam,'E','273',dummy)
      else if ( (so2ave .and. nhival<=24) .or.&
      &(no2ave .and. nhival<=28) .or.&
      &(pm25ave .and. nhival<=28) ) then
! ---       NHIVAL is less than or equal to the design value rank + 20
!           for the THRESH option; issue non-fatal WARNING message
         write(dummy,'(''Max Rank ='',I2)') nhival
         call errhdl(path,modnam,'W','273',dummy)
      end if

   else
! ---    User specified an "lower" bound for rank, rather than threshold

!        Retrieve "lower"  bound for rank
      call stonum(field(5),ilen_fld,fnum,imit)
!        Check for Valid Rank Value
      if (imit /= 1) then
!           Write Error Message:Invalid Numerical Field
         call errhdl(path,modnam,'E','208',keywrd)
         go to 999
      end if
      mxd_rank(indgrp,2) = nint(fnum)
      if (mxd_rank(indgrp,2) > nhival) then
!           Write Error Message:Rank exceeds # of high values;
!           this shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NVAL='',I7)') nhival
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      else if (mxd_rank(indgrp,2) < mxd_rank(indgrp,1)) then
!           Write Error Message: "lower" bound rank < "upper" bound
         write(dummy,'(''U='',I3,2X,''L='',I3)') mxd_rank(indgrp,1),&
         &mxd_rank(indgrp,2)
         call errhdl(path,modnam,'E','272',dummy)
         go to 999
! ---    Add warning regarding MXD_RANK > 366
      else if (mxd_rank(indgrp,2) > 366) then
!           Write Warning Message:Rank exceeds max number of days in a year
         write(dummy,'('' NVAL='',I6)') mxd_rank(indgrp,2)
         call errhdl(path,modnam,'W','290',dummy)

      end if

! ---    Assign value of 0.0 to Threshold value
      maxd_thresh(indgrp) = 0.0d0

      if ((loce(6)-locb(6)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         maxdcont_file(indgrp) = runst1(locb(6):loce(6))
      else
!           WRITE Error Message:  MAXDCONT_FILE Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
      end if

!        Retrieve File Unit If Input, or Assign File Unit and OPEN File
      if (ifc == 7) then
         call stonum(field(7),ilen_fld,fnum,imit)
!           Check for Valid File Unit Value
         if (imit /= 1) then
!              Write Error Message: Invalid Numerical Field
            call errhdl(path,modnam,'E','208',keywrd)
            go to 999
         end if
!           Check for Conflict With System Files
         if (nint(fnum) <= 30) then
!              WRITE Error Message:  Invalid File Unit Specified
            call errhdl(path,modnam,'E','560',keywrd)
            go to 999
         else if (nint(fnum) > 100) then
!              WRITE Warning Message:  Suspect File Unit Specified
!              Unit May Conflict With Dynamically Allocated File Units
            call errhdl(path,modnam,'W','565',keywrd)
            imxdcunt(indgrp) = nint(fnum)
         else
            imxdcunt(indgrp) = nint(fnum)
         end if
      else
!           Dynamically Allocate File Unit (700's)
         imxdcunt(indgrp) = 701 + (indgrp)*2
         if (indgrp >= 10) then
!              WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            call errhdl(path,modnam,'W','565',keywrd)
         end if
      end if

   end if

!     Check for Earlier Use of This Filename and File Unit
   found = .false.
   do i = 1, numgrp
      if (i /= indgrp) then
         if (maxdcont_file(indgrp)==maxdcont_file(i) .and.&
         &imxdcunt(indgrp)==imxdcunt(i)) then
            found = .true.
         elseif(maxdcont_file(indgrp)==maxdcont_file(i) .and.&
         &imxdcunt(indgrp)/=imxdcunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         elseif(maxdcont_file(indgrp)/=maxdcont_file(i) .and.&
         &imxdcunt(indgrp)==imxdcunt(i)) then
!              Write Error Message: Conflicting Inputs
            call errhdl(path,modnam,'E','550',keywrd)
            go to 999
         end if
      end if
   end do

   if (.not. found) then
! ---    This is a new file; open the file
      open(imxdcunt(indgrp),err=99,file=maxdcont_file(indgrp),&
      &iostat=ioerrn,form='FORMATTED',status='REPLACE')
   end if

   go to 999

!     WRITE Error Message for Error Opening File
99 write(dummy,'("MXDCN",I3.3)') imxdcunt(indgrp)
   call errhdl(path,modnam,'E','500',dummy)

999 return
end subroutine oumaxd_cont

subroutine noheader
!***********************************************************************
!                 NOHEADER Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Process NOHEADER Output Selections
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
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
   integer :: i
   character :: modnam*12

!     Variable Initializations
   modnam = 'NOHEADER'

!     Check If Enough Fields
   if (ifc == 2) then
!        Error Message: No Fields
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc < 3) then
!        Error Message: Not Enough Fields
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 10) then
!        Error Message: Too Many Fields
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

   do i = 3, ifc
      if (field(i) == 'ALL') then
!           No headers for any ouput file type
         L_NoHeader(:) = .true.
         exit
      else if (field(i) == 'MAXIFILE') then
!           No headers for MAXIFILE
         L_NoHeader(1) = .true.
      else if (field(i) == 'POSTFILE') then
!           No headers for POSTFILE
         L_NoHeader(2) = .true.
      else if (field(i) == 'PLOTFILE') then
!           No headers for PLOTFILE
         L_NoHeader(3) = .true.
      else if (field(i) == 'SEASONHR') then
!           No headers for SEASONHR
         L_NoHeader(4) = .true.
      else if (field(i) == 'RANKFILE') then
!           No headers for RANKFILE
         L_NoHeader(5) = .true.
      else if (field(i) == 'MAXDAILY') then
!           No headers for MAXDAILY
         L_NoHeader(6) = .true.
      else if (field(i) == 'MXDYBYYR') then
!           No headers for MXDYBYYR
         L_NoHeader(7) = .true.
      else if (field(i) == 'MAXDCONT') then
!           No headers for MAXDCONT
         L_NoHeader(8) = .true.
      else
!           Write Error Message:  Invalid output file type
         call errhdl(path,modnam,'E','203',field(i))
      end if
   end do

999 return
end subroutine noheader

