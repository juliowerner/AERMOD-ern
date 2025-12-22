subroutine perave
!***********************************************************************
!                 PERAVE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates PERIOD Averages
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Removed 75 percent limit on calculation of the
!                    denominator, SNUM - 4/19/93
!
!        INPUTS:  Array of Period Sums and Counters
!
!        OUTPUTS: Array of Period Averages
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: snum, snumscm, stothrs

!     Variable Initializations
   modnam = 'PERAVE'

!     Calculate Denominator Considering Calms and Missing
   snum = dble(ianhrs - ianclm - ianmsg)
   if (.not. scim) then
      stothrs = dble(ianhrs)
      snumscm = snum
   else if (scim) then
      stothrs = dble(nskiptot)                       ! Total no. of hours
      snumscm = dble(ianhrs   - ianclm   - ianmsg)   ! Sampled SCIM'd hours
   endif

!     Calculate Period Average Concentrations for Each Source Group and Receptor

!     Begin LOOP Over Output Types
   do ityp = 1, numtyp

      if (outtyp(ityp) == 'CONC') then

         annval(1:numrec,1:numgrp,1) =&
         &annval(1:numrec,1:numgrp,1)/snum

      else if (scim) then

         annval(1:numrec,1:numgrp,ityp) =&
         &annval(1:numrec,1:numgrp,ityp)*(stothrs/snumscm)

      end if

   end do
!     End LOOP Over Output Types

   return
end subroutine perave

subroutine shave
!***********************************************************************
!                 SHAVE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Season/Hour Averages
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    June 5, 1997
!
!        INPUTS:  Array of Season/Hour Sums and Counters
!
!        OUTPUTS: Season/Hour Output Files
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: snum

!     Variable Initializations
   modnam = 'SHAVE'

!     Calculate Period Average Concentrations for Each Source Group and Receptor

   do iseas = 1, 4
      do ihour = 1, 24

!           Calculate Denominator Considering Calms and Missing
         snum = dble(nseahr(iseas,ihour) - nseacm(iseas,ihour))

         shvals(1:numrec,1:numgrp,iseas,ihour,1) =&
         &shvals(1:numrec,1:numgrp,iseas,ihour,1) / snum

      end do
   end do

   return
end subroutine shave

subroutine hiper
!***********************************************************************
!                 HIPER Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Selects Highest PERIOD Average Values
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Moved call to RSDUMP for MULTYR option to MAIN.
!                    R.W. Brode, MACTEC (f/k/a PES), Inc.,  09/06/05
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        INPUTS:  Array of Period Averages
!
!        OUTPUTS: Array of Highest Period Averages By Source Group
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j

!     Variable Initializations
   modnam = 'HIPER'

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec
         if (nhiann > 1) then
            if (annval(irec,igrp,ityp) >&
            &amxval(nhiann,igrp,ityp)) then
               do j = nhiann-1, 1, -1
                  if (annval(irec,igrp,ityp) <=&
                  &amxval(j,igrp,ityp))then
                     amxval(j+1,igrp,ityp) = annval(irec,igrp,ityp)
                     imxloc(j+1,igrp,ityp) = irec
!                       Exit Block
                     cycle receptor_loop
                  else
                     amxval(j+1,igrp,ityp) = amxval(j,igrp,ityp)
                     imxloc(j+1,igrp,ityp) = imxloc(j,igrp,ityp)
                     if (j == 1) then
                        amxval(1,igrp,ityp) = annval(irec,igrp,ityp)
                        imxloc(1,igrp,ityp) = irec
                     end if
                  end if
               end do
            end if
         else if (nhiann == 1) then
            if (annval(irec,igrp,ityp) > amxval(1,igrp,ityp)) then
               amxval(1,igrp,ityp) = annval(irec,igrp,ityp)
               imxloc(1,igrp,ityp) = irec
            end if
         end if
      end do receptor_loop
!        End Receptor LOOP
   end do
!     End Source Group LOOP

   return
end subroutine hiper

subroutine pstann
!***********************************************************************
!                 PSTANN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Postprocessor Files for PERIOD Results
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:
!                    Modified to properly handle PERIOD vs. ANNUAL
!                    POSTFILEs.  PERIOD postfiles include averages
!                    across the full data period; ANNUAL postfiles
!                    include the average ANNUAL values for each year
!                    in the data period.
!
!                    The PERIOD postfiles include the number of
!                    hours in the data period; ANNUAL postfiles
!                    include the number of years in the data period.
!                    R. W. Brode, U.S. EPA, 06/30/2015
!
!                    Increased length of HDRFRM variable to avoid
!                    potential problems with portability of code.
!                    R. Brode, MACTEC/PES, 10/17/2005
!
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
!
!        INPUTS:  Array of High Values
!
!        OUTPUTS: File of High Values for Postprocessing
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

! Unused:      INTEGER :: I
   character :: perchr*6, hdrfrm*400

!     Variable Initializations
   modnam = 'PSTANN'

!     Set Averaging Label and Create Header Format for Columns
   if (period) then
      perchr = 'PERIOD'
      write(hdrfrm,9020) numtyp, numtyp+2
   else if (annual) then
      perchr = 'ANNUAL'
      write(hdrfrm,9021) numtyp, numtyp+2
   end if

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Check for Selection of PERIOD POSTFILE for This Group
      if (ianpst(igrp) == 1) then
         if (ianfrm(igrp) == 0) then
!              WRITE Results to Unformatted POSTFILE
            if (period) then
               write(iapunt(igrp),err=99) kurdat, ianhrs,&
               &grpid(igrp), ((annval(irec,igrp,ityp),irec=1,numrec),&
               &ityp=1,numtyp)
            else if (annual) then
               write(iapunt(igrp),err=99) kurdat, numyrs,&
               &grpid(igrp), ((annval(irec,igrp,ityp),irec=1,numrec),&
               &ityp=1,numtyp)
            end if
         else
!              WRITE Results to Formatted Plot File
!              Write Header Information
            write(iapunt(igrp),9005) versn, title1(1:68), rundat
            write(iapunt(igrp),9007) c_metver, runtim,&
            &MODOPS_String(1:len_trim(MODOPS_String))
            if (period) then
               write(iapunt(igrp),9010) perchr,grpid(igrp),&
               &numrec,pstfrm
               write(iapunt(igrp),hdrfrm) (chidep(1,ityp),&
               &chidep(2,ityp),&
               &chidep(3,ityp),&
               &ityp=1,numtyp)
            else if (annual) then
               write(iapunt(igrp),9011) perchr,numyrs,grpid(igrp),&
               &numrec,pstfrm
               write(iapunt(igrp),hdrfrm) (chidep(1,ityp),&
               &chidep(2,ityp),&
               &chidep(3,ityp),&
               &ityp=1,numtyp)
            end if
!              Begin Receptor LOOP
            do irec = 1, numrec
               if (period) then
                  write(iapunt(igrp),pstfrm,err=99)&
                  &axr(irec), ayr(irec), (annval(irec,igrp,ityp),&
                  &ityp=1,numtyp),&
                  &azelev(irec), azhill(irec), azflag(irec),&
                  &perchr, grpid(igrp), ianhrs, netid(irec)
               else if (annual) then
                  write(iapunt(igrp),pstfrm,err=99)&
                  &axr(irec), ayr(irec), (annval(irec,igrp,ityp),&
                  &ityp=1,numtyp),&
                  &azelev(irec), azhill(irec), azflag(irec),&
                  &perchr, grpid(igrp), numyrs, netid(irec)
               end if
            end do
!              End Receptor LOOP
         end if
      end if
   end do
!     End Source Group LOOP

   go to 999

!     WRITE Error Message for Problem Writing to Postprocessor File
99 write(dummy,'("PSTFL",I3.3)') iapunt(igrp)
   call errhdl(path,modnam,'E','520',dummy)

9005 format('* AERMOD (',a6,'): ',a68,5x,a8)
9007 format('* AERMET (',a6,'):',t93,a8,&
   &/'* MODELING OPTIONS USED: ',a:)
9010 format('*',9x,'POST/PLOT FILE OF ',a6,' VALUES FOR ',&
   &'SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ', i5,' RECEPTORS.',&
   &/'*',9x,'FORMAT: ',a:)
9011 format('*',9x,'POST/PLOT FILE OF ',a6,' VALUES FOR YEAR ',&
   &i3,' FOR SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ', i5,' RECEPTORS.',&
   &/'*',9x,'FORMAT: ',a:)
9020 format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM HRS'',&
   &  3X,''NET ID'',/,''*'',',i1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')
! --- Modified to show the Year Number for ANNUAL averages, instead of
!     number of years, in v15181
9021 format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',5X,''YEAR NUM'',&
   &  3X,''NET ID'',/,''*'',',i1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')

999 return
end subroutine pstann

subroutine pltann
!***********************************************************************
!                 PLTANN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Files To Plot Annual (i.e. PERIOD) Results
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:
!                    Modified to properly handle PERIOD vs. ANNUAL
!                    PLOTFILEs.  PERIOD plotfiles include averages
!                    across the full data period; ANNUAL plotfiles
!                    include the multi-year average ANNUAL values.
!                    The PERIOD plotfiles include the number of
!                    hours in the data period; ANNUAL plotfiles
!                    include the number of years in the data period.
!                    R. W. Brode, U.S. EPA, 06/30/2015
!
!                    Increased length of HDRFRM variable to avoid
!                    potential problems with portability of code.
!                    R. Brode, MACTEC/PES, 10/17/2005
!
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
!
!        INPUTS:  Array of High Values
!
!        OUTPUTS: File of High Values for Plotting
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

! Unused:      INTEGER :: I
   character :: perchr*6, hdrfrm*400

!     Variable Initializations
   modnam = 'PLTANN'

!     Set Averaging Label and Create Header Format for Columns
   if (period) then
      perchr = 'PERIOD'
      write(hdrfrm,9020) numtyp, numtyp+2
   else if (annual) then
      perchr = 'ANNUAL'
      write(hdrfrm,9021) numtyp, numtyp+2
   end if

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Check for Selection of PERIOD PLOTFILE for This Group
      if (ianplt(igrp) == 1) then
!           Write Header Information
         write(ippunt(igrp),9005) versn, title1(1:68), rundat
         write(ippunt(igrp),9007) c_metver, runtim,&
         &MODOPS_String(1:len_trim(MODOPS_String))
         write(ippunt(igrp),9011) perchr,numyrs,grpid(igrp),&
         &numrec,pstfrm
         write(ippunt(igrp),hdrfrm) (chidep(1,ityp),chidep(2,ityp),&
         &chidep(3,ityp),ityp=1,numtyp)
!           Begin Receptor LOOP
         do irec = 1, numrec
            if (period) then
               write(ippunt(igrp),pstfrm,err=99)&
               &axr(irec), ayr(irec), (annval(irec,igrp,ityp),&
               &ityp=1,numtyp), azelev(irec), azhill(irec),&
               &azflag(irec), perchr,grpid(igrp), ianhrs, netid(irec)
            else if (annual) then
               write(ippunt(igrp),pstfrm,err=99)&
               &axr(irec), ayr(irec), (annval(irec,igrp,ityp),&
               &ityp=1,numtyp), azelev(irec), azhill(irec),&
               &azflag(irec), perchr,grpid(igrp), numyrs, netid(irec)
            end if
         end do
!           End Receptor LOOP
      end if
   end do
!     End Source Group LOOP

   go to 999

!     WRITE Error Message for Problem Writing to Plot File
99 write(dummy,'("PLTFL",I3.3)') ippunt(igrp)
   call errhdl(path,modnam,'E','520',dummy)

9005 format('* AERMOD (',a6,'): ',a68,5x,a8)
9007 format('* AERMET (',a6,'):',t93,a8,&
   &/'* MODELING OPTIONS USED: ',a:)
! Unused: 9010 FORMAT('*',9X,'PLOT FILE OF ',A6,' VALUES FOR ',
!     &       'SOURCE GROUP: ',A8,
!     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
!     &      /'*',9X,'FORMAT: ',A:)
9011 format('*',9x,'PLOT FILE OF ',a6,' VALUES AVERAGED ACROSS ',&
   &i3,' YEARS FOR SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ', i5,' RECEPTORS.',&
   &/'*',9x,'FORMAT: ',a:)
9020 format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM HRS'',&
   &  3X,''NET ID'',/,''*'',',i1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')
9021 format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM YRS'',&
   &  3X,''NET ID'',/,''*'',',i1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')

999 return
end subroutine pltann

subroutine plotfl
!***********************************************************************
!                 PLOTFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Files To Plot
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Increased length of HDRFRM variable to avoid
!                    potential problems with portability of code.
!                    R. Brode, MACTEC/PES, 10/17/2005
!
!        MODIFIED:   Corrected to output SUMH4H array for post-1997
!                    PM10 processing.
!                    R.W. Brode, PES, Inc.,  12/2/98
!
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
!
!        INPUTS:  Array of High Values
!
!        OUTPUTS: File of High Values for Plotting
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, ival, idec, imod
   character :: nchr1(10)*3, nchr2(10)*5, chrval*5, hdrfrm*400

!     Variable Initializations
   data (nchr1(i),i=1,10) /'YR1','YR2','YR3','YR4','YR5',&
   &'YR6','YR7','YR8','YR9','Y10'/
   data (nchr2(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'PLOTFL'

!     Create Header Format for Columns
   if (pm25ave .or. no2ave .or. so2ave) then
      write(hdrfrm,9019) numyrs, numyrs
   else
      write(hdrfrm,9020) numtyp, chidep(3,1), numtyp+2
   end if

!     Begin Averaging Period LOOP
   do iave = 1, numave
!        Begin Source Group LOOP
      do igrp = 1, numgrp
!           Begin High Value LOOP
         do ival = 1, nhival
!              Decide if we should go through the processing
            if (ipltfl(ival,igrp,iave) == 1) then

! ---             Assign character label for rank
               if (ival <= 10) then
                  chrval = nchr2(ival)
               else if (mod(ival,100) > 10 .and.&
               &mod(ival,100) < 20) then
                  idec = int(ival/10)
                  imod = mod(ival,10)
                  write(chrval,'(I2,I1,"TH")') idec, imod
               else if (ival <= 999) then
                  idec = int(ival/10)
                  imod = mod(ival,10)
                  if (imod == 0) imod = 10
                  write(chrval,'(I2,A3)') idec, nchr2(imod)(3:5)
               end if

               if (.not. L_NoHeader(3)) then
!                 Write Header Information
                  write(iplunt(ival,igrp,iave),9005) versn,&
                  &title1(1:68),rundat
                  write(iplunt(ival,igrp,iave),9007) c_metver,&
                  &title2(1:68),runtim,&
                  &MODOPS_String(1:len_trim(MODOPS_String))
                  if (pm25ave .or. no2ave .or. so2ave) then
                     write(iplunt(ival,igrp,iave),9009) chrval,&
                     &chrave(iave), numyrs, grpid(igrp), numrec,&
                     &pltfrm
                  else
                     write(iplunt(ival,igrp,iave),9010) chrval,&
                     &chrave(iave), grpid(igrp), numrec, pltfrm
                  end if
                  if (pm25ave .or. no2ave .or. so2ave) then
                     write(iplunt(ival,igrp,iave),hdrfrm) chidep(1,1),&
                     &chidep(2,1),chidep(3,1),&
                     &(nchr1(i),nchr1(i),i=1,numyrs)
                  else
                     write(iplunt(ival,igrp,iave),hdrfrm)&
                     &(chidep(1,ityp),chidep(2,ityp),chidep(3,ityp),&
                     &ityp=1,numtyp)
                  end if
               end if

!                 Begin Receptor LOOP
               do irec = 1, numrec
                  if (pm25ave .or. no2ave .or. so2ave) then
                     write(iplunt(ival,igrp,iave),pltfrm,err=99)&
                     &axr(irec), ayr(irec), sumhnh(irec,igrp,ival),&
                     &azelev(irec),azhill(irec),azflag(irec),&
                     &chrave(iave),grpid(igrp),chrval,netid(irec),&
                     &(himxdly_byyr(irec,igrp,ival,j),&
                     &nhidatmxd_byyr(irec,igrp,ival,j),j=1,numyrs)
                  else
                     write(iplunt(ival,igrp,iave),pltfrm,err=99)&
                     &axr(irec), ayr(irec), (hivalu(irec,ival,igrp,&
                     &iave,ityp),ityp=1,numtyp),&
                     &azelev(irec),azhill(irec),azflag(irec),&
                     &chrave(iave),grpid(igrp),chrval,&
                     &netid(irec), nhidat(irec,ival,igrp,iave,1)
                  end if
               end do
!                 End Receptor LOOP
            end if
         end do
!           End High Value LOOP
      end do
!        End Source Group LOOP
   end do
!     End Averaging Period LOOP

   go to 999

!     WRITE Error Message for Problem Writing to Plot File
99 write(dummy,'("PLTFL",I3.3)') iplunt(ival,igrp,iave)
   call errhdl(path,modnam,'E','520',dummy)

9005 format('* AERMOD (',a6,'): ',a68,5x,a8)
9007 format('* AERMET (',a6,'): ',a68,t93,a8,&
   &/'* MODELING OPTIONS USED: ',a:)
! Unused:  9008 FORMAT('*',9X,'PLOT FILE OF ',A5,'-HIGHEST ',A5,
!     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
!     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
!     &      /'*',9X,'FORMAT: ',A:)
9009 format('*',9x,'PLOT FILE OF ',a5,'-HIGHEST MAX DAILY ',a5,&
   &' VALUES AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ',i5,' RECEPTORS.',&
   &/'*',9x,'FORMAT: ',a:)
9010 format('*',9x,'PLOT FILE OF  HIGH ',a5,' HIGH ',a5,&
   &' VALUES FOR SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ',i5,' RECEPTORS.',&
   &/'*',9x,'FORMAT: ',a:)
9019 format('(''*'',8X,''X'',13X,''Y'',4X,(2X,3A4),4X,''ZELEV'',4X,&
   &''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,&
   &''NET ID'',1X,',i1,'(2X,''AVER CONC '',A3,''  DATE '',A3),/,''*'',&
   &3(1X,''____________ ''),1X,3('' ______  ''),&
   &''______  ________  ________  ________'',',&
   &i1,'(''  _____________  ________''))')
9020 format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,&
   &  ''NET ID'',3X,''DATE(',a4,')'',/,''*'',',i1,'(1X,&
   & ''____________ ''),1X,3('' ______  ''),&
   & ''______  ________  ________  ________  ________'')')

999 return
end subroutine plotfl

subroutine output
!***********************************************************************
!                 OUTPUT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Output of Printed Model Results
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To write out "EV STARTING" and "EV FINSISHED" to
!                    temporary event file if no RECTABLE card is used.
!                    R. Brode, PES, Inc. - 02/19/99
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'OUTPUT'
   path = 'OU'

   if (period .or. annual) then
      do ityp = 1, numtyp
!           Print Out Summary of Period Averages            ---   CALL PRTANN
         call prtann
      end do
   end if

   if (pm25ave .and. numave==1) then
!        Print Out Table of Average High-N-High Values for PM2.5
      do ityp = 1, numtyp
         call prtpm25
      end do
      call maxpm25
   else if (no2ave .and. numave>=1) then
!        Print Out Table of Average High-N-High Values for NO2
      do ityp = 1, numtyp
         call prtpm25
      end do
      call maxpm25
   else if (so2ave .and. numave>=1) then
!        Print Out Table of Average High-N-High Values for SO2
      do ityp = 1, numtyp
         call prtpm25
      end do
      call maxpm25
   else if (nhival > 0) then
      do ityp = 1, numtyp
!           Print Out Summary of High Values by Receptor    ---   CALL PRTNHI
         call prtnhi
      end do
   else if (events) then
!        Write the 'EV STARTING' and 'EV FINISHED' Cards to the Temp-EVent File
      write(itevut,9000)
      write(itevut,9001)
9000  format('EV STARTING')
9001  format('EV FINISHED')
   end if

   if (nmxval > 0) then
      do ityp = 1, numtyp
!           Print Out Summary of Overall Maximum Values     ---   CALL PRTMAX
         call prtmax
      end do
   end if

   if (period .or. annual .or. nhival > 0) then
      do ityp = 1, numtyp
! ---       Print Out Summary of Results                    ---   CALL PRTSUM
!           Note that summary of short-term results for PM25 24h, NO2/SO2 1h
!           is provided in separate subroutine PRTPM25SUM
         call prtsum(iounit)
         if (summfile) then
            call prtsum(isumunt)
         end if
      end do
   end if

! --- Print out summary of short-term results for PM25 24h, NO2/SO2 1h
   if (pm25ave .and. nhival > 0) then
      do ityp = 1, numtyp
!           Print Out Summary of PM-2.5 Results             ---   CALL PRTPM25SUM
         call prtpm25sum(iounit)
         if (summfile) then
            call prtpm25sum(isumunt)
         end if
      end do
   else if (no2ave .and. nhival > 0) then
      do ityp = 1, numtyp
!           Print Out Summary of NO2 Results                ---   CALL PRTPM25SUM
         call prtpm25sum(iounit)
         if (summfile) then
            call prtpm25sum(isumunt)
         end if
      end do
   else if (so2ave .and. nhival > 0) then
      do ityp = 1, numtyp
!           Print Out Summary of SO2 Results                ---   CALL PRTPM25SUM
         call prtpm25sum(iounit)
         if (summfile) then
            call prtpm25sum(isumunt)
         end if
      end do
   end if

   if (seasonhr) then
      call shout
   end if

   if (rkfile) then
      do ityp = 1, numtyp
!           Write Short Term High Values to Plot File       ---   CALL RANKFL
         call rankfl
      end do
   end if

!     Generate The EVENT Input File                         ---   CALL EVEFIL
   if (events) then
      call evefil
   end if

   return
end subroutine output

subroutine prtann
!***********************************************************************
!                 PRTANN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Annual Average Data
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include EVALCART receptors with DISCCART
!                    receptors for output.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
!
!        MODIFIED:   To remove references to BOUNDARY receptors
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
!
!        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
!                    to better accommodate UTM coordinates - 9/29/92
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   OUTPUT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, ii, nx, ny, indz, indc, indexw
   double precision :: ycoval, xrms, yrms, dist, dir
   character :: perchr*6, buf132*132

!     Variable Initializations
   modnam = 'PRTANN'
   buf132 = ' '
   indz   = 0

   if (period) then
      perchr = 'PERIOD'
   else if (annual) then
      perchr = 'ANNUAL'
   end if

!     Begin Source Group LOOP
   do igrp = 1, numgrp

      if (.not. psdcredit) then
!           Fill Work Array With SRCIDs For This Group
         indgrp = 0
         do isrc = 1, numsrc
            if (igroup(isrc,igrp) == 1) then
               indgrp = indgrp + 1
               workid(indgrp) = srcid(isrc)
            end if
         end do
      else
!           Assign 'N/A' for source IDs for PSDCREDIT option
         indgrp = 1
         workid(indgrp) = 'N/A'
      end if

! ---    Check for BACKGROUND "source" being included
!        in source group
      if (grp_back(igrp)) then
         indgrp = indgrp + 1
         workid(indgrp) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
         indexw = min(29,nsrc+1)
      else
         indexw = min(29,nsrc)
      end if
!        Check for More Than 29 Sources Per Group
      if (indgrp > indexw) then
         workid(indexw) = ' . . . '
         indgrp = indexw
      end if

!        Print Receptor Network Coordinates:
!        Set Number of Columns Per Page, NCPP
      ncpp = 9
!        Set Number of Rows Per Page, NRPP
      nrpp = 40
!        Begin LOOP Through Networks
      do i = 1, innet
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         nppx = 1 + int((numxpt(i)-1)/ncpp)
         nppy = 1 + int((numypt(i)-1)/nrpp)
         do nx = 1, nppx
            do ny = 1, nppy
               call header(iounit)
               if (period) then
                  write(iounit,9032) perchr, ianhrs,&
                  &(chidep(ii,ityp),ii=1,6),grpid(igrp),(workid(k),k = 1,indgrp)
               else if (annual) then
                  write(iounit,9033) perchr,(chidep(ii,ityp),ii=1,6),&
                  &numyrs,grpid(igrp),(workid(k),k = 1,indgrp)
               end if
               write(iounit,9037) ntid(i), nttyp(i)
!                 Print The Values By Source Group
               write(iounit,9011) chidep(3,ityp), pollut,perlbl(ityp)
               if (nx == nppx) then
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  end if
               else
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  end if
               end if
               write(iounit,9010)
               if (ny == nppy) then
                  do k = 1+nrpp*(ny-1), numypt(i)
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(annval(indz+j-1,igrp,ityp),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(annval(indz+j-1,igrp,ityp),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               else
                  do k = 1+nrpp*(ny-1), nrpp*ny
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(annval(indz+j-1,igrp,ityp),j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(annval(indz+j-1,igrp,ityp),j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               end if
            end do
         end do
      end do
!        End LOOP Through Networks

      if (irstat(4)/=0 .or. irstat(8)/=0) then
! ---       Include EVALCART receptors with DISCCART receptors.
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
         indc = 0
         do irec = 1, numrec
            if (rectyp(irec) == 'DC') then
               indc = indc + 1
               if (mod(indc-1,80) == 0) then
                  call header(iounit)
                  if (period) then
                     write(iounit,9032) perchr,ianhrs,(chidep(ii,ityp),&
                     &ii=1,6),grpid(igrp),(workid(k),k = 1,indgrp)
                  else if (annual) then
                     write(iounit,9033) perchr,(chidep(ii,ityp),&
                     &ii=1,6),numyrs,grpid(igrp),(workid(k),k=1,indgrp)
                  end if
                  write(iounit,9043)
                  write(iounit,9011) chidep(3,ityp), pollut,&
                  &perlbl(ityp)
                  write(iounit,9048) chidep(3,ityp), chidep(3,ityp)
               end if
               if (mod(indc,2) /= 0) then
                  write(buf132(1:60),9045) axr(irec), ayr(irec),&
                  &annval(irec,igrp,ityp)
               else
                  write(buf132(61:120),9045) axr(irec), ayr(irec),&
                  &annval(irec,igrp,ityp)
                  write(iounit,9090) buf132
                  write(buf132,9095)
               end if
            end if
         end do
         if (mod(indc,2) /= 0) then
            write(iounit,9090) buf132
            write(buf132,9095)
         end if
      end if

      if (irstat(5) /= 0) then
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
         indc = 0
         do irec = 1, numrec
            if (rectyp(irec) == 'DP') then
               indc = indc + 1
               xrms = axr(irec) - axs(iref(irec))
               yrms = ayr(irec) - ays(iref(irec))
               dist = dsqrt(xrms*xrms + yrms*yrms)
               dir  = datan2(xrms, yrms) * rtodeg
               if (dir <= 0.0d0) dir = dir + 360.0d0
               if (mod(indc-1,80) == 0) then
                  call header(iounit)
                  if (period) then
                     write(iounit,9032) perchr,ianhrs,(chidep(ii,ityp),&
                     &ii=1,6),grpid(igrp),(workid(k),k = 1,indgrp)
                  else if (annual) then
                     write(iounit,9033) perchr,(chidep(ii,ityp),&
                     &ii=1,6),numyrs,grpid(igrp),(workid(k),k=1,indgrp)
                  end if
                  write(iounit,9044)
                  write(iounit,9011) chidep(3,ityp), pollut,&
                  &perlbl(ityp)
                  write(iounit,9049) chidep(3,ityp), chidep(3,ityp)
               end if
               if (mod(indc,2) /= 0) then
                  write(buf132(1:65),9047) srcid(iref(irec)), dist,&
                  &dir, annval(irec,igrp,ityp)
               else
                  write(buf132(66:130),9047) srcid(iref(irec)), dist,&
                  &dir, annval(irec,igrp,ityp)
                  write(iounit,9090) buf132
                  write(buf132,9095)
               end if
            end if
         end do
         if (mod(indc,2) /= 0) then
            write(iounit,9090) buf132
            write(buf132,9095)
         end if
      end if

!     End Source Group Loop
   end do

9011 format(/40x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9010 format(66(' -')/)
9013 format(2x,f10.2,1x,'|',1x,9(f13.5))
9016 format(3x,' Y-COORD  |',48x,'X-COORD (METERS)')
9017 format(3x,' (METERS) |',1x,9(1x,f12.2,:))
9018 format(3x,'DIRECTION |',48x,'DISTANCE (METERS)')
9019 format(3x,'(DEGREES) |',1x,9(1x,f12.2,:))
9032 format(/30x,'*** THE ',a6,' (',i6,' HRS) ',6a4,&
   &'VALUES FOR SOURCE GROUP:',1x,a8,' ***',&
   &/34x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
9033 format(/19x,'*** THE ',a6,1x,6a4,' VALUES AVERAGED OVER ',&
   &i3,' YEARS FOR SOURCE GROUP:',1x,a8,' ***',&
   &/34x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
9037 format(/35x,'*** NETWORK ID: ',a8,' ;  NETWORK TYPE: ',&
   &a8,' ***')
9043 format(/45x,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 format(/47x,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 format(6x,2(f12.2,2x),f13.5)
9047 format(2x,a12,': ',f12.2,2x,f10.2,2x,f13.5)
9048 format(6x,' X-COORD (M)   Y-COORD (M)        ',a4,&
   &22x,' X-COORD (M)   Y-COORD (M)        ',a4,/65(' -'))
9049 format(5x,'ORIGIN',59x,'ORIGIN',&
   &/5x,' SRCID         DIST (M)   DIR (DEG)        ',a4,&
   &18x,' SRCID         DIST (M)   DIR (DEG)        ',a4,&
   &/65(' -'))
9090 format(a132)
9095 format(132(' '))

   return
end subroutine prtann

subroutine prtnhi
!***********************************************************************
!                 PRTNHI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Specified Highest Value
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To store high short term values in global arrays
!                    rather than local arrays for later summary table
!                    output.
!                    R.W. Brode, PES, Inc. - August 15, 1995.
!
!        MODIFIED:   To add one more decimal place to receptor elevations
!                    and flagpole heights for the temporary event file.
!                    R.W. Brode, PES, Inc. - November 15, 1995.
!
!        INPUTS:  Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs for Short Term Values
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: iwhp(nval), ihst, ival, k, it1, kwrt, igrp_tmp
   double precision :: xr2, yr2, ze2, zh2, zf2
   character :: nameev*10

!     Variable Initialization
   modnam = 'PRTNHI'

! --- Initialize IWHP array
   iwhp = 0

!     Write Out the 'EV STARTING' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   if (ityp == 1) then
      write(itevut,9000)
   end if

   do iave = 1, numave
!        Decide if Print The Period
      ihst = 0
      do ival = 1, nval
         if (nhiave(ival,iave) == 1) then
            ihst = ihst + 1
            iwhp(ihst) = ival
         end if
      end do
      if (ihst == 0) then
!           No High Values for This IAVE; Cycle to Next Averaging Period
         cycle
      end if
!        Print The Data
      do ival = 1, nval
         if (nhiave(ival,iave) == 1) then
!              Print Out High Value By Receptor Table       ---   CALL SPRTHT
            call sprtht(ival)
         end if
      end do
!        Print Out The Temporary File
      do igrp = 1, numgrp
!           Print Out the High Values
         do irec = 1, numrec
!               Get The Maximum in Nth Highest
            do k = 1, ihst
               if (hivalu(irec,iwhp(k),igrp,iave,ityp) >&
               &hmax(k,igrp,iave,ityp)) then
                  hmax(k,igrp,iave,ityp)   = hivalu(irec,iwhp(k),igrp,iave,ityp)
                  hmdate(k,igrp,iave,ityp) = nhidat(irec,iwhp(k),igrp,iave,ityp)
                  hmclm(k,igrp,iave,ityp)  = hclmsg(irec,iwhp(k),igrp,iave,ityp)
                  hmloc(k,igrp,iave,ityp)  = irec
               end if
            end do
         end do
!
!           Output The Max-Upto-IHST to the TempEVent File for the
!           First Output Type Only (i.e., ITYP = 1)
         if (ityp == 1) then
            do k = 1, ihst
               it1 = min( 999, iwhp(k) )
               if (hmloc(k,igrp,iave,ityp) == 0) then
                  xr2 = 0.0d0
                  yr2 = 0.0d0
                  ze2 = 0.0d0
                  zh2 = 0.0d0
                  zf2 = 0.0d0
               else
                  xr2 = axr(hmloc(k,igrp,iave,ityp))
                  yr2 = ayr(hmloc(k,igrp,iave,ityp))
                  ze2 = azelev(hmloc(k,igrp,iave,ityp))
                  zh2 = azhill(hmloc(k,igrp,iave,ityp))
                  zf2 = azflag(hmloc(k,igrp,iave,ityp))
               end if

               if (igrp > 999) then
!                    Number of Source Groups Exceeds Limit of EVNAME Field,
!                    Write Warning Message and Reset to 999
                  if (igrp <= 9999) then
                     write(dummy,'(I2.2,''hr'',1X,''IG='',I4)')&
                     &iave, igrp
                  else
                     write(dummy,'(I2.2,''hr'',1X,''IG>9999'')')&
                     &iave
                  end if
                  call errhdl(path,modnam,'W','235',dummy)
                  igrp_tmp = 999
               else
                  igrp_tmp = igrp
               end if

               if (kave(iave) <= 24) then
                  write(nameev,'(A1,I3.3,A1,I2.2,I3.3)')&
                  &'H',it1,'H',kave(iave),igrp_tmp
               else
!                    KAVE > 24 Means MONTH Average; Write Out as 72 (=720/10)
                  kwrt = kave(iave)/10
                  write(nameev,'(A1,I3.3,A1,I2.2,I3.3)')&
                  &'H',it1,'H',kwrt,igrp_tmp
               end if
               write(itevut,9001) nameev, kave(iave),&
               &grpid(igrp), hmdate(k,igrp,iave,ityp),&
               &hmax(k,igrp,iave,ityp)
               write(itevut,9002) nameev, xr2, yr2, ze2, zh2, zf2
            end do
         end if

      end do

   end do

!     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   if (ityp == 1) then
      write(itevut,9009)
   end if

9000 format('EV STARTING')
9001 format(3x,'EVENTPER',1x,a10,1x,i3,2x,a8,3x,i8.8,1x,f17.5)
9002 format(3x,'EVENTLOC',1x,a10,1x,'XR= ',f15.6,' YR= ',f15.6,&
   &3(1x,f10.4))
9009 format('EV FINISHED')

   return
end subroutine prtnhi

subroutine sprtht(ihnum)
!***********************************************************************
!                 SPRTHT Module of AERMOD Model
!
!        PURPOSE: Print Out The Highest Result Values by Receptor Net
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include EVALCART receptors with DISCCART
!                    receptors for output.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
!
!        MODIFIED:   To remove references to BOUNDARY receptors
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
!
!        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
!                    to better accommodate UTM coordinates - 9/29/92
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   LTOUT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: ihnum, i, j, k, ii, indz, indc, nx, ny, indexw
   integer :: idec, imod
   double precision :: ycoval, xrms, yrms, dist, dir
   character :: buf132*132, chrval*5, chrvals(10)*5

!     Variable Initializations
   data (chrvals(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'SPRTHT'
   buf132 = ' '
   indz   = 0

! --- Assign character label for rank
   if (ihnum <= 10) then
      chrval = chrvals(ihnum)
   else if (mod(ihnum,100) > 10 .and.&
   &mod(ihnum,100) < 20) then
      idec = int(ihnum/10)
      imod = mod(ihnum,10)
      write(chrval,'(I2,I1,"TH")') idec, imod
   else if (ihnum <= 999) then
      idec = int(ihnum/10)
      imod = mod(ihnum,10)
      if (imod == 0) imod = 10
      write(chrval,'(I2,A3)') idec, chrvals(imod)(3:5)
   end if

   do igrp = 1, numgrp

      if (.not. psdcredit) then
!           Fill Work Array With SRCIDs For This Group
         indgrp = 0
         do isrc = 1, numsrc
            if (igroup(isrc,igrp) == 1) then
               indgrp = indgrp + 1
               workid(indgrp) = srcid(isrc)
            end if
         end do
      else
!           Assign 'N/A' for source IDs for PSDCREDIT option
         indgrp = 1
         workid(indgrp) = 'N/A'
      end if

! ---    Check for BACKGROUND "source" being included
!        in source group
      if (grp_back(igrp)) then
         indgrp = indgrp + 1
         workid(indgrp) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
         indexw = min(29,nsrc+1)
      else
         indexw = min(29,nsrc)
      end if
!        Check for More Than 29 Sources Per Group
      if (indgrp > indexw) then
         workid(indexw) = ' . . . '
         indgrp = indexw
      end if

!        Print Receptor Network Coordinates:
!        Set Number of Columns Per Page, NCPP
      ncpp = 5
!        Set Number of Rows Per Page, NRPP
      nrpp = 40
!        Begin LOOP Through Networks
      do i = 1, innet
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         nppx = 1 + int((numxpt(i)-1)/ncpp)
         nppy = 1 + int((numypt(i)-1)/nrpp)
         do nx = 1, nppx
            do ny = 1, nppy
               call header(iounit)
               write(iounit,9032) chrval, chrave(iave),&
               &(chidep(ii,ityp),ii=1,6),grpid(igrp),(workid(j),j=1,indgrp)
               write(iounit,9037) ntid(i), nttyp(i)
!                 Print The Values By Source Group
               write(iounit,9011) chidep(3,ityp), pollut,&
               &outlbl(ityp)
               if (nx == nppx) then
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  end if
               else
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  end if
               end if
               write(iounit,9010)
               if (ny == nppy) then
                  do k = 1+nrpp*(ny-1), numypt(i)
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(hivalu(indz+j-1,ihnum,igrp,iave,ityp),&
                        &hclmsg(indz+j-1,ihnum,igrp,iave,ityp),&
                        &nhidat(indz+j-1,ihnum,igrp,iave,ityp),&
                        &j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(hivalu(indz+j-1,ihnum,igrp,iave,ityp),&
                        &hclmsg(indz+j-1,ihnum,igrp,iave,ityp),&
                        &nhidat(indz+j-1,ihnum,igrp,iave,ityp),&
                        &j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               else
                  do k = 1+nrpp*(ny-1), nrpp*ny
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(hivalu(indz+j-1,ihnum,igrp,iave,ityp),&
                        &hclmsg(indz+j-1,ihnum,igrp,iave,ityp),&
                        &nhidat(indz+j-1,ihnum,igrp,iave,ityp),&
                        &j=1+ncpp*(nx-1),numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(hivalu(indz+j-1,ihnum,igrp,iave,ityp),&
                        &hclmsg(indz+j-1,ihnum,igrp,iave,ityp),&
                        &nhidat(indz+j-1,ihnum,igrp,iave,ityp),&
                        &j=1+ncpp*(nx-1),ncpp*nx)
                     end if
                  end do
               end if
            end do
         end do
      end do
!        End LOOP Through Networks

      if (irstat(4)/=0 .or. irstat(8)/=0) then
! ---       Include EVALCART receptors with DISCCART receptors.
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
         indc = 0
         do irec = 1, numrec
            if (rectyp(irec) == 'DC') then
               indc = indc + 1
               if (mod(indc-1,80) == 0) then
                  call header(iounit)
                  write(iounit,9032) chrval, chrave(iave),&
                  &(chidep(ii,ityp),ii=1,6),grpid(igrp),(workid(j),j=1,indgrp)
                  write(iounit,9043)
                  write(iounit,9011) chidep(3,ityp), pollut,&
                  &outlbl(ityp)
                  write(iounit,9048) chidep(3,ityp), chidep(3,ityp)
               end if
               if (mod(indc,2) /= 0) then
                  write(buf132(1:65),9045) axr(irec), ayr(irec),&
                  &hivalu(irec,ihnum,igrp,iave,ityp),&
                  &hclmsg(irec,ihnum,igrp,iave,ityp),&
                  &nhidat(irec,ihnum,igrp,iave,ityp)
               else
                  write(buf132(66:130),9045) axr(irec), ayr(irec),&
                  &hivalu(irec,ihnum,igrp,iave,ityp),&
                  &hclmsg(irec,ihnum,igrp,iave,ityp),&
                  &nhidat(irec,ihnum,igrp,iave,ityp)
                  write(iounit,9090) buf132
                  write(buf132,9095)
               end if
            end if
         end do
         if (mod(indc,2) /= 0) then
            write(iounit,9090) buf132
            write(buf132,9095)
         end if
      end if

      if (irstat(5) /= 0) then
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
         indc = 0
         do irec = 1, numrec
            if (rectyp(irec) == 'DP') then
               indc = indc + 1
               xrms = axr(irec) - axs(iref(irec))
               yrms = ayr(irec) - ays(iref(irec))
               dist = dsqrt(xrms*xrms + yrms*yrms)
               dir  = datan2(xrms, yrms) * rtodeg
               if (dir <= 0.0d0) dir = dir + 360.0d0
               if (mod(indc-1,80) == 0) then
                  call header(iounit)
                  write(iounit,9032) chrval, chrave(iave),&
                  &(chidep(ii,ityp),ii=1,6),grpid(igrp),(workid(j),j=1,indgrp)
                  write(iounit,9044)
                  write(iounit,9011) chidep(3,ityp), pollut,&
                  &outlbl(ityp)
                  write(iounit,9049) chidep(3,ityp), chidep(3,ityp)
               end if
               if (mod(indc,2) /= 0) then
                  write(buf132(1:66),9047) srcid(iref(irec)), dist,&
                  &dir, hivalu(irec,ihnum,igrp,iave,ityp),&
                  &hclmsg(irec,ihnum,igrp,iave,ityp),&
                  &nhidat(irec,ihnum,igrp,iave,ityp)
               else
                  write(buf132(67:132),9047) srcid(iref(irec)), dist,&
                  &dir, hivalu(irec,ihnum,igrp,iave,ityp),&
                  &hclmsg(irec,ihnum,igrp,iave,ityp),&
                  &nhidat(irec,ihnum,igrp,iave,ityp)
                  write(iounit,9090) buf132
                  write(buf132,9095)
               end if
            end if
         end do
         if (mod(indc,2) /= 0) then
            write(iounit,9090) buf132
            write(buf132,9095)
         end if
      end if

   end do

9011 format(/40x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9010 format(66(' -')/)
9016 format(1x,' Y-COORD  |',50x,'X-COORD (METERS)')
9017 format(1x,' (METERS) |',3x,f13.2,4(11x,f13.2,:))
9018 format(1x,'DIRECTION |',50x,'DISTANCE (METERS)')
9019 format(1x,'(DEGREES) |',3x,f13.2,4(11x,f13.2,:))
9013 format(1x,f9.1,1x,'|',5(f13.5,a1,'(',i8.8,')',:))
9032 format(/30x,'*** THE ',a5,' HIGHEST ',a5,1x,6a4,&
   &'VALUES FOR SOURCE GROUP:',2x,a8,' ***',&
   &/34x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
9037 format(/35x,'*** NETWORK ID: ',a8,' ;  NETWORK TYPE: ',&
   &a8,' ***')
9043 format(/45x,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 format(/47x,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 format(6x,2(f11.2,2x),f13.5,a1,1x,'(',i8.8,')')
9047 format(1x,a12,': ',f11.2,1x,f10.2,1x,f13.5,a1,1x,'(',i8.8,')')
9048 format(6x,'X-COORD (M)  Y-COORD (M)        ',a4,5x,'(YYMMDDHH)',&
   &14x,'X-COORD (M)  Y-COORD (M)        ',a4,5x,'(YYMMDDHH)',&
   &/66(' -'))
9049 format(4x,'ORIGIN',60x,'ORIGIN',&
   &/5x,'SRCID        DIST (M)  DIR (DEG)       ',a4,&
   &5x,'(YYMMDDHH)',&
   &8x,'SRCID        DIST (M)  DIR (DEG)       ',a4,&
   &5x,'(YYMMDDHH)',&
   &/65(' -'))
9090 format(a132)
9095 format(132(' '))

   return
end subroutine sprtht

subroutine prtmax
!***********************************************************************
!                 PRTMAX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Overall Maximum Value Tables
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To correct potential error when IMXVAL = 1 and
!                    MXLOCA = 0.  R.W. Brode, PES, Inc. - 12/2/98
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   OUTPUT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j, k, l, npg, nrows, jstrt, ii, j1, kmax1, kmax2,&
   &indexw
   double precision :: xr1, yr1, xr2, yr2
   character :: nty1*2, nty2*2

!     Variable Initializations
   modnam = 'PRTMAX'

   do iave = 1, numave
!        Check Array to See IF Maximum Values Are Needed For This AVEPER
      if (maxave(iave) /= 1) cycle

      do igrp = 1, numgrp
         if (.not. psdcredit) then
!              Fill Work Array With SRCIDs For This Group
            indgrp = 0
            do isrc = 1, numsrc
               if (igroup(isrc,igrp) == 1) then
                  indgrp = indgrp + 1
                  workid(indgrp) = srcid(isrc)
               end if
            end do
         else
!              Assign 'N/A' for source IDs for PSDCREDIT option
            indgrp = 1
            workid(indgrp) = 'N/A'
         end if

! ---       Check for BACKGROUND "source" being included
!           in source group
         if (grp_back(igrp)) then
            indgrp = indgrp + 1
            workid(indgrp) = 'BACKGROUND'
!              Check for More Than 29 Sources Per Group
            indexw = min(29,nsrc+1)
         else
            indexw = min(29,nsrc)
         end if
!           Check for More Than 29 Sources Per Group
         if (indgrp > indexw) then
            workid(indexw) = ' . . . '
            indgrp = indexw
         end if

         if (imxval(iave) >= 2) then
!              Determine Number of Pages @ 80 Per Page, NPG
            npg = 1 + int((imxval(iave)-1)/80)
            do l = 1, npg
!                 Determine Number of Rows for This Page, NROWS
               if (l == npg) then
                  nrows = (imxval(iave)-80*(l-1))/2
               else
                  nrows = 40
               end if
!                 Write Out Header Information for This Page
               call header(iounit)
               write(iounit,9032) imxval(iave), chrave(iave),&
               &(chidep(ii,ityp),ii=1,6), grpid(igrp), (workid(k),&
               &k = 1,indgrp)
               write(iounit,9011) chidep(3,ityp), pollut,&
               &outlbl(ityp)
               write(iounit,1) chidep(3,ityp), chidep(3,ityp)
!                 Set Start Row of Loop for This Page, JSTRT
               jstrt = 1 + 80*(l-1)
               do j = jstrt, jstrt+nrows-1
                  j1 = j + nrows
                  if (l==npg .and. mod(imxval(iave),2)/=0) then
                     j1 = j1 + 1
                  end if
                  kmax1 = mxloca(j,igrp,iave,ityp)
                  kmax2 = mxloca(j1,igrp,iave,ityp)
                  if (kmax1 == 0) then
                     xr1 = 0.0d0
                     yr1 = 0.0d0
                     nty1 = ' '
                  else
                     xr1 = axr(kmax1)
                     yr1 = ayr(kmax1)
                     nty1 = rectyp(kmax1)
                  end if
                  if (kmax2 == 0) then
                     xr2 = 0.0d0
                     yr2 = 0.0d0
                     nty2 = ' '
                  else
                     xr2 = axr(kmax2)
                     yr2 = ayr(kmax2)
                     nty2 = rectyp(kmax2)
                  end if
                  write(iounit,2) j, rmxval(j,igrp,iave,ityp),&
                  &mclmsg(j,igrp,iave,ityp), mxdate(j,igrp,iave,ityp),&
                  &xr1, yr1, nty1, j1,&
                  &rmxval(j1,igrp,iave,ityp), mclmsg(j1,igrp,iave,ityp),&
                  &mxdate(j1,igrp,iave,ityp), xr2, yr2, nty2
               end do
            end do
            if (mod(imxval(iave),2) /= 0) then
!                 Odd Number of Max Values - Print Out Last Value
               j = int(imxval(iave)/2) + 1 + 40*(npg-1)
               kmax1 = mxloca(j,igrp,iave,ityp)
               if (kmax1 == 0) then
                  xr1 = 0.0d0
                  yr1 = 0.0d0
                  nty1 = ' '
               else
                  xr1 = axr(kmax1)
                  yr1 = ayr(kmax1)
                  nty1 = rectyp(kmax1)
               end if
               write(iounit,3) j, rmxval(j,igrp,iave,ityp),&
               &mclmsg(j,igrp,iave,ityp), mxdate(j,igrp,iave,ityp),&
               &xr1, yr1, nty1
            end if
         else
            j = 1
            kmax1 = mxloca(j,igrp,iave,ityp)
            if (kmax1 == 0) then
               xr1 = 0.0d0
               yr1 = 0.0d0
               nty1 = '  '
            else
               xr1 = axr(kmax1)
               yr1 = ayr(kmax1)
               nty1 = rectyp(kmax1)
            end if
            call header(iounit)
            write(iounit,9032) imxval(iave), chrave(iave),&
            &(chidep(ii,ityp),ii=1,6), grpid(igrp), (workid(k),&
            &k = 1,indgrp)
            write(iounit,9011) chidep(3,ityp), pollut, outlbl(ityp)
            write(iounit,1) chidep(3,ityp), chidep(3,ityp)
            write(iounit,3) j, rmxval(j,igrp,iave,ityp),&
            &mclmsg(j,igrp,iave,ityp), mxdate(j,igrp,iave,ityp),&
            &xr1, yr1, nty1
         end if

!           WRITE Out Explanation of Receptor Types
         write(iounit,9050)

      end do
   end do

1  format(1x,'RANK',8x,a4,4x,'(YYMMDDHH) AT',6x,&
   &'RECEPTOR (XR,YR) OF TYPE ',3x,&
   &'RANK',8x,a4,4x,'(YYMMDDHH) AT',6x,&
   &'RECEPTOR (XR,YR) OF TYPE ',&
   &/66(' -'))
2  format(1x,i4,'.',1x,f13.5,a1,'(',i8.8,') AT',1x,&
   &'(',f10.2,', ',f10.2,')  ',a2,5x,&
   &i4,'.',1x,f13.5,a1,'(',i8.8,') AT',1x,&
   &'(',f10.2,', ',f10.2,')  ',a2)
3  format(1x,i4,'.',1x,f13.5,a1,'(',i8.8,') AT',1x,&
   &'(',f10.2,', ',f10.2,')  ',a2)
9011 format(/40x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9032 format(/30x,'*** THE MAXIMUM ',i4,2x,a5,1x,6a4,&
   &'VALUES FOR SOURCE GROUP:',2x,a8,' ***'&
   &/34x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
9050 format(/1x,' *** RECEPTOR TYPES:  GC = GRIDCART',&
   &/23x,'GP = GRIDPOLR',&
   &/23x,'DC = DISCCART',&
   &/23x,'DP = DISCPOLR')

   return
end subroutine prtmax

subroutine prtsum(iount)
!***********************************************************************
!                 PRTSUM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out the Result Summary Tables
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Adjusted format of column headers and other write
!                    statements, including removal of '1X' used to
!                    skip the Fortran carriage-control character, which
!                    is no longer needed.  Also included number of years
!                    for MULTYEAR option.
!                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        MODIFIED:   To use arrays for high short term values, rather
!                    than reading from temporary event file.
!                    R.W. Brode, PES, Inc. - August 15, 1995.
!
!        INPUTS:  EVENT.TMP File Which Contains Maximum Values
!
!        OUTPUTS: Result Summary Table By Average Period
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: iwhp(nval), i, ival, indmx, ihst, indloc, iount
   integer :: idec, imod
   double precision :: axr1, ayr1, azelv1, azhil1, azflg1, xr2, yr2,&
   &ze2, zh2,zf2
   character :: perchr*6, rank(10)*5, chrval*5

!     Variable Initializations
   data (rank(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'PRTSUM'

   if (period) then
      perchr = 'PERIOD'
   else if (annual) then
      perchr = 'ANNUAL'
   end if

!     Print Maximum PERIOD Averages, If Appropriate
   if (period .or. annual) then
!        Calculate Number of Groups Per Page, NGPP
      ngpp = max( 1, int(50/(nhiann+1)) )
      do igrp = 1, numgrp
         if (mod(igrp-1, ngpp) == 0) then
            call header(iount)
            if (period .and. multyr) then
               write(iount,9020) perchr, ianhrs, numyrs
            else if (period) then
               write(iount,9021) perchr, ianhrs
            else if (annual) then
               write(iount,9023) perchr, numyrs
            end if
            write(iount,9011) chidep(3,ityp), pollut, perlbl(ityp)
            write(iount,9022) chidep(1,ityp), chidep(2,ityp),&
            &chidep(3,ityp)
         end if
         do ival = 1, nhiann
            indmx = imxloc(ival,igrp,ityp)
            if (ival == 1 .and. indmx /= 0) then
               write(iount,1012) grpid(igrp), rank(ival),&
               &amxval(ival,igrp,ityp), axr(indmx), ayr(indmx),&
               &azelev(indmx), azhill(indmx), azflag(indmx),&
               &rectyp(indmx), netid(indmx)
            else if (ival == 1 .and. indmx == 0) then
               axr1 = 0.0d0
               ayr1 = 0.0d0
               azelv1 = 0.0d0
               azhil1 = 0.0d0
               azflg1 = 0.0d0
               write(iount,1014) grpid(igrp), rank(ival),&
               &amxval(ival,igrp,ityp), axr1, ayr1, azelv1, azhil1,&
               &azflg1
            else if (indmx == 0) then
               axr1 = 0.0d0
               ayr1 = 0.0d0
               azelv1 = 0.0d0
               azhil1 = 0.0d0
               azflg1 = 0.0d0
               write(iount,1015) rank(ival),&
               &amxval(ival,igrp,ityp), axr1, ayr1, azelv1, azhil1,&
               &azflg1
            else
               write(iount,1013) rank(ival),&
               &amxval(ival,igrp,ityp), axr(indmx), ayr(indmx),&
               &azelev(indmx), azhill(indmx), azflag(indmx),&
               &rectyp(indmx), netid(indmx)
            end if
         end do
      end do
!        WRITE Out Explanation of Receptor Types
      write(iount,9050)
   end if

! --- Skip "standard" summary of short-term averages for
!     PM25AVE, NO2AVE, or SO2AVE processing
   if (pm25ave .or. no2ave .or. so2ave) return

! ---
!     Begin LOOP Through Averaging Periods
   do iave = 1, numave
! ---    Determine which high ranked values are requested for
!        this averaging period
      ihst = 0
      do ival = 1, nval
         if (nhiave(ival,iave) == 1) then
            ihst = ihst + 1
            iwhp(ihst) = ival
         end if
      end do
      if (ihst == 0) then
!           No High Values for This IAVE; Cycle to Next Averaging Period
         cycle
      end if
!        Calculate Number of Groups Per Page, NGPP
      ngpp = max( 1, int(50/(ihst+1)) )

!        Begin Source Group LOOP
      do igrp = 1, numgrp
!           Begin LOOP Through High Values
         do i = 1, ihst

! ---          Assign character label for rank
            if (iwhp(i) <= 10) then
               chrval = rank(iwhp(i))
            else if (mod(iwhp(i),100) > 10 .and.&
            &mod(iwhp(i),100) < 20) then
               idec = int(iwhp(i)/10)
               imod = mod(iwhp(i),10)
               write(chrval,'(I2,I1,"TH")') idec, imod
            else if (iwhp(i) <= 999) then
               idec = int(iwhp(i)/10)
               imod = mod(iwhp(i),10)
               if (imod == 0) imod = 10
               write(chrval,'(I2,A3)') idec, rank(imod)(3:5)
            end if

            indloc = hmloc(i,igrp,iave,ityp)
            if (i == 1) then
               if (mod(igrp-1,ngpp) == 0) then
                  call header(iount)
                  if (multyr) then
                     write(iount,9030) chrave(iave), numyrs
                  else
                     write(iount,9031) chrave(iave)
                  end if
                  write(iount,9011) chidep(3,ityp),pollut,&
                  &outlbl(ityp)
                  write(iount,9032) chidep(1,ityp),&
                  &chidep(2,ityp),chidep(3,ityp)
               end if
               write(iount,*) ' '
               if (indloc == 0) then
                  xr2 = 0.0d0
                  yr2 = 0.0d0
                  ze2 = 0.0d0
                  zh2 = 0.0d0
                  zf2 = 0.0d0
                  write(iount,1004) grpid(igrp), chrval,&
                  &hmax(i,igrp,iave,ityp),&
                  &hmclm(i,igrp,iave,ityp),&
                  &hmdate(i,igrp,iave,ityp),&
                  &xr2, yr2, ze2, zh2, zf2
               else
                  xr2 = axr(indloc)
                  yr2 = ayr(indloc)
                  ze2 = azelev(indloc)
                  zh2 = azhill(indloc)
                  zf2 = azflag(indloc)
                  write(iount,1002) grpid(igrp), chrval,&
                  &hmax(i,igrp,iave,ityp),&
                  &hmclm(i,igrp,iave,ityp),&
                  &hmdate(i,igrp,iave,ityp),&
                  &xr2, yr2, ze2, zh2, zf2, rectyp(indloc),&
                  &netid(indloc)
               end if
            else
               if (indloc == 0) then
                  xr2 = 0.0d0
                  yr2 = 0.0d0
                  ze2 = 0.0d0
                  zf2 = 0.0d0
                  write(iount,1005) chrval,&
                  &hmax(i,igrp,iave,ityp),&
                  &hmclm(i,igrp,iave,ityp),&
                  &hmdate(i,igrp,iave,ityp),&
                  &xr2, yr2, ze2, zh2, zf2
               else
                  xr2 = axr(indloc)
                  yr2 = ayr(indloc)
                  ze2 = azelev(indloc)
                  zh2 = azhill(indloc)
                  zf2 = azflag(indloc)
                  write(iount,1003) chrval,&
                  &hmax(i,igrp,iave,ityp),&
                  &hmclm(i,igrp,iave,ityp),&
                  &hmdate(i,igrp,iave,ityp),&
                  &xr2, yr2, ze2, zh2, zf2, rectyp(indloc),&
                  &netid(indloc)
               end if
            end if
         end do
      end do

!        WRITE Out Explanation of Receptor Types
      write(iount,9050)

!     End loop through averaging periods
   end do

1002 format(a8,' HIGH ',a5,' HIGH VALUE IS',f14.5,a1,' ON ',&
   &i8.8,': AT ','(',2(f11.2,', '),2(f8.2,', '),f7.2,')',&
   &2x,a2,2x,a8)
1003 format(8x,' HIGH ',a5,' HIGH VALUE IS',f14.5,a1,' ON ',&
   &i8.8,': AT ','(',2(f11.2,', '),2(f8.2,', '),f7.2,')',&
   &2x,a2,2x,a8)
1004 format(a8,' HIGH ',a5,' HIGH VALUE IS',f14.5,a1,' ON ',&
   &i8.8,': AT ','(',2(f11.2,', '),2(f8.2,', '),f7.2,')')
1005 format(8x,' HIGH ',a5,' HIGH VALUE IS',f14.5,a1,' ON ',&
   &i8.8,': AT ','(',2(f11.2,', '),2(f8.2,', '),f7.2,')')
1012 format(/a8,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')',2x,a2,2x,a8)
1013 format(8x,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')',2x,a2,2x,a8)
1014 format(/a8,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')')
1015 format(8x,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')')
9011 format(/36x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9020 format(/40x,'*** THE SUMMARY OF MAXIMUM ',a6,' (',i6,&
   &' HRS) RESULTS ***',/40x,'***    ACROSS ',i6,&
   &' YEARS WITH THE MULTYEAR OPTION    ***'/)
9021 format(/40x,'*** THE SUMMARY OF MAXIMUM ',a6,' (',i6,&
   &' HRS) RESULTS ***'/)
9023 format(/35x,'*** THE SUMMARY OF MAXIMUM ',a6,' RESULTS ',&
   &'AVERAGED OVER ',i3,' YEARS ***'/)
9022 format(109x,'NETWORK',/,'GROUP ID',23x,3a4,&
   &16x,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',2x,'OF TYPE',&
   &2x,'GRID-ID',/60('- '))
9030 format(/48x,'*** THE SUMMARY OF HIGHEST ',a5,' RESULTS ***'/,&
   &44x,'*** ACROSS ',i6,&
   &' YEARS WITH THE MULTYEAR OPTION ***'/)
9031 format(/48x,'*** THE SUMMARY OF HIGHEST ',a5,' RESULTS ***'/)
9032 format(54x,'DATE',68x,'NETWORK',/,'GROUP ID',26x,3a4,5x,&
   &'(YYMMDDHH)',13x,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',&
   &4x,'OF TYPE',2x,'GRID-ID',/66('- '))
9050 format(//' *** RECEPTOR TYPES:  GC = GRIDCART',&
   &/22x,'GP = GRIDPOLR',&
   &/22x,'DC = DISCCART',&
   &/22x,'DP = DISCPOLR')

   return
end subroutine prtsum

subroutine evefil
!***********************************************************************
!                 EVEFIL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generate EVENT Input File
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To allow for changes in the ISTRG PARAMETER, currently
!                    set to 132.  Also moved the code to insert a blank line
!                    after each pathway to SUB. SETUP.
!                    R.W. Brode, PES, Inc. - November 15, 1995.
!
!        INPUTS:  EVENT.TMP File Which Contains Maximum 10 Values
!
!        OUTPUTS: EVENT Input Runstream Image File
!
!        CALLED FROM: OUTPUT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: iavep
   double precision :: conc1
   character :: evfrm*20
   logical :: hitin

!     Variable Initializations
   modnam = 'EVEFIL'
   hitin  = .false.
   eof    = .false.

!     Setup WRITE format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(evfrm,9300) istrg
9300 format('(A',i4.4,')')

!     Rewind Temporary Event File
   rewind itevut

!     Read Records From The Temporary Event File
   do while (.not. eof)
      if (.not. hitin) then
!           Not in the Event Pathway - Echo Input to EVENT File
         read(itevut,evfrm,end=999) runst1
         if (runst1(1:11) == 'EV STARTING') then
!              Event Pathway Starts - Set Logical Switch
            hitin = .true.
            if (locb(1) == 1) then
               write(ievunt,'(a:)') runst1(1:len_trim(runst1))
            else if (locb(1) == 2) then
               write(ievunt,'(1x,a:)') runst1(1:len_trim(runst1))
            else if (locb(1) == 3) then
               write(ievunt,'(2x,a:)') runst1(1:len_trim(runst1))
            else if (locb(1) == 4) then
               write(ievunt,'(3x,a:)') runst1(1:len_trim(runst1))
            end if
         else
            write(ievunt,'(a:)') runst1(1:len_trim(runst1))
         end if
      else
         read(itevut,evfrm,end=999) runst1
         if (runst1(1:11) == 'EV FINISHED') then
            if (mxfile) then
!                 Add Events From Max Value (>Thresh) Files ---   CALL MXEVNT
               call mxevnt
            end if
            if (locb(1) == 1) then
               write(ievunt,'(a:)') runst1(1:len_trim(runst1))
            else if (locb(1) == 2) then
               write(ievunt,'(1x,a:)') runst1(1:len_trim(runst1))
            else if (locb(1) == 3) then
               write(ievunt,'(2x,a:)') runst1(1:len_trim(runst1))
            else if (locb(1) == 4) then
               write(ievunt,'(3x,a:)') runst1(1:len_trim(runst1))
            end if
            hitin = .false.
         end if
         if (hitin .and. runst1(locb(1):locb(1)+10) ==&
         &'   EVENTPER') then
            read(runst1(locb(1)+23:),'(I3)') iavep
            read(runst1(locb(1)+47:),'(F18.5)',err=99) conc1
         end if

         go to 100

!           Write Out Warning Message:  Error Reading CONC From TmpEvent File
99       call errhdl(path,modnam,'W','570',&
         &runst1(locb(1)+12:locb(1)+19))
!           Set CONC1 To Large Value for Event File
         conc1 = 1.0d9

100      continue
         if (hitin.and. iavep/=720 .and. conc1/=0.0d0) then
!              Write Out EVENTPER & EVENTLOC Cards, Allowing for Column Shift
            write(ievunt,'(a:)') runst1(locb(1):len_trim(runst1))
         end if
      end if

      go to 11

999   eof = .true.
11    continue
   end do

!     Write OU Pathway Images to EVENT File, Allowing For Column Shift
   if (locb(1) == 1) then
      write(ievunt,1011) evparm
   else if (locb(1) == 2) then
      write(ievunt,1012) evparm
   else if (locb(1) == 3) then
      write(ievunt,1013) evparm
   else if (locb(1) == 4) then
      write(ievunt,1014) evparm
   end if

   close(unit=ievunt)

1011 format(/'OU STARTING',&
   &/'   EVENTOUT  ',a6,&
   &/'OU FINISHED')
1012 format(/' OU STARTING',&
   &/'    EVENTOUT  ',a6,&
   &/' OU FINISHED')
1013 format(/'  OU STARTING',&
   &/'     EVENTOUT  ',a6,&
   &/'  OU FINISHED')
1014 format(/'   OU STARTING',&
   &/'      EVENTOUT  ',a6,&
   &/'   OU FINISHED')

   return
end subroutine evefil

subroutine mxevnt
!***********************************************************************
!                 MXEVNT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generate EVENT File Inputs From
!                 Maximum Value (>Threshold) Files
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To add one more decimal place to receptor elevations
!                    and flagpole heights for the event file.
!                    R.W. Brode, PES, Inc. - November 15, 1995.
!
!        INPUTS:  Maximum Value Files
!
!        OUTPUTS: Events for EVENT Input Runstream File
!
!        CALLED FROM: EVEFIL
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: iavep, kdate
   double precision :: conc1, xr2, yr2, ze2, zh2, zf2
   character :: nameev*10, gid*8, bufin*90

!     Variable Initializations
   modnam = 'MXEVNT'

!     Begin Averaging Period LOOP
   do iave = 1, numave
!        Initialize Event Counter for This IAVE
      numeve = 0
!        Begin Source Group LOOP
      do igrp = 1, numgrp
         if (maxfle(igrp,iave) == 1) then
!              Maximum Value File Exists for This Group and AvePer
!              Rewind File
            rewind imxunt(igrp,iave)
            eof = .false.

!              Loop Through Threshold File and Write Out Events to EVENT File
            do while (.not. eof)
               read(imxunt(igrp,iave),100,err=99,end=999) bufin
100            format(a90)
!                 Skip Record if Part of Header, '*' in Column 1
               if (bufin(1:1) == '*') go to 11
               read(bufin,thrfrm,err=99) iavep,&
               &gid, kdate, xr2, yr2, ze2, zh2, zf2, conc1
               if (iavep/=720 .and. iavep==kave(iave) .and.&
               &gid==grpid(igrp)) then
!                    Increment Event Counter and Generate Event Name
                  numeve = numeve + 1

                  if (numeve > 999999) then
!                       Number of Events Exceeds Limit of Field,
!                       Write Warning Message and Reset to 1
                     write(dummy,'(3X,I2.2,3X)') iavep
                     call errhdl(path,modnam,'W','413',dummy)
                     numeve = 1
                  end if

                  write(nameev,'("TH",I2.2,I6.6)') iavep, numeve
!                    Write EVENTPER & EVENTLOC Cards, Allowing for Col. Shift
                  if (locb(1) == 1) then
                     write(ievunt,1901) nameev,iavep,gid,kdate,conc1
                     write(ievunt,1911) nameev, xr2, yr2, ze2, zh2,&
                     &zf2
                  else if (locb(1) == 2) then
                     write(ievunt,1902) nameev,iavep,gid,kdate,conc1
                     write(ievunt,1912) nameev, xr2, yr2, ze2, zh2,&
                     &zf2
                  else if (locb(1) == 3) then
                     write(ievunt,1903) nameev,iavep,gid,kdate,conc1
                     write(ievunt,1913) nameev, xr2, yr2, ze2, zh2,&
                     &zf2
                  else if (locb(1) == 4) then
                     write(ievunt,1904) nameev,iavep,gid,kdate,conc1
                     write(ievunt,1914) nameev, xr2, yr2, ze2, zh2,&
                     &zf2
                  end if
                  go to 11
               else
                  go to 11
               end if

999            eof = .true.
11             continue
            end do

         end if
      end do
!        End Source Group LOOP
   end do
!     End Averaging Period LOOP

   go to 1000

!     WRITE Error Message for Error Reading Threshold File
99 write(dummy,'("MAXFL",I3.3)') imxunt(igrp,iave)
   call errhdl(path,modnam,'E','510',dummy)

1901 format(3x,'EVENTPER',1x,a10,1x,i3,2x,a8,3x,i8.8,1x,f17.5)
1902 format(4x,'EVENTPER',1x,a10,1x,i3,2x,a8,3x,i8.8,1x,f17.5)
1903 format(5x,'EVENTPER',1x,a10,1x,i3,2x,a8,3x,i8.8,1x,f17.5)
1904 format(6x,'EVENTPER',1x,a10,1x,i3,2x,a8,3x,i8.8,1x,f17.5)
1911 format(3x,'EVENTLOC',1x,a10,1x,'XR= ',f15.6,' YR= ',f15.6,&
   &3(1x,f10.4))
1912 format(4x,'EVENTLOC',1x,a10,1x,'XR= ',f15.6,' YR= ',f15.6,&
   &3(1x,f10.4))
1913 format(5x,'EVENTLOC',1x,a10,1x,'XR= ',f15.6,' YR= ',f15.6,&
   &3(1x,f10.4))
1914 format(6x,'EVENTLOC',1x,a10,1x,'XR= ',f15.6,' YR= ',f15.6,&
   &3(1x,f10.4))

1000 return
end subroutine mxevnt

subroutine prtpm25
!***********************************************************************
!                 PRTPM25 Module of AERMOD Model
!
!        PURPOSE: Print Out The Average H8H Values for PM25
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       June 19, 1998
!
!        MODIFIED:   To allow user-specified rank for PM2.5 processing
!                    to accommodate latest guidance for PM2.5 modeling.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   To include EVALCART receptors with DISCCART
!                    receptors for output.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
!
!        MODIFIED:   To remove references to BOUNDARY receptors
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   OUTPUT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, ii, indz, indc, nx, ny, indexw, n
   integer :: idec, imod
   double precision :: ycoval, xrms, yrms, dist, dir
   character :: buf132*132

   character :: rank(10)*5, chrval*5

!     Variable Initializations
   data (rank(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'PRTPM25'
   buf132 = ' '
   indz   = 0

!     Write Out the 'EV STARTING' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   if (ityp == 1) then
      write(itevut,9000)
   end if

! --- Loop through ranks
   do n = 1, nval

!      If this rank not applicable, cycle to next rank
      if( nhiave(n,1) /= 1 ) cycle

! ---  Loop through source groups
      do igrp = 1, numgrp

         if (.not. psdcredit) then
!           Fill Work Array With SRCIDs For This Group
            indgrp = 0
            do isrc = 1, numsrc
               if (igroup(isrc,igrp) == 1) then
                  indgrp = indgrp + 1
                  workid(indgrp) = srcid(isrc)
               end if
            end do
         else
!           Assign 'N/A' for source IDs for PSDCREDIT option
            indgrp = 1
            workid(indgrp) = 'N/A'
         end if

! ---    Check for BACKGROUND "source" being included
!        in source group
         if (grp_back(igrp)) then
            indgrp = indgrp + 1
            workid(indgrp) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
            indexw = min(29,nsrc+1)
         else
            indexw = min(29,nsrc)
         end if
!        Check for More Than 29 Sources Per Group
         if (indgrp > indexw) then
            workid(indexw) = ' . . . '
            indgrp = indexw
         end if

!        Print Receptor Network Coordinates:
!        Set Number of Columns Per Page, NCPP
         ncpp = 9
!        Set Number of Rows Per Page, NRPP
         nrpp = 40
!        Begin LOOP Through Networks
         do i = 1, innet
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            nppx = 1 + int((numxpt(i)-1)/ncpp)
            nppy = 1 + int((numypt(i)-1)/nrpp)
            do nx = 1, nppx
               do ny = 1, nppy
                  call header(iounit)
                  if (nhiave(n,1) == 1) then
! ---                Assign character label for rank
                     if (n <= 10) then
                        chrval = rank(n)
                     else if (mod(n,100) > 10 .and.&
                     &mod(n,100) < 20) then
                        idec = int(n/10)
                        imod = mod(n,10)
                        write(chrval,'(I2,I1,"TH")') idec, imod
                     else if (n <= 999) then
                        idec = int(n/10)
                        imod = mod(n,10)
                        if (imod == 0) imod = 10
                        write(chrval,'(I2,A3)') idec, rank(imod)(3:5)
                     end if

                     if (no2ave .or. so2ave) then
                        write(iounit,90321) chrval,&
                        &(chidep(ii,ityp),ii=1,6),numyrs,&
                        &grpid(igrp),(workid(k),k = 1,indgrp)
                     else
                        write(iounit,9032) chrval, chrave(1),&
                        &(chidep(ii,ityp),ii=1,6),numyrs,&
                        &grpid(igrp),(workid(k),k = 1,indgrp)
                     end if
                  end if
                  write(iounit,9037) ntid(i), nttyp(i)
!                 Print The Values By Source Group
                  write(iounit,9011) chidep(3,ityp), pollut,perlbl(ityp)
                  if (nx == nppx) then
                     if (nttyp(i) == 'GRIDCART') then
                        write(iounit,9016)
                        write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                        &numxpt(i))
                     else if (nttyp(i) == 'GRIDPOLR') then
                        write(iounit,9018)
                        write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                        &numxpt(i))
                     end if
                  else
                     if (nttyp(i) == 'GRIDCART') then
                        write(iounit,9016)
                        write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                        &ncpp*nx)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        write(iounit,9018)
                        write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                        &ncpp*nx)
                     end if
                  end if
                  write(iounit,9010)
                  if (ny == nppy) then
                     do k = 1+nrpp*(ny-1), numypt(i)
                        if (nttyp(i) == 'GRIDCART') then
                           indz = netend(i) - k*numxpt(i) + 1
                           ycoval = ycoord(numypt(i)-k+1,i)
                        else if (nttyp(i) == 'GRIDPOLR') then
                           indz = netsta(i) + (k-1)*numxpt(i)
                           ycoval = ycoord(k,i)
                        end if
                        if (nx == nppx) then
                           write(iounit,9013) ycoval,&
                           &(sumhnh(indz+j-1,igrp,n),j=1+ncpp*(nx-1),numxpt(i))
                        else
                           write(iounit,9013) ycoval,&
                           &(sumhnh(indz+j-1,igrp,n),j=1+ncpp*(nx-1),ncpp*nx)
                        end if
                     end do
                  else
                     do k = 1+nrpp*(ny-1), nrpp*ny
                        if (nttyp(i) == 'GRIDCART') then
                           indz = netend(i) - k*numxpt(i) + 1
                           ycoval = ycoord(numypt(i)-k+1,i)
                        else if (nttyp(i) == 'GRIDPOLR') then
                           indz = netsta(i) + (k-1)*numxpt(i)
                           ycoval = ycoord(k,i)
                        end if
                        if (nx == nppx) then
                           write(iounit,9013) ycoval,&
                           &(sumhnh(indz+j-1,igrp,n),j=1+ncpp*(nx-1),numxpt(i))
                        else
                           write(iounit,9013) ycoval,&
                           &(sumhnh(indz+j-1,igrp,n),j=1+ncpp*(nx-1),ncpp*nx)
                        end if
                     end do
                  end if
               end do
            end do
         end do
!        End LOOP Through Networks

         if (irstat(4)/=0 .or. irstat(8)/=0) then
! ---       Include EVALCART receptors with DISCCART receptors.
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            indc = 0
            do irec = 1, numrec
               if (rectyp(irec) == 'DC') then
                  indc = indc + 1
                  if (mod(indc-1,80) == 0) then
                     call header(iounit)
                     if (nhiave(n,1) == 1) then
! ---                   Assign character label for rank
                        if (n <= 10) then
                           chrval = rank(n)
                        else if (mod(n,100) > 10 .and.&
                        &mod(n,100) < 20) then
                           idec = int(n/10)
                           imod = mod(n,10)
                           write(chrval,'(I2,I1,"TH")') idec, imod
                        else if (n <= 999) then
                           idec = int(n/10)
                           imod = mod(n,10)
                           if (imod == 0) imod = 10
                           write(chrval,'(I2,A3)') idec, rank(imod)(3:5)
                        end if

                        if (no2ave .or. so2ave) then
                           write(iounit,90321) chrval,&
                           &(chidep(ii,ityp),ii=1,6),&
                           &numyrs,grpid(igrp),(workid(k),k=1,indgrp)
                        else
                           write(iounit,9032) chrval, chrave(1),&
                           &(chidep(ii,ityp),ii=1,6),&
                           &numyrs,grpid(igrp),(workid(k),k=1,indgrp)
                        end if
                     end if
                     write(iounit,9043)
                     write(iounit,9011) chidep(3,ityp), pollut,&
                     &perlbl(ityp)
                     write(iounit,9048) chidep(3,ityp), chidep(3,ityp)
                  end if
                  if (mod(indc,2) /= 0) then
                     write(buf132(1:60),9045) axr(irec), ayr(irec),&
                     &sumhnh(irec,igrp,n)
                  else
                     write(buf132(61:120),9045) axr(irec), ayr(irec),&
                     &sumhnh(irec,igrp,n)
                     write(iounit,9090) buf132
                     write(buf132,9095)
                  end if
               end if
            end do
            if (mod(indc,2) /= 0) then
               write(iounit,9090) buf132
               write(buf132,9095)
            end if
         end if

         if (irstat(5) /= 0) then
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            indc = 0
            do irec = 1, numrec
               if (rectyp(irec) == 'DP') then
                  indc = indc + 1
                  xrms = axr(irec) - axs(iref(irec))
                  yrms = ayr(irec) - ays(iref(irec))
                  dist = dsqrt(xrms*xrms + yrms*yrms)
                  dir  = datan2(xrms, yrms) * rtodeg
                  if (dir <= 0.0d0) dir = dir + 360.0d0
                  if (mod(indc-1,80) == 0) then
                     call header(iounit)
                     if (nhiave(n,1) == 1) then
! ---                   Assign character label for rank
                        if (n <= 10) then
                           chrval = rank(n)
                        else if (mod(n,100) > 10 .and.&
                        &mod(n,100) < 20) then
                           idec = int(n/10)
                           imod = mod(n,10)
                           write(chrval,'(I2,I1,"TH")') idec, imod
                        else if (n <= 999) then
                           idec = int(n/10)
                           imod = mod(n,10)
                           if (imod == 0) imod = 10
                           write(chrval,'(I2,A3)') idec, rank(imod)(3:5)
                        end if

                        if (no2ave .or. so2ave) then
                           write(iounit,90321) chrval,&
                           &(chidep(ii,ityp),ii=1,6),&
                           &numyrs,grpid(igrp),(workid(k),k=1,indgrp)
                        else
                           write(iounit,9032) chrval, chrave(1),&
                           &(chidep(ii,ityp),ii=1,6),&
                           &numyrs,grpid(igrp),(workid(k),k=1,indgrp)
                        end if
                     end if
                     write(iounit,9044)
                     write(iounit,9011) chidep(3,ityp), pollut,&
                     &perlbl(ityp)
                     write(iounit,9049) chidep(3,ityp), chidep(3,ityp)
                  end if
                  if (mod(indc,2) /= 0) then
                     write(buf132(1:65),9047) srcid(iref(irec)), dist,&
                     &dir, sumhnh(irec,igrp,n)
                  else
                     write(buf132(66:130),9047) srcid(iref(irec)), dist,&
                     &dir, sumhnh(irec,igrp,n)
                     write(iounit,9090) buf132
                     write(buf132,9095)
                  end if
               end if
            end do
            if (mod(indc,2) /= 0) then
               write(iounit,9090) buf132
               write(buf132,9095)
            end if
         end if

      end do   ! End loop on source groups
   end do    ! End loop on ranks

!     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   if (ityp == 1) then
      write(itevut,9009)
   end if

9000 format('EV STARTING')
9009 format('EV FINISHED')
9011 format(/40x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9010 format(66(' -')/)
9013 format(2x,f10.2,1x,'|',1x,9(f13.5))
9016 format(3x,' Y-COORD  |',48x,'X-COORD (METERS)')
9017 format(3x,' (METERS) |',1x,9(1x,f12.2,:))
9018 format(3x,'DIRECTION |',48x,'DISTANCE (METERS)')
9019 format(3x,'(DEGREES) |',1x,9(1x,f12.2,:))
9032 format(/13x,'*** THE ',a5,'-HIGHEST ',a5,1x,6a4,' VALUES ',&
   &'AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP:',1x,a8,' ***',&
   &/34x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
90321 format(/4x,'*** THE ',a5,'-HIGHEST MAX DAILY 1-HR ',6a4,&
   &' VALUES ',&
   &'AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP:',1x,a8,' ***',&
   &/34x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
9037 format(/35x,'*** NETWORK ID: ',a8,' ;  NETWORK TYPE: ',&
   &a8,' ***')
9043 format(/45x,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 format(/47x,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 format(6x,2(f12.2,2x),f13.5)
9047 format(2x,a12,': ',f12.2,2x,f10.2,2x,f13.5)
9048 format(6x,' X-COORD (M)   Y-COORD (M)        ',a4,&
   &22x,' X-COORD (M)   Y-COORD (M)        ',a4,/65(' -'))
9049 format(5x,'ORIGIN',59x,'ORIGIN',&
   &/5x,' SRCID         DIST (M)   DIR (DEG)        ',a4,&
   &18x,' SRCID         DIST (M)   DIR (DEG)        ',a4,&
   &/65(' -'))
9090 format(a132)
9095 format(132(' '))

   return
end subroutine prtpm25

subroutine maxpm25
!***********************************************************************
!                 MAXPM25 Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Overall Maximum Value Arrays
!                 NMXPM = 10 Assigned in PARAMETER Statement in MAIN1
!                 Note: For duplicate values, the earlier occurrence keeps
!                       its rank within the array
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    June 19, 1998
!
!        INPUTS:  Maximum Value Table Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value Array
!                 Updated Maximum Date Array
!                 Updated Maximum Receptor Array
!
!        CALLED FROM:   HIVALS
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j, n

!     Variable Initializations
   modnam = 'MAXPM25'

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Begin loop through ranks
      do n = 1, nval
!           Cycle to next rank if this rank is not applicable
         if( nhiave(n,1) /= 1 ) cycle
!           Begin Receptor LOOP
         receptor_loop: do irec = 1, numrec
            if (nmxpm > 1) then
               if (sumhnh(irec,igrp,n) >&
               &mxpmval(nmxpm,igrp,n)) then
                  do j = nmxpm-1, 1, -1
                     if(sumhnh(irec,igrp,n) <=&
                     &mxpmval(j,igrp,n)) then
                        mxpmval(j+1,igrp,n) = sumhnh(irec,igrp,n)
                        mxpmloc(j+1,igrp,n) = irec
!                          Exit Block
                        cycle receptor_loop
                     else
                        mxpmval(j+1,igrp,n) = mxpmval(j,igrp,n)
                        mxpmloc(j+1,igrp,n) = mxpmloc(j,igrp,n)
                        if (j == 1) then
                           mxpmval(1,igrp,n) = sumhnh(irec,igrp,n)
                           mxpmloc(1,igrp,n) = irec
                        end if
                     end if
                  end do
               end if
            else if (nmxpm == 1) then
               if (sumhnh(irec,igrp,n) > mxpmval(1,igrp,n)) then
                  mxpmval(1,igrp,n) = sumhnh(irec,igrp,n)
                  mxpmloc(1,igrp,n) = irec
               end if
            end if
         end do receptor_loop
!           End Receptor LOOP
      end do
!        End loop through ranks
   end do
!     End Source Group LOOP

   return
end subroutine maxpm25

subroutine prtpm25sum(iount)
!***********************************************************************
!                 PRTPM25SUM Module of AERMOD Model
!
!        PURPOSE: Print Out the Result Summary Tables for PM25
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       June 19, 1998
!
!        MODIFIED:   To allow user-specified rank for PM2.5 processing
!                    to accommodate latest guidance for PM2.5 modeling.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   Adjusted format of column headers and other write
!                    statements, including removal of '1X' used to
!                    skip the Fortran carriage-control character, which
!                    is no longer needed.
!                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        INPUTS:  Arrays Containing Maximum Values
!
!        OUTPUTS: Result Summary Table By Average Period
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ival, indmx, iount, n
   integer :: idec, imod
   double precision :: axr1, ayr1, azelv1, azhil1, azflg1
   character :: rank(10)*5, chrval*5

!     Variable Initializations
   data (rank(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'PRTPM25SUM'

   if (numave == 1) then
!        Calculate Number of Groups Per Page, NGPP
      ngpp = max( 1, int(50/(nmxpm+1)) )
      do n = 1, nval
         if (nhiave(n,1) /= 1) cycle
         do igrp = 1, numgrp
            if (mod(igrp-1, ngpp) == 0) then
               call header(iount)
! ---             Assign character label for rank
               if (n <= 10) then
                  chrval = rank(n)
               else if (mod(n,100) > 10 .and.&
               &mod(n,100) < 20) then
                  idec = int(n/10)
                  imod = mod(n,10)
                  write(chrval,'(I2,I1,"TH")') idec, imod
               else if (n <= 999) then
                  idec = int(n/10)
                  imod = mod(n,10)
                  if (imod == 0) imod = 10
                  write(chrval,'(I2,A3)') idec, rank(imod)(3:5)
               end if

               if (pm25ave) then
                  write(iount,9091) chrval, chrave(1), numyrs
               else if (no2ave .or. so2ave) then
                  write(iount,99091) chrval, chrave(1), numyrs
               end if
               write(iount,9011) chidep(3,ityp), pollut, outlbl(ityp)
               write(iount,9022) chidep(1,ityp), chidep(2,ityp),&
               &chidep(3,ityp)
            end if
            do ival = 1, nmxpm
               indmx = mxpmloc(ival,igrp,n)
               if (ival == 1 .and. indmx /= 0) then
                  write(iount,1012) grpid(igrp), rank(ival),&
                  &mxpmval(ival,igrp,n), axr(indmx), ayr(indmx),&
                  &azelev(indmx), azhill(indmx), azflag(indmx),&
                  &rectyp(indmx), netid(indmx)
               else if (ival == 1 .and. indmx == 0) then
                  axr1 = 0.0d0
                  ayr1 = 0.0d0
                  azelv1 = 0.0d0
                  azhil1 = 0.0d0
                  azflg1 = 0.0d0
                  write(iount,1014) grpid(igrp), rank(ival),&
                  &mxpmval(ival,igrp,n), axr1, ayr1, azelv1,&
                  &azhil1, azflg1
               else if (indmx == 0) then
                  axr1 = 0.0d0
                  ayr1 = 0.0d0
                  azelv1 = 0.0d0
                  azhil1 = 0.0d0
                  azflg1 = 0.0d0
                  write(iount,1015) rank(ival),&
                  &mxpmval(ival,igrp,n), axr1, ayr1, azelv1,&
                  &azhil1, azflg1
               else
                  write(iount,1013) rank(ival),&
                  &mxpmval(ival,igrp,n), axr(indmx), ayr(indmx),&
                  &azelev(indmx), azhill(indmx), azflag(indmx),&
                  &rectyp(indmx), netid(indmx)
               end if
            end do
         end do
      end do
!        WRITE Out Explanation of Receptor Types
      write(iount,9050)
   end if

1012 format(/a8,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')',2x,a2,2x,a8)
1013 format(8x,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')',2x,a2,2x,a8)
1014 format(/a8,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')')
1015 format(8x,a5,' HIGHEST VALUE IS',f14.5,' AT ',&
   &'(',2(f11.2,', '),2(f8.2,', '),f7.2,')')
9011 format(/36x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9091 format(/27x,'*** THE SUMMARY OF MAXIMUM ',a5,'-HIGHEST ',&
   &a5,' RESULTS AVERAGED OVER ',i3,' YEARS ***'/)
99091 format(/22x,'*** THE SUMMARY OF MAXIMUM ',a5,'-HIGHEST ',&
   &'MAX DAILY ',a5,' RESULTS AVERAGED OVER ',i3,' YEARS ***'/)
9022 format(109x,'NETWORK',/'GROUP ID',23x,3a4,&
   &16x,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',2x,'OF TYPE',&
   &2x,'GRID-ID',/60('- '))
9050 format(//' *** RECEPTOR TYPES:  GC = GRIDCART',&
   &/22x,'GP = GRIDPOLR',&
   &/22x,'DC = DISCCART',&
   &/22x,'DP = DISCPOLR')

   return
end subroutine prtpm25sum

subroutine shout
!***********************************************************************
!                 SHOUT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Files of Season/Hour Results
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    June 5, 1997
!
!        MODIFIED:   Adjusted the header format variable (HDFRM) for
!                    cases with multiple output types.
!                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!                    Increased length of HDRFRM variable to avoid
!                    potential problems with portability of code.
!                    R. Brode, MACTEC/PES, 10/17/2005
!
!        INPUTS:  Array of Season/Hour Values
!
!        OUTPUTS: File of Season/Hour Values
!
!        CALLED FROM:   OUTPUT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

! Unused:      INTEGER :: I
   character :: hdrfrm*400, seafrm*80

!     Variable Initializations
   modnam = 'SHOUT'

!     Create Header Format for Columns Based on Number of Output Types
   write(hdrfrm,9020) numtyp, numtyp+2

! --- Generate ouptut format based on number of output types and
!     file format ('FIX' or 'EXP')
   if (file_format == 'FIX') then
!        Use FIXed format (F13.8) for concentrations and fluxes
      write(seafrm,1009) numtyp
1009  format('(2(1X,F13.5),',i1,'(1X,F13.8),3(1X,F7.2),2X,A8,2X,',&
      &'3(I4,2X),A8)')
   else if (file_format == 'EXP') then
!        Use EXPonential format (E13.6) for concentrations and fluxes
      write(seafrm,1010) numtyp
1010  format('(2(1X,F13.5),',i1,'(1X,E13.6),3(1X,F7.2),2X,A8,2X,',&
      &'3(I4,2X),A8)')
   end if

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Check for Selection of SEASONHR File for This Group
      if (iseahr(igrp) == 1) then
         if (.not. L_NoHeader(4)) then
!              Write Header Information
            write(ishunt(igrp),9005) versn, title1(1:68), rundat
            write(ishunt(igrp),9007) c_metver, runtim,&
            &MODOPS_String(1:len_trim(MODOPS_String))
            write(ishunt(igrp),9010) grpid(igrp), numrec, seafrm
            write(ishunt(igrp),hdrfrm)(chidep(1,ityp),chidep(2,ityp),&
            &chidep(3,ityp),ityp=1,numtyp)
         end if
! ---       Write data
         do iseas = 1, 4
            do ihour = 1, 24
!                 Begin Receptor LOOP
               do irec = 1, numrec
                  inum = nseahr(iseas,ihour) - nseacm(iseas,ihour)
                  write(ishunt(igrp),seafrm,err=99)&
                  &axr(irec), ayr(irec),&
                  &(shvals(irec,igrp,iseas,ihour,ityp),ityp=1,numtyp),&
                  &azelev(irec), azhill(irec), azflag(irec),&
                  &grpid(igrp), inum, iseas, ihour, netid(irec)
               end do
!                 End Receptor LOOP
            end do
         end do
      end if
   end do
!     End Source Group LOOP

   go to 999

!     WRITE Error Message for Problem Writing to Plot File
99 write(dummy,'("SHFIL",I3.3)') ishunt(igrp)
   call errhdl(path,modnam,'E','520',dummy)

9005 format('* AERMOD (',a6,'): ',a68,5x,a8)
9007 format('* AERMET (',a6,'):',t93,a8,&
   &/'* MODELING OPTIONS USED: ',a:)
9010 format('*',9x,'FILE OF SEASON/HOUR VALUES FOR ',&
   &'SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ', i5,' RECEPTORS.',&
   &/'*',9x,'FORMAT: ',a:)
9020 format('(''*'',8X,''X'',13X,''Y'',4X,',i1,'(2X,3A4),3X,''ZELEV'',&
   &  3X,''ZHILL'',3X,''ZFLAG'',4X,''GRP'',5X,''NHRS'',2X,''SEAS'',&
   &  2X,''HOUR'',3X,''NET ID'',/,''*'',',i1,'(1X,''____________ ''),&
   &  3('' ______ ''),'' ________  ____  ____  ____  ________'')')

999 return
end subroutine shout

subroutine rankfl
!***********************************************************************
!                 RANKFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Files To RANK
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Increased output field width for Rank to allow
!                    for up to 6 digits.
!                    R. Brode, US EPA, OAQPS, AQMG, 10/19/2009
!
!        INPUTS:  Array of High Values
!
!        OUTPUTS: File of High Values for Plotting
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, irank, idatsv(nmxval)
   logical :: found

!     Variable Initializations
   modnam = 'RANKFL'

!     Begin Averaging Period LOOP
   do iave = 1, numave
!        Decide if we should go through the processing
      if (irnkfl(iave) == 1) then
         if (.not. L_NoHeader(5)) then
!              Write Header to File
            write(irkunt(iave),9005) versn, title1(1:68), rundat
            write(irkunt(iave),9007) c_metver, runtim,&
            &MODOPS_String(1:len_trim(MODOPS_String))
            write(irkunt(iave),9010) irkval(iave), chrave(iave),&
            &numgrp, rnkfrm
            write(irkunt(iave),9020) chidep(1,ityp), chidep(2,ityp),&
            &chidep(3,ityp)
         end if

!           Begin Source Group LOOP
         do igrp = 1, numgrp
!              Initialize IDATSV array, which saves dates of ranked values
            idatsv(:) = 0
            irank = 0
!              Begin LOOP Through Max Values
            do i = 1, irkval(iave)
               found = .false.
               do j = 1, nmxval
                  if (mxdate(i,igrp,iave,ityp) == idatsv(j)) then
                     found = .true.
                     exit
                  end if
               end do
               if (.not.found .and. irank<irkval(iave)) then
                  irank = irank + 1
                  irec  = mxloca(i,igrp,iave,ityp)
                  idatsv(irank) = mxdate(i,igrp,iave,ityp)
                  write(irkunt(iave),rnkfrm,err=99) irank,&
                  &rmxval(i,igrp,iave,ityp), mxdate(i,igrp,iave,ityp),&
                  &axr(irec), ayr(irec), azelev(irec), azhill(irec),&
                  &azflag(irec), grpid(igrp)
               end if
            end do
!              End LOOP Through Max Values
         end do
!           End Source Group LOOP
      end if
   end do
!     End Averaging Period LOOP

   go to 999

!     WRITE Error Message for Problem Writing to RANK File
99 write(dummy,'("RNKFL",I3.3)') irkunt(iave)
   call errhdl(path,modnam,'E','520',dummy)

9005 format('* AERMOD (',a6,'): ',a68,5x,a8)
9007 format('* AERMET (',a6,'):',t93,a8,&
   &/'* MODELING OPTIONS USED: ',a:)
9010 format('*',9x,'RANK-FILE OF UP TO ',i5,' TOP ',a5,' VALUES ',&
   &'FOR ',i5,' SOURCE GROUPS',&
   &/'*',9x,'INCLUDES OVERALL MAXIMUM VALUES WITH DUPLICATE ',&
   &'DATA PERIODS REMOVED',&
   &/'*',9x,'FORMAT: ',a:)
9020 format('*  RANK',2x,3a4,3x,'DATE',10x,'X',13x,'Y',8x,'ZELEV',&
   &3x,'ZHILL',3x,'ZFLAG',5x,'GRP',&
   &/'*_______',1x,'____________',1x,'________',&
   &2(2x,'____________'),3(2x,'______'),'  ________')

999 return
end subroutine rankfl

subroutine maxdcalc
!***********************************************************************
!                 MAXDCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Recalculates results based on maximmum daily values
!                 (24-hour for PM2.5 and 1-hour averages for NO2 and SO2
!                 NAAQS) to perform source group contribution analyses
!                 under the OU MAXDCONT option.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED:   To add the Aircraft Engine Parameter in VOLUME/AREA
!                    Source for Aircraft source group.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Incorporated non-DFAULT GRSM option for NO2.
!                    CERC, 11/30/20
!
!        MODIFIED:   Incorporated non-DFAULT/BETA ARM2 option for NO2
!                    Mark Podrez, RTP Environmental
!                    R. Brode, US EPA, OAQPS, AQMG, August 15, 2013
!
!        MODIFIED:   Check for whether the HOURLY emissions option is used
!                    for specific sources to correct a bug in the MAXDCONT
!                    option for applications that include hourly emissions
!                    (HOUREMIS) for at least one source, but not all sources.
!                    R. Brode, US EPA, OAQPS, AQMG, 02/29/2012
!
!        INPUTS:  MAXDAILY file options
!
!        OUTPUTS:
!
!        CALLED FROM:   MAXDCONT_LOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none

   character :: modnam*12
! Unused:      CHARACTER (LEN=6) :: SPEC1, SPEC2, SPEC3
! Unused:      CHARACTER (LEN=8) :: CUSI, CSSI, COSI
! Unused:      INTEGER :: METVER, IOSI, ISSI, IUSI
!     JAT D065, 8/9/21 FOPEN SET BUT NOT USED
!     ILSAVE IS SET BUT NEVER USED
!      LOGICAL :: FOPEN
   integer :: ilsave
! Unused:      DOUBLE PRECISION :: RDUM
! Unused:      DOUBLE PRECISION :: O3VALUES(24), O3MIN, O3MAX24
! Unused:       INTEGER :: I
!***** ARM2 Modification ******
! Unused:      INTEGER :: Itempp
!******************************

!     Variable Initializations
   modnam = 'MAXDCALC'
   eof   = .false.
!     JAT D065, 8/9/21 FOPEN SET BUT NOT USED
!      FOPEN = .FALSE.
! --- Reset RSTSAV to false for MAXDCALC
   rstsav   = .false.

! --- Set IAVE = 1
   iave = 1

! --- Extract met data from arrays
   call maxd_metext

!     Save ILINE as ILSAVE and Initialize ILINE
!     JAT D065, 8/9/21 ILSAVE IS SET BUT NEVER USED
!      ILSAVE = ILINE

   if (hourly) then
!        Process Hourly Emissions from File
!        Begin Source Loop
      do isrc = 1, numsrc
! ---       Check for HOURLY emissions for this source
         if (qflag(isrc) == 'HOURLY') then
! ---          Retrieve hourly emissions for MAXDCONT option
            aqs(isrc) = aaqs(ihr_ndx,iyr_ndx,isrc)

            if (srctyp(isrc)(1:5) == 'POINT') then
               ats(isrc) =  aats(ihr_ndx,iyr_ndx,isrc)
               avs(isrc) =  aavs(ihr_ndx,iyr_ndx,isrc)
!**  Added for Aircraft Plume Rise; UNC-IE
            else if (srctyp(isrc) == 'VOLUME' .and.&
            &aftsrc(isrc) == 'Y') then
               amfuel(isrc) = aamfuel(ihr_ndx,iyr_ndx,isrc)
               athrust(isrc) = aathrust(ihr_ndx,iyr_ndx,isrc)
               avaa(isrc) = aavaa(ihr_ndx,iyr_ndx,isrc)
               aafr(isrc) = aaafr(ihr_ndx,iyr_ndx,isrc)
               abypr(isrc) = aabypr(ihr_ndx,iyr_ndx,isrc)
               asrcangle(isrc) = aasrcangle(ihr_ndx,iyr_ndx,isrc)
               arpwr(isrc) = aarpwr(ihr_ndx,iyr_ndx,isrc)
            else if (srctyp(isrc) == 'VOLUME' .and.&
            &l_hrlysig(isrc).and.&
            &aftsrc(isrc) == 'Y') then
               ahs(isrc)    =  aahs(ihr_ndx,iyr_ndx,isrc)
               asyini(isrc) =  aasyini(ihr_ndx,iyr_ndx,isrc)
               aszini(isrc) =  aaszini(ihr_ndx,iyr_ndx,isrc)
               amfuel(isrc) = aamfuel(ihr_ndx,iyr_ndx,isrc)
               athrust(isrc) = aathrust(ihr_ndx,iyr_ndx,isrc)
               avaa(isrc) = aavaa(ihr_ndx,iyr_ndx,isrc)
               aafr(isrc) = aaafr(ihr_ndx,iyr_ndx,isrc)
               abypr(isrc) = aabypr(ihr_ndx,iyr_ndx,isrc)
               asrcangle(isrc) = aasrcangle(ihr_ndx,iyr_ndx,isrc)
               arpwr(isrc) = aarpwr(ihr_ndx,iyr_ndx,isrc)
!**  End Aircarft Plume Rise insert; April 2023
            else if (srctyp(isrc) == 'VOLUME' .and.&
            &aftsrc(isrc) == 'N'     .and.&   ! Added for Aircraft; UNC-IE
            &l_hrlysig(isrc)) then
               ahs(isrc)    =  aahs(ihr_ndx,iyr_ndx,isrc)
               asyini(isrc) =  aasyini(ihr_ndx,iyr_ndx,isrc)
               aszini(isrc) =  aaszini(ihr_ndx,iyr_ndx,isrc)
!**  Added for Aircraft Plume Rise; UNC-IE
            else if (srctyp(isrc)(1:4) == 'AREA' .and.&
            &aftsrc(isrc) == 'Y') then
               amfuel(isrc) = aamfuel(ihr_ndx,iyr_ndx,isrc)
               athrust(isrc) = aathrust(ihr_ndx,iyr_ndx,isrc)
               avaa(isrc) = aavaa(ihr_ndx,iyr_ndx,isrc)
               aafr(isrc) = aaafr(ihr_ndx,iyr_ndx,isrc)
               abypr(isrc) = aabypr(ihr_ndx,iyr_ndx,isrc)
               asrcangle(isrc) = aasrcangle(ihr_ndx,iyr_ndx,isrc)
               arpwr(isrc) = aarpwr(ihr_ndx,iyr_ndx,isrc)
            else if (srctyp(isrc)(1:4) == 'AREA' .and.&
            &l_hrlysig(isrc).and.&
            &aftsrc(isrc) == 'Y') then
               ahs(isrc)    =  aahs(ihr_ndx,iyr_ndx,isrc)
               aszini(isrc) =  aaszini(ihr_ndx,iyr_ndx,isrc)
               amfuel(isrc) = aamfuel(ihr_ndx,iyr_ndx,isrc)
               athrust(isrc) = aathrust(ihr_ndx,iyr_ndx,isrc)
               avaa(isrc) = aavaa(ihr_ndx,iyr_ndx,isrc)
               aafr(isrc) = aaafr(ihr_ndx,iyr_ndx,isrc)
               abypr(isrc) = aabypr(ihr_ndx,iyr_ndx,isrc)
               asrcangle(isrc) = aasrcangle(ihr_ndx,iyr_ndx,isrc)
               arpwr(isrc) = aarpwr(ihr_ndx,iyr_ndx,isrc)
!**  End Aircraft Plume Rise insert; April 2023
            else if (srctyp(isrc)(1:4) == 'AREA' .and.&
            &aftsrc(isrc) == 'N'     .and.&  ! Added for Aircraft; UNC-IE
            &l_hrlysig(isrc)) then
               ahs(isrc)    =  aahs(ihr_ndx,iyr_ndx,isrc)
               aszini(isrc) =  aaszini(ihr_ndx,iyr_ndx,isrc)

            else if (srctyp(isrc) == 'LINE' .and.&
            &l_hrlysig(isrc)) then
               ahs(isrc)    =  aahs(ihr_ndx,iyr_ndx,isrc)
               aszini(isrc) =  aaszini(ihr_ndx,iyr_ndx,isrc)
            else if (srctyp(isrc) == 'BUOYLINE') then
               afp(isrc) = aafp(ihr_ndx,iyr_ndx,isrc)
            end if
         end if
      end do
!*       End Source Loop
   end if
!*----
!     Save ILINE as ILSAVE and Initialize ILINE
!     JAT D065, 8/9/21 ILSAVE IS SET BUT NEVER USED
!      ILSAVE = ILINE

   if (l_backgrnd) then
! ---    Assign BACKGRND concentration based on the hour and
!        year index; this value accounts for HOURLY BACKGRND
!        with or without substitution based on other temporally-
!        varying BACKGRND values, or is based soley on the
!        user-specified temporally-varying BACKGRND
      bgconc = abgconc(ihr_ndx,iyr_ndx)
   else
      bgconc = 0.0d0
   end if

   if (pvmrm .or. olm .or. runttrm .or. grsm) then
! ---    Assign background Ozone concentration based on the
!        hour and year index; this value accounts for hourly O3
!        with or without substitution based on other temporally-
!        varying O3 values, or is based soley on the
!        user-specified annual or temporally-varying O3
      o3conc = ao3conc(ihr_ndx,iyr_ndx)
!---     Set O3MISS = .T. if stored O3CONC is less than 0.
      if (o3conc < 0.0d0) then
         o3miss = .true.
      else
         o3miss = .false.
      end if
   end if

!     CERC 11/30/20
   if (grsm) then
! ---    Assign background NOx concentration based on the
!        hour and year index; this value accounts for hourly NOx
!        with or without substitution based on other temporally-
!        varying NOx values, or is based soley on the
!        user-specified annual or temporally-varying NOx
      noxbgconc = anoxbgconc(ihr_ndx,iyr_ndx)
!---     Set NOXMISS = .T. if stored NOXBGCONC is less than 0.
      if (noxbgconc < 0.0d0) then
         noxmiss = .true.
      else
         noxmiss = .false.
      end if
   end if

   if (fulldate>=isdate .and. fulldate<=iedate) then

      if (clmhr .and. clmpro) then
!           Check for Calm Hr & Processing and Increment Counters
         numhrs(1) = numhrs(1) + 1
         numclm(1) = numclm(1) + 1
      else if (msghr .and. msgpro) then
!           Check for Missing Hour & Processing and Increment Counters
         numhrs(1) = numhrs(1) + 1
         nummsg(1) = nummsg(1) + 1
      else if (zi <= 0.0d0) then
!           Write Out The Informational Message & Increment Counters
         numhrs(1) = numhrs(1) + 1
      else
!           Set CALCS Flag, Increment Counters & Calculate HRVAL
         calcs = .true.
         numhrs(1) = numhrs(1) + 1

!           Calculate CONC or DEPOS Values               ---   CALL CALC
         call calc
      end if

      if (.not.clmhr .and. .not.msghr) then
! ---       Non-calm, non-missing hour; apply NO2 options as appropriate

!! Added for TTRM2
!! If TTRM2 (TTRM with the compare option) is requested then
!! perform TTRM >> FIRST <<
         if (runttrm2) then
! ---             Process Hourly Values for TTRM Option
            cmeth = 3
            call ttrm_calc
! ---          Flush HRVAL Arrays (1:NUMTYP)
            hrval(:)   = 0.0d0
            if (pvmrm .and. .not. psdcredit) then
               call pvmrm_calc('ALLSRCS')
            else if (olm) then
               call olm_calc
            else if (arm2) then
               call arm2_calc
            end if
            go to 8857
         end if
         if (pvmrm .and. .not.psdcredit) then
! ---          Process Hourly Values for PVMRM Option
            call pvmrm_calc('ALLSRCS')

         else if (pvmrm .and. psdcredit) then
! ---          Process Hourly Values for PVMRM Option and PSD credits
! ---          Need to process two separate sets of sources - the
!              increment consumption sources ('NAAQSRC') and the
!              increment expanding sources ('ALLBASE')
            call pvmrm_calc('NAAQSRC')
            call pvmrm_calc('ALLBASE')

         else if (olm) then
! ---          Process Hourly Values for OLM Option
            call olm_calc
!
!   Added for TTRM; AECOM
         else if (runttrm) then
! ---          Process Hourly Values for TTRM Option
            if (.not. runttrm2) then
               call ttrm_calc
            end if
!   End TTRM insert; Feb. 2021; Modified Nov. 2021

         else if (arm2) then
! ---          Process Hourly Values for ARM2 Option
            call arm2_calc

         else if (grsm) then
! ---          CERC 11/30/20 Process Hourly Values for GRSM Option
            call grsm_calc

         end if
8857     continue
      end if

      iave = 1
!        Check for End of Averaging Period
      if (kave(1)/=1 .and. mod(ihour,kave(1))==0) then
!           Calculate Applicable Averages          ---   CALL AVER
         call aver
! ---       Reinitialize NUMHRS, NUMCLM and NUMMSG
         numhrs(:) = 0
         numclm(:) = 0
         nummsg(:) = 0
      end if

!        Flush HRVAL Arrays (1:NUMTYP)
      hrval   = 0.0d0
      aerval  = 0.0d0
      prmval  = 0.0d0

      if (pvmrm .or. olm .or. arm2&
      &.or. runttrm .or. grsm) then
!           Flush CHI(NUMREC,NUMSRC,NUMTYP) Array
         chi(:,:,:) = 0.0d0
         if(runttrm2)then
            ttrmcompare(:,:,:,:) = 0.0d0
         endif
         if(grsm)then
            chi_ttravplm = 0.0d0
            chi_ttravpan = 0.0d0
            chi_ttravaer = 0.0d0
            chi_ttravprm = 0.0d0
            chi_ttravchm(:,:) = 0.0d0
            bldfac(:,:) = 0.0d0
            PRMVAL_Src1 = 0.0d0
         end if
         if (psdcredit) then
!              Flush ABVAL(NUMREC,NUMTYP) and BCVAL(NUMREC,NUMTYP) Arrays
            abval(:,:) = 0.0d0
            bcval(:,:) = 0.0d0
         end if
      end if

   end if

!     Reset CALCS and ENDMON Flags
   calcs  = .false.
   endmon = .false.

!     Save precipitation rates for two previous hours
   prec2 = prec1
   prec1 = Prate

   return
end subroutine maxdcalc

subroutine maxdcnt_file(igrp1,ival)
!***********************************************************************
!                 MAXDCONT_FILE Module of AERMOD
!
!        PURPOSE: Process MAXDCONT option for source group contributions
!                 to maximum daily 1-hour values for NO2 or SO2, or
!                 maximum daily (24-hour) values for PM2.5, averaged
!                 across years of met data processed.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  Array of High Values
!
!        OUTPUTS: File of High Values for Plotting
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ival, idec, imod, igrp1, igrp2
! JAT 06/22/21 D065 REMOVE NCHR1 AS UNUSED VARIABLE
!      CHARACTER NCHR1(10)*3, NCHR2(10)*5, CHRVAL*5, HDRFRM*400,
   character :: nchr2(10)*5, chrval*5, hdrfrm*400,&
   &mxdfmt*100
! Unused:       INTEGER :: J

!     Variable Initializations
! JAT 06/22/21 D065 REMOVE NCHR1 INITIALIZATION AS UNUSED VARIABLE
!      DATA (NCHR1(I),I=1,10) /'YR1','YR2','YR3','YR4','YR5',
!     &                        'YR6','YR7','YR8','YR9','Y10'/
   data (nchr2(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'MAXDCNT_FILE'

!     Create Header Format for Columns
   write(hdrfrm,9019) numgrp, numgrp

! --- Generate write format, depending on FILE_FORMAT
   if (file_format == 'FIX') then
      write(mxdfmt,9001) min( numgrp, 9999 )
   else if (file_format == 'EXP') then
      write(mxdfmt,9002) min( numgrp, 9999 )
   end if

! --- Assign character label for rank
   if (ival <= 10) then
      chrval = nchr2(ival)
   else if (mod(ival,100) > 10 .and.&
   &mod(ival,100) < 20) then
      idec = int(ival/10)
      imod = mod(ival,10)
      write(chrval,'(I2,I1,"TH")') idec, imod
   else if (ival <= 999) then
      idec = int(ival/10)
      imod = mod(ival,10)
      if (imod == 0) imod = 10
      write(chrval,'(I2,A3)') idec, nchr2(imod)(3:5)
   end if

   if (.not. L_NoHeader(8)) then
!        Write Header Information
      write(imxdcunt(igrp1),9005) versn, title1(1:68), rundat
      write(imxdcunt(igrp1),9007) c_metver, runtim,&
      &MODOPS_String(1:len_trim(MODOPS_String))
!
      if (pm25ave) then
         if (maxd_thresh(igrp1) > 0.0d0) then
            write(imxdcunt(igrp1),9008) chrval,chrave(1),&
            &numyrs, grpid(igrp1), maxd_thresh(igrp1),&
            &nrec, numgrp, mxdfmt
         else
            write(imxdcunt(igrp1),9108) chrval,chrave(1),&
            &numyrs, grpid(igrp1), nrec, numgrp, mxdfmt
         end if
      else if (no2ave .or. so2ave) then
         if (maxd_thresh(igrp1) > 0.0d0) then
            write(imxdcunt(igrp1),9009) chrval,chrave(1),&
            &numyrs, grpid(igrp1), maxd_thresh(igrp1),&
            &nrec, numgrp, mxdfmt
         else
            write(imxdcunt(igrp1),9109) chrval,chrave(1),&
            &numyrs, grpid(igrp1), nrec, numgrp, mxdfmt
         end if
      end if
      write(imxdcunt(igrp1),hdrfrm) chidep(1,1),&
      &chidep(2,1), chidep(3,1),&
      &(grpid(i),i=1,numgrp)
   end if

! --- Begin Receptor LOOP
   do irec = 1, nrec
! ---    Write results for specified source group with contributions
!        from other source groups, paired in time and space
      write(imxdcunt(igrp1),mxdfmt,err=99)&
      &axr_sav(irec), ayr_sav(irec),&
      &sumhnh(irec,igrp1,ival), azelev_sav(irec),&
      &azhill_sav(irec), azflag_sav(irec),&
      &chrave(1), grpid(igrp1), chrval, netid(irec),&
      &(sumval_maxd(ival,igrp2,igrp1,irec),&
      &igrp2=1,min(numgrp,9999))
   end do
! --- End Receptor LOOP

   go to 999

!     WRITE Error Message for Problem Writing to Plot File
99 write(dummy,'("MXDCN",I3.3)') imxdcunt(igrp1)
   call errhdl(path,modnam,'E','520',dummy)

! --- Generate write formats for FIX and EXP FILE_FORMATs:
9001 format('(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,',i4,&
   &'(F13.5,2X:))')
9002 format('(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,'&
   &,'2X,',i4,'(E13.6,2X:))')

9005 format('* AERMOD (',a6,'): ',a68,5x,a8)
9007 format('* AERMET (',a6,'):',t93,a8,&
   &/'* MODELING OPTIONS USED: ',a:)
9008 format('*',9x,'MAXDCONT FILE OF ',a5,'-HIGHEST ',a5,&
   &' VALUES AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP: ',a8,&
   &' ; ABOVE THRESH = ',f13.5,&
   &/'*',9x,'FOR A TOTAL OF ',i6,' RECEPTORS AND ',i4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9x,'FORMAT: ',a:)
9108 format('*',9x,'MAXDCONT FILE OF ',a5,'-HIGHEST ',a5,&
   &' VALUES AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ',i6,' RECEPTORS AND ',i4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9x,'FORMAT: ',a:)
9009 format('*',9x,'MAXDCONT FILE OF ',a5,'-HIGHEST MAX DAILY ',a5,&
   &' VALUES AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP: ',a8,&
   &' ; ABOVE THRESH = ',f13.5,&
   &/'*',9x,'FOR A TOTAL OF ',i6,' RECEPTORS AND ',i4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9x,'FORMAT: ',a:)
9109 format('*',9x,'MAXDCONT FILE OF ',a5,'-HIGHEST MAX DAILY ',a5,&
   &' VALUES AVERAGED OVER ',i3,' YEARS FOR SOURCE GROUP: ',a8,&
   &/'*',9x,'FOR A TOTAL OF ',i6,' RECEPTORS AND ',i4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9x,'FORMAT: ',a:)
9019 format('(''*'',8X,''X'',13X,''Y'',4X,(2X,3A4),4X,''ZELEV'',4X,&
   &''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,&
   &''NET ID'',1X,',i4,'(2X,''CONT '',A8),/,''*'',&
   &3(1X,''____________ ''),1X,3('' ______  ''),&
   &''______  ________  ________  ________'',',&
   &i4,'(''  _____________''))')

999 continue

! --- Close and reopen file to flush data from file buffer
   close(unit=imxdcunt(igrp1))
   open(imxdcunt(igrp1),err=99,file=maxdcont_file(igrp1),&
   &iostat=ioerrn,form='FORMATTED',status='OLD',&
   &position='APPEND')

   return
end subroutine maxdcnt_file

