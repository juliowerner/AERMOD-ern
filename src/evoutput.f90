subroutine averev
!***********************************************************************
!                 AVEREV Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Sums Values and Calculates Averages
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Averaging Time Option Switches
!                 Array of CONC or DEPOS Values for One Hour, HRVALS
!
!        OUTPUTS: Updated Array of Cumulative Values and Averages, AVEVAL
!
!        CALLED FROM:   EVLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
!     JAT 7/22/21 D065 BCKGRD NOT USED
!      DOUBLE PRECISION :: SNUM, BCKGRD
   double precision :: snum

!     Variable Initializations
   modnam = 'AVEREV'
   snum   = 0.0d0
!     JAT 7/22/21 D065 BCKGRD NOT USED
!      BCKGRD = 0.0D0

!     Calculate Average CONCentrations If Hour is Right
   if (conc) then
      if (evaper(ievent) /= 1) then
!           Calculate Denominator Considering Calms and Missing,
!           Skipping Averaging if Averaging Period is 1-Hour
         snum = max(dble(ev_numhrs - ev_numclm - ev_nummsg),&
         &dnint(dble(ev_numhrs)*0.75d0+0.4d0))
!           Begin Source Group LOOP
         do isrc = 1, numsrc
            if (igroup(isrc,idxev(ievent)) == 1) then
               ev_aveval(isrc) = ev_aveval(isrc) / snum
            endif
         end do
!           End Source Group LOOP
         grpave(idxev(ievent)) = grpave(idxev(ievent))/snum

         if (grp_back(idxev(ievent))) then
            backave(idxev(ievent)) = backave(idxev(ievent))/snum
         end if

         if (olmdebug) then
! ---          Include averages for multi-hour events in OLM debug file
            write(olmdbg,99871) kurdat, ievent, evname(ievent),&
            &evaper(ievent), grpid(idxev(ievent)),&
            &evaper(ievent), grpave(idxev(ievent))
99871       format(1x,i8.8,i6,2x,a10,1x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',122x,e12.5)
         else if (arm2debug) then
! ---          Include averages for multi-hour events in OLM debug file
            write(arm2dbg,99873) kurdat, ievent, evname(ievent),&
            &evaper(ievent), grpid(idxev(ievent)),&
            &evaper(ievent), grpave(idxev(ievent))
99873       format(1x,i8.8,i6,2x,a10,1x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',55x,e12.5)
         else if (pvmrmdbg) then
! ---          Include averages for multi-hour events in PVMRM debug file
            write(pvmdbg,99874) kurdat, ievent, evname(ievent),&
            &evaper(ievent), grpid(idxev(ievent)),&
            &evaper(ievent), grpave(idxev(ievent))
99874       format(21x,i8.8,i6,4x,a10,2x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',175x,e12.5)
         else if (grsmdebug) then
! ---          Include averages for multi-hour events in GRSM debug file
            write(grsmdbg,99875) kurdat, ievent, evname(ievent),&
            &evaper(ievent), grpid(idxev(ievent)),&
            &evaper(ievent), grpave(idxev(ievent))
99875       format(21x,i8.8,i6,4x,a10,2x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',175x,e12.5)
         end if
      end if
   end if

   return
end subroutine averev

subroutine ev_output
!***********************************************************************
!                 EV_OUTPUT Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Controls Output of Printed Model Results
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
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
   modnam = 'EV_OUTPUT'

   if (socont) then
!        Print Out Source Contribution To the Event         ---   CALL PRTSOC
      call prtsoc
   else if (detail) then
!        Print Out Detal Summary of The Event               ---   CALL PRTDET
      call prtdet
   end if

   return
end subroutine ev_output

subroutine prtsoc
!***********************************************************************
!                 PRTSOC Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Print Out The Source Contribution Data
!                 To The Event
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To correct overflow on format statement 9068, and
!                    to use separate array for source IDs in the header
!                    (HEADID) - 9/29/92
!
!        INPUTS:  Arrays of Source Parameters
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

   integer :: i, n, nrow, npage, indexw, iogrp
   double precision :: wavev(nsrc+1)
   character (len=12) :: headid(nsrc+1)

!     Variable Initializations
   modnam = 'PRTSOC'
   headid(:) = ''
   wavev(:)  = 0.0d0

!     Set Up The Print Array
!     Fill Work Array With SRCIDs For This Group
   indgrp = 0
   do isrc = 1, numsrc
      if (igroup(isrc,idxev(ievent)) == 1) then
         indgrp = indgrp + 1
         workid(indgrp) = srcid(isrc)
         headid(indgrp) = srcid(isrc)
         wavev(indgrp)  = ev_aveval(isrc)
      end if
   end do
! --- Check for BACKGROUND "source" being included
!     in source group
   if (grp_back(idxev(ievent))) then
      indgrp = indgrp + 1
      workid(indgrp) = 'BACKGROUND'
      headid(indgrp) = 'BACKGROUND'
      wavev(indgrp)  = backave(idxev(ievent))
!        Check for More Than 31 Sources Per Group
      indexw = min(31,nsrc+1)
   else
      indexw = min(31,nsrc)
   end if
!     Check for More Than 31 Sources Per Group
   if (indgrp > indexw) then
      headid(indexw) = ' . . . '
      iogrp = 31
   else
      iogrp = indgrp
   end if

!     Determine Number of Rows, NROW, @ 3 Values Per Row
   nrow = 1 + int((indgrp-1)/3)
!     Determine Number of Pages, NPAGE, @ 40 Rows Per Page
   npage = 1 + int((nrow-1)/40)

!     Loop Through Pages For This Event
   do n = 1, npage

!        Print The Source Contributions
      call header(iounit)
      write(iounit,9058) evname(ievent), evaper(ievent),&
      &evdate(ievent), axr(ievent), ayr(ievent),&
      &azelev(ievent), azflag(ievent)

      write(iounit,9068) evgrp(ievent), (headid(i),i=1,iogrp)
      write(iounit,9070) grpave(idxev(ievent))
      write(iounit,9062)

!        Print Out The Source Contributions
      if (n == npage) then
         if (file_format == 'FIX') then
            write(iounit,9066) (workid(i),wavev(i),i=1+120*(n-1),&
            &indgrp)
         else if (file_format == 'EXP') then
            write(iounit,9067) (workid(i),wavev(i),i=1+120*(n-1),&
            &indgrp)
         end if
      else
         if (file_format == 'FIX') then
            write(iounit,9066) (workid(i),wavev(i),i=1+120*(n-1),&
            &120*n)
         else if (file_format == 'EXP') then
            write(iounit,9067) (workid(i),wavev(i),i=1+120*(n-1),&
            &120*n)
         end if
      end if

   end do

9058 format(42x,'*** SOURCE CONTRIBUTIONS FOR EVENT: ',&
   &a10,' ***'//1x,'---> AVE. PER.: ',i3,' HRS;',&
   &'  END DATE:  ',i8.8,';  LOCATION (XR,YR,ZELEV,ZFLAG):',&
   &4f11.2,' (M)'/)
9068 format(1x,'GROUP ID: ',a8,1x,'OF SOURCES: ',7(a12,', ')/&
   &18x,8(a12,', ')/18x,8(a12,', ')/18x,8(a12,', '))
9070 format(/3x,'*** GROUP VALUE = ',f14.5,' ***'/)
9062 format(3('   SOURCE ID      CONTRIBUTION ',10x)/&
   &3(' -------------    ------------ ',10x))
9066 format(3(2x,a12,3x,f13.5,11x:))
9067 format(3(2x,a12,3x,e13.6,11x:))

   return
end subroutine prtsoc

subroutine prtdet
!***********************************************************************
!                 PRTDET Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Print Out The Source Contribution Data
!                 To The Event
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Arrays of Source Parameters
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

   integer :: i, j, n, nspp, npage, ihr, ingrp
   double precision :: wavev(nsrc+1), whrval(24,nsrc+1)

!     Variable Initializations
   modnam = 'PRTDET'
   wavev(:)    = 0.0d0
   whrval(:,:) = 0.0d0

!     Set Up The Printing Work Array
   ingrp = 0
   do isrc = 1, numsrc
      if (igroup(isrc,idxev(ievent)) == 1) then
         ingrp = ingrp + 1
         workid(ingrp) = srcid(isrc)
         wavev(ingrp)  = ev_aveval(isrc)
         do ihr = istahr, iendhr
            whrval(ihr,ingrp) = hrvals(ihr,isrc)
         end do
      end if
   end do

! --- Check for BACKGROUND "source" being included
!     in source group
   if (grp_back(idxev(ievent))) then
      ingrp = ingrp + 1
      workid(ingrp) = 'BACKGROUND'
      wavev(ingrp)  = backave(idxev(ievent))
      do ihr = istahr, iendhr
         whrval(ihr,ingrp) = backhr(idxev(ievent),ihr)
      end do
   end if

!     Set Number of Sources Per Page, NSPP
   nspp = 8
!     Calculate Number of Pages for This Event (NSPP Sources per Page)
   npage = 1 + int((ingrp-1)/nspp)
   do n = 1, npage
      call header(iounit)
      write(iounit,9058) evname(ievent), grpave(idxev(ievent)),&
      &evaper(ievent),evdate(ievent), axr(ievent),ayr(ievent),&
      &azelev(ievent),azflag(ievent)

      if (n == npage) then
!           Print Out The Values for the Last Page
         write(iounit,9068) evgrp(ievent), (workid(i),&
         &i=1+nspp*(n-1),ingrp)
         write(iounit,9066)

!           Print Out The Source Contributions for the Last Page
         do i = istahr, iendhr
            if (file_format == 'FIX') then
               write(iounit,9062) i,grpval(idxev(ievent),i),&
               &(whrval(i,j),j=1+nspp*(n-1),ingrp)
            else if (file_format == 'EXP') then
               write(iounit,9063) i,grpval(idxev(ievent),i),&
               &(whrval(i,j),j=1+nspp*(n-1),ingrp)
            end if
         end do
         if (file_format == 'FIX') then
            write(iounit,9064) grpave(idxev(ievent)),&
            &(wavev(i),i=1+nspp*(n-1),ingrp)
         else if (file_format == 'EXP') then
            write(iounit,9065) grpave(idxev(ievent)),&
            &(wavev(i),i=1+nspp*(n-1),ingrp)
         end if
      else
!           Print Out The Values for the Current Page
         write(iounit,9068) evgrp(ievent), (workid(i),&
         &i=1+nspp*(n-1),nspp*n)
         write(iounit,9066)

!           Print Out The Source Contributions for the Current Page
         do i = istahr, iendhr
            if (file_format == 'FIX') then
               write(iounit,9062) i,grpval(idxev(ievent),i),&
               &(whrval(i,j),j=1+nspp*(n-1),nspp*n)
            else if (file_format == 'EXP') then
               write(iounit,9063) i,grpval(idxev(ievent),i),&
               &(whrval(i,j),j=1+nspp*(n-1),nspp*n)
            end if
         end do
         if (file_format == 'FIX') then
            write(iounit,9064) grpave(idxev(ievent)),&
            &(wavev(i),i=1+nspp*(n-1),nspp*n)
         else if (file_format == 'EXP') then
            write(iounit,9065) grpave(idxev(ievent)),&
            &(wavev(i),i=1+nspp*(n-1),nspp*n)
         end if
      end if

      if (n == 1) then
!           Write Out the Meteorology Data
         newmet = .true.
         do ihour = istahr, iendhr
            uref   = auref(ihour,1)
            urefht = aurefht(ihour,1)
            ta     = ata(ihour,1)
            trefht = atrefht(ihour,1)
            wdref  = awdref(ihour,1)
            sfchf  = asfchf(ihour,1)
            ustar  = austar(ihour,1)
            wstar  = awstar(ihour,1)
            ziconv = aziconv(ihour,1)
            zimech = azimech(ihour,1)
            obulen = aobulen(ihour,1)
            vptgzi = avptgzi(ihour,1)
            sfcz0  = asfcz0(ihour,1)
            bowen  = abowen(ihour,1)
            albedo = aalbedo(ihour,1)
!              Write Out The Meteorology Data
            call metdet
         end do
      end if
   end do

9058 format(29x,'*** SOURCE CONTRIBUTIONS FOR EVENT: ',a10,&
   &4x,'GROUP VALUE = ',f14.5,' ***'/1x,'---> AVE. PER.: ',&
   &i3,' HRS;','  END DATE:  ',i8.8,&
   &';  LOCATION (XR,YR,ZELEV,ZFLAG):',4f11.2,' (M)')
9068 format(1x,'HR GROUP:',a8,' OF',1x,a12,7(2x,a12:))
9066 format(66('- '))
9062 format(1x,i2,1x,9(1x,f13.5:))
9063 format(1x,i2,1x,9(1x,e13.6:))
9064 format(66('- ')/1x,'AVE',9(1x,f13.5:))
9065 format(66('- ')/1x,'AVE',9(1x,e13.6:))

   return
end subroutine prtdet

subroutine metdet
!***********************************************************************
!                 METDET Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
!
!        PURPOSE: Print Out The Details Of The Meteorology Data
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Meteorology Input Data
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   PRTDET
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'METDET'

!     Meteorology Data Summary
   if (newmet) then
      newmet = .false.
      write(iounit,9025)
   end if
   write(iounit,9026) iyear, imonth, iday, ihour,&
   &sfchf, ustar, wstar, vptgzi, ziconv, zimech, obulen,&
   &sfcz0, bowen, albedo, uref, wdref, urefht, ta, trefht


9025 format (' MET DATA --> YR MO DY HR','     H0','     U*','     W*',&
   &'  DT/DZ',' ZICNV', ' ZIMCH','  M-O LEN','    Z0',&
   &'  BOWEN',' ALBEDO','  REF WS', '   WD', '     HT',&
   &'  REF TA', '     HT')
9026 format (14x,4(i2.2,1x),f6.1,1x,3(f6.3,1x),&
   &2(f5.0,1x),f8.1,1x,f5.2,1x,2(f6.2,1x),f7.2,1x,f5.0,&
   &3(1x,f6.1) )

   return
end subroutine metdet

subroutine ev_flush
!***********************************************************************
!                 Module EV_FLUSH of AERMOD Model - EVENT
!
!        PURPOSE: To Flush AVEVAL and HRVALS Array
!
!        PROGRAMMER: Todd Hawes, Roger Brode and Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  AVEVAL, HRVALS
!
!        OUTPUTS: Flushed AVEVAL and HRVALS
!
!        CALLED FROM:  MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

! Unused:      INTEGER :: I

!     Variable Initializations
   modnam = 'EV_FLUSH'

!     Flush the Hourly Value
   hrval(:) = 0.0d0

!     Flush arrays, if allocated
! Multiple_BuoyLines_D41_Wood
!     Removed allocation of PARTCH - now declared as a scaler
   if (allocated(chi))  chi(:,:,:) = 0.0d0
   if(grsm)then
      chi_ttravplm = 0.0d0
      chi_ttravpan = 0.0d0
      chi_ttravaer = 0.0d0
      chi_ttravprm = 0.0d0
      chi_ttravchm(:,:) = 0.0d0
      bldfac(:,:) = 0.0d0
      PRMVAL_Src1 = 0.0d0
   end if
   if (allocated(chibl)) chibl(:) = 0.0d0
   grpave(:)    = 0.0d0
   grpval(:,:)  = 0.0d0
   if (allocated(backave)) backave(:) = 0.0d0
   if (allocated(backhr))  backhr(:,:)= 0.0d0
   ev_aveval(:) = 0.0d0
   hrvals(:,:)  = 0.0d0

   return
end subroutine ev_flush
