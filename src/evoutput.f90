SUBROUTINE AVEREV
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
!     JAT 7/22/21 D065 BCKGRD NOT USED
!      DOUBLE PRECISION :: SNUM, BCKGRD
   DOUBLE PRECISION :: SNUM

!     Variable Initializations
   MODNAM = 'AVEREV'
   SNUM   = 0.0D0
!     JAT 7/22/21 D065 BCKGRD NOT USED
!      BCKGRD = 0.0D0

!     Calculate Average CONCentrations If Hour is Right
   IF (CONC) THEN
      IF (EVAPER(IEVENT) .NE. 1) THEN
!           Calculate Denominator Considering Calms and Missing,
!           Skipping Averaging if Averaging Period is 1-Hour
         SNUM = MAX(DBLE(EV_NUMHRS - EV_NUMCLM - EV_NUMMSG),&
         &DNINT(DBLE(EV_NUMHRS)*0.75D0+0.4D0))
!           Begin Source Group LOOP
         DO ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) THEN
               EV_AVEVAL(ISRC) = EV_AVEVAL(ISRC) / SNUM
            ENDIF
         END DO
!           End Source Group LOOP
         GRPAVE(IDXEV(IEVENT)) = GRPAVE(IDXEV(IEVENT))/SNUM

         IF (GRP_BACK(IDXEV(IEVENT))) THEN
            BACKAVE(IDXEV(IEVENT)) = BACKAVE(IDXEV(IEVENT))/SNUM
         END IF

         if (OLMDEBUG) then
! ---          Include averages for multi-hour events in OLM debug file
            WRITE(OLMDBG,99871) kurdat, ievent, EVNAME(IEVENT),&
            &EVAPER(IEVENT), grpid(IDXEV(IEVENT)),&
            &EVAPER(IEVENT), grpave(IDXEV(IEVENT))
99871       format(1x,i8.8,i6,2x,a10,1x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',122x,e12.5)
         else if (ARM2DEBUG) then
! ---          Include averages for multi-hour events in OLM debug file
            WRITE(ARM2DBG,99873) kurdat, ievent, EVNAME(IEVENT),&
            &EVAPER(IEVENT), grpid(IDXEV(IEVENT)),&
            &EVAPER(IEVENT), grpave(IDXEV(IEVENT))
99873       format(1x,i8.8,i6,2x,a10,1x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',55x,e12.5)
         else if (PVMRMDBG) then
! ---          Include averages for multi-hour events in PVMRM debug file
            WRITE(PVMDBG,99874) kurdat, ievent, EVNAME(IEVENT),&
            &EVAPER(IEVENT), grpid(IDXEV(IEVENT)),&
            &EVAPER(IEVENT), grpave(IDXEV(IEVENT))
99874       format(21x,i8.8,i6,4x,a10,2x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',175x,e12.5)
         else if (GRSMDEBUG) then
! ---          Include averages for multi-hour events in GRSM debug file
            WRITE(GRSMDBG,99875) kurdat, ievent, EVNAME(IEVENT),&
            &EVAPER(IEVENT), grpid(IDXEV(IEVENT)),&
            &EVAPER(IEVENT), grpave(IDXEV(IEVENT))
99875       format(21x,i8.8,i6,4x,a10,2x,i4,2x,a8,8x,i2.2,&
            &'HR_AVERAGE',175x,e12.5)
         end if
      END IF
   END IF

   RETURN
END

SUBROUTINE EV_OUTPUT
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'EV_OUTPUT'

   IF (SOCONT) THEN
!        Print Out Source Contribution To the Event         ---   CALL PRTSOC
      CALL PRTSOC
   ELSE IF (DETAIL) THEN
!        Print Out Detal Summary of The Event               ---   CALL PRTDET
      CALL PRTDET
   END IF

   RETURN
END

SUBROUTINE PRTSOC
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, N, NROW, NPAGE, INDEXW, IOGRP
   DOUBLE PRECISION :: WAVEV(NSRC+1)
   CHARACTER (LEN=12) :: HEADID(NSRC+1)

!     Variable Initializations
   MODNAM = 'PRTSOC'
   HEADID(:) = ''
   WAVEV(:)  = 0.0D0

!     Set Up The Print Array
!     Fill Work Array With SRCIDs For This Group
   INDGRP = 0
   DO ISRC = 1, NUMSRC
      IF (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) THEN
         INDGRP = INDGRP + 1
         WORKID(INDGRP) = SRCID(ISRC)
         HEADID(INDGRP) = SRCID(ISRC)
         WAVEV(INDGRP)  = EV_AVEVAL(ISRC)
      END IF
   END DO
! --- Check for BACKGROUND "source" being included
!     in source group
   IF (GRP_BACK(IDXEV(IEVENT))) THEN
      INDGRP = INDGRP + 1
      WORKID(INDGRP) = 'BACKGROUND'
      HEADID(INDGRP) = 'BACKGROUND'
      WAVEV(INDGRP)  = BACKAVE(IDXEV(IEVENT))
!        Check for More Than 31 Sources Per Group
      INDEXW = MIN(31,NSRC+1)
   ELSE
      INDEXW = MIN(31,NSRC)
   END IF
!     Check for More Than 31 Sources Per Group
   IF (INDGRP .GT. INDEXW) THEN
      HEADID(INDEXW) = ' . . . '
      IOGRP = 31
   ELSE
      IOGRP = INDGRP
   END IF

!     Determine Number of Rows, NROW, @ 3 Values Per Row
   NROW = 1 + INT((INDGRP-1)/3)
!     Determine Number of Pages, NPAGE, @ 40 Rows Per Page
   NPAGE = 1 + INT((NROW-1)/40)

!     Loop Through Pages For This Event
   DO N = 1, NPAGE

!        Print The Source Contributions
      CALL HEADER(IOUNIT)
      WRITE(IOUNIT,9058) EVNAME(IEVENT), EVAPER(IEVENT),&
      &EVDATE(IEVENT), AXR(IEVENT), AYR(IEVENT),&
      &AZELEV(IEVENT), AZFLAG(IEVENT)

      WRITE(IOUNIT,9068) EVGRP(IEVENT), (HEADID(I),I=1,IOGRP)
      WRITE(IOUNIT,9070) GRPAVE(IDXEV(IEVENT))
      WRITE(IOUNIT,9062)

!        Print Out The Source Contributions
      IF (N .EQ. NPAGE) THEN
         IF (FILE_FORMAT .EQ. 'FIX') THEN
            WRITE(IOUNIT,9066) (WORKID(I),WAVEV(I),I=1+120*(N-1),&
            &INDGRP)
         ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
            WRITE(IOUNIT,9067) (WORKID(I),WAVEV(I),I=1+120*(N-1),&
            &INDGRP)
         END IF
      ELSE
         IF (FILE_FORMAT .EQ. 'FIX') THEN
            WRITE(IOUNIT,9066) (WORKID(I),WAVEV(I),I=1+120*(N-1),&
            &120*N)
         ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
            WRITE(IOUNIT,9067) (WORKID(I),WAVEV(I),I=1+120*(N-1),&
            &120*N)
         END IF
      END IF

   END DO

9058 FORMAT(42X,'*** SOURCE CONTRIBUTIONS FOR EVENT: ',&
   &A10,' ***'//1X,'---> AVE. PER.: ',I3,' HRS;',&
   &'  END DATE:  ',I8.8,';  LOCATION (XR,YR,ZELEV,ZFLAG):',&
   &4F11.2,' (M)'/)
9068 FORMAT(1X,'GROUP ID: ',A8,1X,'OF SOURCES: ',7(A12,', ')/&
   &18x,8(A12,', ')/18x,8(A12,', ')/18x,8(A12,', '))
9070 FORMAT(/3X,'*** GROUP VALUE = ',F14.5,' ***'/)
9062 FORMAT(3('   SOURCE ID      CONTRIBUTION ',10X)/&
   &3(' -------------    ------------ ',10X))
9066 FORMAT(3(2X,A12,3X,F13.5,11X:))
9067 FORMAT(3(2X,A12,3X,E13.6,11X:))

   RETURN
END

SUBROUTINE PRTDET
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   INTEGER :: I, J, N, NSPP, NPAGE, IHR, INGRP
   DOUBLE PRECISION :: WAVEV(NSRC+1), WHRVAL(24,NSRC+1)

!     Variable Initializations
   MODNAM = 'PRTDET'
   WAVEV(:)    = 0.0D0
   WHRVAL(:,:) = 0.0D0

!     Set Up The Printing Work Array
   INGRP = 0
   DO ISRC = 1, NUMSRC
      IF (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) THEN
         INGRP = INGRP + 1
         WORKID(INGRP) = SRCID(ISRC)
         WAVEV(INGRP)  = EV_AVEVAL(ISRC)
         DO IHR = ISTAHR, IENDHR
            WHRVAL(IHR,INGRP) = HRVALS(IHR,ISRC)
         END DO
      END IF
   END DO

! --- Check for BACKGROUND "source" being included
!     in source group
   IF (GRP_BACK(IDXEV(IEVENT))) THEN
      INGRP = INGRP + 1
      WORKID(INGRP) = 'BACKGROUND'
      WAVEV(INGRP)  = BACKAVE(IDXEV(IEVENT))
      DO IHR = ISTAHR, IENDHR
         WHRVAL(IHR,INGRP) = BACKHR(IDXEV(IEVENT),IHR)
      END DO
   END IF

!     Set Number of Sources Per Page, NSPP
   NSPP = 8
!     Calculate Number of Pages for This Event (NSPP Sources per Page)
   NPAGE = 1 + INT((INGRP-1)/NSPP)
   DO N = 1, NPAGE
      CALL HEADER(IOUNIT)
      WRITE(IOUNIT,9058) EVNAME(IEVENT), GRPAVE(IDXEV(IEVENT)),&
      &EVAPER(IEVENT),EVDATE(IEVENT), AXR(IEVENT),AYR(IEVENT),&
      &AZELEV(IEVENT),AZFLAG(IEVENT)

      IF (N .EQ. NPAGE) THEN
!           Print Out The Values for the Last Page
         WRITE(IOUNIT,9068) EVGRP(IEVENT), (WORKID(I),&
         &I=1+NSPP*(N-1),INGRP)
         WRITE(IOUNIT,9066)

!           Print Out The Source Contributions for the Last Page
         DO I = ISTAHR, IENDHR
            IF (FILE_FORMAT .EQ. 'FIX') THEN
               WRITE(IOUNIT,9062) I,GRPVAL(IDXEV(IEVENT),I),&
               &(WHRVAL(I,J),J=1+NSPP*(N-1),INGRP)
            ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
               WRITE(IOUNIT,9063) I,GRPVAL(IDXEV(IEVENT),I),&
               &(WHRVAL(I,J),J=1+NSPP*(N-1),INGRP)
            END IF
         END DO
         IF (FILE_FORMAT .EQ. 'FIX') THEN
            WRITE(IOUNIT,9064) GRPAVE(IDXEV(IEVENT)),&
            &(WAVEV(I),I=1+NSPP*(N-1),INGRP)
         ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
            WRITE(IOUNIT,9065) GRPAVE(IDXEV(IEVENT)),&
            &(WAVEV(I),I=1+NSPP*(N-1),INGRP)
         END IF
      ELSE
!           Print Out The Values for the Current Page
         WRITE(IOUNIT,9068) EVGRP(IEVENT), (WORKID(I),&
         &I=1+NSPP*(N-1),NSPP*N)
         WRITE(IOUNIT,9066)

!           Print Out The Source Contributions for the Current Page
         DO I = ISTAHR, IENDHR
            IF (FILE_FORMAT .EQ. 'FIX') THEN
               WRITE(IOUNIT,9062) I,GRPVAL(IDXEV(IEVENT),I),&
               &(WHRVAL(I,J),J=1+NSPP*(N-1),NSPP*N)
            ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
               WRITE(IOUNIT,9063) I,GRPVAL(IDXEV(IEVENT),I),&
               &(WHRVAL(I,J),J=1+NSPP*(N-1),NSPP*N)
            END IF
         END DO
         IF (FILE_FORMAT .EQ. 'FIX') THEN
            WRITE(IOUNIT,9064) GRPAVE(IDXEV(IEVENT)),&
            &(WAVEV(I),I=1+NSPP*(N-1),NSPP*N)
         ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
            WRITE(IOUNIT,9065) GRPAVE(IDXEV(IEVENT)),&
            &(WAVEV(I),I=1+NSPP*(N-1),NSPP*N)
         END IF
      END IF

      IF (N .EQ. 1) THEN
!           Write Out the Meteorology Data
         NEWMET = .TRUE.
         DO IHOUR = ISTAHR, IENDHR
            UREF   = AUREF(IHOUR,1)
            UREFHT = AUREFHT(IHOUR,1)
            TA     = ATA(IHOUR,1)
            TREFHT = ATREFHT(IHOUR,1)
            WDREF  = AWDREF(IHOUR,1)
            SFCHF  = ASFCHF(IHOUR,1)
            USTAR  = AUSTAR(IHOUR,1)
            WSTAR  = AWSTAR(IHOUR,1)
            ZICONV = AZICONV(IHOUR,1)
            ZIMECH = AZIMECH(IHOUR,1)
            OBULEN = AOBULEN(IHOUR,1)
            VPTGZI = AVPTGZI(IHOUR,1)
            SFCZ0  = ASFCZ0(IHOUR,1)
            BOWEN  = ABOWEN(IHOUR,1)
            ALBEDO = AALBEDO(IHOUR,1)
!              Write Out The Meteorology Data
            CALL METDET
         END DO
      END IF
   END DO

9058 FORMAT(29X,'*** SOURCE CONTRIBUTIONS FOR EVENT: ',A10,&
   &4X,'GROUP VALUE = ',F14.5,' ***'/1X,'---> AVE. PER.: ',&
   &I3,' HRS;','  END DATE:  ',I8.8,&
   &';  LOCATION (XR,YR,ZELEV,ZFLAG):',4F11.2,' (M)')
9068 FORMAT(1X,'HR GROUP:',A8,' OF',1X,A12,7(2X,A12:))
9066 FORMAT(66('- '))
9062 FORMAT(1X,I2,1X,9(1X,F13.5:))
9063 FORMAT(1X,I2,1X,9(1X,E13.6:))
9064 FORMAT(66('- ')/1X,'AVE',9(1X,F13.5:))
9065 FORMAT(66('- ')/1X,'AVE',9(1X,E13.6:))

   RETURN
END

SUBROUTINE METDET
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'METDET'

!     Meteorology Data Summary
   IF (NEWMET) THEN
      NEWMET = .FALSE.
      WRITE(IOUNIT,9025)
   END IF
   WRITE(IOUNIT,9026) IYEAR, IMONTH, IDAY, IHOUR,&
   &SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,&
   &SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT


9025 FORMAT (' MET DATA --> YR MO DY HR','     H0','     U*','     W*',&
   &'  DT/DZ',' ZICNV', ' ZIMCH','  M-O LEN','    Z0',&
   &'  BOWEN',' ALBEDO','  REF WS', '   WD', '     HT',&
   &'  REF TA', '     HT')
9026 FORMAT (14X,4(I2.2,1X),F6.1,1X,3(F6.3,1X),&
   &2(F5.0,1X),F8.1,1X,F5.2,1X,2(F6.2,1X),F7.2,1X,F5.0,&
   &3(1X,F6.1) )

   RETURN
END

SUBROUTINE EV_FLUSH
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
   USE MAIN1
   USE BUOYANT_LINE
   IMPLICIT NONE
   CHARACTER MODNAM*12

! Unused:      INTEGER :: I

!     Variable Initializations
   MODNAM = 'EV_FLUSH'

!     Flush the Hourly Value
   HRVAL(:) = 0.0D0

!     Flush arrays, if allocated
! Multiple_BuoyLines_D41_Wood
!     Removed allocation of PARTCH - now declared as a scaler
   IF (ALLOCATED(CHI))  CHI(:,:,:) = 0.0D0
   IF(GRSM)THEN
      CHI_TTRAVPLM = 0.0D0
      CHI_TTRAVPAN = 0.0D0
      CHI_TTRAVAER = 0.0D0
      CHI_TTRAVPRM = 0.0D0
      CHI_TTRAVCHM(:,:) = 0.0D0
      BLDFAC(:,:) = 0.0D0
      PRMVAL_Src1 = 0.0D0
   END IF
   IF (ALLOCATED(CHIBL)) CHIBL(:) = 0.0D0
   GRPAVE(:)    = 0.0D0
   GRPVAL(:,:)  = 0.0D0
   IF (ALLOCATED(BACKAVE)) BACKAVE(:) = 0.0D0
   IF (ALLOCATED(BACKHR))  BACKHR(:,:)= 0.0D0
   EV_AVEVAL(:) = 0.0D0
   HRVALS(:,:)  = 0.0D0

   RETURN
END
