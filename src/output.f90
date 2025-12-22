SUBROUTINE PERAVE
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION :: SNUM, SNUMSCM, STOTHRS

!     Variable Initializations
   MODNAM = 'PERAVE'

!     Calculate Denominator Considering Calms and Missing
   SNUM = DBLE(IANHRS - IANCLM - IANMSG)
   IF (.NOT. SCIM) THEN
      STOTHRS = DBLE(IANHRS)
      SNUMSCM = SNUM
   ELSE IF (SCIM) THEN
      STOTHRS = DBLE(NSKIPTOT)                       ! Total no. of hours
      SNUMSCM = DBLE(IANHRS   - IANCLM   - IANMSG)   ! Sampled SCIM'd hours
   ENDIF

!     Calculate Period Average Concentrations for Each Source Group and Receptor

!     Begin LOOP Over Output Types
   DO ITYP = 1, NUMTYP

      IF (OUTTYP(ITYP) == 'CONC') THEN

         ANNVAL(1:NUMREC,1:NUMGRP,1) =&
         &ANNVAL(1:NUMREC,1:NUMGRP,1)/SNUM

      ELSE IF (SCIM) THEN

         ANNVAL(1:NUMREC,1:NUMGRP,ITYP) =&
         &ANNVAL(1:NUMREC,1:NUMGRP,ITYP)*(STOTHRS/SNUMSCM)

      END IF

   END DO
!     End LOOP Over Output Types

   RETURN
END SUBROUTINE PERAVE

SUBROUTINE SHAVE
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION :: SNUM

!     Variable Initializations
   MODNAM = 'SHAVE'

!     Calculate Period Average Concentrations for Each Source Group and Receptor

   DO ISEAS = 1, 4
      DO IHOUR = 1, 24

!           Calculate Denominator Considering Calms and Missing
         SNUM = DBLE(NSEAHR(ISEAS,IHOUR) - NSEACM(ISEAS,IHOUR))

         SHVALS(1:NUMREC,1:NUMGRP,ISEAS,IHOUR,1) =&
         &SHVALS(1:NUMREC,1:NUMGRP,ISEAS,IHOUR,1) / SNUM

      END DO
   END DO

   RETURN
END SUBROUTINE SHAVE

SUBROUTINE HIPER
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: J

!     Variable Initializations
   MODNAM = 'HIPER'

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
         IF (NHIANN > 1) THEN
            IF (ANNVAL(IREC,IGRP,ITYP) >&
            &AMXVAL(NHIANN,IGRP,ITYP)) THEN
               DO J = NHIANN-1, 1, -1
                  IF (ANNVAL(IREC,IGRP,ITYP) <=&
                  &AMXVAL(J,IGRP,ITYP))THEN
                     AMXVAL(J+1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                     IMXLOC(J+1,IGRP,ITYP) = IREC
!                       Exit Block
                     CYCLE RECEPTOR_LOOP
                  ELSE
                     AMXVAL(J+1,IGRP,ITYP) = AMXVAL(J,IGRP,ITYP)
                     IMXLOC(J+1,IGRP,ITYP) = IMXLOC(J,IGRP,ITYP)
                     IF (J == 1) THEN
                        AMXVAL(1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                        IMXLOC(1,IGRP,ITYP) = IREC
                     END IF
                  END IF
               END DO
            END IF
         ELSE IF (NHIANN == 1) THEN
            IF (ANNVAL(IREC,IGRP,ITYP) > AMXVAL(1,IGRP,ITYP)) THEN
               AMXVAL(1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
               IMXLOC(1,IGRP,ITYP) = IREC
            END IF
         END IF
      END DO RECEPTOR_LOOP
!        End Receptor LOOP
   END DO
!     End Source Group LOOP

   RETURN
END SUBROUTINE HIPER

SUBROUTINE PSTANN
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

! Unused:      INTEGER :: I
   CHARACTER :: PERCHR*6, HDRFRM*400

!     Variable Initializations
   MODNAM = 'PSTANN'

!     Set Averaging Label and Create Header Format for Columns
   IF (PERIOD) THEN
      PERCHR = 'PERIOD'
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2
   ELSE IF (ANNUAL) THEN
      PERCHR = 'ANNUAL'
      WRITE(HDRFRM,9021) NUMTYP, NUMTYP+2
   END IF

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Check for Selection of PERIOD POSTFILE for This Group
      IF (IANPST(IGRP) == 1) THEN
         IF (IANFRM(IGRP) == 0) THEN
!              WRITE Results to Unformatted POSTFILE
            IF (PERIOD) THEN
               WRITE(IAPUNT(IGRP),ERR=99) KURDAT, IANHRS,&
               &GRPID(IGRP), ((ANNVAL(IREC,IGRP,ITYP),IREC=1,NUMREC),&
               &ITYP=1,NUMTYP)
            ELSE IF (ANNUAL) THEN
               WRITE(IAPUNT(IGRP),ERR=99) KURDAT, NUMYRS,&
               &GRPID(IGRP), ((ANNVAL(IREC,IGRP,ITYP),IREC=1,NUMREC),&
               &ITYP=1,NUMTYP)
            END IF
         ELSE
!              WRITE Results to Formatted Plot File
!              Write Header Information
            WRITE(IAPUNT(IGRP),9005) VERSN, TITLE1(1:68), RUNDAT
            WRITE(IAPUNT(IGRP),9007) C_METVER, RUNTIM,&
            &MODOPS_String(1:LEN_TRIM(MODOPS_String))
            IF (PERIOD) THEN
               WRITE(IAPUNT(IGRP),9010) PERCHR,GRPID(IGRP),&
               &NUMREC,PSTFRM
               WRITE(IAPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),&
               &CHIDEP(2,ITYP),&
               &CHIDEP(3,ITYP),&
               &ITYP=1,NUMTYP)
            ELSE IF (ANNUAL) THEN
               WRITE(IAPUNT(IGRP),9011) PERCHR,NUMYRS,GRPID(IGRP),&
               &NUMREC,PSTFRM
               WRITE(IAPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),&
               &CHIDEP(2,ITYP),&
               &CHIDEP(3,ITYP),&
               &ITYP=1,NUMTYP)
            END IF
!              Begin Receptor LOOP
            DO IREC = 1, NUMREC
               IF (PERIOD) THEN
                  WRITE(IAPUNT(IGRP),PSTFRM,ERR=99)&
                  &AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),&
                  &ITYP=1,NUMTYP),&
                  &AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),&
                  &PERCHR, GRPID(IGRP), IANHRS, NETID(IREC)
               ELSE IF (ANNUAL) THEN
                  WRITE(IAPUNT(IGRP),PSTFRM,ERR=99)&
                  &AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),&
                  &ITYP=1,NUMTYP),&
                  &AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),&
                  &PERCHR, GRPID(IGRP), NUMYRS, NETID(IREC)
               END IF
            END DO
!              End Receptor LOOP
         END IF
      END IF
   END DO
!     End Source Group LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to Postprocessor File
99 WRITE(DUMMY,'("PSTFL",I3.3)') IAPUNT(IGRP)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
9007 FORMAT('* AERMET (',A6,'):',T93,A8,&
   &/'* MODELING OPTIONS USED: ',A:)
9010 FORMAT('*',9X,'POST/PLOT FILE OF ',A6,' VALUES FOR ',&
   &'SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',&
   &/'*',9X,'FORMAT: ',A:)
9011 FORMAT('*',9X,'POST/PLOT FILE OF ',A6,' VALUES FOR YEAR ',&
   &I3,' FOR SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',&
   &/'*',9X,'FORMAT: ',A:)
9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM HRS'',&
   &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')
! --- Modified to show the Year Number for ANNUAL averages, instead of
!     number of years, in v15181
9021 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',5X,''YEAR NUM'',&
   &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')

999 RETURN
END SUBROUTINE PSTANN

SUBROUTINE PLTANN
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

! Unused:      INTEGER :: I
   CHARACTER :: PERCHR*6, HDRFRM*400

!     Variable Initializations
   MODNAM = 'PLTANN'

!     Set Averaging Label and Create Header Format for Columns
   IF (PERIOD) THEN
      PERCHR = 'PERIOD'
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2
   ELSE IF (ANNUAL) THEN
      PERCHR = 'ANNUAL'
      WRITE(HDRFRM,9021) NUMTYP, NUMTYP+2
   END IF

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Check for Selection of PERIOD PLOTFILE for This Group
      IF (IANPLT(IGRP) == 1) THEN
!           Write Header Information
         WRITE(IPPUNT(IGRP),9005) VERSN, TITLE1(1:68), RUNDAT
         WRITE(IPPUNT(IGRP),9007) C_METVER, RUNTIM,&
         &MODOPS_String(1:LEN_TRIM(MODOPS_String))
         WRITE(IPPUNT(IGRP),9011) PERCHR,NUMYRS,GRPID(IGRP),&
         &NUMREC,PSTFRM
         WRITE(IPPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),CHIDEP(2,ITYP),&
         &CHIDEP(3,ITYP),ITYP=1,NUMTYP)
!           Begin Receptor LOOP
         DO IREC = 1, NUMREC
            IF (PERIOD) THEN
               WRITE(IPPUNT(IGRP),PSTFRM,ERR=99)&
               &AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),&
               &ITYP=1,NUMTYP), AZELEV(IREC), AZHILL(IREC),&
               &AZFLAG(IREC), PERCHR,GRPID(IGRP), IANHRS, NETID(IREC)
            ELSE IF (ANNUAL) THEN
               WRITE(IPPUNT(IGRP),PSTFRM,ERR=99)&
               &AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),&
               &ITYP=1,NUMTYP), AZELEV(IREC), AZHILL(IREC),&
               &AZFLAG(IREC), PERCHR,GRPID(IGRP), NUMYRS, NETID(IREC)
            END IF
         END DO
!           End Receptor LOOP
      END IF
   END DO
!     End Source Group LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to Plot File
99 WRITE(DUMMY,'("PLTFL",I3.3)') IPPUNT(IGRP)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
9007 FORMAT('* AERMET (',A6,'):',T93,A8,&
   &/'* MODELING OPTIONS USED: ',A:)
! Unused: 9010 FORMAT('*',9X,'PLOT FILE OF ',A6,' VALUES FOR ',
!     &       'SOURCE GROUP: ',A8,
!     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
!     &      /'*',9X,'FORMAT: ',A:)
9011 FORMAT('*',9X,'PLOT FILE OF ',A6,' VALUES AVERAGED ACROSS ',&
   &I3,' YEARS FOR SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',&
   &/'*',9X,'FORMAT: ',A:)
9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM HRS'',&
   &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')
9021 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM YRS'',&
   &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,&
   & 3('' ______  ''),''______  ________  ________  ________'')')

999 RETURN
END SUBROUTINE PLTANN

SUBROUTINE PLOTFL
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, IVAL, IDEC, IMOD
   CHARACTER :: NCHR1(10)*3, NCHR2(10)*5, CHRVAL*5, HDRFRM*400

!     Variable Initializations
   DATA (NCHR1(I),I=1,10) /'YR1','YR2','YR3','YR4','YR5',&
   &'YR6','YR7','YR8','YR9','Y10'/
   DATA (NCHR2(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'PLOTFL'

!     Create Header Format for Columns
   IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
      WRITE(HDRFRM,9019) NUMYRS, NUMYRS
   ELSE
      WRITE(HDRFRM,9020) NUMTYP, CHIDEP(3,1), NUMTYP+2
   END IF

!     Begin Averaging Period LOOP
   DO IAVE = 1, NUMAVE
!        Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
!           Begin High Value LOOP
         DO IVAL = 1, NHIVAL
!              Decide if we should go through the processing
            IF (IPLTFL(IVAL,IGRP,IAVE) == 1) THEN

! ---             Assign character label for rank
               IF (IVAL <= 10) THEN
                  CHRVAL = NCHR2(IVAL)
               ELSE IF (MOD(IVAL,100) > 10 .and.&
               &MOD(IVAL,100) < 20) THEN
                  IDEC = INT(IVAL/10)
                  IMOD = MOD(IVAL,10)
                  WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
               ELSE IF (IVAL <= 999) THEN
                  IDEC = INT(IVAL/10)
                  IMOD = MOD(IVAL,10)
                  IF (IMOD == 0) IMOD = 10
                  WRITE(CHRVAL,'(I2,A3)') IDEC, NCHR2(IMOD)(3:5)
               END IF

               IF (.NOT. L_NoHeader(3)) THEN
!                 Write Header Information
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9005) VERSN,&
                  &TITLE1(1:68),RUNDAT
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9007) C_METVER,&
                  &TITLE2(1:68),RUNTIM,&
                  &MODOPS_String(1:LEN_TRIM(MODOPS_String))
                  IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),9009) CHRVAL,&
                     &CHRAVE(IAVE), NUMYRS, GRPID(IGRP), NUMREC,&
                     &PLTFRM
                  ELSE
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),9010) CHRVAL,&
                     &CHRAVE(IAVE), GRPID(IGRP), NUMREC, PLTFRM
                  END IF
                  IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),HDRFRM) CHIDEP(1,1),&
                     &CHIDEP(2,1),CHIDEP(3,1),&
                     &(NCHR1(I),NCHR1(I),I=1,NUMYRS)
                  ELSE
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),HDRFRM)&
                     &(CHIDEP(1,ITYP),CHIDEP(2,ITYP),CHIDEP(3,ITYP),&
                     &ITYP=1,NUMTYP)
                  END IF
               END IF

!                 Begin Receptor LOOP
               DO IREC = 1, NUMREC
                  IF (PM25AVE .or. NO2AVE .or. SO2AVE) THEN
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),PLTFRM,ERR=99)&
                     &AXR(IREC), AYR(IREC), SUMHNH(IREC,IGRP,IVAL),&
                     &AZELEV(IREC),AZHILL(IREC),AZFLAG(IREC),&
                     &CHRAVE(IAVE),GRPID(IGRP),CHRVAL,NETID(IREC),&
                     &(HIMXDLY_BYYR(IREC,IGRP,IVAL,J),&
                     &NHIDATMXD_BYYR(IREC,IGRP,IVAL,J),J=1,NUMYRS)
                  ELSE
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),PLTFRM,ERR=99)&
                     &AXR(IREC), AYR(IREC), (HIVALU(IREC,IVAL,IGRP,&
                     &IAVE,ITYP),ITYP=1,NUMTYP),&
                     &AZELEV(IREC),AZHILL(IREC),AZFLAG(IREC),&
                     &CHRAVE(IAVE),GRPID(IGRP),CHRVAL,&
                     &NETID(IREC), NHIDAT(IREC,IVAL,IGRP,IAVE,1)
                  END IF
               END DO
!                 End Receptor LOOP
            END IF
         END DO
!           End High Value LOOP
      END DO
!        End Source Group LOOP
   END DO
!     End Averaging Period LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to Plot File
99 WRITE(DUMMY,'("PLTFL",I3.3)') IPLUNT(IVAL,IGRP,IAVE)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
9007 FORMAT('* AERMET (',A6,'): ',A68,T93,A8,&
   &/'* MODELING OPTIONS USED: ',A:)
! Unused:  9008 FORMAT('*',9X,'PLOT FILE OF ',A5,'-HIGHEST ',A5,
!     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
!     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
!     &      /'*',9X,'FORMAT: ',A:)
9009 FORMAT('*',9X,'PLOT FILE OF ',A5,'-HIGHEST MAX DAILY ',A5,&
   &' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',&
   &/'*',9X,'FORMAT: ',A:)
9010 FORMAT('*',9X,'PLOT FILE OF  HIGH ',A5,' HIGH ',A5,&
   &' VALUES FOR SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',&
   &/'*',9X,'FORMAT: ',A:)
9019 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,(2X,3A4),4X,''ZELEV'',4X,&
   &''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,&
   &''NET ID'',1X,',I1,'(2X,''AVER CONC '',A3,''  DATE '',A3),/,''*'',&
   &3(1X,''____________ ''),1X,3('' ______  ''),&
   &''______  ________  ________  ________'',',&
   &I1,'(''  _____________  ________''))')
9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',&
   &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,&
   &  ''NET ID'',3X,''DATE(',A4,')'',/,''*'',',I1,'(1X,&
   & ''____________ ''),1X,3('' ______  ''),&
   & ''______  ________  ________  ________  ________'')')

999 RETURN
END SUBROUTINE PLOTFL

SUBROUTINE OUTPUT
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

!     Variable Initializations
   MODNAM = 'OUTPUT'
   PATH = 'OU'

   IF (PERIOD .or. ANNUAL) THEN
      DO ITYP = 1, NUMTYP
!           Print Out Summary of Period Averages            ---   CALL PRTANN
         CALL PRTANN
      END DO
   END IF

   IF (PM25AVE .and. NUMAVE==1) THEN
!        Print Out Table of Average High-N-High Values for PM2.5
      DO ITYP = 1, NUMTYP
         CALL PRTPM25
      END DO
      CALL MAXPM25
   ELSE IF (NO2AVE .and. NUMAVE>=1) THEN
!        Print Out Table of Average High-N-High Values for NO2
      DO ITYP = 1, NUMTYP
         CALL PRTPM25
      END DO
      CALL MAXPM25
   ELSE IF (SO2AVE .and. NUMAVE>=1) THEN
!        Print Out Table of Average High-N-High Values for SO2
      DO ITYP = 1, NUMTYP
         CALL PRTPM25
      END DO
      CALL MAXPM25
   ELSE IF (NHIVAL > 0) THEN
      DO ITYP = 1, NUMTYP
!           Print Out Summary of High Values by Receptor    ---   CALL PRTNHI
         CALL PRTNHI
      END DO
   ELSE IF (EVENTS) THEN
!        Write the 'EV STARTING' and 'EV FINISHED' Cards to the Temp-EVent File
      WRITE(ITEVUT,9000)
      WRITE(ITEVUT,9001)
9000  FORMAT('EV STARTING')
9001  FORMAT('EV FINISHED')
   END IF

   IF (NMXVAL > 0) THEN
      DO ITYP = 1, NUMTYP
!           Print Out Summary of Overall Maximum Values     ---   CALL PRTMAX
         CALL PRTMAX
      END DO
   END IF

   IF (PERIOD .or. ANNUAL .or. NHIVAL > 0) THEN
      DO ITYP = 1, NUMTYP
! ---       Print Out Summary of Results                    ---   CALL PRTSUM
!           Note that summary of short-term results for PM25 24h, NO2/SO2 1h
!           is provided in separate subroutine PRTPM25SUM
         CALL PRTSUM(IOUNIT)
         IF (SUMMFILE) THEN
            CALL PRTSUM(ISUMUNT)
         END IF
      END DO
   END IF

! --- Print out summary of short-term results for PM25 24h, NO2/SO2 1h
   IF (PM25AVE .and. NHIVAL > 0) THEN
      DO ITYP = 1, NUMTYP
!           Print Out Summary of PM-2.5 Results             ---   CALL PRTPM25SUM
         CALL PRTPM25SUM(IOUNIT)
         IF (SUMMFILE) THEN
            CALL PRTPM25SUM(ISUMUNT)
         END IF
      END DO
   ELSE IF (NO2AVE .and. NHIVAL > 0) THEN
      DO ITYP = 1, NUMTYP
!           Print Out Summary of NO2 Results                ---   CALL PRTPM25SUM
         CALL PRTPM25SUM(IOUNIT)
         IF (SUMMFILE) THEN
            CALL PRTPM25SUM(ISUMUNT)
         END IF
      END DO
   ELSE IF (SO2AVE .and. NHIVAL > 0) THEN
      DO ITYP = 1, NUMTYP
!           Print Out Summary of SO2 Results                ---   CALL PRTPM25SUM
         CALL PRTPM25SUM(IOUNIT)
         IF (SUMMFILE) THEN
            CALL PRTPM25SUM(ISUMUNT)
         END IF
      END DO
   END IF

   IF (SEASONHR) THEN
      CALL SHOUT
   END IF

   IF (RKFILE) THEN
      DO ITYP = 1, NUMTYP
!           Write Short Term High Values to Plot File       ---   CALL RANKFL
         CALL RANKFL
      END DO
   END IF

!     Generate The EVENT Input File                         ---   CALL EVEFIL
   IF (EVENTS) THEN
      CALL EVEFIL
   END IF

   RETURN
END SUBROUTINE OUTPUT

SUBROUTINE PRTANN
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K, II, NX, NY, INDZ, INDC, INDEXW
   DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
   CHARACTER :: PERCHR*6, BUF132*132

!     Variable Initializations
   MODNAM = 'PRTANN'
   BUF132 = ' '
   INDZ   = 0

   IF (PERIOD) THEN
      PERCHR = 'PERIOD'
   ELSE IF (ANNUAL) THEN
      PERCHR = 'ANNUAL'
   END IF

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP

      IF (.NOT. PSDCREDIT) THEN
!           Fill Work Array With SRCIDs For This Group
         INDGRP = 0
         DO ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IGRP) == 1) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = SRCID(ISRC)
            END IF
         END DO
      ELSE
!           Assign 'N/A' for source IDs for PSDCREDIT option
         INDGRP = 1
         WORKID(INDGRP) = 'N/A'
      END IF

! ---    Check for BACKGROUND "source" being included
!        in source group
      IF (GRP_BACK(IGRP)) THEN
         INDGRP = INDGRP + 1
         WORKID(INDGRP) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
         INDEXW = MIN(29,NSRC+1)
      ELSE
         INDEXW = MIN(29,NSRC)
      END IF
!        Check for More Than 29 Sources Per Group
      IF (INDGRP > INDEXW) THEN
         WORKID(INDEXW) = ' . . . '
         INDGRP = INDEXW
      END IF

!        Print Receptor Network Coordinates:
!        Set Number of Columns Per Page, NCPP
      NCPP = 9
!        Set Number of Rows Per Page, NRPP
      NRPP = 40
!        Begin LOOP Through Networks
      DO I = 1, INNET
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
         NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
         DO NX = 1, NPPX
            DO NY = 1, NPPY
               CALL HEADER(IOUNIT)
               IF (PERIOD) THEN
                  WRITE(IOUNIT,9032) PERCHR, IANHRS,&
                  &(CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
               ELSE IF (ANNUAL) THEN
                  WRITE(IOUNIT,9033) PERCHR,(CHIDEP(II,ITYP),II=1,6),&
                  &NUMYRS,GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
               END IF
               WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
!                 Print The Values By Source Group
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,PERLBL(ITYP)
               IF (NX == NPPX) THEN
                  IF (NTTYP(I) == 'GRIDCART') THEN
                     WRITE(IOUNIT,9016)
                     WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NUMXPT(I))
                  ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                     WRITE(IOUNIT,9018)
                     WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NUMXPT(I))
                  END IF
               ELSE
                  IF (NTTYP(I) == 'GRIDCART') THEN
                     WRITE(IOUNIT,9016)
                     WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NCPP*NX)
                  ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                     WRITE(IOUNIT,9018)
                     WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NCPP*NX)
                  END IF
               END IF
               WRITE(IOUNIT,9010)
               IF (NY == NPPY) THEN
                  DO K = 1+NRPP*(NY-1), NUMYPT(I)
                     IF (NTTYP(I) == 'GRIDCART') THEN
                        INDZ = NETEND(I) - K*NUMXPT(I) + 1
                        YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                     ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                        INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                        YCOVAL = YCOORD(K,I)
                     END IF
                     IF (NX == NPPX) THEN
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NUMXPT(I))
                     ELSE
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NCPP*NX)
                     END IF
                  END DO
               ELSE
                  DO K = 1+NRPP*(NY-1), NRPP*NY
                     IF (NTTYP(I) == 'GRIDCART') THEN
                        INDZ = NETEND(I) - K*NUMXPT(I) + 1
                        YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                     ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                        INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                        YCOVAL = YCOORD(K,I)
                     END IF
                     IF (NX == NPPX) THEN
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NUMXPT(I))
                     ELSE
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NCPP*NX)
                     END IF
                  END DO
               END IF
            END DO
         END DO
      END DO
!        End LOOP Through Networks

      IF (IRSTAT(4)/=0 .or. IRSTAT(8)/=0) THEN
! ---       Include EVALCART receptors with DISCCART receptors.
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
         INDC = 0
         DO IREC = 1, NUMREC
            IF (RECTYP(IREC) == 'DC') THEN
               INDC = INDC + 1
               IF (MOD(INDC-1,80) == 0) THEN
                  CALL HEADER(IOUNIT)
                  IF (PERIOD) THEN
                     WRITE(IOUNIT,9032) PERCHR,IANHRS,(CHIDEP(II,ITYP),&
                     &II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                  ELSE IF (ANNUAL) THEN
                     WRITE(IOUNIT,9033) PERCHR,(CHIDEP(II,ITYP),&
                     &II=1,6),NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                  END IF
                  WRITE(IOUNIT,9043)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                  &PERLBL(ITYP)
                  WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               END IF
               IF (MOD(INDC,2) /= 0) THEN
                  WRITE(BUF132(1:60),9045) AXR(IREC), AYR(IREC),&
                  &ANNVAL(IREC,IGRP,ITYP)
               ELSE
                  WRITE(BUF132(61:120),9045) AXR(IREC), AYR(IREC),&
                  &ANNVAL(IREC,IGRP,ITYP)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) /= 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(5) /= 0) THEN
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
         INDC = 0
         DO IREC = 1, NUMREC
            IF (RECTYP(IREC) == 'DP') THEN
               INDC = INDC + 1
               XRMS = AXR(IREC) - AXS(IREF(IREC))
               YRMS = AYR(IREC) - AYS(IREF(IREC))
               DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
               DIR  = DATAN2(XRMS, YRMS) * RTODEG
               IF (DIR <= 0.0D0) DIR = DIR + 360.0D0
               IF (MOD(INDC-1,80) == 0) THEN
                  CALL HEADER(IOUNIT)
                  IF (PERIOD) THEN
                     WRITE(IOUNIT,9032) PERCHR,IANHRS,(CHIDEP(II,ITYP),&
                     &II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                  ELSE IF (ANNUAL) THEN
                     WRITE(IOUNIT,9033) PERCHR,(CHIDEP(II,ITYP),&
                     &II=1,6),NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                  END IF
                  WRITE(IOUNIT,9044)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                  &PERLBL(ITYP)
                  WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               END IF
               IF (MOD(INDC,2) /= 0) THEN
                  WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)), DIST,&
                  &DIR, ANNVAL(IREC,IGRP,ITYP)
               ELSE
                  WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)), DIST,&
                  &DIR, ANNVAL(IREC,IGRP,ITYP)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) /= 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

!     End Source Group Loop
   END DO

9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9010 FORMAT(66(' -')/)
9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
9032 FORMAT(/30X,'*** THE ',A6,' (',I6,' HRS) ',6A4,&
   &'VALUES FOR SOURCE GROUP:',1X,A8,' ***',&
   &/34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
9033 FORMAT(/19X,'*** THE ',A6,1X,6A4,' VALUES AVERAGED OVER ',&
   &I3,' YEARS FOR SOURCE GROUP:',1X,A8,' ***',&
   &/34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',&
   &A8,' ***')
9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 FORMAT(6X,2(F12.2,2X),F13.5)
9047 FORMAT(2X,A12,': ',F12.2,2X,F10.2,2X,F13.5)
9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,&
   &22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',&
   &/5X,' SRCID         DIST (M)   DIR (DEG)        ',A4,&
   &18X,' SRCID         DIST (M)   DIR (DEG)        ',A4,&
   &/65(' -'))
9090 FORMAT(A132)
9095 FORMAT(132(' '))

   RETURN
END SUBROUTINE PRTANN

SUBROUTINE PRTNHI
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: IWHP(NVAL), IHST, IVAL, K, IT1, KWRT, IGRP_TMP
   DOUBLE PRECISION :: XR2, YR2, ZE2, ZH2, ZF2
   CHARACTER :: NAMEEV*10

!     Variable Initialization
   MODNAM = 'PRTNHI'

! --- Initialize IWHP array
   IWHP = 0

!     Write Out the 'EV STARTING' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   IF (ITYP == 1) THEN
      WRITE(ITEVUT,9000)
   END IF

   DO IAVE = 1, NUMAVE
!        Decide if Print The Period
      IHST = 0
      DO IVAL = 1, NVAL
         IF (NHIAVE(IVAL,IAVE) == 1) THEN
            IHST = IHST + 1
            IWHP(IHST) = IVAL
         END IF
      END DO
      IF (IHST == 0) THEN
!           No High Values for This IAVE; Cycle to Next Averaging Period
         CYCLE
      END IF
!        Print The Data
      DO IVAL = 1, NVAL
         IF (NHIAVE(IVAL,IAVE) == 1) THEN
!              Print Out High Value By Receptor Table       ---   CALL SPRTHT
            CALL SPRTHT(IVAL)
         END IF
      END DO
!        Print Out The Temporary File
      DO IGRP = 1, NUMGRP
!           Print Out the High Values
         DO IREC = 1, NUMREC
!               Get The Maximum in Nth Highest
            DO K = 1, IHST
               IF (HIVALU(IREC,IWHP(K),IGRP,IAVE,ITYP) >&
               &HMAX(K,IGRP,IAVE,ITYP)) THEN
                  HMAX(K,IGRP,IAVE,ITYP)   = HIVALU(IREC,IWHP(K),IGRP,IAVE,ITYP)
                  HMDATE(K,IGRP,IAVE,ITYP) = NHIDAT(IREC,IWHP(K),IGRP,IAVE,ITYP)
                  HMCLM(K,IGRP,IAVE,ITYP)  = HCLMSG(IREC,IWHP(K),IGRP,IAVE,ITYP)
                  HMLOC(K,IGRP,IAVE,ITYP)  = IREC
               END IF
            END DO
         END DO
!
!           Output The Max-Upto-IHST to the TempEVent File for the
!           First Output Type Only (i.e., ITYP = 1)
         IF (ITYP == 1) THEN
            DO K = 1, IHST
               IT1 = MIN( 999, IWHP(K) )
               IF (HMLOC(K,IGRP,IAVE,ITYP) == 0) THEN
                  XR2 = 0.0D0
                  YR2 = 0.0D0
                  ZE2 = 0.0D0
                  ZH2 = 0.0D0
                  ZF2 = 0.0D0
               ELSE
                  XR2 = AXR(HMLOC(K,IGRP,IAVE,ITYP))
                  YR2 = AYR(HMLOC(K,IGRP,IAVE,ITYP))
                  ZE2 = AZELEV(HMLOC(K,IGRP,IAVE,ITYP))
                  ZH2 = AZHILL(HMLOC(K,IGRP,IAVE,ITYP))
                  ZF2 = AZFLAG(HMLOC(K,IGRP,IAVE,ITYP))
               END IF

               IF (IGRP > 999) THEN
!                    Number of Source Groups Exceeds Limit of EVNAME Field,
!                    Write Warning Message and Reset to 999
                  IF (IGRP <= 9999) THEN
                     WRITE(DUMMY,'(I2.2,''hr'',1X,''IG='',I4)')&
                     &IAVE, IGRP
                  ELSE
                     WRITE(DUMMY,'(I2.2,''hr'',1X,''IG>9999'')')&
                     &IAVE
                  END IF
                  CALL ERRHDL(PATH,MODNAM,'W','235',DUMMY)
                  IGRP_TMP = 999
               ELSE
                  IGRP_TMP = IGRP
               END IF

               IF (KAVE(IAVE) <= 24) THEN
                  WRITE(NAMEEV,'(A1,I3.3,A1,I2.2,I3.3)')&
                  &'H',IT1,'H',KAVE(IAVE),IGRP_TMP
               ELSE
!                    KAVE > 24 Means MONTH Average; Write Out as 72 (=720/10)
                  KWRT = KAVE(IAVE)/10
                  WRITE(NAMEEV,'(A1,I3.3,A1,I2.2,I3.3)')&
                  &'H',IT1,'H',KWRT,IGRP_TMP
               END IF
               WRITE(ITEVUT,9001) NAMEEV, KAVE(IAVE),&
               &GRPID(IGRP), HMDATE(K,IGRP,IAVE,ITYP),&
               &HMAX(K,IGRP,IAVE,ITYP)
               WRITE(ITEVUT,9002) NAMEEV, XR2, YR2, ZE2, ZH2, ZF2
            END DO
         END IF

      END DO

   END DO

!     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   IF (ITYP == 1) THEN
      WRITE(ITEVUT,9009)
   END IF

9000 FORMAT('EV STARTING')
9001 FORMAT(3X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
9002 FORMAT(3X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,&
   &3(1X,F10.4))
9009 FORMAT('EV FINISHED')

   RETURN
END SUBROUTINE PRTNHI

SUBROUTINE SPRTHT(IHNUM)
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: IHNUM, I, J, K, II, INDZ, INDC, NX, NY, INDEXW
   INTEGER :: IDEC, IMOD
   DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
   CHARACTER :: BUF132*132, CHRVAL*5, CHRVALS(10)*5

!     Variable Initializations
   DATA (CHRVALS(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'SPRTHT'
   BUF132 = ' '
   INDZ   = 0

! --- Assign character label for rank
   IF (IHNUM <= 10) THEN
      CHRVAL = CHRVALS(IHNUM)
   ELSE IF (MOD(IHNUM,100) > 10 .and.&
   &MOD(IHNUM,100) < 20) THEN
      IDEC = INT(IHNUM/10)
      IMOD = MOD(IHNUM,10)
      WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
   ELSE IF (IHNUM <= 999) THEN
      IDEC = INT(IHNUM/10)
      IMOD = MOD(IHNUM,10)
      IF (IMOD == 0) IMOD = 10
      WRITE(CHRVAL,'(I2,A3)') IDEC, CHRVALS(IMOD)(3:5)
   END IF

   DO IGRP = 1, NUMGRP

      IF (.NOT. PSDCREDIT) THEN
!           Fill Work Array With SRCIDs For This Group
         INDGRP = 0
         DO ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IGRP) == 1) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = SRCID(ISRC)
            END IF
         END DO
      ELSE
!           Assign 'N/A' for source IDs for PSDCREDIT option
         INDGRP = 1
         WORKID(INDGRP) = 'N/A'
      END IF

! ---    Check for BACKGROUND "source" being included
!        in source group
      IF (GRP_BACK(IGRP)) THEN
         INDGRP = INDGRP + 1
         WORKID(INDGRP) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
         INDEXW = MIN(29,NSRC+1)
      ELSE
         INDEXW = MIN(29,NSRC)
      END IF
!        Check for More Than 29 Sources Per Group
      IF (INDGRP > INDEXW) THEN
         WORKID(INDEXW) = ' . . . '
         INDGRP = INDEXW
      END IF

!        Print Receptor Network Coordinates:
!        Set Number of Columns Per Page, NCPP
      NCPP = 5
!        Set Number of Rows Per Page, NRPP
      NRPP = 40
!        Begin LOOP Through Networks
      DO I = 1, INNET
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
         NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
         DO NX = 1, NPPX
            DO NY = 1, NPPY
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9032) CHRVAL, CHRAVE(IAVE),&
               &(CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
               WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
!                 Print The Values By Source Group
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
               &OUTLBL(ITYP)
               IF (NX == NPPX) THEN
                  IF (NTTYP(I) == 'GRIDCART') THEN
                     WRITE(IOUNIT,9016)
                     WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NUMXPT(I))
                  ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                     WRITE(IOUNIT,9018)
                     WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NUMXPT(I))
                  END IF
               ELSE
                  IF (NTTYP(I) == 'GRIDCART') THEN
                     WRITE(IOUNIT,9016)
                     WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NCPP*NX)
                  ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                     WRITE(IOUNIT,9018)
                     WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                     &NCPP*NX)
                  END IF
               END IF
               WRITE(IOUNIT,9010)
               IF (NY == NPPY) THEN
                  DO K = 1+NRPP*(NY-1), NUMYPT(I)
                     IF (NTTYP(I) == 'GRIDCART') THEN
                        INDZ = NETEND(I) - K*NUMXPT(I) + 1
                        YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                     ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                        INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                        YCOVAL = YCOORD(K,I)
                     END IF
                     IF (NX == NPPX) THEN
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &J=1+NCPP*(NX-1),NUMXPT(I))
                     ELSE
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &J=1+NCPP*(NX-1),NCPP*NX)
                     END IF
                  END DO
               ELSE
                  DO K = 1+NRPP*(NY-1), NRPP*NY
                     IF (NTTYP(I) == 'GRIDCART') THEN
                        INDZ = NETEND(I) - K*NUMXPT(I) + 1
                        YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                     ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                        INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                        YCOVAL = YCOORD(K,I)
                     END IF
                     IF (NX == NPPX) THEN
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &J=1+NCPP*(NX-1),NUMXPT(I))
                     ELSE
                        WRITE(IOUNIT,9013) YCOVAL,&
                        &(HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),&
                        &J=1+NCPP*(NX-1),NCPP*NX)
                     END IF
                  END DO
               END IF
            END DO
         END DO
      END DO
!        End LOOP Through Networks

      IF (IRSTAT(4)/=0 .or. IRSTAT(8)/=0) THEN
! ---       Include EVALCART receptors with DISCCART receptors.
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
         INDC = 0
         DO IREC = 1, NUMREC
            IF (RECTYP(IREC) == 'DC') THEN
               INDC = INDC + 1
               IF (MOD(INDC-1,80) == 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9032) CHRVAL, CHRAVE(IAVE),&
                  &(CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                  WRITE(IOUNIT,9043)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                  &OUTLBL(ITYP)
                  WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               END IF
               IF (MOD(INDC,2) /= 0) THEN
                  WRITE(BUF132(1:65),9045) AXR(IREC), AYR(IREC),&
                  &HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
               ELSE
                  WRITE(BUF132(66:130),9045) AXR(IREC), AYR(IREC),&
                  &HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) /= 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(5) /= 0) THEN
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
         INDC = 0
         DO IREC = 1, NUMREC
            IF (RECTYP(IREC) == 'DP') THEN
               INDC = INDC + 1
               XRMS = AXR(IREC) - AXS(IREF(IREC))
               YRMS = AYR(IREC) - AYS(IREF(IREC))
               DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
               DIR  = DATAN2(XRMS, YRMS) * RTODEG
               IF (DIR <= 0.0D0) DIR = DIR + 360.0D0
               IF (MOD(INDC-1,80) == 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9032) CHRVAL, CHRAVE(IAVE),&
                  &(CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                  WRITE(IOUNIT,9044)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                  &OUTLBL(ITYP)
                  WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               END IF
               IF (MOD(INDC,2) /= 0) THEN
                  WRITE(BUF132(1:66),9047) SRCID(IREF(IREC)), DIST,&
                  &DIR, HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
               ELSE
                  WRITE(BUF132(67:132),9047) SRCID(IREF(IREC)), DIST,&
                  &DIR, HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),&
                  &NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) /= 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

   END DO

9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9010 FORMAT(66(' -')/)
9016 FORMAT(1X,' Y-COORD  |',50X,'X-COORD (METERS)')
9017 FORMAT(1X,' (METERS) |',3X,F13.2,4(11X,F13.2,:))
9018 FORMAT(1X,'DIRECTION |',50X,'DISTANCE (METERS)')
9019 FORMAT(1X,'(DEGREES) |',3X,F13.2,4(11X,F13.2,:))
9013 FORMAT(1X,F9.1,1X,'|',5(F13.5,A1,'(',I8.8,')',:))
9032 FORMAT(/30X,'*** THE ',A5,' HIGHEST ',A5,1X,6A4,&
   &'VALUES FOR SOURCE GROUP:',2X,A8,' ***',&
   &/34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',&
   &A8,' ***')
9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 FORMAT(6X,2(F11.2,2X),F13.5,A1,1X,'(',I8.8,')')
9047 FORMAT(1X,A12,': ',F11.2,1X,F10.2,1X,F13.5,A1,1X,'(',I8.8,')')
9048 FORMAT(6X,'X-COORD (M)  Y-COORD (M)        ',A4,5X,'(YYMMDDHH)',&
   &14X,'X-COORD (M)  Y-COORD (M)        ',A4,5X,'(YYMMDDHH)',&
   &/66(' -'))
9049 FORMAT(4X,'ORIGIN',60X,'ORIGIN',&
   &/5X,'SRCID        DIST (M)  DIR (DEG)       ',A4,&
   &5X,'(YYMMDDHH)',&
   &8X,'SRCID        DIST (M)  DIR (DEG)       ',A4,&
   &5X,'(YYMMDDHH)',&
   &/65(' -'))
9090 FORMAT(A132)
9095 FORMAT(132(' '))

   RETURN
END SUBROUTINE SPRTHT

SUBROUTINE PRTMAX
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: J, K, L, NPG, NROWS, JSTRT, II, J1, KMAX1, KMAX2,&
   &INDEXW
   DOUBLE PRECISION :: XR1, YR1, XR2, YR2
   CHARACTER :: NTY1*2, NTY2*2

!     Variable Initializations
   MODNAM = 'PRTMAX'

   DO IAVE = 1, NUMAVE
!        Check Array to See IF Maximum Values Are Needed For This AVEPER
      IF (MAXAVE(IAVE) /= 1) CYCLE

      DO IGRP = 1, NUMGRP
         IF (.NOT. PSDCREDIT) THEN
!              Fill Work Array With SRCIDs For This Group
            INDGRP = 0
            DO ISRC = 1, NUMSRC
               IF (IGROUP(ISRC,IGRP) == 1) THEN
                  INDGRP = INDGRP + 1
                  WORKID(INDGRP) = SRCID(ISRC)
               END IF
            END DO
         ELSE
!              Assign 'N/A' for source IDs for PSDCREDIT option
            INDGRP = 1
            WORKID(INDGRP) = 'N/A'
         END IF

! ---       Check for BACKGROUND "source" being included
!           in source group
         IF (GRP_BACK(IGRP)) THEN
            INDGRP = INDGRP + 1
            WORKID(INDGRP) = 'BACKGROUND'
!              Check for More Than 29 Sources Per Group
            INDEXW = MIN(29,NSRC+1)
         ELSE
            INDEXW = MIN(29,NSRC)
         END IF
!           Check for More Than 29 Sources Per Group
         IF (INDGRP > INDEXW) THEN
            WORKID(INDEXW) = ' . . . '
            INDGRP = INDEXW
         END IF

         IF (IMXVAL(IAVE) >= 2) THEN
!              Determine Number of Pages @ 80 Per Page, NPG
            NPG = 1 + INT((IMXVAL(IAVE)-1)/80)
            DO L = 1, NPG
!                 Determine Number of Rows for This Page, NROWS
               IF (L == NPG) THEN
                  NROWS = (IMXVAL(IAVE)-80*(L-1))/2
               ELSE
                  NROWS = 40
               END IF
!                 Write Out Header Information for This Page
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9032) IMXVAL(IAVE), CHRAVE(IAVE),&
               &(CHIDEP(II,ITYP),II=1,6), GRPID(IGRP), (WORKID(K),&
               &K = 1,INDGRP)
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
               &OUTLBL(ITYP)
               WRITE(IOUNIT,1) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
!                 Set Start Row of Loop for This Page, JSTRT
               JSTRT = 1 + 80*(L-1)
               DO J = JSTRT, JSTRT+NROWS-1
                  J1 = J + NROWS
                  IF (L==NPG .and. MOD(IMXVAL(IAVE),2)/=0) THEN
                     J1 = J1 + 1
                  END IF
                  KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
                  KMAX2 = MXLOCA(J1,IGRP,IAVE,ITYP)
                  IF (KMAX1 == 0) THEN
                     XR1 = 0.0D0
                     YR1 = 0.0D0
                     NTY1 = ' '
                  ELSE
                     XR1 = AXR(KMAX1)
                     YR1 = AYR(KMAX1)
                     NTY1 = RECTYP(KMAX1)
                  END IF
                  IF (KMAX2 == 0) THEN
                     XR2 = 0.0D0
                     YR2 = 0.0D0
                     NTY2 = ' '
                  ELSE
                     XR2 = AXR(KMAX2)
                     YR2 = AYR(KMAX2)
                     NTY2 = RECTYP(KMAX2)
                  END IF
                  WRITE(IOUNIT,2) J, RMXVAL(J,IGRP,IAVE,ITYP),&
                  &MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),&
                  &XR1, YR1, NTY1, J1,&
                  &RMXVAL(J1,IGRP,IAVE,ITYP), MCLMSG(J1,IGRP,IAVE,ITYP),&
                  &MXDATE(J1,IGRP,IAVE,ITYP), XR2, YR2, NTY2
               END DO
            END DO
            IF (MOD(IMXVAL(IAVE),2) /= 0) THEN
!                 Odd Number of Max Values - Print Out Last Value
               J = INT(IMXVAL(IAVE)/2) + 1 + 40*(NPG-1)
               KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
               IF (KMAX1 == 0) THEN
                  XR1 = 0.0D0
                  YR1 = 0.0D0
                  NTY1 = ' '
               ELSE
                  XR1 = AXR(KMAX1)
                  YR1 = AYR(KMAX1)
                  NTY1 = RECTYP(KMAX1)
               END IF
               WRITE(IOUNIT,3) J, RMXVAL(J,IGRP,IAVE,ITYP),&
               &MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),&
               &XR1, YR1, NTY1
            END IF
         ELSE
            J = 1
            KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
            IF (KMAX1 == 0) THEN
               XR1 = 0.0D0
               YR1 = 0.0D0
               NTY1 = '  '
            ELSE
               XR1 = AXR(KMAX1)
               YR1 = AYR(KMAX1)
               NTY1 = RECTYP(KMAX1)
            END IF
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,9032) IMXVAL(IAVE), CHRAVE(IAVE),&
            &(CHIDEP(II,ITYP),II=1,6), GRPID(IGRP), (WORKID(K),&
            &K = 1,INDGRP)
            WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT, OUTLBL(ITYP)
            WRITE(IOUNIT,1) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
            WRITE(IOUNIT,3) J, RMXVAL(J,IGRP,IAVE,ITYP),&
            &MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),&
            &XR1, YR1, NTY1
         END IF

!           WRITE Out Explanation of Receptor Types
         WRITE(IOUNIT,9050)

      END DO
   END DO

1  FORMAT(1X,'RANK',8X,A4,4X,'(YYMMDDHH) AT',6X,&
   &'RECEPTOR (XR,YR) OF TYPE ',3X,&
   &'RANK',8X,A4,4X,'(YYMMDDHH) AT',6X,&
   &'RECEPTOR (XR,YR) OF TYPE ',&
   &/66(' -'))
2  FORMAT(1X,I4,'.',1X,F13.5,A1,'(',I8.8,') AT',1X,&
   &'(',F10.2,', ',F10.2,')  ',A2,5X,&
   &I4,'.',1X,F13.5,A1,'(',I8.8,') AT',1X,&
   &'(',F10.2,', ',F10.2,')  ',A2)
3  FORMAT(1X,I4,'.',1X,F13.5,A1,'(',I8.8,') AT',1X,&
   &'(',F10.2,', ',F10.2,')  ',A2)
9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9032 FORMAT(/30X,'*** THE MAXIMUM ',I4,2X,A5,1X,6A4,&
   &'VALUES FOR SOURCE GROUP:',2X,A8,' ***'&
   &/34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
9050 FORMAT(/1X,' *** RECEPTOR TYPES:  GC = GRIDCART',&
   &/23X,'GP = GRIDPOLR',&
   &/23X,'DC = DISCCART',&
   &/23X,'DP = DISCPOLR')

   RETURN
END SUBROUTINE PRTMAX

SUBROUTINE PRTSUM(IOUNT)
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: IWHP(NVAL), I, IVAL, INDMX, IHST, INDLOC, IOUNT
   INTEGER :: IDEC, IMOD
   DOUBLE PRECISION :: AXR1, AYR1, AZELV1, AZHIL1, AZFLG1, XR2, YR2,&
   &ZE2, ZH2,ZF2
   CHARACTER :: PERCHR*6, RANK(10)*5, CHRVAL*5

!     Variable Initializations
   DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'PRTSUM'

   IF (PERIOD) THEN
      PERCHR = 'PERIOD'
   ELSE IF (ANNUAL) THEN
      PERCHR = 'ANNUAL'
   END IF

!     Print Maximum PERIOD Averages, If Appropriate
   IF (PERIOD .or. ANNUAL) THEN
!        Calculate Number of Groups Per Page, NGPP
      NGPP = MAX( 1, INT(50/(NHIANN+1)) )
      DO IGRP = 1, NUMGRP
         IF (MOD(IGRP-1, NGPP) == 0) THEN
            CALL HEADER(IOUNT)
            IF (PERIOD .and. MULTYR) THEN
               WRITE(IOUNT,9020) PERCHR, IANHRS, NUMYRS
            ELSE IF (PERIOD) THEN
               WRITE(IOUNT,9021) PERCHR, IANHRS
            ELSE IF (ANNUAL) THEN
               WRITE(IOUNT,9023) PERCHR, NUMYRS
            END IF
            WRITE(IOUNT,9011) CHIDEP(3,ITYP), POLLUT, PERLBL(ITYP)
            WRITE(IOUNT,9022) CHIDEP(1,ITYP), CHIDEP(2,ITYP),&
            &CHIDEP(3,ITYP)
         END IF
         DO IVAL = 1, NHIANN
            INDMX = IMXLOC(IVAL,IGRP,ITYP)
            IF (IVAL == 1 .and. INDMX /= 0) THEN
               WRITE(IOUNT,1012) GRPID(IGRP), RANK(IVAL),&
               &AMXVAL(IVAL,IGRP,ITYP), AXR(INDMX), AYR(INDMX),&
               &AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),&
               &RECTYP(INDMX), NETID(INDMX)
            ELSE IF (IVAL == 1 .and. INDMX == 0) THEN
               AXR1 = 0.0D0
               AYR1 = 0.0D0
               AZELV1 = 0.0D0
               AZHIL1 = 0.0D0
               AZFLG1 = 0.0D0
               WRITE(IOUNT,1014) GRPID(IGRP), RANK(IVAL),&
               &AMXVAL(IVAL,IGRP,ITYP), AXR1, AYR1, AZELV1, AZHIL1,&
               &AZFLG1
            ELSE IF (INDMX == 0) THEN
               AXR1 = 0.0D0
               AYR1 = 0.0D0
               AZELV1 = 0.0D0
               AZHIL1 = 0.0D0
               AZFLG1 = 0.0D0
               WRITE(IOUNT,1015) RANK(IVAL),&
               &AMXVAL(IVAL,IGRP,ITYP), AXR1, AYR1, AZELV1, AZHIL1,&
               &AZFLG1
            ELSE
               WRITE(IOUNT,1013) RANK(IVAL),&
               &AMXVAL(IVAL,IGRP,ITYP), AXR(INDMX), AYR(INDMX),&
               &AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),&
               &RECTYP(INDMX), NETID(INDMX)
            END IF
         END DO
      END DO
!        WRITE Out Explanation of Receptor Types
      WRITE(IOUNT,9050)
   END IF

! --- Skip "standard" summary of short-term averages for
!     PM25AVE, NO2AVE, or SO2AVE processing
   IF (PM25AVE .or. NO2AVE .or. SO2AVE) RETURN

! ---
!     Begin LOOP Through Averaging Periods
   DO IAVE = 1, NUMAVE
! ---    Determine which high ranked values are requested for
!        this averaging period
      IHST = 0
      DO IVAL = 1, NVAL
         IF (NHIAVE(IVAL,IAVE) == 1) THEN
            IHST = IHST + 1
            IWHP(IHST) = IVAL
         END IF
      END DO
      IF (IHST == 0) THEN
!           No High Values for This IAVE; Cycle to Next Averaging Period
         CYCLE
      END IF
!        Calculate Number of Groups Per Page, NGPP
      NGPP = MAX( 1, INT(50/(IHST+1)) )

!        Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
!           Begin LOOP Through High Values
         DO I = 1, IHST

! ---          Assign character label for rank
            IF (IWHP(I) <= 10) THEN
               CHRVAL = RANK(IWHP(I))
            ELSE IF (MOD(IWHP(I),100) > 10 .and.&
            &MOD(IWHP(I),100) < 20) THEN
               IDEC = INT(IWHP(I)/10)
               IMOD = MOD(IWHP(I),10)
               WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
            ELSE IF (IWHP(I) <= 999) THEN
               IDEC = INT(IWHP(I)/10)
               IMOD = MOD(IWHP(I),10)
               IF (IMOD == 0) IMOD = 10
               WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
            END IF

            INDLOC = HMLOC(I,IGRP,IAVE,ITYP)
            IF (I == 1) THEN
               IF (MOD(IGRP-1,NGPP) == 0) THEN
                  CALL HEADER(IOUNT)
                  IF (MULTYR) THEN
                     WRITE(IOUNT,9030) CHRAVE(IAVE), NUMYRS
                  ELSE
                     WRITE(IOUNT,9031) CHRAVE(IAVE)
                  END IF
                  WRITE(IOUNT,9011) CHIDEP(3,ITYP),POLLUT,&
                  &OUTLBL(ITYP)
                  WRITE(IOUNT,9032) CHIDEP(1,ITYP),&
                  &CHIDEP(2,ITYP),CHIDEP(3,ITYP)
               END IF
               WRITE(IOUNT,*) ' '
               IF (INDLOC == 0) THEN
                  XR2 = 0.0D0
                  YR2 = 0.0D0
                  ZE2 = 0.0D0
                  ZH2 = 0.0D0
                  ZF2 = 0.0D0
                  WRITE(IOUNT,1004) GRPID(IGRP), CHRVAL,&
                  &HMAX(I,IGRP,IAVE,ITYP),&
                  &HMCLM(I,IGRP,IAVE,ITYP),&
                  &HMDATE(I,IGRP,IAVE,ITYP),&
                  &XR2, YR2, ZE2, ZH2, ZF2
               ELSE
                  XR2 = AXR(INDLOC)
                  YR2 = AYR(INDLOC)
                  ZE2 = AZELEV(INDLOC)
                  ZH2 = AZHILL(INDLOC)
                  ZF2 = AZFLAG(INDLOC)
                  WRITE(IOUNT,1002) GRPID(IGRP), CHRVAL,&
                  &HMAX(I,IGRP,IAVE,ITYP),&
                  &HMCLM(I,IGRP,IAVE,ITYP),&
                  &HMDATE(I,IGRP,IAVE,ITYP),&
                  &XR2, YR2, ZE2, ZH2, ZF2, RECTYP(INDLOC),&
                  &NETID(INDLOC)
               END IF
            ELSE
               IF (INDLOC == 0) THEN
                  XR2 = 0.0D0
                  YR2 = 0.0D0
                  ZE2 = 0.0D0
                  ZF2 = 0.0D0
                  WRITE(IOUNT,1005) CHRVAL,&
                  &HMAX(I,IGRP,IAVE,ITYP),&
                  &HMCLM(I,IGRP,IAVE,ITYP),&
                  &HMDATE(I,IGRP,IAVE,ITYP),&
                  &XR2, YR2, ZE2, ZH2, ZF2
               ELSE
                  XR2 = AXR(INDLOC)
                  YR2 = AYR(INDLOC)
                  ZE2 = AZELEV(INDLOC)
                  ZH2 = AZHILL(INDLOC)
                  ZF2 = AZFLAG(INDLOC)
                  WRITE(IOUNT,1003) CHRVAL,&
                  &HMAX(I,IGRP,IAVE,ITYP),&
                  &HMCLM(I,IGRP,IAVE,ITYP),&
                  &HMDATE(I,IGRP,IAVE,ITYP),&
                  &XR2, YR2, ZE2, ZH2, ZF2, RECTYP(INDLOC),&
                  &NETID(INDLOC)
               END IF
            END IF
         END DO
      END DO

!        WRITE Out Explanation of Receptor Types
      WRITE(IOUNT,9050)

!     End loop through averaging periods
   END DO

1002 FORMAT(A8,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',&
   &I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')',&
   &2X,A2,2X,A8)
1003 FORMAT(8X,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',&
   &I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')',&
   &2X,A2,2X,A8)
1004 FORMAT(A8,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',&
   &I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
1005 FORMAT(8X,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',&
   &I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
1012 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
1013 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
1014 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
1015 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
9011 FORMAT(/36X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9020 FORMAT(/40X,'*** THE SUMMARY OF MAXIMUM ',A6,' (',I6,&
   &' HRS) RESULTS ***',/40X,'***    ACROSS ',I6,&
   &' YEARS WITH THE MULTYEAR OPTION    ***'/)
9021 FORMAT(/40X,'*** THE SUMMARY OF MAXIMUM ',A6,' (',I6,&
   &' HRS) RESULTS ***'/)
9023 FORMAT(/35X,'*** THE SUMMARY OF MAXIMUM ',A6,' RESULTS ',&
   &'AVERAGED OVER ',I3,' YEARS ***'/)
9022 FORMAT(109X,'NETWORK',/,'GROUP ID',23X,3A4,&
   &16X,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',2X,'OF TYPE',&
   &2X,'GRID-ID',/60('- '))
9030 FORMAT(/48X,'*** THE SUMMARY OF HIGHEST ',A5,' RESULTS ***'/,&
   &44X,'*** ACROSS ',I6,&
   &' YEARS WITH THE MULTYEAR OPTION ***'/)
9031 FORMAT(/48X,'*** THE SUMMARY OF HIGHEST ',A5,' RESULTS ***'/)
9032 FORMAT(54X,'DATE',68X,'NETWORK',/,'GROUP ID',26X,3A4,5X,&
   &'(YYMMDDHH)',13X,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',&
   &4X,'OF TYPE',2X,'GRID-ID',/66('- '))
9050 FORMAT(//' *** RECEPTOR TYPES:  GC = GRIDCART',&
   &/22X,'GP = GRIDPOLR',&
   &/22X,'DC = DISCCART',&
   &/22X,'DP = DISCPOLR')

   RETURN
END SUBROUTINE PRTSUM

SUBROUTINE EVEFIL
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: IAVEP
   DOUBLE PRECISION :: CONC1
   CHARACTER :: EVFRM*20
   LOGICAL :: HITIN

!     Variable Initializations
   MODNAM = 'EVEFIL'
   HITIN  = .FALSE.
   EOF    = .FALSE.

!     Setup WRITE format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   WRITE(EVFRM,9300) ISTRG
9300 FORMAT('(A',I4.4,')')

!     Rewind Temporary Event File
   REWIND ITEVUT

!     Read Records From The Temporary Event File
   DO WHILE (.NOT. EOF)
      IF (.NOT. HITIN) THEN
!           Not in the Event Pathway - Echo Input to EVENT File
         READ(ITEVUT,EVFRM,END=999) RUNST1
         IF (RUNST1(1:11) == 'EV STARTING') THEN
!              Event Pathway Starts - Set Logical Switch
            HITIN = .TRUE.
            IF (LOCB(1) == 1) THEN
               WRITE(IEVUNT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
            ELSE IF (LOCB(1) == 2) THEN
               WRITE(IEVUNT,'(1x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
            ELSE IF (LOCB(1) == 3) THEN
               WRITE(IEVUNT,'(2x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
            ELSE IF (LOCB(1) == 4) THEN
               WRITE(IEVUNT,'(3x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
            END IF
         ELSE
            WRITE(IEVUNT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
         END IF
      ELSE
         READ(ITEVUT,EVFRM,END=999) RUNST1
         IF (RUNST1(1:11) == 'EV FINISHED') THEN
            IF (MXFILE) THEN
!                 Add Events From Max Value (>Thresh) Files ---   CALL MXEVNT
               CALL MXEVNT
            END IF
            IF (LOCB(1) == 1) THEN
               WRITE(IEVUNT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
            ELSE IF (LOCB(1) == 2) THEN
               WRITE(IEVUNT,'(1x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
            ELSE IF (LOCB(1) == 3) THEN
               WRITE(IEVUNT,'(2x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
            ELSE IF (LOCB(1) == 4) THEN
               WRITE(IEVUNT,'(3x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
            END IF
            HITIN = .FALSE.
         END IF
         IF (HITIN .and. RUNST1(LOCB(1):LOCB(1)+10) ==&
         &'   EVENTPER') THEN
            READ(RUNST1(LOCB(1)+23:),'(I3)') IAVEP
            READ(RUNST1(LOCB(1)+47:),'(F18.5)',ERR=99) CONC1
         END IF

         GO TO 100

!           Write Out Warning Message:  Error Reading CONC From TmpEvent File
99       CALL ERRHDL(PATH,MODNAM,'W','570',&
         &RUNST1(LOCB(1)+12:LOCB(1)+19))
!           Set CONC1 To Large Value for Event File
         CONC1 = 1.0D9

100      CONTINUE
         IF (HITIN.and. IAVEP/=720 .and. CONC1/=0.0D0) THEN
!              Write Out EVENTPER & EVENTLOC Cards, Allowing for Column Shift
            WRITE(IEVUNT,'(a:)') RUNST1(LOCB(1):LEN_TRIM(RUNST1))
         END IF
      END IF

      GO TO 11

999   EOF = .TRUE.
11    CONTINUE
   END DO

!     Write OU Pathway Images to EVENT File, Allowing For Column Shift
   IF (LOCB(1) == 1) THEN
      WRITE(IEVUNT,1011) EVPARM
   ELSE IF (LOCB(1) == 2) THEN
      WRITE(IEVUNT,1012) EVPARM
   ELSE IF (LOCB(1) == 3) THEN
      WRITE(IEVUNT,1013) EVPARM
   ELSE IF (LOCB(1) == 4) THEN
      WRITE(IEVUNT,1014) EVPARM
   END IF

   CLOSE(UNIT=IEVUNT)

1011 FORMAT(/'OU STARTING',&
   &/'   EVENTOUT  ',A6,&
   &/'OU FINISHED')
1012 FORMAT(/' OU STARTING',&
   &/'    EVENTOUT  ',A6,&
   &/' OU FINISHED')
1013 FORMAT(/'  OU STARTING',&
   &/'     EVENTOUT  ',A6,&
   &/'  OU FINISHED')
1014 FORMAT(/'   OU STARTING',&
   &/'      EVENTOUT  ',A6,&
   &/'   OU FINISHED')

   RETURN
END SUBROUTINE EVEFIL

SUBROUTINE MXEVNT
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: IAVEP, KDATE
   DOUBLE PRECISION :: CONC1, XR2, YR2, ZE2, ZH2, ZF2
   CHARACTER :: NAMEEV*10, GID*8, BUFIN*90

!     Variable Initializations
   MODNAM = 'MXEVNT'

!     Begin Averaging Period LOOP
   DO IAVE = 1, NUMAVE
!        Initialize Event Counter for This IAVE
      NUMEVE = 0
!        Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
         IF (MAXFLE(IGRP,IAVE) == 1) THEN
!              Maximum Value File Exists for This Group and AvePer
!              Rewind File
            REWIND IMXUNT(IGRP,IAVE)
            EOF = .FALSE.

!              Loop Through Threshold File and Write Out Events to EVENT File
            DO WHILE (.NOT. EOF)
               READ(IMXUNT(IGRP,IAVE),100,ERR=99,END=999) BUFIN
100            FORMAT(A90)
!                 Skip Record if Part of Header, '*' in Column 1
               IF (BUFIN(1:1) == '*') GO TO 11
               READ(BUFIN,THRFRM,ERR=99) IAVEP,&
               &GID, KDATE, XR2, YR2, ZE2, ZH2, ZF2, CONC1
               IF (IAVEP/=720 .and. IAVEP==KAVE(IAVE) .and.&
               &GID==GRPID(IGRP)) THEN
!                    Increment Event Counter and Generate Event Name
                  NUMEVE = NUMEVE + 1

                  IF (NUMEVE > 999999) THEN
!                       Number of Events Exceeds Limit of Field,
!                       Write Warning Message and Reset to 1
                     WRITE(DUMMY,'(3X,I2.2,3X)') IAVEP
                     CALL ERRHDL(PATH,MODNAM,'W','413',DUMMY)
                     NUMEVE = 1
                  END IF

                  WRITE(NAMEEV,'("TH",I2.2,I6.6)') IAVEP, NUMEVE
!                    Write EVENTPER & EVENTLOC Cards, Allowing for Col. Shift
                  IF (LOCB(1) == 1) THEN
                     WRITE(IEVUNT,1901) NAMEEV,IAVEP,GID,KDATE,CONC1
                     WRITE(IEVUNT,1911) NAMEEV, XR2, YR2, ZE2, ZH2,&
                     &ZF2
                  ELSE IF (LOCB(1) == 2) THEN
                     WRITE(IEVUNT,1902) NAMEEV,IAVEP,GID,KDATE,CONC1
                     WRITE(IEVUNT,1912) NAMEEV, XR2, YR2, ZE2, ZH2,&
                     &ZF2
                  ELSE IF (LOCB(1) == 3) THEN
                     WRITE(IEVUNT,1903) NAMEEV,IAVEP,GID,KDATE,CONC1
                     WRITE(IEVUNT,1913) NAMEEV, XR2, YR2, ZE2, ZH2,&
                     &ZF2
                  ELSE IF (LOCB(1) == 4) THEN
                     WRITE(IEVUNT,1904) NAMEEV,IAVEP,GID,KDATE,CONC1
                     WRITE(IEVUNT,1914) NAMEEV, XR2, YR2, ZE2, ZH2,&
                     &ZF2
                  END IF
                  GO TO 11
               ELSE
                  GO TO 11
               END IF

999            EOF = .TRUE.
11             CONTINUE
            END DO

         END IF
      END DO
!        End Source Group LOOP
   END DO
!     End Averaging Period LOOP

   GO TO 1000

!     WRITE Error Message for Error Reading Threshold File
99 WRITE(DUMMY,'("MAXFL",I3.3)') IMXUNT(IGRP,IAVE)
   CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

1901 FORMAT(3X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
1902 FORMAT(4X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
1903 FORMAT(5X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
1904 FORMAT(6X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
1911 FORMAT(3X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,&
   &3(1X,F10.4))
1912 FORMAT(4X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,&
   &3(1X,F10.4))
1913 FORMAT(5X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,&
   &3(1X,F10.4))
1914 FORMAT(6X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,&
   &3(1X,F10.4))

1000 RETURN
END SUBROUTINE MXEVNT

SUBROUTINE PRTPM25
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, K, II, INDZ, INDC, NX, NY, INDEXW, N
   INTEGER :: IDEC, IMOD
   DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
   CHARACTER :: BUF132*132

   CHARACTER :: RANK(10)*5, CHRVAL*5

!     Variable Initializations
   DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'PRTPM25'
   BUF132 = ' '
   INDZ   = 0

!     Write Out the 'EV STARTING' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   IF (ITYP == 1) THEN
      WRITE(ITEVUT,9000)
   END IF

! --- Loop through ranks
   DO N = 1, NVAL

!      If this rank not applicable, cycle to next rank
      IF( NHIAVE(N,1) /= 1 ) CYCLE

! ---  Loop through source groups
      DO IGRP = 1, NUMGRP

         IF (.NOT. PSDCREDIT) THEN
!           Fill Work Array With SRCIDs For This Group
            INDGRP = 0
            DO ISRC = 1, NUMSRC
               IF (IGROUP(ISRC,IGRP) == 1) THEN
                  INDGRP = INDGRP + 1
                  WORKID(INDGRP) = SRCID(ISRC)
               END IF
            END DO
         ELSE
!           Assign 'N/A' for source IDs for PSDCREDIT option
            INDGRP = 1
            WORKID(INDGRP) = 'N/A'
         END IF

! ---    Check for BACKGROUND "source" being included
!        in source group
         IF (GRP_BACK(IGRP)) THEN
            INDGRP = INDGRP + 1
            WORKID(INDGRP) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
            INDEXW = MIN(29,NSRC+1)
         ELSE
            INDEXW = MIN(29,NSRC)
         END IF
!        Check for More Than 29 Sources Per Group
         IF (INDGRP > INDEXW) THEN
            WORKID(INDEXW) = ' . . . '
            INDGRP = INDEXW
         END IF

!        Print Receptor Network Coordinates:
!        Set Number of Columns Per Page, NCPP
         NCPP = 9
!        Set Number of Rows Per Page, NRPP
         NRPP = 40
!        Begin LOOP Through Networks
         DO I = 1, INNET
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  IF (NHIAVE(N,1) == 1) THEN
! ---                Assign character label for rank
                     IF (N <= 10) THEN
                        CHRVAL = RANK(N)
                     ELSE IF (MOD(N,100) > 10 .and.&
                     &MOD(N,100) < 20) THEN
                        IDEC = INT(N/10)
                        IMOD = MOD(N,10)
                        WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                     ELSE IF (N <= 999) THEN
                        IDEC = INT(N/10)
                        IMOD = MOD(N,10)
                        IF (IMOD == 0) IMOD = 10
                        WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                     END IF

                     IF (NO2AVE .or. SO2AVE) THEN
                        WRITE(IOUNIT,90321) CHRVAL,&
                        &(CHIDEP(II,ITYP),II=1,6),NUMYRS,&
                        &GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     ELSE
                        WRITE(IOUNIT,9032) CHRVAL, CHRAVE(1),&
                        &(CHIDEP(II,ITYP),II=1,6),NUMYRS,&
                        &GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     END IF
                  END IF
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
!                 Print The Values By Source Group
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,PERLBL(ITYP)
                  IF (NX == NPPX) THEN
                     IF (NTTYP(I) == 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                        &NUMXPT(I))
                     ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                        &NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) == 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                        &NCPP*NX)
                     ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),&
                        &NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY == NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) == 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX == NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,&
                           &(SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,&
                           &(SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) == 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) == 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX == NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,&
                           &(SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,&
                           &(SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END DO
!        End LOOP Through Networks

         IF (IRSTAT(4)/=0 .or. IRSTAT(8)/=0) THEN
! ---       Include EVALCART receptors with DISCCART receptors.
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) == 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) == 0) THEN
                     CALL HEADER(IOUNIT)
                     IF (NHIAVE(N,1) == 1) THEN
! ---                   Assign character label for rank
                        IF (N <= 10) THEN
                           CHRVAL = RANK(N)
                        ELSE IF (MOD(N,100) > 10 .and.&
                        &MOD(N,100) < 20) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                        ELSE IF (N <= 999) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           IF (IMOD == 0) IMOD = 10
                           WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                        END IF

                        IF (NO2AVE .or. SO2AVE) THEN
                           WRITE(IOUNIT,90321) CHRVAL,&
                           &(CHIDEP(II,ITYP),II=1,6),&
                           &NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        ELSE
                           WRITE(IOUNIT,9032) CHRVAL, CHRAVE(1),&
                           &(CHIDEP(II,ITYP),II=1,6),&
                           &NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        END IF
                     END IF
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                     &PERLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) /= 0) THEN
                     WRITE(BUF132(1:60),9045) AXR(IREC), AYR(IREC),&
                     &SUMHNH(IREC,IGRP,N)
                  ELSE
                     WRITE(BUF132(61:120),9045) AXR(IREC), AYR(IREC),&
                     &SUMHNH(IREC,IGRP,N)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
            END DO
            IF (MOD(INDC,2) /= 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

         IF (IRSTAT(5) /= 0) THEN
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) == 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = DATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR <= 0.0D0) DIR = DIR + 360.0D0
                  IF (MOD(INDC-1,80) == 0) THEN
                     CALL HEADER(IOUNIT)
                     IF (NHIAVE(N,1) == 1) THEN
! ---                   Assign character label for rank
                        IF (N <= 10) THEN
                           CHRVAL = RANK(N)
                        ELSE IF (MOD(N,100) > 10 .and.&
                        &MOD(N,100) < 20) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                        ELSE IF (N <= 999) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           IF (IMOD == 0) IMOD = 10
                           WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                        END IF

                        IF (NO2AVE .or. SO2AVE) THEN
                           WRITE(IOUNIT,90321) CHRVAL,&
                           &(CHIDEP(II,ITYP),II=1,6),&
                           &NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        ELSE
                           WRITE(IOUNIT,9032) CHRVAL, CHRAVE(1),&
                           &(CHIDEP(II,ITYP),II=1,6),&
                           &NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        END IF
                     END IF
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,&
                     &PERLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) /= 0) THEN
                     WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)), DIST,&
                     &DIR, SUMHNH(IREC,IGRP,N)
                  ELSE
                     WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)), DIST,&
                     &DIR, SUMHNH(IREC,IGRP,N)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
            END DO
            IF (MOD(INDC,2) /= 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

      END DO   ! End loop on source groups
   END DO    ! End loop on ranks

!     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
!     First Output Type Only (i.e., ITYP = 1)
   IF (ITYP == 1) THEN
      WRITE(ITEVUT,9009)
   END IF

9000 FORMAT('EV STARTING')
9009 FORMAT('EV FINISHED')
9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9010 FORMAT(66(' -')/)
9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
9032 FORMAT(/13X,'*** THE ',A5,'-HIGHEST ',A5,1X,6A4,' VALUES ',&
   &'AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP:',1X,A8,' ***',&
   &/34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
90321 FORMAT(/4X,'*** THE ',A5,'-HIGHEST MAX DAILY 1-HR ',6A4,&
   &' VALUES ',&
   &'AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP:',1X,A8,' ***',&
   &/34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),&
   &/17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',&
   &A8,' ***')
9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 FORMAT(6X,2(F12.2,2X),F13.5)
9047 FORMAT(2X,A12,': ',F12.2,2X,F10.2,2X,F13.5)
9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,&
   &22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',&
   &/5X,' SRCID         DIST (M)   DIR (DEG)        ',A4,&
   &18X,' SRCID         DIST (M)   DIR (DEG)        ',A4,&
   &/65(' -'))
9090 FORMAT(A132)
9095 FORMAT(132(' '))

   RETURN
END SUBROUTINE PRTPM25

SUBROUTINE MAXPM25
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: J, N

!     Variable Initializations
   MODNAM = 'MAXPM25'

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Begin loop through ranks
      DO N = 1, NVAL
!           Cycle to next rank if this rank is not applicable
         IF( NHIAVE(N,1) /= 1 ) CYCLE
!           Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
            IF (NMXPM > 1) THEN
               IF (SUMHNH(IREC,IGRP,N) >&
               &MXPMVAL(NMXPM,IGRP,N)) THEN
                  DO J = NMXPM-1, 1, -1
                     IF(SUMHNH(IREC,IGRP,N) <=&
                     &MXPMVAL(J,IGRP,N)) THEN
                        MXPMVAL(J+1,IGRP,N) = SUMHNH(IREC,IGRP,N)
                        MXPMLOC(J+1,IGRP,N) = IREC
!                          Exit Block
                        CYCLE RECEPTOR_LOOP
                     ELSE
                        MXPMVAL(J+1,IGRP,N) = MXPMVAL(J,IGRP,N)
                        MXPMLOC(J+1,IGRP,N) = MXPMLOC(J,IGRP,N)
                        IF (J == 1) THEN
                           MXPMVAL(1,IGRP,N) = SUMHNH(IREC,IGRP,N)
                           MXPMLOC(1,IGRP,N) = IREC
                        END IF
                     END IF
                  END DO
               END IF
            ELSE IF (NMXPM == 1) THEN
               IF (SUMHNH(IREC,IGRP,N) > MXPMVAL(1,IGRP,N)) THEN
                  MXPMVAL(1,IGRP,N) = SUMHNH(IREC,IGRP,N)
                  MXPMLOC(1,IGRP,N) = IREC
               END IF
            END IF
         END DO RECEPTOR_LOOP
!           End Receptor LOOP
      END DO
!        End loop through ranks
   END DO
!     End Source Group LOOP

   RETURN
END SUBROUTINE MAXPM25

SUBROUTINE PRTPM25SUM(IOUNT)
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IVAL, INDMX, IOUNT, N
   INTEGER :: IDEC, IMOD
   DOUBLE PRECISION :: AXR1, AYR1, AZELV1, AZHIL1, AZFLG1
   CHARACTER :: RANK(10)*5, CHRVAL*5

!     Variable Initializations
   DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'PRTPM25SUM'

   IF (NUMAVE == 1) THEN
!        Calculate Number of Groups Per Page, NGPP
      NGPP = MAX( 1, INT(50/(NMXPM+1)) )
      DO N = 1, NVAL
         IF (NHIAVE(N,1) /= 1) CYCLE
         DO IGRP = 1, NUMGRP
            IF (MOD(IGRP-1, NGPP) == 0) THEN
               CALL HEADER(IOUNT)
! ---             Assign character label for rank
               IF (N <= 10) THEN
                  CHRVAL = RANK(N)
               ELSE IF (MOD(N,100) > 10 .and.&
               &MOD(N,100) < 20) THEN
                  IDEC = INT(N/10)
                  IMOD = MOD(N,10)
                  WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
               ELSE IF (N <= 999) THEN
                  IDEC = INT(N/10)
                  IMOD = MOD(N,10)
                  IF (IMOD == 0) IMOD = 10
                  WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
               END IF

               IF (PM25AVE) THEN
                  WRITE(IOUNT,9091) CHRVAL, CHRAVE(1), NUMYRS
               ELSE IF (NO2AVE .or. SO2AVE) THEN
                  WRITE(IOUNT,99091) CHRVAL, CHRAVE(1), NUMYRS
               END IF
               WRITE(IOUNT,9011) CHIDEP(3,ITYP), POLLUT, OUTLBL(ITYP)
               WRITE(IOUNT,9022) CHIDEP(1,ITYP), CHIDEP(2,ITYP),&
               &CHIDEP(3,ITYP)
            END IF
            DO IVAL = 1, NMXPM
               INDMX = MXPMLOC(IVAL,IGRP,N)
               IF (IVAL == 1 .and. INDMX /= 0) THEN
                  WRITE(IOUNT,1012) GRPID(IGRP), RANK(IVAL),&
                  &MXPMVAL(IVAL,IGRP,N), AXR(INDMX), AYR(INDMX),&
                  &AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),&
                  &RECTYP(INDMX), NETID(INDMX)
               ELSE IF (IVAL == 1 .and. INDMX == 0) THEN
                  AXR1 = 0.0D0
                  AYR1 = 0.0D0
                  AZELV1 = 0.0D0
                  AZHIL1 = 0.0D0
                  AZFLG1 = 0.0D0
                  WRITE(IOUNT,1014) GRPID(IGRP), RANK(IVAL),&
                  &MXPMVAL(IVAL,IGRP,N), AXR1, AYR1, AZELV1,&
                  &AZHIL1, AZFLG1
               ELSE IF (INDMX == 0) THEN
                  AXR1 = 0.0D0
                  AYR1 = 0.0D0
                  AZELV1 = 0.0D0
                  AZHIL1 = 0.0D0
                  AZFLG1 = 0.0D0
                  WRITE(IOUNT,1015) RANK(IVAL),&
                  &MXPMVAL(IVAL,IGRP,N), AXR1, AYR1, AZELV1,&
                  &AZHIL1, AZFLG1
               ELSE
                  WRITE(IOUNT,1013) RANK(IVAL),&
                  &MXPMVAL(IVAL,IGRP,N), AXR(INDMX), AYR(INDMX),&
                  &AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),&
                  &RECTYP(INDMX), NETID(INDMX)
               END IF
            END DO
         END DO
      END DO
!        WRITE Out Explanation of Receptor Types
      WRITE(IOUNT,9050)
   END IF

1012 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
1013 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
1014 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
1015 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',&
   &'(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
9011 FORMAT(/36X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
9091 FORMAT(/27X,'*** THE SUMMARY OF MAXIMUM ',A5,'-HIGHEST ',&
   &A5,' RESULTS AVERAGED OVER ',I3,' YEARS ***'/)
99091 FORMAT(/22X,'*** THE SUMMARY OF MAXIMUM ',A5,'-HIGHEST ',&
   &'MAX DAILY ',A5,' RESULTS AVERAGED OVER ',I3,' YEARS ***'/)
9022 FORMAT(109X,'NETWORK',/'GROUP ID',23X,3A4,&
   &16X,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',2X,'OF TYPE',&
   &2X,'GRID-ID',/60('- '))
9050 FORMAT(//' *** RECEPTOR TYPES:  GC = GRIDCART',&
   &/22X,'GP = GRIDPOLR',&
   &/22X,'DC = DISCCART',&
   &/22X,'DP = DISCPOLR')

   RETURN
END SUBROUTINE PRTPM25SUM

SUBROUTINE SHOUT
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

! Unused:      INTEGER :: I
   CHARACTER :: HDRFRM*400, SEAFRM*80

!     Variable Initializations
   MODNAM = 'SHOUT'

!     Create Header Format for Columns Based on Number of Output Types
   WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

! --- Generate ouptut format based on number of output types and
!     file format ('FIX' or 'EXP')
   IF (FILE_FORMAT == 'FIX') THEN
!        Use FIXed format (F13.8) for concentrations and fluxes
      WRITE(SEAFRM,1009) NUMTYP
1009  FORMAT('(2(1X,F13.5),',I1,'(1X,F13.8),3(1X,F7.2),2X,A8,2X,',&
      &'3(I4,2X),A8)')
   ELSE IF (FILE_FORMAT == 'EXP') THEN
!        Use EXPonential format (E13.6) for concentrations and fluxes
      WRITE(SEAFRM,1010) NUMTYP
1010  FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F7.2),2X,A8,2X,',&
      &'3(I4,2X),A8)')
   END IF

!     Begin Source Group LOOP
   DO IGRP = 1, NUMGRP
!        Check for Selection of SEASONHR File for This Group
      IF (ISEAHR(IGRP) == 1) THEN
         IF (.NOT. L_NoHeader(4)) THEN
!              Write Header Information
            WRITE(ISHUNT(IGRP),9005) VERSN, TITLE1(1:68), RUNDAT
            WRITE(ISHUNT(IGRP),9007) C_METVER, RUNTIM,&
            &MODOPS_String(1:LEN_TRIM(MODOPS_String))
            WRITE(ISHUNT(IGRP),9010) GRPID(IGRP), NUMREC, SEAFRM
            WRITE(ISHUNT(IGRP),HDRFRM)(CHIDEP(1,ITYP),CHIDEP(2,ITYP),&
            &CHIDEP(3,ITYP),ITYP=1,NUMTYP)
         END IF
! ---       Write data
         DO ISEAS = 1, 4
            DO IHOUR = 1, 24
!                 Begin Receptor LOOP
               DO IREC = 1, NUMREC
                  INUM = NSEAHR(ISEAS,IHOUR) - NSEACM(ISEAS,IHOUR)
                  WRITE(ISHUNT(IGRP),SEAFRM,ERR=99)&
                  &AXR(IREC), AYR(IREC),&
                  &(SHVALS(IREC,IGRP,ISEAS,IHOUR,ITYP),ITYP=1,NUMTYP),&
                  &AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),&
                  &GRPID(IGRP), INUM, ISEAS, IHOUR, NETID(IREC)
               END DO
!                 End Receptor LOOP
            END DO
         END DO
      END IF
   END DO
!     End Source Group LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to Plot File
99 WRITE(DUMMY,'("SHFIL",I3.3)') ISHUNT(IGRP)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
9007 FORMAT('* AERMET (',A6,'):',T93,A8,&
   &/'* MODELING OPTIONS USED: ',A:)
9010 FORMAT('*',9X,'FILE OF SEASON/HOUR VALUES FOR ',&
   &'SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',&
   &/'*',9X,'FORMAT: ',A:)
9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',&
   &  3X,''ZHILL'',3X,''ZFLAG'',4X,''GRP'',5X,''NHRS'',2X,''SEAS'',&
   &  2X,''HOUR'',3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),&
   &  3('' ______ ''),'' ________  ____  ____  ____  ________'')')

999 RETURN
END SUBROUTINE SHOUT

SUBROUTINE RANKFL
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, J, IRANK, IDATSV(NMXVAL)
   LOGICAL :: FOUND

!     Variable Initializations
   MODNAM = 'RANKFL'

!     Begin Averaging Period LOOP
   DO IAVE = 1, NUMAVE
!        Decide if we should go through the processing
      IF (IRNKFL(IAVE) == 1) THEN
         IF (.NOT. L_NoHeader(5)) THEN
!              Write Header to File
            WRITE(IRKUNT(IAVE),9005) VERSN, TITLE1(1:68), RUNDAT
            WRITE(IRKUNT(IAVE),9007) C_METVER, RUNTIM,&
            &MODOPS_String(1:LEN_TRIM(MODOPS_String))
            WRITE(IRKUNT(IAVE),9010) IRKVAL(IAVE), CHRAVE(IAVE),&
            &NUMGRP, RNKFRM
            WRITE(IRKUNT(IAVE),9020) CHIDEP(1,ITYP), CHIDEP(2,ITYP),&
            &CHIDEP(3,ITYP)
         END IF

!           Begin Source Group LOOP
         DO IGRP = 1, NUMGRP
!              Initialize IDATSV array, which saves dates of ranked values
            IDATSV(:) = 0
            IRANK = 0
!              Begin LOOP Through Max Values
            DO I = 1, IRKVAL(IAVE)
               FOUND = .FALSE.
               DO J = 1, NMXVAL
                  IF (MXDATE(I,IGRP,IAVE,ITYP) == IDATSV(J)) THEN
                     FOUND = .TRUE.
                     EXIT
                  END IF
               END DO
               IF (.NOT.FOUND .and. IRANK<IRKVAL(IAVE)) THEN
                  IRANK = IRANK + 1
                  IREC  = MXLOCA(I,IGRP,IAVE,ITYP)
                  IDATSV(IRANK) = MXDATE(I,IGRP,IAVE,ITYP)
                  WRITE(IRKUNT(IAVE),RNKFRM,ERR=99) IRANK,&
                  &RMXVAL(I,IGRP,IAVE,ITYP), MXDATE(I,IGRP,IAVE,ITYP),&
                  &AXR(IREC), AYR(IREC), AZELEV(IREC), AZHILL(IREC),&
                  &AZFLAG(IREC), GRPID(IGRP)
               END IF
            END DO
!              End LOOP Through Max Values
         END DO
!           End Source Group LOOP
      END IF
   END DO
!     End Averaging Period LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to RANK File
99 WRITE(DUMMY,'("RNKFL",I3.3)') IRKUNT(IAVE)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
9007 FORMAT('* AERMET (',A6,'):',T93,A8,&
   &/'* MODELING OPTIONS USED: ',A:)
9010 FORMAT('*',9X,'RANK-FILE OF UP TO ',I5,' TOP ',A5,' VALUES ',&
   &'FOR ',I5,' SOURCE GROUPS',&
   &/'*',9X,'INCLUDES OVERALL MAXIMUM VALUES WITH DUPLICATE ',&
   &'DATA PERIODS REMOVED',&
   &/'*',9X,'FORMAT: ',A:)
9020 FORMAT('*  RANK',2X,3A4,3X,'DATE',10X,'X',13X,'Y',8X,'ZELEV',&
   &3X,'ZHILL',3X,'ZFLAG',5X,'GRP',&
   &/'*_______',1X,'____________',1X,'________',&
   &2(2X,'____________'),3(2X,'______'),'  ________')

999 RETURN
END SUBROUTINE RANKFL

SUBROUTINE MAXDCALC
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
   USE MAIN1
   IMPLICIT NONE

   CHARACTER :: MODNAM*12
! Unused:      CHARACTER (LEN=6) :: SPEC1, SPEC2, SPEC3
! Unused:      CHARACTER (LEN=8) :: CUSI, CSSI, COSI
! Unused:      INTEGER :: METVER, IOSI, ISSI, IUSI
!     JAT D065, 8/9/21 FOPEN SET BUT NOT USED
!     ILSAVE IS SET BUT NEVER USED
!      LOGICAL :: FOPEN
   INTEGER :: ILSAVE
! Unused:      DOUBLE PRECISION :: RDUM
! Unused:      DOUBLE PRECISION :: O3VALUES(24), O3MIN, O3MAX24
! Unused:       INTEGER :: I
!***** ARM2 Modification ******
! Unused:      INTEGER :: Itempp
!******************************

!     Variable Initializations
   MODNAM = 'MAXDCALC'
   EOF   = .FALSE.
!     JAT D065, 8/9/21 FOPEN SET BUT NOT USED
!      FOPEN = .FALSE.
! --- Reset RSTSAV to false for MAXDCALC
   RSTSAV   = .FALSE.

! --- Set IAVE = 1
   IAVE = 1

! --- Extract met data from arrays
   CALL MAXD_METEXT

!     Save ILINE as ILSAVE and Initialize ILINE
!     JAT D065, 8/9/21 ILSAVE IS SET BUT NEVER USED
!      ILSAVE = ILINE

   IF (HOURLY) THEN
!        Process Hourly Emissions from File
!        Begin Source Loop
      DO ISRC = 1, NUMSRC
! ---       Check for HOURLY emissions for this source
         IF (QFLAG(ISRC) == 'HOURLY') THEN
! ---          Retrieve hourly emissions for MAXDCONT option
            AQS(ISRC) = AAQS(IHR_NDX,IYR_NDX,ISRC)

            IF (SRCTYP(ISRC)(1:5) == 'POINT') THEN
               ATS(ISRC) =  AATS(IHR_NDX,IYR_NDX,ISRC)
               AVS(ISRC) =  AAVS(IHR_NDX,IYR_NDX,ISRC)
!**  Added for Aircraft Plume Rise; UNC-IE
            ELSE IF (SRCTYP(ISRC) == 'VOLUME' .and.&
            &AFTSRC(ISRC) == 'Y') THEN
               AMFUEL(ISRC) = AAMFUEL(IHR_NDX,IYR_NDX,ISRC)
               ATHRUST(ISRC) = AATHRUST(IHR_NDX,IYR_NDX,ISRC)
               AVAA(ISRC) = AAVAA(IHR_NDX,IYR_NDX,ISRC)
               AAFR(ISRC) = AAAFR(IHR_NDX,IYR_NDX,ISRC)
               ABYPR(ISRC) = AABYPR(IHR_NDX,IYR_NDX,ISRC)
               ASRCANGLE(ISRC) = AASRCANGLE(IHR_NDX,IYR_NDX,ISRC)
               ARPWR(ISRC) = AARPWR(IHR_NDX,IYR_NDX,ISRC)
            ELSE IF (SRCTYP(ISRC) == 'VOLUME' .and.&
            &L_HRLYSIG(ISRC).and.&
            &AFTSRC(ISRC) == 'Y') THEN
               AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)
               ASYINI(ISRC) =  AASYINI(IHR_NDX,IYR_NDX,ISRC)
               ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
               AMFUEL(ISRC) = AAMFUEL(IHR_NDX,IYR_NDX,ISRC)
               ATHRUST(ISRC) = AATHRUST(IHR_NDX,IYR_NDX,ISRC)
               AVAA(ISRC) = AAVAA(IHR_NDX,IYR_NDX,ISRC)
               AAFR(ISRC) = AAAFR(IHR_NDX,IYR_NDX,ISRC)
               ABYPR(ISRC) = AABYPR(IHR_NDX,IYR_NDX,ISRC)
               ASRCANGLE(ISRC) = AASRCANGLE(IHR_NDX,IYR_NDX,ISRC)
               ARPWR(ISRC) = AARPWR(IHR_NDX,IYR_NDX,ISRC)
!**  End Aircarft Plume Rise insert; April 2023
            ELSE IF (SRCTYP(ISRC) == 'VOLUME' .and.&
            &AFTSRC(ISRC) == 'N'     .and.&   ! Added for Aircraft; UNC-IE
            &L_HRLYSIG(ISRC)) THEN
               AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)
               ASYINI(ISRC) =  AASYINI(IHR_NDX,IYR_NDX,ISRC)
               ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
!**  Added for Aircraft Plume Rise; UNC-IE
            ELSE IF (SRCTYP(ISRC)(1:4) == 'AREA' .and.&
            &AFTSRC(ISRC) == 'Y') THEN
               AMFUEL(ISRC) = AAMFUEL(IHR_NDX,IYR_NDX,ISRC)
               ATHRUST(ISRC) = AATHRUST(IHR_NDX,IYR_NDX,ISRC)
               AVAA(ISRC) = AAVAA(IHR_NDX,IYR_NDX,ISRC)
               AAFR(ISRC) = AAAFR(IHR_NDX,IYR_NDX,ISRC)
               ABYPR(ISRC) = AABYPR(IHR_NDX,IYR_NDX,ISRC)
               ASRCANGLE(ISRC) = AASRCANGLE(IHR_NDX,IYR_NDX,ISRC)
               ARPWR(ISRC) = AARPWR(IHR_NDX,IYR_NDX,ISRC)
            ELSE IF (SRCTYP(ISRC)(1:4) == 'AREA' .and.&
            &L_HRLYSIG(ISRC).and.&
            &AFTSRC(ISRC) == 'Y') THEN
               AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)
               ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
               AMFUEL(ISRC) = AAMFUEL(IHR_NDX,IYR_NDX,ISRC)
               ATHRUST(ISRC) = AATHRUST(IHR_NDX,IYR_NDX,ISRC)
               AVAA(ISRC) = AAVAA(IHR_NDX,IYR_NDX,ISRC)
               AAFR(ISRC) = AAAFR(IHR_NDX,IYR_NDX,ISRC)
               ABYPR(ISRC) = AABYPR(IHR_NDX,IYR_NDX,ISRC)
               ASRCANGLE(ISRC) = AASRCANGLE(IHR_NDX,IYR_NDX,ISRC)
               ARPWR(ISRC) = AARPWR(IHR_NDX,IYR_NDX,ISRC)
!**  End Aircraft Plume Rise insert; April 2023
            ELSE IF (SRCTYP(ISRC)(1:4) == 'AREA' .and.&
            &AFTSRC(ISRC) == 'N'     .and.&  ! Added for Aircraft; UNC-IE
            &L_HRLYSIG(ISRC)) THEN
               AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)
               ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)

            ELSE IF (SRCTYP(ISRC) == 'LINE' .and.&
            &L_HRLYSIG(ISRC)) THEN
               AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)
               ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
            ELSE IF (SRCTYP(ISRC) == 'BUOYLINE') THEN
               AFP(ISRC) = AAFP(IHR_NDX,IYR_NDX,ISRC)
            END IF
         END IF
      END DO
!*       End Source Loop
   END IF
!*----
!     Save ILINE as ILSAVE and Initialize ILINE
!     JAT D065, 8/9/21 ILSAVE IS SET BUT NEVER USED
!      ILSAVE = ILINE

   IF (L_BACKGRND) THEN
! ---    Assign BACKGRND concentration based on the hour and
!        year index; this value accounts for HOURLY BACKGRND
!        with or without substitution based on other temporally-
!        varying BACKGRND values, or is based soley on the
!        user-specified temporally-varying BACKGRND
      BGCONC = ABGCONC(IHR_NDX,IYR_NDX)
   ELSE
      BGCONC = 0.0D0
   END IF

   IF (PVMRM .or. OLM .or. RUNTTRM .or. GRSM) THEN
! ---    Assign background Ozone concentration based on the
!        hour and year index; this value accounts for hourly O3
!        with or without substitution based on other temporally-
!        varying O3 values, or is based soley on the
!        user-specified annual or temporally-varying O3
      O3CONC = AO3CONC(IHR_NDX,IYR_NDX)
!---     Set O3MISS = .T. if stored O3CONC is less than 0.
      IF (O3CONC < 0.0D0) THEN
         O3MISS = .TRUE.
      ELSE
         O3MISS = .FALSE.
      END IF
   END IF

!     CERC 11/30/20
   IF (GRSM) THEN
! ---    Assign background NOx concentration based on the
!        hour and year index; this value accounts for hourly NOx
!        with or without substitution based on other temporally-
!        varying NOx values, or is based soley on the
!        user-specified annual or temporally-varying NOx
      NOXBGCONC = ANOXBGCONC(IHR_NDX,IYR_NDX)
!---     Set NOXMISS = .T. if stored NOXBGCONC is less than 0.
      IF (NOXBGCONC < 0.0D0) THEN
         NOXMISS = .TRUE.
      ELSE
         NOXMISS = .FALSE.
      END IF
   END IF

   IF (FULLDATE>=ISDATE .and. FULLDATE<=IEDATE) THEN

      IF (CLMHR .and. CLMPRO) THEN
!           Check for Calm Hr & Processing and Increment Counters
         NUMHRS(1) = NUMHRS(1) + 1
         NUMCLM(1) = NUMCLM(1) + 1
      ELSE IF (MSGHR .and. MSGPRO) THEN
!           Check for Missing Hour & Processing and Increment Counters
         NUMHRS(1) = NUMHRS(1) + 1
         NUMMSG(1) = NUMMSG(1) + 1
      ELSE IF (ZI <= 0.0D0) THEN
!           Write Out The Informational Message & Increment Counters
         NUMHRS(1) = NUMHRS(1) + 1
      ELSE
!           Set CALCS Flag, Increment Counters & Calculate HRVAL
         CALCS = .TRUE.
         NUMHRS(1) = NUMHRS(1) + 1

!           Calculate CONC or DEPOS Values               ---   CALL CALC
         CALL CALC
      END IF

      IF (.NOT.CLMHR .and. .NOT.MSGHR) THEN
! ---       Non-calm, non-missing hour; apply NO2 options as appropriate

!! Added for TTRM2
!! If TTRM2 (TTRM with the compare option) is requested then
!! perform TTRM >> FIRST <<
         IF (RUNTTRM2) THEN
! ---             Process Hourly Values for TTRM Option
            CMETH = 3
            CALL TTRM_CALC
! ---          Flush HRVAL Arrays (1:NUMTYP)
            HRVAL(:)   = 0.0D0
            IF (PVMRM .and. .NOT. PSDCREDIT) THEN
               CALL PVMRM_CALC('ALLSRCS')
            ELSE IF (OLM) THEN
               CALL OLM_CALC
            ELSE IF (ARM2) THEN
               CALL ARM2_CALC
            END IF
            GO TO 8857
         END IF
         IF (PVMRM .and. .NOT.PSDCREDIT) THEN
! ---          Process Hourly Values for PVMRM Option
            CALL PVMRM_CALC('ALLSRCS')

         ELSE IF (PVMRM .and. PSDCREDIT) THEN
! ---          Process Hourly Values for PVMRM Option and PSD credits
! ---          Need to process two separate sets of sources - the
!              increment consumption sources ('NAAQSRC') and the
!              increment expanding sources ('ALLBASE')
            CALL PVMRM_CALC('NAAQSRC')
            CALL PVMRM_CALC('ALLBASE')

         ELSE IF (OLM) THEN
! ---          Process Hourly Values for OLM Option
            CALL OLM_CALC
!
!   Added for TTRM; AECOM
         ELSE IF (RUNTTRM) THEN
! ---          Process Hourly Values for TTRM Option
            IF (.NOT. RUNTTRM2) THEN
               CALL TTRM_CALC
            END IF
!   End TTRM insert; Feb. 2021; Modified Nov. 2021

         ELSE IF (ARM2) THEN
! ---          Process Hourly Values for ARM2 Option
            CALL ARM2_CALC

         ELSE IF (GRSM) THEN
! ---          CERC 11/30/20 Process Hourly Values for GRSM Option
            CALL GRSM_CALC

         END IF
8857     CONTINUE
      END IF

      IAVE = 1
!        Check for End of Averaging Period
      IF (KAVE(1)/=1 .and. MOD(IHOUR,KAVE(1))==0) THEN
!           Calculate Applicable Averages          ---   CALL AVER
         CALL AVER
! ---       Reinitialize NUMHRS, NUMCLM and NUMMSG
         NUMHRS(:) = 0
         NUMCLM(:) = 0
         NUMMSG(:) = 0
      END IF

!        Flush HRVAL Arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      PRMVAL  = 0.0D0

      IF (PVMRM .or. OLM .or. ARM2&
      &.or. RUNTTRM .or. GRSM) THEN
!           Flush CHI(NUMREC,NUMSRC,NUMTYP) Array
         CHI(:,:,:) = 0.0D0
         IF(RUNTTRM2)THEN
            TTRMCOMPARE(:,:,:,:) = 0.0D0
         ENDIF
         IF(GRSM)THEN
            CHI_TTRAVPLM = 0.0D0
            CHI_TTRAVPAN = 0.0D0
            CHI_TTRAVAER = 0.0D0
            CHI_TTRAVPRM = 0.0D0
            CHI_TTRAVCHM(:,:) = 0.0D0
            BLDFAC(:,:) = 0.0D0
            PRMVAL_Src1 = 0.0D0
         END IF
         IF (PSDCREDIT) THEN
!              Flush ABVAL(NUMREC,NUMTYP) and BCVAL(NUMREC,NUMTYP) Arrays
            ABVAL(:,:) = 0.0D0
            BCVAL(:,:) = 0.0D0
         END IF
      END IF

   END IF

!     Reset CALCS and ENDMON Flags
   CALCS  = .FALSE.
   ENDMON = .FALSE.

!     Save precipitation rates for two previous hours
   prec2 = prec1
   prec1 = Prate

   RETURN
END SUBROUTINE MAXDCALC

SUBROUTINE MAXDCNT_FILE(IGRP1,IVAL)
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
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER :: I, IVAL, IDEC, IMOD, IGRP1, IGRP2
! JAT 06/22/21 D065 REMOVE NCHR1 AS UNUSED VARIABLE
!      CHARACTER NCHR1(10)*3, NCHR2(10)*5, CHRVAL*5, HDRFRM*400,
   CHARACTER :: NCHR2(10)*5, CHRVAL*5, HDRFRM*400,&
   &MXDFMT*100
! Unused:       INTEGER :: J

!     Variable Initializations
! JAT 06/22/21 D065 REMOVE NCHR1 INITIALIZATION AS UNUSED VARIABLE
!      DATA (NCHR1(I),I=1,10) /'YR1','YR2','YR3','YR4','YR5',
!     &                        'YR6','YR7','YR8','YR9','Y10'/
   DATA (NCHR2(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   MODNAM = 'MAXDCNT_FILE'

!     Create Header Format for Columns
   WRITE(HDRFRM,9019) NUMGRP, NUMGRP

! --- Generate write format, depending on FILE_FORMAT
   IF (FILE_FORMAT == 'FIX') THEN
      WRITE(MXDFMT,9001) MIN( NUMGRP, 9999 )
   ELSE IF (FILE_FORMAT == 'EXP') THEN
      WRITE(MXDFMT,9002) MIN( NUMGRP, 9999 )
   END IF

! --- Assign character label for rank
   IF (IVAL <= 10) THEN
      CHRVAL = NCHR2(IVAL)
   ELSE IF (MOD(IVAL,100) > 10 .and.&
   &MOD(IVAL,100) < 20) THEN
      IDEC = INT(IVAL/10)
      IMOD = MOD(IVAL,10)
      WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
   ELSE IF (IVAL <= 999) THEN
      IDEC = INT(IVAL/10)
      IMOD = MOD(IVAL,10)
      IF (IMOD == 0) IMOD = 10
      WRITE(CHRVAL,'(I2,A3)') IDEC, NCHR2(IMOD)(3:5)
   END IF

   IF (.NOT. L_NoHeader(8)) THEN
!        Write Header Information
      WRITE(IMXDCUNT(IGRP1),9005) VERSN, TITLE1(1:68), RUNDAT
      WRITE(IMXDCUNT(IGRP1),9007) C_METVER, RUNTIM,&
      &MODOPS_String(1:LEN_TRIM(MODOPS_String))
!
      IF (PM25AVE) THEN
         IF (MAXD_THRESH(IGRP1) > 0.0D0) THEN
            WRITE(IMXDCUNT(IGRP1),9008) CHRVAL,CHRAVE(1),&
            &NUMYRS, GRPID(IGRP1), MAXD_THRESH(IGRP1),&
            &NREC, NUMGRP, MXDFMT
         ELSE
            WRITE(IMXDCUNT(IGRP1),9108) CHRVAL,CHRAVE(1),&
            &NUMYRS, GRPID(IGRP1), NREC, NUMGRP, MXDFMT
         END IF
      ELSE IF (NO2AVE .or. SO2AVE) THEN
         IF (MAXD_THRESH(IGRP1) > 0.0D0) THEN
            WRITE(IMXDCUNT(IGRP1),9009) CHRVAL,CHRAVE(1),&
            &NUMYRS, GRPID(IGRP1), MAXD_THRESH(IGRP1),&
            &NREC, NUMGRP, MXDFMT
         ELSE
            WRITE(IMXDCUNT(IGRP1),9109) CHRVAL,CHRAVE(1),&
            &NUMYRS, GRPID(IGRP1), NREC, NUMGRP, MXDFMT
         END IF
      END IF
      WRITE(IMXDCUNT(IGRP1),HDRFRM) CHIDEP(1,1),&
      &CHIDEP(2,1), CHIDEP(3,1),&
      &(GRPID(I),I=1,NUMGRP)
   END IF

! --- Begin Receptor LOOP
   DO IREC = 1, NREC
! ---    Write results for specified source group with contributions
!        from other source groups, paired in time and space
      WRITE(IMXDCUNT(IGRP1),MXDFMT,ERR=99)&
      &AXR_SAV(IREC), AYR_SAV(IREC),&
      &SUMHNH(IREC,IGRP1,IVAL), AZELEV_SAV(IREC),&
      &AZHILL_SAV(IREC), AZFLAG_SAV(IREC),&
      &CHRAVE(1), GRPID(IGRP1), CHRVAL, NETID(IREC),&
      &(SUMVAL_MAXD(IVAL,IGRP2,IGRP1,IREC),&
      &IGRP2=1,MIN(NUMGRP,9999))
   END DO
! --- End Receptor LOOP

   GO TO 999

!     WRITE Error Message for Problem Writing to Plot File
99 WRITE(DUMMY,'("MXDCN",I3.3)') IMXDCUNT(IGRP1)
   CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

! --- Generate write formats for FIX and EXP FILE_FORMATs:
9001 FORMAT('(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,',I4,&
   &'(F13.5,2X:))')
9002 FORMAT('(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,'&
   &,'2X,',I4,'(E13.6,2X:))')

9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
9007 FORMAT('* AERMET (',A6,'):',T93,A8,&
   &/'* MODELING OPTIONS USED: ',A:)
9008 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST ',A5,&
   &' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,&
   &' ; ABOVE THRESH = ',F13.5,&
   &/'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9X,'FORMAT: ',A:)
9108 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST ',A5,&
   &' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9X,'FORMAT: ',A:)
9009 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST MAX DAILY ',A5,&
   &' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,&
   &' ; ABOVE THRESH = ',F13.5,&
   &/'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9X,'FORMAT: ',A:)
9109 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST MAX DAILY ',A5,&
   &' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,&
   &/'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,&
   &' SOURCE GROUPS;  WITH ',&
   &'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',&
   &'SPACE',&
   &/'*',9X,'FORMAT: ',A:)
9019 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,(2X,3A4),4X,''ZELEV'',4X,&
   &''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,&
   &''NET ID'',1X,',I4,'(2X,''CONT '',A8),/,''*'',&
   &3(1X,''____________ ''),1X,3('' ______  ''),&
   &''______  ________  ________  ________'',',&
   &I4,'(''  _____________''))')

999 CONTINUE

! --- Close and reopen file to flush data from file buffer
   CLOSE(UNIT=IMXDCUNT(IGRP1))
   OPEN(IMXDCUNT(IGRP1),ERR=99,FILE=MAXDCONT_FILE(IGRP1),&
   &IOSTAT=IOERRN,FORM='FORMATTED',STATUS='OLD',&
   &POSITION='APPEND')

   RETURN
END SUBROUTINE MAXDCNT_FILE

