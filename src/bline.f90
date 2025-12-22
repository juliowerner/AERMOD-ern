SUBROUTINE BL_CALC (KK)
!***********************************************************************
!             BL_CALC Routine of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates concentration or deposition values
!                 for BUOYANT LINE sources
!                 (Adpated from the BLP source code)
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    June 30, 2015
!
!        MODIFIED:
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
!                 Particular Source
!
!        CALLED FROM:   CALC
!
!        Assumptions:
!
!***********************************************************************
!     Variable Declarations
   USE MAIN1
   USE BUOYANT_LINE

   IMPLICIT NONE

   CHARACTER :: MODNAM*12

   DOUBLE PRECISION, PARAMETER  :: BLCRIT = 0.02D0
   DOUBLE PRECISION, PARAMETER  :: SRT2DP = 0.7978846D0
   DOUBLE PRECISION, PARAMETER  :: AVFACT = 1.0D0
   DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::&
   &WSPEXP = [0.10D0, 0.15D0, 0.20D0, 0.25D0, 0.30D0, 0.30D0]
   DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::&
   &TERAN = [0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.30D0, 0.30D0]
   DOUBLE PRECISION, PARAMETER, DIMENSION(2) ::&
   &DTHTA = [0.02D0, 0.035D0]

   INTEGER :: I, KK, NBLINES
! Unused:      INTEGER :: J, K, INOUT
   INTEGER, PARAMETER :: MAXIT  = 14
   INTEGER, PARAMETER, DIMENSION(7) :: NSEGA = [3,5,9,17,33,65,129]

   DOUBLE PRECISION   :: DECFAC, FTSAVE(129)
   DOUBLE PRECISION   :: BL_XVZ, BL_XVY
   DOUBLE PRECISION   :: BL_UEFF, BL_THETA
! JAT 06/22/21 D065 REMOVE DPBL AS UNUSED VARIABLE
!      DOUBLE PRECISION   :: P, S, TER1, FBRG, WSST, DPBL, CUQ, SUMFPRM
   DOUBLE PRECISION   :: P, S, TER1, FBRG, WSST, CUQ, SUMFPRM
   DOUBLE PRECISION   :: RI, UNSRT, XFSXX, DLMIN, ZLINEHS, ZLINEBASE
   DOUBLE PRECISION   :: YV, ZV, XB, YB, XE, YE, XMAXL, YMAXL, XMINL
   DOUBLE PRECISION   :: YMINL, DXEL, DYEL, SUMM, XRECEP, THT, HNT
   DOUBLE PRECISION   :: DOWNX, CROSSY, SY0, SZ0, SYC, YRECEP
   DOUBLE PRECISION   :: XRMXKM, YLOW, YHIGH, VIRTXZ, VIRTXY
   DOUBLE PRECISION   :: VXYKM, VXZKM, SIGY, SIGZ, Z
   DOUBLE PRECISION   :: TERRAN, H, FT, DELTAT, SUM2, DIFF, CORR
   DOUBLE PRECISION   :: XSEG1, XDIFF, YSEG1, YDIFF, WEIGHT
   DOUBLE PRECISION   :: XCLN, YCLN, EM, B, SFCZ0_SAV
! CRCO D095 added for BLP debug
   DOUBLE PRECISION   :: FPLMHT
   INTEGER :: JITCT, IWPBL, ITHETA, IDELTA, IWOSIG, IDW, IDIV
   INTEGER :: IDIST, ISEG, ISEG2, ISEG3, ITER, INDL, I2, IG
   INTEGER :: IGSUB, NSEG, NSEG0, NSEGM1, INDLSV, INDLLN, NNEW
   INTEGER :: NCONTRIB, IBMIN, IBMAX, IBMAX1, IH, IGMAX, IGMAX1
   INTEGER :: LNUM, KSRC, LSRC, ISRC_SAV, KST_SAV, BL_URBKST

!     Variable Initializations
   MODNAM = 'BL_CALC'
   JITCT  = 0
   IWPBL  = 0

!CRT  Intialize variables ! 12/27/2017
   TER1 = 0.0D0
   FBRG = 0.0D0                                            ! Wood_D41

   SFCZ0_SAV = 0.0D0
   ISRC_SAV  = 0
   KST_SAV   = 0

!     The original BLP used meteorology from the met processor CRSTER.
!      CRSTER did not allow wind speeds to be less than 1 m/s.  Set the
!      reference wind speed used in the calculations below to be a minimum
!      of 1.0 m/s.  Use the local variable BL_UREF.  Keep the reference
!      wind speed height as UREFHT.
!      IF (UREF .GE. 1.0D0) THEN
!         BL_UREF = UREF
!      ELSE
!         BL_UREF = 1.0D0
!        WRITE Message              ! Ref ht wind speed less than minimum
!         WRITE(DUMMY,'(''on '',I8.8)') KURDAT
!         CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
!      END IF

!     Save the source number, as determined when the source records are
!       processed by AERMOD, when BL_CALC is called since this routine
!       is not exited until all (one or more) bouyant line source groups
!       are processed
   ISRC_SAV = ISRC

!     LTOPG is called in METEXT.f in subr.SET_METDATA for a buoyant line
!       source; save that value in case it is changed for an urban source
   KST_SAV  = KST

!     Save original value for Z0
   SFCZ0_SAV = SFCZ0

! --- Loop over the buoyant line source groups
!jop      BLPGROUP_LOOP: DO KK = 1, NUMBLGRPS

!        Initialize total concentration and partial contributions from
!          each line to 0 for all receptors
   CHIBL(:)  = 0.0D0
   PARTCH    = 0.0D0

!        Assign the average rise parameters from the arrays (processed
!         during setup) to the scalar
   BLAVGLLEN = BLAVGINP_LLEN(KK)
   BLAVGBHGT = BLAVGINP_BHGT(KK)
   BLAVGBWID = BLAVGINP_BWID(KK)
   BLAVGLWID = BLAVGINP_LWID(KK)
   BLAVGBSEP = BLAVGINP_BSEP(KK)
   BLAVGFPRM = BLAVGINP_FPRM(KK)

! ---    Assign # of individual lines for this BL source group to a scalar
   NBLINES = NBLINGRP(KK)

!        Set Mixing Height and Obukhov Length for Urban Option if Needed
!         A tacit assumption is that all lines in a buoyant line source
!         are either ALL urban or rural, i.e., no combination of urban/rural
!         is allowed.  This condition should have been checked in the QA of
!         the source input.

   IF (L_BLURBAN(KK)) THEN
!           Find urban area index for buoyant lines
!            Remember that once one individual BL is encountered that
!            all buoyant lines and BL source groups are processed
!            before moving on to the next non-BL source, so
!            ISRC will not be changing.  The BL lines are already
!            grouped together (group ID index = KK)
      LNUM = 0
      IF (KK ==1 )THEN
         KSRC = ISRC

      ELSE IF (KK >= 2) THEN
         DO I = 1,NUMSRC
            IF (SRCTYP(I) == 'BUOYLINE') THEN
               LNUM = LNUM + 1
               IF( BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
                  KSRC = I
                  EXIT
               END IF
            END IF
         END DO
      END IF

      DO I = 1, NUMURB
         IF (IURBGRP(KSRC,I) == 1) THEN
            IURB = I
            EXIT
         END IF
      END DO

      IF (STABLE .or. L_MorningTrans(IURB)) THEN
         URBSTAB = .TRUE.

!              Set AERMET file values to urban values for use below
         ZI = MAX( ZIURB(IURB), ZIMECH )

!              ... and for LTOPG
         SFCZ0 = URBZ0(IURB)
         IF (L_MorningTrans(IURB)) THEN
            OBULEN = URBOBULEN(IURB)
! ---          Call LTOPG for a buoyant line source in an urban area
            CALL LTOPG(BL_URBKST)
            KST = BL_URBKST
         ELSE
            OBULEN = DABS( URBOBULEN(IURB) )
! CRCO 12/8/2021 Decision was to set stability to 4 for all urban nighttime hours
            KST = 4
         END IF





      ELSE
!              Reset ZI and OBULEN back to rural values that were saved in
!              SET_METDATA and URBCALC (both in metext.f)
!              KST was not changed and retains the rural/original value
         URBSTAB = .FALSE.
         ZI = ZIRUR
         OBULEN = RUROBULEN
      END IF

   ELSE IF (URBAN .and. .NOT. L_BLURBAN(KK) ) THEN
!           Reset ZI and OBULEN back to rural values that were saved in
!           SET_METDATA and URBCALC (both in metext.f)
!           KST was not changed and retains the rural/original value
      URBSTAB = .FALSE.
      ZI = ZIRUR
      OBULEN = RUROBULEN

   ELSE
! ---       Rural
!           KST was not changed and retains the rural/original value
      URBSTAB = .FALSE.
   END IF

!        Use the decay coefficient, DECOEF (default or input by the user),
!         but only under the conditions noted, for BLP's decay factor (DECFAC)
!         IF (DFAULT .and. URBAN .and. POLLUT.EQ.'SO2' .and.
!        &    L_BLURBAN(KK)) THEN
!        modified 9/12/17 JAT, use half-life for SO2 URBAN even without DFAULT
!        if HALFLIFE or DCAYCOEF used, use that value, not 4-hours

   IF (URBAN .and. POLLUT=='SO2' .and. L_BLURBAN(KK) .and.&
   &((ICSTAT(7) == 0 .and. ICSTAT(8) == 0) .or. DFAULT)) THEN
      DECFAC = DECOEF
   ELSE IF ((POLLUT=='SO2' .and. L_BLURBAN(KK)) .or.&
   &DFAULT) THEN  !rural source for SO2 or default modified 10/12/17
      DECFAC = 0.0D0
   ELSE
!           DECFAC = 0.0D0 !commented out JAT 10/12/17
      DECFAC = DECOEF !now add ability to use DECAY coefficient or half life
   END IF

! ---    Wood, 12/1/2019: modified for multiple buoyant lines
!        If there is an hourly emissions file with buoyant lines,
!          calculate the average FPRIME (buoyancy parameter) from the
!          values in the file
!
   IF (HRLYBLCOUNT(KK) > 0) THEN
!           Compute the average BLAVGFPRM from ther values in the
!            HOUREMIS file
      LNUM = 0
      SUMFPRM = 0.0

      DO ISRC = 1, NUMSRC
         IF (SRCTYP(ISRC) == 'BUOYLINE') THEN
            LNUM = LNUM + 1
            LSRC = BLINEPARMS(LNUM)%ISRCNUM
            IF (QFLAG(LSRC) == 'HOURLY' .and.&
            &BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
               SUMFPRM = SUMFPRM + AFP(LSRC)
            END IF
         END IF
      END DO
      BLAVGFPRM = SUMFPRM / DBLE(HRLYBLCOUNT(KK))
   END IF

!        Calculate distance (from XFB) to final neutral plume rise
!         assuming plumes interact before reaching terminal rise
!         Note: the multiplication needs to be done when hourly values
!               are not available from an HOUREMIS file and BLAVGFPRM
!               is entered on the BLPINPUT record(s)
   FBRG = DBLE(NBLINES) * BLAVGFPRM/PI
   IF (FBRG <= 55.0D0) THEN
!           Value: 49 = 3.5*14.
      BL_XFINAL = 49.0D0 * FBRG**0.625D0
   ELSE
!           Value: 120.72 = 3.5 * 34.49 (34.49=CONST3 in BLP)
      BL_XFINAL =  120.72D0 * FBRG**0.4D0
   END IF
   XMATCH = BL_XFINAL

!        Compute the difference between the endpoint of the x coordinates
!         AFTER the first translation/rotation for the BL group being
!         processed
   DO LNUM = 1,NBLTOTAL
      IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
         DEL(LNUM) =&
         &BLINEPARMS(LNUM)%XEND_TR1-BLINEPARMS(LNUM)%XBEG_TR1
      END IF
   END DO

!        Rotate source and receptors so the y-axis of the buoyant line
!         is parallel to the wind direction for this BL source group

   CALL BL_ROTATE2 (WDREF, BL_THETA, ITHETA, NUMREC, NBLTOTAL, KK)

! ---    Create array of BLINEs (part of RWB method for determining if
!          receptor in in bounding box - did not work for horizontal and
!          vertical sources)
!         DO K = 1, 4
!          DO I = 1, NBLINES
!             DO J = 1, 129
!                XBL_RCS_MAX(K) = MAX( XBL_RCS_MAX(K), XS_RCS(I,J) )
!                YBL_RCS_MAX(K) = MAX( YBL_RCS_MAX(K), YS_RCS(I,J) )
!                XBL_RCS_MIN(K) = MIN( XBL_RCS_MIN(K), XS_RCS(I,J) )
!                YBL_RCS_MIN(K) = MIN( YBL_RCS_MIN(K), YS_RCS(I,J) )
!             ENDDO
!          ENDDO
!         ENDDO

!        Get the power law exponent based on the stability;
!         calculate WSST = stack height wind speed
!         calculate S for stable conditions (BLTA is the saved ambient
!         temperature since it is modified when point sources are run)
   P = WSPEXP(KST)
   WSST = BL_UREF * (BLAVGBHGT/UREFHT)**P
   IF (KST > 4) S = 9.80616D0 * DTHTA(KST-4)/BLTA

!        Calculate an effective wind speed, BL_UEFF, with the line source
!         plume rise eqn
!        INPUT:  KST, WSST, S, and P
!        OUTPUT: BL_UEFF = an effective wind speed
   CALL BL_WSC(KST,WSST,BL_UEFF,S,P,NBLINES)

!        Calculate XFB,LEFF,LD,R0
   CALL BL_LENG(BL_THETA,BL_UEFF,AVFACT,NBLINES)

!        Calculate distance to final rise
!         BL_XFS = distance to final rise
!         BL_XFB = distance to full buoyancy
!         BL_XFINAL = final neutral plume rise
   IF (KST <= 4) THEN
!           Calculate distance to final rise for neutral/unstable conditions
      BL_XFS = BL_XFB + BL_XFINAL

!           Find 5 intermediate downwind distances (in addition to XFB)
!           at which plume rise will be calculated
      DO IDIST = 2,7
         RI = DBLE(IDIST)
         BL_XDIST(IDIST) = BL_XFS -&
         &(BL_XFS - BL_XFB)*(7.0D0 - RI)/5.0D0
      END DO

   ELSE
!           Calculate distance to final rise for stable conditions
      UNSRT = (16.0D0*BL_UEFF*BL_UEFF/S) - (BL_XFB*BL_XFB/3.0D0)

      IF (UNSRT <= 0.0D0) THEN
         BL_XFS = (12.0D0*BL_XFB*BL_UEFF*BL_UEFF/S)**(0.333333D0)
      ELSE
         BL_XFS = 0.5D0 * (BL_XFB + DSQRT(UNSRT))
      END IF

      XFSXX = BL_UEFF*PI/DSQRT(S)
      BL_XFS = DMIN1(BL_XFS,XFSXX)

      IF (BL_XFS <= BL_XFB) THEN
         DO IDIST = 2,7
            BL_XDIST(IDIST) = BL_XFS
         END DO

      ELSE
!              Find 5 intermediate downwind distances (in addition to XFB)
!               at which plume rise will be calculated
         DO IDIST = 2,7
            RI=DBLE(IDIST)
            BL_XDIST(IDIST) = BL_XFS -&
            &(BL_XFS - BL_XFB)*(7.0D0-RI)/5.0D0
         END DO
      END IF
   END IF


   CALL BL_RISE(BL_UEFF,KST, S)
!
!        CALCULATE CONCENTRATIONS DUE TO THE LINE SOURCES
!
!        LOOP OVER LINES
!
!        Since this routine is called only once for each bouyant line source
!         group, start the search for the buoyant lines with the current
!         source, i.e. the first buoyant line encountered in the group
!
!        The original source number was saved above to the correct source
!         so as we change the source number here, the processing does not
!         get messed up; the original source number is returned at the end
!         of the BUOY_LINES loop.

   BUOY_LINES: DO LNUM = 1,NBLTOTAL
      IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
         ISRC = BLINEPARMS(LNUM)%ISRCNUM
         DLMIN     = DEL(LNUM)/128.0D0
         ZLINEBASE = BLINEPARMS(LNUM)%ELEV
         ZLINEHS   = BLINEPARMS(LNUM)%BLHS

! CRCO D095 added for BLP debug, specifically compute plume rise
         IF (BLPDBUG) THEN
            FPLMHT = ZLINEHS + DH(7)
            WRITE(BLPUNT,3333) SRCID(ISRC),IYEAR,IMONTH,IDAY,IHOUR,DH,&
            &BL_XDIST,BL_XFB,BL_XFS,KST,URBOBULEN,&
            &OBULEN,ZLINEHS,FPLMHT
3333        FORMAT(1X,A12,4(2X,I2),14(F8.2),2X,2(F8.2),4X,I1,&
            &4(1X,F8.2))
         ENDIF

!              Compute the profiled wind speed at the buoyant line
!               release height
         WSST  = BL_UREF * (ZLINEHS/UREFHT)**P

!              Assign the emissions stored in the variable AQS to QS
!               (either the constant on the SRCPARM keyword or the
!                hourly emission rate)
         CALL SETSRC

!              Apply the emission factor to the input argument QS
!               (result is stored in QTK)
         CALL EMFACT(QS)
!MGS               D183_BUOYLINE_EMISUNIT_WSP 3/1/2024: replaced 1.0E6 w/EMIFAC(1) below
!MGS               CUQ   = QTK * 1.0D06 / (DBLE(NSEGA(1)-1)*WSST)
         CUQ   = QTK * EMIFAC(1) / (DBLE(NSEGA(1)-1)*WSST) !EMIFAC(1) is for concentration
         SZ0   = R0 * SRT2DP
         ZV    = 1000.0D0 * BL_XVZ(SZ0,KST)
         SY0   = SZ0/2.0D0
         YV    = 1000.0D0 * BL_XVY(SY0,KST)
         XB    = XS_RCS(LNUM,1)
         YB    = YS_RCS(LNUM,1)
         XE    = XS_RCS(LNUM,129)
         YE    = YS_RCS(LNUM,129)
         XMAXL = DMAX1(XB,XE)
         XMINL = DMIN1(XB,XE)
         YMAXL = DMAX1(YB,YE)
         YMINL = DMIN1(YB,YE)
         DXEL  = XE - XB
         DYEL  = YE - YB

!              LOOP OVER RECEPTORS
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
            SUMM      = 0.0D0
            PARTCH    = 0.0D0
            NSEG      = 0
            NCONTRIB  = 0
            XRECEP    = XR_RCS(IREC)

! Wood_D11 - Include flagpole receptor heights
            ZFLAG = AZFLAG(IREC)
!WSP                  THT   = AZELEV(IREC) - ZLINEBASE + ZFLAG
!WSP --- Begin: D173 WSP add 7/24/2023
            IF (L_FLATSRC(ISRC)) THEN
               THT   = ZFLAG
            ELSE
               THT   = AZELEV(IREC) - ZLINEBASE + ZFLAG
            END IF
!WSP --- End: D173 WSP add 7/24/2023

!                 If the receptor is upwind of the line
            IF (XRECEP <= XMINL) THEN
               CYCLE RECEPTOR_LOOP
            END IF

!                 If the receptor is inside the rectangle defined by the source
!                  cycle to the next receptor ...
            IF (BL_RFLAG(IREC,KK)) THEN
               CYCLE RECEPTOR_LOOP
            END IF

!                 Check for receptor located inside "boundary" of BUOYLINE sources
!                   (RWB method)
!                  DO I = 1, 4
!                     IF( XR_RCS(I) .LE. XBL_RCS_MAX(I) .and.
!           &             XR_RCS(I) .GE. XBL_RCS_MIN(I) )THEN
!                        CHIBL(IREC) = 0.0D0
!                        CYCLE RECEPTOR_LOOP
!                     ENDIF
!                  END DO

            YRECEP    = YR_RCS(IREC)
!                 IWOSIG keeps track of whether any line segment is within
!                  one sigma y of the current receptor (0=NO,1=YES)
            IWOSIG = 0

!                 Define region of influence
!                  Max distance from any source segment to current receptor
!                  is equal to (XRECEP-XMINL)
            XRMXKM = (XRECEP-XMINL)/1000.0D0
            CALL BL_SIGMAY(XRMXKM,KST,SYC)

            YLOW  = YMINL - 4.0D0*SYC
            YHIGH = YMAXL + 4.0D0*SYC
            IF (YRECEP < YLOW .or. YRECEP > YHIGH) THEN
               CYCLE RECEPTOR_LOOP
            END IF

            YLOW  = YLOW  + DLMIN
            YHIGH = YHIGH - DLMIN
            IF (YRECEP < YLOW .or. YRECEP > YHIGH) THEN
               CYCLE RECEPTOR_LOOP
            END IF

!                 Check if receptor is directly downwind of the line
!                  (IDW=0=NO,IDW=1=YES)
            IDW = 1

            IF (YRECEP < YMINL .or. YRECEP > YMAXL) IDW = 0

!                 Check if receptor is on the downwind side of the line
            IF (XRECEP < XMAXL )THEN
               IF (MOD(ITHETA,90) /= 0) THEN
                  EM = DYEL/DXEL
                  B = YE - EM*XE
                  IF (XRECEP < (YRECEP-B)/EM) NCONTRIB = 999
               END IF
            END IF

            NSEG0  = NSEGA(1)
            NNEW   = NSEG0
            ITER   = 0
            INDL   = 1
            IDELTA = 128/(NSEG0-1)
498         CONTINUE
            NSEG = NSEG+NNEW
!
!                 Loop over line segments
!
            SEGMENT_LOOP: DO ISEG = 1,NNEW
               FTSAVE(INDL) = 0.0D0

!                    If current receptor is upwind of a source segment,
!                    then this source segment does not contribute

               IF (XS_RCS(LNUM,INDL) >= XRECEP) GO TO 495
               DOWNX  = XRECEP - XS_RCS(LNUM,INDL)
               CROSSY = YRECEP - YS_RCS(LNUM,INDL)
               VIRTXZ = DOWNX + ZV
               VIRTXY = DOWNX + YV
               VXYKM  = VIRTXY/1000.0D0
               VXZKM  = VIRTXZ/1000.0D0

               if( VXYKM < 0.0D0 .or. VXZKM < 0.0D0 )then
! ---                   Virtual distance is < 0.0; skip this segment
                  GO TO 495
               endif
               CALL BL_DBTSIG(VXZKM,VXYKM,KST,SIGY,SIGZ)

!                    If crosswind distance > 4 * SIGY, then this source
!                     segment does not contribute
               IF (4.0D0*SIGY < DABS(CROSSY)) GO TO 495
               IF (DABS(CROSSY) < SIGY) IWOSIG = 1
               CALL BL_ZRISE(LNUM,INDL,IREC,Z)
!
!                    Include terrain correction in determining the plume height
!
               TER1 = 1.0D0 - TERAN(KST)
               HNT  = Z + ZLINEHS
!                    TER1=(1.0-TERAN(KST)); THT=RELEV(I)-LELEV(LNUM)
!                    ZI is the AERMOD-determined mixing height
               TERRAN = TER1 * DMIN1(HNT,THT)
               H = HNT - TERRAN

               IF (H > ZI .and. KST <= 4)GO TO 495
!
!                    Solve the gaussian point source equation
!
               CALL BL_GAUSS(KST,ZI,CROSSY,SIGY,SIGZ,H,FT)
!                    Include decay in determining chi
               DELTAT = DOWNX/WSST
               FTSAVE(INDL) = FT*(1.0D0 - DELTAT*DECFAC)
               NCONTRIB = NCONTRIB + 1
495            INDL   = INDL + IDELTA
            END DO SEGMENT_LOOP

!
!                 First time through loop, calculate the first chi estimate
!
            IF (NNEW /= NSEG0) GO TO 714
            INDL = 1
            NSEGM1 = NSEG0 - 1
            SUMM = (FTSAVE(1) + FTSAVE(129))/2.0D0
            DO ISEG2 = 2,NSEGM1
               INDL = INDL + IDELTA
               SUMM = SUMM + FTSAVE(INDL)
            END DO

!                 If receptor is within region of influence but not directly
!                  downwind of any part of the line, and SUM=0.0, CHI=0.0
            IF (SUMM <= 0.0D0 .and. IDW /= 1) THEN
               CYCLE RECEPTOR_LOOP
            END IF
!
!                 Calculate the refined chi estimate
!
713         CONTINUE
            ITER   = ITER + 1
            IDIV   = MIN0(ITER,2)
            IDELTA = IDELTA/IDIV
            INDL   = 1 + IDELTA/2
!                 INDL is the subcript of the first new line segment
!                   (save as INDLSV)
            INDLSV = INDL

            NNEW = NSEGM1**ITER + 0.1D0

!                 If more than 129 line segments (i.e., 64 new segments)
!                  are required, continue to increase the number of
!                  segments but only over the section of the line
!                  which is contributing

            IF (NNEW > 64) GO TO 759
            GO TO 498

714         CONTINUE

!                 Subscript of the first new line segment is INDLSV
!                 Subscript of the last new line segment is INDLLN
            INDLLN = 129 - IDELTA/2

!                 Sum the first and last new line segments
            SUM2 = FTSAVE(INDLSV) + FTSAVE(INDLLN)

!                 If there are only 2 new line segments, skip this loop
            IF (NNEW > 2) THEN
               INDL = INDLSV
               I2   = NNEW-1
!
!                    Find the sum of all the new line segments
               DO ISEG3=2,I2
                  INDL = INDL + IDELTA
                  SUM2 = SUM2 + FTSAVE(INDL)
               END DO

            END IF
!
!                 Compare the new estimate with the previous estimate
!
            SUM2 = SUMM/2.0D0 + SUM2/(2.0D0**ITER)

!                 At least one line segment must be within one SIGMA Y of
!                  the line (if the receptor is directly downwind of any
!                  part of the line)
            IF (IDW == 1 .and. IWOSIG /= 1) GO TO 758

            DIFF = DABS(SUM2-SUMM)
!MGS               D183_BUOYLINE_EMISUNIT_WSP 3/4/2024: Added the 1.0E6/EMIFAC(1) to leave this
!MGS                            comparison equivalent to before EMIFAC(1) was added to CUQ above.
!MGS                  IF (DIFF*CUQ .LT. 0.1D0) THEN
            IF (DIFF*CUQ*1.0D6/EMIFAC(1) < 0.1D0) THEN    !EMIFAC(1) is for concentration
               GO TO 720
            END IF

            CORR = DIFF/SUM2

            IF (CORR < BLCRIT) THEN
               GO TO 720
            END IF

758         CONTINUE
            SUMM = SUM2
            GO TO 713

!                 If 129 source segments not sufficient, continue
!                  to increase number of segments, but only over the
!                  section of line which is contributing
759         CONTINUE

            CALL BL_SORT(FTSAVE,IBMIN,IBMAX,IWPBL)

            IF (IWPBL /= 999) GO TO 4949
            IWPBL  = 0
            PARTCH = 0.0D0
            CYCLE RECEPTOR_LOOP

4949        CONTINUE

            IBMAX1 = IBMAX - 1
            IH     = 0
            IGMAX  = 1

939         CONTINUE
            SUM2   = 0.0D0
            IGMAX1 = IGMAX + 1

            DO 940 IG = IBMIN,IBMAX1
!                    XCLN = x coordinate (RCS) of current (newest) line segment
!                    YCLN = y coordinate (RCS) of current (newest) line segment
!                    XRCS and YRCS are the translated and rotated line source
!                      segments w.r.t. the hourly flow vector
               XSEG1 = XS_RCS(LNUM,IG)
               XDIFF = XS_RCS(LNUM,IG+1) - XSEG1
               YSEG1 = YS_RCS(LNUM,IG)
               YDIFF = YS_RCS(LNUM,IG+1) - YSEG1

               DO 941 IGSUB = 1,IGMAX
                  WEIGHT = DBLE(IGSUB)/DBLE(IGMAX1)
                  XCLN   = XSEG1 + WEIGHT*XDIFF
                  YCLN   = YSEG1 + WEIGHT*YDIFF
                  DOWNX  = XRECEP - XCLN
                  CROSSY = YRECEP - YCLN
                  VIRTXZ = DOWNX + ZV
                  VIRTXY = DOWNX + YV
                  VXYKM  = VIRTXY/1000.0D0
                  VXZKM  = VIRTXZ/1000.0D0

                  if( VXYKM < 0.0D0 .or. VXZKM < 0.0D0 )then
! ---                      Virtual distance is < 0.0; skip this segment
                     GO TO 941                                   ! not sure if this is best fix
                  endif

                  CALL BL_DBTSIG(VXZKM,VXYKM,KST,SIGY,SIGZ)
                  CALL BL_ZRISE(LNUM,IG,IREC,Z)

!                       Include terrain correction in determining plume height
                  HNT    = Z + ZLINEHS
!                       TER1   = (1.0-TERAN(KST)); THT=RELEV(I)-LELEV(LNUM)
                  TERRAN = TER1*DMIN1(HNT,THT)
                  H      = HNT - TERRAN
                  CALL BL_GAUSS(KST,ZI,CROSSY,SIGY,SIGZ,H,FT)

!                       Include decay in determining chi
                  DELTAT = DOWNX/WSST
                  FT     = FT*(1.0D0 - DELTAT*DECFAC)
                  SUM2   = SUM2 + FT
                  NCONTRIB = NCONTRIB + 1
941            CONTINUE

940         CONTINUE

!                 Compare the new estimate with the previous estimate
            SUM2 = SUMM/2.0D0 + SUM2/(2.0D0**ITER)

            DIFF = ABS(SUM2-SUMM)
!MGS               D183_BUOYLINE_EMISUNIT_WSP 3/4/2024: Added the 1.0E6/EMIFAC(1) to leave this
!MGS                            comparison equivalent to before EMIFAC(1) was added to CUQ above.
!MGS                  IF (DIFF*CUQ .LT. 0.1D0) THEN
            IF (DIFF*CUQ*1.0D6/EMIFAC(1) < 0.1D0) THEN    !EMIFAC(1) is for concentration
               GO TO 720
            ENDIF

            CORR = DIFF/SUM2
            IF (CORR < BLCRIT) THEN
               GO TO 720
            END IF

            SUMM = SUM2
            ITER = ITER + 1
            IF (ITER >= MAXIT) THEN
               SUMM = SUM2
               PARTCH      = CUQ*SUMM
               CHIBL(IREC) = CHIBL(IREC) + PARTCH
               IF (DEBUG) THEN
                  WRITE(DBGUNT,6000) IREC, CHIBL(IREC)
6000              FORMAT(/,5X,'Buoyant Line iteration fails to ',&
                  &' converge for receptor',I6,', conc. =',F13.6)
                  JITCT = JITCT+1
                  IF (MOD(JITCT,100) == 0 ) THEN
                     WRITE(DBGUNT,6001) JITCT
6001                 FORMAT(/,5X,'Buoyant Line iterations for',&
                     &' all receptors fails for ',I8,'th time' )
                  END IF
               ENDIF
               CYCLE RECEPTOR_LOOP
            END IF
            IH = IH + 1
            IGMAX = 2**IH
            GO TO 939
720         CONTINUE

            SUMM = SUM2

!                 Test to make sure at least two line segments contributed
!                  to the chi estimate (unless receptor is on the upwind side
!                  of the line with some source segments downwind and some
!                  source segments upwind -- in that case just use the test
!                  for convergence)
            IF (NCONTRIB < 2) GO TO 713

!                 Calculate concentration (in micrograms);
!                  use stack height wind speed for dilution

            PARTCH = CUQ*SUMM
            CHIBL(IREC)  = CHIBL(IREC) + PARTCH
            HRVAL = PARTCH

!                 For the initial implementation of BLP into AERMOD, we
!                  are skipping calculations with PVMRM, OLM.
!                  As BLP is integrated more into AERMOD, those options
!                  will be included (e.g., see PCALC).
!
            IF (ARM2) THEN
! ---                Store conc by source and receptor for ARM/ARM2 options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

!                    Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0

            END IF

!                 Sum HRVAL to AVEVAL and ANNVAL Arrays      ---   CALL SUMVAL
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               DO IGRP = 1, NUMGRP
                  CALL SUMVAL
               END DO
            END IF

            IF (EVAL(ISRC)) THEN
!                    Check ARC centerline values for EVALFILE
!                    output                                  ---   CALL EVALCK
               CALL EVALCK
            END IF

!                 Initialize __VAL arrays (1:NUMTYP)
            HRVAL   = 0.0D0

         END DO RECEPTOR_LOOP
      END IF                    ! BLINEPARMS(LNUM)%IBLPGRPNUM = KK
   END DO BUOY_LINES

!        Since all buoyant line groups and the associated individual
!         buoyant lines are processed in consecutive calls to BL_CALC
!         from subr,CALC, the PG stability, mixing height, roughness
!         length, and Obukhov length need to be restored to the
!         original values if the BL group was an urban source
   IF (URBSTAB) THEN
      ISRC = ISRC_SAV
      KST  = KST_SAV
      ZI   = ZIRUR
      SFCZ0  = SFCZ0_sav
      OBULEN = RUROBULEN
   ELSE
      ISRC = ISRC_SAV
      KST  = KST_SAV
      SFCZ0  = SFCZ0_sav
   END IF

!     END DO BLPGROUP_LOOP

!     ISRC = ISRC_SAV
!     KST  = KST_SAV
!     ZI   = ZIRUR
!     SFCZ0  = SFCZ0_sav
!     OBULEN = RUROBULEN

   RETURN
END SUBROUTINE BL_CALC

SUBROUTINE BL_DBTSIG (XZ,XY,ISTAB,SY,SZ)
!***********************************************************************
!             BL_DBTSIG Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE: To estimate sigma-y and sigma-z for buoyant line sources
!
!     INPUT:
!       XZ     Virtual distance (kilometers) - VXZKM in calling program
!       XY     Virtual distance (kilometers) - VXYKM in calling program
!       ISTAB  PG stability category
!
!     OUTPUT:
!       SY     Sigma-y
!       SZ     Sigma-z
!
!     ------------------------------------------------------------------
!
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(IN)  :: XZ, XY
   DOUBLE PRECISION, INTENT(OUT) :: SY, SZ
   INTEGER, INTENT (IN) :: ISTAB
   INTEGER :: ID

   DOUBLE PRECISION :: TH

   DOUBLE PRECISION, PARAMETER, DIMENSION(7) ::&
   &XA = [0.5D0, 0.4D0, 0.3D0, 0.25D0, 0.2D0, 0.15D0, 0.1D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(2) ::&
   &XB = [0.4D0, 0.2D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(5) ::&
   &XD = [30.0D0, 10.0D0, 3.0D0, 1.0D0, 0.3D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::&
   &XE = [40.0D0, 20.0D0, 10.0D0, 4.0D0, 2.0D0,&
   &1.0D0, 0.3D0, 0.1D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::&
   &XF = [60.0D0, 30.0D0, 15.0D0, 7.0D0, 3.0D0,&
   &2.0D0, 1.0D0, 0.7D0, 0.2D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::&
   &AA = [453.85D0, 346.75D0, 258.89D0, 217.41D0,&
   &179.52D0, 170.22D0, 158.08D0, 122.8D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::&
   &BA = [2.1166D0, 1.7283D0, 1.4094D0, 1.2644D0,&
   &1.1262D0, 1.0932D0, 1.0542D0, 0.9447D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::&
   &AB = [109.30D0, 98.483D0, 90.673D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::&
   &BB = [1.0971D0, 0.98332D0, 0.93198D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::&
   &AD = [44.053D0, 36.650D0, 33.504D0, 32.093D0,&
   &32.093D0, 34.459D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::&
   &BD = [0.51179D0, 0.56589D0, 0.60486D0, 0.64403D0,&
   &0.81066D0, 0.86974D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::&
   &AE = [47.618D0, 35.420D0, 26.970D0, 24.703D0,&
   &22.534D0, 21.628D0, 21.628D0, 23.331D0, 24.26D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::&
   &BE = [0.29592D0, 0.37615D0, 0.46713D0, 0.50527D0,&
   &0.57154D0, 0.63077D0, 0.75660D0, 0.81956D0, 0.8366D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::&
   &AF = [34.219D0, 27.074D0, 22.651D0, 17.836D0, 16.187D0,&
   &14.823D0, 13.953D0, 13.953D0, 14.457D0, 15.209D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::&
   &BF = [0.21716D0, 0.27436D0, 0.32681D0, 0.41507D0, 0.46490D0,&
   &0.54503D0, 0.63227D0, 0.68465D0, 0.78407D0, 0.81558D0]

!      GO TO (10,20,30,40,50,60),ISTAB

!CRT  Initialize TH, 12/27/2017
   TH = 0.0D0

   SELECT CASE (ISTAB)
    CASE (1)
!           STABILITY A (10)
      TH = (24.167D0 - 2.5334D0 * DLOG(XY)) / 57.2958D0
      IF (XZ > 3.11D0) THEN
         SZ = 5000.0D0
      ELSE
         DO ID = 1,7
            IF (XZ >= XA(ID)) GO TO 12
         END DO
         ID = 8
12       SZ = AA(ID) * XZ ** BA(ID)
      ENDIF

    CASE (2)
!           STABILITY B (20)
      TH = (18.333D0 - 1.8096D0 * DLOG(XY)) / 57.2958D0
      IF (XZ > 35.0D0) THEN
         SZ = 5000.0D0
      ELSE
         DO ID = 1,2
            IF (XZ >= XB(ID)) GO TO 22
         END DO
         ID = 3
22       SZ = AB(ID) * XZ ** BB(ID)
         IF (SZ > 5000.0D0) SZ = 5000.0D0
      ENDIF

    CASE (3)
!           STABILITY C (30)
      TH = (12.5D0 - 1.0857D0 * DLOG(XY)) / 57.2958D0
      SZ = 61.141D0 * XZ ** 0.91465D0
      IF (SZ > 5000.0D0) SZ = 5000.0D0

    CAsE (4)
!           STABILITY D (40)
      TH = (8.3333D0 - 0.72382D0 * DLOG(XY)) / 57.2958D0

      DO ID = 1,5
         IF (XZ >= XD(ID)) GO TO 42
      END DO
      ID = 6
42    SZ = AD(ID) * XZ ** BD(ID)
      IF (SZ > 5000.0D0) SZ = 5000.0D0

    CASE (5)
!           STABILITY E (50)
      TH = (6.25D0 - 0.54287D0 * DLOG(XY)) / 57.2958D0
      DO ID = 1,8
         IF (XZ >= XE(ID)) GO TO 52
      END DO
      ID = 9
52    SZ = AE(ID) * XZ ** BE(ID)
      IF (SZ > 5000.0D0) SZ = 5000.0D0

    CASE (6)
!           STABILITY F (60)
      TH = (4.1667D0 - 0.36191D0 * DLOG(XY)) / 57.2958D0
      DO ID = 1,9
         IF (XZ >= XF(ID)) GO TO 62
      END DO
      ID = 10
62    SZ = AF(ID) * XZ ** BF(ID)
      IF (SZ > 5000.0D0) SZ = 5000.0D0
   END SELECT

   SY = 1000.0D0 * XY * DSIN(TH)/(2.15D0 * DCOS(TH))

   RETURN
END SUBROUTINE BL_DBTSIG
!
!     ------------------------------------------------------------------
SUBROUTINE BL_SIGMAY(XKM,ISTAB,SY)
!
!             BL_SIGMAY Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates sigma Y
!
!     INPUT:
!       XKM    Virtual distance (kilometers) - VXZKM in calling program
!       ISTAB  PG stability category
!
!     OUTPUT:
!       SY     Sigma-y
!
!     ------------------------------------------------------------------
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(IN)  :: XKM
   DOUBLE PRECISION, INTENT(OUT) :: SY
   INTEGER, INTENT(IN)           :: ISTAB

   DOUBLE PRECISION              :: TH

!CRT  Initialize TH  ! 12/27/2017
   TH = 0.0D0

   SELECT CASE (ISTAB)

    CASE (1)
!           STABILITY A(10)
      TH = (24.167D0 - 2.5334D0 * DLOG(XKM)) / 57.2958D0

    CASE (2)
!           STABILITY B
      TH = (18.333D0 - 1.8096D0 * DLOG(XKM)) / 57.2958D0

!           STABILITY C
    CASE (3)
      TH = (12.5D0- 1.0857D0 * DLOG(XKM)) / 57.2958D0

!           STABILITY D
    CASE (4)
      TH = (8.3333 - 0.72385D0 * DLOG(XKM)) / 57.2958D0

!           STABILITY E
    CASE (5)
      TH = (6.25D0 - 0.54287D0 * DLOG(XKM)) / 57.2958D0

!           STABILITY F
    CASE (6)
      TH = (4.1667D0 - 0.36191D0 * DLOG(XKM)) / 57.2958D0

   END SELECT

   SY = 1000.0D0 * XKM * DSIN(TH)/(2.15D0 * DCOS(TH))

   RETURN
END SUBROUTINE BL_SIGMAY

SUBROUTINE BL_RISE(U,ISTAB,S)
!***********************************************************************
!             BL_RISE Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates line source plume rise using an optional vertical wind
!     shear corrected 'effective' wind speed for both neutral and
!     stable conditions
!
!     Input
!       U       = wind speed
!       S       = computed parameter for stable conditions
!       ISTAB   = P-G stability category
!
!     Input via module.f
!       LEFF    = effective building length
!       R0      = edge radius
!       LD      = LEFF * sin (theta), where theta = angle between flow
!                  vector and the orientation of the line source
!       FPRMXNB = fprime * number of lines
!       BL_XFB  = distance to full buoyancy
!       BL_XFS  = distance to final rise for stable conditions
!       XDIST   = array of intermediate distances for downwash calcs
!
!     Output
!       DH      = array of intermediate plume rise values
!
!     ------------------------------------------------------------------
   USE BUOYANT_LINE
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(IN) :: U, S
   INTEGER, INTENT(IN) :: ISTAB

   DOUBLE PRECISION :: X, A, B, C, Z
   INTEGER :: IDIST
!
!
!     CONSTANT 1.5915494 = 3./(PI*BETA) WITH BETA=0.6
!     CONSTANT 5.0 = 3./BETA WITH BETA=0.6
   A = 1.5915494D0 * LEFF + 5.0D0 * R0

!     CONSTANT 5.3051648 = 6./(PI*BETA*BETA) WITH BETA=0.6
!     CONSTANT 8.3333333 = 3./(BETA*BETA) WITH BETA=0.6
   B = R0 * (5.3051648D0 * LD + 8.333333D0 * R0)

   DO 1000 IDIST = 2,7
      X = BL_XDIST(IDIST)

      IF (ISTAB <= 4  .or. X < BL_XFS) THEN
         IF( DABS(X - BL_XFB) < 1.0D-10 )THEN
!              CONSTANT 0.4420971 = 1./(2.*PI*BETA*BETA) WITH BETA=0.6
            C = -0.4420971D0 * (FPRMXNB/BL_XFB) * (X/U)**3
            CALL BL_CUBIC(A,B,C,Z)
            DH(IDIST) = Z
         ELSE
!              CONSTANT 1.3262912 = 3./(2.*PI*BETA*BETA) WITH BETA=0.6
            C = -1.3262912D0 * FPRMXNB *&
            &(BL_XFB*BL_XFB/3.0D0 + X*X - BL_XFB*X)/U**3
            CALL BL_CUBIC(A,B,C,Z)
            DH(IDIST) = Z

         END IF

      ELSE
!           With stable conditions, use neutral rise equation
!           for transitional rise calculations, but calculate
!           FINAL RISE BASED ON THE FINAL STABLE RISE EQUATION

!           Calculate final (stable) plume rise
!           Constant 5.3051648 = 6./(PI*BETA*BETA) WITH BETA=0.6
         C = -5.3051648D0 * FPRMXNB / (U*S)
         CALL BL_CUBIC(A,B,C,Z)
         DH(IDIST) = Z

      END IF

1000 CONTINUE

   RETURN
END SUBROUTINE BL_RISE
!
SUBROUTINE BL_ZRISE(IL,IS,IR,Z)
!***********************************************************************
!             BL_ZRISE Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Z1 is the plume height of the highest plume segment at X = XFB
!       (except in the special case of stable conditions with the
!       distance to final rise (XFS) less than XFB -- in that case, Z1
!       is the height of the highest plume element at X=XFS)
!
!     XI is the distance of the current line segment to XFB
!
!     Input:
!       IL = line number
!       IS = INDL = line segment
!       IR = receptor number
!
!     Output:
!       Z
!
!     ------------------------------------------------------------------
   USE BUOYANT_LINE

   DOUBLE PRECISION, INTENT(OUT) :: Z
   INTEGER, INTENT(IN)           :: IL, IR, IS
   DOUBLE PRECISION :: Z1, Z2, XI, ZXFB
!
   Z1 = DH(2)

   XI = BL_XFB - XS_RCS(IL,IS)
   XI = DMAX1(XI,0.0D0)
   XI = DMIN1(XI,BL_XFB)
   ZXFB = Z1*(1.0D0 - (BL_XFB-XI)/BL_XFB)

!     Z2, returned from BLP_INTRSE, is the plume height of the highest
!         segment at X
   CALL BL_INTRSE(XR_RCS(IR),Z2)

   Z = ZXFB + Z2 - Z1

   RETURN
END SUBROUTINE BL_ZRISE

!     ------------------------------------------------------------------
SUBROUTINE BL_INTRSE(X,Z)
!
!             BL_INTRSE Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Interpolates the plume rise of the top (highest) plume element
!       at any distance X using the calculated plume rise at
!       seven points (BL_XDIST(1-7)), as computed in BL_RISE just before
!       entering the loop over the buoyant lines.
!
!     Input:
!       X   = Distance X
!
!     Output:
!       Z   = Interpolated plume rise
!
!     ------------------------------------------------------------------
   USE BUOYANT_LINE
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(IN)  :: X
   DOUBLE PRECISION, INTENT(OUT) :: Z

   INTEGER    :: NDEX, NDEX1, IDIST
!C
   IF (X > BL_XDIST(7)) THEN
!        PLUME REACHES FINAL RISE
      Z = DH(7)
   ELSE
! Wood_D117: BL_INTRSE error
!         NDEX = 5                                          ! D117
!         DO 10 IDIST = 2,6                                 ! D117

      DO 10 IDIST = 2,7                                  ! D117
         IF (X < BL_XDIST(IDIST)) THEN                ! D117
            NDEX = IDIST
            EXIT
         ENDIF
10    CONTINUE
      NDEX1 = NDEX-1
      Z = DH(NDEX) - (DH(NDEX)-DH(NDEX1)) * (BL_XDIST(NDEX) - X) /&
      &(BL_XDIST(NDEX) - BL_XDIST(NDEX1))
   END IF

   RETURN
END SUBROUTINE BL_INTRSE

SUBROUTINE BL_GAUSS(ISTAB,DPBL,CROSSY,SIGY,SIGZ,H,FT)
!***********************************************************************
!             BL_GAUSS Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Input:
!       ISTAB   = PG stability category
!       DPBL    = mixing height
!       CROSSY  = crosswind distance from receptor to source segment
!       SIGY    = sigma-y calculated in BL_DBTSIG
!       SIGZ    = sigma-z calculated in BL_DBTSIG
!       H       = terrain-dependent height (for ??)
!
!     Output:
!       FT
!     ------------------------------------------------------------------
   USE BUOYANT_LINE

   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ISTAB
   DOUBLE PRECISION, INTENT(IN) :: DPBL, CROSSY, SIGY, SIGZ, H
   DOUBLE PRECISION, INTENT(OUT) :: FT

   DOUBLE PRECISION :: TMIN, TMAX, TD1, YPSIG, EXPYP, EXPHP
   DOUBLE PRECISION :: ARG, F, F1, T, H2, HPSIG, EPSIL

   DATA TMIN/0.0512D0/,TMAX/9.21D0/, EPSIL/1.0D-30/

   TD1   = 3.1415927D0 * SIGY * SIGZ
   YPSIG = CROSSY/SIGY
   EXPYP = 0.5D0 * YPSIG * YPSIG

!     Prevent underflows
   IF (EXPYP > 50.0D0) THEN
      F  = 0.0D0                 ! not really needed
      F1 = 0.0D0                 ! not really needed
      FT = 0.0D0
      RETURN
   END IF

   F = EXP(-EXPYP)

!     If mixing height (DPBL) GE 5000 meters or for stable conditions,
!     neglect the reflection terms
   IF (ISTAB >= 5 .or. DPBL > 5000.0D0) GO TO 451

!     If SIGZ GT 1.6*DPBL, assume a uniform vertical distribution
   IF (SIGZ > 1.6D0*DPBL) GO TO 460

!     Calculate multiple eddy reflections terms
!     using a fourier series method -- see ERT MEMO CS 093

   F1 = 1
   T  = (SIGZ/DPBL)**2
   H2 = H/DPBL

   IF (T < 0.6D0) THEN
      ARG = 2.0D0 * (1.0D0 - H2)/T
      IF (ARG < TMAX) THEN
         IF (ARG < TMIN) THEN
            F1 = F1 + 1.0D0 - ARG
         ENDIF
         IF(ARG >= TMIN) THEN
            F1 = F1 + EXP(-ARG)
         ENDIF
         ARG = 2.0D0 * (1.0D0 + H2)/T
         IF (ARG < TMAX) THEN
            F1 = F1 + EXP(-ARG)
            ARG = 4.0D0 * (2.0D0 - H2)/T
            IF (ARG < TMAX) THEN
               F1 = F1 + EXP(-ARG)
               ARG = 4.0D0 * (2.0D0 + H2)/T
               IF (ARG < TMAX) THEN
                  F1 = F1 + EXP(-ARG)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      ARG = -0.5D0 * H2 * H2/T
      IF (ARG < -90.0D0) THEN
         F1 = 0.0D0
      ENDIF

!        The constant 0.797885 = SQRT(2./PI)
      IF (ARG >= -90.0D0) THEN
         F1 = 0.797885D0 * F1 * EXP(ARG)/SIGZ
      ENDIF
      IF (F1 < EPSIL) F1 = 0.0D0

   ELSE
!        The constant 4.934802 = PI*PI/2.
      ARG = 4.934802D0 * T
      IF (ARG < TMAX) THEN
         F1 = F1 + 2.0D0 * DEXP(-ARG) * DCOS(3.141593D0*H2)

!            The constant 19.739209 = 2.*PI*PI
         ARG = 19.739209D0 * T
         IF (ARG < TMAX) THEN
            F1 = F1 + 2.0D0 * DEXP(-ARG) * DCOS(6.283185D0*H2)
         ENDIF
      ENDIF
      F1 = F1/DPBL
      IF (F1 < EPSIL) F1 = 0.0D0
   ENDIF

!     The constant 1.25331414 = SQRT(PI/2.)
   F1 = 1.25331414D0 * SIGZ * F1
   GO TO 445
451 CONTINUE

   HPSIG = H/SIGZ
   EXPHP = 0.5D0 * HPSIG * HPSIG
   IF (EXPHP > 50.0D0) THEN
      F1 = 0.0D0
   ELSE
      F1 = DEXP(-EXPHP)
   ENDIF

445 CONTINUE

!     Find product of exponential terms divided by (PI*SIGY*SIGZ)
   FT = F * F1/TD1
   GO TO 470
460 CONTINUE

!     Vertical distribution assumed uniform
!     The constant 2.5066283 = SQRT(2.*PI)
   FT = F/(2.5066283D0 * SIGY * DPBL)

470 RETURN
END SUBROUTINE BL_GAUSS

!
!     ------------------------------------------------------------------
DOUBLE PRECISION FUNCTION BL_XVZ (SZO,ISTAB)
!
!
!     ------------------------------------------------------------------
   DOUBLE PRECISION :: SZO
   INTEGER :: ISTAB, ID

   DOUBLE PRECISION, PARAMETER, DIMENSION(7) ::&
   &SA = [13.95D0, 21.40D0, 29.3D0, 37.67D0, 47.44D0,&
   &71.16D0 ,104.65D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(2) ::&
   &SB = [20.23D0, 40.0D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(5) ::&
   &SD = [12.09D0, 32.09D0, 65.12D0, 134.9D0, 251.2D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::&
   &SE = [3.534D0, 8.698D0, 21.628D0, 33.489D0, 49.767D0,&
   &79.07D0, 109.3D0, 141.86D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::&
   &SF = [4.093D0, 10.93D0, 13.953D0, 21.627D0, 26.976D0,&
   &40.0D0, 54.89D0, 68.84D0, 83.25D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::&
   &AA = [122.8D0, 158.08D0, 170.22D0, 179.52D0, 217.41D0,&
   &258.89D0, 346.75D0, 453.85D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::&
   &AB = [90.673D0, 98.483D0, 109.3D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::&
   &AD = [34.459D0, 32.093D0, 32.093D0, 33.504D0,&
   &36.650D0, 44.053D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::&
   &AE = [24.26D0, 23.331D0, 21.628D0, 21.628D0, 22.534D0,&
   &24.703D0, 26.97D0, 35.42D0, 47.618D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::&
   &AF = [15.209D0, 14.457D0, 13.953D0, 13.953D0, 14.823D0,&
   &16.187D0, 17.836D0, 22.651D0, 27.074D0, 34.219D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::&
   &CA = [1.0585D0, 0.9486D0, 0.9147D0, 0.8879D0, 0.7909D0,&
   &0.7095D0, 0.5786D0, 0.4725D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::&
   &CB = [1.073D0, 1.017D0, 0.9115D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::&
   &CD = [1.1498D0, 1.2336D0, 1.5527D0, 1.6533D0,&
   &1.7671D0, 1.9539D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::&
   &CE = [1.1953D0, 1.2202D0, 1.3217D0, 1.5854D0, 1.7497D0,&
   &1.9791D0, 2.1407D0, 2.6585D0, 3.3793D0]

   DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::&
   &CF = [1.2261D0, 1.2754D0, 1.4606D0, 1.5816D0, 1.8348D0,&
   &2.151D0, 2.4092D0, 3.0599D0, 3.6448D0, 4.6049D0]
!
!CRT  Initialize BL_XVZ
   BL_XVZ = 0.0D0

   SELECT CASE (ISTAB)

    CASE (1)
!          STABILITY A
      DO ID = 1,7
         IF (SZO <= SA(ID)) GO TO 12
      END DO
      ID = 8
12    BL_XVZ =(SZO/AA(ID))**CA(ID)

    CASE (2)
!          STABILITY B
      DO ID = 1,2
         IF (SZO <= SB(ID)) GO TO 22
      END DO
      ID = 3
22    BL_XVZ = (SZO/AB(ID))**CB(ID)

    CASE (3)
!          STABILITY C
      BL_XVZ = (SZO/61.141D0)**1.0933D0

    CASE (4)
!          STABILITY D
      DO ID = 1,5
         IF(SZO <= SD(ID)) GO TO 42
      END DO
      ID = 6
42    BL_XVZ = (SZO/AD(ID))**CD(ID)

    CASE (5)
!          STABILITY E
      DO ID = 1,8
         IF (SZO <= SE(ID)) GO TO 52
      END DO
      ID = 9
52    BL_XVZ = (SZO/AE(ID))**CE(ID)

    CASE (6)
!          STABILITY F
      DO ID = 1,9
         IF (SZO <= SF(ID)) GO TO 62
      END DO
      ID = 10
62    BL_XVZ = (SZO/AF(ID))**CF(ID)

   END SELECT

END FUNCTION BL_XVZ
!
!     ------------------------------------------------------------------
DOUBLE PRECISION FUNCTION BL_XVY (SYO,ISTAB)
!
!
!     ------------------------------------------------------------------
   IMPLICIT NONE
   DOUBLE PRECISION :: SYO
   INTEGER          :: ISTAB
!
!
!CRT  Initialize BL_XVY, 12/27/2017
   BL_XVY = 0.0D0

   SELECT CASE (ISTAB)
    CASE (1)
      BL_XVY = (SYO/213.0D0)**1.1148D0

    CASE (2)
      BL_XVY = (SYO/155.0D0)**1.097D0

    CASE (3)
      BL_XVY = (SYO/103.0D0)**1.092D0

    CAsE (4)
      BL_XVY = (SYO/68.0D0)**1.076D0

    CASE (5)
      BL_XVY = (SYO/50.0D0)**1.086D0

    CASE (6)
      BL_XVY = (SYO/33.5D0)**1.083D0
   END SELECT

END FUNCTION BL_XVY

SUBROUTINE BL_WSC(ISTAB,UM,U,S,P,NBL)
!***********************************************************************
!             BL_WSC Routinee of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates an effective U using the line source plume rise
!     equation (line source term only) matched at X = XF (final rise)
!
!     INPUT:
!       ISTAB = P-G stability category
!       UM    = WSST = stack height wind speed from power law
!       S     = a computed parameter for stable conditions
!       NBL   = number of individual buoyant lines in the BL group
!     OUTPUT:
!       U
!
!     ------------------------------------------------------------------
   USE BUOYANT_LINE
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(OUT) :: U
   DOUBLE PRECISION, INTENT(IN)  :: UM, S, P
   INTEGER, INTENT(IN) :: ISTAB, NBL

   DOUBLE PRECISION :: P2, P3, EP, EPI, T1, Z


   IF (ISTAB <= 4) THEN
!
!        NEUTRAL (OR UNSTABLE) CONDITIONS
!
      P3  = 3.0D0 * P
      EP  = 2.0D0 + P3
      EPI = 1.0D0 / EP

!        CONSTANT 2.4=4.*BETA WITH BETA=0.6
      T1 = (EP*EP * DBLE(NBL) * BLAVGFPRM * BLAVGBHGT**P3 /&
      &(2.4D0 * (2.0D0 + P) * BLAVGLLEN * UM**3))**EPI
      Z  = T1*XMATCH**(2.0D0*EPI)

!        CONSTANT 1.2 = 2.*BETA WITH BETA=0.6
      U  = (DBLE(NBL) * BLAVGFPRM /&
      &(1.2D0 * BLAVGLLEN) * (XMATCH/Z)*(XMATCH/Z))**0.333333D0
      U  = DMAX1(U,UM)

   ELSE
!
!        STABLE CONDITIONS
!
      P2 = 2.0D0 + P

!        CONSTANT 0.6 = BETA
      Z = (P2 * BLAVGBHGT**P * DBLE(NBL) * BLAVGFPRM /&
      &(0.6D0 * BLAVGLLEN * UM * S))**(1.0D0/P2)

!        CONSTANT 3.3333333 = 2./BETA WITH BETA=0.6
      U = 3.3333333D0 * DBLE(NBL) * BLAVGFPRM / (BLAVGLLEN*S*Z*Z)
      U = DMAX1(U,UM)
   ENDIF

   RETURN
END SUBROUTINE BL_WSC
!
SUBROUTINE BL_LENG(THETA, U, AVFACTOR, NBL)
!***********************************************************************
!             BL_LENG Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates XFB,LEFF,LD,R0 (all in the buoyant_line module)
!
!     Input:
!       THETA = angle of second rotation for the source y-axis (after
!               the first rotation) to align with the flow vector
!       U     = effective wind speed (BL_UEFF in calling routine)
!       AVFACTOR = constant (parameter) = 1.0D0
!       NBL   = number of individual buoyant lines in the BL group
!
!     ------------------------------------------------------------------
   USE BUOYANT_LINE
   IMPLICIT NONE

   DOUBLE PRECISION, INTENT(IN) :: U, THETA, AVFACTOR
   DOUBLE PRECISION :: LEFF1, LEFFV, DXM, DXM833, RAD, T1, XI
   DOUBLE PRECISION :: THRAD, SINT, COST
   INTEGER, INTENT(IN) :: NBL

   DATA RAD/0.0174533D0/
!
!     BLAVGFPRM is the 'average' buoyancy flux of one line;
!     FPRMXNB is the 'effective' buoyancy flux of n lines
   FPRMXNB = DBLE(NBL) * BLAVGFPRM
   THRAD    = THETA * RAD
   SINT    = ABS(DSIN(THRAD))
   COST    = ABS(DCOS(THRAD))

!     Calculate distance of full buoyancy (XFB)
   DXM    = BLAVGBSEP + BLAVGBWID
   BL_XFB = BLAVGLLEN * COST + DBLE(NBL-1) * DXM * SINT

!     Calculate effective line source length (LEFF) and
!     effective downwash line length (LD)
   LEFF1 = BLAVGLLEN * SINT

   IF (NBL == 1) THEN
!        IF N = 1, NO INTERACTION AT ANY X, I.E.,
!        LEFFV = average line width;
!        'Effective' buoyancy flux (FPRMXNB) = average buoyancy flux;
!        Distance to full buoyancy (BL_XFB) =
!         (average building length) * COST + (average line width) * SINT
      LEFFV = BLAVGLWID
      FPRMXNB = BLAVGFPRM
      BL_XFB = BL_XFB + BLAVGLWID*SINT

   ELSE
!        CONSTANT 0.8333333 = 1./(2.*BETA) WITH BETA=0.6
      DXM833 = 0.8333333D0*DXM       ! DXM is 'average' distance between two lines

!        CONSTANT 2.2619467 = 2.*PI*BETA*BETA WITH BETA=0.6
!        CONSTANT 1.5915494 = 3./(PI*BETA) WITH BETA=0.6
      T1 = (2.2619467D0 * U**3 / BLAVGFPRM) *&
      &DXM833 * DXM833 * (DXM833 + 1.5915494D0*BLAVGLWID)
      XI = (T1*BLAVGLLEN)**0.333333D0

      IF (XI > BLAVGLLEN) THEN
         XI = BLAVGLLEN/2.0D0 +&
         &DSQRT(12.0D0*T1 - 3.0D0*BLAVGLLEN*BLAVGLLEN)/6.0D0

!           CONSTANT 1.2 = 2.*BETA WITH BETA=0.6
!           CONSTANT 0.6283185 = PI*BETA/3. WITH BETA=0.6
         LEFFV = FPRMXNB * (BLAVGLLEN*BLAVGLLEN/3.0D0 +&
         &XI*(XI-BLAVGLLEN)) / (1.2D0 * U**3 * DXM833 * DXM833) -&
         &0.6283185D0 * DXM833

      ELSE
!           CONSTANT 3.6 = 6.*BETA WITH BETA=0.6
!           CONSTANT 0.6283185 = PI*BETA/3. WITH BETA=0.6
         LEFFV = FPRMXNB/(3.6D0 * BLAVGLLEN * DXM833 * DXM833) *&
         &(XI/U)**3 - 0.6283185D0 * DXM833
      ENDIF
   ENDIF

   LEFF = LEFF1 + LEFFV*COST
   LD   = LEFF * SINT

!     Calculate downwashed edge radius
   R0 = DMIN1(BLAVGBHGT,LD)/AVFACTOR

   RETURN
END SUBROUTINE BL_LENG
!
!     ------------------------------------------------------------------
SUBROUTINE BL_SORT(FTSAVE,IBMIN,IBMAX,IWPBL)
!
!             BL_SORT Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     ------------------------------------------------------------------
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(INOUT) :: FTSAVE(129)
   INTEGER, INTENT(INOUT)          :: IBMIN, IBMAX, IWPBL

! Unused:      DOUBLE PRECISION :: FT
   INTEGER          :: IB, ISAFE, ILEVEL, NEACHL, INCR, INDEXI, NC
   INTEGER          :: INCRM, INCRP

   ISAFE = 0
   IB    = 0
   IF (FTSAVE(129) /= 0.0D0) IB = 129
   IF (FTSAVE(1)   /= 0.0D0) IB = 1
   IF (IB /= 0) GO TO 970

   OUTER: DO ILEVEL = 1,7
      NEACHL = 2**(ILEVEL-1)
      INCR   = 2**(8-ILEVEL)
      INDEXI = 1 + INCR/2
      DO NC = 1,NEACHL
         IF (FTSAVE(INDEXI) == 0.0D0) GO TO 944
         IB = INDEXI
         GO TO 970
944      INDEXI = INDEXI + INCR
      END DO
   END DO OUTER

   IF (IB /= 0) GO TO 970
   IWPBL = 999
   RETURN

970 IBMIN = IB - 1
   IBMAX = IB + 1
   IBMIN = MAX(IBMIN,1)
   IBMAX = MIN(IBMAX,129)

975 CONTINUE
   INCRM = 0
   INCRP = 0
   IF (FTSAVE(IBMIN) /= 0.0D0) INCRM = 1
   IF (IBMIN ==1) INCRM = 0
   IF (FTSAVE(IBMAX) /= 0.0D0) INCRP = 1
   IF (IBMAX == 129) INCRP = 0

   IBMIN = IBMIN - INCRM
   IBMAX = IBMAX + INCRP
   IF (INCRM == 0 .and. INCRP == 0) GO TO 980
   ISAFE = ISAFE + 1
   IF (ISAFE > 129) GO TO 980
   GO TO 975
980 CONTINUE

   RETURN
END SUBROUTINE BL_SORT
!
!     ------------------------------------------------------------------
SUBROUTINE BL_CUBIC(A,B,C,Z)
!
!             BL_CUBIC Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Solves for one root of the cubic equation:
!        Z**3 + A*Z**2 + B*Z + C = 0
!     ------------------------------------------------------------------
!
   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN)  :: A, B, C
   DOUBLE PRECISION, INTENT(OUT) :: Z
   DOUBLE PRECISION :: A3, AP, BP, AP3, BP2, TROOT, TR
   DOUBLE PRECISION :: APP, BSV, ONE, BPP, CM, ALPHA, SGN

   DATA ONE/1.0D0/

   A3    = A/3.0D0
   AP    = B - A*A3
   BP    = 2.0D0*A3**3 - A3*B + C
   AP3   = AP/3.0D0
   BP2   = BP/2.0D0
   TROOT = BP2*BP2 + AP3*AP3*AP3

   IF (TROOT > 0.0D0) THEN
      TR  = DSQRT(TROOT)
      APP = (-BP2 + TR)**0.333333D0
      BSV = -BP2 - TR

      IF (BSV == 0.0D0) THEN
!           BSV (& BPP) = 0.0
         Z = APP-A3

      ELSE
         SGN = DSIGN(ONE,BSV)
         BPP = SGN*(DABS(BSV))**0.333333D0
         Z   = APP + BPP - A3
      ENDIF

   ELSE
      CM    = 2.0D0 * DSQRT(-AP3)
      ALPHA = DACOS(BP/(AP3*CM))/3.0D0
      Z     = CM*DCOS(ALPHA) - A3
   ENDIF

   RETURN
END SUBROUTINE BL_CUBIC

SUBROUTINE BL_ROTATE2 (WD1, THETA, ITHETA, NRECEP, NBL, KK)
!***********************************************************************
!             BL_ROTATE2 Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Rotates Sources and Receptors for Buoyant Line Source
!                 Calculations - Aligns the Y-Axis of the Source
!                 Parallel to the Wind Direction
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    January 5, 2015
!
!        INPUT:
!          WD1    - wind direction for the hour
!          NRECEP - number of receptors to rotate
!          NBL    - total number of buoyant lines
!          KK     - bouyant line source group number
!
!        OUTPUTS:
!          Rotated Source and Receptor Coordinates
!          THETA  - Second rotation angle
!          ITHETA - Integer value of THETA
!
!        CALLED FROM:   BL_CALC
!***********************************************************************
!     Variable Declarations

   USE BUOYANT_LINE
! JAT 06/22/21 D065 COMMENT OUT KURDAT, NOT USED
!      USE MAIN1, ONLY : KURDAT

   IMPLICIT NONE
   DOUBLE PRECISION, INTENT(IN)  :: WD1
   DOUBLE PRECISION, INTENT(OUT) :: THETA
   INTEGER, INTENT(IN)  :: NRECEP, NBL, KK
   INTEGER, INTENT(OUT) :: ITHETA

   DOUBLE PRECISION, DIMENSION(4) :: TCHK =&
   &[90.0D0,180.0D0,270.0D0,360.0D0]
   DOUBLE PRECISION :: DXX, XN, YN, XSAVE, YSAVE
   DOUBLE PRECISION :: COSTHETA, SINTHETA
   DOUBLE PRECISION,PARAMETER  :: DEG_PER_RAD = 57.29578D0

   INTEGER, DIMENSION(4) :: IL = [1,1,1,1]
   INTEGER, DIMENSION(4) :: ISEG = [1,129,129,1]
! JAT 06/22/21 REMOVE ISAVE AS UNUSED VARIABLE
!      INTEGER               :: I, J, ILINE, ISAVE, ISEGN, LNUM
   INTEGER               :: I, J, ILINE, ISEGN, LNUM
   INTEGER               :: IL12, IL34

   CHARACTER (LEN=12) :: MODNAM

!     Variable Initializations
   MODNAM = 'BL_ROTATE2'

!CRT  Initialize variables ! 12/27/2017
   YN = 0.0D0
   XN = 0.0D0
! D41_Wood  Initialize variables
   ILINE = 0
   ISEGN = 0

!     THETA is the rotation that aligns the y-axis (previously rotated
!      to be perpendicular to the buoyant lines) with the flow vector.

   THETA = 360.0D0 - (WD1 + TCOR(KK))
   IF (THETA < 0.0D0) THETA = 360.0D0 + THETA
   THETA    = DMOD(THETA,360.0D0)
   ITHETA   = NINT(THETA)
   COSTHETA = DCOS(THETA/DEG_PER_RAD)
   SINTHETA = DSIN(THETA/DEG_PER_RAD)
!
!     Calculate source coordinates for each source line segment
!      Note that the coordinate YS_SCS was translated and rotated in
!      BL_ROTATE1 (soset.f)
!
   DO LNUM = 1,NBL
      IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
         DXX = DEL(LNUM)/128.0D0
         XS_SCS(LNUM,1) = BLINEPARMS(LNUM)%XBEG_TR1

         DO J=2,129
            XS_SCS(LNUM,J) = XS_SCS(LNUM,J-1) + DXX
         END DO
      END IF
   END DO

! D41_WOOD: The values of the array IL must be adjusted so the correct
!           XN and YN are determined for each BL source group
   IF (KK == 1) THEN
      IL(1) = 1
      IL(2) = 1
      IL(3) = NBLINGRP(KK)
      IL(4) = NBLINGRP(KK)

   ELSE IF (KK >= 2) THEN
      IL12 = 1
      IL34 = NBLINGRP(1)
      DO J = 2,KK
         IL12 = IL12 + NBLINGRP(J-1)
         IL34 = IL34 + NBLINGRP(J)
      END DO
      IL(1) = IL12
      IL(2) = IL12
      IL(3) = IL34
      IL(4) = IL34
   END IF

!
!     Calculate XN, YN (origins of translated coordinate system
!      in terms of the SCS coordinates)
!
   OUTER: DO LNUM = 1,NBL
      IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN

         INNER:       DO I=1,4
            IF (THETA >= TCHK(I)) CYCLE
            ILINE = IL(I)
            ISEGN = ISEG(I)
            XN = XS_SCS(ILINE,ISEGN)
            YN = YS_SCS(ILINE)
            EXIT OUTER
         END DO INNER
      END IF
   END DO OUTER

!
! --- Translate line source segment coordinates for this BL source
   DO LNUM = 1,NBL
      IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
         DO J=1,129
            XS_RCS(LNUM,J) = XS_SCS(LNUM,J) - XN
            YS_RCS(LNUM,J) = YS_SCS(LNUM) - YN
            if(mod(j,10) == 0.0) then
            endif

         END DO
      END IF
   END DO
!
!     Translate receptor coordinates
   DO  I = 1,NRECEP
      XR_RCS(I) = XR_SCS(I,KK) - XN
      YR_RCS(I) = YR_SCS(I,KK) - YN
   END DO
!
!     Rotate coordinate system
!
!     Rotate line source segment coordinates
   DO LNUM = 1,NBL

      IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN

         DO J=1,129
            XSAVE = XS_RCS(LNUM,J)
            YSAVE = YS_RCS(LNUM,J)
            XS_RCS(LNUM,J) = XSAVE*COSTHETA + YSAVE*SINTHETA
            YS_RCS(LNUM,J) = YSAVE*COSTHETA - XSAVE*SINTHETA
            if(mod(j,10) == 0.0) then
            endif
         END DO
      END IF
   END DO

!     Rotate receptor coordinates
   DO I = 1,NRECEP
      XSAVE = XR_RCS(I)
      YSAVE = YR_RCS(I)
      XR_RCS(I) = XSAVE*COSTHETA + YSAVE*SINTHETA
      YR_RCS(I) = YSAVE*COSTHETA - XSAVE*SINTHETA
   END DO

   RETURN
END SUBROUTINE BL_ROTATE2
