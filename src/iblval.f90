SUBROUTINE IBLVAL (XARG)
!=======================================================================
!             IBLVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:  Calculating effective parameters for the inhomogeneous
!             boundary layer (IBL).
!
!   Input:    Downwind distance, XARG (m)
!
!   Output:   Effective parameters for wind speed, turbulence and
!             lapse rate
!
!   Called by:  PCALC, VCALC, ACALC, PLUMEF, PWIDTH
!
!   Assumptions:
!
!   Developer(s): Roger Brode, PES, Inc.
!   Date:         January 17, 1995
!
!   Revision history:
!
!                 Modified to include LINE source type in the call to
!                 subroutine ADISZ.
!                 R.W. Brode, EPA/AQMG, 03/19/2014

!RWB              Modified to use ZRT (height of receptor above stack
!                 base) instead of ZFLAG (height of receptor above
!                 ground) in defining the layer for the effective
!                 parameters.
!                 R.W. Brode, PES, 8/5/98
!
!RWB              Added calculation of effective Dtheta/Dz (TGEFF and
!                 TGEFF3) for use in calculating stable sigma-z.
!                 R.W. Brode, PES, 8/5/98
!
!RWB              Modified to let plume centroid height follow plume
!                 centerline height above ZI/2.  Also limit upper bound
!                 of averaging layer for direct plume to be .LE. ZI.
!                 This is needed to address cases where the
!                 plume height may exceed ZI.  For the SBL, the effective
!                 parameters are calculated at the plume centerline height.
!                 R.W. Brode, PES, 1/26/95
!
!   Reference(s): "Options for the Treatment of Inhomogeneity",
!                 Al Cimorelli, Revision 5, 12/13/94
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   INTEGER :: NDXEFF, NDXBHI, NDXBLO, NDXALO
!     JAT 7/22/21 D065 SZOLD, SZ3OLD NOT USED
!      DOUBLE PRECISION :: XARG, SZNEW, ZHI, ZLO, SZOLD, SZ3NEW, SZ3OLD,
   DOUBLE PRECISION :: XARG, SZNEW, ZHI, ZLO, SZ3NEW,&
!     JAT 7/22/21 DO65 SZDOLD NOT USED
!     &                    SZDAVG, SZDNEW, SZDOLD
   &SZDAVG, SZDNEW

!
!---- Data dictionary
!
!---- Data initializations
   MODNAM = 'IBLVAL'
!
!     *************************************************************
!
!  ****** FOR HIGHLY BUOYANT PLUME ****** added code JAN 2023--kja
!  **  PPFN should be 1 when mixing height > top of penetrated plume
   IF (HBPLUME) THEN
      PPFN = 1.0D0
   ENDIF
!  *********************************added code end --kja
!RWB  Initialize the effective parameters based on
!RWB  values at plume height
   IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
      HTEFF = HE
      CALL LOCATE(GRIDHT, 1, MXGLVL, HTEFF, NDXEFF)
      CALL GINTRP( GRIDHT(NDXEFF), GRIDWS(NDXEFF),&
      &GRIDHT(NDXEFF+1), GRIDWS(NDXEFF+1), HTEFF, UEFF)
      CALL GINTRP( GRIDHT(NDXEFF), GRIDSV(NDXEFF),&
      &GRIDHT(NDXEFF+1), GRIDSV(NDXEFF+1), HTEFF,SVEFF)
      CALL GINTRP( GRIDHT(NDXEFF), GRIDSW(NDXEFF),&
      &GRIDHT(NDXEFF+1), GRIDSW(NDXEFF+1), HTEFF,SWEFF)
      CALL GINTRP( GRIDHT(NDXEFF), GRIDTG(NDXEFF),&
      &GRIDHT(NDXEFF+1), GRIDTG(NDXEFF+1), HTEFF,TGEFF)
      IF (PVMRM .OR. GRSM) THEN
         CALL GINTRP( GRIDHT(NDXEFF), GRIDEPS(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDEPS(NDXEFF+1),HTEFF,EPSEFF)
      END IF

!RWB     Modify treatment of low wind/low turbulence cases.
!RWB     R. Brode, PES, 8/15/96
      SWEFF = MAX( SWEFF, SWMIN )
      SVEFF = MAX( SVEFF, SVMIN, SVUMIN*UEFF )
      IF( L_VECTORWS )THEN
         UEFF  = DSQRT( UEFF*UEFF + 2.0D0*SVEFF*SVEFF )
      ENDIF
      UEFF  = MAX( UEFF, WSMIN )

!RJP     Add temporary debugging statement here.

      IF(DEBUG) THEN
         WRITE(DBGUNT, 6014) UEFF, SVEFF, SWEFF
6014     FORMAT(5X,'Initial effective parameters ',&
         &'for the stable ',&
         &'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',&
         &'SVeff = ',F7.2,&
         &' m/s; SWeff = ',F7.2,' m/s.',/)
      ENDIF

   ELSE IF (UNSTAB .AND. (HS.LT.ZI)) THEN

!        Direct and Indirect Source

      IF (PPF .LT. 1.0D0) THEN

!RWB        Initialize effective parameters based on vlues at the
!RWB        plume centroid height (CENTER)
         HTEFF = CENTER
         CALL LOCATE(GRIDHT, 1, MXGLVL, HTEFF, NDXEFF)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDWS(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDWS(NDXEFF+1),HTEFF, UEFFD)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDSV(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDSV(NDXEFF+1),HTEFF,SVEFFD)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDSW(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDSW(NDXEFF+1),HTEFF,SWEFFD)
         IF (PVMRM .OR. GRSM) THEN
            CALL GINTRP( GRIDHT(NDXEFF), GRIDEPS(NDXEFF),&
            &GRIDHT(NDXEFF+1), GRIDEPS(NDXEFF+1),HTEFF,EPSEFFD)
         END IF

!RWB        Modify treatment of low wind/low turbulence cases.
!RWB        R. Brode, PES, 8/15/96
         SWEFFD = MAX( SWEFFD, SWMIN )
         SVEFFD = MAX( SVEFFD, SVMIN, SVUMIN*UEFFD )
         IF( L_VECTORWS )THEN
            UEFFD = DSQRT( UEFFD*UEFFD + 2.0D0*SVEFFD*SVEFFD )
         ENDIF
         UEFFD = MAX( UEFFD, WSMIN )

!RJP        Add temporary debugging statement here.

         IF(DEBUG) THEN
            WRITE(DBGUNT, 6015) UEFFD, SVEFFD, SWEFFD
6015        FORMAT(5X,'Initial effective parameters ',&
            &'for the direct convective ',&
            &'plume:',//,5x,'UeffD = ',F7.2,' m/s; ',&
            &'SVeffD = ',F7.2,&
            &' m/s; SWeffD = ',F7.2,' m/s.',/)
         ENDIF

      END IF
!RJP
!RJP     Penetrated source
!RJP
      IF (PPF .GT. 0.0D0) THEN
!  ****** FOR HIGHLY BUOYANT PLUME ****** added code JAN 2023--kja
! ** determine next hour mix height ZIN from mechanical and convective heights
         IF (HBPLUME) THEN
            IF(ZICONVN .GT. 0.0D0 .AND. ZIMECHN .GT. 0.0D00) THEN
               ZIN = MAX(ZICONVN,ZIMECHN)
            ELSEIF( ZICONVN .LT. 0.0D0 .AND. ZIMECHN .GT. 0.0D0) THEN
               ZIN = ZIMECHN
            ELSEIF( ZICONVN .GT. 0.0D0 .AND. ZIMECHN .LT. 0.0D0) THEN
               ZIN = ZICONVN
            ELSE
               ZIN = ZI
            END IF
! ** Calculate average height between hours
            ZIAVG = (ZI+ZIN)/2.0D0
            IF(DEBUG) THEN
               WRITE(DBGUNT,6019) ZICONVN, ZIMECHN, ZIN,ZIAVG, HE3
6019           FORMAT(1X,'CONVN= ',F10.2,' MECHN= ',F10.2,' ZIN= ',F10.2,&
               &'ZIAVG= ',F10.2,'HE3= ',F10.2)
            ENDIF
         ENDIF
!  ******************************* added code end --kja
         HTEFF = HE3
         CALL LOCATE(GRIDHT, 1, MXGLVL, HTEFF, NDXEFF)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDWS(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDWS(NDXEFF+1), HTEFF, UEFF3)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDSV(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDSV(NDXEFF+1), HTEFF,SVEFF3)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDSW(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDSW(NDXEFF+1), HTEFF,SWEFF3)
         CALL GINTRP( GRIDHT(NDXEFF), GRIDTG(NDXEFF),&
         &GRIDHT(NDXEFF+1), GRIDTG(NDXEFF+1), HTEFF,TGEFF3)
         IF (PVMRM .OR. GRSM) THEN
            CALL GINTRP( GRIDHT(NDXEFF), GRIDEPS(NDXEFF),&
            &GRIDHT(NDXEFF+1), GRIDEPS(NDXEFF+1),HTEFF,EPSEFF3)
         END IF

!RWB        Modify treatment of low wind/low turbulence cases.
!RWB        R. Brode, PES, 8/15/96
         SWEFF3 = MAX( SWEFF3, SWMIN )
         SVEFF3 = MAX( SVEFF3, SVMIN, SVUMIN*UEFF3 )
         IF( L_VECTORWS )THEN
            UEFF3 = DSQRT( UEFF3*UEFF3 + 2.0D0*SVEFF3*SVEFF3 )
         ENDIF
         UEFF3  = MAX( UEFF3, WSMIN )

!RJP        Add temporary debugging statement here.

         IF(DEBUG) THEN
            WRITE(DBGUNT, 6016) PPF, UEFF3,&
            &SVEFF3, SWEFF3
6016        FORMAT(5X,'Penetration fraction = ',f6.3,/,&
            &5X,'Initial effective parameters ',&
            &'for the penetrated ',&
            &'plume:',//,5x,'Ueff3 = ',F7.2,' m/s; ',&
            &'SVeff3 = ',F7.2,&
            &' m/s; SWeff3 = ',F7.2,' m/s.',/)
         END IF
      END IF

   END IF

!     End initialization.  Next compute averages across plume layer.

   IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
!        Determine Dispersion Parameters              ---   CALL PDIS
      CALL PDIS ( XARG )
   ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
!        Determine Dispersion Parameters              ---   CALL VDIS
      CALL VDIS ( XARG )
   ELSE IF (SRCTYP(ISRC) .EQ. 'AREA' .OR.&
   &SRCTYP(ISRC) .EQ. 'AREAPOLY' .OR.&
   &SRCTYP(ISRC) .EQ. 'AREACIRC' .OR.&
   &SRCTYP(ISRC) .EQ. 'LINE' .OR.&
   &SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
!        Determine Vertical Dispersion Parameters     ---   CALL ADISZ
      CALL ADISZ ( XARG )
   END IF

   IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN

      SZNEW  = SZ
      CENTER = HE
      IF (CENTER .LE. 5.0D0 .AND. ZRT .LE. 5.0D0) THEN
         ZHI = 5.0D0
         ZHI = MIN( ZHI, ZI )
         ZLO = 0.0D0
      ELSE IF (CENTER .GT. ZRT) THEN
         ZHI = CENTER
         ZLO = MAX(CENTER - SZCOEF*SZNEW, ZRT)
      ELSE
         ZHI = MIN(CENTER + SZCOEF*SZNEW, ZRT)
         ZLO = CENTER
      END IF

      IF(DEBUG) THEN
         IF( EVONLY )THEN
            WRITE(DBGUNT, 6030) IEVENT, CENTER, SZNEW, ZRT,ZLO,ZHI
6030        FORMAT(5X,'Stable plume calculation',&
            &' for EVENT # ',I3,//,&
            &5x,'Height of plume center of mass = ',f6.1,&
            &' m; Sigma-z estimate = ',f11.1,' m; ',&
            &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
            &'effective parameters are averaged between ',&
            &f6.1,' and ',F6.1,' meters.',/)
         ELSE
            WRITE(DBGUNT, 6031) IREC, CENTER, SZNEW, ZRT,ZLO,ZHI
6031        FORMAT(5X,'Stable plume calculation',&
            &' for receptor # ',I3,//,&
            &5x,'Height of plume center of mass = ',f6.1,&
            &' m; Sigma-z estimate = ',f11.1,' m; ',&
            &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
            &'effective parameters are averaged between ',&
            &f6.1,' and ',F6.1,' meters.',/)
         ENDIF
      END IF

      CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
      CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)
      NDXALO = NDXBLO + 1
      CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZLO,NDXALO,&
      &ZHI,NDXBHI,UEFF )
      CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV, ZLO,NDXALO,&
      &ZHI,NDXBHI,SVEFF )
      CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW, ZLO,NDXALO,&
      &ZHI,NDXBHI,SWEFF )
      CALL ANYAVG ( MXGLVL, GRIDHT, GRIDTG, ZLO,NDXALO,&
      &ZHI,NDXBHI,TGEFF )
      IF (PVMRM .OR. GRSM) THEN
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDEPS, ZLO,NDXALO,&
         &ZHI,NDXBHI,EPSEFF )
      END IF
!     JAT 7/22/21 D065 SZOLD NOT USED
!         SZOLD = SZ

!RWB     Modify treatment of low wind/low turbulence cases.
!RWB     R. Brode, PES, 8/15/96
      SWEFF = MAX( SWEFF, SWMIN )
      SVEFF = MAX( SVEFF, SVMIN, SVUMIN*UEFF )
      IF( L_VECTORWS )THEN
         UEFF = DSQRT( UEFF*UEFF + 2.0D0*SVEFF*SVEFF )
      ENDIF
      UEFF = MAX( UEFF, WSMIN )

!RJP     Add temporary debugging statement here.

      IF(DEBUG) THEN
         WRITE(DBGUNT, 6032) UEFF, SVEFF, SWEFF
6032     FORMAT(5X,'Effective parameters for stable ',&
         &'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',&
         &'SVeff = ',F7.2,&
         &' m/s; SWeff = ',F7.2,' m/s.',/)
      END IF

   ELSE IF (UNSTAB .AND. (HS.LT.ZI)) THEN
!RJP
!RJP  Process effective values for direct and penetrated plumes
!RJP
!RJP  First, process the penetrated plume, then the direct plumes.
!RJP

      IF( PPF .GT. 0.0D0 )THEN
!  ********* FOR HIGHLY BUOYANT PLUME ********* added code JAN 2023--kja
!  ** how much of penetrated plume still above ZIAVG
!  ** assuming gaussian entrainment factor
         IF (HBPLUME) THEN
            HHTOP = HE3 + 2.15D0*SZ3  ! top of plume
            HHBOT = MAX(HE3 - 2.15D0*SZ3,ZRT)  ! Bottom of plume
! ** width of plume to 2.15 sigma-z - where conc. falls to 10% of centerline
            PPWID = HHTOP - HHBOT
! ** difference between top of plume and ZIAVG mixing height
            HTOPDIF = HHTOP - ZIAVG
            IF (HTOPDIF .GT. 0.0D0) THEN  ! top of plume > mixing ht
! ** PPFN should be between 0 - 1
               IF(HTOPDIF .LT. PPWID) THEN ! mixing ht within plume
                  IF(ZIAVG .LE. HE3) THEN
! ** PPFN from 0 to 0.5 - amount of penetrated plume entrained
! ** lower half of plume
                     PPFN = 0.5D0*ERF((ZIAVG-HHBOT)/SZ3/DSQRT(2.0D0))
                  ELSE
! ** PPFN from 0.5 to 1.0 - amount of penetrated plume entrained
! ** more than half of plume entrained
                     PPFN = 0.5D0*(1.0D0 +&
                     &ERF((ZIAVG-HE3)/SZ3/DSQRT(2.0D0)))
                  ENDIF
               ELSE
! ** whole penetrated plume is still above average mixing height
! ** no contribution from penetrated plume
                  PPFN = 0.0D0
               ENDIF
            ELSE
! ** whole penetrated plume below below ZIAVG
               PPFN = 1.0D0
            END IF
            SZ3DBG = SZ3
         ENDIF
!  ***********************************************  added code end --kja
         SZ3NEW = SZ3

!RWB        Change ZEFF to ZRT in following block. RWB 1/23/95
         IF(HE3 .GT. ZRT) THEN
            ZHI = HE3
            ZLO = MAX(HE3 - SZCOEF*SZ3NEW, ZRT)
         ELSE
            ZHI = MIN(HE3 + SZCOEF*SZ3NEW, ZRT)
            ZLO = HE3
         END IF

!RJP        Add temporary debugging statement here.

         IF (DEBUG) THEN
            IF( EVONLY )THEN
               WRITE(DBGUNT, 6040) IEVENT, HE3, SZ3NEW,&
               &ZRT,ZLO,ZHI
6040           FORMAT(5X,'Penetrated plume calculation',&
               &' for EVENT # ',I3,//,&
               &5x,'Height of plume center of mass = ',&
               &f6.1,' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',F6.1,' meters.',/)
            ELSE
               WRITE(DBGUNT, 6041) IREC, HE3, SZ3NEW,&
               &ZRT,ZLO,ZHI
6041           FORMAT(5X,'Penetrated plume calculation',&
               &' for receptor # ',I3,//,&
               &5x,'Height of plume center of mass = ',&
               &f6.1,' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',F6.1,' meters.',/)
            ENDIF
         END IF

         CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)
         NDXALO = NDXBLO + 1
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS,ZLO,NDXALO,&
         &ZHI,NDXBHI,UEFF3 )
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV,ZLO,NDXALO,&
         &ZHI,NDXBHI,SVEFF3 )
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW,ZLO,NDXALO,&
         &ZHI,NDXBHI,SWEFF3 )
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDTG,ZLO,NDXALO,&
         &ZHI,NDXBHI,TGEFF3 )
         IF (PVMRM .OR. GRSM) THEN
            CALL ANYAVG ( MXGLVL, GRIDHT, GRIDEPS, ZLO,NDXALO,&
            &ZHI,NDXBHI,EPSEFF3 )
         END IF
!     JAT 7/22/21 D065 SZ3OLD NOT USED
!            SZ3OLD = SZ3

!RWB        Modify treatment of low wind/low turbulence cases.  R. Brode, PES,
!RWB        8/15/96
         SWEFF3 = MAX( SWEFF3, SWMIN )
         SVEFF3 = MAX( SVEFF3, SVMIN, SVUMIN*UEFF3 )
         IF( L_VECTORWS )THEN
            UEFF3 = DSQRT( UEFF3*UEFF3 + 2.0D0*SVEFF3*SVEFF3 )
         ENDIF
         UEFF3  = MAX( UEFF3, WSMIN )

!RJP        Add temporary debugging statement here.

         IF(DEBUG) THEN
            WRITE(DBGUNT, 6042) UEFF3, SVEFF3, SWEFF3
6042        FORMAT(5X,'Effective parameters for penetrated ',&
            &'plume:',//,5x,'Ueff3 = ',F7.2,' m/s; ',&
            &'SVeff3 = ',F7.2,&
            &' m/s; SWeff3 = ',F7.2,' m/s.',/)
         END IF

      END IF

      IF (PPF .LT. 1.0D0) THEN

!RJP        Process the direct plume components here. *************************

         SZDAVG = 0.5D0 * (SZD1 + SZD2)
         SZDNEW = SZDAVG

!RWB        Change ZEFF to ZRT in following block. RWB 1/23/95
         IF (CENTER .LE. 5.0D0 .AND. ZRT .LE. 5.0D0) THEN
            ZHI = MIN( 5.0D0, ZI )
            ZHI = MIN( ZHI, ZI )
            ZLO = 0.0D0
         ELSE IF(CENTER .GT. ZRT) THEN
!RWB           Limit ZHI to be .LE. ZI
            ZHI = MIN (CENTER, ZI)
            ZLO = MAX (CENTER - SZCOEF*SZDNEW, ZRT)
         ELSE
            ZHI = MIN (CENTER + SZCOEF*SZDNEW, ZRT)
            ZHI = MIN (ZHI, ZI)
            ZLO = CENTER
         ENDIF

!RJP        Add temporary debugging statement here.

         IF(DEBUG) THEN
            IF( EVONLY )THEN
               WRITE(DBGUNT, 6050) IEVENT, CENTER,&
               &SZDNEW, ZRT, ZLO, ZHI
6050           FORMAT(5X,'Direct plume calculation',&
               &' for EVENT # ',I3,//,&
               &5x,'Height of plume center of mass = ',f6.1,&
               &' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',F6.1,' meters.',/)
            ELSE
               WRITE(DBGUNT, 6051) IREC, CENTER,&
               &SZDNEW, ZRT, ZLO, ZHI
6051           FORMAT(5X,'Direct plume calculation',&
               &' for receptor # ',I3,//,&
               &5x,'Height of plume center of mass = ',f6.1,&
               &' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',F6.1,' meters.',/)
            ENDIF
         END IF

!RWB        Check for ZHI .LE. ZLO, skip averages
         IF (ZHI .GT. ZLO) THEN
            CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
            CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)
            NDXALO = NDXBLO + 1
            CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZLO,&
            &NDXALO,ZHI,NDXBHI,UEFFD )
            CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV, ZLO,&
            &NDXALO,ZHI,NDXBHI,SVEFFD )
            CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW, ZLO,&
            &NDXALO,ZHI,NDXBHI,SWEFFD )
            IF (PVMRM .OR. GRSM) THEN
               CALL ANYAVG ( MXGLVL, GRIDHT, GRIDEPS, ZLO,&
               &NDXALO,ZHI,NDXBHI,EPSEFFD )
            END IF
         ELSE
!RWB           Use values at ZI if ZHI .LE. ZLO
            HTEFF = ZI
            CALL LOCATE(GRIDHT, 1, MXGLVL, HTEFF, NDXEFF)
            CALL GINTRP( GRIDHT(NDXEFF), GRIDWS(NDXEFF),&
            &GRIDHT(NDXEFF+1), GRIDWS(NDXEFF+1), HTEFF, UEFFD)
            CALL GINTRP( GRIDHT(NDXEFF), GRIDSV(NDXEFF),&
            &GRIDHT(NDXEFF+1), GRIDSV(NDXEFF+1), HTEFF,SVEFFD)
            CALL GINTRP( GRIDHT(NDXEFF), GRIDSW(NDXEFF),&
            &GRIDHT(NDXEFF+1), GRIDSW(NDXEFF+1), HTEFF,SWEFFD)
            IF (PVMRM .OR. GRSM) THEN
               CALL GINTRP( GRIDHT(NDXEFF), GRIDEPS(NDXEFF),&
               &GRIDHT(NDXEFF+1), GRIDEPS(NDXEFF+1), HTEFF,EPSEFFD)
            END IF
         END IF
!     JAT 7/22/21 DO65 SZDOLD NOT USED
!            SZDOLD = SZDAVG


!RWB        Modify treatment of low wind/low turbulence cases.
!RWB        R. Brode, PES, 8/15/96
         SWEFFD = MAX( SWEFFD, SWMIN )
         SVEFFD = MAX( SVEFFD, SVMIN, SVUMIN*UEFFD )
         IF( L_VECTORWS )THEN
            UEFFD = DSQRT( UEFFD*UEFFD + 2.0D0*SVEFFD*SVEFFD )
         ENDIF
         UEFFD  = MAX( UEFFD, WSMIN )

!RJP        Add temporary debugging statement here.

         IF(DEBUG) THEN
            WRITE(DBGUNT, 6052) UEFFD, SVEFFD, SWEFFD
6052        FORMAT(5X,'Effective parameters for direct ',&
            &'plume:',//,5x,'UeffD = ',F7.2,' m/s; ',&
            &'SVeffD = ',F7.2,&
            &' m/s; SWeffD = ',F7.2,' m/s.',/)
         END IF

      END IF

   END IF

!RWB  Set effective parameters for indirect source = direct source
   IF (UNSTAB .AND. HS.LT.ZI) THEN
      UEFFN  = UEFFD
      SVEFFN = SVEFFD
      SWEFFN = SWEFFD
   END IF

   RETURN
END

SUBROUTINE METINI
!=======================================================================
!             METINI Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:  To compute the met parameters at stack top and the averages
!             within the mixed layer
!
!   Input:
!
!   Output:
!
!   Called by:   PCALC
!
!   Assumptions:
!
!   Developer(s): Jim Paumier and Roger Brode, PES, Inc.
!   Date:         30 September 1993
!
!   Revision history:
!                      Added SVP and SWP for Aircraft Plume Rise
!                      Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA
!                      04/01/2023
!
!                      Added initialization of effective parameters
!                      to stack top parameters, including TGEFF and
!                      TGEFF3, replacing intializations that were
!                      formerly included in subroutine PCALC.
!                      R.W. Brode, PES, 12/6/99
!
!                      Calls to ZIAVER to average sigma-V, sigma-W
!                      and wind speed moved here from METEXT.  This
!                      allows averaging up to HS when it is higher
!                      than ZI.  It now averages from the surface to
!                      the higher of ZI or HS.  Ref:  Summary of AERMOD
!                      equations, A. Venkatram, 7/7/94.  Changed 7/12/94
!                      by Russell F. Lee.
!
!                      Added calculation of local vertical lagrangian
!                      time scales at stack height and at ZI/2.  These
!                      are needed for calculating the effective TsubLZ
!                      and the horizontal lagrangian time scale,
!                      respectively.  Changed 7/14/94 by R.F. Lee
!
!
!   Reference(s): "Inhomogeneous Boundary Layer", A. Venkatram, 6/25/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: VALABV, VBELOW
!---- Declare SVS2 variable to save SVS before SVMIN adjustment for use
!     in US adjustment under LowWind2 option
!      DOUBLE PRECISION :: SVS2

!---- Data dictionary
!
!---- Data initializations
!---- Data initializations
   MODNAM = 'METINI'
!
!.......................................................................
!---- Compute the parameter values at stack height

!CRFL
!CRFL  Add calculation of local vertical lagrangian time scale
!CRFL  at stack height and at ZI/2.
!CRFL

   IF (NDXSTK(ISRC) .GE. 1) THEN
!----    Sigma_V at stack height
      CALL GINTRP( GRIDHT(NDXSTK(ISRC)), GRIDSV(NDXSTK(ISRC)),&
      &GRIDHT(NDXSTK(ISRC)+1), GRIDSV(NDXSTK(ISRC)+1),&
      &HS, SVS )

!----    Sigma_W
      CALL GINTRP( GRIDHT(NDXSTK(ISRC)), GRIDSW(NDXSTK(ISRC)),&
      &GRIDHT(NDXSTK(ISRC)+1), GRIDSW(NDXSTK(ISRC)+1),&
      &HS, SWS )

!----    Wind speed
      CALL GINTRP( GRIDHT(NDXSTK(ISRC)), GRIDWS(NDXSTK(ISRC)),&
      &GRIDHT(NDXSTK(ISRC)+1), GRIDWS(NDXSTK(ISRC)+1),&
      &HS, US )

!----    Wind direction
!----    Check for 360 crossover and adjust if necessary
      VALABV = GRIDWD(NDXSTK(ISRC)+1)
      VBELOW = GRIDWD(NDXSTK(ISRC))

      IF( (VALABV-VBELOW) .LT. -180.0D0) THEN
         VALABV = VALABV + 360.0D0
      ELSE IF( (VALABV-VBELOW) .GT. 180.0D0) THEN
         VALABV = VALABV - 360.0D0
      END IF

!----    Assign Wind direction
      IF (VBELOW .EQ. VALABV) THEN
         WDIR = VBELOW
      ELSE
!----       Interpolate to HS
         CALL GINTRP( GRIDHT(NDXSTK(ISRC)), VBELOW,&
         &GRIDHT(NDXSTK(ISRC)+1), VALABV,&
         &HS, WDIR )
      END IF

!        Check for WDIR > 360 or < 0
      IF (WDIR .GT. 360.0D0) THEN
         WDIR = WDIR - 360.0D0
      ELSE IF (WDIR .LE. 0.0D0) THEN
         WDIR = WDIR + 360.0D0
      END IF
!
!----    Potential temperature gradient
      CALL GINTRP( GRIDHT(NDXSTK(ISRC)), GRIDTG(NDXSTK(ISRC)),&
      &GRIDHT(NDXSTK(ISRC)+1), GRIDTG(NDXSTK(ISRC)+1),&
      &HS, TGS )

!----    Potential temperature
      CALL GINTRP( GRIDHT(NDXSTK(ISRC)), GRIDPT(NDXSTK(ISRC)),&
      &GRIDHT(NDXSTK(ISRC)+1), GRIDPT(NDXSTK(ISRC)+1),&
      &HS, PTS )

   ELSE
!        Use GRID value for lowest level
      SVS  = GRIDSV(1)
      SWS  = GRIDSW(1)
      US   = GRIDWS(1)
      WDIR = GRIDWD(1)
      TGS  = GRIDTG(1)
      PTS  = GRIDPT(1)
   END IF

!RWB  Modify the treatment of low wind/low turbulence cases per 7/31/96
!RWB  write-up by Steve Perry.  R. Brode, PES, 8/15/96
   SWS = MAX( SWS, SWMIN )
   SVS = MAX( SVS, SVMIN, SVUMIN*US )
   IF( L_VECTORWS )THEN
      US  = DSQRT( US*US + 2.0D0*SVS*SVS )
   ENDIF
   US  = MAX( US, WSMIN )

!
!---- If the wind for the hour is not calm or missing, then convert
!     direction to radians, compute sine and cosine of direction,
!     and determine nearest 10-degree sector.  Note, we shouldn't
!     reach this point if CLMHR or MSGHR is .TRUE.
!
   IF( .NOT.CLMHR .AND. .NOT.MSGHR )THEN
!
!---->   wind direction = wind direction in degrees * DTORAD

      WDSIN = DSIN(WDIR * DTORAD)
      WDCOS = DCOS(WDIR * DTORAD)

      AFV = WDIR - 180.0D0
      IF (AFV .LT. 0.0D0) THEN
         AFV = AFV + 360.0D0
      END IF
      IFVSEC = IDINT (AFV*0.10D0 + 0.4999D0)
      IF (IFVSEC .EQ. 0) IFVSEC = 36

   END IF

!
!     ------------------------------------------------------------
!     Apply lower limit of 0.002 K/m to lapse rate for stable
!     layers.
!     ------------------------------------------------------------
!
!RJP
!RJP  ASSIGN TGP AS TGS INITIALLY
!RJP
   TGP = TGS
!

!---- Calculate potential temperature at stack height, PTS, for plume
!     rise calculations.  Compute stack height ambient temperature, TA.
!     NOTE:  TA is no longer the temperature read in by METEXT from the
!            scalar file
   TA = PTS - GOVRCP * ( HS + ZBASE )

!---- Assign wind speed to use for plume rise, UP = US
   UP = US
   SVP = SVS       ! Added for ARISE; UNC-IE
   SWP = SWS       ! Added for ARISE; UNC-IE
!     Compute the Brunt-Vaisala frequency, BVF, at stack height for STABLE
!     conditions or for UNSTAB releases above ZI.  Check for TGS < 0 first.
   IF ( (TGS.GT.0.0D0) .AND.&
   &(STABLE .OR. (UNSTAB .AND. HS.GE.ZI)) ) THEN
      BVF = DSQRT( G * TGS / PTS )
   ELSE
      BVF = 1.0D-10
   END IF

   IF( BVF .LT. 1.0D-10 )THEN
      BVF =  1.0D-10
   END IF

   BVPRIM  = 0.7D0 * BVF

!RJP  For downwash calculations, set temporarily assigned effective values
   UEFF  = US
   SVEFF = SVS
   SWEFF = SWS
   TGEFF = TGS
   UEFFD  = US
   SVEFFD = SVS
   SWEFFD = SWS
!RWB  Add effective parameters for indirect plume.  RWB, 12/8/94
   UEFFN  = US
   SVEFFN = SVS
   SWEFFN = SWS
   UEFF3  = US
   SVEFF3 = SVS
   SWEFF3 = SWS
   TGEFF3 = TGS

!     Define temporary values of CENTER and SURFAC based on HS
   CENTER = HS
   IF( CENTER .LT. 0.1D0*ZI )THEN
      SURFAC = .TRUE.
   ELSE
      SURFAC = .FALSE.
   END IF

   RETURN
END

SUBROUTINE LOCATE ( PARRAY, LVLBLW, LVLABV, VALUE, NDXBLW )
!=======================================================================
!             LOCATE Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To return the array index such that VALUE is between
!                PARRAY(NDXBLW) and PARRAY(NDXBLW+1).
!
!   Input:       Array of gridded values (PARRAY)
!                Lower array bound at which to start the search (LVLBLW)
!                Upper array bound at which to end the search (LVLABV)
!                Value being searched for (VALUE)
!
!   Output:      Index of PARRAY immediately below VALUE (NDXBLW)
!
!   Called by:   Utility routine that can be used by any module:
!                  SRCSET (in SOSET) for stack heights
!                  METEXT for mixing height
!
!   Assumptions: PARRAY must be montonically increasing or decreasing;
!                LVLBLW can be no less than 1;
!
!   Developer(s): Jim Paumier and Roger Brode, PES, Inc.
!   Date:         30 September 1993
!
!   Revision history:
!                <none>
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE

   INTEGER   LVLABV, LVLBLW, NDXBLW, JL, JM, JU
   DOUBLE PRECISION  PARRAY(LVLABV), VALUE
!
!---- Data dictionary
!     JL   lower bound temporary variable
!     JM   midpoint temporary variable
!     JU   upper bound temporary variable
!
!----
   JL = LVLBLW - 1
   JU = LVLABV + 1

   DO WHILE( (JU - JL) .GT. 1 )

      JM = (JU + JL) / 2

      IF( VALUE .GE. PARRAY(JM) )THEN
         JL = JM
      ELSE
         JU = JM
      ENDIF

   ENDDO

   NDXBLW = MIN( JL, LVLABV-1 )

   RETURN
END


!RJP  Add subroutine ANYAVG

SUBROUTINE ANYAVG ( NLVLS,HTS,PARRAY,ZBOT,NDXABV,ZTOP,NDXBLW,&
&VALAVG)
!***********************************************************************
!             ANYAVG Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To compute the average value of the parameter between
!                any two heights (ZBOT and ZTOP)
!
!   Input:       Number of levels in the profile (NLVLS)
!                Array of gridded profile heights (HTS)
!                Parameter array (PARRAY)
!                Lower bound of averaging layer (ZBOT)
!                Index of the level gridded profile height immediately
!                   above ZBOT (NDXABV)
!                Upper bound of averaging layer (ZTOP)
!                Index of the level gridded profile height immediately
!                   below ZTOP (NDXBLW)
!
!   Output:      Average value of parameter in layer (VALAVG);
!
!   Called by:   METEXT
!
!   Assumptions: If ZTOP is above the highest profile height (5000 m),
!                then we assume the profile is constant
!                (= PARRAY(NLVLS)) above 5000 m and compute
!                the average accordingly.
!
!   Adjustments: If ZBOT is less than 0.5 m, it is set to 0.5 m.  If ZTOP
!                is less than 0.5 m, it is set to 0.51 m.
!
!   Programmer:  Bob Paine
!
!   Date:        October 4, 1994
!
!   Revision history:
!                Derived from ZIAVER
!
!   Reference(s): Alternative Approach to Treatment of inhomogeneity
!                 October 3, 1994 (Al Cimorelli)
!
!***********************************************************************
!
!---- Variable declarations
!
   IMPLICIT NONE

   INTEGER   I, NLVLS, NDXABV, NDXBLW
   DOUBLE PRECISION  HTS(NLVLS), PARRAY(NLVLS), ZBOT, ZTOP,&
   &SUM, VALAVG
   DOUBLE PRECISION  VALBOT, VALTOP
!
!---- Data initializations
!
!.......................................................................
!
   SUM = 0.0D0
!
!     NDXABV is the profile index of the height just above ZBOT, and
!     NDXBLW is the profile index of the height just below ZTOP.
!
!---- Sum over each layer of the gridded profile (PARRAY) from NDXABV
!     to NDXBLW.  First, check to see if ZBOT and ZTOP are so close
!     together that summation over several profile levels is not
!     necessary.
!
!     Check for minimum values of ZTOP and ZBOT.
!
   IF(ZBOT .LT. 0.5D0) THEN
      ZBOT = 0.5D0
      NDXABV = 2
   ENDIF
   IF(ZTOP .LT. 0.51D0) THEN
      ZTOP = 0.51D0
      NDXBLW = 2
   ENDIF
!
   IF(NDXBLW .LT. NDXABV) GO TO 300
   IF(NDXBLW .EQ. NDXABV) GO TO 200
!
!     Sum using trapezoidal rule over intermediate profile layers.
!
   DO I = NDXABV+1, NDXBLW
      SUM = SUM + (HTS(I) - HTS(I-1)) * 0.5D0 *&
      &(PARRAY(I) + PARRAY(I-1))
   END DO
!
!---- Finish the summation over partial layers at bottom (first), then
!     the top.
!
200 CONTINUE
   IF(NDXABV .GT. 1) THEN
      CALL GINTRP(HTS(NDXABV-1),PARRAY(NDXABV-1),HTS(NDXABV),&
      &PARRAY(NDXABV),ZBOT,VALBOT)
      SUM = SUM + (HTS(NDXABV) - ZBOT) * 0.5D0 *&
      &(VALBOT + PARRAY(NDXABV) )
   ELSE
      SUM = SUM + (HTS(1) - ZBOT) * PARRAY(1)
   ENDIF

   IF(NDXBLW .LT. NLVLS) THEN
      CALL GINTRP(HTS(NDXBLW),PARRAY(NDXBLW),HTS(NDXBLW+1),&
      &PARRAY(NDXBLW+1),ZTOP,VALTOP)
      SUM = SUM + (ZTOP - HTS(NDXBLW)) * 0.5D0 *&
      &(VALTOP + PARRAY(NDXBLW) )
   ELSE
      SUM = SUM + (ZTOP - HTS(NLVLS)) * PARRAY(NLVLS)
   ENDIF
!
!     Take average
!
   VALAVG = SUM / (ZTOP - ZBOT)
   GO TO 999
!
!     At 300, just take the interpolated value halfway between ZBOT
!     and ZTOP, because both are within the same profile layer.
!
300 CALL GINTRP(HTS(NDXABV-1),PARRAY(NDXABV-1),HTS(NDXABV),&
   &PARRAY(NDXABV),0.5D0*(ZBOT+ZTOP),VALAVG)
!
999 RETURN
END
