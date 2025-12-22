SUBROUTINE DELTAH ( XARG )
!***********************************************************************
!             DELTAH Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise
!
!   PROGRAMMER: Roger Brode, Jim Paumier, PES, Inc.
!
!   DATE:    September 30, 1993
!
!   CHANGES:  Removed reference to PARAMS.PRI include file.
!             R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!   CHANGES:  Modified to maintain better consistency with ISCST3 for
!             Schulman-Scire downwash algorithm.
!             Roger Brode, PES, Inc. - 12/6/99
!
!   CHANGES:  Corrected variable name from SVMP to SVPM.
!             Roger Brode, PES, Inc. - 5/24/99
!
!   CHANGES:  Added XARG as actual argument for CBLPRN.
!             Roger Brode, PES, Inc. - 12/5/94
!
!   INPUTS:  The distance at which to make the computation, XARG
!
!   OUTPUTS: Distance-Dependent Plume Rise, DHP (m)
!
!   CALLED FROM:   PCALC
!
!   Assumptions:  All plume rise calculations are for gradual rise,
!                 except in stable conditions when the downwind distance
!                 exceeds XMAX
!
!   References:   "Dispersion in the Stable Boundary Layer",
!                 A. Venkatram, 2/12/93
!                 "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!                 "Plume Penetration of the CBL and Source 3: Source
!                 Strength and Plume Rise", J. Weil, 9/1/93
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE

   CHARACTER MODNAM*12
   INTEGER :: KITER, NDXZPL
   DOUBLE PRECISION :: XARG, XMAXTMP, XRISE, ZPLM, DHPOLD,&
   &SVPM, UPM, TGPM, PTPM, PTP

!     Variable Initializations
   MODNAM = 'DELTAH'

   IF( (STABLE  .OR.  (UNSTAB  .AND.  (HS .GE. ZI)))  .AND.&
   &(XARG .GE. XMAX) )THEN
!        Use final stable plume rise (DHF) calculated in DISTF (DHP)
!        at XMAX
      DHP = DHFAER


   ELSE IF( (STABLE  .OR. (UNSTAB  .AND.  (HS .GE. ZI))) .AND.&
   &(XARG .LT. XMAX) ) THEN
!----    Compute stable plume rise for the distance XARG   --- CALL SBLRIS
!        Use iterative approach to plume rise calculations.
!        First compute temporary distance to "final rise" based on current
!        values of UP and BVPRIM.  Then, don't pass a distance larger than
!        XMAXTMP to SBLRIS.  This avoids potential for math error in
!        SUB. SBLRIS for distances beyond the value of XMAX computed
!----    iteratively outside the receptor loop in SUB. DISTF.
      XMAXTMP = UP * DATAN2( FM*BVPRIM, -FB ) / BVPRIM
      XRISE   = MIN( XARG, XMAXTMP )
      CALL SBLRIS ( XRISE )
      KITER = 0

50    ZPLM = HSP + 0.5D0 * DHP
      DHPOLD = DHP

!----    Locate index below ZPLM

      CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

!----    Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
!        vertical potential temperature gradient, if stable.

      CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
      CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )

      SVPM = MAX( SVPM, SVMIN, SVUMIN*UPM )
      IF( L_VECTORWS )THEN
         UPM = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
      ENDIF
      UPM  = MAX( UPM, WSMIN )

!RWB     Use average of stack top and midpoint wind speeds.
      UP = 0.5D0 * (US + UPM)

      CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
      CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),&
      &GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
!RWB     Use average of stack top and midpoint temperature gradients.
      TGP = 0.5D0 * (TGS + TGPM)
      PTP = 0.5D0 * (PTS + PTPM)
      BVF = DSQRT( G * TGP / PTP)
      IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
      BVPRIM  = 0.7D0 * BVF

!        Repeat calculation of temporary distance to "final rise" using
!        current values of UP and BVPRIM.
      XMAXTMP = UP * DATAN2( FM*BVPRIM, -FB ) / BVPRIM
      XRISE   = MIN( XARG, XMAXTMP )
      CALL SBLRIS ( XRISE )

      KITER = KITER + 1

!RJP     Add temporary debugging statements

      IF(DEBUG) THEN
         WRITE(DBGUNT,6001) KITER,DHPOLD, DHP, ZPLM, UP,TGP
6001     FORMAT(/,5X,'OPTH2 ITER. #',I1,': OLD DELH = ',&
         &F6.1,' M; NEW DELH = ',F6.1,' M; MET LEVEL = ',&
         &F6.1,' M; NEW Upl = ',F5.2,' M/S; NEW DTHDZ = ',&
         &F7.4,' K/M')
      ENDIF

!        Check for convergence
      IF(DHP.GT.0.0D0 .AND. DABS((DHPOLD-DHP)/DHP).LT.0.001D0 .AND.&
      &KITER .GE. 5)THEN
         IF( DHP .LE. 1.0D-5 )THEN
            DHP = 1.0D-5
         ENDIF
         GO TO 60
      ELSEIF(KITER .LT. 10)THEN
         GO TO 50
      ENDIF

      IF(KITER .GE. 5) THEN
         DHP = 0.5D0 * (DHP + DHPOLD)
         IF(DEBUG) WRITE(DBGUNT,6002) DHP
6002     FORMAT(/,5X,'OPTH2 ITERATION FAILED TO CONVERGE; PLUME',&
         &' RISE SET AT ',F6.1,' METERS.',/)
         GO TO 60
      ELSE
         GO TO 50
      ENDIF

60    CONTINUE

!RWB     After completing iteration, reset UP and TGP to stack top
!RWB     values for subsequent distance-dependent plume rise calcs.
      UP  = US
      TGP = TGS
      PTP = PTS
      BVF = DSQRT( G * TGP / PTP )
      IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
      BVPRIM  = 0.7D0 * BVF
!crfl-3/6/95 Make sure SBL rise is not greater than CBL rise.
      CALL CBLPRD(XARG)
      DHP = MIN(DHP,DHP1)
      DHP = MIN(DHP,DHFAER)

   ELSEIF( UNSTAB )THEN
!        (i.e., for UNSTABle cases, with HS < ZI)

!        Compute  plume rise for the direct plume       --- CALL CBLPRD
      CALL CBLPRD ( XARG )

!        Compute  plume rise for the indirect plume        --- CALL CBLPRN
      CALL CBLPRN ( XARG )

      IF( PPF .GT. 0.0D0 )THEN
!           Compute plume rise for the penetrated plume    --- CALL CBLPR3
         CALL CBLPR3

      ELSE
!           No plume penetration - plume rise is zero for this source
         DHP3 = 0.0D0

      ENDIF

   ENDIF

   RETURN
END

SUBROUTINE PRMDELH ( XARG, L_INWAKE )
!***********************************************************************
!             PRMDELH Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for PRIME concentration
!
!   PROGRAMMER: Roger Brode, PES, Inc.
!
!   DATE:     July 5, 2001
!
!   CHANGES:  Replaced reference to PARAMS.PRI include file with
!             PRIME_wakedat MODULE subprogram.
!             R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!             Modified to save L_INWAKE to local variable.
!             R. W. Brode, MACTEC (f/k/a PES), Inc., 08/02/05
!
!   INPUTS:  The distance at which to make the computation, XARG
!
!   OUTPUTS: Distance-Dependent Plume Rise, DHP (m)
!
!   CALLED FROM:   PRMCALC
!***********************************************************************

!     Variable Declarations
   USE MAIN1
! --- Include PRIME plume rise parameters
   USE PRIME_wakedat
   USE PRM2_WAKEDAT, ONLY: DFSN2CALL
   IMPLICIT NONE

   CHARACTER MODNAM*12
   INTEGER :: NUMWAKE, ierr
   DOUBLE PRECISION :: XARG
   DOUBLE PRECISION, SAVE :: hseff, reff
! --- Dimension work arrays for PRIME numerical plume rise
   DOUBLE PRECISION, SAVE :: xtr(mxntr), ytr(mxntr),&
   &ztr(mxntr), rtr(mxntr)

   LOGICAL       :: L_INWAKE
   LOGICAL, SAVE :: L_INWAKE_SAVE, CAPPED, HORIZ

!     Variable Initializations
   MODNAM = 'PRMDELH'
   CAPPED = .FALSE.
   HORIZ  = .FALSE.

!
! --- PRIME ---------------------------------------------------
   IF (WAKE) THEN
! ---    Calculate final rise & array of transitional rise values
! ---    for first receptor only
      if (PRM_FSTREC) then
         PRM_FSTREC = .FALSE.
         L_INWAKE = .FALSE.
         DFSN2CALL = .FALSE.
         hseff=hs
! ---       Compute stack radius from diameter
         reff=0.5D0*ds
         if (srctyp(isrc) .eq. 'POINTCAP') then
            capped = .TRUE.
         else
            capped = .FALSE.
         end if
         if (srctyp(isrc) .eq. 'POINTHOR') then
            horiz = .TRUE.
         else
            horiz = .FALSE.
         end if
! ---       Calculate transitional & final plume rise       ---   CALL NUMRISE
         call NUMRISE(PRIMEDBG,hseff,reff,ts,vs,mxntr,capped,horiz,&
         &dsfact,xtr,ytr,ztr,rtr,L_INWAKE,numwake,ierr,&
         &PRMDBUNT)
         IF (ierr .eq. 1) then
! ---          Error occurred during PRIME numerical plume rise.
!              Write fatal error message - source parameters may be suspect.
            CALL ERRHDL(PATH,MODNAM,'E','499',SRCID(ISRC))
            RUNERR = .TRUE.
            RETURN
         END IF
         IF (NUMWAKE .LE. 1) THEN
            L_INWAKE = .FALSE.
         END IF
! ---       ZTR is effective plume ht. - compute final rise
         DHF = ztr(mxntr) - hseff
! ---       Report selected data to file for debug          ---   CALL WAKE_DBG
         if(PRIMEDBG) call WAKE_DBG(PRMDBUNT,mxntr,xtr,ytr,ztr,rtr,&
         &nobid,hseff)
         L_INWAKE_SAVE = L_INWAKE
      else
         L_INWAKE = L_INWAKE_SAVE
      endif
!
! ---    Determine the plume rise for current receptor
      IF (XARG .LT. xtr(mxntr)) THEN
! ---       Interpolate in rise table to get gradual rise   ---   CALL NUMGRAD
         call NUMGRAD(xarg,xtr,ztr,mxntr,zeff)
         dhp = zeff - hseff
      ELSE
         DHP = ztr(mxntr) - hseff
      END IF

   ENDIF

   RETURN
END

FUNCTION HSPRIM(US,VS,HS,DS)
!***********************************************************************
!                 HSPRIM Module of the ISC Model - Version 2
!
!        PURPOSE: Calculates Stack Height Adjusted for Stack
!                 Tip Downwash (HS')
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Arrays of Source Parameters
!                 Wind Speed Adjusted to Stack Height
!
!        OUTPUTS: Adjusted Stack Height (m)
!
!        CALLED FROM:   PHEFF
!***********************************************************************

!     Variable Declarations
   IMPLICIT NONE
   DOUBLE PRECISION :: US, VS, HS, DS, HSPRIM
   CHARACTER MODNAM*6
!     Variable Initializations
   MODNAM = 'HSPRIM'

!     Calculate Adjusted Stack Height (Eqn. 1-7)

   IF (VS .LT. 1.5D0*US) THEN
      HSPRIM = HS - 2.0D0*DS*(1.5D0-VS/US)
   ELSE
      HSPRIM = HS
   END IF

   IF (HSPRIM .LT. 0.0D0)  HSPRIM = 0.0D0

   RETURN
END

SUBROUTINE SBLRIS ( XARG )
!***********************************************************************
!             SBLRIS Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE:  To calculate plume rise for the stable boundary layer
!             or releases above the convective boundary layer
!
!   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
!
!   DATE:    September 30, 1993
!
!   INPUTS:  Brunt-Vaisala frequency, S
!            Buoyancy flux, FB
!            Momentum flux, FM
!            Downwind distance, XARG
!            Wind speed at stack top, UP
!
!   OUTPUTS: Plume Rise, DHP (m)
!
!   CALLED FROM:   PCALC
!
!   Assumptions:  Wind speed is nonzero at stack top
!
!   References:   "Dispersion in the Stable Boundary Layer",
!                 A. Venkatram, 2/12/93
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XARG, TERMA, TERMB, TERMC, TERMD, TERME
   DOUBLE PRECISION :: XLN, DELHNN


!     Variable Initializations
   MODNAM = 'SBLRIS'

!---- Compute the stable plume rise; FB and BVF and UP have all been
!     checked previously to insure all are greater than 0.0

   TERMA =  FB / (BVF * BVF * UP)
   TERMB =  BVPRIM * FM / FB
   TERMC =  DSIN(BVPRIM * XARG / UP)
   TERMD =  DCOS(BVPRIM * XARG / UP)

! --- Calculate TERME to check for possible negative argument for DHP
   TERME = (TERMB*TERMC+1.0D0-TERMD)

   IF( TERME .GT. 0.0D0 )THEN
      DHP = 2.66D0 * (TERMA *(TERMB*TERMC+1.0D0-TERMD))**THIRD
   ELSE
      DHP = 2.66D0 * (TERMA*TERMB*TERMC)**THIRD
   ENDIF


! --- Equation 95 of MFD for distant-dependent stable plume rise
!     DHP = 2.66D0 * TERMA**THIRD * (TERMB*TERMC+1.0D0-TERMD)**THIRD

!      DHP = 2.66 * (FB / (BVF * BVF * UP) ) ** 0.333333 *
!     &              ( (BVPRIM * FM / FB) * SIN(BVPRIM * XARG / UP) +
!     &                1.0 - COS(BVPRIM * XARG / UP) ) ** 0.333333

! --- Apply lower limit on stable plume rise based on Equation 98
!     of the MFD
   XLN = FB/(UP*USTAR*USTAR)
   DELHNN = 1.2D0*XLN**0.6D0 * (HSP + 1.2D0*XLN)**0.4D0

   DHP = MIN( DHP, DHFAER, DELHNN )

   RETURN
END

SUBROUTINE CBLPRD ( XARG )
!***********************************************************************
!             CBLPRD Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the direct plume in the
!            convective boundary layer
!
!   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
!
!   DATE:    September 30, 1993
!
!   INPUTS:  Downwind distance (xarg)
!
!   OUTPUTS: Plume Rise, DHP1 (m)
!
!   CALLED FROM:   DELTAH
!
!   Assumptions:  Wind speed is nonzero at stack top
!                 BETA1 = 0.6D0 (assigned in MODULE MAIN1)
!
!   References:   "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XARG

!     Variable Initializations
   MODNAM = 'CBLPRD'

! --- Original code based on Eq. 91 of MFD.
   DHP1 = (3.0D0 * FM * XARG / (BETA1*BETA1 * UP*UP) +&
   &3.0D0 * FB * XARG*XARG /&
   &(2.0D0 * BETA1*BETA1 * UP*UP*UP) )**THIRD

   RETURN
END

SUBROUTINE CBLPRN ( XARG )
!***********************************************************************
!             CBLPRN Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the indirect plume in the
!            convective boundary layer
!
!   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
!
!   DATE:    September 30, 1993
!
!   CHANGES:  Equation for DHP2 revised per memorandum from Jeff Weil
!             to AERMIC dated June 20, 1994.
!
!             Added XARG as formal argument for new formulation.
!             Roger Brode, PES, Inc. - 12/5/94
!
!   INPUTS:
!
!   OUTPUTS: Plume Rise, DHP2 (m)
!
!   CALLED FROM:   DELTAH
!
!   Assumptions:
!
!   References:   "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12
   DOUBLE PRECISION :: XARG, RSUBH, RYRZ, DELHI

!     Variable Initializations
   MODNAM = 'CBLPRN'

   RSUBH = BETA2 * (ZI - HSP)
   RYRZ  = RSUBH * RSUBH + 0.25D0*ASUBE*(LAMDAY**1.5D0) *&
   &WSTAR*WSTAR*XARG*XARG/(UP*UP)
   DELHI = DSQRT( (2.D0*FB*ZI)/(ALPHAR*UP*RYRZ) ) * (XARG/UP)
   DHP2  = DELHI

   RETURN
END

SUBROUTINE CBLPR3
!***********************************************************************
!             CBLPR3 Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the penetrated plume in the
!            convective boundary layer
!
!   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
!
!   DATE:    September 30, 1993
!
!   INPUTS:  The ratio of delta(Hsub_e) to delta(Hsub_h), HEDHH
!            Mixing height, ZI
!            Source release height, HS
!
!   OUTPUTS: Plume Rise, DHP3 (m)
!
!   CALLED FROM:   DELTAH
!
!   Assumptions:
!
!   References:   "A Dispersion Model for the Convective Boundary
!                  Layer", J. Weil, 8/17/93
!                 "Plume Penetration of the CBL and Source 3: Source
!                  Strength and Plume Rise", J. Weil, 9/1/93
!
!***********************************************************************

!     Variable Declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

!     Variable Initializations
   MODNAM = 'CBLPR3'

!     The plume rise for the penetrated source is delta(Hsub_3), given
!     by Eq. 9 in Jeff Weil's 9/1/93 document.  The variable HEDHH is
!     delta(Hsub_e)/delta(Hsub_h), calculated from Eq. 26a of Jeff Weil's
!     8/17/93 document, where delta(Hsub_h) is ZI-HS.

   IF (PPF .EQ. 1.0D0) THEN
      DHP3 = HEDHH * (ZI-HSP)
   ELSE
      DHP3 = 0.75D0 * (ZI-HSP) * HEDHH + 0.5D0 * (ZI-HSP)
   END IF

   RETURN
END
