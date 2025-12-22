SUBROUTINE COMPTG ()
!=======================================================================
!                COMPTG module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the vertical potential temperature gradient
!                from a profile of observed temperatures.
!
!   Input:       Array of temperatures in profile
!                Array of heights in the profile
!
!   Output:      Array of potential temperature gradients.
!
!   Assumptions: Subroutine is called even if there is only 1
!                level of observed temperatures in the profile - the
!                logic takes care of this situation
!
!   Called by:   METEXT
!
!   Programmer:  Jim Paumier (PES, Inc.)              30 Sept 1993
!
!   Revision history:
!         12/5/94 - J. Paumier (PES, Inc.)
!                 - changed the tolerance for a gridded profile height
!                   to be within 0.1 m of an observed profile height
!                   rather than 0.5 m
!         07/5/95 - J. Paumier (PES, Inc.)
!                 - the minimum value for the observed potential
!                   temperature gradient in stable atmosphere is
!                   0.002 K/m (value is set in SPTGMN in a parameter
!                   statement in MAIN1.INC)
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER   :: NDXBLW, NDXABV, NLVL

   MODNAM = 'COMPTG'
   PATH   = 'MX'

!---- Definitions
!
!     NDXBLW    Index of an observed profile level below
!     NDXABV    Index of an observed profile level above

!---- Variable initializations

   NDXBLW = 1
   NDXABV = 2
   NTGLVL = 0

!     Loop through the levels, searching for two levels of nonmissing
!     temperature data.  The constant GOVRCP is the conversion from
!     temperature gradient to potential temperature gradient.

   DO WHILE( NDXABV <= NPLVLS )

      IF( PFLTA(NDXBLW) > 0.0D0 )THEN

         IF( PFLTA(NDXABV) > 0.0D0 )THEN

            NTGLVL = NTGLVL + 1
            PFLTG(NTGLVL) = (PFLTA(NDXABV) - PFLTA(NDXBLW)) /&
            &(PFLHT(NDXABV) - PFLHT(NDXBLW)) + GOVRCP
            PFLTGZ(NTGLVL) = (PFLHT(NDXABV) + PFLHT(NDXBLW)) / 2.0D0
            NDXBLW = NDXABV
            NDXABV = NDXABV + 1

         ELSE
            NDXABV = NDXABV + 1

         ENDIF

      ELSE
         NDXABV = NDXABV + 1
         NDXBLW = NDXBLW + 1

      ENDIF

   ENDDO

!     For a stable atmosphere, check the observation and do not let it
!     be less than a minimum value, defined by SPTGMN = 0.002 in MAIN1.INC
   IF( STABLE )THEN
      DO NLVL = 1, NTGLVL
         PFLTG(NLVL) = MAX( SPTGMN, PFLTG(NLVL) )
      END DO
   ENDIF

!     For the structure in GRDPTG to be the same as in the other
!     profiling modules, the number of levels of data must be
!     at least one level of data, whether it is missing or not

   IF( NTGLVL == 0 )THEN
      NTGLVL = 1
   ENDIF

   RETURN
END SUBROUTINE COMPTG


SUBROUTINE TGINIT ()
!=======================================================================
!                TGINIT module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the temperature scaling parameter and
!                gradient at 5 m for the stable atmosphere
!
!   Input:       Friction velocity, Obukhov length, ambient temperature,
!                surface roughness length
!
!   Output:      THETA_STAR and dTHETA/dZ at TREFHT
!
!   Assumptions:
!
!   Called by:   GRDPTG
!
!   Programmer:  Jim Paumier (PES, Inc.)              30 Sept 1993
!
!   Revision history:
!     JOP  March 14, 1995  The initial gradient is now stored in
!                          TG4PFL rather than the first level of
!                          profiled gradients
!
!   References:  Model Coding Abstract for the Met Interface dated
!                August 6, 1992 and all subsequent addenda/corrigenda
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION, PARAMETER :: TGMINHT = 2.0D0, TGMAXHT = 100.0D0
   DOUBLE PRECISION :: THSTAR0, THSTARN, NCLOUD8, USTARMax
   INTEGER :: LVL
   DOUBLE PRECISION :: REFLVL
! Unused:      DOUBLE PRECISION :: USTAR0, THSTR1, USTAR1

   MODNAM = 'TGINIT'
   PATH   = 'MX'

!---- Variable initializations


! --- The temperature gradient computations are made only for a stable atmosphere

   IF( STABLE )THEN

! ---    First check for observed temperature profile to use in
!        calculating THSTAR, and determine the reference level
!        for computing a temperature gradient for profiling

      REFLVL = -99.0D0
      LVL    =  1
      DO WHILE( REFLVL < 0.0D0 .and.  LVL <= NTGLVL )

         IF( PFLTGZ(LVL) > SFCZ0 )THEN
            REFLVL = PFLTGZ(LVL)

         ELSE
            LVL = LVL + 1

         ENDIF

      ENDDO

! ---    Make initial calculation of THSTAR based on observed
!        temperature profile, if available, or with "standard"
!        approach
      IF( REFLVL > 0.0D0 .and. REFLVL <= 100.0D0 )THEN
         THSTAR = PFLTG(LVL) * VONKAR * PFLTGZ(LVL) /&
         &( 1.0D0 + 5.0D0 * PFLTGZ(LVL) / OBULEN )

      ELSE
! ---       Calculate THSTAR based on USTAR, OBULEN, and TA since
!           observed temperature profile is not available
         THSTAR = USTAR**2 / ( G * VONKAR * OBULEN / TA )

      ENDIF

! ---    Make adjustments to THSTAR calculations if ADJ_U* option is used or
!        if no observed temperature profile is available (i.e., REFLVL < 0.0)
!
      IF( L_AdjUstar .and. L_BULKRN .and. REFLVL < 0.0D0 )THEN
! ---       Use Luhar/Raynor approach (2009, BLM v132) for ADJ_U* w/ BULKRN,
!           unless observed temperature profile is available

         USTARMax = 0.6D0 * ( UREFHT/&
         &DLOG((UREFHT-5.*SFCZ0)/SFCZ0) )**0.333333D0

         NCLOUD8 = DBLE(NINT(DBLE(NCLOUD)*0.8D0))

         THSTAR0 = 1.4D0* (0.005D0/(TAN(0.17D0*(2.0D0*NCLOUD8 +&
         &1.0D0))) + 0.05D0)

         THSTARN = THSTAR0*(1.0D0 -&
         &( (2.0D0*USTAR/USTARMAX)-1.0D0 )**2.)

         THSTAR = THSTARN

      ELSEIF( L_AdjUstar .and. .NOT. L_BULKRN&
      &.and. REFLVL < 0.0D0 )THEN
! ---       Use constant THSTAR = 0.08 per Venkatram paper (2011, BLM v138),
!           unless observed temperature profile is available
         THSTAR = 0.08D0

      ELSEIF( REFLVL < 0.0D0 )THEN
! ---       No ADJ_U* option and no observed temperature profile;
! ---       Apply "standard" approach for THSTAR
         THSTAR = USTAR**2 / ( G * VONKAR * OBULEN / TA )

      ENDIF

      CONTINUE

!        Compute DTHETA/dZ at TREFHT

      IF( L_AdjUstar )THEN
         TG4PFL = ( THSTAR / ( VONKAR * TGMINHT ) ) *&
         &( 0.74D0 + 4.7D0 * TGMINHT / OBULEN )
         TG4XTR = ( THSTAR / ( VONKAR * TGMAXHT ) ) *&
         &( 0.74D0 + 4.7D0 * TGMAXHT / OBULEN )

      ELSE
         TG4PFL = ( THSTAR / ( VONKAR * TGMINHT ) ) *&
         &( 1.0D0 + 5.0D0 * TGMINHT / OBULEN )
         TG4XTR = ( THSTAR / ( VONKAR * TGMAXHT ) ) *&
         &( 1.0D0 + 5.0D0 * TGMAXHT / OBULEN )
      ENDIF

! ---    Check for TG4PFL out-of-range; issue warning if lapse rate > 0.5 K/m
      IF( TG4PFL > 0.5D0 )THEN
         WRITE(DUMMY,'("TG4PFL=",F5.3)') TG4PFL
         CALL ERRHDL(PATH,MODNAM,'W','479',DUMMY)
      ENDIF

   ELSE

!        For the unstable case
      THSTAR = -9.0D0
      TG4PFL = XVAL

   ENDIF

   RETURN
END SUBROUTINE TGINIT

SUBROUTINE GRDPTG ()
!=======================================================================
!                GRDPTG module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of the
!                vertical potential temperature gradient
!
!   Input:       Profile array of observed data (PFLTG)
!                Number of levels in the profile (NTGLVL)
!                Gridded heights at which data are required (GRIDHT)
!
!   Output:      Potential temperature gradient at the grid heights
!                (GRIDTG)
!
!   Assumptions: No value of the potential temperature gradient
!                (observed or computed) is less than -50.0
!
!   Called by:   METEXT
!
!   Programmer:  Jim Paumier (PES, Inc.)              30 Sept 1993
!
!   Revision history:
!         12/5/94  - J. Paumier (PES, Inc)
!                  - changed the tolerance for a gridded profile height
!                    to be within 0.1 m of an observed profile height
!                    rather than 0.5 m
!         07/07/95 - J. Paumier (PES,Inc)
!                  - moved the check for a minimum value in stable
!                    layers to outside the initial DO WHILE ... ENDDO
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER     :: PINDEX, GINDEX, NDX
   DOUBLE PRECISION        :: VBELOW, HTBELO
!
!---- Data definitions
!        PINDEX    Array index for the profile of observed values
!        GINDEX    Array index for the profile of gridded values
!        VBELOW    Nonmissing value from the observed data profile
!                  that is below the the gridded level
!
!
!---- Data initializations
!
!
   MODNAM = 'GRDPTG'
   GINDEX = 1
   PINDEX = 1
   VBELOW = -999.0D0
!
!     ------------------------------------------------------------------
!     Loop over each grid level until a value is computed for each level
!     where a value is required OR the number of levels in the gridded
!     profile exceeds the maximum number
!     ------------------------------------------------------------------
!
   DO WHILE( GINDEX <= MXGLVL )
!
!       -------------------------------------------
!       Now begin looping over the observed profile
!       -------------------------------------------

!       The 'blending' of the reference profile with observations now
!       applies only to the stable atmosphere

      IF( STABLE )THEN
!
         DO WHILE( GRIDTG(GINDEX)<-90.0D0 .and. PINDEX<=NTGLVL )
!
            IF( PFLTG(PINDEX) >= -50.0D0 )THEN
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
               IF( DABS(PFLTGZ(PINDEX)-GRIDHT(GINDEX)) <= 0.1D0 )THEN
!                 USE the parameter at this level
                  GRIDTG(GINDEX) = PFLTG(PINDEX)
!
               ELSEIF( GRIDHT(GINDEX)  >  PFLTGZ(PINDEX) )THEN
                  IF( PINDEX < NTGLVL )THEN
!                    SAVE value for possible interpolation
                     VBELOW = PFLTG(PINDEX)
                     HTBELO = PFLTGZ(PINDEX)

                  ELSE   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPTG
                     CALL XTRPTG ( PFLTGZ(PINDEX), PFLTG(PINDEX),&
                     &GRIDHT(GINDEX), GRIDTG(GINDEX) )
                  ENDIF
!
               ELSEIF( GRIDHT(GINDEX)  <  PFLTGZ(PINDEX) )THEN
                  IF( VBELOW >= -50.0D0 )THEN
!                    INTERPOLATE between the two values    --- CALL NTRPTG
                     CALL NTRPTG ( HTBELO, VBELOW, PFLTGZ(PINDEX),&
                     &PFLTG(PINDEX), GRIDHT(GINDEX),&
                     &GRIDTG(GINDEX) )

                  ELSE   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPDN

                     CALL XTRPDN ( GRIDHT(GINDEX), GRIDTG(GINDEX) )

                  ENDIF
!
               ELSE
!                 This section is for DEBUGging - the program should never
!                 reach this point
                  PRINT *, ' ---> ERROR: The search for data to'
                  PRINT *, '             construct the gridded profile'
                  PRINT *, '             failed on ', KURDAT
!
               ENDIF
!
            ELSE
!
!              -------------------------------------------------------
!              The parameter at this level is missing - if this is not
!              the top level, continue the search; if it is the top
!              level, then make a computation.
!              -------------------------------------------------------
!
               IF( PINDEX == NTGLVL )THEN
                  IF( VBELOW  >=  -50.0D0 )THEN
!                    PROFILE up from BELOW                 --- CALL XTRPTG
                     CALL XTRPTG ( PFLTGZ(PINDEX), PFLTG(PINDEX),&
                     &GRIDHT(GINDEX), GRIDTG(GINDEX) )

                  ELSE   ! there are no data
!                    COMPUTE value: full parameterization  --- CALL REFPTG
                     CALL REFPTG ( GRIDHT(GINDEX), GRIDTG(GINDEX) )
                  ENDIF
!
               ELSE   ! this is not the top level, repeat loop
                  CONTINUE
!
               ENDIF
!
            ENDIF   ! parameter (not) missing at this level
!
!           ---------------------------------------------------------
!           Increment the observed profile counter if a value at this
!           grid level was not computed on this pass and continue
!           processing
!           ---------------------------------------------------------
!
            IF( (GRIDTG(GINDEX) < -50.0D0)  .and.&
            &(PINDEX < NTGLVL) )THEN
               PINDEX = PINDEX + 1
            ENDIF
!
         END DO   ! Loop over observed data profile

      ELSEIF( UNSTAB )THEN

         CALL REFPTG( GRIDHT(GINDEX), GRIDTG(GINDEX) )

      ENDIF
!        ------------------------------------------------------------
!        Increment the gridded profile counter and repeat the process
!        starting with the observed value from the profile height as
!        defined by PINDEX
!        ------------------------------------------------------------
!
      GINDEX = GINDEX + 1
!
   END DO   ! Loop over gridded data profile
!
!
!        ------------------------------------------------------------
!        Apply lower limit of SPTGMN (=0.002 K/m in MAIN1.INC) to
!        lapse rate for stable layers.
!        ------------------------------------------------------------
!
   DO NDX = 1,MXGLVL
      IF( STABLE .or. (UNSTAB .and. GRIDHT(NDX)>ZI) )THEN
         GRIDTG(NDX) = MAX( SPTGMN, GRIDTG(NDX) )
      ENDIF
   END DO

   RETURN
END SUBROUTINE GRDPTG


SUBROUTINE REFPTG ( HTINP, VALUE )
!=======================================================================
!                REFPTG Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of the potential temperature
!                gradient from the surface friction velocity, Monin-
!                Obukhov length and roughness length in the stable
!                boundary layer; in the unstable boundary layer, the
!                reference gradient is constant in the various layers.
!
!   Input:       Stability, mixing height, potential temperature
!                gradient between near the surface (TG4PFL),
!                the height of the computation for TG4PFL (TREFHT)
!
!   Output:      Potential temperature gradient (VALUE) at the
!                required height (HEIGHT)
!
!   Assumptions: All parameters required to make the computation are
!                not missing.
!
!   Programmer:  Jim Paumier (PES, Inc.)             30 September 1993
!
!   Revision history:
!                Roger Brode, PES                     22 January 1998
!                Modified to use Stull for entire profile above TREFHT.
!
!                Roger Brode, PES                     19 November 1996
!                Modified to incorporate guidance from AERMIC for
!                second round of Beta testing.
!
!                Jim Paumier, PES                     7 July 1995
!                In keeping with the modification made by Russ Lee (EPA)
!                during testing, HEIGHT in the aguement list is replaced
!                by HTINP and the statement HEIGHT=HTINP is added.  This
!                modification prevents some unintended changes from
!                happening to GRIDHT(1).
!
!                Profiling below 10 m or the lowest level of observed
!                gradients uses Businger's surface layer similarity.
!
!
!   Reference(s): Model Coding Abstract for the Met Interface, dated
!                 August 6, 1992 and and subsequent addenda
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION, PARAMETER :: HDELTH = 100.0D0
   DOUBLE PRECISION               :: HTINP, HEIGHT, VALUE, EXPARG

   MODNAM = 'REFPTG'

!
!---- Data dictionary
!          VALUE   = potential temperature gradient at requested level
!          XVAL    = potential temperature gradient in the mixing layer;
!                    assigned as 0.0 in MODULE MAIN1
!          EFOLDH  = 0.44 and is defined as a parameter in MODULE MAIN1
!
!.......................................................................
!---- Check the stability and then the location of the height relative
!     to the mixing height.

   HEIGHT = HTINP
   IF( UNSTAB )THEN
!
      IF( HEIGHT  <=  ZI )THEN
! ---       Assign unstable lapse rate of 0.0 for height .LE. ZI;
!           value of XVAL is assigned 0.0 in MODULE MAIN1
         VALUE  = XVAL

      ELSE IF( HEIGHT <= ZI+500.0D0 )THEN
         VALUE  = VPTGZI

      ELSE
         VALUE  = 0.005D0

      ENDIF
!
   ELSE   ! stable atmosphere
!        THETA_STAR and TG4PFL (dTheta/dZ at TREFHT) are computed
!        in TGINIT
!
      IF (HEIGHT <= 2.0D0) THEN

         VALUE = TG4PFL

      ELSE IF (HEIGHT <= 100.0D0) THEN

         IF( L_AdjUstar )THEN
            VALUE = ( THSTAR / ( VONKAR * HEIGHT ) ) *&
            &( 0.74D0 + 4.7D0 * HEIGHT / OBULEN )
         ELSE
            VALUE = ( THSTAR / ( VONKAR * HEIGHT ) ) *&
            &( 1.00D0 + 5.0D0 * HEIGHT / OBULEN )
         ENDIF

      ELSE
!           COMPUTE gradient from gradient at TREFHT
         EXPARG =  -1.0D0*(HEIGHT-100.0D0) / (EFOLDH*MAX(HDELTH,ZI) )
         IF (EXPARG > EXPLIM) THEN
            VALUE = TG4XTR * DEXP( EXPARG )
         ELSE
            VALUE = 0.0D0
         END IF

      ENDIF

!        Apply minimum value of 0.002 to all levels of stable profile.
      VALUE = MAX( VALUE, SPTGMN )

   ENDIF

   RETURN
END SUBROUTINE REFPTG


SUBROUTINE NTRPTG ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
!=======================================================================
!                NTRPTG Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the potential temperature gradient at an
!                intermediate level by interpolating between two
!                observed values.
!
!   Input:       Profile heights and values above and below the height
!                at which the value is required
!
!   Output:      Potential temperature gradient at the required level
!
!   Called by:   REFPTG
!
!   Assumptions:
!
!
!   Programmer:  Jim Paumier (PES, Inc.)            30 September 1993
!
!   Revision history:
!      JOP    March 14, 1995 - modified the check on the ratio for a
!                              positive slope and observed values
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE
   DOUBLE PRECISION :: HTBELO, VBELOW, HTABOV, VABOVE, REQDHT, VALUE,&
   &REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT

!
!---- Data dictionary
!
!      REFABV  = Reference profile value above the height at which a
!                value is required (HTABOV)
!      REFBLW  = Reference profile value bbelow the height at which a
!                value is required (HTBELO)
!      REFREQ  = Reference profile value at the height at which a
!                value is required (REQDHT)
!
!---- Data initializations
!
!
!.......................................................................
!---- The computation requires 3 estimates from the reference/theoretical
!     profile: one height above, one height below and from the level at
!     which the parameter is needed.  The ratio of the differences
!     [EST(requested ht) - EST(ht below)] / [EST(ht above) - EST(ht below)]
!     is applied to the difference between the observed values to obtain
!     interpolated value.
!
!     Compute the reference profile value at the height below the
!     requested height                                     --- CALL REFPTG
   CALL REFPTG ( HTBELO, REFBLW )
!
!     Compute the reference profile value at the height above the
!     requested height                                     --- CALL REFPTG
   CALL REFPTG ( HTABOV, REFABV )
!
!     Compute the reference profile value at the requested height
!                                                          --- CALL REFPTG
   CALL REFPTG ( REQDHT, REFREQ )
!
   IF( DABS(REFABV - REFBLW) > 0.0001D0 )THEN
!
!        Linearly interpolate to REQDHT from observed and reference profiles
      CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
      CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
!        REFREQ is value from REFerence profile at REQuired height
!        REFINT is value from REFerence profile linearly INTerpolated to req ht
!        VALINT is the observed VALue linearly INTerpolated to required height
      RATIO = REFREQ/REFINT
      VALUE = RATIO * VALINT
   ELSE
!        INTERPOLATE between VABOVE and VBELOW
      CALL GINTRP ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
!
   ENDIF

   RETURN
END SUBROUTINE NTRPTG


SUBROUTINE XTRPTG ( PFLZ, PFLVAL, GRDZ, VALUE )
!=======================================================================
!                XTRPTG Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the potential temperature gradient
!                by extrapolating outside (either above or below)
!                the range of observed data (i.e., there is at least
!                one observation of the gradient in the profile).
!
!   Input:       Profile height and value below(above) the height (PFLZ
!                and PFLVAL ) at which the value is required and the
!                height at which the value is required (GRDZ)
!
!   Output:      Potential temperature gradient (VALUE) at the
!                required level (GRDZ)
!
!   Called by:   REFPTG
!
!   Assumptions:
!
!   Programmer:  Jim Paumier (PES, Inc.)            30 September 1993
!
!   Revision history:
!                Modified to "restart" the exponential term when
!                extrapolating above the highest Dtheta/Dz measurement
!                level if that level is greater than 100m.
!                R. Brode, PES, Inc. - 8/9/01
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION, PARAMETER :: HDELTH = 100.0D0
   DOUBLE PRECISION  :: VALOBS, VALGRD, PFLZ, GRDZ, VALUE, PFLVAL, RATIO

   MODNAM = 'XTRPTG'
!
!---- Data dictionary

!     VALOBS  = Reference value at the measurement height (PFLZ)
!     VALGRD  = Reference value at the gridded profile height (GRDZ)

!---- Data initializations
!
!.......................................................................
!---- The computation requires estimates from the reference/theoretical
!     profile at the height of the highest(lowest) observation and at
!     the height the parameter is needed.  The ratio of these two values
!     is applied to the observed parameter at the highest(lowest)
!     observed height.
!
!     Compute the reference profile value at the height of the highest
!     (lowest) observed value                              --- CALL REFPTG

   CALL REFPTG ( PFLZ, VALOBS )

!     Compute the reference profile value at the height where a value
!     is required                                          --- CALL REFPTG

   CALL REFPTG ( GRDZ, VALGRD )

!     The potential temperature gradient is the only profile parameter
!     that can take on a negative value (and in the initial programming
!     of AERMOD, this is in the well-mixed layer for an unstable
!     atmosphere); therefore, if VALOBS is zero, then RATIO = 1.0.

   IF( DABS( VALOBS ) < 0.0001D0 ) THEN
      RATIO = 1.0D0
   ELSE
      RATIO = VALGRD / VALOBS
   ENDIF
!
   IF (PFLZ <= 100.0D0) THEN
      VALUE = RATIO * PFLVAL
   ELSE
!        Highest measured Dtheta/Dz is above 100m.  Apply exponential
!        extrapolation term above PFLZ.
      VALUE = PFLVAL * DEXP( -(GRDZ-PFLZ) /&
      &(EFOLDH * MAX(HDELTH,ZI) ) )
   END IF
!
   RETURN
END SUBROUTINE XTRPTG


SUBROUTINE XTRPDN ( GRDZ, VALUE )
!=======================================================================
!                XTRPDN Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the potential temperature gradient
!                by extrapolating downward from the uppermost height of
!                observed potential temperature gradients
!                (i.e., there is at least one gradient in the profile).
!
!   Input:       The height at which the value is required
!
!   Output:      Potential temperature gradient (VALUE) at the
!                required level (GRDZ)
!
!   Called by:   GRDPTG
!
!   Assumptions:
!
!   Programmer:  Jim Paumier (PES, Inc.)            05 August 1998
!
!   Revision history:
!                <none>
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations

   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   DOUBLE PRECISION  :: GRDZ, VALUE

   MODNAM = 'XTRPDN'
!
!---- Data dictionary

!     GRDZ    = Grid profile height
!     VALUE   = Value at the gridded profile height (GRDZ)

!---- Data initializations
!
!.......................................................................
   IF( UNSTAB )THEN

      IF( GRDZ  <=  ZI )THEN
         VALUE  = XVAL

      ELSE IF( GRDZ <= ZI+500.0D0 )THEN
         VALUE  = VPTGZI

      ELSE
         VALUE  = 0.005D0

      ENDIF

   ELSE   ! stable atmosphere
!        THETA_STAR and TG4PFL (dTheta/dZ at TREFHT) were computed
!        in TGINIT

      IF( GRDZ < 2.0D0 )THEN
         VALUE = TG4PFL

      ELSE
!           Extrapolate gradient using similarity
         IF( L_AdjUstar )THEN
            VALUE  = ( THSTAR / ( VONKAR * GRDZ ) ) *&
            &( 0.74D0  +  4.7D0 * GRDZ / OBULEN )
         ELSE
            VALUE  = ( THSTAR / ( VONKAR * GRDZ ) ) *&
            &( 1.0D0  +  5.0D0 * GRDZ / OBULEN )
         ENDIF

      ENDIF
   ENDIF

   RETURN
END SUBROUTINE XTRPDN


SUBROUTINE GRDPT ()
!=======================================================================
!                GRDPT module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of
!                potential temperature
!
!   Input:       Profile of gridded potential temperature gradients
!                Temperature at the reference height
!                Profile of grid heights
!
!   Output:      Potential temperature profile at the grid heights.
!
!   Assumptions: There is at least one grid level below the reference
!                temperature height (which should be satisfied
!                because the lowest grid level is 0.5 meters)
!
!   Called by:   METEXT
!
!   Programmer:  Jim Paumier                          30 Sept 1993
!                Pacific Environmental Services
!
!   Revision history:
!        12/10/97  - R. Brode, PES, Inc.
!                    Corrected the order of array indices used for profiling
!                    potential temperature above the reference height.
!        12/16/94  - J. Paumier, PES, Inc.
!                  - CALL LOCATE to get the number of levels below the
!                    temperature reference height, replacing the original
!                    method which relied on grid heights being every 10 m
!
!-----------------------------------------------------------------------
!
!---- Variable declarations

   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: L, NBELOW
   DOUBLE PRECISION :: PTREF

   MODNAM = 'GRDPT'

!---- Data definitions
!
!
!---- Data initializations
!
!
!.......................................................................
!

!---- Determine the grid level below the temperature reference
!     height (as defined in the scalar file)               ---- CALL LOCATE

   CALL LOCATE( GRIDHT, 1, MXGLVL, TREFHT, NBELOW )

!---- Compute the potential temperature at the reference level
!     using the reference temperature (TA), the reference
!     temperature height (TREFHT), and the average stack base
!     elevation of all the emission sources (ZBASE)

   PTREF = TA + GOVRCP * (TREFHT + ZBASE)

!---- Compute the potential temperature at the grid level below
!     the temperature reference height

   GRIDPT(NBELOW) = PTREF -&
   &0.5D0 * (GRIDTG(NBELOW+1) + GRIDTG(NBELOW)) *&
   &(TREFHT - GRIDHT(NBELOW))


!---- Compute Potential Temp Values for Grid Levels Below Reference Ht.
   DO L = NBELOW-1, 1, -1

      GRIDPT(L) = GRIDPT(L+1) - 0.5D0 * (GRIDTG(L+1) + GRIDTG(L)) *&
      &(GRIDHT(L+1) - GRIDHT(L))

   END DO


!---- Compute Potential Temp Values for Grid Levels Above Reference Ht.
   DO L = NBELOW+1, MXGLVL

      GRIDPT(L) = GRIDPT(L-1) + 0.5D0 * (GRIDTG(L) + GRIDTG(L-1)) *&
      &(GRIDHT(L) - GRIDHT(L-1))

   END DO

   RETURN
END SUBROUTINE GRDPT


SUBROUTINE GRDDEN
!=======================================================================
!                GRDDEN module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of ambient
!                air density - use in PRIME downwash algorithm.
!
!   Input:       Profile of gridded potential temperature gradients
!                Temperature at the reference height
!                Profile of grid heights
!
!   Output:      Ambient air density profile at the grid heights.
!
!   Called by:   METEXT
!
!   Programmer:  Roger W. Brode                         August 9, 2001
!                Pacific Environmental Services
!
!   Revision history:
!                <none>
!
!-----------------------------------------------------------------------
!
!---- Variable declarations

   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   INTEGER :: I
   DOUBLE PRECISION :: TAMB0, TAMB, TBAR, RAMB0, RGASM

   MODNAM = 'GRDDEN'

!---- Data definitions
!
!
!---- Data initializations
!
!
!.......................................................................
!
! --- Set the surface temperature (deg. K) & air density (kg/m**3)
   tamb0=TA
   ramb0=1.2D0

! --- Set the gas constant (m**2/s**2/deg. K)
   rgasm=287.026D0

!---- Compute Ambient Air Density Values
   DO I = 1, MXGLVL

! ---    Compute ambient air density at height, ZGPTA(i)

      tamb = gridpt(i) - govrcp * (gridht(i) + zbase)
      tbar = 0.5D0 * (tamb + tamb0)
      GRIDRHO(I) = ramb0*(tamb0/tamb)*DEXP(-g*gridht(i)/&
      &(rgasm*tbar))

   END DO

   RETURN
END SUBROUTINE GRDDEN
