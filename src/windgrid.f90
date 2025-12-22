SUBROUTINE GRDWS
!=======================================================================
!                GRDWS module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of wind speed
!
!   Input:       Parameter profile array
!                Number of levels in the profile
!                Height at which data are required
!
!   Output:      Array of values at the specified grid heights.
!
!   Assumptions:
!
!   Called by:
!
!   Programmer:  Jim Paumier                          30 Sept 1993
!                Pacific Environmental Services
!
!   Revision history:
!         12/5/94 - J. Paumier (Pacific Environmental Svcs., Inc)
!                 - changed the tolerance for a gridded profile height
!                   to be within 0.1 m of an observed profile height
!                   rather than 0.5 m
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER (LEN=12) :: MODNAM

   INTEGER          :: PINDEX, GINDEX
   DOUBLE PRECISION :: VBELOW, HTBELO
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
   MODNAM = 'GRDWS       '
   PATH   = 'MX'
!
   GINDEX = 1
   PINDEX = 1
   VBELOW = -999.0D0
!
!     ------------------------------------------------------------------
!     Loop over each grid level until a value is computed for each level
!     where a value is required OR the number of levels in the gridded
!     profile exceeds the maximum number
!     ------------------------------------------------------------------

   DO WHILE( GINDEX <= MXGLVL )


!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      DO WHILE( GRIDWS(GINDEX)<-90.0D0 .and. PINDEX<=NPLVLS )
!
         IF( PFLWS(PINDEX) >= 0.0D0 )THEN
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
            IF( DABS(PFLHT(PINDEX) - GRIDHT(GINDEX) )<=0.1D0 )THEN
!                 USE the parameter at this level
               GRIDWS(GINDEX) = PFLWS(PINDEX)
!
            ELSEIF( GRIDHT(GINDEX)  >  PFLHT(PINDEX) )THEN
               IF( PINDEX < NPLVLS )THEN
!                    SAVE value for possible interpolation
                  VBELOW = PFLWS(PINDEX)
                  HTBELO = PFLHT(PINDEX)

               ELSE   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPWS
                  CALL XTRPWS ( PFLHT(PINDEX), PFLWS(PINDEX),&
                  &GRIDHT(GINDEX), GRIDWS(GINDEX) )
               ENDIF
!
            ELSEIF( GRIDHT(GINDEX)  <  PFLHT(PINDEX) )THEN
               IF( VBELOW >= 0.0D0 )THEN
!                    INTERPOLATE between the two values    --- CALL NTRPWS
                  CALL NTRPWS ( HTBELO, VBELOW, PFLHT(PINDEX),&
                  &PFLWS(PINDEX), GRIDHT(GINDEX),&
                  &GRIDWS(GINDEX) )

               ELSE   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPWS
                  CALL XTRPWS ( PFLHT(PINDEX), PFLWS(PINDEX),&
                  &GRIDHT(GINDEX), GRIDWS(GINDEX) )

               ENDIF
!
            ELSE
!                 This section is for DEBUGging - the program should never
!                 reach this point
               PRINT *, ' ---> ERROR: The search for data to'
               PRINT *, '             construct the gridded profile'
               PRINT *, '    of speed failed on ', KURDAT
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
            IF( PINDEX == NPLVLS )THEN
               IF( VBELOW  >=  0.0D0 )THEN
!                    PROFILE up from BELOW                 --- CALL XTRPWS
                  CALL XTRPWS ( HTBELO, VBELOW,&
                  &GRIDHT(GINDEX), GRIDWS(GINDEX) )

               ELSE   ! there are no data
!                    PROFILE up from BELOW with UREF       --- CALL XTRPWS
                  CALL XTRPWS ( UREFHT, UREF,&
                  &GRIDHT(GINDEX), GRIDWS(GINDEX) )
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
!           grid level was not computed on this pass; continue
!           processing
!           ---------------------------------------------------------
!
         IF( (GRIDWS(GINDEX) < 0.0D0)  .and.&
         &(PINDEX < NPLVLS) )THEN
            PINDEX = PINDEX + 1
         ENDIF
!
      END DO   ! Loop over observed data profile
!
!        ------------------------------------------------------------
!        Increment the gridded profile counter and repeat the process
!        starting with the observed value from the profile height as
!        defined by PINDEX
!        ------------------------------------------------------------
!

!        The wind speed at any gridded level cannot be less than
!        UMINGR, a value defined in a PARAMETER  statement in
!        MAIN1.INC, and taken to be 0.01 m/s for now

      GRIDWS(GINDEX) = MAX( UMINGR, GRIDWS(GINDEX) )

      GINDEX = GINDEX + 1
!
   END DO   ! Loop over gridded data profile
!
   RETURN
END SUBROUTINE GRDWS

SUBROUTINE REFWS( PRFLHT, UTHEOR )
!=======================================================================
!                REFWS  Module of the AERMOD Dispersion Model
!
!   Purpose:     To estimate the theoretical wind speed at the
!                specified height from a single observation of wind
!                speed (first nonmissing value above 7*Z0M)
!                and the log-profile function with corrections
!                for stability.
!
!   Input:       Observed wind speed at the reference height (UREF)
!                Observed reference height (UREFHT)
!                Height at which data are needed (PRFLHT)
!                Monin-Obukhov length (OBULEN) as a stability parameter
!                Surface roughness length (SFCZ0)
!
!   Output:      Wind speed at the required level (UTHEOR)
!
!   Assumptions: All the input data exist (i.e., are nonmissing)
!
!   Subprograms:
!                UNSTU  -  algorithm to compute the wind speed at PRFLHT
!                           for an unstable atmosphere (OBULEN < 0.0)
!                STBLU  -  algorithm to compute the wind speed at PRFLHT
!                           for a stable atmosphere
!
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                Jim Paumier, PES                    5 July 1995
!                  - pass the value of pi as an argument for the
!                    unstable case
!
!   Reference(s): Addendum (dated 6/2/93) to the 8/6/92 Interface Model
!                 Coding Abstract
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12
   DOUBLE PRECISION :: UNSTU, STBLU, PRFLHT, UTHEOR, VLDLB, PIVALU,&
   &KVALU, UFACT, UFACT2

!
!
!---- Data dictionary
!
!     UNSTU    Wind speed returned from the theoretical profile for
!              unstable atmosphere
!     STBLU    Wind speed returned from the theoretical profile for
!              stable atmosphere
!     VLDLB    Lower bound height for which the computations are valid
!
!---- Data initializations
!
   MODNAM = ' REFWS'
   PIVALU = PI
   KVALU  = VONKAR
!
!
!     Compute the minimum valid height (VLDLB) for these computations.
!     ----------------------------------------------------------------
!
   VLDLB =  7.0D0 * SFCZ0
!
!     -------------------------------------------------------------
!     Check the location of the reference height and the height at
!     which data are required and profile accordingly.
!
!     The computation is stability dependent: check the stability
!     (the sign of the Monin-Obukhov length: negative for an
!     unstable amosphere, positive for a stable atmosphere).
!     -------------------------------------------------------------
!
   IF( (UREFHT > VLDLB)  .and.  (UREFHT <= ZI) )THEN
!
!        ----------------------------------------------------------
!        The reference height is above the valid lower bound height
!        and below the mixing height
!        ----------------------------------------------------------
!
      IF( (PRFLHT > VLDLB)  .and.  (PRFLHT <= ZI) )THEN
!
!           ---------------------------------------------------------
!           The required height is above the valid lower bound height
!           and below the mixing height - profile directly to the
!           required height
!           ---------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,PRFLHT,OBULEN,SFCZ0,&
            &PIVALU, KVALU )
         ELSE
            UTHEOR = STBLU( USTAR,PRFLHT,OBULEN,SFCZ0,KVALU )
         ENDIF
!
      ELSEIF( PRFLHT <= VLDLB )THEN
!
!           ----------------------------------------------------------
!           The required height is below the displacement height -
!RJP        profile only to the valid lower bound height and persist
!RJP        down to the required height
!           profile only to the valid lower bound height and linearly
!           interpolate down to the required height (assuming about a
!RJP        zero wind speed at Z=0.
!           ----------------------------------------------------------
!
!RJP        Add UFACT

         UFACT = PRFLHT / VLDLB

         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,VLDLB,OBULEN,SFCZ0,&
            &PIVALU, KVALU ) * UFACT
         ELSE
            UTHEOR = STBLU( USTAR,VLDLB,OBULEN,&
            &SFCZ0,KVALU ) * UFACT
         ENDIF
!
      ELSEIF( PRFLHT > ZI )THEN
!
!           --------------------------------------------------------
!           The required height is above the mixing height - profile
!           only to the mixing height and persist to the required ht
!           --------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,ZI,OBULEN,SFCZ0,&
            &PIVALU, KVALU )
         ELSE
            UTHEOR = STBLU( USTAR,ZI,OBULEN,SFCZ0,KVALU )
         ENDIF
!
      ENDIF   ! required ht
!
   ELSEIF( UREFHT > ZI )THEN
!
!        -----------------------------------------------
!        The reference height is above the mixing height
!        -----------------------------------------------
!

      IF( (PRFLHT > VLDLB)  .and.  (PRFLHT <= ZI) )THEN
!
!           ---------------------------------------------------------
!           The required height is above the valid lower bound height
!           and below the mixing height.
!           ---------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,PRFLHT,OBULEN,SFCZ0,&
            &PIVALU, KVALU )
         ELSE
            UTHEOR = STBLU( USTAR,PRFLHT,OBULEN,SFCZ0,KVALU )
         ENDIF
!
!
      ELSEIF( PRFLHT < VLDLB )THEN
!
!           ---------------------------------------------------------
!           The required height is below the valid lower bound height
!           ---------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,VLDLB,OBULEN,SFCZ0,&
            &PIVALU, KVALU )
         ELSE
            UTHEOR = STBLU( USTAR,VLDLB,OBULEN,&
            &SFCZ0, KVALU )
         ENDIF
!
      ELSEIF( PRFLHT > ZI )THEN
!
!           --------------------------------------------------------
!           The required height is above the mixing height - use the
!           value from the reference height
!           --------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UREF
         ELSE
            UTHEOR = UREF
         ENDIF
!
      ENDIF   ! required ht

   ELSEIF (UREFHT <= VLDLB) THEN
!
      UFACT2 = UREFHT/VLDLB

      IF( (PRFLHT > VLDLB)  .and.  (PRFLHT <= ZI) )THEN
!
!           ---------------------------------------------------------
!           The required height is above the valid lower bound height
!           and below the mixing height - profile directly to the
!           required height
!           ---------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,PRFLHT,OBULEN,SFCZ0,&
            &PIVALU, KVALU )
         ELSE
            UTHEOR = STBLU( USTAR,PRFLHT,OBULEN,SFCZ0,KVALU )
         ENDIF
!
      ELSEIF( PRFLHT <= VLDLB )THEN
!
!           ----------------------------------------------------------
!           The required height is below the displacement height -
!RJP        profile only to the valid lower bound height and persist
!RJP        down to the required height
!           profile only to the valid lower bound height and linearly
!           interpolate down to the required height (assuming about a
!RJP        zero wind speed at Z=0.
!           ----------------------------------------------------------
!
!RJP        Add UFACT

         UFACT = PRFLHT / VLDLB

         UTHEOR = UREF * UFACT / UFACT2

!
      ELSEIF( PRFLHT > ZI )THEN
!
!           --------------------------------------------------------
!           The required height is above the mixing height - profile
!           only to the mixing height and persist to the required ht
!           --------------------------------------------------------
!
         IF( OBULEN < 0.0D0 )THEN
            UTHEOR = UNSTU( USTAR,ZI,OBULEN,SFCZ0,&
            &PIVALU, KVALU )
         ELSE
            UTHEOR = STBLU( USTAR,ZI,OBULEN,SFCZ0,KVALU )
         ENDIF
!
      ENDIF   ! required ht
!
   ENDIF   ! reference ht
!
!
   RETURN
END SUBROUTINE REFWS
!
!
DOUBLE PRECISION FUNCTION UNSTU( USTR, Z, OBLEN, Z0, VALPI,&
&VALK )
!=======================================================================
!                UNSTU  Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the wind speed at the specified height
!                for an unstable atmosphere
!
!   Input:       Friction velocity (USTR)
!                Height at which data are needed (Z)
!                Monin-Obukhov length (OBLEN) as a stability parameter
!                Surface roughness length (Z0)
!                Value of pi (3.14159) (VALPI)
!                von Karman constant (VALK = 0.4)
!
!   Output:      Wind speed at the required level (UNSTU)
!
!   Assumptions: All the input data exist (i.e., are nonmissing)
!
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!     12/07/94 -  Added the PSI(Z0/L) term to the theoretical wind speed
!     07/05/95 -  removed inclusion of MAIN1.INC; passing in value of pi
!     09/18/95 -  Corrected PSIM terms to include 2 rather than z in the
!                 denominator of the ALOG term.
!
!   Reference(s):  August 6, 1992 Model Coding Abstract and addenda from
!                  the AERMIC workgroup
!
!-----------------------------------------------------------------------
!
!C  X values in this subroutine changed to X0 to avoid conflict
!C   with the global variable--rfl 5/13/94
!
!---- Variable declarations
!
   IMPLICIT NONE

   DOUBLE PRECISION :: USTR, Z, OBLEN, Z0, X0, XZ0, PSIM,&
   &PSIMZ0, VALPI, VALK
!
!
!---- Data dictionary
!
!     VALPI   = 3.14159...
!     VALK    = 0.4 (von Karman constant)
!     PSIM    Correction for stability at Z
!     PSIMZ0  Correction for stability at Z0
!     X0      Interim calculation
!
!---- Data initializations
!
!.......................................................................
!

   X0  = ( 1.0D0 - 16.0D0 * Z / OBLEN ) ** 0.25D0

!jop  Compute term for computation of PSI(Z0/L)
   XZ0 = ( 1.0D0 - 16.0D0 * Z0/ OBLEN ) ** 0.25D0
!
   PSIM   =  2.0D0 * DLOG( (1.0D0 + X0) / 2.0D0 )    +&
   &DLOG( (1.0D0 + X0*X0) / 2.0D0 ) -&
   &2.0D0 * DATAN( X0 ) + VALPI / 2.0D0

!jop  Compute PSI(Z0/L)
   PSIMZ0 =  2.0D0 * DLOG( (1.0D0 + XZ0) / 2.0D0 )     +&
   &DLOG( (1.0D0 + XZ0*XZ0) / 2.0D0 ) -&
   &2.0D0 * DATAN( XZ0 ) + VALPI / 2.0D0
!
   UNSTU = (USTR/VALK) * ( DLOG( Z / Z0 ) - PSIM + PSIMZ0 )
!
   RETURN
END FUNCTION UNSTU
!
!
DOUBLE PRECISION FUNCTION STBLU( USTR, Z, OBLEN, Z0, VALK )
!=======================================================================
!                STBLU  Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the wind speed at the specified height
!                for the stable atmosphere
!
!   Input:       Friction velocity (USTR)
!                Height at which data are needed (Z)
!                Monin-Obukhov length (OBLEN) as a stability parameter
!                Surface roughness length (Z0)
!                von Karman constant (VALK = 0.4)
!
!   Output:      Wind speed at the required level (STBLU)
!
!   Assumptions: All the input data exist (i.e., are nonmissing)
!
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!     12/7/94 -  Added the PSI(Z0/L) term to the theoretical wind speed
!
!   Reference(s):  August 6, 1992 Model Coding Abstract and addenda from
!                  the AERMIC workgroup
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE

   DOUBLE PRECISION :: USTR, Z, OBLEN, Z0, PSIM, VALK,&
   &PSIMZ0
!
!
!---- Data dictionary
!
!     VALK    = 0.4 (von Karman constant)
!     PSIM    Correction for stability at Z
!     PSIMZ0  Correction for stability at Z0
!
!---- Data initializations
!
!.......................................................................
!
   PSIM   = -17.0D0 * ( 1.0D0 - DEXP( -0.29D0 * Z / OBLEN ) )

!jop  Compute PSI(Z0/L)
   PSIMZ0 = -17.0D0 * ( 1.0D0 - DEXP( -0.29D0 * Z0 / OBLEN ) )

   STBLU = (USTR/VALK) * ( DLOG( Z / Z0 ) - PSIM + PSIMZ0 )
!
   RETURN
END FUNCTION STBLU
!
!
SUBROUTINE NTRPWS ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALOUT )
!=======================================================================
!                NTRPWS Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the wind speed at an intermediate level by
!                interpolating between two observed values.
!
!   Input:       Profile height below the level at which wind speed
!                   is required (HTBELO)
!                Wind speed at HTBELO
!                Profile height above the level at which wind speed
!                   is required (HTABOV)
!                Wind speed at HTABOV
!                Height at which data are required (REQDHT)
!
!   Output:      Wind speed at the required level (VALOUT)
!
!   Called by:
!
!   Assumptions: No missing data
!
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!      JOP    March 14, 1995 - modified the check on the ratio for a
!                              positive slope and observed values
!
!   Reference(s):  Revision dated 6/2/93 to the Interface Model Coding
!                  Abstract
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE

   DOUBLE PRECISION :: REFABV, REFBLW, RATIO, VALOUT, VABOVE, HTABOV,&
   &VBELOW, REQDHT, HTBELO, REFREQ, VALINT, REFINT
!
!---- Data dictionary
!
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
!     COMPUTE the value of the parameter from the reference profile
!     at the height below the requested height (REFBLW)    --- CALL REFWS
!
   CALL REFWS (  HTBELO, REFBLW )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)    --- CALL REFWS
!
   CALL REFWS (  HTABOV, REFABV )
!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                     --- CALL REFWS
!
   CALL REFWS ( REQDHT, REFREQ )

!
!     Linearly interpolate to REQDHT from observed and reference profiles
   CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
   CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height

   RATIO  = REFREQ/REFINT
   VALOUT = RATIO * VALINT

   RETURN
END SUBROUTINE NTRPWS

SUBROUTINE XTRPWS ( PFLZ, PFLVAL, GRDZ, VALOUT )
!=======================================================================
!                XTRPWS Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the wind speed by extrapolating outside
!                (either above or below) the range of observed data
!                (i.e., there is at least one observation of wind
!                speed in the profile).
!
!   Input:       Profile height from which data are extrapolated (PFLZ)
!                Value at the height PFLZ (PFLVAL)
!                Gridded height at which wind speed is required (GRDZ)
!
!   Output:      Wind speed at the required level (VALOUT)
!
!   Called by:
!
!   Assumptions: No missing data
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                <none>
!
!   Reference(s):  Revision dated 6/2/93 to the Interface Model Coding
!                  Abstract
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE

   DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALOUT, VALOBS, VALGRD,&
   &RATIO
!
!---- Data dictionary
!     VALOBS   Value returned from the reference profile at PFLZ
!     VALGRD   Value returned from the reference profile at GRDZ
!
!---- Data initializations
!
!
!.......................................................................
!---- The computation requires estimates from the reference/theoretical
!     profile at the height of the highest(lowest) observation and at
!     the height the parameter is needed.  The ratio of these two values
!     is applied to the observed parameter at the highest(lowest)
!     observed height.
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height of the highest(lowest) observed value --- CALL REFWS
!
   CALL REFWS ( PFLZ, VALOBS )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required             --- CALL REFWS
!
   CALL REFWS ( GRDZ, VALGRD )
!
   RATIO  = VALGRD / VALOBS
!
   VALOUT = RATIO * PFLVAL
!
   RETURN
END SUBROUTINE XTRPWS


SUBROUTINE GRDWD
!=======================================================================
!                GRDWD module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of wind
!                direction.
!
!   Input:       Parameter profile array
!                Number of levels in the profile
!                Height at which data are required
!
!   Output:      Array of values at the specified grid heights.
!
!   Assumptions:
!
!   Called by:
!
!   Programmer:  Jim Paumier                          30 Sept 1993
!                Pacific Environmental Services
!
!   Revision history:
!         12/5/94 - J. Paumier (Pacific Environmental Svcs., Inc)
!                 - changed the tolerance for a gridded profile height
!                   to be within 0.1 m of an observed profile height
!                   rather than 0.5 m
!
!                <none>
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER :: MODNAM*12

   INTEGER          :: PINDEX, GINDEX
   DOUBLE PRECISION :: VBELOW, HTBELO
!
!---- Data definitions
!        PINDEX    Array index for the profile of observed values
!        GINDEX    Array index for the profile of gridded values
!        VBELOW    Value from the observed data profile that is
!                  below the the gridded level
!
!
!---- Data initializations
!
   MODNAM = 'GRDWD '
!
   GINDEX = 1
   PINDEX = 1
   VBELOW = -999.0D0
!
!     ------------------------------------------------------------------
!     Loop over each grid level until a value is computed for each level
!     where a value is required OR the number of levels in the gridded
!     profile exceeds the maximum number
!     NOTE: There are 3 options for turning the wind with height.
!     ------------------------------------------------------------------

   DO WHILE( GINDEX <= MXGLVL )

!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      DO WHILE( GRIDWD(GINDEX)<-90.0D0 .and. PINDEX<=NPLVLS )
!
         IF( PFLWD(PINDEX) >= 0.0D0 )THEN
!
!              ----------------------------------------------------------
!              Data at this level are not missing; determine its location
!              relative to the height at which data are required and act
!              accordingly.
!              ----------------------------------------------------------
            IF( DABS(PFLHT(PINDEX)-GRIDHT(GINDEX)) <= 0.1D0 )THEN
!                 USE the parameter at this level

               IF (PFLWD(PINDEX) > 360.0D0) THEN
                  PFLWD(PINDEX) = PFLWD(PINDEX) - 360.0D0
               ELSE IF (PFLWD(PINDEX) <= 0.0D0) THEN
                  PFLWD(PINDEX) = PFLWD(PINDEX) + 360.0D0
               END IF

               GRIDWD(GINDEX) = PFLWD(PINDEX)
!
            ELSEIF( GRIDHT(GINDEX)  >  PFLHT(PINDEX) )THEN
               IF( PINDEX < NPLVLS )THEN
!                    SAVE value for possible interpolation
                  VBELOW = PFLWD(PINDEX)
                  HTBELO = PFLHT(PINDEX)

               ELSE   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPWD
! JAT 06/22/21 D065 REMOVE PFLHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( PFLHT(PINDEX), PFLWD(PINDEX),
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  CALL XTRPWD (  PFLWD(PINDEX),GRIDWD(GINDEX) )
               ENDIF
!
            ELSEIF( GRIDHT(GINDEX)  <  PFLHT(PINDEX) )THEN
               IF( VBELOW >= 0.0D0 )THEN
!                    INTERPOLATE between the two values    --- CALL NTRPWD
                  CALL NTRPWD ( HTBELO, VBELOW, PFLHT(PINDEX),&
                  &PFLWD(PINDEX), GRIDHT(GINDEX),&
                  &GRIDWD(GINDEX) )

               ELSE   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPWD
! JAT 06/22/21 D065 REMOVE PFLHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( PFLHT(PINDEX), PFLWD(PINDEX),
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  CALL XTRPWD ( PFLWD(PINDEX),GRIDWD(GINDEX) )
               ENDIF
!
            ELSE
!                 This section is for DEBUGging - the program should never
!                 reach this point
               PRINT *, ' ---> ERROR: The search for data to'
               PRINT *, '             construct the gridded profile'
               PRINT *, '      of WDIR failed on ', KURDAT
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
            IF( PINDEX == NPLVLS )THEN
               IF( VBELOW >= 0.0D0 )THEN
!                    PROFILE up from BELOW                 --- CALL XTRPWD
! JAT 06/22/21 REMOVE HTBELO AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( HTBELO, VBELOW,
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  CALL XTRPWD ( VBELOW, GRIDWD(GINDEX) )

               ELSE   ! there are no data
!                    PROFILE up from BELOW with WDREF      --- CALL XTRPWD
! JAT 06/22/21 REMOVE UREFHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( UREFHT, WDREF,
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  CALL XTRPWD ( WDREF, GRIDWD(GINDEX) )

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
!           grid level was not computed on this pass
!           ---------------------------------------------------------
!
         IF( (GRIDWD(GINDEX) < 0.0D0)  .and.&
         &(PINDEX < NPLVLS) )THEN
            PINDEX = PINDEX + 1
         ENDIF
!
      END DO   ! Loop over observed data profile
!
!        ------------------------------------------------------------
!        Increment the gridded profile counter and repeat the process
!        starting with the observed value from the profile height as
!        defined by PINDEX
!        ------------------------------------------------------------
!
      GINDEX = GINDEX + 1

   END DO   ! Loop over gridded data profile
!
   RETURN
END SUBROUTINE GRDWD

SUBROUTINE REFWD1( WDREQD )
!=======================================================================
!                REFWD1 Module of the AERMOD Dispersion Model
!
!   Purpose:     To estimate the 'theoretical' wind direction at the
!                specified height from a single observation of the
!                direction (first level of nonmissing winds above
!                7*Z0).
!
!   Input:       Wind direction at reference height (WDREF (in COMMON))
!
!   Output:      Wind direction at the required height (WDREQD)
!
!   Assumptions: The wind does not turn with height (as selected by
!                the user), so the theoretical wind direction is
!                simply the value at the reference height.
!                The calling programs take care of properly
!                interpreting the results.
!
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                <none>
!
!   Reference(s): Addendum (dated 6/2/93) to the 8/6/92 Interface Model
!                 Coding Abstract
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   DOUBLE PRECISION :: WDREQD

!
!---- Data dictionary
!
!---- Data initializations
!
!
   WDREQD = WDREF
!
   RETURN
END SUBROUTINE REFWD1

SUBROUTINE NTRPWD ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALOUT )
!=======================================================================
!                NTRPWD Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the wind direction at an intermediate level
!                by interpolating between two observed values.
!
!   Input:
!
!
!
!
!   Output:      Value of the parameter at the required level
!
!   Called by:
!
!   Assumptions:
!
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!      RWB    August 26, 1996 - modified to use local variable for
!                               VALABV (value above) rather than
!                               VABOVE from argument list, since
!                               the value may be modified by the
!                               subroutine.
!
!      JOP    March 14, 1995  - modified the check on the ratio for a
!                               positive slope and observed values
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE

   DOUBLE PRECISION :: HTBELO, VBELOW, HTABOV, VABOVE, REQDHT,VALOUT,&
   &VALABV, REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT
!
!---- Data dictionary
!
!
!---- Data initializations
   VALABV = VABOVE
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
!     COMPUTE the value of the parameter from the reference profile
!     at the height below the requested height (REFBLW)   --- CALL REFWD1

   CALL REFWD1 ( REFBLW )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)   --- CALL REFWD1

   CALL REFWD1 ( REFABV )

!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                    --- CALL REFWD1

   CALL REFWD1 ( REFREQ )

   IF( (VALABV-VBELOW) < -180.0D0) THEN
      VALABV = VALABV + 360.0D0
   ELSE IF( (VALABV-VBELOW) > 180.0D0) THEN
      VALABV = VALABV - 360.0D0
   END IF

!     Linearly interpolate to REQDHT from observed and reference profiles
   CALL GINTRP ( HTBELO,VBELOW, HTABOV, VALABV, REQDHT,VALINT )
   CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height
   RATIO  = REFREQ/REFINT
   VALOUT = RATIO * VALINT

   IF (VALOUT > 360.0D0) THEN
      VALOUT = VALOUT - 360.0D0
   ELSE IF (VALOUT <= 0.0D0) THEN
      VALOUT = VALOUT + 360.0D0
   END IF

!
   RETURN
END SUBROUTINE NTRPWD

! JAT 06/22/21 D065
! REMOVE PFLZ AND GRDZ AS UNUSED INPUT ARGUMENTS
!      SUBROUTINE XTRPWD ( PFLZ, PFLVAL, GRDZ, VALOUT )
SUBROUTINE XTRPWD (  PFLVAL, VALOUT )
!=======================================================================
!                XTRPWD Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the wind direction at the required height
!                by extrapolating outside (either above or below)
!                the range of observed data (i.e., there is at least
!                one observation of the wind direction in the profile).
!
!   Input:
!
!
!
!
!   Output:      Value of the parameter at the required level
!
!   Called by:
!
!   Assumptions:
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                <none>
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
! JAT 06/22/21 D065
! REMOVE PFLZ AND GRDZ AS UNUSED VARIABLES
!      DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALOUT, RATIO, VALOBS,
   DOUBLE PRECISION :: PFLVAL, VALOUT, RATIO, VALOBS,&
   &VALGRD
!
!---- Data dictionary
!
!     Note that PFLZ and GRDZ are no longer used since reference WD
!     profile is constant with height.
!
!---- Data initializations
!
!
!.......................................................................
!---- The computation requires estimates from the reference/theoretical
!     profile at the height of the highest(lowest) observation and at
!     the height the parameter is needed.  The ratio of these two values
!     is applied to the observed parameter at the highest(lowest)
!     observed height.
!
!     COMPUTE the value of the parameter from the reference profile
!     at height of the highest(lowest) observed value  --- CALL REFWD1

   CALL REFWD1 ( VALOBS )

!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required          --- CALL REFWD1
!

   CALL REFWD1 ( VALGRD )

   RATIO  = VALGRD / VALOBS
!
   VALOUT = RATIO * PFLVAL

   IF (VALOUT > 360.0D0) THEN
      VALOUT = VALOUT - 360.0D0
   ELSE IF (VALOUT <= 0.0D0) THEN
      VALOUT = VALOUT + 360.0D0
   END IF

!
   RETURN
END SUBROUTINE XTRPWD
