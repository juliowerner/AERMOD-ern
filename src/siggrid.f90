SUBROUTINE GRDSV ()
!=======================================================================
!                GRDSV module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of the
!                standard deviation of the horizontal wind speed
!                flucuations.
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
   CHARACTER MODNAM*12

   INTEGER     PINDEX, GINDEX
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
   MODNAM = 'GRDSV '

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
   DO WHILE( GINDEX .LE. MXGLVL )
!
!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      DO WHILE( GRIDSV(GINDEX) .LT. -90.0D0 .AND. PINDEX.LE.NPLVLS )
!
         IF( PFLSV(PINDEX) .GE. 0.0D0 )THEN
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
            IF( DABS( PFLHT(PINDEX)-GRIDHT(GINDEX)) .LE. 0.1D0 )THEN
!                 USE the parameter at this level
               GRIDSV(GINDEX) = PFLSV(PINDEX)
!
            ELSEIF( GRIDHT(GINDEX)  .GT.  PFLHT(PINDEX) )THEN
               IF( PINDEX .LT. NPLVLS )THEN
!                    SAVE value for possible interpolation
                  VBELOW = PFLSV(PINDEX)
                  HTBELO = PFLHT(PINDEX)

               ELSE   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPSV
                  CALL XTRPSV ( PFLHT(PINDEX), PFLSV(PINDEX),&
                  &GRIDHT(GINDEX), GRIDSV(GINDEX) )
               ENDIF
!
            ELSEIF( GRIDHT(GINDEX)  .LT.  PFLHT(PINDEX) )THEN
               IF( VBELOW .GE. 0.0D0 )THEN
!                    INTERPOLATE between the two values    --- CALL NTRPSV
                  CALL NTRPSV ( HTBELO, VBELOW, PFLHT(PINDEX),&
                  &PFLSV(PINDEX), GRIDHT(GINDEX),&
                  &GRIDSV(GINDEX) )

               ELSE   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPSV
                  CALL XTRPSV ( PFLHT(PINDEX), PFLSV(PINDEX),&
                  &GRIDHT(GINDEX), GRIDSV(GINDEX) )

               ENDIF
!
            ELSE
!                 This section is for DEBUGging - the program should never
!                 reach this point
               WRITE(*,*) ' ---> ERROR: The search for data to'
               WRITE(*,*) '             construct the gridded '
               WRITE(*,*) '             profile failed on ', KURDAT
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
            IF( PINDEX .EQ. NPLVLS )THEN
               IF( VBELOW  .GE.  0.0D0 )THEN
!                    PROFILE up from BELOW                 --- CALL XTRPSV
                  CALL XTRPSV ( HTBELO, VBELOW,&
                  &GRIDHT(GINDEX), GRIDSV(GINDEX) )

               ELSE   ! there are no data
!                    COMPUTE value: full parameterization  --- CALL REFSV
                  CALL REFSV ( GRIDHT(GINDEX), GRIDSV(GINDEX) )
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
         IF( (GRIDSV(GINDEX) .LT. 0.0D0)  .AND.&
         &(PINDEX .LT. NPLVLS) )THEN
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
!
   END DO   ! Loop over gridded data profile
!
   RETURN
END

SUBROUTINE REFSV ( HTINP, VALUE )
!=======================================================================
!                REFSV Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of sigma-V, the horizontal
!                (lateral) dispersion parameter from the surface
!                friction velocity and convective scaling velocity
!                alone (i.e., no observations of sigma-V)
!
!   Input:       Mixing height (ZI)
!                Friction velocity (USTAR)
!                Convective scaling velocity (WSTAR)
!                Height at which sigma-V is needed (HEIGHT)
!
!   Output:      sigmaV at the required level (VALUE)
!
!   Assumptions: Mixing height, friction and convective scaling
!                velocities are nonmissing
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
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: HEIGHT, VALUE, SVC, SVM
   DOUBLE PRECISION :: HTINP
!
!---- Data dictionary
!
!---- Data initializations
!
   MODNAM = 'REFSV '

   HEIGHT = HTINP
!
!.......................................................................
!
   IF( UNSTAB )THEN
!        Compute the convective component of sigma-v, SVC
      CALL REFSVC( HEIGHT, SVC )

!        Compute the mechanical component of sigma-v, SVM
      CALL REFSVM( HEIGHT, SVM )

      VALUE = DSQRT( SVC*SVC + SVM*SVM )

   ELSEIF( STABLE )THEN
!        Compute the mechanical component of sigma-v, SVM
      CALL REFSVM( HEIGHT, SVM )

      VALUE = SVM

   ENDIF

!
   RETURN
END

SUBROUTINE REFSVC ( HTINP, VALUE )
!=======================================================================
!                REFSVC Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of sigma-V, the horizontal
!                (lateral) dispersion parameter from the surface
!                friction velocity and convective scaling velocity
!                alone (i.e., no observations of sigma-V)
!
!   Input:       Mixing height (ZI)
!                Friction velocity (USTAR)
!                Convective scaling velocity (WSTAR)
!                Height at which sigma-V is needed (HEIGHT)
!
!   Output:      sigmaV at the required level (VALUE)
!
!   Assumptions: Mixing height, friction and convective scaling
!                velocities are nonmissing
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
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: HEIGHT, VALUE, SV2, ZDCRS, ATZI, SV2DCR, VAL2
   DOUBLE PRECISION :: HTINP
!
!---- Data dictionary
!     SV2    = variance of the horizontal component of the wind
!     ZDCRS  = value of 1.2 * ZICONV used in profiling SV2
!     ATZI   = value of SV at ZICONV
!     AT1PT2 = the fraction of and above the mixing height
!              through which the parameter is changing
!
!     AT1PT2 is defined in a PARAMETER statement in MODULE MAIN1
!
!---- Data initializations
!
   MODNAM = 'REFSVC'

   HEIGHT = HTINP
   ZDCRS  =  AT1PT2 * ZICONV
!
!.......................................................................
!
   SV2 = 0.35D0 * WSTAR**2
!
   IF( HEIGHT  .LE.  ZICONV )THEN
      VALUE = DSQRT( SV2 )

   ELSEIF( HEIGHT .GT. ZICONV  .AND.  HEIGHT .LE. ZDCRS )THEN
!        COMPUTE sigmaV at 1.2*ZI
      SV2DCR = MIN( SV2, 0.25D0 )
!        INTERPOLATE between value of SV2 at ZI and at 1.2*ZI
      CALL GINTRP ( ZICONV, SV2, ZDCRS, SV2DCR, HEIGHT, VAL2 )
      VALUE = DSQRT( VAL2 )

   ELSE   ! requested height is above 1.2*mixing height
      ATZI  = DSQRT( SV2 )
      VALUE = MIN( ATZI, 0.5D0 )

   ENDIF
!
   RETURN
END

SUBROUTINE REFSVM ( HTINP, VALUE )
!=======================================================================
!                REFSVM Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of sigma-V, the horizontal
!                (lateral) dispersion parameter from the surface
!                friction velocity and convective scaling velocity
!                alone (i.e., no observations of sigma-V)
!
!   Input:       Mixing height (ZI)
!                Friction velocity (USTAR)
!                Convective scaling velocity (WSTAR)
!                Height at which sigma-V is needed (HEIGHT)
!
!   Output:      sigmaV at the required level (VALUE)
!
!   Assumptions: Mixing height, friction and convective scaling
!                velocities are nonmissing
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
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: HEIGHT, VALUE, SV02, SV2ZI, VARV
   DOUBLE PRECISION :: HTINP
!
!---- Data dictionary
!     SV02   = variance of the horizontal component of the wind at the
!              surface.
!
!---- Data initializations
!
   MODNAM = 'REFSVM'

   HEIGHT = HTINP
!
!.......................................................................
!
!     Compute SV2 at the surface
   SV02 = 3.6D0 * USTAR**2
!
!     Compute SV2 at ZI;
!     Do not let SV2 at ZI exceed the surface value.
   SV2ZI = MIN( SV02, 0.25D0 )

   IF( HEIGHT  .LE.  ZIMECH )THEN
!        INTERPOLATE between these two values of the variance if HEIGHT is
!        below ZIMECH
      CALL GINTRP ( 0.0D0, SV02, ZIMECH, SV2ZI, HEIGHT, VARV )
      VALUE = DSQRT(VARV)

   ELSE
!        Persist value at ZIMECH upward.
      VALUE = DSQRT(SV2ZI)

   ENDIF
!
   RETURN
END

SUBROUTINE NTRPSV ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
!=======================================================================
!                NTRPSV Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the deviation of the horizontal wind speed
!                at an intermdiate level by interpolating between two
!                observed values.
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
!                Jim Paumier, PES                    5 July 1995
!                  - using AT1PT2 for 1.2, with the value defined
!                    in MAIN1.INC
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE
   DOUBLE PRECISION ::  HTBELO, VBELOW, HTABOV, VABOVE, REQDHT,&
   &VALUE, REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT
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
!     at the height below the requested height (REFBLW)    --- CALL REFSV
!
   CALL REFSV (  HTBELO, REFBLW )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)    --- CALL REFSV
!
   CALL REFSV (  HTABOV, REFABV )
!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                     --- CALL REFSV
!
   CALL REFSV ( REQDHT, REFREQ )

!
!     Linearly interpolate to REQDHT from observed and reference profiles
   CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
   CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height
   RATIO = REFREQ/REFINT
   VALUE = RATIO * VALINT

   RETURN
END

SUBROUTINE XTRPSV ( PFLZ, PFLVAL, GRDZ, VALUE )
!=======================================================================
!                XTRPSV Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the deviation of the horizontal wind
!                speed by extrapolating outside (either above or
!                below) the range of observed data (i.e., there is
!                at least one observation of sigmaV in the profile).
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
   IMPLICIT NONE
   DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALUE, VALOBS, VALGRD,&
   &RATIO
!
!---- Data dictionary
!
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
!     at the height of the highest(lowest) observed value --- CALL REFSV
!
   CALL REFSV ( PFLZ, VALOBS )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required             --- CALL REFSV
!
   CALL REFSV ( GRDZ, VALGRD )
!
   RATIO = VALGRD / VALOBS
!
   VALUE = RATIO * PFLVAL
!
   RETURN
END



SUBROUTINE GRDSW ()
!=======================================================================
!                GRDSW module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of the
!                deviation of the vertical wind speed
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
   CHARACTER MODNAM*12

   INTEGER     PINDEX, GINDEX
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
   MODNAM = 'GRDSW '
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

   DO WHILE( GINDEX .LE. MXGLVL )

!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      DO WHILE( GRIDSW(GINDEX) .LT. -90.0D0 .AND. PINDEX.LE.NPLVLS )
!
         IF( PFLSW(PINDEX) .GE. 0.0D0 )THEN
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
            IF( DABS( PFLHT(PINDEX) - GRIDHT(GINDEX) ).LE.0.1D0 )THEN
!                 USE the parameter at this level
               GRIDSW(GINDEX) = PFLSW(PINDEX)
!
            ELSEIF( GRIDHT(GINDEX)  .GT.  PFLHT(PINDEX) )THEN
               IF( PINDEX .LT. NPLVLS )THEN
!                    SAVE value for possible interpolation
                  VBELOW = PFLSW(PINDEX)
                  HTBELO = PFLHT(PINDEX)

               ELSE   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPSW
                  CALL XTRPSW ( PFLHT(PINDEX), PFLSW(PINDEX),&
                  &GRIDHT(GINDEX), GRIDSW(GINDEX) )
               ENDIF
!
            ELSEIF( GRIDHT(GINDEX)  .LT.  PFLHT(PINDEX) )THEN
               IF( VBELOW .GE. 0.0D0 )THEN
!                    INTERPOLATE between the two values    --- CALL NTRPSW
                  CALL NTRPSW ( HTBELO, VBELOW, PFLHT(PINDEX),&
                  &PFLSW(PINDEX), GRIDHT(GINDEX),&
                  &GRIDSW(GINDEX) )

               ELSE   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPSW
                  CALL XTRPSW ( PFLHT(PINDEX), PFLSW(PINDEX),&
                  &GRIDHT(GINDEX), GRIDSW(GINDEX) )

               ENDIF
!
            ELSE
!                 This section is for DEBUGging - the program should never
!                 reach this point
               WRITE(*,*) ' ---> ERROR: The search for data to'
               WRITE(*,*) '             construct the gridded '
               WRITE(*,*) '             profile failed on ', KURDAT
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
            IF( PINDEX .EQ. NPLVLS )THEN
               IF( VBELOW  .GE.  0.0D0 )THEN
!                    PROFILE up from BELOW                 --- CALL XTRPSW
                  CALL XTRPSW ( HTBELO, VBELOW,&
                  &GRIDHT(GINDEX), GRIDSW(GINDEX) )

               ELSE   ! there are no data
!                    COMPUTE value: full parameterization  --- CALL REFSW
                  CALL REFSW ( GRIDHT(GINDEX), GRIDSW(GINDEX) )
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
         IF( (GRIDSW(GINDEX) .LT. 0.0D0)  .AND.&
         &(PINDEX .LT. NPLVLS) )THEN
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
!
   END DO   ! Loop over gridded data profile
!


   RETURN
END

SUBROUTINE REFSW ( HTINP, VALUE )
!=======================================================================
!                REFSW Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of sigma-W, the vertical
!                dispersion parameter from the surface friction
!                velocity and convective scaling velocity alone
!                (i.e., no observations of sigma-W)
!
!   Input:
!
!
!
!
!   Output:      Value of the parameter at the required level
!
!   Assumptions: Mixing height, friction and convective scaling
!                velocities are nonmissing
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                Jim Paumier, PES                    5 July 1995
!                  - modified the coefficient on the u* term for CBL;
!                  - made the value 1.2 a parameter in MAIN1;
!                  - modified the form of sigma-w at all height for SBL;
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: HEIGHT, VALUE, SWC, SWM
   DOUBLE PRECISION :: HTINP
!
!---- Data dictionary
!
!     SWC    = convective component of the variance of the vertical velocity
!     SWM    = mechanical component of the variance of the vertical velocity
!
!---- Data initializations
!
   MODNAM = 'REFSW '

   HEIGHT = HTINP
!
!.......................................................................
!
   IF (UNSTAB) THEN
!        Compute the convective component of sigma-w, SWC
      CALL REFSWC( HEIGHT, SWC )

!        Compute the mechanical component of sigma-w, SWM
      CALL REFSWM( HEIGHT, SWM )

!        Apply a lower limit > 0 to convective and mechanical components of sigma-w
      SWC = MAX( SWC, 0.0001D0 )
      SWM = MAX( SWM, 0.0001D0 )

      VALUE = DSQRT( SWC*SWC + SWM*SWM )

!rwb     Output convective and mechanical components of profile.
!rwb         if (METEOR) then
!rwb            if (height .eq. 0.0D0) then
!rwb               write(dbmunt,909) kurdat, ziconv, zimech, zi
!rwb909            format(1x,'DATE= ',i8,2x,'ZIc= ',f8.2,2x,'ZIm= ',f8.2,2x,
!rwb     &                'ZI = ',f8.2,/,
!rwb     &          '   GRIDHT    SIGW_COVN      SIGW_MECH       SIGW_TOT')
!rwb            end if
!rwb            write(dbmunt,919) height, swc, swm, value
!rwb919         format(1x,f8.2,3(3x,g13.5))
!rwb         end if

   ELSE IF (STABLE) THEN

!        Compute the mechanical component of sigma-w, SWM
      CALL REFSWM( HEIGHT, SWM )

!        Apply a lower limit > 0 to convective and mechanical components of sigma-w
      VALUE = MAX( SWM, 0.0001D0 )

   END IF

   RETURN
END

SUBROUTINE REFSWC ( HTINP, VALUE )
!=======================================================================
!                REFSWC Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of sigma-W, the vertical
!                dispersion parameter from the surface friction
!                velocity and convective scaling velocity alone
!                for the convective boundary layer.
!                (i.e., no observations of sigma-W)
!
!   Input:
!
!
!
!
!   Output:      Value of the parameter at the required level
!
!   Assumptions: Mixing height, friction and convective scaling
!                velocities are nonmissing
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                Jim Paumier, PES                    5 July 1995
!                  - modified the coefficient on the u* term for CBL;
!                  - made the value 1.2 a parameter in MAIN1;
!                  - modified the form of sigma-w at all height for SBL;
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: HEIGHT, VALUE, SW2, EXPARG
   DOUBLE PRECISION :: HTINP
!
!---- Data dictionary
!
!     SW2    = variance of the vertical velocity
!
!---- Data initializations
!
   MODNAM = 'REFSWC'

   HEIGHT = HTINP
!
!.......................................................................
!
   IF( HEIGHT  .LE.  0.1D0*ZICONV )THEN
      SW2 = 1.6D0 * ( HEIGHT / ZICONV )**(2.0D0*THIRD) * WSTAR**2
      VALUE  = DSQRT( SW2 )

   ELSEIF( HEIGHT .GT. 0.1D0*ZICONV  .AND.  HEIGHT .LE. ZICONV )THEN
      VALUE = DSQRT( 0.35D0 * WSTAR**2 )

   ELSEIF( HEIGHT .GT. ZICONV )THEN
      EXPARG = -6.0D0*(HEIGHT - ZICONV)/ZICONV
      IF (EXPARG .GT. EXPLIM) THEN
         SW2 = 0.35D0 * WSTAR**2 * DEXP( EXPARG )
         VALUE = DSQRT( SW2 )
      ELSE
         SW2 = 0.0D0
         VALUE = 0.0D0
      END IF

   ENDIF
!

   RETURN
END

SUBROUTINE REFSWM ( HTINP, VALUE )
!=======================================================================
!                REFSWM Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the value of sigma-W, the vertical
!                dispersion parameter from the surface friction
!                velocity and convective scaling velocity alone
!                for the mechanical boundary layer.
!                (i.e., no observations of sigma-W)
!
!   Input:
!
!
!
!
!   Output:      Value of the parameter at the required level
!
!   Assumptions: Mixing height, friction and convective scaling
!                velocities are nonmissing
!
!   Programmer:  Jim Paumier                        30 September 1993
!                Pacific Environmental Services
!
!   Revision history:
!                Jim Paumier, PES                    5 July 1995
!                  - modified the coefficient on the u* term for CBL;
!                  - made the value 1.2 a parameter in MAIN1;
!                  - modified the form of sigma-w at all height for SBL;
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   USE MAIN1
   IMPLICIT NONE
   CHARACTER MODNAM*12

   DOUBLE PRECISION :: HEIGHT, VALUE, SWR, SWBL
   DOUBLE PRECISION :: HTINP
!
!---- Data dictionary
!
!     SWR    = variance of the vertical velocity due to residual turbulence
!     SWBL   = variance of the vertical velocity in the boundary layer
!
!---- Data initializations
!
   MODNAM = 'REFSWM'

   HEIGHT = HTINP
!
!.......................................................................
!
!

   SWR  = SWRMAX * MIN( 1.0D0, HEIGHT/ZI)

   IF (HEIGHT .LT. ZI) THEN
      SWBL = 1.3D0 * USTAR * DSQRT( 1.0D0 - HEIGHT/ZI)
   ELSE
      SWBL = 0.0D0
   END IF

   VALUE = DSQRT( SWR*SWR + SWBL*SWBL )


   RETURN
END

SUBROUTINE NTRPSW ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
!=======================================================================
!                NTRPSW Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the deviation of the vertical wind
!                speed at an intermediate level by interpolating
!                between two observed values.
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
!      JOP    March 14, 1995 - modified the check on the ratio for a
!                              positive slope and observed values
!
!   Reference(s):
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   IMPLICIT NONE

   DOUBLE PRECISION :: REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT,&
   &HTBELO, VBELOW, HTABOV, VABOVE, REQDHT, VALUE
!
!---- Data dictionary
!
!
!---- Data initializations
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
!     at the height below the requested height (REFBLW)         --- CALL REFSW
   CALL REFSW ( HTBELO, REFBLW )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)         --- CALL REFSW
   CALL REFSW ( HTABOV, REFABV )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                          --- CALL REFSW
   CALL REFSW ( REQDHT, REFREQ )
!
!
!     Linearly interpolate to REQDHT from observed and reference profiles
   CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
   CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height
   RATIO = REFREQ/REFINT
   VALUE = RATIO * VALINT

   RETURN
END

SUBROUTINE XTRPSW ( PFLZ, PFLVAL, GRDZ, VALUE )
!=======================================================================
!                XTRPSW Module of the AERMOD Dispersion Model
!
!   Purpose:     To compute the the deviation of the vertical wind
!                speed by extrapolating outside (either above or below)
!                the range of observed data (i.e., there is at least
!                one observation of sigma_W in the profile).
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
   IMPLICIT NONE

   DOUBLE PRECISION :: GRDZ, PFLVAL, PFLZ, RATIO, VALOBS, VALGRD,&
   &VALUE
!
!---- Data dictionary
!
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
!     at the height of the highest(lowest) observed value      --- CALL REFSW
   CALL REFSW ( PFLZ, VALOBS )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required                  --- CALL REFSW
   CALL REFSW ( GRDZ, VALGRD )
!
   RATIO = VALGRD / VALOBS
!
   VALUE = RATIO * PFLVAL
!
   RETURN
END
