subroutine grdws
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
   use main1
   implicit none
   character (len=12) :: modnam

   integer          :: pindex, gindex
   double precision :: vbelow, htbelo
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
   modnam = 'GRDWS       '
   path   = 'MX'
!
   gindex = 1
   pindex = 1
   vbelow = -999.0d0
!
!     ------------------------------------------------------------------
!     Loop over each grid level until a value is computed for each level
!     where a value is required OR the number of levels in the gridded
!     profile exceeds the maximum number
!     ------------------------------------------------------------------

   do while( gindex <= mxglvl )


!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      do while( gridws(gindex)<-90.0d0 .and. pindex<=nplvls )
!
         if( pflws(pindex) >= 0.0d0 )then
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
            if( dabs(pflht(pindex) - gridht(gindex) )<=0.1d0 )then
!                 USE the parameter at this level
               gridws(gindex) = pflws(pindex)
!
            elseif( gridht(gindex)  >  pflht(pindex) )then
               if( pindex < nplvls )then
!                    SAVE value for possible interpolation
                  vbelow = pflws(pindex)
                  htbelo = pflht(pindex)

               else   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPWS
                  call xtrpws ( pflht(pindex), pflws(pindex),&
                  &gridht(gindex), gridws(gindex) )
               endif
!
            elseif( gridht(gindex)  <  pflht(pindex) )then
               if( vbelow >= 0.0d0 )then
!                    INTERPOLATE between the two values    --- CALL NTRPWS
                  call ntrpws ( htbelo, vbelow, pflht(pindex),&
                  &pflws(pindex), gridht(gindex),&
                  &gridws(gindex) )

               else   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPWS
                  call xtrpws ( pflht(pindex), pflws(pindex),&
                  &gridht(gindex), gridws(gindex) )

               endif
!
            else
!                 This section is for DEBUGging - the program should never
!                 reach this point
               print *, ' ---> ERROR: The search for data to'
               print *, '             construct the gridded profile'
               print *, '    of speed failed on ', kurdat
!
            endif
!
         else
!
!              -------------------------------------------------------
!              The parameter at this level is missing - if this is not
!              the top level, continue the search; if it is the top
!              level, then make a computation.
!              -------------------------------------------------------
!
            if( pindex == nplvls )then
               if( vbelow  >=  0.0d0 )then
!                    PROFILE up from BELOW                 --- CALL XTRPWS
                  call xtrpws ( htbelo, vbelow,&
                  &gridht(gindex), gridws(gindex) )

               else   ! there are no data
!                    PROFILE up from BELOW with UREF       --- CALL XTRPWS
                  call xtrpws ( urefht, uref,&
                  &gridht(gindex), gridws(gindex) )
               endif
!
            else   ! this is not the top level, repeat loop
               continue
!
            endif
!
         endif   ! parameter (not) missing at this level
!
!           ---------------------------------------------------------
!           Increment the observed profile counter if a value at this
!           grid level was not computed on this pass; continue
!           processing
!           ---------------------------------------------------------
!
         if( (gridws(gindex) < 0.0d0)  .and.&
         &(pindex < nplvls) )then
            pindex = pindex + 1
         endif
!
      end do   ! Loop over observed data profile
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

      gridws(gindex) = max( umingr, gridws(gindex) )

      gindex = gindex + 1
!
   end do   ! Loop over gridded data profile
!
   return
end subroutine grdws

subroutine refws( prflht, utheor )
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
   use main1
   implicit none
   character :: modnam*12
   double precision :: unstu, stblu, prflht, utheor, vldlb, pivalu,&
   &kvalu, ufact, ufact2

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
   modnam = ' REFWS'
   pivalu = pi
   kvalu  = vonkar
!
!
!     Compute the minimum valid height (VLDLB) for these computations.
!     ----------------------------------------------------------------
!
   vldlb =  7.0d0 * sfcz0
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
   if( (urefht > vldlb)  .and.  (urefht <= zi) )then
!
!        ----------------------------------------------------------
!        The reference height is above the valid lower bound height
!        and below the mixing height
!        ----------------------------------------------------------
!
      if( (prflht > vldlb)  .and.  (prflht <= zi) )then
!
!           ---------------------------------------------------------
!           The required height is above the valid lower bound height
!           and below the mixing height - profile directly to the
!           required height
!           ---------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,prflht,obulen,sfcz0,&
            &pivalu, kvalu )
         else
            utheor = stblu( ustar,prflht,obulen,sfcz0,kvalu )
         endif
!
      elseif( prflht <= vldlb )then
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

         ufact = prflht / vldlb

         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,vldlb,obulen,sfcz0,&
            &pivalu, kvalu ) * ufact
         else
            utheor = stblu( ustar,vldlb,obulen,&
            &sfcz0,kvalu ) * ufact
         endif
!
      elseif( prflht > zi )then
!
!           --------------------------------------------------------
!           The required height is above the mixing height - profile
!           only to the mixing height and persist to the required ht
!           --------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,zi,obulen,sfcz0,&
            &pivalu, kvalu )
         else
            utheor = stblu( ustar,zi,obulen,sfcz0,kvalu )
         endif
!
      endif   ! required ht
!
   elseif( urefht > zi )then
!
!        -----------------------------------------------
!        The reference height is above the mixing height
!        -----------------------------------------------
!

      if( (prflht > vldlb)  .and.  (prflht <= zi) )then
!
!           ---------------------------------------------------------
!           The required height is above the valid lower bound height
!           and below the mixing height.
!           ---------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,prflht,obulen,sfcz0,&
            &pivalu, kvalu )
         else
            utheor = stblu( ustar,prflht,obulen,sfcz0,kvalu )
         endif
!
!
      elseif( prflht < vldlb )then
!
!           ---------------------------------------------------------
!           The required height is below the valid lower bound height
!           ---------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,vldlb,obulen,sfcz0,&
            &pivalu, kvalu )
         else
            utheor = stblu( ustar,vldlb,obulen,&
            &sfcz0, kvalu )
         endif
!
      elseif( prflht > zi )then
!
!           --------------------------------------------------------
!           The required height is above the mixing height - use the
!           value from the reference height
!           --------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = uref
         else
            utheor = uref
         endif
!
      endif   ! required ht

   elseif (urefht <= vldlb) then
!
      ufact2 = urefht/vldlb

      if( (prflht > vldlb)  .and.  (prflht <= zi) )then
!
!           ---------------------------------------------------------
!           The required height is above the valid lower bound height
!           and below the mixing height - profile directly to the
!           required height
!           ---------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,prflht,obulen,sfcz0,&
            &pivalu, kvalu )
         else
            utheor = stblu( ustar,prflht,obulen,sfcz0,kvalu )
         endif
!
      elseif( prflht <= vldlb )then
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

         ufact = prflht / vldlb

         utheor = uref * ufact / ufact2

!
      elseif( prflht > zi )then
!
!           --------------------------------------------------------
!           The required height is above the mixing height - profile
!           only to the mixing height and persist to the required ht
!           --------------------------------------------------------
!
         if( obulen < 0.0d0 )then
            utheor = unstu( ustar,zi,obulen,sfcz0,&
            &pivalu, kvalu )
         else
            utheor = stblu( ustar,zi,obulen,sfcz0,kvalu )
         endif
!
      endif   ! required ht
!
   endif   ! reference ht
!
!
   return
end subroutine refws
!
!
double precision function unstu( ustr, z, oblen, z0, valpi,&
&valk )
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
   implicit none

   double precision :: ustr, z, oblen, z0, x0, xz0, psim,&
   &psimz0, valpi, valk
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

   x0  = ( 1.0d0 - 16.0d0 * z / oblen ) ** 0.25d0

!jop  Compute term for computation of PSI(Z0/L)
   xz0 = ( 1.0d0 - 16.0d0 * z0/ oblen ) ** 0.25d0
!
   psim   =  2.0d0 * dlog( (1.0d0 + x0) / 2.0d0 )    +&
   &dlog( (1.0d0 + x0*x0) / 2.0d0 ) -&
   &2.0d0 * datan( x0 ) + valpi / 2.0d0

!jop  Compute PSI(Z0/L)
   psimz0 =  2.0d0 * dlog( (1.0d0 + xz0) / 2.0d0 )     +&
   &dlog( (1.0d0 + xz0*xz0) / 2.0d0 ) -&
   &2.0d0 * datan( xz0 ) + valpi / 2.0d0
!
   unstu = (ustr/valk) * ( dlog( z / z0 ) - psim + psimz0 )
!
   return
end function unstu
!
!
double precision function stblu( ustr, z, oblen, z0, valk )
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
   implicit none

   double precision :: ustr, z, oblen, z0, psim, valk,&
   &psimz0
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
   psim   = -17.0d0 * ( 1.0d0 - dexp( -0.29d0 * z / oblen ) )

!jop  Compute PSI(Z0/L)
   psimz0 = -17.0d0 * ( 1.0d0 - dexp( -0.29d0 * z0 / oblen ) )

   stblu = (ustr/valk) * ( dlog( z / z0 ) - psim + psimz0 )
!
   return
end function stblu
!
!
subroutine ntrpws ( htbelo,vbelow, htabov,vabove, reqdht,valout )
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
   implicit none

   double precision :: refabv, refblw, ratio, valout, vabove, htabov,&
   &vbelow, reqdht, htbelo, refreq, valint, refint
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
   call refws (  htbelo, refblw )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)    --- CALL REFWS
!
   call refws (  htabov, refabv )
!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                     --- CALL REFWS
!
   call refws ( reqdht, refreq )

!
!     Linearly interpolate to REQDHT from observed and reference profiles
   call gintrp ( htbelo,vbelow, htabov, vabove, reqdht,valint )
   call gintrp ( htbelo,refblw, htabov, refabv, reqdht,refint )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height

   ratio  = refreq/refint
   valout = ratio * valint

   return
end subroutine ntrpws

subroutine xtrpws ( pflz, pflval, grdz, valout )
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
   implicit none

   double precision :: pflz, pflval, grdz, valout, valobs, valgrd,&
   &ratio
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
   call refws ( pflz, valobs )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required             --- CALL REFWS
!
   call refws ( grdz, valgrd )
!
   ratio  = valgrd / valobs
!
   valout = ratio * pflval
!
   return
end subroutine xtrpws


subroutine grdwd
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
   use main1
   implicit none
   character :: modnam*12

   integer          :: pindex, gindex
   double precision :: vbelow, htbelo
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
   modnam = 'GRDWD '
!
   gindex = 1
   pindex = 1
   vbelow = -999.0d0
!
!     ------------------------------------------------------------------
!     Loop over each grid level until a value is computed for each level
!     where a value is required OR the number of levels in the gridded
!     profile exceeds the maximum number
!     NOTE: There are 3 options for turning the wind with height.
!     ------------------------------------------------------------------

   do while( gindex <= mxglvl )

!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      do while( gridwd(gindex)<-90.0d0 .and. pindex<=nplvls )
!
         if( pflwd(pindex) >= 0.0d0 )then
!
!              ----------------------------------------------------------
!              Data at this level are not missing; determine its location
!              relative to the height at which data are required and act
!              accordingly.
!              ----------------------------------------------------------
            if( dabs(pflht(pindex)-gridht(gindex)) <= 0.1d0 )then
!                 USE the parameter at this level

               if (pflwd(pindex) > 360.0d0) then
                  pflwd(pindex) = pflwd(pindex) - 360.0d0
               else if (pflwd(pindex) <= 0.0d0) then
                  pflwd(pindex) = pflwd(pindex) + 360.0d0
               end if

               gridwd(gindex) = pflwd(pindex)
!
            elseif( gridht(gindex)  >  pflht(pindex) )then
               if( pindex < nplvls )then
!                    SAVE value for possible interpolation
                  vbelow = pflwd(pindex)
                  htbelo = pflht(pindex)

               else   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPWD
! JAT 06/22/21 D065 REMOVE PFLHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( PFLHT(PINDEX), PFLWD(PINDEX),
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  call xtrpwd (  pflwd(pindex),gridwd(gindex) )
               endif
!
            elseif( gridht(gindex)  <  pflht(pindex) )then
               if( vbelow >= 0.0d0 )then
!                    INTERPOLATE between the two values    --- CALL NTRPWD
                  call ntrpwd ( htbelo, vbelow, pflht(pindex),&
                  &pflwd(pindex), gridht(gindex),&
                  &gridwd(gindex) )

               else   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPWD
! JAT 06/22/21 D065 REMOVE PFLHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( PFLHT(PINDEX), PFLWD(PINDEX),
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  call xtrpwd ( pflwd(pindex),gridwd(gindex) )
               endif
!
            else
!                 This section is for DEBUGging - the program should never
!                 reach this point
               print *, ' ---> ERROR: The search for data to'
               print *, '             construct the gridded profile'
               print *, '      of WDIR failed on ', kurdat
!
            endif
!
         else
!
!              -------------------------------------------------------
!              The parameter at this level is missing - if this is not
!              the top level, continue the search; if it is the top
!              level, then make a computation.
!              -------------------------------------------------------
!
            if( pindex == nplvls )then
               if( vbelow >= 0.0d0 )then
!                    PROFILE up from BELOW                 --- CALL XTRPWD
! JAT 06/22/21 REMOVE HTBELO AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( HTBELO, VBELOW,
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  call xtrpwd ( vbelow, gridwd(gindex) )

               else   ! there are no data
!                    PROFILE up from BELOW with WDREF      --- CALL XTRPWD
! JAT 06/22/21 REMOVE UREFHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
!                     CALL XTRPWD ( UREFHT, WDREF,
!     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                  call xtrpwd ( wdref, gridwd(gindex) )

               endif
!
            else   ! this is not the top level, repeat loop
               continue
!
            endif
!
         endif   ! parameter (not) missing at this level
!
!           ---------------------------------------------------------
!           Increment the observed profile counter if a value at this
!           grid level was not computed on this pass
!           ---------------------------------------------------------
!
         if( (gridwd(gindex) < 0.0d0)  .and.&
         &(pindex < nplvls) )then
            pindex = pindex + 1
         endif
!
      end do   ! Loop over observed data profile
!
!        ------------------------------------------------------------
!        Increment the gridded profile counter and repeat the process
!        starting with the observed value from the profile height as
!        defined by PINDEX
!        ------------------------------------------------------------
!
      gindex = gindex + 1

   end do   ! Loop over gridded data profile
!
   return
end subroutine grdwd

subroutine refwd1( wdreqd )
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
   use main1
   implicit none
   double precision :: wdreqd

!
!---- Data dictionary
!
!---- Data initializations
!
!
   wdreqd = wdref
!
   return
end subroutine refwd1

subroutine ntrpwd ( htbelo,vbelow, htabov,vabove, reqdht,valout )
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
   use main1
   implicit none

   double precision :: htbelo, vbelow, htabov, vabove, reqdht,valout,&
   &valabv, refabv, refblw, ratio, refreq, valint, refint
!
!---- Data dictionary
!
!
!---- Data initializations
   valabv = vabove
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

   call refwd1 ( refblw )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)   --- CALL REFWD1

   call refwd1 ( refabv )

!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                    --- CALL REFWD1

   call refwd1 ( refreq )

   if( (valabv-vbelow) < -180.0d0) then
      valabv = valabv + 360.0d0
   else if( (valabv-vbelow) > 180.0d0) then
      valabv = valabv - 360.0d0
   end if

!     Linearly interpolate to REQDHT from observed and reference profiles
   call gintrp ( htbelo,vbelow, htabov, valabv, reqdht,valint )
   call gintrp ( htbelo,refblw, htabov, refabv, reqdht,refint )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height
   ratio  = refreq/refint
   valout = ratio * valint

   if (valout > 360.0d0) then
      valout = valout - 360.0d0
   else if (valout <= 0.0d0) then
      valout = valout + 360.0d0
   end if

!
   return
end subroutine ntrpwd

! JAT 06/22/21 D065
! REMOVE PFLZ AND GRDZ AS UNUSED INPUT ARGUMENTS
!      SUBROUTINE XTRPWD ( PFLZ, PFLVAL, GRDZ, VALOUT )
subroutine xtrpwd (  pflval, valout )
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
   use main1
   implicit none
! JAT 06/22/21 D065
! REMOVE PFLZ AND GRDZ AS UNUSED VARIABLES
!      DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALOUT, RATIO, VALOBS,
   double precision :: pflval, valout, ratio, valobs,&
   &valgrd
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

   call refwd1 ( valobs )

!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required          --- CALL REFWD1
!

   call refwd1 ( valgrd )

   ratio  = valgrd / valobs
!
   valout = ratio * pflval

   if (valout > 360.0d0) then
      valout = valout - 360.0d0
   else if (valout <= 0.0d0) then
      valout = valout + 360.0d0
   end if

!
   return
end subroutine xtrpwd
