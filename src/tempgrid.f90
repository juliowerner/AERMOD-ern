subroutine comptg ()
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
   use main1
   implicit none
   character :: modnam*12

   integer   :: ndxblw, ndxabv, nlvl

   modnam = 'COMPTG'
   path   = 'MX'

!---- Definitions
!
!     NDXBLW    Index of an observed profile level below
!     NDXABV    Index of an observed profile level above

!---- Variable initializations

   ndxblw = 1
   ndxabv = 2
   ntglvl = 0

!     Loop through the levels, searching for two levels of nonmissing
!     temperature data.  The constant GOVRCP is the conversion from
!     temperature gradient to potential temperature gradient.

   do while( ndxabv <= nplvls )

      if( pflta(ndxblw) > 0.0d0 )then

         if( pflta(ndxabv) > 0.0d0 )then

            ntglvl = ntglvl + 1
            pfltg(ntglvl) = (pflta(ndxabv) - pflta(ndxblw)) /&
            &(pflht(ndxabv) - pflht(ndxblw)) + govrcp
            pfltgz(ntglvl) = (pflht(ndxabv) + pflht(ndxblw)) / 2.0d0
            ndxblw = ndxabv
            ndxabv = ndxabv + 1

         else
            ndxabv = ndxabv + 1

         endif

      else
         ndxabv = ndxabv + 1
         ndxblw = ndxblw + 1

      endif

   enddo

!     For a stable atmosphere, check the observation and do not let it
!     be less than a minimum value, defined by SPTGMN = 0.002 in MAIN1.INC
   if( stable )then
      do nlvl = 1, ntglvl
         pfltg(nlvl) = max( sptgmn, pfltg(nlvl) )
      end do
   endif

!     For the structure in GRDPTG to be the same as in the other
!     profiling modules, the number of levels of data must be
!     at least one level of data, whether it is missing or not

   if( ntglvl == 0 )then
      ntglvl = 1
   endif

   return
end subroutine comptg


subroutine tginit ()
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
   use main1
   implicit none
   character :: modnam*12

   double precision, parameter :: tgminht = 2.0d0, tgmaxht = 100.0d0
   double precision :: thstar0, thstarn, ncloud8, USTARMax
   integer :: lvl
   double precision :: reflvl
! Unused:      DOUBLE PRECISION :: USTAR0, THSTR1, USTAR1

   modnam = 'TGINIT'
   path   = 'MX'

!---- Variable initializations


! --- The temperature gradient computations are made only for a stable atmosphere

   if( stable )then

! ---    First check for observed temperature profile to use in
!        calculating THSTAR, and determine the reference level
!        for computing a temperature gradient for profiling

      reflvl = -99.0d0
      lvl    =  1
      do while( reflvl < 0.0d0 .and.  lvl <= ntglvl )

         if( pfltgz(lvl) > sfcz0 )then
            reflvl = pfltgz(lvl)

         else
            lvl = lvl + 1

         endif

      enddo

! ---    Make initial calculation of THSTAR based on observed
!        temperature profile, if available, or with "standard"
!        approach
      if( reflvl > 0.0d0 .and. reflvl <= 100.0d0 )then
         thstar = pfltg(lvl) * vonkar * pfltgz(lvl) /&
         &( 1.0d0 + 5.0d0 * pfltgz(lvl) / obulen )

      else
! ---       Calculate THSTAR based on USTAR, OBULEN, and TA since
!           observed temperature profile is not available
         thstar = ustar**2 / ( g * vonkar * obulen / ta )

      endif

! ---    Make adjustments to THSTAR calculations if ADJ_U* option is used or
!        if no observed temperature profile is available (i.e., REFLVL < 0.0)
!
      if( L_AdjUstar .and. l_bulkrn .and. reflvl < 0.0d0 )then
! ---       Use Luhar/Raynor approach (2009, BLM v132) for ADJ_U* w/ BULKRN,
!           unless observed temperature profile is available

         USTARMax = 0.6d0 * ( urefht/&
         &dlog((urefht-5.*sfcz0)/sfcz0) )**0.333333d0

         ncloud8 = dble(nint(dble(ncloud)*0.8d0))

         thstar0 = 1.4d0* (0.005d0/(tan(0.17d0*(2.0d0*ncloud8 +&
         &1.0d0))) + 0.05d0)

         thstarn = thstar0*(1.0d0 -&
         &( (2.0d0*ustar/ustarmax)-1.0d0 )**2.)

         thstar = thstarn

      elseif( L_AdjUstar .and. .not. l_bulkrn&
      &.and. reflvl < 0.0d0 )then
! ---       Use constant THSTAR = 0.08 per Venkatram paper (2011, BLM v138),
!           unless observed temperature profile is available
         thstar = 0.08d0

      elseif( reflvl < 0.0d0 )then
! ---       No ADJ_U* option and no observed temperature profile;
! ---       Apply "standard" approach for THSTAR
         thstar = ustar**2 / ( g * vonkar * obulen / ta )

      endif

      continue

!        Compute DTHETA/dZ at TREFHT

      if( L_AdjUstar )then
         tg4pfl = ( thstar / ( vonkar * tgminht ) ) *&
         &( 0.74d0 + 4.7d0 * tgminht / obulen )
         tg4xtr = ( thstar / ( vonkar * tgmaxht ) ) *&
         &( 0.74d0 + 4.7d0 * tgmaxht / obulen )

      else
         tg4pfl = ( thstar / ( vonkar * tgminht ) ) *&
         &( 1.0d0 + 5.0d0 * tgminht / obulen )
         tg4xtr = ( thstar / ( vonkar * tgmaxht ) ) *&
         &( 1.0d0 + 5.0d0 * tgmaxht / obulen )
      endif

! ---    Check for TG4PFL out-of-range; issue warning if lapse rate > 0.5 K/m
      if( tg4pfl > 0.5d0 )then
         write(dummy,'("TG4PFL=",F5.3)') tg4pfl
         call errhdl(path,modnam,'W','479',dummy)
      endif

   else

!        For the unstable case
      thstar = -9.0d0
      tg4pfl = xval

   endif

   return
end subroutine tginit

subroutine grdptg ()
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
   use main1
   implicit none
   character :: modnam*12

   integer     :: pindex, gindex, ndx
   double precision        :: vbelow, htbelo
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
   modnam = 'GRDPTG'
   gindex = 1
   pindex = 1
   vbelow = -999.0d0
!
!     ------------------------------------------------------------------
!     Loop over each grid level until a value is computed for each level
!     where a value is required OR the number of levels in the gridded
!     profile exceeds the maximum number
!     ------------------------------------------------------------------
!
   do while( gindex <= mxglvl )
!
!       -------------------------------------------
!       Now begin looping over the observed profile
!       -------------------------------------------

!       The 'blending' of the reference profile with observations now
!       applies only to the stable atmosphere

      if( stable )then
!
         do while( gridtg(gindex)<-90.0d0 .and. pindex<=ntglvl )
!
            if( pfltg(pindex) >= -50.0d0 )then
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
               if( dabs(pfltgz(pindex)-gridht(gindex)) <= 0.1d0 )then
!                 USE the parameter at this level
                  gridtg(gindex) = pfltg(pindex)
!
               elseif( gridht(gindex)  >  pfltgz(pindex) )then
                  if( pindex < ntglvl )then
!                    SAVE value for possible interpolation
                     vbelow = pfltg(pindex)
                     htbelo = pfltgz(pindex)

                  else   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPTG
                     call xtrptg ( pfltgz(pindex), pfltg(pindex),&
                     &gridht(gindex), gridtg(gindex) )
                  endif
!
               elseif( gridht(gindex)  <  pfltgz(pindex) )then
                  if( vbelow >= -50.0d0 )then
!                    INTERPOLATE between the two values    --- CALL NTRPTG
                     call ntrptg ( htbelo, vbelow, pfltgz(pindex),&
                     &pfltg(pindex), gridht(gindex),&
                     &gridtg(gindex) )

                  else   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPDN

                     call xtrpdn ( gridht(gindex), gridtg(gindex) )

                  endif
!
               else
!                 This section is for DEBUGging - the program should never
!                 reach this point
                  print *, ' ---> ERROR: The search for data to'
                  print *, '             construct the gridded profile'
                  print *, '             failed on ', kurdat
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
               if( pindex == ntglvl )then
                  if( vbelow  >=  -50.0d0 )then
!                    PROFILE up from BELOW                 --- CALL XTRPTG
                     call xtrptg ( pfltgz(pindex), pfltg(pindex),&
                     &gridht(gindex), gridtg(gindex) )

                  else   ! there are no data
!                    COMPUTE value: full parameterization  --- CALL REFPTG
                     call refptg ( gridht(gindex), gridtg(gindex) )
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
!           grid level was not computed on this pass and continue
!           processing
!           ---------------------------------------------------------
!
            if( (gridtg(gindex) < -50.0d0)  .and.&
            &(pindex < ntglvl) )then
               pindex = pindex + 1
            endif
!
         end do   ! Loop over observed data profile

      elseif( unstab )then

         call refptg( gridht(gindex), gridtg(gindex) )

      endif
!        ------------------------------------------------------------
!        Increment the gridded profile counter and repeat the process
!        starting with the observed value from the profile height as
!        defined by PINDEX
!        ------------------------------------------------------------
!
      gindex = gindex + 1
!
   end do   ! Loop over gridded data profile
!
!
!        ------------------------------------------------------------
!        Apply lower limit of SPTGMN (=0.002 K/m in MAIN1.INC) to
!        lapse rate for stable layers.
!        ------------------------------------------------------------
!
   do ndx = 1,mxglvl
      if( stable .or. (unstab .and. gridht(ndx)>zi) )then
         gridtg(ndx) = max( sptgmn, gridtg(ndx) )
      endif
   end do

   return
end subroutine grdptg


subroutine refptg ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision, parameter :: hdelth = 100.0d0
   double precision               :: htinp, height, value, exparg

   modnam = 'REFPTG'

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

   height = htinp
   if( unstab )then
!
      if( height  <=  zi )then
! ---       Assign unstable lapse rate of 0.0 for height .LE. ZI;
!           value of XVAL is assigned 0.0 in MODULE MAIN1
         value  = xval

      else if( height <= zi+500.0d0 )then
         value  = vptgzi

      else
         value  = 0.005d0

      endif
!
   else   ! stable atmosphere
!        THETA_STAR and TG4PFL (dTheta/dZ at TREFHT) are computed
!        in TGINIT
!
      if (height <= 2.0d0) then

         value = tg4pfl

      else if (height <= 100.0d0) then

         if( L_AdjUstar )then
            value = ( thstar / ( vonkar * height ) ) *&
            &( 0.74d0 + 4.7d0 * height / obulen )
         else
            value = ( thstar / ( vonkar * height ) ) *&
            &( 1.00d0 + 5.0d0 * height / obulen )
         endif

      else
!           COMPUTE gradient from gradient at TREFHT
         exparg =  -1.0d0*(height-100.0d0) / (efoldh*max(hdelth,zi) )
         if (exparg > explim) then
            value = tg4xtr * dexp( exparg )
         else
            value = 0.0d0
         end if

      endif

!        Apply minimum value of 0.002 to all levels of stable profile.
      value = max( value, sptgmn )

   endif

   return
end subroutine refptg


subroutine ntrptg ( htbelo,vbelow, htabov,vabove, reqdht,value )
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
   implicit none
   double precision :: htbelo, vbelow, htabov, vabove, reqdht, value,&
   &refabv, refblw, ratio, refreq, valint, refint

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
   call refptg ( htbelo, refblw )
!
!     Compute the reference profile value at the height above the
!     requested height                                     --- CALL REFPTG
   call refptg ( htabov, refabv )
!
!     Compute the reference profile value at the requested height
!                                                          --- CALL REFPTG
   call refptg ( reqdht, refreq )
!
   if( dabs(refabv - refblw) > 0.0001d0 )then
!
!        Linearly interpolate to REQDHT from observed and reference profiles
      call gintrp ( htbelo,vbelow, htabov, vabove, reqdht,valint )
      call gintrp ( htbelo,refblw, htabov, refabv, reqdht,refint )
!        REFREQ is value from REFerence profile at REQuired height
!        REFINT is value from REFerence profile linearly INTerpolated to req ht
!        VALINT is the observed VALue linearly INTerpolated to required height
      ratio = refreq/refint
      value = ratio * valint
   else
!        INTERPOLATE between VABOVE and VBELOW
      call gintrp ( htbelo,vbelow, htabov,vabove, reqdht,value )
!
   endif

   return
end subroutine ntrptg


subroutine xtrptg ( pflz, pflval, grdz, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision, parameter :: hdelth = 100.0d0
   double precision  :: valobs, valgrd, pflz, grdz, value, pflval, ratio

   modnam = 'XTRPTG'
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

   call refptg ( pflz, valobs )

!     Compute the reference profile value at the height where a value
!     is required                                          --- CALL REFPTG

   call refptg ( grdz, valgrd )

!     The potential temperature gradient is the only profile parameter
!     that can take on a negative value (and in the initial programming
!     of AERMOD, this is in the well-mixed layer for an unstable
!     atmosphere); therefore, if VALOBS is zero, then RATIO = 1.0.

   if( dabs( valobs ) < 0.0001d0 ) then
      ratio = 1.0d0
   else
      ratio = valgrd / valobs
   endif
!
   if (pflz <= 100.0d0) then
      value = ratio * pflval
   else
!        Highest measured Dtheta/Dz is above 100m.  Apply exponential
!        extrapolation term above PFLZ.
      value = pflval * dexp( -(grdz-pflz) /&
      &(efoldh * max(hdelth,zi) ) )
   end if
!
   return
end subroutine xtrptg


subroutine xtrpdn ( grdz, value )
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

   use main1
   implicit none
   character :: modnam*12

   double precision  :: grdz, value

   modnam = 'XTRPDN'
!
!---- Data dictionary

!     GRDZ    = Grid profile height
!     VALUE   = Value at the gridded profile height (GRDZ)

!---- Data initializations
!
!.......................................................................
   if( unstab )then

      if( grdz  <=  zi )then
         value  = xval

      else if( grdz <= zi+500.0d0 )then
         value  = vptgzi

      else
         value  = 0.005d0

      endif

   else   ! stable atmosphere
!        THETA_STAR and TG4PFL (dTheta/dZ at TREFHT) were computed
!        in TGINIT

      if( grdz < 2.0d0 )then
         value = tg4pfl

      else
!           Extrapolate gradient using similarity
         if( L_AdjUstar )then
            value  = ( thstar / ( vonkar * grdz ) ) *&
            &( 0.74d0  +  4.7d0 * grdz / obulen )
         else
            value  = ( thstar / ( vonkar * grdz ) ) *&
            &( 1.0d0  +  5.0d0 * grdz / obulen )
         endif

      endif
   endif

   return
end subroutine xtrpdn


subroutine grdpt ()
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

   use main1
   implicit none
   character :: modnam*12
   integer :: l, nbelow
   double precision :: ptref

   modnam = 'GRDPT'

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

   call locate( gridht, 1, mxglvl, trefht, nbelow )

!---- Compute the potential temperature at the reference level
!     using the reference temperature (TA), the reference
!     temperature height (TREFHT), and the average stack base
!     elevation of all the emission sources (ZBASE)

   ptref = ta + govrcp * (trefht + zbase)

!---- Compute the potential temperature at the grid level below
!     the temperature reference height

   gridpt(nbelow) = ptref -&
   &0.5d0 * (gridtg(nbelow+1) + gridtg(nbelow)) *&
   &(trefht - gridht(nbelow))


!---- Compute Potential Temp Values for Grid Levels Below Reference Ht.
   do l = nbelow-1, 1, -1

      gridpt(l) = gridpt(l+1) - 0.5d0 * (gridtg(l+1) + gridtg(l)) *&
      &(gridht(l+1) - gridht(l))

   end do


!---- Compute Potential Temp Values for Grid Levels Above Reference Ht.
   do l = nbelow+1, mxglvl

      gridpt(l) = gridpt(l-1) + 0.5d0 * (gridtg(l) + gridtg(l-1)) *&
      &(gridht(l) - gridht(l-1))

   end do

   return
end subroutine grdpt


subroutine grdden
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

   use main1
   implicit none
   character :: modnam*12
   integer :: i
   double precision :: tamb0, tamb, tbar, ramb0, rgasm

   modnam = 'GRDDEN'

!---- Data definitions
!
!
!---- Data initializations
!
!
!.......................................................................
!
! --- Set the surface temperature (deg. K) & air density (kg/m**3)
   tamb0=ta
   ramb0=1.2d0

! --- Set the gas constant (m**2/s**2/deg. K)
   rgasm=287.026d0

!---- Compute Ambient Air Density Values
   do i = 1, mxglvl

! ---    Compute ambient air density at height, ZGPTA(i)

      tamb = gridpt(i) - govrcp * (gridht(i) + zbase)
      tbar = 0.5d0 * (tamb + tamb0)
      gridrho(i) = ramb0*(tamb0/tamb)*dexp(-g*gridht(i)/&
      &(rgasm*tbar))

   end do

   return
end subroutine grdden
