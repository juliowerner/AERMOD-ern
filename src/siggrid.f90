subroutine grdsv ()
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
   use main1
   implicit none
   character :: modnam*12

   integer     :: pindex, gindex
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
   modnam = 'GRDSV '

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
!        -------------------------------------------
!        Now begin looping over the observed profile
!        -------------------------------------------
!
      do while( gridsv(gindex) < -90.0d0 .and. pindex<=nplvls )
!
         if( pflsv(pindex) >= 0.0d0 )then
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
            if( dabs( pflht(pindex)-gridht(gindex)) <= 0.1d0 )then
!                 USE the parameter at this level
               gridsv(gindex) = pflsv(pindex)
!
            elseif( gridht(gindex)  >  pflht(pindex) )then
               if( pindex < nplvls )then
!                    SAVE value for possible interpolation
                  vbelow = pflsv(pindex)
                  htbelo = pflht(pindex)

               else   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPSV
                  call xtrpsv ( pflht(pindex), pflsv(pindex),&
                  &gridht(gindex), gridsv(gindex) )
               endif
!
            elseif( gridht(gindex)  <  pflht(pindex) )then
               if( vbelow >= 0.0d0 )then
!                    INTERPOLATE between the two values    --- CALL NTRPSV
                  call ntrpsv ( htbelo, vbelow, pflht(pindex),&
                  &pflsv(pindex), gridht(gindex),&
                  &gridsv(gindex) )

               else   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPSV
                  call xtrpsv ( pflht(pindex), pflsv(pindex),&
                  &gridht(gindex), gridsv(gindex) )

               endif
!
            else
!                 This section is for DEBUGging - the program should never
!                 reach this point
               write(*,*) ' ---> ERROR: The search for data to'
               write(*,*) '             construct the gridded '
               write(*,*) '             profile failed on ', kurdat
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
!                    PROFILE up from BELOW                 --- CALL XTRPSV
                  call xtrpsv ( htbelo, vbelow,&
                  &gridht(gindex), gridsv(gindex) )

               else   ! there are no data
!                    COMPUTE value: full parameterization  --- CALL REFSV
                  call refsv ( gridht(gindex), gridsv(gindex) )
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
         if( (gridsv(gindex) < 0.0d0)  .and.&
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
!
   end do   ! Loop over gridded data profile
!
   return
end subroutine grdsv

subroutine refsv ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: height, value, svc, svm
   double precision :: htinp
!
!---- Data dictionary
!
!---- Data initializations
!
   modnam = 'REFSV '

   height = htinp
!
!.......................................................................
!
   if( unstab )then
!        Compute the convective component of sigma-v, SVC
      call refsvc( height, svc )

!        Compute the mechanical component of sigma-v, SVM
      call refsvm( height, svm )

      value = dsqrt( svc*svc + svm*svm )

   elseif( stable )then
!        Compute the mechanical component of sigma-v, SVM
      call refsvm( height, svm )

      value = svm

   endif

!
   return
end subroutine refsv

subroutine refsvc ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: height, value, sv2, zdcrs, atzi, sv2dcr, val2
   double precision :: htinp
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
   modnam = 'REFSVC'

   height = htinp
   zdcrs  =  at1pt2 * ziconv
!
!.......................................................................
!
   sv2 = 0.35d0 * wstar**2
!
   if( height  <=  ziconv )then
      value = dsqrt( sv2 )

   elseif( height > ziconv  .and.  height <= zdcrs )then
!        COMPUTE sigmaV at 1.2*ZI
      sv2dcr = min( sv2, 0.25d0 )
!        INTERPOLATE between value of SV2 at ZI and at 1.2*ZI
      call gintrp ( ziconv, sv2, zdcrs, sv2dcr, height, val2 )
      value = dsqrt( val2 )

   else   ! requested height is above 1.2*mixing height
      atzi  = dsqrt( sv2 )
      value = min( atzi, 0.5d0 )

   endif
!
   return
end subroutine refsvc

subroutine refsvm ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: height, value, sv02, sv2zi, varv
   double precision :: htinp
!
!---- Data dictionary
!     SV02   = variance of the horizontal component of the wind at the
!              surface.
!
!---- Data initializations
!
   modnam = 'REFSVM'

   height = htinp
!
!.......................................................................
!
!     Compute SV2 at the surface
   sv02 = 3.6d0 * ustar**2
!
!     Compute SV2 at ZI;
!     Do not let SV2 at ZI exceed the surface value.
   sv2zi = min( sv02, 0.25d0 )

   if( height  <=  zimech )then
!        INTERPOLATE between these two values of the variance if HEIGHT is
!        below ZIMECH
      call gintrp ( 0.0d0, sv02, zimech, sv2zi, height, varv )
      value = dsqrt(varv)

   else
!        Persist value at ZIMECH upward.
      value = dsqrt(sv2zi)

   endif
!
   return
end subroutine refsvm

subroutine ntrpsv ( htbelo,vbelow, htabov,vabove, reqdht,value )
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
   implicit none
   double precision ::  htbelo, vbelow, htabov, vabove, reqdht,&
   &value, refabv, refblw, ratio, refreq, valint, refint
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
   call refsv (  htbelo, refblw )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)    --- CALL REFSV
!
   call refsv (  htabov, refabv )
!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                     --- CALL REFSV
!
   call refsv ( reqdht, refreq )

!
!     Linearly interpolate to REQDHT from observed and reference profiles
   call gintrp ( htbelo,vbelow, htabov, vabove, reqdht,valint )
   call gintrp ( htbelo,refblw, htabov, refabv, reqdht,refint )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height
   ratio = refreq/refint
   value = ratio * valint

   return
end subroutine ntrpsv

subroutine xtrpsv ( pflz, pflval, grdz, value )
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
   implicit none
   double precision :: pflz, pflval, grdz, value, valobs, valgrd,&
   &ratio
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
   call refsv ( pflz, valobs )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required             --- CALL REFSV
!
   call refsv ( grdz, valgrd )
!
   ratio = valgrd / valobs
!
   value = ratio * pflval
!
   return
end subroutine xtrpsv



subroutine grdsw ()
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
   use main1
   implicit none
   character :: modnam*12

   integer     :: pindex, gindex
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
   modnam = 'GRDSW '
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
      do while( gridsw(gindex) < -90.0d0 .and. pindex<=nplvls )
!
         if( pflsw(pindex) >= 0.0d0 )then
!
!              -------------------------------------------------
!              Data at this level are not missing; determine its
!              location relative to the height at which data are
!              required and act accordingly.
!              -------------------------------------------------
            if( dabs( pflht(pindex) - gridht(gindex) )<=0.1d0 )then
!                 USE the parameter at this level
               gridsw(gindex) = pflsw(pindex)
!
            elseif( gridht(gindex)  >  pflht(pindex) )then
               if( pindex < nplvls )then
!                    SAVE value for possible interpolation
                  vbelow = pflsw(pindex)
                  htbelo = pflht(pindex)

               else   ! this is the top level
!                    PROFILE upward from this level        --- CALL XTRPSW
                  call xtrpsw ( pflht(pindex), pflsw(pindex),&
                  &gridht(gindex), gridsw(gindex) )
               endif
!
            elseif( gridht(gindex)  <  pflht(pindex) )then
               if( vbelow >= 0.0d0 )then
!                    INTERPOLATE between the two values    --- CALL NTRPSW
                  call ntrpsw ( htbelo, vbelow, pflht(pindex),&
                  &pflsw(pindex), gridht(gindex),&
                  &gridsw(gindex) )

               else   ! BELOW is missing
!                    PROFILE down from this level          --- CALL XTRPSW
                  call xtrpsw ( pflht(pindex), pflsw(pindex),&
                  &gridht(gindex), gridsw(gindex) )

               endif
!
            else
!                 This section is for DEBUGging - the program should never
!                 reach this point
               write(*,*) ' ---> ERROR: The search for data to'
               write(*,*) '             construct the gridded '
               write(*,*) '             profile failed on ', kurdat
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
!                    PROFILE up from BELOW                 --- CALL XTRPSW
                  call xtrpsw ( htbelo, vbelow,&
                  &gridht(gindex), gridsw(gindex) )

               else   ! there are no data
!                    COMPUTE value: full parameterization  --- CALL REFSW
                  call refsw ( gridht(gindex), gridsw(gindex) )
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
         if( (gridsw(gindex) < 0.0d0)  .and.&
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
!
   end do   ! Loop over gridded data profile
!


   return
end subroutine grdsw

subroutine refsw ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: height, value, swc, swm
   double precision :: htinp
!
!---- Data dictionary
!
!     SWC    = convective component of the variance of the vertical velocity
!     SWM    = mechanical component of the variance of the vertical velocity
!
!---- Data initializations
!
   modnam = 'REFSW '

   height = htinp
!
!.......................................................................
!
   if (unstab) then
!        Compute the convective component of sigma-w, SWC
      call refswc( height, swc )

!        Compute the mechanical component of sigma-w, SWM
      call refswm( height, swm )

!        Apply a lower limit > 0 to convective and mechanical components of sigma-w
      swc = max( swc, 0.0001d0 )
      swm = max( swm, 0.0001d0 )

      value = dsqrt( swc*swc + swm*swm )

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

   else if (stable) then

!        Compute the mechanical component of sigma-w, SWM
      call refswm( height, swm )

!        Apply a lower limit > 0 to convective and mechanical components of sigma-w
      value = max( swm, 0.0001d0 )

   end if

   return
end subroutine refsw

subroutine refswc ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: height, value, sw2, exparg
   double precision :: htinp
!
!---- Data dictionary
!
!     SW2    = variance of the vertical velocity
!
!---- Data initializations
!
   modnam = 'REFSWC'

   height = htinp
!
!.......................................................................
!
   if( height  <=  0.1d0*ziconv )then
      sw2 = 1.6d0 * ( height / ziconv )**(2.0d0*third) * wstar**2
      value  = dsqrt( sw2 )

   elseif( height > 0.1d0*ziconv  .and.  height <= ziconv )then
      value = dsqrt( 0.35d0 * wstar**2 )

   elseif( height > ziconv )then
      exparg = -6.0d0*(height - ziconv)/ziconv
      if (exparg > explim) then
         sw2 = 0.35d0 * wstar**2 * dexp( exparg )
         value = dsqrt( sw2 )
      else
         sw2 = 0.0d0
         value = 0.0d0
      end if

   endif
!

   return
end subroutine refswc

subroutine refswm ( htinp, value )
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
   use main1
   implicit none
   character :: modnam*12

   double precision :: height, value, swr, swbl
   double precision :: htinp
!
!---- Data dictionary
!
!     SWR    = variance of the vertical velocity due to residual turbulence
!     SWBL   = variance of the vertical velocity in the boundary layer
!
!---- Data initializations
!
   modnam = 'REFSWM'

   height = htinp
!
!.......................................................................
!
!

   swr  = swrmax * min( 1.0d0, height/zi)

   if (height < zi) then
      swbl = 1.3d0 * ustar * dsqrt( 1.0d0 - height/zi)
   else
      swbl = 0.0d0
   end if

   value = dsqrt( swr*swr + swbl*swbl )


   return
end subroutine refswm

subroutine ntrpsw ( htbelo,vbelow, htabov,vabove, reqdht,value )
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
   implicit none

   double precision :: refabv, refblw, ratio, refreq, valint, refint,&
   &htbelo, vbelow, htabov, vabove, reqdht, value
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
   call refsw ( htbelo, refblw )

!     COMPUTE the value of the parameter from the reference profile
!     at the height above the requested height (REFABV)         --- CALL REFSW
   call refsw ( htabov, refabv )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the requested height (REFREQ)                          --- CALL REFSW
   call refsw ( reqdht, refreq )
!
!
!     Linearly interpolate to REQDHT from observed and reference profiles
   call gintrp ( htbelo,vbelow, htabov, vabove, reqdht,valint )
   call gintrp ( htbelo,refblw, htabov, refabv, reqdht,refint )
!     REFREQ is value from REFerence profile at REQuired height
!     REFINT is value from REFerence profile linearly INTerpolated to req ht
!     VALINT is the observed VALue linearly INTerpolated to required height
   ratio = refreq/refint
   value = ratio * valint

   return
end subroutine ntrpsw

subroutine xtrpsw ( pflz, pflval, grdz, value )
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
   implicit none

   double precision :: grdz, pflval, pflz, ratio, valobs, valgrd,&
   &value
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
   call refsw ( pflz, valobs )
!
!     COMPUTE the value of the parameter from the reference profile
!     at the height where a value is required                  --- CALL REFSW
   call refsw ( grdz, valgrd )
!
   ratio = valgrd / valobs
!
   value = ratio * pflval
!
   return
end subroutine xtrpsw
