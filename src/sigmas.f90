subroutine sigy( xarg )
!***********************************************************************
!             SIGY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates ambient sigma-Y values
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        REVISIONS:  Modified to incorporate meander algorithm
!                    as per Venky's memo of 6/24/98
!                    R. Paine, ENSR, 6/26/98
!
!                    Modified to use OPTB1 for HS .GT. 30m and
!                    OPTB2 for HS .LE. 30m.
!                    R. Brode, PES, 7/14/95
!
!                    Added Developmental Options, OPTB1 and OPTB2
!                    R. Brode, PES, 1/6/95
!
!        INPUTS:  Downwind distance, XARG, in meters
!                 Effective wind speed and sigma_V, UEFF and SVEFF
!
!        OUTPUTS: Ambient Lateral Dispersion Coefficient, SYAMB
!
!        CALLED FROM:   PDIS
!                       VDIS (when volume sources are implemented)
!                       ADIS (when area sources are implemnted)
!
!        Assumptions:
!
!        References:  "Global Organizations and Linkage of Formulas",
!                      7/30/93
!                     "A Dispersion Model for the Convective
!                      Boundary Layer (Revised)", J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision, parameter :: expon = 1.0d0
   double precision :: xarg, svovru, tyeff,&
   &xnodim, denomi, betalph, tyeff3

!     Variable Initializations
   modnam = 'SIGY'

   if( stable .or. (unstab .and. (hs >= zi)) )then
!        The atmosphere is stable or the release is above the CBL mixing ht.
!        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
      svovru = max (svumin, sveff/ueff)
      tyeff  = (zimech/(156.0d0*sveff)) * (max(he, 0.46d0)/0.46d0)
      syamb  = (svovru * xarg)/(1.0d0+xarg/(2.0d0*ueff*tyeff))**0.3d0

   elseif( unstab )then
!        The atmosphere is unstable and the release is below the CBL mixing ht.
!        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
      svovru = max (svumin, sveffd/ueffd)
      xnodim = svovru * xarg / zi
      denomi  = max (hs, 0.46d0)
! ---    BETALPH= MAX( 78.0D0*(0.46D0/DENOMI)**EXPON , 0.7D0)
! ---             recode without EXPON, since EXPON=1
      betalph= max( 78.0d0*(0.46d0/denomi), 0.7d0)
      syamb  = (svovru * xarg) / (1.0d0+betalph*xnodim)**0.3d0

!        Assign ambient value for indirect source = direct source
      syan   = syamb

!        Calculate the ambient sigma_Y for a penetrated plume, SYA3
      if( ppf > 0.0d0 )then
!           Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
         svovru = max (svumin, sveff3/ueff3)
         tyeff3 = (zimech/(156.0d0*sveff3))*(max(he3,0.46d0)/0.46d0)
         sya3   = (svovru*xarg)/&
         &(1.0d0+xarg/(2.0d0*ueff3*tyeff3))**0.3d0

      else
         sya3 = 0.0d0
      endif

   endif

   return
end subroutine sigy

subroutine sigz( xarg )
!***********************************************************************
!             SIGZ Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates ambient sigma-z values
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        REVISIONS:  Modified to use sigma-z formulation from CTDMPLUS
!                    for stable and penetrated plumes.
!                    R. Brode, PES, 8/5/98
!
!                    Added check to sign of TGEFF before calculation
!                    of BVFRQ.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!
!        INPUTS:  Downwind Distance, XARG (in meters)
!                 Effective wind speed, UEFF, and sigma_W, SWEFF
!
!        OUTPUTS: Vertical Dispersion Coefficient, SZ
!
!        CALLED FROM:   PDIS
!                       VDIS (when volume sources are implemented)
!                       ADIS (when area sources are implemented)
!
!        Assumptions:
!
!        References:    "Dispersion in the Stable Boundary Layer",
!                        A. Venkatram, 2/12/93
!                       "A Dispersion Model for the Convective Boundary
!                        Layer (Revised)", J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: ndxzhe
   double precision :: xarg, ttravl, ptp, bvfrq, ztmp, sigf, alphab

!     TTRAVL  = Travel time

!     Variable Initializations
   modnam = 'SIGZ'

   if( stable .or. (unstab .and. hs >= zi) )then
!        The atmosphere is stable or the release was above the CBL mixing ht.
!        See Eq. 1 of the document by Venkatram referenced above.

      ttravl = xarg / ueff

!----    Apply Sigma-Z formulation from CTDMPLUS
!----    Locate index below HE, and retrieve potential temperature at HE

      call locate(gridht, 1, mxglvl, he, ndxzhe)

      call gintrp( gridht(ndxzhe), gridpt(ndxzhe),&
      &gridht(ndxzhe+1), gridpt(ndxzhe+1), he, ptp )

      if (tgeff > 0.0d0) then
         bvfrq = dsqrt( g * tgeff / ptp )
      else
         bvfrq = 1.0d-10
      end if

      if(bvfrq < 1.0d-10) bvfrq = 1.0d-10

!        Set height for calculating sigma-z, ZTMP
      ztmp = max( hs, he, 1.0d-4 )

      if (urbstab) then
!           Set BVF term to zero for urban stable boundary layer
         szamb = sweff * ttravl / dsqrt( 1.0d0 + sweff*ttravl *&
         &( 1.0d0/(0.72d0*ztmp) ) )

      else
         szamb = sweff * ttravl / dsqrt( 1.0d0 + sweff*ttravl *&
         &( 1.0d0/(0.72d0*ztmp) + bvfrq/(0.54d0*sweff) ) )
      end if

      if (he < zi) then
         call szsfcl (xarg)

         sigf = min ( he/zi, 1.0d0)
         szas = (1.0d0 - sigf) * szsurf + sigf * szamb
      else
         szas = szamb
      end if


   elseif( unstab )then
!        The atmosphere is unstable and the release is below the mixing ht.
!        See Eqs. 5c and 24a of the document by Weil referenced above
!        SZAD1 = ambient sigma_Z for the direct plume updraft
!        SZAD2 = ambient sigma_Z for the direct plume downdraft
!        SZAN1 = ambient sigma_Z for the indirect plume updraft
!        SZAN2 = ambient sigma_Z for the indirect plume downdraft

      if (ppf < 1.0d0) then
         if (.not.surfac) then
            alphab = 1.0d0
         else
            alphab = 0.6d0 + 0.4d0*(center/(0.1d0*zi))
         end if

         szad1 = alphab * bsub1 * wstar * xarg / ueffd
         szad2 = alphab * bsub2 * wstar * xarg / ueffd

         call szsfcl (xarg)

         szad1 = dsqrt( szad1*szad1 + szsurf*szsurf )
         szad2 = dsqrt( szad2*szad2 + szsurf*szsurf )

      else
         szad1 = 1.0d0
         szad2 = 1.0d0

      end if

      szan1 = szad1
      szan2 = szad2

!        Calculate the ambient sigma_Z for a penetrated plume, SZA3
      if( ppf > 0.0d0 )then

         ttravl = xarg / ueff3

!----       Apply Sigma-Z formulation from CTDMPLUS
!----       Set BVF term to zero for penetrated plume
         sza3 = sweff3 * ttravl / dsqrt( 1.0d0 + sweff3*ttravl *&
         &( 1.0d0/(0.72d0*he3) ) )

      else
         sza3 = 0.0d0
      endif

   endif

   return
end subroutine sigz

subroutine bid
!***********************************************************************
!             BID Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Bouyancy-Induced Dispersion to
!                 Sigma-y and Sigma-z
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Sigma-y
!                 Sigma-z
!                 Downwind Distance
!                 Buoyancy and Momentum Fluxes
!                 Source Parameter Arrays
!
!        PARAMETERS:  BETA2 = 0.4 (assigned in MODULE MAIN1)
!
!        OUTPUTS: BID contributions to sigma_Y and sigma_Z
!
!        CALLED FROM:   PDIS
!
!        References:  "A Dispersion Model for the Convective Boundary
!                      Layer (Revised)", J. Weil, 8/27/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'BID'

!     Calculate the buoyancy-induced contribution, which will be added
!     to the other contributions in RMSSIG

   if( stable  .or.  (unstab .and. (hs >= zi) ) )then

      szb = beta2 * dhp / rtof2

      syb = szb

      szbd = 0.0d0
      szbn = 0.0d0
      syb3 = 0.0d0
      szb3 = 0.0d0

   elseif( unstab )then

!        Direct source contribution
      szbd = beta2 * dhp1 / rtof2

!        Set SZBN = SZBD.
      szbn = szbd

!        The penetrated source contribution
      if( ppf > 0.0d0 )then
         szb3 = beta2 * ppf * dhp3 / rtof2

      else
         szb3 = 0.0d0

      endif

      syb  = szbd
      syb3 = szb3

   endif

   return
end subroutine bid

subroutine szsfcl (xarg)
!***********************************************************************
!             SZSFCL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the surface layer dispersion term for sigma_Z
!
!        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        REVISIONS:  SZSURF formula revised according to P.D.F. Model
!                    for Dispersion in the Convective Boundary Layer,
!                    J.C. Weil.  Revised 7/13/94, R.F. Lee.
!
!                    Fixed stable SZSURF to match MFD.  R. Brode, PES, 12/5/94
!
!        INPUTS:  Stack height (HS)
!                 Mixing height (ZI)
!                 Friction velocity (USTAR)
!                 Downwind distance (XARG)
!                 Effective wind speed (UEFF)
!                 Monin-Obukhov length (OBULEN)
!
!        OUTPUTS: Surface layer dispersion contribution to sigma_Z
!
!        CALLED FROM:   PDIS
!
!        References:  "A Dispersion Model for the Convective Boundary
!                      Layer", J. Weil, 8/27/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'SZSFCL'

! NOTE --->  BSUBC = 0.5 is set in a PARAMETER stmt in MODULE MAIN1

!---- Calculate the surface layer contribution, which will be added
!     to the other contributions in RMSSIG, from Eqn 31a

   if (unstab .and. surfac) then

      szsurf = bsubc * ( 1.0d0 - 10.0d0 * (center / zi)) *&
      &(ustar / ueffd)*(ustar / ueffd)  *&
      &(xarg * xarg / dabs( obulen ))

   elseif (stable) then
! ---    Always apply SZSURF for STABLE

      szsurf = (rtof2/rtofpi) * ustar * (xarg/ueff) *&
      &(1.0d0 + 0.7d0*xarg/obulen)**(-1.0d0*third)

   else

      szsurf = 0.0d0

   endif

   return
end subroutine szsfcl

subroutine rmssig
!***********************************************************************
!             RMSSIG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the root-mean-square value of sigma_Y and
!                 sigma_Z
!
!        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
!
!        REVISIONS:  Stable treatment of surface releases revised
!                    according to Summary of AERMOD Equations,
!                    A. Venkatram, 7/7/94.  Revision made 7/13/94,
!                    R.F. Lee.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Ambient terms
!                 Virtual source terms
!                 Buoyancy induced terms
!                 Surface layer term
!
!        OUTPUTS: Total sigma_Y (SY) and sigma_Z (SZ)
!
!        CALLED FROM:   PDIS
!
!        References:
!
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'RMSSIG'

!---- Calculate the root-mean-square sigma values

!     1/25/2012, CRT: D063 Add platform sigmas for all plume types.
!     Platforms sigmas are initialized to 0.0 in SUB.PDIS prior to
!     calling SUB.DOWNWASH. Calls to SUB.DOWNWASH can enabled
!     or disabled by plume type without having to alter RMS
!     equations.
!
!     7/8/2021, MGS: Added debug statements to test/explore platform influence
!     on total sigmaY & sigmaZ.

   if( stable  .or.  (unstab .and. (hs >= zi) ) )then
!----    The atmosphere is stable or the atmosphere is unstable and the
!        release is above the mixing height

      sy = dsqrt( syb*syb + syamb*syamb + vsigy*vsigy&
      &+ platsy*platsy )

      sz = dsqrt( szb*szb + szas*szas + vsigz*vsigz&
      &+ platsz*platsz )

      syn  = 0.0d0
      szd1 = 0.0d0
      szd2 = 0.0d0
      szn1 = 0.0d0
      szn2 = 0.0d0

      sy3  = 0.0d0
      sz3  = 0.0d0

!CRT     D063 Platform Downwash Debug
      if (platfmdbg .and. osplat(isrc) .and.&
      &(platsy > 0.0d0 .or. platsz > 0.0d0)) then
         write(platfmdbunt,'(A, 3(2X, A, F7.3))')&
         &'sigmas.f/RMSSIG (STABLE): ',&
         &'PLATSY=', platsy,&
         &'SY = ', dsqrt( syb*syb + syamb*syamb + vsigy*vsigy),&
         &'SY_tot = ', sy
         write(platfmdbunt,'(A, 3(2X, A, F8.3))')&
         &'sigmas.f/RMSSIG (STABLE): ',&
         &'PLATSZ=', platsz,&
         &'SZ = ', dsqrt( szb*szb + szas*szas + vsigz*vsigz),&
         &'SZ_tot = ', sz
      end if

   elseif( unstab )then
!----    The atmosphere is unstable and the release is below the mixing ht.
!CRT     D063 Platform Downwash Sigmas
      sy   = dsqrt( syb*syb + syamb*syamb + vsigy*vsigy&
      &+ max(platsyd1,platsyd2)*max(platsyd1,platsyd2) )
      syn  = dsqrt( syb*syb + syan*syan + vsyn*vsyn&
      &+ max(platsyn1,platsyn2)*max(platsyn1,platsyn2) )
      szd1 = dsqrt( szbd*szbd + szad1*szad1 + vszd1*vszd1&
      &+ platszd1*platszd1 )
      szd2 = dsqrt( szbd*szbd + szad2*szad2 + vszd2*vszd2&
      &+ platszd2*platszd2 )
      szn1 = dsqrt( szbn*szbn + szan1*szan1 + vszn1*vszn1&
      &+ platszn1*platszn1 )
      szn2 = dsqrt( szbn*szbn + szan2*szan2 + vszn2*vszn2&
      &+ platszn2*platszn2 )

      if( ppf > 0.0d0 )then
         sy3 = dsqrt( syb3*syb3 + sya3*sya3&
         &+ platsyp*platsyp )
         sz3 = dsqrt( szb3*szb3 + sza3*sza3&
         &+ platszp*platszp )

      else
         sy3 = 0.0d0
         sz3 = 0.0d0

      endif

!CRT     D063 Platform Downwash Debug
      if (platfmdbg .and. osplat(isrc)) then
         if (platsyd1 > 0.0d0 .or. platsyd2 > 0.0d0) then
            write(platfmdbunt,'(A, 4(2X, A, F8.3))')&
            &'sigmas.f/RMSSIG (UNSTABLE - Direct Plume): ',&
            &'PLATSYD1=', platsyd1,&
            &'PLATSYD2=', platsyd2,&
            &'SY= ', dsqrt( syb*syb + syamb*syamb + vsigy*vsigy),&
            &'SY_tot = ', dsqrt( syb*syb + syamb*syamb + vsigy*vsigy&
            &+ max(platsyd1,platsyd2)*max(platsyd1,platsyd2))
         end if

         if (platsyn1 > 0.0d0 .or. platsyn2 > 0.0d0) then
            write(platfmdbunt,'(A, 4(2X, A, 2X, F7.3))')&
            &'sigmas.f/RMSSIG (UNSTABLE - Indirect Plume): ',&
            &'PLATSYN1=', platsyn1,&
            &'PLATSYN2=', platsyn2,&
            &'SYN =', dsqrt( syb*syb + syan*syan + vsyn*vsyn),&
            &'SYN_tot =', dsqrt( syb*syb + syan*syan + vsyn*vsyn&
            &+ max(platsyn1,platsyn2)*max(platsyn1,platsyn2))
         end if

         if (platszd1 > 0.0d0 .or. platszd2 > 0.0d0) then
            write(platfmdbunt,'(A, 6(2X, A, 2X, F7.3))')&
            &'sigmas.f/RMSSIG (UNSTABLE - Direct Plume): ',&
            &'PLATSZD1=',platszd1,&
            &'SZD1 =', dsqrt( szbd*szbd + szad1*szad1 + vszd1*vszd1),&
            &'SZD1_tot =', dsqrt( szbd*szbd + szad1*szad1 + vszd1*vszd1&
            &+ platszd1*platszd1),&
            &'PLATSZD2=',platszd2,&
            &'SZD2 =', dsqrt( szbd*szbd + szad2*szad2 + vszd2*vszd2),&
            &'SZD2_tot =', dsqrt( szbd*szbd + szad2*szad2 + vszd2*vszd2&
            &+ platszd2*platszd2)
         end if

         if (platszn1 > 0.0d0 .or. platszn2 > 0.0d0) then
            write(platfmdbunt,'(A, 6(2X, A, 2X, F7.3))')&
            &'sigmas.f/RMSSIG (UNSTABLE - Indirect Plume): ',&
            &'PLATSZN1=',platszn1,&
            &'SZN1 =', dsqrt( szbn*szbn + szan1*szan1 + vszn1*vszn1),&
            &'SZN1_tot =', dsqrt( szbn*szbn + szan1*szan1 + vszn1*vszn1&
            &+ platszn1*platszn1),&
            &'PLATSZN2=',platszn2,&
            &'SZN2 =', dsqrt( szbn*szbn + szan2*szan2 + vszn2*vszn2),&
            &'SZN2_tot =', dsqrt( szbn*szbn + szan2*szan2 + vszn2*vszn2&
            &+ platszn2*platszn2)
         end if
      end if

   endif

   return
end subroutine rmssig
