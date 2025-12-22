subroutine afluxes
!***********************************************************************
!      AFLUXES Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    Compute Buoyancy Flux for Aircraft Sources
!                    (Area/Volume Sources)
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA &
!                    Sarav Arunachalam, UNC-IE, Chapel Hill, NC, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Met Variables:
!                    Ambient temperature (K), TA
!                 Engine Parameters:
!                    Fuel Burn Rate (kg/s), MFUEL
!                    Thrust (N), THRUST
!                    Aircraft Velocity (m/s), VAA
!                    Air Fuel Ratio, AFR
!                    By-Pass Ratio, BYPR
!                    Rated Power (kilo watt), RPWR
!                 Constants:
!                    Acceleration due to Gravity (m/s), G = 9.81
!                    Ambient Pressure (N/m2), PAA = 1.013e05
!                    Gas Constant of Air (J/kgK), RAA = 287
!                    Specific Heat of Exhaust Gases (J/kgK), CPA = 1003
!                    Heating Value of Fuel (J/kg), HFUEL = 43e06
!                    Power Setting, PWST = 7% (Idle), 30% (Approach),
!                                   PWST = 85% (Climb-out), 100% (Take-off)
!                    Power setting is based on the Wayson et al. (2009)
!
!        OUTPUTS: Buoyancy Flux, FB (m4/s3)
!
!        CALLED FROM:   ARISE
!
!        REFERENCE: Equations are from Pandey et al., (2023) (In Review),
!                   Accounting for Plume Rise of Aircraft Emissions in AERMOD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
!     Local Variables:

!      DOUBLE PRECISION  :: MFUEL, VAA, THRUST, MAIR, AFR, BYPR, RPWR
   double precision  :: mair, ve, vavg
   double precision  :: vemax, thrstmx
   double precision  :: qee, te, rhoe, etat, pwst, rpwrw

!     Fixed VALUES
!      DOUBLE PRECISION, PARAMETER  ::
!     &                     PAA = 1.013D+5, RAA = 287.058D0,
!     &                     HFUEL = 4.3D+7, CPA = 1003.0D0

!     Variable Initializations
   modnam = 'AFLUXES'

   pi = 4.0d0 * datan(1.0d0)

!     For Turbofan and Turbojet Engines
   if(bypr > 0.0d0) then
!     Check for Positive Fuel burn rate and Thrust
      if (mfuel > 0.0d0 .and. thrust > 0.0d0) then
         mair = mfuel * afr * (1.d0 + bypr)                             ! Total Mass Flow (Equation 22)
         ve   = vaa + (thrust / mair)                                   ! Exhaust Velocity (Equation 24)
         te   = (hfuel*mfuel/mair-((ve*ve)-(vaa*vaa))/2.0d0)/cpa + ta   ! Exhaust Temperature (By substituting eqns 22 and 27 in eqn 21)
         qee  = mair * cpa * (te - ta)                                  ! Thermal Power/Heat Rejection (Equation 27)
      else
         te   = 1.0d-10
         qee  = 1.0d-10
      end if
!     For Non-Turbofan/Turbojet Engines or Shaft-based Engines,
!     Turboprop, Turboshaft, Piston, and Helicopter
!     PWST is based on Table 4 of Wayson et al.(2009)
   else if (bypr == -999.d0) then
      if (afr == 106.d0) then
         pwst = 0.07d0                                               ! Idle/Taxi
      else if (afr == 83.d0) then
         pwst = 0.30d0                                               ! Approach/Landing
      else if (afr == 51.d0) then
         pwst = 0.85d0                                               ! Climb-out
      else
         pwst = 1.0d0                                                ! Take-Off
      end if
!     kilo-Watt to Watt Conversion for Rated Power
      rpwrw = 1000.0d0*rpwr

      etat = ( pwst * rpwrw ) / ( mfuel * hfuel )                   ! Thermal Efficiency (Equation 30)
      qee =  mfuel * hfuel * (1.d0 - etat)                          ! Thermal Power (Equation 31)
      te = ta + (((1.d0 - etat) * hfuel) / (afr * cpa))             ! Exhaust Temperature (Equation 32)
   else
      te  = 1.0d-10
      qee  = 1.0d-10
   end if

   if (te == 1.0d-10) then
      rhoe = 1.0d-10
   else
      rhoe = paa / (raa * te)                                        ! Exhaust Density (Equation 26 or 33)
   end if

   if (qee < 0.0d0 .or. qee == 1.0d-10) then
      fb = 1.0d-10
   else
      fb = g * qee / (pi * rhoe * cpa * ta)                          ! Buoyancy Flux (Equation 5 or 25)
   end if

   return
end subroutine afluxes


subroutine momentum_plumerise (xarg)
!***********************************************************************
!      AIRCRAFT_MOMENTUM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate Momentum Plume Rise for Aircraft Sources.
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA &
!                    Sarav Arunachalam, UNC-IE, Chapel Hill, NC, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Met Variables:
!                    Ambient temperature (K), TA
!                    Wind Speed (m/s), UEFFA
!                    Lateral Wind Velocity Fluctuation (m/s), SVEFFA
!                 Engine Parameters:
!                    Thrust (N), THRUST
!                 Constants:
!                    Ambient Pressure (N/m2), PAA
!                    Gas Constant of Air (J/kgK), RAA = 287.058
!                    Momentum Entrainment Constant, ALPHAM = 0.10
!                    Initial Plume Radius (m), R00 = 2
!
!        OUTPUTS: Momentum Plume Rise, HPM;
!                 Momentum Plume Displacement for Airborne Sources, HDISP;
!                 Final Plume Radius, RP0;
!                 Maximum Distance, XMAXX
!
!        CALLED FROM:   ADELTAH
!
!        REFERENCE: Equations are from Pandey et al., (2023) (In Review),
!                   Accounting for Plume Rise of Aircraft Emissions in AERMOD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*20
!     Local Variables:

   double precision  :: rhoa, rmax, xarg, xpmax, xmaxx
   double precision  :: rp01, rp02, xp, srcangleu
   double precision  :: vemax, thrstmx, ratt,ueffa,sveffa

!     Fixed VALUES
!      DOUBLE PRECISION, PARAMETER  ::
!     &                     PAA = 1.013D+5, RAA = 287.058D0,
!     &                     R00 = 2.0D0, ALPHAM = 0.10D0,
!     &                     HFUEL = 4.3D+7


!     Variable Initializations
   modnam = 'MOMENTUM_PLUMERISE'

!     Set initial effective parameters
   ueffa  = up
   sveffa = svp

   rp0 = 0.0d0                                                       ! Initialize the Maximum Plume Radius
   hpm = 0.0d0                                                       ! Initialize the Momentum Plume Rise
   hdisp = 0.0d0                                                     ! Initialize the Momentum Plume Rise for Airborne Sources

!     For turbofan engines
   if (bypr > 0.0d0) then
!      Check for positive thrust
      if (thrust > 0.0d0) then
!      Calculation of maximum values of exhaust velocity and thrust to
!      avoid inconsistency between thrust and fuel burn rate
         vemax   = sqrt(2.0d0*hfuel/(afr*(1.d0+bypr))+(vaa*vaa))        ! Maximum Exhaust Velocity
         thrstmx = mfuel*afr*(1.d0+bypr)*(vemax-vaa)                    ! Maximum Thrust

         ratt = thrust/thrstmx                                          ! Thrust Ratio

!     For inconsistent thrust and fuel burn values
         if (ratt > 1.0d0 ) then
            thrust = thrstmx
         else
            thrust = thrust
         end if

         rhoa  = paa / (raa * ta)                                       ! Air Density
         rmax  = sqrt(thrust/(pi*rhoa*((vaa+ueffa)+sveffa)*sveffa))     ! Maximum Plume Radius (Equation 11)
         xmaxx = (abs(rmax - r00)) / alpham                             ! Distance for RMAX (Equation 13)

         if ( xarg <= xmaxx) then
            rp0 = r00 + (alpham * xarg/2.0d0)                          ! Final Plume Radius (Equation 15)
            hpm = r00 + (alpham * xarg)                                ! Momentum Plume Rise (Equation 12)
         else
            rp01 = (xmaxx / xarg) * (r00 + (alpham * xmaxx/2.0d0))
            rp02 = rmax * (1.0d0 - (xmaxx/xarg))
            rp0 = rp01 + rp02                                          ! Final Plume Radius (Equation 15)
            hpm = rmax                                                 ! Momentum Plume Rise (Equation 12)
         end if

!        For Airborne Sources, calculate plume displacement using
!        source angle and xpmax
         if (hs > 12.0d0) then
            xpmax = (rp0 - r00) / alpham
!           Set the minimum value for the source angle
            if (srcangle == 0.0d0) then
               srcangleu = 0.01
            else
               srcangleu = srcangle
            end if
            hdisp = xpmax * sin(srcangleu*pi/180.0d0)
         end if

!      For zero thrust values in Turbofan Engines
      else

         rp0 = r00
         hpm = 1.0d-10

      end if

!     For Shaft-based/non-turbofan engines, we assume that momentum
!     induced by Shaft-based/non-turbofan engines is almost negligible
   else

      rp0 = r00
      hpm   = 1.0d-10

   end if


   return
end subroutine momentum_plumerise


subroutine adistf
!***********************************************************************
!                 ADISTF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Distance to Final Plume Rise for Aircraft
!                 Sources.
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Arrays of Source Parameters
!                 Buoyancy Flux, Momentum plumse rise call from arise
!                 Meteorological Variables for One Hour
!                 Wind Speed
!
!        OUTPUTS: Distance to Final Plume Rise, XMAX (m), and Final
!                 Rise (DHFAER)
!
!        CALLED FROM:  VCALC,ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: xln, delhnn, xmaxn
   double precision :: dhfsav       ! save original DHFAER for URBSTAB cases
   double precision :: bvzi2
   double precision :: xmaxi, atmax

!     External Functions:
   double precision, external  :: bisec_tmax

!     Variable Initializations
   modnam = 'ADISTF'


!     For turbofan and non-turbofan/shaft-based engines having greater
!     than 1.0D-10 buoyancy flux
   if ( fb > 1.0d-10 ) then

      if( stable  .or.  (unstab .and. (hs >= zi) ) )then
!        Compute the distance to final rise, XMAX;
!        The negative sign appears on the FB term to ensure that the
!        resulting angle is between 0 and PI (i.e., positive)

         xmax = up * 4.0d0 * datan(1.0d0)/ bvprim
!        Call the momentum plume rise code for RP0             --- CALL MOMENTUM_PLUMERISE
         call momentum_plumerise (xmax)

!        Compute the final stable plume rise, DHF, from Eqn. 3-113 of MFD
         dhfaer = ((rp0/beta1)**3.0d0 + (6.0d0/(beta1**(2.0d0)))*&
         &fb*((4.0d0*datan(1.0d0)/bvf)**2.0d0)/(up+vaa))**(third)-&
         &(rp0/beta1)

         xln = fb/((up+vaa)*ustar*ustar)
         delhnn = 1.2d0*xln**0.6d0 * (hsp + 1.2d0*xln)**0.4d0
         dhfaer = min( dhfaer, delhnn )

!        Compute Neutral/Unstable Final Rise

         xmaxn = ((1.0d0/beta1)**2.0d0)*&
         &((fb*up/(up+vaa))/(swp**3.0d0))

         call cblprd(xmaxn)
         dhfaer = min( dhfaer, dhp1 )

!        Apply calm, stable rise limit
         dhfaer = min( dhfaer, 4.0d0 * fb**0.25d0 / (bvf*bvf)**0.375d0 )

! ---    Save "original" DHFAER for URBSTAB cases
         dhfsav = dhfaer

!        For urban stable boundary layers, limit plume rise to 1.25*ZI - HSP
         if (urbstab) then
! ---       "New fomulation" for v15181 to account for "partial penetration" of plume
!           above the urban "stable" mixing height, similar to approach used for
!           convective conditions
            if( hsp+dhfaer >= zi )then
! ---          Stack height + plume rise is .GE. ZI; use pseudo-penetrated plume
!              approach for URBAN SBL cases

!              Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

               bvzi2 = (g / ptatzi) * 0.005d0

!              Compute the value of PsubS, Eq. 26b in the 2nd reference
               psubs = fb /((up+vaa)*bvzi2*(zi-hsp)*(zi-hsp)*(zi-hsp))

!              Compute the ratio of delta(Hsub_e)/delta(Hsub_h), HEDHH
!              (Eq. 25 in the 2nd ref.
!              NOTE: 17.576 = (2.6)**3 and 0.296296 is (2/3)**3
               hedhh = (17.576d0 * psubs + 0.296296d0) ** third

!              Check the value of HEDHH and compute the plume penetration, P
               if( hedhh < (2.0d0*third) )then
                  ppf = 0.0d0

               else if( hedhh > 2.0d0 )then
                  ppf = 1.0d0

               else
                  ppf = 1.5d0 - (1.0d0 / hedhh)

               end if

! ---          Include calculation of penetrated plume rise and height
               if( ppf > 0.0d0 )then

!                 Compute the plume height for the penetrated source
!                 (See Eq. 8 in the reference for Source 3)
                  if (ppf == 1.0d0) then
                     dhfaer = hedhh * (zi-hsp)
                  else
                     dhfaer = 0.75d0 * (zi-hsp) * hedhh + 0.5d0*(zi-hsp)
                  end if

               else
! ---             Use "original" DHFAER value
                  dhfaer = dhfsav

               end if

            end if
         end if

      else
!        Unstable plume

         xmaxi = ((1.0d0/beta1)**2.0d0)*&
         &((fb*up/(up+vaa))/(swp**3.0d0))

!        Call the momentum plume rise code for RP0             --- CALL MOMENTUM_PLUMERISE
         call momentum_plumerise (xmaxi)

!        Calculation of ATMAX using the BISECTION_TMAX function
         atmax = bisec_tmax(fb,vaa,rp0,up,swp)

         xmax  = up * atmax

!        Call the momentum plume rise code for RP0             --- CALL MOMENTUM_PLUMERISE
         call momentum_plumerise (xmax)

         if (hs <= 12.0d0) then

!       Calculation of DHP is based on the equation 6 of Pandey et al. (2023)
            dhp1 = ((rp0/beta1)**3.0d0 + (1.5d0/beta1**(2.0d0))*&
            &fb*((atmax)**2.0d0)/(up+vaa))**(third)- rp0/beta1

            dhfaer = dhp1
         else
            dhfaer = 1.85d0 * (fb/(swp*swp*(up+vaa)))
         end if

      end if
!     Turbofan Engines (having zero thrust) and Shaft-based Engines
!     (having negligible or zero buoyancy flux)
   else
      xmax  = 0.0d0
      dhfaer = 1.0d-10
   end if

!     Check the Aircraft Debug Option to print the debug file
   if(arcftdebug) then

      write(arcftdbg,*) '===========================================',&
      &'=============================================================',&
      &'========================'
      write (arcftdbg,*)'SOURCE TYPE:  ',srctyp(isrc),&
      &'    SOURCE ID:  ',srcid(isrc)  ,'      SOURCE NO.:  ',isrc
      write (arcftdbg,7130) kurdat

      write (arcftdbg,7135) mfuel, thrust, vaa, afr, bypr,&
      &rpwr, srcangle, fb, xmax, dhfaer, rp0, hpm,&
      &hdisp

7130  format(' YR/MN/DY/HR:  ', i8,&
      &//,2x,'<----------------------------------- SOURCE INFORMATION',&
      &' ----------------------------------->',/,&
      &'  MFUEL    THRUST     VAA     AFR     BYPR',&
      &'      RPWR      SRCANGLE    FB      XMAX     DHFAER    RP0',&
      &'     HPM      HDISP',/,&
      &' (KG/S)     (N)      (M/S)    (#)     (#)',&
      &'      (kW)      (degree)   (M4/S3)    (M)      (M)',&
      &'      (M)     (M)       (M)',/)
7135  format(1x,f5.2,2x,f10.1,1x,f7.2,3x,f5.1,2x,f7.2,2x,f10.1,3x,f6.2,&
      &2x,f9.1,1x,f8.2,1x,f8.1,2x,f5.1,4x,f5.1,4x,f5.1)


   end if


   return
end subroutine adistf

double precision function bisec_tmax(fbb,vab,rp00,aueffff,sweffaa)
!***********************************************************************
!        COMPUTE_TMAX Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate MaxRMAimum time when the rate of plume rise equal
!                 to SIGMA-W.
!
!        PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!                    Akula Venkatram, UC-Riverside, CA, USA.
!
!        DATE:    April 01, 2023
!
!        INPUTS:  Variables:
!                  Buoyancy Flux (m4/s3), FBB
!                  Aircraft Speed (m/s), VAB
!                  Wind Speed (m/s), AUEFFF
!                  Plume Radius (m), RP00
!                  Vertical Wind Velocity Fluctuation (m/s), SWEFFAA
!                 Constants:
!                  Buoyancy Entrainment Constant, BETA1 = 0.6
!        OUTPUTS:  Time (s), ATMAX
!
!        CALLED FROM: MAXIMUM_PLUMERISE
!***********************************************************************
!     Variable Declarations
!      USE MAIN1
   implicit none

   double precision  :: atmax, rp00
   double precision  :: a, b, c, fbb, vab
   double precision  :: xm, x1, x2, fright,&
   &xright, xleft, delx, xmid,&
   &fmid, aueffff, sweffaa

   integer :: i, aaaiter, nsteps
!     Fixed Values
   double precision, parameter  :: errlimit = 1.0e-04, beta1 = 0.60d0

   nsteps = anint(dlog(1.0d0/errlimit))

   xm = ((2.0d0/3.0d0/beta1)**2.0d0)*&
   &((fbb/(aueffff+vab))/(sweffaa**3.0d0))        ! Equation 17

   x1 = xm/2.0

   x2 = 2.0*xm

   a = (fbb/(aueffff+vab)/beta1)**(2.0d0)

   b = (rp00/beta1)**3.0d0

   c = (b + a*1.5d0*x2**2.0d0)

   fright = a*x2 - sweffaa*c**(2.0d0/3.0d0)

   if (fright > 0.0d0) then

      xright = x2
      xleft  = x1

   else

      xright = x1

      xleft  = x2

   end if

   do i = 1, nsteps

      delx = xright - xleft

      xmid = xleft + 0.5d0*delx

      c = (b + a*1.5d0*xmid**2.0d0)

      fmid = a*xmid - sweffaa*c**(2.0d0/3.0d0)

      if (fmid > 0.0d0) then

         xright = xmid

      else

         xleft = xmid

      end if

   end do

   atmax = xmid

end function bisec_tmax


subroutine adeltah ( xarg )
!***********************************************************************
!             ADELTAH Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA &
!               Sarav Arunachalam, UNC-IE, Chapel Hill, NC, USA.
!
!   DATE:    April 01, 2023
!
!   INPUTS:  The distance at which to make the computation, XARG
!
!   OUTPUTS: Distance-Dependent Plume Rise, DHP (m)
!
!   CALLED FROM:  VCALC, ACALC
!
!   Assumptions:  All plume rise calculations are for gradual rise,
!                 except in stable conditions when the downwind distance
!                 exceeds XMAX
!
!   References:   "Accounting for Plume Rise of Aircraft Emissions in AERMOD"
!                 G.Pandey, A. Venkatram, & S. Arunachalam; In Review 2023
!                 "Dispersion in the Stable Boundary Layer",
!                 A. Venkatram, 2/12/93
!                 "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!                 "Plume Penetration of the CBL and Source 3: Source
!                 Strength and Plume Rise", J. Weil, 9/1/93
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none

   character :: modnam*12
   integer :: kiter, ndxzpl
   double precision :: xarg, xmaxtmp, xrise, zplm, dhpold,&
   &svpm, swpm, upm, tgpm, ptpm, ptp

!     Variable Initializations
   modnam = 'ADELTAH'


   if( (stable  .or.  (unstab  .and.  (hs >= zi)))  .and.&
   &(xarg >= xmax) )then
!        Use final stable plume rise (DHF) calculated in DISTF (DHP)
!        at XMAX
      dhp = dhfaer

   else if( (stable  .or. (unstab  .and.  (hs >= zi))) .and.&
   &(xarg < xmax) ) then
!----    Compute stable plume rise for the distance XARG   --- CALL ASBLRIS
!        Use iterative approach to plume rise calculations.
!        First compute temporary distance to "final rise" based on current
!        values of UP and BVPRIM.  Then, don't pass a distance larger than
!        XMAXTMP to ASBLRIS.  This avoids potential for math error in
!        SUB. ASBLRIS for distances beyond the value of XMAX computed
!----    iteratively outside the receptor loop in SUB. ADISTF.

      xmaxtmp = up * 4.0d0 * datan(1.0d0)/ bvprim
      xrise   = min( xarg, xmaxtmp )
      call asblris ( xrise )
      kiter = 0

50    zplm = hsp + 0.5d0 * dhp
      dhpold = dhp

!----    Locate index below ZPLM

      call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----    Get Wind speed at ZPLM; replace UP, SVP, SWP.  Also, replace TGP,
!        vertical potential temperature gradient, if stable.

      call gintrp( gridht(ndxzpl), gridsv(ndxzpl),&
      &gridht(ndxzpl+1), gridsv(ndxzpl+1), zplm, svpm )
      call gintrp( gridht(ndxzpl), gridsw(ndxzpl),&
      &gridht(ndxzpl+1), gridsw(ndxzpl+1),&
      &zplm, swpm )

      call gintrp( gridht(ndxzpl), gridws(ndxzpl),&
      &gridht(ndxzpl+1), gridws(ndxzpl+1), zplm, upm )

      svpm = max( svpm, svmin, svumin*upm )
      swpm = max( swpm, swmin )

      if( l_vectorws )then
         upm = dsqrt( upm*upm + 2.0d0*svpm*svpm )
      endif
      upm  = max( upm, wsmin )

!RWB     Use average of stack top and midpoint wind speeds and others.
      up = 0.5d0 * (us + upm)
      svp = 0.5d0 * (svs + svpm)
      swp = 0.5d0 * (sws + swpm)

      call gintrp( gridht(ndxzpl), gridtg(ndxzpl),&
      &gridht(ndxzpl+1), gridtg(ndxzpl+1), zplm, tgpm )
      call gintrp( gridht(ndxzpl), gridpt(ndxzpl),&
      &gridht(ndxzpl+1), gridpt(ndxzpl+1), zplm, ptpm )
!RWB     Use average of stack top and midpoint temperature gradients.
      tgp = 0.5d0 * (tgs + tgpm)
      ptp = 0.5d0 * (pts + ptpm)
      bvf = dsqrt( g * tgp / ptp)
      if(bvf < 1.0d-10) bvf = 1.0d-10
      bvprim  = 0.7d0 * bvf

!        Repeat calculation of temporary distance to "final rise" using
!        current values of UP and BVPRIM.
      xmaxtmp = up * 4.0d0 * datan(1.0d0)/ bvprim
      xrise   = min( xarg, xmaxtmp )
      call asblris ( xrise )

      kiter = kiter + 1

!RJP     Add temporary debugging statements

      if(arcftdebug) then
         write(arcftdbg,6001) kiter,dhpold, dhp, zplm, up,tgp
6001     format(/,5x,'OPTH2 ITER. #',i1,': OLD DELH = ',&
         &f6.1,' M; NEW DELH = ',f6.1,' M; MET LEVEL = ',&
         &f6.1,' M; NEW Upl = ',f5.2,' M/S; NEW DTHDZ = ',&
         &f7.4,' K/M')
      endif

!        Check for convergence
      if(dhp>0.0d0 .and. dabs((dhpold-dhp)/dhp)<0.001d0 .and.&
      &kiter >= 5)then
         if( dhp <= 1.0d-5 )then
            dhp = 1.0d-5
         endif
         go to 60
      elseif(kiter < 10)then
         go to 50
      endif

      if(kiter >= 5) then
         dhp = 0.5d0 * (dhp + dhpold)
         if(arcftdebug) write(arcftdbg,6002) dhp
6002     format(/,5x,'OPTH2 ITERATION FAILED TO CONVERGE; PLUME',&
         &' RISE SET AT ',f6.1,' METERS.',/)
         go to 60
      else
         go to 50
      endif

60    continue

!RWB     After completing iteration, reset UP, SVP, SWP and TGP to stack top
!RWB     values for subsequent distance-dependent plume rise calcs.
      up  = us
      svp = svs
      swp = sws
      tgp = tgs
      ptp = pts
      bvf = dsqrt( g * tgp / ptp )
      if(bvf < 1.0d-10) bvf = 1.0d-10
      bvprim  = 0.7d0 * bvf
!crfl-3/6/95 Make sure SBL rise is not greater than CBL rise.
      call acblprd(xarg)
      dhp = min(dhp,dhp1)
      dhp = min(dhp,dhfaer)

   elseif( unstab )then
!        (i.e., for UNSTABle cases, with HS < ZI)

!        Compute  plume rise for the direct plume          --- CALL ACBLPRD
      call acblprd ( xarg )

!        Compute  plume rise for the indirect plume        --- CALL ACBLPRN
      call acblprn ( xarg )

      if( ppf > 0.0d0 )then
!           Compute plume rise for the penetrated plume    --- CALL ACBLPR3
         call acblpr3

      else
!           No plume penetration - plume rise is zero for this source
         dhp3 = 0.0d0

      endif

   endif

   return
end subroutine adeltah


subroutine asblris ( xarg )
!***********************************************************************
!             ASBLRIS Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE:  To calculate plume rise for the stable boundary layer
!             or releases above the convective boundary layer for
!             Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA.
!
!   DATE:    April 01, 2023
!
!
!   INPUTS:  Brunt-Vaisala frequency, S
!            Buoyancy flux, FB
!            Momentum Plume Rise, HPM
!            Maximum Plume Radius, RP0
!            Downwind distance, XARG
!            Wind speed at stack top, UP
!            Aircraft Speed, VAA
!
!   OUTPUTS: Plume Rise, DHP (m)
!
!   CALLED FROM:  VCALC, ACALC
!
!   Assumptions:  Wind speed is nonzero at stack top
!
!   References:   "Accounting for Plume Rise of Aircraft Emissions in AERMOD"
!                 G.Pandey, A. Venkatram, & S. Arunachalam; In Review 2023
!                 "Dispersion in the Stable Boundary Layer",
!                 A. Venkatram, 2/12/93
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg, terma, termc, termd, terme
   double precision :: xln, delhnn

!     Variable Initializations
   modnam = 'ASBLRIS'

!     Call MOMENTUM_PLUMERISE to calculate the RP0 (plume radius) and
!     (HPM) horizontal momentum-induced plume rise

   call momentum_plumerise (xarg)                                   ! CALL MOMENTUM_PLUMERISE

!     For turbofan and non-turbofan/shaft-based engines
   if (bypr > 0.0d0 .or. bypr == -999.0d0 .and.&
   &fb > 1.0d-10) then

!       Calculation of DHP is based on the equation 6 of Pandey et al. (2023)
      dhp = ((rp0/beta1)**3.0d0 + (1.5d0/beta1**(2.0d0))*&
      &fb*((xarg/up)**2.0d0)/(vaa+up))**(third)- rp0/beta1 +&
      &hpm                                                   ! Momentum Plume Rise

! ---   Apply lower limit on stable plume rise based on Equation 98
!       of the MFD

      xln = fb/((up+vaa)*ustar*ustar)
      delhnn = 1.2d0*xln**0.6d0 * (hsp + 1.2d0*xln)**0.4d0

      dhp = min( dhp, dhfaer, delhnn )
      dhp = min(dhp, abs(zi-hs))

!      Turbofan Engines (having zero thrust) and Shaft-based Engines
!      (having negligible or zero buoyancy flux), in that case total plume rise
!      equal to momentum plume rise only)
   else

      dhp = hpm
      dhp = min(dhp, abs(zi-hs))

   end if

!     For Airborne Aircraft Sources
   if (hs > 12) then
      dhp = min(dhp, abs(zi-hs))
      dhp = max(hpm,(dhp-hdisp))
   end if

   return
end subroutine asblris

subroutine acblprd ( xarg )
!***********************************************************************
!             ACBLPRD Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the direct plume in the
!            convective boundary layer for Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA.
!
!   DATE:    April 01, 2023
!
!
!   INPUTS:  Buoyancy flux, FB
!            Momentum Plume Rise, HPM
!            Maximum Plume Radius, RP0
!            Downwind distance, XARG
!            Wind speed at stack top, UP
!            Aircraft Speed, VAA
!
!   OUTPUTS: Plume Rise, DHP1 (m)
!
!   CALLED FROM:  ADELTAH, VCALC, ACALC
!
!   Assumptions:  Wind speed is nonzero at stack top
!                 BETA1 = 0.6D0 (assigned in MODULE MAIN1)
!
!   References:   "Accounting for Plume Rise of Aircraft Emissions in AERMOD"
!                 G.Pandey, A. Venkatram, & S. Arunachalam; In Review 2023
!
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'ACBLPRD'

!     Call MOMENTUM_PLUMERISE to calculate the RP0 (plume radius) and
!     (HPM) horizontal momentum-induced plume rise

   call momentum_plumerise (xarg)                                   ! CALL MOMENTUM_PLUMERISE

!     For turbofan and non-turbofan/shaft-based engines
   if (bypr > 0.0d0 .or. bypr == -999.0d0 .and.&
   &fb > 1.0d-10) then

!      Calculation of DHP is based on the equation 6 of Pandey et al. (2023)
      dhp1 = ((rp0/beta1)**3.0d0 + (1.5d0/beta1**(2.0d0))*&
      &fb*((xarg/up)**2.0d0)/(vaa+up))**(third)- rp0/beta1 +&
      &hpm                                                    ! Momentum Plume Rise

      dhp1 = min(dhp1, abs(zi-hs))

!      Turbofan Engines (having zero thrust) and Shaft-based Engines
!      (having negligible or zero buoyancy flux), in that case total plume rise
!      equal to momentum plume rise only)
   else

      dhp1 = hpm
      dhp1 = min(dhp1, abs(zi-hs))

   end if

!     For Airborne Aircraft Sources
   if (hs > 12) then

      dhp1 = min(dhp1, abs(zi-hs))
      dhp1 = max(hpm,(dhp1-hdisp))

   end if

   return
end subroutine acblprd

subroutine acblprn ( xarg )
!***********************************************************************
!             ACBLPRN Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the indirect plume in the
!            convective boundary layer for Aircraft Sources
!
!   PROGRAMMER: Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA &
!               Akula Venkatram, UC-Riverside, CA, USA.
!
!   DATE:     April 01, 2023
!
!   CHANGES:  Added Aircraft speed along with wind speed to calculate the
!             aircraft related plume rise
!
!             Equation for DHP2 revised per memorandum from Jeff Weil
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
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg, rsubh, ryrz, delhi

!     Variable Initializations
   modnam = 'ACBLPRN'

   rsubh = beta2 * (zi - hsp)
   ryrz  = rsubh * rsubh + 0.25d0*asube*(lamday**1.5d0) *&
   &wstar*wstar*xarg*xarg/(up*up)
   delhi = dsqrt((2.d0*fb*zi)/(alphar*(up+vaa)*ryrz))*(xarg/up)
   dhp2  = delhi

   return
end subroutine acblprn

subroutine acblpr3
!***********************************************************************
!             ACBLPR3 Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: To calculate plume rise for the penetrated plume in the
!            convective boundary layer for aircraft sources
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
!CC The framework of the plume rise calculations is adopted from PRISE.f CC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'CBLPR3'

!     The plume rise for the penetrated source is delta(Hsub_3), given
!     by Eq. 9 in Jeff Weil's 9/1/93 document.  The variable HEDHH is
!     delta(Hsub_e)/delta(Hsub_h), calculated from Eq. 26a of Jeff Weil's
!     8/17/93 document, where delta(Hsub_h) is ZI-HS.

   if (ppf == 1.0d0) then
      dhp3 = hedhh * (zi-hsp)
   else
      dhp3 = 0.75d0 * (zi-hsp) * hedhh + 0.5d0 * (zi-hsp)
   end if

   return

end subroutine acblpr3
