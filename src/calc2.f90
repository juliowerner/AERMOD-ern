subroutine emfact (qarg)
!***********************************************************************
!                 EMFACT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Variable Emission Rate and
!                 Unit Conversion Factors
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!        MODIFIED  : for handling OpenPit Source Type - PES, 7/26/94
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To include options to vary emissions by
!                    hour-of-day and day-of-week (HRDOW and HRDOW7).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG,  10/19/2009
!
!        MODIFIED:   To include options to vary emissions by month,
!                    hour-of-day, and day-of-week (MHRDOW and MHRDOW7).
!                    R.W. Brode, MACTEC (f/k/a PES), Inc., 06/22/05
!
!        MODIFIED:   To include an option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, 4/10/2000
!
!        INPUTS:  Arrays of Source Parameters
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: Adjusted Emission Rate, QTK
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: qarg

!     Variable Initializations
   modnam = 'EMFACT'

! --- Apply Variable Emission Rate Factor, Based on Value of QFLAG
!     Emission unit factor is applied later since it varies by
!     output type
   if (qflag(isrc) == ' ') then
      qtk = qarg

!*----   ISCSTM Modification: To handle hourly emissions - jah 11/4/94
   else if (qflag(isrc) == 'HOURLY') then
      qtk = qarg
!*----
!*#

   else if (qflag(isrc) == 'MONTH') then
      qtk = qarg * qfact(imonth,isrc)

   else if (qflag(isrc) == 'HROFDY') then
      qtk = qarg * qfact(ihour,isrc)

   else if (qflag(isrc) == 'WSPEED') then
      qtk = qarg * qfact(iucat,isrc)

   else if (qflag(isrc) == 'SEASON') then
      qtk = qarg * qfact(iseas,isrc)

   else if (qflag(isrc) == 'SEASHR') then
      qtk = qarg * qfact((ihour+(iseas-1)*24),isrc)

   else if (qflag(isrc) == 'HRDOW') then
      qtk = qarg * qfact((ihour +&
      &(iday_of_week-1)*24),isrc)

   else if (qflag(isrc) == 'HRDOW7') then
      qtk = qarg * qfact((ihour +&
      &(iday_of_week7-1)*24),isrc)

   else if (qflag(isrc) == 'SHRDOW') then
      qtk = qarg * qfact((ihour+(iseas-1)*24+&
      &(iday_of_week-1)*96),isrc)

   else if (qflag(isrc) == 'SHRDOW7') then
      qtk = qarg * qfact((ihour+(iseas-1)*24+&
      &(iday_of_week7-1)*96),isrc)

   else if (qflag(isrc) == 'MHRDOW') then
      qtk = qarg * qfact((ihour+(imonth-1)*24+&
      &(iday_of_week-1)*288),isrc)

   else if (qflag(isrc) == 'MHRDOW7') then
      qtk = qarg * qfact((ihour+(imonth-1)*24+&
      &(iday_of_week7-1)*288),isrc)

   end if

   return
end subroutine emfact

subroutine bgval (isect,barg)
!***********************************************************************
!                 BGVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes temporally-varying BACKGRND concentrations
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    February 28, 2011
!
!        INPUTS:  Arrays of Source Parameters
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: Adjusted Emission Rate, QTK
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer          :: isect

   double precision :: barg

!     Variable Initializations
   modnam = 'BGVAL'

! --- Apply temporally-varying background concentration value
!     to BARG variable
   if (bflag(isect) == 'ANNUAL') then
      barg = backgrnd(1,isect)

   else if (bflag(isect) == 'MONTH') then
      barg = backgrnd(imonth,isect)

   else if (bflag(isect) == 'HROFDY') then
      barg = backgrnd(ihour,isect)

   else if (bflag(isect) == 'WSPEED') then
      barg = backgrnd(iucat,isect)

   else if (bflag(isect) == 'SEASON') then
      barg = backgrnd(iseas,isect)

   else if (bflag(isect) == 'SEASHR') then
      barg = backgrnd(ihour+(iseas-1)*24,isect)

   else if (bflag(isect) == 'HRDOW') then
      barg = backgrnd(ihour +&
      &(iday_of_week-1)*24,isect)

   else if (bflag(isect) == 'HRDOW7') then
      barg = backgrnd(ihour +&
      &(iday_of_week7-1)*24,isect)

   else if (bflag(isect) == 'SHRDOW') then
      barg = backgrnd(ihour+(iseas-1)*24+&
      &(iday_of_week-1)*96,isect)

   else if (bflag(isect) == 'SHRDOW7') then
      barg = backgrnd(ihour+(iseas-1)*24+&
      &(iday_of_week7-1)*96,isect)

   else if (bflag(isect) == 'MHRDOW') then
      barg = backgrnd(ihour+(imonth-1)*24+&
      &(iday_of_week-1)*288,isect)

   else if (bflag(isect) == 'MHRDOW7') then
      barg = backgrnd(ihour+(imonth-1)*24+&
      &(iday_of_week7-1)*288,isect)

   end if

! --- Adjust background concentration units to UG/M3 if needed;
!     conversion is based on reference temperature (25C) and
!     pressure (1013.25 mb)
   if (pollut == 'NO2') then
      if (BackUnits == 'PPB') then
         barg = barg / no2_ppb
      else if (BackUnits == 'PPM') then
         barg = barg / no2_ppm
      end if
   else if (pollut == 'SO2') then
      if (BackUnits == 'PPB') then
         barg = barg / so2_ppb
      else if (BackUnits == 'PPM') then
         barg = barg / so2_ppm
      end if
   else if (pollut == 'CO') then
      if (BackUnits == 'PPB') then
         barg = barg * co_ppb
      else if (BackUnits == 'PPM') then
         barg = barg * co_ppm
      end if
   end if

   return
end subroutine bgval

subroutine ozonvals (isect,o3arg)
!***********************************************************************
!                 OZONVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Variable Ozone Concentrations
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    February 28, 2011
!
!        MODIFIED:  Modified to use ISECT instead of IO3SECT for
!                   HRDOW option.
!
!                   Roger Brode, EPA
!                   January XX, 2015
!
!        INPUTS:  Arrays of Ozone Values
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: Ozone value in ug/m3
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer          :: isect

   double precision :: o3arg

!     Variable Initializations
   modnam = 'OZONVALS'

! --- Apply Variable Background O3 values Based on Value of O3FLAG
   if (o3flag(isect) == 'ANNUAL') then
      o3arg = o3vary(1,isect)

   else if (o3flag(isect) == 'MONTH') then
      o3arg = o3vary(imonth,isect)

   else if (o3flag(isect) == 'HROFDY') then
      o3arg = o3vary(ihour,isect)

   else if (o3flag(isect) == 'WSPEED') then
      o3arg = o3vary(iucat,isect)

   else if (o3flag(isect) == 'SEASON') then
      o3arg = o3vary(iseas,isect)

   else if (o3flag(isect) == 'SEASHR') then
      o3arg = o3vary(ihour+(iseas-1)*24,isect)

   else if (o3flag(isect) == 'HRDOW') then
      o3arg = o3vary(ihour +&
      &(iday_of_week-1)*24,isect)

   else if (o3flag(isect) == 'HRDOW7') then
      o3arg = o3vary(ihour +&
      &(iday_of_week7-1)*24,isect)

   else if (o3flag(isect) == 'SHRDOW') then
      o3arg = o3vary(ihour+(iseas-1)*24+&
      &(iday_of_week-1)*96,isect)

   else if (o3flag(isect) == 'SHRDOW7') then
      o3arg = o3vary(ihour+(iseas-1)*24+&
      &(iday_of_week7-1)*96,isect)

   else if (o3flag(isect) == 'MHRDOW') then
      o3arg = o3vary(ihour+(imonth-1)*24+&
      &(iday_of_week-1)*288,isect)

   else if (o3flag(isect) == 'MHRDOW7') then
      o3arg = o3vary(ihour+(imonth-1)*24+&
      &(iday_of_week7-1)*288,isect)

   end if

! --- Convert O3ARG from PPB or PPM to UG/M3, based on reference
!     temperature (25C) and pressure (1013.25 mb)
   if (OzoneUnits == 'PPB') then
      o3arg = o3arg * o3_ppb
   else if (OzoneUnits == 'PPM') then
      o3arg = o3arg * o3_ppm
   else if (OzoneUnits == 'UG/M3') then
      o3arg = o3arg
   else
! ---    Default units are PPB
      o3arg = o3arg * o3_ppb
   end if

   return
end subroutine ozonvals

subroutine varynoxvals (isect,noxarg)
!***********************************************************************
!                 VARYNOXVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Applies Variable NOx Concentrations
!
!        PROGRAMMER: CERC
!
!        DATE:     November 2020
!
!        INPUTS:  Arrays of NOx Values
!                 Date and Hour
!                 Meteorological Variables for One Hour
!                 Variable Emission Rate Flags and Factors
!                 Unit Conversion Rate Factors
!
!        OUTPUTS: NOx value in ug/m3
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer          :: isect

   double precision :: noxarg

!     Variable Initializations
   modnam = 'VARYNOXVALS'

! --- Apply Variable Background NOX values Based on Value of NOXFLAG
   if (noxflag(isect) == 'ANNUAL') then
      noxarg = noxvary(1,isect)

   else if (noxflag(isect) == 'MONTH') then
      noxarg = noxvary(imonth,isect)

   else if (noxflag(isect) == 'HROFDY') then
      noxarg = noxvary(ihour,isect)

   else if (noxflag(isect) == 'WSPEED') then
      noxarg = noxvary(iucat,isect)

   else if (noxflag(isect) == 'SEASON') then
      noxarg = noxvary(iseas,isect)

   else if (noxflag(isect) == 'SEASHR') then
      noxarg = noxvary(ihour+(iseas-1)*24,isect)

   else if (noxflag(isect) == 'HRDOW') then
      noxarg = noxvary(ihour +&
      &(iday_of_week-1)*24,isect)

   else if (noxflag(isect) == 'HRDOW7') then
      noxarg = noxvary(ihour +&
      &(iday_of_week7-1)*24,isect)

   else if (noxflag(isect) == 'SHRDOW') then
      noxarg = noxvary(ihour+(iseas-1)*24+&
      &(iday_of_week-1)*96,isect)

   else if (noxflag(isect) == 'SHRDOW7') then
      noxarg = noxvary(ihour+(iseas-1)*24+&
      &(iday_of_week7-1)*96,isect)

   else if (noxflag(isect) == 'MHRDOW') then
      noxarg = noxvary(ihour+(imonth-1)*24+&
      &(iday_of_week-1)*288,isect)

   else if (noxflag(isect) == 'MHRDOW7') then
      noxarg = noxvary(ihour+(imonth-1)*24+&
      &(iday_of_week7-1)*288,isect)

   end if

! --- Convert NOXARG from PPB or PPM to UG/M3, based on reference
!     temperature (25C) and pressure (1013.25 mb)
! --- using NO2 factors (NOx expressed as 'NOx as NO2')
   if (NOxUnits == 'PPB') then
      noxarg = noxarg / no2_ppb
   else if (NOxUnits == 'PPM') then
      noxarg = noxarg / no2_ppm
   else if (NOxUnits == 'UG/M3') then
      noxarg = noxarg
   else
! ---    Default units are PPB
      noxarg = noxarg / no2_ppb
   end if

   return
end subroutine varynoxvals

subroutine distf
!***********************************************************************
!                 DISTF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Distance to Final Plume Rise
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Arrays of Source Parameters
!                 Buoyancy and Momentum Fluxes
!                 Meteorological Variables for One Hour
!                 Wind Speed Adjusted to Stack Height
!
!        OUTPUTS: Distance to Final Plume Rise, XMAX (m), and Final
!                 Rise (DHFAER)
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: xln, delhnn, xmaxn
   double precision :: dhfsav       ! save original DHFAER for URBSTAB cases
   double precision :: bvzi2

!     Variable Initializations
   modnam = 'DISTF'

   if( stable  .or.  (unstab .and. (hs >= zi) ) )then

!        Compute the distance to final rise, XMAX;
!        The negative sign appears on the FB term to insure that the
!           resulting angle is between 0 and PI (i.e., positive)
      xmax   = up * datan2( fm*bvprim, -fb ) / bvprim

!        Compute the final stable plume rise, DHF, from Eqn. 3-113 of MFD
      dhfaer = 2.66d0 * (fb/(bvf*bvf*up))**third
      xln = fb/(up*ustar*ustar)
      delhnn = 1.2d0*xln**0.6d0 * (hsp + 1.2d0*xln)**0.4d0
      dhfaer = min( dhfaer, delhnn )

!        Compute Neutral/Unstable Final Rise
      if(fb <= 0.0d0) then
         xmaxn = 4.d0*ds*(vs+3.d0*up)*(vs+3.d0*up)/(vs*up)
         dhfaer   = min( dhfaer, 3.0d0 * ds * vs / up )
      else
         if(fb >= 55.0d0) then
            xmaxn = 119.0d0 * fb**0.4d0
         else
            xmaxn = 49.0d0 * fb**0.625d0
         end if
         call cblprd(xmaxn)
         dhfaer = min( dhfaer, dhp1 )
      end if

!        Apply calm, stable rise limit
      dhfaer = min( dhfaer, 4.0d0 * fb**0.25d0 / (bvf*bvf)**0.375d0 )

! ---    Save "original" DHFAER for URBSTAB cases
      dhfsav = dhfaer

!        For urban stable boundary layers, limit plume rise to 1.25*ZI - HSP
      if (urbstab) then
! ---       "New fomulation" for v15181 to account for "partial penetration" of plume
!           above the urban "stable" mixing height, similar to approach used for
!           convective conditions

! JAT 3/8/22: D009_TALL_STACK_HT_EQUAL_MIX_HT_JAT :
! Modify logic to only perform the calculations below when the stack
! is below the mixing height.  For cases where stack height is at or above mixing
! height, use the DHFAER as calculated above.  This is done to avoid NaN's for PPF
! and HEDHH. Also, HEDHH calculations are not appropriate for stacks at or above mixing
! height.  HEDHH is not calculated in the subroutine PENFCT and not used in DELTAH
! for stacks at or above mixing height
!            IF( HSP+DHFAER .GE. ZI )THEN
         if( hsp+dhfaer >= zi .and. hsp < zi )then
! ---          Stack height + plume rise is .GE. ZI; use pseudo-penetrated plume
!              approach for URBAN SBL cases

!              Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

            bvzi2 = (g / ptatzi) * 0.005d0

!              Compute the value of PsubS, Eq. 26b in the 2nd reference
            psubs = fb / ( up * bvzi2 * (zi-hsp)*(zi-hsp)*(zi-hsp) )

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

      if( fb <= 0.0d0 )then
         xmax = 4.d0*ds*(vs+3.0d0*up)*(vs+3.0d0*up)/(vs*up)
         dhfaer = 3.0d0 * ds * vs / up
      else
         if (fb >= 55.0d0) then
            xmax = 119.d0 * fb**0.4d0
         else
            xmax = 49.d0 * fb**0.625d0
         end if
         call cblprd(xmax)
         dhfaer = dhp1
      end if

   end if

   return
end subroutine distf

subroutine wakflg
!***********************************************************************
!                 WAKFLG Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: To Set Wake Flags for Building Downwash Algorithms
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To remove check on stack height >/= EPA formula
!                    height as a criterion for ignoring building
!                    downwash effects.  A one-time warning is issued
!                    for each source with HS >/= EPA formula height.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        INPUTS:  Building Dimensions
!                 Source Parameters
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Logical Flags for Wake Switch, WAKE;
!                 And Building
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'WAKFLG'

!     Select Building Dimensions for This Sector
   if (ifvsec <= nsec) then
      dsbh = adsbh(ifvsec,isrc)
      dsbw = adsbw(ifvsec,isrc)

! --- PRIME ---------------------------------
      dsbl = adsbl(ifvsec,isrc)
      xadj = adsxadj(ifvsec,isrc)
      yadj = adsyadj(ifvsec,isrc)
      b_subs = min( dsbh, dsbw )
      b_subl = max( dsbh, dsbw )
      b_subl = min( b_subl, 8.0d0*b_subs )
      rscale = b_subs**(2.0d0*third) * b_subl**third
! -------------------------------------------

   end if

!     Set Initial Wake Switches Based on Building Dimensions
   if (dsbh <= 1.0d-5 .or. dsbw <= 1.0d-5) then
! ---    No building inputs defined for this source, set WAKE = .F.
      wake   = .false.
   else
! ---    Building inputs defined for this source, set WAKE = .T.;
!        PRIME downwash algorithm will determine whether plume is
!        subject to downwash influences based on source/building
!        characteristics and meteorology
      wake   = .true.
! ---    Check for stack height greater the EPA formula height;
!        issue warning message once for each source that meets
!        this criterion

!CRT 3/5/2021 D067: Delete GEP stack height warning - causes confusion
!CRT This warning was added in v.11059 when WAKEFLG was disabled to
!CRT inform user that downwash would be applied even though stack height
!CRT was at or above GEP calculated for the wind direction of the current
!CRT hour based on sector specific building dimensions.  However, message
!CRT is confusing as it implies stack height is >= EPA formula GEP based
!CRT on building ht and max projected width, independent of wind direction.

!         IF (HS .GE. (DSBH + 1.5D0*MIN(DSBH,DSBW))) THEN
!            IF (.NOT. L_WakeMessage(ISRC)) THEN
!C              Write Warning Message:  Stack height > EPA formula ht
!               CALL ERRHDL(PATH,MODNAM,'W','305',SRCID(ISRC))
!               L_WakeMessage(ISRC) = .TRUE.
!            END IF
!         END IF
   end if

! --- PRIME ----------------------------------------------------

   return
end subroutine wakflg

subroutine xydist(indx)
!***********************************************************************
!                 XYDIST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sets Receptor Variables and Calculates Downwind (X)
!                 and Crosswind (Y) Distances,
!                 and Radial Distance from Source to Receptor (DISTR)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED BY R.W. Brode, PES, Inc. to use calling argument to
!                 specify array index, so that routine can be used by
!                 both the regular ISCST3 routines and the routines of
!                 the EVENT processor (ISCEV3). - 12/29/97
!
!        INPUTS:  Source Location
!                 Arrays of Receptor Locations
!                 SIN and COS of Wind Direction FROM Which Wind
!                 is Blowing, WDSIN and WDCOS
!
!        OUTPUTS: Values of X, Y, and DISTR (m)
!
!        CALLED FROM:   PCALC
!                       VCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: indx

!     Variable Initializations
   modnam = 'XYDIST'

!     Set Receptor Coordinates, Terrain Elevation and Flagpole Heights
   xr = axr(indx)
   yr = ayr(indx)
   zelev = azelev(indx)
   zhill = azhill(indx)
   zflag = azflag(indx)

!     Calculate Downwind (X) and Crosswind (Y) Distances
   x = -((xr-xs)*wdsin + (yr-ys)*wdcos)
   y =   (xr-xs)*wdcos - (yr-ys)*wdsin

!     Calculate Source-Receptor (Radial) Distance, DISTR
   distr = dsqrt(x*x + y*y)

!     Calculate height of receptor above stack base, ZRT
   if (l_flatsrc(isrc)) then
      zrt = zflag
   else
      zrt = zelev - zs + zflag
   end if

!     Check for SCREENing Mode and Set X,Y to Force Centerline Calc.
   if (screen) then
      x = distr
      y = 0.0d0
   end if

   return
end subroutine xydist

subroutine fterm
!***********************************************************************
!             FTERM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Calculate the Value of 'F' Which is Related to the
!                 Fraction of Plume Material Below HCrit
!
!        PROGRAMMER: Roger Brode, Jayant Hardikar
!
!        DATE:    September 30, 1993
!
!        INPUTS:  PHEE - Fraction of Plume Material Below HCrit
!
!        OUTPUTS: FOPT  - The 'F' Term
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'FTERM'

   fopt = 0.5d0 * (1.0d0 + phee)

   return
end subroutine fterm


subroutine fyplm(syarg,fyout)
!***********************************************************************
!             FYPLM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Calculate the Value of the Horizontal Gaussian
!                 Distribution Function for the Coherent Plume
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        INPUTS:
!                 SY   - Sigma-Y
!                 Y    - The Crosswind Distance of the Receptor from
!                        the Plume
!
!        OUTPUTS: 'FSUBY' Term
!
!        CALLED FROM:   AERCALC, PRMCALC, VOLCALC, ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: syarg, exparg, fyout

!     Variable Initializations
   modnam = 'FYPLM'

   exparg = -(y*y / (2.0d0*syarg*syarg))
!
!     Add meander component
!
   if (exparg > explim) then
!        Calculate lateral term for Gaussian plume
      fyout  = dexp(exparg)/(srt2pi*syarg)
   else
      fyout  = 0.0d0
   end if

   return
end subroutine fyplm


subroutine fypan(fyout)
!***********************************************************************
!             FYPAN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Calculate the Value of the Horizontal Gaussian
!                 Distribution Function for the Random ("Pancake")
!                 Component
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        INPUTS:
!                 DISTR - Real - Radial distance of receptor from the
!                                source (m)
!
!        OUTPUTS: 'FSUBY' Term
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: fyout

!     Variable Initializations
   modnam = 'FYPAN'

   fyout = 1.0d0/(twopi * distr)

   return
end subroutine fypan


subroutine meandr( uef, svef, fran )
!***********************************************************************
!             MEANDR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates fraction of random plume for lateral
!                 meander
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:       June 26, 2001
!
!        MODIFICATIONS:
!
!                    To use UEF instead of UMEAN in denominator of TTRAV
!                    term in calculation of SIGRAN.
!                    R.W. Brode, PES, Inc.  8/28/01
!
!                    To use radial distance (DISTR) in calculation of
!                    TTRAV instead of downwind distance (X).
!                    R.W. Brode, PES, Inc.  6/19/01
!
!        INPUTS:  Effective wind speed, UEF, in m/s
!                 Effective wind sigma_V, SVEF, in m/s
!
!        OUTPUTS: Fraction of plume in random lateral distribution, FRAN
!
!        CALLED FROM:   AERCALC
!                       PRMCALC
!                       VCALC
!
!        CALLS:         None
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
!RCO 9/28/2020 D061 User BIGT
!RCO BIGT is now defined as user input for LOW_WIND
!RCO default is set in coset. (CRT: Now declared in modules.f)
!      DOUBLE PRECISION, PARAMETER :: BIGT = 24.0D0
   double precision :: uef, svef, fran, umean, totkin, tran, ttrav,&
   &sigran
   double precision :: sqrtarg

!     Variable Initializations
   modnam = 'MEANDR'

!     Compute meander fraction of horizontal distribution function
!     from Venky's memo of 6/24/98.

! --- Calculate time scale (s) for random dispersion,
!     based value of BIGT (default = 24.0 hrs)
   tran = bigt * 3600.0d0

!     Remove the SVeff component from UEF
!
   sqrtarg = uef*uef - 2.0d0*svef*svef
   if (sqrtarg >= 0.01d0) then
      umean = dsqrt( sqrtarg )
   else
      umean = 0.1d0
   end if
   totkin = uef * uef
   ttrav  = distr/uef
   sigran = 2.0d0*svef*svef + umean*umean*(1.0d0-dexp(-ttrav/tran))
   fran   = sigran/totkin

!CRT  4/12/2022 D131 FRAN Alpha PBAL
   if (L_PBal) fran = dsqrt(fran)

! --- Issue informational messages for FRAN > FRANMAX
!CRT  5/1/2018: Add logical variable to condition so message is written
!CRT  only if FRANMAX is specified by user under LOW_WIND option
   if( L_UserFRANmax .and. (fran > franmax) )then
      write( dummy,'(I2.2,1X,I2.2,1X,I2.2,1X,I3)') imonth, iday,&
      &ihour,&
      &min(isrc,999)
      call errhdl(path,modnam,'I','494',dummy)
   endif

! --- Wood 3/18/2022 D127 Issue informational messages for FRAN < FRANMIN
   if( L_UserFRANmin .and. (fran < franmin) )then
      write( dummy,'(I2.2,1X,I2.2,1X,I2.2,1X,I3)') imonth, iday,&
      &ihour,&
      &min(isrc,999)
      call errhdl(path,modnam,'I','424',dummy)
   endif

!     Reset FRAN to min. of computed FRAN and FRANMAX (could be default
!     value or user-defined value)
!CRT  5/1/2018: moved outside of conditional statement above
   fran = min( fran, franmax )

!     3/18/22 Wood D127 - added FRANMIN keyword to LOW_WIND option
   fran = max( fran, franmin )

   if (debug) then
      write(dbgunt,*)
      write(dbgunt,*) 'SVEF, UEF, UMEAN:'
      write(dbgunt,*)  svef, uef, umean
      write(dbgunt,*)
      write(dbgunt,*) 'DISTR, TTRAV, TRAN:'
      write(dbgunt,*)  distr, ttrav, tran
      write(dbgunt,*)
      write(dbgunt,*) 'TOTKIN, SIGRAN, FRAN:'
      write(dbgunt,*)  totkin, sigran, fran
      write(dbgunt,*) ' '
   end if

   return
end subroutine meandr

subroutine critds (hearg)
!***********************************************************************
!             CRITDS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Compute the critical dividing streamline for each receptor
!                 (this routine is source dependent)
!
!        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        MODIFICATIONS:
!
!        MODIFIED:    Allowed RLINE/RLINEXT sources to use the RL_GRIDWS
!                     Wood, 7/5/22
!
!                    To redefine the upper limit on the integration for
!                    HCRIT as the minimum of the plume height above
!                    the receptor height, and the height scale input
!                    from AERMAP.
!                    R.W. Brode, PES, Inc.  9/4/01
!
!        INPUTS:  Plume Height, HEARG
!                 Gridded profile heights, GRIDHT
!                                 wind speed, GRIDWS
!                                 potential temperature, GRIDPT
!                                 potential temperature gradient, GRIDTG
!                 Hill height scale, ZHILL, input from AERMAP
!
!        OUTPUTS: Critical dividing streamline for the receptor
!
!        Assumptions:
!
!        References:  "User's Guide to the Complex Terrain Dispersion
!                      Model Plus ..."
!                     "Approach for Determining Hill Heights for AERMOD",
!                      A. Cimorelli, 6/25/93
!
!        CALLED FROM:
!***********************************************************************

!     Variable Declarations
   use main1
   use rline_data
   implicit none
   character :: modnam*12
   integer :: ndx4hc
   double precision :: hhill, uathh, ptathh, tgathh, pthc, tghc,&
   &topht, wstop
   double precision :: hearg

!     DEFINE LOCAL VARIABLES
   double precision :: a, ac4, b, b2, c, deter, hbot, htop,&
   &ls(mxglvl), rs(mxglvl), xn2(mxglvl), n2, dws,&
   &dws2, zmid
   integer :: ib, it, nl, nlev

!     Variable Initializations
   modnam = 'CRITDS'

!CRT  Initialize ZMID, 12/27/2017
   zmid =  0.0d0
   topht = 0.0d0
   tghc =  0.0d0
   pthc =  0.0d0


!     Compute the upper limit on the integration for HCRIT, called HHILL.
!     Set as minimum of plume height above receptor height, and height
!     scale above the receptor height (ZHILL) input from AERMAP.

   if (l_flatsrc(isrc)) then
! ---    This source is being modeled with FLAT terrain
!        Set HHILL and HCRIT to 0.0 and return
      hhill = 0.0d0
      hcrit = 0.0d0
      return
   else
! ---    Calculate 'effective' hill height for HCRIT calculation
      hhill = min( zhill - zs, zelev - zs + hearg )
   end if

   if ( stable .and. elev .and. hhill > 0.0d0 ) then
!        The hill elevation is above the source elevation and we are
!        using elevated terrain;

!        Determine the index of the gridded height immediately below
!        the hill height and determine the number of levels to use

      call locate ( gridht, 1, mxglvl, hhill, ndx4hc )
      nlev = ndx4hc + 1

!        Compute values at hill height

!........Added IF Statments for RLINE and RLINEXT source to use RL_GRIDWS - WOOD 6-28-2022
!         CALL GINTRP ( GRIDHT(NDX4HC), GRIDWS(NDX4HC),
!     &                  GRIDHT(NDX4HC+1), GRIDWS(NDX4HC+1),
!     &                  HHILL, UATHH )

      if ((srctyp(isrc) == 'RLINE') .or.&
      &(srctyp(isrc) == 'RLINEXT')) then
         call gintrp ( gridht(ndx4hc), rl_gridws(ndx4hc,i_alpha),&
         &gridht(ndx4hc+1), rl_gridws(ndx4hc+1,i_alpha),&
         &hhill, uathh )
      else
         call gintrp ( gridht(ndx4hc), gridws(ndx4hc),&
         &gridht(ndx4hc+1), gridws(ndx4hc+1),&
         &hhill, uathh )
      end if !End IF for RL_GRIDWS windspeed - Wood 7/5/2022

      call gintrp ( gridht(ndx4hc), gridpt(ndx4hc),&
      &gridht(ndx4hc+1), gridpt(ndx4hc+1),&
      &hhill, ptathh )
      call gintrp ( gridht(ndx4hc), gridtg(ndx4hc),&
      &gridht(ndx4hc+1), gridtg(ndx4hc+1),&
      &hhill, tgathh )

!        Compute the left side of Eq. 32 in the CTDMPLUS User's Guide
!        for all gridded levels; the actual number of levels to use is
!        determined later in the routine

      do nl = 1, nlev - 1
! - Wood 7/5/2022       LS(NL) = 0.5D0 * GRIDWS(NL) * GRIDWS(NL)

! begin - Use RLINE windspeeds if RLINE sourcetype - Wood 7/5/2022
         if ((srctyp(isrc) == 'RLINE') .or.&
         &(srctyp(isrc) == 'RLINEXT')) then
            ls(nl) = 0.5d0 * rl_gridws(nl,i_alpha) *&
            &rl_gridws(nl,i_alpha)
         else
            ls(nl) = 0.5d0 * gridws(nl) * gridws(nl)
         end if
! end -  Use RLINE windspeeds if RLINE sourcetype  - Wood 7/5/2022
      end do

!        Define LS at the hill top
      ls(nlev) = 0.5d0 * uathh * uathh

!        Compute the right-hand side (RHS) of Eq. 32 in the CTDMPLUS
!        User's Guide using the midpoint of each layer

      rs(nlev) = 0.0d0
      do nl = nlev-1, 1, -1

         if( nl < nlev-1 )then
            zmid = 0.5d0 * ( gridht(nl+1) + gridht(nl) )
            pthc = 0.5d0 * ( gridpt(nl+1) + gridpt(nl) )
            tghc = 0.5d0 * ( gridtg(nl+1) + gridtg(nl) )
            topht = gridht(nl+1)

         else if( nl == nlev-1 )then
            zmid = 0.5d0 * ( hhill + gridht(nl) )
            pthc = 0.5d0 * ( ptathh + gridpt(nl) )
            tghc = 0.5d0 * ( tgathh + gridtg(nl) )
            topht = hhill

         end if

!           Compute the Brunt-Vaisala frequency and then the RHS of Eq. 32

         xn2(nl) = (g / pthc) * tghc
         rs(nl)  = rs(nl+1) + xn2(nl) * ( (hhill - zmid) *&
         &( topht - gridht(nl) ) )

      end do

!        Find the layer(s) where Eq. 32 is satisfied; the lowest layer
!        is saved for the computation

      it = 1
      do nl = nlev, 1, -1
         if( ls(nl) >= rs(nl) )then
            it = nl
         end if
      end do

!        Interpolate to get the critical dividing streamline, HC,
!        assuming a linear change of variables within a layer;
!        the result is a quadratic equation for HC
!
!        DWS is wind speed shear; N2 is the Brunt-Vaisala frequency.
!
      if( it > 1 )then

         if( it == nlev )then
            wstop = uathh
            htop  = hhill
         else
! Wood 7/5/2022 WSTOP = GRIDWS(IT)
! Wood 7/5/2022 HTOP  = GRIDHT(IT)

! begin - Use RLINE wind speed if RLINE or RLINEXT source
            if ((srctyp(isrc) == 'RLINE') .or.&
            &(srctyp(isrc) == 'RLINEXT')) then
               wstop = rl_gridws(it,i_alpha)
               htop  = gridht(it)
            else
               wstop = gridws(it)
               htop  = gridht(it)
            end if
! end - Use RLINE wind speed if RLINE or RLINEXT source
         end if

         ib = it - 1
         hbot = gridht(ib)
! Wood 7/5/2022 DWS = (WSTOP - GRIDWS(IB)) / (HTOP - HBOT)

! begin - Use RLINE wind speed if RLINE or RLINEXT source
         if ((srctyp(isrc) == 'RLINE') .or.&
         &(srctyp(isrc) == 'RLINEXT')) then
            dws = (wstop - rl_gridws(ib, i_alpha)) / (htop - hbot)
         else
            dws = (wstop - gridws(ib)) / (htop - hbot)
         end if
! end - Use RLINE wind speed if RLINE or RLINEXT source

         dws2 = dws * dws
         n2 = xn2(ib)
!
!           Solve the quadratic eqn
!
         a = 0.5d0 * (n2 - dws2)
         b = (htop * dws2 - wstop * dws - n2 * hhill)
         c = (n2 * hhill * htop) - 0.5d0 * (n2 * htop * htop) -&
         &0.5d0 * (dws2 * htop * htop) + wstop * dws * htop -&
         &(ls(it) - rs(it))
         b2 = b * b
         ac4 = 4.0d0 * a * c
!crfl 6/19/96 Avoid sqrt (neg #) when near zero.
         if ((b2-ac4)/b2<0.0d0 .and. (b2-ac4)/b2>-0.001d0) then
            ac4 = b2
         end if
!crflendtest
         deter = dsqrt( b2 - ac4 )
         hcrit = (-b - deter) / (2.0d0 * a)
      else
         hcrit = 0.0d0
      end if

   else
!        The hill height is less than zero (i.e., the hill elevation is
!        less than stack base); set HCRIT = 0.0 for this receptor
      hcrit = 0.0d0

   end if

   return
end subroutine critds

subroutine pdf
!=======================================================================
!             PDF Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the parameters required by the CBL
!                probability density function
!
!   Input:
!
!
!   Output:
!
!   Assumptions:
!
!   Called by:   PCALC
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

!---- Data initializations

   modnam = 'PDF'

!---- Calculate the skewness, SKEW                         --- CALL SKCALC
   call skcalc

!---- Calculate the Lagrangian correlation function, R     --- CALL CRCALC
   call crcalc

!---- Calculate the parameter ALPHPD                       --- CALL ALCALC
   call alcalc

!---- Calculate the parameter BETAPD                       --- CALL BECALC
   call becalc

!---- Calculate the ratio of the mean updraft and downdraft velocities
!     to the standard deviation of the vertical velocity, ASUB1 and ASUB2,
!     respectively                                         --- CALL AACALC
   call aacalc

!---- Calculate the ratio of the turbulent energy in the updrafts
!     and downdrafts to the standard deviation of the total vertical
!     velocity, BSUB1 and BSUB2, respectively              --- CALL BBCALC
   call bbcalc

!---- Calculate the relative frequencies of updrafts and
!     downdrafts, LAMDA1 and LAMDA2, respectively          --- CALL LLCALC
   call llcalc

   return
end subroutine pdf

subroutine skcalc
!=======================================================================
!             SKCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the skewness of the vertical velocity
!
!   Input:       Height at which computation is made, HEIGHT
!                Convective scaling velocity, WSTAR
!RJP
!RJP             Change SWEFF to SWEFFD throughout
!RJP
!RJP             Effective sigma_W, SWEFF
!                Effective sigma_W, SWEFFD
!
!   Output:      Skewness, SKEW
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer(s):  Jim Paumier, PES, Inc.
!   Date:           September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

   double precision  :: wbar3

!---- Data initializations
!
   modnam = 'SKCALC'

!---- Define the mean of the third moment of vertical velocity
   if( surfac )then
!        This is a surface layer release
      wbar3 = 1.25d0 * (wstar**3) * (center/zi)

   else
!        The release is above the surface layer
      wbar3 = 0.125d0 * (wstar**3)

   end if

!---- Calculate the skewness
   skew = wbar3 / (sweffd*sweffd*sweffd)

   return
end subroutine skcalc

subroutine crcalc ( )
!=======================================================================
!             CRCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the lagrangian correlation coefficient
!
!   Input:       Convective scaling velocity, WSTAR
!                Surface friction velocity, USTAR
!
!   Output:      Correlation coefficient, R
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

!---- Data initializations
!
   modnam = 'CRCALC'

!---- Set value of R to 2.0

   r = 2.0d0

   return
end subroutine crcalc

subroutine alcalc ( )
!=======================================================================
!             ALCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the coefficient ALPHPD for the CBL PDF
!
!   Input:       Lagrangian correlation coefficient, R
!
!   Output:      Coefficient, ALPHPD
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

!---- Data initializations
!
   modnam = 'ALCALC'

!---- Calculate the coefficient ALPHPD

   alphpd = ( 1.0d0 + r*r ) / (1.0d0 + 3.0d0*r*r)

   return
end subroutine alcalc

subroutine becalc ( )
!=======================================================================
!             BECALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To calculate the coefficient BETAPD for the CBL PDF
!
!   Input:       Lagrangian correlation coefficient, R
!
!   Output:      Coefficient, BETAPD
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

!---- Data initializations
!
   modnam = 'BECALC'

!---- Calculate the coefficient BETAPD

   betapd = 1.0d0 + r*r

   return
end subroutine becalc

subroutine aacalc ( )
!=======================================================================
!             AACALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     Calculate the ratio of the mean updraft and downdraft
!                velocities to the standard deviation of the vertical
!                velocity
!
!   Input:       Skewness, SKEW
!                The coefficients ALPHPD and BETAPD
!
!   Output:      ASUB1 and ASUB2
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12
   double precision :: determ, swratio

!---- Data initializations
!
   modnam = 'AACALC'

!---- These two coefficients appear to be the solutions to a quadratic
!     equation.  Therefore, first compute the value of the determinant.

   determ = (alphpd*alphpd) * (skew*skew) + (4.0d0 / betapd)

!---- Compute square root of sigma-wc^2/wstar^2
   swratio = sweffd/wstar

!---- Calculate the coefficients ASUB1 and ASUB2

   asub1 = swratio * (0.5d0 * alphpd * skew + 0.5d0 * dsqrt(determ))
   asub2 = swratio * (0.5d0 * alphpd * skew - 0.5d0 * dsqrt(determ))

   return
end subroutine aacalc

subroutine bbcalc ( )
!=======================================================================
!             BBCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     Calculate the ratio of the turbulent energy in the
!                updrafts and downdrafts to the standard deviation of
!                the vertical velocity
!
!   Input:       The Lagrangian correlation, R
!                The coefficients ASUB1 and ASUB2
!
!   Output:      BSUB1 and BSUB2
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

!---- Data initializations
!
   modnam = 'BBCALC'

!---- Calculate the coefficients BSUB1 and BSUB2

   bsub1 =  r * asub1
   bsub2 = -r * asub2

   return
end subroutine bbcalc

subroutine llcalc ( )
!=======================================================================
!             LLCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     Calculate the relative frequencies of updrafts and
!                downdrafts
!
!   Input:       The coefficients ASUB1 and ASUB2
!
!   Output:      LAMDA1 and LAMDA2
!
!   Assumptions:
!
!   Called by:   PDF
!
!   Programmer:  Jim Paumier, PES, Inc.
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   References:  "Summary of Expressions for the CBL", J. Weil, 4/12/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12

!---- Data initializations
!
   modnam = 'LLCALC'

!---- Calculate the coefficients LAMDA1 and LAMDA2

   lamda1 = asub2 / (asub2 - asub1)
   lamda2 = 1.0d0 - lamda1

   return
end subroutine llcalc

subroutine decay (xarg)
!***********************************************************************
!                 DECAY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Decay Term for Use in Gaussian Plume Equation
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Downwind Distance, XARG (m)
!                 Stack Top Wind Speed, US (m/s)
!                 Decay Coefficient, DECOEFF (1/s)
!
!        OUTPUTS: Decay Term, D
!
!        CALLED FROM:   CHI
!                       DEP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     JAT DC1 is a temporary DCOEF variable
   double precision :: xarg,dc1

!     Variable Initializations
   modnam = 'DECAY'

   d = 1.0d0
   dc1=decoef !SET TEMPORARY DECAY HOLDER JAT

!      IF (DFAULT .and. URBAN .and. POLLUT.EQ.'SO2' .and.
!     &    URBSRC(ISRC).EQ.'Y') THEN !commented out 9/12/17 JAT
!     modified 9/29/17 JAT, use half-life for SO2 URBAN even without DFAULT
!     if HALFLIFE or DCAYCOEF used, use that value, not 4-hours
   if (urban .and. pollut=='SO2' .and. urbsrc(isrc)=='Y' .and.&
   &((icstat(7) == 0 .and. icstat(8) == 0) .or. dfault)) then !urban SO2 source
      decoef = 4.81d-5
   else if ((pollut=='SO2' .and. urbsrc(isrc)=='N') .or.&
   &dfault) then  !rural source for SO2 or default modified 10/12/17
      decoef = 0.0d0
!      ELSE IF (DFAULT) THEN !removed and moved to else if above JAT 10/12/17
!         DECOEF = 0.0D0
   end if

   if (decoef > 0.0d0) then
      if (stable .or. (unstab .and. hs>=zi)) then
         d = dexp (max (explim, -decoef*xarg/ueff))
      else
         d = dexp (max (explim, -decoef*xarg/ueffd))
      end if
   end if
   decoef=dc1 !RESET DECOEF TO ORIGINAL VALUE JAT

   return
end subroutine decay

subroutine vrtsbl (szarg, hearg, ziarg)
!***********************************************************************
!        VRTSBL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume
!                 Equation for Stable Conditions.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE and ZI for cases
!                 with receptors below stack base (ZR < 0) - 12/26/00
!
!        INPUTS:  Plume Height, HE
!                 Vertical Dispersion Parameter, SZ
!                 Mixing/Reflection Height, HSBL (= max(zi,he))
!                 Receptor Height, ZR
!
!        OUTPUTS: Vertical Term, FSUBZ
!
!        ASSUMPTIONS:   Vertical term for STABLE plumes includes
!                       multiple reflection terms.
!
!        REVISIONS:  Concentrations for receptors above HSBL forced
!                    to zero.  Change made 8/31/94 by R.F. Lee.
!
!        CALLED FROM:   WRAP, LIFT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   double precision :: szarg, hearg, ziarg, a1, a2, a3, a4, a5, a6,&
   &twoizi, sum, t, v
   double precision :: hetmp, zitmp

!     Variable Initializations
   modnam = 'VRTSBL'
   v = 0.0d0

   if (zr == 0.0d0) then
!        Vertical Term for Case With FLAT Terrain and No Flagpole
!        Receptor (ZR = 0.0D0)
      a1 = (-0.5d0/(szarg*szarg)) * hearg * hearg
      if (a1 > explim)  v = dexp(a1)
      sum = 0.0d0
      do i = 1, 100
         t  = 0.0d0
!           Use ZIARG (set in PCALC = max(HE,ZI)) instead of ZI.
         twoizi = 2.0d0*dble(i)*ziarg
         a2 = (-0.5d0/(szarg*szarg)) * (twoizi-hearg) *&
         &(twoizi-hearg)
         a3 = (-0.5d0/(szarg*szarg)) * (twoizi+hearg) *&
         &(twoizi+hearg)
         if (a2 > explim)  t = dexp(a2)
         if (a3 > explim)  t = t + dexp(a3)
         sum = sum + t

!RWB        Modify convergence criterion to use relative value of T
         if (dabs(t) <= 5.0d-7*dabs(sum)) then
!              Exit Loop
            exit
         end if
      end do
!        Calculate Total Vert. Term - (2.*) was Removed for Optimization
      v  = 2.0d0*(v + sum)

   else if (zr <= ziarg) then
!        Vertical Term for Case of ZR .NE. 0.0
!        First adjust for terrain below stack base with ZR < 0,
!        by keeping HE and ZI horizontal.
      hetmp = max( hearg, hearg - zr )
      zitmp = max( ziarg, ziarg - zr )

      a1 = (-0.5d0/(szarg*szarg)) * (zr-hetmp) * (zr-hetmp)
      a2 = (-0.5d0/(szarg*szarg)) * (zr+hetmp) * (zr+hetmp)
      if (a1 > explim)  v = dexp(a1)
      if (a2 > explim)  v = v + dexp(a2)
      sum = 0.0d0
      do i = 1, 100
         t  = 0.0d0
         twoizi = 2.0d0*dble(i)*zitmp
         a3 = (-0.5d0/(szarg*szarg)) * (zr-(twoizi-hetmp)) *&
         &(zr-(twoizi-hetmp))
         a4 = (-0.5d0/(szarg*szarg)) * (zr+(twoizi-hetmp)) *&
         &(zr+(twoizi-hetmp))
         a5 = (-0.5d0/(szarg*szarg)) * (zr-(twoizi+hetmp)) *&
         &(zr-(twoizi+hetmp))
         a6 = (-0.5d0/(szarg*szarg)) * (zr+(twoizi+hetmp)) *&
         &(zr+(twoizi+hetmp))
         if (a3 > explim)  t = t + dexp(a3)
         if (a4 > explim)  t = t + dexp(a4)
         if (a5 > explim)  t = t + dexp(a5)
         if (a6 > explim)  t = t + dexp(a6)
         sum = sum + t

!RWB        Modify convergence criterion to use relative value of T
         if (dabs(t) <= 1.0d-6*dabs(sum)) then
!              Exit Loop
            exit
         end if
      end do
      v  = v + sum
!CRFL
!CRFL  Add 'ELSE' to cover case where receptor is above HSBL, and
!CRFL  set V = 0 for that case.
   else
      v = 0.0d0
   end if

!     Calculate FSUBZ from V;  FSUBZ = V / (SQRT(2*PI) * SZARG)
   fsubz = v / (srt2pi*szarg)

   return
end subroutine vrtsbl

subroutine vrtsbn (szarg, hearg)
!***********************************************************************
!        VRTSBN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume
!                 Equation for Stable Conditions WITHOUT mixing lid.
!                 This subroutine is used for plumes above the CBL.
!
!        PROGRAMMER: Russ Lee, adapted from SUBROUTINE VRTSBL written
!                 by Roger Brode
!
!        DATE:    August 31, 1994
!
!        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE for cases
!                 with receptors below stack base (ZR < 0) - 12/26/00
!
!        INPUTS:  Plume Height, HE
!                 Vertical Dispersion Parameter, SZ
!                 Receptor Height, ZR
!
!        OUTPUTS: Vertical Term, FSUBZ
!
!        ASSUMPTIONS:   This routine for Vertical term for STABLE
!                       plumes does not include multiple reflection
!                       terms (used in stable layer above CBL).
!
!        CALLED FROM:   WRAP, LIFT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: szarg, hearg, a1, a2, v
   double precision :: hetmp

!     Variable Initializations
   modnam = 'VRTSBN'
   v = 0.0d0

   if (zr == 0.0d0) then
!        Vertical Term for Case With FLAT Terrain and No Flagpole
!        Receptor (ZR = 0.0)
      a1 = (-0.5d0/(szarg*szarg)) * hearg * hearg
      if (a1 > explim)  v = dexp(a1)
      v  = 2.d0 * v
   else
!        Vertical Term for Case of ZR .NE. 0.0
!        First adjust for terrain below stack base with ZR < 0,
!        by keeping HE and ZI horizontal.
      hetmp = max( hearg, hearg - zr )

      a1 = (-0.5d0/(szarg*szarg)) * (zr-hetmp) * (zr-hetmp)
      a2 = (-0.5d0/(szarg*szarg)) * (zr+hetmp) * (zr+hetmp)
      if (a1 > explim)  v = dexp(a1)
      if (a2 > explim)  v = v + dexp(a2)
   end if

!     Calculate FSUBZ from V;  FSUBZ = V / (SQRT(2*PI) * SZ)
   fsubz = v / (srt2pi*szarg)

   return
end subroutine vrtsbn


subroutine vrtcbl (he1, he2, sz1, sz2, fact)
!***********************************************************************
!        VRTCBL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Vertical Term for Use in Bi-Gaussian Plume
!                 Equation for Unstable (Convective) Conditions.
!                 Skewness of the plume is treated
!                 using two Gaussian plumes.  Revised from VRTCBL as
!                 programmed by Roger Brode, September 30, 1993.
!
!        PROGRAMMERS: Roger Brode, Russ Lee
!
!        DATE:    July 20, 1994
!
!        MODIFIED BY R.W. Brode, PES, Inc. to adjust HE and ZI for cases
!                 with receptors below stack base (ZR < 0) - 12/26/00
!
!        MODIFIED BY R.W. Brode, PES, Inc. to set vertical term to 0.0
!                 for cases when receptor is above mixing height - 1/22/98
!
!        INPUTS:  Plume 1 Height (arg), HE1
!                 Plume 2 Height (arg), HE2
!                 Vertical Dispersion Parameter (Plume 1), SZ1
!                 Vertical Dispersion Parameter (Plume 2), SZ2
!                 Factor to distinguish between direct
!                    and indirect plumes, FACT =  1.0 for Direct Plume
!                                         FACT = -1.0 for Indirect Plume
!                 Mixing Height, ZI
!                 Receptor Height, ZR
!
!        OUTPUTS: Vertical Term, FSUBZ
!
!        ASSUMPTIONS:   Vertical term for UNSTAB plumes includes
!                       one-half of reflection terms corresponding
!                       to the updraft portion of the plume.  Plume
!                       heights and sigma-z's are passed as arguments.
!
!        CALLED FROM:   WRAP, LIFT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   double precision :: he1, he2, sz1, sz2, fact, hearg1,&
   &hearg2, a1, a2, a3, a4, twoizi, sum, t1, t2, term, v
   double precision :: he1tmp, he2tmp, zitmp

!     Variable Initializations
   modnam = 'VRTCBL'
   v = 0.0d0

   if (dabs(zr-0.0d0) < 1.0d-10) then
!        Vertical Term for Case With FLAT Terrain and No Flagpole
!        Receptor (ZR = 0.0)
      sum = 0.0d0

      do i = 0, 1000
         t1 = 0.0d0
         t2 = 0.0d0
         twoizi = 2.0d0*dble(i)*zi * fact
!           Check for FACT < 0 and skip first term.
         if (fact < 0.0d0 .and. i == 0) cycle

         hearg1 = twoizi+he1
         hearg2 = twoizi+he2
         a1 = (-0.5d0/(sz1*sz1)) * (hearg1) * (hearg1)
         if (a1 > explim)  t1 = dexp(a1)
         a2 = (-0.5d0/(sz2*sz2)) * (hearg2) * (hearg2)
         if (a2 > explim)  t2 = dexp(a2)

!           Sum the Plume 1 and Plume 2 Portions
         term = (lamda1/sz1)*t1 + (lamda2/sz2)*t2
         sum = sum + term

!           Check for Convergence of Summation Term
         if (dabs(term) <= 5.0d-7*dabs(sum)) then
!              Exit Loop
            exit
         end if

      end do

!        Calculate Total Vert. Term - (2.*) was Removed for Optimization
      v  = 2.0d0* sum

   else if (zr <= zi) then
!        Vertical Term for Case of ZR .NE. 0.0
!        First adjust for terrain below stack base with ZR < 0,
!        by keeping HE and ZI horizontal.
      he1tmp = max( he1, he1 - zr )
      he2tmp = max( he2, he2 - zr )
      zitmp  = max( zi, zi - zr )

      sum = 0.0d0

      do i = 0, 1000
         t1 = 0.0d0
         t2 = 0.0d0
         twoizi = 2.0d0*dble(i)*zitmp * fact
!           Check for FACT < 0 and skip first term.
         if (fact < 0.0d0 .and. i == 0) cycle
!
!      Note:  The following code can be used for the indirect plume
!      as well as the direct plume, since HEn, for the indirect plume,
!      already contains ZI, and thus represents the first "reflection"
!      off the top of the mixed layer.
!
         hearg1 = twoizi+he1tmp
         hearg2 = twoizi+he2tmp
         a1 = (-0.5d0/(sz1*sz1)) * (zr-(hearg1)) *&
         &(zr-(hearg1))
         a2 = (-0.5d0/(sz1*sz1)) * (zr+(hearg1)) *&
         &(zr+(hearg1))
         if (a1 > explim)  t1 = dexp(a1)
         if (a2 > explim)  t1 = t1 + dexp(a2)
         a3 = (-0.5d0/(sz2*sz2)) * (zr-(hearg2)) *&
         &(zr-(hearg2))
         a4 = (-0.5d0/(sz2*sz2)) * (zr+(hearg2)) *&
         &(zr+(hearg2))
         if (a3 > explim)  t2 = dexp(a3)
         if (a4 > explim)  t2 = t2 + dexp(a4)

!           Sum the Plume 1 and Plume 2 Portions
         term = (lamda1/sz1)*t1 + (lamda2/sz2)*t2
         sum = sum + term

!           Check for Convergence of Summation Term
         if (dabs(term) <= 1.0d-6*dabs(sum)) then
!              Exit Loop
            exit
         end if

      end do

      v  = sum

   else
!        Receptor is above mixing height, set V=0.
      v = 0.0d0

   end if

!     Calculate FSUBZ from V;  FSUBZ = V / SQRT(2*PI)
!     (Note that 1/SZ term is included in V)
   fsubz = v / srt2pi

   return
end subroutine vrtcbl


subroutine pfract (hearg)
!***********************************************************************
!        PFRACT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Fraction of Plume Material Below HCRIT
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Plume Height, HEARG
!                 Vertical Dispersion Parameter, SZEFF
!                 Mixing/Reflection Height, HSBL (= max(zi,he))
!                 Critical Dividing Streamline Height, HCRIT
!
!        OUTPUTS: Fraction of plume below HCRIT, PHEE
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   double precision :: twoizi, hcint, hearg

   double precision :: a1, a2, a3, a4, a5, a6, b1, b2, b3, b4, b5, b6,&
   &t, sum, erfx

!     Variable Initializations
   modnam = 'PFRACT'
   phee = 0.0d0

   if (stable .and. (hcrit > 0.0d0)) then

!        Define HCINT = MIN( HSBL, HCRIT) as the limit of the integral,
!        where HSBL = MAX( HE, ZI).
      hcint = min( hsbl, hcrit)

!        Calculate Terms Corresponding to n=0.
      a1 = (hcint-hearg)/(rtof2 * sz)
      a2 = (hcint+hearg)/(rtof2 * sz)
      b1 = erfx (a1)
      b2 = erfx (a2)

!        Calculate Summation Term.
      sum = 0.0d0
      do i = 1, 100
         t  = 0.0d0
!           Use HSBL (set in PCALC = max(HE,ZI)) instead of ZI.
         twoizi = 2.0d0*dble(i)*hsbl
         a3 = (hcint-hearg+twoizi)/(rtof2 * sz)
         a4 = (hcint+hearg+twoizi)/(rtof2 * sz)
         a5 = (hcint-hearg-twoizi)/(rtof2 * sz)
         a6 = (hcint+hearg-twoizi)/(rtof2 * sz)
         b3 = erfx (a3)
         b4 = erfx (a4)
         b5 = erfx (a5)
         b6 = erfx (a6)

         t = b3 + b4 + b5 + b6
         sum = sum + t

!           Check for convergence of summation term
         if (dabs(t) <= 1.0d-6*dabs(sum)) then
! ---          Set lower limit of 5 on number of iterations
            if( i >= 5 )then
!                 Exit Loop
               exit
            endif
         end if

      end do

      phee = 0.5d0 * (b1 + b2 + sum)

!        Check for PHEE > 1.01 and Set = 1.0
!        (this patch may need to be changed).
      if (phee > 1.01d0 .and. .not. L_SkipMessages)  then
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'I','405',dummy)
         phee = 1.0d0
      end if

   end if

   return
end subroutine pfract

function erfx(arg)
!***********************************************************************
!        ERFX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Error Function, Using Method Documented
!                 on Page 187 of "Approximations for Digital Computers"
!                 by Cecil Hastings, Princeton University Press, 1955
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Error Function Argument, ARG
!
!        OUTPUTS: Error Function Value, ERFX
!
!        CALLED FROM:   PFRACT
!***********************************************************************

!     Variable Declarations
   implicit none
   character :: modnam*12

   double precision :: arg, x, erfx

!     Variable Initializations
   modnam = 'ERFX'

   if (arg > 4.0d0) then
      erfx = 1.0d0
   else if (arg < -4.0d0) then
      erfx = -1.0d0
   else if (dabs(arg) < 1.0d-10) then
      erfx = 0.0d0
   else
      x = dabs(arg)
      erfx = 1.d0 - 1.d0/(1.d0+x*(0.705230784d-1+x*(0.422820123d-1+x*&
      &(0.92705272d-2+x*(0.1520143d-3+x*(0.2765672d-3+x*&
      &0.430638d-4))))))**16.0d0
      if (arg < 0.0d0)  erfx = -erfx
   end if

   return
end function erfx

subroutine sumval
!***********************************************************************
!                 SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!
!        INPUTS:  HRVAL - Hourly Value for (IREC,ISRC) Combination
!                 Averaging Period Options
!                 Source Groupings
!
!        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!                       RLCALC
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SUMVAL'

!     Begin LOOP Over Output Types
   do ityp = 1, numtyp
      if (hrval(ityp) /= 0.0d0) then
!           Check for Source Belonging to Group
         if (igroup(isrc,igrp) == 1) then
!              Begin Averaging Period LOOP
            do iave = 1, numave
               aveval(irec,igrp,iave,ityp) = hrval(ityp) +&
               &aveval(irec,igrp,iave,ityp)
            end do
!              End Averaging Period LOOP
            if (period .or. annual) then
               annval(irec,igrp,ityp) = hrval(ityp) +&
               &annval(irec,igrp,ityp)
            end if
            if (iseahr(igrp) == 1) then
               shvals(irec,igrp,iseas,ihour,ityp) = hrval(ityp) +&
               &shvals(irec,igrp,iseas,ihour,ityp)
            end if
         end if
      end if
   end do
!     End LOOP Over Output Types

   return
end subroutine sumval

subroutine sumback
!***********************************************************************
!                 SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums Background Values to AVEVAL and ANNVAL Arrays
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:
!
!        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
!
!        CALLED FROM:   PCALC
!                       VCALC
!                       ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   double precision :: bckgrd
   character :: modnam*12

!     Variable Initializations
   modnam = 'SUMBACK'
   bckgrd = 0.0d0

!     Begin LOOP Over Output Types
!     JAT 6/25/19 ADDED FROM 18081
!     TWO MODIFICATIONS
!     1.  Multiply BGCONC by ratio of EMIFAC/1.0E6 to account for the
!         that background concentrations are in
!         micrograms/m^3 for internal calculations
!         but modeled output may be in other units
!         such as ppb.  This division puts the
!         background in the same units as the modeled
!         concentrations before adding them.
!     2.  Only add background to the concentration output
!         type, i.e. ityp=1.  Background concentration (ug/^3)
!         should not be added to deposition (g/m^2)
   ityp=1
!      DO ITYP = 1, NUMTYP  !JAT comment out do loop since ITYP only = 1
   if (grp_back(igrp)) then
! ---       Include background for this source group
      bckgrd = bgconc*emifac(ityp)/1.0d6
      backave(igrp) = backave(igrp) + bgconc*emifac(ityp)/1.0d6
      if (period .or. annual) then
         backann(igrp) = backann(igrp) + bgconc*emifac(ityp)/1.0d6
      end if
      if (iseahr(igrp) == 1) then
         backseashr(igrp,iseas,ihour) =&
         &backseashr(igrp,iseas,ihour) + bgconc*emifac(ityp)/1.0d6
      end if
   else
! ---       Do not include background for this source group
      bckgrd = 0.0d0
   end if
!        Begin Averaging Period LOOP
   do iave = 1, numave
      aveval(1:numrec,igrp,iave,ityp) = bckgrd +&
      &aveval(1:numrec,igrp,iave,ityp)
   end do
!        End Averaging Period LOOP
   if (period .or. annual) then
      annval(1:numrec,igrp,ityp) = bckgrd +&
      &annval(1:numrec,igrp,ityp)
   end if
   if (iseahr(igrp) == 1) then
      shvals(1:numrec,igrp,iseas,ihour,ityp) = bckgrd +&
      &shvals(1:numrec,igrp,iseas,ihour,ityp)
   end if
!      END DO !JAT comment out do loop since ITYP only = 1
!     End LOOP Over Output Types

   return
end subroutine sumback

subroutine sumback_no2
!***********************************************************************
!                 SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums Background Values to BACKAVE, BACKANN, and
!                 BACKSEASHR arrays, and to AVEVAL, ANNVAL, and SHVALS
!                 Arrays for the current receptor for the ARM2,
!                 OLM, PVMRM and GRSM options for modeling NO2
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    May 14, 2014
!
!        INPUTS:
!
!        OUTPUTS: Updated Sums of BACKAVE, BACKANN, BACKSEASHR, AVEVAL,
!                 ANNVAL, and SHVALS Arrays, as applicable
!
!        CALLED FROM:   ARM2_CALC, OLM_CALC, PVMRM_CALC and GRSM_CALC
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SUMBACK_NO2'

!     Begin LOOP Over Output Types
!     RCO 7/27/20 ADDED FROM 19191
!     TWO MODIFICATIONS
!     1.  Multiply BGCONC by ratio of EMIFAC/1.0E6 to account for the
!         that background concentrations are in
!         micrograms/m^3 for internal calculations
!         but modeled output may be in other units
!         such as ppb.  This division puts the
!         background in the same units as the modeled
!         concentrations before adding them.
!     2.  Only add background to the concentration output
!         type, i.e. ityp=1.  Background concentration (ug/^3)
!         should not be added to deposition (g/m^2)
   ityp=1
!      DO ITYP = 1, NUMTYP  !RCO comment out do loop since ITYP only = 1
   if (grp_back(igrp) .and. bgconc > 0.0d0) then
! ---       Include background for this source group
      backave(igrp) = backave(igrp) + bgconc*emifac(ityp)/1.0d6
      if (period .or. annual) then
         backann(igrp) = backann(igrp) +&
         &bgconc*emifac(ityp)/1.0d6
      end if
      if (iseahr(igrp) == 1) then
         backseashr(igrp,iseas,ihour) =&
         &backseashr(igrp,iseas,ihour) +&
         &bgconc*emifac(ityp)/1.0d6
      end if

!           Begin Averaging Period LOOP
      do iave = 1, numave
         aveval(irec,igrp,iave,ityp) =&
         &bgconc*emifac(ityp)/1.0d6 +&
         &aveval(irec,igrp,iave,ityp)
      end do
!           End Averaging Period LOOP
      if (period .or. annual) then
         annval(irec,igrp,ityp) = bgconc*emifac(ityp)/1.0d6 +&
         &annval(irec,igrp,ityp)
      end if
      if (iseahr(igrp) == 1) then
         shvals(irec,igrp,iseas,ihour,ityp) =&
         &bgconc*emifac(ityp)/1.0d6 +&
         &shvals(irec,igrp,iseas,ihour,ityp)
      end if
   end if
!      END DO !JAT comment out do loop since ITYP only = 1
!     End LOOP Over Output Types

   return
end subroutine sumback_no2

subroutine sumvalpsd(srcs2use)
!***********************************************************************
!                 SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sums HRVAL to ANNVAL Arrays for PSD credit and
!                 increment consumption
!
!                 There are only two source groups to consider:
!                 NAAQS and increment expanding
!
!        PROGRAMMER: J Paumier
!
!        DATE:    September 30, 2006
!
!        INPUTS:  HRVAL - Hourly Value for (IREC,ISRC) Combination
!                 Averaging Period Options
!
!        OUTPUTS: Updated Sums of ANNVAL Arrays
!
!        CALLED FROM:   PVMRM_CALC
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12, srcs2use*7

!     Variable Initializations
   modnam = 'SUMVALPSD'

!     Begin LOOP Over Output Types
   do ityp = 1, numtyp

!        Begin Averaging Period LOOP
      do iave = 1, numave

         if( trim(srcs2use) == 'NAAQSRC' )then
!              NAAQS group: assign to (A+B) group 1
            aveval(irec,1,iave,ityp) = abval(irec,ityp) +&
            &aveval(irec,1,iave,ityp)

         else if( trim(srcs2use) == 'ALLBASE' )then
!              PSDINC group: assign (A+B)-(B+C) to group 2
            aveval(irec,2,iave,ityp) = (abval(irec,ityp) -&
            &bcval(irec,ityp)) +&
            &aveval(irec,2,iave,ityp)
         end if

      end do

!        Check for ANNUAL or PERIOD Averaging
      if (period .or. annual) then

         if( trim(srcs2use) == 'NAAQSRC' )then
!              NAAQS group: assign to (A+B) group 1
            annval(irec,1,ityp) = abval(irec,ityp) +&
            &annval(irec,1,ityp)

         else if( trim(srcs2use) == 'ALLBASE' )then
!              PSDINC group: assign (A+B)-(B+C) to group 2
            annval(irec,2,ityp) = (abval(irec,ityp) -&
            &bcval(irec,ityp)) +&
            &annval(irec,2,ityp)
         end if

      end if

   end do
!     End LOOP Over Output Types

   return
end subroutine sumvalpsd

subroutine aver
!***********************************************************************
!                 AVER Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Short Term (<=24 hr) Average Concentrations
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Averaging Time Option Switches
!                 Updated Array of Cumulative Values, AVEVAL
!
!        OUTPUTS: Updated Array of Averages, AVEVAL
!
!        CALLED FROM: HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: snum

!     Variable Initializations
   modnam = 'AVER'
   path = 'CN'  !D081 CRT 5/13/2024

   if (kave(iave) /= 1) then
!        Calculate Denominator Considering Calms and Missing,
!        Skipping Averaging if Averaging Period is 1-Hour
      snum = max(dble(numhrs(iave)-numclm(iave)-nummsg(iave)),&
      &dnint(dble(numhrs(iave))*0.75d0+0.4d0))
!        D081 - Add warning message - less than 18 hours used for 24-hr avg (WSP, 4/2023))
      if(dble(numhrs(iave)-numclm(iave)-nummsg(iave)) < 18&
      &.and. kave(iave) == 24) then
         write(dummy,'(I10)') fulldate
         call errhdl(path,modnam,'W','732',dummy)
!        D081 - Add warning message - less than 6 hours used for 8-hr avg (CRT, 5/1/2023)
      elseif (dble(numhrs(iave)-numclm(iave)-nummsg(iave)) < 6&
      &.and. kave(iave) == 8) then
         write(dummy,'(I10)') fulldate
         call errhdl(path,modnam,'W','733',dummy)
!        D081 - Add warning message - less than 3 hours used for 3-hr avg (CRT, 5/1/2023)
      elseif (dble(numhrs(iave)-numclm(iave)-nummsg(iave)) < 3&
      &.and. kave(iave) == 3) then
         write(dummy,'(I10)') fulldate
         call errhdl(path,modnam,'W','734',dummy)
      end if
      aveval(1:numrec,1:numgrp,iave,1) =&
      &aveval(1:numrec,1:numgrp,iave,1) / snum
   end if

   return
end subroutine aver

subroutine hivals
!***********************************************************************
!                 HIVALS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Updates High Value Tables
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To change subroutine name MAXVAL to MAXVALUE to
!                    avoid conflicts with intrinsic function MAXVAL under
!                    Fortran 90.  R. Brode, PES, 12/29/97
!
!        INPUTS:  High Value Option Switches
!                 Array of CONC or DEPOS Averages
!
!        OUTPUTS: Updated High Value Arrays
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'HIVALS'

!     Check for High/Max Value Options - Skip Update If KAVE=1,
!     And No CALCS Were Made for the Current Hour
   if (calcs .or. kave(iave)/=1) then
      if (inhi(iave) == 1) then
         do ityp = 1, numtyp
!              Update High Values for Each Receptor            ---   CALL NHIGH
            call nhigh
         end do
      end if
      if (maxave(iave) == 1) then
         do ityp = 1, numtyp
!              Update Maximum Value Table for KAVE             ---   CALL MAXVALUE
            call maxvalue
         end do
      end if
   end if
!     Reset Counters for This Averaging Period
   numhrs(iave) = 0
   numclm(iave) = 0
   nummsg(iave) = 0

   return
end subroutine hivals

subroutine nhigh
!***********************************************************************
!                 NHIGH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Highest Value by Receptor Arrays
!                 Note: For duplicate values, the earlier occurrence keeps its
!                       rank within the array
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  High Value Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Highest Value Array
!                 Updated Highest Date Array
!
!        CALLED FROM:   HIVALS
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j

!     Variable Initializations
   modnam = 'NHIGH'

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec
         if (nhival > 1) then
            if (aveval(irec,igrp,iave,ityp) >&
            &hivalu(irec,nhival,igrp,iave,ityp)) then
               do j = nhival-1, 1, -1
                  if (aveval(irec,igrp,iave,ityp) <=&
                  &hivalu(irec,j,igrp,iave,ityp)) then
                     hivalu(irec,j+1,igrp,iave,ityp) =&
                     &aveval(irec,igrp,iave,ityp)
                     if (numclm(iave)==0 .and.&
                     &nummsg(iave)==0) then
                        hclmsg(irec,j+1,igrp,iave,ityp) = ' '
                     else
!                          Set Indicator Of Calm and Msg    ---   CALL HSETFG
                        call hsetfg(0,j)
                     end if
                     nhidat(irec,j+1,igrp,iave,ityp) = kurdat
!                       Exit Block
                     cycle receptor_loop
                  else
                     hivalu(irec,j+1,igrp,iave,ityp) =&
                     &hivalu(irec,j,igrp,iave,ityp)
                     hclmsg(irec,j+1,igrp,iave,ityp) =&
                     &hclmsg(irec,j,igrp,iave,ityp)
                     nhidat(irec,j+1,igrp,iave,ityp) =&
                     &nhidat(irec,j,igrp,iave,ityp)
                     if (j == 1) then
                        hivalu(irec,1,igrp,iave,ityp) =&
                        &aveval(irec,igrp,iave,ityp)
                        if (numclm(iave)==0 .and.&
                        &nummsg(iave)==0) then
                           hclmsg(irec,1,igrp,iave,ityp) = ' '
                        else
!                             Set Indicator Of Calm and Msg ---   CALL HSETFG
                           call hsetfg(1,1)
                        end if
                        nhidat(irec,1,igrp,iave,ityp) = kurdat
                     end if
                  end if
               end do
            end if
         else if (nhival == 1) then
            if (aveval(irec,igrp,iave,ityp) >&
            &hivalu(irec,1,igrp,iave,ityp)) then
               hivalu(irec,1,igrp,iave,ityp) = aveval(irec,igrp,iave,ityp)
               if (numclm(iave)==0 .and.&
               &nummsg(iave)==0) then
                  hclmsg(irec,1,igrp,iave,ityp) = ' '
               else
!                    Set Indicator Of Calm and Missing      ---   CALL HSETFG
                  call hsetfg(1,1)
               end if
               nhidat(irec,1,igrp,iave,ityp) = kurdat
            end if
         end if
      end do receptor_loop
!        End Receptor LOOP
   end do
!     End Source Group LOOP

   return
end subroutine nhigh

subroutine hsetfg(indt,j)
!***********************************************************************
!                 HSETFG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Set Calm and Missing Flag Of the Result
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To correct error in order of indices for array
!                    HCLMSG on first assignment to 'b' - 9/29/92
!
!        INPUTS:  High Value Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Highest Value Flag Array
!
!        CALLED FROM:   NHIGH
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j, indt

!     Variable Initializations
   modnam = 'HSETFG'

   if (indt == 0) then
!        Set Indicator Of Calm and Missing
      if (numclm(iave)/=0 .and.&
      &nummsg(iave)==0) then
         hclmsg(irec,j+1,igrp,iave,ityp) = 'c'
      else if (numclm(iave)==0 .and.&
      &nummsg(iave)/=0) then
         hclmsg(irec,j+1,igrp,iave,ityp) = 'm'
      else if (numclm(iave)/=0 .and.&
      &nummsg(iave)/=0) then
         hclmsg(irec,j+1,igrp,iave,ityp) = 'b'
      end if
   else if (indt == 1) then
!        Set Indicator Of Calm and Missing
      if (numclm(iave)/=0 .and.&
      &nummsg(iave)==0) then
         hclmsg(irec,1,igrp,iave,ityp) = 'c'
      else if (numclm(iave)==0 .and.&
      &nummsg(iave)/=0) then
         hclmsg(irec,1,igrp,iave,ityp) = 'm'
      else if (numclm(iave)/=0 .and.&
      &nummsg(iave)/=0) then
         hclmsg(irec,1,igrp,iave,ityp) = 'b'
      end if
   end if

   return
end subroutine hsetfg

subroutine maxvalue
!***********************************************************************
!                 MAXVALUE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Overall Maximum Value Arrays
!                 NMAX = 50 Assigned in PARAMETER Statement
!                 Note: For duplicate values, the earlier occurrence keeps
!                       its rank within the array
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Maximum Value Table Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value Array
!                 Updated Maximum Date Array
!                 Updated Maximum Receptor Array
!
!        CALLED FROM:   HIVALS
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j

!     Variable Initializations
   modnam = 'MAXVALUE'

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec
         if (nmxval > 1) then
            if (aveval(irec,igrp,iave,ityp) >&
            &rmxval(nmxval,igrp,iave,ityp)) then
               do j = nmxval-1, 1, -1
                  if(aveval(irec,igrp,iave,ityp) <=&
                  &rmxval(j,igrp,iave,ityp)) then
                     rmxval(j+1,igrp,iave,ityp) = aveval(irec,igrp,iave,ityp)
                     if (numclm(iave)==0 .and.&
                     &nummsg(iave)==0) then
                        mclmsg(j+1,igrp,iave,ityp) = ' '
                     else
!                          Set Indicator Of Calm and Msg    ---   CALL MSETFG
                        call msetfg(0,j)
                     end if
                     mxdate(j+1,igrp,iave,ityp) = kurdat
                     mxloca(j+1,igrp,iave,ityp) = irec
!                       Exit Block
                     cycle receptor_loop
                  else
                     rmxval(j+1,igrp,iave,ityp) = rmxval(j,igrp,iave,ityp)
                     mxdate(j+1,igrp,iave,ityp) = mxdate(j,igrp,iave,ityp)
                     mclmsg(j+1,igrp,iave,ityp) = mclmsg(j,igrp,iave,ityp)
                     mxloca(j+1,igrp,iave,ityp) = mxloca(j,igrp,iave,ityp)
                     if (j == 1) then
                        rmxval(1,igrp,iave,ityp) = aveval(irec,igrp,iave,ityp)
                        if (numclm(iave)==0 .and.&
                        &nummsg(iave)==0) then
                           mclmsg(1,igrp,iave,ityp) = ' '
                        else
!                             Set Indicator Of Calm and Msg ---   CALL MSETFG
                           call msetfg(1,1)
                        end if
                        mxdate(1,igrp,iave,ityp) = kurdat
                        mxloca(1,igrp,iave,ityp) = irec
                     end if
                  end if
               end do
            end if
         else if (nmxval == 1) then
            if (aveval(irec,igrp,iave,ityp) >&
            &rmxval(1,igrp,iave,ityp)) then
               rmxval(1,igrp,iave,ityp) = aveval(irec,igrp,iave,ityp)
               if (numclm(iave)==0 .and.&
               &nummsg(iave)==0) then
                  mclmsg(1,igrp,iave,ityp) = ' '
               else
!                    Set Indicator Of Calm and Missing      ---   CALL MSETFG
                  call msetfg(1,1)
               end if
               mxdate(1,igrp,iave,ityp) = kurdat
               mxloca(1,igrp,iave,ityp) = irec
            end if
         end if
      end do receptor_loop
!        End Receptor LOOP
   end do
!     End Source Group LOOP

   return
end subroutine maxvalue

subroutine msetfg(indt,j)
!***********************************************************************
!                 MSETFG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Set Calm and Missing Flag Of the Max Result
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Maximum Value Table Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value Flag Array
!
!        CALLED FROM:   MAXVALUE
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: j, indt

!     Variable Initializations
   modnam = 'MSETFG'

   if (indt == 0) then
!        Set Indicator Of Calm and Missing
      if (numclm(iave)/=0 .and.&
      &nummsg(iave)==0) then
         mclmsg(j+1,igrp,iave,ityp) = 'c'
      else if (numclm(iave)==0 .and.&
      &nummsg(iave)/=0) then
         mclmsg(j+1,igrp,iave,ityp) = 'm'
      else if (numclm(iave)/=0 .and.&
      &nummsg(iave)/=0) then
         mclmsg(j+1,igrp,iave,ityp) = 'b'
      end if
   else if (indt == 1) then
!        Set Indicator Of Calm and Missing
      if (numclm(iave)/=0 .and.&
      &nummsg(iave)==0) then
         mclmsg(1,igrp,iave,ityp) = 'c'
      else if (numclm(iave)==0 .and.&
      &nummsg(iave)/=0) then
         mclmsg(1,igrp,iave,ityp) = 'm'
      else if (numclm(iave)/=0 .and.&
      &nummsg(iave)/=0) then
         mclmsg(1,igrp,iave,ityp) = 'b'
      end if
   end if

   return
end subroutine msetfg

subroutine maxfil
!***********************************************************************
!                 MAXFIL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update Maximum Value File (>Threshold)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Moved check for RSTSAV (SAVEFILE option) outside
!                    the receptor loop, and replaced 'read to end' loop
!                    with POSITION='APPEND' in OPEN statement for
!                    Fortran 90 version.
!                    R.W. Brode, PES, Inc.,  6/23/98
!
!        INPUTS:  Maximum File Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated Maximum Value File
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'MAXFIL'

!     Check for High/Max Value Options - Skip Update If KAVE=1,
!     And No CALCS Were Made for the Current Hour
   if (calcs .or. kave(iave)/=1) then
!        Begin Source Group LOOP
      do igrp = 1, numgrp
!           Check for MAXIFILE Option for This IGRP,IAVE Combination
         if (maxfle(igrp,iave) == 1) then
!              Begin Receptor LOOP
            do irec = 1, numrec
!                 For the Values Over Threshold
               if (aveval(irec,igrp,iave,1) >=&
               &thresh(igrp,iave)) then
                  write(imxunt(igrp,iave),thrfrm,err=99) kave(iave),&
                  &grpid(igrp), kurdat, axr(irec), ayr(irec),&
                  &azelev(irec), azhill(irec), azflag(irec),&
                  &aveval(irec,igrp,iave,1)
               end if
            end do
!              End Receptor LOOP
            if (rstsav) then
!                 Saving Intermediate Results to File for Later Re-start
!                 Close MAXIFILE and Reposition to End
               close (imxunt(igrp,iave))
               open(imxunt(igrp,iave),file=thrfil(igrp,iave),&
               &position='APPEND')
            end if
         end if
      end do
!        End Source Group LOOP
   end if

   go to 999

!     WRITE Error Message for Problem Writing to Maximum Value File
99 write(dummy,'("MAXFL",I3.3)') imxunt(igrp,iave)
   call errhdl(path,modnam,'E','520',dummy)
   runerr = .true.

999 return
end subroutine maxfil

subroutine postfl
!***********************************************************************
!                 POSTFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Write Concurrent Values to File for Postprocessing
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Replaced 'read to end' loop with POSITION='APPEND'
!                    in OPEN statements for Fortran 90 version with
!                    RSTSAV (SAVEFILE option).
!                    R.W. Brode, PES, Inc.,  6/23/98
!
!        INPUTS:  Postprocessing File Options
!                 Array of CONC or DEPOS Averages
!
!        OUTPUTS: Postprocessor Files
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'POSTFL'

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Check for POSTFILE Option for This IGRP,IAVE Combination
      if (ipstfl(igrp,iave) == 1) then
         if (ipsfrm(igrp,iave) == 0) then
!              WRITE Results to Unformatted POSTFILE
            write(ipsunt(igrp,iave),err=99) kurdat, kave(iave),&
            &grpid(igrp), ((aveval(irec,igrp,iave,ityp),&
            &irec=1,numrec),ityp=1,numtyp)
            if (rstsav) then
!                 Saving Intermediate Results to File for Later Re-start
!                 Close POSTFILE and Reposition to End
               close (ipsunt(igrp,iave))
               open(ipsunt(igrp,iave),file=pstfil(igrp,iave),&
               &form='UNFORMATTED',position='APPEND')
            end if
         else
!              WRITE Results to Formatted Plot File
!              Begin Receptor LOOP
            do irec = 1, numrec
               write(ipsunt(igrp,iave),pstfrm,err=99)&
               &axr(irec), ayr(irec), (aveval(irec,igrp,iave,ityp),&
               &ityp=1,numtyp),&
               &azelev(irec), azhill(irec), azflag(irec),&
               &chrave(iave), grpid(igrp), kurdat, netid(irec)
            end do
!              End Receptor LOOP
            if (rstsav) then
!                 Saving Intermediate Results to File for Later Re-start
!                 Close POSTFILE and Reposition to End
               close (ipsunt(igrp,iave))
               open(ipsunt(igrp,iave),file=pstfil(igrp,iave),&
               &form='FORMATTED',position='APPEND')
            end if
         end if
      end if
   end do
!     End Source Group LOOP

   go to 999

!     WRITE Error Message for Problem Writing to Postprocessor File
99 write(dummy,'("PSTFL",I3.3)') ipsunt(igrp,iave)
   call errhdl(path,modnam,'E','520',dummy)
   runerr = .true.

999 return
end subroutine postfl

subroutine mxdlyfl
!***********************************************************************
!                 MXDLYFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    Update daily maximum value arrays, and write values to
!                    file for the MAXDAILY output option if applicable.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED:   Included IF-THEN block to account for cases with
!                    NHIMXDLY = 1, i.e., 1st-highest rank only.
!                    Previous version resulted in all values being 0.0
!                    if only the 1st-highest rank was selected for
!                    applications involving the special processing
!                    for daily maximum values (24hr PM25, 1hr NO2 and
!                    1hr SO2).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011
!
!        INPUTS:     MAXDAILY file options
!                    Array of maximum daily 1-hour values
!
!        OUTPUTS:
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none

   integer :: j
   character :: modnam*12

!     Variable Initializations
   modnam = 'MXDLYFL'

!     Begin Source Group LOOP
   do igrp = 1, numgrp
      receptor_loop: do irec = 1, numrec
         ityp = 1
!           Check for MAXDAILY Option for This IGRP
         if (imxdly(igrp) == 1) then
!              WRITE Results to MAXDAILY file for this day
            write(imdunt(igrp),mxdfrm,err=99) axr(irec),&
            &ayr(irec), mxdval(irec,igrp), azelev(irec),&
            &azhill(irec), azflag(irec),'  1-HR',&
            &grpid(igrp), jday, imxdhr(irec,igrp),&
            &(kurdat/100) * 100 + imxdhr(irec,igrp),&
            &netid(irec)
         end if
! ---       Update arrays of highest MAXDAILY values
         if (nhimxdly > 1) then
            if (mxdval(irec,igrp) >&
            &himxdly(irec,igrp,nhimxdly)) then
               do j = nhimxdly-1, 1, -1
                  if (mxdval(irec,igrp) <=&
                  &himxdly(irec,igrp,j)) then
                     himxdly(irec,igrp,j+1) =&
                     &mxdval(irec,igrp)
                     nhidatmxd(irec,igrp,j+1) =&
                     &(kurdat/100) * 100 + imxdhr(irec,igrp)
!                       Exit Block
                     cycle receptor_loop
                  else
                     himxdly(irec,igrp,j+1) =&
                     &himxdly(irec,igrp,j)
                     nhidatmxd(irec,igrp,j+1) =&
                     &nhidatmxd(irec,igrp,j)
                     if (j == 1) then
                        himxdly(irec,igrp,1) =&
                        &mxdval(irec,igrp)
                        nhidatmxd(irec,igrp,1) =&
                        &(kurdat/100) * 100 + imxdhr(irec,igrp)
                     end if
                  end if
               end do
            end if
         else if (nhimxdly == 1) then
            if (mxdval(irec,igrp) >&
            &himxdly(irec,igrp,1)) then
               himxdly(irec,igrp,1) = mxdval(irec,igrp)
               nhidatmxd(irec,igrp,1) = (kurdat/100) * 100 +&
               &imxdhr(irec,igrp)
            end if
         end if
      end do receptor_loop
!        End Receptor LOOP
      if (rstsav .and. imxdly(igrp) == 1) then
!           Saving Intermediate Results to File for Later Re-start
!           Close MAXDAILY file and Reposition to End
         close (imdunt(igrp))
         open(imdunt(igrp),file=maxdly(igrp),&
         &form='FORMATTED',position='APPEND')
      end if
   end do
!     End Source Group LOOP

! --- Reinitialize MXDVAL array
   mxdval = 0.0d0
   imxdhr = 0

   go to 999

!     WRITE Error Message for Problem Writing to Postprocessor File
99 write(dummy,'("MXDLY",I3.3)') imdunt(igrp)
   call errhdl(path,modnam,'E','520',dummy)
   runerr = .true.

999 return
end subroutine mxdlyfl

subroutine toxxfl
!***********************************************************************
!                 TOXXFL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Update TOXXFILE Buffers, and Write Out if Full
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 29, 1992
!
!        INPUTS:  TOXXFILE Options
!                 Array of CONC or DEPOS Averages
!                 Averaging Period
!
!        OUTPUTS: Updated TOXXFILE Buffers and File
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ig, icode
   double precision :: cutoff

!     Variable Initializations
   modnam = 'TOXXFL'

!     Check for TOXXFILE Option - Skip Update If KAVE=1,
!     And No CALCS Were Made for the Current Hour
   if (itoxfl(iave)==1 .and. (calcs .or. kave(iave)/=1)) then
!        Convert TOXXFILE Threshold to User Units
      cutoff = toxthr(iave) * emifac(1)

!        Begin Receptor LOOP
      do irec = 1, numrec

!           Begin Source Group LOOP
         do igrp = 1, numgrp

!              For the Values Over Threshold (in user units), Fill Buffers
            if (aveval(irec,igrp,iave,1) >= cutoff) then
               do ig = 1, numgrp
!                    Loop Through Groups and Write Values to Buffer
                  ipair = ipair + 1
                  icode = 100000*iline + 1000*ig + irec
                  idconc(iave,ipair) = icode
!                    Convert CONC Values Back to Units of g/s
                  txconc(iave,ipair)=aveval(irec,ig,iave,1)/emifac(1)
                  if (ipair == npair) then
!                       Write Out Full Buffers and Reset Counter
                     write(itxunt(iave),err=99) (idconc(iave,i),&
                     &i=1,npair)
                     write(itxunt(iave),err=99) (txconc(iave,i),&
                     &i=1,npair)
                     ipair = 0
                  end if
               end do
!                 Exit Source Group LOOP
               exit
            end if

         end do
!           End Source Group LOOP

      end do
!        End Receptor LOOP
   end if

   go to 999

!     WRITE Error Message for Problem Writing to TOXXFILE
99 write(dummy,'("TOXFL",I3.3)') itxunt(iave)
   call errhdl(path,modnam,'E','520',dummy)
   runerr = .true.

999 return
end subroutine toxxfl

subroutine prtday
!***********************************************************************
!                 PRTDAY Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Write Concurrent Values to Printed Output File
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To remove obsolete reference to BOUNDARY receptors.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
!                    to better accommodate UTM coordinates - 9/29/92
!
!        INPUTS:  Postprocessing File Options
!                 Array of CONC or DEPOS Averages
!
!        OUTPUTS: Postprocessor Files
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, ii, nx, ny, indz, indc, indexw
   double precision :: ycoval, xrms, yrms, dist, dir
   character :: buf132*132

!     Variable Initializations
   modnam = 'PRTDAY'
   buf132 = ' '
   indz   = 0

!     Begin Source Group LOOP
   do igrp = 1, numgrp

!        Fill Work Array With SRCIDs For This Group
      if (.not. psdcredit) then
!           Fill Work Array With SRCIDs For This Group
         indgrp = 0
         do isrc = 1, numsrc
            if (igroup(isrc,igrp) == 1) then
               indgrp = indgrp + 1
               workid(indgrp) = srcid(isrc)
            end if
         end do
      else
!           Assign 'N/A' for source IDs for PSDCREDIT option
         indgrp = 1
         workid(indgrp) = 'N/A'
      end if

! ---    Check for BACKGRND "source" being included
!        in source group
      if (grp_back(igrp)) then
         indgrp = indgrp + 1
         workid(indgrp) = 'BACKGROUND'
!           Check for More Than 29 Sources Per Group
         indexw = min(29,nsrc+1)
      else
         indexw = min(29,nsrc)
      end if
!        Check for More Than 29 Sources Per Group
      if (indgrp > indexw) then
         workid(indexw) = ' . . . '
         indgrp = indexw
      end if

!        Print Results for Receptor Networks
!        Set Number of Columns Per Page, NCPP
      ncpp = 9
!        Set Number of Rows Per Page, NRPP
      nrpp = 40
!        Begin LOOP Through Networks
      do i = 1, innet
!           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
         nppx = 1 + int((numxpt(i)-1)/ncpp)
         nppy = 1 + int((numypt(i)-1)/nrpp)
         do nx = 1, nppx
            do ny = 1, nppy
               call header(iounit)
               write(iounit,9032) chrave(iave), (chidep(ii,ityp),&
               &ii=1,6),&
               &ihour,jday,iyr,grpid(igrp),(workid(k),k = 1,indgrp)
               write(iounit,9037) ntid(i), nttyp(i)
!                 Print The Value By Groups
               write(iounit,9011) chidep(3,ityp), pollut,outlbl(ityp)
               if (nx == nppx) then
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &numxpt(i))
                  end if
               else
                  if (nttyp(i) == 'GRIDCART') then
                     write(iounit,9016)
                     write(iounit,9017) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  else if (nttyp(i) == 'GRIDPOLR') then
                     write(iounit,9018)
                     write(iounit,9019) (xcoord(j,i),j=1+ncpp*(nx-1),&
                     &ncpp*nx)
                  end if
               end if
               write(iounit,9010)
               if (ny == nppy) then
                  do k = 1+nrpp*(ny-1), numypt(i)
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(aveval(indz+j-1,igrp,iave,ityp),j=1+ncpp*(nx-1),&
                        &numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(aveval(indz+j-1,igrp,iave,ityp),j=1+ncpp*(nx-1),&
                        &ncpp*nx)
                     end if
                  end do
               else
                  do k = 1+nrpp*(ny-1), nrpp*ny
                     if (nttyp(i) == 'GRIDCART') then
                        indz = netend(i) - k*numxpt(i) + 1
                        ycoval = ycoord(numypt(i)-k+1,i)
                     else if (nttyp(i) == 'GRIDPOLR') then
                        indz = netsta(i) + (k-1)*numxpt(i)
                        ycoval = ycoord(k,i)
                     end if
                     if (nx == nppx) then
                        write(iounit,9013) ycoval,&
                        &(aveval(indz+j-1,igrp,iave,ityp),j=1+ncpp*(nx-1),&
                        &numxpt(i))
                     else
                        write(iounit,9013) ycoval,&
                        &(aveval(indz+j-1,igrp,iave,ityp),j=1+ncpp*(nx-1),&
                        &ncpp*nx)
                     end if
                  end do
               end if
            end do
         end do
      end do
!        End LOOP Through Networks

      if (irstat(4)/=0 .or. irstat(8)/=0) then
!RWB        Include EVALCART receptors with DISCCART receptors.  2/14/95
!           Print Out The Coord. & Concentrations For Discrete Cart Receptors
         indc = 0
         do irec = 1, numrec
            if (rectyp(irec) == 'DC') then
               indc = indc + 1
               if (mod(indc-1,80) == 0) then
                  call header(iounit)
                  write(iounit,9032) chrave(iave),(chidep(ii,ityp),&
                  &ii=1,6),ihour,jday,iyr,grpid(igrp),(workid(k),k=1,indgrp)
                  write(iounit,9043)
                  write(iounit,9011) chidep(3,ityp), pollut,&
                  &outlbl(ityp)
                  write(iounit,9048) chidep(3,ityp), chidep(3,ityp)
               end if
               if (mod(indc,2) /= 0) then
                  write(buf132(1:60),9045) axr(irec),ayr(irec),&
                  &aveval(irec,igrp,iave,ityp)
               else
                  write(buf132(61:120),9045) axr(irec),&
                  &ayr(irec), aveval(irec,igrp,iave,ityp)
                  write(iounit,9090) buf132
                  write(buf132,9095)
               end if
            end if
         end do
         if (mod(indc,2) /= 0) then
            write(iounit,9090) buf132
            write(buf132,9095)
         end if
      end if

      if (irstat(5) /= 0) then
!           Print Out The Coord. & Concentrations For Discrete Polar Receptors
         indc = 0
         do irec = 1, numrec
            if (rectyp(irec) == 'DP') then
               indc = indc + 1
               xrms = axr(irec) - axs(iref(irec))
               yrms = ayr(irec) - ays(iref(irec))
               dist = dsqrt(xrms*xrms + yrms*yrms)
               dir  = datan2(xrms, yrms) * rtodeg
               if (dir <= 0.0d0) dir = dir + 360.0d0
               if (mod(indc-1,80) == 0) then
                  call header(iounit)
                  write(iounit,9032) chrave(iave), (chidep(ii,ityp),&
                  &ii=1,6),ihour,jday,iyr,grpid(igrp),(workid(k),k=1,indgrp)
                  write(iounit,9044)
                  write(iounit,9011) chidep(3,ityp), pollut,&
                  &outlbl(ityp)
                  write(iounit,9049) chidep(3,ityp), chidep(3,ityp)
               end if
               if (mod(indc,2) /= 0) then
                  write(buf132(1:65),9047) srcid(iref(irec)),&
                  &dist, dir, aveval(irec,igrp,iave,ityp)
               else
                  write(buf132(66:130),9047) srcid(iref(irec)),&
                  &dist, dir, aveval(irec,igrp,iave,ityp)
                  write(iounit,9090) buf132
                  write(buf132,9095)
               end if
            end if
         end do
         if (mod(indc,2) /= 0) then
            write(iounit,9090) buf132
            write(buf132,9095)
         end if
      end if

   end do
!     End Source Group LOOP

9011 format(/40x,'** ',a4,' OF ',a8,' IN ',a40,' **'/)
9010 format(66(' -')/)
9013 format(2x,f10.2,1x,'|',1x,9(f13.5))
9016 format(3x,' Y-COORD  |',48x,'X-COORD (METERS)')
9017 format(3x,' (METERS) |',1x,9(1x,f12.2,:))
9018 format(3x,'DIRECTION |',48x,'DISTANCE (METERS)')
9019 format(3x,'(DEGREES) |',1x,9(1x,f12.2,:))
9032 format(20x,'*** CONCURRENT ',a5,1x,6a4,'VALUES',&
   &' ENDING WITH HOUR ',i2,' FOR DAY ',i3,' OF ',i4,' ***'&
   &/24x,'FOR SOURCE GROUP:',1x,a8,&
   &/24x,'INCLUDING SOURCE(S):     ',5(a12,', ',:),&
   &/17x,8(a12,', ',:)/17x,8(a12,', ',:)/17x,8(a12,', ',:))
9037 format(/35x,'*** NETWORK ID: ',a8,' ;  NETWORK TYPE: ',&
   &a8,' ***')
9043 format(/45x,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
9044 format(/47x,'*** DISCRETE POLAR RECEPTOR POINTS ***')
9045 format(6x,2(f12.2,2x),f13.5)
9047 format(2x,a12,': ',f12.2,2x,f10.2,2x,f13.5)
9048 format(6x,' X-COORD (M)   Y-COORD (M)        ',a4,&
   &22x,' X-COORD (M)   Y-COORD (M)        ',a4,/65(' -'))
9049 format(5x,'ORIGIN',59x,'ORIGIN',&
   &/5x,' SRCID         DIST (M)   DIR (DEG)        ',a4,&
   &18x,' SRCID         DIST (M)   DIR (DEG)        ',a4,&
   &/65(' -'))
9090 format(a132)
9095 format(132(' '))

   return
end subroutine prtday

subroutine rsdump
!***********************************************************************
!                 RSDUMP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Save Intermediate Results Arrays for Later Restart
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance.  Specifically, to output the
!                    10-digit date variable (FULLDATE) with 4-digit
!                    year for date comparisons.
!                    Also modified to output arrays associated with
!                    post-1997 PM10 processing.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        MODIFIED:   Changed parameter for specifying the number of
!                    high annual/period averages from NVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        INPUTS:  Current Date Variable
!                 Array Limits
!                 Results Arrays
!
!        OUTPUTS: Unformatted File of Intermediate Results
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k, l, m

!     Variable Initializations
   modnam = 'RSDUMP'
   ndump = ndump + 1

!     Check for Monthly Averages and Only Dump at End of Month
   if (month .and. .not.endmon)  go to 1000

   if (savfil == savfl2 .or. mod(ndump,2) /= 0) then
      open(unit=idpunt,err=99,file=savfil,form='UNFORMATTED',&
      &iostat=ioerrn,status='REPLACE')
      write(idpunt) fulldate, ntothrs
      write(idpunt) nhival, nmxval, numrec, numgrp, numave, numtyp

      if (nhival > 0) then
         write(idpunt) (((((hivalu(i,j,k,l,m),i=1,numrec),j=1,nhival),&
         &k=1,numgrp),l=1,numave),m=1,numtyp)
         write(idpunt) (((((nhidat(i,j,k,l,m),i=1,numrec),j=1,nhival),&
         &k=1,numgrp),l=1,numave),m=1,numtyp)
         write(idpunt) (((((hclmsg(i,j,k,l,m),i=1,numrec),j=1,nhival),&
         &k=1,numgrp),l=1,numave),m=1,numtyp)

! ---       Include arrays associated with multi-year processing of high
!           ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
         if (pm25ave .or. no2ave .or. so2ave) then
            write(idpunt) numyrs
            write(idpunt) (((sumhnh(i,j,k),i=1,numrec),j=1,numgrp),&
            &k=1,nhival)
            write(idpunt) (((himxdly(i,j,k),i=1,numrec),j=1,numgrp),&
            &k=1,nhival)
            write(idpunt)(((nhidatmxd(i,j,k),i=1,numrec),j=1,numgrp),&
            &k=1,nhival)
            write(idpunt) ((((himxdly_byyr(i,j,k,l),i=1,numrec),&
            &j=1,numgrp),k=1,nhival),l=1,numyrs)
            write(idpunt) ((((nhidatmxd_byyr(i,j,k,l),i=1,numrec),&
            &j=1,numgrp),k=1,nhival),l=1,numyrs)
         end if

      end if

      if (nmxval > 0) then
         write(idpunt) ((((rmxval(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
         write(idpunt) ((((mxdate(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
         write(idpunt) ((((mxloca(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
         write(idpunt) ((((mclmsg(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
      end if

      if (seasonhr) then
         write(idpunt) (((((shvals(i,j,k,l,m),i=1,numrec),&
         &j=1,numgrp),k=1,4),l=1,24),m=1,numtyp)
         write(idpunt) ((nseahr(i,j),i=1,4),j=1,24)
         write(idpunt) ((nseacm(i,j),i=1,4),j=1,24)
      end if

      if (period) then
         write(idpunt) ianhrs, ianclm, ianmsg, numyrs
         write(idpunt) (((annval(i,j,k),i=1,numrec),j=1,numgrp),&
         &k=1,numtyp)
         if (multyr) then
            write(idpunt) (((amxval(i,j,k),i=1,nhiann),j=1,numgrp),&
            &k=1,numtyp)
            write(idpunt) (((imxloc(i,j,k),i=1,nhiann),j=1,numgrp),&
            &k=1,numtyp)
         end if
      else if (annual) then
         write(idpunt) ianhrs, ianclm, ianmsg, numyrs
         write(idpunt) (((annval(i,j,k),i=1,numrec),j=1,numgrp),&
         &k=1,numtyp)
         write(idpunt) (((sumann(i,j,k),i=1,numrec),j=1,numgrp),&
         &k=1,numtyp)
      end if

      close (idpunt)

   else
      open(unit=idpun2,err=99,file=savfl2,form='UNFORMATTED',&
      &iostat=ioerrn,status='REPLACE')
      write(idpun2) fulldate, ntothrs
      write(idpun2) nhival, nmxval, numrec, numgrp, numave, numtyp

      if (nhival > 0) then
         write(idpun2) (((((hivalu(i,j,k,l,m),i=1,numrec),j=1,nhival),&
         &k=1,numgrp),l=1,numave),m=1,numtyp)
         write(idpun2) (((((nhidat(i,j,k,l,m),i=1,numrec),j=1,nhival),&
         &k=1,numgrp),l=1,numave),m=1,numtyp)
         write(idpun2) (((((hclmsg(i,j,k,l,m),i=1,numrec),j=1,nhival),&
         &k=1,numgrp),l=1,numave),m=1,numtyp)

! ---       Include arrays associated with multi-year processing of high
!           ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
         if (pm25ave .or. no2ave .or. so2ave) then
            write(idpun2) numyrs
            write(idpun2) (((sumhnh(i,j,k),i=1,numrec),j=1,numgrp),&
            &k=1,nhival)
            write(idpun2) (((himxdly(i,j,k),i=1,numrec),j=1,numgrp),&
            &k=1,nhival)
            write(idpun2)(((nhidatmxd(i,j,k),i=1,numrec),j=1,numgrp),&
            &k=1,nhival)
            write(idpun2) ((((himxdly_byyr(i,j,k,l),i=1,numrec),&
            &j=1,numgrp),k=1,nhival),l=1,numyrs)
            write(idpun2) ((((nhidatmxd_byyr(i,j,k,l),i=1,numrec),&
            &j=1,numgrp),k=1,nhival),l=1,numyrs)
         end if

      end if

      if (nmxval > 0) then
         write(idpun2) ((((rmxval(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
         write(idpun2) ((((mxdate(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
         write(idpun2) ((((mxloca(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
         write(idpun2) ((((mclmsg(i,j,k,l),i=1,nmxval),j=1,numgrp),&
         &k=1,numave),l=1,numtyp)
      end if

      if (seasonhr) then
         write(idpun2) (((((shvals(i,j,k,l,m),i=1,numrec),&
         &j=1,numgrp),k=1,4),l=1,24),m=1,numtyp)
         write(idpun2) ((nseahr(i,j),i=1,4),j=1,24)
         write(idpun2) ((nseacm(i,j),i=1,4),j=1,24)
      end if

      if (period) then
         write(idpun2) ianhrs, ianclm, ianmsg, numyrs
         write(idpun2) (((annval(i,j,k),i=1,numrec),j=1,numgrp),&
         &k=1,numtyp)
         if (multyr) then
            write(idpun2) (((amxval(i,j,k),i=1,nhiann),j=1,numgrp),&
            &k=1,numtyp)
            write(idpun2) (((imxloc(i,j,k),i=1,nhiann),j=1,numgrp),&
            &k=1,numtyp)
         end if
      else if (annual) then
         write(idpun2) ianhrs, ianclm, ianmsg, numyrs
         write(idpun2) (((annval(i,j,k),i=1,numrec),j=1,numgrp),&
         &k=1,numtyp)
         write(idpun2) (((sumann(i,j,k),i=1,numrec),j=1,numgrp),&
         &k=1,numtyp)
      end if

      close (idpun2)

   end if

   go to 1000

99 call errhdl(path,modnam,'E','500','SAVEFILE')
   runerr = .true.

1000 return
end subroutine rsdump


subroutine evlini
!***********************************************************************
!                 EVLINI Module of AERMOD
!
!        PURPOSE: Initialize ARC Values for EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        MODIFIED:   To include initializations for SZMAX(:) and
!                    HSBLMX(:) arrays.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        REVISIONS:  Added ARCCL() variable for true centerline
!                    calculations.  Changed 7/25/94, R.F. Lee.
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'EVLINI'

   arcmax(:) = 0.0d0
!CRFL
!CRFL  Add true centerline calculations:  add ARCCL(I)
!CRFL  Changed 7/25/94, R.F. Lee
!CRFL
   arccl(:)  = 0.0d0
   qmax(:)   = 0.0d0
   dxmax(:)  = 0.0d0
   umax(:)   = 0.0d0
   u3max(:)  = 0.0d0
   svmax(:)  = 0.0d0
   swmax(:)  = 0.0d0
   symax(:)  = 0.0d0
   sy3mx(:)  = 0.0d0
   szmax(:)  = 0.0d0
   hemax(:)  = 0.0d0
   chidmw(:) = 0.0d0
   chinmw(:) = 0.0d0
   chi3mw(:) = 0.0d0
   chidml(:) = 0.0d0
   chinml(:) = 0.0d0
   chi3ml(:) = 0.0d0
   hsblmx(:) = 0.0d0

   return
end subroutine evlini

subroutine evalck
!***********************************************************************
!                 EVALCK Module of AERMOD
!
!        PURPOSE: Check ARC Values for EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        REVISIONS:  Added true centerline calculations.
!                    Changed 7/25/94, R.F. Lee.
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: index
   double precision :: chioq

!     Variable Initializations
   modnam = 'EVALCK'

!     Set ARC Index
   index = ndxarc(irec)

!     Check for INDEX = 0, i.e., this receptor is not an EVALCART receptor.
!     Skip to RETURN for INDEX = 0
   if (index == 0) go to 99

!CRFL
!CRFL  Add true centerline calculations:  add CHIOQC
!CRFL  Change made 7/25/94, R.F. Lee.
!CRFL
!     Calculate Normalized Concentration, CHI/Q
   chioq = hrval(1)/(qtk*emifac(1))
!     Check ARCMAX Array
   if (chioq > arcmax(index)) then
      arcmax(index) = chioq
!CRFL
!CRFL  Add true centerline calculations:  add arc centerline
!CRFL  calculation ARCCL(INDEX).  Note that, although ARCCL is
!CRFL  is calculated redundantly for all receptors in the arc,
!CRFL  the value calculated at the receptor showing the max is
!CRFL  used.  This assures that the most reasonable downwind
!CRFL  distance will be used in the calculation.
!CRFL  Changed 7/25/94, R.F. Lee.
!CRFL
      arccl(index) = chioq
      qmax(index)  = qtk * emifac(1)
      dxmax(index) = distr
!RJP
!RJP     Use appropriate effective parameters
!RJP
      if( stable .or. (unstab .and. (hs >= zi) ) )  then
         umax(index)   = ueff
         svmax(index)  = sveff
         swmax(index)  = sweff
         symax(index)  = sy
         hemax(index)  = he
!crfl 5/19/95 Grab SZ at maximum receptor in arc
         szmax(index)  = sz
         chidmw(index) = 0.0d0
         chinmw(index) = 0.0d0
         chi3mw(index) = 0.0d0
         chidml(index) = 0.0d0
         chinml(index) = 0.0d0
         chi3ml(index) = 0.0d0
         hsblmx(index) = hsbl
      else if (ppf > 0.999d0) then
         umax(index)   = ueffd
         u3max(index)  = ueff3
         svmax(index)  = sveff3
         swmax(index)  = sweff3
         symax(index)  = sy
         sy3mx(index)  = sy3
         hemax(index)  = hsp + dhp3
         chidmw(index) = chidw/qmax(index)
         chinmw(index) = chinw/qmax(index)
         chi3mw(index) = chi3w/qmax(index)
         chidml(index) = chidl/qmax(index)
         chinml(index) = chinl/qmax(index)
         chi3ml(index) = chi3l/qmax(index)
         hsblmx(index) = hpen
      else
         umax(index)   = ueffd
         u3max(index)  = ueff3
         svmax(index)  = sveffd
         swmax(index)  = sweffd
         symax(index)  = sy
         sy3mx(index)  = sy3
         hemax(index)  = hsp + dhp1
         chidmw(index) = chidw/qmax(index)
         chinmw(index) = chinw/qmax(index)
         chi3mw(index) = chi3w/qmax(index)
         chidml(index) = chidl/qmax(index)
         chinml(index) = chinl/qmax(index)
         chi3ml(index) = chi3l/qmax(index)
         hsblmx(index) = hpen
      end if
   end if

99 return
end subroutine evalck

subroutine evalfl
!***********************************************************************
!                 EVALFL Module of AERMOD
!
!        PURPOSE: Output ARC Values for EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        REVISIONS:  Added true centerline calculations.
!                    Changed 7/25/94, R.F. Lee.
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
!     JAT 7/22/21 D065 UOUST AND SVOU NOT USED
!      DOUBLE PRECISION :: CWIC, CWICN, CWICW, CWICL, UOUST, SVOU, HEOZI,
   double precision :: cwic, cwicn, cwicw, cwicl, heozi,&
!     JAT 7/22/21 D065 ZIOL AND UOWST NOT USED
!     &        ZIOL, FSTAR, UOWST, XNDIM, PWSTAR, UOUT, SYOUT, OBUOUT
   &fstar, xndim, pwstar, uout, syout, obuout
   integer :: i

!     Variable Initializations
   modnam = 'EVALFL'

!     LOOP Through ARCs
   do i = 1, numarc
!C
!C   Changes dated 2/25/94, 3/2/94, 3/4/94, 3/8/94, 3/9/94, 3/14/94,
!C     and 4/20/94
!C     by Russ Lee, to add Bowen Ratio and additional parameters.
!C
!        Calculate Crosswind Integrated Concentration, CWIC
!CRFL
!CRFL  "ARCMAX" was changed to ARCCL in the following statement to
!CRFL  give a "true" CWIC.  Changed 7/25/94, R.F. Lee.
!CRFL
!RWB         CWIC = SRT2PI * SYMAX(I) * ARCCL(I)
!RWB     Modify CWIC to be sum of CWIC's of individual "plumes".  2/13/95
!RWB     Note that WRAP and LIFT components are included in ARCCL.
      if( stable .or. (unstab .and. hs>=zi) )then
         cwic  = srt2pi * symax(i) * arccl(i)
!           Now calculate CWIC with U*ZI normalization,
!           using maximum of HE & ZI, instead of ZI.
         cwicn = cwic * umax(i) * max( hemax(i), zi)

      else
!           Calculate WRAP and LIFT components of CWIC
!           First calculate CWIC without U*ZI normalization.
!           Note that the CHIDM_, CHINM_ and CHI3M_ terms have already been
!           normalized by QTK.
         cwicw = srt2pi * symax(i) * (chidmw(i)+chinmw(i)) +&
         &srt2pi * sy3mx(i) *  chi3mw(i)

         cwicl = srt2pi * symax(i) * (chidml(i)+chinml(i)) +&
         &srt2pi * sy3mx(i) *  chi3ml(i)
!           Combine WRAP and LIFT components. Include decay and normalization.
         cwic  = (fopt * cwicw + (1.0d0-fopt) * cwicl) * d

!           Calculate WRAP and LIFT components of CWIC
!           Now calculate CWIC with U*ZI normalization.
!           Use HPEN (=MAX(HE3,ZI)) for penetrated source instead of ZI.
         cwicw = srt2pi * symax(i)*umax(i)*zi*(chidmw(i)+chinmw(i)) +&
         &srt2pi * sy3mx(i)*u3max(i)*hpen*chi3mw(i)

         cwicl = srt2pi * symax(i)*umax(i)*zi*(chidml(i)+chinml(i)) +&
         &srt2pi * sy3mx(i)*u3max(i)*hpen*chi3ml(i)
!           Combine WRAP and LIFT components. Include decay and normalization.
         cwicn = (fopt * cwicw + (1.0d0-fopt) * cwicl) * d

      end if

!        Calculate U/Ustar
!     JAT 7/22/21 D065 UOUST AND SVOU NOT USED
!         IF (USTAR .GE. 1.0D-10) THEN
!            UOUST = UMAX(I) / USTAR
!         ELSE
!            UOUST = -999.0D0
!         END IF

!        Calculate sigma-v / U
!         IF (UMAX(I) .GE. 1.0D-10) THEN
!            SVOU = SVMAX(I)/UMAX(I)
!         ELSE
!            SVOU = -999.0D0
!         END IF

!        Calculate He / Zi
      if (zi >= 1.0d-10) then
         heozi = hemax(i) / zi
      else
         heozi = -999.0d0
      end if

!        Calculate Zi / L
!     JAT 7/22/21 D065 ZIOL NOT USED
!         IF (DABS(OBULEN) .GE. 1.0D-10) THEN
!            ZIOL = ZI / OBULEN
!         ELSE
!            ZIOL = 999.0D0
!         END IF

!RWBC      Calculate total F
!RWB       FTOT = FB + FM
!RWB     Replace FTOT with FSTAR (non-dimensional buoyancy flux).  2/13/95
!RWB     Note that UP is the latest value for plume rise wind speed
!RWB     from the iterative stable plume rise.
      if (wstar >= 1.0d-10) then
         fstar = fb / (up * wstar * wstar * zi)
      else
         fstar = -999.0d0
      end if

      if (obulen < 0.0d0) then

!           Calculate U / WSTAR when L < 0
!         JAT 7/22/21 D065 UOWST NOT USED
!            IF (WSTAR .GE. 1.0D-10) THEN
!               UOWST = UMAX(I) / WSTAR
!            ELSE
!               UOWST = -999.0D0
!            END IF

!           Calculate nondimensional distance when L < 0
         if (umax(i) >= 1.0d-10 .and. zi >= 1.0d-10) then
            xndim = dxmax(i) * wstar / (umax(i) * zi)
         else
            xndim = -999.0d0
         end if
!crfl 5/18/95 When unstable, put WSTAR into PWSTAR variable to be printed.
         pwstar = wstar

      else

!           Set UOWST and XNDIM to -999 when L >= 0
!         JAT 7/22/21 D065 UOWST NOT USED
!            UOWST = -999.0D0
         xndim = -999.0d0
!crfl 5/18/95 When stable, put Sigma-Z into PWSTAR variable to be printed.
         pwstar = szmax(i)
      end if

!CRFL
!CRFL  Added ARCCL(I), arc true centerline concentration for the arc.
!CRFL  Change made 7/25/94, R.F. Lee.
!CRFL
!RWB           WRITE(IELUNT(ISRC),9000) SRCID(ISRC), KURDAT, ARCID(I),
!RWB     &                      ARCMAX(I), QMAX(I), CWIC,
!RWB     &                      DXMAX(I), UMAX(I), SVMAX(I),
!RWB     &                      SWMAX(I), SYMAX(I), HEMAX(I),
!RWB     &                      OBULEN, ZI, USTAR, WSTAR, FB, FM,
!RWB     &                      BOWEN, UOUST, SVOU, ZIOL, UOWST, XNDIM,
!RWB     &                      HEOZI, FTOT, AHS(ISRC), ARCCL(I), DOPTS
!RWBCRWB                        Added DOPTS, Developmental Options (C*10)

!RWB     Modified to output CHI's for individual "plumes".  2/13/95
!RWB     First select appropriate sigma-y to print out. Use SY3 for mostly
!RWB     penetrated plumes.
      if (unstab .and. hs<zi .and. ppf>0.999d0) then
         uout  = u3max(i)
         syout = sy3mx(i)
      else
         uout  = umax(i)
         syout = symax(i)
      end if

      if (urbstab) then
         obuout = urbobulen(iurb)
      else
         obuout = obulen
      end if


!crfl 5/18/95 Changed WSTAR to PWSTAR so I could output another variable
!crfl         (Sigma-Z) in stable conditions without upsetting WSTAR.
      write(ielunt(isrc),9000) srcid(isrc), kurdat, arcid(i),&
      &arcmax(i), qmax(i), cwic, cwicn,&
      &dxmax(i), uout, svmax(i),&
      &swmax(i), syout, hemax(i),&
      &obuout, zi, ustar, pwstar, fb, fm,&
      &bowen, ppf, chidml(i), chinml(i), chi3ml(i),&
      &xndim, heozi, fstar, ahs(isrc), arccl(i),&
      &afv, hsblmx(i)
!RWB                      Added Flow Vector, AFV
!RWB                      Added height of effective reflecting surface, HSBLMX


   end do
!RCO remove line breaks 9/2/21
9000 format(1x,a12,1x,i8.8,1x,a8,4(1x,g12.6),&
   &9x,6(1x,g12.4),9x,6(1x,g12.4),&
   &9x,6(1x,g12.4),9x,4(1x,g12.4),1x,'0000000000',&
   &1x,g12.4,1x,g12.4)
!C   End of changes dated 2/25/94 through 3/14/94 by Russ Lee
!C

   return
end subroutine evalfl

subroutine mxdybyyr(n)
!***********************************************************************
!                 MXDYBYYR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Write Maximum Daily 1-hour Values for the Year to
!                 a file by rank
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       February 28, 2011
!
!        INPUTS:  MAXDAILY file options
!                 Array of maximum daily 1-hour values
!
!        OUTPUTS:
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none

   integer :: i, n
   integer :: idec, imod
!     JAT 7/22/21 D065 ICHR NOT USED
!      INTEGER :: ICYR, ICMN, ICDY, ICHR, ICDAT, ICDAT8, ICJDY
   integer :: icyr, icmn, icdy, icdat, icdat8, icjdy
   character :: rank(10)*5, chrval*5
   character :: modnam*12
! Unused:       INTEGER :: J
! Unused:       CHARACTER PERCHR*6

!     Variable Initializations
   data (rank(i),i=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',&
   &'  6TH','  7TH','  8TH','  9TH',' 10TH'/
   modnam = 'MXDYBYYR'
   icdat  = 0

! --- Assign character label for rank
   if (n <= 10) then
      chrval = rank(n)
   else if (mod(n,100) > 10 .and.&
   &mod(n,100) < 20) then
      idec = int(n/10)
      imod = mod(n,10)
      write(chrval,'(I2,I1,"TH")') idec, imod
   else if (n <= 999) then
      idec = int(n/10)
      imod = mod(n,10)
      if (imod == 0) imod = 10
      write(chrval,'(I2,A3)') idec, rank(imod)(3:5)
   end if

!     Begin Source Group LOOP
   do igrp = 1, numgrp
!        Check for MXDYBYYR Option for This IGRP
      if (imxdly_byyr(igrp) == 1) then

         receptor_loop: do irec = 1, numrec
            ityp = 1
!              WRITE Results to Formatted Plot File
!              Begin Receptor LOOP

! ---          Extract date information
            icdat8 = nhidatmxd(irec,igrp,n)
            icyr   = icdat8/1000000

!            D001 Call LONG_DATE and determine the longform date Wood 9/15/22
            call long_date(icdat8,icdat,icyr,icyr)
! ---  D001 remove original calculation of year Wood 9/15/22
!               IF (ICYR .GE. ISTRT_WIND .and. ICYR .LE. 99) THEN
!                  ICYR  = ISTRT_CENT*100 + ICYR
!                  ICDAT = ISTRT_CENT*100000000 + ICDAT8
!               ELSE IF (ICYR .LT. ISTRT_WIND) THEN
!                  ICYR  = (ISTRT_CENT+1)*100 + ICYR
!                  ICDAT = (ISTRT_CENT+1)*100000000 + ICDAT8
!               END IF

            icmn = (icdat/10000) - (icdat/1000000)*100
            icdy = (icdat/100) - (icdat/10000)*100
!             JAT 7/22/21 D065 ICHR NOT USED
!               ICHR =  ICDAT - (ICDAT/100)*100
!              Calculate JULIAN Day for Start and End Dates
!              but first make sure date variables are not 0
            if (icmn>0 .and. icdy>0) then
               call julian (icyr,icmn,icdy,icjdy)
            else
               icjdy = 0
            end if

! ---          Write MXDYBYYR values to file
            write(imdunt_byyr(igrp),mxdfrm,err=99)&
            &axr(irec), ayr(irec), himxdly(irec,igrp,n),&
            &azelev(irec), azhill(irec), azflag(irec),chrval,&
            &grpid(igrp), icjdy,&
            &nhidatmxd(irec,igrp,n)-(nhidatmxd(irec,igrp,n)/100)*100,&
            &nhidatmxd(irec,igrp,n), netid(irec)

         end do receptor_loop
      end if
   end do
!     End Source Group LOOP

   go to 999

!     WRITE Error Message for Problem Writing to MXDYBYYR File
99 write(dummy,'("MXDLY",I3.3)') imdunt_byyr(igrp)
   call errhdl(path,modnam,'E','520',dummy)
   runerr = .true.

999 return
end subroutine mxdybyyr

