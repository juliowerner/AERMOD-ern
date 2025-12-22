subroutine deltah ( xarg )
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
   use main1
   implicit none

   character :: modnam*12
   integer :: kiter, ndxzpl
   double precision :: xarg, xmaxtmp, xrise, zplm, dhpold,&
   &svpm, upm, tgpm, ptpm, ptp

!     Variable Initializations
   modnam = 'DELTAH'

   if( (stable  .or.  (unstab  .and.  (hs >= zi)))  .and.&
   &(xarg >= xmax) )then
!        Use final stable plume rise (DHF) calculated in DISTF (DHP)
!        at XMAX
      dhp = dhfaer


   else if( (stable  .or. (unstab  .and.  (hs >= zi))) .and.&
   &(xarg < xmax) ) then
!----    Compute stable plume rise for the distance XARG   --- CALL SBLRIS
!        Use iterative approach to plume rise calculations.
!        First compute temporary distance to "final rise" based on current
!        values of UP and BVPRIM.  Then, don't pass a distance larger than
!        XMAXTMP to SBLRIS.  This avoids potential for math error in
!        SUB. SBLRIS for distances beyond the value of XMAX computed
!----    iteratively outside the receptor loop in SUB. DISTF.
      xmaxtmp = up * datan2( fm*bvprim, -fb ) / bvprim
      xrise   = min( xarg, xmaxtmp )
      call sblris ( xrise )
      kiter = 0

50    zplm = hsp + 0.5d0 * dhp
      dhpold = dhp

!----    Locate index below ZPLM

      call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----    Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
!        vertical potential temperature gradient, if stable.

      call gintrp( gridht(ndxzpl), gridsv(ndxzpl),&
      &gridht(ndxzpl+1), gridsv(ndxzpl+1), zplm, svpm )
      call gintrp( gridht(ndxzpl), gridws(ndxzpl),&
      &gridht(ndxzpl+1), gridws(ndxzpl+1), zplm, upm )

      svpm = max( svpm, svmin, svumin*upm )
      if( l_vectorws )then
         upm = dsqrt( upm*upm + 2.0d0*svpm*svpm )
      endif
      upm  = max( upm, wsmin )

!RWB     Use average of stack top and midpoint wind speeds.
      up = 0.5d0 * (us + upm)

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
      xmaxtmp = up * datan2( fm*bvprim, -fb ) / bvprim
      xrise   = min( xarg, xmaxtmp )
      call sblris ( xrise )

      kiter = kiter + 1

!RJP     Add temporary debugging statements

      if(debug) then
         write(dbgunt,6001) kiter,dhpold, dhp, zplm, up,tgp
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
         if(debug) write(dbgunt,6002) dhp
6002     format(/,5x,'OPTH2 ITERATION FAILED TO CONVERGE; PLUME',&
         &' RISE SET AT ',f6.1,' METERS.',/)
         go to 60
      else
         go to 50
      endif

60    continue

!RWB     After completing iteration, reset UP and TGP to stack top
!RWB     values for subsequent distance-dependent plume rise calcs.
      up  = us
      tgp = tgs
      ptp = pts
      bvf = dsqrt( g * tgp / ptp )
      if(bvf < 1.0d-10) bvf = 1.0d-10
      bvprim  = 0.7d0 * bvf
!crfl-3/6/95 Make sure SBL rise is not greater than CBL rise.
      call cblprd(xarg)
      dhp = min(dhp,dhp1)
      dhp = min(dhp,dhfaer)

   elseif( unstab )then
!        (i.e., for UNSTABle cases, with HS < ZI)

!        Compute  plume rise for the direct plume       --- CALL CBLPRD
      call cblprd ( xarg )

!        Compute  plume rise for the indirect plume        --- CALL CBLPRN
      call cblprn ( xarg )

      if( ppf > 0.0d0 )then
!           Compute plume rise for the penetrated plume    --- CALL CBLPR3
         call cblpr3

      else
!           No plume penetration - plume rise is zero for this source
         dhp3 = 0.0d0

      endif

   endif

   return
end subroutine deltah

subroutine prmdelh ( xarg, l_inwake )
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
   use main1
! --- Include PRIME plume rise parameters
   use PRIME_wakedat
   use prm2_wakedat, only: dfsn2call
   implicit none

   character :: modnam*12
   integer :: numwake, ierr
   double precision :: xarg
   double precision, save :: hseff, reff
! --- Dimension work arrays for PRIME numerical plume rise
   double precision, save :: xtr(mxntr), ytr(mxntr),&
   &ztr(mxntr), rtr(mxntr)

   logical       :: l_inwake
   logical, save :: l_inwake_save, capped, horiz

!     Variable Initializations
   modnam = 'PRMDELH'
   capped = .false.
   horiz  = .false.

!
! --- PRIME ---------------------------------------------------
   if (wake) then
! ---    Calculate final rise & array of transitional rise values
! ---    for first receptor only
      if (prm_fstrec) then
         prm_fstrec = .false.
         l_inwake = .false.
         dfsn2call = .false.
         hseff=hs
! ---       Compute stack radius from diameter
         reff=0.5d0*ds
         if (srctyp(isrc) == 'POINTCAP') then
            capped = .true.
         else
            capped = .false.
         end if
         if (srctyp(isrc) == 'POINTHOR') then
            horiz = .true.
         else
            horiz = .false.
         end if
! ---       Calculate transitional & final plume rise       ---   CALL NUMRISE
         call numrise(primedbg,hseff,reff,ts,vs,mxntr,capped,horiz,&
         &dsfact,xtr,ytr,ztr,rtr,l_inwake,numwake,ierr,&
         &prmdbunt)
         if (ierr == 1) then
! ---          Error occurred during PRIME numerical plume rise.
!              Write fatal error message - source parameters may be suspect.
            call errhdl(path,modnam,'E','499',srcid(isrc))
            runerr = .true.
            return
         end if
         if (numwake <= 1) then
            l_inwake = .false.
         end if
! ---       ZTR is effective plume ht. - compute final rise
         dhf = ztr(mxntr) - hseff
! ---       Report selected data to file for debug          ---   CALL WAKE_DBG
         if(primedbg) call wake_dbg(prmdbunt,mxntr,xtr,ytr,ztr,rtr,&
         &nobid,hseff)
         l_inwake_save = l_inwake
      else
         l_inwake = l_inwake_save
      endif
!
! ---    Determine the plume rise for current receptor
      if (xarg < xtr(mxntr)) then
! ---       Interpolate in rise table to get gradual rise   ---   CALL NUMGRAD
         call numgrad(xarg,xtr,ztr,mxntr,zeff)
         dhp = zeff - hseff
      else
         dhp = ztr(mxntr) - hseff
      end if

   endif

   return
end subroutine prmdelh

function hsprim(us,vs,hs,ds)
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
   implicit none
   double precision :: us, vs, hs, ds, hsprim
   character :: modnam*6
!     Variable Initializations
   modnam = 'HSPRIM'

!     Calculate Adjusted Stack Height (Eqn. 1-7)

   if (vs < 1.5d0*us) then
      hsprim = hs - 2.0d0*ds*(1.5d0-vs/us)
   else
      hsprim = hs
   end if

   if (hsprim < 0.0d0)  hsprim = 0.0d0

   return
end function hsprim

subroutine sblris ( xarg )
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
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg, terma, termb, termc, termd, terme
   double precision :: xln, delhnn


!     Variable Initializations
   modnam = 'SBLRIS'

!---- Compute the stable plume rise; FB and BVF and UP have all been
!     checked previously to insure all are greater than 0.0

   terma =  fb / (bvf * bvf * up)
   termb =  bvprim * fm / fb
   termc =  dsin(bvprim * xarg / up)
   termd =  dcos(bvprim * xarg / up)

! --- Calculate TERME to check for possible negative argument for DHP
   terme = (termb*termc+1.0d0-termd)

   if( terme > 0.0d0 )then
      dhp = 2.66d0 * (terma *(termb*termc+1.0d0-termd))**third
   else
      dhp = 2.66d0 * (terma*termb*termc)**third
   endif


! --- Equation 95 of MFD for distant-dependent stable plume rise
!     DHP = 2.66D0 * TERMA**THIRD * (TERMB*TERMC+1.0D0-TERMD)**THIRD

!      DHP = 2.66 * (FB / (BVF * BVF * UP) ) ** 0.333333 *
!     &              ( (BVPRIM * FM / FB) * SIN(BVPRIM * XARG / UP) +
!     &                1.0 - COS(BVPRIM * XARG / UP) ) ** 0.333333

! --- Apply lower limit on stable plume rise based on Equation 98
!     of the MFD
   xln = fb/(up*ustar*ustar)
   delhnn = 1.2d0*xln**0.6d0 * (hsp + 1.2d0*xln)**0.4d0

   dhp = min( dhp, dhfaer, delhnn )

   return
end subroutine sblris

subroutine cblprd ( xarg )
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
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'CBLPRD'

! --- Original code based on Eq. 91 of MFD.
   dhp1 = (3.0d0 * fm * xarg / (beta1*beta1 * up*up) +&
   &3.0d0 * fb * xarg*xarg /&
   &(2.0d0 * beta1*beta1 * up*up*up) )**third

   return
end subroutine cblprd

subroutine cblprn ( xarg )
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
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg, rsubh, ryrz, delhi

!     Variable Initializations
   modnam = 'CBLPRN'

   rsubh = beta2 * (zi - hsp)
   ryrz  = rsubh * rsubh + 0.25d0*asube*(lamday**1.5d0) *&
   &wstar*wstar*xarg*xarg/(up*up)
   delhi = dsqrt( (2.d0*fb*zi)/(alphar*up*ryrz) ) * (xarg/up)
   dhp2  = delhi

   return
end subroutine cblprn

subroutine cblpr3
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
end subroutine cblpr3
