subroutine averts(xvin,yvin,xwd,ywd,numv)
!***********************************************************************
!*                AVERTS Module of AERMOD Model
!*
!*       PURPOSE: Calculates coordinates of vertices for Wind
!*                Direction Coordinate system for OPENPIT sources.
!*
!*       PROGRAMMER: Jeff Wang, Roger Brode
!*       MODIFIED:   Jayant Hardikar, Roger Brode (for OPENPIT sources)
!*
!*       DATE:      July 7, 1993
!*
!*       INPUTS:  Source Coordinates for Specific Source
!*                Number of vertices + 1
!*
!*       OUTPUTS: Array of Vertex Coordinates for Specific Source
!*
!*       CALLED FROM:   PITEFF
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: numv, nsp

   double precision :: xvin(nvmax), yvin(nvmax)
   double precision :: xwd(nvmax),  ywd(nvmax)

!*    Variable Initializations
   modnam = 'AVERTS'

   do nsp = 1, numv
      xwd(nsp) = -(xvin(nsp)*wdsin + yvin(nsp)*wdcos)
      ywd(nsp) =   xvin(nsp)*wdcos - yvin(nsp)*wdsin
   end do

   return
end subroutine averts

subroutine areain
!***********************************************************************
!                 AREAIN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Hourly Concentration for AREA Sources
!                 Using Numerical Integration Algorithm
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        MODIFIED BY R. Brode, PES, Inc., 4/2/99, to change the lower limit
!                 on (ua-ub) from 1.0 to 0.01 meter.  This prevents potential
!                 anomalous results for very small area sources (about
!                 1.0 meter wide).
!
!        MODIFIED BY R. Brode, PES, Inc. to use -3.9 to +3.9 for width of
!                 the plume for internal consistency with PWIDTH.  Also
!                 added error checks on the number of "sides" exceeding
!                 NVMAX, which could happen with complex AREAPOLY shapes.
!                 12/14/98
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Concentration for Particular Source/Receptor Combination
!
!        CALLED FROM:   ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i
   integer :: kside, ncp, ksp, ks
   double precision :: ucrit, ua, ub, va, vb, vnmin, vnmax, wa, vnorm
   double precision :: val, dval
   logical :: qgo

!     Variable Initializations
   modnam = 'AREAIN'

!     INITIALIZE VARIABLES FOR INTEGRATION PROCEDURE.
   ucrit = 1.01d0
   val   = 0.0d0
   uvert = 0.0d0
   vvert = 0.0d0
   kside = 0

   do ncp = 1, nvert
      ua = spa(ncp,1)
      ub = spa(ncp+1,1)
      va = spa(ncp,2)
      vb = spa(ncp+1,2)
      if (ua >= ucrit) then
         kside = kside + 1

         if (kside <= nvmax+1) then
            uvert(kside) = ua
            vvert(kside) = va
         else
!              Write Error Message:  Number of "sides" exceeds NVMAX
            call errhdl(path,modnam,'E','406',srcid(isrc))
            runerr = .true.
            return
         end if
      end if
      if ((ua >= ucrit .and. ub < ucrit) .or.&
      &(ua < ucrit .and. ub >= ucrit)) then
         kside = kside+1
         if (kside <= nvmax+1) then
            uvert(kside) = ucrit
            vvert(kside) = va+(ucrit-ua)*(vb-va)/(ub-ua)
         else
!              Write Error Message:  Number of "sides" exceeds NVMAX
            call errhdl(path,modnam,'E','406',srcid(isrc))
            runerr = .true.
            return
         end if
      end if
   end do

   if( areadbg )then
      if( .not. evonly )then
! ---       Non-EVENT processing
         write(areadbunt,100) kurdat, srcid(isrc), kside
100      format(/1x,'AREA Debug Sub_AREAIN:   YYMMDDHH = ',i8.8,&
         &//1x, a12,' kside= ',i3,//&
         &'  I          UVERT           VVERT' )
      else
! ---      EVENT processing
         write(areadbunt,1001) evdate(ievent), srcid(isrc), kside
1001     format(/1x,'AREA Debug Sub_AREAIN:   YYMMDDHH = ',i8.8,&
         &//1x, a12,' kside= ',i3,//&
         &'  I          UVERT           VVERT' )
      endif
      do i=1,kside
         write(areadbunt,101) i, uvert(i), vvert(i)
101      format(1x, i2, 2(2x,f15.6))
      enddo
   endif

   qgo = .false.
   if (kside > 2) then
      qgo = .true.
      vnmin=  3.9d0
      vnmax= -3.9d0
      do ncp = 1, kside
         ua = uvert(ncp)
         va = vvert(ncp)
         call pwidth(ua,va,vnorm,wa)
         vnvert(ncp) = vnorm
         wvert(ncp)  = wa
         vnmax = max(vnorm,vnmax)
         vnmin = min(vnorm,vnmin)
      end do
      if (vnmin >= 3.9d0 .or. vnmax <= -3.9d0) qgo = .false.

      if( areadbg )then
         write(areadbunt,*)
         write(areadbunt,*) ' I          VNVERT           WVERT'
         do i=1,kside
            write(areadbunt,101) i,vnvert(i),wvert(i)
         enddo
         write(areadbunt,*)
         write(areadbunt,102) vnmin, vnmax, qgo
102      format(' VNMIN= ',f15.6,'; VNMAX= ',f15.6,'; QGO= ',l2/)
      end if
   end if

!     Integrate Between Vertices u(1),u(2) THEN u(2),u(3); etc.
   if (qgo) then
!        MAKE 1st Point Same as Last
      ksp = kside+1
      if (ksp <= nvmax+1) then
         uvert(ksp)  = uvert(1)
         vvert(ksp)  = vvert(1)
         vnvert(ksp) = vnvert(1)
         wvert(ksp)  = wvert(1)
      else
!           Write Error Message:  Number of "sides" exceeds NVMAX
         call errhdl(path,modnam,'E','406',srcid(isrc))
         runerr = .true.
         return
      end if
      nsegs = 0
      lseg = .false.
      do ks = 1, kside
         qgo = .true.
         ivert = ks
         ua = uvert(ks)
         ub = uvert(ks+1)
         dval  = 0.0d0
         if (dabs(ua-ub) < 0.01d0) qgo = .false.
         if (qgo .and. fastarea) then
            call pside_tox(ua,ub,dval)
         else if (qgo) then
            call pside(ua,ub,dval)
         end if
         val  = val + dval
      end do
      if (nsegs > 0) then
         lseg = .true.
         if (fastarea) then
            call pside2_tox(dval)
         else
            call pside2(dval)
         end if
         val  = val + dval
      end if

!----    Calculate hourly value, HRVAL
!----    Note that 1/U term is now included in VAL.
!----    Added 1/(2pi) when calculating meander/pancake plume Wood 5/5/22
      if (l_aplume) then
         hrval(ityp) = dabs(val)*qtk*emifac(ityp)
      else
         hrval(ityp) = dabs(val)*qtk*emifac(ityp)*(1.0d0/twopi)
      end if
   else

      hrval(ityp)  = 0.0d0
   end if

   if (debug) then
!        Print Out Debugging Information                    ---   CALL DEBOUT
      call debout
   end if

   return
end subroutine areain

subroutine plumef(xarg,pout)
!***********************************************************************
!                 PLUMEF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Driving Routine for Plume Calculations
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        MODIFIED:   To add the Aircraft's plume rise.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   To include call to CRITDS for gases, and to
!                    correct a problem with the AREADPLT option.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
!
!        MODIFIED:   To assign value to XDIST for use in dry depletion.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
!
!        MODIFIED BY R. Brode, PES, Inc. to call separate ADIS_ routines
!                    for sigma-y and sigma-z for optimization. - 12/10/98
!
!        MODIFIED BY D. Strimaitis and Yicheng Zhuang, SRC (for DEPOSITION)
!
!        MODIFIED BY R. Brode, PES, Inc. to move calculation of dispersion
!                    coefficients to a new ADIS subroutine - 7/21/94
!
!        DATE:    September 28, 1993
!
!        INPUTS:  Downwind Distance (in km !)
!                 Source Parameter Arrays
!
!        OUTPUTS: Concentration for Particular Source/Receptor Combination
!                 For A Certain Downwind Distance
!
!        CALLED FROM:   TRAPZD
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Declare Local Variables
   integer :: j
   double precision :: xarg, pout, ptmp, val, vt, vn, adj

!     Variable Initializations
   modnam = 'PLUMEF'

   ptmp  = 0.0d0
   pout  = 0.0d0

! --- Assign XDIST for use in dry depletion (FUNCTION F2INT)
   xdist = xarg

!     Determine Deposition Correction Factors
   if (npd == 0 .and. .not.ardplete .and. (ldgas .or. lwgas)) then
      call pdepg (xarg)
   else if (.not.ardplete) then
      dqcorg = 1.0d0
      wqcorg = 1.0d0
   end if
   if (.not.ardplete .and. (ldpart .or. lwpart)) then
      call pdep (xarg)
   else if (.not.ardplete .and. npd > 0) then
!        Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
      dqcor = 1.0d0
      wqcor = 1.0d0
   endif

!     Define plume centroid height (CENTER) for use in
!     inhomogeniety calculations
   call centroid (xarg)

!     If the atmosphere is unstable and the stack
!     top is below the mixing height, calculate
!     the CBL PDF coefficients                              ---   CALL PDF
   if( unstab  .and.  (hs < zi) ) then
      call pdf
   endif

!     Determine Effective Plume Height                      ---   CALL HEFF
!**   Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) == 'Y') then
!      Find Aircraft Index for This Source
!       Calculate Buoyancy Flux                             ---   CALL AFLUXES
      call afluxes
!        Calculate Distance to Final Rise                   ---   CALL ADELTAH
      call adeltah ( xarg )
      call heff ( xarg )
   else
      call heff ( xarg )
   end if
!**  End Aircraft Plume Rise insert; April 2023
!     Iterative average through plume rise layer
   call iblval (xarg)

!     Call PDF & HEFF again for final CBL plume heights
   if (unstab .and. (hs<zi) ) then
      call pdf
!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) == 'Y') then
!       Find Aircraft Index for This Source
!       Calculate Buoyancy Flux                             --- CALL AFLUXES
         call afluxes
!        Calculate Distance to Final Rise                   --- CALL ADELTAH
         call adeltah ( xarg )
         call heff ( xarg )
      else
         call heff ( xarg )
      end if
!**  End Aircraft Plume Rise insert; April 2023
   end if

! --- Determine lateral term, VAL
!     MODIFIED to NOT compute vn, val for cases with val=1.0, uses LSEG,
!     a logical variable set in PSIDE2, AREAIN to establish case
   if (lseg) then
!        Calculate vertical dispersion coefficients, SZ
!        Lateral dispersion coefficient not needed since VAL = 1.0
      call adisz(xarg)
      val = 1.0d0
   else
!        Determine width area covered by the plume
      call xwidth(xarg,vt)
      call pwidth(xarg,vt,vn,val)
!        Calculate vertical dispersion coefficient, SZ
      call adisz(xarg)
   end if

   if (npd == 0) then
!        Perform calculations for gases
!        Assign plume tilt, HV = 0.0
      hv = 0.0d0

      adj = dqcorg * wqcorg

      if (stable .or. (unstab.and.(hs>=zi))) then
!           Calculate height of the "effective reflecting surface"
!**  Added for Aircraft Plume Rise; UNC-IE
         if (aftsrc(isrc) == 'Y') then
            call refl_ht (he, xarg, szb, vsigz, hsbl)
         else
!**  End Aircraft Plume Rise insert; April 2023
            call refl_ht (he, xarg, 0.0d0, vsigz, hsbl)
         end if
      elseif ( unstab ) then
         hsbl = 0.0d0
      endif

!        Determine the CRITical Dividing Streamline---   CALL CRITDS
      call critds (he)

!        Calculate the fraction of plume below
!        HCRIT, PHEE                               ---   CALL PFRACT
      call pfract (he)

!        Calculate FOPT = f(PHEE)                  ---   CALL FTERM
      call fterm

!        Calculate Concentration
      call aer_achi( xarg, adj, vdepg, 0, val, pout )

   else
!        Perform calculations for particles, loop through particle sizes

      do j = 1, npd

!           Calculate Plume Tilt Due to Settling, HV
         hv = (xarg/us) * vgrav(j)

!           Adjust Jth contribution by mass fraction and source
!           depletion
         adj = phi(j) * dqcor(j) * wqcor(j)

         if (stable .or. (unstab.and.(hs>=zi))) then
!              Calculate height of the "effective reflecting surface"
!              Calculate Settled Plume Height(s), HESETL
            hesetl = max( 0.0d0, he - hv )
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) == 'Y') then
               call refl_ht (hesetl, xarg, szb, vsigz, hsbl)
            else
!**  End Aircraft Plume Rise insert; April 2023
               call refl_ht (hesetl, xarg, 0.0d0, vsigz, hsbl)
            end if
         else if ( unstab ) then
!              Calculate Settled Plume Height(s), HESETL
            hesetl = max( 0.0d0, 0.5d0*(hed1+hed2) - hv )
            hsbl = 0.0d0
         end if

!           Determine the CRITical Dividing Streamline---   CALL CRITDS
         call critds (hesetl)

!           Calculate the fraction of plume below
!           HCRIT, PHEE                               ---   CALL PFRACT
         call pfract (hesetl)

!           Calculate FOPT = f(PHEE)                  ---   CALL FTERM
         call fterm

!           Calculate Concentration
         call aer_achi( xarg, adj, vdep(j), j, val, ptmp )
         pout  = pout + ptmp

      end do

   end if

   return
end subroutine plumef


subroutine pside(u1,u2,dval)
!***********************************************************************
!                 PSIDE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: INTEGRATES SIDE K of POLYGON
!                 int f(u)*CNF(v(u)/sig(u))=f(u)*vn(u) from u1 to u2
!                           CNF = cumulative normal distribution
!                 Computes W(1),W(2)--normalized plume width at   u1    u2
!                 Checks for w(i) outside of -3.9,3.9 with i+, i-
!                 L=-3.9  U=3.9  = bounds for testing
!                 Integrates according to case encountered:
!                 situation     CASE    iplus    iminus  integral limits
!                 L<w1,w2<U      1        0        0         u1,u2
!                 w1,w2<L        2        0       1+2      don't compute
!                 w1,w2>U        3       1+2       0         u1,u2
!                 w1<L<w2<U      4        0        1         u-,u2
!                 w2<L<w1<U      5        0        2         u1,u-
!                 L<w1<U<w2      6        2        0       u1,u+  u+,u2
!                 L<w2<U<w1      7        1        0       u1,u+  u+,u2
!                 w1<L<U<w2      8        2        1       u-,u+  u+,u2
!                 w2<L<U<w1      9        1        2       u1,u+  u+,u-
!
!                 u+ = value of u such that w(u)=U=3.9
!                 u- =     "                w(u)=L=-3.9
!                 u+,u- computed with Brent's Algorithm
!
!                 IF uplus >0, part of side is outside plume
!                 but is integrated anyway, unless there is
!                 a corresponding part on another side that will
!                 cause cancellation.  This is determined in
!                 PSIDE2;
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        MODIFIED by Roger Brode, PES, Inc. to use QATR routine for
!                    integration, and to pass VN(1) and VN(2) to
!                    ZBRENT.  Also changed to use -3.9 to +3.9 for
!                    width of plume for internal consistency with
!                    SUBROUTINE PWIDTH. - 12/14/98
!
!        MODIFIED by Roger Brode, PES, Inc. to correct lower integration
!                    limit for Case 4, and to remove extraneous calls
!                    to XWIDTH and PWIDTH after calls to ZBRENT. - 7/29/94
!
!        INPUTS:  End Points of The Segments
!
!        OUTPUTS: Integral Value (if any) for Segment
!
!        CALLED FROM:   AREAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!---- Set convergence criteria for calls to QATR3:
!         NDIM = maximum number of integration steps
!         IMIN = minimum number of integration steps
!         EPSR = relative error tolerance for integral
!         EPST = minimum value threshold for integral
!----
   integer, parameter :: ndim = 12, imin = 5
   double precision, parameter :: epsr = 1.0d-2, epst = 1.0d-5

   integer :: icase
   integer :: i, ks, iminus, iplus, nout, icon
   double precision :: dval, u1, u2, uminus, uplus, aux(ndim),&
!     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
!     &                    u(2), v1(2), vn(2), w(2)
   &u(2), vn(2)

!     Variable Initializations
   modnam = 'PSIDE'

!     NSEG = number of segments; set to 0 in AREAIN
!     for each source/rcvr/time step
   dval  = 0.0d0
!     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
   do i =  1,2
      ks    = ivert + i-1
      u(i)  = uvert(ks)
!         v1(i) = vvert(ks)
      vn(i) = vnvert(ks)
!         w(i)  = wvert(ks)
   end do

   iminus = 0
   iplus  = 0
   uminus = -1.0d0
   uplus  = -1.0d0
   do i = 1,2
      if (vn(i) < -3.9d0) iminus = i + iminus
      if (vn(i) >  3.9d0) iplus  = i + iplus
   end do

   if (iplus==1 .or. iplus==2) then
      call zbrent( 1,u(1),u(2),vn(1),vn(2),1.0d-3,uplus)
   end if
   if (iminus==1 .or. iminus==2) then
      call zbrent(-1,u(1),u(2),vn(1),vn(2),1.0d-3,uminus)
   end if

   if( areadbg) then
      write(areadbunt,*)
      write(areadbunt,*) 'AREA Debug Sub_PSIDE: '
      write(areadbunt,*) '  ISRC   = ',isrc
! ---
      if( evonly )then
         write(areadbunt,*) '  IEVT   = ',ievent
      else
         write(areadbunt,*) '  IREC   = ',irec
      endif
      write(areadbunt,*) ' iplus  iminus  case'
      if( iplus==0 .and. iminus==0 )then
         icase = 1
      elseif( iplus==0 .and. iminus==3 )then
         icase = 2
      elseif( iplus==0 .and. iminus==1 )then
         icase = 4
      elseif( iplus==0 .and. iminus==2 )then
         icase = 5
      elseif( iplus==1 .and. iminus==0 )then
         icase = 7
      elseif( iplus==1 .and. iminus==2 )then
         icase = 9
      elseif( iplus==2 .and. iminus==0 )then
         icase = 6
      elseif( iplus==2 .and. iminus==1 )then
         icase = 8
      elseif( iplus==3 .and. iminus==0 )then
         icase = 3
      endif
      write(areadbunt,'(I5,I7,I6)') iplus, iminus, icase
      write(areadbunt,*)
   endif

!---- CASE DEPENDs on iplus, iminus
   if (iplus == 0) then
      if (iminus == 0) then
!                                             iplus  iminus  case
!                                               0     0       1
         call qatr3(u1,u2,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      else if (iminus == 3) then
!                                               0     3       2
         dval = 0.0d0
      else if (iminus == 1) then
!                                               0     1       4
         call qatr3(uminus,u2,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      else
!                                               0     2       5
         call qatr3(u1,uminus,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      end if

   else if (iplus == 1) then
      nsegs = nsegs+1
      uasegs(nsegs) = u1
      ubsegs(nsegs) = uplus
      if (iminus == 0) then
!                                               1     0       7
         call qatr3(uplus,u2,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      else
!                                               1     2       9
         call qatr3(uplus,uminus,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      end if

   else if (iplus == 2) then
      nsegs = nsegs+1
      uasegs(nsegs) = uplus
      ubsegs(nsegs) = u2
      if (iminus == 0) then
!                                               2     0       6
         call qatr3(u1,uplus,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      else
!                                               2     1       8
         call qatr3(uminus,uplus,epsr,epst,ndim,imin,dval,&
         &icon,nout,aux)
      end if

   else
!                                               3     0       3
      nsegs = nsegs+1
      uasegs(nsegs) = u1
      ubsegs(nsegs) = u2
   end if

   if( areadbg )then
      write(areadbunt,*)
      write(areadbunt,*) '  ISRC   = ',isrc

      if( evonly )then
         write(areadbunt,*) '  IEVT   = ',ievent
      else
         write(areadbunt,*) '  IREC   = ',irec
      endif

      write(areadbunt,*) '  NSEGS  = ',nsegs
      if( nsegs > 0 )then
         write(areadbunt,*) '  UASEGS = ',uasegs(nsegs)
         write(areadbunt,*) '  UBSEGS = ',ubsegs(nsegs)
      else
         write(areadbunt,*) '  UASEGS = NA'
         write(areadbunt,*) '  UBSEGS = NA'
      endif
      write(areadbunt,*) '  DVAL   = ',dval
   endif

   return
end subroutine pside

subroutine xwidth(u,xout)
!***********************************************************************
!                 XWIDTH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Given Any Y Coordinate of A Vertex of an Area
!                 Source, Calculate the X Coordinate
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        INPUTS:  The Y Coordinate
!
!        OUTPUTS: The X Coordinate Value
!
!        CALLED FROM:   ZBRENT
!                       PSIDE
!                       PLUMEF
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: xout, u, u1, u2, v1, v2

!     Variable Initializations
   modnam = 'XWIDTH'

   u1 = uvert(ivert)
   u2 = uvert(ivert+1)
   v1 = vvert(ivert)
   v2 = vvert(ivert+1)
   xout = v1+(u-u1)*(v2-v1)/(u2-u1)

   return
end subroutine xwidth

subroutine pwidth(xarg,v1,vn,width)
!***********************************************************************
!                 PWIDTH Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates The Effective Area of The Plume for A
!                 Certain Downwind Distance
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        MODIFIED BY Gavendra Pandey; UNC-IE, CHapel Hill, NC, USA to
!                    add the Aircraft's Plume Rise - 04/01/2023
!
!        MODIFIED BY R. Brode, PES, Inc. to move calculation of dispersion
!                    coefficients to a new ADIS subroutine - 7/21/94
!
!        MODIFIED BY R. Brode, PES, Inc. to correct table of GA values
!                    and extend GA to 79 values - 7/29/94
!
!        INPUTS: XARG   - Downwind Distance (m) between source and receptor
!                V1     - Crosswind Distance
!
!        OUTPUTS: VN    - Lateral Dispersion Parameter (Effective Width)
!                 WIDTH - Vertical Dispersion Parameter
!
!        CALLED FROM:   ZBRENT
!                       PSIDE
!                       PLUMEF
!                       AREAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: itemp
   double precision :: xarg, width, vn, v1, temp, ga(79)

!     Variable Initializations
!     GA ARE VALUES OF THE CUMULATIVE NORMAL DISTRIBUTION IN
!     INCREMENTS OF 0.1 S.
   data ga/0.0d0,.0001d0,.0001d0,.0002d0,.0002d0,.0003d0,.0005d0,&
   &.0007d0,.0010d0,.0013d0,.0019d0,.0026d0,.0035d0,.0047d0,.0062d0,&
   &.0082d0,.0107d0,.0139d0,.0179d0,.0227d0,.0287d0,.0359d0,.0446d0,&
   &.0548d0,.0668d0,.0808d0,.0968d0,.1151d0,.1357d0,.1587d0,.1841d0,&
   &.2119d0,.2420d0,.2742d0,.3085d0,.3445d0,.3821d0,.4207d0,.4602d0,&
   &.5000d0,.5398d0,.5793d0,.6179d0,.6555d0,.6915d0,.7258d0,.7580d0,&
   &.7881d0,.8159d0,.8413d0,.8643d0,.8849d0,.9032d0,.9192d0,.9332d0,&
   &.9452d0,.9554d0,.9641d0,.9713d0,.9773d0,.9821d0,.9861d0,.9893d0,&
   &.9918d0,.9938d0,.9953d0,.9965d0,.9974d0,.9981d0,.9987d0,.9990d0,&
   &.9993d0,.9995d0,.9997d0,.9998d0,.9998d0,.9999d0,.9999d0,1.000d0/

   modnam = 'PWIDTH'

!    Initialized variables that are not updated with calculating meander/pancake plume Wood 6/3/22
   width = 0.0d0
   vn = 0.0d0

   if (xarg == 0.0d0) then
      sy = 1.0d0
      vn = v1
      width = vn
!        Exit Routine
      go to 999
   end if

!     Added calculation for Sigmay if calculating meander 5/4/22 Wood
!     XARG is the approximate distance from the receptor to the downwind distance of the source
   if (.not. l_aplume) then
      width = dlog(v1 +dsqrt(xarg*xarg + v1*v1))
      return
   end if

!     Define plume centroid height (CENTER) for use in
!     inhomogeniety calculations                            ---   CALL CENTROID
   call centroid (xarg)

!     If the atmosphere is unstable and the stack
!     top is below the mixing height, calculate
!     the CBL PDF coefficients                              ---   CALL PDF
   if( unstab  .and.  (hs < zi) ) then
      call pdf
   endif

!     Determine Effective Plume Height                      ---   CALL HEFF
!**  Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) == 'Y') then
!      Find Aircraft Index for This Source
!      Calculate Buoyancy Flux                              ---   CALL AFLUXES
      call afluxes
!        Calculate Distance to Final Rise                   ---   CALL ADELTAH
      call adeltah ( xarg )
      call heff ( xarg )
   else
      call heff ( xarg )
   end if
!**  End Aircraft Plume Rise insert; April 2023

!     Iterative average through plume rise layer
   call iblval (xarg)

!     Calculate lateral dispersion coefficient, SY          ---   CALL ADISY
   call adisy (xarg)

   vn = v1/sy
   temp = 10.0d0*vn + 40.0d0
   itemp = idint(temp)
   width = 0.0d0

   if (itemp > 78) then
      width = 1.0000d0
   else
      if (itemp >= 1) then
         width = ga(itemp)+(temp-dble(itemp))*&
         &(ga(itemp+1)-ga(itemp))
      end if
   end if

999 return
end subroutine pwidth

subroutine zbrent(ifd,x1,x2,vn1,vn2,tol,outval)
!***********************************************************************
!                 ZBRENT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Divide The Segments According to The Plume Split
!                 And Edge Effects
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        INPUTS:  Downwind Distance
!                 Crosswind Distance
!                 Plume Height
!                 Lateral Dispersion Parameter
!                 Vertical Dispersion Parameter
!                 Source Parameter Arrays
!
!        OUTPUTS: The Effective Integration Segments
!
!        CALLED FROM:   PSIDE
!***********************************************************************

!     Variable Declarations
   implicit none
   character :: modnam*12

   integer, parameter :: itmax = 10
   double precision, parameter :: epsz  = 1.0d-3
   integer :: ifd, iter
   double precision :: outval, tol, x1, x2, a1, b1, v1, w1, vn, fa,&
   &fb, fc, c1, d1, e1, tol1, xm, p1, q1, r1, s1, vn1, vn2

!     Variable Initializations
   modnam = 'ZBRENT'

!CRT  Initialize variables, 12/27/2017
   e1 = 0.0d0
   c1 = 0.0d0

   a1 = x1
   b1 = x2
   fa = vn1-dble(ifd)*3.9d0
   fb = vn2-dble(ifd)*3.9d0

   if (fb*fa <= 0.0d0) then
      fc = fb
      do iter = 1, itmax
         if (fb*fc > 0.0d0) then
            c1 = a1
            fc = fa
            d1 = b1-a1
            e1 = d1
         end if
         if (dabs(fc) < dabs(fb)) then
            a1 = b1
            b1 = c1
            c1 = a1
            fa = fb
            fb = fc
            fc = fa
         end if
         tol1 = 2.0d0*epsz*dabs(b1)+0.5d0*tol
         xm = 0.5d0*(c1-b1)
         if (dabs(xm)<=tol1  .or. fb == 0.0d0) then
            outval = b1
            return
         end if
         if (dabs(e1)>=tol1 .and. dabs(fa)>dabs(fb)) then
            s1 = fb/fa
            if (a1 == c1) then
               p1 = 2.0d0*xm*s1
               q1 = 1.0d0-s1
            else
               q1 = fa/fc
               r1 = fb/fc
               p1 = s1*(2.0d0*xm*q1*(q1-r1)-(b1-a1)*(r1-1.0d0))
               q1 = (q1-1.0d0)*(r1-1.0d0)*(s1-1.0d0)
            end if
            if(p1 > 0.0d0) q1 = -q1
            p1 = dabs(p1)
            if (2.0d0*p1<min(3.0d0*xm*q1-&
            &dabs(tol1*q1),dabs(e1-q1))) then
               e1 = d1
               d1 = p1/q1
            else
               d1 = xm
               e1 = d1
            end if
         else
            d1 = xm
            e1 = d1
         end if
         a1 = b1
         fa = fb
         if (dabs(d1)> tol1) then
            b1 = b1+d1
         else
            b1 = b1+dsign(tol1,xm)
         end if
         call xwidth(b1,v1)
         call pwidth(b1,v1,vn,w1)
         fb = vn-dble(ifd)*3.9d0
      end do
      outval = b1
   end if

   return
end subroutine zbrent

subroutine pside2(dval)
!***********************************************************************
!                 PSIDE2 Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Integrates Over Segments For Which DABS(VN) > VNTEST
!                 Eliminates Overlap of Segments And Useless Integration
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        INPUTS:   Number of The Original Segments
!                  End Points Array of The Segments
!
!        OUTPUT:   The Correction of The Results From PSIDE
!
!        CALLED FROM:   AREAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!---- Set convergence criteria for call to QATR3:
!         NDIM = maximum number of integration steps
!         IMIN = minimum number of integration steps
!         EPSR = relative error tolerance for integral
!         EPST = minimum value threshold for integral
!----
   integer, parameter :: ndim = 12, imin = 5
   double precision, parameter :: epsr = 1.0d-2, epst = 1.0d-5

   integer :: i, j, iseg, npts, nout, icon
   double precision :: dval, temp, u1, u2, uav, ubv, tmpval,&
   &aux(ndim)
   double precision :: ulist(nvmax2), useg(nvmax,2)
   integer :: usign(nvmax), ufac, usegf(nvmax)
   logical :: Ltest1,Ltest2

!     Variable Initializations
   modnam = 'PSIDE2'

   j = 1
   do i = 1, nsegs
      ulist(j) = uasegs(i)
      j = j+1
      ulist(j) = ubsegs(i)
      j = j+1
   end do
   npts = 2*nsegs

   call hpsort(npts,ulist,nvmax2)

   do i = 1, nsegs
      usign(i) = 1
      if (uasegs(i) > ubsegs(i)) then
         usign(i) = -1
         temp = uasegs(i)
         uasegs(i) = ubsegs(i)
         ubsegs(i) = temp
      end if
      if (uasegs(i) == ubsegs(i)) usign(i) = 0
   end do
   iseg = 0

   do i = 2,npts
      u1 = ulist(i-1)
      u2 = ulist(i)
      ufac = 0
!*****
!           compare segment [u1,u2] against each ua,ub
!*****
      if (u1 /= u2) then
         do j = 1, nsegs
            if (u1>=uasegs(j) .and. u2 <= ubsegs(j)) then
               ufac = ufac + usign(j)
            end if
         end do
!*****
!              make table of segments and factors
!*****
         if (ufac /= 0) then
            iseg = iseg+1
            useg(iseg,1) = u1
            useg(iseg,2) = u2
            usegf(iseg) = ufac
         end if
      end if
   end do
!*****
!            CONSOLIDATE SEGMENTS IF iseg>1
!*****
   nsegs = iseg
   if (nsegs > 1) then
      do iseg = 2, nsegs
         Ltest1 = useg(iseg,1) == useg(iseg-1,2)
         Ltest2 = usegf(iseg)*usegf(iseg-1) > 0
         if (Ltest1 .and. Ltest2) then
            usegf(iseg-1) = 0
            useg(iseg,1) = useg(iseg-1,1)
         end if
      end do
   end if
   dval  = 0.0d0
   if (nsegs > 0) then
      do iseg = 1, nsegs
         if (usegf(iseg) /= 0) then
            uav = useg(iseg,1)
            ubv = useg(iseg,2)
            ufac = usegf(iseg)
            call qatr3(uav,ubv,epsr,epst,ndim,imin,tmpval,&
            &icon,nout,aux)
            dval  = dval + dble(ufac)*tmpval
         end if
      end do
   end if

   if( areadbg )then
      write(areadbunt,*)
      write(areadbunt,*) 'AREA Debug Sub_PSIDE2: '
      write(areadbunt,*) '  ISRC   = ',isrc

      if( evonly )then
         write(areadbunt,*) '  IEVT   = ',ievent
      else
         write(areadbunt,*) '  IREC   = ',irec
      endif

      write(areadbunt,*) '  NSEGS  = ',nsegs
      if( nsegs > 0 )then
         write(areadbunt,*) '  UASEGS = ',uasegs(nsegs)
         write(areadbunt,*) '  UBSEGS = ',ubsegs(nsegs)
      else
         write(areadbunt,*) '  UASEGS = NA'
         write(areadbunt,*) '  UBSEGS = NA'
      endif
      write(areadbunt,*) '  DVAL   = ',dval
   endif

   return
end subroutine pside2

subroutine hpsort(nvar,uvar,idim)
!***********************************************************************
!                 HPSORT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: A General Program For Heap Sort of An Array
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        MODIFIED BY R. Brode, PES, Inc. to include a single RETURN,
!                    avoided "no path to statement" messages for
!                    some compilers. - 12/1/97
!
!        INPUTS:  The Array To Be Sorted
!
!        OUTPUTS: The Array Sorted
!
!        CALLED FROM:   PSIDE2
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, idim, nvar, ilmid, ir
   double precision :: ru, uvar(idim)

!     Variable Initializations
   modnam = 'HPSORT'

   ilmid = nvar/2 + 1
   ir = nvar

10 continue

   if (ilmid>1) then
      ilmid = ilmid-1
      ru = uvar(ilmid)
   else
      ru = uvar(ir)
      uvar(ir) = uvar(1)
      ir = ir-1
      if (ir == 1)then
         uvar(1) = ru
!           Processing is done
         go to 999
      end if
   end if

   i = ilmid
   j = ilmid+ilmid

   do while (j<= ir)
      if (j< ir) then
         if (uvar(j)<uvar(j+1) ) j = j+1
      end if
      if (ru<uvar(j)) then
         uvar(i) = uvar(j)
         i = j
         j = 2*j
      else
         j = ir+1
      end if
   end do

   uvar(i) = ru

   go to 10

999 return
end subroutine hpsort

!***  End new code for area source numerical integration algorithm - 7/7/93



!***  Subroutines for OPENPIT Source algorithms - 7/19/94


subroutine escape(icat)
!***********************************************************************
!*                ESCAPE Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Calculate Escape Fractions for a Particle
!*                Size Category
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Index for Particle Size Category Being Processed
!*                Gravitational Settling Velocity for Current
!*                     Particle Size Category & Current Source
!*                10-meter Wind Speed for the Current Hour
!*                Constant ALPHA (= 0.029)
!*
!*
!*       OUTPUTS: The Escape Fraction for the current Size Category
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: icat
!*    Variable Initializations
   modnam = 'ESCAPE'

   efrac(icat) = 1.0d0/(1.0d0 + vgrav(icat) / (alpha * uref10) )

   return
end subroutine escape


subroutine adjemi(icat,qptot)
!***********************************************************************
!*                ADJEMI Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Adjust Emission Rate for Current Particle
!*                Size Category Being Processed
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Index for Particle Size Category Being Processed
!*                Escape Fraction for the Current Size Category
!*                Mass Fraction of the Current Size Category
!*                Total Emission Rate Per Unit Area
!*
!*
!*       OUTPUTS: Adjusted Emission Rate for the Current Size Category
!*                Cumulative Adjusted Emission Rate Over All Categories
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: icat
   double precision :: qptot
!*    Variable Initializations
   modnam = 'ADJEMI'

   qpart(icat) = efrac(icat) * phi(icat) * qs
   qptot = qptot + qpart(icat)

   return
end subroutine adjemi


subroutine amfrac(qptot)
!***********************************************************************
!*                AMFRAC Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Adjust the Mass Fractions for each Particle
!*                Size Category
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Array of Adjusted Emission Rates
!*                Cumulative Adjusted Emission Rate Over All Categories
!*
!*       OUTPUTS: Array of Adjusted Mass Fractions
!*
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: icat
   double precision :: qptot
!*    Variable Initializations
   modnam = 'AMFRAC'

   do icat = 1,npd
      phi(icat) = qpart(icat)/qptot
   end do

   return
end subroutine amfrac


subroutine lwind
!***********************************************************************
!*                LWIND Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Calculate the Along-Wind Length of the OPENPIT
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Wind Flow Vector for the Current Hour
!*                Angle of the Long OPENPIT dimension from the North
!*                Length of the OPENPIT
!*                Width of the OPENPIT
!*
!*       OUTPUTS: Along-Wind Length of the OPENPIT
!*
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   double precision :: thout
   character :: modnam*12

!*    Variable Initializations
   modnam = 'LWIND'
   thout = 0.0d0

!*    Determine the Wind Direction Angle Relative to the Long
!*    Axis of the OpenPit
   call ctheta(afv,palpha,thout)
!*    Assign THOUT to global variable, THETA
   theta = thout

!*    Determine the Along-Wind Length of the OPENPIT
   pitl = pitlen * (1.0d0 - theta/90.d0) + pitwid * (theta / 90.d0)

   return
end subroutine lwind


subroutine pdepth
!***********************************************************************
!*                PDEPTH Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Calculate the Relative Depth of the OPENPIT Source
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Effective Depth of the OPENPIT
!*                Release Height Above
!*                Along Wind Length of the OPENPIT
!*
!*       OUTPUTS: Relative Depth of the OPENPIT
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!*    Variable Initializations
   modnam = 'PDEPTH'

   pdrel = (pdeff-emihgt) / pitl

   return
end subroutine pdepth


subroutine ptfrac
!***********************************************************************
!*                PTFRAC Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Calculate the Fractional Size of the Effective
!*                Area of the OPENPIT Source
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Relative Pit Depth
!*
!*       OUTPUTS: Fractional Size of the Effective Area of the OPENPIT
!*
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!*    Variable Initializations
   modnam = 'PTFRAC'

   if (pdrel >= 0.2d0) then
      pitfra = 0.08d0
   else
      pitfra = dsqrt(1.0d0 - 1.7d0*(pdrel**third) )
   endif

   return
end subroutine ptfrac


subroutine piteff
!**************************************3********************************
!*                PITEFF Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Determine the Coordinates of the OPENPIT Source
!*                in Wind Direction Coordinate System
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!        MODIFIED:   Moved calculation of center of effective area
!                    source for OPENPIT sources from subroutine ARDIST.
!                    Previous versions would have skipped calculation
!                    of center coordinates if first receptor was located
!                    inside the actual OPENPIT source.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
!
!*       INPUTS:
!*
!*       OUTPUTS: Coordinates of the OPENPIT Source in Wind
!*                Direction Coordinate System
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ii, iupwnd
   double precision :: spamin, effang, effwid, efflen
   double precision :: xtemp(nvmax), ytemp(nvmax)
   double precision, parameter :: epslon = 1.0d-05

!*    Variable Initializations
   modnam = 'PITEFF'
   xtemp(:) = 0.0d0
   ytemp(:) = 0.0d0

!*    Get Vertices of Actual Pit in WD-Coordinate System    ---   CALL AVERTS
   call averts(xvert,yvert,xtemp,ytemp,nvert+1)

!*    Find the Upwind Vertex of the Pit (one with minimum X)
   spamin = 1.0d+20
   iupwnd = 0
   do ivert = 1,nvert
      if (xtemp(ivert) < spamin) then
         iupwnd = ivert
         spamin = xtemp(ivert)-epslon
      endif
   end do

!*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
!     specified, otherwise in the MODEL debug file
   if (areadbg) then
      write (areadbunt,*)
      write (areadbunt,*) 'ACTUAL PIT COORDINATES:'
      write (areadbunt,*) '----------------------'
      write (areadbunt,8009)
8009  format (' SYSTEM        X1        Y1        X2        Y2',&
      &'        X3        Y3        X4        Y4'/&
      &' ---------  -------- ---------  -------- ---------',&
      &'  -------- ---------  -------- ---------')
      write (areadbunt,8000) (xvert(ii), yvert(ii), ii=1,nvert)
      write (areadbunt,8100) (xtemp(ii), ytemp(ii), ii=1,nvert)
      write (areadbunt,*)
      write (areadbunt,*) ' UPWIND VERTEX OF THE PIT        = ',&
      &iupwnd
      write (areadbunt,*) ' WIND DIR W.R.T. PIT LONG AXIS   = ',&
      &theta
      write (areadbunt,*) ' ALONGWIND LENGTH OF THE PIT     = ',&
      &pitl
      write (areadbunt,*) ' RELATIVE DEPTH OF THE PIT       = ',&
      &pdrel
      write (areadbunt,*)
8000  format (1x,'User      ',8(f9.0,1x))
8100  format (1x,'Wind-Dir  ',8(f9.0,1x))
   elseif (debug) then
      write (dbgunt,*)
      write (dbgunt,*) 'ACTUAL PIT COORDINATES:'
      write (dbgunt,*) '----------------------'
      write (dbgunt,8009)
      write (dbgunt,8000) (xvert(ii), yvert(ii), ii=1,nvert)
      write (dbgunt,8100) (xtemp(ii), ytemp(ii), ii=1,nvert)
      write (dbgunt,*)
      write (dbgunt,*) ' UPWIND VERTEX OF THE PIT        = ', iupwnd
      write (dbgunt,*) ' WIND DIR W.R.T. PIT LONG AXIS   = ', theta
      write (dbgunt,*) ' ALONGWIND LENGTH OF THE PIT     = ', pitl
      write (dbgunt,*) ' RELATIVE DEPTH OF THE PIT       = ', pdrel
      write (dbgunt,*)
   endif

!*    Determine the Angle of the Effective Pit Relative to North
   effang = angle + (90.d0*dble(iupwnd - 1))

!*    Determine Length and Width Dimensions of the
!*    Effective Pit Area
   effwid = pitfra**(1.0d0 - (dcos(theta*dtorad))**2)*pitwid
   efflen = pitfra**((dcos(theta*dtorad))**2)*pitlen

!*    Calculate the Coordinates of the Vertices of the Effective Pit Area
!*    Set Coordinates of Vertices for Rectangular Area (in Kilometers).
!*    Vertices Start with the "Southwest" Corner and Are Defined
!*    Clockwise.  The First Vertex is Repeated as the Last Vertex.


!*    First determine proper 'x-dim' and 'y-dim' for effective area,
!*    taking into account angle of orientation and relation to actual pit.

   if (xinit <= yinit .and. (iupwnd==1 .or. iupwnd==3)) then
      xeff = effwid
      yeff = efflen
   else if (xinit<=yinit .and. (iupwnd==2 .or. iupwnd==4)) then
      xeff = efflen
      yeff = effwid
   else if (xinit>yinit .and. (iupwnd==1 .or. iupwnd==3)) then
      xeff = efflen
      yeff = effwid
   else if (xinit>yinit .and. (iupwnd==2 .or. iupwnd==4)) then
      xeff = effwid
      yeff = efflen
   end if

   xtemp(1) = xvert(iupwnd)
   ytemp(1) = yvert(iupwnd)

   xtemp(2) = xtemp(1) + (yeff*dsin(effang*dtorad))
   ytemp(2) = ytemp(1) + (yeff*dcos(effang*dtorad))

   xtemp(3) = xtemp(2) + (xeff*dcos(effang*dtorad))
   ytemp(3) = ytemp(2) - (xeff*dsin(effang*dtorad))

   xtemp(4) = xtemp(3) - (yeff*dsin(effang*dtorad))
   ytemp(4) = ytemp(3) - (yeff*dcos(effang*dtorad))

   xtemp(5) = xvert(iupwnd)
   ytemp(5) = yvert(iupwnd)


!*    Calculate Coordinates of the Effective Pit Area in
!*    Wind Direction Coordinate System                      ---   CALL AVERTS
   call averts(xtemp,ytemp,xvert,yvert,nvert+1)

!*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
!     specified, otherwise in the MODEL debug file
   if (areadbg) then
      write (areadbunt,*)
      write (areadbunt,*) 'EFFECTIVE PIT COORDINATES:'
      write (areadbunt,*) '-------------------------'
      write (areadbunt,8009)

      write (areadbunt,8000) (xtemp(ii), ytemp(ii), ii=1,nvert)
      write (areadbunt,8100) (xvert(ii), yvert(ii), ii=1,nvert)
      write (areadbunt,*)

      write (areadbunt,*) ' EFFECTIVE PIT LENGTH            = ',&
      &efflen
      write (areadbunt,*) ' EFFECTIVE PIT WIDTH             = ',&
      &effwid
      write (areadbunt,*) ' ORIENTATION RELATIVE TO NORTH   = ',&
      &effang
      write (areadbunt,*) ' FRACTIONAL SIZE OF EFF PIT AREA = ',&
      &pitfra
   elseif (debug) then
      write (dbgunt,*)
      write (dbgunt,*) 'EFFECTIVE PIT COORDINATES:'
      write (dbgunt,*) '-------------------------'
      write (dbgunt,8009)

      write (dbgunt,8000) (xtemp(ii), ytemp(ii), ii=1,nvert)
      write (dbgunt,8100) (xvert(ii), yvert(ii), ii=1,nvert)
      write (dbgunt,*)

      write (dbgunt,*) ' EFFECTIVE PIT LENGTH            = ', efflen
      write (dbgunt,*) ' EFFECTIVE PIT WIDTH             = ', effwid
      write (dbgunt,*) ' ORIENTATION RELATIVE TO NORTH   = ', effang
      write (dbgunt,*) ' FRACTIONAL SIZE OF EFF PIT AREA = ', pitfra
   endif

!     Reassign Effective Area Coordinates to Global Arrays for Subsequent Calcs.
   do i = 1, 5
      xvert(i) = xtemp(i)
      yvert(i) = ytemp(i)
   end do

! --- Calculate coordinates for center of the effective pit source
!     (formerly done in subroutine ARDIST)
   xcntr = 0.0d0
   ycntr = 0.0d0
   do i = 1, nvert
      xcntr = xcntr + xvert(i)
      ycntr = ycntr + yvert(i)
   end do
   xcntr = xcntr/dble(nvert)
   ycntr = ycntr/dble(nvert)

!*    If DEBUG Requested, Write Out coordinates for center of effective pit source;
!     include in AREA debug file if specified, otherwise in the MODEL debug file
   if (areadbg) then
      write (areadbunt,*)
      write (areadbunt,*) ' X-COORD FOR CENTER OF EFF PIT   = ',&
      &xcntr
      write (areadbunt,*) ' Y-COORD FOR CENTER OF EFF PIT   = ',&
      &ycntr
   elseif (debug) then
      write (dbgunt,*)
      write (dbgunt,*) ' X-COORD FOR CENTER OF EFF PIT   = ',&
      &xcntr
      write (dbgunt,*) ' Y-COORD FOR CENTER OF EFF PIT   = ',&
      &ycntr
   endif

   return
end subroutine piteff


subroutine pitemi(qptot)
!***********************************************************************
!*                PITEMI Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Determine the Emission Rate for the Effective
!*                Pit Area
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 20, 1994
!*
!*       INPUTS:  Fractional Area of the Pit
!*                Total Adjusted Emission Rate
!*
!*       OUTPUTS: Emission Rate for the Effective Area of the Current
!*                OPENPIT Source
!*
!*
!*       CALLED FROM:   OCALC
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: qptot
!*    Variable Initializations
   modnam = 'PITEMI'

   qeff = qptot / pitfra

   return
end subroutine pitemi


subroutine ctheta(afvin,alfin,thout)
!***********************************************************************
!*                CTHETA Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To Determine the Wind Direction Angle Relative to
!*                the Pit Long Axis
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    July 26, 1994
!*
!*       INPUTS:  Flow Vector
!*                Angle of Pit Long Axis from North
!*
!*       OUTPUTS: THETA - Wind Direction Angle Relative to
!*                the Pit Long Axis
!*
!*
!*       CALLED FROM:   LWIND
!***********************************************************************

   implicit none

   double precision :: thout, alfin, afvin, theta

   if (dabs(afvin-alfin) <= 90.0d0) then
      thout = dabs(afvin-alfin)
   else if (afvin > alfin) then
      theta = afvin - alfin
      if (theta > 90.0d0) then
         theta = afvin-180.0d0 - alfin
      endif
      if (theta > 90.0d0) then
         theta = afvin-360.0d0 - alfin
      endif
      if (theta > 90.0d0) then
         theta = afvin-540.0d0 - alfin
      endif
      thout = dabs(theta)
   else if (afvin < alfin) then
      theta = afvin - alfin
      if (theta < -90.0d0) then
         theta = afvin + 180.0d0 - alfin
      endif
      if (theta < -90.0d0) then
         theta = afvin + 360.0d0 - alfin
      endif
      if (theta < -90.0d0) then
         theta = afvin + 540.0d0 - alfin
      endif
      thout = dabs(theta)
   endif

   return
end subroutine ctheta

!-----------------------------------------------------------------------
subroutine qatr3(xl,xu,eps,epst,ndim,imin,y,ier,i,aux)
!-----------------------------------------------------------------------
!
! --- AERMOD    QATR3
!
! PURPOSE:      Integration routine adapted from the IBM SSP program
!               DQATR.  Modified for single precision.  This is a COPY
!               of QATR for use in area source integrations.
!
! ARGUMENTS:
!    PASSED:    xl,xu   lower and upper limits of integration        [r]
!               eps     fractional error used to define convergence  [r]
!               epst    lower theshold check for value of integral   [r]
!               ndim    dimension of array aux (max no. of steps)    [i]
!               imin    minimum number of "steps" for integral       [i]
!               fct     external function (integrand)                [r]
!               aux     working array, passed to allow variable dim. [r]
!  RETURNED:    y       value of integral                            [r]
!               ier     status flag at termination                   [i]
!               i       number of subdivision steps                  [i]
!
! CALLING ROUTINES:     PSIDE, PSIDE2
!
! EXTERNAL ROUTINES:    none
!-----------------------------------------------------------------------

!  NOTES: status flags denote the following --
!               ier=0   value of integral converged to within eps
!               ier=1   value of integral is diverging (not used)
!               ier=2   value of integral did not converge to within
!                       eps before ndim limit was reached

   implicit none

   integer :: ndim, imin, ier
   double precision :: y, eps, epst, xu, xl, h, hh, delt2, p,&
!     JAT D065 8/9/21 DELT1 SET BUT NOT USED
!     &           DELT1, HD, X, SM, Q, AUX(NDIM),
   &hd, x, sm, q, aux(ndim),&
   &p1, p2
   integer :: i, ii, ji, j, jj

!---  Initialize AUX array
   aux(:)  = 0.0d0

!---  Preparations for Romberg loop
   call plumef(xl,p1)
   call plumef(xu,p2)
   aux(1) = 0.5d0 * (p1+p2)
   h = xu - xl

   if(h == 0.0d0 .or. aux(1) == 0.0d0) then
      ier=0
      y  = 0.0d0
      return
   endif

   hh = h
   delt2 = 0.d0
   p  = 1.0d0
   jj = 1

!     JAT D065 8/9/21 DELT1 SET BUT NOT USED
   do i = 2, ndim
      y = aux(1)
!         delt1 = delt2
      hd = hh
      hh = 0.5d0 * hh
      p  = 0.5d0 * p
      x  = xl + hh
      sm  = 0.0d0

      do j = 1, jj
         call plumef(x,p1)
         sm  = sm + p1
         x   = x + hd
      end do

!----    A new approximation to the integral is computed by means
!        of the trapezoidal rule
      aux(i)  = 0.5d0*aux(i-1) + p*sm

!----    Start of Rombergs extrapolation method

      q  = 1.0d0
      ji = i-1
      do j = 1, ji
         ii = i-j
         q  = q+q
         q  = q+q
         aux(ii)  = aux(ii+1) + (aux(ii+1)-aux(ii))/(q-1.0d0)
      end do

!----    End of Romberg step

!        Compute absolute error, delt2
      delt2 = dabs(y-aux(1))

      if (i >= imin) then
!           Check for covergence of algorithm
         if (dabs(aux(1)) < epst) then
!              Lower threshold convergence test
            ier = 0
            y  = h*aux(1)
            return
         else if (delt2 <= eps*dabs(aux(1)) ) then
!              Relative error convergence test
            ier = 0
            y  = h*aux(1)
            return
         else if (dabs(hh) < 1.0d0) then
!              Minimum "delta-x" convergence test; < 1.0m
            ier = 0
            y  = h*aux(1)
            return
         end if
      end if

      jj = jj+jj
   end do

!     Convergence not reached within maximum number of steps
   ier = 2
   y  = h*aux(1)

   return
end subroutine qatr3

subroutine pside_tox(u1,u2,dval)
!***********************************************************************
!                 PSIDE_TOX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        Special version of PSIDE optimized for TOXICS applications.
!        Utilizes Romberg Integration (QATR3) or Gaussian Quadrature (QG2)
!        depending on the source receptor geometry.
!
!        PURPOSE: INTEGRATES SIDE K of POLYGON
!                 int f(u)*CNF(v(u)/sig(u))=f(u)*vn(u) from u1 to u2
!                           CNF = cumulative normal distribution
!                 Computes W(1),W(2)--normalized plume width at   u1    u2
!                 Checks for w(i) outside of -3.9,3.9 with i+, i-
!                 L=-3.9  U=3.9  = bounds for testing
!                 Integrates according to case encountered:
!                 situation     CASE    iplus    iminus  integral limits
!                 L<w1,w2<U      1        0        0         u1,u2
!                 w1,w2<L        2        0       1+2      don't compute
!                 w1,w2>U        3       1+2       0         u1,u2
!                 w1<L<w2<U      4        0        1         u-,u2
!                 w2<L<w1<U      5        0        2         u1,u-
!                 L<w1<U<w2      6        2        0       u1,u+  u+,u2
!                 L<w2<U<w1      7        1        0       u1,u+  u+,u2
!                 w1<L<U<w2      8        2        1       u-,u+  u+,u2
!                 w2<L<U<w1      9        1        2       u1,u+  u+,u-
!
!                 u+ = value of u such that w(u)=U=3.9
!                 u- =     "                w(u)=L=-3.9
!                 u+,u- computed with Brent's Algorithm
!
!                 IF uplus >0, part of side is outside plume
!                 but is integrated anyway, unless there is
!                 a corresponding part on another side that will
!                 cause cancellation.  This is determined in
!                 PSIDE2;
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        MODIFIED by Roger Brode, PES, Inc. to use QATR routine for
!                    integration, and to pass VN(1) and VN(2) to
!                    ZBRENT.  Also changed to use -3.9 to +3.9 for
!                    width of plume for internal consistency with
!                    SUBROUTINE PWIDTH. - 12/14/98
!
!        MODIFIED by Roger Brode, PES, Inc. to correct lower integration
!                    limit for Case 4, and to remove extraneous calls
!                    to XWIDTH and PWIDTH after calls to ZBRENT. - 7/29/94
!
!        INPUTS:  End Points of The Segments
!
!        OUTPUTS: Integral Value (if any) for Segment
!
!        CALLED FROM:   AREAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!---- Set convergence criteria for calls to QATR3:
!         NDIM = maximum number of integration steps
!         IMIN = minimum number of integration steps
!         EPSR = relative error tolerance for integral
!         EPST = minimum value threshold for integral
!----
   integer, parameter :: ndim = 10, imin = 4
   double precision, parameter :: epsr = 2.0d-2, epst = 1.0d-5

!---- Set distance factor for switching to Gaussian Quadrature, QG_FACT
!     If Xmax - Xmin is .LT. QG_FACT*Xmin, then use QG2, where
!     Xmax and Xmin are the distances to the endpoints of the side.
   double precision, parameter :: qg_fact = 5.0d0

   integer :: icase
   integer :: i, ks, iminus, iplus, nout, icon
   double precision :: dval, u1, u2, uminus, uplus, aux(ndim),&
!     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
!     &                    u(2), v1(2), vn(2), w(2)
   &u(2), vn(2)

!     Variable Initializations
   modnam = 'PSIDE_TOX'

!     NSEG = number of segments; set to 0 in AREAIN
!     for each source/rcvr/time step
   dval  = 0.0d0
!     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
   do i =  1,2
      ks    = ivert + i-1
      u(i)  = uvert(ks)
!         v1(i) = vvert(ks)
      vn(i) = vnvert(ks)
!         w(i)  = wvert(ks)
   end do

   iminus = 0
   iplus  = 0
   uminus = -1.0d0
   uplus  = -1.0d0
   do i = 1,2
      if (vn(i) < -3.9d0) iminus = i + iminus
      if (vn(i) >  3.9d0) iplus  = i + iplus
   end do

   if (iplus==1 .or. iplus==2) then
      call zbrent( 1,u(1),u(2),vn(1),vn(2),1.0d-3,uplus)
   end if
   if (iminus==1 .or. iminus==2) then
      call zbrent(-1,u(1),u(2),vn(1),vn(2),1.0d-3,uminus)
   end if

   if( areadbg) then
      write(areadbunt,*)
      write(areadbunt,*) 'AREA Debug Sub_PSIDE_TOX: '
      write(areadbunt,*) '  ISRC   = ',isrc

      if( evonly )then
         write(areadbunt,*) '  IEVT   = ',ievent
      else
         write(areadbunt,*) '  IREC   = ',irec
      endif

      write(areadbunt,*) ' iplus  iminus  case'
      if( iplus==0 .and. iminus==0 )then
         icase = 1
      elseif( iplus==0 .and. iminus==3 )then
         icase = 2
      elseif( iplus==0 .and. iminus==1 )then
         icase = 4
      elseif( iplus==0 .and. iminus==2 )then
         icase = 5
      elseif( iplus==1 .and. iminus==0 )then
         icase = 7
      elseif( iplus==1 .and. iminus==2 )then
         icase = 9
      elseif( iplus==2 .and. iminus==0 )then
         icase = 6
      elseif( iplus==2 .and. iminus==1 )then
         icase = 8
      elseif( iplus==3 .and. iminus==0 )then
         icase = 3
      endif
      write(areadbunt,'(I5,I7,I6)') iplus, iminus, icase
      write(areadbunt,*)
   endif

!---- CASE DEPENDs on iplus, iminus
   if (iplus == 0) then
      if (iminus == 0) then
!                                             iplus  iminus  case
!                                               0     0       1
         if (dabs(u2-u1) < qg_fact*min(u1,u2)) then
            call qg2(u1,u2,dval)
         else
            call qatr3(u1,u2,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      else if (iminus == 3) then
!                                               0     3       2
         dval = 0.0d0
      else if (iminus == 1) then
!                                               0     1       4
         if (dabs(u2-uminus) < qg_fact*min(uminus,u2)) then
            call qg2(uminus,u2,dval)
         else
            call qatr3(uminus,u2,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      else
!                                               0     2       5
         if (dabs(uminus-u1) < qg_fact*min(u1,uminus)) then
            call qg2(u1,uminus,dval)
         else
            call qatr3(u1,uminus,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      end if

   else if (iplus == 1) then
      nsegs = nsegs+1
      uasegs(nsegs) = u1
      ubsegs(nsegs) = uplus
      if (iminus == 0) then
!                                               1     0       7
         if (dabs(u2-uplus) < qg_fact*min(uplus,u2)) then
            call qg2(uplus,u2,dval)
         else
            call qatr3(uplus,u2,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      else
!                                               1     2       9
         if (dabs(uminus-uplus) < qg_fact*min(uplus,uminus)) then
            call qg2(uplus,uminus,dval)
         else
            call qatr3(uplus,uminus,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      end if

   else if (iplus == 2) then
      nsegs = nsegs+1
      uasegs(nsegs) = uplus
      ubsegs(nsegs) = u2
      if (iminus == 0) then
!                                               2     0       6
         if (dabs(uplus-u1) < qg_fact*min(u1,uplus)) then
            call qg2(u1,uplus,dval)
         else
            call qatr3(u1,uplus,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      else
!                                               2     1       8
         if (dabs(uplus-uminus) < qg_fact*min(uminus,uplus)) then
            call qg2(uminus,uplus,dval)
         else
            call qatr3(uminus,uplus,epsr,epst,ndim,imin,dval,&
            &icon,nout,aux)
         end if
      end if

   else
!                                               3     0       3
      nsegs = nsegs+1
      uasegs(nsegs) = u1
      ubsegs(nsegs) = u2
   end if

   if( areadbg )then
      write(areadbunt,*)
      write(areadbunt,*) '  ISRC   = ',isrc

      if( evonly )then
         write(areadbunt,*) '  IEVT   = ',ievent
      else
         write(areadbunt,*) '  IREC   = ',irec
      endif
      write(areadbunt,*) '  NSEGS  = ',nsegs
      if( nsegs > 0 )then
         write(areadbunt,*) '  UASEGS = ',uasegs(nsegs)
         write(areadbunt,*) '  UBSEGS = ',ubsegs(nsegs)
      else
         write(areadbunt,*) '  UASEGS = NA'
         write(areadbunt,*) '  UBSEGS = NA'
      endif
      write(areadbunt,*) '  DVAL   = ',dval
   endif

   return
end subroutine pside_tox

subroutine pside2_tox(dval)
!***********************************************************************
!                 PSIDE2_TOX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        Special version of PSIDE2_TOX optimized for TOXICS applications.
!        Utilizes Romberg Integration (QATR3) or Gaussian Quadrature (QG2)
!        depending on the source receptor geometry.
!
!        PURPOSE: Integrates Over Segments For Which DABS(VN) > VNTEST
!                 Eliminates Overlap of Segments And Useless Integration
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!                    Adapted From Codes By Richard Strelitz, CSC
!
!        DATE:    July 7, 1993
!
!        INPUTS:   Number of The Original Segments
!                  End Points Array of The Segments
!
!        OUTPUT:   The Correction of The Results From PSIDE
!
!        CALLED FROM:   AREAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!---- Set convergence criteria for call to QATR3:
!         NDIM = maximum number of integration steps
!         IMIN = minimum number of integration steps
!         EPSR = relative error tolerance for integral
!         EPST = minimum value threshold for integral
!----
   integer, parameter :: ndim = 10, imin = 4
   double precision, parameter :: epsr = 2.0d-2, epst = 1.0d-5

!---- Set distance factor for switching to Gaussian Quadrature, QG_FACT
!     If Xmax - Xmin is .LT. QG_FACT*Xmin, then use QG2, where
!     Xmax and Xmin are the distances to the endpoints of the side.
   double precision, parameter :: qg_fact = 5.0d0

   integer :: i, j, iseg, npts, nout, icon
   double precision :: dval, temp, u1, u2, uav, ubv, tmpval,&
   &aux(ndim)
   double precision :: ulist(nvmax2), useg(nvmax,2)
   integer :: usign(nvmax), ufac, usegf(nvmax)
   logical :: Ltest1,Ltest2

!     Variable Initializations
   modnam = 'PSIDE2_TOX'

   j = 1
   do i = 1, nsegs
      ulist(j) = uasegs(i)
      j = j+1
      ulist(j) = ubsegs(i)
      j = j+1
   end do
   npts = 2*nsegs

   call hpsort(npts,ulist,nvmax2)

   do i = 1, nsegs
      usign(i) = 1
      if (uasegs(i) > ubsegs(i)) then
         usign(i) = -1
         temp = uasegs(i)
         uasegs(i) = ubsegs(i)
         ubsegs(i) = temp
      end if
      if (uasegs(i) == ubsegs(i)) usign(i) = 0
   end do
   iseg = 0

   do i = 2,npts
      u1 = ulist(i-1)
      u2 = ulist(i)
      ufac = 0
!*****
!           compare segment [u1,u2] against each ua,ub
!*****
      if (u1 /= u2) then
         do j = 1, nsegs
            if (u1>=uasegs(j) .and. u2 <= ubsegs(j)) then
               ufac = ufac + usign(j)
            end if
         end do
!*****
!              make table of segments and factors
!*****
         if (ufac /= 0) then
            iseg = iseg+1
            useg(iseg,1) = u1
            useg(iseg,2) = u2
            usegf(iseg) = ufac
         end if
      end if
   end do
!*****
!            CONSOLIDATE SEGMENTS IF iseg>1
!*****
   nsegs = iseg
   if (nsegs > 1) then
      do iseg = 2, nsegs
         Ltest1 = useg(iseg,1) == useg(iseg-1,2)
         Ltest2 = usegf(iseg)*usegf(iseg-1) > 0
         if (Ltest1 .and. Ltest2) then
            usegf(iseg-1) = 0
            useg(iseg,1) = useg(iseg-1,1)
         end if
      end do
   end if
   dval  = 0.0d0
   if (nsegs > 0) then
      do iseg = 1, nsegs
         if (usegf(iseg) /= 0) then
            uav = useg(iseg,1)
            ubv = useg(iseg,2)
            ufac = usegf(iseg)
            if (dabs(ubv-uav) < qg_fact*min(uav,ubv)) then
               call qg2(uav,ubv,tmpval)
            else
               call qatr3(uav,ubv,epsr,epst,ndim,imin,tmpval,&
               &icon,nout,aux)
            end if
            dval  = dval + dble(ufac)*tmpval
         end if
      end do
   end if

   if( areadbg )then
      write(areadbunt,*)
      write(areadbunt,*) 'AREA Debug Sub_PSIDE2_TOX: '
      write(areadbunt,*) '  ISRC   = ',isrc

      if( evonly )then
         write(areadbunt,*) '  IEVT   = ',ievent
      else
         write(areadbunt,*) '  IREC   = ',irec
      endif
      write(areadbunt,*) '  NSEGS  = ',nsegs
      if( nsegs > 0 )then
         write(areadbunt,*) '  UASEGS = ',uasegs(nsegs)
         write(areadbunt,*) '  UBSEGS = ',ubsegs(nsegs)
      else
         write(areadbunt,*) '  UASEGS = NA'
         write(areadbunt,*) '  UBSEGS = NA'
      endif
      write(areadbunt,*) '  DVAL   = ',dval
   endif

   return
end subroutine pside2_tox

!
!     ..................................................................
!
!        SUBROUTINE QG2
!
!        PURPOSE
!           TO COMPUTE INTEGRAL(FCT(X), SUMMED OVER X FROM XL TO XU)
!
!        USAGE
!           CALL QG3 (XL,XU,FCT,Y)
!           PARAMETER FCT REQUIRES AN EXTERNAL STATEMENT
!
!        DESCRIPTION OF PARAMETERS
!           XL     - THE LOWER BOUND OF THE INTERVAL.
!           XU     - THE UPPER BOUND OF THE INTERVAL.
!           FCT    - THE NAME OF AN EXTERNAL FUNCTION SUBPROGRAM USED.
!           Y      - THE RESULTING INTEGRAL VALUE.
!
!        REMARKS
!           NONE
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           THE EXTERNAL FUNCTION SUBPROGRAM FCT(X) MUST BE FURNISHED
!           BY THE USER.
!
!        METHOD
!           EVALUATION IS DONE BY MEANS OF 2-POINT GAUSS QUADRATURE
!           FORMULA, WHICH INTEGRATES POLYNOMIALS UP TO DEGREE 3
!           EXACTLY.
!           FOR REFERENCE, SEE
!           V.I.KRYLOV, APPROXIMATE CALCULATION OF INTEGRALS,
!           MACMILLAN, NEW YORK/LONDON, 1962, PP.100-111 AND 337-338.
!
!     ..................................................................
!
subroutine qg2(xl,xu,y)
!
!
   implicit none

   double precision :: a, b, y, xl, xu, p1, p2

   a = 0.5d0*(xu+xl)
   b = xu-xl
   y = 0.2886751d0*b
   call plumef(a+y,p1)
   call plumef(a-y,p2)
   y = 0.5d0*b*(p1+p2)

   return
end subroutine qg2
