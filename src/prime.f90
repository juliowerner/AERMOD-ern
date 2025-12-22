!----------------------------------------------------------------------
subroutine prime1
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                PRIME1
!                J. Scire, D. Strimaitis, EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Initialize the variables used by the PRIME
!               building downwash algorithm
!
! --- INPUTS:  none
!
! --- OUTPUT:
!
!     Common block /DFSN/ variables:
!           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
!           dua_ua,xdecay,xdecayi,
!           rurliz,rurliy,urbniz,urbniy
!
! --- PRIME1 called by:  MAIN (Host)
! --- PRIME1 calls:      none
!----------------------------------------------------------------------

! --- Include common blocks
   use main1, only: L_ORD_Turb
   use PRIME_dfsn

   implicit none

! -----------------------
! --- /DFSN/ variables
! -----------------------

! --- Set the factor for defining when turb Approaches Asymptotic
! --- value, and also define the maximum allowed scaled distance
   afac   = 1.3d0
   xbyrmax= 15.0d0

! --- Turbulence intensities in wake (from Briggs rural curves)
   wiz0 = 0.06d0

! --- ORD_DWNW option - new limit on turbulence intensity
   if (L_ORD_Turb) then
      wiz0 = 0.07d0                                             ! EMM
   end if

   wiy0 = 0.08d0

! --- Wake Factors for sigw and sigv from Weil (1996)
   wfz = 1.7d0
   wfy = 1.7d0
! --- deltaU0/U0
   dua_ua = 0.7d0
! --- Power-law exponent for turbulence intensity change in distance
   xdecay  = 2.0d0/3.0d0
   xdecayi = 1.5d0

   return
end subroutine prime1

!----------------------------------------------------------------------
subroutine numpr1
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                NUMPR1
!                J. Scire, EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Initialize the variables used by the numerical
!               plume rise algorithm
!
! --- INPUTS:
!
!       Parameters:
!           MXENT, MXENTP1, MXNZ, MXNZP1, IO6
!
! --- OUTPUT:
!
!       Common block /NUMPARM/ variables:
!          GRAVI,RGAS,ZMIN,DS,NSTEP,SLAST,RP,ALPHAP(mxent),
!          BETAP(mxent),XCAT(mxentp1),NENT
!       Common block /AMBIENT/ variables:
!          ADIA,PTGRAD0,ZGPTA(mxnz),ZFACEA(mxnzp1)
!
! --- NUMPR1 called by:  MAIN (Host)
! --- NUMPR1 calls:      none
!----------------------------------------------------------------------

! --- Include common blocks
   use PRIME_params
   use PRIME_numparm
   use PRIME_ambient
   use main1, only : l_awma_entrain, AWMA_Beta_EntrainCoeff

   implicit none

   double precision :: dz

   integer :: nzap1, nn, i

! -----------------------
! --- /NUMPARM/ variables
! -----------------------
!
! --- Set the acceleration due to gravity (m/s**2)
   gravi=9.80616d0

! --- Set the gas constant (m**2/s**2/deg. K)
   rgas=287.026d0

! --- Set the minimum plume centerline height (m)
   zmin=0.001d0

! --- Set the step size (m) in the numerical plume rise algorithm
   ds=1.0d0

! --- Set the internal save frequency of plume rise calculations (i.e.,
!     every DS*NSTEP meters) (NOTE: this the frequency with which the
!     results are saved internally -- not that passed back from the
!     NUMRISE routine)
   nstep=1

! --- Set the termination distance (m) of the plume rise calculation
   slast=5000.0d0

! --- Set the radiation coefficient (kg/m**2/deg. K**3/s)
   rp=9.1d-11

! --- Set the perturbed entrainment coefficients
!     ALPHAP (parallel direction), BETAP (normal direction)
   nent=0
   alphap(1)=0.11d0

! --- AWMA version D20350
!     Change entrainment constant from 0.6 to 0.35, which is set in modules.f,
!       if the ALPHA option AWMAEntrain downwash option was set
   if (.not. l_awma_entrain) then
      betap(1) = 0.6d0
   else
      betap(1) = AWMA_Beta_EntrainCoeff
!         betap(1) = 0.35d0
   end if

   xcat(1)=-9.0d9
   xcat(2)= 9.0d9

! -----------------------
! --- /AMBIENT/ variables
! -----------------------

! --- Set dry adiabatic lapse rate (deg. K/m)
   adia=0.00977d0

! --- Set minimum potential temperature lapse rate (deg. K/m)
   ptgrad0=0.0d0

! --- Set the default number of layers
   nza=45
   nzap1=nza+1
   if(nza>mxnz)then
      write(io6,*)'ERROR in SUBR. NUMPR1 -- NZA is too large -- ',&
      &'NZA = ',nza,' MXNZ = ',mxnz
      stop
   endif
   if(nzap1>mxnzp1)then
      write(io6,*)'ERROR in SUBR. NUMPR1 -- NZAP1 is too large -- ',&
      &'NZAP1 = ',nzap1,' MXNZP1 = ',mxnzp1
      stop
   endif

! --- Define the meteorological grid
! --- Set grid points every 10 m from 10-200 m
   dz=10.d0
   nn=1
   zgpta(nn)=dz
   do i=2,20
      nn=nn+1
      zgpta(nn)=zgpta(nn-1)+dz
   enddo
! --- Set grid points every 50 m from 250-500 m
   dz=50.d0
   do i=21,26
      nn=nn+1
      zgpta(nn)=zgpta(nn-1)+dz
   enddo
! --- Set grid points every 100 m from 600-2000 m
   dz=100.d0
   do i=27,41
      nn=nn+1
      zgpta(nn)=zgpta(nn-1)+dz
   enddo
! --- Set grid points every 500 m from 2500-4000 m
   dz=500.d0
   do i=42,45
      nn=nn+1
      zgpta(nn)=zgpta(nn-1)+dz
   enddo

! --- Compute the cell face heights from the grid point values
   zfacea(1)=0.0d0
   do i=2,nza
      zfacea(i)=0.5d0*(zgpta(i)+zgpta(i-1))
   enddo
   zfacea(nzap1)=zgpta(nza)+0.5d0*(zgpta(nza)-zgpta(nza-1))

   return
end subroutine numpr1

!----------------------------------------------------------------------
!MACTEC      subroutine numrise(ldbhr,h,reff,texit,wexit,
!MACTEC     &                   ntr,xtr,ytr,ztr,rtr,linwake,numwake)
subroutine numrise(ldbhr,h,reff,texit,wexit,ntr,capped,horiz,&
&capfact,xtr,ytr,ztr,rtr,linwake,numwake,ierr,&
&dbgunt)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  980310               NUMRISE
!                X.(J.) Zhang, J. Scire, D. Strimaitis,  EARTH TECH
!
!                Adapted from CALPUFF routine NUMRISE
!                for EPRI under contract WO3527-01
!
! --- PURPOSE:  Compute plume rise using a numerical solution to the
!               Non-Boussinesq conservation laws.  Model allows:
!
!               (1) arbitrary ambient temperature stratifications
!               (2) arbitrary uni-directional wind stratification
!               (3) any size of finite emission source
!               (4) is free of the Boussinesq approximation
!               (5) radiative heat loss
!
!               Concurrently, compute diffusion (sigmas) in bldg wake
!               and determine plume/cavity interaction
!
! --- MODIFIED: To include error code (ierr) to track runtime error
!               associated with NN > MXNW.
!               R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
! --- MODIFIED: To incorporate BETA-test draft options for capped stack
!               (POINTCAP) and horizontal releases (POINTHOR). The POINTHOR
!               option currently assumes that horizontal release is
!               aligned with the wind direction for each hour.
!               R.W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
!               R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
!
! --- MODIFIED: To resolve potential problem with some compilers due to the
!               variable xbi not being defined if LINWAKE is .FALSE.
!               The resolution was to split an IF-THEN statement.
!               R.W. Brode, PES, Inc. - 04/08/02
!
! --- MODIFIED: For use with the AERMOD model.  Modified the critical value
!               of PHI from 45 degrees to 20 degrees.  Replaced calls to
!               SIGY and SIGZ with calls to SIGYPR and SIGZPR.  Added call
!               to new WAKE_DFSN2 routine after plume height array is
!               complete to obtain final sigma tables and virtual source
!               terms.  Also returns linwake and numwake to calling routine.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!         LDBHR - logical       - Flag for debug write statements
!             H - real          - Release height (m)
!          REFF - real          - Effective radius of release (m)
!            TP - real          - Exit temperature (deg. K)
!         WEXIT - real          - Exit velocity (m/s)
!           NTR - integer       - Number of points in trajectory passed
!                                 back to calling program (final point
!                                 is "final rise")
!        CAPPED - logical       - Flag for capped stacks
!        HORIZ  - logical       - Flag for horizontal releases
!
!     Common block /AMBIENT/ variables:
!           NZA,UAMB(mxnz),RAMB(mxnz),DEDZ(mxnzp1),TAMB(mxnz),
!           ZFACEA(mxnzp1),TAMB0,RAMB0
!     Common block /NUMPARM/ variables:
!           ZMIN, DS, NSTEP, SLAST, GRAVI
!     Common block /WAKEDAT/ variables:
!           HB, WB, XLB, RB, HR, XLR, XLC, XBADJ, YBADJ
!     Parameters:
!           MXNZ, MXNZP1, MXENT, MXENTP1, IO6
!
! --- OUTPUT:
!        XTR(ntr) - real          - Downwind distance from source (m)
!        YTR(ntr) - real          - Crosswind distance from source (m)
!        ZTR(ntr) - real          - Plume centerline height (m)
!        RTR(ntr) - real          - Plume radius (m)
!        IERR     - integer       - Error code, = 1 if NN > MXNW
!
!     Common block /WAKEDAT/ variables:
!           FQCAV
!
! --- NUMRISE called by:  PHEFF (HOST subroutine)
! --- NUMRISE calls:      ZMET,LUMP,RATE,MARCHING,UNLUMP
!                         ZSTREAM, POSITION, WAKE_U
!                         WAKE_DRDX, WAKE_DFSN, SIGZPR, SIGYPR,
!                         WAKE_FQC, WAKE_DFSN2
!----------------------------------------------------------------------
! --- Notation --- in (KG,M,S) units
!               NN:     Number of points along rise trajectory
!               X:      PLUME LOCATION (downwind from source)
!               Y:      PLUME LOCATION (crosswind from source)
!               Z:      PLUME HEIGHT
!               H:      Release height (flame ht., stack ht.)
!               ZE:     PLUME EQUILIBRIUM HEIGHT
!               S:      LENGTH ALONG PLUME CENTERLINE
!               R:      PLUME RADIUS
!               U:      PLUME HORIZONTAL VELOCITY
!               W:      PLUME VERTICAL VELOCITY
!               USC:    VELOCITY ALONG PLUME CENTERLINE
!               PHI:    ANGLE BETWEEN PLUME TRAJECTORY AND GROUND
!               TP:     PLUME TEMPERATURE
!               ua:     HORIZONTAL WIND SPEED
!               dudz:   WIND SHEAR
!               ta:     AMBIENT TEMPERATURE
!               dpdz:   AMBIENT POTENTIAL TEMPERATURE GRADIENT
!               ramb:   AMBIENT DENSITY
!               ra:     PLUME DENSITY
!               zmin:   Minimum plume centerline height (m)
!               ds:     Step size (m) in the numerical plume rise calc.
!               nstep:  Reporting frequency of numerical calc.
!               slast:  Termination distance (m) of plume rise calc.
!               gravi:  Acceleration due to gravity (m/s**2)
!----------------------------------------------------------------------
! --- Include files
   use PRIME_params
   use PRIME_ambient
   use PRIME_numparm
   use PRIME_wakedat
   use prime_plu
   use prm2_wakedat, only: dfsn2call, xtr_sav, ztr_sav
   use main1, only:  l_awma_uturb, l_awma_uturbhx

   implicit none

   integer :: ntr
   double precision :: xtr(ntr),ytr(ntr),ztr(ntr),rtr(ntr)

   double precision :: xt(mxnw),yt(mxnw),zt(mxnw),rt(mxnw)
   double precision :: rhs(7),rhstemp(7),f(7),ftemp(7)
   double precision :: h, max, reff, wexit, texit, drdxa, ds0, zcumul,&
   &r15src, ua0, ra, ta, dudz0, dpdz, xb,yb, zb,&
   &dufac, ua, dudz, deltat, fdum, fb, fm,&
   &uam, wbyu, xmaxm, xmax, xmaxb, znf, znf0, dmin,&
   &xnp, dxds, dzdx, dzstrm, dzds, dyds, ufac, zc,&
   &deltaz, xbi, base, rise, bidsq, szi, syi, zfin,&
   &yfin, xfin, rfin, xfin0, x15r, dsfin, dx15r, xbb,&
   &xbe, dzdxb, dzdxe, deln, rn, delzfin


   integer :: i, numwake, ierr, ipositn, nnp, np, nn, n15r, nbeg, nend,&
   &in, ipos, jn, it, dbgunt

   logical :: ldb,ldbnn,ldbu,ldbhr,linwake
! MACTEC Begin change
! --- Specify CAPFACT parameter, factor by which initial effective
!     radius of plume is increased by raincap.
   double precision :: capfact
   logical :: capped, horiz
! MACTEC End change

! --- Use LDB as a local switch for more extensive debug output
   ldb=ldbhr
! !!!      ldb=.FALSE.
! !!!      ldbu=ldb
   ldbu=.false.

   ierr = 0
   nn = 0

   linwake=.false.
   x=0.0d0
   y=0.0d0
   z=max(h,zmin)
   s=0.0d0
   r=reff
   u=0.000002d0
   w=wexit
   tp=texit
   drdxa=0.0d0
   ipositn=4

! --- Store stepping length
   ds0=ds

! --- Introduce ZCUMUL to track change in the vertical coordinate of the
! --- trajectory that arises from streamline inclination from horizontal
! --- This adjustment is applied outside of the system of rise equations
! --- out to a distance of 15R from downwind face of building
   zcumul=0.0d0
   r15src=xbadj+(xLb+15.d0*Rb)

! --- Get met. variables at release height
   call zmet(z,ua0,ra,ta,dudz0,dpdz)

! --- Apply reduction in wake wind speed
   xb=x-xbadj
   yb=y-ybadj
   zb=max(z,zmin)
   call position(xb,yb,zb,ipositn)
   ufac=1.0d0
   dufac=0.0d0

   if(ipositn<4) then
      call wake_u(.false.,xb,yb,zb,ufac,dufac,dbgunt)
   end if
   ua=ufac*ua0
   dudz=ufac*dudz0+dufac*ua0

! --- Use Briggs plume rise estimates to set distance scale
! --- Compute initial buoyancy flux (m**4/s**3)
   deltat=max(tp-ta,0.0d0)
   fdum=w*r*r/tp
   fb=gravi*fdum*deltat
! --- Compute momentum flux (m**4/s**2)
   fm=w*fdum*ta

! MACTEC Begin change
   if (capped) then
! ---    Adjust vertical and horizontal velocities of plume to account
! ---    for influence of cap; recalc FM
      r = capfact * reff
      u = wexit / (capfact**2)
      w = 0.001d0
      fm=w*w*r*r*ta/tp
   else if (horiz) then
! ---    Adjust vertical and horizontal velocities of plume to account
! ---    for horizontal release; recalc FM
      u = wexit
      w = 0.001d0
      fm=w*w*r*r*ta/tp
   end if
! MACTEC End change

! --- Final neutral rise distance
   uam=max(ua,1.0d0)
! --- Momentum only: (do not base xmax on case where w<uam)
   wbyu=max(1.0d0,w/uam)
   xmaxm=8.0d0*r*wbyu*(1.0d0+3.0d0/wbyu)**2
   if(fb<=0.0d0)then
! ---    No buoyancy, momentum only
      xmax=xmaxm
   elseif(fb<55.0d0)then
! ---    Buoyancy flux < 55 m**4/s**3
      xmaxb=49.0d0*fb**0.625d0
      xmax=max(xmaxm,xmaxb)
   else
! ---    Buoyancy flux .GE. 55 m**4/s**3
      xmax=119.d0*fb**0.4d0
   endif

! --- Use Briggs neutral rise to identify "minimal rise" cases
! --- Compute Briggs neutral final rise
   if(fb<=0.0d0) then
! ---    No buoyancy, momentum only
      znf=6.0d0*r*w/uam
   elseif(fb<55.0d0) then
! ---    Buoyancy flux < 55 m**4/s**3
      znf=21.425d0*(fb**0.75d0)/uam
   else
! ---    Buoyancy flux .GE. 55 m**4/s**3
      znf=38.71d0*(fb**0.60d0)/uam
   endif
! --- Set minimum rise to 0.1 m
   znf0=max(0.1d0,znf)

! --- Guard against step length greater than likely rise
   dmin=0.5d0*znf0
   if(ds>dmin) then
      ds=dmin
      if(ldb) then
         write(dbgunt,*)'NUMRISE - initial step reset'
         write(dbgunt,'(1x,a,3(f12.4,2x))')'znf,ds0,ds  :',znf,ds0,ds
      endif
   endif

! --- INDIRECT VARIABLES
   usc=dsqrt(u*u+w*w)
   phi=datan(w/u)
! --- PARAMETERS
   np=nstep
   xnp=dble(np)
   nnp=1

! --- START MARCHING LOOP
   den=ra*ta/texit
   call lump(ua,ta,f)

999 continue

! --- Set local debug logical
   if(nnp<150) then
      ldbnn=ldb
   else
      ldbnn=.false.
   endif

! --- Define coordinates of plume relative to bldg.-defined origin
   xb=x-xbadj
   yb=y-ybadj
   zb=max(z+zcumul,zmin)
! --- Obtain mean streamline slopes here (within 15R of building)
   dxds=0.0d0
   dzds=0.0d0
   dzdx=0.0d0
   dzstrm=0.0d0
   call position(xb,yb,zb,ipositn)
   if(ipositn>2 .and. x<=r15src) then
      call zstream(hb,wb,xLb,rb,xLr,hr,xb,yb,zb,dzdx)
      dxds=u/usc
      dzds=dzdx*dxds
      dzstrm=dzds*ds
   endif
! --- Define the crosswind velocity component = zero
   dyds=0.0d0
   v=0.0d0

! --- Compute RHS of rate equations for this location
   call rate(ua,dudz,ra,dpdz,ta,drdxa,rhs)

! --- PREDICTOR MARCHING
   call marching(f,ftemp,rhs,ds)
   call unlump(ua,ta,ra,ftemp)

! --- Extract met and apply reduction in wake wind speed
   zb=max(z+zcumul+dzstrm,zmin)
   call zmet(zb,ua0,ra,ta,dudz0,dpdz)
   call position(xb,yb,zb,ipositn)
   ufac=1.0d0
   dufac=0.0d0

   if(ipositn<4) then
      call wake_u(ldbu,xb,yb,zb,ufac,dufac,dbgunt)
   end if
   ua=ufac*ua0
   dudz=ufac*dudz0+dufac*ua0
   call rate(ua,dudz,ra,dpdz,ta,drdxa,rhstemp)

! --- CORRECTOR
   do i=1,7
      rhs(i)=0.5d0*(rhstemp(i)-rhs(i))
   enddo
   call marching(ftemp,f,rhs,ds)
   call unlump(ua,ta,ra,f)

! --- Compute incremental change in plume height to account for
! --- streamline ascent/descent, and add to cumulative change
   zcumul=zcumul+dzstrm
! --- Apply cumulative adjustment to plume height
   zc=max(z+zcumul,zmin)
! --- Define coordinates of plume relative to bldg.-defined origin
   xb=x-xbadj
   yb=y-ybadj
   zb=zc
   call position(xb,yb,zb,ipositn)

! --- Numerical procedure may result in small negative downwind
! --- distance:  reset to zero and go to next step
   if(x<0.0d0) then
      x=0.0d0
      s=s+ds
      nnp=nnp-1
      goto 96
   endif

! --- Write debug output if in debug mode
   if(ldbnn)then
      if(mod(nnp,1000)==1) write(dbgunt,112)
112   format(/3x,'NNP',7x,'X',6x,'Y',6x,'Z',6x,'R',6x,'U',5x,'V',&
      &6x,'W',5x,'USC',5x,'PHI',4x,'DEN',3x,'TP',5x,'UA',5x,'RA',4x,&
      &'TA',6x,'DUDZ',4x,'DPDZ',3x,'DZDS',3x,'DYDS',1x,'IPOS',&
      &1x,'DELTAZ')
      deltaz=zc-h
      write(dbgunt,114)nnp,x,y,zc,r,u,v,w,usc,phi,den,tp,ua,ra,ta,&
      &dudz,dpdz,dzds,dyds,ipositn,deltaz
114   format(1x,i5,f9.2,3f7.2,4f7.2,&
      &f8.4,f6.3,f7.2,f6.2,f7.3,f7.2,f8.3,&
      &f8.3,2f7.3,i5,f7.3)
   endif

! --- When trajectory inclination falls below 20 degrees, ignoring
! --- streamline descent, check for wake influence
   if(phi<=0.349065850398866d0 .and. ipositn<4) then
      if(.not.linwake) then
! ---       Plume centerline has just entered wake
         linwake=.true.
         xbi=xb
! ---       Use unadjusted rise for BID
         base=max(h,zmin)
         rise=max(0.0d0,z-base)
         bidsq=(rise/3.5d0)**2
! ---       Guard against x.LE.0 due to precision
         if(x<=0.0d0) then
            szi=dsqrt(bidsq)
            syi=szi
         else
            call sigzpr(x,z,szi)
            szi=dsqrt(szi**2+bidsq)
            call sigypr(x,z,syi)
            syi=dsqrt(syi**2+bidsq)
         endif

! ---       Normal debug output
         if(ldbhr) then
            write(dbgunt,*) ' '
            write(dbgunt,*)'NUMRISE call to WAKE_DFSN -- A'
            write(dbgunt,'(1x,a,4(f12.5,2x))')'x,y,z,z+zcum: ',&
            &x,y,z,zc
            write(dbgunt,'(1x,a,3(f12.5,2x))')'ds,u,w      : ',&
            &ds,u,w
            write(dbgunt,'(1x,a,2(f12.5,2x))')'xb,phi      : ',&
            &xb,phi
            write(dbgunt,'(1x,a,2(f12.5,2x))')'szi,syi     : ',&
            &szi,syi
         endif

! ---       Compute table of sigmas and growth rate in wake region
         call wake_dfsn(ldb,xbi,szi,syi,z,dbgunt)
         numwake = nwak
      endif
! ---    Select plume radius growth rate for this location
      call wake_drdx(x,drdxa)
   endif

! --- Process new position
   s=s+ds
   if(dble(nnp/np)==dble(nnp)/xnp) then
      nn=nnp/np
      if(nn>mxnw)then
         write(dbgunt,*)' '
         write(dbgunt,*)'Error in Subr. NUMRISE -- NN too large:'
         write(dbgunt,*)'                          NN   = ',nn
         write(dbgunt,*)'                          MXNW = ',mxnw
         write(dbgunt,*)' '
         write(dbgunt,*)'Source parameters may be suspect.'
         write(dbgunt,*)'Do not use Clearinghouse procedure for '
         write(dbgunt,*)'capped/horizontal stacks with PRIME!'
         write(dbgunt,*)' '
         ierr = 1
         return
      endif
      xt(nn)=x
      yt(nn)=y
      zt(nn)=zc
      rt(nn)=r
! --- CHECK FOR PLUME EQUILIBRIUM HEIGHT
      if(x >= xmax) then
         zfin=zc
         yfin=y
         xfin=x
         rfin=r
         goto 97
      endif
   endif

! --- Extract met and apply reduction in wake wind speed
96 call zmet(zb,ua0,ra,ta,dudz0,dpdz)
   ufac=1.0d0
   dufac=0.0d0

   if(ipositn<4) then
      call wake_u(.false.,xb,yb,zb,ufac,dufac,dbgunt)
   end if
   ua=ufac*ua0
   dudz=ufac*dudz0+dufac*ua0

! --- Next increment
   nnp=nnp+1
! --- Stop rise at local maximum (excludes streamline descent effects)
   if(w<0.0d0)then
      zfin=zc
      yfin=y
      xfin=x
      rfin=r
      go to 97
   endif

! --- Adjust ds toward ds0 for next step
   if(ds<ds0) ds=min(ds0,2.0d0*ds)

   if(s<slast) goto 999
   zfin=zc
   yfin=y
   xfin=x
   rfin=r

97 continue

! --- Complete trajectory out to "15R" if required, to account for
! --- streamline slope (vertical only)
   xfin0=xfin
   x15r=r15src-xfin0
   if(x15r>0.0d0) then
! ---    Set stepsize
      dsfin=dble(nstep)*ds
      dx15r=x15r/dble(mxnw-nn)
      dx15r=max(dsfin,dx15r)
! ---    Set range for additional steps
      n15r=min(idint(x15r/dx15r),mxnw-nn)
      nbeg=nn+1
      nend=nn+n15r
      do in=nbeg,nend
! ---       Define coordinates of plume relative to bldg.-defined origin
         xbb=xt(in-1)-xbadj
         xbe=xbb+dx15r
         yb=yt(in-1)-ybadj
         zb=zt(in-1)
! ---       Obtain mean streamline slope
         dzdx=0.0d0
         call position(xbb,yb,zb,ipos)
         if(ipos>2) then
            call zstream(hb,wb,xLb,rb,xLr,hr,xbb,yb,zb,dzdxb)
            call zstream(hb,wb,xLb,rb,xLr,hr,xbe,yb,zb,dzdxe)
            dzdx=0.5d0*(dzdxb+dzdxe)
         endif
         xt(in)=xt(in-1)+dx15r
         yt(in)=yfin
         zt(in)=max(zmin,zt(in-1)+dzdx*dx15r)
         rt(in)=rfin
         zcumul=zcumul+dzdx*dx15r

! ---       Check for wake entry if this has not already happened
         if(.not.linwake) then
            if(ipos<4) then
! ---             Plume centerline has just entered wake
               linwake=.true.
! ---             Set "internal" variable names
               x=xt(in)
               z=zt(in)-zcumul
               xbi=x-xbadj
! ---             Use unadjusted rise for BID
               base=max(h,zmin)
               rise=max(0.0d0,z-base)
               bidsq=(rise/3.5d0)**2
               call sigzpr(x,z,szi)
               szi=dsqrt(szi**2+bidsq)
               call sigypr(x,z,syi)
               syi=dsqrt(syi**2+bidsq)

! ---             Normal debug output
               if(ldbhr) then
                  write(dbgunt,*) ' '
                  write(dbgunt,*)'NUMRISE call to WAKE_DFSN -- B'
                  write(dbgunt,'(1x,a,4(f12.5,2x))')'x,y,z,z+zcum:',&
                  &x,yfin,z,zt(in)
                  write(dbgunt,'(1x,a,2(f12.5,2x))')'xb,phi      :',&
                  &xbi,phi
                  write(dbgunt,'(1x,a,2(f12.5,2x))')'szi,syi     :',&
                  &szi,syi
               endif

! ---             Compute table of sigmas and growth rate in wake region
               call wake_dfsn(ldb,xbi,szi,syi,z,dbgunt)
               numwake = nwak
            endif
! ---          Select plume radius growth rate for this location
            call wake_drdx(x,drdxa)
         endif

      enddo
! ---    Update nn and reset "fin" data
      nn=nend
      xfin=xt(nn)
      zfin=zt(nn)
   endif

! --- Construct trajectory arrays for calling program
   if(nn>ntr) then
! ---    Sample a subset of the nn points
      xtr(ntr)=xfin
      ytr(ntr)=yfin
      ztr(ntr)=zfin
      rtr(ntr)=rfin
      if(nn<=2*ntr) then
! ---       Fill elements with nearest values
         deln=dble(nn)/dble(ntr)
         do in=1,ntr-1
            jn=idint(dble(in)*deln)
            xtr(in)=xt(jn)
            ytr(in)=yt(jn)
            ztr(in)=zt(jn)
            rtr(in)=rt(jn)
         enddo
      else
! ---       Use sliding step-size to sample nearfield more frequently
         deln=2.0d0*dble(nn-ntr)/dble(ntr*(ntr-1))
         rn=0.0d0
         do in=1,ntr-1
            rn=rn+1.0d0+dble(in-1)*deln
            jn=idint(rn)
            xtr(in)=xt(jn)
            ytr(in)=yt(jn)
            ztr(in)=zt(jn)
            rtr(in)=rt(jn)
         enddo
      endif
   else
! ---    Fill elements directly
      do in=1,nn
         xtr(in)=xt(in)
         ytr(in)=yt(in)
         ztr(in)=zt(in)
         rtr(in)=rt(in)
      enddo
! ---    Fill excess elements with final rise properties
      do it=nn+1,ntr
         xtr(it)=xfin
         ytr(it)=yfin
         ztr(it)=zfin
         rtr(it)=rfin
      enddo
   endif

! --- Restore step size (may have changed)
   ds=ds0

!RWB  NOTE!  xbi is not defined if LINWAKE = .FALSE.  The following should solve this
   if(linwake) then
! ---    Determine maximum fraction of plume captured in cavity
      if (xbi<(xLb+xLR)) then
! ---       Plume centerline enters wake boundary before clearing cavity
         call wake_fqc(ldb,xbi,xtr,ztr,mxntr,dbgunt)
      else
         fqcav = 0.0d0
      endif
   else
      fqcav=0.0d0
   endif

! --- Normal debug output
   if(ldbhr) then
      delzfin=zfin-h
      write(dbgunt,*)
      write(dbgunt,*)'      Initial Plume Temperature = ',texit
      write(dbgunt,*)'             Buoyancy flux (FB) = ',fb
      write(dbgunt,*)'             Momentum flux (FM) = ',fm
      write(dbgunt,*)'  Neutral dist. to final rise   = ',xmax
      write(dbgunt,*)'  Calc distance to final rise   = ',xfin0
      write(dbgunt,*)'Distance from final rise to 15R = ',x15r
      write(dbgunt,*)'Total distance tabulated (XFIN) = ',xfin
      write(dbgunt,*)'    Final Y displacement (YFIN) = ',yfin
      write(dbgunt,*)'      Final plume height (ZFIN) = ',zfin
      write(dbgunt,*)'     Final plume rise (DELZFIN) = ',delzfin
      write(dbgunt,*)'      Final plume radius (RFIN) = ',rfin
      write(dbgunt,*)'Cumul. streamline adj. (ZCUMUL) = ',zcumul
      write(dbgunt,*)
      write(dbgunt,*)'    Fraction of plume in CAVITY = ',fqcav
      write(dbgunt,*)
   endif

! --- AWMA/Petersen:
!       Save the trajectory arrays from the first call to WAKE_DFSN for
!       use in WAKE_DFSN2
   if(ldb) then
! ---    Write the arrays passed back to the calling routine
      write(dbgunt,26)
26    format('  -- Trajectory arrays before CALL WAKE_DFSN2')
      write(dbgunt,28)
      do i=1,ntr
         write(dbgunt,32)i,xtr(i),ytr(i),ztr(i),rtr(i),rtr(i)*0.8d0
      enddo
      write(dbgunt,*)
   endif

! --- AWMA version D20350
   if (l_awma_uturb .or. L_AWMA_UTurbHX) then
      xtr_sav = xtr
      ztr_sav = ztr
   end if

!
! --- Compute new table of sigmas and growth rate in wake region with
! --- full array of plume heights for use in calls to ambient sigmas.
! --- Also redefines virtual source sigmas.
   if (linwake) then
      dfsn2call = .true.
      call wake_dfsn2(ldb,xbi,szi,syi,xtr,ztr,ntr,dbgunt)
   end if

! --- Extended debug output
   if(ldb) then
! ---    Write the arrays passed back to the calling routine
      write(dbgunt,27)
27    format('  -- Trajectory arrays after CALL WAKE_DFSN2')
      write(dbgunt,28)
28    format(/4x,'I',10x,'XTR',8x,'YTR',8x,'ZTR',8x,'RTR',8x,'sz?'/)
      do i=1,ntr
         write(dbgunt,32)i,xtr(i),ytr(i),ztr(i),rtr(i),rtr(i)*0.8d0
32       format(i5,3x,5(f10.4,1x))
      enddo
      write(dbgunt,*)
   endif

   return
end subroutine numrise

!----------------------------------------------------------------------
subroutine rate(ua,dudz,ra,dpdz,ta,drdxa,rhs)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                  RATE
!                X. Zhang, J. Scire,   EARTH TECH
!
!                Adapted from CALPUFF routine RATE
!                for EPRI under contract WO3527-01
!
! --- PURPOSE:  Compute the right-hand side of the equations
!
! --- INPUTS:
!         UA - real    - Current ambient wind speed (m/s)
!       DUDZ - real    - Current wind shear (1/s)
!         RA - real    - Current ambient air density (kg/m**3)
!       DPDZ - real    - Current ambient potential temperature gradient
!                        (deg. K/m)
!         TA - real    - Current ambient air temperature (deg. K)
!     ALPHA0 - real    - Plume entrainment coefficient (parallel)
!      DRDXA - real    - Growth rate of plume radius due to ambient turb
!
!     Common block /PLU/ variables:
!           X,R,U,V,W,USC,PHI,DEN,TP
!     Common block /NUMPARM/ variables:
!           GRAVI, RP,
!           NENT, ALPHAP(mxent), BETAP(mxent), XCAT(mxentp1),
!     Parameters:
!           MXENT, MXENTP1
!
! --- OUTPUT:
!        RHS(7) - real     - Right hand terms
!
! --- RATE called by:  NUMRISE
! --- RATE calls:      none
!----------------------------------------------------------------------
! --- Include files
   use PRIME_numparm
   use prime_plu
   use main1, only : l_awma_entrain, AWMA_Beta_EntrainCoeff

   implicit none

   double precision :: rhs(7)
   double precision :: alpha, alpha0, beta, beta0, drdxa, ra, ua, ta,&
   &rhs1a, dudz, dpdz
   integer :: i, nentp1
! ---   Constants:
!          GRAVI - Gravitational acceleration (m/s**2)
!          RP    - Radiation coefficient (kg/m**2/deg. K**3/s)
! --- Set default entrainment coefficients
   data alpha0/0.11d0/, beta0/0.6d0/
! --- Define the entrainment coefficients
   alpha=alpha0

! --- AWMA version D20350
!     Change entrainment constant from 0.6 to 0.35, which is set in modules.f,
!       if the ALPHA option AWMAEntrain downwash option was set
   if (.not. l_awma_entrain) then
      beta=beta0
   else
      beta = AWMA_Beta_EntrainCoeff
!         beta = 0.35d0
   end if

   if(nent>0)then
! ---    Check if the plume is in the area where perturbed entrainment
! ---    coefficients apply
      if(x<xcat(1))go to 99
      nentp1=nent+1
      do i=2,nentp1
         if(x<=xcat(i))then
            alpha=alphap(i-1)
            beta=betap(i-1)
            go to 99
         endif
      enddo
! ---    Override any ambient growth rate
      drdxa=0.0d0
   endif
99 continue

   rhs(1)=(2.0d0*r*ra)*(alpha*dabs(usc-ua*u/usc)+&
   &beta*dabs(ua*dsin(phi)))
!
! --- Condition entrainment to be .GE. growth due to ambient turb.
   rhs1a =(2.0d0*r*ra)*ua*drdxa
   rhs(1)=max(rhs(1),rhs1a)
!
   rhs(2)=-(r*r*den*w)*dudz
   rhs(3)=gravi*r*r*(ra-den)
   rhs(4)=-(r*r*den*w)*dpdz-rp*r*(tp**4-ta**4)
   rhs(5)=w/usc
   rhs(6)=u/usc
   rhs(7)=v/usc
   return
end subroutine rate

!----------------------------------------------------------------------
subroutine lump(ua,ta,f)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                  LUMP
!                X.(J.) Zhang, J. Scire,  EARTH TECH
!
! --- PURPOSE:  Calculate the lumped variables
!
! --- INPUTS:
!         UA - real    - Current ambient wind speed (m/s)
!         TA - real    - Current ambient air temperature (K)
!
! --- OUTPUT:
!          F(7) - real     - lumped variables
!
! --- LUMP called by:  NUMRISE
! --- LUMP calls:      none
!----------------------------------------------------------------------
   use prime_plu

   implicit none

   double precision :: f(7), ua, ta
   f(1)=den*usc*r*r
   f(2)=f(1)*(u-ua)
   f(3)=f(1)*w
   f(4)=f(1)*(tp-ta)
   f(5)=z
   f(6)=x
   f(7)=y
   return

end subroutine lump

!----------------------------------------------------------------------
subroutine marching(fold,fnew,rhs,ds)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              MARCHING
!                X.(J.) Zhang, J. Scire,  EARTH TECH
!
! --- PURPOSE:  Marching S one step, either PREDICTOR or CORRECTOR
!
! --- INPUTS:
!       FOLD(7) - real     - Old values
!        RHS(7) - real     - Right hand terms
!            DS - real     - Distance (m) along plume axis
!
!
! --- OUTPUT:
!       FNEW(7) - real     - New values
!
! --- MARCHING called by:  NUMRISE
! --- MARCHING calls:      none
!----------------------------------------------------------------------
   implicit none

   double precision :: fnew(7),fold(7),rhs(7)
   double precision :: ds
   integer :: i

   do i=1,7
      fnew(i)=fold(i)+rhs(i)*ds
   end do

   return
end subroutine marching

!----------------------------------------------------------------------
subroutine unlump(ua,ta,ra,f)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                UNLUMP
!                X.(J.) Zhang, J. Scire,  EARTH TECH
!
! --- PURPOSE:  Calculate physical variables from lumped variables
!
! --- MODIFIED: To limit plume temperature (TP) to be .GE. ambient
!               temperature (TA) minus 10K to avoid potential math error.
!               R.W. Brode, PES/MACTEC, Inc. - 06/17/03
!
! --- INPUTS:
!         UA - real    - Current ambient wind speed (m/s)
!         TA - real    - Current ambient air temperature (K)
!         RA - real    - Current ambient air density (kg/m^3)
!       F(7) - real    - Lumped variables
!
! --- OUTPUT:
!       common /PLU/:
!          U,V,W,USC,R,TP,PHI,Z,Y,X
!
! --- UNLUMP called by:  NUMRISE
! --- UNLUMP calls:      none
!----------------------------------------------------------------------
   use prime_plu

   implicit none

   double precision :: f(7), ua, ta, ra

   u=ua+f(2)/f(1)
   w=f(3)/f(1)
   usc=dsqrt(u*u+w*w)
   tp=ta+f(4)/f(1)
!PES    Limit plume temperature (TP) to be .GE. ambient (TA) - 10 K to
!PES    avoid potential math error.  R. Brode, PES/MACTEC, 6/21/03
   tp = max( tp, ta-10.0d0 )
   den=(ra*ta)/tp
   r=dsqrt(f(1)/(usc*den))
   phi=datan(w/u)
   z=f(5)
   x=f(6)
   y=f(7)

   return
end subroutine unlump

!>>>c   This is the old ZMET, replace with new ZMET that uses AERMOD profiles
!>>>c----------------------------------------------------------------------
!>>>      subroutine zmet(z,ua,ra,ta,dudz,dpdz)
!>>>c----------------------------------------------------------------------
!>>>c
!>>>c --- PRIME      Version:  1.0     Level:  970812                  ZMET
!>>>c                X.(J.) Zhang, J. Scire,  EARTH TECH
!>>>c
!>>>c --- PURPOSE:  Obtain ambient met parameters at height z
!>>>c               by interpolation of gridded values
!>>>c
!>>>c --- INPUTS:
!>>>c          Z - real    - Height (m)
!>>>c
!>>>c     Common block /AMBIENT/ variables:
!>>>c           NZA,UAMB(mxnz),RAMB(mxnz),TAMB(mxnz),ZFACEA(mxnzp1),
!>>>c           ZGPTA(mxnz),TAMB0,RAMB0,ADIA,PTGRAD0
!>>>c     Parameters:
!>>>c           MXNZ, MXNZP1
!>>>c
!>>>c --- OUTPUT:
!>>>c         UA - real    - Current ambient wind speed (m/s)
!>>>c         RA - real    - Current ambient air density (kg/m**3)
!>>>c         TA - real    - Current ambient air temperature (deg. K)
!>>>c       DUDZ - real    - Current wind shear (1/s)
!>>>c       DPDZ - real    - Current ambient potential temperature gradient
!>>>c                        (deg. K/m)
!>>>c
!>>>c --- ZMET called by:  NUMRISE
!>>>c --- ZMET calls:      none
!>>>c----------------------------------------------------------------------
!>>>c     Defined at grid center: uamb,tamb,ramb
!>>>c     Defined at zface:       dedz
!>>>c----------------------------------------------------------------------
!>>>c --- Include files
!>>>      include 'params
!>>>      include 'ambient
!>>>
!>>>c --- Interpolate variables defined at grid cell center
!>>>      if(z.lt.zgpta(1))then
!>>>
!>>>c ---    Height is below first grid point
!>>>         zfact=(zgpta(1)-z)/zgpta(1)
!>>>         ta=tamb(1)-(tamb(1)-tamb0)*zfact
!>>>         ra=ramb(1)-(ramb(1)-ramb0)*zfact
!>>>c ---    Wind speed at z=0 is assumed to be zero
!>>>         ua=uamb(1)*(1.0-zfact)
!>>>         dudz=uamb(1)/zgpta(1)
!>>>         dpdz=adia+(tamb(1)-tamb0)/zgpta(1)
!>>>         dpdz=amax1(dpdz,ptgrad0)
!>>>
!>>>      else if(z.lt.zgpta(nza))then
!>>>
!>>>c ---    Find the layer containing height, Z
!>>>         do i=2,nza
!>>>            if(z.le.zgpta(i))then
!>>>               im1=i-1
!>>>               delz=zgpta(i)-zgpta(im1)
!>>>               zfact=(zgpta(i)-z)/delz
!>>>               ta=tamb(i)-(tamb(i)-tamb(im1))*zfact
!>>>               ra=ramb(i)-(ramb(i)-ramb(im1))*zfact
!>>>               ua=uamb(i)-(uamb(i)-uamb(im1))*zfact
!>>>c ---          Compute wind speed gradient & pot. temp. gradient
!>>>               dudz=(uamb(i)-uamb(im1))/delz
!>>>               dpdz=adia+(tamb(i)-tamb(im1))/delz
!>>>               dpdz=amax1(dpdz,ptgrad0)
!>>>               go to 101
!>>>            endif
!>>>         enddo
!>>>
!>>>      else
!>>>
!>>>c ---    Height is at or above the top grid point -- persist values
!>>>c ---    at the top grid cell
!>>>         ta=tamb(nza)
!>>>         ra=ramb(nza)
!>>>         ua=uamb(nza)
!>>>c ---    Hold wind speed and temperature constant above top layer at
!>>>c ---    values at top grid point
!>>>         dudz=0.0
!>>>         dpdz=adia
!>>>         dpdz=amax1(dpdz,ptgrad0)
!>>>      endif
!>>>
!>>>101   continue
!>>>
!>>>      return
!>>>      end

!----------------------------------------------------------------------
subroutine zmet(z,ua,ra,tap,dudz,dpdz)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                  ZMET
!                X.(J.) Zhang, J. Scire,  EARTH TECH
!
! --- PURPOSE:  Obtain ambient met parameters at height z
!               by interpolation of gridded values
!
! --- MODIFIED: For use with the AERMOD model.  Uses AERMOD meteorological
!               profiles.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!          Z - real    - Height (m)
!
!     Common block /AMBIENT/ variables:
!           NZA,UAMB(mxnz),RAMB(mxnz),TAMB(mxnz),ZFACEA(mxnzp1),
!           ZGPTA(mxnz),TAMB0,RAMB0,ADIA,PTGRAD0
!     Parameters:
!           MXNZ, MXNZP1
!
! --- OUTPUT:
!         UA - real    - Current ambient wind speed (m/s)
!         RA - real    - Current ambient air density (kg/m**3)
!         TA - real    - Current ambient air temperature (deg. K)
!       DUDZ - real    - Current wind shear (1/s)
!       DPDZ - real    - Current ambient potential temperature gradient
!                        (deg. K/m)
!
! --- ZMET called by:  NUMRISE
! --- ZMET calls:      none
!----------------------------------------------------------------------
!     Defined at grid center: uamb,tamb,ramb
!     Defined at zface:       dedz
!----------------------------------------------------------------------
! --- Include files
   use main1
   use PRIME_ambient

   implicit none

   double precision :: z, ua, ra, tap, dudz, dpdz
   double precision :: svatz, swatz, uatz, ptz, delz

   integer :: ndxblz

!---- Compute the parameter values at height Z for PRIME
!---- Locate index below height Z

   call locate(gridht, 1, mxglvl, z, ndxblz)

   if (ndxblz >= 1) then

!----    Sigma_V
      call gintrp( gridht(ndxblz), gridsv(ndxblz),&
      &gridht(ndxblz+1), gridsv(ndxblz+1),&
      &z, svatz )

!----    Sigma_W
      call gintrp( gridht(ndxblz), gridsw(ndxblz),&
      &gridht(ndxblz+1), gridsw(ndxblz+1),&
      &z, swatz )

!----    Wind speed
      call gintrp( gridht(ndxblz), gridws(ndxblz),&
      &gridht(ndxblz+1), gridws(ndxblz+1),&
      &z, uatz )

!----    Potential temperature gradient
      call gintrp( gridht(ndxblz), gridtg(ndxblz),&
      &gridht(ndxblz+1), gridtg(ndxblz+1),&
      &z, dpdz )

!----    Potential temperature
      call gintrp( gridht(ndxblz), gridpt(ndxblz),&
      &gridht(ndxblz+1), gridpt(ndxblz+1),&
      &z, ptz )

!----    Ambient air density
      call gintrp( gridht(ndxblz), gridrho(ndxblz),&
      &gridht(ndxblz+1), gridrho(ndxblz+1),&
      &z, ra )

!----    Compute wind speed gradient
      delz = (gridht(ndxblz+1) - gridht(ndxblz))
      dudz = (gridws(ndxblz+1) - gridws(ndxblz)) / delz

   else
!        Use GRID value for lowest level
      svatz  = gridsv(1)
      swatz  = gridsw(1)
      uatz   = gridws(1)
      dpdz   = gridtg(1)
      ptz    = gridpt(1)
      ra     = gridrho(1)
      dudz   = gridws(1) / gridht(1)
   end if

!---- Calculate ambient temperature from potential temperature profile
   tap = ptz - adia * (z + zbase)

!RWB  Modify the treatment of low wind/low turbulence cases per 7/31/96
!RWB  write-up by Steve Perry.  R. Brode, PES, 8/15/96
   swatz = max( swatz, swmin )
   svatz = max( svatz, svmin, svumin*uatz )
   if( l_vectorws )then
      ua = dsqrt( uatz*uatz + 2.0d0*svatz*svatz )
   endif
   ua  = max( uatz, wsmin )

   return
end subroutine zmet

!----------------------------------------------------------------------
subroutine zstream(h,w,l,r,lr,hr,x,y,z,dzdx)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812               ZSTREAM
!                L. Schulman, J. Scire,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Estimates the local mean slope of streamlines in the
!               vicinity of buildings.  The local slope is equal to
!               w/u, where w is the local mean vertical velocity and
!               u the local mean longitudinal velocity.  For modeling
!               plume rise, the streamline deflection effect is modeled
!               as (w/u)(dx).
!
! --- Modification 2/24/2017
!               Added user control to include AWMADWNW extended diagnostics
!
! --- INPUTS:
!                H - real              - Building height above ground
!                W - real              - Projected building width
!                L - real              - Along-wind building length
!                R - real              - Scale length from H and W
!               LR - real              - Length of downwind cavity from
!                                         lee face
!               HR - real              - Maximum cavity height above
!                                         ground
!                x - real              - downwind distances
!                y - real              - crosswind distances
!                z - real              - heights above ground
!
! --- OUTPUT:
!             dzdx - real              - Vertical streamline slope
!
! --- ZSTREAM called by:  NUMRISE
! --- ZSTREAM calls:      none
!----------------------------------------------------------------------
!
   implicit none
! JAT 06/22/21 D065
! REMOVE EXPZ1,EXPZ2,EXPX AS UNUSED VARIABLES
!      DOUBLE PRECISION H,W,L,R,HR,LR,expz1,expz2,expzg,expx
   double precision :: h,w,l,r,hr,lr,expzg
   double precision :: x,y,z,dzdx2,zslope,dzdx,ypos,hbyw,onebyr,&
   &wby2,rby2,zg,zslopeLR,yscale

! JAT 06/22/21 D065
! REMOVE EXPZ1,EXPZ2,EXPX AS UNUSED VARIABLES
!      data expx/1.0D0/, expz1/3.0D0/, expz2/1.0D0/
!
! --- Check for a building
   if(h<=0.0d0)then
      dzdx=0.0d0
      go to 900
   endif

! --- Initialize
   dzdx2 = 0.0d0

! --- Set a few constants
   hbyw=h/w
   ypos=dabs(y)
   onebyr=1.0d0/r
   wby2=0.5d0*w
   rby2=0.5d0*r

! --- Power law exponent for slope approaching zero at ground
! --- Exponent modified for tall, narrow buidings
! --- zg is level below which slope is zero in far wake
   zg=0.0d0
   expzg=0.3d0
   if(hbyw >= 2.0d0) expzg=expzg*(0.5d0*hbyw)**2

!
! --- Local streamline slope (zslope) at z=H
! --- Local two-dimensional streamline slope (dzdx2)
! --- Local three-dimensional streamline slope (dzdx)
! --- (x,y,z)=(0,0,0) at ground of center of upwind building face
!
   if(x < -r) then
! ---    Upwind of building influence
      zslope = 0.0d0
      dzdx2  = 0.0d0

   elseif(x < 0.0d0) then
! ---    Ascent upwind of building:
! ---    parobolic fit to slope=0 at (-R,0) with increasing slope
! ---    to (0,(HR-H))
! ---    vertical decay above building using expz1
! ---    below building nonzero slope above 2/3 H for R<H reducing
! ---    to ground as R approaches 2H
      zslope = 2.0d0*(hr-h)*(x+r)*onebyr**2
      if(z > h) then
! ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz1
! ---               recode with explicit exponent, expz1=3
         dzdx2 = zslope/((z-h+r)*onebyr)**3
      elseif(r <= h .and. z <= 0.67d0*h) then
         dzdx2 = 0.0d0
      elseif(r <= h .and. z > 0.67d0*h) then
         dzdx2 = zslope
      elseif(r > h .and. z <= 0.67d0*(2.0d0*h-r)) then
         dzdx2 = 0.0d0
      elseif(r > h .and. z > 0.67d0*(2.0d0*h-r)) then
         dzdx2 = zslope
      else
         print *,'z out of bounds      ',x,z
      endif

   elseif(x <= rby2) then
! ---    Ascent over building
! ---    parobolic fit from (0,0) with decreasing slope to
! ---    to (0.5R,(HR-H))
! ---    vertical decay above building using expz1
      zslope = (-(hr-h)*4.0d0*onebyr)*(2.0d0*x*onebyr-1.0d0)
      if(z <= h) then
         dzdx2 = zslope
      else
! ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz1
! ---               recode with explicit exponent, expz1=3
         dzdx2 = zslope/((z-h+r)*onebyr)**3
      endif

   elseif(x <= l+lr) then
! ---    Descent over building to end of near wake
! ---    parobolic fit from (.5R,(HR-H)) with increasing slope to
! ---    to (L+LR,-H/2)
! ---    vertical decay above z=H using expz2
! ---    vertical decay below z=H using expzg
      zslope = (hr-h)*(r-2.0d0*x)/((l-rby2+lr)**2)
      if(z > h) then
! ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz2
! ---               recode without expz2 since expz2=1
         dzdx2 = zslope/((z-h+r)*onebyr)
      elseif(z <= zg) then
         dzdx2 = 0.0d0
      else
         dzdx2 = zslope*((z-zg)/(h-zg))**expzg
      endif

   else
! ---    Descent beyond near wake (far wake)
! ---    horizontal decay beyond L+LR using expx
! ---    vertical decay above z=H using expz2
! ---    vertical decay below z=H using expzg
      zslopeLR  = -2.0d0*(hr-h)/(l-rby2+lr)
! ---    zslope = zslopeLR/((x-(L+LR-R))*onebyr)**expx
! ---             recode without expx since expx=1
      zslope = zslopeLR/((x-(l+lr-r))*onebyr)
      if(z > h) then
! ---       dzdx2 = zslope/((z-H+R)*onebyr)**expz2
! ---               recode without expz2 since expz2=1
         dzdx2 = zslope/((z-h+r)*onebyr)
      elseif(z <= zg) then
         dzdx2 = 0.0d0
      else
         dzdx2 = zslope*((z-zg)/(h-zg))**expzg
      endif

   endif

! --- Calculate 3-D slopes,: dzdx : from 2-d centerplane slope,: dzdx2
   if(ypos > (wby2+r/3.0d0))then
      dzdx=0.0d0
   elseif(ypos <= wby2)then
      dzdx=dzdx2
   else
      yscale=1.0d0+(3.0d0*onebyr)*(wby2-ypos)
      dzdx=dzdx2*yscale
   endif

900 continue

   return
end subroutine zstream

!-----------------------------------------------------------------------
subroutine position(x,y,z,ipositn)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              POSITION
!                L. Schulman, J. Scire, D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Identifies if (x,y,z) location is in the building,
!               in the near wake, in the far wake, or outside.
!               IPOSITN is set to: 1 if within the bldg.
!                                  2 if within the near wake
!                                  3 if within the far wake
!                                  4 if outside wake region
!
! --- INPUTS:
!                x - real              - downwind distance from center
!                                        of upwind face of bldg
!                y - real              - crosswind distance from center
!                                        of upwind face of bldg
!                z - real              - height above ground
!
!     Common block /WAKEDAT/ variables:
!           Hb,Wb,xLb,Rb,HR,xLR,xLC
!
! --- OUTPUT:
!
!          ipositn - integer           - 1 if (x,y,z) within building
!                                        2 if location within near wake
!                                        3 if location within far wake
!                                        4 if location outside
!
! --- POSITION called by:  NUMRISE, PCALC (HOST subroutine)
! --- POSITION calls:      WAKE_DIM
!----------------------------------------------------------------------
!
! --- Include commons
   use PRIME_wakedat

   implicit none

   double precision :: x, y, z, ypos, rby2, rby3, wby2, xtest, ytest,&
   &ztest, zcav, ycav, zwake, ywake
   double precision, parameter :: zero = 0.0d0,&
   &half = 0.5d0
   double precision, parameter :: skin = 0.99998d0    ! fractional boundary just inside building

   integer :: iposy, iposz, ipositn

! --- Initialize
   iposy=4
   iposz=4
   ipositn=4

! --- Screen out any cases without building
   if(Hb<=zero) return

! --- Screen out positions upwind of building (and on windward face)
   if(x<=zero) return

! --- Set y positive for calculations
   ypos=dabs(y)

! --- Set selected length scale products
   rby2=half*Rb
   rby3=third*Rb
   wby2=half*Wb

! --- Set ipositn to 1 if location within building
   xtest=x/xLB
   ytest=ypos/wby2
   ztest=z/Hb
   if(xtest<skin .and. ztest<skin .and. ytest<skin) then
      ipositn=1
      return
   endif

! --- Calculate if location below height of near wake boundary
   if(xLC < xLb)then
! ---    Reattachment
      if(x<xLb) then
! ---       Cavity height equal to building height
         zcav=Hb
         if(z <= zcav) iposz=2
      elseif(x<(xLb+xLR)) then
! ---       Cavity height is ellipse with a=LR and b=H
         zcav=Hb*dsqrt(1.0d0-((x-xLb)/xLR)**2)
         if(z <= zcav) iposz=2
      endif
   else
! ---    No reattachment
      if(x<=rby2) then
! ---       Cavity height is parabola with vertex at height MAX(0.5R,HR)
! ---       and passing thru upwind building edge (0,H)
         zcav=hr+4.0d0*(x-rby2)**2*(Hb-hr)/(Rb**2)
         if(z <= zcav) iposz=2
      elseif(x<(xLb+xLR)) then
! ---       Cavity height is ellipse with a=LR+L-0.5R and b=HR
         zcav=hr*dsqrt(1.0d0-((x-rby2)/(xLb+xLR-rby2))**2)
         if(z <= zcav) iposz=2
      endif
   endif

! --- Calculate x-y near wake boundary
   if(x<=Rb) then
! ---    Cavity width is parabola with vertex @ width MAX(R,W/2+R/3)
! ---    and passing thru upwind building edge (0,W/2)
      ycav=(wby2+rby3)-(x-Rb)**2/(3.0d0*Rb)
      if(ypos <= ycav) iposy=2
   elseif(x<(xLb+xLR)) then
! ---    Cavity width is ellipse with a=W/2+R/3 and b=LR+L-R
      ycav=(wby2+rby3)*dsqrt(1.0d0-((x-Rb)/(xLb+xLR-Rb))**2)
      if(ypos <= ycav) iposy=2
   endif

! --- Set ipositn to 2 if (x,y,z) location within near wake
   if( iposz == 2 .and. iposy == 2) ipositn=2

! --- Test for position in far wake if still 4
   if(ipositn==4) then
      call wake_dim(x,Hb,Wb,Rb,zwake,ywake)
      if(z<=zwake .and. ypos<=ywake) ipositn=3
   endif

   return
end subroutine position

!----------------------------------------------------------------------
subroutine numgrad(x,xtr,ztr,ntr,zeff)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812               NUMGRAD
!                J. Scire,  EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Compute the effective gradual plume height by
!               interpolation of the stored values.  Effective
!               plume height is the stack height + plume rise.
!
! --- INPUTS:
!                X - real       - Downwind distance (m)
!         XTR(ntr) - real array - Array of stored downwind distances (m)
!         ZTR(ntr) - real array - Array of stored effective plume height
!                                 at each downwind distance
!              NTR - integer    - Number of stored values in XTR, ZTR
!
! --- OUTPUT:
!             ZEFF - real       - Effective plume height (m) at
!                                 downwind distance X
!
! --- NUMGRAD called by:  PHEFF
! --- NUMGRAD calls:      none
!----------------------------------------------------------------------
!
   implicit none

   integer :: i, ntr, ntrm1, ip1
   double precision :: x, xtr(ntr), ztr(ntr), zeff
!
   if(x>=xtr(ntr))then
      zeff=ztr(ntr)
   else
      ntrm1=ntr-1
      zeff=ztr(1)
      do i=ntrm1,1,-1
         if(x>=xtr(i))then
            ip1=i+1
            zeff=ztr(ip1)-(ztr(ip1)-ztr(i))*(xtr(ip1)-x)/&
            &(xtr(ip1)-xtr(i))
            return
         endif
      enddo
   endif

   return
end subroutine numgrad

!----------------------------------------------------------------------
subroutine wake_drdx(x,drdx)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812             WAKE_DRDX
!                J. Scire, D. Strimaitis,  EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Compute the plume radius growth rate in the wake
!               by interpolating among the stored values.
!
! --- INPUTS:
!                X - real       - Downwind distance (m) from source
!
!     Common block /PARAMS/ variables:
!           MXNTR
!     Common block /WAKEDAT/ variables:
!           NWAK, XWAK(mxntr), DRWAK(mxntr)
!
! --- OUTPUT:
!             DRDX - real       - Rate of growth of plume radius at
!                                 downwind distance X from source
!
! --- WAKE_DRDX called by:  NUMRISE
! --- WAKE_DRDX calls:      none
!----------------------------------------------------------------------
!
   use PRIME_wakedat

   implicit none

   double precision :: x, drdx
   integer :: i, nwkm1, ip1
!
! --- Set growth rate to zero outside interpolation region
! --- (all x outside wake)
   if(x>xwak(nwak) .or. x<xwak(1))then
      drdx=0.0d0
   elseif(nwak<=1) then
! ---    Wake turbulence does not alter this plume
      drdx=0.0d0
   else
      nwkm1=nwak-1
      drdx=drwak(1)
      do i=nwkm1,1,-1
         if(x>=xwak(i))then
            ip1=i+1
            drdx=drwak(ip1)-(drwak(ip1)-drwak(i))*(xwak(ip1)-x)/&
            &(xwak(ip1)-xwak(i))
            return
         endif
      enddo
   endif

   return
end subroutine wake_drdx

!----------------------------------------------------------------------
subroutine wake_ini(kurdat,ldbhr,dbgunt,rural,dsbh,dsbw,dsbl,&
&xadj,yadj,ubldg,ustack)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              WAKE_INI
!                D. Strimaitis, EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Refreshes variables in /wakedat/ common
!
! --- INPUTS:
!
!      LDBHR - logical - Debug output written when .TRUE.
!      RURAL - logical - Denotes rural dispersion when .TRUE.
!       DSBH - real    - Effective building height (m)
!       DSBW - real    - Effective building width (m) across flow
!       DSBL - real    - Effective building length (m) along flow
!       XADJ - real    - Distance (m) from source to upwind face of bldg
!                        along flow
!       YADJ - real    - Distance (m) from source to center of upwind
!                        face of bldg across flow
!      UBLDG - real    - Wind speed (m/s) at top of building
!     USTACK - real    - Wind speed (m/s) at release height
!
!     Parameters:
!           MXNTR
!
! --- OUTPUT:
!
!     Common block /WAKEDAT/ variables:
!           HB,WB,XLB,RSCALE,HR,XLR,XLC,XBADJ,YBADJ,
!           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
!           DRWAK(mxntr), XZVWAK, XYVWAK, UB, URH,
!           LRURL, ISTAB
!
! --- WAKE_INI called by:  PCALC (HOST subroutine)
! --- WAKE_INI calls:      WAKE_SCALES
!----------------------------------------------------------------------

! --- Include common blocks
   use PRIME_wakedat

   implicit none

   double precision :: dsbh,dsbw,dsbl,xadj,yadj,ubldg,ustack
   double precision, parameter :: zero = 0.0d0

   integer :: kurdat, dbgunt

   logical :: rural,ldbhr

! --- Calculate global variable, third=1/3, used in various places
   third = 1.0d0/3.0d0

! --- Transfer arguments to /wakedat/ variables
   lrurl = rural
   Hb    = dsbh
   Wb    = dsbw
   xLb   = dsbl
   xbadj = xadj
   ybadj = yadj
   Ub    = ubldg
   Urh   = ustack

! --- Compute wake dimensions and related parameters
   call wake_scales(kurdat,ldbhr,dbgunt)

! --- Reset contents of sigma arrays for wake region
   nwak=1
   xwak(1)=zero
   szwak(1)=zero
   sywak(1)=zero
   drwak(1)=zero

!aerc --- Reset virtual distances for sigmas beyond wake
!aer      xzvwak=zero
!aer      xyvwak=zero

   return
end subroutine wake_ini

!-----------------------------------------------------------------------
subroutine wake_scales(kurdat,ldbhr,dbgunt)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812           WAKE_SCALES
!                L. Schulman, D. Strimaitis,  J. Scire,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Calculates length scale and wake dimensions
!
! --- INPUTS:
!            LDBHR - logical           - Control variable for debug
!                                        write statements
!
!     Common block /WAKEDAT/ variables:
!           Hb,Wb,xLb
!     Parameters:  IO6
!
! --- OUTPUT:
!
!     Common block /WAKEDAT/ variables:
!           Rb,HR,xLR,xLC
!
! --- WAKE_SCALES called by:  WAKE_INI
! --- WAKE_SCALES calls:      none
!----------------------------------------------------------------------
!
! --- Include commons
   use PRIME_params
   use PRIME_wakedat

   implicit none

   double precision :: twoby3, rw, rl, hh, ww, explh

   integer :: kurdat, dbgunt

   logical :: ldbhr

! --- Set misc. constants, third=1/3 defined in sub. wake_ini
   twoby3 = 2.0d0*third

   if(hb<=0.0d0) then
! ---    No building
      Rb=0.0d0
      Hb=0.0d0
      xLR=0.0d0
      xLC=0.0d0
   else
!
! ---    Set ratios
      rw = Wb/Hb
      rl = xLb/Hb
! ---    Fackrell limits on aspect ratio L/H
      if(rl < 0.3d0) rl=0.3d0
      if(rl > 3.0d0) rl=3.0d0
!
! ---    Length scale R --- Wilson
! ---    Wilson limits for length scale R
! ---    H/W or W/H not greater than 8 --  already behaves as 2-D
      hh=Hb                    ! only modify H to calculate R
      ww=Wb                    ! only modify W to calculate R
      if(hh>8.0d0*ww)hh=8.0d0*ww
      if(ww>8.0d0*hh)ww=8.0d0*hh
      Rb= (min(hh,ww)**twoby3) * (max(hh,ww)**third)
!
! ---    Reattachment for LC < L
      xLC = 0.9d0*Rb
!
! ---    Recirculation cavity length---Fackrell
! ---    Modify Fackrell for W/H less than 1 by weakening dependence
! ---    on L/H.  Snyder found that cavity did not increase in length
! ---    as W/H = L/H decreased from 1 to 0.33.
! ---    Let L/H dependence decrease from Fackrell dependence at W/H=1
! ---    to no dependence at W/H=0.33.
      explh = 0.3d0
      if(rw < 1.0d0) explh=max(0.0d0,0.3d0*(rw-0.33d0)/0.67d0)
      xLR = 1.8d0*Wb/(rl**explh*(1.0d0+0.24d0*rw))
!
! ---    Maximum cavity height  (Wilson,ASHRAE):
      hr = Hb+0.22d0*Rb

   endif

! --- Write the results
   if(ldbhr)then
      write(dbgunt,*)
      write(dbgunt,*)'YR/MN/DY/HR:      ', kurdat
      write(dbgunt,*)
      write(dbgunt,*)'WAKE_SCALES inputs: '
      write(dbgunt,*)'   HB    = ',Hb,' (m)'
      write(dbgunt,*)'   WB    = ',Wb,' (m)'
      write(dbgunt,*)'   LB    = ',xLb,' (m)'
      write(dbgunt,*)
      write(dbgunt,*)'WAKE_SCALES output: '
      write(dbgunt,*)'   Scale length (R)               = ',Rb
      write(dbgunt,*)'   Max. cavity height (HR)        = ',hr
      write(dbgunt,*)'   Length of downwind cavity (LR) = ',xLR
      write(dbgunt,*)'   Length of roof cavity (LC)     = ',xLC
   endif
!
   return
end subroutine wake_scales

!-----------------------------------------------------------------------
subroutine wake_dfsn(ldbhr,xi,szi,syi,z,dbgunt)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812             WAKE_DFSN
!                L. Schulman, D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Tabulates sigmas and rate of plume growth as function
!              of location within the wake from modified Weil (1996)
!              analytical expressions
!
! --- MODIFIED: To initialize dummy variables zkdum and ykdum to 0.0.
!               Otherwise these variables may be undefined in subrountine
!               WAKE_SIG under some circumstances.
!               R.W. Brode, MACTEC (f/k/a PES), Inc. - 09/01/05
!
! --- MODIFIED: To modify calling arguments in call to subroutine
!               WAKE_SIG from turby and turbz to wakiy and wakiz.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- MODIFIED: For use with the AERMOD model. Use virtual source sigmas
!               added in quadrature to ambient sigmas instead of
!               virtual source distances.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!            ldbhr - logical     - Flag for debug write statements
!                                  to upwind bldg wall
!               xi - real        - distance (m) from upwind bldg wall
!                                  to point where plume intersects wake
!              szi - real        - sigma-z (m) at xi
!              syi - real        - sigma-y (m) at xi
!                z - real        - plume height (m) at xi
!
!     Common block /PARAMS/ variables:
!           MXNTR, MXNW
!     Common block /WAKEDAT/ variables:
!           XBADJ, Hb, Wb, xLb, Rb, xLR
!
! --- OUTPUT:
!
!     Common block /WAKEDAT/ variables:
!           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
!           DRWAK(mxntr), XZVWAK, XYVWAK,
!           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
!           XZVCAV, XYVCAV, LRURL, ISTAB
!
! --- WAKE_DFSN called by:  NUMRISE
! --- WAKE_DFSN calls    :  SIGZPR, SIGYPR,
!                           WAKE_XA, WAKE_CAV0, WAKE_TURB, WAKE_SIG
!----------------------------------------------------------------------
!
   use PRIME_params
   use PRIME_numparm
   use PRIME_wakedat
   use main1, only: awmadwdbunt, awmadwdbg

   implicit none

! --- Define local variable arrays for fine-steps
   double precision :: dist(mxnw),asigz(mxnw),asigy(mxnw),dsz(mxnw)
   double precision :: csigz(mxnw),csigy(mxnw)
   double precision :: xi,szi,syi,z
   double precision :: xamx,xamn,xay,zkdum,ykdum,xcave,distc,xbc
   double precision :: xaz,xdc,szcav0,sycav0,xd,dx,xlow,xhi,xrange
   double precision :: dxi,x,xold,sycav0r,xmid,wakiz,wakiy,zk,yk,zkc,&
   &ykc,dzrate,xnew,sigzxa,sydum,sz,szdum,sigyxa,sy,&
   &deln,rn
   double precision, parameter :: zero=0.0d0, half=0.5d0, one=1.0d0,&
   &rtpiby2 = 1.25331413731550d0

   integer :: i,np,nws,ncs,n,ir,npw,in,jn,inp,npc,dbgunt
   logical :: ldbhr,lcav,lwak,lrevcav

! --- Compute xa, where turbulent growth in the wake transitions
! --- to ambient growth rate, measured from upwind face of bldg
   call wake_xa(xLb,Rb,xaz,xay)
   xamx=max(xaz,xay)
   xamn=min(xaz,xay)

! --- Initialize virtual source sigma terms
   vsigy  = 0.0d0
   vsigz  = 0.0d0
   vsigyc = 0.0d0
   vsigzc = 0.0d0

! --- Initialize dummy variables for zk and yk
   zkdum = 0.0d0
   ykdum = 0.0d0

! --- Initialize CAVITY parameters
! --------------------------------
! --- Set distance from upwind face of bldg to END of cavity
   xcave=xLb+xLR
! --- Set distance from source to start of cavity
   distc=xLb+xbadj
! --- Set downwind distance to effective cavity source (when present),
! --- from the upwind face of bldg
   xbc=max(xi,xLb)
   xbc=min(xbc,xcave)
! --- Location of downwind edge of PDF region from effective cavity
! --- source
   xdc=xbc+xLR
! --- Set initial sigmas for cavity source using sigma-y at xi
   call wake_cav0(syi,szcav0,sycav0)
! --- The cavity sigma-y will need to be revised if xi lies upwind of
! --- the downwind face of the bldg.
   if(xi<xLb) then
      lrevcav=.true.
   else
      lrevcav=.false.
   endif

! --- Determine if any plume material in cavity may be modeled
! ------------------------------------------------------------
! --- Initialize output arrays
   ncav=1
   xcav(1)=xbc+xbadj
   szcav(1)=szcav0
   sycav(1)=sycav0
   if(xi>=xcave) then
      lcav=.false.
      lrevcav=.false.
   else
      lcav=.true.
   endif

! --- Is plume affected by wake turbulence?
! -------------------------------------------
! --- Initialize output arrays
   nwak=1
   xwak(1)=xi+xbadj
   szwak(1)=szi
   sywak(1)=syi
   drwak(1)=zero
   if(xi>=xamx) then
      lwak=.false.
      if(ldbhr) then
         write(dbgunt,*) ' '
         write(dbgunt,*)'----- WAKE_DFSN:        NWAK = ',nwak
         write(dbgunt,*)'Z-dispersion reaches ambient at: ',xaz+xbadj
         write(dbgunt,*)'Y-dispersion reaches ambient at: ',xay+xbadj
!aer            write(DBGUNT,*)'z,y virtual distances (m)    = ',xzvwak,xyvwak
         write(dbgunt,'(1x,a,2x,3(f14.5,2x))')&
         &'xadj, yadj, xi  (m) = ',xbadj,ybadj,xi
         write(dbgunt,*)'Plume NOT altered by wake turbulence!'
         write(dbgunt,*)
      endif
      if (awmadwdbg) then
         write(awmadwdbunt,*) ' '
         write(awmadwdbunt,*)'----- WAKE_DFSN:        NWAK = ',nwak
         write(awmadwdbunt,*)'Z-dispersion reaches ambient at: ',&
         &xaz+xbadj
         write(awmadwdbunt,*)'Y-dispersion reaches ambient at: ',&
         &xay+xbadj
         write(awmadwdbunt,'(1x,a,2x,3(f14.5,2x))')&
         &'xadj, yadj, xi  (m) = ',xbadj,ybadj,xi
         write(awmadwdbunt,*)'Plume NOT altered by wake turbulence!'
         write(awmadwdbunt,*)
      endif
   else
      lwak=.true.
   endif

! --- Return now if sigmas in wake do not need to be tabulated
   if(.not.lwak .and. .not.lcav) return

! --- Compute location of downwind edge of PDF region from xi
   xd=xi+xLR

! --- Set stepping parameters
   dx=2.0d0
! --- Range of table is from point of entry into wake (xi), to the point
! --- at which ambient growth rate resumes (xa), plus one "ds" so that
! --- both sigmas reach ambient, and virtual distances are computed.
! --- When cavity sigmas are also computed, range may start at the
! --- downwind bldg face, and extend to end of cavity.
   xlow=xi
   xhi=xamx
   if(lcav) then
      xlow=min(xi,xbc)
      xhi=max(xamx,xcave)
   endif
   xrange=xhi-xlow+dx
   np=idnint(xrange/dx)+1
   np=min(np,mxnw-1)
   dx=xrange/(dble(np)-one)
   dxi=one/dx
   nws=0
   ncs=0

! --- Fill first element of marching arrays using values at xlow
   dist(1)=xlow+xbadj
   if(lwak) then
      asigz(1)=szi
      asigy(1)=syi
! ---    Set inital plume growth rate in wake to zero
      dsz(1)=zero
   endif
   if(lcav) then
      csigz(1)=szcav0
      csigy(1)=sycav0
   endif

! --- Initialize distance (from upwind face of bldg)
   x=xlow

! --- Loop over steps in wake region
! -----------------------------------
   do n=2,np
      xold=x
      x=x+dx
      dist(n)=dist(n-1)+dx

! ---    Check to see if cavity data should be revised based on
! ---    data from previous step
      if(lrevcav .and. xold>=xLb) then
         call wake_cav0(asigy(n-1),szcav0,sycav0r)
         if(sycav0r>sycav0) then
            sycav0=sycav0r
            sycav(1)=sycav0
! ---          Replace sigma-y values in stepping arrays
            do ir=1,n-1
               csigy(ir)=max(csigy(ir),sycav0)
            enddo
         endif
         lrevcav=.false.
      endif

! ---    Obtain sigmas for this step

! ---    First, persist initial values if upwind of starting point
      if(lwak .and. (xi>=x)) then
         asigz(n)=asigz(n-1)
         asigy(n)=asigy(n-1)
         dsz(n)=dsz(n-1)
! ---       Set index for skipping entry when filling wake arrays
         nws=n
      endif
      if(lcav .and. (xbc>=x)) then
         csigz(n)=szcav0
         csigy(n)=sycav0
! ---       Set index for skipping entry when filling cav arrays
         ncs=n
      endif

! ---    Now test again and apply full treatment when needed
      if(xold>xamx) then
! ---       Ambient growth region in wake: use virtuals
         if(lwak .and. (xi<x)) then
            vsigz = max( vsigz, szi )
            vsigy = max( vsigy, syi )
            call sigzpr(dist(n),z,asigz(n))
            call sigypr(dist(n),z,asigy(n))
            asigz(n) = dsqrt( asigz(n)**2 + vsigz**2 )
            asigy(n) = dsqrt( asigy(n)**2 + vsigy**2 )
            dsz(n)=(asigz(n)-asigz(n-1))*dxi
         endif
! ---       Cavity source ---
         if(lcav .and. (xbc<x)) then
            vsigzc = max( vsigzc, szcav0 )
            vsigyc = max( vsigyc, sycav0 )
            call sigzpr(dist(n),0.0d0,csigz(n))
            call sigypr(dist(n),0.0d0,csigy(n))
            csigz(n) = dsqrt( csigz(n)**2 + vsigzc**2 )
            csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
         endif
      else
         if(x<xamn) then
! ---          Wake growth for both sigz and sigy
! ---          Set x at mid-point of step
            xmid=half*(x+xold)
! ---          Compute turbulence intensities at midpoint
            call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
            if(lwak .and. (xi<=x)) then
! ---             Compute sigmas in wake
               call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),&
               &asigy(n-1),Hb,Wb,Rb,zk,yk,&
               &asigz(n),asigy(n),dsz(n))
            endif
! ---          Cavity source ---
            if(lcav .and. (xbc<=x)) then
               call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&
               &csigy(n-1),Hb,Wb,Rb,zkc,ykc,&
               &csigz(n),csigy(n),dzrate)
               csigz(n) = max( csigz(n), szcav0 )       ! ctmp cavity bug fix
            endif
         else
! ---          At least one of the sigmas reaches ambient growth in wake
! ---          Process SIGMA-Z
            if(xold>=xaz) then
! ---             Ambient growth region in wake: use virtual x
               if(lwak .and. (xi<=x)) then
                  call sigzpr(dist(n),z,asigz(n))
                  vsigz = max( vsigz, szi )
                  asigz(n) = dsqrt( asigz(n)**2 + vsigz**2 )
                  dsz(n)=(asigz(n)-asigz(n-1))*dxi
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=x)) then
                  call sigzpr(dist(n),0.0d0,csigz(n))
                  csigz(n) = dsqrt( csigz(n)**2 + vsigzc**2 )
                  csigz(n) = max( csigz(n), szcav0 )       ! ctmp cavity bug fix
               endif
            elseif(x>=xaz) then
! ---             Transition from wake to ambient
               xnew=xaz
               xmid=half*(xnew+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=xnew)) then
! ---                Compute wake sigma at xaz
                  call wake_sig(xnew,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zk,ykdum,&
                  &sigzxa,sydum,dzrate)
! ---                Get virtual source term as difference in quadrature between
! ---                wake and ambient sigmas at transition distance
                  call sigzpr(xaz+xbadj,z,sz)
                  if (sigzxa > sz) then
                     vsigz = dsqrt( sigzxa**2 - sz**2 )
                  else
                     vsigz = 0.0d0
                  end if
! ---                Now compute sigma at dist(n) with virtual source
                  call sigzpr(dist(n),z,asigz(n))
                  asigz(n) = dsqrt( asigz(n)**2 + vsigz**2 )
                  dsz(n)=(asigz(n)-asigz(n-1))*dxi
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=xnew)) then
! ---                Compute wake sigma at xaz
                  call wake_sig(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykdum,&
                  &sigzxa,sydum,dzrate)
! ---                Get virtual source term as difference in quadrature between
! ---                wake and ambient sigmas at transition distance
                  call sigzpr(xaz+xbadj,0.0d0,sz)
                  if (sigzxa > sz) then
                     vsigzc = dsqrt( sigzxa**2 - sz**2 )
                  else
                     vsigzc = 0.0d0
                  end if
                  vsigzc = max( vsigzc, szcav0 )           ! ctmp cavity bug fix
! ---                Now compute sigma at dist(n) with virtual source
                  call sigzpr(dist(n),0.0d0,csigz(n))
                  csigz(n) = dsqrt( csigz(n)**2 + vsigzc**2 )
               endif
            else
! ---             Wake growth for sigz
! ---             Set x at mid-point of step
               xmid=half*(x+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=x)) then
! ---                Compute sigmaz
                  call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zk,ykdum,&
                  &asigz(n),sydum,dsz(n))
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=x)) then
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykdum,&
                  &csigz(n),sydum,dzrate)
                  csigz(n) = max( csigz(n), szcav0 )       ! ctmp cavity bug fix
               endif
            endif
! ---          Process SIGMA-Y
            if(xold>=xay) then
! ---             Ambient growth region in wake: use virtual x
               if(lwak .and. (xi<=x)) then
                  call sigypr(dist(n),z,asigy(n))
                  vsigy = max( vsigy, syi )
                  asigy(n) = dsqrt( asigy(n)**2 + vsigy**2 )
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=x)) then
                  call sigypr(dist(n),0.0d0,csigy(n))
                  vsigyc = max( vsigyc, sycav0 )
                  csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
               endif
            elseif(x>=xay) then
! ---             Transition from wake to ambient
               xnew=xay
               xmid=half*(xnew+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=xnew)) then
! ---                Compute sigma at xay
!RWB                     call WAKE_SIG(xnew,xd,xold,turbz,turby,asigz(n-1),
!RWB                 turbz and turby appear to be the wrong variables for this
!RWB                 call to WAKE_SIG, try wakiz and wakiy
                  call wake_sig(xnew,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zkdum,yk,&
                  &szdum,sigyxa,dzrate)
! ---                Get virtual source term as difference in quadrature between
! ---                wake and ambient sigmas at transition distance
                  call sigypr(xay+xbadj,z,sy)
                  if (sigyxa > sy) then
                     vsigy = dsqrt( sigyxa**2 - sy**2 )
                  else
                     vsigy = 0.0d0
                  end if
! ---                Now compute sigma at dist(n) with virtual source
                  call sigypr(dist(n),z,asigy(n))
                  asigy(n) = dsqrt( asigy(n)**2 + vsigy**2 )
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=xnew)) then
                  call wake_sig(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkdum,ykc,&
                  &szdum,sigyxa,dzrate)
                  call sigypr(xay+xbadj,0.0d0,sy)
                  if (sigyxa > sy) then
                     vsigyc = dsqrt( sigyxa**2 - sy**2 )
                  else
                     vsigyc = 0.0d0
                  end if
                  call sigypr(dist(n),0.0d0,csigy(n))
                  csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
               endif
            else
! ---             Wake growth for sigy
! ---             Set x at mid-point of step
               xmid=half*(x+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=x)) then
! ---                Compute sigmay
                  call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zkdum,yk,&
                  &szdum,asigy(n),dzrate)
               endif
! ---             Cavity source
               if(lcav .and. (xbc<=x)) then
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkdum,ykc,&
                  &szdum,csigy(n),dzrate)
               endif
            endif
         endif
      endif

! --- Next distance
   enddo

! --- Construct arrays for /WAKEDAT/
! ----------------------------------

   if(lwak) then
! ---    WAK arrays:
      npw=np-nws

! ---    Place initial values into first element
      xwak(1)=xi+xbadj
      szwak(1)=szi
      sywak(1)=syi
      drwak(1)=zero
      if(npw>=mxntr) then
! ---       Sample a subset of the npw points
         nwak=mxntr
         xwak(nwak)=dist(np)
         szwak(nwak)=asigz(np)
         sywak(nwak)=asigy(np)
         drwak(nwak)=rtpiby2*dsz(np)
         if(npw<=2*mxntr) then
! ---          Fill elements with nearest values
            deln=dble(npw)/dble(nwak)
            do in=2,nwak-1
               jn=idint(dble(in)*deln)+nws
               xwak(in)=dist(jn)
               szwak(in)=asigz(jn)
               sywak(in)=asigy(jn)
               drwak(in)=rtpiby2*dsz(jn)
            enddo
         else
! ---          Use sliding step-size to sample nearfield more frequently
            deln=2.0d0*dble(npw-mxntr)/dble(mxntr*(mxntr-1))
            rn=one
            do in=2,nwak-1
               rn=rn+one+dble(in-1)*deln
               jn=idint(rn)+nws
               xwak(in)=dist(jn)
               szwak(in)=asigz(jn)
               sywak(in)=asigy(jn)
               drwak(in)=rtpiby2*dsz(jn)
            enddo
         endif
      else
! ---       Fill only those elements used
         nwak=npw
         do in=2,npw
            inp=in+nws
            xwak(in)=dist(inp)
            szwak(in)=asigz(inp)
            sywak(in)=asigy(inp)
            drwak(in)=rtpiby2*dsz(inp)
         enddo
      endif
   endif

   if(lcav) then
! ---    CAV arrays:
      npc=np-ncs

! ---    Place initial values into first element
      xcav(1)=xbc+xbadj
      szcav(1)=szcav0
      sycav(1)=sycav0
      if(npc>=mxntr) then
! ---       Sample a subset of the npc points
         ncav=mxntr
         xcav(ncav)=dist(np)
         szcav(ncav)=csigz(np)
         sycav(ncav)=csigy(np)
         if(npc<=2*mxntr) then
! ---          Fill elements with nearest values
            deln=dble(npc)/dble(ncav)
            do in=2,ncav-1
               jn=idint(dble(in)*deln)+ncs
               xcav(in)=dist(jn)
               szcav(in)=csigz(jn)
               sycav(in)=csigy(jn)
            enddo
         else
! ---          Use sliding step-size to sample nearfield more frequently
            deln=2.0d0*dble(npc-mxntr)/dble(mxntr*(mxntr-1))
            rn=one
            do in=2,ncav-1
               rn=rn+one+dble(in-1)*deln
               jn=idint(rn)+ncs
               xcav(in)=dist(jn)
               szcav(in)=csigz(jn)
               sycav(in)=csigy(jn)
            enddo
         endif
      else
! ---       Fill only those elements used
         ncav=npc
         do in=2,npc
            inp=in+ncs
            xcav(in)=dist(inp)
            szcav(in)=csigz(inp)
            sycav(in)=csigy(inp)
         enddo
      endif
   endif

   if(ldbhr) then

      write(dbgunt,*)
      write(dbgunt,*)'----- WAKE_DFSN:        NWAK = ',nwak
      write(dbgunt,*)'Z-dispersion reaches ambient at: ', xaz+xbadj
      write(dbgunt,*)'Y-dispersion reaches ambient at: ', xay+xbadj
!aer         write(DBGUNT,*)'z,y virtual dist (m) - WAKE  = ',xzvwak,xyvwak
!aer         write(DBGUNT,*)'z,y virtual dist (m) - CAV   = ',xzvcav,xyvcav
      write(dbgunt,'(1x,a,2x,3(f14.5,2x))')&
      &'xadj, yadj, xi  (m) = ', xbadj,ybadj,xi
      write(dbgunt,'(1x,a,2x,3(f14.5,2x))')&
      &'xbc, distc, xdc (m) = ', xbc,distc,xdc
      write(dbgunt,*) 'lwak,  nws,  npw    = ', lwak,nws,npw
      write(dbgunt,*) 'lcav,  ncs,  npc    = ', lcav,ncs,npc
      write(dbgunt,*)
!
! ---    Write the arrays passed back to the calling routine
      write(dbgunt,28)
28    format(/4x,'I',9x,'XWAK',6x,'SZWAK',6x,'SYWAK',6x,'DRWAK',/)
      do i=1,nwak
         write(dbgunt,32)i,xwak(i),szwak(i),sywak(i),drwak(i)
32       format(i5,3x,4(f10.4,1x))
      enddo
      write(dbgunt,*)

      write(dbgunt,29)
29    format(/4x,'I',9x,'XCAV',6x,'SZCAV',6x,'SYCAV',/)
      do i=1,ncav
         write(dbgunt,33)i,xcav(i),szcav(i),sycav(i)
33       format(i5,3x,3(f10.4,1x))
      enddo
      write(dbgunt,*)
   endif

   if (awmadwdbg) then
      write(awmadwdbunt,*)
      write(awmadwdbunt,*)'----- WAKE_DFSN:        NWAK = ',nwak
      write(awmadwdbunt,*)'Z-disp reaches ambient at xaz+xbadj: ',&
      &xaz+xbadj
      write(awmadwdbunt,*)'Y-disp reaches ambient at xay+xbadj: ',&
      &xay+xbadj
      write(awmadwdbunt,'(1x,a,2x,3(f14.5,2x))')&
      &'xadj, yadj, xi  (m) = ', xbadj,ybadj,xi
      write(awmadwdbunt,'(1x,a,2x,3(f14.5,2x))')&
      &'xbc, distc, xdc (m) = ', xbc,distc,xdc
      write(awmadwdbunt,*) 'lwak,  nws,  npw    = ', lwak,nws,npw
      write(awmadwdbunt,*) 'lcav,  ncs,  npc    = ', lcav,ncs,npc
      write(awmadwdbunt,*)
   end if
   return
end subroutine wake_dfsn

!-----------------------------------------------------------------------
subroutine wake_dfsn2(ldbhr,xi,szi,syi,xtr,ztr,ntr,dbgunt)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  010706             WAKE_DFSN2
!                R.W. Brode, PES, Inc.
!
! --- PURPOSE: This is a modified version of WAKE_DFSN that is called
!              after plume height array is completed in order to use
!              actual plume heights in calls to SIGYPR and SIGZPR to
!              obtain virtual source sigma-y and sigma-z terms and for
!              portions of sigma tables that include ambient dispersion.
!              NOTE: Much of this code is redundant with WAKE_DFSN, and
!              may be modified later to be more efficient.
!
! --- MODIFIED: To initialize dummy variables zkdum and ykdum to 0.0.
!               Otherwise these variables may be undefined in subrountine
!               WAKE_SIG under some circumstances.
!               R.W. Brode, MACTEC (f/k/a PES), Inc. - 09/01/05
!
! --- MODIFIED: To modify calling arguments in call to subroutine
!               WAKE_SIG from turby and turbz to wakiy and wakiz.
!               R.W. Brode, MACTEC (PES), Inc. - 07/23/04
!
! --- INPUTS:
!            ldbhr - logical     - Flag for debug write statements
!                                  to upwind bldg wall
!               xi - real        - distance (m) from upwind bldg wall
!                                  to point where plume intersects wake
!              szi - real        - sigma-z (m) at xi
!              syi - real        - sigma-y (m) at xi
!         XTR(ntr) - real        - Downwind distance (m)
!         ZTR(ntr) - real        - Plume centerline height (m)
!              ntr - integer     - Array size for XTR and ZTR
!
!     Common block /PARAMS/ variables:
!           MXNTR, MXNW
!     Common block /WAKEDAT/ variables:
!           XBADJ, Hb, Wb, xLb, Rb, xLR
!
! --- OUTPUT:
!
!     Common block /WAKEDAT/ variables:
!           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
!           DRWAK(mxntr), XZVWAK, XYVWAK,
!           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
!           VSIGY, VSIGZ, VSIGYC, VSIGZC
!
! --- WAKE_DFSN called by:  NUMRISE
! --- WAKE_DFSN calls    :  SIGZPR, SIGYPR,
!                           WAKE_XA, WAKE_CAV0, WAKE_TURB, WAKE_SIG
!----------------------------------------------------------------------
!
   use PRIME_params
   use PRIME_numparm
   use PRIME_wakedat
   use main1, only : L_ORD_Cav, awmadwdbunt, awmadwdbg

   implicit none

! --- Define local variable arrays for fine-steps
   integer :: ntr,np,nws,ncs,n,ir,npw,in,jn,inp,npc,dbgunt

   double precision :: dist(mxnw),asigz(mxnw),asigy(mxnw),dsz(mxnw)
   double precision :: csigz(mxnw),csigy(mxnw)
   double precision :: xtr(ntr), ztr(ntr), zxay, zxaz
   double precision :: xi,szi,syi
!     JAT D065 8/9/21 DISTC SET BUT NEVER USED
!      DOUBLE PRECISION xamx,xamn,xay,zkdum,ykdum,xcave,distc,xbc
   double precision :: xamx,xamn,xay,zkdum,ykdum,xcave,xbc
   double precision :: xaz,xdc,szcav0,sycav0,xd,dx,xlow,xhi,xrange
   double precision :: dxi,x,xold,sycav0r,xmid,wakiz,wakiy,zk,yk,zkc,&
   &ykc,dzrate,xnew,sigzxa,sydum,sz,szdum,sigyxa,sy,&
   &deln,rn,zdist
   double precision, parameter :: zero=0.0d0, half=0.5d0, one=1.0d0,&
   &rtpiby2 = 1.25331413731550d0

   logical :: ldbhr,lcav,lwak,lrevcav

! --- Compute xa, where turbulent growth in the wake transitions
! --- to ambient growth rate, measured from upwind face of bldg
   call wake_xa(xLb,Rb,xaz,xay)
   xamx=max(xaz,xay)
   xamn=min(xaz,xay)

! --- Retrieve plume height at transition points from table
   call numgrad(xay+xbadj,xtr,ztr,ntr,zxay)
   call numgrad(xaz+xbadj,xtr,ztr,ntr,zxaz)

! --- Reinitialize virtual source sigma terms
   vsigy  = 0.0d0
   vsigz  = 0.0d0
   vsigyc = 0.0d0
   vsigzc = 0.0d0

! --- Initialize dummy variables for zk and yk
   zkdum = 0.0d0
   ykdum = 0.0d0

! --- Initialize CAVITY parameters
! --------------------------------
! --- Set distance from upwind face of bldg to END of cavity
   xcave=xLb+xLR
! --- Set distance from source to start of cavity
!     JAT D065 8/9/21 DISTC SET BUT NEVER USED
!      distc=xLb+xbadj
! --- Set downwind distance to effective cavity source (when present),
! --- from the upwind face of bldg
   xbc=max(xi,xLb)
   xbc=min(xbc,xcave)
! --- Location of downwind edge of PDF region from effective cavity
! --- source
   xdc=xbc+xLR
! --- Set initial sigmas for cavity source using sigma-y at xi
   call wake_cav0(syi,szcav0,sycav0)
! --- The cavity sigma-y will need to be revised if xi lies upwind of
! --- the downwind face of the bldg.
   if(xi<xLb) then
      lrevcav=.true.
   else
      lrevcav=.false.
   endif

! --- Determine if any plume material in cavity may be modeled
! ------------------------------------------------------------
! --- Initialize output arrays
   ncav=1
   xcav(1)=xbc+xbadj
   szcav(1)=szcav0
   sycav(1)=sycav0
   if(xi>=xcave) then
      lcav=.false.
      lrevcav=.false.
   else
      lcav=.true.
   endif

! --- Is plume affected by wake turbulence?
! -------------------------------------------
! --- Initialize output arrays
   nwak=1
   xwak(1)=xi+xbadj
   szwak(1)=szi
   sywak(1)=syi
   drwak(1)=zero
   if(xi>=xamx) then
      lwak=.false.
      if(ldbhr) then
         write(dbgunt,*)' '
         write(dbgunt,*)'----- WAKE_DFSN2:       NWAK = ',nwak
         write(dbgunt,*)'Z-dispersion reaches ambient at: ',xaz+xbadj
         write(dbgunt,*)'Y-dispersion reaches ambient at: ',xay+xbadj
!aer            write(DBGUNT,*)'z,y virtual distances (m)    = ',xzvwak,xyvwak
         write(dbgunt,*)'xadj, yadj, xi        (m)    = ',&
         &xbadj,ybadj,xi
         write(dbgunt,*)'Plume NOT altered by wake turbulence!'
         write(dbgunt,*)
      endif

      if(awmadwdbg) then
         write(awmadwdbunt,*)' '
         write(awmadwdbunt,*)'----- WAKE_DFSN2:       NWAK = ',nwak
         write(awmadwdbunt,*)'Z-dispersion reaches ambient at: '&
         &,xaz+xbadj
         write(awmadwdbunt,*)'Y-dispersion reaches ambient at: ',&
         &xay+xbadj
         write(awmadwdbunt,*)'xadj, yadj, xi        (m)    = ',&
         &xbadj,ybadj,xi
         write(awmadwdbunt,*)'Plume NOT altered by wake turbulence!'
         write(awmadwdbunt,*)
      endif
   else
      lwak=.true.
   endif

! --- Return now if sigmas in wake do not need to be tabulated
   if(.not.lwak .and. .not.lcav) return

! --- Compute location of downwind edge of PDF region from xi
   xd=xi+xLR

! --- Set stepping parameters
   dx=2.0d0
! --- Range of table is from point of entry into wake (xi), to the point
! --- at which ambient growth rate resumes (xa), plus one "ds" so that
! --- both sigmas reach ambient, and virtual distances are computed.
! --- When cavity sigmas are also computed, range may start at the
! --- downwind bldg face, and extend to end of cavity.
   xlow=xi
   xhi=xamx
   if(lcav) then
      xlow=min(xi,xbc)
      xhi=max(xamx,xcave)
   endif
   xrange=xhi-xlow+dx
   np=idnint(xrange/dx)+1
   np=min(np,mxnw-1)
   dx=xrange/(dble(np)-one)
   dxi=one/dx
   nws=0
   ncs=0

! --- Fill first element of marching arrays using values at xlow
   dist(1)=xlow+xbadj
   if(lwak) then
      asigz(1)=szi
      asigy(1)=syi
! ---    Set inital plume growth rate in wake to zero
      dsz(1)=zero
   endif
   if(lcav) then
      csigz(1)=szcav0
      csigy(1)=sycav0
   endif

! --- Initialize distance (from upwind face of bldg)
   x=xlow

   if(awmadwdbg) then
      write(awmadwdbunt,*)' '
      write(awmadwdbunt,*)'----- WAKE_DFSN2:'
      write(awmadwdbunt,*)'       xaz  = distance (m) from upwind '
      write(awmadwdbunt,*)'              bldg wall at which wake '
      write(awmadwdbunt,*)'              turbulence Iz = ambient'
      write(awmadwdbunt,*)'       zxaz = plume height at transition'
      write(awmadwdbunt,*)'              points from plume ht array'

!         write (AWMADWDBUNT, 6000) n,np, xaz+xbadj,zxaz
! 6000    format(' ', / ,'  dfsn2(0): wake growth for sigz, sigy -',
!     &              ' step ',i4,' of ',i4,'; xaz+xbadj, zxaz =', 2F10.4)
   end if

! --- Loop over steps in wake region
! -----------------------------------
   do n=2,np
      xold=x
      x=x+dx
      dist(n)=dist(n-1)+dx

! ---    Check to see if cavity data should be revised based on
! ---    data from previous step
      if(lrevcav .and. xold>=xLb) then
         call wake_cav0(asigy(n-1),szcav0,sycav0r)
         if(sycav0r>sycav0) then
            sycav0=sycav0r
            sycav(1)=sycav0
! ---          Replace sigma-y values in stepping arrays
            do ir=1,n-1
               csigy(ir)=max(csigy(ir),sycav0)
            enddo
         endif
         lrevcav=.false.
      endif

! ---    Obtain sigmas for this step

! ---    First, persist initial values if upwind of starting point
      if(lwak .and. (xi>=x)) then
         asigz(n)=asigz(n-1)
         asigy(n)=asigy(n-1)
         dsz(n)=dsz(n-1)
! ---       Set index for skipping entry when filling wake arrays
         nws=n
      endif
      if(lcav .and. (xbc>=x)) then
         csigz(n)=szcav0
         csigy(n)=sycav0
! ---       Set index for skipping entry when filling cav arrays
         ncs=n
      endif

! ---    Now test again and apply full treatment when needed
      if(xold>xamx) then
! ---       Ambient growth region in wake: use virtuals
         if(lwak .and. (xi<x)) then
            vsigz = max( vsigz, szi )
            vsigy = max( vsigy, syi )
            call numgrad(dist(n),xtr,ztr,ntr,zdist)
            call sigzpr(dist(n),zdist,asigz(n))
            call sigypr(dist(n),zdist,asigy(n))
            asigz(n) = dsqrt( asigz(n)**2 + vsigz**2 )
            asigy(n) = dsqrt( asigy(n)**2 + vsigy**2 )
            dsz(n)=(asigz(n)-asigz(n-1))*dxi
         endif
! ---       Cavity source ---
         if(lcav .and. (xbc<x)) then
            vsigzc = max( vsigzc, szcav0 )
            vsigyc = max( vsigyc, sycav0 )
            call sigzpr(dist(n),0.0d0,csigz(n))
            call sigypr(dist(n),0.0d0,csigy(n))
            csigz(n) = dsqrt( csigz(n)**2 + vsigzc**2 )
            csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
         endif
      else
         if(x<xamn) then
! ---          Wake growth for both sigz and sigy
! ---          Set x at mid-point of step
            xmid=half*(x+xold)
! ---          Compute turbulence intensities at midpoint
            call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
            if(lwak .and. (xi<=x)) then
! ---             Compute sigmas in wake
               call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),&
               &asigy(n-1),Hb,Wb,Rb,zk,yk,&
               &asigz(n),asigy(n),dsz(n))
            endif

! ---          Cavity source ---
            if( L_ORD_Cav )then                                 ! ORD (EMM) change
! ---             ORD Downwash Modification
               if (lcav.and.(xbc<=x).and.(x>=xcave)) then        ! ORD (EMM) change
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&   ! ORD (EMM) change
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykc,&
                  &csigz(n),csigy(n),dzrate)
                  csigz(n) = max( csigz(n), szcav0 )                 ! ctmp cavity bug fix
               elseif(lcav.and.(xbc<=x).and.(x<xcave)) then     ! ORD (EMM) change
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&   ! ORD (EMM) change
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykc,&
                  &csigz(n),csigy(n),dzrate)

                  csigz(n) = 1.00d0*csigz(1)                         ! ORD (EMM) change ** added to write over above
                  csigy(n) = 1.00d0*csigy(1)                         ! ORD (EMM) change ** added to write over above
               endif

            else
! ---             Regulatory AERMOD code
               if(lcav .and. (xbc<=x)) then
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykc,&
                  &csigz(n),csigy(n),dzrate)
                  csigz(n) = max( csigz(n), szcav0 )         !ctmp cavity bug fix
               endif
            end if

         else
! ---          At least one of the sigmas reaches ambient growth in wake
! ---          Process SIGMA-Z
            if(xold>=xaz) then
! ---             Ambient growth region in wake: use virtual x
               if(lwak .and. (xi<=x)) then
                  call numgrad(dist(n),xtr,ztr,ntr,zdist)
                  call sigzpr(dist(n),zdist,asigz(n))
                  vsigz = max( vsigz, szi )
                  asigz(n) = dsqrt( asigz(n)**2 + vsigz**2 )
                  dsz(n)=(asigz(n)-asigz(n-1))*dxi
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=x)) then
                  vsigzc = max( vsigzc, szcav0 )           ! ctmp cavity bug fix
                  call sigzpr(dist(n),0.0d0,csigz(n))
                  csigz(n) = dsqrt( csigz(n)**2 + vsigzc**2 )
               endif
            elseif(x>=xaz) then
! ---             Transition from wake to ambient
               xnew=xaz
               xmid=half*(xnew+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=xnew)) then
! ---                Compute wake sigma at xaz
                  call wake_sig(xnew,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zk,ykdum,&
                  &sigzxa,sydum,dzrate)
! ---                Get virtual source term as difference in quadrature between
! ---                wake and ambient sigmas at transition distance
                  call sigzpr(xaz+xbadj,zxaz,sz)
                  if (sigzxa > sz) then
                     vsigz = dsqrt( sigzxa**2 - sz**2 )
                  else
                     vsigz = 0.0d0
                  end if
! ---                Now compute sigma at dist(n) with virtual source
                  call numgrad(dist(n),xtr,ztr,ntr,zdist)
                  call sigzpr(dist(n),zdist,asigz(n))
                  asigz(n) = dsqrt( asigz(n)**2 + vsigz**2 )
                  dsz(n)=(asigz(n)-asigz(n-1))*dxi
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=xnew)) then
! ---                Compute wake sigma at xaz
                  call wake_sig(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykdum,&
                  &sigzxa,sydum,dzrate)
! ---                Get virtual source term as difference in quadrature between
! ---                wake and ambient sigmas at transition distance
                  call sigzpr(xaz+xbadj,0.0d0,sz)
                  if (sigzxa > sz) then
                     vsigzc = dsqrt( sigzxa**2 - sz**2 )
                  else
                     vsigzc = 0.0d0
                  end if
                  vsigzc = max( vsigzc, szcav0 )           ! ctmp cavity bug fix
! ---                Now compute sigma at dist(n) with virtual source
                  call sigzpr(dist(n),0.0d0,csigz(n))
                  csigz(n) = dsqrt( csigz(n)**2 + vsigzc**2 )
               endif
            else
! ---             Wake growth for sigz
! ---             Set x at mid-point of step
               xmid=half*(x+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=x)) then
! ---                Compute sigmaz
                  call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zk,ykdum,&
                  &asigz(n),sydum,dsz(n))
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=x)) then
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkc,ykdum,&
                  &csigz(n),sydum,dzrate)
                  csigz(n) = max( csigz(n), szcav0 )       ! ctmp cavity bug fix
               endif
            endif
! ---          Process SIGMA-Y
            if(xold>=xay) then
! ---             Ambient growth region in wake: use virtual x
               if(lwak .and. (xi<=x)) then
                  call numgrad(dist(n),xtr,ztr,ntr,zdist)
                  call sigypr(dist(n),zdist,asigy(n))
                  vsigy = max( vsigy, syi )
                  asigy(n) = dsqrt( asigy(n)**2 + vsigy**2 )
               endif

! ---             Cavity source ---
               if( L_ORD_Cav )then                              ! ORD (EMM) change
! ---                Modifications for AWMADWNW
                  if(lcav.and.(x>=xcave).and.(xbc<=x)) then      ! ORD (EMM) change
                     call sigypr(dist(n),0.0d0,csigy(n))
                     vsigyc = max( vsigyc, sycav0 )
                     csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
                  elseif(lcav.and.(x<xcave).and.(xbc<=x)) then  ! ORD (EMM) change
                     call sigypr(dist(n),0.0d0,csigy(n))             ! ORD (EMM) change
                     vsigyc = max( vsigyc, sycav0 )                  ! ORD (EMM) change
!                        csigy(n) = DSQRT( csigy(n)**2 + vsigyc**2 )     ! ORD (EMM) change
                     csigy(n) = 1.00d0*csigy(1)                      ! ORD (EMM) change
                  endif

               else
! ---                Regulatory AERMOD code
                  if(lcav .and. (xbc<=x)) then
                     call sigypr(dist(n),0.0d0,csigy(n))
                     vsigyc = max( vsigyc, sycav0 )
                     csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
                  endif
               end if

            elseif(x>=xay) then
! ---             Transition from wake to ambient
               xnew=xay
               xmid=half*(xnew+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=xnew)) then
! ---                Compute sigma at xay
!RWB                 Modify call to WAKE_SIG to include wakiz and wakiy
!RWB                 instead of turbz and turby.
!RWB                     call WAKE_SIG(xnew,xd,xold,turbz,turby,asigz(n-1),
                  call wake_sig(xnew,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zkdum,yk,&
                  &szdum,sigyxa,dzrate)
! ---                Get virtual source term as difference in quadrature between
! ---                wake and ambient sigmas at transition distance
                  call sigypr(xay+xbadj,zxay,sy)
                  if (sigyxa > sy) then
                     vsigy = dsqrt( sigyxa**2 - sy**2 )
                  else
                     vsigy = 0.0d0
                  end if
! ---                Now compute sigma at dist(n) with virtual source
                  call numgrad(dist(n),xtr,ztr,ntr,zdist)
                  call sigypr(dist(n),zdist,asigy(n))
                  asigy(n) = dsqrt( asigy(n)**2 + vsigy**2 )
               endif
! ---             Cavity source ---
               if(lcav .and. (xbc<=xnew)) then
                  call wake_sig(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkdum,ykc,&
                  &szdum,sigyxa,dzrate)
                  call sigypr(xay+xbadj,0.0d0,sy)
                  if (sigyxa > sy) then
                     vsigyc = dsqrt( sigyxa**2 - sy**2 )
                  else
                     vsigyc = 0.0d0
                  end if
                  call sigypr(dist(n),0.0d0,csigy(n))
                  csigy(n) = dsqrt( csigy(n)**2 + vsigyc**2 )
               endif
            else
! ---             Wake growth for sigy
! ---             Set x at mid-point of step
               xmid=half*(x+xold)
! ---             Compute turbulence intensities at midpoint
               call wake_turb(xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .and. (xi<=x)) then
! ---                Compute sigmay
                  call wake_sig(x,xd,xold,wakiz,wakiy,asigz(n-1),&
                  &asigy(n-1),Hb,Wb,Rb,zkdum,yk,&
                  &szdum,asigy(n),dzrate)
               endif
! ---             Cavity source
               if(lcav .and. (xbc<=x)) then
                  call wake_sig(x,xdc,xold,wakiz,wakiy,csigz(n-1),&
                  &csigy(n-1),Hb,Wb,Rb,zkdum,ykc,&
                  &szdum,csigy(n),dzrate)
               endif
            endif
         endif
      endif

! --- Next distance
   enddo

! --- Construct arrays for /WAKEDAT/
! ----------------------------------

   if(lwak) then
! ---    WAK arrays:
      npw=np-nws

! ---    Place initial values into first element
      xwak(1)=xi+xbadj
      szwak(1)=szi
      sywak(1)=syi
      drwak(1)=zero
      if(npw>=mxntr) then
! ---       Sample a subset of the npw points
         nwak=mxntr
         xwak(nwak)=dist(np)
         szwak(nwak)=asigz(np)
         sywak(nwak)=asigy(np)
         drwak(nwak)=rtpiby2*dsz(np)
         if(npw<=2*mxntr) then
! ---          Fill elements with nearest values
            deln=dble(npw)/dble(nwak)
            do in=2,nwak-1
               jn=idint(dble(in)*deln)+nws
               xwak(in)=dist(jn)
               szwak(in)=asigz(jn)
               sywak(in)=asigy(jn)
               drwak(in)=rtpiby2*dsz(jn)
            enddo
         else
! ---          Use sliding step-size to sample nearfield more frequently
            deln=2.0d0*dble(npw-mxntr)/dble(mxntr*(mxntr-1))
            rn=one
            do in=2,nwak-1
               rn=rn+one+dble(in-1)*deln
               jn=idint(rn)+nws
               xwak(in)=dist(jn)
               szwak(in)=asigz(jn)
               sywak(in)=asigy(jn)
               drwak(in)=rtpiby2*dsz(jn)
            enddo
         endif
      else
! ---       Fill only those elements used
         nwak=npw
         do in=2,npw
            inp=in+nws
            xwak(in)=dist(inp)
            szwak(in)=asigz(inp)
            sywak(in)=asigy(inp)
            drwak(in)=rtpiby2*dsz(inp)
         enddo
      endif
   endif

   if(lcav) then
! ---    CAV arrays:
      npc=np-ncs

! ---    Place initial values into first element
      xcav(1)=xbc+xbadj
      szcav(1)=szcav0
      sycav(1)=sycav0
      if(npc>=mxntr) then
! ---       Sample a subset of the npc points
         ncav=mxntr
         xcav(ncav)=dist(np)
         szcav(ncav)=csigz(np)
         sycav(ncav)=csigy(np)
         if(npc<=2*mxntr) then
! ---          Fill elements with nearest values
            deln=dble(npc)/dble(ncav)
            do in=2,ncav-1
               jn=idint(dble(in)*deln)+ncs
               xcav(in)=dist(jn)
               szcav(in)=csigz(jn)
               sycav(in)=csigy(jn)
            enddo
         else
! ---          Use sliding step-size to sample nearfield more frequently
            deln=2.0d0*dble(npc-mxntr)/dble(mxntr*(mxntr-1))
            rn=one
            do in=2,ncav-1
               rn=rn+one+dble(in-1)*deln
               jn=idint(rn)+ncs
               xcav(in)=dist(jn)
               szcav(in)=csigz(jn)
               sycav(in)=csigy(jn)
            enddo
         endif
      else
! ---       Fill only those elements used
         ncav=npc
         do in=2,npc
            inp=in+ncs
            xcav(in)=dist(inp)
            szcav(in)=csigz(inp)
            sycav(in)=csigy(inp)
         enddo
      endif
   endif

   if(ldbhr) then

      write(dbgunt,*)
      write(dbgunt,*)'----- WAKE_DFSN2:'
      write(dbgunt,*)'PRIMARY SOURCE:'
      write(dbgunt,*)' Lateral virtual source sigma,  VSIGY (m)  = ',&
      &vsigy
      write(dbgunt,*)' Vertical virtual source sigma, VSIGZ (m)  = ',&
      &vsigz
      write(dbgunt,*)'CAVITY SOURCE:'
      write(dbgunt,*)' Lateral virtual source sigma,  VSIGYC (m) = ',&
      &vsigyc
      write(dbgunt,*)' Vertical virtual source sigma, VSIGZC (m) = ',&
      &vsigzc
      write(dbgunt,*)

   endif

   if (awmadwdbg) then
      write(awmadwdbunt,*)
      write(awmadwdbunt,*)'----- WAKE_DFSN2:'
      write(awmadwdbunt,*)'PRIMARY SOURCE:'
      write(awmadwdbunt,*)' Lateral virtual source sigma,  ',&
      &'VSIGY (m)  = ', vsigy
      write(awmadwdbunt,*)' Vertical virtual source sigma, ',&
      &'VSIGZ (m)  = ', vsigz
      write(awmadwdbunt,*)'CAVITY SOURCE:'
      write(awmadwdbunt,*)' Lateral virtual source sigma,  ',&
      &'VSIGYC (m) = ', vsigyc
      write(awmadwdbunt,*)' Vertical virtual source sigma, ',&
      &'VSIGZC (m) = ', vsigzc
      write(awmadwdbunt,*)
   end if

   return
end subroutine wake_dfsn2

!-----------------------------------------------------------------------
subroutine wake_turb(xinp,l,Rinp,tiz,tiy)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812             WAKE_TURB
!                L. Schulman, D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Calculates turbulence intensity as a function of
!              location within the wake from modified Weil (1996)
!              analytical expressions
!
! --- MODIFIED: For use with the AERMOD model.  Added MAIN1 module to
!               access global AERMOD data.  Modified ambiy and ambiz to
!               use AERMOD turbulence intensities.
!               R.W. Brode, PES, Inc. - 07/05/01
!
!               Modified for AWMADWNW 5/25/18,
!               for enhanced calculation of tiz, tiy using sub wake_u_turb.
!
! --- INPUTS:
!              kst - integer     - PG stability class (1-6)
!            lrurl - logical     - Rural flag (T=Rural, F=Urban)
!             xinp - real        - distance (m) from upwind bldg wall
!                L - real        - dist (m) of downwind bldg wall from
!                                  upwind bldg wall
!                R - real        - wake scaling length (m)
!
!     Common block /DFSN/ variables:
!           wiz0,wiy0,wfz,wfy,
!           dua_ua,xdecay,xdecayi,
!           rurliz,rurliy,urbniz,urbniy
!
! --- OUTPUT:
!
!              tiz - real        - turbulence intensity sigw/u
!              tiy - real        - turbulence intensity sigv/u
!
! --- WAKE_TURB called by:  WAKE_DFSN
! --- WAKE_TURB calls    :  none
!----------------------------------------------------------------------
!
   use main1
   use PRIME_dfsn
   use prm2_wakedat

   implicit none

   double precision :: xinp, l, Rinp, tiz, tiy
   double precision :: ambiz, ambiy, fariz, fariy, xmL, xfac, xfrac
   double precision :: zhi, zlo
   integer          :: ndxbhi, ndxblo, ndxalo

   double precision, parameter :: one = 1.0d0, zero = 0.0d0
   double precision :: zdum ! forces wake_u_turb to use z from wake_u
   double precision :: dum  ! for consistent wake_u_turb argument list

! --- Specify ambient turbulence intensities from AERMOD effective parameters
   ambiz = sweff/ueff
   ambiy = sveff/ueff

! --- Compute asymptotic turbulence intensity in far wake
   fariz=min(wiz0,ambiz)
   fariy=min(wiy0,ambiy)

! --- AWMA version D20350
!     If AWMAUTurbHX option set and WAKE_DFSN2 is called in WAKE_TURB,
!       recompute effective parameters, turbulence intensities

   if (L_AWMA_UTurbHX .and. dfsn2call) then

! ---    Determine the plume rise at the current position, xinp
      if (xinp < xtr_sav(p2mxntr)) then
! ---          Interpolate in rise table to get gradual rise   ---   CALL NUMGRAD
         call numgrad(xinp,xtr_sav,ztr_sav,p2mxntr,Zeff_prm2)
      else
         Zeff_prm2 = ztr_sav(p2mxntr)
      end if

      zhi = Zeff_PRM2 + 5.0d0
      zlo = Zeff_PRM2 - 5.0d0

! ---    Recompute average values between ZLO and ZHI
      call locate(gridht, 1, mxglvl, zhi, ndxbhi)
      call locate(gridht, 1, mxglvl, zlo, ndxblo)

      ndxalo = ndxblo + 1
      call anyavg ( mxglvl, gridht, gridws, zlo,ndxalo,&
      &zhi,ndxbhi,ueff )

      call anyavg ( mxglvl, gridht, gridsv, zlo,ndxalo,&
      &zhi,ndxbhi,sveff )

      call anyavg ( mxglvl, gridht, gridsw, zlo,ndxalo,&
      &zhi,ndxbhi,sweff )

      call anyavg ( mxglvl, gridht, gridtg, zlo,ndxalo,&
      &zhi, ndxbhi, tgeff )

! ---    Specify ambient turbulence intensities from AWMA effective parameters
      ambiz = sweff/ueff
      ambiy = sveff/ueff

! --- Compute asymptotic turbulence intensity in far wake
!         fariz and fariy are not used in further calculaitons
!         fariz=MIN(wiz0,ambiz)
!         fariy=MIN(wiy0,ambiy)

   end if

! --- AWMA version D20350
   if (L_AWMA_UTurb .or. L_AWMA_UTurbHX) then
! ---    Modifications for AWMADWNW
!        ZINP is passed from WAKE_DFSN (xmid,...)
      zdum = -1.d0 ! ensure that prior z from wake_u is used
      call wake_u_turb ('turb', xinp, zdum, ambiz, ambiy, dum,&
      &tiz,tiy)
   else
! ---    Original AERMOD code
! ---     Compute turbulence intensity at position downwind of bldg
      xmL=max(zero,xinp-l)
      xfac=one/(((xmL+Rinp)/Rinp)**xdecay-dua_ua)
      tiz=fariz*(one+((wfz*wiz0/fariz-one)+dua_ua)*xfac)
      tiy=fariy*(one+((wfy*wiy0/fariy-one)+dua_ua)*xfac)
   end if

! --- Interpolate turbulence intensity if over roof of bldg
   if (xinp<l) then
      xfrac=xinp/l
      tiz=ambiz+xfrac*(tiz-ambiz)
      tiy=ambiy+xfrac*(tiy-ambiy)
   endif

! --- Reset the effective parameters to the original values computed in
!       PRMCALC
! --- AWMA version D20350
   if (L_AWMA_UTurbHX) then
      Ueff  = Ueff_save
      SWeff = SWeff_save
      SVeff = SVeff_save
   end if

   return
end subroutine wake_turb

!-----------------------------------------------------------------------
subroutine wake_u(ldb,x,y,z,ubyua,dufac,dbgunt)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  990726 (99207)           WAKE_U
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Calculates speed ratio u(wake)/u(ambient) as a function
!              of location within the wake
!
!              Modified by B. de Foy, 26th July 1999,
!              To set fmin as a minimum value for ubyua
!
!              Modified for AWMA PRIME2 Subcommitee Downwash 5/25/2018,
!              Enhanced calculation of du_ua using subroutine wake_u_turb
!
! --- INPUTS:
!              ldb - logical     - flag for debug output
!                x - real        - downwind distance (m) from upwind
!                                  bldg wall
!                y - real        - crosswind distance (m) from center of
!                                  upwind bldg wall
!                z - real        - height (m) above ground
!
!     Common block /PARAMS/ variables:
!           MXNTR, MXNW
!     Common block /WAKEDAT/ variables:
!           Hb, Wb, xLb, Rb, xLR
!     Common block /DFSN/ variables:
!           dua_ua,xdecay,xdecayi
!
! --- OUTPUT:
!
!            ubyua - real        - U(x,z)/Ua speed in wake scaled by
!                                  ambient speed
!            dufac - real        - Gradient in speed factor above
!                                  Zcav
!
! --- WAKE_U called by:  NUMRISE, WAKE_DBG
! --- WAKE_U calls    :  CAVITY_HT, WAKE_DIM
!----------------------------------------------------------------------
!
   use main1, only : L_AWMA_UTurb, L_AWMA_UTurbHX
   use PRIME_params
   use PRIME_dfsn
   use PRIME_wakedat

   implicit none
   integer :: dbgunt
   double precision :: x,y,z,zcav,ycav,hwake,wwake,yabs,&
   &ubyua,dufac,ymin,du_ua,ydiff,xml,xfrac,ucbyua,&
   &zz

   double precision, parameter :: zero=0.0d0, one=1.0d0, two=2.0d0,&
   &fmin=0.01d0

   double precision :: dum    ! used for for consistency in wake_u_turb arg list
   logical :: ldb

! --- Compute cavity height above ground, and width
   call cavity_ht(Hb,Wb,xLb,Rb,xLC,xLR,hr,x,zcav,ycav)

! --- Compute far wake height above ground, and width
   call wake_dim(x,Hb,Wb,Rb,hwake,wwake)

! --- Return "null" values if point is outside wake
   yabs  = dabs(y)
   ubyua = one
   dufac = zero
   if(z>=hwake .or. yabs>=wwake) return

! --- AWMA version D20350
   if (L_AWMA_UTurb .or. L_AWMA_UTurbHX) then
! ---    Modifications for AWMADWNW
! ---    Get new value of velocity deficit
      call wake_u_turb ('vel',x, z, dum, dum, du_ua, dum, dum)

! ---    Adjust "base" speed deficit dua_ua if lateral position is
! ---    beyond bldg width projection, but within the wake
      ymin  = max(0.5d0*Wb,wwake-Rb/3.0d0)
      ydiff = wwake-ymin
      if(yabs>ymin .and. ydiff>zero) then
         du_ua = du_ua * (one-(yabs-ymin)/ydiff)
      endif

   else
! ---    Regulatory AERMOD
! ---    Adjust "base" speed deficit dua_ua if lateral position is
! ---    beyond bldg width projection, but within the wake
      ymin  = max(0.5d0*Wb,wwake-Rb/3.0d0)
      du_ua = dua_ua
      ydiff = wwake-ymin
      if(yabs>ymin .and. ydiff>zero) then
         du_ua = dua_ua*(one-(yabs-ymin)/ydiff)
      endif

! ---    Scale speed deficit (Ua-U)/Ua =  du_ua in wake for
! ---    position x downwind of bldg face
      xmL   = max(zero,x-xLb)
      du_ua = du_ua*((xmL+Rb)/Rb)**(-xdecay)

   end if

! --- Interpolate factor if over roof of bldg (linear)
   if(x<xLb) then
      xfrac=x/xLb
      du_ua=xfrac*du_ua
   endif

! --- Compute speed factor Ucav/Ua at top of cavity
! --- Assume that speed is constant below ZCAV, and increases linearly
! --- with height to ambient speed at HWAKE
   ucbyua=max(zero,(one-two*hwake*du_ua/(hwake+zcav)))

! --- Compute gradient in speed factor (zero below Zcav)
   dufac=zero
   if(z>zcav) then
      dufac=(one-ucbyua)/(hwake-zcav)
   endif

! --- Compute speed factor U/Ua at height z
   zz=min(z,hwake)
! --- Ensure fmin as lower bound for ubyua
   ubyua=max(fmin,(ucbyua+dufac*(zz-zcav)))

   if(ldb) then
      write(dbgunt,*)'WAKE_U         '
      write(dbgunt,*)'       x,y,z = ',x,y,z
      write(dbgunt,*)'hwake, zcav  = ',hwake, zcav
      write(dbgunt,*)'wwake, ymin  = ',wwake, ymin
      write(dbgunt,*)'du_ua, ucbyua= ',du_ua, ucbyua
      write(dbgunt,*)'ubyua, dufac = ',ubyua,dufac
      write(dbgunt,*)
   endif

   return
end subroutine wake_u

!-----------------------------------------------------------------------
subroutine wake_u_turb&
&(scope, xinp, znew, ambiz, ambiy, du_ua, tiz, tiy)
   !-----  ---  ---
!-----------------------------------------------------------------------
! --- New subroutine for AWMADWNW 5/25/2018
!
! --- PURPOSE: Calculate all new equations for enhanced wake flow as
!              needed by subroutines wake_u, wake_turb.
!
! --- INPUTS:
!        scope
!        xinp
!        znew   -  When called from wake_u, will be > 0 and used for z.
!                    When called from wake_turb, will be < 0 and previous
!                    value of z will be used.
!        ambiz -  Only needed when scope = 'turb'. Otherwise, a dummy
!        ambiy -     "
!        du_ua_new - [similar to znew]
!        zeff_PRM2 - Effective height (from module prm2_wakedat)
!
! --- OUTPUT:
!        du_ua -  Needed when called from wake_u. Otherwise for internal use
!        tiz   -  Needed when called from wake_turb. Otherwise ignored
!        tiy   -     "
!
! --- CALLED BY:
!        wake_u
!        wake_turb
!
! --- CALLS:      None
!-----------------------------------------------------------------------
   use main1, only: l_strmln_bldg, l_rect_bldg, L_AWMA_UTurbHX,&
   &awmadwdbunt, awmadwdbg
   use prime_wakedat, only: hb, wb, hr, xLb, mxntr
   use prm2_wakedat, only:  dfsn2call, xtr_sav, ztr_sav

   use prm2_wakedat

   implicit none

!    ! args
   character(*), intent (in)        :: scope
   double precision, intent (in)    :: xinp,znew, ambiz, ambiy
   double precision, intent (out)   :: du_ua
   double precision, intent (out)   :: tiz, tiy
!    ! constants
   double precision ::&
   &vufac,               svfac,               swfac,&
   &Au,                  Asv,                 Asw,&
   &nu,                  nsv,                 nsw,&
   &Bu,                  Bsv,                 Bsw,&
   &ru,                  rsv,                 rsw,&
   &LrUDfac,             LrSVfac,             LrSWfac,&
   &HWminU,              HWminSV,             HWminSW,&
   &HLminU,              HLminSV,             HLminSW,&
   &HWmaxU,              HWmaxSV,             HWmaxSW,&
   &HLmaxU,              HLmaxSV,             HLmaxSW

! Unused:      double precision iyo, izo

!     Additional "constants" needed for Bsv, Bsw, rsv, rsw

   double precision ::&
   &iy0,  iz0,&
   &ABsv, BBsv, CBsv,    ABsw, BBsw, CBsw,&
   &Arsv, Brsv, Crsv,    Arsw, Brsw, Crsw
! Unused:       DOUBLE PRECISION :: BsvMax, BsvMin, BswMax, BswMin, rsvMax, rsvMin, rswMax, rswMin

!     local vars
   double precision :: zudmax_hb, udmax !, dummy, dummyy, dummyyy
   double precision :: a1, a2           ! Parts of larger expressions
   double precision :: HWlimU, HWlimSW, HWlimSV
   double precision :: HLlimU, HLlimSW, HLlimSV
   double precision :: xLRUD, xLRSW, xLRSV
   double precision :: XiUD_HB          !! 1-17-2017
   double precision :: xml, eps
   double precision :: XiSW_HB, XiSV_HB
   double precision :: sw, SWmaxx, Zsw_HB, Zsv_HB
   double precision :: sv, SVmaxx
   double precision :: z, zvel_sav, zplm
! Unused:      DOUBLE PRECISION :: PorFacSW = 1.d0, PorFacUD = 1.d0

   save z, zvel_sav


!---- Set constants according to the building type (rect. vs. streamlined)

   if (l_rect_bldg ) then                         ! Rectangular bldgs
      vufac =  0.294d0;    svfac =  0.320d0;    swfac =  0.290d0
      Au =     0.500d0;    Asv =    0.700d0;    Asw =    0.650d0
      nu =     0.000d0;    nsv =    0.356d0;    nsw =    0.336d0
      Bu =     3.494d0   ! Bsv             ;    Bsw
      ru =     1.667d0   ! rsv             ;    rsw
      LrUDfac= 1.135d0;    LrSVfac= 1.349d0;    LrSWfac= 1.794d0
      HWminU = 0.449d0;    HWminSV= 0.200d0;    HWminSW= 0.375d0
      HLminU = 0.753d0;    HLminSV= 0.321d0;    HLminSW= 0.308d0
      HWmaxU = 3.00d0;     HWmaxSV= 3.00d0;     HWmaxSW= 3.00d0
      HLmaxU = 3.00d0;     HLmaxSV= 3.00d0;     HLmaxSW= 3.00d0
   else if (l_strmln_bldg) then                   ! Streamlined bldgs
      vufac =  0.294d0;    svfac =  0.225d0;    swfac =  0.225d0
      Au =     0.400d0;    Asv =    0.550d0;    Asw =    0.450d0
      nu =     0.631d0;    nsv =    0.477d0;    nsw =    0.570d0
      Bu =     2.113d0   ! Bsv             ;    Bsw
      ru =     1.796d0   ! rsv             ;    rsw
      LrUDfac= 0.563d0;    LrSVfac= 0.971d0;    LrSWfac= 0.640d0
      HWminU = 0.449d0;    HWminSV= 0.375d0;    HWminSW= 0.375d0
      HLminU = 0.753d0;    HLminSV= 0.308d0;    HLminSW= 0.308d0
      HWmaxU = 3.00d0;     HWmaxSV= 3.00d0;     HWmaxSW= 3.00d0
      HLmaxU = 3.00d0;     HLmaxSV= 3.00d0;     HLmaxSW= 3.00d0
   end if
!---- Get additional "constants" Bsv, Vsw, rsv, rsw

   if (l_rect_bldg ) then                         ! Rectangular bldgs
      ABsv = 0.018571d0;   BBsv = -1.10574d0;   CBsv = 18.30949d0
      ABsw = 0.045142d0;   BBsw = -2.1585d0;    CBsw = 29.33697d0
      Arsv = -0.00124d0;   Brsv = 0.033975d0;   Crsv = 1.431187d0
      Arsw = 0.000155d0;   Brsw = -0.00626d0;   Crsw = 1.562781d0
      !!!BsvMax = 9.036d0;    BsvMin = 1.856d0
      !!!BswMax = 16.593d0;   BswMin = 4.806d0
      !!!rsvMax = 1.648d0;    rsvMin = 1.369d0
      !!!rswMax = 1.527d0;    rswMin = 1.500d0
      iy0 = sv30 / u30 * 100.d0
      if (iy0 > 29.2d0) iy0 = 29.2d0
      if (iy0 < 5.0d0) iy0 = 5.0d0
      iz0 = sw30 / u30 * 100.d0
      if (iz0 > 18.6d0) iz0 = 18.6d0
      if (iz0 < 2.0d0) iz0 = 2.0d0
      Bsv = ABsv * iy0**2 + BBsv * iy0 + CBsv
      Bsw = ABsw * iz0**2 + BBsw * iz0 + CBsw
      rsv = Arsv * iy0**2 + Brsv * iy0 + Crsv
      rsw = Arsw * iz0**2 + Brsw * iz0 + Crsw

   else if (l_strmln_bldg) then                   ! Streamlined bldgs
      ABsv = 0.008223d0;   BBsv = -0.42265d0;   CBsv = 9.330299d0
      ABsw = 0.092864d0;   BBsw = -3.68931d0;   CBsw = 39.21321d0
      Arsv = 0.000861d0;   Brsv = 0.004194d0;   Crsv = 1.385942d0
      Arsw = 0.004627d0;   Brsw = -0.15270d0;   Crsw = 2.939305d0
      !!!BsvMax = 5.900d0;    BsvMin = 4.000d0
      !!!BswMax = 18.180d0;   BswMin = 2.720d0
      !!!rsvMax = 2.243d0;    rsvMin = 1.516d0
      !!!rswMax = 2.110d0;    rswMin = 1.700d0
      iy0 = sv30 / u30 * 100.d0
      iz0 = sw30 / u30 * 100.d0
      Bsv = ABsv * iy0**2 + BBsv * iy0 + CBsv
      Bsw = ABsw * iz0**2 + BBsw * iz0 + CBsw
      rsv = Arsv * iy0**2 + Brsv * iy0 + Crsv
      rsw = Arsw * iz0**2 + Brsw * iz0 + Crsw
   end if

   !!!IF (Bsv > BsvMax) Bsv = BsvMax
   !!!IF (Bsv < BsvMin) Bsv = BsvMin
   !!!IF (Bsw > BswMax) Bsw = BswMax
   !!!IF (Bsw < BswMin) Bsw = BswMin
   !!!IF (rsv > rsvMax) rsv = rsvMax
   !!!IF (rsv < rsvMin) rsv = rsvMin
   !!!IF (rsw > rswMax) rsw = rswMax
   !!!IF (rsw < rswMin) rsw = rswMin

!-----Get the value of z.
!       If scope = 'vel', znew is the value to use
!       If scope = 'turb', then use previous value or zeff_PRM2.

!     If < 0, then use previous value.
   if (scope == 'vel') then
      z = znew
      zvel_sav = znew
   elseif (scope == 'turb') then
      z = min(zvel_sav,zeff_PRM2)

! ---    AWMA version D20350
      if (L_AWMA_UTurbHX .and. dfsn2call) then
! ---       Conditional with call to WAKE_DFSN2
!           This is a repeat of what is found in wake_turb to get 'z';
!             with further thought the value of 'z' computed there could
!             be passed to wake_u_turb and used here such that the
!             following code could be removed
         if (xinp < xtr_sav(p2mxntr)) then
!              xinp is the input argument, which is xinp in CALL WAKE_U_TURB
! ---          Interpolate in rise table to get gradual rise ---   CALL NUMGRAD
            call numgrad(xinp,xtr_sav,ztr_sav,p2mxntr,Zplm)
            z = zplm
         else
            Zplm = ztr_sav(p2mxntr)
            z = zplm
         end if
      end if
   else
! ---    Coding error - should not get here in an AERMOD mode run
      print *, ' Improper u/turb scope in wake_u_turb'
      print *, '  - must be "vel" or "turb" (lower case)'
      stop
   endif

!---- Compute du_ua as needed by wake_u and wake_turb

   ! Eq (11)
   xml = max (0.d0, xinp-xLb)
   eps = xml + hb

   ! Eq (8a)
   HWlimU = hb / wb
   if (HWlimU < HWminU) HWlimU = HWminU
   if (HWlimU > HWmaxU) HWlimU = HWmaxU
   ! Eq (8b)
   HLlimU = hb / xLB
   if (HLlimU < HLminU) HLlimU = HLminU
   if (HLlimU > HLmaxU) HLlimU = HLmaxU

   ! Eq (7a) (dependent on 8a,8b)
   a1 = 1.d0 / HWlimU
   a2 = (1.d0 / HLlimU**.3d0) * (1.d0 + .24d0 / HWlimU)
   xLRUD = 1.8d0 * a1/a2

   ! Eq (10a) (dependent on 7a)
   XiUD_HB = max ((LRUDfac*xLRUD+1), eps/hb)

   ! Eq (5) (dependent on 8a, 8b, 10) (rev 1/17/2017)
   a1 = XiUD_HB * HWlimU**(2.d0/3.d0)
   a2 = HLlimU**(-nu)
   UDmax = Bu * (a1*a2)**(-ru)

   ! Eq (4a)
   ZUDmax_HB = au * (hr/hb)**1.5

   ! Eq (2) (dependent on 5,4a,z)
   !if (zc/HB >= ZUDMAX_HB) then
   if (z/hb >= zudmax_hb) then
      !if (zeff_PRM2/HB >= ZUDMAX_HB) then
      !A1 = (ZUDmax_HB - zc/HB)**2
      !A1 = (ZUDmax_HB - z/HB)**2
      a1 = (ZUDmax_HB - z/hb)**2
      a2 = (vufac**2) * XiUD_HB ! New eq 1-17-2017
      du_ua = UDmax * exp(-a1/a2)
   else
      du_ua = UDmax
   endif
   if (scope == 'vel') then
      if (awmadwdbg) write (awmadwdbunt, '(3A, 8(A, f9.3))')&
      &'    scope=', scope, ':',&
      &'  x=',xinp,&
      &'  xml=', xml,&
      &'  znew(inarg)=', znew,&
      &'  z=', z,&
      &'  eps/HB=', eps/hb,&
      &'  XiUD_HB=', XiuD_HB,&
      &'  UDmax=', UDmax,&
      &'  du_ua=', du_ua
   end if

!---- Can now bail if call was for wake_u.
!      Otherwise, continue to compute tiz, tiy as needed by wake_turb.

   if (scope == 'vel') return

   ! Eq (9a1)
   HWlimSW = hb / wb
   if (HWlimSW < HWminSW) HWlimSW = HWminSW
   if (HWlimSW > HWmaxSW) HWlimSW = HWmaxSW
   ! Eq (9a2)
   HLlimSW = hb / xLB
   if (HLlimSW < HLminSW) HLlimSW = HLminSW
   if (HLlimSW > HLmaxSW) HLlimSW = HLmaxSW

   ! Eq (9b1)
   HWlimSV = hb / wb
   if (HWlimSV < HWminSV) HWlimSV = HWminSV
   if (HWlimSV > HWmaxSV) HWlimSV = HWmaxSV
   ! Eq (9b2)
   HLlimSV = hb / xLB
   if (HLlimSV < HLminSW) HLlimSV = HLminSV
   if (HLlimSV > HLmaxSW) HLlimSV = HLmaxSV

   ! Eq (7b) (dependent on 9)
   a1 = 1.d0 / HWlimSW
   a2 = (1.d0 / HLlimSW**.3d0) * (1.d0 + .24d0 / HWlimSW)
   xLRSW = 1.8d0 * a1/a2
   ! Eq (7c) (dependent on 9b)
   a1 = 1.d0 / HWlimSV
   a2 = (1.d0 / HLlimSV**.3d0) * (1.d0 + .24d0 / HWlimSV)
   xLRSV = 1.8d0 * a1/a2

   ! Eq (10b) (dependent on 7b)
   XiSW_HB = max ((LrSWfac*xLRSW+1), eps/hb)
   ! Eq (10c) (dependent on 7c) (added 6/12/2017)
   XiSV_HB = max ((LrSVfac*xLRSV+1), eps/hb)

   ! Eq (6a) (dependent on 7,9) (Rev 1/7/2017)
   a1 = XiSW_HB * HWlimSW**(2.d0/3.d0)
   a2 = HLlimSW**(-nsw)
   SWmaxx = Bsw * (a1*a2)**(-rsw)
   ! Eq (6b) (dependent on 7,9) (added 6/12/2017)
   a1 = XiSV_HB * HWlimSV**(2.d0/3.d0)
   a2 = HLlimSV**(-nsv)
   SVmaxx = Bsv * (a1*a2)**(-rsv)

   ! Eq (4b)
   Zsw_HB = Asw * (hr/hb)**1.5d0
   ! Eq (4b) (added 6/12/2017)
   Zsv_HB = Asv * (hr/hb)**1.5d0

   ! Eq (3) (dependent on 4b)
   ! Eq (3a)
   !A1 = (Zsw_HB - zc/HB)**2
   !A1 = (Zsw_HB - z/HB)**2
   a1 = (Zsw_HB - z/hb)**2
   a2 = (swfac**2) * XiSW_HB !! New equation 1-17-2017
   !IF (zc/HB >= Zsw_HB) THEN
   !IF (z/HB >= Zsw_HB) THEN
   if (z/hb >= Zsw_HB) then
      sw = SWmaxx * exp(-a1/a2)
   else
      sw = SWmaxx
   end if
   ! Eq (3b) (dependent on 4c) (added 6/12/2017)
   a1 = (Zsv_HB - z/hb)**2
   a2 = (svfac**2) * XiSV_HB !! New equation 1-17-2017
   if (z/hb >= Zsv_HB) then
      sv = SVmaxx * exp(-a1/a2)
   else
      sv = SVmaxx
   end if

   ! Constraint on UD per RLP7/6/2017
!     D116 Update to AWMA Turbulence Enhancement Limit, 11/2/21
!     New constraints per RPetersen
   du_ua = min (du_ua, 0.9d0)
   ! Eq (1a)
   a1 = (1.d0 + sw) / (1.d0 - du_ua)
!      D116 Comment original constraint (50), update value to 18
!      tiz = ambiz * MIN (50.0d0, A1)
   tiz = ambiz * min ( 18.0d0, a1)
   tiz = min (0.9d0, tiz)
   ! Eq (1b)
   a1 = (1.d0 + sv) / (1.d0 - du_ua)
!      D116 Comment original constraint (50), update value to 6
!      tiz = ambiz * MIN (50.0d0, A1)
   tiy = ambiy * min ( 6.0d0, a1)
   tiy = min (0.9d0, tiy)

   if( awmadwdbg ) write (awmadwdbunt, '(3A, 19(A, f9.3))')&
   &'  scope=', scope, ':',&
   &'  x=', xinp,&
   &'  znew(inarg)=' ,znew,&
   &'  z=', z,&
   &'  eps/HB=', eps/hb,&
   &'  XiUD/HB=', XiuD_HB,&
   &'  XiSW/HB=', XiSW_HB,&
   &'  SW=', sw,&
   &'  SWmaxx=', SWmaxx,&
   &'  SV=', sv,&
   &'  SVmaxx=', SVmaxx,&
   &'  SW30=', sw30,&
   &'  SV30=', sv30,&
   &'  U30=', u30,&
   &'  tiz=', tiz,&
   &'  tiy=', tiy,&
   &'  Bsv=', Bsv,&
   &'  Bsw=', Bsw,&
   &'  rsv=', rsv,&
   &'  rsw=', rsw

end subroutine wake_u_turb

!-----------------------------------------------------------------------
subroutine wake_xa(l,Rinp,xaz,xay)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  980310               WAKE_XA
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Calculates the distance from the upwind face of the
!              building to the point at which the turbulence intensity
!              in the wake approaches that in the ambient flow.
!
!              Final distances are limited to "xbyrmax" scale-lengths
!              (R) set in prime1, measured from downwind bldg face
!
! --- MODIFIED: For use with the AERMOD model.  Added MAIN1 module to
!               access global AERMOD data.  Modified ambiy and ambiz to
!               use AERMOD turbulence intensities.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!              kst - integer     - PG stability class (1-6)
!            lrurl - logical     - Rural flag (T=Rural, F=Urban)
!                L - real        - dist (m) of downwind bldg wall from
!                                  upwind bldg wall
!                R - real        - wake scaling length (m)
!
!     Common block /DFSN/ variables:
!           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
!           dua_ua,xdecay,xdecayi,
!           rurliz,rurliy,urbniz,urbniy
!
! --- OUTPUT:
!
!              xaz - real        - distance (m) from upwind bldg wall
!                                  at which wake turbulence Iz = ambient
!              xay - real        - distance (m) from upwind bldg wall
!                                  at which wake turbulence Iy = ambient
!
! --- WAKE_XA called by:  WAKE_DFSN
! --- WAKE_XA calls    :  none
!----------------------------------------------------------------------
!

   use main1

   use PRIME_dfsn

   implicit none

   double precision :: l, Rinp, xaz, xay
   double precision :: ambiz, ambiy, fariz, fariy, farizt, fariyt,&
   &x0byr, xbyr
   double precision, parameter :: one=1.0d0

! --- Specify ambient turbulence intensities from AERMOD effective parameters
   ambiz = sweff/ueff
   ambiy = sveff/ueff

! --- Compute asymptotic turbulence intensity in far wake
   fariz=min(wiz0,ambiz)
   fariy=min(wiy0,ambiy)

! --- Define the turbulence intensity at the transition point
   farizt=max(ambiz,afac*fariz)
   fariyt=max(ambiy,afac*fariy)

! --- Compute leading term
   x0byr=l/Rinp-one

! --- Compute scaled distance at which Iz equals transition Iz
   xaz=x0byr+(dua_ua+(wfz*wiz0-fariz*(one-dua_ua))/&
   &(farizt-fariz))**xdecayi

! --- Compute distance at which Iy equals transition Iy
   xay=x0byr+(dua_ua+(wfy*wiy0-fariy*(one-dua_ua))/&
   &(fariyt-fariy))**xdecayi

! --- Cap distances
   xbyr=l/Rinp+xbyrmax
   xaz=Rinp*min(xbyr,xaz)
   xay=Rinp*min(xbyr,xay)

   if (primedbg) then
      write(prmdbunt,*) ' '
      write(prmdbunt,*) 'WAKE_XA Calculations:'
      write(prmdbunt,*) 'ambiz,  ambiy  = ', ambiz, ambiy
      write(prmdbunt,*) 'farizt, fariyt = ', farizt, fariyt
      write(prmdbunt,*) 'xaz,    xay    = ', xaz, xay
   end if

   return
end subroutine wake_xa

subroutine wake_xa2(l,Rinp,xaz,xay)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  980310               WAKE_XA2
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!                Modified version of wake_xa for use with AERMOD-PRIME
!                R. Brode, PES, Inc. - 8/9/01
!
! --- PURPOSE: Calculates the distance from the upwind face of the
!              building to the point at which the turbulence intensity
!              in the wake approaches that in the ambient flow.
!
!              Final distances are NOT limited to "xbyrmax" scale-lengths
!              (R) set in prime1.
!
! --- INPUTS:
!                L - real        - dist (m) of downwind bldg wall from
!                                  upwind bldg wall
!                Rinp - real     - wake scaling length (m)
!
!     Common block /DFSN/ variables:
!           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
!           dua_ua,xdecay,xdecayi,
!           rurliz,rurliy,urbniz,urbniy
!
! --- OUTPUT:
!
!              xaz - real        - distance (m) from upwind bldg wall
!                                  at which wake turbulence Iz = ambient
!              xay - real        - distance (m) from upwind bldg wall
!                                  at which wake turbulence Iy = ambient
!
! --- WAKE_XA called by:  GAMCALC
! --- WAKE_XA calls    :  none
!----------------------------------------------------------------------
!

   use main1

   use PRIME_dfsn

   implicit none

   double precision :: l, Rinp, xaz, xay
   double precision :: ambiz, ambiy, fariz, fariy, farizt, fariyt,&
   &x0byr
   double precision, parameter :: one=1.0d0

! --- Specify ambient turbulence intensities from AERMOD effective parameters
   ambiz = sweff/ueff
   ambiy = sveff/ueff

! --- Compute asymptotic turbulence intensity in far wake
   fariz=min(wiz0,ambiz)
   fariy=min(wiy0,ambiy)

! --- Define the turbulence intensity at the transition point
   farizt=max(ambiz,afac*fariz)
   fariyt=max(ambiy,afac*fariy)

! --- Compute leading term
   x0byr=l/Rinp-one

! --- Compute scaled distance at which Iz equals transition Iz
   xaz=x0byr+(dua_ua+(wfz*wiz0-fariz*(one-dua_ua))/&
   &(farizt-fariz))**xdecayi

! --- Compute distance at which Iy equals transition Iy
   xay=x0byr+(dua_ua+(wfy*wiy0-fariy*(one-dua_ua))/&
   &(fariyt-fariy))**xdecayi

! --- Apply scaling factor (without capping at 15R)
   xaz=Rinp*xaz
   xay=Rinp*xay

   return
end subroutine wake_xa2

!-----------------------------------------------------------------------
subroutine wake_dim(x,h,w,r,hwake,wwake)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              WAKE_DIM
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Calculates the vertical height and lateral half-width
!              of a building wake at a distance x from the upwind
!              face of the bldg
!
! --- INPUTS:
!                x - real        - dist (m) from upwind bldg face
!                H - real        - building height (m)
!                W - real        - building width (m)
!                R - real        - wake scaling length (m)
!
! --- OUTPUT:
!
!            hwake - real        - wake height (m)
!            wwake - real        - wake half-width (m)
!
! --- WAKE_DIM called by:  POSITION, WAKE_SIG
! --- WAKE_DIM calls    :  none
!----------------------------------------------------------------------
! --- Wake height from combination of Wilson (1979) and Weil (1996)
! --- limits for uniform approach wind

   implicit none

   double precision :: x,h,w,r,hwake,wwake
   double precision :: xbyr,xpbyr,dxbyr,xbyr3rd
   double precision :: third
   double precision, parameter :: cwkht = 1.2d0, half=0.5d0,&
   &zero=0.0d0

! --- Misc. local constants
   third = 1.0d0/3.0d0

! --- Scale distance by R
   xbyr=x/r
   xbyr3rd=xbyr**third

! --- Compute match to bdlg height at x=0
   xpbyr=-(h/(cwkht*r))**3
   dxbyr=xbyr-xpbyr

! --- Wake height
   hwake=zero
   if(xbyr>zero) hwake =cwkht*r*dxbyr**third

! --- Wake half-width from empirical fit to Snyder wind tunnel data
   wwake=zero
   if(xbyr>zero) wwake=half*w+(third*r)*xbyR3rd

   return
end subroutine wake_dim

!-----------------------------------------------------------------------
subroutine wake_sig(x,xd,xold,turbz,turby,szold,syold,&
&h,w,r,zk,yk,sz,sy,dsz)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              WAKE_SIG
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Calculates sigmas and d(sigma)/dx within the wake
!              at a distance x from the upwind face of the bldg,
!              prior to the ambient growth regime, for a "small"
!              distance increment
!
! --- INPUTS:
!                x - real        - dist (m) from upwind bldg face
!               xd - real        - dist (m) at which PDF growth ends
!             xold - real        - starting x of this step (m)
!     turbz, turby - real        - current turbulence intensities
!     szold, syold - real        - sigmas (m) at start of step
!   htwake, hwwake - real        - height and half-width (m) of wake
!           zk, yk - real        - matching constants for PDF transition
!
! --- OUTPUT:
!
!           zk, yk - real        - matching constants for PDF transition
!           sz, sy - real        - sigmas (m) at end of step
!              dsz - real        - d(sigmaz)/dx over step
!
! --- WAKE_SIG called by:  WAKE_DFSN
! --- WAKE_SIG calls    :  WAKE_DIM
!----------------------------------------------------------------------
! --- Wake height from combination of Wilson (1979) and Weil (1996)
! --- limits for uniform approach wind

   implicit none

   double precision :: x,xd,xold,turbz,turby,szold,syold,&
   &h,w,r,zk,yk,sz,sy,dsz
   double precision :: htwake,hwwake,fwwake,delx,xstepi,dsz2,dsy2,&
   &sigzd,sigyd
   double precision, parameter :: two = 2.0d0

! --- Get wake dimensions
   call wake_dim(x,h,w,r,htwake,hwwake)

! --- Use full width of the wake to scale lateral diffusivity
   fwwake=two*hwwake

   delx=x-xold
   xstepi=1.0d0/delx
   if(x<xd) then
! ---    Pure PDF Form
      dsz=turbz
      sz=szold + delx*turbz
      sy=syold + delx*turby
   elseif(xold>xd) then
! ---    Pure Wake Diffusivity Form
      dsz2=zk*turbz*htwake
      dsy2=yk*turby*fwwake
      sz=dsqrt(szold**2+delx*dsz2)
      sy=dsqrt(syold**2+delx*dsy2)
      dsz=(sz-szold)*xstepi
   else
! ---    Transition from PDF to Diffusivity Form
! ---    To end of PDF:
      delx=xd-xold
      sigzd=szold + delx*turbz
      sigyd=syold + delx*turby
      zk=two*sigzd/htwake
      yk=two*sigyd/fwwake
! ---    Beyond end of PDF:
      delx=x-xd
      dsz2=zk*turbz*htwake
      dsy2=yk*turby*fwwake
      sz=dsqrt(sigzd**2+delx*dsz2)
      sy=dsqrt(sigyd**2+delx*dsy2)
      dsz=(sz-szold)*xstepi
   endif

   return
end subroutine wake_sig

!-----------------------------------------------------------------------
subroutine wake_dbg(io,ntr,xtr,ytr,ztr,rtr,nobid,hstack)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              WAKE_DBG
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Reports salient features of PRIME results to
!              file for DEBUG purposes
!
! --- MODIFIED: For use with the AERMOD model.  Added hstack to calling
!               arguments for WAKE_XSIG.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!               io - integer     - unit for output file
!         XTR(ntr) - real        - Downwind distance (m)
!         YTR(ntr) - real        - Crosswind distance (m)
!         ZTR(ntr) - real        - Plume centerline height (m)
!         RTR(ntr) - real        - Plume radius (m)
!            NOBID - logical     - flag for BID
!           HSTACK - real        - height (m) of release
!
!     Common block /PARAMS/ variables:
!           MXNTR, MXNW
!     Common block /WAKEDAT/ variables:
!           XBADJ, Hb, Wb, xLb, Rb, xLR, xLC, HR,
!           XCAV, SZCAV, SYCAV
!
! --- OUTPUT: (written to file)
!
!          DBXB - real    - Distance (m) from upwind bldg face
!           DBX - real    - Distance (m) from source along wind
!           DBZ - real    - Plume centerline height above ground (m)
!          DBHC - real    - Cavity height above ground (m)
!          DBHW - real    - Wake height above ground (m)
!          DBSZ - real    - Sigma-z (m)
!          DBSY - real    - Sigma-y (m)
!          DBUW - real    - Wind speed factor at DBZ  (u/Ua)
!         DBRSZ - real    - Sigma-y (m) inferred from plume radius
!       IPOSITN - integer - 1: in bldg
!                           2: in cavity
!                           3: in far wake
!                           4: outside bldg influence
!         DBSZC - real    - Sigma-z (m) for cavity source
!         DBSYC - real    - Sigma-y (m) for cavity source
!
! --- WAKE_DBG called by:  PHEFF
! --- WAKE_DBG calls    :  WAKE_XSIG, WAKE_DIM, CAVITY_HT,
!                          POSITION, WAKE_U
!----------------------------------------------------------------------
!
   use PRIME_wakedat

   implicit none

   integer :: io, ntr, ipositn, it

   double precision :: xtr(ntr),ytr(ntr),ztr(ntr),rtr(ntr),hstack
   double precision :: dbx,dby,dbz,dbsz,dbsy,dbhw,dbhc,dbrsz,dbuw,&
   &rise,xb,yb,zb,dbxb,dbszc,dbsyc,dbdrdx,dbww,&
   &dbwc,dbduw,xzero

   double precision, parameter :: rt2bypi = 0.797884560802866d0

   logical :: nobid,ldb

   ldb=.false.

! --- Write section header to file
   write(io,*)
   write(io,*)'------------------------------------------------'
   write(io,*)'PRIME Module Results for Current Source and Hour'
   write(io,*)'          (all lengths in meters)'
   write(io,*)'------------------------------------------------'
   write(io,*)
   write(io,100)
   write(io,*)

! --- Report start of cavity as first point if it lies upwind of source
   if(xcav(1)<0.0d0) then
! ---    Set plume coordinates
      dbx=xcav(1)
      dby=ytr(1)
      dbz=0.0d0

! ---    Set initial values
      dbsz=0.0d0
      dbsy=0.0d0
      dbhw=0.0d0
      dbhc=0.0d0
      dbrsz=0.0d0
      dbuw=1.0d0
      ipositn=4

! ---    Compute related data
      rise=0.0d0
      xb=dbx-xbadj
      yb=dby-ybadj
      zb=dbz
      dbxb=xb

! ---    Set sigmas
      dbsz=0.0d0
      dbsy=0.0d0
      dbszc=szcav(1)
      dbsyc=sycav(1)

! ---    Set dr/dx of plume radius within wake region
      dbdrdx=0.0d0

      if(xb>=0.0d0) then
! ---       Set wake dimension along center plane from bldg
         call wake_dim(xb,Hb,Wb,Rb,dbhw,dbww)

! ---       Set cavity dimension along centerplane from bldg
         call cavity_ht(Hb,Wb,xLb,Rb,xLC,xLR,hr,xb,dbhc,dbwc)

! ---       Set speed factor
         call position(xb,yb,zb,ipositn)
         dbuw=1.0d0
         if(ipositn<4)then
            call wake_u(ldb,xb,yb,zb,dbuw,dbduw,io)
         else
            dbduw = 0.0d0
         endif
      endif

! ---    Report values
      write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,&
      &dbrsz,dbdrdx,ipositn,dbszc,dbsyc
   endif

! --- Process point of release
! --- Set plume coordinates
   dbx=0.0d0
   dby=ytr(1)
   dbz=hstack

! --- Set initial values
   dbsz=0.0d0
   dbsy=0.0d0
   dbhw=0.0d0
   dbhc=0.0d0
   dbrsz=0.0d0
   dbuw=1.0d0
   ipositn=4

! --- Compute related data
   rise=dbz-hstack
   xb=dbx-xbadj
   yb=dby-ybadj
   zb=dbz
   dbxb=xb

! --- Set sigmas just downwind of source
   xzero=0.001d0
   call wake_xsig(xzero,hstack,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

! --- Set dr/dx of plume radius within wake region
   call wake_drdx(dbx,dbdrdx)

   if(xb>=0.0d0) then
! ---    Set wake dimension along center plane from bldg
      call wake_dim(xb,Hb,Wb,Rb,dbhw,dbww)

! ---    Set cavity dimension along centerplane from bldg
      call cavity_ht(Hb,Wb,xLb,Rb,xLC,xLR,hr,xb,dbhc,dbwc)

! ---    Set speed factor
      call position(xb,yb,zb,ipositn)
      dbuw=1.0d0
      if(ipositn<4)then
         call wake_u(ldb,xb,yb,zb,dbuw,dbduw,io)
      else
         dbduw = 0.0d0
      endif
   else
      dbduw  = 0.0d0
   endif

! --- Report values
   write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,dbrsz,&
   &dbdrdx,ipositn,dbszc,dbsyc

! --- Now loop over entries in plume rise array
   do it=1,ntr

! ---    Set plume coordinates
      dbx=xtr(it)
      dby=ytr(it)
      dbz=ztr(it)
      dbrsz=rtr(it)*rt2bypi

! ---    Set initial values
      dbhw=0.0d0
      dbhc=0.0d0
      dbuw=1.0d0
      ipositn=4

! ---    Compute related data
      rise=dbz-hstack
      xb=dbx-xbadj
      yb=dby-ybadj
      zb=dbz
      dbxb=xb

! ---    Set sigmas
      call wake_xsig(dbx,hstack,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

! ---    Set dr/dx of plume radius within wake region
      call wake_drdx(dbx,dbdrdx)

      if(xb>=0.0d0) then
! ---       Set wake dimension along center plane from bldg
         call wake_dim(xb,Hb,Wb,Rb,dbhw,dbww)

! ---       Set cavity dimension along centerplane from bldg
         call cavity_ht(Hb,Wb,xLb,Rb,xLC,xLR,hr,xb,dbhc,dbwc)

! ---       Set speed factor
         call position(xb,yb,zb,ipositn)
         dbuw=1.0d0
         if(ipositn<4)then
            call wake_u(ldb,xb,yb,zb,dbuw,dbduw,io)
         else
            dbduw = 0.0d0
         endif
      else
         dbduw  = 0.0d0
      endif

! ---    Report values
      write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,&
      &dbrsz,dbdrdx,ipositn,dbszc,dbsyc

   enddo
   write(io,*)

100 format('     XB      X      Z   Hwake   Hcav    Sz     S',&
   &'y    Ufac  dUfac  R->Sz  dRdx  Pos  Szcav  Sycav')
101 format(1x,7f7.1,2f7.3,f7.1,f7.3,i4,2f7.1)

   return
end subroutine wake_dbg

!-----------------------------------------------------------------------
subroutine wake_cav0(sycapt,szcav0,sycav0)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              WAKE_CAV0
!                D. Strimaitis, L. Schulman,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Compute the sigmas for a source placed on the floor
!               of the cavity, which produce the target cavity
!               concentration
!
! --- INPUTS:
!
!        SYCAPT - real    - Sigma-y (m) of plume at point where
!                           mass is captured in cavity
!
!     Common block /WAKEDAT/ variables:
!           Hb, Wb, xLR, xLC, HR, Ub, Urh
!
! --- OUTPUT:
!
!        SZCAV0 - real    - Initial sigma-z (m) of cavity source
!        SYCAV0 - real    - Initial sigma-y (m) of cavity source
!
!                 Note    - These sigmas reproduce the cavity
!                           concentration when they are used in:
!                           C = Qc / (pi * us * szcav0 * sycav0)
!                           where us is the wind speed for the primary
!                           source, and Qc is the mass flux captured by
!                           and released from the cavity.
!
! --- WAKE_CAV0 called by:  WAKE_DFSN
! --- WAKE_CAV0 calls    :  none
!----------------------------------------------------------------------
!
   use PRIME_wakedat

   implicit none

   double precision :: sycapt,szcav0,sycav0

   double precision :: wcapt, wscale, hcav, uratio

   double precision, parameter :: rt2pi   = 2.50662827463100d0,&
   &rt2bypi = 0.797884560802866d0


! --- Interpret plume sigma-y at cavity entry point as equivalent
! --- top-hat;  limit to full width of bldg
   wcapt=min(rt2pi*sycapt,Wb)

! --- Set width scale for lateral distribution in cavity
   wscale=min(Wb,3.0d0*Hb)
   wscale=max(wscale,third*Hb)
   wscale=max(wscale,wcapt)

! --- Sigma-y for equivalent top-hat distribution
   sycav0=wscale/rt2pi

! --- Set height of cavity behind the bldg
   if(xLC < xLb) then
! ---    Reattachment
      hcav=Hb
   else
! ---    No Reattachment
      hcav=hr
   endif

! --- Set sigma-z that results in centerline concentration equal to
! --- cavity concentration
! --- Wilson & Britter 1982 approach to cavity concentration
   uratio=Ub/Urh
   szcav0=rt2bypi*uratio*hcav*third

   return
end subroutine wake_cav0

!-----------------------------------------------------------------------
subroutine cav_src(xr,yr,zr,fqcav0,qc,hc,yrc,zrc,szc,syc,n1,n2)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812               CAV_SRC
!                D. Strimaitis,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Select plume data for computing concentration at a
!               receptor due to mass contained in / released from cavity
!
! --- MODIFIED:
!               Replaced undefined variable lbd with .TRUE. in calls to
!               WAKE_XSIG to ignore BID for "outside" cavity source.
!               R.W. Brode, MACTEC (f/k/a PES), Inc. - 08/02/05
!
!               For use with the AERMOD model.  Added hc to calling
!               arguments for WAKE_XSIG.  Also modified case where only
!               primary source and "outside" cavity source contribute
!               to keep "outside" cavity source in slot 3 by setting
!               emission rate for "inside" cavity source to 0.0.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!            XR - real    - Downwind distance (m) from stack to receptor
!            YR - real    - Crosswind distance (m) from stack to receptor
!            ZR - real    - Receptor height (m) above ground
!
!     Common block /WAKEDAT/ variables:
!           Hb, Wb, xLb, Rb, xLC, xLR, HR,
!           XBADJ, YBADJ, SZCAV, SYCAV, FQCAV
!
! --- OUTPUT:
!
!        FQCAV0 - real    - Fraction of plume mass rate captured
!                           and released by cavity
!         QC(3) - real    - Normalized emission rate (q/s) for cavity
!                           sources --- QC(1)+QC(2)=1.0
!         HC(3) - real    - Height (m) of cavity sources
!        YRC(3) - real    - Crosswind distance (m) from centerline
!                           of cavity sources to receptor
!        ZRC(3) - real    - Receptor height (m) above cavity
!        SZC(3) - real    - Sigma-z (m) for cavity sources
!        SYC(3) - real    - Sigma-y (m) for cavity sources
!         N1,N2 - integer - Index range for active sources
!                           1,1: Primary source ONLY (no cavity source
!                                contributions)
!                           1,2: Primary and "outside" cavity source
!                                contribution
!                           1,3: Primary and both "outside" and "inside"
!                                cavity source contributions
!                           2,2: "outside" cavity source ONLY
!                           2,3: Both "outside" and "inside" cavity
!                                sources
!                           3,3: "inside" cavity source ONLY
!
! ------------------------------------
!     NOTE:  3 sources are considered:
!                           (1)- the primary (point) source
!                           (2)- the cavity source that dominates
!                                "outside" of the cavity
!                           (3)- the cavity source that dominates
!                                "inside" of the cavity
!            For the 2 cavity sources, array data elements are ordered:
!                           (1)- RESERVED for primary source data
!                           (2)- "outside" cavity source
!                           (3)- "inside" cavity source
!
! --- CAV_SRC called by:  PSIMPL(HOST subroutine)
! --- CAV_SRC calls    :  POSITION, CAVITY_HT, WAKE_XSIG
!----------------------------------------------------------------------
!
   use PRIME_wakedat

   implicit none

   integer :: i, n1, n2, mode, ipositn

   double precision :: xr,yr,zr,fqcav0,szcav0,sycav0,xrb,yrb,zrb,&
   &x115b,x85b,wtop,ybmax,ysb,dumz,dumy,zcav,&
   &wcav

   double precision :: qc(3),hc(3),yrc(3),zrc(3),szc(3),syc(3)

   double precision, parameter :: rt2pi   = 2.50662827463100d0,&
   &rt2bypi = 0.797884560802866d0

! --- Extract cavity sigmas from the first entry in the cavity arrays
   szcav0=szcav(1)
   sycav0=sycav(1)

! --- Pass mass fraction to calling program
   fqcav0=fqcav

! --- Set cavity source heights to zero
   hc(2)=0.0d0
   hc(3)=0.0d0

! --- Initialize cavity source mode
! --- (0: none, 1: "outside", 2: "inside", 3: both)
   mode=0

   if(fqcav<=0.0d0) then
! ---    No mass in cavity
      n1=1
      n2=1
      do i=2,3
         qc(i)=0.0d0
         yrc(i)=yr
         zrc(i)=zr
         szc(i)=szcav0
         syc(i)=sycav0
      enddo
   else
! ---    Find receptor location relative to center of upwind bldg face
      xrb=xr-xbadj
      yrb=yr-ybadj
      zrb=zr
      call position(xrb,yrb,zrb,ipositn)

! ---    Set limits of transition zone at end of cavity
      x115b=xLb+1.15d0*xLR
      x85b=xLb+0.85d0*xLR
! ---    Adjust relative contribution of cavity sources near end
! ---    of cavity region
      if(xrb>=x115b) then
! ---       Receptor well outside cavity; use only "outside" source
         qc(2)=1.0d0
         qc(3)=0.0d0
         mode=1
      elseif(xrb>x85b) then
! ---       Mix relative contribution so that they are equal at
! ---       end of cavity
         qc(2)=(xrb-x85b)/(x115b-x85b)
         qc(3)=1.0d0-qc(2)
         mode=3
      elseif(xrb>xLb) then
! ---       Receptor well within cavity; use only "inside" source
         qc(2)=0.0d0
         qc(3)=1.0d0
         mode=2
      else
! ---       Receptor upwind of trailing edge of projected bldg;
! ---       use "inside" source, but drop mass fraction linearly
! ---       to zero at windward face of projected bldg
         qc(2)=0.0d0
         qc(3)=max(0.0d0,xrb/xLb)
         mode=2
      endif

      if(ipositn==4) then
! ---       Not within wake, so drop cavity source contribution
         mode=0
         n1=1
         n2=1
         do i=2,3
            qc(i)=0.0d0
            yrc(i)=yr
            zrc(i)=zr
            szc(i)=szcav0
            syc(i)=sycav0
         enddo
      else
! ---       Set receptor offset from centerline of cavity plume
! ---       Top-hat equivalent width of cavity sigma-y
         wtop=sycav0*rt2pi
! ---       Max distance from bldg center to centerline of cavity plume
         ybmax=0.5d0*(Wb-wtop)
         if(ybmax<=0.0d0) then
! ---          Plume spread exceeds bldg width so cavity source is
! ---          centered on bldg
            yrc(2)=yrb
         else
! ---          Source location relative to center of bldg
            ysb=-ybadj
            if(ysb<0.0d0) then
               yrc(2)=yrb-max(ysb,-ybmax)
            else
               yrc(2)=yrb-min(ysb,ybmax)
            endif
         endif
         yrc(3)=yrc(2)

         if(ipositn<=2) then
! ---          Within cavity/bldg, so drop primary source contribution,
! ---          and place receptor on ground
            if(mode==3) then
               n1=2
               n2=3
            elseif(mode==2) then
               n1=3
               n2=3
            elseif(mode==1) then
               n1=2
               n2=2
            endif
            do i=n1,n2
               zrc(i)=0.0d0
               szc(i)=szcav0
               syc(i)=sycav0
            enddo
            if((mode==1 .or. mode==3) .and.&
            &xr>0.0d0) call wake_xsig(xr,hc(2),0.0d0,.true.,&
            &dumz,dumy,szc(2),syc(2))
         else
! ---          Contributions from primary & possibly both cavity plumes
            n1=1
            n2=3
! ---          Set pole height to height above cavity boundary
            if(xrb>=(xLb+xLR)) then
               zrc(2)=zr
            else
               call cavity_ht(Hb,Wb,xLb,Rb,xLC,xLR,hr,xrb,zcav,wcav)
               zrc(2)=max(0.0d0,zr-zcav)
            endif
            zrc(3)=zrc(2)
            if(mode==2) then
! ---             No contribution from "outside" cavity source, so
! ---             set emission rate for "outside" source to zero.
               qc(2)=0.0d0
               szc(2)=szcav0
               syc(2)=sycav0
               szc(3)=szcav0
               syc(3)=sycav0
               n2=3
            elseif(mode==1) then
! ---             No contribution from "inside" cavity source, so
! ---             reset n2=2
               call wake_xsig(xr,hc(2),0.0d0,.true.,dumz,dumy,szc(2),&
               &syc(2))
               n2=2
            else
! ---             Both cavity sources are used
               szc(2)=szcav0
               syc(2)=sycav0
               szc(3)=szcav0
               syc(3)=sycav0
               if(xr>=0.0d0) call wake_xsig(xr,hc(2),0.0d0,.true.,&
               &dumz,dumy,szc(2),syc(2))
            endif
         endif
      endif
   endif

! --- Final check: receptor upwind of primary source, or all mass in cav
! --- Do not allow n1=1 (primary source contribution)
   if(n1==1 .and. (xr<=0.0d0 .or. fqcav==1.0d0)) n1=2

   return
end subroutine cav_src

!-----------------------------------------------------------------------
subroutine wake_fqc(ldb,xbi,xtr,ztr,ntr,dbgunt)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812              WAKE_FQC
!                D. Strimaitis, L. Schulman,   EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE: Computes the maximum plume mass captured by cavity.
! ---          Plume centerline enters wake boundary before clearing
! ---          downwind end of cavity, so WAKE_FQC is used to find
! ---          point where mass in cavity is greatest.  Note that
! ---          distances are measured from center of upwind face of bldg
!
! --- MODIFIED: For use with the AERMOD model.  Added Hb or Zplm to calling
!               arguments for WAKE_XSIG.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!              ldb - logical     - Debug output switch
!              xbi - real        - Downwind distance (m) from upwind
!                                  face of bldg to point where plume
!                                  centerline enters wake
!         XTR(ntr) - real        - Downwind distance from source (m)
!         ZTR(ntr) - real        - Plume centerline height (m)
!              NTR - integer     - Number of entries in arrays
!
!     Common block /PARAMS/ variables:
!           MXNTR, MXNW
!     Common block /WAKEDAT/ variables:
!           Hb, Wb, xLb, Rb, xLC, xLR, HR, XBADJ, YBADJ
!
! --- OUTPUT:
!
!     Common block /WAKEDAT/ variables:
!           FQCAV
!
! --- WAKE_FQC called by:  NUMRISE
! --- WAKE_FQC calls    :  NUMGRAD, WAKE_XSIG, CAVITY_HT, FRGAUSS
!----------------------------------------------------------------------
!
   use PRIME_params
   use PRIME_wakedat

   implicit none

   integer :: ntr, nstep, is, dbgunt

   double precision :: xbi, xtr(ntr), ztr(ntr)
   double precision :: xbstart,xbend,xrange,yba,xend,xstep,x,xb,fz,&
   &fract,xb85,fq85,zplm,sz,sy,szc,syc,fractz0,&
   &zcav,ycav,fractz,fracty,xmax,fzmax

   logical :: ldb

   fqcav=0.0d0

! --- Define range of distances from upwind face of bldg at which to
! --- evaluate plume mass fraction within cavity
   xbstart=max(xbi,xLb)
   xbend=xLb+xLR
   xrange=0.99d0*(xbend-xbstart)
   yba=dabs(ybadj)
! --- Distance from source to end of cavity
   xend=xbend+xbadj

! --- Use at least 5 steps, with a maximum length of 10 m
   nstep=max(5,idint(1.0d0+xrange/10.d0))
   xstep=xrange/dble(nstep)

! --- For vertical plane, compute mass fraction below Hb at the
! --- downwind end of the cavity.  This allows the influence of plume
! --- rise to continue lifting plume mass out of the influence of
! --- of the cavity structure for strongly buoyant releases.
! --- Use this value as a cap to fractz.
   call numgrad(xend,xtr,ztr,ntr,zplm)
   call wake_xsig(xend,Hb,0.0d0,.true.,sz,sy,szc,syc)
   call frgauss(zplm,sz,Hb,-Hb,fractz0)

   do is=0,nstep
      xb=xbstart+dble(is)*xstep
      x=xb+xbadj
      call numgrad(x,xtr,ztr,ntr,zplm)
      call cavity_ht(Hb,Wb,xLb,Rb,xLC,xLR,hr,xb,zcav,ycav)
      call wake_xsig(x,zplm,0.0d0,.true.,sz,sy,szc,syc)
      call frgauss(zplm,sz,zcav,-zcav,fractz)
      call frgauss(yba,sy,ycav,-ycav,fracty)
      fz=min(fractz,fractz0)
      fract=fz*fracty
      if(fract>fqcav) then
         fqcav=fract
         xmax=x
         fzmax=fz
      endif
   enddo

! --- Additional constraint:  account for fluctuations in cavity
! --- boundary on capturing low-level plumes by imposing a maximum
! --- capture fraction that linearly drops from 1.0 at 85% of the
! --- cavity length, to 0% at the end of the cavity.
   xb85=xLb+0.85d0*xLR
   if(xbstart>xb85) then
      fq85=max( 0.0d0, 1.0d0-(xbstart-xb85)/(xbend-xb85) )
      fqcav=min(fqcav,fq85)
   endif

   if(ldb) then
      write(dbgunt,*)
      write(dbgunt,*)'WAKE_FQC:'
      write(dbgunt,'(a,2x,3(f12.5,2x))') 'xbi, xbstart, xbend   = ',&
      &xbi, xbstart, xbend
      write(dbgunt,'(a,2x,f12.5,I14,2x,f12.5)')&
      &'xstep, nstep, fractz0 = ', xstep, nstep, fractz0
      write(dbgunt,'(a,2x,2(f12.5,2x))') 'xb85,  xmax           = ',&
      &xb85,  xmax
      write(dbgunt,'(a,2x,2(f12.5,2x))') 'fqcav, fzmax          = ',&
      &fqcav, fzmax
      write(dbgunt,*)
   endif

   return
end subroutine wake_fqc

!----------------------------------------------------------------------
subroutine frgauss(hcntr,sigma,h1,h2,fract)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812               FRGAUSS
!                J. Scire, D. Strimaitis,  EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Compute the fraction of a Gaussian distribution between
!               two limits
!
! --- INPUTS:
!            HCNTR - real    - Center of Gaussian distribution (m)
!            SIGMA - real    - Standard deviation (m) of the
!                              distribution
!           H1, H2 - real    - Limits between which the distribution
!                              is to be integrated
!
! --- OUTPUT:
!            FRACT - real    - Fraction of the Gaussian distribution
!                              between H1 and H2
!
! --- FRGAUSS called by: WAKE_FQC
! --- FRGAUSS calls:     ERFDIF
!----------------------------------------------------------------------
!
   implicit none

   double precision :: hcntr,sigma,h1,h2,fract,s,z1,z2,erfdif
   double precision, parameter :: sqrt2=1.41421356237310d0,&
   &small=1.0d-5
!
! --- Prevent numerical problems with very small sigmas
   s=sqrt2*max(sigma,small)
!
   z1=(h1-hcntr)/s
   z2=(h2-hcntr)/s
!
   fract=0.5d0*dabs(erfdif(z1,z2))
!
   return
end subroutine frgauss

!----------------------------------------------------------------------
double precision function erfdif(x1,x2)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                ERFDIF
!
!     Taken from:
! --- CALPUFF    Version: 4.0       Level: 900228                ERFDIF
!                R. Yamartino, SRC
!
! --- PURPOSE:  Computes the difference: erfdif = erf(x1) - erf(x2).
!               Various methods are used to avoid roundoff errors
!               depending on the values of the two arguments.
!
! --- INPUTS:
!
!                X1 - real    - Argument 1 (no units)
!                X2 - real    - Argument 2 (no units)
!
! --- OUTPUTS:
!
!            ERFDIF - real    - erf(x1) - erf(x2)
!
! --- ERFDIF called by:  FRGAUSS
! --- ERFDIF calls:      ERF,ERFC
!----------------------------------------------------------------------
! *** V3.21
!
   implicit none

   integer :: isign
   double precision :: fnerf, fnerfc
   double precision :: x1,x2,xtest,xx1,xx2,erfcx1,erfcx2

   erfdif=0.0d0
   if(x1==x2) go to 40
   if((x1*x2) <= 0.0d0) go to 50
   xtest=dabs(x2)
   if(dabs(x1)<xtest) xtest=dabs(x1)
! --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
!     IF(XTEST.GE.13.306D0) GO TO 40
   if(xtest >= 9.15d0) go to 40
   if(xtest < 0.47d0) go to 50
!     CAN ONLY REACH HERE WHEN X1 AND X2 HAVE SAME SIGN.
   isign=1
   xx1=x1
   xx2=x2
   if(x1>0.0d0) go to 30
   isign=-1
   xx1=-xx1
   xx2=-xx2
!  30 ERFDIF=ISIGN*(FNERFC(XX2)-FNERFC(XX1))
30 erfcx1=0.0d0
   erfcx2=0.0d0
! --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
!     IF(XX1.LT.13.306D0) ERFCX1=FNERFC(XX1)
!     IF(XX2.LT.13.306D0) ERFCX2=FNERFC(XX2)
   if(xx1 < 9.15d0) erfcx1=fnerfc(xx1)
   if(xx2 < 9.15d0) erfcx2=fnerfc(xx2)
   erfdif=dble(isign)*(erfcx2-erfcx1)
! --- Protect against flakey LAHEY compiler 4/9/89
   if(erfcx2==erfcx1) erfdif=0.0d0
40 return
50 erfdif=fnerf(x1)-fnerf(x2)
   return
end function erfdif

!-----------------------------------------------------------------------
double precision function fnerf(xx)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                   ERF
!
!     Taken from:
! --- CALPUFF    Version: 4.0       Level: 941228                   ERF
!                R. Yamartino, SRC
!
! --- PURPOSE:  Computes the error function, erf(x).
! ---           This is the Quick medium accuracy ERROR FUNCTION from
! ---           NBS 55.  Using an approximation due to Hastings;
! ---           absolute error about 3e-7
!
!
! --- INPUTS:
!
!                XX - real    - Argument  (no units)
!
! --- OUTPUTS:
!
!               ERF - real    - error function of x
!
! --- ERF called by:  ERFDIF
! --- ERF calls:   no routines
!----------------------------------------------------------------------
!
   implicit none

   double precision :: x, xx ,t, t16, a(6), xcut

   data a/0.0000430638d0, 0.0002765672d0, 0.0001520143d0,&
   &0.0092705272d0, 0.0422820123d0, 0.0705230784d0/
   data xcut/ 3.919206d0/
!
   x = dabs(xx)
   if(x > xcut) then
      t16 = 0.0d0
   else
!
      t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *&
      &x ) + a(5) ) * x ) + a(6) ) * x
!
      t = 1.0d0 / (t + 1.0d0)
!
      t16 = t * t * t * t
      t16 = t16 * t16 * t16 * t16
   endif
!
   if(xx > 0.0d0) then
      fnerf =  1.0d0 - t16
   else
      fnerf =  t16 - 1.0d0
   endif
!
   return
end function fnerf

!-----------------------------------------------------------------------
double precision function fnerfc(xx)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812                  ERFC
!
!     Taken from:
! --- CALPUFF    Version: 4.0       Level: 941228                  ERFC
!                R. Yamartino, SRC
!
! --- PURPOSE:  Computes the complementary error function, 1-erf(x).
! ---           This is the Quick medium accuracy COMP. ERROR FUNCTION
! ---           from NBS 55.  Using an approximation due to Hastings;
! ---           absolute error about 3e-7.  Asymptotic expression added
! ---           for large xx to reduce percent error.
!
!
! --- INPUTS:
!
!                XX - real    - Argument  (no units)
!
! --- OUTPUTS:
!
!              ERFC - real    - complementary error function of x
!
! --- ERFC called by:  ERFDIF
! --- ERFC calls:   no routines
!-----------------------------------------------------------------------
!
   implicit none

   double precision :: x, xx ,t, t16, a(6)
   double precision :: xcutl, xcuth, rtpii, z

   data a/0.0000430638d0, 0.0002765672d0, 0.0001520143d0,&
   &0.0092705272d0, 0.0422820123d0, 0.0705230784d0/
   data xcutl/-3.919206d0/
   data xcuth/13.306d0   /
   data rtpii/0.564189583547756d0/
!
   if(xx > xcuth) then
      fnerfc = 0.0d0
!
   elseif(xx < xcutl) then
      fnerfc = 2.0d0
!
   elseif(xx > 2.79d0) then
      x = dabs(xx)
      z = 1.0d0 / x
      fnerfc = rtpii*z*dexp(-x*x)*(1.0d0-0.5d0*z*z*(1.0d0-1.5d0*z*z))
!
   else
      x = dabs(xx)
      t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *&
      &x ) + a(5) ) * x ) + a(6) ) * x
!
      t = 1.0d0 / (t + 1.0d0)
!
!        fnerfc = t**16   for x > 0
      t16 = t * t * t * t
      t16 = t16 * t16 * t16 * t16
!
      if(xx > 0.0d0) then
         fnerfc =  t16
      else
         fnerfc =  2.0d0 - t16
      endif
!
   endif
!
   return
end function fnerfc

!----------------------------------------------------------------------
subroutine wake_xsig(x,hstk,rise,nobid,sz,sy,szc,syc)
!----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812             WAKE_XSIG
!                D. Strimaitis,  EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Extract sigmas in the wake by interpolating among the
!               stored values; compute sigmas outside tabulated range
!               using HOST sigma curves with BID or virtual source
!               adjustments
!
! --- MODIFIED: For use with the AERMOD model.  Replaced calls to SIGY
!               and SIGZ with calls to SIGYPR and SIGZPR.  Added virtual
!               source sigmas to be added to HOST sigmas in quadrature.
!               Also added physical stack height to calling arguments.
!               R.W. Brode, PES, Inc. - 07/05/01
!
! --- INPUTS:
!                X - real       - Downwind distance (m) from source
!             HSTK - real       - Physical stack height (m)
!             RISE - real       - Gradual plume rise (m)
!            NOBID - logical    - Directs use of buoyancy enhancement
!
!     Common block /PARAMS/ variables:
!           MXNTR
!     Common block /WAKEDAT/ variables:
!           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
!           VSIGZ, VSIGY,
!           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
!           VSIGZC, VSIGYC
!
! --- OUTPUT:
!               SZ - real       - Sigma-z (m) at downwind distance X
!                                 due to primary source
!               SY - real       - Sigma-y (m) at downwind distance X
!                                 due to primary source
!              SZC - real       - Sigma-z (m) of cavity source at
!                                 downwind distance X from primary source
!              SYC - real       - Sigma-y (m) of cavity source at
!                                 downwind distance X from primary source
!
! --- WAKE_XSIG called by:  PDIS
! --- WAKE_XSIG calls:      SIGZ, SIGY
!----------------------------------------------------------------------
!
   use PRIME_wakedat

   implicit none

   integer :: i, nwkm1, ip1, ncvm1

   double precision :: x,hstk,rise,sz,sy,szc,syc
   double precision :: bidsq,fac
!
   logical :: nobid

! --- Primary source:
! -------------------
   if(x<=0.0d0) then
! ---    Report null values (these should never get used!)
      sz=0.0d0
      sy=0.0d0
   elseif(nwak<=1) then
! ---    Plume never altered by wake turbulence; use HOST curves
      call sigzpr(x,hstk+rise,sz)
      call sigypr(x,hstk+rise,sy)
      if(.not.nobid) then
         bidsq=(rise/3.5d0)**2
         sz=dsqrt(sz**2+bidsq)
         sy=dsqrt(sy**2+bidsq)
      endif
   elseif(x<xwak(1)) then
! ---    Point lies upwind of wake region; use HOST curves
      call sigzpr(x,hstk+rise,sz)
      call sigypr(x,hstk+rise,sy)
      if(.not.nobid) then
         bidsq=(rise/3.5d0)**2
         sz=dsqrt(sz**2+bidsq)
         sy=dsqrt(sy**2+bidsq)
      endif
   elseif(x>xwak(nwak)) then
! ---    Point lies downwind of transition to ambient growth; use
! ---    HOST curves with virtual source term
      call sigzpr(x,hstk+rise,sz)
      call sigypr(x,hstk+rise,sy)
      sz = dsqrt(sz*sz + vsigz*vsigz )
      sy = dsqrt(sy*sy + vsigy*vsigy )
   else
! ---    Point lies within range of tabulated values
      nwkm1=nwak-1
      sz=szwak(1)
      sy=sywak(1)
      do i=nwkm1,1,-1
         if(x>=xwak(i))then
            ip1=i+1
            fac=(xwak(ip1)-x)/(xwak(ip1)-xwak(i))
            sz=szwak(ip1)-(szwak(ip1)-szwak(i))*fac
            sy=sywak(ip1)-(sywak(ip1)-sywak(i))*fac
            goto 50
         endif
      enddo
   endif

! --- Cavity source:
! -------------------
50 if(ncav<=1) then
! ---    No contribution from cavity source (report initial values)
      szc=szcav(1)
      syc=sycav(1)
   elseif(x<xcav(1)) then
! ---    Point lies upwind of cavity region (report initial values)
      szc=szcav(1)
      syc=sycav(1)
   elseif(x>xcav(ncav)) then
! ---    Point lies downwind of transition to ambient growth; use
! ---    HOST curves with virtual source term
      call sigzpr(x,0.0d0,szc)
      call sigypr(x,0.0d0,syc)
      szc = dsqrt(szc*szc + vsigzc*vsigzc)
      syc = dsqrt(syc*syc + vsigyc*vsigyc)
   else
! ---    Point lies within range of tabulated values
      ncvm1=ncav-1
      szc=szcav(1)
      syc=sycav(1)
      do i=ncvm1,1,-1
         if(x>=xcav(i))then
            ip1=i+1
            fac=(xcav(ip1)-x)/(xcav(ip1)-xcav(i))
            szc=szcav(ip1)-(szcav(ip1)-szcav(i))*fac
            syc=sycav(ip1)-(sycav(ip1)-sycav(i))*fac
            return
         endif
      enddo
   endif

   return
end subroutine wake_xsig

!-----------------------------------------------------------------------
subroutine cavity_ht(h,w,l,r,lc,lr,hr,x,zcav,ycav)
!-----------------------------------------------------------------------
!
! --- PRIME      Version:  1.0     Level:  970812             CAVITY_HT
!                L. Schulman, EARTH TECH
!                Prepared for EPRI under contract WO3527-01
!
! --- PURPOSE:  Calculates height of cavity envelope as function of x
!
! --- INPUTS:
!                H - real              - Building height above ground
!                W - real              - Projected building width
!                L - real              - Along-wind building length
!                                        building face
!                R - real              - Scale length from H and W
!               LC - real              - Length of roof cavity
!               LR - real              - Length of downwind cavity from
!                                         lee face
!               HR - real              - Maximum cavity height above
!                                         ground
!                x - real              - downwind distances
!
! --- OUTPUT:
!
!          zcav    - real              - cavity height as function of x
!
!          ycav    - real              - cavity half-width as f(x)

! --- CAVITY_HT called by:  PRIME
! --- CAVITY_HT calls:      none
!----------------------------------------------------------------------
!
!
!
   implicit none

   double precision :: h,w,l,r,hr,lr,lc,x,zcav,ycav

! --- Initialize
   zcav=0.0d0
   ycav=0.0d0

! --- Cavity is not present upwind of bldg or at/beyond L+LR
   if(x>=(l+lr)) then
      return
   elseif(x<0.0d0) then
      return
   endif
!
!     calculate x-y near wake boundary
!
   if(x>=0.0d0 .and. x<=r) then
      ycav=(w/2.d0+r/3.d0)-(x-r)**2/(3.d0*r)
   elseif(x>r .and. x<=(l+lr)) then
      ycav=(w/2.d0+r/3.d0)*dsqrt(1.d0-((x-r)/(l+lr-r))**2)
   endif

!     calculate x-z near wake boundary
!
   if(lc < l)then       ! reattachment
!
      if(x>=0.d0 .and. x<=l) then
         zcav=h
      elseif(x>=l .and. x<=(l+lr)) then
         zcav=h*dsqrt(1.d0-((x-l)/lr)**2)
      endif
!
   else                    ! nonreattachment
      if(x>=0.d0 .and. x<=0.5d0*r) then
         zcav=hr+4.d0*(x-0.5d0*r)**2*(h-hr)/(r**2)
      elseif(x>0.5d0*r .and. x<=(l+lr)) then
         zcav=hr*dsqrt(1.d0-((x-0.5d0*r)/(l+lr-0.5d0*r))**2)
      endif
   endif
!
   return
end subroutine cavity_ht

subroutine sigypr(xarg,zarg,syout)
!***********************************************************************
!                 SIGYPR Module of AERMOD Model
!
!        PURPOSE: Calculates Ambient Sigma-y Values From Dispersion Curves
!                 for use with the PRIME calculation
!
!        PROGRAMMER: Roger Brode
!
!        DATE:       July 5, 2001
!
!        INPUTS:  Downwind Distance
!                 Stability Class
!                 Rural or Urban Dispersion Option
!
!        OUTPUTS: Lateral Dispersion Coefficient, SYOUT
!
!        CALLED FROM:   NUMRISE, WAKE_DFSN, WAKE_XSIG
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character (len=8) :: modnam
   double precision, parameter :: expon = 1.0d0
   double precision :: xarg, svovru, tyeff,&
   &xnodim, denomi, betalph,&
   &zarg, syout

!     Variable Initializations
   modnam = 'SIGYPR'

   if( stable )then
!        The atmosphere is stable or the release is above the CBL mixing ht.
!        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
      svovru = max (svumin, sveff/ueff)
      tyeff  = (zimech/(156.0d0*sveff)) * (max(zarg, 0.46d0)/0.46d0)
      syamb  = (svovru * xarg)/(1.0d0+xarg/(2.0d0*ueff*tyeff))**0.3d0

   elseif( unstab )then
!        The atmosphere is unstable and the release is below the CBL mixing ht.
!        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
      svovru = max (svumin, sveff/ueff)
      xnodim = svovru * xarg / zi
      denomi = max (zarg, 0.46d0)
! ---    BETALPH= MAX( 78.0D0*(0.46D0/DENOMI)**EXPON , 0.7D0)
! ---             recode without EXPON, since EXPON=1
      betalph= max( 78.0d0*(0.46d0/denomi), 0.7d0)
      syamb  = (svovru * xarg) / (1.0d0+betalph*xnodim)**0.3d0

   endif

   syout = syamb

   return
end subroutine sigypr

subroutine sigzpr(xarg,zarg,szout)
!***********************************************************************
!                 SIGZPR Module of AERMOD Model
!
!        PURPOSE: Calculates Ambient Sigma-z Values From Dispersion Curves
!                 for use with the PRIME calculation
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    July 5, 2001
!
!        INPUTS:  Downwind Distance
!                 Stability Class
!                 Rural or Urban Dispersion Option
!
!        OUTPUTS: Vertical Dispersion CoEFFicient, SZOUT
!
!        CALLED FROM:   NUMRISE, WAKE_DFSN, WAKE_XSIG
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character (len=8) :: modnam
   integer :: ndxzhe
   double precision :: xarg, ttravl, ptp, bvfrq, ztmp, sigf, alphab,&
   &zarg, szad, szout

!     Variable Initializations
   modnam = 'SIGZPR'

!     TTRAVL  = Travel time

   if( stable )then
!        The atmosphere is stable or the release was above the CBL mixing ht.
!        See Eq. 1 of the document by Venkatram referenced above.

      ttravl = xarg / ueff

!----    Apply Sigma-Z formulation from CTDMPLUS
!----    Locate index below HE, and retrieve potential temperature at HE

      call locate(gridht, 1, mxglvl, zarg, ndxzhe)

      if (ndxzhe >= 1) then
         call gintrp( gridht(ndxzhe), gridpt(ndxzhe),&
         &gridht(ndxzhe+1), gridpt(ndxzhe+1), zarg, ptp )
      else
         ptp  = gridpt(1)
      end if

      bvfrq = dsqrt( g * tgeff / ptp )
      if(bvfrq < 1.0d-10) bvfrq = 1.0d-10

!        Set height for calculating sigma-z, ZTMP
      ztmp = max( zarg, 1.0d-4 )

      if (urbstab) then
!           Set BVF term to zero for urban stable boundary layer
         szamb = sweff * ttravl / dsqrt( 1.0d0 + sweff*ttravl *&
         &( 1.0d0/(0.72d0*ztmp) ) )

      else
         szamb = sweff * ttravl / dsqrt( 1.0d0 + sweff*ttravl *&
         &( 1.0d0/(0.72d0*ztmp) + bvfrq/(0.54d0*sweff) ) )
      end if

      if (zarg < zi) then
         call szsfclpr (xarg,zarg)

         sigf = min ( zarg/zi, 1.0d0)
         szas = (1.0d0 - sigf) * szsurf + sigf * szamb
      else
         szas = szamb
      end if

      szout = szas


   elseif( unstab )then

      if (zarg >= 0.1d0*zi) then
         alphab = 1.0d0
      else
         alphab = 0.6d0 + 0.4d0*(zarg/(0.1d0*zi))
      end if

      szad = alphab * sweff * xarg / ueff

      call szsfclpr (xarg,zarg)

      szout = dsqrt( szad*szad + szsurf*szsurf )

   endif

   return
end subroutine sigzpr

subroutine szsfclpr (xarg, zarg)
!***********************************************************************
!             SZSFCLPR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the surface layer dispersion term for sigma_Z,
!                 for use with the PRIME calculation
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
!        CALLED FROM:   SIGZPR
!
!        References:  "A Dispersion Model for the Convective Boundary
!                      Layer", J. Weil, 8/27/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   double precision :: xarg, zarg

!     Variable Initializations
   modnam = 'SZSFCLPR'

! NOTE --->  BSUBC = 0.5 is set in a PARAMETER stmt in MODULE MAIN1

!---- Calculate the surface layer contribution.

   if (unstab .and. zarg < 0.1d0*zi) then
      szsurf = bsubc * ( 1.0d0 - 10.d0 * (zarg / zi)) *&
      &(ustar / ueff)*(ustar / ueff)  *&
      &(xarg * xarg / dabs( obulen ))

   elseif (stable) then
      szsurf = (rtof2/rtofpi) * ustar * (xarg/ueff) *&
      &(1.0d0 + 0.7d0*xarg/obulen)**(-third)

   else
      szsurf = 0.0d0

   endif

   return
end subroutine szsfclpr
