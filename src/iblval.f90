subroutine iblval (xarg)
!=======================================================================
!             IBLVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:  Calculating effective parameters for the inhomogeneous
!             boundary layer (IBL).
!
!   Input:    Downwind distance, XARG (m)
!
!   Output:   Effective parameters for wind speed, turbulence and
!             lapse rate
!
!   Called by:  PCALC, VCALC, ACALC, PLUMEF, PWIDTH
!
!   Assumptions:
!
!   Developer(s): Roger Brode, PES, Inc.
!   Date:         January 17, 1995
!
!   Revision history:
!
!                 Modified to include LINE source type in the call to
!                 subroutine ADISZ.
!                 R.W. Brode, EPA/AQMG, 03/19/2014

!RWB              Modified to use ZRT (height of receptor above stack
!                 base) instead of ZFLAG (height of receptor above
!                 ground) in defining the layer for the effective
!                 parameters.
!                 R.W. Brode, PES, 8/5/98
!
!RWB              Added calculation of effective Dtheta/Dz (TGEFF and
!                 TGEFF3) for use in calculating stable sigma-z.
!                 R.W. Brode, PES, 8/5/98
!
!RWB              Modified to let plume centroid height follow plume
!                 centerline height above ZI/2.  Also limit upper bound
!                 of averaging layer for direct plume to be .LE. ZI.
!                 This is needed to address cases where the
!                 plume height may exceed ZI.  For the SBL, the effective
!                 parameters are calculated at the plume centerline height.
!                 R.W. Brode, PES, 1/26/95
!
!   Reference(s): "Options for the Treatment of Inhomogeneity",
!                 Al Cimorelli, Revision 5, 12/13/94
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12
   integer :: ndxeff, ndxbhi, ndxblo, ndxalo
!     JAT 7/22/21 D065 SZOLD, SZ3OLD NOT USED
!      DOUBLE PRECISION :: XARG, SZNEW, ZHI, ZLO, SZOLD, SZ3NEW, SZ3OLD,
   double precision :: xarg, sznew, zhi, zlo, sz3new,&
!     JAT 7/22/21 DO65 SZDOLD NOT USED
!     &                    SZDAVG, SZDNEW, SZDOLD
   &szdavg, szdnew

!
!---- Data dictionary
!
!---- Data initializations
   modnam = 'IBLVAL'
!
!     *************************************************************
!
!  ****** FOR HIGHLY BUOYANT PLUME ****** added code JAN 2023--kja
!  **  PPFN should be 1 when mixing height > top of penetrated plume
   if (hbplume) then
      ppfn = 1.0d0
   endif
!  *********************************added code end --kja
!RWB  Initialize the effective parameters based on
!RWB  values at plume height
   if( stable  .or.  (unstab .and. (hs >= zi) ) )then
      hteff = he
      call locate(gridht, 1, mxglvl, hteff, ndxeff)
      call gintrp( gridht(ndxeff), gridws(ndxeff),&
      &gridht(ndxeff+1), gridws(ndxeff+1), hteff, ueff)
      call gintrp( gridht(ndxeff), gridsv(ndxeff),&
      &gridht(ndxeff+1), gridsv(ndxeff+1), hteff,sveff)
      call gintrp( gridht(ndxeff), gridsw(ndxeff),&
      &gridht(ndxeff+1), gridsw(ndxeff+1), hteff,sweff)
      call gintrp( gridht(ndxeff), gridtg(ndxeff),&
      &gridht(ndxeff+1), gridtg(ndxeff+1), hteff,tgeff)
      if (pvmrm .or. grsm) then
         call gintrp( gridht(ndxeff), grideps(ndxeff),&
         &gridht(ndxeff+1), grideps(ndxeff+1),hteff,epseff)
      end if

!RWB     Modify treatment of low wind/low turbulence cases.
!RWB     R. Brode, PES, 8/15/96
      sweff = max( sweff, swmin )
      sveff = max( sveff, svmin, svumin*ueff )
      if( l_vectorws )then
         ueff  = dsqrt( ueff*ueff + 2.0d0*sveff*sveff )
      endif
      ueff  = max( ueff, wsmin )

!RJP     Add temporary debugging statement here.

      if(debug) then
         write(dbgunt, 6014) ueff, sveff, sweff
6014     format(5x,'Initial effective parameters ',&
         &'for the stable ',&
         &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
         &'SVeff = ',f7.2,&
         &' m/s; SWeff = ',f7.2,' m/s.',/)
      endif

   else if (unstab .and. (hs<zi)) then

!        Direct and Indirect Source

      if (ppf < 1.0d0) then

!RWB        Initialize effective parameters based on vlues at the
!RWB        plume centroid height (CENTER)
         hteff = center
         call locate(gridht, 1, mxglvl, hteff, ndxeff)
         call gintrp( gridht(ndxeff), gridws(ndxeff),&
         &gridht(ndxeff+1), gridws(ndxeff+1),hteff, ueffd)
         call gintrp( gridht(ndxeff), gridsv(ndxeff),&
         &gridht(ndxeff+1), gridsv(ndxeff+1),hteff,sveffd)
         call gintrp( gridht(ndxeff), gridsw(ndxeff),&
         &gridht(ndxeff+1), gridsw(ndxeff+1),hteff,sweffd)
         if (pvmrm .or. grsm) then
            call gintrp( gridht(ndxeff), grideps(ndxeff),&
            &gridht(ndxeff+1), grideps(ndxeff+1),hteff,epseffd)
         end if

!RWB        Modify treatment of low wind/low turbulence cases.
!RWB        R. Brode, PES, 8/15/96
         sweffd = max( sweffd, swmin )
         sveffd = max( sveffd, svmin, svumin*ueffd )
         if( l_vectorws )then
            ueffd = dsqrt( ueffd*ueffd + 2.0d0*sveffd*sveffd )
         endif
         ueffd = max( ueffd, wsmin )

!RJP        Add temporary debugging statement here.

         if(debug) then
            write(dbgunt, 6015) ueffd, sveffd, sweffd
6015        format(5x,'Initial effective parameters ',&
            &'for the direct convective ',&
            &'plume:',//,5x,'UeffD = ',f7.2,' m/s; ',&
            &'SVeffD = ',f7.2,&
            &' m/s; SWeffD = ',f7.2,' m/s.',/)
         endif

      end if
!RJP
!RJP     Penetrated source
!RJP
      if (ppf > 0.0d0) then
!  ****** FOR HIGHLY BUOYANT PLUME ****** added code JAN 2023--kja
! ** determine next hour mix height ZIN from mechanical and convective heights
         if (hbplume) then
            if(ziconvn > 0.0d0 .and. zimechn > 0.0d00) then
               zin = max(ziconvn,zimechn)
            elseif( ziconvn < 0.0d0 .and. zimechn > 0.0d0) then
               zin = zimechn
            elseif( ziconvn > 0.0d0 .and. zimechn < 0.0d0) then
               zin = ziconvn
            else
               zin = zi
            end if
! ** Calculate average height between hours
            ziavg = (zi+zin)/2.0d0
            if(debug) then
               write(dbgunt,6019) ziconvn, zimechn, zin,ziavg, he3
6019           format(1x,'CONVN= ',f10.2,' MECHN= ',f10.2,' ZIN= ',f10.2,&
               &'ZIAVG= ',f10.2,'HE3= ',f10.2)
            endif
         endif
!  ******************************* added code end --kja
         hteff = he3
         call locate(gridht, 1, mxglvl, hteff, ndxeff)
         call gintrp( gridht(ndxeff), gridws(ndxeff),&
         &gridht(ndxeff+1), gridws(ndxeff+1), hteff, ueff3)
         call gintrp( gridht(ndxeff), gridsv(ndxeff),&
         &gridht(ndxeff+1), gridsv(ndxeff+1), hteff,sveff3)
         call gintrp( gridht(ndxeff), gridsw(ndxeff),&
         &gridht(ndxeff+1), gridsw(ndxeff+1), hteff,sweff3)
         call gintrp( gridht(ndxeff), gridtg(ndxeff),&
         &gridht(ndxeff+1), gridtg(ndxeff+1), hteff,tgeff3)
         if (pvmrm .or. grsm) then
            call gintrp( gridht(ndxeff), grideps(ndxeff),&
            &gridht(ndxeff+1), grideps(ndxeff+1),hteff,epseff3)
         end if

!RWB        Modify treatment of low wind/low turbulence cases.
!RWB        R. Brode, PES, 8/15/96
         sweff3 = max( sweff3, swmin )
         sveff3 = max( sveff3, svmin, svumin*ueff3 )
         if( l_vectorws )then
            ueff3 = dsqrt( ueff3*ueff3 + 2.0d0*sveff3*sveff3 )
         endif
         ueff3  = max( ueff3, wsmin )

!RJP        Add temporary debugging statement here.

         if(debug) then
            write(dbgunt, 6016) ppf, ueff3,&
            &sveff3, sweff3
6016        format(5x,'Penetration fraction = ',f6.3,/,&
            &5x,'Initial effective parameters ',&
            &'for the penetrated ',&
            &'plume:',//,5x,'Ueff3 = ',f7.2,' m/s; ',&
            &'SVeff3 = ',f7.2,&
            &' m/s; SWeff3 = ',f7.2,' m/s.',/)
         end if
      end if

   end if

!     End initialization.  Next compute averages across plume layer.

   if (srctyp(isrc)(1:5) == 'POINT') then
!        Determine Dispersion Parameters              ---   CALL PDIS
      call pdis ( xarg )
   else if (srctyp(isrc) == 'VOLUME') then
!        Determine Dispersion Parameters              ---   CALL VDIS
      call vdis ( xarg )
   else if (srctyp(isrc) == 'AREA' .or.&
   &srctyp(isrc) == 'AREAPOLY' .or.&
   &srctyp(isrc) == 'AREACIRC' .or.&
   &srctyp(isrc) == 'LINE' .or.&
   &srctyp(isrc) == 'OPENPIT') then
!        Determine Vertical Dispersion Parameters     ---   CALL ADISZ
      call adisz ( xarg )
   end if

   if( stable  .or.  (unstab .and. (hs >= zi) ) )then

      sznew  = sz
      center = he
      if (center <= 5.0d0 .and. zrt <= 5.0d0) then
         zhi = 5.0d0
         zhi = min( zhi, zi )
         zlo = 0.0d0
      else if (center > zrt) then
         zhi = center
         zlo = max(center - szcoef*sznew, zrt)
      else
         zhi = min(center + szcoef*sznew, zrt)
         zlo = center
      end if

      if(debug) then
         if( evonly )then
            write(dbgunt, 6030) ievent, center, sznew, zrt,zlo,zhi
6030        format(5x,'Stable plume calculation',&
            &' for EVENT # ',i3,//,&
            &5x,'Height of plume center of mass = ',f6.1,&
            &' m; Sigma-z estimate = ',f11.1,' m; ',&
            &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
            &'effective parameters are averaged between ',&
            &f6.1,' and ',f6.1,' meters.',/)
         else
            write(dbgunt, 6031) irec, center, sznew, zrt,zlo,zhi
6031        format(5x,'Stable plume calculation',&
            &' for receptor # ',i3,//,&
            &5x,'Height of plume center of mass = ',f6.1,&
            &' m; Sigma-z estimate = ',f11.1,' m; ',&
            &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
            &'effective parameters are averaged between ',&
            &f6.1,' and ',f6.1,' meters.',/)
         endif
      end if

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
      &zhi,ndxbhi,tgeff )
      if (pvmrm .or. grsm) then
         call anyavg ( mxglvl, gridht, grideps, zlo,ndxalo,&
         &zhi,ndxbhi,epseff )
      end if
!     JAT 7/22/21 D065 SZOLD NOT USED
!         SZOLD = SZ

!RWB     Modify treatment of low wind/low turbulence cases.
!RWB     R. Brode, PES, 8/15/96
      sweff = max( sweff, swmin )
      sveff = max( sveff, svmin, svumin*ueff )
      if( l_vectorws )then
         ueff = dsqrt( ueff*ueff + 2.0d0*sveff*sveff )
      endif
      ueff = max( ueff, wsmin )

!RJP     Add temporary debugging statement here.

      if(debug) then
         write(dbgunt, 6032) ueff, sveff, sweff
6032     format(5x,'Effective parameters for stable ',&
         &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
         &'SVeff = ',f7.2,&
         &' m/s; SWeff = ',f7.2,' m/s.',/)
      end if

   else if (unstab .and. (hs<zi)) then
!RJP
!RJP  Process effective values for direct and penetrated plumes
!RJP
!RJP  First, process the penetrated plume, then the direct plumes.
!RJP

      if( ppf > 0.0d0 )then
!  ********* FOR HIGHLY BUOYANT PLUME ********* added code JAN 2023--kja
!  ** how much of penetrated plume still above ZIAVG
!  ** assuming gaussian entrainment factor
         if (hbplume) then
            hhtop = he3 + 2.15d0*sz3  ! top of plume
            hhbot = max(he3 - 2.15d0*sz3,zrt)  ! Bottom of plume
! ** width of plume to 2.15 sigma-z - where conc. falls to 10% of centerline
            ppwid = hhtop - hhbot
! ** difference between top of plume and ZIAVG mixing height
            htopdif = hhtop - ziavg
            if (htopdif > 0.0d0) then  ! top of plume > mixing ht
! ** PPFN should be between 0 - 1
               if(htopdif < ppwid) then ! mixing ht within plume
                  if(ziavg <= he3) then
! ** PPFN from 0 to 0.5 - amount of penetrated plume entrained
! ** lower half of plume
                     ppfn = 0.5d0*erf((ziavg-hhbot)/sz3/dsqrt(2.0d0))
                  else
! ** PPFN from 0.5 to 1.0 - amount of penetrated plume entrained
! ** more than half of plume entrained
                     ppfn = 0.5d0*(1.0d0 +&
                     &erf((ziavg-he3)/sz3/dsqrt(2.0d0)))
                  endif
               else
! ** whole penetrated plume is still above average mixing height
! ** no contribution from penetrated plume
                  ppfn = 0.0d0
               endif
            else
! ** whole penetrated plume below below ZIAVG
               ppfn = 1.0d0
            end if
            sz3dbg = sz3
         endif
!  ***********************************************  added code end --kja
         sz3new = sz3

!RWB        Change ZEFF to ZRT in following block. RWB 1/23/95
         if(he3 > zrt) then
            zhi = he3
            zlo = max(he3 - szcoef*sz3new, zrt)
         else
            zhi = min(he3 + szcoef*sz3new, zrt)
            zlo = he3
         end if

!RJP        Add temporary debugging statement here.

         if (debug) then
            if( evonly )then
               write(dbgunt, 6040) ievent, he3, sz3new,&
               &zrt,zlo,zhi
6040           format(5x,'Penetrated plume calculation',&
               &' for EVENT # ',i3,//,&
               &5x,'Height of plume center of mass = ',&
               &f6.1,' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',f6.1,' meters.',/)
            else
               write(dbgunt, 6041) irec, he3, sz3new,&
               &zrt,zlo,zhi
6041           format(5x,'Penetrated plume calculation',&
               &' for receptor # ',i3,//,&
               &5x,'Height of plume center of mass = ',&
               &f6.1,' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',f6.1,' meters.',/)
            endif
         end if

         call locate(gridht, 1, mxglvl, zhi, ndxbhi)
         call locate(gridht, 1, mxglvl, zlo, ndxblo)
         ndxalo = ndxblo + 1
         call anyavg ( mxglvl, gridht, gridws,zlo,ndxalo,&
         &zhi,ndxbhi,ueff3 )
         call anyavg ( mxglvl, gridht, gridsv,zlo,ndxalo,&
         &zhi,ndxbhi,sveff3 )
         call anyavg ( mxglvl, gridht, gridsw,zlo,ndxalo,&
         &zhi,ndxbhi,sweff3 )
         call anyavg ( mxglvl, gridht, gridtg,zlo,ndxalo,&
         &zhi,ndxbhi,tgeff3 )
         if (pvmrm .or. grsm) then
            call anyavg ( mxglvl, gridht, grideps, zlo,ndxalo,&
            &zhi,ndxbhi,epseff3 )
         end if
!     JAT 7/22/21 D065 SZ3OLD NOT USED
!            SZ3OLD = SZ3

!RWB        Modify treatment of low wind/low turbulence cases.  R. Brode, PES,
!RWB        8/15/96
         sweff3 = max( sweff3, swmin )
         sveff3 = max( sveff3, svmin, svumin*ueff3 )
         if( l_vectorws )then
            ueff3 = dsqrt( ueff3*ueff3 + 2.0d0*sveff3*sveff3 )
         endif
         ueff3  = max( ueff3, wsmin )

!RJP        Add temporary debugging statement here.

         if(debug) then
            write(dbgunt, 6042) ueff3, sveff3, sweff3
6042        format(5x,'Effective parameters for penetrated ',&
            &'plume:',//,5x,'Ueff3 = ',f7.2,' m/s; ',&
            &'SVeff3 = ',f7.2,&
            &' m/s; SWeff3 = ',f7.2,' m/s.',/)
         end if

      end if

      if (ppf < 1.0d0) then

!RJP        Process the direct plume components here. *************************

         szdavg = 0.5d0 * (szd1 + szd2)
         szdnew = szdavg

!RWB        Change ZEFF to ZRT in following block. RWB 1/23/95
         if (center <= 5.0d0 .and. zrt <= 5.0d0) then
            zhi = min( 5.0d0, zi )
            zhi = min( zhi, zi )
            zlo = 0.0d0
         else if(center > zrt) then
!RWB           Limit ZHI to be .LE. ZI
            zhi = min (center, zi)
            zlo = max (center - szcoef*szdnew, zrt)
         else
            zhi = min (center + szcoef*szdnew, zrt)
            zhi = min (zhi, zi)
            zlo = center
         endif

!RJP        Add temporary debugging statement here.

         if(debug) then
            if( evonly )then
               write(dbgunt, 6050) ievent, center,&
               &szdnew, zrt, zlo, zhi
6050           format(5x,'Direct plume calculation',&
               &' for EVENT # ',i3,//,&
               &5x,'Height of plume center of mass = ',f6.1,&
               &' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',f6.1,' meters.',/)
            else
               write(dbgunt, 6051) irec, center,&
               &szdnew, zrt, zlo, zhi
6051           format(5x,'Direct plume calculation',&
               &' for receptor # ',i3,//,&
               &5x,'Height of plume center of mass = ',f6.1,&
               &' m; Sigma-z estimate = ',f11.1,' m; ',&
               &'Receptor height = ',f6.1,' m; ',/,5x,'New ',&
               &'effective parameters are averaged between ',&
               &f6.1,' and ',f6.1,' meters.',/)
            endif
         end if

!RWB        Check for ZHI .LE. ZLO, skip averages
         if (zhi > zlo) then
            call locate(gridht, 1, mxglvl, zhi, ndxbhi)
            call locate(gridht, 1, mxglvl, zlo, ndxblo)
            ndxalo = ndxblo + 1
            call anyavg ( mxglvl, gridht, gridws, zlo,&
            &ndxalo,zhi,ndxbhi,ueffd )
            call anyavg ( mxglvl, gridht, gridsv, zlo,&
            &ndxalo,zhi,ndxbhi,sveffd )
            call anyavg ( mxglvl, gridht, gridsw, zlo,&
            &ndxalo,zhi,ndxbhi,sweffd )
            if (pvmrm .or. grsm) then
               call anyavg ( mxglvl, gridht, grideps, zlo,&
               &ndxalo,zhi,ndxbhi,epseffd )
            end if
         else
!RWB           Use values at ZI if ZHI .LE. ZLO
            hteff = zi
            call locate(gridht, 1, mxglvl, hteff, ndxeff)
            call gintrp( gridht(ndxeff), gridws(ndxeff),&
            &gridht(ndxeff+1), gridws(ndxeff+1), hteff, ueffd)
            call gintrp( gridht(ndxeff), gridsv(ndxeff),&
            &gridht(ndxeff+1), gridsv(ndxeff+1), hteff,sveffd)
            call gintrp( gridht(ndxeff), gridsw(ndxeff),&
            &gridht(ndxeff+1), gridsw(ndxeff+1), hteff,sweffd)
            if (pvmrm .or. grsm) then
               call gintrp( gridht(ndxeff), grideps(ndxeff),&
               &gridht(ndxeff+1), grideps(ndxeff+1), hteff,epseffd)
            end if
         end if
!     JAT 7/22/21 DO65 SZDOLD NOT USED
!            SZDOLD = SZDAVG


!RWB        Modify treatment of low wind/low turbulence cases.
!RWB        R. Brode, PES, 8/15/96
         sweffd = max( sweffd, swmin )
         sveffd = max( sveffd, svmin, svumin*ueffd )
         if( l_vectorws )then
            ueffd = dsqrt( ueffd*ueffd + 2.0d0*sveffd*sveffd )
         endif
         ueffd  = max( ueffd, wsmin )

!RJP        Add temporary debugging statement here.

         if(debug) then
            write(dbgunt, 6052) ueffd, sveffd, sweffd
6052        format(5x,'Effective parameters for direct ',&
            &'plume:',//,5x,'UeffD = ',f7.2,' m/s; ',&
            &'SVeffD = ',f7.2,&
            &' m/s; SWeffD = ',f7.2,' m/s.',/)
         end if

      end if

   end if

!RWB  Set effective parameters for indirect source = direct source
   if (unstab .and. hs<zi) then
      ueffn  = ueffd
      sveffn = sveffd
      sweffn = sweffd
   end if

   return
end subroutine iblval

subroutine metini
!=======================================================================
!             METINI Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:  To compute the met parameters at stack top and the averages
!             within the mixed layer
!
!   Input:
!
!   Output:
!
!   Called by:   PCALC
!
!   Assumptions:
!
!   Developer(s): Jim Paumier and Roger Brode, PES, Inc.
!   Date:         30 September 1993
!
!   Revision history:
!                      Added SVP and SWP for Aircraft Plume Rise
!                      Gavendra Pandey, UNC-IE, Chapel Hill, NC, USA
!                      04/01/2023
!
!                      Added initialization of effective parameters
!                      to stack top parameters, including TGEFF and
!                      TGEFF3, replacing intializations that were
!                      formerly included in subroutine PCALC.
!                      R.W. Brode, PES, 12/6/99
!
!                      Calls to ZIAVER to average sigma-V, sigma-W
!                      and wind speed moved here from METEXT.  This
!                      allows averaging up to HS when it is higher
!                      than ZI.  It now averages from the surface to
!                      the higher of ZI or HS.  Ref:  Summary of AERMOD
!                      equations, A. Venkatram, 7/7/94.  Changed 7/12/94
!                      by Russell F. Lee.
!
!                      Added calculation of local vertical lagrangian
!                      time scales at stack height and at ZI/2.  These
!                      are needed for calculating the effective TsubLZ
!                      and the horizontal lagrangian time scale,
!                      respectively.  Changed 7/14/94 by R.F. Lee
!
!
!   Reference(s): "Inhomogeneous Boundary Layer", A. Venkatram, 6/25/93
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   use main1
   implicit none
   character :: modnam*12
   double precision :: valabv, vbelow
!---- Declare SVS2 variable to save SVS before SVMIN adjustment for use
!     in US adjustment under LowWind2 option
!      DOUBLE PRECISION :: SVS2

!---- Data dictionary
!
!---- Data initializations
!---- Data initializations
   modnam = 'METINI'
!
!.......................................................................
!---- Compute the parameter values at stack height

!CRFL
!CRFL  Add calculation of local vertical lagrangian time scale
!CRFL  at stack height and at ZI/2.
!CRFL

   if (ndxstk(isrc) >= 1) then
!----    Sigma_V at stack height
      call gintrp( gridht(ndxstk(isrc)), gridsv(ndxstk(isrc)),&
      &gridht(ndxstk(isrc)+1), gridsv(ndxstk(isrc)+1),&
      &hs, svs )

!----    Sigma_W
      call gintrp( gridht(ndxstk(isrc)), gridsw(ndxstk(isrc)),&
      &gridht(ndxstk(isrc)+1), gridsw(ndxstk(isrc)+1),&
      &hs, sws )

!----    Wind speed
      call gintrp( gridht(ndxstk(isrc)), gridws(ndxstk(isrc)),&
      &gridht(ndxstk(isrc)+1), gridws(ndxstk(isrc)+1),&
      &hs, us )

!----    Wind direction
!----    Check for 360 crossover and adjust if necessary
      valabv = gridwd(ndxstk(isrc)+1)
      vbelow = gridwd(ndxstk(isrc))

      if( (valabv-vbelow) < -180.0d0) then
         valabv = valabv + 360.0d0
      else if( (valabv-vbelow) > 180.0d0) then
         valabv = valabv - 360.0d0
      end if

!----    Assign Wind direction
      if (vbelow == valabv) then
         wdir = vbelow
      else
!----       Interpolate to HS
         call gintrp( gridht(ndxstk(isrc)), vbelow,&
         &gridht(ndxstk(isrc)+1), valabv,&
         &hs, wdir )
      end if

!        Check for WDIR > 360 or < 0
      if (wdir > 360.0d0) then
         wdir = wdir - 360.0d0
      else if (wdir <= 0.0d0) then
         wdir = wdir + 360.0d0
      end if
!
!----    Potential temperature gradient
      call gintrp( gridht(ndxstk(isrc)), gridtg(ndxstk(isrc)),&
      &gridht(ndxstk(isrc)+1), gridtg(ndxstk(isrc)+1),&
      &hs, tgs )

!----    Potential temperature
      call gintrp( gridht(ndxstk(isrc)), gridpt(ndxstk(isrc)),&
      &gridht(ndxstk(isrc)+1), gridpt(ndxstk(isrc)+1),&
      &hs, pts )

   else
!        Use GRID value for lowest level
      svs  = gridsv(1)
      sws  = gridsw(1)
      us   = gridws(1)
      wdir = gridwd(1)
      tgs  = gridtg(1)
      pts  = gridpt(1)
   end if

!RWB  Modify the treatment of low wind/low turbulence cases per 7/31/96
!RWB  write-up by Steve Perry.  R. Brode, PES, 8/15/96
   sws = max( sws, swmin )
   svs = max( svs, svmin, svumin*us )
   if( l_vectorws )then
      us  = dsqrt( us*us + 2.0d0*svs*svs )
   endif
   us  = max( us, wsmin )

!
!---- If the wind for the hour is not calm or missing, then convert
!     direction to radians, compute sine and cosine of direction,
!     and determine nearest 10-degree sector.  Note, we shouldn't
!     reach this point if CLMHR or MSGHR is .TRUE.
!
   if( .not.clmhr .and. .not.msghr )then
!
!---->   wind direction = wind direction in degrees * DTORAD

      wdsin = dsin(wdir * dtorad)
      wdcos = dcos(wdir * dtorad)

      afv = wdir - 180.0d0
      if (afv < 0.0d0) then
         afv = afv + 360.0d0
      end if
      ifvsec = idint (afv*0.10d0 + 0.4999d0)
      if (ifvsec == 0) ifvsec = 36

   end if

!
!     ------------------------------------------------------------
!     Apply lower limit of 0.002 K/m to lapse rate for stable
!     layers.
!     ------------------------------------------------------------
!
!RJP
!RJP  ASSIGN TGP AS TGS INITIALLY
!RJP
   tgp = tgs
!

!---- Calculate potential temperature at stack height, PTS, for plume
!     rise calculations.  Compute stack height ambient temperature, TA.
!     NOTE:  TA is no longer the temperature read in by METEXT from the
!            scalar file
   ta = pts - govrcp * ( hs + zbase )

!---- Assign wind speed to use for plume rise, UP = US
   up = us
   svp = svs       ! Added for ARISE; UNC-IE
   swp = sws       ! Added for ARISE; UNC-IE
!     Compute the Brunt-Vaisala frequency, BVF, at stack height for STABLE
!     conditions or for UNSTAB releases above ZI.  Check for TGS < 0 first.
   if ( (tgs>0.0d0) .and.&
   &(stable .or. (unstab .and. hs>=zi)) ) then
      bvf = dsqrt( g * tgs / pts )
   else
      bvf = 1.0d-10
   end if

   if( bvf < 1.0d-10 )then
      bvf =  1.0d-10
   end if

   bvprim  = 0.7d0 * bvf

!RJP  For downwash calculations, set temporarily assigned effective values
   ueff  = us
   sveff = svs
   sweff = sws
   tgeff = tgs
   ueffd  = us
   sveffd = svs
   sweffd = sws
!RWB  Add effective parameters for indirect plume.  RWB, 12/8/94
   ueffn  = us
   sveffn = svs
   sweffn = sws
   ueff3  = us
   sveff3 = svs
   sweff3 = sws
   tgeff3 = tgs

!     Define temporary values of CENTER and SURFAC based on HS
   center = hs
   if( center < 0.1d0*zi )then
      surfac = .true.
   else
      surfac = .false.
   end if

   return
end subroutine metini

subroutine locate ( parray, lvlblw, lvlabv, value, ndxblw )
!=======================================================================
!             LOCATE Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To return the array index such that VALUE is between
!                PARRAY(NDXBLW) and PARRAY(NDXBLW+1).
!
!   Input:       Array of gridded values (PARRAY)
!                Lower array bound at which to start the search (LVLBLW)
!                Upper array bound at which to end the search (LVLABV)
!                Value being searched for (VALUE)
!
!   Output:      Index of PARRAY immediately below VALUE (NDXBLW)
!
!   Called by:   Utility routine that can be used by any module:
!                  SRCSET (in SOSET) for stack heights
!                  METEXT for mixing height
!
!   Assumptions: PARRAY must be montonically increasing or decreasing;
!                LVLBLW can be no less than 1;
!
!   Developer(s): Jim Paumier and Roger Brode, PES, Inc.
!   Date:         30 September 1993
!
!   Revision history:
!                <none>
!
!-----------------------------------------------------------------------
!
!---- Variable declarations
!
   implicit none

   integer   :: lvlabv, lvlblw, ndxblw, jl, jm, ju
   double precision  :: parray(lvlabv), value
!
!---- Data dictionary
!     JL   lower bound temporary variable
!     JM   midpoint temporary variable
!     JU   upper bound temporary variable
!
!----
   jl = lvlblw - 1
   ju = lvlabv + 1

   do while( (ju - jl) > 1 )

      jm = (ju + jl) / 2

      if( value >= parray(jm) )then
         jl = jm
      else
         ju = jm
      endif

   enddo

   ndxblw = min( jl, lvlabv-1 )

   return
end subroutine locate


!RJP  Add subroutine ANYAVG

subroutine anyavg ( nlvls,hts,parray,zbot,ndxabv,ztop,ndxblw,&
&valavg)
!***********************************************************************
!             ANYAVG Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To compute the average value of the parameter between
!                any two heights (ZBOT and ZTOP)
!
!   Input:       Number of levels in the profile (NLVLS)
!                Array of gridded profile heights (HTS)
!                Parameter array (PARRAY)
!                Lower bound of averaging layer (ZBOT)
!                Index of the level gridded profile height immediately
!                   above ZBOT (NDXABV)
!                Upper bound of averaging layer (ZTOP)
!                Index of the level gridded profile height immediately
!                   below ZTOP (NDXBLW)
!
!   Output:      Average value of parameter in layer (VALAVG);
!
!   Called by:   METEXT
!
!   Assumptions: If ZTOP is above the highest profile height (5000 m),
!                then we assume the profile is constant
!                (= PARRAY(NLVLS)) above 5000 m and compute
!                the average accordingly.
!
!   Adjustments: If ZBOT is less than 0.5 m, it is set to 0.5 m.  If ZTOP
!                is less than 0.5 m, it is set to 0.51 m.
!
!   Programmer:  Bob Paine
!
!   Date:        October 4, 1994
!
!   Revision history:
!                Derived from ZIAVER
!
!   Reference(s): Alternative Approach to Treatment of inhomogeneity
!                 October 3, 1994 (Al Cimorelli)
!
!***********************************************************************
!
!---- Variable declarations
!
   implicit none

   integer   :: i, nlvls, ndxabv, ndxblw
   double precision  :: hts(nlvls), parray(nlvls), zbot, ztop,&
   &sum, valavg
   double precision  :: valbot, valtop
!
!---- Data initializations
!
!.......................................................................
!
   sum = 0.0d0
!
!     NDXABV is the profile index of the height just above ZBOT, and
!     NDXBLW is the profile index of the height just below ZTOP.
!
!---- Sum over each layer of the gridded profile (PARRAY) from NDXABV
!     to NDXBLW.  First, check to see if ZBOT and ZTOP are so close
!     together that summation over several profile levels is not
!     necessary.
!
!     Check for minimum values of ZTOP and ZBOT.
!
   if(zbot < 0.5d0) then
      zbot = 0.5d0
      ndxabv = 2
   endif
   if(ztop < 0.51d0) then
      ztop = 0.51d0
      ndxblw = 2
   endif
!
   if(ndxblw < ndxabv) go to 300
   if(ndxblw == ndxabv) go to 200
!
!     Sum using trapezoidal rule over intermediate profile layers.
!
   do i = ndxabv+1, ndxblw
      sum = sum + (hts(i) - hts(i-1)) * 0.5d0 *&
      &(parray(i) + parray(i-1))
   end do
!
!---- Finish the summation over partial layers at bottom (first), then
!     the top.
!
200 continue
   if(ndxabv > 1) then
      call gintrp(hts(ndxabv-1),parray(ndxabv-1),hts(ndxabv),&
      &parray(ndxabv),zbot,valbot)
      sum = sum + (hts(ndxabv) - zbot) * 0.5d0 *&
      &(valbot + parray(ndxabv) )
   else
      sum = sum + (hts(1) - zbot) * parray(1)
   endif

   if(ndxblw < nlvls) then
      call gintrp(hts(ndxblw),parray(ndxblw),hts(ndxblw+1),&
      &parray(ndxblw+1),ztop,valtop)
      sum = sum + (ztop - hts(ndxblw)) * 0.5d0 *&
      &(valtop + parray(ndxblw) )
   else
      sum = sum + (ztop - hts(nlvls)) * parray(nlvls)
   endif
!
!     Take average
!
   valavg = sum / (ztop - zbot)
   go to 999
!
!     At 300, just take the interpolated value halfway between ZBOT
!     and ZTOP, because both are within the same profile layer.
!
300 call gintrp(hts(ndxabv-1),parray(ndxabv-1),hts(ndxabv),&
   &parray(ndxabv),0.5d0*(zbot+ztop),valavg)
!
999 return
end subroutine anyavg
