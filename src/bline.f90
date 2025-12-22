subroutine bl_calc (kk)
!***********************************************************************
!             BL_CALC Routine of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates concentration or deposition values
!                 for BUOYANT LINE sources
!                 (Adpated from the BLP source code)
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    June 30, 2015
!
!        MODIFIED:
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
!                 Particular Source
!
!        CALLED FROM:   CALC
!
!        Assumptions:
!
!***********************************************************************
!     Variable Declarations
   use main1
   use buoyant_line

   implicit none

   character :: modnam*12

   double precision, parameter  :: blcrit = 0.02d0
   double precision, parameter  :: srt2dp = 0.7978846d0
   double precision, parameter  :: avfact = 1.0d0
   double precision, parameter, dimension(6) ::&
   &wspexp = [0.10d0, 0.15d0, 0.20d0, 0.25d0, 0.30d0, 0.30d0]
   double precision, parameter, dimension(6) ::&
   &teran = [0.5d0, 0.5d0, 0.5d0, 0.5d0, 0.30d0, 0.30d0]
   double precision, parameter, dimension(2) ::&
   &dthta = [0.02d0, 0.035d0]

   integer :: i, kk, nblines
! Unused:      INTEGER :: J, K, INOUT
   integer, parameter :: maxit  = 14
   integer, parameter, dimension(7) :: nsega = [3,5,9,17,33,65,129]

   double precision   :: decfac, ftsave(129)
   double precision   :: bl_xvz, bl_xvy
   double precision   :: bl_ueff, bl_theta
! JAT 06/22/21 D065 REMOVE DPBL AS UNUSED VARIABLE
!      DOUBLE PRECISION   :: P, S, TER1, FBRG, WSST, DPBL, CUQ, SUMFPRM
   double precision   :: p, s, ter1, fbrg, wsst, cuq, sumfprm
   double precision   :: ri, unsrt, xfsxx, dlmin, zlinehs, zlinebase
   double precision   :: yv, zv, xb, yb, xe, ye, xmaxl, ymaxl, xminl
   double precision   :: yminl, dxel, dyel, summ, xrecep, tht, hnt
   double precision   :: downx, crossy, sy0, sz0, syc, yrecep
   double precision   :: xrmxkm, ylow, yhigh, virtxz, virtxy
   double precision   :: vxykm, vxzkm, sigy, sigz, z
   double precision   :: terran, h, ft, deltat, sum2, diff, corr
   double precision   :: xseg1, xdiff, yseg1, ydiff, weight
   double precision   :: xcln, ycln, em, b, sfcz0_sav
! CRCO D095 added for BLP debug
   double precision   :: fplmht
   integer :: jitct, iwpbl, itheta, idelta, iwosig, idw, idiv
   integer :: idist, iseg, iseg2, iseg3, iter, indl, i2, ig
   integer :: igsub, nseg, nseg0, nsegm1, indlsv, indlln, nnew
   integer :: ncontrib, ibmin, ibmax, ibmax1, ih, igmax, igmax1
   integer :: lnum, ksrc, lsrc, isrc_sav, kst_sav, bl_urbkst

!     Variable Initializations
   modnam = 'BL_CALC'
   jitct  = 0
   iwpbl  = 0

!CRT  Intialize variables ! 12/27/2017
   ter1 = 0.0d0
   fbrg = 0.0d0                                            ! Wood_D41

   sfcz0_sav = 0.0d0
   isrc_sav  = 0
   kst_sav   = 0

!     The original BLP used meteorology from the met processor CRSTER.
!      CRSTER did not allow wind speeds to be less than 1 m/s.  Set the
!      reference wind speed used in the calculations below to be a minimum
!      of 1.0 m/s.  Use the local variable BL_UREF.  Keep the reference
!      wind speed height as UREFHT.
!      IF (UREF .GE. 1.0D0) THEN
!         BL_UREF = UREF
!      ELSE
!         BL_UREF = 1.0D0
!        WRITE Message              ! Ref ht wind speed less than minimum
!         WRITE(DUMMY,'(''on '',I8.8)') KURDAT
!         CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
!      END IF

!     Save the source number, as determined when the source records are
!       processed by AERMOD, when BL_CALC is called since this routine
!       is not exited until all (one or more) bouyant line source groups
!       are processed
   isrc_sav = isrc

!     LTOPG is called in METEXT.f in subr.SET_METDATA for a buoyant line
!       source; save that value in case it is changed for an urban source
   kst_sav  = kst

!     Save original value for Z0
   sfcz0_sav = sfcz0

! --- Loop over the buoyant line source groups
!jop      BLPGROUP_LOOP: DO KK = 1, NUMBLGRPS

!        Initialize total concentration and partial contributions from
!          each line to 0 for all receptors
   chibl(:)  = 0.0d0
   partch    = 0.0d0

!        Assign the average rise parameters from the arrays (processed
!         during setup) to the scalar
   blavgllen = blavginp_llen(kk)
   blavgbhgt = blavginp_bhgt(kk)
   blavgbwid = blavginp_bwid(kk)
   blavglwid = blavginp_lwid(kk)
   blavgbsep = blavginp_bsep(kk)
   blavgfprm = blavginp_fprm(kk)

! ---    Assign # of individual lines for this BL source group to a scalar
   nblines = nblingrp(kk)

!        Set Mixing Height and Obukhov Length for Urban Option if Needed
!         A tacit assumption is that all lines in a buoyant line source
!         are either ALL urban or rural, i.e., no combination of urban/rural
!         is allowed.  This condition should have been checked in the QA of
!         the source input.

   if (l_blurban(kk)) then
!           Find urban area index for buoyant lines
!            Remember that once one individual BL is encountered that
!            all buoyant lines and BL source groups are processed
!            before moving on to the next non-BL source, so
!            ISRC will not be changing.  The BL lines are already
!            grouped together (group ID index = KK)
      lnum = 0
      if (kk ==1 )then
         ksrc = isrc

      else if (kk >= 2) then
         do i = 1,numsrc
            if (srctyp(i) == 'BUOYLINE') then
               lnum = lnum + 1
               if( blineparms(lnum)%iblpgrpnum == kk) then
                  ksrc = i
                  exit
               end if
            end if
         end do
      end if

      do i = 1, numurb
         if (iurbgrp(ksrc,i) == 1) then
            iurb = i
            exit
         end if
      end do

      if (stable .or. L_MorningTrans(iurb)) then
         urbstab = .true.

!              Set AERMET file values to urban values for use below
         zi = max( ziurb(iurb), zimech )

!              ... and for LTOPG
         sfcz0 = urbz0(iurb)
         if (L_MorningTrans(iurb)) then
            obulen = urbobulen(iurb)
! ---          Call LTOPG for a buoyant line source in an urban area
            call ltopg(bl_urbkst)
            kst = bl_urbkst
         else
            obulen = dabs( urbobulen(iurb) )
! CRCO 12/8/2021 Decision was to set stability to 4 for all urban nighttime hours
            kst = 4
         end if





      else
!              Reset ZI and OBULEN back to rural values that were saved in
!              SET_METDATA and URBCALC (both in metext.f)
!              KST was not changed and retains the rural/original value
         urbstab = .false.
         zi = zirur
         obulen = rurobulen
      end if

   else if (urban .and. .not. l_blurban(kk) ) then
!           Reset ZI and OBULEN back to rural values that were saved in
!           SET_METDATA and URBCALC (both in metext.f)
!           KST was not changed and retains the rural/original value
      urbstab = .false.
      zi = zirur
      obulen = rurobulen

   else
! ---       Rural
!           KST was not changed and retains the rural/original value
      urbstab = .false.
   end if

!        Use the decay coefficient, DECOEF (default or input by the user),
!         but only under the conditions noted, for BLP's decay factor (DECFAC)
!         IF (DFAULT .and. URBAN .and. POLLUT.EQ.'SO2' .and.
!        &    L_BLURBAN(KK)) THEN
!        modified 9/12/17 JAT, use half-life for SO2 URBAN even without DFAULT
!        if HALFLIFE or DCAYCOEF used, use that value, not 4-hours

   if (urban .and. pollut=='SO2' .and. l_blurban(kk) .and.&
   &((icstat(7) == 0 .and. icstat(8) == 0) .or. dfault)) then
      decfac = decoef
   else if ((pollut=='SO2' .and. l_blurban(kk)) .or.&
   &dfault) then  !rural source for SO2 or default modified 10/12/17
      decfac = 0.0d0
   else
!           DECFAC = 0.0D0 !commented out JAT 10/12/17
      decfac = decoef !now add ability to use DECAY coefficient or half life
   end if

! ---    Wood, 12/1/2019: modified for multiple buoyant lines
!        If there is an hourly emissions file with buoyant lines,
!          calculate the average FPRIME (buoyancy parameter) from the
!          values in the file
!
   if (hrlyblcount(kk) > 0) then
!           Compute the average BLAVGFPRM from ther values in the
!            HOUREMIS file
      lnum = 0
      sumfprm = 0.0

      do isrc = 1, numsrc
         if (srctyp(isrc) == 'BUOYLINE') then
            lnum = lnum + 1
            lsrc = blineparms(lnum)%isrcnum
            if (qflag(lsrc) == 'HOURLY' .and.&
            &blineparms(lnum)%iblpgrpnum == kk) then
               sumfprm = sumfprm + afp(lsrc)
            end if
         end if
      end do
      blavgfprm = sumfprm / dble(hrlyblcount(kk))
   end if

!        Calculate distance (from XFB) to final neutral plume rise
!         assuming plumes interact before reaching terminal rise
!         Note: the multiplication needs to be done when hourly values
!               are not available from an HOUREMIS file and BLAVGFPRM
!               is entered on the BLPINPUT record(s)
   fbrg = dble(nblines) * blavgfprm/pi
   if (fbrg <= 55.0d0) then
!           Value: 49 = 3.5*14.
      bl_xfinal = 49.0d0 * fbrg**0.625d0
   else
!           Value: 120.72 = 3.5 * 34.49 (34.49=CONST3 in BLP)
      bl_xfinal =  120.72d0 * fbrg**0.4d0
   end if
   xmatch = bl_xfinal

!        Compute the difference between the endpoint of the x coordinates
!         AFTER the first translation/rotation for the BL group being
!         processed
   do lnum = 1,nbltotal
      if (blineparms(lnum)%iblpgrpnum == kk) then
         del(lnum) =&
         &blineparms(lnum)%xend_tr1-blineparms(lnum)%xbeg_tr1
      end if
   end do

!        Rotate source and receptors so the y-axis of the buoyant line
!         is parallel to the wind direction for this BL source group

   call bl_rotate2 (wdref, bl_theta, itheta, numrec, nbltotal, kk)

! ---    Create array of BLINEs (part of RWB method for determining if
!          receptor in in bounding box - did not work for horizontal and
!          vertical sources)
!         DO K = 1, 4
!          DO I = 1, NBLINES
!             DO J = 1, 129
!                XBL_RCS_MAX(K) = MAX( XBL_RCS_MAX(K), XS_RCS(I,J) )
!                YBL_RCS_MAX(K) = MAX( YBL_RCS_MAX(K), YS_RCS(I,J) )
!                XBL_RCS_MIN(K) = MIN( XBL_RCS_MIN(K), XS_RCS(I,J) )
!                YBL_RCS_MIN(K) = MIN( YBL_RCS_MIN(K), YS_RCS(I,J) )
!             ENDDO
!          ENDDO
!         ENDDO

!        Get the power law exponent based on the stability;
!         calculate WSST = stack height wind speed
!         calculate S for stable conditions (BLTA is the saved ambient
!         temperature since it is modified when point sources are run)
   p = wspexp(kst)
   wsst = bl_uref * (blavgbhgt/urefht)**p
   if (kst > 4) s = 9.80616d0 * dthta(kst-4)/blta

!        Calculate an effective wind speed, BL_UEFF, with the line source
!         plume rise eqn
!        INPUT:  KST, WSST, S, and P
!        OUTPUT: BL_UEFF = an effective wind speed
   call bl_wsc(kst,wsst,bl_ueff,s,p,nblines)

!        Calculate XFB,LEFF,LD,R0
   call bl_leng(bl_theta,bl_ueff,avfact,nblines)

!        Calculate distance to final rise
!         BL_XFS = distance to final rise
!         BL_XFB = distance to full buoyancy
!         BL_XFINAL = final neutral plume rise
   if (kst <= 4) then
!           Calculate distance to final rise for neutral/unstable conditions
      bl_xfs = bl_xfb + bl_xfinal

!           Find 5 intermediate downwind distances (in addition to XFB)
!           at which plume rise will be calculated
      do idist = 2,7
         ri = dble(idist)
         bl_xdist(idist) = bl_xfs -&
         &(bl_xfs - bl_xfb)*(7.0d0 - ri)/5.0d0
      end do

   else
!           Calculate distance to final rise for stable conditions
      unsrt = (16.0d0*bl_ueff*bl_ueff/s) - (bl_xfb*bl_xfb/3.0d0)

      if (unsrt <= 0.0d0) then
         bl_xfs = (12.0d0*bl_xfb*bl_ueff*bl_ueff/s)**(0.333333d0)
      else
         bl_xfs = 0.5d0 * (bl_xfb + dsqrt(unsrt))
      end if

      xfsxx = bl_ueff*pi/dsqrt(s)
      bl_xfs = dmin1(bl_xfs,xfsxx)

      if (bl_xfs <= bl_xfb) then
         do idist = 2,7
            bl_xdist(idist) = bl_xfs
         end do

      else
!              Find 5 intermediate downwind distances (in addition to XFB)
!               at which plume rise will be calculated
         do idist = 2,7
            ri=dble(idist)
            bl_xdist(idist) = bl_xfs -&
            &(bl_xfs - bl_xfb)*(7.0d0-ri)/5.0d0
         end do
      end if
   end if


   call bl_rise(bl_ueff,kst, s)
!
!        CALCULATE CONCENTRATIONS DUE TO THE LINE SOURCES
!
!        LOOP OVER LINES
!
!        Since this routine is called only once for each bouyant line source
!         group, start the search for the buoyant lines with the current
!         source, i.e. the first buoyant line encountered in the group
!
!        The original source number was saved above to the correct source
!         so as we change the source number here, the processing does not
!         get messed up; the original source number is returned at the end
!         of the BUOY_LINES loop.

   buoy_lines: do lnum = 1,nbltotal
      if (blineparms(lnum)%iblpgrpnum == kk) then
         isrc = blineparms(lnum)%isrcnum
         dlmin     = del(lnum)/128.0d0
         zlinebase = blineparms(lnum)%elev
         zlinehs   = blineparms(lnum)%blhs

! CRCO D095 added for BLP debug, specifically compute plume rise
         if (blpdbug) then
            fplmht = zlinehs + dh(7)
            write(blpunt,3333) srcid(isrc),iyear,imonth,iday,ihour,dh,&
            &bl_xdist,bl_xfb,bl_xfs,kst,urbobulen,&
            &obulen,zlinehs,fplmht
3333        format(1x,a12,4(2x,i2),14(f8.2),2x,2(f8.2),4x,i1,&
            &4(1x,f8.2))
         endif

!              Compute the profiled wind speed at the buoyant line
!               release height
         wsst  = bl_uref * (zlinehs/urefht)**p

!              Assign the emissions stored in the variable AQS to QS
!               (either the constant on the SRCPARM keyword or the
!                hourly emission rate)
         call setsrc

!              Apply the emission factor to the input argument QS
!               (result is stored in QTK)
         call emfact(qs)
!MGS               D183_BUOYLINE_EMISUNIT_WSP 3/1/2024: replaced 1.0E6 w/EMIFAC(1) below
!MGS               CUQ   = QTK * 1.0D06 / (DBLE(NSEGA(1)-1)*WSST)
         cuq   = qtk * emifac(1) / (dble(nsega(1)-1)*wsst) !EMIFAC(1) is for concentration
         sz0   = r0 * srt2dp
         zv    = 1000.0d0 * bl_xvz(sz0,kst)
         sy0   = sz0/2.0d0
         yv    = 1000.0d0 * bl_xvy(sy0,kst)
         xb    = xs_rcs(lnum,1)
         yb    = ys_rcs(lnum,1)
         xe    = xs_rcs(lnum,129)
         ye    = ys_rcs(lnum,129)
         xmaxl = dmax1(xb,xe)
         xminl = dmin1(xb,xe)
         ymaxl = dmax1(yb,ye)
         yminl = dmin1(yb,ye)
         dxel  = xe - xb
         dyel  = ye - yb

!              LOOP OVER RECEPTORS
         receptor_loop: do irec = 1, numrec
            summ      = 0.0d0
            partch    = 0.0d0
            nseg      = 0
            ncontrib  = 0
            xrecep    = xr_rcs(irec)

! Wood_D11 - Include flagpole receptor heights
            zflag = azflag(irec)
!WSP                  THT   = AZELEV(IREC) - ZLINEBASE + ZFLAG
!WSP --- Begin: D173 WSP add 7/24/2023
            if (l_flatsrc(isrc)) then
               tht   = zflag
            else
               tht   = azelev(irec) - zlinebase + zflag
            end if
!WSP --- End: D173 WSP add 7/24/2023

!                 If the receptor is upwind of the line
            if (xrecep <= xminl) then
               cycle receptor_loop
            end if

!                 If the receptor is inside the rectangle defined by the source
!                  cycle to the next receptor ...
            if (bl_rflag(irec,kk)) then
               cycle receptor_loop
            end if

!                 Check for receptor located inside "boundary" of BUOYLINE sources
!                   (RWB method)
!                  DO I = 1, 4
!                     IF( XR_RCS(I) .LE. XBL_RCS_MAX(I) .and.
!           &             XR_RCS(I) .GE. XBL_RCS_MIN(I) )THEN
!                        CHIBL(IREC) = 0.0D0
!                        CYCLE RECEPTOR_LOOP
!                     ENDIF
!                  END DO

            yrecep    = yr_rcs(irec)
!                 IWOSIG keeps track of whether any line segment is within
!                  one sigma y of the current receptor (0=NO,1=YES)
            iwosig = 0

!                 Define region of influence
!                  Max distance from any source segment to current receptor
!                  is equal to (XRECEP-XMINL)
            xrmxkm = (xrecep-xminl)/1000.0d0
            call bl_sigmay(xrmxkm,kst,syc)

            ylow  = yminl - 4.0d0*syc
            yhigh = ymaxl + 4.0d0*syc
            if (yrecep < ylow .or. yrecep > yhigh) then
               cycle receptor_loop
            end if

            ylow  = ylow  + dlmin
            yhigh = yhigh - dlmin
            if (yrecep < ylow .or. yrecep > yhigh) then
               cycle receptor_loop
            end if

!                 Check if receptor is directly downwind of the line
!                  (IDW=0=NO,IDW=1=YES)
            idw = 1

            if (yrecep < yminl .or. yrecep > ymaxl) idw = 0

!                 Check if receptor is on the downwind side of the line
            if (xrecep < xmaxl )then
               if (mod(itheta,90) /= 0) then
                  em = dyel/dxel
                  b = ye - em*xe
                  if (xrecep < (yrecep-b)/em) ncontrib = 999
               end if
            end if

            nseg0  = nsega(1)
            nnew   = nseg0
            iter   = 0
            indl   = 1
            idelta = 128/(nseg0-1)
498         continue
            nseg = nseg+nnew
!
!                 Loop over line segments
!
            segment_loop: do iseg = 1,nnew
               ftsave(indl) = 0.0d0

!                    If current receptor is upwind of a source segment,
!                    then this source segment does not contribute

               if (xs_rcs(lnum,indl) >= xrecep) go to 495
               downx  = xrecep - xs_rcs(lnum,indl)
               crossy = yrecep - ys_rcs(lnum,indl)
               virtxz = downx + zv
               virtxy = downx + yv
               vxykm  = virtxy/1000.0d0
               vxzkm  = virtxz/1000.0d0

               if( vxykm < 0.0d0 .or. vxzkm < 0.0d0 )then
! ---                   Virtual distance is < 0.0; skip this segment
                  go to 495
               endif
               call bl_dbtsig(vxzkm,vxykm,kst,sigy,sigz)

!                    If crosswind distance > 4 * SIGY, then this source
!                     segment does not contribute
               if (4.0d0*sigy < dabs(crossy)) go to 495
               if (dabs(crossy) < sigy) iwosig = 1
               call bl_zrise(lnum,indl,irec,z)
!
!                    Include terrain correction in determining the plume height
!
               ter1 = 1.0d0 - teran(kst)
               hnt  = z + zlinehs
!                    TER1=(1.0-TERAN(KST)); THT=RELEV(I)-LELEV(LNUM)
!                    ZI is the AERMOD-determined mixing height
               terran = ter1 * dmin1(hnt,tht)
               h = hnt - terran

               if (h > zi .and. kst <= 4)go to 495
!
!                    Solve the gaussian point source equation
!
               call bl_gauss(kst,zi,crossy,sigy,sigz,h,ft)
!                    Include decay in determining chi
               deltat = downx/wsst
               ftsave(indl) = ft*(1.0d0 - deltat*decfac)
               ncontrib = ncontrib + 1
495            indl   = indl + idelta
            end do segment_loop

!
!                 First time through loop, calculate the first chi estimate
!
            if (nnew /= nseg0) go to 714
            indl = 1
            nsegm1 = nseg0 - 1
            summ = (ftsave(1) + ftsave(129))/2.0d0
            do iseg2 = 2,nsegm1
               indl = indl + idelta
               summ = summ + ftsave(indl)
            end do

!                 If receptor is within region of influence but not directly
!                  downwind of any part of the line, and SUM=0.0, CHI=0.0
            if (summ <= 0.0d0 .and. idw /= 1) then
               cycle receptor_loop
            end if
!
!                 Calculate the refined chi estimate
!
713         continue
            iter   = iter + 1
            idiv   = min0(iter,2)
            idelta = idelta/idiv
            indl   = 1 + idelta/2
!                 INDL is the subcript of the first new line segment
!                   (save as INDLSV)
            indlsv = indl

            nnew = nsegm1**iter + 0.1d0

!                 If more than 129 line segments (i.e., 64 new segments)
!                  are required, continue to increase the number of
!                  segments but only over the section of the line
!                  which is contributing

            if (nnew > 64) go to 759
            go to 498

714         continue

!                 Subscript of the first new line segment is INDLSV
!                 Subscript of the last new line segment is INDLLN
            indlln = 129 - idelta/2

!                 Sum the first and last new line segments
            sum2 = ftsave(indlsv) + ftsave(indlln)

!                 If there are only 2 new line segments, skip this loop
            if (nnew > 2) then
               indl = indlsv
               i2   = nnew-1
!
!                    Find the sum of all the new line segments
               do iseg3=2,i2
                  indl = indl + idelta
                  sum2 = sum2 + ftsave(indl)
               end do

            end if
!
!                 Compare the new estimate with the previous estimate
!
            sum2 = summ/2.0d0 + sum2/(2.0d0**iter)

!                 At least one line segment must be within one SIGMA Y of
!                  the line (if the receptor is directly downwind of any
!                  part of the line)
            if (idw == 1 .and. iwosig /= 1) go to 758

            diff = dabs(sum2-summ)
!MGS               D183_BUOYLINE_EMISUNIT_WSP 3/4/2024: Added the 1.0E6/EMIFAC(1) to leave this
!MGS                            comparison equivalent to before EMIFAC(1) was added to CUQ above.
!MGS                  IF (DIFF*CUQ .LT. 0.1D0) THEN
            if (diff*cuq*1.0d6/emifac(1) < 0.1d0) then    !EMIFAC(1) is for concentration
               go to 720
            end if

            corr = diff/sum2

            if (corr < blcrit) then
               go to 720
            end if

758         continue
            summ = sum2
            go to 713

!                 If 129 source segments not sufficient, continue
!                  to increase number of segments, but only over the
!                  section of line which is contributing
759         continue

            call bl_sort(ftsave,ibmin,ibmax,iwpbl)

            if (iwpbl /= 999) go to 4949
            iwpbl  = 0
            partch = 0.0d0
            cycle receptor_loop

4949        continue

            ibmax1 = ibmax - 1
            ih     = 0
            igmax  = 1

939         continue
            sum2   = 0.0d0
            igmax1 = igmax + 1

            do 940 ig = ibmin,ibmax1
!                    XCLN = x coordinate (RCS) of current (newest) line segment
!                    YCLN = y coordinate (RCS) of current (newest) line segment
!                    XRCS and YRCS are the translated and rotated line source
!                      segments w.r.t. the hourly flow vector
               xseg1 = xs_rcs(lnum,ig)
               xdiff = xs_rcs(lnum,ig+1) - xseg1
               yseg1 = ys_rcs(lnum,ig)
               ydiff = ys_rcs(lnum,ig+1) - yseg1

               do 941 igsub = 1,igmax
                  weight = dble(igsub)/dble(igmax1)
                  xcln   = xseg1 + weight*xdiff
                  ycln   = yseg1 + weight*ydiff
                  downx  = xrecep - xcln
                  crossy = yrecep - ycln
                  virtxz = downx + zv
                  virtxy = downx + yv
                  vxykm  = virtxy/1000.0d0
                  vxzkm  = virtxz/1000.0d0

                  if( vxykm < 0.0d0 .or. vxzkm < 0.0d0 )then
! ---                      Virtual distance is < 0.0; skip this segment
                     go to 941                                   ! not sure if this is best fix
                  endif

                  call bl_dbtsig(vxzkm,vxykm,kst,sigy,sigz)
                  call bl_zrise(lnum,ig,irec,z)

!                       Include terrain correction in determining plume height
                  hnt    = z + zlinehs
!                       TER1   = (1.0-TERAN(KST)); THT=RELEV(I)-LELEV(LNUM)
                  terran = ter1*dmin1(hnt,tht)
                  h      = hnt - terran
                  call bl_gauss(kst,zi,crossy,sigy,sigz,h,ft)

!                       Include decay in determining chi
                  deltat = downx/wsst
                  ft     = ft*(1.0d0 - deltat*decfac)
                  sum2   = sum2 + ft
                  ncontrib = ncontrib + 1
941            continue

940         continue

!                 Compare the new estimate with the previous estimate
            sum2 = summ/2.0d0 + sum2/(2.0d0**iter)

            diff = abs(sum2-summ)
!MGS               D183_BUOYLINE_EMISUNIT_WSP 3/4/2024: Added the 1.0E6/EMIFAC(1) to leave this
!MGS                            comparison equivalent to before EMIFAC(1) was added to CUQ above.
!MGS                  IF (DIFF*CUQ .LT. 0.1D0) THEN
            if (diff*cuq*1.0d6/emifac(1) < 0.1d0) then    !EMIFAC(1) is for concentration
               go to 720
            endif

            corr = diff/sum2
            if (corr < blcrit) then
               go to 720
            end if

            summ = sum2
            iter = iter + 1
            if (iter >= maxit) then
               summ = sum2
               partch      = cuq*summ
               chibl(irec) = chibl(irec) + partch
               if (debug) then
                  write(dbgunt,6000) irec, chibl(irec)
6000              format(/,5x,'Buoyant Line iteration fails to ',&
                  &' converge for receptor',i6,', conc. =',f13.6)
                  jitct = jitct+1
                  if (mod(jitct,100) == 0 ) then
                     write(dbgunt,6001) jitct
6001                 format(/,5x,'Buoyant Line iterations for',&
                     &' all receptors fails for ',i8,'th time' )
                  end if
               endif
               cycle receptor_loop
            end if
            ih = ih + 1
            igmax = 2**ih
            go to 939
720         continue

            summ = sum2

!                 Test to make sure at least two line segments contributed
!                  to the chi estimate (unless receptor is on the upwind side
!                  of the line with some source segments downwind and some
!                  source segments upwind -- in that case just use the test
!                  for convergence)
            if (ncontrib < 2) go to 713

!                 Calculate concentration (in micrograms);
!                  use stack height wind speed for dilution

            partch = cuq*summ
            chibl(irec)  = chibl(irec) + partch
            hrval = partch

!                 For the initial implementation of BLP into AERMOD, we
!                  are skipping calculations with PVMRM, OLM.
!                  As BLP is integrated more into AERMOD, those options
!                  will be included (e.g., see PCALC).
!
            if (arm2) then
! ---                Store conc by source and receptor for ARM/ARM2 options
               do ityp = 1, numtyp
                  chi(irec,isrc,ityp) = hrval(ityp)
               end do

!                    Initialize __VAL arrays (1:NUMTYP)
               hrval   = 0.0d0

            end if

!                 Sum HRVAL to AVEVAL and ANNVAL Arrays      ---   CALL SUMVAL
            if (evonly) then
               call ev_sumval
            else
               do igrp = 1, numgrp
                  call sumval
               end do
            end if

            if (eval(isrc)) then
!                    Check ARC centerline values for EVALFILE
!                    output                                  ---   CALL EVALCK
               call evalck
            end if

!                 Initialize __VAL arrays (1:NUMTYP)
            hrval   = 0.0d0

         end do receptor_loop
      end if                    ! BLINEPARMS(LNUM)%IBLPGRPNUM = KK
   end do buoy_lines

!        Since all buoyant line groups and the associated individual
!         buoyant lines are processed in consecutive calls to BL_CALC
!         from subr,CALC, the PG stability, mixing height, roughness
!         length, and Obukhov length need to be restored to the
!         original values if the BL group was an urban source
   if (urbstab) then
      isrc = isrc_sav
      kst  = kst_sav
      zi   = zirur
      sfcz0  = SFCZ0_sav
      obulen = rurobulen
   else
      isrc = isrc_sav
      kst  = kst_sav
      sfcz0  = SFCZ0_sav
   end if

!     END DO BLPGROUP_LOOP

!     ISRC = ISRC_SAV
!     KST  = KST_SAV
!     ZI   = ZIRUR
!     SFCZ0  = SFCZ0_sav
!     OBULEN = RUROBULEN

   return
end subroutine bl_calc

subroutine bl_dbtsig (xz,xy,istab,sy,sz)
!***********************************************************************
!             BL_DBTSIG Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     PURPOSE: To estimate sigma-y and sigma-z for buoyant line sources
!
!     INPUT:
!       XZ     Virtual distance (kilometers) - VXZKM in calling program
!       XY     Virtual distance (kilometers) - VXYKM in calling program
!       ISTAB  PG stability category
!
!     OUTPUT:
!       SY     Sigma-y
!       SZ     Sigma-z
!
!     ------------------------------------------------------------------
!
   implicit none

   double precision, intent(in)  :: xz, xy
   double precision, intent(out) :: sy, sz
   integer, intent (in) :: istab
   integer :: id

   double precision :: th

   double precision, parameter, dimension(7) ::&
   &xa = [0.5d0, 0.4d0, 0.3d0, 0.25d0, 0.2d0, 0.15d0, 0.1d0]

   double precision, parameter, dimension(2) ::&
   &xb = [0.4d0, 0.2d0]

   double precision, parameter, dimension(5) ::&
   &xd = [30.0d0, 10.0d0, 3.0d0, 1.0d0, 0.3d0]

   double precision, parameter, dimension(8) ::&
   &xe = [40.0d0, 20.0d0, 10.0d0, 4.0d0, 2.0d0,&
   &1.0d0, 0.3d0, 0.1d0]

   double precision, parameter, dimension(9) ::&
   &xf = [60.0d0, 30.0d0, 15.0d0, 7.0d0, 3.0d0,&
   &2.0d0, 1.0d0, 0.7d0, 0.2d0]

   double precision, parameter, dimension(8) ::&
   &aa = [453.85d0, 346.75d0, 258.89d0, 217.41d0,&
   &179.52d0, 170.22d0, 158.08d0, 122.8d0]

   double precision, parameter, dimension(8) ::&
   &ba = [2.1166d0, 1.7283d0, 1.4094d0, 1.2644d0,&
   &1.1262d0, 1.0932d0, 1.0542d0, 0.9447d0]

   double precision, parameter, dimension(3) ::&
   &ab = [109.30d0, 98.483d0, 90.673d0]

   double precision, parameter, dimension(3) ::&
   &bb = [1.0971d0, 0.98332d0, 0.93198d0]

   double precision, parameter, dimension(6) ::&
   &ad = [44.053d0, 36.650d0, 33.504d0, 32.093d0,&
   &32.093d0, 34.459d0]

   double precision, parameter, dimension(6) ::&
   &bd = [0.51179d0, 0.56589d0, 0.60486d0, 0.64403d0,&
   &0.81066d0, 0.86974d0]

   double precision, parameter, dimension(9) ::&
   &ae = [47.618d0, 35.420d0, 26.970d0, 24.703d0,&
   &22.534d0, 21.628d0, 21.628d0, 23.331d0, 24.26d0]

   double precision, parameter, dimension(9) ::&
   &be = [0.29592d0, 0.37615d0, 0.46713d0, 0.50527d0,&
   &0.57154d0, 0.63077d0, 0.75660d0, 0.81956d0, 0.8366d0]

   double precision, parameter, dimension(10) ::&
   &af = [34.219d0, 27.074d0, 22.651d0, 17.836d0, 16.187d0,&
   &14.823d0, 13.953d0, 13.953d0, 14.457d0, 15.209d0]

   double precision, parameter, dimension(10) ::&
   &bf = [0.21716d0, 0.27436d0, 0.32681d0, 0.41507d0, 0.46490d0,&
   &0.54503d0, 0.63227d0, 0.68465d0, 0.78407d0, 0.81558d0]

!      GO TO (10,20,30,40,50,60),ISTAB

!CRT  Initialize TH, 12/27/2017
   th = 0.0d0

   select case (istab)
    case (1)
!           STABILITY A (10)
      th = (24.167d0 - 2.5334d0 * dlog(xy)) / 57.2958d0
      if (xz > 3.11d0) then
         sz = 5000.0d0
      else
         do id = 1,7
            if (xz >= xa(id)) go to 12
         end do
         id = 8
12       sz = aa(id) * xz ** ba(id)
      endif

    case (2)
!           STABILITY B (20)
      th = (18.333d0 - 1.8096d0 * dlog(xy)) / 57.2958d0
      if (xz > 35.0d0) then
         sz = 5000.0d0
      else
         do id = 1,2
            if (xz >= xb(id)) go to 22
         end do
         id = 3
22       sz = ab(id) * xz ** bb(id)
         if (sz > 5000.0d0) sz = 5000.0d0
      endif

    case (3)
!           STABILITY C (30)
      th = (12.5d0 - 1.0857d0 * dlog(xy)) / 57.2958d0
      sz = 61.141d0 * xz ** 0.91465d0
      if (sz > 5000.0d0) sz = 5000.0d0

    CAsE (4)
!           STABILITY D (40)
      th = (8.3333d0 - 0.72382d0 * dlog(xy)) / 57.2958d0

      do id = 1,5
         if (xz >= xd(id)) go to 42
      end do
      id = 6
42    sz = ad(id) * xz ** bd(id)
      if (sz > 5000.0d0) sz = 5000.0d0

    case (5)
!           STABILITY E (50)
      th = (6.25d0 - 0.54287d0 * dlog(xy)) / 57.2958d0
      do id = 1,8
         if (xz >= xe(id)) go to 52
      end do
      id = 9
52    sz = ae(id) * xz ** be(id)
      if (sz > 5000.0d0) sz = 5000.0d0

    case (6)
!           STABILITY F (60)
      th = (4.1667d0 - 0.36191d0 * dlog(xy)) / 57.2958d0
      do id = 1,9
         if (xz >= xf(id)) go to 62
      end do
      id = 10
62    sz = af(id) * xz ** bf(id)
      if (sz > 5000.0d0) sz = 5000.0d0
   end select

   sy = 1000.0d0 * xy * dsin(th)/(2.15d0 * dcos(th))

   return
end subroutine bl_dbtsig
!
!     ------------------------------------------------------------------
subroutine bl_sigmay(xkm,istab,sy)
!
!             BL_SIGMAY Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates sigma Y
!
!     INPUT:
!       XKM    Virtual distance (kilometers) - VXZKM in calling program
!       ISTAB  PG stability category
!
!     OUTPUT:
!       SY     Sigma-y
!
!     ------------------------------------------------------------------
   implicit none

   double precision, intent(in)  :: xkm
   double precision, intent(out) :: sy
   integer, intent(in)           :: istab

   double precision              :: th

!CRT  Initialize TH  ! 12/27/2017
   th = 0.0d0

   select case (istab)

    case (1)
!           STABILITY A(10)
      th = (24.167d0 - 2.5334d0 * dlog(xkm)) / 57.2958d0

    case (2)
!           STABILITY B
      th = (18.333d0 - 1.8096d0 * dlog(xkm)) / 57.2958d0

!           STABILITY C
    case (3)
      th = (12.5d0- 1.0857d0 * dlog(xkm)) / 57.2958d0

!           STABILITY D
    case (4)
      th = (8.3333 - 0.72385d0 * dlog(xkm)) / 57.2958d0

!           STABILITY E
    case (5)
      th = (6.25d0 - 0.54287d0 * dlog(xkm)) / 57.2958d0

!           STABILITY F
    case (6)
      th = (4.1667d0 - 0.36191d0 * dlog(xkm)) / 57.2958d0

   end select

   sy = 1000.0d0 * xkm * dsin(th)/(2.15d0 * dcos(th))

   return
end subroutine bl_sigmay

subroutine bl_rise(u,istab,s)
!***********************************************************************
!             BL_RISE Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates line source plume rise using an optional vertical wind
!     shear corrected 'effective' wind speed for both neutral and
!     stable conditions
!
!     Input
!       U       = wind speed
!       S       = computed parameter for stable conditions
!       ISTAB   = P-G stability category
!
!     Input via module.f
!       LEFF    = effective building length
!       R0      = edge radius
!       LD      = LEFF * sin (theta), where theta = angle between flow
!                  vector and the orientation of the line source
!       FPRMXNB = fprime * number of lines
!       BL_XFB  = distance to full buoyancy
!       BL_XFS  = distance to final rise for stable conditions
!       XDIST   = array of intermediate distances for downwash calcs
!
!     Output
!       DH      = array of intermediate plume rise values
!
!     ------------------------------------------------------------------
   use buoyant_line
   implicit none

   double precision, intent(in) :: u, s
   integer, intent(in) :: istab

   double precision :: x, a, b, c, z
   integer :: idist
!
!
!     CONSTANT 1.5915494 = 3./(PI*BETA) WITH BETA=0.6
!     CONSTANT 5.0 = 3./BETA WITH BETA=0.6
   a = 1.5915494d0 * leff + 5.0d0 * r0

!     CONSTANT 5.3051648 = 6./(PI*BETA*BETA) WITH BETA=0.6
!     CONSTANT 8.3333333 = 3./(BETA*BETA) WITH BETA=0.6
   b = r0 * (5.3051648d0 * ld + 8.333333d0 * r0)

   do 1000 idist = 2,7
      x = bl_xdist(idist)

      if (istab <= 4  .or. x < bl_xfs) then
         if( dabs(x - bl_xfb) < 1.0d-10 )then
!              CONSTANT 0.4420971 = 1./(2.*PI*BETA*BETA) WITH BETA=0.6
            c = -0.4420971d0 * (fprmxnb/bl_xfb) * (x/u)**3
            call bl_cubic(a,b,c,z)
            dh(idist) = z
         else
!              CONSTANT 1.3262912 = 3./(2.*PI*BETA*BETA) WITH BETA=0.6
            c = -1.3262912d0 * fprmxnb *&
            &(bl_xfb*bl_xfb/3.0d0 + x*x - bl_xfb*x)/u**3
            call bl_cubic(a,b,c,z)
            dh(idist) = z

         end if

      else
!           With stable conditions, use neutral rise equation
!           for transitional rise calculations, but calculate
!           FINAL RISE BASED ON THE FINAL STABLE RISE EQUATION

!           Calculate final (stable) plume rise
!           Constant 5.3051648 = 6./(PI*BETA*BETA) WITH BETA=0.6
         c = -5.3051648d0 * fprmxnb / (u*s)
         call bl_cubic(a,b,c,z)
         dh(idist) = z

      end if

1000 continue

   return
end subroutine bl_rise
!
subroutine bl_zrise(il,is,ir,z)
!***********************************************************************
!             BL_ZRISE Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Z1 is the plume height of the highest plume segment at X = XFB
!       (except in the special case of stable conditions with the
!       distance to final rise (XFS) less than XFB -- in that case, Z1
!       is the height of the highest plume element at X=XFS)
!
!     XI is the distance of the current line segment to XFB
!
!     Input:
!       IL = line number
!       IS = INDL = line segment
!       IR = receptor number
!
!     Output:
!       Z
!
!     ------------------------------------------------------------------
   use buoyant_line

   double precision, intent(out) :: z
   integer, intent(in)           :: il, ir, is
   double precision :: z1, z2, xi, zxfb
!
   z1 = dh(2)

   xi = bl_xfb - xs_rcs(il,is)
   xi = dmax1(xi,0.0d0)
   xi = dmin1(xi,bl_xfb)
   zxfb = z1*(1.0d0 - (bl_xfb-xi)/bl_xfb)

!     Z2, returned from BLP_INTRSE, is the plume height of the highest
!         segment at X
   call bl_intrse(xr_rcs(ir),z2)

   z = zxfb + z2 - z1

   return
end subroutine bl_zrise

!     ------------------------------------------------------------------
subroutine bl_intrse(x,z)
!
!             BL_INTRSE Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Interpolates the plume rise of the top (highest) plume element
!       at any distance X using the calculated plume rise at
!       seven points (BL_XDIST(1-7)), as computed in BL_RISE just before
!       entering the loop over the buoyant lines.
!
!     Input:
!       X   = Distance X
!
!     Output:
!       Z   = Interpolated plume rise
!
!     ------------------------------------------------------------------
   use buoyant_line
   implicit none

   double precision, intent(in)  :: x
   double precision, intent(out) :: z

   integer    :: ndex, ndex1, idist
!C
   if (x > bl_xdist(7)) then
!        PLUME REACHES FINAL RISE
      z = dh(7)
   else
! Wood_D117: BL_INTRSE error
!         NDEX = 5                                          ! D117
!         DO 10 IDIST = 2,6                                 ! D117

      do 10 idist = 2,7                                  ! D117
         if (x < bl_xdist(idist)) then                ! D117
            ndex = idist
            exit
         endif
10    continue
      ndex1 = ndex-1
      z = dh(ndex) - (dh(ndex)-dh(ndex1)) * (bl_xdist(ndex) - x) /&
      &(bl_xdist(ndex) - bl_xdist(ndex1))
   end if

   return
end subroutine bl_intrse

subroutine bl_gauss(istab,dpbl,crossy,sigy,sigz,h,ft)
!***********************************************************************
!             BL_GAUSS Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Input:
!       ISTAB   = PG stability category
!       DPBL    = mixing height
!       CROSSY  = crosswind distance from receptor to source segment
!       SIGY    = sigma-y calculated in BL_DBTSIG
!       SIGZ    = sigma-z calculated in BL_DBTSIG
!       H       = terrain-dependent height (for ??)
!
!     Output:
!       FT
!     ------------------------------------------------------------------
   use buoyant_line

   implicit none
   integer, intent(in) :: istab
   double precision, intent(in) :: dpbl, crossy, sigy, sigz, h
   double precision, intent(out) :: ft

   double precision :: tmin, tmax, td1, ypsig, expyp, exphp
   double precision :: arg, f, f1, t, h2, hpsig, epsil

   data tmin/0.0512d0/,tmax/9.21d0/, epsil/1.0d-30/

   td1   = 3.1415927d0 * sigy * sigz
   ypsig = crossy/sigy
   expyp = 0.5d0 * ypsig * ypsig

!     Prevent underflows
   if (expyp > 50.0d0) then
      f  = 0.0d0                 ! not really needed
      f1 = 0.0d0                 ! not really needed
      ft = 0.0d0
      return
   end if

   f = exp(-expyp)

!     If mixing height (DPBL) GE 5000 meters or for stable conditions,
!     neglect the reflection terms
   if (istab >= 5 .or. dpbl > 5000.0d0) go to 451

!     If SIGZ GT 1.6*DPBL, assume a uniform vertical distribution
   if (sigz > 1.6d0*dpbl) go to 460

!     Calculate multiple eddy reflections terms
!     using a fourier series method -- see ERT MEMO CS 093

   f1 = 1
   t  = (sigz/dpbl)**2
   h2 = h/dpbl

   if (t < 0.6d0) then
      arg = 2.0d0 * (1.0d0 - h2)/t
      if (arg < tmax) then
         if (arg < tmin) then
            f1 = f1 + 1.0d0 - arg
         endif
         if(arg >= tmin) then
            f1 = f1 + exp(-arg)
         endif
         arg = 2.0d0 * (1.0d0 + h2)/t
         if (arg < tmax) then
            f1 = f1 + exp(-arg)
            arg = 4.0d0 * (2.0d0 - h2)/t
            if (arg < tmax) then
               f1 = f1 + exp(-arg)
               arg = 4.0d0 * (2.0d0 + h2)/t
               if (arg < tmax) then
                  f1 = f1 + exp(-arg)
               endif
            endif
         endif
      endif
      arg = -0.5d0 * h2 * h2/t
      if (arg < -90.0d0) then
         f1 = 0.0d0
      endif

!        The constant 0.797885 = SQRT(2./PI)
      if (arg >= -90.0d0) then
         f1 = 0.797885d0 * f1 * exp(arg)/sigz
      endif
      if (f1 < epsil) f1 = 0.0d0

   else
!        The constant 4.934802 = PI*PI/2.
      arg = 4.934802d0 * t
      if (arg < tmax) then
         f1 = f1 + 2.0d0 * dexp(-arg) * dcos(3.141593d0*h2)

!            The constant 19.739209 = 2.*PI*PI
         arg = 19.739209d0 * t
         if (arg < tmax) then
            f1 = f1 + 2.0d0 * dexp(-arg) * dcos(6.283185d0*h2)
         endif
      endif
      f1 = f1/dpbl
      if (f1 < epsil) f1 = 0.0d0
   endif

!     The constant 1.25331414 = SQRT(PI/2.)
   f1 = 1.25331414d0 * sigz * f1
   go to 445
451 continue

   hpsig = h/sigz
   exphp = 0.5d0 * hpsig * hpsig
   if (exphp > 50.0d0) then
      f1 = 0.0d0
   else
      f1 = dexp(-exphp)
   endif

445 continue

!     Find product of exponential terms divided by (PI*SIGY*SIGZ)
   ft = f * f1/td1
   go to 470
460 continue

!     Vertical distribution assumed uniform
!     The constant 2.5066283 = SQRT(2.*PI)
   ft = f/(2.5066283d0 * sigy * dpbl)

470 return
end subroutine bl_gauss

!
!     ------------------------------------------------------------------
double precision function bl_xvz (szo,istab)
!
!
!     ------------------------------------------------------------------
   double precision :: szo
   integer :: istab, id

   double precision, parameter, dimension(7) ::&
   &sa = [13.95d0, 21.40d0, 29.3d0, 37.67d0, 47.44d0,&
   &71.16d0 ,104.65d0]

   double precision, parameter, dimension(2) ::&
   &sb = [20.23d0, 40.0d0]

   double precision, parameter, dimension(5) ::&
   &sd = [12.09d0, 32.09d0, 65.12d0, 134.9d0, 251.2d0]

   double precision, parameter, dimension(8) ::&
   &se = [3.534d0, 8.698d0, 21.628d0, 33.489d0, 49.767d0,&
   &79.07d0, 109.3d0, 141.86d0]

   double precision, parameter, dimension(9) ::&
   &sf = [4.093d0, 10.93d0, 13.953d0, 21.627d0, 26.976d0,&
   &40.0d0, 54.89d0, 68.84d0, 83.25d0]

   double precision, parameter, dimension(8) ::&
   &aa = [122.8d0, 158.08d0, 170.22d0, 179.52d0, 217.41d0,&
   &258.89d0, 346.75d0, 453.85d0]

   double precision, parameter, dimension(3) ::&
   &ab = [90.673d0, 98.483d0, 109.3d0]

   double precision, parameter, dimension(6) ::&
   &ad = [34.459d0, 32.093d0, 32.093d0, 33.504d0,&
   &36.650d0, 44.053d0]

   double precision, parameter, dimension(9) ::&
   &ae = [24.26d0, 23.331d0, 21.628d0, 21.628d0, 22.534d0,&
   &24.703d0, 26.97d0, 35.42d0, 47.618d0]

   double precision, parameter, dimension(10) ::&
   &af = [15.209d0, 14.457d0, 13.953d0, 13.953d0, 14.823d0,&
   &16.187d0, 17.836d0, 22.651d0, 27.074d0, 34.219d0]

   double precision, parameter, dimension(8) ::&
   &ca = [1.0585d0, 0.9486d0, 0.9147d0, 0.8879d0, 0.7909d0,&
   &0.7095d0, 0.5786d0, 0.4725d0]

   double precision, parameter, dimension(3) ::&
   &cb = [1.073d0, 1.017d0, 0.9115d0]

   double precision, parameter, dimension(6) ::&
   &cd = [1.1498d0, 1.2336d0, 1.5527d0, 1.6533d0,&
   &1.7671d0, 1.9539d0]

   double precision, parameter, dimension(9) ::&
   &ce = [1.1953d0, 1.2202d0, 1.3217d0, 1.5854d0, 1.7497d0,&
   &1.9791d0, 2.1407d0, 2.6585d0, 3.3793d0]

   double precision, parameter, dimension(10) ::&
   &cf = [1.2261d0, 1.2754d0, 1.4606d0, 1.5816d0, 1.8348d0,&
   &2.151d0, 2.4092d0, 3.0599d0, 3.6448d0, 4.6049d0]
!
!CRT  Initialize BL_XVZ
   bl_xvz = 0.0d0

   select case (istab)

    case (1)
!          STABILITY A
      do id = 1,7
         if (szo <= sa(id)) go to 12
      end do
      id = 8
12    bl_xvz =(szo/aa(id))**ca(id)

    case (2)
!          STABILITY B
      do id = 1,2
         if (szo <= sb(id)) go to 22
      end do
      id = 3
22    bl_xvz = (szo/ab(id))**cb(id)

    case (3)
!          STABILITY C
      bl_xvz = (szo/61.141d0)**1.0933d0

    case (4)
!          STABILITY D
      do id = 1,5
         if(szo <= sd(id)) go to 42
      end do
      id = 6
42    bl_xvz = (szo/ad(id))**cd(id)

    case (5)
!          STABILITY E
      do id = 1,8
         if (szo <= se(id)) go to 52
      end do
      id = 9
52    bl_xvz = (szo/ae(id))**ce(id)

    case (6)
!          STABILITY F
      do id = 1,9
         if (szo <= sf(id)) go to 62
      end do
      id = 10
62    bl_xvz = (szo/af(id))**cf(id)

   end select

end function bl_xvz
!
!     ------------------------------------------------------------------
double precision function bl_xvy (syo,istab)
!
!
!     ------------------------------------------------------------------
   implicit none
   double precision :: syo
   integer          :: istab
!
!
!CRT  Initialize BL_XVY, 12/27/2017
   bl_xvy = 0.0d0

   select case (istab)
    case (1)
      bl_xvy = (syo/213.0d0)**1.1148d0

    case (2)
      bl_xvy = (syo/155.0d0)**1.097d0

    case (3)
      bl_xvy = (syo/103.0d0)**1.092d0

    CAsE (4)
      bl_xvy = (syo/68.0d0)**1.076d0

    case (5)
      bl_xvy = (syo/50.0d0)**1.086d0

    case (6)
      bl_xvy = (syo/33.5d0)**1.083d0
   end select

end function bl_xvy

subroutine bl_wsc(istab,um,u,s,p,nbl)
!***********************************************************************
!             BL_WSC Routinee of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates an effective U using the line source plume rise
!     equation (line source term only) matched at X = XF (final rise)
!
!     INPUT:
!       ISTAB = P-G stability category
!       UM    = WSST = stack height wind speed from power law
!       S     = a computed parameter for stable conditions
!       NBL   = number of individual buoyant lines in the BL group
!     OUTPUT:
!       U
!
!     ------------------------------------------------------------------
   use buoyant_line
   implicit none

   double precision, intent(out) :: u
   double precision, intent(in)  :: um, s, p
   integer, intent(in) :: istab, nbl

   double precision :: p2, p3, ep, epi, t1, z


   if (istab <= 4) then
!
!        NEUTRAL (OR UNSTABLE) CONDITIONS
!
      p3  = 3.0d0 * p
      ep  = 2.0d0 + p3
      epi = 1.0d0 / ep

!        CONSTANT 2.4=4.*BETA WITH BETA=0.6
      t1 = (ep*ep * dble(nbl) * blavgfprm * blavgbhgt**p3 /&
      &(2.4d0 * (2.0d0 + p) * blavgllen * um**3))**epi
      z  = t1*xmatch**(2.0d0*epi)

!        CONSTANT 1.2 = 2.*BETA WITH BETA=0.6
      u  = (dble(nbl) * blavgfprm /&
      &(1.2d0 * blavgllen) * (xmatch/z)*(xmatch/z))**0.333333d0
      u  = dmax1(u,um)

   else
!
!        STABLE CONDITIONS
!
      p2 = 2.0d0 + p

!        CONSTANT 0.6 = BETA
      z = (p2 * blavgbhgt**p * dble(nbl) * blavgfprm /&
      &(0.6d0 * blavgllen * um * s))**(1.0d0/p2)

!        CONSTANT 3.3333333 = 2./BETA WITH BETA=0.6
      u = 3.3333333d0 * dble(nbl) * blavgfprm / (blavgllen*s*z*z)
      u = dmax1(u,um)
   endif

   return
end subroutine bl_wsc
!
subroutine bl_leng(theta, u, avfactor, nbl)
!***********************************************************************
!             BL_LENG Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Calculates XFB,LEFF,LD,R0 (all in the buoyant_line module)
!
!     Input:
!       THETA = angle of second rotation for the source y-axis (after
!               the first rotation) to align with the flow vector
!       U     = effective wind speed (BL_UEFF in calling routine)
!       AVFACTOR = constant (parameter) = 1.0D0
!       NBL   = number of individual buoyant lines in the BL group
!
!     ------------------------------------------------------------------
   use buoyant_line
   implicit none

   double precision, intent(in) :: u, theta, avfactor
   double precision :: leff1, leffv, dxm, dxm833, rad, t1, xi
   double precision :: thrad, sint, cost
   integer, intent(in) :: nbl

   data rad/0.0174533d0/
!
!     BLAVGFPRM is the 'average' buoyancy flux of one line;
!     FPRMXNB is the 'effective' buoyancy flux of n lines
   fprmxnb = dble(nbl) * blavgfprm
   thrad    = theta * rad
   sint    = abs(dsin(thrad))
   cost    = abs(dcos(thrad))

!     Calculate distance of full buoyancy (XFB)
   dxm    = blavgbsep + blavgbwid
   bl_xfb = blavgllen * cost + dble(nbl-1) * dxm * sint

!     Calculate effective line source length (LEFF) and
!     effective downwash line length (LD)
   leff1 = blavgllen * sint

   if (nbl == 1) then
!        IF N = 1, NO INTERACTION AT ANY X, I.E.,
!        LEFFV = average line width;
!        'Effective' buoyancy flux (FPRMXNB) = average buoyancy flux;
!        Distance to full buoyancy (BL_XFB) =
!         (average building length) * COST + (average line width) * SINT
      leffv = blavglwid
      fprmxnb = blavgfprm
      bl_xfb = bl_xfb + blavglwid*sint

   else
!        CONSTANT 0.8333333 = 1./(2.*BETA) WITH BETA=0.6
      dxm833 = 0.8333333d0*dxm       ! DXM is 'average' distance between two lines

!        CONSTANT 2.2619467 = 2.*PI*BETA*BETA WITH BETA=0.6
!        CONSTANT 1.5915494 = 3./(PI*BETA) WITH BETA=0.6
      t1 = (2.2619467d0 * u**3 / blavgfprm) *&
      &dxm833 * dxm833 * (dxm833 + 1.5915494d0*blavglwid)
      xi = (t1*blavgllen)**0.333333d0

      if (xi > blavgllen) then
         xi = blavgllen/2.0d0 +&
         &dsqrt(12.0d0*t1 - 3.0d0*blavgllen*blavgllen)/6.0d0

!           CONSTANT 1.2 = 2.*BETA WITH BETA=0.6
!           CONSTANT 0.6283185 = PI*BETA/3. WITH BETA=0.6
         leffv = fprmxnb * (blavgllen*blavgllen/3.0d0 +&
         &xi*(xi-blavgllen)) / (1.2d0 * u**3 * dxm833 * dxm833) -&
         &0.6283185d0 * dxm833

      else
!           CONSTANT 3.6 = 6.*BETA WITH BETA=0.6
!           CONSTANT 0.6283185 = PI*BETA/3. WITH BETA=0.6
         leffv = fprmxnb/(3.6d0 * blavgllen * dxm833 * dxm833) *&
         &(xi/u)**3 - 0.6283185d0 * dxm833
      endif
   endif

   leff = leff1 + leffv*cost
   ld   = leff * sint

!     Calculate downwashed edge radius
   r0 = dmin1(blavgbhgt,ld)/avfactor

   return
end subroutine bl_leng
!
!     ------------------------------------------------------------------
subroutine bl_sort(ftsave,ibmin,ibmax,iwpbl)
!
!             BL_SORT Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     ------------------------------------------------------------------
   implicit none
   double precision, intent(inout) :: ftsave(129)
   integer, intent(inout)          :: ibmin, ibmax, iwpbl

! Unused:      DOUBLE PRECISION :: FT
   integer          :: ib, isafe, ilevel, neachl, incr, indexi, nc
   integer          :: incrm, incrp

   isafe = 0
   ib    = 0
   if (ftsave(129) /= 0.0d0) ib = 129
   if (ftsave(1)   /= 0.0d0) ib = 1
   if (ib /= 0) go to 970

   outer: do ilevel = 1,7
      neachl = 2**(ilevel-1)
      incr   = 2**(8-ilevel)
      indexi = 1 + incr/2
      do nc = 1,neachl
         if (ftsave(indexi) == 0.0d0) go to 944
         ib = indexi
         go to 970
944      indexi = indexi + incr
      end do
   end do outer

   if (ib /= 0) go to 970
   iwpbl = 999
   return

970 ibmin = ib - 1
   ibmax = ib + 1
   ibmin = max(ibmin,1)
   ibmax = min(ibmax,129)

975 continue
   incrm = 0
   incrp = 0
   if (ftsave(ibmin) /= 0.0d0) incrm = 1
   if (ibmin ==1) incrm = 0
   if (ftsave(ibmax) /= 0.0d0) incrp = 1
   if (ibmax == 129) incrp = 0

   ibmin = ibmin - incrm
   ibmax = ibmax + incrp
   if (incrm == 0 .and. incrp == 0) go to 980
   isafe = isafe + 1
   if (isafe > 129) go to 980
   go to 975
980 continue

   return
end subroutine bl_sort
!
!     ------------------------------------------------------------------
subroutine bl_cubic(a,b,c,z)
!
!             BL_CUBIC Routine of the AMS/EPA Regulatory Model - AERMOD
!
!     Solves for one root of the cubic equation:
!        Z**3 + A*Z**2 + B*Z + C = 0
!     ------------------------------------------------------------------
!
   implicit none
   double precision, intent(in)  :: a, b, c
   double precision, intent(out) :: z
   double precision :: a3, ap, bp, ap3, bp2, troot, tr
   double precision :: app, bsv, one, bpp, cm, alpha, sgn

   data one/1.0d0/

   a3    = a/3.0d0
   ap    = b - a*a3
   bp    = 2.0d0*a3**3 - a3*b + c
   ap3   = ap/3.0d0
   bp2   = bp/2.0d0
   troot = bp2*bp2 + ap3*ap3*ap3

   if (troot > 0.0d0) then
      tr  = dsqrt(troot)
      app = (-bp2 + tr)**0.333333d0
      bsv = -bp2 - tr

      if (bsv == 0.0d0) then
!           BSV (& BPP) = 0.0
         z = app-a3

      else
         sgn = dsign(one,bsv)
         bpp = sgn*(dabs(bsv))**0.333333d0
         z   = app + bpp - a3
      endif

   else
      cm    = 2.0d0 * dsqrt(-ap3)
      alpha = dacos(bp/(ap3*cm))/3.0d0
      z     = cm*dcos(alpha) - a3
   endif

   return
end subroutine bl_cubic

subroutine bl_rotate2 (wd1, theta, itheta, nrecep, nbl, kk)
!***********************************************************************
!             BL_ROTATE2 Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Rotates Sources and Receptors for Buoyant Line Source
!                 Calculations - Aligns the Y-Axis of the Source
!                 Parallel to the Wind Direction
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    January 5, 2015
!
!        INPUT:
!          WD1    - wind direction for the hour
!          NRECEP - number of receptors to rotate
!          NBL    - total number of buoyant lines
!          KK     - bouyant line source group number
!
!        OUTPUTS:
!          Rotated Source and Receptor Coordinates
!          THETA  - Second rotation angle
!          ITHETA - Integer value of THETA
!
!        CALLED FROM:   BL_CALC
!***********************************************************************
!     Variable Declarations

   use buoyant_line
! JAT 06/22/21 D065 COMMENT OUT KURDAT, NOT USED
!      USE MAIN1, ONLY : KURDAT

   implicit none
   double precision, intent(in)  :: wd1
   double precision, intent(out) :: theta
   integer, intent(in)  :: nrecep, nbl, kk
   integer, intent(out) :: itheta

   double precision, dimension(4) :: tchk =&
   &[90.0d0,180.0d0,270.0d0,360.0d0]
   double precision :: dxx, xn, yn, xsave, ysave
   double precision :: costheta, sintheta
   double precision,parameter  :: deg_per_rad = 57.29578d0

   integer, dimension(4) :: il = [1,1,1,1]
   integer, dimension(4) :: iseg = [1,129,129,1]
! JAT 06/22/21 REMOVE ISAVE AS UNUSED VARIABLE
!      INTEGER               :: I, J, ILINE, ISAVE, ISEGN, LNUM
   integer               :: i, j, iline, isegn, lnum
   integer               :: il12, il34

   character (len=12) :: modnam

!     Variable Initializations
   modnam = 'BL_ROTATE2'

!CRT  Initialize variables ! 12/27/2017
   yn = 0.0d0
   xn = 0.0d0
! D41_Wood  Initialize variables
   iline = 0
   isegn = 0

!     THETA is the rotation that aligns the y-axis (previously rotated
!      to be perpendicular to the buoyant lines) with the flow vector.

   theta = 360.0d0 - (wd1 + tcor(kk))
   if (theta < 0.0d0) theta = 360.0d0 + theta
   theta    = dmod(theta,360.0d0)
   itheta   = nint(theta)
   costheta = dcos(theta/deg_per_rad)
   sintheta = dsin(theta/deg_per_rad)
!
!     Calculate source coordinates for each source line segment
!      Note that the coordinate YS_SCS was translated and rotated in
!      BL_ROTATE1 (soset.f)
!
   do lnum = 1,nbl
      if (blineparms(lnum)%iblpgrpnum == kk) then
         dxx = del(lnum)/128.0d0
         xs_scs(lnum,1) = blineparms(lnum)%xbeg_tr1

         do j=2,129
            xs_scs(lnum,j) = xs_scs(lnum,j-1) + dxx
         end do
      end if
   end do

! D41_WOOD: The values of the array IL must be adjusted so the correct
!           XN and YN are determined for each BL source group
   if (kk == 1) then
      il(1) = 1
      il(2) = 1
      il(3) = nblingrp(kk)
      il(4) = nblingrp(kk)

   else if (kk >= 2) then
      il12 = 1
      il34 = nblingrp(1)
      do j = 2,kk
         il12 = il12 + nblingrp(j-1)
         il34 = il34 + nblingrp(j)
      end do
      il(1) = il12
      il(2) = il12
      il(3) = il34
      il(4) = il34
   end if

!
!     Calculate XN, YN (origins of translated coordinate system
!      in terms of the SCS coordinates)
!
   outer: do lnum = 1,nbl
      if (blineparms(lnum)%iblpgrpnum == kk) then

         inner:       do i=1,4
            if (theta >= tchk(i)) cycle
            iline = il(i)
            isegn = iseg(i)
            xn = xs_scs(iline,isegn)
            yn = ys_scs(iline)
            exit outer
         end do inner
      end if
   end do outer

!
! --- Translate line source segment coordinates for this BL source
   do lnum = 1,nbl
      if (blineparms(lnum)%iblpgrpnum == kk) then
         do j=1,129
            xs_rcs(lnum,j) = xs_scs(lnum,j) - xn
            ys_rcs(lnum,j) = ys_scs(lnum) - yn
            if(mod(j,10) == 0.0) then
            endif

         end do
      end if
   end do
!
!     Translate receptor coordinates
   do  i = 1,nrecep
      xr_rcs(i) = xr_scs(i,kk) - xn
      yr_rcs(i) = yr_scs(i,kk) - yn
   end do
!
!     Rotate coordinate system
!
!     Rotate line source segment coordinates
   do lnum = 1,nbl

      if (blineparms(lnum)%iblpgrpnum == kk) then

         do j=1,129
            xsave = xs_rcs(lnum,j)
            ysave = ys_rcs(lnum,j)
            xs_rcs(lnum,j) = xsave*costheta + ysave*sintheta
            ys_rcs(lnum,j) = ysave*costheta - xsave*sintheta
            if(mod(j,10) == 0.0) then
            endif
         end do
      end if
   end do

!     Rotate receptor coordinates
   do i = 1,nrecep
      xsave = xr_rcs(i)
      ysave = yr_rcs(i)
      xr_rcs(i) = xsave*costheta + ysave*sintheta
      yr_rcs(i) = ysave*costheta - xsave*sintheta
   end do

   return
end subroutine bl_rotate2
